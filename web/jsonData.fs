namespace Aqualis

/// User-facing failures and limits for JSON data access.
type JsonFailurePolicy = {
    ReadPublicMessage: string
    SchemaPublicMessage: string
    UpdatePublicMessage: string
    DiagnosticPrefix: string
    ReadOptions: JsonReadOptions
    UpdateOptions: JsonUpdateOptions
}

[<RequireQualifiedAccess>]
/// Default public messages and limits for JSON data operations.
module JsonFailurePolicy =
    /// Default public messages, diagnostics, and JSON operation limits.
    let defaults = {
        ReadPublicMessage = "データを読み込めませんでした。管理者に連絡してください。"
        SchemaPublicMessage = "データ形式が正しくありません。管理者に連絡してください。"
        UpdatePublicMessage = "データを保存できませんでした。管理者に連絡してください。"
        DiagnosticPrefix = "JSON data"
        ReadOptions = JsonReadOptions.defaults
        UpdateOptions = JsonUpdateOptions.defaults
    }

[<RequireQualifiedAccess>]
/// Typed JSON file access and atomic update helpers for PHP generation.
module JsonData =
    /// Builds a schema-validation expression for JSON data.
    let private valid (schema:JsonSchema) (data:PHPdata) (shape:PHPdata) =
        bool0(Var(Nt,JsonSchema.expression schema data shape,NaN),data.Context)

    /// Decodes the source JSON while retaining object shape information.
    let private decodeShape (ctx:Aqualis) (name:string) (source:PHPdata) (maxDepth:int) =
        let shape = PHPdata.var(ctx,name)
        ctx.php.phpcode <| fun () ->
            ctx.writein(shape.code + " = json_decode(" + source.code + ", false, " +
                        InvariantFormat.integer maxDepth + ", JSON_THROW_ON_ERROR);")
        shape

    /// Retain object shapes for associative arrays, while allowing a replacement list.
    let private preserveObjectShapes (ctx:Aqualis) (name:string) (data:PHPdata) (originalShape:PHPdata) (replacements:string) =
        let rebuild = "$" + name + "_rebuildShape"
        let published = PHPdata.var(ctx,name + "_publishValue")
        ctx.php.phpcode <| fun () ->
            ctx.writein(rebuild + " = function ($value, $original, $path) use (&" + rebuild + ", " + replacements + ") {")
            ctx.writein("if (isset(" + replacements + "[serialize($path)])) {")
            ctx.writein("if (!is_array($value)) { return $value; }")
            ctx.writein("$result = [];")
            ctx.writein("foreach ($value as $key => $item) {")
            ctx.writein("$property = (string)$key;")
            ctx.writein("$previous = is_object($original) ? (property_exists($original, $property) ? $original->{$property} : null) : (is_array($original) && array_key_exists($key, $original) ? $original[$key] : null);")
            ctx.writein("$result[$key] = " + rebuild + "($item, $previous, [...$path, $key]);")
            ctx.writein("}")
            ctx.writein("return $result;")
            ctx.writein("}")
            ctx.writein("if (!is_array($value)) { return $value; }")
            ctx.writein("if (is_object($original)) {")
            ctx.writein("$originalArray = (array)$original;")
            ctx.writein("if ($value !== [] && array_values($value) === $value && ($originalArray === [] || array_values($originalArray) !== $originalArray)) {")
            ctx.writein("$result = [];")
            ctx.writein("foreach ($value as $key => $item) {")
            ctx.writein("$property = (string)$key;")
            ctx.writein("$previous = property_exists($original, $property) ? $original->{$property} : null;")
            ctx.writein("$result[$key] = " + rebuild + "($item, $previous, [...$path, $key]);")
            ctx.writein("}")
            ctx.writein("return $result;")
            ctx.writein("}")
            ctx.writein("$result = new \\stdClass();")
            ctx.writein("foreach ($value as $key => $item) {")
            ctx.writein("$property = (string)$key;")
            ctx.writein("$previous = property_exists($original, $property) ? $original->{$property} : null;")
            ctx.writein("$result->{$property} = " + rebuild + "($item, $previous, [...$path, $key]);")
            ctx.writein("}")
            ctx.writein("return $result;")
            ctx.writein("}")
            ctx.writein("$result = [];")
            ctx.writein("foreach ($value as $key => $item) {")
            ctx.writein("$previous = is_array($original) && array_key_exists($key, $original) ? $original[$key] : null;")
            ctx.writein("$result[$key] = " + rebuild + "($item, $previous, [...$path, $key]);")
            ctx.writein("}")
            ctx.writein("return $result;")
            ctx.writein("};")
            ctx.writein(published.code + " = " + rebuild + "(" + data.code + ", " + originalShape.code + ", []);")
        published

    /// Rejects an update inside the atomic transaction so cleanup and unlocking still run.
    let requireDuringUpdate (ctx:Aqualis) (condition:bool0) (diagnostic:string) =
        Aqualis.merge ctx condition.Context |> ignore
        ctx.php.phpcode <| fun () ->
            ctx.writein(
                "if (!(" + condition.code + ")) { throw new \\RuntimeException(" +
                (PHPdata diagnostic).code + "); }")

    /// Read JSON produced by the same application without checking its data shape.
    let readWithoutSchema (ctx:Aqualis) (resultName:PhpVariableName) (filename:PHPdata) (policy:JsonFailurePolicy) =
        let result = ctx.php.tryReadJsonFile(resultName,filename,policy.ReadOptions)
        ctx.response.Require(
            result.IsSuccess,
            ServiceUnavailable,
            policy.ReadPublicMessage,
            PHPdata (policy.DiagnosticPrefix + " read failed: ") ++ result.ErrorCode)
        result.Value

    /// Update application-owned JSON atomically without checking its data shape.
    let updateAtomicWithoutSchema
        (ctx:Aqualis)
        (resultName:PhpVariableName)
        (filename:PHPdata)
        (policy:JsonFailurePolicy)
        (update:PHPdata -> unit) =
        let result =
            ctx.php.updateJsonFileAtomicWithSource(resultName,filename,policy.UpdateOptions,fun latest sourceText ->
                let baseName = PhpVariableName.value resultName
                let inputShape = decodeShape ctx (baseName + "_sourceShape") sourceText policy.UpdateOptions.MaxDepth
                let replacements = "$" + baseName + "_replacedPaths"
                ctx.php.phpcode <| fun () -> ctx.writein(replacements + " = [];")
                latest.TrackJsonReplacements replacements
                update latest
                let published = preserveObjectShapes ctx baseName latest inputShape replacements
                Some published)
        ctx.response.Require(
            result.IsSuccess,
            ServiceUnavailable,
            policy.UpdatePublicMessage,
            PHPdata (policy.DiagnosticPrefix + " update failed: ") ++ result.ErrorCode)
        result.Value

    /// Reads JSON and rejects the request if reading or schema validation fails.
    let read (ctx:Aqualis) (resultName:PhpVariableName) (filename:PHPdata) (schema:JsonSchema) (policy:JsonFailurePolicy) =
        JsonSchema.requireContext ctx schema
        let result = ctx.php.tryReadJsonFile(resultName,filename,policy.ReadOptions)
        ctx.response.Require(
            result.IsSuccess,
            ServiceUnavailable,
            policy.ReadPublicMessage,
            PHPdata (policy.DiagnosticPrefix + " read failed: ") ++ result.ErrorCode)
        let shape =
            decodeShape ctx (PhpVariableName.value resultName + "_schemaShape") result.SourceText policy.ReadOptions.MaxDepth
        ctx.response.Require(
            valid schema result.Value shape,
            ServiceUnavailable,
            policy.SchemaPublicMessage,
            PHPdata (policy.DiagnosticPrefix + " schema validation failed."))
        result.Value

    /// Updates application-owned JSON atomically and validates its schema before and after the update.
    let updateAtomic
        (ctx:Aqualis)
        (resultName:PhpVariableName)
        (filename:PHPdata)
        (schema:JsonSchema)
        (policy:JsonFailurePolicy)
        (update:PHPdata -> unit) =
        JsonSchema.requireContext ctx schema
        let assertValid (data:PHPdata) (shape:PHPdata) =
            let expression = JsonSchema.expression schema data shape
            ctx.php.phpcode <| fun () ->
                ctx.writein(
                    "if (!(" + expression + ")) { throw new \\RuntimeException(" +
                    (PHPdata (policy.DiagnosticPrefix + " schema validation failed during update.")).code +
                    "); }")
        let result =
            ctx.php.updateJsonFileAtomicWithSource(resultName,filename,policy.UpdateOptions,fun latest sourceText ->
                let baseName = PhpVariableName.value resultName
                let inputShape = decodeShape ctx (baseName + "_sourceShape") sourceText policy.UpdateOptions.MaxDepth
                assertValid latest inputShape
                let replacements = "$" + baseName + "_replacedPaths"
                ctx.php.phpcode <| fun () -> ctx.writein(replacements + " = [];")
                latest.TrackJsonReplacements replacements
                update latest
                let published = preserveObjectShapes ctx baseName latest inputShape replacements
                let outputText = PHPdata.var(ctx,baseName + "_outputText")
                ctx.php.phpcode <| fun () ->
                    ctx.writein(outputText.code + " = json_encode(" + published.code + ", JSON_THROW_ON_ERROR);")
                let outputShape = decodeShape ctx (baseName + "_schemaShape") outputText policy.UpdateOptions.MaxDepth
                let outputData = PHPdata.var(ctx,baseName + "_schemaData")
                ctx.php.phpcode <| fun () ->
                    ctx.writein(outputData.code + " = json_decode(" + outputText.code + ", true, " +
                                InvariantFormat.integer policy.UpdateOptions.MaxDepth + ", JSON_THROW_ON_ERROR);")
                assertValid outputData outputShape
                Some published)
        ctx.response.Require(
            result.IsSuccess,
            ServiceUnavailable,
            policy.UpdatePublicMessage,
            PHPdata (policy.DiagnosticPrefix + " update failed: ") ++ result.ErrorCode)
        result.Value
