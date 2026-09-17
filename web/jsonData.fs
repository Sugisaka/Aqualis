namespace Aqualis

/// User-facing failures and limits for a validated JSON data file.
type JsonFailurePolicy = {
    ReadPublicMessage: string
    SchemaPublicMessage: string
    UpdatePublicMessage: string
    DiagnosticPrefix: string
    ReadOptions: JsonReadOptions
    UpdateOptions: JsonUpdateOptions
}

[<RequireQualifiedAccess>]
module JsonFailurePolicy =
    let defaults = {
        ReadPublicMessage = "データを読み込めませんでした。管理者に連絡してください。"
        SchemaPublicMessage = "データ形式が正しくありません。管理者に連絡してください。"
        UpdatePublicMessage = "データを保存できませんでした。管理者に連絡してください。"
        DiagnosticPrefix = "JSON data"
        ReadOptions = JsonReadOptions.defaults
        UpdateOptions = JsonUpdateOptions.defaults
    }

[<RequireQualifiedAccess>]
module JsonData =
    let private valid (schema:JsonSchema) (data:PHPdata) =
        bool0(Var(Nt,JsonSchema.expression schema data,NaN),data.Context)

    /// Rejects an update inside the atomic transaction so cleanup and unlocking still run.
    let requireDuringUpdate (ctx:Aqualis) (condition:bool0) (diagnostic:string) =
        Aqualis.merge ctx condition.Context |> ignore
        ctx.php.phpcode <| fun () ->
            ctx.writein(
                "if (!(" + condition.code + ")) { throw new \\RuntimeException(" +
                (PHPdata diagnostic).code + "); }")

    let read (ctx:Aqualis) (resultName:PhpVariableName) (filename:PHPdata) (schema:JsonSchema) (policy:JsonFailurePolicy) =
        JsonSchema.requireContext ctx schema
        let result = ctx.php.tryReadJsonFile(resultName,filename,policy.ReadOptions)
        ctx.response.Require(
            result.IsSuccess,
            ServiceUnavailable,
            policy.ReadPublicMessage,
            PHPdata (policy.DiagnosticPrefix + " read failed: ") ++ result.ErrorCode)
        ctx.response.Require(
            valid schema result.Value,
            ServiceUnavailable,
            policy.SchemaPublicMessage,
            PHPdata (policy.DiagnosticPrefix + " schema validation failed."))
        result.Value

    let updateAtomic
        (ctx:Aqualis)
        (resultName:PhpVariableName)
        (filename:PHPdata)
        (schema:JsonSchema)
        (policy:JsonFailurePolicy)
        (update:PHPdata -> unit) =
        JsonSchema.requireContext ctx schema
        let assertValid (data:PHPdata) =
            let expression = JsonSchema.expression schema data
            ctx.php.phpcode <| fun () ->
                ctx.writein(
                    "if (!(" + expression + ")) { throw new \\RuntimeException(" +
                    (PHPdata (policy.DiagnosticPrefix + " schema validation failed during update.")).code +
                    "); }")
        let result =
            ctx.php.updateJsonFileAtomic(resultName,filename,policy.UpdateOptions,fun latest ->
                assertValid latest
                update latest
                assertValid latest)
        ctx.response.Require(
            result.IsSuccess,
            ServiceUnavailable,
            policy.UpdatePublicMessage,
            PHPdata (policy.DiagnosticPrefix + " update failed: ") ++ result.ErrorCode)
        result.Value
