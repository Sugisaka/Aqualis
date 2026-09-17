namespace Aqualis

open System
open System.Text.RegularExpressions

/// A reusable schema for JSON values decoded as PHP associative arrays.
type JsonSchema internal (render:string -> string -> int -> string, dependencies:Aqualis list) =
    /// Renders a parenthesized PHP expression that validates a decoded value.
    member internal _.Render(value:string, shape:string, depth:int) = "(" + render value shape depth + ")"
    /// Gets contexts referenced by the schema's generated expression.
    member internal _.Dependencies = dependencies

[<RequireQualifiedAccess>]
/// Builds composable validators for JSON decoded into PHP values.
module JsonSchema =
    /// Encodes a PHP string literal for schema generation.
    let private literal (value:string) = PhpEncoding.stringLiteral value
    /// Combines schema conditions with logical AND.
    let private all conditions =
        match conditions with
        | [] -> "true"
        | _ -> conditions |> List.map (fun condition -> "(" + condition + ")") |> String.concat " && "
    /// Creates a JSON schema from a rendering function.
    let private create render = JsonSchema(render,[])
    /// Creates a JSON schema with additional constraints.
    let private createWith dependencies render = JsonSchema(render,dependencies)
    /// Builds a JSON element-count validation expression.
    let private checkCount count =
        if count < 0 then invalidArg (nameof count) "A JSON list length cannot be negative."

    /// Escape hatch for application-specific relationships between fields.
    /// The callback receives an expression for the decoded PHP value.
    let predicate (render:string -> string) =
        if isNull (box render) then nullArg (nameof render)
        create (fun value _ _ -> render value)

    /// Accepts only JSON strings.
    let string = create (fun value _ _ -> "is_string(" + value + ")")
    /// Accepts only JSON integers.
    let int = create (fun value _ _ -> "is_int(" + value + ")")
    /// Accepts only JSON booleans.
    let bool = create (fun value _ _ -> "is_bool(" + value + ")")
    /// Accepts finite JSON integers or floating-point values.
    let finiteNumber = create (fun value _ _ -> "(is_int(" + value + ") || is_float(" + value + ")) && is_finite((float)" + value + ")")
    /// Accepts an integer within the inclusive bounds; rejects reversed bounds.
    let intRange minimum maximum =
        if minimum > maximum then invalidArg (nameof minimum) "The minimum exceeds the maximum."
        create (fun value _ _ -> "is_int(" + value + ") && " + value + " >= " + InvariantFormat.integer minimum + " && " + value + " <= " + InvariantFormat.integer maximum)
    /// Accepts integers greater than or equal to the minimum.
    let intAtLeast minimum =
        create (fun value _ _ -> "is_int(" + value + ") && " + value + " >= " + InvariantFormat.integer minimum)
    /// Accepts only the specified integer.
    let literalInt expected =
        create (fun value _ _ -> "is_int(" + value + ") && " + value + " === " + InvariantFormat.integer expected)
    /// Accepts only the specified string, compared strictly in PHP.
    let literalString expected =
        if isNull expected then nullArg (nameof expected)
        create (fun value _ _ -> "is_string(" + value + ") && " + value + " === " + literal expected)
    /// Accepts a string present in the supplied list using strict comparison.
    let oneOfStrings (values:string list) =
        if values |> List.exists isNull then invalidArg (nameof values) "JSON enum values cannot be null."
        let encoded = values |> List.map literal |> String.concat ", "
        create (fun value _ _ -> "is_string(" + value + ") && in_array(" + value + ", [" + encoded + "], true)")
    /// Accepts an integer present in the supplied list using strict comparison.
    let oneOfInts (values:int list) =
        let encoded = values |> List.map InvariantFormat.integer |> String.concat ", "
        create (fun value _ _ -> "is_int(" + value + ") && in_array(" + value + ", [" + encoded + "], true)")
    /// Accepts strings matching the supplied PHP regular expression.
    let matches (pattern:string) =
        if String.IsNullOrEmpty pattern then invalidArg (nameof pattern) "A JSON string pattern is required."
        create (fun value _ _ -> "is_string(" + value + ") && preg_match(" + literal pattern + ", " + value + ") === 1")
    /// Requires both the supplied schema and strict equality with a PHP expression.
    let sameAs (expected:PHPdata) (schema:JsonSchema) =
        let expectedExpression =
            // A variable or fixed array offset is already one PHP expression.
            // Parenthesize everything else so ||, ??, and similar operators stay on the right of ===.
            if Regex.IsMatch(expected.code, @"^\$[A-Za-z_][A-Za-z0-9_]*(?:\[""[A-Za-z0-9_]+""\]|\['[A-Za-z0-9_]+'\]|\[[0-9]+\])*$") then expected.code
            else "(" + expected.code + ")"
        createWith (expected.Context :: schema.Dependencies)
            (fun value shape depth -> schema.Render(value,shape,depth) + " && " + value + " === " + expectedExpression)

    /// Accepts a sequential JSON array whose every item matches the schema.
    let list (item:JsonSchema) =
        createWith item.Dependencies (fun value shape depth ->
            let itemName = if depth = 0 then "$item" else "$aqualisItem" + InvariantFormat.integer depth
            let keyName = if depth = 0 then "$key" else "$aqualisKey" + InvariantFormat.integer depth
            all [ "is_array(" + shape + ")"
                  "is_array(" + value + ")"
                  "array_values(" + value + ") === " + value
                  "count(" + shape + ") === count(" + value + ")"
                  "count(array_filter(" + value + ", static fn(" + itemName + ", " + keyName + "): bool => " +
                  item.Render(itemName,shape + "[" + keyName + "]",depth + 1) +
                  ", ARRAY_FILTER_USE_BOTH)) === count(" + value + ")" ])
    /// Accepts a sequential JSON array with exactly the requested number of items.
    let listExactly count item =
        checkCount count
        let baseSchema = list item
        createWith baseSchema.Dependencies (fun value shape depth -> baseSchema.Render(value,shape,depth) + " && count(" + value + ") === " + InvariantFormat.integer count)

    /// Required fields; additional fields are accepted for compatibility with existing files.
    let obj (fields:(string * JsonSchema) list) =
        if fields |> List.exists (fun (key,_) -> isNull key) then invalidArg (nameof fields) "JSON field names cannot be null."
        if (fields |> List.map fst |> List.distinct).Length <> fields.Length then invalidArg (nameof fields) "JSON field names must be unique."
        createWith (fields |> List.collect (fun (_,schema) -> schema.Dependencies)) (fun value shape depth ->
            "is_object(" + shape + ") && is_array(" + value + ") && " +
            (fields
             |> List.map (fun (key,schema) ->
                 let encoded = literal key
                 "property_exists(" + shape + ", " + encoded + ") && array_key_exists(" + encoded + ", " + value + ") && " +
                 schema.Render(value + "[" + encoded + "]",shape + "->{" + encoded + "}",depth))
             |> all))
    /// Adds required string-valued keys to an object schema.
    let withRequiredStringKeys (keys:string list) (schema:JsonSchema) =
        if keys |> List.exists isNull then invalidArg (nameof keys) "JSON field names cannot be null."
        if (List.distinct keys).Length <> keys.Length then invalidArg (nameof keys) "JSON field names must be unique."
        createWith schema.Dependencies (fun value shape depth ->
            all ("is_object(" + shape + ")" :: schema.Render(value,shape,depth) ::
                 (keys |> List.map (fun key ->
                     let encoded = literal key
                     "property_exists(" + shape + ", " + encoded + ") && array_key_exists(" + encoded + ", " + value + ") && is_string(" + value + "[" + encoded + "]" + ")"))))
    /// A map with exactly the supplied keys and no additional entries.
    let mapExactly (keys:string list) (item:JsonSchema) =
        if keys |> List.exists isNull then invalidArg (nameof keys) "JSON map keys cannot be null."
        if (List.distinct keys).Length <> keys.Length then invalidArg (nameof keys) "JSON map keys must be unique."
        createWith item.Dependencies (fun value shape depth ->
            all ([ "is_object(" + shape + ")"
                   "is_array(" + value + ")"
                   "count(" + value + ") === " + InvariantFormat.integer keys.Length ] @
                 (keys |> List.map (fun key ->
                     let encoded = literal key
                     "property_exists(" + shape + ", " + encoded + ") && array_key_exists(" + encoded + ", " + value + ") && " +
                     item.Render(value + "[" + encoded + "]",shape + "->{" + encoded + "}",depth)))))
    /// Validates an optional object field when that field is present.
    let optionalField key (field:JsonSchema) (schema:JsonSchema) =
        if isNull key then nullArg (nameof key)
        createWith (schema.Dependencies @ field.Dependencies) (fun value shape depth ->
            let encoded = literal key
            all [ "is_object(" + shape + ")"
                  schema.Render(value,shape,depth)
                  "(!property_exists(" + shape + ", " + encoded + ") || (array_key_exists(" + encoded + ", " + value + ") && " +
                  field.Render(value + "[" + encoded + "]",shape + "->{" + encoded + "}",depth) + "))" ])
    /// Accepts values satisfying every rule; an empty list accepts all values.
    let andAlso (rules:JsonSchema list) =
        createWith (rules |> List.collect (fun rule -> rule.Dependencies))
            (fun value shape depth -> rules |> List.map (fun rule -> rule.Render(value,shape,depth)) |> all)
    /// Accepts values satisfying at least one rule; an empty list rejects all values.
    let anyOf (rules:JsonSchema list) =
        createWith (rules |> List.collect (fun rule -> rule.Dependencies)) (fun value shape depth ->
            match rules with
            | [] -> "false"
            | _ -> rules |> List.map (fun rule -> rule.Render(value,shape,depth)) |> String.concat " || ")
    /// Requires the named field to be an array whose entries have unique scalar
    /// or null values in the specified child field.
    let uniqueFieldInList listField uniqueField =
        let listKey = literal listField
        let uniqueKey = literal uniqueField
        create (fun value _ _ ->
            let entries = value + "[" + listKey + "]"
            all [ "is_array(" + value + ")"
                  "array_key_exists(" + listKey + ", " + value + ")"
                  "is_array(" + entries + ")"
                  "count(array_filter(" + entries + ", static fn($aqualisEntry): bool => is_array($aqualisEntry) && array_key_exists(" + uniqueKey + ", $aqualisEntry) && (is_scalar($aqualisEntry[" + uniqueKey + "]) || is_null($aqualisEntry[" + uniqueKey + "])))) === count(" + entries + ")"
                  "count(array_unique(array_column(" + entries + ", " + uniqueKey + "), SORT_STRING)) === count(" + entries + ")" ])
    /// Requires two named array fields to have equal lengths.
    let sameLength leftField rightField =
        let left = literal leftField
        let right = literal rightField
        create (fun value _ _ ->
            let leftEntries = value + "[" + left + "]"
            let rightEntries = value + "[" + right + "]"
            all [ "is_array(" + value + ")"
                  "array_key_exists(" + left + ", " + value + ")"
                  "array_key_exists(" + right + ", " + value + ")"
                  "is_array(" + leftEntries + ")"
                  "is_array(" + rightEntries + ")"
                  "count(" + leftEntries + ") === count(" + rightEntries + ")" ])

    /// Builds the PHP validation expression for a schema.
    let internal expression (schema:JsonSchema) (data:PHPdata) (shape:PHPdata) = schema.Render(data.code,shape.code,0)
    /// Requires that a schema belongs to the supplied generation context.
    let internal requireContext (ctx:Aqualis) (schema:JsonSchema) =
        Aqualis.mergeMany (ctx :: schema.Dependencies) |> ignore

/// Rules that relate fields within an otherwise valid JSON object.
[<RequireQualifiedAccess>]
module JsonRule =
    /// Requires unique child-field values within an array field.
    let uniqueFieldInList = JsonSchema.uniqueFieldInList
    /// Requires two array fields to have the same length.
    let sameLength = JsonSchema.sameLength
    /// Creates a validation rule from a PHP expression callback.
    let custom = JsonSchema.predicate
