namespace Aqualis.Tests

open System
open System.IO
open Xunit
open Aqualis

module JsonDataGenerationTests =
    let private generate code =
        use output = new TemporaryDirectory()
        Aqualis.makeProgramWithContext(output.Path,"json-data.php",PHP) code
        File.ReadAllText(Path.Combine(output.Path,"json-data.php"))

    [<Fact>]
    let ``validated read and update share the same schema under the update lock`` () =
        let generated =
            generate <| fun ctx ->
                let expectedUser = ctx.php.var "expectedUser"
                let schema =
                    JsonSchema.obj [
                        "SchemaVersion", JsonSchema.literalInt 1
                        "UserId", JsonSchema.sameAs expectedUser JsonSchema.string
                        "Score", JsonSchema.listExactly 2 (JsonSchema.oneOfInts [0; 1; 2])
                        "Answer", JsonSchema.mapExactly ["q1"; "q2"] JsonSchema.finiteNumber
                    ]
                let path = ctx.php.var "path"
                let policy = { JsonFailurePolicy.defaults with DiagnosticPrefix = "test score" }
                JsonData.read ctx (PhpVariableName.create "scoreRead") path schema policy |> ignore
                JsonData.updateAtomic
                    ctx
                    (PhpVariableName.create "scoreUpdate")
                    path
                    schema
                    policy
                    (fun latest -> latest["Score"].[0] <== 2)
                |> ignore

        Assert.Contains("test score schema validation failed.",generated)
        Assert.Contains("test score schema validation failed during update.",generated)
        Assert.Contains("$scoreRead[\"value\"][\"UserId\"] === $expectedUser",generated)
        Assert.Contains("count($scoreUpdate_data[\"Score\"]) === 2",generated)
        Assert.Contains("is_finite((float)$scoreUpdate_data[\"Answer\"][\"q1\"])",generated)
        let lockIndex = generated.IndexOf("flock($scoreUpdate_lockHandle, LOCK_EX)",StringComparison.Ordinal)
        let validationIndex = generated.IndexOf("test score schema validation failed during update.",StringComparison.Ordinal)
        let changeIndex = generated.IndexOf("$scoreUpdate_data[\"Score\"][0] = 2",StringComparison.Ordinal)
        let renameIndex = generated.IndexOf("@rename($scoreUpdate_temporaryPath, $scoreUpdate_target)",StringComparison.Ordinal)
        Assert.True(lockIndex >= 0 && lockIndex < validationIndex)
        Assert.True(validationIndex < changeIndex && changeIndex < renameIndex)

    [<Fact>]
    let ``schema constructors reject inconsistent declarations`` () =
        Assert.Throws<ArgumentException>(fun () -> JsonSchema.listExactly -1 JsonSchema.int |> ignore) |> ignore
        Assert.Throws<ArgumentException>(fun () ->
            JsonSchema.obj ["ID",JsonSchema.string; "ID",JsonSchema.int] |> ignore) |> ignore
        Assert.Throws<ArgumentException>(fun () ->
            JsonSchema.mapExactly ["q1";"q1"] JsonSchema.string |> ignore) |> ignore

    [<Fact>]
    let ``runtime-bound schema values must belong to the generation context`` () =
        use output = new TemporaryDirectory()
        use first = new Aqualis(Some output.Path,Some "first.php",PHP)
        use second = new Aqualis(Some output.Path,Some "second.php",PHP)
        let schema = JsonSchema.sameAs (first.php.var "expected") JsonSchema.string
        Assert.Throws<InvalidOperationException>(fun () ->
            JsonData.read
                second
                (PhpVariableName.create "read")
                (PHPdata "data.json")
                schema
                JsonFailurePolicy.defaults
            |> ignore) |> ignore
