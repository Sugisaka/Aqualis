open System
open System.IO
open Aqualis

[<EntryPoint>]
let main _ =
    let expectedVersion = "188.0.1"
    let outputDirectory =
        Path.Combine(Path.GetTempPath(), "aqualis-package-smoke-" + Guid.NewGuid().ToString("N"))

    Directory.CreateDirectory(outputDirectory) |> ignore

    try
        if Aqualis.Version <> expectedVersion then
            failwithf "Expected Aqualis %s, but loaded %s." expectedVersion Aqualis.Version

        Compile [Python] outputDirectory "nuget_smoke" "1.0.0" <| fun context ->
            context.print.s "Aqualis NuGet package works"

        let generatedPath = Path.Combine(outputDirectory, "nuget_smoke.py")
        if not (File.Exists generatedPath) then
            failwithf "Generated Python file was not found: %s" generatedPath

        let generatedCode = File.ReadAllText generatedPath
        if not (generatedCode.Contains("Aqualis NuGet package works", StringComparison.Ordinal)) then
            failwith "The generated Python file does not contain the expected output."

        printfn "Aqualis %s installed from the package and generated Python successfully." Aqualis.Version
        0
    finally
        Directory.Delete(outputDirectory, true)
