namespace Aqualis.GeneratedCodeSmoke

open System
open System.IO
open Aqualis

module Program =
    let private generationTargets =
        [ "c", C99
          "fortran", Fortran
          "python", Python
          "javascript", JavaScript
          "php", PHP ]

    let private generate outputRoot (directoryName, language) =
        let outputDirectory = Path.Combine(outputRoot, directoryName)
        Directory.CreateDirectory(outputDirectory) |> ignore

        Compile [language] outputDirectory "smoke" "1.0" <| fun context ->
            // Node.js does not provide the browser-style print function emitted by Aqualis.
            if language = JavaScript then
                context.writein "globalThis.print = console.log;"

            let value = context.var.i0 "value"
            value <== 40
            value <== value + 2

            if language = C99 || language = Fortran || language = Python then
                let fileNamePrefix =
                    String.replicate 120 "a" + "-%-'quoted'-"
                context.io.fileOutput (fileNamePrefix ++ value ++ ".txt") <| fun writer ->
                    writer.t value

            context.print.t value

    let private generatePythonSciPy outputRoot =
        let outputDirectory = Path.Combine(outputRoot, "python-scipy")
        Directory.CreateDirectory(outputDirectory) |> ignore

        Compile [Python] outputDirectory "smoke" "1.0" <| fun context ->
            let argument = context.var.d0 "argument"
            argument <== 0.0
            asm.besselj0 argument <| fun result -> context.print.t result

    [<EntryPoint>]
    let main arguments =
        match arguments with
        |[| outputRoot |] ->
            let outputRoot = Path.GetFullPath(outputRoot)
            Directory.CreateDirectory(outputRoot) |> ignore
            generationTargets |> List.iter (generate outputRoot)
            generatePythonSciPy outputRoot
            printfn "Generated runtime smoke programs in %s" outputRoot
            0
        |_ ->
            eprintfn "Usage: Aqualis.GeneratedCodeSmoke <output-directory>"
            2
