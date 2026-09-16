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
                // Based on test/test5/test5.fsx: exercise text and binary write/read round trips.
                let fileNamePrefix =
                    String.replicate 120 "a" + "-%-'quoted'-"
                let textFileName = fileNamePrefix ++ value ++ ".txt"
                context.io.fileOutput textFileName <| fun writer ->
                    writer.t value
                let textValue = context.var.i0 "textValue"
                textValue <== 0
                context.io.fileInput textFileName <| fun reader ->
                    reader.t textValue

                let binaryFileName = "binary-" ++ value ++ ".bin"
                context.io.binfileOutput binaryFileName <| fun writer ->
                    writer.b value
                let binaryValue = context.var.i0 "binaryValue"
                binaryValue <== 0
                context.io.binfileInput binaryFileName <| fun reader ->
                    reader.b binaryValue

                // Based on test/test0/test0.fsx: exercise all dynamic-array ranks.
                let vector = context.var.i1 "vector"
                let matrix = context.var.i2 "matrix"
                let tensor = context.var.i3 "tensor"
                vector.allocate 2
                matrix.allocate(2, 3)
                tensor.allocate(2, 3, 4)
                vector[1] <== textValue
                matrix[1, 2] <== binaryValue
                tensor[1, 2, 3] <== -42
                let result = context.var.i0 "result"
                result <== vector[1] + matrix[1, 2] + tensor[1, 2, 3]
                vector.deallocate()
                matrix.deallocate()
                tensor.deallocate()
                context.print.t result
            else
                context.print.t value

    let private generateCArrayCase outputRoot caseName (code:Aqualis -> unit) =
        let outputDirectory = Path.Combine(outputRoot, "c-array-" + caseName)
        Directory.CreateDirectory(outputDirectory) |> ignore
        Compile [C99] outputDirectory "smoke" "1.0" <| fun context ->
            context.Setting.DebugMode ON
            code context

    let private generateCArrayCases outputRoot =
        generateCArrayCase outputRoot "success" <| fun context ->
            let vector = context.var.i1 "vector"
            let matrix = context.var.i2 "matrix"
            let tensor = context.var.i3 "tensor"
            vector.allocate 2
            matrix.allocate(2, 3)
            tensor.allocate(2, 3, 4)
            vector[1] <== 10
            matrix[1, 2] <== 20
            tensor[1, 2, 3] <== 12
            let result = context.var.i0 "result"
            result <== vector[1] + matrix[1, 2] + tensor[1, 2, 3]
            vector.deallocate()
            matrix.deallocate()
            tensor.deallocate()
            context.print.t result

        generateCArrayCase outputRoot "double-allocate" <| fun context ->
            let vector = context.var.i1 "vector"
            vector.allocate 2
            vector.allocate 2

        generateCArrayCase outputRoot "unallocated-access" <| fun context ->
            let vector = context.var.i1 "vector"
            vector[0] <== 1

        generateCArrayCase outputRoot "double-free" <| fun context ->
            let vector = context.var.i1 "vector"
            vector.allocate 2
            vector.deallocate()
            vector.deallocate()

        generateCArrayCase outputRoot "invalid-size" <| fun context ->
            let vector = context.var.i1 "vector"
            vector.allocate 0

        generateCArrayCase outputRoot "overflow" <| fun context ->
            let tensor = context.var.i3 "tensor"
            tensor.allocate(Int32.MaxValue, Int32.MaxValue, Int32.MaxValue)

        generateCArrayCase outputRoot "out-of-bounds" <| fun context ->
            let matrix = context.var.i2 "matrix"
            matrix.allocate(2, 3)
            matrix[2, 0] <== 1

        generateCArrayCase outputRoot "malloc-failure" <| fun context ->
            let vector = context.var.i1 "vector"
            vector.allocate 2
        File.WriteAllText(
            Path.Combine(outputRoot, "c-array-malloc-failure", "malloc-fail.c"),
            "#include <stddef.h>\nvoid *__wrap_malloc(size_t size) { (void)size; return NULL; }\n")

    let private generatePhpUpload outputRoot directoryName destinationDirectory additionalPublicDirectories =
        let outputDirectory = Path.Combine(outputRoot, directoryName)
        Directory.CreateDirectory(outputDirectory) |> ignore
        let policy =
            { UploadPolicy.create destinationDirectory 1024L ["text/plain", "txt"] with
                AdditionalPublicDirectories = additionalPublicDirectories }
        Compile [PHP] outputDirectory "upload" "1.0" <| fun context ->
            let upload = postFile.single(context, "file")
            let result = upload.save(policy)
            context.php.phpcode <| fun () ->
                context.writein "header('Content-Type: application/json');"
                context.writein ("echo json_encode(" + result.Results.code + ", JSON_THROW_ON_ERROR);")

    let private generatePhpUploads outputRoot =
        generatePhpUpload outputRoot "php-upload-private" "../private-uploads" []
        generatePhpUpload outputRoot "php-upload-public" "." []
        generatePhpUpload outputRoot "php-upload-alias" "../private-uploads" ["public-link"]

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
            generateCArrayCases outputRoot
            generatePhpUploads outputRoot
            printfn "Generated runtime smoke programs in %s" outputRoot
            0
        |_ ->
            eprintfn "Usage: Aqualis.GeneratedCodeSmoke <output-directory>"
            2
