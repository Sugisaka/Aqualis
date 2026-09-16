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

    let private generateCArrayCase outputRoot caseName debugMode (code:Aqualis -> unit) =
        let outputDirectory = Path.Combine(outputRoot, "c-array-" + caseName)
        Directory.CreateDirectory(outputDirectory) |> ignore
        Compile [C99] outputDirectory "smoke" "1.0" <| fun context ->
            if debugMode then context.Setting.DebugMode ON
            code context

    let private generateCArrayCases outputRoot =
        generateCArrayCase outputRoot "success" true <| fun context ->
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

        generateCArrayCase outputRoot "double-allocate" true <| fun context ->
            let vector = context.var.i1 "vector"
            vector.allocate 2
            vector.allocate 2

        generateCArrayCase outputRoot "unallocated-access" true <| fun context ->
            let vector = context.var.i1 "vector"
            vector[0] <== 1

        generateCArrayCase outputRoot "double-free" true <| fun context ->
            let vector = context.var.i1 "vector"
            vector.allocate 2
            vector.deallocate()
            vector.deallocate()

        generateCArrayCase outputRoot "invalid-size" true <| fun context ->
            let vector = context.var.i1 "vector"
            vector.allocate 0

        generateCArrayCase outputRoot "overflow" true <| fun context ->
            let tensor = context.var.i3 "tensor"
            tensor.allocate(Int32.MaxValue, Int32.MaxValue, Int32.MaxValue)

        generateCArrayCase outputRoot "out-of-bounds" true <| fun context ->
            let matrix = context.var.i2 "matrix"
            matrix.allocate(2, 3)
            matrix[2, 0] <== 1

        generateCArrayCase outputRoot "malloc-failure" true <| fun context ->
            let vector = context.var.i1 "vector"
            vector.allocate 2
        File.WriteAllText(
            Path.Combine(outputRoot, "c-array-malloc-failure", "malloc-fail.c"),
            "#include <stddef.h>\nvoid *__wrap_malloc(size_t size) { (void)size; return NULL; }\n")

        generateCArrayCase outputRoot "release-negative" false <| fun context ->
            let vector = context.var.i1 "vector"
            vector.allocate -1

        generateCArrayCase outputRoot "release-overflow" false <| fun context ->
            let tensor = context.var.i3 "tensor"
            tensor.allocate(Int32.MaxValue, Int32.MaxValue, Int32.MaxValue)

        generateCArrayCase outputRoot "release-malloc-failure" false <| fun context ->
            let vector = context.var.i1 "vector"
            vector.allocate 2
        File.WriteAllText(
            Path.Combine(outputRoot, "c-array-release-malloc-failure", "malloc-fail.c"),
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

    let private generateDistributedC outputRoot =
        let outputDirectory = Path.Combine(outputRoot, "c-distributed")
        Directory.CreateDirectory(outputDirectory) |> ignore
        Compile [C99] outputDirectory "distributed" "1.0" <| fun context ->
            use scripts = new shellscript.Shell(context, outputDirectory, "distributed", 1)
            scripts.AddProcess()
            context.print.t (int0(Int 42))

    let private generateLeadingHyphenC outputRoot =
        let outputDirectory = Path.Combine(outputRoot, "c-leading-hyphen")
        Directory.CreateDirectory(outputDirectory) |> ignore
        Compile [C99] outputDirectory "-leading" "1.0" <| fun context ->
            context.print.t (int0(Int 42))

    let private generateLeadingHyphenFortran outputRoot =
        let outputDirectory = Path.Combine(outputRoot, "fortran-leading-hyphen")
        Directory.CreateDirectory(outputDirectory) |> ignore
        Compile [Fortran] outputDirectory "-leading" "1.0" <| fun context ->
            context.print.t (int0(Int 42))

    let private generatePhpTextValidation outputRoot =
        let outputDirectory = Path.Combine(outputRoot, "php-text-validation")
        Directory.CreateDirectory(outputDirectory) |> ignore
        Compile [PHP] outputDirectory "validation" "1.0" <| fun context ->
            let value =
                context.request.post.RequiredText(
                    FieldName.create "value",
                    minLength = 2,
                    maxLength = 2,
                    maxUtf8Bytes = 6)
            context.php.phpcode <| fun () ->
                for assignment in
                    [ "$_POST['value'] = 'あい';"
                      "$_POST['value'] = 'abc';"
                      "$_POST['value'] = chr(255);"
                      "$_POST['value'] = 'あいえ';" ] do
                    context.writein assignment
                    context.writein ("echo (" + value.IsValid.code + ") ? '1' : '0';")

    let private generateOutputEscaping outputRoot (directoryName, language) =
        let outputDirectory = Path.Combine(outputRoot, "output-" + directoryName)
        Directory.CreateDirectory(outputDirectory) |> ignore
        Compile [language] outputDirectory "smoke" "1.0" <| fun context ->
            if language = JavaScript then
                context.writein "globalThis.print = console.log;"
            let value = context.var.i0 "value"
            value <== 7
            context.print.s "a\"b\\c%"
            context.print.tt (st "x%=" ++ value)
            context.print.tt (st "only%text")
            if language = C99 || language = Python then
                context.io.fileOutput "literal.txt" <| fun writer ->
                    writer.cc (st "file\"\\%")
                    writer.cc (st "mix%=" ++ value)

    let private generateTextRead outputRoot (directoryName, language) =
        let outputDirectory = Path.Combine(outputRoot, "read-" + directoryName)
        Directory.CreateDirectory(outputDirectory) |> ignore
        Compile [language] outputDirectory "smoke" "1.0" <| fun context ->
            let value = context.var.i0 "value"
            let sum = context.var.i0 "sum"
            sum <== 0
            context.io.file_Read (st "data file.txt") (iv value) <| fun _ ->
                sum <== sum + value
            context.print.t sum

    let private generateSplineValidation outputRoot (directoryName, language) caseName =
        let outputDirectory = Path.Combine(outputRoot, "spline-" + directoryName + "-" + caseName)
        Directory.CreateDirectory(outputDirectory) |> ignore
        Compile [language] outputDirectory "smoke" "1.0" <| fun context ->
            let spline = context.interpolate.splineDouble()
            spline.X.allocate 2
            spline.Y.allocate 2
            spline.X[0] <== 0.0
            spline.X[1] <== (if caseName = "unordered" then 0.0 else 1.0)
            spline.Y[0] <== 0.0
            spline.Y[1] <== (if caseName = "non-finite-y" then Double.NaN else 10.0)
            spline.set()
            let result = context.var.d0 "result"
            let query = context.var.d0 "query"
            query <== (if caseName = "out-of-range" then -1.0 else 0.5)
            if caseName = "literal" then
                spline.p result (double0(Dbl 0.5))
            else
                spline.p result query
            context.print.t result

    let private generateFixedFileIo outputRoot (directoryName, language) =
        let generate caseName code =
            let outputDirectory = Path.Combine(outputRoot, caseName + "-" + directoryName)
            Directory.CreateDirectory(outputDirectory) |> ignore
            Compile [language] outputDirectory "smoke" "1.0" code

        generate "text-writer" <| fun context ->
            context.io.fileOutput "result.txt" <| fun writer ->
                writer.t "hello\"\\%"
                writer.tt (st "value=" ++ int0(Int 7))

        generate "text-reader" <| fun context ->
            let value = context.var.i0 "value"
            value <== 9
            context.io.fileInput "input.txt" <| fun reader -> reader.t value
            context.print.t value

        generate "text-pair" <| fun context ->
            let first = context.var.i0 "first"
            let second = context.var.i0 "second"
            first <== 3
            second <== 4
            context.io.fileOutput "pair.txt" <| fun writer ->
                writer.tt (iv first ++ second)
            first <== 0
            second <== 0
            context.io.fileInput "pair.txt" <| fun reader ->
                reader.tt (iv first ++ second)
            context.print.t (first + second)

        generate "binary-reader" <| fun context ->
            let value = context.var.i0 "value"
            value <== 9
            context.io.binfileInput "input.bin" <| fun reader -> reader.b value
            context.print.t value

    let private generateSingularSolve outputRoot (directoryName, language) =
        let outputDirectory = Path.Combine(outputRoot, "singular-solve-" + directoryName)
        Directory.CreateDirectory(outputDirectory) |> ignore
        Compile [language] outputDirectory "smoke" "1.0" <| fun context ->
            let matrix = context.var.d2 "matrix"
            let rhs = context.var.d1 "rhs"
            matrix.allocate(2, 2)
            rhs.allocate 2
            for i in 0..1 do
                for j in 0..1 do matrix[i,j] <== 0.0
                rhs[i] <== 1.0
            context.la.solve_simuleq(matrix, rhs)
            context.print.t rhs[0]

    let private generateComplexSplineNonFinite outputRoot =
        let outputDirectory = Path.Combine(outputRoot, "spline-c-complex-non-finite-y")
        Directory.CreateDirectory(outputDirectory) |> ignore
        Compile [C99] outputDirectory "smoke" "1.0" <| fun context ->
            let spline = context.interpolate.splineComplex(true)
            spline.X.allocate 2
            spline.Y.allocate 2
            spline.X[0] <== 0.0
            spline.X[1] <== 1.0
            spline.Y[0] <== complex0(Cpx(0.0, 0.0))
            spline.Y[1] <== complex0(Cpx(1.0, Double.NaN))
            spline.set()

    let private generateRegressionCases outputRoot (directoryName, language) =
        let generate caseName code =
            let outputDirectory = Path.Combine(outputRoot, caseName + "-" + directoryName)
            Directory.CreateDirectory(outputDirectory) |> ignore
            Compile [language] outputDirectory "smoke" "1.0" code

        generate "read-byte" <| fun context ->
            let value = context.var.i0 "value"
            let sum = context.var.i0 "sum"
            sum <== 0
            context.io.fileInput "bytes.dat" <| fun reader ->
                for _ in 1..5 do
                    reader.b value
                    sum <== sum + value
            context.print.t sum

        generate "inverse-real" <| fun context ->
            let matrix = context.var.d2 "matrix"
            let inverse = context.var.d2 "inverse"
            matrix.allocate(2, 2)
            inverse.allocate(2, 2)
            matrix.clear()
            matrix[0,0] <== 2.0
            matrix[1,1] <== 4.0
            context.la.inverse_matrix(inverse,matrix)
            context.print.t (inverse[0,0] + inverse[1,1])

        generate "inverse-complex" <| fun context ->
            let matrix = context.var.z2 "matrix"
            let inverse = context.var.z2 "inverse"
            matrix.allocate(2, 2)
            inverse.allocate(2, 2)
            matrix.clear()
            matrix[0,0] <== complex0(Cpx(2.0, 0.0))
            matrix[1,1] <== complex0(Cpx(4.0, 0.0))
            context.la.inverse_matrix(inverse,matrix)
            context.print.t (inverse[0,0].re + inverse[1,1].re)

        generate "inverse-singular" <| fun context ->
            let matrix = context.var.d2 "matrix"
            let inverse = context.var.d2 "inverse"
            matrix.allocate(2, 2)
            inverse.allocate(2, 2)
            matrix.clear()
            context.la.inverse_matrix(inverse,matrix)

        generate "inverse-non-square" <| fun context ->
            let matrix = context.var.d2 "matrix"
            let inverse = context.var.d2 "inverse"
            matrix.allocate(2, 3)
            inverse.allocate(2, 2)
            context.la.inverse_matrix(inverse,matrix)

        generate "inverse-small-output" <| fun context ->
            let matrix = context.var.z2 "matrix"
            let inverse = context.var.z2 "inverse"
            matrix.allocate(2, 2)
            inverse.allocate(1, 1)
            context.la.inverse_matrix(inverse,matrix)

        generate "solve-short-rhs" <| fun context ->
            let matrix = context.var.d2 "matrix"
            let rhs = context.var.d1 "rhs"
            matrix.allocate(2, 2)
            rhs.allocate 1
            context.la.solve_simuleq(matrix,rhs)

        generate "solve-wrong-rhs-rows" <| fun context ->
            let matrix = context.var.z2 "matrix"
            let rhs = context.var.z2 "rhs"
            matrix.allocate(2, 2)
            rhs.allocate(1, 2)
            context.la.solve_simuleqs(matrix,rhs)

        generate "determinant-non-square" <| fun context ->
            let matrix = context.var.d2 "matrix"
            matrix.allocate(2, 3)
            context.la.determinant matrix <| fun result -> context.print.t result

        generate "determinant-complex-non-square" <| fun context ->
            let matrix = context.var.z2 "matrix"
            matrix.allocate(2, 3)
            context.la.determinant matrix <| fun result -> context.print.t result

        generate "rank-real" <| fun context ->
            let matrix = context.var.d2 "matrix"
            let rank = context.var.i0 "rank"
            matrix.allocate(2, 2)
            matrix.clear()
            matrix[0,0] <== 2.0
            matrix[1,1] <== 4.0
            context.la.rank(rank, matrix, double0(Dbl 1e-10))
            context.print.t rank

        generate "rank-complex" <| fun context ->
            let matrix = context.var.z2 "matrix"
            let rank = context.var.d0 "rank"
            matrix.allocate(2, 2)
            matrix.clear()
            matrix[0,0] <== complex0(Cpx(2.0, 0.0))
            matrix[1,1] <== complex0(Cpx(4.0, 0.0))
            context.la.rank(rank, matrix, double0(Dbl 1e-10))
            context.print.t rank

        let generateEigenStandard caseName (rows:int) (columns:int) (valueCount:int) (vectorOrder:int) =
            generate caseName <| fun context ->
                let matrix = context.var.z2 "matrix"
                let values = context.var.z1 "values"
                let vectors = context.var.z2 "vectors"
                matrix.allocate(rows, columns)
                values.allocate valueCount
                vectors.allocate(vectorOrder, vectorOrder)
                matrix.clear()
                matrix[0,0] <== complex0(Cpx(2.0, 0.0))
                matrix[1,1] <== complex0(Cpx(4.0, 0.0))
                context.la.eigen_matrix (values, vectors) matrix
                context.print.t values[0].re

        let generateEigenGeneralized caseName (secondOrder:int) (secondValueCount:int) =
            generate caseName <| fun context ->
                let matrix = context.var.z2 "matrix"
                let second = context.var.z2 "second"
                let alpha = context.var.z1 "alpha"
                let beta = context.var.z1 "beta"
                let vectors = context.var.z2 "vectors"
                matrix.allocate(2, 2)
                second.allocate(secondOrder, secondOrder)
                alpha.allocate 2
                beta.allocate secondValueCount
                vectors.allocate(2, 2)
                matrix.clear()
                second.clear()
                matrix[0,0] <== complex0(Cpx(2.0, 0.0))
                matrix[1,1] <== complex0(Cpx(4.0, 0.0))
                second[0,0] <== complex0(Cpx(1.0, 0.0))
                second[1,1] <== complex0(Cpx(1.0, 0.0))
                context.la.eigen_matrix2 (alpha, beta, vectors) matrix second
                context.print.t (alpha[0].re / beta[0].re)

        generateEigenStandard "eigen-standard" 2 2 2 2
        generateEigenStandard "eigen-standard-non-square" 2 3 2 2
        generateEigenGeneralized "eigen-generalized" 2 2
        generateEigenGeneralized "eigen-generalized-mismatch" 3 2

        if language = C99 || language = Fortran then
            generateEigenStandard "eigen-standard-short-values" 2 2 1 2
            generateEigenStandard "eigen-standard-small-vectors" 2 2 2 1
            generateEigenGeneralized "eigen-generalized-short-beta" 2 1

            generateEigenStandard "eigen-standard-info" 2 2 2 2
            generateEigenGeneralized "eigen-generalized-info" 2 2

        if language = C99 || language = Fortran then
            let generateSvd caseName (singularLength:int) (vtOrder:int) =
                generate caseName <| fun context ->
                    let matrix = context.var.d2 "matrix"
                    let u = context.var.d2 "u"
                    let singular = context.var.d1 "singular"
                    let vt = context.var.d2 "vt"
                    matrix.allocate(2, 2)
                    u.allocate(2, 2)
                    singular.allocate singularLength
                    vt.allocate(vtOrder, vtOrder)
                    matrix.clear()
                    matrix[0,0] <== 2.0
                    matrix[1,1] <== 4.0
                    context.la.svd matrix (u, singular, vt)
                    context.print.t singular[0]
            generateSvd "svd-real" 2 2
            generateSvd "svd-short-singular" 1 2
            generateSvd "svd-small-vt" 2 1

            generate "svd-complex" <| fun context ->
                let matrix = context.var.z2 "matrix"
                let u = context.var.z2 "u"
                let singular = context.var.d1 "singular"
                let vt = context.var.z2 "vt"
                matrix.allocate(2, 2)
                u.allocate(2, 2)
                singular.allocate 2
                vt.allocate(2, 2)
                matrix.clear()
                matrix[0,0] <== complex0(Cpx(2.0, 0.0))
                matrix[1,1] <== complex0(Cpx(4.0, 0.0))
                context.la.svd matrix (u, singular, vt)
                context.print.t singular[0]

        generate "spline-load" <| fun context ->
            let spline = context.interpolate.splineDouble()
            spline.load "data"
            let result = context.var.d0 "result"
            spline.p result (double0(Dbl 0.5))
            context.print.t result

        generate "persistence-invalid-version" <| fun context ->
            let value = context.var.i0 "value"
            value <== 9
            context.io.load(value,"data.bin")
            context.print.t value

        generate "persistence-array" <| fun context ->
            let value = context.var.i1 "value"
            context.io.load(value,"data.bin")
            context.print.t value[0]

        generate "persistence-empty-array" <| fun context ->
            let value = context.var.i1 "value"
            context.io.load(value,"data.bin")
            context.print.t value.size1

        generate "persistence-tensor" <| fun context ->
            let value = context.var.i3 "value"
            context.io.load(value,"data.bin")
            context.print.t value[0,0,0]

    [<EntryPoint>]
    let main arguments =
        match arguments with
        |[| outputRoot |] ->
            let outputRoot = Path.GetFullPath(outputRoot)
            Directory.CreateDirectory(outputRoot) |> ignore
            generationTargets |> List.iter (generate outputRoot)
            generatePythonSciPy outputRoot
            generateDistributedC outputRoot
            generateLeadingHyphenC outputRoot
            generateLeadingHyphenFortran outputRoot
            generatePhpTextValidation outputRoot
            generationTargets |> List.iter (generateOutputEscaping outputRoot)
            ["c", C99; "fortran", Fortran; "python", Python]
            |> List.iter (generateTextRead outputRoot)
            for target in ["c", C99; "fortran", Fortran; "python", Python] do
                generateFixedFileIo outputRoot target
                generateSingularSolve outputRoot target
                generateRegressionCases outputRoot target
                for caseName in ["valid"; "literal"; "unordered"; "out-of-range"] do
                    generateSplineValidation outputRoot target caseName
            generateSplineValidation outputRoot ("c", C99) "non-finite-y"
            generateComplexSplineNonFinite outputRoot
            generateCArrayCases outputRoot
            generatePhpUploads outputRoot
            printfn "Generated runtime smoke programs in %s" outputRoot
            0
        |_ ->
            eprintfn "Usage: Aqualis.GeneratedCodeSmoke <output-directory>"
            2
