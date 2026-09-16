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

    let private generateBiCgStabCases outputRoot (directoryName, language) =
        let generate caseName code =
            let outputDirectory = Path.Combine(outputRoot, "bicgstab-" + caseName + "-" + directoryName)
            Directory.CreateDirectory(outputDirectory) |> ignore
            Compile [language] outputDirectory "smoke" "1.0" code

        generate "identity" <| fun context ->
            let rhs = context.var.z1("rhs", 1)
            let solution = context.var.z1("solution", 1)
            rhs[0] <== 1
            solution[0] <== 0
            simuleq.BiCGSTAB context rhs solution 1e-8 10
                (fun (result,input) -> result[0] <== input[0]) None
            context.print.t (asm.abs solution[0])

        for caseName, magnitude in ["large", 1e200; "small", 1e-200] do
            generate caseName <| fun context ->
                let rhs = context.var.z1("rhs", 1)
                let solution = context.var.z1("solution", 1)
                rhs[0] <== magnitude
                solution[0] <== 0
                simuleq.BiCGSTAB context rhs solution 1e-8 10
                    (fun (result,input) -> result[0] <== input[0]) None
                context.print.t (asm.abs solution[0])

        generate "residual-cancellation" <| fun context ->
            let rhs = context.var.z1("rhs", 1)
            let solution = context.var.z1("solution", 1)
            rhs[0] <== 1e308
            solution[0] <== -1e308
            simuleq.BiCGSTAB context rhs solution 1e-8 10
                (fun (result,input) -> result[0] <== input[0]) None
            context.print.t (asm.abs solution[0])

        generate "solution-overflow" <| fun context ->
            let rhs = context.var.z1("rhs", 1)
            let solution = context.var.z1("solution", 1)
            rhs[0] <== 1e200
            solution[0] <== 0
            simuleq.BiCGSTAB context rhs solution 1e-8 10
                (fun (result,input) -> result[0] <== 1e-200 * input[0]) None

        generate "breakdown" <| fun context ->
            let rhs = context.var.z1("rhs", 1)
            let solution = context.var.z1("solution", 1)
            rhs[0] <== 1
            solution[0] <== 0
            simuleq.BiCGSTAB context rhs solution 1e-8 10
                (fun (result,_) -> result[0] <== 0) None

        generate "diagonal" <| fun context ->
            let rhs = context.var.z1("rhs", 2)
            let solution = context.var.z1("solution", 2)
            rhs[0] <== 1
            rhs[1] <== 1
            solution[0] <== 0
            solution[1] <== 0
            simuleq.BiCGSTAB context rhs solution 1e-12 10
                (fun (result,input) ->
                    result[0] <== input[0]
                    result[1] <== 2 * input[1]) None
            context.print.t (asm.abs solution[1])

        generate "limit" <| fun context ->
            let rhs = context.var.z1("rhs", 2)
            let solution = context.var.z1("solution", 2)
            rhs[0] <== 1
            rhs[1] <== 1
            solution[0] <== 0
            solution[1] <== 0
            simuleq.BiCGSTAB context rhs solution 1e-12 1
                (fun (result,input) ->
                    result[0] <== input[0]
                    result[1] <== 2 * input[1]) None

    let private generateAssociatedLegendreCases outputRoot (directoryName, language) =
        let generate caseName l m code =
            let outputDirectory = Path.Combine(outputRoot, "legendre-" + caseName + "-" + directoryName)
            Directory.CreateDirectory(outputDirectory) |> ignore
            Compile [language] outputDirectory "smoke" "1.0" <| fun context ->
                let result = context.var.d0 "result"
                context.math.aplgndr result (I l, I m, D 0.5)
                code context result

        generate "valid" 2 0 (fun context result -> context.print.t result)
        generate "invalid" 0 1 (fun _ _ -> ())

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

        generate "array-shape-vector" <| fun context ->
            let target = context.var.i1 "target"
            let source = context.var.i1 "source"
            target.allocate 2
            source.allocate 1
            target <== source

        generate "array-shape-matrix" <| fun context ->
            let target = context.var.i2 "target"
            let source = context.var.i2 "source"
            target.allocate(2, 2)
            source.allocate(2, 1)
            target <== source

        generate "array-shape-tensor" <| fun context ->
            let target = context.var.i3 "target"
            let source = context.var.i3 "source"
            target.allocate(2, 2, 2)
            source.allocate(2, 2, 1)
            target <== source

        generate "array-shape-expression" <| fun context ->
            let target = context.var.i1 "target"
            let source = context.var.i1 "source"
            target.allocate 2
            source.allocate 1
            let result = target + source
            target <== result

        generate "minmax-empty-vector" <| fun context ->
            let source = context.var.i1 "source"
            let result = context.var.i0 "result"
            asm.max(source, result, None)

        generate "minmax-empty-matrix" <| fun context ->
            let source = context.var.d2 "source"
            let result = context.var.d0 "result"
            asm.min(source, result, None, None)

        generate "minmax-empty-tensor" <| fun context ->
            let source = context.var.i3 "source"
            let result = context.var.i0 "result"
            asm.max(source, result, None, None, None)

        generate "linear-range-real" <| fun context ->
            let result = context.var.d0 "result"
            result <== 99.0
            let interpolation = context.interpolate.linearDouble("sample", [0.0; 1.0], [0.0; 10.0])
            interpolation.y (double0(Dbl 2.0)) (fun value -> result <== value)
            context.print.t result

        generate "linear-valid-real" <| fun context ->
            let result = context.var.d0 "result"
            let interpolation = context.interpolate.linearDouble("sample", [0.0; 1.0], [0.0; 10.0])
            interpolation.y (double0(Dbl 0.5)) (fun value -> result <== value)
            context.print.t result

        generate "linear-range-complex" <| fun context ->
            let result = context.var.z0 "result"
            let interpolation = context.interpolate.linearComplex("sample", [0.0; 1.0], [(0.0, 0.0); (10.0, 1.0)])
            interpolation.y (double0(Dbl 2.0)) (fun value -> result <== value)
            context.print.t result

        generate "read-byte" <| fun context ->
            let value = context.var.i0 "value"
            let sum = context.var.i0 "sum"
            sum <== 0
            context.io.fileInput "bytes.dat" <| fun reader ->
                for _ in 1..5 do
                    reader.b value
                    sum <== sum + value
            context.print.t sum

        generate "matvec-alias" <| fun context ->
            let matrix = context.var.d2 "matrix"
            let vector = context.var.d1 "vector"
            matrix.allocate(2, 2)
            vector.allocate 2
            matrix.clear()
            matrix[0,0] <== 2.0
            matrix[1,1] <== 3.0
            vector[0] <== 3.0
            vector[1] <== 4.0
            context.la.matmul(vector, matrix, vector)
            context.print.t (vector[0] + vector[1])

        generate "matmul-alias" <| fun context ->
            let left = context.var.d2 "left"
            let right = context.var.d2 "right"
            left.allocate(2, 2)
            right.allocate(2, 2)
            left.clear()
            right.clear()
            left[0,0] <== 2.0
            left[1,1] <== 3.0
            right[0,0] <== 4.0
            right[1,1] <== 5.0
            context.la.matmul(left, left, right)
            context.print.t (left[0,0] + left[1,1])

        generate "matvec-short-input" <| fun context ->
            let matrix = context.var.d2 "matrix"
            let vector = context.var.d1 "vector"
            let output = context.var.d1 "output"
            matrix.allocate(2, 3)
            vector.allocate 2
            output.allocate 2
            context.la.matmul(output, matrix, vector)

        generate "matvec-short-output" <| fun context ->
            let matrix = context.var.d2 "matrix"
            let vector = context.var.d1 "vector"
            let output = context.var.d1 "output"
            matrix.allocate(2, 2)
            vector.allocate 2
            output.allocate 1
            context.la.matmul(output, matrix, vector)

        generate "matmul-inner-mismatch" <| fun context ->
            let left = context.var.d2 "left"
            let right = context.var.d2 "right"
            let output = context.var.d2 "output"
            left.allocate(2, 3)
            right.allocate(2, 2)
            output.allocate(2, 2)
            context.la.matmul(output, left, right)

        generate "matmul-small-output" <| fun context ->
            let left = context.var.d2 "left"
            let right = context.var.d2 "right"
            let output = context.var.d2 "output"
            left.allocate(2, 2)
            right.allocate(2, 2)
            output.allocate(1, 2)
            context.la.matmul(output, left, right)

        generate "dot-length-mismatch" <| fun context ->
            let left = context.var.d1 "left"
            let right = context.var.d1 "right"
            let output = context.var.d0 "output"
            left.allocate 2
            right.allocate 1
            context.la.dot(output, left, right)

        generate "dot-alias-real" <| fun context ->
            let left = context.var.d1 "left"
            let right = context.var.d1 "right"
            left.allocate 2
            right.allocate 2
            left[0] <== 2.0
            left[1] <== 3.0
            right[0] <== 4.0
            right[1] <== 5.0
            context.la.dot(left[0], left, right)
            context.print.t left[0]

        generate "dot-alias-complex" <| fun context ->
            let left = context.var.z1 "left"
            let right = context.var.z1 "right"
            left.allocate 2
            right.allocate 2
            left[0] <== complex0(Cpx(2.0, 0.0))
            left[1] <== complex0(Cpx(3.0, 0.0))
            right[0] <== complex0(Cpx(4.0, 0.0))
            right[1] <== complex0(Cpx(5.0, 0.0))
            context.la.dot(left[0], left, right)
            context.print.t left[0].re

        generate "norm-large" <| fun context ->
            let vector = context.var.d1 "vector"
            vector.allocate 2
            vector[0] <== 1e200
            vector[1] <== 1e200
            context.la.norm vector <| fun value -> context.print.t (value / 1e200)

        generate "norm-small" <| fun context ->
            let vector = context.var.d1 "vector"
            vector.allocate 2
            vector[0] <== 1e-200
            vector[1] <== 1e-200
            context.la.norm vector <| fun value -> context.print.t (value / 1e-200)

        generate "normalize-large" <| fun context ->
            let vector = context.var.d1 "vector"
            vector.allocate 2
            vector[0] <== 1e308
            vector[1] <== 1e308
            context.la.normalize vector
            context.print.t vector[0]

        generate "normalize-small-complex" <| fun context ->
            let vector = context.var.z1 "vector"
            vector.allocate 1
            vector[0] <== complex0(Cpx(1e-200, 1e-200))
            context.la.normalize vector
            context.print.t vector[0].re

        generate "findmin-short-direction" <| fun context ->
            let initial = context.var.d1 "initial"
            let direction = context.var.d1 "direction"
            let output = context.var.d1 "output"
            initial.allocate 2
            direction.allocate 1
            output.allocate 2
            context.optimization.findmin 1 (initial, direction) (D 1.0)
                (fun value point -> value <== point[0] * point[0]) output

        generate "findmin-short-output" <| fun context ->
            let initial = context.var.d1 "initial"
            let direction = context.var.d1 "direction"
            let output = context.var.d1 "output"
            initial.allocate 2
            direction.allocate 2
            output.allocate 1
            context.optimization.findmin 1 (initial, direction) (D 1.0)
                (fun value point -> value <== point[0] * point[0]) output

        for caseName, directionValue in ["findmin-large-direction", 1e200; "findmin-small-direction", 1e-200] do
            generate caseName <| fun context ->
                let initial = context.var.d1 "initial"
                let direction = context.var.d1 "direction"
                let output = context.var.d1 "output"
                initial.allocate 1
                direction.allocate 1
                output.allocate 1
                initial[0] <== 0.0
                direction[0] <== directionValue
                context.optimization.findmin 0 (initial, direction) (D 1.0)
                    (fun value point -> value <== (point[0] - 1.0) * (point[0] - 1.0)) output
                context.print.t output[0]

        generate "findmin-huge-direction" <| fun context ->
            let initial = context.var.d1 "initial"
            let direction = context.var.d1 "direction"
            let output = context.var.d1 "output"
            initial.allocate 2
            direction.allocate 2
            output.allocate 2
            initial.clear()
            direction[0] <== 1e308
            direction[1] <== 1e308
            context.optimization.findmin 0 (initial, direction) (D 1.0)
                (fun value point -> value <== (point[0] - 1.0) * (point[0] - 1.0)) output
            context.print.t output[0]

        for caseName, nonfinite in ["findmin-nan-objective", Double.NaN; "findmin-infinite-objective", Double.PositiveInfinity] do
            generate caseName <| fun context ->
                let initial = context.var.d1("initial", 1)
                let direction = context.var.d1("direction", 1)
                let output = context.var.d1("output", 1)
                initial[0] <== 0.0
                direction[0] <== 1.0
                context.optimization.findmin 1 (initial,direction) (D 1.0)
                    (fun value _ -> value <== nonfinite) output

        for caseName, initialValue, directionValue, stepWidth in
            ["findmin-nan-initial", Double.NaN, 1.0, 1.0;
             "findmin-nan-direction", 0.0, Double.NaN, 1.0;
             "findmin-nan-step", 0.0, 1.0, Double.NaN] do
            generate caseName <| fun context ->
                let initial = context.var.d1("initial", 1)
                let direction = context.var.d1("direction", 1)
                let output = context.var.d1("output", 1)
                initial[0] <== initialValue
                direction[0] <== directionValue
                context.optimization.findmin 0 (initial,direction) (D stepWidth)
                    (fun value _ -> value <== 0.0) output

        generate "normalize-zero" <| fun context ->
            let vector = context.var.d1 "vector"
            vector.allocate 2
            vector.clear()
            context.la.normalize vector

        generate "normalize-complex-zero" <| fun context ->
            let vector = context.var.z1 "vector"
            vector.allocate 2
            vector.clear()
            context.la.normalize vector

        generate "pseudoinverse-zero-real" <| fun context ->
            let matrix = context.var.d2 "matrix"
            let inverse = context.var.d2 "inverse"
            matrix.allocate(2, 2)
            inverse.allocate(2, 2)
            matrix.clear()
            context.la.inverse_matrix2(inverse, matrix, double0(Dbl 1e-10))
            context.print.t (inverse[0,0] + inverse[1,1])

        generate "pseudoinverse-zero-complex" <| fun context ->
            let matrix = context.var.z2 "matrix"
            let inverse = context.var.z2 "inverse"
            matrix.allocate(2, 2)
            inverse.allocate(2, 2)
            matrix.clear()
            context.la.inverse_matrix2(inverse, matrix, double0(Dbl 1e-10))
            context.print.t (inverse[0,0].re + inverse[1,1].re)

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

        generate "inverse-alias-real" <| fun context ->
            let matrix = context.var.d2 "matrix"
            matrix.allocate(2, 2)
            matrix.clear()
            matrix[0,0] <== 2.0
            matrix[1,1] <== 4.0
            context.la.inverse_matrix(matrix,matrix)
            context.print.t (matrix[0,0] + matrix[1,1])

        generate "inverse-alias-complex" <| fun context ->
            let matrix = context.var.z2 "matrix"
            matrix.allocate(2, 2)
            matrix.clear()
            matrix[0,0] <== complex0(Cpx(2.0, 0.0))
            matrix[1,1] <== complex0(Cpx(4.0, 0.0))
            context.la.inverse_matrix(matrix,matrix)
            context.print.t (matrix[0,0].re + matrix[1,1].re)

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

        generate "svd-alias-real" <| fun context ->
            let matrix = context.var.d2 "matrix"
            let singular = context.var.d1 "singular"
            let vt = context.var.d2 "vt"
            matrix.allocate(2, 2)
            singular.allocate 2
            vt.allocate(2, 2)
            matrix.clear()
            matrix[0,0] <== 2.0
            matrix[1,1] <== 4.0
            context.la.svd matrix (matrix,singular,vt)
            context.print.t singular[0]

        generate "svd-alias-complex" <| fun context ->
            let matrix = context.var.z2 "matrix"
            let singular = context.var.d1 "singular"
            let vt = context.var.z2 "vt"
            matrix.allocate(2, 2)
            singular.allocate 2
            vt.allocate(2, 2)
            matrix.clear()
            matrix[0,0] <== complex0(Cpx(2.0, 0.0))
            matrix[1,1] <== complex0(Cpx(4.0, 0.0))
            context.la.svd matrix (matrix,singular,vt)
            context.print.t singular[0]

        generate "svd-alias-vt-real" <| fun context ->
            let matrix = context.var.d2 "matrix"
            let u = context.var.d2 "u"
            let singular = context.var.d1 "singular"
            matrix.allocate(2, 2)
            u.allocate(2, 2)
            singular.allocate 2
            matrix.clear()
            matrix[0,0] <== 2.0
            matrix[1,1] <== 4.0
            context.la.svd matrix (u,singular,matrix)
            context.print.t singular[0]

        generate "svd-alias-vt-complex" <| fun context ->
            let matrix = context.var.z2 "matrix"
            let u = context.var.z2 "u"
            let singular = context.var.d1 "singular"
            matrix.allocate(2, 2)
            u.allocate(2, 2)
            singular.allocate 2
            matrix.clear()
            matrix[0,0] <== complex0(Cpx(2.0, 0.0))
            matrix[1,1] <== complex0(Cpx(4.0, 0.0))
            context.la.svd matrix (u,singular,matrix)
            context.print.t singular[0]

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

        generate "determinant-regular-real" <| fun context ->
            let matrix = context.var.d2 "matrix"
            matrix.allocate(2, 2)
            matrix.clear()
            matrix[0,0] <== 2.0
            matrix[1,1] <== 5.0
            context.la.determinant matrix <| fun result -> context.print.t result

        generate "determinant-regular-complex" <| fun context ->
            let matrix = context.var.z2 "matrix"
            matrix.allocate(2, 2)
            matrix.clear()
            matrix[0,0] <== complex0(Cpx(2.0, 0.0))
            matrix[1,1] <== complex0(Cpx(5.0, 0.0))
            context.la.determinant matrix <| fun result -> context.print.t result

        generate "determinant-singular-real" <| fun context ->
            let matrix = context.var.d2 "matrix"
            matrix.allocate(2, 2)
            matrix.clear()
            matrix[0,0] <== 2.0
            context.la.determinant matrix <| fun result -> context.print.t result

        generate "determinant-singular-complex" <| fun context ->
            let matrix = context.var.z2 "matrix"
            matrix.allocate(2, 2)
            matrix.clear()
            matrix[0,0] <== complex0(Cpx(2.0, 0.0))
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

        generate "homogeneous-real" <| fun context ->
            let matrix = context.var.d2 "matrix"
            let solution = context.var.d1 "solution"
            matrix.allocate(2, 2)
            solution.allocate 2
            matrix.clear()
            matrix[0,0] <== 1.0
            context.la.solve_homogeneq(matrix, solution)
            context.print.t (asm.abs solution[1])

        generate "homogeneous-complex-wide" <| fun context ->
            let matrix = context.var.z2 "matrix"
            let solution = context.var.z1 "solution"
            matrix.allocate(2, 3)
            solution.allocate 3
            matrix.clear()
            matrix[0,0] <== complex0(Cpx(1.0, 0.0))
            matrix[1,1] <== complex0(Cpx(1.0, 0.0))
            context.la.solve_homogeneq(matrix, solution)
            context.print.t (asm.abs solution[2])

        generate "homogeneous-short-output" <| fun context ->
            let matrix = context.var.d2 "matrix"
            let solution = context.var.d1 "solution"
            matrix.allocate(2, 2)
            solution.allocate 1
            context.la.solve_homogeneq(matrix, solution)

        generate "pseudoinverse-real" <| fun context ->
            let matrix = context.var.d2 "matrix"
            let inverse = context.var.d2 "inverse"
            matrix.allocate(2, 3)
            inverse.allocate(3, 2)
            matrix.clear()
            matrix[0,0] <== 1.0
            matrix[1,1] <== 2.0
            context.la.inverse_matrix2(inverse, matrix, double0(Dbl 1e-10))
            context.print.t (inverse[0,0] + inverse[1,1])

        generate "pseudoinverse-complex" <| fun context ->
            let matrix = context.var.z2 "matrix"
            let inverse = context.var.z2 "inverse"
            matrix.allocate(2, 2)
            inverse.allocate(2, 2)
            matrix.clear()
            matrix[0,0] <== complex0(Cpx(1.0, 1.0))
            matrix[1,1] <== complex0(Cpx(2.0, 0.0))
            context.la.inverse_matrix2(inverse, matrix, double0(Dbl 1e-10))
            context.print.t inverse[0,0].im

        generate "pseudoinverse-small-output" <| fun context ->
            let matrix = context.var.d2 "matrix"
            let inverse = context.var.d2 "inverse"
            matrix.allocate(2, 3)
            inverse.allocate(1, 1)
            context.la.inverse_matrix2(inverse, matrix, double0(Dbl 1e-10))

        generate "tikhonov-complex" <| fun context ->
            let matrix = context.var.z2 "matrix"
            let rhs = context.var.z1 "rhs"
            matrix.allocate(2, 1)
            rhs.allocate 2
            matrix.clear()
            rhs.clear()
            matrix[0,0] <== complex0(Cpx(1.0, 1.0))
            matrix[1,0] <== complex0(Cpx(1.0, 0.0))
            rhs[0] <== complex0(Cpx(1.0, 0.0))
            context.la.solve_simuleq_t(matrix, rhs) <| fun solution ->
                context.print.t solution[0].re
                context.print.t solution[0].im

        generate "tikhonov-column" <| fun context ->
            let matrix = context.var.z2 "matrix"
            let rhs = context.var.z2 "rhs"
            matrix.allocate(2, 1)
            rhs.allocate(2, 1)
            matrix.clear()
            rhs.clear()
            matrix[0,0] <== complex0(Cpx(1.0, 0.0))
            rhs[0,0] <== complex0(Cpx(1.0, 0.0))
            context.la.solve_simuleq_tt2(matrix, rhs, double0(Dbl 1e-6)) <| fun solution ->
                context.print.t solution[0].re

        generate "tikhonov-short-rhs" <| fun context ->
            let matrix = context.var.d2 "matrix"
            let rhs = context.var.d1 "rhs"
            matrix.allocate(2, 1)
            rhs.allocate 1
            context.la.solve_simuleq_t(matrix, rhs) ignore

        generate "tikhonov-wide-rhs" <| fun context ->
            let matrix = context.var.z2 "matrix"
            let rhs = context.var.z2 "rhs"
            matrix.allocate(2, 1)
            rhs.allocate(2, 2)
            context.la.solve_simuleq_tt2(matrix, rhs, double0(Dbl 1e-6)) ignore

        generate "tikhonov-overflow-real" <| fun context ->
            let matrix = context.var.d2 "matrix"
            let rhs = context.var.d1 "rhs"
            matrix.allocate(1, 1)
            rhs.allocate 1
            matrix[0,0] <== 1e200
            rhs[0] <== 1.0
            context.la.solve_simuleq_tt(matrix, rhs, 1.0) ignore

        generate "tikhonov-overflow-complex" <| fun context ->
            let matrix = context.var.z2 "matrix"
            let rhs = context.var.z1 "rhs"
            matrix.allocate(1, 1)
            rhs.allocate 1
            matrix[0,0] <== complex0(Cpx(1e200, 0.0))
            rhs[0] <== complex0(Cpx(1.0, 0.0))
            context.la.solve_simuleq_tt(matrix, rhs, 1.0) ignore

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

        if language = C99 || language = Fortran || language = Python then
            generate "fft1-short-output" <| fun context ->
                let input = context.var.z1 "input"
                let output = context.var.z1 "output"
                input.allocate 4
                output.allocate 2
                context.fft1.fft("forwardPlan",input,output)

            generate "fft2-short-output" <| fun context ->
                let input = context.var.z2 "input"
                let output = context.var.z2 "output"
                input.allocate(2, 2)
                output.allocate(2, 1)
                context.fft2.fft("forwardPlan",input,output)

            generate "fft1-empty-input" <| fun context ->
                let input = context.var.z1 "input"
                let output = context.var.z1 "output"
                input.allocate 0
                output.allocate 0
                context.fft1.fft("forwardPlan",input,output)

            generate "ifftshift2-single-row" <| fun context ->
                let matrix = context.var.z2 "matrix"
                matrix.allocate(1, 2)
                matrix[0,0] <== complex0(Cpx(1.0, 0.0))
                matrix[0,1] <== complex0(Cpx(2.0, 0.0))
                fft2.ifftshift2 context matrix
                context.print.t (10.0 * matrix[0,0].re + matrix[0,1].re)

            generate "fft1-roundtrip" <| fun context ->
                let input = context.var.z1 "input"
                let transformed = context.var.z1 "transformed"
                let recovered = context.var.z1 "recovered"
                let forwardValue = context.var.d0 "forwardValue"
                input.allocate 4
                transformed.allocate 4
                recovered.allocate 4
                input.clear()
                input[2] <== complex0(Cpx(4.0, 0.0))
                context.fft1.fft("forwardPlan",input,transformed)
                forwardValue <== transformed[0].re
                context.fft1.ifft("inversePlan",transformed,recovered)
                context.print.t (forwardValue + recovered[2].re)

            generate "fft2-roundtrip" <| fun context ->
                let input = context.var.z2 "input"
                let transformed = context.var.z2 "transformed"
                let recovered = context.var.z2 "recovered"
                let forwardMagnitude = context.var.d0 "forwardMagnitude"
                input.allocate(2, 3)
                transformed.allocate(2, 3)
                recovered.allocate(2, 3)
                input.clear()
                input[1,1] <== complex0(Cpx(6.0, 0.0))
                context.fft2.fft("forwardPlan",input,transformed)
                forwardMagnitude <== asm.abs(transformed[0,0])
                context.fft2.ifft("inversePlan",transformed,recovered)
                context.print.t (forwardMagnitude + recovered[1,1].re)

            generate "fft2-coefficients" <| fun context ->
                let input = context.var.z2 "input"
                let transformed = context.var.z2 "transformed"
                input.allocate(2, 3)
                transformed.allocate(2, 3)
                for i in 0..1 do
                    for j in 0..2 do
                        input[i,j] <== complex0(Cpx(float (1+i*3+j), float (i-j)))
                context.fft2.fft("forwardPlan",input,transformed)
                for i in 0..1 do
                    for j in 0..2 do
                        context.print.t transformed[i,j].re
                        context.print.t transformed[i,j].im

        generateEigenStandard "eigen-standard-short-values" 2 2 1 2
        generateEigenStandard "eigen-standard-small-vectors" 2 2 2 1
        generateEigenGeneralized "eigen-generalized-short-beta" 2 1

        if language = C99 || language = Fortran then
            generateEigenStandard "eigen-standard-info" 2 2 2 2
            generateEigenGeneralized "eigen-generalized-info" 2 2

        let generateSvd caseName (uOrder:int) (singularLength:int) (vtOrder:int) =
            generate caseName <| fun context ->
                let matrix = context.var.d2 "matrix"
                let u = context.var.d2 "u"
                let singular = context.var.d1 "singular"
                let vt = context.var.d2 "vt"
                matrix.allocate(2, 2)
                u.allocate(uOrder, uOrder)
                singular.allocate singularLength
                vt.allocate(vtOrder, vtOrder)
                matrix.clear()
                matrix[0,0] <== 2.0
                matrix[1,1] <== 4.0
                context.la.svd matrix (u, singular, vt)
                context.print.t singular[0]
        generateSvd "svd-real" 2 2 2
        generateSvd "svd-small-u" 1 2 2
        generateSvd "svd-short-singular" 2 1 2
        generateSvd "svd-small-vt" 2 2 1

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

    let private generateWebProductValidation outputRoot (directoryName, language) =
        let outputDirectory = Path.Combine(outputRoot, "dot-length-mismatch-" + directoryName)
        Directory.CreateDirectory(outputDirectory) |> ignore
        Compile [language] outputDirectory "smoke" "1.0" <| fun context ->
            let left = context.var.d1 "left"
            let right = context.var.d1 "right"
            let output = context.var.d0 "output"
            left.allocate 2
            right.allocate 1
            context.la.dot(output, left, right)

    let private generateWebArrayValidation outputRoot (directoryName, language) =
        let outputDirectory = Path.Combine(outputRoot, "array-shape-vector-" + directoryName)
        Directory.CreateDirectory(outputDirectory) |> ignore
        Compile [language] outputDirectory "smoke" "1.0" <| fun context ->
            let target = context.var.i1 "target"
            let source = context.var.i1 "source"
            target.allocate 2
            source.allocate 1
            target <== source

    let private generatePhpInitializedArrays outputRoot =
        let generate caseName code =
            let outputDirectory = Path.Combine(outputRoot, "php-initialized-" + caseName)
            Directory.CreateDirectory(outputDirectory) |> ignore
            Compile [PHP] outputDirectory "smoke" "1.0" code

        generate "integer" <| fun context ->
            let values = context.var.ip1("values", [3; 7])
            context.print.t values[1]

        generate "real" <| fun context ->
            let values = context.var.dp1("values", [1.25; 2.5])
            context.print.t values[1]

        generate "duplicate" <| fun context ->
            let values = context.var.ip1("values", [1])
            values[0] <== 5
            let duplicate = context.var.ip1("values", [1])
            context.print.t duplicate[0]

        generate "integer-branch" <| fun context ->
            let flag = context.var.i0 "flag"
            flag <== 0
            context.br.if2 (flag .= 1)
                (fun () -> context.var.ip1("values", [7]) |> ignore)
                (fun () ->
                    let values = context.var.ip1("values", [7])
                    context.print.t values[0])

        generate "real-branch" <| fun context ->
            let flag = context.var.i0 "flag"
            flag <== 0
            context.br.if2 (flag .= 1)
                (fun () -> context.var.dp1("values", [2.5]) |> ignore)
                (fun () ->
                    let values = context.var.dp1("values", [2.5])
                    context.print.t values[0])

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
                generateBiCgStabCases outputRoot target
                generateAssociatedLegendreCases outputRoot target
                generateRegressionCases outputRoot target
                for caseName in ["valid"; "literal"; "unordered"; "out-of-range"] do
                    generateSplineValidation outputRoot target caseName
            ["javascript", JavaScript; "php", PHP]
            |> List.iter (generateWebProductValidation outputRoot)
            ["javascript", JavaScript; "php", PHP]
            |> List.iter (generateWebArrayValidation outputRoot)
            generatePhpInitializedArrays outputRoot
            generateSplineValidation outputRoot ("c", C99) "non-finite-y"
            generateComplexSplineNonFinite outputRoot
            generateCArrayCases outputRoot
            generatePhpUploads outputRoot
            printfn "Generated runtime smoke programs in %s" outputRoot
            0
        |_ ->
            eprintfn "Usage: Aqualis.GeneratedCodeSmoke <output-directory>"
            2
