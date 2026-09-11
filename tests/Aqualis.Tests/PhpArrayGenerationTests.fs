namespace Aqualis.Tests

open System.IO
open Xunit
open Aqualis

module PhpArrayGenerationTests =
    [<Fact>]
    let ``PHP dynamic arrays allocate and deallocate inside PHP blocks`` () =
        use output = new TemporaryDirectory()
        let project = "php-dynamic-arrays"

        Compile [PHP] output.Path project "1.0" <| fun context ->
            let vector = context.var.i1 "vector"
            let matrix = context.var.d2 "matrix"
            let tensor = context.var.z3 "tensor"

            vector.allocate 2
            matrix.allocate(2,3)
            tensor.allocate(2,3,4)
            vector.deallocate()
            matrix.deallocate()
            tensor.deallocate()

        let generated = File.ReadAllText(Path.Combine(output.Path, project + ".php"))
        let generatedLines =
            generated.Split('\n')
            |> Array.map (fun line -> line.TrimEnd('\r').Trim())

        for name in ["$vector"; "$matrix"; "$tensor"] do
            Assert.Contains("<?php " + name + " = []; ?>", generated)
            Assert.Contains("<?php unset(" + name + "); ?>", generated)
            Assert.DoesNotContain(name + " = [];", generatedLines)
            Assert.DoesNotContain("unset(" + name + ");", generatedLines)

    [<Fact>]
    let ``PHP multidimensional array deallocation resets every dimension`` () =
        use output = new TemporaryDirectory()
        let project = "php-array-sizes"

        Compile [PHP] output.Path project "1.0" <| fun context ->
            let matrix = context.var.i2 "matrix"
            let tensor = context.var.d3 "tensor"

            matrix.allocate(2,3)
            tensor.allocate(2,3,4)
            matrix.deallocate()
            tensor.deallocate()

        let generated = File.ReadAllText(Path.Combine(output.Path, project + ".php"))

        for statement in
            [ "$matrix_size[0] = -1;"
              "$matrix_size[1] = -1;"
              "$tensor_size[0] = -1;"
              "$tensor_size[1] = -1;"
              "$tensor_size[2] = -1;" ] do
            Assert.Contains("<?php " + statement + " ?>", generated)
