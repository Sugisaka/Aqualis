namespace Aqualis.Tests

open System
open System.IO
open Xunit
open Aqualis

module PhpArrayGenerationTests =
    [<Fact>]
    let ``PHP initialized numeric arrays emit their values and sizes`` () =
        use output = new TemporaryDirectory()
        let project = "php-initialized-arrays"

        Compile [PHP] output.Path project "1.0" <| fun context ->
            let integers = context.var.ip1("integers", [7; 8])
            let reals = context.var.dp1("reals", [2.5])
            context.print.t integers[1]
            context.print.t reals[0]

        let generated = File.ReadAllText(Path.Combine(output.Path, project + ".php"))
        Assert.Contains("<?php $integers = [7,8]; ?>", generated)
        Assert.Contains("<?php $integers_size = [2]; ?>", generated)
        Assert.Contains("<?php $reals = [", generated)
        Assert.Contains("<?php $reals_size = [1]; ?>", generated)

    [<Fact>]
    let ``PHP initialized complex arrays fail explicitly`` () =
        use output = new TemporaryDirectory()
        Assert.Throws<NotSupportedException>(fun () ->
            Compile [PHP] output.Path "php-complex-array" "1.0" <| fun context ->
                context.var.zp1("values", [(1.0, 2.0)]) |> ignore)
        |> ignore

    [<Fact>]
    let ``PHP duplicate initialized arrays initialize unvisited branches without resetting existing values`` () =
        use output = new TemporaryDirectory()
        let project = "php-initialized-branches"

        Compile [PHP] output.Path project "1.0" <| fun context ->
            let flag = context.var.i0 "flag"
            flag <== 0
            context.br.if2 (flag .= 1)
                (fun () -> context.var.ip1("values", [7]) |> ignore)
                (fun () ->
                    let values = context.var.ip1("values", [7])
                    context.print.t values[0])

        let generated = File.ReadAllText(Path.Combine(output.Path, project + ".php"))
        Assert.Contains("if (!isset($values)) { $values = [7]; $values_size = [1]; }", generated)

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
