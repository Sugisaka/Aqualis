namespace Aqualis.Tests

open System
open Xunit
open Aqualis

module AssignmentTests =
    [<Fact>]
    let ``scalar assignment writes through the left hand context`` () =
        use output = new TemporaryDirectory()

        makeProgramWithContext
            [output.Path, "assignment.c", C99]
            (fun context ->
                let value = var.i0 "value"
                value <== 42
                context.CurrentProgram.close())

        let generated =
            System.IO.File.ReadAllText(
                System.IO.Path.Combine(output.Path, "assignment.c"))
            |> TestHelpers.normalizeGeneratedCode

        Assert.Equal("value = 42;", generated)

    [<Fact>]
    let ``assignment rejects values from different contexts`` () =
        use output = new TemporaryDirectory()
        let leftContext =
            GenerationContext [new program(output.Path, "left.c", C99)]
        let rightContext =
            GenerationContext [new program(output.Path, "right.c", C99)]

        try
            let left =
                num0(Var(It 4, "left", NaN), context=leftContext)
            let right =
                num0(Var(It 4, "right", NaN), context=rightContext)

            Assert.Throws<InvalidOperationException>(Action(fun () ->
                left <== right))
            |> ignore
        finally
            leftContext.CurrentProgram.close()
            rightContext.CurrentProgram.close()

    [<Fact>]
    let ``array assignments retain their generation context`` () =
        use output = new TemporaryDirectory()

        makeProgramWithContext
            [output.Path, "arrays.c", C99]
            (fun context ->
                let values1 = var.i1("values1", 2)
                let values2 = var.i2("values2", 2, 2)
                let values3 = var.i3("values3", 2, 2, 2)

                Assert.Same(context, values1.Context.Value)
                Assert.Same(context, values2.Context.Value)
                Assert.Same(context, values3.Context.Value)

                values1 <== 1
                values2 <== 2
                values3 <== 3
                context.CurrentProgram.close())

        let generated =
            System.IO.File.ReadAllText(
                System.IO.Path.Combine(output.Path, "arrays.c"))

        Assert.Contains("values1", generated)
        Assert.Contains("values2", generated)
        Assert.Contains("values3", generated)
