namespace Aqualis.Tests

open System
open Xunit
open Aqualis

module AssignmentTests =
    [<Fact>]
    let ``numeric constants ignore an explicitly supplied generation context`` () =
        use output = new TemporaryDirectory()
        let context = GenerationContext [new program(output.Path, "constants.c", C99)]

        try
            Assert.True(int0(Int 1, context=context).Context.IsNone)
            Assert.True(double0(Dbl 0.0, context=context).Context.IsNone)
            Assert.True(complex0(Cpx(0.0, 1.0), context=context).Context.IsNone)
        finally
            context.CurrentProgram.close()

    [<Fact>]
    let ``operators merge constant and variable contexts`` () =
        use output = new TemporaryDirectory()
        let context = GenerationContext [new program(output.Path, "merge.c", C99)]

        try
            let variable = int0(Var(It 4, "value", NaN), context=context)
            let result = variable + int0(Int 1)
            Assert.Same(context, result.Context.Value)
        finally
            context.CurrentProgram.close()

    [<Fact>]
    let ``operators reject operands from different output contexts`` () =
        use output = new TemporaryDirectory()
        let first = GenerationContext [new program(output.Path, "first-op.c", C99)]
        let second = GenerationContext [new program(output.Path, "second-op.c", C99)]

        try
            let left = int0(Var(It 4, "left", NaN), context=first)
            let right = int0(Var(It 4, "right", NaN), context=second)
            Assert.Throws<InvalidOperationException>(Action(fun () -> left + right |> ignore))
            |> ignore
        finally
            first.CurrentProgram.close()
            second.CurrentProgram.close()

    [<Fact>]
    let ``assignment rejects a target without a generation context`` () =
        let target = int0(Var(It 4, "target", NaN))
        Assert.Throws<InvalidOperationException>(Action(fun () -> target <== 1))
        |> ignore

    [<Fact>]
    let ``numeric constants do not capture the active generation context`` () =
        use output = new TemporaryDirectory()
        let secondContext =
            GenerationContext [new program(output.Path, "second.c", C99)]

        try
            let constant = _0d

            Assert.True(constant.Context.IsNone)

            let target =
                double0(Var(Dt, "target", NaN), context=secondContext)
            target <== constant

            secondContext.CurrentProgram.close()
            let generated =
                System.IO.File.ReadAllText(
                    System.IO.Path.Combine(output.Path, "second.c"))
                |> TestHelpers.normalizeGeneratedCode

            Assert.Equal("target = 0.0E0;", generated)
        finally
            secondContext.CurrentProgram.close()

    [<Fact>]
    let ``scalar assignment writes through the left hand context`` () =
        use output = new TemporaryDirectory()

        makeProgramWithContext
            [output.Path, "assignment.c", C99]
            (fun context ->
                let value = CompilationEnvironment(Some context).var.i0 "value"
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
                int0(Var(It 4, "left", NaN), context=leftContext)
            let right =
                int0(Var(It 4, "right", NaN), context=rightContext)

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
                let variables = CompilationEnvironment(Some context).var
                let values1 = variables.i1("values1", 2)
                let values2 = variables.i2("values2", 2, 2)
                let values3 = variables.i3("values3", 2, 2, 2)

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

    [<Fact>]
    let ``array size values retain their generation context`` () =
        use output = new TemporaryDirectory()
        let context =
            GenerationContext [
                new program(output.Path, "array-sizes.c", C99)
            ]
        let variables = CompilationEnvironment(Some context).var

        try
            let values1 = variables.i1("values1", 2)
            let values2 = variables.i2("values2", 2, 3)
            let values3 = variables.i3("values3", 2, 3, 4)

            Assert.Same(context, values1.size1.Context.Value)
            Assert.Same(context, values2.size1.Context.Value)
            Assert.Same(context, values2.size2.Context.Value)
            Assert.Same(context, values3.size1.Context.Value)
            Assert.Same(context, values3.size2.Context.Value)
            Assert.Same(context, values3.size3.Context.Value)
        finally
            context.CurrentProgram.close()
            context.Deactivate()
