namespace Aqualis.Tests

open System
open System.IO
open Xunit
open Aqualis

module AssignmentTests =
    let private createContext path name =
        new Aqualis(Some path, Some name, C99)

    let private assignRealPlusOne (target:double0, value:#IReal0) =
        target <== value.ToDouble0 + 1

    let private assignNumericPlusOne (target:complex0, value:#INum0) =
        target <== value.ToComplex0 + 1

    [<Fact>]
    let ``real scalar marker includes integers and doubles but excludes complex values`` () =
        Assert.True(typeof<IReal0>.IsAssignableFrom(typeof<int0>))
        Assert.True(typeof<IReal0>.IsAssignableFrom(typeof<double0>))
        Assert.False(typeof<IReal0>.IsAssignableFrom(typeof<complex0>))

    [<Fact>]
    let ``real scalar constraint accepts integer and double expressions`` () =
        use output = new TemporaryDirectory()
        use context = createContext output.Path "real-scalar-assignment.c"
        let target = double0(Var(Dt, "target", NaN), context)
        let integerValue = int0(Var(It 4, "integerValue", NaN), context)
        let doubleValue = double0(Var(Dt, "doubleValue", NaN), context)

        Assert.Same(integerValue.Expr, (integerValue :> IReal0).ToDouble0.Expr)
        Assert.Same(context, (integerValue :> IReal0).ToDouble0.Context)
        Assert.Same(doubleValue.Expr, (doubleValue :> IReal0).ToDouble0.Expr)
        Assert.Same(context, (doubleValue :> IReal0).ToDouble0.Context)
        assignRealPlusOne(target, integerValue)
        assignRealPlusOne(target, doubleValue)

    [<Fact>]
    let ``numeric scalar conversion to complex preserves expressions and contexts`` () =
        use output = new TemporaryDirectory()
        use context = createContext output.Path "numeric-scalar-assignment.c"
        let target = complex0(Var(Zt, "target", NaN), context)
        let integerValue = int0(Var(It 4, "integerValue", NaN), context)
        let doubleValue = double0(Var(Dt, "doubleValue", NaN), context)
        let complexValue = complex0(Var(Zt, "complexValue", NaN), context)

        let values : INum0 list = [integerValue; doubleValue; complexValue]
        for value in values do
            Assert.Same(value.Expr, value.ToComplex0.Expr)
            Assert.Same(context, value.ToComplex0.Context)

        assignNumericPlusOne(target, integerValue)
        assignNumericPlusOne(target, doubleValue)
        assignNumericPlusOne(target, complexValue)

    [<Fact>]
    let ``operators merge constant and variable contexts`` () =
        use output = new TemporaryDirectory()
        use context = createContext output.Path "merge.c"
        let variable = int0(Var(It 4, "value", NaN), context)
        let result = variable + int0(Int 1)
        Assert.Same(context, result.Context)

    [<Fact>]
    let ``operators reject operands from different output contexts`` () =
        use output = new TemporaryDirectory()
        use first = createContext output.Path "first-op.c"
        use second = createContext output.Path "second-op.c"
        let left = int0(Var(It 4, "left", NaN), first)
        let right = int0(Var(It 4, "right", NaN), second)
        Assert.Throws<InvalidOperationException>(Action(fun () -> left + right |> ignore))
        |> ignore

    [<Fact>]
    let ``numeric constants inherit the assignment target context`` () =
        use output = new TemporaryDirectory()
        let path = Path.Combine(output.Path, "constant.c")
        use context = createContext output.Path "constant.c"
        let constant = _0d
        Assert.True(constant.Context.CodeFile.IsNone)
        let target = double0(Var(Dt, "target", NaN), context)
        target <== constant
        context.close()
        let generated = File.ReadAllText(path) |> TestHelpers.normalizeGeneratedCode
        Assert.Equal("target = 0.0E0;", generated)

    [<Fact>]
    let ``scalar assignment writes through the left hand context`` () =
        use output = new TemporaryDirectory()
        Aqualis.makeProgramWithContext (output.Path, "assignment.c", C99) <| fun context ->
            let value = context.var.i0 "value"
            value <== 42
        let generated =
            File.ReadAllText(Path.Combine(output.Path, "assignment.c"))
            |> TestHelpers.normalizeGeneratedCode
        Assert.Equal("value = 42;", generated)

    [<Fact>]
    let ``assignment rejects values from different contexts`` () =
        use output = new TemporaryDirectory()
        use leftContext = createContext output.Path "left.c"
        use rightContext = createContext output.Path "right.c"
        let left = int0(Var(It 4, "left", NaN), leftContext)
        let right = int0(Var(It 4, "right", NaN), rightContext)
        Assert.Throws<InvalidOperationException>(Action(fun () -> left <== right))
        |> ignore

    [<Fact>]
    let ``array assignments retain their generation context`` () =
        use output = new TemporaryDirectory()
        Aqualis.makeProgramWithContext (output.Path, "arrays.c", C99) <| fun context ->
            let values1 = context.var.i1("values1", 2)
            let values2 = context.var.i2("values2", 2, 2)
            let values3 = context.var.i3("values3", 2, 2, 2)
            Assert.Same(context, values1.Context)
            Assert.Same(context, values2.Context)
            Assert.Same(context, values3.Context)
            Assert.Same(context, values1.size1.Context)
            Assert.Same(context, values2.size2.Context)
            Assert.Same(context, values3.size3.Context)
            values1 <== 1
            values2 <== 2
            values3 <== 3
        let generated = File.ReadAllText(Path.Combine(output.Path, "arrays.c"))
        Assert.Contains("values1", generated)
        Assert.Contains("values2", generated)
        Assert.Contains("values3", generated)
