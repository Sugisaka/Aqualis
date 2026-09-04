namespace Aqualis.Tests

open Xunit
open Aqualis

module ExpressionSimplificationTests =
    [<Theory>]
    [<InlineData(1, 1, true)>]
    [<InlineData(2, 1, true)>]
    [<InlineData(0, 1, false)>]
    let ``integer greater-or-equal constants simplify correctly`` left right expected =
        let actual = expr.simpGreaterEq(Int left, Int right)
        Assert.Equal((if expected then True else False), actual)

    [<Theory>]
    [<InlineData(1.0, 1.0, true)>]
    [<InlineData(2.0, 1.0, true)>]
    [<InlineData(0.0, 1.0, false)>]
    let ``double greater-or-equal constants simplify correctly`` left right expected =
        let actual = expr.simpGreaterEq(Dbl left, Dbl right)
        Assert.Equal((if expected then True else False), actual)

    [<Fact>]
    let ``mixed numeric equality simplifies to true`` () =
        Assert.Equal(True, expr.simpGreaterEq(Int 1, Dbl 1.0))
        Assert.Equal(True, expr.simpGreaterEq(Dbl 1.0, Int 1))

    [<Fact>]
    let ``nonconstant greater-or-equal remains symbolic`` () =
        let variable = Var(It 4, "value", NaN)
        match expr.simpGreaterEq(variable, Int 1) with
        |GreaterEq(Var(It 4, "value", _), Int 1) -> ()
        |actual -> Assert.Fail($"Expected a symbolic greater-or-equal expression, but got {actual}.")

    [<Fact>]
    let ``numeric greater-or-equal executes true branch for equal constants`` () =
        let mutable trueBranchRan = false
        let mutable falseBranchRan = false

        Aqualis.runWithWriterlessContext Numeric (fun context ->
            context.br.if2 (int0(Int 1) .>= int0(Int 1))
                (fun () -> trueBranchRan <- true)
                (fun () -> falseBranchRan <- true))

        Assert.True(trueBranchRan)
        Assert.False(falseBranchRan)
