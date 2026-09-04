namespace Aqualis.Tests

open Xunit
open Aqualis

module ExpressionSimplificationTests =
    let private namedInteger name =
        int0(Var(It 4, name, Int 0))

    let private assertRemainsDivision (expression:expr) =
        match expression.simp with
        |Div _ -> ()
        |actual -> Assert.Fail($"Expected division to remain symbolic, but got {actual}.")

    let private assertRemainsModulo (expression:expr) =
        match expression.simp with
        |Mod _ -> ()
        |actual -> Assert.Fail($"Expected modulo to remain symbolic, but got {actual}.")

    let private assertComparisonChain expected actual =
        let comparisonShape expression =
            match expression with
            |Less(Var(_,left,_),Var(_,right,_)) -> "<", left, right
            |LessEq(Var(_,left,_),Var(_,right,_)) -> "<=", left, right
            |Greater(Var(_,left,_),Var(_,right,_)) -> ">", left, right
            |GreaterEq(Var(_,left,_),Var(_,right,_)) -> ">=", left, right
            |_ -> failwithf "Expected a comparison expression, but got %A." expression

        match actual with
        |AND expressions ->
            Assert.Equal<(string*string*string) list>(expected, List.map comparisonShape expressions)
        |_ ->
            Assert.Fail($"Expected a comparison chain, but got {actual}.")

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
    let ``division preserves expressions whose denominator may be zero`` () =
        let realValue = Var(Dt, "realValue", NaN)
        let complexValue = Var(Zt, "complexValue", NaN)

        assertRemainsDivision (Div(Dt, Dbl 0.0, realValue))
        assertRemainsDivision (Div(Dt, realValue, realValue))
        assertRemainsDivision (Div(Zt, Cpx(0.0, 0.0), complexValue))
        assertRemainsDivision (Div(Zt, complexValue, complexValue))

    [<Fact>]
    let ``integer division and modulo preserve expressions whose denominator may be zero`` () =
        let value = Var(It 4, "value", NaN)

        assertRemainsDivision (Div(It 4, Int 0, value))
        assertRemainsDivision (Div(It 4, value, value))
        assertRemainsModulo (Mod(It 4, Int 0, value))

    [<Fact>]
    let ``division and modulo still fold safe constants`` () =
        match Div(Dt, Dbl 0.0, Dbl 2.0).simp with
        |Dbl value -> Assert.Equal(0.0, value)
        |actual -> Assert.Fail($"Expected a folded double zero, but got {actual}.")

        for expression,expected in
            [
                Div(It 4, Int 6, Int 3), 2
                Div(It 4, Int 0, Int 2), 0
                Mod(It 4, Int 0, Int 2), 0
            ] do
            match expression.simp with
            |Int value -> Assert.Equal(expected, value)
            |actual -> Assert.Fail($"Expected a folded integer, but got {actual}.")

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

    [<Fact>]
    let ``chained comparisons preserve each leading comparison operator`` () =
        let a = namedInteger "a"
        let b = namedInteger "b"
        let c = namedInteger "c"

        assertComparisonChain ["<", "a", "b"; "<", "b", "c"] ((a .< b .< c).Expr)
        assertComparisonChain ["<", "a", "b"; "<=", "b", "c"] ((a .< b .<= c).Expr)
        assertComparisonChain ["<", "a", "b"; ">", "b", "c"] ((a .< b .> c).Expr)
        assertComparisonChain ["<", "a", "b"; ">=", "b", "c"] ((a .< b .>= c).Expr)
        assertComparisonChain ["<=", "a", "b"; "<", "b", "c"] ((a .<= b .< c).Expr)
        assertComparisonChain ["<=", "a", "b"; "<=", "b", "c"] ((a .<= b .<= c).Expr)
        assertComparisonChain ["<=", "a", "b"; ">", "b", "c"] ((a .<= b .> c).Expr)
        assertComparisonChain ["<=", "a", "b"; ">=", "b", "c"] ((a .<= b .>= c).Expr)
        assertComparisonChain [">", "a", "b"; "<", "b", "c"] ((a .> b .< c).Expr)
        assertComparisonChain [">", "a", "b"; "<=", "b", "c"] ((a .> b .<= c).Expr)
        assertComparisonChain [">", "a", "b"; ">", "b", "c"] ((a .> b .> c).Expr)
        assertComparisonChain [">", "a", "b"; ">=", "b", "c"] ((a .> b .>= c).Expr)
        assertComparisonChain [">=", "a", "b"; "<", "b", "c"] ((a .>= b .< c).Expr)
        assertComparisonChain [">=", "a", "b"; "<=", "b", "c"] ((a .>= b .<= c).Expr)
        assertComparisonChain [">=", "a", "b"; ">", "b", "c"] ((a .>= b .> c).Expr)
        assertComparisonChain [">=", "a", "b"; ">=", "b", "c"] ((a .>= b .>= c).Expr)

    [<Fact>]
    let ``comparison chains retain all expressions beyond three operands`` () =
        let a = namedInteger "a"
        let b = namedInteger "b"
        let c = namedInteger "c"
        let d = namedInteger "d"

        assertComparisonChain
            [">=", "a", "b"; "<=", "b", "c"; "<", "c", "d"]
            ((a .>= b .<= c .< d).Expr)

    [<Fact>]
    let ``numeric chained comparisons retain inclusive boundaries`` () =
        let mutable ascendingBranchRan = false
        let mutable descendingBranchRan = false

        Aqualis.runWithWriterlessContext Numeric (fun context ->
            context.br.if1 (int0(Int 1) .<= int0(Int 1) .< int0(Int 2))
                (fun () -> ascendingBranchRan <- true)
            context.br.if1 (int0(Int 3) .>= int0(Int 2) .> int0(Int 1))
                (fun () -> descendingBranchRan <- true))

        Assert.True(ascendingBranchRan)
        Assert.True(descendingBranchRan)
