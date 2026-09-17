namespace Aqualis.Tests

open Xunit
open Aqualis

module ExpressionSimplificationTests =
    [<Fact>]
    let ``zero arithmetic with a real variable remains symbolic`` () =
        let value = double0(Var(Dt, "value", NaN))
        match (D 0.0 * value).Expr.simp with
        |Mul(Dt,_,_) -> ()
        |actual -> Assert.Fail($"Expected multiplication to remain symbolic, but got {actual}.")
        match (value - value).Expr.simp with
        |Sub(Dt,_,_) -> ()
        |actual -> Assert.Fail($"Expected subtraction to remain symbolic, but got {actual}.")
        for expression in [(D 0.0 * D 3.0).Expr; (D 3.0 - D 3.0).Expr] do
            match expression.simp with
            |Dbl zero -> Assert.Equal(0.0, zero)
            |actual -> Assert.Fail($"Expected a real zero, but got {actual}.")

    [<Fact>]
    let ``addition and division preserve their declared real type`` () =
        for expression,expected in
            [(I 2 + D 0.0).Expr, 2.0
             (I 2 / I 1).Expr, 2.0
             (I 2 / I 2).Expr, 1.0] do
            match expression.simp with
            |Dbl actual -> Assert.Equal(expected,actual)
            |actual -> Assert.Fail($"Expected a real literal, but got {actual}.")

    [<Fact>]
    let ``complex literal division remains finite across extreme magnitudes`` () =
        for magnitude in [1.0e-308; 1.0e-200; 1.0e200; 1.0e308] do
            let value = complex0(Cpx(magnitude,magnitude))
            match (value/value).Expr.simp with
            |Cpx(real,imaginary) ->
                Assert.Equal(1.0,real,12)
                Assert.Equal(0.0,imaginary,12)
            |actual -> Assert.Fail($"Expected a finite quotient, but got {actual}.")

    [<Fact>]
    let ``power simplification preserves the requested numeric domain`` () =
        match (double0.powr(D -4.0, D 0.5)).Expr.simp with
        |Dbl result -> Assert.True(System.Double.IsNaN(result))
        |actual -> Assert.Fail($"Expected a real NaN, but got {actual}.")

        match (complex0.powr(complex0(Dbl -4.0), D 0.5)).Expr.simp with
        |Pow(Zt,_,_) -> ()
        |actual -> Assert.Fail($"Expected a complex power, but got {actual}.")

    [<Fact>]
    let ``real negative square root remains real and complex square root remains complex`` () =
        for value in [asm.sqrt (D -4.0); asm.sqrt (-(D 4.0))] do
            match value.Expr.simp with
            |Dbl result -> Assert.True(System.Double.IsNaN(result))
            |actual -> Assert.Fail($"Expected a real NaN, but got {actual}.")

        match (asm.sqrt (complex0(Cpx(-4.0, 0.0)))).Expr.simp with
        |Cpx(real, imaginary) ->
            Assert.Equal(0.0, real, 12)
            Assert.Equal(2.0, imaginary, 12)
        |actual -> Assert.Fail($"Expected a complex square root, but got {actual}.")

    [<Fact>]
    let ``complex literal magnitude and logarithm use the true magnitude`` () =
        let value = complex0(Cpx(3.0, 4.0))

        match (asm.abs value).Expr.simp with
        |Dbl magnitude -> Assert.Equal(5.0, magnitude, 12)
        |actual -> Assert.Fail($"Expected a real magnitude, but got {actual}.")

        match (asm.log value).Expr.simp with
        |Cpx(real, imaginary) ->
            Assert.Equal(System.Math.Log(5.0), real, 12)
            Assert.Equal(System.Math.Atan2(4.0, 3.0), imaginary, 12)
        |actual -> Assert.Fail($"Expected a complex logarithm, but got {actual}.")

        match (asm.abs (complex0(Cpx(1.0e308, 1.0e308)))).Expr.simp with
        |Dbl magnitude -> Assert.True(System.Double.IsFinite(magnitude))
        |actual -> Assert.Fail($"Expected a finite magnitude, but got {actual}.")

    [<Fact>]
    let ``complex real-axis literals stay in the complex domain`` () =
        let negative = complex0(Cpx(-4.0, 0.0))
        let outside = complex0(Cpx(2.0, 0.0))
        let assertComplex expectedReal expectedImaginary (expression:expr) =
            match expression.simp with
            |Cpx(real, imaginary) ->
                Assert.Equal(expectedReal, real, 12)
                Assert.Equal(expectedImaginary, imaginary, 12)
            |actual -> Assert.Fail($"Expected a complex literal, but got {actual}.")

        assertComplex (System.Math.Log 4.0) System.Math.PI (asm.log negative).Expr
        assertComplex (System.Math.Log10 4.0) (System.Math.PI / System.Math.Log 10.0) (asm.log10 negative).Expr
        assertComplex (System.Math.PI / 2.0) (System.Math.Acosh 2.0) (asm.asin outside).Expr
        assertComplex 0.0 (-(System.Math.Acosh 2.0)) (asm.acos outside).Expr

        let symbolicReal = double0(Var(Dt, "symbolicReal", NaN)).ToComplex0
        for expression in
            [ (asm.log symbolicReal).Expr
              (asm.log10 symbolicReal).Expr
              (asm.asin symbolicReal).Expr
              (asm.acos symbolicReal).Expr
              (asm.sqrt symbolicReal).Expr ] do
            Assert.Equal(Zt, expression.simp.etype)

    [<Fact>]
    let ``large finite complex square root remains finite`` () =
        for real,imaginary,expectedReal,expectedImaginary in
            [ 1.0e308, 1.0e308, 1.09868411346781e154, 4.5508986056222734e153
              -1.0e308, 1.0e308, 4.5508986056222734e153, 1.09868411346781e154 ] do
            match (asm.sqrt (complex0(Cpx(real,imaginary)))).Expr.simp with
            |Cpx(actualReal, actualImaginary) ->
                Assert.True(System.Double.IsFinite actualReal)
                Assert.True(System.Double.IsFinite actualImaginary)
                Assert.InRange(abs (actualReal / expectedReal - 1.0), 0.0, 1.0e-12)
                Assert.InRange(abs (actualImaginary / expectedImaginary - 1.0), 0.0, 1.0e-12)
            |actual -> Assert.Fail($"Expected a finite complex square root, but got {actual}.")

    [<Fact>]
    let ``complex trigonometric literals remain finite when their results are finite`` () =
        let large = complex0(Cpx(1.0,710.0))
        for expression,expectedReal,expectedImaginary in
            [ (asm.sin large).Expr, 9.399208879688905e307, 6.035162617272641e307
              (asm.cos large).Expr, 6.035162617272641e307, -9.399208879688905e307
              (asm.tan large).Expr, 0.0, 1.0
              (asm.tan (complex0(Cpx(1.0,750.0)))).Expr, 0.0, 1.0 ] do
            match expression.simp with
            |Cpx(real,imaginary) ->
                Assert.True(System.Double.IsFinite real)
                Assert.True(System.Double.IsFinite imaginary)
                if expectedReal=0.0 then Assert.InRange(abs real,0.0,1.0e-12)
                else Assert.InRange(abs (real/expectedReal-1.0),0.0,1.0e-12)
                Assert.InRange(abs (imaginary/expectedImaginary-1.0),0.0,1.0e-12)
            |actual -> Assert.Fail($"Expected a finite complex value, but got {actual}.")

        match (asm.sin (complex0(Cpx(1.0,1.0e-308)))).Expr.simp with
        |Cpx(_,imaginary) -> Assert.True(imaginary>0.0)
        |actual -> Assert.Fail($"Expected a nonzero complex imaginary component, but got {actual}.")

    [<Fact>]
    let ``complex trigonometric functions retain their type for symbolic real inputs`` () =
        let value = double0(Var(Dt,"value",NaN)).ToComplex0
        for expression in
            [ (asm.exp value).Expr
              (asm.sin value).Expr
              (asm.cos value).Expr
              (asm.tan value).Expr
              (asm.atan value).Expr ] do
            Assert.Equal(Zt,expression.simp.etype)

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
    let ``algebraic simplification preserves integer quotient boundaries`` () =
        let a = namedInteger "a"
        let b = namedInteger "b"

        match ((a ./ 3) + (b ./ 3)).Expr.simp with
        |Add(It 4, Div(It 4,_,_), Div(It 4,_,_)) -> ()
        |actual -> Assert.Fail($"Integer quotient sum was changed: {actual}.")

        match ((a ./ 3) - (b ./ 3)).Expr.simp with
        |Sub(It 4, Div(It 4,_,_), Div(It 4,_,_)) -> ()
        |actual -> Assert.Fail($"Integer quotient difference was changed: {actual}.")

        match ((a ./ 3) * 2).Expr.simp with
        |Mul(It 4, Div(It 4,_,_), Int 2) -> ()
        |actual -> Assert.Fail($"Integer quotient product was changed: {actual}.")

        match (2 * (a ./ 3)).Expr.simp with
        |Mul(It 4, Int 2, Div(It 4,_,_)) -> ()
        |actual -> Assert.Fail($"Reversed integer quotient product was changed: {actual}.")

        match ((a ./ 3) / 2).Expr.simp with
        |Div(Dt, Div(It 4,_,_), _) -> ()
        |actual -> Assert.Fail($"Integer quotient before real division was changed: {actual}.")

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
