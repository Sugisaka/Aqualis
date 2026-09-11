namespace Aqualis.Tests

open System
open System.IO
open System.Text.RegularExpressions
open Xunit
open Aqualis

module OptimizationTests =
    let private generateFindmin maxBracketExpansions =
        use output = new TemporaryDirectory()
        let project = "findmin-bracket-limit"

        Compile [C99] output.Path project "1.0" <| fun context ->
            let position = context.var.d1("position", 1)
            let direction = context.var.d1("direction", 1)
            let result = context.var.d1("result", 1)
            context.optimization.findminWithBracketLimit
                (3, maxBracketExpansions)
                (position, direction)
                (D 1.0)
                (fun value point -> value <== -point.[0])
                result

        File.ReadAllText(Path.Combine(output.Path, project + ".c"))

    let private generateBfgsUpdate () =
        use output = new TemporaryDirectory()
        let project = "bfgs-update"

        Compile [C99] output.Path project "1.0" <| fun context ->
            let inverseHessian = context.var.d2("inverseHessian", 2, 2)
            let gradientDelta = context.var.d1("gradientDelta", 2)
            let step = context.var.d1("step", 2)
            let curvature = context.var.d0("curvature")
            context.optimization.updateInverseHessianBfgs(inverseHessian,gradientDelta,step,curvature)

        File.ReadAllText(Path.Combine(output.Path, project + ".c"))

    [<Fact>]
    let ``findmin rejects a negative bracket expansion limit`` () =
        use context = Aqualis.BlankWriter C99
        let position = context.var.d1("position", 1)
        let direction = context.var.d1("direction", 1)

        let error =
            Assert.Throws<ArgumentException>(fun () ->
                context.optimization.findminWithBracketLimit
                    (3, -1)
                    (position, direction)
                    (D 1.0)
                    (fun _ _ -> ())
                    position)

        Assert.Equal("maxBracketExpansions", error.ParamName)

    [<Fact>]
    let ``findmin emits a bounded bracket expansion and failure fallback`` () =
        let generated = generateFindmin 2

        Assert.Matches(Regex(@"if\s*\([^\r\n]*<\s*2\)"), generated)
        Assert.Contains("&&", generated)
        Assert.Contains("Aqualis: line-search bracket expansion limit reached.", generated)

    [<Fact>]
    let ``findmin guards zero direction before normalization and retains initial point`` () =
        let generated = generateFindmin 2

        Assert.Matches(Regex(@"if\s*\([^\r\n]*>\s*0"), generated)
        Assert.Matches(Regex(@"result\[[^\]]+\]\s*=\s*position\[[^\]]+\]"), generated)
        Assert.Contains("Aqualis: line-search direction is zero; initial point retained.", generated)

    [<Fact>]
    let ``bounded variants are available for every higher-level optimizer`` () =
        let methodNames =
            typeof<ContextOptimization>.GetMethods()
            |> Array.map _.Name
            |> Set.ofArray

        [
            "findmin_GradientDescentWithBracketLimit"
            "findmin_ConjugateGradient1WithBracketLimit"
            "findmin_ConjugateGradient2WithBracketLimit"
            "findmin_NewtonWithBracketLimit"
            "findmin_quasiNewtonWithBracketLimit"
        ]
        |> List.iter (fun name -> Assert.Contains(name, methodNames))

    [<Fact>]
    let ``transpose-left matrix multiplication transposes the left operand`` () =
        use output = new TemporaryDirectory()
        let project = "transpose-left-matmul"

        Compile [C99] output.Path project "1.0" <| fun context ->
            let result = context.var.d2("result", 3, 4)
            let left = context.var.d2("left", 2, 3)
            let right = context.var.d2("right", 2, 4)
            context.la.matmulTransposeLeft(result,left,right)

        let generated = File.ReadAllText(Path.Combine(output.Path, project + ".c"))
        let transposedProduct =
            Regex(
                @"result\[(?<i>i\d+)\+(?<j>i\d+)\*result_size\[0\]\]\s*=\s*result\[\k<i>\+\k<j>\*result_size\[0\]\]\+left\[(?<k>i\d+)\+\k<i>\*left_size\[0\]\]\*right\[\k<k>\+\k<j>\*right_size\[0\]\]")
        Assert.Matches(transposedProduct, generated)

    [<Fact>]
    let ``quasi Newton update applies the right factor before its transposed left factor`` () =
        let generated = generateBfgsUpdate()
        Assert.Matches(Regex(@"if\s*\(curvature\s*>\s*0"), generated)

        let rightProduct =
            Regex.Match(
                generated,
                @"(?<u>d2\d+)\[[^\r\n]+\]\s*=\s*\k<u>\[[^\r\n]+\]\+inverseHessian\[[^\r\n]+\]\*(?<t>d2\d+)\[[^\r\n]+\]")
        Assert.True(rightProduct.Success, "Expected the first matrix product to be B * T.")

        let transposedLeftProduct =
            Regex(
                @"inverseHessian\[[^\r\n]+\]\s*=\s*inverseHessian\[[^\r\n]+\]\+"
                + Regex.Escape(rightProduct.Groups.["t"].Value)
                + @"\[[^\r\n]+\]\*"
                + Regex.Escape(rightProduct.Groups.["u"].Value)
                + @"\[[^\r\n]+\]")
        let transposedLeftMatch = transposedLeftProduct.Match(generated, rightProduct.Index + rightProduct.Length)
        Assert.True(transposedLeftMatch.Success, "Expected the second matrix product to be transpose(T) * (B * T).")
