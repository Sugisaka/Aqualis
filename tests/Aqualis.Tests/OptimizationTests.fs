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
