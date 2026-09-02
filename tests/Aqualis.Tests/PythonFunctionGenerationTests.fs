namespace Aqualis.Tests

open System.IO
open System.Text.RegularExpressions
open Xunit
open Aqualis

module PythonFunctionGenerationTests =
    let private generate project code =
        use output = new TemporaryDirectory()
        Compile [Python] output.Path project "1.0" code
        File.ReadAllText(Path.Combine(output.Path, project + ".py"))

    let private assertBareReturn functionName (source:string) =
        Assert.Matches(
            Regex("(?ms)^def " + Regex.Escape(functionName) + @"\([^\r\n]*\):.*?^  return\s*$"),
            source)

    [<Fact>]
    let ``Python function without arguments uses a bare call`` () =
        let source =
            generate "python-no-arguments" (fun context ->
                context.func "noop" ignore)

        Assert.Contains("def noop():", source)
        assertBareReturn "noop" source
        Assert.Matches(Regex("(?m)^noop\(\)$"), source)
        Assert.DoesNotMatch(Regex("(?m)^\s*=\s*noop\("), source)

    [<Fact>]
    let ``Python function with only array arguments uses a bare call`` () =
        let source =
            generate "python-array-argument" (fun context ->
                let values = context.var.d1("values", 2)
                context.func "clear_values" <| fun childContext ->
                    values.farg childContext <| fun functionValues ->
                        functionValues[0] <== 0.0)

        assertBareReturn "clear_values" source
        Assert.Matches(Regex("(?m)^clear_values\([^\r\n]*\)$"), source)
        Assert.DoesNotMatch(Regex("(?m)^\s*=\s*clear_values\("), source)

    [<Fact>]
    let ``Python mixed arguments write back only scalar values`` () =
        let source =
            generate "python-mixed-arguments" (fun context ->
                let result = context.var.d0 "result"
                let values = context.var.d1("values", 2)
                context.func "read_first" <| fun childContext ->
                    result.farg childContext <| fun functionResult ->
                    values.farg childContext <| fun functionValues ->
                        functionResult <== functionValues[0])

        Assert.Matches(Regex("(?m)^  return arg\d+$"), source)
        Assert.Matches(Regex("(?m)^result = read_first\([^\r\n]*\)$"), source)
        Assert.DoesNotMatch(Regex("(?m)^result,\s*values"), source)

    [<Fact>]
    let ``Python function preserves multiple scalar write backs`` () =
        let source =
            generate "python-multiple-results" (fun context ->
                let left = context.var.d0 "left"
                let right = context.var.d0 "right"
                context.func "swap_values" <| fun childContext ->
                    left.farg childContext <| fun functionLeft ->
                    right.farg childContext <| fun functionRight ->
                        childContext.ch.d <| fun temporary ->
                            temporary <== functionLeft
                            functionLeft <== functionRight
                            functionRight <== temporary)

        Assert.Matches(Regex("(?m)^  return arg\d+, arg\d+$"), source)
        Assert.Matches(Regex("(?m)^left, right = swap_values\([^\r\n]*\)$"), source)
