namespace Aqualis.Tests

open System.IO
open Xunit
open Aqualis

module ControlFlowGenerationTests =
    let private extension = function
        |C99 -> ".c"
        |Fortran -> ".f90"
        |Python -> ".py"
        |JavaScript -> ".js"
        |PHP -> ".php"
        |language -> failwithf "Unsupported test language: %A" language

    let private generate language name code =
        use output = new TemporaryDirectory()
        let fileName = name + extension language
        Aqualis.makeProgramWithContext (output.Path,fileName,language) code
        File.ReadAllText(Path.Combine(output.Path,fileName))

    [<Fact>]
    let ``statically empty ranges preserve generation state without emitting their bodies`` () =
        for language in [C99; Fortran; Python; JavaScript; PHP] do
            let mutable callbackCount = 0
            let generated =
                generate language "empty-range" <| fun context ->
                    expr.range context None (Int 2) (Int 1) <| fun _ ->
                        callbackCount <- callbackCount + 1
                        context.codewritein "AQUALIS_EMPTY_RANGE_BODY"

            Assert.Equal(1, callbackCount)
            Assert.DoesNotContain("AQUALIS_EMPTY_RANGE_BODY", generated)

    [<Fact>]
    let ``statically empty early-exit ranges emit neither body nor exit syntax`` () =
        for language in [C99; Fortran; Python; JavaScript; PHP] do
            let mutable callbackCount = 0
            let generated =
                generate language "empty-exit-range" <| fun context ->
                    expr.range_exit context None (Int 2) (Int 1) <| fun (exitLoop,_) ->
                        callbackCount <- callbackCount + 1
                        exitLoop()
                        context.codewritein "AQUALIS_EMPTY_EXIT_BODY"

            Assert.Equal(1, callbackCount)
            Assert.DoesNotContain("AQUALIS_EMPTY_EXIT_BODY", generated)

    [<Fact>]
    let ``C early-exit ranges use valid identifiers and statements`` () =
        let generated =
            generate C99 "c-exit-range" <| fun context ->
                expr.range_exitC context None (Int 0) (Int 2) <| fun (exitLoop,_) ->
                    exitLoop()

        Assert.Contains("goto _11;", generated)
        Assert.Contains("_11:;", generated)
        Assert.DoesNotContain("goto 11", generated)

    [<Fact>]
    let ``Python loops implement early exit with scoped exceptions`` () =
        let generated =
            generate Python "python-exit-loops" <| fun context ->
                expr.loopPy context <| fun (exitLoop,_) -> exitLoop()
                expr.range_exitPy context None (Int 0) (Int 2) <| fun (exitLoop,_) ->
                    exitLoop()

        Assert.Contains("class _AqualisLoopExit11(Exception):", generated)
        Assert.Contains("raise _AqualisLoopExit11()", generated)
        Assert.Contains("except _AqualisLoopExit11:", generated)
        Assert.Contains("class _AqualisLoopExit12(Exception):", generated)
        Assert.DoesNotContain("goto ", generated)
        Assert.DoesNotContain("flag =", generated)

    [<Fact>]
    let ``PHP early-exit ranges keep goto and labels inside PHP tags`` () =
        let generated =
            generate PHP "php-exit-range" <| fun context ->
                expr.range_exitPh context None (Int 0) (Int 2) <| fun (exitLoop,_) ->
                    exitLoop()

        Assert.Contains("goto _11; ?>", generated)
        Assert.Contains("_11:; ?>", generated)
        Assert.DoesNotContain("goto 11", generated)

    [<Fact>]
    let ``PHP print helpers emit executable escaped PHP statements`` () =
        let generated =
            generate PHP "php-print" <| fun context ->
                let value = context.var.i0 "value"
                value <== 6
                context.print.t value
                context.print.s "a\"b"

        Assert.Contains("<?php print($value); ?>", generated)
        Assert.Contains("<?php print(\"a\\\"b\"); ?>", generated)
