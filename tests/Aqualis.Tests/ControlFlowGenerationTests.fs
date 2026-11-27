namespace Aqualis.Tests

open System.IO
open Xunit
open Aqualis

module ControlFlowGenerationTests =
    [<Fact>]
    let ``Python constant upper boundary is incremented without 32-bit folding`` () =
        use output = new TemporaryDirectory()
        Compile [Python] output.Path "python_max_bound" "1.0" (fun context ->
            context.iter.range(System.Int32.MaxValue,System.Int32.MaxValue) (fun i -> context.print.t i))
        let generated = File.ReadAllText(Path.Combine(output.Path,"python_max_bound.py"))
        let loop = generated.Split('\n') |> Array.find (fun line -> line.TrimStart().StartsWith("for "))
        Assert.Contains("(2147483647) + 1",loop)
        Assert.DoesNotContain("-2147483648",loop)

    [<Fact>]
    let ``conditional temporaries use the promoted branch type`` () =
        for language in [C99;Fortran;Python] do
            use output = new TemporaryDirectory()
            Compile [language] output.Path "promoted_condition" "1.0" (fun context ->
                let flag = context.var.i0 "flag"
                let condition = Eq(flag.Expr,Int 1)
                let expression = IfEl(condition,Int 1,Dbl 2.5)
                let rendered = expression.eval context
                Assert.Contains(rendered,context.d0.varList)
                let complexExpression = IfEl(condition,Dbl 1.0,Cpx(2.0,3.0))
                let complexRendered = complexExpression.eval context
                Assert.Contains(complexRendered,context.z0.varList))

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
    let ``Python empty suites receive pass statements`` () =
        let generated =
            generate Python "python-empty-suites" <| fun context ->
                expr.forLoopPy context (Int 0,Int 1) (fun _ -> ())
                expr.whiledoPy context False (fun () -> ())
                expr.rangePy context None (Int 0) (Int 1) (fun _ -> ())
                expr.range_exitPy context None (Int 0) (Int 1) (fun _ -> ())
                expr.branchPy context <| fun (ifCode,elifCode,elseCode) ->
                    ifCode True (fun () -> ())
                    elifCode False (fun () -> ())
                    elseCode (fun () -> ())
        let passCount = generated.Split("pass").Length - 1
        Assert.True(passCount >= 7,$"Expected seven empty Python suites to contain pass, but found {passCount}.\n{generated}")

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

    [<Fact>]
    let ``named loop counters reject invalid and runtime names before their bodies`` () =
        for language in [C99;Fortran;Python;JavaScript;PHP] do
            let specific =
                match language with
                |Python -> ["numpy";"math";"class"]
                |C99 -> ["for";"_private"]
                |JavaScript -> ["Math";"for"]
                |PHP -> ["GLOBALS";"$GLOBALS";"this"]
                |_ -> []
            for name in ["1invalid";"with space";"aqualis_reserved";"d0001";"arg01"] @ specific do
                for empty in [false;true] do
                    for earlyExit in [false;true] do
                        use output = new TemporaryDirectory()
                        let mutable bodyCalled = false
                        Assert.Throws<System.ArgumentException>(fun () ->
                            Compile [language] output.Path "invalid_counter" "1" (fun context ->
                                let last = if empty then I -1 else I 1
                                if earlyExit then context.iter.range_exit(name,I 0,last) (fun _ -> bodyCalled <- true)
                                else context.iter.range(name,I 0,last) (fun _ -> bodyCalled <- true))) |> ignore
                        Assert.False(bodyCalled)
                        Assert.Empty(Directory.GetFileSystemEntries output.Path)

    [<Fact>]
    let ``valid named counters retain target specific identifier behavior`` () =
        for language,names in [C99,["index"];Fortran,["index";"do"];Python,["index";"NumPy"];JavaScript,["index"];PHP,["index";"$index";"for"]] do
            for name in names do
                use output = new TemporaryDirectory()
                Compile [language] output.Path "valid_counter" "1" (fun context ->
                    context.iter.range(name,I 0,I 1) (fun i -> context.print.t i)
                    context.iter.range_exit(name,I 0,I 1) (fun (exit,_) -> exit()))
                Assert.NotEmpty(Directory.GetFileSystemEntries output.Path)
