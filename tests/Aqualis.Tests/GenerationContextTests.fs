namespace Aqualis.Tests

open System
open System.Threading.Tasks
open Xunit
open Aqualis

module GenerationContextTests =
    let private createContext path name language =
        GenerationContext [new program(path, name, language)]

    [<Fact>]
    let ``Activate restores the outer context after normal completion`` () =
        use output = new TemporaryDirectory()
        let outer = createContext output.Path "outer.tmp" C99
        let inner = createContext output.Path "inner.tmp" Python

        outer.Activate(fun () ->
            Assert.Same(outer, GenerationContext.TryCurrent.Value)

            inner.Activate(fun () ->
                Assert.Same(inner, GenerationContext.TryCurrent.Value))

            Assert.Same(outer, GenerationContext.TryCurrent.Value))

        outer.CurrentProgram.close()
        inner.CurrentProgram.close()

    [<Fact>]
    let ``Activate restores the outer context after an exception`` () =
        use output = new TemporaryDirectory()
        let outer = createContext output.Path "outer.tmp" C99
        let inner = createContext output.Path "inner.tmp" Python

        outer.Activate(fun () ->
            Assert.Throws<InvalidOperationException>(Action(fun () ->
                inner.Activate(fun () ->
                    invalidOp "expected")))
            |> ignore

            Assert.Same(outer, GenerationContext.TryCurrent.Value))

        outer.CurrentProgram.close()
        inner.CurrentProgram.close()

    [<Fact>]
    let ``WithProgram keeps the parent index immutable after an exception`` () =
        use output = new TemporaryDirectory()
        let context =
            GenerationContext [
                new program(output.Path, "first.tmp", C99)
                new program(output.Path, "second.tmp", Python)
            ]

        Assert.Equal(0, context.CurrentIndex)

        context.Activate(fun () ->
            Assert.Throws<InvalidOperationException>(Action(fun () ->
                context.WithProgram(1, fun () ->
                    Assert.Equal(0, context.CurrentIndex)
                    Assert.Equal(1, GenerationContext.TryCurrent.Value.CurrentIndex)
                    invalidOp "expected")))
            |> ignore

            Assert.Same(context, GenerationContext.TryCurrent.Value))

        Assert.Equal(0, context.CurrentIndex)
        context.Programs |> Array.iter _.close()

    [<Fact>]
    let ``WithProgram rejects an invalid index`` () =
        use output = new TemporaryDirectory()
        let context = createContext output.Path "only.tmp" C99

        Assert.Throws<ArgumentException>(Action(fun () ->
            context.WithProgram(1, ignore)))
        |> ignore

        context.CurrentProgram.close()

    [<Fact>]
    let ``compatibility accessors work without the legacy program globals`` () =
        use output = new TemporaryDirectory()
        let context =
            createContext output.Path "context-only.c" C99

        try
            context.Activate(fun () ->
                Assert.Equal(C99, language())
                Assert.Equal(C99, AqualisCompiler.language)
                Assert.Equal("context-only.c", AqualisCompiler.projectName)

                AqualisCompiler.intFormatSet 8
                AqualisCompiler.incld "<stdint.h>"
                AqualisCompiler.option "O2"
                writein "context_only = 1;")
        finally
            context.CurrentProgram.close()

        let generated =
            System.IO.File.ReadAllText(
                System.IO.Path.Combine(output.Path, "context-only.c"))

        Assert.Contains("context_only = 1;", generated)
        Assert.Contains("<stdint.h>", context.CurrentProgram.hlist.list)
        Assert.Contains("-O2", context.CurrentProgram.olist.list)

    [<Fact>]
    let ``independent generation contexts can write concurrently`` () =
        use output = new TemporaryDirectory()

        let generate filename variableName value =
            Task.Run(Action(fun () ->
                makeProgramWithContext
                    [output.Path, filename, C99]
                    (fun context ->
                        try
                            let variable = var.i0 variableName
                            variable <== value
                        finally
                            context.CurrentProgram.close())))

        let first = generate "first.c" "first_value" 1
        let second = generate "second.c" "second_value" 2
        Task.WaitAll(first, second)

        let firstCode =
            System.IO.File.ReadAllText(
                System.IO.Path.Combine(output.Path, "first.c"))
        let secondCode =
            System.IO.File.ReadAllText(
                System.IO.Path.Combine(output.Path, "second.c"))

        Assert.Contains("first_value = 1;", firstCode)
        Assert.DoesNotContain("second_value", firstCode)
        Assert.Contains("second_value = 2;", secondCode)
        Assert.DoesNotContain("first_value", secondCode)

    [<Fact>]
    let ``generation flags are isolated between contexts`` () =
        use output = new TemporaryDirectory()
        let first = createContext output.Path "flags-first.c" C99
        let second = createContext output.Path "flags-second.c" C99

        try
            first.Activate(fun () ->
                AqualisCompiler.set_DisplaySection ON
                first.IsOpenMpUsed <- true
                first.Functions.Add("first"))

            second.Activate(fun () ->
                Assert.False(second.DisplaySection)
                Assert.False(second.IsOpenMpUsed)
                Assert.Empty(second.Functions))

            Assert.True(first.DisplaySection)
            Assert.True(first.IsOpenMpUsed)
            Assert.Equal<string list>(["first"], first.DistinctFunctions)
        finally
            first.CurrentProgram.close()
            second.CurrentProgram.close()

    [<Fact>]
    let ``parallel mode is restored after an exception`` () =
        use output = new TemporaryDirectory()
        let context = createContext output.Path "parallel-mode.c" C99

        try
            context.Activate(fun () ->
                Assert.Throws<InvalidOperationException>(
                    Action(fun () ->
                        context.WithParallelMode(fun () ->
                            Assert.True(context.IsParallelMode)
                            invalidOp "expected")))
                |> ignore

                Assert.False(context.IsParallelMode))
        finally
            context.CurrentProgram.close()

    [<Fact>]
    let ``debug mode is restored after normal completion and an exception`` () =
        use output = new TemporaryDirectory()
        let context = createContext output.Path "debug-mode.c" C99

        try
            context.Activate(fun () ->
                Assert.False(context.Debug.debugMode)

                AqualisCompiler.debug(fun () ->
                    Assert.True(context.Debug.debugMode))

                Assert.False(context.Debug.debugMode)

                Assert.Throws<InvalidOperationException>(
                    Action(fun () ->
                        AqualisCompiler.debug(fun () ->
                            Assert.True(context.Debug.debugMode)
                            invalidOp "expected")))
                |> ignore

                Assert.False(context.Debug.debugMode))
        finally
            context.CurrentProgram.close()

    [<Fact>]
    let ``debug mode preserves an existing enabled state and supports nesting`` () =
        use output = new TemporaryDirectory()
        let context = createContext output.Path "nested-debug-mode.c" C99

        try
            context.Activate(fun () ->
                AqualisCompiler.set_DebugMode ON

                AqualisCompiler.debug(fun () ->
                    Assert.True(context.Debug.debugMode)

                    AqualisCompiler.debug(fun () ->
                        Assert.True(context.Debug.debugMode))

                    Assert.True(context.Debug.debugMode))

                Assert.True(context.Debug.debugMode))
        finally
            context.CurrentProgram.close()

    [<Fact>]
    let ``program switches on one parent context do not interfere`` () =
        use output = new TemporaryDirectory()
        let context =
            GenerationContext [
                new program(output.Path, "switch-first.c", C99)
                new program(output.Path, "switch-second.py", Python)
            ]

        try
            context.Activate(fun () ->
                let first =
                    Task.Run(Action(fun () ->
                        context.WithProgram(0, fun () ->
                            Assert.Equal(0, GenerationContext.TryCurrent.Value.CurrentIndex)
                            writein "first_context")))

                let second =
                    Task.Run(Action(fun () ->
                        context.WithProgram(1, fun () ->
                            Assert.Equal(1, GenerationContext.TryCurrent.Value.CurrentIndex)
                            writein "second_context")))

                Task.WaitAll(first, second)
                Assert.Equal(0, context.CurrentIndex)
                Assert.Same(context, GenerationContext.TryCurrent.Value))
        finally
            context.Programs |> Array.iter _.close()

        let firstCode =
            System.IO.File.ReadAllText(
                System.IO.Path.Combine(output.Path, "switch-first.c"))
        let secondCode =
            System.IO.File.ReadAllText(
                System.IO.Path.Combine(output.Path, "switch-second.py"))

        Assert.Contains("first_context", firstCode)
        Assert.DoesNotContain("second_context", firstCode)
        Assert.Contains("second_context", secondCode)
        Assert.DoesNotContain("first_context", secondCode)

    [<Fact>]
    let ``controllers are isolated between generation contexts`` () =
        use output = new TemporaryDirectory()
        let first = createContext output.Path "controllers-first.c" C99
        let second = createContext output.Path "controllers-second.c" C99

        try
            first.Activate(fun () ->
                AqualisCompiler.set_DebugMode ON
                first.Errors.inc()
                Assert.Equal("11", first.GotoLabels.nextGotoLabel()))

            second.Activate(fun () ->
                Assert.False(second.Debug.debugMode)
                Assert.Equal("1", second.Errors.ID)
                Assert.Equal("11", second.GotoLabels.nextGotoLabel()))

            first.Activate(fun () ->
                Assert.True(first.Debug.debugMode)
                Assert.Equal("2", first.Errors.ID)
                Assert.Equal("12", first.GotoLabels.nextGotoLabel()))
        finally
            first.CurrentProgram.close()
            second.CurrentProgram.close()
