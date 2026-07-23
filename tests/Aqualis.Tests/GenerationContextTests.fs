namespace Aqualis.Tests

open System
open System.IO
open System.Threading.Tasks
open Xunit
open Aqualis

module GenerationContextTests =
    let private createContext path name language =
        GenerationContext [new program(path, name, language)]

    let private closeContext (context:GenerationContext) =
        context.Programs |> Array.iter _.close()

    [<Fact>]
    let ``program views have stable and distinct output identities`` () =
        use output = new TemporaryDirectory()
        let context =
            GenerationContext [
                new program(output.Path, "id-main.c", C99)
                new program(output.Path, "id-body.c", C99)
            ]

        try
            Assert.Equal(context.ContextId, context.ForProgram(0).ContextId)
            Assert.NotEqual(context.ContextId, context.ForProgram(1).ContextId)
        finally
            closeContext context

    [<Fact>]
    let ``Compile supplies generated and Numeric environments explicitly`` () =
        use output = new TemporaryDirectory()
        let mutable numericIterations = 0
        let mutable generatedVariable = ""

        Compile [C99] output.Path "explicit-environment" "1" <| fun environment ->
            Assert.True(environment.GenerationContext.IsSome)
            environment.emit.writein "/* emitted through context */"
            environment.ch.i <| fun value ->
                generatedVariable <- value.code
                value <== 0

        Compile [Numeric] output.Path "numeric-environment" "1" <| fun environment ->
            Assert.True(environment.GenerationContext.IsNone)
            environment.iter.range(0, 2) <| fun _ -> numericIterations <- numericIterations + 1

        Assert.Equal(3, numericIterations)
        let generated = File.ReadAllText(Path.Combine(output.Path, "explicit-environment.c"))
        Assert.Contains("/* emitted through context */", generated)
        Assert.Contains(generatedVariable + " = 0;", generated)

    [<Fact>]
    let ``emit is unavailable during Numeric execution`` () =
        let environment = CompilationEnvironment(None)
        Assert.Throws<InvalidOperationException>(fun () -> environment.emit.writein "invalid")
        |> ignore

    [<Fact>]
    let ``PHP compilation writes a php file through explicit services`` () =
        use output = new TemporaryDirectory()

        Compile [PHP] output.Path "page" "1" <| fun environment ->
            let value = environment.php.var "value"
            value <== environment.php.file_get_contents "data.json"
            let input = environment.form.textBox "user"
            environment.html.form "page.php" <| fun () -> input.show()

        let phpPath = Path.Combine(output.Path, "page.php")
        Assert.True(File.Exists phpPath)
        Assert.False(File.Exists(Path.Combine(output.Path, "page.c")))
        let generated = File.ReadAllText phpPath
        Assert.Contains("$value = file_get_contents", generated)
        Assert.Contains("<form", generated)

    [<Fact>]
    let ``operators functions and indexers share context validation`` () =
        use output = new TemporaryDirectory()
        let first = createContext output.Path "merge-first.c" C99
        let second = createContext output.Path "merge-second.c" C99

        try
            let x = double0(Var(Dt, "x", NaN), context=first)
            let y = double0(Var(Dt, "y", NaN), context=second)
            let rounded = asm.floor x
            Assert.Equal(first.ContextId, rounded.Context.Value.ContextId)

            Assert.Throws<InvalidOperationException>(fun () -> x + y |> ignore) |> ignore
            Assert.Throws<InvalidOperationException>(fun () -> dv x ++ dv y |> ignore) |> ignore
            Assert.Throws<InvalidOperationException>(fun () -> Or [x .< 0.0; y .< 0.0] |> ignore) |> ignore

            let values = int2(It 4, Var2(A2(2, 2), "values"), context=first)
            let foreignIndex = int0(Var(It 4, "index", NaN), context=second)
            Assert.Throws<InvalidOperationException>(fun () -> values[foreignIndex, 0] |> ignore) |> ignore

            let neutral = int2(It 4, Arx2(I 2, I 2, fun _ -> Int 1))
            let inherited = neutral + int0(Var(It 4, "value", NaN), context=first)
            Assert.Equal(first.ContextId, inherited.Context.Value.ContextId)
        finally
            closeContext first
            closeContext second

    [<Fact>]
    let ``HTML sequence callback receives the switched program environment`` () =
        use output = new TemporaryDirectory()
        let mutable callbackIndex = -1

        Compile [HTMLSequenceDiagram] output.Path "sequence-context" "1" <| fun environment ->
            callbackIndex <- environment.GenerationContext.Value.CurrentIndex

        Assert.Equal(1, callbackIndex)

    [<Fact>]
    let ``values and contexts cannot escape a Compile callback`` () =
        use output = new TemporaryDirectory()
        let mutable escapedValue:int0 option = None
        let mutable escapedContext:GenerationContext option = None

        Compile [C99] output.Path "escaped-context" "1" <| fun environment ->
            escapedContext <- environment.GenerationContext
            environment.ch.i <| fun value -> escapedValue <- Some value

        Assert.Throws<InvalidOperationException>(fun () -> escapedValue.Value.code |> ignore) |> ignore
        Assert.Throws<InvalidOperationException>(fun () -> escapedValue.Value <== 1) |> ignore
        Assert.Throws<InvalidOperationException>(fun () -> escapedContext.Value.ContextId |> ignore) |> ignore

    [<Fact>]
    let ``WithProgram passes a child context and preserves the parent view`` () =
        use output = new TemporaryDirectory()
        let context =
            GenerationContext [
                new program(output.Path, "main.c", C99)
                new program(output.Path, "body.c", C99)
            ]

        try
            let parentId = context.ContextId
            context.WithProgram(1, fun child ->
                Assert.Equal(1, child.CurrentIndex)
                Assert.NotEqual(parentId, child.ContextId)
                writein child "body")
            Assert.Equal(0, context.CurrentIndex)
            Assert.Equal(parentId, context.ContextId)
        finally
            closeContext context

    [<Fact>]
    let ``parallel and debug child modes do not mutate their parent`` () =
        use output = new TemporaryDirectory()
        let context = createContext output.Path "modes.c" C99

        try
            Assert.False(context.IsParallelMode)
            context.WithParallelMode(fun child -> Assert.True(child.IsParallelMode))
            Assert.False(context.IsParallelMode)

            Assert.False(context.Debug.debugMode)
            context.WithDebugMode(true, fun child -> Assert.True(child.Debug.debugMode))
            Assert.False(context.Debug.debugMode)
        finally
            closeContext context

    [<Fact>]
    let ``atomic generation remains usable after an exception`` () =
        use output = new TemporaryDirectory()
        let context = createContext output.Path "atomic.c" C99

        Assert.Throws<InvalidOperationException>(Action(fun () ->
            context.GenerateAtomically(fun _ -> invalidOp "expected")))
        |> ignore

        context.GenerateAtomically(fun current -> writein current "after-exception")
        closeContext context
        Assert.Contains("after-exception", File.ReadAllText(Path.Combine(output.Path, "atomic.c")))

    [<Fact>]
    let ``atomic generation serializes statement groups`` () =
        use output = new TemporaryDirectory()
        let context = createContext output.Path "atomic-groups.c" C99
        let workers = 8

        Array.init workers (fun worker ->
            Task.Run(Action(fun () ->
                context.GenerateAtomically(fun current ->
                    writein current $"begin-{worker}"
                    writein current $"end-{worker}"))))
        |> Task.WaitAll

        closeContext context
        let lines = File.ReadAllLines(Path.Combine(output.Path, "atomic-groups.c"))
        for index in 0 .. 2 .. lines.Length - 2 do
            let beginId = lines[index].Substring("begin-".Length)
            let endId = lines[index + 1].Substring("end-".Length)
            Assert.Equal(beginId, endId)
