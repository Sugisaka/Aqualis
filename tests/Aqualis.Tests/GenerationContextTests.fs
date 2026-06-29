namespace Aqualis.Tests

open System
open Xunit
open Aqualis

module GenerationContextTests =
    let private createContext path name language =
        GenerationContext [program(path, name, language)]

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
    let ``WithProgram restores the previous index after an exception`` () =
        use output = new TemporaryDirectory()
        let context =
            GenerationContext [
                program(output.Path, "first.tmp", C99)
                program(output.Path, "second.tmp", Python)
            ]

        Assert.Equal(0, context.CurrentIndex)

        Assert.Throws<InvalidOperationException>(Action(fun () ->
            context.WithProgram(1, fun () ->
                Assert.Equal(1, context.CurrentIndex)
                invalidOp "expected")))
        |> ignore

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
