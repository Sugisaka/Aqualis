namespace Aqualis.Tests

open System
open Xunit
open Aqualis

module ChTests =
    [<Fact>]
    let ``ch releases a scalar temporary when the callback throws`` () =
        use output = new TemporaryDirectory()

        Aqualis.makeProgramWithContext
            (output.Path, "temporary.c", C99)
            (fun context ->
                let environment = context
                let mutable firstName = ""

                Assert.Throws<InvalidOperationException>(
                    Action(fun () ->
                        environment.ch.i (fun value ->
                            firstName <- value.code
                            invalidOp "expected")))
                |> ignore

                Assert.Empty(context.i0.OnlineNumList)

                environment.ch.i (fun value ->
                    Assert.Equal(firstName, value.code)))

    [<Fact>]
    let ``ch releases every temporary from a multiple acquisition`` () =
        use output = new TemporaryDirectory()

        Aqualis.makeProgramWithContext
            (output.Path, "temporary-list.c", C99)
            (fun context ->
                let environment = context
                Assert.Throws<InvalidOperationException>(
                    Action(fun () ->
                        environment.ch.ix 3 (fun _ ->
                            invalidOp "expected")))
                |> ignore

                Assert.Empty(context.i0.OnlineNumList)
                Assert.Equal(3, context.i0.OfflineNumList.Length))

    [<Fact>]
    let ``ch2 releases an array temporary when the callback throws`` () =
        use output = new TemporaryDirectory()

        Aqualis.makeProgramWithContext
            (output.Path, "temporary-array.c", C99)
            (fun context ->
                let environment = context
                Assert.Throws<InvalidOperationException>(
                    Action(fun () ->
                        environment.ch.i01 (fun _ ->
                            invalidOp "expected")))
                |> ignore

                Assert.Empty(context.i1.OnlineNumList))

    [<Fact>]
    let ``ch releases a scalar temporary in a numeric environment`` () =
        use context = Aqualis.BlankWriter Numeric
        let mutable name = ""

        context.ch.i (fun value -> name <- value.code)

        Assert.Equal("", name)
        Assert.Empty(context.i0.OnlineNumList)
        Assert.Single(context.i0.OfflineNumList) |> ignore
