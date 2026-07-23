namespace Aqualis.Tests

open System
open Xunit
open Aqualis

module ChTests =
    [<Fact>]
    let ``ch releases a scalar temporary when the callback throws`` () =
        use output = new TemporaryDirectory()

        makeProgramWithContext
            [output.Path, "temporary.c", C99]
            (fun context ->
                try
                    let environment = Aqualis(Some context)
                    let mutable firstName = ""

                    Assert.Throws<InvalidOperationException>(
                        Action(fun () ->
                            environment.ch.i (fun value ->
                                firstName <- value.code
                                invalidOp "expected")))
                    |> ignore

                    Assert.Empty(context.CurrentProgram.i0.OnlineNumList)

                    environment.ch.i (fun value ->
                        Assert.Equal(firstName, value.code))
                finally
                    context.CurrentProgram.close())

    [<Fact>]
    let ``ch releases every temporary from a multiple acquisition`` () =
        use output = new TemporaryDirectory()

        makeProgramWithContext
            [output.Path, "temporary-list.c", C99]
            (fun context ->
                try
                    let environment = Aqualis(Some context)
                    Assert.Throws<InvalidOperationException>(
                        Action(fun () ->
                            environment.ch.ix 3 (fun _ ->
                                invalidOp "expected")))
                    |> ignore

                    Assert.Empty(context.CurrentProgram.i0.OnlineNumList)
                    Assert.Equal(3, context.CurrentProgram.i0.OfflineNumList.Length)
                finally
                    context.CurrentProgram.close())

    [<Fact>]
    let ``ch2 releases an array temporary when the callback throws`` () =
        use output = new TemporaryDirectory()

        makeProgramWithContext
            [output.Path, "temporary-array.c", C99]
            (fun context ->
                try
                    let environment = Aqualis(Some context)
                    Assert.Throws<InvalidOperationException>(
                        Action(fun () ->
                            environment.ch.i01 (fun _ ->
                                invalidOp "expected")))
                    |> ignore

                    Assert.Empty(context.CurrentProgram.i1.OnlineNumList)
                finally
                    context.CurrentProgram.close())

    [<Fact>]
    let ``ch in a numeric environment fails clearly`` () =
        Assert.Throws<InvalidOperationException>(
            Action(fun () ->
                Aqualis(None).ch.i ignore))
        |> ignore
