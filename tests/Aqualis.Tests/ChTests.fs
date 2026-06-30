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
                    let mutable firstName = ""

                    Assert.Throws<InvalidOperationException>(
                        Action(fun () ->
                            ch.i (fun value ->
                                firstName <- value.code
                                invalidOp "expected")))
                    |> ignore

                    Assert.Empty(context.CurrentProgram.i0.OnlineNumList)

                    ch.i (fun value ->
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
                    Assert.Throws<InvalidOperationException>(
                        Action(fun () ->
                            ch.ix 3 (fun _ ->
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
                    Assert.Throws<InvalidOperationException>(
                        Action(fun () ->
                            ch.i01 (fun _ ->
                                invalidOp "expected")))
                    |> ignore

                    Assert.Empty(context.CurrentProgram.i1.OnlineNumList)
                finally
                    context.CurrentProgram.close())

    [<Fact>]
    let ``ch outside a generation context fails clearly`` () =
        Assert.Throws<InvalidOperationException>(
            Action(fun () ->
                ch.i ignore))
        |> ignore
