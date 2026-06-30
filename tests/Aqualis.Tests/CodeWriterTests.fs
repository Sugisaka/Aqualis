namespace Aqualis.Tests

open System
open System.IO
open Xunit
open Aqualis

module CodeWriterTests =
    [<Fact>]
    let ``disposing a code writer is idempotent and closes the file`` () =
        use output = new TemporaryDirectory()
        let path = Path.Combine(output.Path, "dispose.txt")
        let writer = new codeWriter(path, 2, C99)

        writer.codewritein "content"
        (writer :> IDisposable).Dispose()
        (writer :> IDisposable).Dispose()

        Assert.Throws<ObjectDisposedException>(
            Action(fun () -> writer.codewritein "closed"))
        |> ignore

        use reopened =
            new FileStream(
                path,
                FileMode.Open,
                FileAccess.ReadWrite,
                FileShare.None)

        Assert.True(reopened.Length > 0L)

    [<Fact>]
    let ``appendOpen and deleteOpen replace the owned stream safely`` () =
        use output = new TemporaryDirectory()
        let path = Path.Combine(output.Path, "reopen.txt")
        use writer = new codeWriter(path, 2, C99)

        writer.codewritein "first"
        writer.appendOpen()
        writer.codewritein "second"
        writer.deleteOpen()
        writer.codewritein "replacement"
        writer.close()

        Assert.Equal(
            "replacement\n",
            File.ReadAllText(path))

    [<Fact>]
    let ``makeProgram releases its files when generation throws`` () =
        use output = new TemporaryDirectory()
        let path = Path.Combine(output.Path, "exception.c")

        Assert.Throws<InvalidOperationException>(
            Action(fun () ->
                makeProgram [output.Path, "exception.c", C99] <| fun () ->
                    writein "before_exception"
                    invalidOp "expected"))
        |> ignore

        use reopened =
            new FileStream(
                path,
                FileMode.Open,
                FileAccess.ReadWrite,
                FileShare.None)

        Assert.True(reopened.Length > 0L)
