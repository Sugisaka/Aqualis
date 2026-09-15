namespace Aqualis.Tests

open System
open System.IO
open Xunit
open Aqualis

module CodeWriterTests =
    [<Fact>]
    let ``HTML comments encode text instead of emitting markup`` () =
        use output = new TemporaryDirectory()
        for language,fileName in [HTML, "comment.html"; HTMLSequenceDiagram, "sequence-comment.html"] do
            let path = Path.Combine(output.Path, fileName)
            use writer = new codeWriter(path, 2, language)

            writer.comment "A&B </span><script>alert(1)</script>"
            writer.close()

            let generated = File.ReadAllText(path)
            Assert.Contains(
                "<span class=\"comment\">A&amp;B &lt;/span&gt;&lt;script&gt;alert(1)&lt;/script&gt;</span><br/>",
                generated)
            Assert.DoesNotContain("</span><script>alert(1)</script>", generated)

    [<Fact>]
    let ``internal HTML code view markup remains structured`` () =
        use output = new TemporaryDirectory()
        let path = Path.Combine(output.Path, "code-view.html")
        use context = new Aqualis(Some output.Path, Some "code-view.html", HTML)

        context.iter.range(2, 1) ignore
        context.close()

        let generated = File.ReadAllText(path)
        Assert.Contains("<summary><span class=\"op-loop\">for</span>", generated)
        Assert.DoesNotContain("&lt;summary&gt;", generated)

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
    let ``makeProgram preserves an existing file when generation throws`` () =
        use output = new TemporaryDirectory()
        let path = Path.Combine(output.Path, "exception.c")
        File.WriteAllText(path, "previous generation")

        Assert.Throws<InvalidOperationException>(
            Action(fun () ->
                Aqualis.makeProgramWithContext (output.Path, "exception.c", C99) <| fun context ->
                    context.writein "before_exception"
                    invalidOp "expected"))
        |> ignore

        Assert.Equal("previous generation", File.ReadAllText(path))
        Assert.Empty(Directory.GetFiles(output.Path, ".exception.c.aqualis-*.tmp"))

    [<Fact>]
    let ``makeProgram does not publish a new file when generation throws`` () =
        use output = new TemporaryDirectory()
        let path = Path.Combine(output.Path, "new-file.php")

        Assert.Throws<InvalidOperationException>(
            Action(fun () ->
                Aqualis.makeProgramWithContext (output.Path, "new-file.php", PHP) <| fun context ->
                    context.writein "partial output"
                    invalidOp "expected"))
        |> ignore

        Assert.False(File.Exists(path))
        Assert.Empty(Directory.GetFiles(output.Path, ".new-file.php.aqualis-*.tmp"))

    [<Fact>]
    let ``atomic code writer publishes a complete replacement`` () =
        use output = new TemporaryDirectory()
        let path = Path.Combine(output.Path, "replacement.py")
        File.WriteAllText(path, "previous generation")

        use writer = codeWriter.CreateAtomic(path, 2, Python)
        writer.codewritein "complete replacement"
        writer.publish()

        Assert.Equal("complete replacement\n", File.ReadAllText(path))
        Assert.Empty(Directory.GetFiles(output.Path, ".replacement.py.aqualis-*.tmp"))
