namespace Aqualis.Tests

open System
open System.IO
open Xunit
open Aqualis

module CodeWriterTests =
    let private outputLanguages =
        [ Fortran; C99; LaTeX; HTML; HTMLSequenceDiagram; Python; JavaScript; PHP ]

    [<Fact>]
    let ``code writing layouts are shared by every output language`` () =
        use output = new TemporaryDirectory()
        let text = "first\n\nsecond\n"
        let cases : (string * (codeWriter -> unit) * string) list =
            [ "codewrite", (fun writer -> writer.codewrite text), "firstsecond"
              "codewritei", (fun writer -> writer.codewritei text), "  first  second"
              "codewritei-header", (fun writer -> writer.codewritei(">", text)), ">  first>  second"
              "codewriten", (fun writer -> writer.codewriten text), "first\nsecond\n"
              "codewritein", (fun writer -> writer.codewritein text), "  first\n  second\n"
              "codewritein-header", (fun writer -> writer.codewritein(">", text)), ">  first\n>  second\n" ]

        for language in outputLanguages do
            for caseName,write,expected in cases do
                let path = Path.Combine(output.Path, $"{language}-{caseName}.txt")
                use writer = new codeWriter(path, 2, language)
                writer.indent.inc()

                write writer
                writer.close()

                Assert.Equal(expected, File.ReadAllText(path))

    [<Fact>]
    let ``comments retain only their language-specific formatting`` () =
        use output = new TemporaryDirectory()
        let cases =
            [ Fortran, "  !A&B\n"
              C99, "  /*A&B*/\n"
              LaTeX, "  %A&B\n"
              HTML, "  <span class=\"comment\">A&amp;B</span><br/>\n"
              HTMLSequenceDiagram, "  <span class=\"comment\">A&amp;B</span><br/>\n"
              Python, "  #A&B\n"
              JavaScript, "  //A&B\n"
              PHP, "  /*A&B*/\n" ]

        for language,expected in cases do
            let path = Path.Combine(output.Path, $"{language}-comment.txt")
            use writer = new codeWriter(path, 2, language)
            writer.indent.inc()

            writer.comment "A&B"
            writer.close()

            Assert.Equal(expected, File.ReadAllText(path))

    [<Fact>]
    let ``Numeric code writer keeps every writing operation as a no-op`` () =
        use output = new TemporaryDirectory()
        let path = Path.Combine(output.Path, "numeric.txt")
        use writer = new codeWriter(path, 2, Numeric)
        writer.indent.inc()

        writer.codewrite "raw"
        writer.codewritei "indented"
        writer.codewritei(">", "headed")
        writer.codewriten "line"
        writer.codewritein "indented line"
        writer.codewritein(">", "headed line")
        writer.comment "comment"
        writer.close()

        Assert.Equal("", File.ReadAllText(path))

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
