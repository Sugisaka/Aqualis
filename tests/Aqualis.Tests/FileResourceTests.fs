namespace Aqualis.Tests

open System
open System.IO
open System.Threading
open System.Threading.Tasks
open Xunit
open Aqualis

module FileResourceTests =
    let private occurrenceCount (value:string) (text:string) =
        text.Split(value, StringSplitOptions.None).Length - 1

    let private assertUnlocked path =
        use stream =
            new FileStream(
                path,
                FileMode.Open,
                FileAccess.ReadWrite,
                FileShare.None)
        Assert.True(stream.CanWrite)

    let private withEnvironment outputPath name code =
        use environment = new Aqualis(Some outputPath, Some name, C99)
        code environment

    [<Fact>]
    let ``complex text input retains the target context`` () =
        use output = new TemporaryDirectory()

        withEnvironment output.Path "complex-read.c" <| fun environment ->
            let target = environment.var.z0 "target"

            environment.io.fileInput "input.dat" <| fun reader ->
                reader.t target

            environment.close()
            let generated =
                File.ReadAllText(
                    Path.Combine(output.Path, "complex-read.c"))
                |> TestHelpers.normalizeGeneratedCode

            Assert.Contains("target =", generated)

    [<Theory>]
    [<InlineData("html")>]
    [<InlineData("latex")>]
    let ``byte input generates the same markup as text input`` (target:string) =
        let language,extension =
            if target = "html" then HTML,"html" else LaTeX,"tex"
        let generate useByte =
            use output = new TemporaryDirectory()
            Compile [language] output.Path "read" "1" <| fun context ->
                let value = context.var.i0 "value"
                context.io.fileInput "input.dat" <| fun reader ->
                    if useByte then reader.b value else reader.tt (iv value)
            File.ReadAllText(Path.Combine(output.Path,"read." + extension))
            |> TestHelpers.normalizeGeneratedCode

        Assert.Equal(generate false,generate true)

    [<Fact>]
    let ``PHP byte input reports unsupported operation`` () =
        use output = new TemporaryDirectory()
        use context = new Aqualis(Some output.Path,Some "read.php",PHP)
        let value = context.var.i0 "value"
        let error =
            Assert.Throws<NotSupportedException>(fun () ->
                context.io.fileInput "input.dat" <| fun reader -> reader.b value)
        Assert.Contains("TextReader.b",error.Message)

    [<Fact>]
    let ``Fortran mixed text and byte reads reject separate cursors`` () =
        use output = new TemporaryDirectory()
        use context = new Aqualis(Some output.Path,Some "read.f90",Fortran)
        let value = context.var.i0 "value"
        let error =
            Assert.Throws<InvalidOperationException>(fun () ->
                context.io.fileInput "input.dat" <| fun reader ->
                    reader.t value
                    reader.b value)
        Assert.Contains("cannot share a file cursor",error.Message)

    [<Fact>]
    let ``sequential Fortran file outputs declare reused temporaries once`` () =
        use output = new TemporaryDirectory()

        Compile [Fortran] output.Path "sequential-file-output" "1" <| fun context ->
            context.io.fileOutput "first.dat" (fun _ -> ())
            context.io.fileOutput "second.dat" (fun _ -> ())

        let generated =
            File.ReadAllText(Path.Combine(output.Path, "sequential-file-output.f90"))
            |> TestHelpers.normalizeGeneratedCode

        Assert.Equal(1, occurrenceCount "character(len=:), allocatable :: t0001" generated)
        Assert.DoesNotContain("character(100)", generated)
        Assert.Contains("allocate(character(len=len('first.dat')) :: t0001)", generated)
        Assert.Contains("deallocate(t0001)", generated)
        Assert.Equal(1, occurrenceCount "integer :: f0000=" generated)
        Assert.Equal(1, occurrenceCount "integer :: f0000=10" generated)

    [<Fact>]
    let ``nested Fortran file outputs declare simultaneous temporaries once each`` () =
        use output = new TemporaryDirectory()

        Compile [Fortran] output.Path "nested-file-output" "1" <| fun context ->
            context.io.fileOutput "outer.dat" <| fun _ ->
                context.io.fileOutput "inner.dat" (fun _ -> ())

        let generated =
            File.ReadAllText(Path.Combine(output.Path, "nested-file-output.f90"))
            |> TestHelpers.normalizeGeneratedCode

        Assert.Equal(1, occurrenceCount "character(len=:), allocatable :: t0001" generated)
        Assert.Equal(1, occurrenceCount "character(len=:), allocatable :: t0002" generated)
        Assert.DoesNotContain("character(100)", generated)
        Assert.Equal(1, occurrenceCount "integer :: f0000=" generated)
        Assert.Equal(1, occurrenceCount "integer :: f0001=" generated)
        Assert.Equal(1, occurrenceCount "integer :: f0000=10" generated)
        Assert.Equal(1, occurrenceCount "integer :: f0001=11" generated)

    [<Fact>]
    let ``C file names use measured dynamic storage and escaped format literals`` () =
        use output = new TemporaryDirectory()
        let longLiteral = String.replicate 256 "x" + "-%-\"quoted\"-\\-"

        Compile [C99] output.Path "dynamic-c-file-name" "1" <| fun context ->
            let index = context.var.i0 "index"
            context.io.fileOutput (longLiteral ++ index ++ ".dat") (fun _ -> ())

        let generated =
            File.ReadAllText(Path.Combine(output.Path, "dynamic-c-file-name.c"))
            |> TestHelpers.normalizeGeneratedCode

        Assert.Contains("char *t0001 = NULL;", generated)
        Assert.DoesNotContain("[100]", generated)
        Assert.DoesNotContain("sprintf(", generated)
        Assert.Equal(2, occurrenceCount "snprintf(" generated)
        Assert.Contains("t0001_length = snprintf(NULL,0,", generated)
        Assert.Contains("%%", generated)
        Assert.Contains("\\\"quoted\\\"", generated)
        Assert.Contains("malloc((size_t)t0001_length + 1U)", generated)
        Assert.Contains("if (f0000 == NULL)", generated)
        Assert.Contains("free(t0001);", generated)
        Assert.Contains("t0001 = NULL;", generated)

    [<Fact>]
    let ``Fortran file names use deferred length allocatable storage`` () =
        use output = new TemporaryDirectory()
        let longLiteral = String.replicate 256 "x" + "O'Brien"

        Compile [Fortran] output.Path "dynamic-fortran-file-name" "1" <| fun context ->
            let index = context.var.i0 "index"
            context.io.fileOutput (longLiteral ++ index ++ ".dat") (fun _ -> ())

        let generated =
            File.ReadAllText(Path.Combine(output.Path, "dynamic-fortran-file-name.f90"))
            |> TestHelpers.normalizeGeneratedCode

        Assert.Contains("character(len=:), allocatable :: t0001", generated)
        Assert.DoesNotContain("character(100)", generated)
        Assert.Contains("allocate(character(len=len('", generated)
        Assert.Contains("O''Brien", generated)
        Assert.Contains(" + 12 + len('.dat')) :: t0001)", generated)
        Assert.Contains("deallocate(t0001)", generated)

    [<Fact>]
    let ``Python file names concatenate escaped literals and formatted integers`` () =
        use output = new TemporaryDirectory()
        let longLiteral = String.replicate 256 "x" + "-%-\"quoted\"-\\-"

        Compile [Python] output.Path "dynamic-python-file-name" "1" <| fun context ->
            let index = context.var.i0 "index"
            context.io.fileOutput (longLiteral ++ index ++ ".dat") (fun _ -> ())

        let generated =
            File.ReadAllText(Path.Combine(output.Path, "dynamic-python-file-name.py"))
            |> TestHelpers.normalizeGeneratedCode

        Assert.Contains("t0001 = \"", generated)
        Assert.Contains("\\\"quoted\\\"", generated)
        Assert.Contains("\\\\", generated)
        Assert.Contains(" + format(index, \"012d\") + ", generated)
        Assert.DoesNotContain("%(", generated)

    [<Fact>]
    let ``file input rejects a target without a context`` () =
        use output = new TemporaryDirectory()

        withEnvironment output.Path "constant-read.c" <| fun environment ->
            let error =
                Assert.Throws<InvalidOperationException>(fun () ->
                    environment.io.fileInput "input.dat" <| fun reader ->
                        reader.t (complex0(Cpx(1.0, 2.0))))

            Assert.Equal(
                "A file-read target is not associated with a GenerationContext.",
                error.Message)

    [<Fact>]
    let ``file input rejects an expression target`` () =
        use output = new TemporaryDirectory()

        withEnvironment output.Path "expression-read.c" <| fun environment ->
            let variable = environment.var.z0 "target"
            let expression = variable + 1

            let error =
                Assert.Throws<InvalidOperationException>(fun () ->
                    environment.io.fileInput "input.dat" <| fun reader ->
                        reader.t expression)

            Assert.Equal(
                "A file-read target must be a variable.",
                error.Message)

    [<Fact>]
    let ``file input rejects a variable from another context`` () =
        use output = new TemporaryDirectory()
        use first =
            new Aqualis(Some output.Path, Some "first-read.c", C99)
        use second =
            new Aqualis(Some output.Path, Some "second-read.c", C99)
        let target =
            complex0(
                Var(Zt, "target", NaN),
                context=second)

        Assert.Throws<InvalidOperationException>(fun () ->
            first.io.fileInput "input.dat" <| fun reader ->
                reader.t target)
        |> ignore

    [<Fact>]
    let ``CSS generation preserves the previous file when its callback throws`` () =
        use output = new TemporaryDirectory()
        let path = Path.Combine(output.Path, "style.css")
        File.WriteAllText(path, "original")

        Assert.Throws<InvalidOperationException>(
            Action(fun () ->
                CSSFile.make output.Path "style.css" (fun _ ->
                    invalidOp "expected")))
        |> ignore

        Assert.Equal("original", File.ReadAllText(path))
        Assert.Empty(Directory.GetFiles(output.Path, ".style.css.aqualis-*.tmp"))
        assertUnlocked path

    [<Fact>]
    let ``parallel CSS generation uses independent staging files`` () =
        use output = new TemporaryDirectory()
        use ready = new Barrier(2)
        let path = Path.Combine(output.Path, "parallel.css")

        let generate color =
            Task.Run(fun () ->
                CSSFile.make output.Path "parallel.css" <| fun css ->
                    css.add(
                        CSSdata(
                            CSSClass "marker",
                            Style [{ Key = "color"; Value = color }]))
                    Assert.True(ready.SignalAndWait(TimeSpan.FromSeconds(10.0))))

        Task.WaitAll [| generate "red"; generate "blue" |]

        let generated = File.ReadAllText(path)
        let hasRed = generated.Contains("color: red;")
        let hasBlue = generated.Contains("color: blue;")
        Assert.True(hasRed <> hasBlue)
        Assert.Contains(".marker {", generated)
        Assert.Empty(Directory.GetFiles(output.Path, ".parallel.css.aqualis-*.tmp"))
        assertUnlocked path

    [<Fact>]
    let ``SVG generation preserves non-square canvas dimensions`` () =
        use output = new TemporaryDirectory()
        let path = Path.Combine(output.Path, "non-square.svg")

        svgfile.make path (320.0, 180.0) 1.0 ignore

        let generated = File.ReadAllText(path)
        Assert.Contains("viewBox=\"0 0 320.000 180.000\"", generated)
        Assert.Contains(
            "enable-background:new 0 0 320.000 180.000;",
            generated)
        Assert.DoesNotContain("0 0 320.000 320.000", generated)

    [<Fact>]
    let ``SVG generators remove temporary files after an exception`` () =
        use output = new TemporaryDirectory()
        let svgPath = Path.Combine(output.Path, "image.svg")
        let aiPath = Path.Combine(output.Path, "image.jsx")
        File.WriteAllText(svgPath, "old-svg")
        File.WriteAllText(aiPath, "old-ai")

        Assert.Throws<InvalidOperationException>(
            Action(fun () ->
                svgfile.make
                    (output.Path+"//image.svg")
                    (100.0, 100.0)
                    1.0
                    (fun _ -> invalidOp "expected")))
        |> ignore

        Assert.Equal("old-svg", File.ReadAllText(svgPath))
        Assert.Equal("old-ai", File.ReadAllText(aiPath))
        Assert.Empty(Directory.GetFiles(output.Path, ".image.svg.aqualis-*.tmp"))
        Assert.Empty(Directory.GetFiles(output.Path, ".image.jsx.aqualis-*.tmp"))
        assertUnlocked svgPath
        assertUnlocked aiPath

    [<Fact>]
    let ``parallel SVG generation uses independent staging files`` () =
        use output = new TemporaryDirectory()
        use ready = new Barrier(2)
        let path = Path.Combine(output.Path, "parallel.svg")

        let generate width height =
            Task.Run(fun () ->
                svgfile.make path (width, height) 1.0 <| fun _ ->
                    Assert.True(ready.SignalAndWait(TimeSpan.FromSeconds(10.0))))

        Task.WaitAll
            [| generate 120.0 80.0
               generate 240.0 160.0 |]

        let generated = File.ReadAllText(path)
        let hasSmall = generated.Contains("viewBox=\"0 0 120.000 80.000\"")
        let hasLarge = generated.Contains("viewBox=\"0 0 240.000 160.000\"")
        Assert.True(hasSmall <> hasLarge)
        Assert.Contains("</svg>", generated)
        Assert.Empty(Directory.GetFiles(output.Path, ".parallel.svg.aqualis-*.tmp"))
        assertUnlocked path

    [<Fact>]
    let ``shell writer arrays are released when generation throws`` () =
        use output = new TemporaryDirectory()

        use environment =
            new Aqualis(Some output.Path, Some "resource.c", C99)

        Assert.Throws<InvalidOperationException>(
            Action(fun () ->
                shellscript.makeShellScript
                    environment
                    output.Path
                    "resource"
                    2
                    (fun _ -> invalidOp "expected")))
        |> ignore

        for index in 1..2 do
            let path =
                Path.Combine(
                    output.Path,
                    "shell_resource_" + index.ToString("00") + ".sh")
            assertUnlocked path

    [<Fact>]
    let ``TeXWriter disposes every owned label writer`` () =
        use output = new TemporaryDirectory()
        let paths =
            [|
                Path.Combine(output.Path, "fig.label")
                Path.Combine(output.Path, "equ.label")
                Path.Combine(output.Path, "tab.label")
                Path.Combine(output.Path, "code.label")
            |]
        let labels =
            paths
            |> Array.map (fun path ->
                WriteLabel(new StreamWriter(path)))

        use context =
            new Aqualis(Some output.Path, Some "document.c", C99)

        use document =
            new TeXWriter(
                context,
                labels[0],
                labels[1],
                labels[2],
                labels[3],
                HTML,
                output.Path)

        (document :> IDisposable).Dispose()

        paths |> Array.iter assertUnlocked
