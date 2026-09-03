namespace Aqualis.Tests

open System
open System.IO
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

    [<Fact>]
    let ``sequential Fortran file outputs declare reused temporaries once`` () =
        use output = new TemporaryDirectory()

        Compile [Fortran] output.Path "sequential-file-output" "1" <| fun context ->
            context.io.fileOutput "first.dat" (fun _ -> ())
            context.io.fileOutput "second.dat" (fun _ -> ())

        let generated =
            File.ReadAllText(Path.Combine(output.Path, "sequential-file-output.f90"))
            |> TestHelpers.normalizeGeneratedCode

        Assert.Equal(1, occurrenceCount "character(100) :: t0001" generated)
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

        Assert.Equal(1, occurrenceCount "character(100) :: t0001" generated)
        Assert.Equal(1, occurrenceCount "character(100) :: t0002" generated)
        Assert.Equal(1, occurrenceCount "integer :: f0000=" generated)
        Assert.Equal(1, occurrenceCount "integer :: f0001=" generated)
        Assert.Equal(1, occurrenceCount "integer :: f0000=10" generated)
        Assert.Equal(1, occurrenceCount "integer :: f0001=11" generated)

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
        Assert.False(File.Exists(path + ".tmp"))
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
        Assert.False(File.Exists(svgPath + ".tmp"))
        Assert.False(File.Exists(aiPath + ".tmp"))
        assertUnlocked svgPath
        assertUnlocked aiPath

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
