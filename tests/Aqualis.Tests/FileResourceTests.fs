namespace Aqualis.Tests

open System
open System.IO
open Xunit
open Aqualis

module FileResourceTests =
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
