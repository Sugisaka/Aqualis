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
        let context =
            GenerationContext [new program(outputPath, name, C99)]
        let environment = CompilationEnvironment(Some context)

        try
            code context environment
        finally
            context.CurrentProgram.close()
            context.Deactivate()

    [<Fact>]
    let ``complex text input retains the target context`` () =
        use output = new TemporaryDirectory()

        withEnvironment output.Path "complex-read.c" <| fun context environment ->
            let target = environment.var.z0 "target"

            environment.io.fileInput "input.dat" <| fun reader ->
                reader.t target

            context.CurrentProgram.close()
            let generated =
                File.ReadAllText(
                    Path.Combine(output.Path, "complex-read.c"))
                |> TestHelpers.normalizeGeneratedCode

            Assert.Contains("target =", generated)

    [<Fact>]
    let ``file input rejects a target without a context`` () =
        use output = new TemporaryDirectory()

        withEnvironment output.Path "constant-read.c" <| fun _ environment ->
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

        withEnvironment output.Path "expression-read.c" <| fun _ environment ->
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
        let first =
            GenerationContext [
                new program(output.Path, "first-read.c", C99)
            ]
        let second =
            GenerationContext [
                new program(output.Path, "second-read.c", C99)
            ]
        let environment = CompilationEnvironment(Some first)
        let target =
            complex0(
                Var(Zt, "target", NaN),
                context=second)

        try
            Assert.Throws<InvalidOperationException>(fun () ->
                environment.io.fileInput "input.dat" <| fun reader ->
                    reader.t target)
            |> ignore
        finally
            first.CurrentProgram.close()
            first.Deactivate()
            second.CurrentProgram.close()
            second.Deactivate()

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
    let ``SVG and AI generators remove temporary files after an exception`` () =
        use output = new TemporaryDirectory()
        let svgPath = Path.Combine(output.Path, "image.svg")
        let aiPath = Path.Combine(output.Path, "image.jsx")
        File.WriteAllText(svgPath, "old-svg")
        File.WriteAllText(aiPath, "old-ai")

        let context = GenerationContext [new program(output.Path, "resources.c", C99)]
        let environment = CompilationEnvironment(Some context)

        try
            Assert.Throws<InvalidOperationException>(
                Action(fun () ->
                    environment.svgfile.make
                        (output.Path, "image.svg")
                        (100.0, 100.0)
                        1.0
                        (fun _ -> invalidOp "expected")))
            |> ignore

            Assert.Throws<InvalidOperationException>(
                Action(fun () ->
                    environment.aiscriptfile.make
                        (output.Path, "image.jsx", 1.0)
                        (fun _ -> invalidOp "expected")))
            |> ignore
        finally
            context.CurrentProgram.close()
            context.Deactivate()

        Assert.Equal("old-svg", File.ReadAllText(svgPath))
        Assert.Equal("old-ai", File.ReadAllText(aiPath))
        Assert.False(File.Exists(svgPath + ".tmp"))
        Assert.False(File.Exists(aiPath + ".tmp"))
        assertUnlocked svgPath
        assertUnlocked aiPath

    [<Fact>]
    let ``shell writer arrays are released when generation throws`` () =
        use output = new TemporaryDirectory()

        let context = GenerationContext [new program(output.Path, "resource.c", C99)]
        let environment = CompilationEnvironment(Some context)

        try
            Assert.Throws<InvalidOperationException>(
                Action(fun () ->
                    shellscript.makeShellScript
                        environment
                        output.Path
                        "resource"
                        2
                        (fun _ -> invalidOp "expected")))
            |> ignore
        finally
            context.CurrentProgram.close()
            context.Deactivate()

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

        let context = GenerationContext [new program(output.Path, "document.c", C99)]

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
        context.CurrentProgram.close()
        context.Deactivate()

        paths |> Array.iter assertUnlocked
