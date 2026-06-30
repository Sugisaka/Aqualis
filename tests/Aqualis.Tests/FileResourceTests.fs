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

        Assert.Throws<InvalidOperationException>(
            Action(fun () ->
                svgfile.make
                    (output.Path, "image.svg")
                    (100.0, 100.0)
                    1.0
                    (fun _ -> invalidOp "expected")))
        |> ignore

        Assert.Throws<InvalidOperationException>(
            Action(fun () ->
                aiscriptfile.make
                    (output.Path, "image.jsx", 1.0)
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

        Assert.Throws<InvalidOperationException>(
            Action(fun () ->
                shellscript.makeShellScript
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

        use document =
            new TeXWriter(
                labels[0],
                labels[1],
                labels[2],
                labels[3],
                HTML,
                output.Path)

        (document :> IDisposable).Dispose()

        paths |> Array.iter assertUnlocked
