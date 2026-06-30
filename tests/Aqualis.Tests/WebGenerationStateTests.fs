namespace Aqualis.Tests

open System
open System.IO
open System.Threading.Tasks
open Xunit
open Aqualis

module WebGenerationStateTests =
    let private createContext path name =
        GenerationContext [new program(path, name, HTML)]

    [<Fact>]
    let ``movie settings are fixed when each context is created`` () =
        use output = new TemporaryDirectory()
        let disabled =
            GenerationContext(
                [new program(output.Path, "movie-disabled.tmp", HTML)],
                { Character = OFF; Subtitle = OFF; Voice = OFF })
        let defaults =
            GenerationContext(
                [new program(output.Path, "movie-default.tmp", HTML)],
                MovieSetting.Default)

        disabled.Activate(fun () ->
            html.switchCharacter())
        defaults.Activate(fun () ->
            html.switchCharacter())

        disabled.CurrentProgram.close()
        defaults.CurrentProgram.close()

        let disabledCode =
            File.ReadAllText(Path.Combine(output.Path, "movie-disabled.tmp"))
        let defaultCode =
            File.ReadAllText(Path.Combine(output.Path, "movie-default.tmp"))

        Assert.DoesNotContain("checked", disabledCode)
        Assert.Contains("checked", defaultCode)

    [<Fact>]
    let ``web counters and sequence settings are isolated between contexts`` () =
        use output = new TemporaryDirectory()
        let first = createContext output.Path "web-first.tmp"
        let second = createContext output.Path "web-second.tmp"

        try
            first.Activate(fun () ->
                Assert.Equal("contentsID0", nextContentsID())
                Assert.Equal("contentsID1", nextContentsID())
                Assert.Equal("0", nextAnimationGroup())

                setSequenceDiagramStyle {
                    TopMargin = 123.0
                    LeftMargin = 40.0
                    VarInterval = 150.0
                    SingleArrowLength = 37.5
                    VarHeaderWidth = 50.0
                    VarHeaderHeight = 20.0
                    LineWidth = 2.0
                    ActiveLineWidth = 10.0
                    FrameMargin = 10.0
                    TimeStep = 10.0
                    FrameBorder = 2.0
                    ColorActiveLine = "active"
                    ColorLoopFrame = "loop"
                    ColorBranchFrame = "branch"
                    ColorSectionFrame = "section"
                })

            second.Activate(fun () ->
                Assert.Equal("contentsID0", nextContentsID())
                Assert.Equal("0", nextAnimationGroup())
                Assert.Equal(40.0, topMargin()))

            first.Activate(fun () ->
                Assert.Equal("contentsID2", nextContentsID())
                Assert.Equal(123.0, topMargin()))
        finally
            first.CurrentProgram.close()
            second.CurrentProgram.close()

    [<Fact>]
    let ``web counters do not interfere during parallel generation`` () =
        use output = new TemporaryDirectory()

        let generate name =
            Task.Run(Func<string list>(fun () ->
                let context = createContext output.Path name
                try
                    context.Activate(fun () ->
                        [
                            nextContentsID()
                            nextContentsID()
                            nextAnimationGroup()
                        ])
                finally
                    context.CurrentProgram.close()))

        let first = generate "parallel-web-first.tmp"
        let second = generate "parallel-web-second.tmp"
        Task.WaitAll(first, second)

        Assert.Equal<string list>(
            ["contentsID0"; "contentsID1"; "0"],
            first.Result)
        Assert.Equal<string list>(
            ["contentsID0"; "contentsID1"; "0"],
            second.Result)
