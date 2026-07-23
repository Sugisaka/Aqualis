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
    let ``movie settings are fixed for each explicit environment`` () =
        use output = new TemporaryDirectory()
        let disabled =
            GenerationContext(
                [new program(output.Path, "movie-disabled.tmp", HTML)],
                { Character = OFF; Subtitle = OFF; Voice = OFF })
        let defaults =
            GenerationContext(
                [new program(output.Path, "movie-default.tmp", HTML)],
                MovieSetting.Default)

        Aqualis(Some disabled).html.switchCharacter()
        Aqualis(Some defaults).html.switchCharacter()
        disabled.CurrentProgram.close()
        defaults.CurrentProgram.close()

        let disabledCode = File.ReadAllText(Path.Combine(output.Path, "movie-disabled.tmp"))
        let defaultCode = File.ReadAllText(Path.Combine(output.Path, "movie-default.tmp"))
        Assert.DoesNotContain("checked", disabledCode)
        Assert.Contains("checked", defaultCode)

    [<Fact>]
    let ``web counters and sequence settings are isolated by context`` () =
        use output = new TemporaryDirectory()
        let first = createContext output.Path "web-first.tmp"
        let second = createContext output.Path "web-second.tmp"
        let firstEnvironment = Aqualis(Some first)
        let secondEnvironment = Aqualis(Some second)

        try
            Assert.Equal("contentsID0", firstEnvironment.htmlio.nextContentsID())
            Assert.Equal("contentsID1", firstEnvironment.htmlio.nextContentsID())
            Assert.Equal("0", firstEnvironment.htmlio.nextAnimationGroup())

            setSequenceDiagramStyle first {
                TopMargin = 123.0; LeftMargin = 40.0; VarInterval = 150.0
                SingleArrowLength = 37.5; VarHeaderWidth = 50.0; VarHeaderHeight = 20.0
                LineWidth = 2.0; ActiveLineWidth = 10.0; FrameMargin = 10.0
                TimeStep = 10.0; FrameBorder = 2.0; ColorActiveLine = "active"
                ColorLoopFrame = "loop"; ColorBranchFrame = "branch"; ColorSectionFrame = "section"
            }

            Assert.Equal("contentsID0", secondEnvironment.htmlio.nextContentsID())
            Assert.Equal("0", secondEnvironment.htmlio.nextAnimationGroup())
            Assert.Equal(40.0, topMargin second.CurrentProgram)
            Assert.Equal("contentsID2", firstEnvironment.htmlio.nextContentsID())
            Assert.Equal(123.0, topMargin first.CurrentProgram)
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
                    let environment = Aqualis(Some context)
                    [ environment.htmlio.nextContentsID()
                      environment.htmlio.nextContentsID()
                      environment.htmlio.nextAnimationGroup() ]
                finally
                    context.CurrentProgram.close()))

        let first = generate "parallel-web-first.tmp"
        let second = generate "parallel-web-second.tmp"
        Task.WaitAll(first, second)
        Assert.Equal<string list>(["contentsID0"; "contentsID1"; "0"], first.Result)
        Assert.Equal<string list>(["contentsID0"; "contentsID1"; "0"], second.Result)
