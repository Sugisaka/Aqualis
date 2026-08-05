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
    let ``block text code emits border color separately from border width`` () =
        use output = new TemporaryDirectory()
        let fileName = "block-text-border.html"
        let context = createContext output.Path fileName

        try
            let environment = Aqualis(Some context)
            environment.html.blockTextcode
                Style.blank
                (position(10.0, 20.0))
                (200.0, 100.0)
                (2.5, "solid", "#123456")
                ["sample"]
            |> ignore
        finally
            context.CurrentProgram.close()
            context.Deactivate()

        let generated =
            File.ReadAllText(Path.Combine(output.Path, fileName))

        Assert.Contains("border-width: 2.5px", generated)
        Assert.Contains("border-style: solid", generated)
        Assert.Contains("border-color: #123456", generated)
        Assert.DoesNotContain("border-width: #123456", generated)

    [<Fact>]
    let ``figure SVG emits valid pixel dimensions and margins`` () =
        use output = new TemporaryDirectory()
        let fileName = "figure-pixels.html"
        let context = createContext output.Path fileName

        try
            let environment = Aqualis(Some context)
            environment.html.fig position.Origin <| fun (figure,_) ->
                figure.rect
                    Style.blank
                    (position(20.0, 30.0))
                    40
                    50
        finally
            context.CurrentProgram.close()
            context.Deactivate()

        let generated =
            File.ReadAllText(Path.Combine(output.Path, fileName))

        Assert.Contains("viewBox=\"0 0 60 70\"", generated)
        Assert.Contains("width=\"60px\"", generated)
        Assert.Contains("height=\"70px\"", generated)
        Assert.Contains("margin-left: 10px", generated)
        Assert.Contains("margin-top: 20px", generated)
        Assert.DoesNotContain("heigth=", generated)
        Assert.DoesNotContain("margin-left: 10;", generated)
        Assert.DoesNotContain("margin-top: 20;", generated)

    [<Fact>]
    let ``CSS pixel helpers use valid invariant values`` () =
        Assert.Equal("12.5px", CssLength.pixels 12.5)
        Assert.Equal("8px", CssLength.pixelsInt 8)
        Assert.Equal(
            "padding: 4px 8px",
            (Style [padding.paddingVH(4, 8)]).code0)

    [<Fact>]
    let ``animation SVG emits valid pixel dimensions and margins`` () =
        use output = new TemporaryDirectory()

        fixedPage
            output.Path
            "animation-pixels"
            "Animation pixels"
            640
            480
            MovieSetting.Default
            None
            (fun environment ->
                environment.html.page
                    []
                    ({ Subtitle = "subtitle"
                       Script = ""
                       AudioFileNumber = None
                       AudioSourceNumber = None },
                     None,
                     "#000000")
                    ignore
                environment.html.animationManual
                    { sX = 320
                      sY = 180
                      mX = 12
                      mY = 34
                      backgroundColor = "#ffffff" }
                    position.Origin
                    (0, 0)
                    ignore)

        let generated =
            File.ReadAllText(
                Path.Combine(output.Path, "animation-pixels.html"))

        Assert.Contains("viewBox=\"0 0 320 180\"", generated)
        Assert.Contains("width=\"320px\"", generated)
        Assert.Contains("height=\"180px\"", generated)
        Assert.Contains("margin-left: 12px", generated)
        Assert.Contains("margin-top: 34px", generated)
        Assert.Contains("font-size: 48px", generated)
        Assert.DoesNotContain("heigth=", generated)
        Assert.DoesNotContain("font-size: 36pt", generated)

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
