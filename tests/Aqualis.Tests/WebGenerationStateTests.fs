namespace Aqualis.Tests

open System.IO
open Xunit
open Aqualis

module WebGenerationStateTests =
    [<Fact>]
    let ``block text code emits border color separately from border width`` () =
        use output = new TemporaryDirectory()
        let fileName = "block-text-border.html"

        use context = new Aqualis(Some output.Path, Some fileName, HTML)
        context.html.blockTextcode
            Style.blank
            (position(10.0, 20.0))
            (200.0, 100.0)
            (2.5, "solid", "#123456")
            ["sample"]
        |> ignore
        context.close()

        let generated = File.ReadAllText(Path.Combine(output.Path, fileName))

        Assert.Contains("border-width: 2.5px", generated)
        Assert.Contains("border-style: solid", generated)
        Assert.Contains("border-color: #123456", generated)
        Assert.DoesNotContain("border-width: #123456", generated)

    [<Fact>]
    let ``figure SVG emits valid pixel dimensions and margins`` () =
        use output = new TemporaryDirectory()
        let fileName = "figure-pixels.html"

        use context = new Aqualis(Some output.Path, Some fileName, HTML)
        context.html.fig position.Origin <| fun (figure,_) ->
            figure.rect Style.blank (position(20.0, 30.0)) 40 50
        context.close()

        let generated = File.ReadAllText(Path.Combine(output.Path, fileName))

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
        Assert.Equal("padding: 4px 8px", (Style [padding.paddingVH(4, 8)]).code0)

    [<Fact>]
    let ``animation SVG emits valid pixel dimensions and margins`` () =
        use output = new TemporaryDirectory()

        htmlpresentation
            output.Path
            "animation-pixels"
            "Animation pixels"
            None
            (Some 640, Some 480)
            false
            (fun context ->
                context.animationManual
                    { sX = 320
                      sY = 180
                      mX = 12
                      mY = 34
                      backgroundColor = "#ffffff" }
                    position.Origin
                    (0, 0)
                    ignore)

        let generated =
            File.ReadAllText(Path.Combine(output.Path, "animation-pixels.html"))

        Assert.Contains("viewBox=\"0 0 320 180\"", generated)
        Assert.Contains("width=\"320px\"", generated)
        Assert.Contains("height=\"180px\"", generated)
        Assert.Contains("margin-left: 12px", generated)
        Assert.Contains("margin-top: 34px", generated)
        Assert.DoesNotContain("heigth=", generated)

    [<Fact>]
    let ``movie switches honor the current web context settings`` () =
        use output = new TemporaryDirectory()

        htmlpresentation output.Path "movie-disabled" "Movie" None (None, None) false <| fun context ->
            context.CharacterEnabled <- false
            context.switchCharacter()

        htmlpresentation output.Path "movie-default" "Movie" None (None, None) false <| fun context ->
            context.switchCharacter()

        let disabledCode = File.ReadAllText(Path.Combine(output.Path, "movie-disabled.html"))
        let defaultCode = File.ReadAllText(Path.Combine(output.Path, "movie-default.html"))
        Assert.DoesNotContain("checked", disabledCode)
        Assert.Contains("checked", defaultCode)

    [<Fact>]
    let ``web counters are isolated by generation context`` () =
        use output = new TemporaryDirectory()
        let mutable firstValues = []
        let mutable secondValues = []

        htmlpresentation output.Path "web-first" "First" None (None, None) false <| fun context ->
            firstValues <-
                [ context.nextContentsID()
                  context.nextContentsID()
                  context.nextAnimationGroup() ]

        htmlpresentation output.Path "web-second" "Second" None (None, None) false <| fun context ->
            secondValues <-
                [ context.nextContentsID()
                  context.nextContentsID()
                  context.nextAnimationGroup() ]

        Assert.Equal<string list>(["contentsID0"; "contentsID1"; "0"], firstValues)
        Assert.Equal<string list>(["contentsID0"; "contentsID1"; "0"], secondValues)
