namespace Aqualis.Tests

open System
open System.IO
open System.Text.RegularExpressions
open System.Text.Json
open Xunit
open Aqualis

module WebGenerationStateTests =
    [<Fact>]
    let ``HTML attributes encode markup characters exactly once`` () =
        let attribute = Atr("data-value", "a&b\"c'd<e>f")

        Assert.Equal(
            "data-value=\"a&amp;b&quot;c&#39;d&lt;e&gt;f\"",
            attribute.code)
        Assert.Equal("controls", Atr("controls").code)

    [<Fact>]
    let ``HTML attributes reject invalid names`` () =
        Assert.Throws<ArgumentException>(fun () ->
            Atr("value onfocus", "alert(1)").code |> ignore)
        |> ignore

    [<Fact>]
    let ``HTML attribute payload cannot create another attribute`` () =
        use output = new TemporaryDirectory()
        let fileName = "attribute-escaping.html"

        use context = new Aqualis(Some output.Path, Some fileName, HTML)
        context.html.taga (
            "input",
            [Atr("value", "x\" autofocus onfocus=\"alert(1)")])
        context.close()

        let generated = File.ReadAllText(Path.Combine(output.Path, fileName))
        Assert.Contains(
            "value=\"x&quot; autofocus onfocus=&quot;alert(1)\"",
            generated)
        Assert.DoesNotContain("value=\"x\" autofocus", generated)

    [<Fact>]
    let ``HTML head resource attributes are encoded`` () =
        use output = new TemporaryDirectory()
        let fileName = "head-attribute-escaping.html"

        use context = new Aqualis(Some output.Path, Some fileName, HTML)
        context.html.head (
            "Title",
            "theme\" onload=\"alert(1).css",
            "application&bundle.js") ignore
        context.close()

        let generated = File.ReadAllText(Path.Combine(output.Path, fileName))
        Assert.Contains(
            "href=\"theme&quot; onload=&quot;alert(1).css\"",
            generated)
        Assert.Contains("src=\"application&amp;bundle.js\"", generated)
        Assert.DoesNotContain("href=\"theme\" onload", generated)

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

    [<Fact>]
    let ``web presentation emits one escaped title`` () =
        use output = new TemporaryDirectory()
        let projectName = "title-project"
        let title = "A&B </title><script>alert(1)</script>"

        htmlpresentation output.Path projectName title None (None, None) false ignore

        let generated = File.ReadAllText(Path.Combine(output.Path, projectName + ".html"))
        let openingTag = "<title>"
        let closingTag = "</title>"
        let titleStart = generated.IndexOf(openingTag, StringComparison.Ordinal) + openingTag.Length
        let titleEnd = generated.IndexOf(closingTag, titleStart, StringComparison.Ordinal)
        let generatedTitle = generated.Substring(titleStart, titleEnd - titleStart).Trim()

        Assert.Equal(1, Regex.Matches(generated, Regex.Escape(openingTag)).Count)
        Assert.Equal(
            "A&amp;B &lt;/title&gt;&lt;script&gt;alert(1)&lt;/script&gt;",
            generatedTitle)
        Assert.DoesNotContain("</title><script>alert(1)</script>", generated)
        Assert.DoesNotContain("<title>" + projectName + "</title>", generated)

    [<Fact>]
    let ``web media assets are copied and referenced by relative URLs`` () =
        use output = new TemporaryDirectory()
        let imagePath = Path.Combine(output.Path, "source image #1.png")
        let videoPath = Path.Combine(output.Path, "source video #1.mp4")
        let characterPath = Path.Combine(output.Path, "character image #1.png")
        File.WriteAllText(imagePath, "image")
        File.WriteAllText(videoPath, "video")
        File.WriteAllText(characterPath, "character")

        let projectName = "web assets 日本語"
        htmlpresentation output.Path projectName "Assets" None (None, None) false <| fun context ->
            context.image Style.blank imagePath
            context.video Style.blank videoPath
            context.imageA Style.blank position.Origin imagePath
            context.animationManual
                { sX = 320
                  sY = 180
                  mX = 0
                  mY = 0
                  backgroundColor = "#ffffff" }
                position.Origin
                (0, 0)
                (fun (figure,_) -> figure.image Style.blank position.Origin imagePath)
            context.page
                [{ CharacterImageFile = characterPath
                   CharacterImageStyle = "display: block;" }]
                ({ Subtitle = ""
                   Script = ""
                   AudioFileNumber = None
                   AudioSourceNumber = None }, None, "#000000")
                ignore

        let generated = File.ReadAllText(Path.Combine(output.Path, projectName + ".html"))
        let urlPrefix = "contents_web%20assets%20%E6%97%A5%E6%9C%AC%E8%AA%9E/"
        let imageUrl = urlPrefix + "source%20image%20%231.png"
        let videoUrl = urlPrefix + "source%20video%20%231.mp4"
        let characterUrl = urlPrefix + "character%20image%20%231.png"

        Assert.True(Regex.Matches(generated, Regex.Escape(imageUrl)).Count >= 3)
        Assert.Contains(videoUrl, generated)
        Assert.Contains(characterUrl, generated)
        Assert.True(File.Exists(Path.Combine(output.Path, "contents_" + projectName, Path.GetFileName imagePath)))
        Assert.True(File.Exists(Path.Combine(output.Path, "contents_" + projectName, Path.GetFileName videoPath)))
        Assert.True(File.Exists(Path.Combine(output.Path, "contents_" + projectName, Path.GetFileName characterPath)))

        Regex.Matches(generated, "src\\s*=\\s*\"([^\"]*)\"")
        |> Seq.cast<Match>
        |> Seq.map (fun matched -> matched.Groups[1].Value)
        |> Seq.filter (fun source -> source.StartsWith("contents_"))
        |> Seq.iter (fun source ->
            Assert.DoesNotContain("\\", source)
            Assert.DoesNotContain(output.Path, source))

    [<Fact>]
    let ``explicit web asset context supports shared assets in PHP pages`` () =
        use output = new TemporaryDirectory()
        let imagePath = Path.Combine(output.Path, "source image #1.png")
        File.WriteAllText(imagePath, "image")
        let assets = WebAssetContext(output.Path, "shared images")

        Compile [PHP] output.Path "first-page" "1.0" <| fun context ->
            context.html.image (assets, [Atr("class", "cimage")], imagePath)

        Compile [PHP] output.Path "second-page" "1.0" <| fun context ->
            context.html.image (assets, Style [size.maxWidth "100%"], imagePath)

        let firstPage = File.ReadAllText(Path.Combine(output.Path, "first-page.php"))
        let secondPage = File.ReadAllText(Path.Combine(output.Path, "second-page.php"))
        let imageUrl = "shared%20images/source%20image%20%231.png"

        Assert.Contains("class=\"cimage\"", firstPage)
        Assert.Contains("src=\"" + imageUrl + "\"", firstPage)
        Assert.Contains("style=\"max-width: 100%\"", secondPage)
        Assert.Contains("src=\"" + imageUrl + "\"", secondPage)
        Assert.Equal("image", File.ReadAllText(Path.Combine(assets.ContentsDirectory, "source image #1.png")))

    [<Fact>]
    let ``explicit image assets reject caller supplied src attributes`` () =
        use output = new TemporaryDirectory()
        let imagePath = Path.Combine(output.Path, "source.png")
        File.WriteAllText(imagePath, "image")
        let assets = WebAssetContext(output.Path, "images")
        use context = new Aqualis(Some output.Path, Some "page.php", PHP)

        Assert.Throws<ArgumentException>(fun () ->
            context.html.image (assets, [Atr("src", "unmanaged.png")], imagePath))
        |> ignore

    [<Fact>]
    let ``same-named web assets receive distinct URLs without being overwritten`` () =
        use output = new TemporaryDirectory()
        let firstDirectory = Path.Combine(output.Path, "first")
        let secondDirectory = Path.Combine(output.Path, "second")
        Directory.CreateDirectory(firstDirectory) |> ignore
        Directory.CreateDirectory(secondDirectory) |> ignore
        let firstPath = Path.Combine(firstDirectory, "shared.png")
        let secondPath = Path.Combine(secondDirectory, "shared.png")
        File.WriteAllText(firstPath, "first image")
        File.WriteAllText(secondPath, "second image")

        let projectName = "asset-collision"
        htmlpresentation output.Path projectName "Assets" None (None, None) false <| fun context ->
            context.image firstPath
            context.image firstPath
            context.image secondPath

        let contentsDirectory = Path.Combine(output.Path, "contents_" + projectName)
        let generated = File.ReadAllText(Path.Combine(output.Path, projectName + ".html"))
        let firstUrl = "contents_asset-collision/shared.png"
        let secondUrl = "contents_asset-collision/shared-2.png"

        Assert.Equal("first image", File.ReadAllText(Path.Combine(contentsDirectory, "shared.png")))
        Assert.Equal("second image", File.ReadAllText(Path.Combine(contentsDirectory, "shared-2.png")))
        Assert.Equal(2, Regex.Matches(generated, Regex.Escape(firstUrl)).Count)
        Assert.Equal(1, Regex.Matches(generated, Regex.Escape(secondUrl)).Count)
        Assert.False(File.Exists(Path.Combine(contentsDirectory, "shared-3.png")))

    [<Fact>]
    let ``audio file names are emitted as safe JavaScript strings`` () =
        use output = new TemporaryDirectory()
        let projectName = "audio-escaping"
        let audioFile = "voice\"\\\r\n</script><script>alert(1)</script>.wav"

        fixedPage output.Path projectName "Audio" 640 480 None <| fun context ->
            context.page
                []
                ({ Subtitle = ""
                   Script = ""
                   AudioFileNumber = None
                   AudioSourceNumber = None }, Some audioFile, "#000000")
                ignore

        let generated = File.ReadAllText(Path.Combine(output.Path, projectName + ".html"))
        let prefix = "const audioList = "
        let audioListLine =
            generated.Split([| "\r\n"; "\n" |], StringSplitOptions.None)
            |> Array.find (fun line -> line.StartsWith(prefix, StringComparison.Ordinal))
        let audioListJson =
            audioListLine.Substring(prefix.Length).TrimEnd(';')
        let decoded = JsonSerializer.Deserialize<string array>(audioListJson)

        Assert.Equal<string>(audioFile, Assert.Single(decoded))
        Assert.DoesNotContain("</script><script>alert(1)</script>", generated)

    [<Fact>]
    let ``missing web media asset stops generation`` () =
        use output = new TemporaryDirectory()
        let missingPath = Path.Combine(output.Path, "missing.png")

        let error =
            Assert.Throws<FileNotFoundException>(fun () ->
                htmlpresentation output.Path "missing-asset" "Missing" None (None, None) false <| fun context ->
                    context.image missingPath)

        Assert.Equal(missingPath, error.FileName)
