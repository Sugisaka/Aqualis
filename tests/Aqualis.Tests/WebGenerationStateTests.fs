namespace Aqualis.Tests

open System
open System.IO
open System.Text.RegularExpressions
open System.Text.Json
open Xunit
open Aqualis

type private TestCharacter(context:HtmlGenerationContext, directory:string, name:string) =
    inherit Character(context, directory, name)
    override _.audioFile _ = None
    override _.scriptFile number =
        Path.Combine(directory, name + "_" + string number + ".txt")
    override _.scriptColor = "#000000"

module WebGenerationStateTests =
    [<Theory>]
    [<InlineData(null)>]
    [<InlineData("")>]
    [<InlineData(" ")>]
    [<InlineData("../outside")>]
    [<InlineData("nested/name")>]
    [<InlineData(@"nested\name")>]
    [<InlineData(@"C:\outside")>]
    [<InlineData(".")>]
    [<InlineData("..")>]
    [<InlineData("CON")>]
    [<InlineData("LPT1.txt")>]
    [<InlineData("name.")>]
    [<InlineData("bad:name")>]
    let ``Character rejects names that are unsafe as file-name segments`` (name:string) =
        use output = new TemporaryDirectory()
        use context = new HtmlGenerationContext(output.Path, "character-name-test")

        let error =
            Assert.ThrowsAny<ArgumentException>(fun () ->
                TestCharacter(context, output.Path, name) |> ignore)

        Assert.Equal("name", error.ParamName)

    [<Fact>]
    let ``Character rejects names that exceed the portable UTF-8 length limit`` () =
        use output = new TemporaryDirectory()
        use context = new HtmlGenerationContext(output.Path, "character-name-test")
        let name = String.replicate 201 "a"

        let error =
            Assert.Throws<ArgumentException>(fun () ->
                TestCharacter(context, output.Path, name) |> ignore)

        Assert.Equal("name", error.ParamName)

    [<Fact>]
    let ``Character rejects path traversal before writing outside its directory`` () =
        use output = new TemporaryDirectory()
        use context = new HtmlGenerationContext(output.Path, "character-name-test")
        let escapedName = "../escaped-" + Guid.NewGuid().ToString("N")
        let escapedPath = Path.GetFullPath(Path.Combine(output.Path, escapedName + ".json"))

        Assert.Throws<ArgumentException>(fun () ->
            TestCharacter(context, output.Path, escapedName) |> ignore)
        |> ignore

        Assert.False(File.Exists escapedPath)

    [<Fact>]
    let ``Character accepts a Unicode file-name segment and writes within its directory`` () =
        use output = new TemporaryDirectory()
        use context = new HtmlGenerationContext(output.Path, "character-name-test")
        let character = TestCharacter(context, output.Path, "テール右")

        character.script("字幕", "読み上げ") |> ignore
        character.saveScriptData()

        Assert.Equal("テール右", character.Name)
        Assert.True(File.Exists(Path.Combine(output.Path, "テール右.json")))
        Assert.True(File.Exists(Path.Combine(output.Path, "テール右_0.txt")))

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
    let ``HTML head requires validated and encoded resource URLs`` () =
        use output = new TemporaryDirectory()
        let fileName = "head-attribute-escaping.html"

        use context = new Aqualis(Some output.Path, Some fileName, HTML)
        context.html.head (
            "Title",
            Url.relative "theme.css?x=1&y=2",
            Url.relative "application.js?x=1&y=2") ignore
        context.close()

        let generated = File.ReadAllText(Path.Combine(output.Path, fileName))
        Assert.Contains("href=\"theme.css?x=1&amp;y=2\"", generated)
        Assert.Contains("src=\"application.js?x=1&amp;y=2\"", generated)

    [<Fact>]
    let ``all HTML head overloads keep metadata inside the head element`` () =
        use output = new TemporaryDirectory()

        let generate fileName (render:Aqualis -> unit) =
            use context = new Aqualis(Some output.Path, Some fileName, HTML)
            render context
            context.close()
            File.ReadAllText(Path.Combine(output.Path, fileName))

        let pages =
            [ generate "head-basic.html" (fun context ->
                  context.html.head "Title" ignore)
              generate "head-refresh.html" (fun context ->
                  context.html.head("Title", 5) ignore)
              generate "head-css.html" (fun context ->
                  context.html.head("Title", Url.relative "site.css") ignore)
              generate "head-assets.html" (fun context ->
                  context.html.head("Title", Url.relative "site.css", Url.relative "site.js") ignore)
              generate "head-assets-refresh.html" (fun context ->
                  context.html.head("Title", Url.relative "site.css", Url.relative "site.js", 5) ignore) ]

        for generated in pages do
            let htmlIndex = generated.IndexOf("<html", StringComparison.Ordinal)
            let headIndex = generated.IndexOf("<head>", StringComparison.Ordinal)
            let charsetIndex = generated.IndexOf("<meta charset=", StringComparison.Ordinal)
            let titleIndex = generated.IndexOf("<title>", StringComparison.Ordinal)
            let headEndIndex = generated.IndexOf("</head>", StringComparison.Ordinal)
            let bodyIndex = generated.IndexOf("<body>", StringComparison.Ordinal)

            Assert.Equal(1, Regex.Matches(generated, Regex.Escape("<head>")).Count)
            Assert.Equal(1, Regex.Matches(generated, Regex.Escape("</head>")).Count)
            Assert.True(htmlIndex < headIndex)
            Assert.True(headIndex < charsetIndex)
            Assert.True(charsetIndex < titleIndex)
            Assert.True(titleIndex < headEndIndex)
            Assert.True(headEndIndex < bodyIndex)

            for metadata in Regex.Matches(generated, "<meta\\b") do
                Assert.True(headIndex < metadata.Index && metadata.Index < headEndIndex)

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
    let ``HTML head and text helpers encode markup exactly once`` () =
        use output = new TemporaryDirectory()
        let fileName = "html-text-escaping.html"
        let payload = "A&B </title><script>alert(1)</script>"

        use context = new Aqualis(Some output.Path, Some fileName, HTML)
        context.html.head payload <| fun () ->
            context.html.h1 payload ignore
            context.html.para payload
            context.print.s payload
            context.html.rawHtml "<em>trusted</em>"
        context.close()

        let generated = File.ReadAllText(Path.Combine(output.Path, fileName))
        let encoded = "A&amp;B &lt;/title&gt;&lt;script&gt;alert(1)&lt;/script&gt;"
        Assert.Contains("<title>" + encoded + "</title>", generated)
        Assert.Contains(encoded, generated)
        Assert.DoesNotContain("A&amp;amp;B", generated)
        Assert.DoesNotContain("</title><script>alert(1)</script>", generated)
        Assert.Contains("<em>trusted</em>", generated)

    [<Fact>]
    let ``HTML compiler encodes project metadata`` () =
        use output = new TemporaryDirectory()
        let projectName = "A&B"
        let version = "1</li><script>alert(1)</script>"

        Compile [HTML] output.Path projectName version ignore

        let generated = File.ReadAllText(Path.Combine(output.Path, projectName + ".html"))
        Assert.Contains("<title>A&amp;B</title>", generated)
        Assert.Contains("<h1>A&amp;B</h1>", generated)
        Assert.Contains("Project version: 1&lt;/li&gt;&lt;script&gt;alert(1)&lt;/script&gt;", generated)
        Assert.DoesNotContain("</li><script>alert(1)</script>", generated)
        Assert.Contains(
            "https://fonts.googleapis.com/css2?family=Noto+Sans+JP:wght@500;600;700&display=swap",
            generated)
        Assert.DoesNotContain("family=Noto + Sans + JP", generated)

        let sequenceProjectName = "sequence&A"
        Compile [HTMLSequenceDiagram] output.Path sequenceProjectName "1" ignore
        let sequenceGenerated =
            File.ReadAllText(Path.Combine(output.Path, sequenceProjectName + ".html"))
        Assert.Contains("<title>", sequenceGenerated)
        Assert.Contains("sequence&amp;A", sequenceGenerated)
        Assert.DoesNotContain("<title>sequence&A</title>", sequenceGenerated)

    [<Fact>]
    let ``document headings captions and cells encode HTML text`` () =
        use output = new TemporaryDirectory()
        let fileName = "document-text-escaping.html"
        let payload = "A&B </h1><script>alert(1)</script>"

        use context = new Aqualis(Some output.Path, Some fileName, HTML)
        let document =
            new TeXWriter(
                context,
                ReadLabel [],
                ReadLabel [],
                ReadLabel [],
                ReadLabel [],
                HTML,
                output.Path)
        document.title payload
        document.section payload ignore
        document.table "table" "" "" payload [[payload]]
        (document :> IDisposable).Dispose()
        context.close()

        let generated = File.ReadAllText(Path.Combine(output.Path, fileName))
        let encoded = "A&amp;B &lt;/h1&gt;&lt;script&gt;alert(1)&lt;/script&gt;"
        Assert.True(Regex.Matches(generated, Regex.Escape(encoded)).Count >= 4)
        Assert.DoesNotContain("</h1><script>alert(1)</script>", generated)

    [<Fact>]
    let ``PHP headings escape dynamic values at runtime`` () =
        use output = new TemporaryDirectory()
        let fileName = "php-heading-escaping.php"

        use context = new Aqualis(Some output.Path, Some fileName, PHP)
        let heading = PHPdata.var(context, "heading")
        context.html.h1 heading ignore
        context.close()

        let generated = File.ReadAllText(Path.Combine(output.Path, fileName))
        Assert.Contains(
            "htmlspecialchars((string)($heading), ENT_QUOTES | ENT_SUBSTITUTE, 'UTF-8')",
            generated)

    [<Fact>]
    let ``PHP code blocks escape dynamic values at runtime`` () =
        use output = new TemporaryDirectory()
        let fileName = "php-code-block-escaping.php"

        use context = new Aqualis(Some output.Path, Some fileName, PHP)
        let submittedAnswer = PHPdata.var(context, "submittedAnswer")
        context.html.code submittedAnswer
        context.close()

        let generated = File.ReadAllText(Path.Combine(output.Path, fileName))
        Assert.Contains(
            "htmlspecialchars((string)($submittedAnswer), ENT_QUOTES | ENT_SUBSTITUTE, 'UTF-8')",
            generated)
        Assert.DoesNotContain("<?php echo $submittedAnswer ?>", generated)

    [<Fact>]
    let ``PHP text areas escape dynamic values at runtime`` () =
        use output = new TemporaryDirectory()
        let fileName = "php-text-area-escaping.php"

        use context = new Aqualis(Some output.Path, Some fileName, PHP)
        let submittedAnswer = PHPdata.var(context, "submittedAnswer")
        let answerField = TextArea(context, "answer")
        answerField.show_value(
            submittedAnswer,
            [Atr("class", "textmessage"); Atr("rows", "3")])
        context.close()

        let generated = File.ReadAllText(Path.Combine(output.Path, fileName))
        Assert.Contains("<textarea", generated)
        Assert.Contains(
            "htmlspecialchars((string)($submittedAnswer), ENT_QUOTES | ENT_SUBSTITUTE, 'UTF-8')",
            generated)
        Assert.DoesNotContain("<?php echo $submittedAnswer ?>", generated)

    [<Fact>]
    let ``typed PHP table cells render dynamic values with runtime HTML escaping`` () =
        use output = new TemporaryDirectory()
        let fileName = "php-table-cells.php"

        use context = new Aqualis(Some output.Path, Some fileName, PHP)
        let dynamicValue = PHPdata.var(context, "tableValue")
        context.html.listTableCells "A&B" [TdC; TdC] [TrB]
            [[TableCell.Text "<static>"; TableCell.Php dynamicValue]]
        context.close()

        let generated = File.ReadAllText(Path.Combine(output.Path, fileName))
        Assert.Contains("A&amp;B", generated)
        Assert.Contains("&lt;static&gt;", generated)
        Assert.Contains(
            "htmlspecialchars((string)($tableValue), ENT_QUOTES | ENT_SUBSTITUTE, 'UTF-8')",
            generated)
        Assert.DoesNotContain("&lt;?php", generated)

    [<Fact>]
    let ``HTML helpers reject invalid element names`` () =
        use context = Aqualis.BlankWriter HTML

        Assert.Throws<ArgumentException>(fun () ->
            context.html.tagb "div><script" ignore)
        |> ignore

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
    let ``existing web assets with different content are preserved`` () =
        use output = new TemporaryDirectory()
        let sourceDirectory = Path.Combine(output.Path, "source")
        let contentsDirectory = Path.Combine(output.Path, "images")
        Directory.CreateDirectory(sourceDirectory) |> ignore
        Directory.CreateDirectory(contentsDirectory) |> ignore
        let sourcePath = Path.Combine(sourceDirectory, "shared.png")
        let existingPath = Path.Combine(contentsDirectory, "shared.png")
        File.WriteAllText(sourcePath, "new image")
        File.WriteAllText(existingPath, "existing image")

        let firstContext = WebAssetContext(output.Path, "images")
        let firstUrl = firstContext.Import sourcePath
        let repeatedUrl = firstContext.Import sourcePath

        Assert.Equal("images/shared-2.png", firstUrl)
        Assert.Equal(firstUrl, repeatedUrl)
        Assert.Equal("existing image", File.ReadAllText(existingPath))
        Assert.Equal("new image", File.ReadAllText(Path.Combine(contentsDirectory, "shared-2.png")))

        let secondContext = WebAssetContext(output.Path, "images")
        Assert.Equal(firstUrl, secondContext.Import sourcePath)
        Assert.False(File.Exists(Path.Combine(contentsDirectory, "shared-3.png")))

    [<Fact>]
    let ``existing web assets with matching content are reused`` () =
        use output = new TemporaryDirectory()
        let sourceDirectory = Path.Combine(output.Path, "source")
        let contentsDirectory = Path.Combine(output.Path, "images")
        Directory.CreateDirectory(sourceDirectory) |> ignore
        Directory.CreateDirectory(contentsDirectory) |> ignore
        let sourcePath = Path.Combine(sourceDirectory, "shared.png")
        let existingPath = Path.Combine(contentsDirectory, "shared.png")
        File.WriteAllText(sourcePath, "same image")
        File.WriteAllText(existingPath, "same image")

        let assets = WebAssetContext(output.Path, "images")

        Assert.Equal("images/shared.png", assets.Import sourcePath)
        Assert.Equal("same image", File.ReadAllText(existingPath))
        Assert.False(File.Exists(Path.Combine(contentsDirectory, "shared-2.png")))

    [<Fact>]
    let ``web asset allocation skips occupied files and directories`` () =
        use output = new TemporaryDirectory()
        let sourceDirectory = Path.Combine(output.Path, "source")
        let contentsDirectory = Path.Combine(output.Path, "images")
        Directory.CreateDirectory(sourceDirectory) |> ignore
        Directory.CreateDirectory(contentsDirectory) |> ignore
        let sourcePath = Path.Combine(sourceDirectory, "shared.png")
        File.WriteAllText(sourcePath, "new image")
        File.WriteAllText(Path.Combine(contentsDirectory, "shared.png"), "first")
        File.WriteAllText(Path.Combine(contentsDirectory, "shared-2.png"), "second")
        Directory.CreateDirectory(Path.Combine(contentsDirectory, "shared-3.png")) |> ignore

        let assets = WebAssetContext(output.Path, "images")

        Assert.Equal("images/shared-4.png", assets.Import sourcePath)
        Assert.Equal("first", File.ReadAllText(Path.Combine(contentsDirectory, "shared.png")))
        Assert.Equal("second", File.ReadAllText(Path.Combine(contentsDirectory, "shared-2.png")))
        Assert.True(Directory.Exists(Path.Combine(contentsDirectory, "shared-3.png")))
        Assert.Equal("new image", File.ReadAllText(Path.Combine(contentsDirectory, "shared-4.png")))

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
