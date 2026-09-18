namespace Aqualis.Tests

open System
open System.IO
open System.Text.RegularExpressions
open Xunit
open Aqualis

module GenerationContextTests =
    let private createContext path name language =
        new Aqualis(Some path, Some name, language)

    let private transactionDirectories path =
        Directory.EnumerateDirectories(path, ".aqualis-*", SearchOption.TopDirectoryOnly)

    let private generatedManifest path projectName =
        Path.Combine(path, ".aqualis-generated-" + projectName + ".json")

    [<Fact>]
    let ``Compile publishes no files when a later language fails`` () =
        use output = new TemporaryDirectory()
        let projectName = "transaction-new"

        Assert.Throws<InvalidOperationException>(fun () ->
            Compile [C99; Python] output.Path projectName "1" <| fun context ->
                context.writein "generated"
                if context.Language = Python then
                    invalidOp "expected")
        |> ignore

        Assert.False(File.Exists(Path.Combine(output.Path, projectName + ".c")))
        Assert.False(File.Exists(Path.Combine(output.Path, projectName + ".py")))
        Assert.False(File.Exists(Path.Combine(output.Path, "proc_" + projectName + "_C.sh")))
        Assert.False(File.Exists(Path.Combine(output.Path, "proc_" + projectName + "_P.sh")))
        Assert.Empty(transactionDirectories output.Path)

    [<Fact>]
    let ``Compile preserves existing files when a later language fails`` () =
        use output = new TemporaryDirectory()
        let projectName = "transaction-existing"
        let existingFiles =
            [ projectName + ".c", "existing C source"
              projectName + ".py", "existing Python source"
              "proc_" + projectName + "_C.sh", "existing C script"
              "proc_" + projectName + "_P.sh", "existing Python script" ]

        for fileName, contents in existingFiles do
            File.WriteAllText(Path.Combine(output.Path, fileName), contents)

        Assert.Throws<InvalidOperationException>(fun () ->
            Compile [C99; Python] output.Path projectName "1" <| fun context ->
                context.writein "replacement"
                if context.Language = Python then
                    invalidOp "expected")
        |> ignore

        for fileName, contents in existingFiles do
            Assert.Equal(contents, File.ReadAllText(Path.Combine(output.Path, fileName)))
        Assert.Empty(transactionDirectories output.Path)

    [<Fact>]
    let ``Compile publishes every generated file after all languages succeed`` () =
        use output = new TemporaryDirectory()
        let projectName = "transaction-success"
        let observedCodeFiles = ResizeArray<string>()

        Compile [C99; Python] output.Path projectName "1" <| fun context ->
            observedCodeFiles.Add(context.CodeFile.Value)
            context.writein "generated"

        Assert.Equal<string>(
            [ Path.Combine(output.Path, projectName)
              Path.Combine(output.Path, projectName) ],
            observedCodeFiles)
        for fileName in
            [ projectName + ".c"
              projectName + ".py"
              "proc_" + projectName + "_C.sh"
              "proc_" + projectName + "_P.sh" ] do
            Assert.True(File.Exists(Path.Combine(output.Path, fileName)), fileName)
        Assert.Empty(transactionDirectories output.Path)

    [<Fact>]
    let ``Compile rejects colliding HTML targets before generation`` () =
        for languages in
            [ [HTML; HTMLSequenceDiagram]
              [HTMLSequenceDiagram; HTML] ] do
            use output = new TemporaryDirectory()
            let projectName = "colliding-html-targets"
            let targetPath = Path.Combine(output.Path, projectName + ".html")
            let existingContents = "existing HTML"
            let mutable callbackInvoked = false
            File.WriteAllText(targetPath, existingContents)

            let error =
                Assert.Throws<ArgumentException>(fun () ->
                    Compile languages output.Path projectName "1" <| fun context ->
                        callbackInvoked <- true
                        context.writein "replacement")

            Assert.Equal("langgList", error.ParamName)
            Assert.Contains("produce the same output file", error.Message)
            Assert.False(callbackInvoked)
            Assert.Equal(existingContents, File.ReadAllText(targetPath))
            Assert.False(Directory.Exists(Path.Combine(output.Path, "contents_" + projectName)))
            Assert.Empty(transactionDirectories output.Path)

    [<Fact>]
    let ``Compile restores files when publication fails partway through commit`` () =
        use output = new TemporaryDirectory()
        let projectName = "transaction-commit-failure"
        let cPath = Path.Combine(output.Path, projectName + ".c")
        let pythonPath = Path.Combine(output.Path, projectName + ".py")
        File.WriteAllText(cPath, "existing C source")
        Directory.CreateDirectory(pythonPath) |> ignore

        Assert.ThrowsAny<IOException>(fun () ->
            Compile [C99; Python] output.Path projectName "1" <| fun context ->
                context.writein "replacement")
        |> ignore

        Assert.Equal("existing C source", File.ReadAllText(cPath))
        Assert.True(Directory.Exists(pythonPath))
        Assert.False(File.Exists(Path.Combine(output.Path, "proc_" + projectName + "_C.sh")))
        Assert.False(File.Exists(Path.Combine(output.Path, "proc_" + projectName + "_P.sh")))
        Assert.Empty(transactionDirectories output.Path)

    [<Fact>]
    let ``Compile removes only stale files owned by the same project`` () =
        use output = new TemporaryDirectory()
        let projectName = "transaction-stale"
        let cPath = Path.Combine(output.Path, projectName + ".c")
        let pythonPath = Path.Combine(output.Path, projectName + ".py")
        let pythonScript = Path.Combine(output.Path, "proc_" + projectName + "_P.sh")
        let userPath = Path.Combine(output.Path, projectName + ".notes")
        let otherProjectPath = Path.Combine(output.Path, "other-project.py")

        Compile [C99; Python] output.Path projectName "1" <| fun context ->
            context.writein "first version"
        File.WriteAllText(userPath, "user-owned notes")
        Compile [Python] output.Path "other-project" "1" ignore
        let originalOtherProject = File.ReadAllText otherProjectPath

        Compile [C99] output.Path projectName "2" <| fun context ->
            context.writein "second version"

        Assert.True(File.Exists cPath)
        Assert.False(File.Exists pythonPath)
        Assert.False(File.Exists pythonScript)
        Assert.Equal("user-owned notes", File.ReadAllText userPath)
        Assert.Equal(originalOtherProject, File.ReadAllText otherProjectPath)
        Assert.True(File.Exists(generatedManifest output.Path "other-project"))
        Assert.True(File.Exists(generatedManifest output.Path projectName))
        Assert.Empty(transactionDirectories output.Path)

    [<Fact>]
    let ``Compile removes stale nested assets and their empty managed directory`` () =
        use output = new TemporaryDirectory()
        let projectName = "transaction-stale-assets"
        let contentsPath = Path.Combine(output.Path, "contents_" + projectName)
        let assetPath = Path.Combine(contentsPath, "old-asset.txt")

        Compile [C99] output.Path projectName "1" <| fun context ->
            context.writein "first version"
            let staging = transactionDirectories output.Path |> Seq.exactlyOne
            let stagedContents = Path.Combine(staging, "contents_" + projectName)
            Directory.CreateDirectory(stagedContents) |> ignore
            File.WriteAllText(Path.Combine(stagedContents, "old-asset.txt"), "generated asset")

        Assert.True(File.Exists assetPath)
        Compile [C99] output.Path projectName "2" ignore

        Assert.False(File.Exists assetPath)
        Assert.False(Directory.Exists contentsPath)
        Assert.Empty(transactionDirectories output.Path)

    [<Fact>]
    let ``Compile does not claim preexisting files without an ownership manifest`` () =
        use output = new TemporaryDirectory()
        let projectName = "transaction-unmanaged"
        let oldPythonPath = Path.Combine(output.Path, projectName + ".py")
        File.WriteAllText(oldPythonPath, "user-owned file")

        Compile [C99] output.Path projectName "1" ignore

        Assert.Equal("user-owned file", File.ReadAllText oldPythonPath)
        Assert.True(File.Exists(generatedManifest output.Path projectName))
        Assert.Empty(transactionDirectories output.Path)

    [<Fact>]
    let ``Compile leaves old output untouched when a later language fails`` () =
        use output = new TemporaryDirectory()
        let projectName = "transaction-stale-failure"
        Compile [C99; Python] output.Path projectName "1" <| fun context ->
            context.writein "first version"
        let pythonPath = Path.Combine(output.Path, projectName + ".py")
        let manifestPath = generatedManifest output.Path projectName
        let originalPython = File.ReadAllText pythonPath
        let originalManifest = File.ReadAllText manifestPath

        Assert.Throws<InvalidOperationException>(fun () ->
            Compile [C99; JavaScript] output.Path projectName "2" <| fun context ->
                if context.Language = JavaScript then invalidOp "expected"
                context.writein "second version")
        |> ignore

        Assert.Equal(originalPython, File.ReadAllText pythonPath)
        Assert.Equal(originalManifest, File.ReadAllText manifestPath)
        Assert.False(File.Exists(Path.Combine(output.Path, projectName + ".js")))
        Assert.Empty(transactionDirectories output.Path)

    [<Fact>]
    let ``Compile restores stale files when publication fails`` () =
        use output = new TemporaryDirectory()
        let projectName = "transaction-stale-rollback"
        Compile [C99; Python] output.Path projectName "1" <| fun context ->
            context.writein "first version"
        let pythonPath = Path.Combine(output.Path, projectName + ".py")
        let cPath = Path.Combine(output.Path, projectName + ".c")
        let manifestPath = generatedManifest output.Path projectName
        let originalPython = File.ReadAllText pythonPath
        let originalC = File.ReadAllText cPath
        let originalManifest = File.ReadAllText manifestPath
        let blockedJavaScriptPath = Path.Combine(output.Path, projectName + ".js")
        Directory.CreateDirectory(blockedJavaScriptPath) |> ignore

        Assert.ThrowsAny<IOException>(fun () ->
            Compile [C99; JavaScript] output.Path projectName "2" <| fun context ->
                context.writein "replacement")
        |> ignore

        Assert.Equal(originalPython, File.ReadAllText pythonPath)
        Assert.Equal(originalC, File.ReadAllText cPath)
        Assert.Equal(originalManifest, File.ReadAllText manifestPath)
        Assert.True(Directory.Exists blockedJavaScriptPath)
        Assert.Empty(transactionDirectories output.Path)

    [<Fact>]
    let ``Compile refuses to delete a modified old output`` () =
        use output = new TemporaryDirectory()
        let projectName = "transaction-user-edit"
        Compile [C99; Python] output.Path projectName "1" ignore
        let pythonPath = Path.Combine(output.Path, projectName + ".py")
        let manifestPath = generatedManifest output.Path projectName
        let originalManifest = File.ReadAllText manifestPath
        File.WriteAllText(pythonPath, "user-edited source")

        Assert.ThrowsAny<IOException>(fun () ->
            Compile [C99] output.Path projectName "2" ignore)
        |> ignore

        Assert.Equal("user-edited source", File.ReadAllText pythonPath)
        Assert.Equal(originalManifest, File.ReadAllText manifestPath)
        Assert.Empty(transactionDirectories output.Path)

    [<Fact>]
    let ``Compile refuses to replace a modified old output`` () =
        use output = new TemporaryDirectory()
        let projectName = "transaction-user-edit-replacement"
        Compile [C99] output.Path projectName "1" ignore
        let sourcePath = Path.Combine(output.Path, projectName + ".c")
        let manifestPath = generatedManifest output.Path projectName
        let originalManifest = File.ReadAllText manifestPath
        File.WriteAllText(sourcePath, "user-edited source")

        Assert.ThrowsAny<IOException>(fun () ->
            Compile [C99] output.Path projectName "2" ignore)
        |> ignore

        Assert.Equal("user-edited source", File.ReadAllText sourcePath)
        Assert.Equal(originalManifest, File.ReadAllText manifestPath)
        Assert.Empty(transactionDirectories output.Path)

    [<Fact>]
    let ``Compile rejects a manifest path outside the output directory`` () =
        use output = new TemporaryDirectory()
        let projectName = "transaction-bad-manifest"
        let manifestPath = generatedManifest output.Path projectName
        File.WriteAllText(manifestPath, "{\"../user-file.txt\":\"" + String.replicate 64 "0" + "\"}")

        Assert.Throws<InvalidDataException>(fun () ->
            Compile [C99] output.Path projectName "1" ignore)
        |> ignore

        Assert.False(File.Exists(Path.Combine(output.Path, projectName + ".c")))
        Assert.Empty(transactionDirectories output.Path)

    [<Fact>]
    let ``Public version matches the assembly version`` () =
        let assemblyVersion = typeof<Aqualis>.Assembly.GetName().Version

        Assert.Equal("188.0.1", Aqualis.Version)
        Assert.Equal(Aqualis.Version, assemblyVersion.ToString(3))

    [<Theory>]
    [<InlineData(null)>]
    [<InlineData("")>]
    [<InlineData("   ")>]
    [<InlineData(".")>]
    [<InlineData("..")>]
    [<InlineData("../escape")>]
    [<InlineData("folder/escape")>]
    [<InlineData("folder\\escape")>]
    [<InlineData("/absolute")>]
    [<InlineData("C:\\absolute")>]
    [<InlineData("line\nbreak")>]
    [<InlineData("trailing.")>]
    [<InlineData("trailing ")>]
    [<InlineData("bad:name")>]
    [<InlineData("CON")>]
    [<InlineData("com1.txt")>]
    let ``Compile rejects project names that are not portable file-name segments`` (projectName:string) =
        use output = new TemporaryDirectory()
        let mutable callbackInvoked = false

        let error =
            Assert.Throws<ArgumentException>(fun () ->
                Compile [C99; Fortran] output.Path projectName "1" <| fun _ ->
                    callbackInvoked <- true)

        Assert.Equal("projectname", error.ParamName)
        Assert.False(callbackInvoked)
        Assert.Empty(Directory.EnumerateFileSystemEntries(output.Path))

    [<Fact>]
    let ``Compile rejects project names longer than the portable staging limit`` () =
        use output = new TemporaryDirectory()

        for projectName in [String.replicate 201 "a"; String.replicate 67 "あ"] do
            let error =
                Assert.Throws<ArgumentException>(fun () ->
                    Compile [C99] output.Path projectName "1" ignore)

            Assert.Equal("projectname", error.ParamName)

        Assert.Empty(Directory.EnumerateFileSystemEntries(output.Path))

    [<Theory>]
    [<InlineData("project-name")>]
    [<InlineData("project name")>]
    [<InlineData("_project")>]
    [<InlineData("1project")>]
    [<InlineData("日本語")>]
    [<InlineData("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")>]
    let ``Compile derives a valid Fortran identifier from every portable project file name`` projectName =
        use output = new TemporaryDirectory()

        Compile [Fortran] output.Path projectName "1" ignore

        let generated = File.ReadAllText(Path.Combine(output.Path, projectName + ".f90"))
        let programDeclaration = Regex.Match(generated, "(?m)^program (?<name>[A-Za-z][A-Za-z0-9_]{0,62})$")
        Assert.True(programDeclaration.Success)
        let identifier = programDeclaration.Groups["name"].Value
        Assert.Contains("end program " + identifier, generated)

    [<Fact>]
    let ``Compile escapes project titles according to LaTeX text rules`` () =
        use output = new TemporaryDirectory()
        let projectName = "A&B#C%_D$E^{F}~"

        Compile [LaTeX] output.Path projectName "1" ignore

        let generated = File.ReadAllText(Path.Combine(output.Path, projectName + ".tex"))
        Assert.Contains(
            "{\\Large A\\&B\\#C\\%\\_D\\$E\\textasciicircum{}\\{F\\}\\textasciitilde{}}",
            generated)

    [<Fact>]
    let ``Compile keeps version metadata on one comment line in every source language`` () =
        use output = new TemporaryDirectory()
        let version = "1 */\r\n#error injected\u2028alert(1)\u0000"
        let encodedVersion = "1 */\\r\\n#error injected\\u2028alert(1)\\u0000"

        Compile [Fortran; C99; Python; JavaScript] output.Path "metadata_1" version ignore

        let source extension = File.ReadAllText(Path.Combine(output.Path, "metadata_1" + extension))
        Assert.Contains("! Project version: " + encodedVersion, source ".f90")
        Assert.Contains("// Project version: " + encodedVersion, source ".c")
        Assert.Contains("# Project version: " + encodedVersion, source ".py")
        Assert.Contains("// Project version: " + encodedVersion, source ".js")
        for extension in [".f90"; ".c"; ".py"; ".js"] do
            Assert.DoesNotContain("\n#error injected", source extension)

    [<Fact>]
    let ``Compile rejects null version metadata before creating output`` () =
        use output = new TemporaryDirectory()

        let error =
            Assert.Throws<ArgumentNullException>(fun () ->
                Compile [C99] output.Path "metadata" null ignore)

        Assert.Equal("codever", error.ParamName)
        Assert.Empty(Directory.EnumerateFileSystemEntries(output.Path))

    [<Fact>]
    let ``Compile supplies generated and Numeric contexts explicitly`` () =
        use output = new TemporaryDirectory()
        let mutable numericIterations = 0
        let mutable generatedVariable = ""

        Compile [C99] output.Path "explicit-context" "1" <| fun context ->
            Assert.Equal(C99, context.Language)
            Assert.True(context.CodeFile.IsSome)
            context.writein "/* emitted through context */"
            context.ch.i <| fun value ->
                generatedVariable <- value.code
                value <== 0

        Compile [Numeric] output.Path "numeric-context" "1" <| fun context ->
            Assert.Equal(Numeric, context.Language)
            Assert.True(context.CodeFile.IsNone)
            context.iter.range(0, 2) <| fun _ -> numericIterations <- numericIterations + 1

        Assert.Equal(3, numericIterations)
        let generated = File.ReadAllText(Path.Combine(output.Path, "explicit-context.c"))
        Assert.Contains("/* emitted through context */", generated)
        Assert.Contains(generatedVariable + " = 0;", generated)

    [<Fact>]
    let ``Numeric Compile disposes its writerless context`` () =
        use output = new TemporaryDirectory()
        let mutable escapedContext:Aqualis option = None

        Compile [Numeric] output.Path "numeric-lifetime" "1" <| fun context ->
            Assert.Equal(1, context.Active)
            Assert.True(context.CodeFile.IsNone)
            Assert.False(context.IsNeutral)
            escapedContext <- Some context

        Assert.Equal(0, escapedContext.Value.Active)
        Assert.Throws<InvalidOperationException>(fun () ->
            escapedContext.Value.writein "invalid")
        |> ignore
        Assert.False(File.Exists(Path.Combine(output.Path, "numeric-lifetime")))

    [<Fact>]
    let ``Numeric Compile disposes its context when the callback throws`` () =
        use output = new TemporaryDirectory()
        let mutable escapedContext:Aqualis option = None

        Assert.Throws<InvalidOperationException>(fun () ->
            Compile [Numeric] output.Path "numeric-failure" "1" <| fun context ->
                escapedContext <- Some context
                invalidOp "expected")
        |> ignore

        Assert.Equal(0, escapedContext.Value.Active)

    [<Fact>]
    let ``neutral Numeric values keep their independent lifetime`` () =
        let value = int0(Int 1)

        Assert.True(value.Context.IsNeutral)
        Assert.Equal(1, value.Context.Active)

    [<Fact>]
    let ``PHP compilation writes a php file through explicit services`` () =
        use output = new TemporaryDirectory()
        Compile [PHP] output.Path "page" "1" <| fun context ->
            let value = context.php.var "value"
            value <== context.php.readFile "data.json"
            let input = context.form.textBox "user"
            context.html.form (Url.relative "page.php") <| fun () -> input.show()
        let phpPath = Path.Combine(output.Path, "page.php")
        Assert.True(File.Exists phpPath)
        Assert.False(File.Exists(Path.Combine(output.Path, "page.c")))
        let generated = File.ReadAllText phpPath
        Assert.Contains("$value = (function ($filename) { $contents = @file_get_contents", generated)
        Assert.Contains("<form", generated)

    [<Fact>]
    let ``PHP compilation preserves the previous file when generation fails`` () =
        use output = new TemporaryDirectory()
        let phpPath = Path.Combine(output.Path, "page.php")
        File.WriteAllText(phpPath, "previous PHP")
        let mutable observedCodeFile = None

        Assert.Throws<InvalidOperationException>(fun () ->
            Compile [PHP] output.Path "page" "1" <| fun context ->
                observedCodeFile <- context.CodeFile
                context.writein "partial PHP"
                invalidOp "expected")
        |> ignore

        Assert.Equal(Some phpPath, observedCodeFile)
        Assert.Equal("previous PHP", File.ReadAllText(phpPath))
        Assert.Empty(Directory.GetFiles(output.Path, ".page.php.aqualis-*.tmp"))

    [<Fact>]
    let ``compiled language sources remain unchanged when final assembly fails`` () =
        use output = new TemporaryDirectory()
        let cases =
            [ Fortran, "fortran", ".f90"
              C99, "c99", ".c"
              LaTeX, "latex", ".tex"
              HTML, "html", ".html"
              Python, "python", ".py"
              JavaScript, "javascript", ".js" ]

        for language, projectName, extension in cases do
            let targetPath = Path.Combine(output.Path, projectName + extension)
            let intermediatePath = Path.Combine(output.Path, projectName)
            let previousContents = "previous " + projectName
            File.WriteAllText(targetPath, previousContents)

            Assert.Throws<FileNotFoundException>(fun () ->
                Compile [language] output.Path projectName "1" <| fun context ->
                    context.writein "partial generated source"
                    context.flist.add ("missing-function-" + projectName))
            |> ignore

            Assert.Equal(previousContents, File.ReadAllText(targetPath))
            Assert.False(File.Exists(intermediatePath))
            Assert.Empty(
                Directory.GetFiles(
                    output.Path,
                    "." + projectName + extension + ".aqualis-*.tmp"))

    [<Fact>]
    let ``HTML sequence diagrams use the shared web output layout`` () =
        use output = new TemporaryDirectory()
        let projectName = "sequence-layout"

        Compile [HTMLSequenceDiagram] output.Path projectName "1" <| fun context ->
            context.writein "<div id=\"sequence-body\"></div>"

        let mainPath = Path.Combine(output.Path, projectName + ".html")
        let contentsPath = Path.Combine(output.Path, "contents_" + projectName)
        let bodyTemporaryPath = Path.Combine(output.Path, projectName + "_body")
        let generated = File.ReadAllText mainPath

        Assert.True(File.Exists mainPath)
        Assert.False(Directory.Exists contentsPath)
        Assert.False(File.Exists bodyTemporaryPath)
        Assert.Contains("id=\"sequence-body\"", generated)
        Assert.Equal(1, generated.Split("<title>").Length - 1)
        Assert.DoesNotContain("id=\"MathJax-script\"", generated)
        Assert.DoesNotContain("https://", generated)

        [ "animationSeq.js"
          "animationSeqReset.js"
          "animationStart.js"
          "animationReset.js"
          "autoAnimation.js" ]
        |> List.iter (fun asset ->
            Assert.DoesNotContain(asset, generated))

    [<Fact>]
    let ``sequence diagram figures use valid SVG dimensions and margins`` () =
        use output = new TemporaryDirectory()
        let projectName = "sequence-figure"

        Compile [HTMLSequenceDiagram] output.Path projectName "1" <| fun context ->
            expr.fig context position.Origin <| fun (figure,_) ->
                figure.line Style.blank
                    (position(20.5, 30.25))
                    (position(60.5, 80.25))

        let generated =
            File.ReadAllText(Path.Combine(output.Path, projectName + ".html"))

        Assert.Contains("viewBox=\"0 0 60 70\"", generated)
        Assert.Contains("width=\"60px\"", generated)
        Assert.Contains("height=\"70px\"", generated)
        Assert.Contains("margin-left: 10.5px", generated)
        Assert.Contains("margin-top: 20.25px", generated)
        Assert.DoesNotContain("heigth=", generated)
        Assert.DoesNotContain("margin-left: 10.5;", generated)
        Assert.DoesNotContain("margin-top: 20.25;", generated)

    [<Fact>]
    let ``HTML sequence diagram failures remove the temporary body`` () =
        use output = new TemporaryDirectory()
        let projectName = "sequence-failure"
        let bodyTemporaryPath = Path.Combine(output.Path, projectName + "_body")

        Assert.Throws<InvalidOperationException>(fun () ->
            Compile [HTMLSequenceDiagram] output.Path projectName "1" <| fun context ->
                context.writein "partial body"
                invalidOp "expected")
        |> ignore

        Assert.False(File.Exists bodyTemporaryPath)
        Assert.False(File.Exists(Path.Combine(output.Path, projectName + ".html")))
        Assert.False(Directory.Exists(Path.Combine(output.Path, "contents_" + projectName)))

    [<Fact>]
    let ``operators functions and indexers share context validation`` () =
        use output = new TemporaryDirectory()
        use first = createContext output.Path "merge-first.c" C99
        use second = createContext output.Path "merge-second.c" C99
        let x = double0(Var(Dt, "x", NaN), first)
        let y = double0(Var(Dt, "y", NaN), second)
        let rounded = asm.floor x
        Assert.Same(first, rounded.Context)
        Assert.Throws<InvalidOperationException>(fun () -> x + y |> ignore) |> ignore
        Assert.Throws<InvalidOperationException>(fun () -> dv x ++ dv y |> ignore) |> ignore
        Assert.Throws<InvalidOperationException>(fun () -> Or [x .< 0.0; y .< 0.0] |> ignore) |> ignore
        let values = int2(It 4, Var2(A2(2, 2), "values"), first)
        let foreignIndex = int0(Var(It 4, "index", NaN), second)
        Assert.Throws<InvalidOperationException>(fun () -> values[foreignIndex, 0] |> ignore) |> ignore
        let neutral = int2(It 4, Arx2(I 2, I 2, fun _ -> Int 1))
        let inherited = neutral + int0(Var(It 4, "value", NaN), first)
        Assert.Same(first, inherited.Context)

    [<Fact>]
    let ``contexts with the same output path remain distinct`` () =
        use output = new TemporaryDirectory()
        use first = createContext output.Path "same-output.c" C99
        let left = double0(Var(Dt, "left", NaN), first)
        first.close()
        use second = createContext output.Path "same-output.c" C99
        let right = double0(Var(Dt, "right", NaN), second)

        Assert.NotEqual(first.ContextId, second.ContextId)
        Assert.Throws<InvalidOperationException>(fun () -> left + right |> ignore)
        |> ignore

    [<Fact>]
    let ``writerless non-Numeric contexts are not neutral`` () =
        use first = new Aqualis(None, None, C99)
        use second = new Aqualis(None, None, C99)
        let left = int0(Var(It 4, "left", NaN), first)
        let sameContext = int0(Var(It 4, "same", NaN), first)
        let foreign = int0(Var(It 4, "foreign", NaN), second)

        Assert.Same(first, (left + sameContext).Context)
        Assert.Throws<InvalidOperationException>(fun () -> left + foreign |> ignore)
        |> ignore

    [<Fact>]
    let ``Numeric contexts remain neutral during merges`` () =
        let left = int0(Int 1)
        let right = int0(Int 2)
        let combined = left + right
        use empty = Aqualis.mergeMany Seq.empty

        Assert.True(left.Context.IsNeutral)
        Assert.True(right.Context.IsNeutral)
        Assert.Same(left.Context, combined.Context)
        Assert.True(empty.IsNeutral)

    [<Fact>]
    let ``values and contexts cannot write after a Compile callback`` () =
        use output = new TemporaryDirectory()
        let mutable escapedValue:int0 option = None
        let mutable escapedContext:Aqualis option = None
        Compile [C99] output.Path "escaped-context" "1" <| fun context ->
            escapedContext <- Some context
            context.ch.i <| fun value -> escapedValue <- Some value
        Assert.Equal(0, escapedContext.Value.Active)
        Assert.Throws<InvalidOperationException>(fun () -> escapedValue.Value <== 1) |> ignore
        Assert.Throws<InvalidOperationException>(fun () -> escapedContext.Value.writein "invalid") |> ignore

    [<Fact>]
    let ``parallel mode restores normal nested and exceptional scopes`` () =
        use context = Aqualis.BlankWriter C99
        Assert.False(context.ParallelMode)
        let result =
            context.WithParallelMode(fun outer ->
                Assert.True(outer.ParallelMode)
                let nested = outer.WithParallelMode(fun inner ->
                    Assert.True(inner.ParallelMode)
                    42)
                Assert.True(outer.ParallelMode)
                nested)
        Assert.Equal(42, result)
        Assert.False(context.ParallelMode)
        Assert.Throws<InvalidOperationException>(
            Action(fun () ->
                context.WithParallelMode(fun _ -> invalidOp "expected")))
        |> ignore
        Assert.False(context.ParallelMode)
