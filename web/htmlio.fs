namespace Aqualis

open System
open System.IO

type internal WebOutputLayout = {
    OutputDirectory:string
    ProjectName:string
    MainFileName:string
    MainHtmlPath:string
    BodyTemporaryFileName:string
    BodyTemporaryPath:string
    ContentsName:string
    ContentsDirectory:string }

module internal WebOutputLayout =
    let create outputDirectory projectName =
        let mainFileName = projectName + ".html"
        let bodyTemporaryFileName = projectName + "_body"
        let contentsName = "contents_" + projectName
        {
            OutputDirectory = outputDirectory
            ProjectName = projectName
            MainFileName = mainFileName
            MainHtmlPath = Path.Combine(outputDirectory, mainFileName)
            BodyTemporaryFileName = bodyTemporaryFileName
            BodyTemporaryPath = Path.Combine(outputDirectory, bodyTemporaryFileName)
            ContentsName = contentsName
            ContentsDirectory = Path.Combine(outputDirectory, contentsName)
        }

    let assetUrl (layout:WebOutputLayout) (fileName:string) =
        if String.IsNullOrWhiteSpace fileName then
            invalidArg (nameof fileName) "An asset file name is required."

        if Path.IsPathRooted fileName then
            invalidArg (nameof fileName) "An asset URL cannot be built from an absolute path."

        let segments = fileName.Replace('\\', '/').Split('/')
        if segments |> Array.exists (fun segment -> String.IsNullOrWhiteSpace segment || segment = "." || segment = "..") then
            invalidArg (nameof fileName) "An asset path must contain only non-empty relative path segments."

        let encodedPath =
            segments
            |> Array.map Uri.EscapeDataString
            |> String.concat "/"

        Uri.EscapeDataString(layout.ContentsName) + "/" + encodedPath

type HtmlGenerationContext internal (dir:string,projectName:string) =
    let gate = obj()
    let layout = WebOutputLayout.create dir projectName
    let mutable contentsCounter = -1
    let mutable animationSequenceCounter = -1
    let mutable animationGroupCounter = -1
    let mutable figureCounter = 0
    let mutable animationCounter = 0
    let mutable characterEnabled = true
    let mutable subtitleEnabled = true
    let mutable voiceEnabled = true
    let animationButtons = ResizeArray<string * string * int * int>()
    let audioFiles = ResizeArray<string>()
    do Directory.CreateDirectory(layout.ContentsDirectory) |> ignore
    // メインファイル
    let main = new Aqualis(
        Some layout.OutputDirectory,
        Some layout.MainFileName,
        HTML)
    // HTML本体のコード
    let body = new Aqualis(
        Some layout.OutputDirectory,
        Some layout.BodyTemporaryFileName,
        HTML)
    // JavaScriptのコード
    let jsMain = new Aqualis(
        Some layout.OutputDirectory,
        Some (projectName + "_js"),
        JavaScript)
    // スライドアニメーション用javascriptファイル名
    let animationSeq = new Aqualis(
        Some layout.ContentsDirectory,
        Some "animationSeq.js",
        JavaScript)
    // スライドアニメーション(アニメーション開始)用javascript
    let jsAnimationStart = new Aqualis(
        Some layout.ContentsDirectory,
        Some "animationStart.js",
        JavaScript)
    // スライドアニメーション(アニメーションリセット)用javascript
    let jsAnimationSeqReset = new Aqualis(
        Some layout.ContentsDirectory,
        Some "animationSeqReset.js",
        JavaScript)
    // スライドアニメーション(アニメーションリセット)用javascript
    let jsAnimationReset = new Aqualis(
        Some layout.ContentsDirectory,
        Some "animationReset.js",
        JavaScript)
    // オートアニメーション実行用javascript
    let autoAnimation = new Aqualis(
        Some layout.ContentsDirectory,
        Some "autoAnimation.js",
        JavaScript)

    let ownedContexts =
        [| main
           body
           jsMain
           animationSeq
           jsAnimationStart
           jsAnimationSeqReset
           jsAnimationReset
           autoAnimation |]

    member _.BodyContext with get() = body

    member this.switchMain code = code main
    member this.switchBody code = code body
    member this.switchJSMain code = code jsMain
    member this.switchAnimationSeq code = code animationSeq
    member this.switchJSAnimationStart code = code jsAnimationStart
    member this.switchJSAnimationSeqReset code = code jsAnimationSeqReset
    member this.switchJSAnimationReset code = code jsAnimationReset
    member this.switchAutoAnimation code = code autoAnimation

    member _.CharacterEnabled with get() = characterEnabled and set(v) = characterEnabled <- v
    member _.SubtitleEnabled with get() = subtitleEnabled and set(v) = subtitleEnabled <- v
    member _.VoiceEnabled with get() = voiceEnabled and set(v) = voiceEnabled <- v
    
    /// <summary>Gets the directory that receives generated web content.</summary>
    member _.ContentsDirectory = layout.ContentsDirectory

    /// <summary>Gets the relative URL prefix used by generated web assets.</summary>
    member _.ContentsUrlPrefix = layout.ContentsName

    /// <summary>Builds a relative URL for a generated web asset.</summary>
    member _.AssetUrl(fileName:string) = WebOutputLayout.assetUrl layout fileName

    /// <summary>Copies an asset into the generated content directory and returns its relative URL.</summary>
    member internal this.ImportAsset(sourcePath:string) =
        if String.IsNullOrWhiteSpace sourcePath then
            invalidArg (nameof sourcePath) "An asset source path is required."

        if not (File.Exists sourcePath) then
            raise (FileNotFoundException("Asset file was not found.", sourcePath))

        let fileName = Path.GetFileName sourcePath
        if String.IsNullOrWhiteSpace fileName then
            invalidArg (nameof sourcePath) "The asset source path must identify a file."

        Directory.CreateDirectory(layout.ContentsDirectory) |> ignore
        let destinationPath = Path.Combine(layout.ContentsDirectory, fileName)
        let sourceFullPath = Path.GetFullPath sourcePath
        let destinationFullPath = Path.GetFullPath destinationPath
        let comparison =
            if OperatingSystem.IsWindows() then StringComparison.OrdinalIgnoreCase
            else StringComparison.Ordinal

        if not (String.Equals(sourceFullPath, destinationFullPath, comparison)) then
            File.Copy(sourceFullPath, destinationFullPath, true)

        this.AssetUrl(fileName)

    /// <summary>Allocates the next unique HTML content number.</summary>
    member _.NextContentsNumber() =
        lock gate (fun () ->
            contentsCounter <- contentsCounter + 1
            contentsCounter)

    /// <summary>Allocates the next unique animation sequence number.</summary>
    member _.NextAnimationSequenceNumber() =
        lock gate (fun () ->
            animationSequenceCounter <- animationSequenceCounter + 1
            animationSequenceCounter)

    /// <summary>Allocates the next unique animation group number.</summary>
    member _.NextAnimationGroupNumber() =
        lock gate (fun () ->
            animationGroupCounter <- animationGroupCounter + 1
            animationGroupCounter)

    /// <summary>Allocates the next unique animated figure number.</summary>
    member _.NextFigureNumber() =
        lock gate (fun () ->
            figureCounter <- figureCounter + 1
            figureCounter)

    /// <summary>Allocates the next unique animation page number.</summary>
    member _.NextAnimationNumber() =
        lock gate (fun () ->
            animationCounter <- animationCounter + 1
            animationCounter)

    /// <summary>Gets the number assigned to the most recently created animation page.</summary>
    member _.AnimationCount =
        lock gate (fun () -> animationCounter)

    /// <summary>Registers the control button data for an animation.</summary>
    member _.AddAnimationButton(button) =
        lock gate (fun () -> animationButtons.Add(button))

    /// <summary>Removes all registered animation control buttons.</summary>
    member _.ClearAnimationButtons() =
        lock gate animationButtons.Clear

    /// <summary>Returns the most recently registered animation control button, if any.</summary>
    member _.TryLastAnimationButton() =
        lock gate (fun () ->
            if animationButtons.Count = 0 then None
            else Some animationButtons[animationButtons.Count - 1])

    /// <summary>Registers an audio file referenced by the generated presentation.</summary>
    member _.AddAudioFile(audioFile:string) =
        lock gate (fun () -> audioFiles.Add(audioFile))

    /// <summary>Gets a snapshot of the registered audio files.</summary>
    member _.AudioFiles =
        lock gate (fun () -> audioFiles |> Seq.toList)
            

    member this.nextContentsID() =
        "contentsID" + this.NextContentsNumber().ToString()

    member this.nextAnimationSeqID() =
        let number = this.NextAnimationSequenceNumber()
        "animationSeqID" + number.ToString(), "animationSeqResetID" + number.ToString()

    member this.nextAnimationGroup() = this.NextAnimationGroupNumber().ToString()
    member this.animationButtonReset() = this.ClearAnimationButtons()
    member this.addAnimationButton(fnameStart,fnameReset,buttonX,buttonY) =
        this.AddAnimationButton(fnameStart,fnameReset,buttonX,buttonY)

    member this.addAutoAnimation(fnameStart,_) =
        this.switchAutoAnimation (fun child ->
            child.codewritein("animationStartMap['"+fnameStart+"']();"))

    interface IDisposable with
        member _.Dispose() =
            ownedContexts
            |> Array.iter (fun context -> (context :> IDisposable).Dispose())
