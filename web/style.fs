//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

open System
open System.IO
open System.Text.Json
open System.Text.Encodings.Web

/// Pairs subtitle text with its spoken rendering.
type Serif(subtitle:string,hatsuon:string) =
    new(subtitle:string) = Serif(subtitle,subtitle)
    new(subtitle:int0) = Serif("\\("+subtitle.Expr.eval subtitle.Context+"\\)",subtitle.Expr.evalT())
    new(subtitle:double0) = Serif("\\("+subtitle.Expr.eval subtitle.Context+"\\)",subtitle.Expr.evalT())
    new(subtitle:complex0) = Serif("\\("+subtitle.Expr.eval subtitle.Context+"\\)",subtitle.Expr.evalT())
    new(subtitle:bool0) = Serif("\\("+subtitle.Expr.eval subtitle.Context+"\\)",subtitle.Expr.evalT())
    /// Gets text shown in subtitles.
    member _.Subtitle with get() = subtitle
    /// Gets text passed to speech synthesis.
    member _.Hatsuon with get() = hatsuon
    /// Concatenates subtitle and spoken text independently.
    static member (+) (a:Serif,b:Serif) = Serif(a.Subtitle+b.Subtitle,a.Hatsuon+b.Hatsuon)

/// Image file and CSS style for a character.
type CharacterImage = {CharacterImageFile:string; CharacterImageStyle:string}

/// Horizontal alignment for presentation elements.
type Align = |Center |Left

/// 解説音声の設定
type Speak =
    /// 発話なし
    |Silent
    /// 音声ファイル指定
    |AudioFile of string
    /// 音声ファイルがあるディレクトリ指定
    |AudioDir of string

/// Subtitle, narration text, and optional audio file identifiers.
type Audio = {Subtitle:string; Script:string; AudioFileNumber:option<int>; AudioSourceNumber:option<int>}

/// View box coordinates and background color for a presentation.
type ViewBoxStyle = {sX:int; sY:int; mX:int; mY:int; backgroundColor:string}

/// Animation frame duration and frame count.
type AnimationSetting = {
    /// 1フレームの時間(ms)
    FrameTime:int;
    /// アニメーションのフレーム数（時間は0からFrameNumber-1まで進む）
    FrameNumber:int}

[<RequireQualifiedAccess>]
/// Stages and publishes generated character script files.
module private CharacterOutputFile =
    /// Writes content to a staged character-script file.
    let private stage (targetPath:string) (write:string -> unit) =
        let output = AtomicOutputFile.create targetPath
        try
            write output.StagingPath
            output
        with _ ->
            AtomicOutputFile.discard output
            reraise()

    /// Stages a text file for atomic publication.
    let stageText (targetPath:string) (text:string) =
        stage targetPath (fun stagingPath -> File.WriteAllText(stagingPath, text))

    /// Stages a line-oriented file for atomic publication.
    let stageLines (targetPath:string) (lines:string list) =
        stage targetPath (fun stagingPath ->
            use writer = new StreamWriter(stagingPath, false)
            lines |> List.iter writer.WriteLine)

[<AbstractClass>]
/// Stores a presentation character and its narration scripts.
type Character(context:HtmlGenerationContext,scriptDataDir:string,name:string) =
    let name =
        if isNull name then nullArg "name"
        PortableFileNameSegment.validate "name" "character" name
    /// jsonファイル名（フルパス）
    let scriptDataFileName = Path.Combine(scriptDataDir, name + ".json")
    let jsonOptions =
        JsonSerializerOptions(
            WriteIndented = true,
            Encoder = JavaScriptEncoder.UnsafeRelaxedJsonEscaping
        )
    /// 既に存在するjsonファイルからスクリプトデータ取得
    let mutable serif:list<Audio> =
        if File.Exists scriptDataFileName then
            let json = File.ReadAllText scriptDataFileName
            JsonSerializer.Deserialize<list<Audio>>(json, jsonOptions)
        else
            []
    let mutable newScriptCounter:int = 0
    let audioFileCounter:int =
        serif
        |> List.map (fun audio -> match audio.AudioFileNumber with |None -> -1 |Some m -> m)
        |> fun s -> match s with |[] -> -1 |_ -> List.max s
    /// Gets the validated character name.
    member _.Name with get() = name
    /// Resolves an audio file for a narration entry, if one exists.
    abstract member audioFile:Audio->option<string>
    /// Gets the output path for a numbered narration script.
    abstract member scriptFile:int->string
    /// Gets the color used to display the character script.
    abstract member scriptColor:string
    /// Stages and publishes the character script JSON and new narration scripts.
    member this.saveScriptData() =
        match serif with
        |[] ->
            ()
        |_ ->
            let stagedOutputs = ResizeArray<AtomicOutputFile>()
            try
                // すべての出力を一時ファイルに準備してから、完成したファイルだけを公開する。
                let json = JsonSerializer.Serialize(serif, jsonOptions)
                stagedOutputs.Add(CharacterOutputFile.stageText scriptDataFileName json)

                // 音声スクリプトファイル出力（読み上げ用音声合成ソフト入力用）
                let newScripts =
                    serif
                    |> List.filter (fun audio -> audio.AudioFileNumber = Some(audioFileCounter+1))
                    |> List.sortBy (fun audio -> audio.AudioSourceNumber)
                    |> List.map (fun audio -> audio.Script)
                match newScripts with
                |[] -> ()
                |_ ->
                    stagedOutputs.Add(
                        CharacterOutputFile.stageLines
                            (this.scriptFile (audioFileCounter+1))
                            newScripts)

                stagedOutputs |> Seq.iter AtomicOutputFile.publish
            with _ ->
                stagedOutputs |> Seq.iter AtomicOutputFile.discard
                reraise()
    /// Finds or records a narration entry and returns its audio file and display color.
    member this.script(subtitle:string,script:string) =
        match serif |> List.tryFind (fun a -> a.Subtitle=subtitle && a.Script=script) with
        |None ->
            let a = {Subtitle=subtitle; Script=script; AudioFileNumber=Some(audioFileCounter+1); AudioSourceNumber=Some newScriptCounter}
            serif <- serif@[a]
            newScriptCounter <- newScriptCounter + 1
            a, this.audioFile a, this.scriptColor
        |Some x ->
            x, this.audioFile x, this.scriptColor
    /// Converts expression text into subtitle and spoken text for a narration entry.
    member this.script(text:exprString) =
        let subtitle = text.data |> List.fold (fun acc a -> match a with |RStr x -> acc+x |RNvr (x,_) -> acc+"\\("+x.evalH context.BodyContext+"\\)") ""
        let script = text.data |> List.fold (fun acc a -> match a with |RStr x -> acc+x |RNvr (x,_) -> acc+x.evalT()) ""
        this.script(subtitle,script)
    /// Converts expression text into subtitle and spoken text for a narration entry.
    member this.script(text:string) = this.script (exprString text)
    /// Converts expression text into subtitle and spoken text for a narration entry.
    member this.script(text:int0) = this.script (exprString text)
    /// Converts expression text into subtitle and spoken text for a narration entry.
    member this.script(text:double0) = this.script (exprString text)
    /// Converts expression text into subtitle and spoken text for a narration entry.
    member this.script(text:complex0) = this.script (exprString text)
