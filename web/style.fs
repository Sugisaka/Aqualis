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

type Serif(subtitle:string,hatsuon:string) =
    new(subtitle:string) = Serif(subtitle,subtitle)
    new(subtitle:num0) = Serif("\\("+subtitle.Expr.eval ((GenerationScope.currentProgram()))+"\\)",subtitle.Expr.evalT())
    new(subtitle:bool0) = Serif("\\("+subtitle.Expr.eval ((GenerationScope.currentProgram()))+"\\)",subtitle.Expr.evalT())
    member _.Subtitle with get() = subtitle
    member _.Hatsuon with get() = hatsuon
    static member (+) (a:Serif,b:Serif) = Serif(a.Subtitle+b.Subtitle,a.Hatsuon+b.Hatsuon)

type CharacterImage = {CharacterImageFile:string; CharacterImageStyle:string}

type Align = |Center |Left

/// 解説音声の設定
type Speak =
    /// 発話なし
    |Silent
    /// 音声ファイル指定
    |AudioFile of string
    /// 音声ファイルがあるディレクトリ指定
    |AudioDir of string

type Audio = {Subtitle:string; Script:string; AudioFileNumber:option<int>; AudioSourceNumber:option<int>}

type ViewBoxStyle = {sX:int; sY:int; mX:int; mY:int; backgroundColor:string}

type AnimationSetting = {
    /// 1フレームの時間(ms)
    FrameTime:int;
    /// アニメーションのフレーム数（時間は0からFrameNumber-1まで進む）
    FrameNumber:int}

[<AbstractClass>]
type Character(scriptDataDir:string,name:string) =
    /// jsonファイル名（フルパス）
    let scriptDataFileName = scriptDataDir + "\\" + name + ".json"
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
    member _.Name with get() = name
    abstract member audioFile:Audio->option<string>
    abstract member scriptFile:int->string
    abstract member scriptColor:string
    member this.saveScriptData() =
        match serif with
        |[] ->
            ()
        |_ ->
            // jsonファイル出力
            let json = JsonSerializer.Serialize(serif, jsonOptions)
            File.WriteAllText(scriptDataFileName, json)
            // 音声スクリプトファイル出力（読み上げ用音声合成ソフト入力用）
            serif
            |> List.filter (fun audio -> audio.AudioFileNumber = Some(audioFileCounter+1))
            |> List.sortBy (fun audio -> audio.AudioSourceNumber)
            |> fun lst ->
                match lst with
                |[] -> ()
                |_ ->
                    let wr = new StreamWriter(this.scriptFile (audioFileCounter+1), false)
                    lst |> List.iter (fun audio -> wr.WriteLine audio.Script)
                    wr.Close()
    member this.script(subtitle:string,script:string) =
        match serif |> List.tryFind (fun a -> a.Script=script) with
        |None ->
            let a = {Subtitle=subtitle; Script=script; AudioFileNumber=Some(audioFileCounter+1); AudioSourceNumber=Some newScriptCounter}
            serif <- serif@[a]
            newScriptCounter <- newScriptCounter + 1
            a, this.audioFile a, this.scriptColor
        |Some x ->
            x, this.audioFile x, this.scriptColor
    member this.script(text:exprString) =
        let subtitle = text.data |> List.fold (fun acc a -> match a with |RStr x -> acc+x |RNvr x -> acc+"\\("+x.evalH (GenerationScope.currentProgram())+"\\)") ""
        let script = text.data |> List.fold (fun acc a -> match a with |RStr x -> acc+x |RNvr x -> acc+x.evalT()) ""
        this.script(subtitle,script)
    member this.script(text:string) = this.script (exprString text)
    member this.script(text:num0) = this.script (exprString text)
