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
    new(subtitle:num0) = Serif("\\("+subtitle.Expr.eval (programList[prIndex])+"\\)",subtitle.Expr.evalT())
    new(subtitle:bool0) = Serif("\\("+subtitle.Expr.eval (programList[prIndex])+"\\)",subtitle.Expr.evalT())
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
type Character(name:string) = 
    let mutable serif:list<Audio> = []
    let mutable newScriptCounter:int = 0
    let mutable audioFileCounter:int = 0
    member _.Name with get() = name
    member _.scriptDataFile with get() = name + ".json"
    abstract member audioFile:Audio->option<string>
    abstract member scriptFile:int->string
    abstract member scriptColor:string
    member this.saveScriptData() =
        // jsonファイル出力
        let jsonOptions =
            JsonSerializerOptions(
                WriteIndented = true,
                PropertyNamingPolicy = JsonNamingPolicy.CamelCase)
        jsonOptions.Encoder <- JavaScriptEncoder.UnsafeRelaxedJsonEscaping
        let json = JsonSerializer.Serialize(serif, jsonOptions)
        File.WriteAllText(this.scriptDataFile, json)
        // 音声スクリプトファイル出力（読み上げ用音声合成ソフト入力用）
        audioFileCounter <- -1::(serif |> List.map (fun a -> match a.AudioFileNumber with |None -> -1 |Some n -> n)) |> List.max
        let wr = [for i in 0..audioFileCounter -> new StreamWriter(this.scriptFile i,false)]
        for a in serif do
            match a.AudioFileNumber with
            |None -> ()
            |Some i -> wr[i].WriteLine a.Script
        wr |> List.iter (fun w -> w.Close())
    member this.loadScriptData() =
        let jsonOptions =
            JsonSerializerOptions(
                WriteIndented = true,
                PropertyNamingPolicy = JsonNamingPolicy.CamelCase)
        jsonOptions.Encoder <- JavaScriptEncoder.UnsafeRelaxedJsonEscaping
        if File.Exists this.scriptDataFile then
            let json = File.ReadAllText this.scriptDataFile
            serif <- JsonSerializer.Deserialize<list<Audio>>(json, jsonOptions)
            audioFileCounter <- (-1::(serif |> List.map (fun a -> match a.AudioFileNumber with |None -> -1 |Some n -> n)) |> List.max) + 1
    member this.script(text:exprString) =
        let subtitle = text.data |> List.fold (fun acc a -> match a with |RStr x -> acc+x |RNvr x -> acc+"\\("+x.evalH programList[prIndex]+"\\)") ""
        let script = text.data |> List.fold (fun acc a -> match a with |RStr x -> acc+x |RNvr x -> acc+x.evalT()) ""
        match serif |> List.tryFind (fun a -> a.Subtitle=subtitle && a.Script=script) with
        |None ->
            let a = {Subtitle=subtitle; Script=script; AudioFileNumber=Some audioFileCounter; AudioSourceNumber=Some newScriptCounter}
            serif <- serif@[a]
            newScriptCounter <- newScriptCounter + 1
            a, None, this.scriptColor
        |Some x -> 
            x, this.audioFile x, this.scriptColor
    member this.script(text:string) = this.script (exprString text)
    member this.script(text:num0) = this.script (exprString text)
