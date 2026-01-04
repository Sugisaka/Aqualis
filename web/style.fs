// 
// Copyright (c) 2026 Jun-ichiro Sugisaka
// 
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
// 
namespace Aqualis

type Serif(subtitle:string,hatsuon:string) = 
    new(subtitle:string) = Serif(subtitle,subtitle)
    new(subtitle:num0) = Serif("\\("+subtitle.Expr.eval (programList[prIndex])+"\\)",subtitle.Expr.evalT())
    new(subtitle:bool0) = Serif("\\("+subtitle.Expr.eval (programList[prIndex])+"\\)",subtitle.Expr.evalT())
    member _.Subtitle with get() = subtitle
    member _.Hatsuon with get() = hatsuon
    static member (+) (a:Serif,b:Serif) = Serif(a.Subtitle+b.Subtitle,a.Hatsuon+b.Hatsuon)
    
type Character = |Tale |Dang |Armi

type Align = |Center |Left

/// 解説音声の設定
type Speak = 
    /// 発話なし
    |Silent
    /// 音声ファイル指定
    |AudioFile of string
    /// 音声ファイルがあるディレクトリ指定
    |AudioDir of string
    
type Audio = {Speaker:Character; Subtitle:Serif; Source:Speak}

type SubTitle = {Dango:Serif; Tale:Serif; Armi:Serif;}

type ViewBoxStyle = {sX:int; sY:int; mX:int; mY:int; backgroundColor:string}

type AnimationSetting = {
    /// 1フレームの時間(ms)
    FrameTime:int;
    /// アニメーションのフレーム数（時間は0からFrameNumber-1まで進む）
    FrameNumber:int}
