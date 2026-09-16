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

type AnimationType =
    |Loop of int*int
    |Range of int*int

    /// キャラクター表示
    /// 字幕表示
    /// 音声再生

type tposition = {
    /// x座標：時間（フレーム番号）の関数
    X:double0->double0;
    /// y座標：時間（フレーム番号）の関数
    Y:double0->double0}

type Line = {
    /// 始点
    Start:tposition;
    /// 終点
    End:tposition;}

type Ellipse = {
    /// 中心座標
    center:tposition;
    /// 半径(x)
    radiusX:double0->double0;
    /// 半径(y)
    radiusY:double0->double0;}

type Arc = {
    /// 円弧の中心座標
    center:tposition;
    /// 開始角（度数法, 反時計回りに描画）
    angle1:double0->double0;
    /// 終了角（度数法, 反時計回りに描画）
    angle2:double0->double0;
    /// 円弧の半径
    radius:double0->double0;}

type Text = {
    /// 中心座標
    center:tposition;
    /// 表示するテキスト
    str:string; }

type MathText<'a when 'a :> INum0> = {
    /// 中心座標
    center:tposition;
    /// 表示する数式
    eq:'a; }

[<AutoOpen>]
module HtmlGenerationExtensions1 =
    type HtmlGenerationContext with
        
        member this.html = html this.BodyContext

module private AnimationRendering =
    let private target (context:Aqualis) (value:INum0) =
        Aqualis.merge context value.Context |> ignore
        context

    let render (context:Aqualis) (value:INum0) =
        value.Expr.eval (target context value)

    let renderDouble context (value:double0) =
        render context (value :> INum0)

    let inlineMath context (value:INum0) =
        "\\(" + render context value + "\\)"

    let time (context:Aqualis) =
        double0(Var(Dt,"t",NaN), context)

/// <summary>
/// 線分アニメーションを生成するクラス
/// </summary>
/// <param name="s">線の太さ、色を定義するスタイル情報</param>
/// <param name="canvasX">描画領域の横幅</param>
/// <param name="canvasY">描画領域の縦幅</param>
