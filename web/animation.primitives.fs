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

type AnimationLine(context:HtmlGenerationContext,s:Style,canvasX:int,canvasY:int) =
    let id = context.nextContentsID()
    let s0 = Style ([{Key="visibility";Value="hidden"}]@s.list)
    let s1 = Style ([{Key="visibility";Value="visible"}]@s.list)
    do
        context.html.taga ("line", [Atr("id",id);]@[s0.atr])
    /// <summary>
    /// 割り当てられたidを取得する
    /// </summary>
    member this.ID with get() = id
    /// <summary>
    /// 指定したLineオブジェクトをキャンパスに追加する
    /// </summary>
    /// <param name="f">描画対象となる線分</param>
    member this.P (f:Line) =
        context.switchAnimationSeq <| fun ctx ->
            let t = AnimationRendering.time ctx
            ctx.writein ("    var e = document.getElementById(\""+id+"\");")
            ctx.writein ("    var x1 = " + AnimationRendering.renderDouble ctx (f.Start.X t) + ";")
            ctx.writein ("    var y1 = " + AnimationRendering.renderDouble ctx (canvasY - f.Start.Y t) + ";")
            ctx.writein ("    var x2 = " + AnimationRendering.renderDouble ctx (f.End.X t) + ";")
            ctx.writein ("    var y2 = " + AnimationRendering.renderDouble ctx (canvasY - f.End.Y t) + ";")
            ctx.writein ("    e.setAttribute(\"style\"," + "\"" + s1.code0 + "\");")
            ctx.writein "    e.setAttribute(\"x1\", x1);"
            ctx.writein "    e.setAttribute(\"y1\", y1);"
            ctx.writein "    e.setAttribute(\"x2\", x2);"
            ctx.writein "    e.setAttribute(\"y2\", y2);"
        context.switchJSAnimationSeqReset <| fun ctx ->
            ctx.writein ("    var e = document.getElementById(\""+id+"\");")
            ctx.writein ("    e.setAttribute(\"style\"," + "\"" + s0.code0 + "\");")

/// <summary>
/// 円アニメーションを生成するクラス
/// </summary>
/// <param name="s">線の太さ、色を定義するスタイル情報</param>
/// <param name="canvasX">描画領域の横幅</param>
/// <param name="canvasY">描画領域の縦幅</param>
type AnimationEllipse(context:HtmlGenerationContext,s:Style,canvasX:int,canvasY:int) =
    let id = context.nextContentsID()
    let s0 = Style ([{Key="visibility";Value="hidden"}]@s.list)
    let s1 = Style ([{Key="visibility";Value="visible"}]@s.list)
    do
        context.html.taga ("ellipse", [Atr("id",id);]@[s0.atr])
    /// <summary>
    /// 割り当てられたidを取得する
    /// </summary>
    member this.ID with get() = id
    /// <summary>
    /// 指定したEllipseオブジェクトをキャンパスに追加する
    /// </summary>
    /// <param name="e">描画対象となる円</param>
    member this.P (e:Ellipse) =
        context.switchAnimationSeq <| fun ctx ->
            let t = AnimationRendering.time ctx
            ctx.writein ("    var e = document.getElementById(\""+id+"\");")
            ctx.writein ("    var cx = " + AnimationRendering.renderDouble ctx (e.center.X t) + ";")
            ctx.writein ("    var cy = " + AnimationRendering.renderDouble ctx (canvasY - e.center.Y t) + ";")
            ctx.writein ("    var rx = " + AnimationRendering.renderDouble ctx (e.radiusX t) + ";")
            ctx.writein ("    var ry = " + AnimationRendering.renderDouble ctx (e.radiusY t) + ";")
            ctx.writein ("    e.setAttribute(\"style\"," + "\"" + s1.code0 + "\");")
            ctx.writein "    e.setAttribute(\"cx\", cx);"
            ctx.writein "    e.setAttribute(\"cy\", cy);"
            ctx.writein "    e.setAttribute(\"rx\", rx);"
            ctx.writein "    e.setAttribute(\"ry\", ry);"
        context.switchJSAnimationSeqReset <| fun ctx ->
            ctx.writein ("    var e = document.getElementById(\""+id+"\");")
            ctx.writein ("    e.setAttribute(\"style\"," + "\"" + s0.code0 + "\");")

/// <summary>
/// 円弧アニメーションを生成するクラス
/// </summary>
/// <param name="s">線の太さ、色を定義するスタイル情報</param>
/// <param name="canvasX">描画領域の横幅</param>
/// <param name="canvasY">描画領域の縦幅</param>
type AnimationArc(context:HtmlGenerationContext,s:Style,canvasX:int,canvasY:int) =
    let id = context.nextContentsID()
    let s0 = Style ([{Key="visibility";Value="hidden"}]@s.list)
    let s1 = Style ([{Key="visibility";Value="visible"}]@s.list)
    do
        context.html.taga ("path", [Atr("id",id);]@[s0.atr])
    /// <summary>
    /// 割り当てられたidを取得する
    /// </summary>
    member this.ID with get() = id
    /// <summary>
    /// 指定したArcオブジェクトをキャンパスに追加する
    /// </summary>
    /// <param name="e">描画対象となる円弧</param>
    member this.P (e:Arc) =
        context.switchAnimationSeq <| fun ctx ->
            let t = AnimationRendering.time ctx
            ctx.writein ("    var e = document.getElementById(\""+id+"\");")
            let a1 = Math.PI * e.angle1 t / 180
            let x1 = e.center.X t + e.radius t * asm.cos a1
            let y1 = e.center.Y t + e.radius t * asm.sin a1
            ctx.writein ("    var x1 = " + AnimationRendering.renderDouble ctx x1 + ";")
            ctx.writein ("    var y1 = " + AnimationRendering.renderDouble ctx (canvasY - y1) + ";")
            let a2 = Math.PI * e.angle2 t / 180 - 1E-4
            let x2 = e.center.X t + e.radius t * asm.cos a2
            let y2 = e.center.Y t + e.radius t * asm.sin a2
            ctx.writein ("    var x2 = " + AnimationRendering.renderDouble ctx x2 + ";")
            ctx.writein ("    var y2 = " + AnimationRendering.renderDouble ctx (canvasY - y2) + ";")
            ctx.writein ("    var a1 = " + AnimationRendering.renderDouble ctx (e.angle1 t) + ";")
            ctx.writein ("    var a2 = " + AnimationRendering.renderDouble ctx (e.angle2 t) + ";")
            ctx.writein ("    var radiusX = " + AnimationRendering.renderDouble ctx (e.radius t) + ";")
            ctx.writein ("    var radiusY = " + AnimationRendering.renderDouble ctx (e.radius t) + ";")
            ctx.writein "    var da = a2 - a1;"
            ctx.writein "    if(da < 0.0) {da = a2 + 360 - a1;}"
            ctx.writein "    var largerOrSmaller = 0;"
            ctx.writein "    if(da > 180.0) {largerOrSmaller = 1;}"
            ctx.writein ("    d = \"M \" + x1 + \" \" + y1 + \" A \" + radiusX + \" \" + radiusY + \" 0 \" + largerOrSmaller + \" 0 \" + x2 + \" \" + y2 " + ";")
            ctx.writein ("    e.setAttribute(\"style\"," + "\"" + s1.code0 + "\");")
            ctx.writein ("    e.setAttribute(\"d\", " + "d" + ");")
        context.switchJSAnimationSeqReset <| fun ctx ->
            ctx.writein ("    var e = document.getElementById(\""+id+"\");")
            ctx.writein ("    e.setAttribute(\"style\"," + "\"" + s0.code0 + "\");")

/// <summary>
/// テキスト・数式アニメーションを生成するクラス
/// </summary>
/// <param name="s">線の太さ、色を定義するスタイル情報</param>
/// <param name="canvasX">描画領域の横幅</param>
/// <param name="canvasY">描画領域の縦幅</param>
type AnimationText(context:HtmlGenerationContext,s:Style,originX:int,originY:int,canvasX:int,canvasY:int) =
    let id = context.nextContentsID()
    let ss = Style ([{Key="position";Value="absolute"}]@s.list)
    let ss0 = Style ([{Key="display";Value="none"}]@ss.list)
    let ss1 = Style ([{Key="display";Value="block"}]@ss.list)
    do
        context.html.tagb ("div", [Atr("id",id); ss0.atr]) <| fun () -> ()
    /// <summary>
    /// 割り当てられたidを取得
    /// </summary>
    member this.ID with get() = id
    /// <summary>
    /// 指定したTextオブジェクトをキャンパスに追加する
    /// </summary>
    /// <param name="e">対象となるテキスト</param>
    member this.P (e:Text) =
        context.switchAnimationSeq <| fun ctx ->
            let t = AnimationRendering.time ctx
            ctx.writein ("    var e = document.getElementById(\""+id+"\");")
            ctx.writein ("    e.setAttribute(\"style\"," + "\"" + ss1.code0 + "\");")
            ctx.writein ("    e.innerHTML = \"" + e.str + "\";")
            ctx.writein ("    var x = " + AnimationRendering.renderDouble ctx (originX + e.center.X t) + ";")
            ctx.writein ("    var y = " + AnimationRendering.renderDouble ctx (originY + canvasY - e.center.Y t) + ";")
            ctx.writein "    x = x - e.offsetWidth/2;"
            ctx.writein "    y = y - e.offsetHeight/2;"
            ctx.writein ("    e.setAttribute(\"style\"," + "\"" + ss1.code0 + " margin-left: \"+String(x)+\"px; margin-top: \"+String(y)+\"px; \");")
        context.switchJSAnimationSeqReset <| fun ctx ->
            ctx.writein ("    var e = document.getElementById(\""+id+"\");")
            ctx.writein ("    e.setAttribute(\"style\"," + "\"" + ss0.code0 + "\");")
    /// <summary>
    /// 指定したMathTextオブジェクトをキャンパスに追加する
    /// </summary>
    /// <param name="e">対象となる数式</param>
    member this.P (e:MathText<'a>) =
        context.switchAnimationSeq <| fun ctx ->
            let t = AnimationRendering.time ctx
            ctx.writein ("    var e = document.getElementById(\""+id+"\");")
            ctx.writein ("    e.setAttribute(\"style\"," + "\"" + ss1.code0 + "\");")
            ctx.writein ("    e.innerHTML = \"\\\\(" + AnimationRendering.render ctx (e.eq :> INum0) + "\\\\)\";")
            ctx.writein "    MathJax.typeset();"
            ctx.writein ("    var x =" + AnimationRendering.renderDouble ctx (originX + e.center.X t) + ";")
            ctx.writein ("    var y =" + AnimationRendering.renderDouble ctx (originY + canvasY - e.center.Y t) + ";")
            ctx.writein "    x = x - e.offsetWidth/2;"
            ctx.writein "    y = y - e.offsetHeight/2;"
            ctx.writein ("    e.setAttribute(\"style\"," + "\"" + ss1.code0 + " margin-left: \"+String(x)+\"px; margin-top: \"+String(y)+\"px; \");")
        context.switchJSAnimationSeqReset <| fun ctx ->
            ctx.writein ("    var e = document.getElementById(\""+id+"\");")
            ctx.writein ("    e.setAttribute(\"style\"," + "\"" + ss0.code0 + "\");")

/// <summary>
/// 多角形アニメーションを生成するクラス
/// </summary>
/// <param name="s">線の太さ、色を定義するスタイル情報</param>
/// <param name="canvasX">描画領域の横幅</param>
/// <param name="canvasY">描画領域の縦幅</param>
type AnimationPolygon(context:HtmlGenerationContext,s:Style,canvasX:int,canvasY:int) =
    let id = context.nextContentsID()
    let s0 = Style ([{Key="visibility";Value="hidden"}]@s.list)
    let s1 = Style ([{Key="visibility";Value="visible"}]@s.list)
    do
        context.html.taga ("polygon", [Atr("id", id);] @ [s.atr])
    /// <summary>
    /// 割り当てられたidを取得する
    /// </summary>
    member this.ID with get() = id
    /// <summary>
    /// 指定した頂点座標のリストを多角形としてキャンパスに追加する
    /// </summary>
    /// <param name="apex">多角形を構成する頂点座標のリスト</param>
    member this.P (apex:list<tposition>) =
        context.switchAnimationSeq <| fun ctx ->
            let t = AnimationRendering.time ctx
            ctx.writein ("    var e = document.getElementById(\"" + id + "\");")
            ctx.writein "    var p = \"\";"
            for p in apex do
                ctx.writein ("    var x = " + AnimationRendering.renderDouble ctx (p.X t) + ";")
                ctx.writein ("    var y = " + AnimationRendering.renderDouble ctx (canvasY - p.Y t) + ";")
                ctx.writein "    p = p + String(x) + \",\" + String(y) + \" \";"
            ctx.writein ("    e.setAttribute(\"style\"," + "\"" + s1.code0 + "\");")
            ctx.writein "    e.setAttribute(\"points\", p);"
        context.switchJSAnimationSeqReset <| fun ctx ->
            ctx.writein ("    var e = document.getElementById(\""+id+"\");")
            ctx.writein ("    e.setAttribute(\"style\"," + "\"" + s0.code0 + "\");")

/// <summary>
/// スライドアニメーション全体を管轄するクラス
/// </summary>
