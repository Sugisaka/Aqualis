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

/// Draws animated primitives inside a figure canvas.
type FigureAnimation(context:HtmlGenerationContext,figcounter:int,originX:int,originY:int,canvasX:int,canvasY:int) =
    let padding = 10.0
    /// アニメーションの実行順序リスト
    let mutable animeFlow:list<string*string*AnimationSetting*bool> = []
    let mutable counter = 0
    /// Gets the padding applied around the animation canvas.
    member _.Padding with get() = padding
    /// Gets the generated identifier for the current figure animation.
    member _.id with get() = "fa"+InvariantFormat.integer figcounter+"_"+InvariantFormat.integer counter
    /// <summary>
    /// アニメーションの実行順序を返す
    /// </summary>
    /// <param name="setting">アニメーションの実行時間</param>
    /// <param name="setFigure">図形にアニメーション設定を適用する関数</param>
    member this.seq (setting:AnimationSetting) (setFigure:AnimationSetting->unit) =
        // アニメーションシーケンスIDを発行
        let idstart,idreset = context.nextAnimationSeqID()
        context.switchAnimationSeq <| fun ctx ->
            ctx.writein ("function "+idstart+"(t){")
        context.switchJSAnimationSeqReset <| fun ctx ->
            ctx.writein ("function "+idreset+"(){")
        setFigure setting
        context.switchAnimationSeq <| fun ctx ->
            ctx.writein "}"
        context.switchJSAnimationSeqReset <| fun ctx ->
            ctx.writein "}"
        animeFlow <- animeFlow@[idstart,idreset,setting,false]
    /// <summary>
    /// アニメーションをループする
    /// </summary>
    member this.loop (setting:AnimationSetting) (setFigure:AnimationSetting->unit) =
        // アニメーションシーケンスIDを発行
        let idstart,idreset = context.nextAnimationSeqID()
        context.switchAnimationSeq <| fun ctx ->
            ctx.writein ("function "+idstart+"(t){")
        context.switchJSAnimationSeqReset <| fun ctx ->
            ctx.writein ("function "+idreset+"(){")
        setFigure setting
        context.switchAnimationSeq <| fun ctx ->
            ctx.writein "}"
        context.switchJSAnimationSeqReset <| fun ctx ->
            ctx.writein "}"
        animeFlow <- animeFlow@[idstart,idreset,setting,true]
    /// <summary>
    /// キャンバスアニメーションを指定して図形アニメーションを生成
    /// </summary>
    /// <param name="s">アニメーション設定</param>
    member this.animationEllipse s = AnimationEllipse(context,s,canvasX,canvasY)
    /// Creates an animated line with the supplied style.
    member this.animationLine s = AnimationLine(context,s,canvasX,canvasY)
    /// Creates an animated arc with the supplied style.
    member this.animationArc s = AnimationArc(context,s,canvasX,canvasY)
    /// Creates animated text with the supplied style.
    member this.animationText s = AnimationText(context,s,originX,originY,canvasX,canvasY)
    /// Creates an animated polygon with the supplied style.
    member this.animationPolygon s = AnimationPolygon(context,s,canvasX,canvasY)
    /// <summary>
    /// 直線を描画
    /// </summary>
    /// <param name="s">適用するスタイル</param>
    /// <param name="startP, endP">直線の始点、終点</param>
    member this.line (s:Style) (startP:position) (endP:position) =
        let c = [
            Atr("x1",InvariantFormat.number startP.x)
            Atr("y1",InvariantFormat.number (double canvasY-startP.y))
            Atr("x2",InvariantFormat.number endP.x)
            Atr("y2",InvariantFormat.number (double canvasY-endP.y))]
        context.html.taga ("line", [s.atr]@c)
    /// <summary>
    /// 楕円を描画
    /// </summary>
    /// <param name="center">楕円の中心座標</param>
    /// <param name="radiusX, radiusY">x軸、Y軸方向の半径</param>
    member this.ellipse (s:Style) (center:position) (radiusX:float,radiusY:float) =
        let c = [
            Atr("cx",InvariantFormat.number center.x)
            Atr("cy",InvariantFormat.number (double canvasY-center.y))
            Atr("rx",InvariantFormat.number radiusX)
            Atr("ry",InvariantFormat.number radiusY)]
        context.html.taga ("ellipse", [s.atr]@c)
    /// <summary>
    /// 円を描画
    /// </summary>
    member this.circle (s:Style) (center:position) (radius:float) =
        this.ellipse s center (radius,radius)
    /// <summary>
    /// 円弧を描画
    /// </summary>
    /// <param name="center">円弧の中心座標</param>
    /// <param name="radiusX, radiusY">x軸、Y軸方向の半径</param>
    /// <param name="theta1, theta2">円弧の開始角、終了角</param>
    member this.ellipseArc (s:Style) (center:position) (radiusX:float,radiusY:float) (theta1:float,theta2:float) =
        let x1 = center.x + radiusX * cos theta1
        let y1 = center.y + radiusY * sin theta1
        let x2 = center.x + radiusX * cos theta2
        let y2 = center.y + radiusY * sin theta2
        let d =
            if theta2-theta1 < Math.PI then
                "M " + InvariantFormat.number x1 + " " + InvariantFormat.number (float canvasY-y1) + " A " + InvariantFormat.number radiusX + " " + InvariantFormat.number radiusY + " 0 0 0 " + InvariantFormat.number x2 + " " + InvariantFormat.number (float canvasY-y2)
            else
                "M " + InvariantFormat.number x1 + " " + InvariantFormat.number (float canvasY-y1) + " A " + InvariantFormat.number radiusX + " " + InvariantFormat.number radiusY + " 0 1 0 " + InvariantFormat.number x2 + " " + InvariantFormat.number (float canvasY-y2)
        context.html.taga ("path", [s.atr]@[Atr("d",d)])
    /// <summary>
    /// 多角形を描画
    /// </summary>
    /// <param name="apex">多角形を構成する頂点のリスト</param>
    member this.polygon (s:Style) (apex:list<position>) =
        let pp =
            apex
            |> List.map (fun p -> InvariantFormat.number p.x + "," + InvariantFormat.number (double canvasY-p.y))
            |> fun s -> String.Join(",",s)
        context.html.taga ("polygon", [s.atr]@[Atr("points",pp)])
    /// <summary>
    /// 折れ線を描画
    /// </summary>
    /// <param name="apex">折れ線を構成する頂点のリスト</param>
    member this.polyline (s:Style) (apex:list<position>) =
        let pp =
            apex
            |> List.map (fun p -> InvariantFormat.number p.x + "," + InvariantFormat.number (double canvasY-p.y))
            |> fun s -> String.Join(",",s)
        context.html.taga ("polyline", [s.atr]@[Atr("points",pp)])
    /// <summary>
    /// 始点から終点に向かう矢印付き直線を描画
    /// </summary>
    /// <param name="lineWidth">線の太さ</param>
    /// <param name="startP, endP">直線の始点、終点</param>
    member this.linearrow (s:Style) (lineWidth:float) (startP:position) (endP:position) =
        let r = 12.0
        let pi = 3.14159265358979
        let t0 = atan2 (startP.y-endP.y) (startP.x-endP.x)
        let q1x = endP.x + r*cos(t0-15.0*pi/180.0)
        let q1y = endP.y + r*sin(t0-15.0*pi/180.0)
        let q2x = endP.x + r*cos(t0+15.0*pi/180.0)
        let q2y = endP.y + r*sin(t0+15.0*pi/180.0)
        let ux,uy =
            let c = lineWidth/sqrt((endP.x-startP.x)*(endP.x-startP.x)+(endP.y-startP.y)*(endP.y-startP.y))
            endP.x + (startP.x-endP.x)*c,
            endP.y + (startP.y-endP.y)*c
        this.line (s+Style[stroke.width lineWidth]) startP (position(ux,uy))
        this.polygon s [position(q1x,q1y);endP;position(q2x,q2y)]
    /// <summary>
    /// 四角形を描画
    /// </summary>
    /// <param name="center">四角形の中心座標</param>
    /// <param name="sx, sy">四角形の横幅、縦幅</param>
    member this.rect (s:Style) (center:position) (sx:float,sy:float) =
        let c = [
            Atr("x",InvariantFormat.number (center.x-0.5*sx))
            Atr("y",InvariantFormat.number (double canvasY-center.y-0.5*sy))
            Atr("width",InvariantFormat.number sx)
            Atr("height",InvariantFormat.number sy)]
        context.html.taga ("rect", [s.atr]@c)
    /// <summary>
    /// テキストを表示
    /// </summary>
    /// <param name="center">テキスト表示位置</param>
    /// <param name="str">表示するテキスト</param>
    member this.text (s:Style) (center:position) (str:string) =
        let c = [
            {Key="display";Value="block"}
            {Key="position";Value="absolute"}
            {Key="margin-left";Value=InvariantFormat.number (double originX+center.x)+"px"}
            {Key="margin-top";Value=InvariantFormat.number (double originY+double canvasY-center.y)+"px"}]
        let ss = Style (s.list@c)
        context.html.tagb ("div", [ss.atr]) <| fun () ->
            context.BodyContext.html.text str
    /// <summary>
    /// 数式を描画
    /// </summary>
    /// <param name="e">表示する数式</param>
    member this.eqi (s:Style) (center:position) (e:int0) =
        let c = [
            {Key="display";Value="block"}
            {Key="position";Value="absolute"}
            {Key="margin-left";Value=InvariantFormat.number (double originX+center.x)+"px"}
            {Key="margin-top";Value=InvariantFormat.number (double originY+double canvasY-center.y)+"px"}]
        let ss = Style (s.list@c)
        context.html.tagb ("div", [ss.atr]) <| fun () ->
            context.BodyContext.writein ("\\(" + e.Expr.evalH context.BodyContext + "\\)")
    /// <summary>
    /// 数式を描画
    /// </summary>
    /// <param name="e">表示する数式</param>
    member this.eqd (s:Style) (center:position) (e:double0) =
        let c = [
            {Key="display";Value="block"}
            {Key="position";Value="absolute"}
            {Key="margin-left";Value=InvariantFormat.number (double originX+center.x)+"px"}
            {Key="margin-top";Value=InvariantFormat.number (double originY+double canvasY-center.y)+"px"}]
        let ss = Style (s.list@c)
        context.html.tagb ("div", [ss.atr]) <| fun () ->
            context.BodyContext.writein ("\\(" + e.Expr.evalH context.BodyContext + "\\)")
    /// <summary>
    /// 数式を描画
    /// </summary>
    /// <param name="e">表示する数式</param>
    member this.eqz (s:Style) (center:position) (e:complex0) =
        let c = [
            {Key="display";Value="block"}
            {Key="position";Value="absolute"}
            {Key="margin-left";Value=InvariantFormat.number (double originX+center.x)+"px"}
            {Key="margin-top";Value=InvariantFormat.number (double originY+double canvasY-center.y)+"px"}]
        let ss = Style (s.list@c)
        context.html.tagb ("div", [ss.atr]) <| fun () ->
            context.BodyContext.writein ("\\(" + e.Expr.evalH context.BodyContext + "\\)")
    /// <summary>
    /// 画像を表示
    /// </summary>
    /// <param name="filename">画像のファイル名</param>
    member this.image (s:Style) (center:position) (filename:string) =
        let sourceUrl = context.ImportAsset filename
        let c = [
            {Key="display";Value="block"}
            {Key="position";Value="absolute"}
            {Key="margin-left";Value=InvariantFormat.number (double originX+center.x)+"px"}
            {Key="margin-top";Value=InvariantFormat.number (double originY+double canvasY-center.y)+"px"}]
        let ss = Style (s.list@c)
        context.html.taga ("img", [ss.atr; Atr("src", sourceUrl)])
    /// <summary>
    /// 開始ボタンの制御用JavaScriptコードを生成
    /// </summary>
    /// <param name="buttonIndex">対象となるボタンの識別子</param>
    member this.jsStartControll(buttonIndex:string) =
        let fname = "start" + buttonIndex
        context.switchJSAnimationStart <| fun ctx ->
            ctx.writein (fname+": () => {")
            for idstart,_,setting,isLoop in animeFlow do
                if isLoop then
                    ctx.writein ("    repeat(" + idstart + ", " + InvariantFormat.integer setting.FrameTime + ", " + InvariantFormat.integer setting.FrameNumber + ");")
                else
                    ctx.writein ("    repeatSeq(" + idstart + ", " + InvariantFormat.integer setting.FrameTime + ", " + InvariantFormat.integer setting.FrameNumber + ", () => {")
            for _,_,_,isLoop in animeFlow do
                if isLoop then
                    ()
                else
                    ctx.writein "    });"
            ctx.writein "},"
        fname
    /// <summary>
    /// リセットボタンの制御用JavaScriptコードを生成
    /// </summary>
    member this.jsResetControll(buttonIndex:string) =
        let fname = "reset" + buttonIndex
        context.switchJSAnimationReset <| fun ctx ->
            ctx.writein (fname+": () => {")
            for _,idreset,_,_ in animeFlow do
                ctx.writein ("    " + idreset + "();")
            ctx.writein "},"
        fname
    /// <summary>
    /// アニメーション用のJavaScriptコードを生成
    /// </summary>
    member _.jsAnimation codejs =
        context.switchBody <| fun ctx ->
            ctx.writein "var t = 0;"
            ctx.writein "var dt = 1;"
            ctx.writein "window.onload=function(){"
            ctx.writein "    var timer;"
            ctx.writein "    var delay = 33;"
            ctx.writein "    var loop = function(){"
            ctx.writein "        t = t + dt;"
            ctx.writein "        if(t >= 100){t = 0;}"
            ctx.writein "        clearTimeout(timer);"
            ctx.writein "        timer=setTimeout(loop,delay);"
            ctx.writein "    }"
            ctx.writein "    loop();"
            ctx.writein "}"
            ctx.writein codejs

