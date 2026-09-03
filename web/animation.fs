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
type ContextSlideAnimation internal (context:HtmlGenerationContext) =
    /// <summary>
    /// 登録された音声ファイルの一覧を書きだす
    /// </summary>
    member this.writeAudioList() =
        context.switchJSMain <| fun ctx ->
            let audioFilesJson = JsonSerializer.Serialize(context.AudioFiles)
            ctx.writein ("const audioList = " + audioFilesJson + ";")
    /// <summary>
    /// キャラクター表示を制御するJavaScriptコードの生成
    /// </summary>
    member this.jsSetCharacter() =
        context.switchJSMain <| fun ctx ->
            ctx.writein "let pagecount = 1;"
            ctx.writein "function setCharacter()"
            ctx.writein "{"
            ctx.writein "        const swc = document.getElementById(\"switchCharacter\");"
            ctx.writein "        const c = document.getElementById(\"c\"+pagecount);"
            ctx.writein "        if(swc.checked)"
            ctx.writein "        {"
            ctx.writein "            c.style.display = \"block\";"
            ctx.writein "        }"
            ctx.writein "        else"
            ctx.writein "        {"
            ctx.writein "            c.style.display = \"none\";"
            ctx.writein "        }"
            ctx.writein "}"
    /// <summary>
    /// 字幕表示を制御するJavaScriptコードの生成
    /// </summary>
    member this.jsSetSubtitle() =
        context.switchJSMain <| fun ctx ->
            ctx.writein "function setSubtitle()"
            ctx.writein "{"
            ctx.writein "        const sws = document.getElementById(\"switchSubtitle\");"
            ctx.writein "        const b2 = document.getElementById(\"sb\"+pagecount);"
            ctx.writein "        const s2 = document.getElementById(\"s\"+pagecount);"
            ctx.writein "        if(sws.checked)"
            ctx.writein "        {"
            ctx.writein "            b2.style.display = \"block\";"
            ctx.writein "            s2.style.display = \"block\";"
            ctx.writein "        }"
            ctx.writein "        else"
            ctx.writein "        {"
            ctx.writein "            b2.style.display = \"none\";"
            ctx.writein "            s2.style.display = \"none\";"
            ctx.writein "        }"
            ctx.writein "}"
    /// <summary>
    /// 次のページへの遷移を制御するJavaScriptコードの生成
    /// </summary>
    member this.jsDrawNext(audioDir:string) =
        context.switchJSMain <| fun ctx ->
            let animationCount = context.AnimationCount
            let audioDirectoryLiteral = JsonSerializer.Serialize(audioDir + "/")
            ctx.writein "function drawNext()"
            ctx.writein "{"
            ctx.writein "    resetAll();"
            ctx.writein ("    if(pagecount<"+animationCount.ToString()+")")
            ctx.writein "    {"
            ctx.writein "        const swc = document.getElementById(\"switchCharacter\");"
            ctx.writein "        const sws = document.getElementById(\"switchSubtitle\");"
            ctx.writein "        const swa = document.getElementById(\"switchAudio\");"
            ctx.writein "        "
            ctx.writein "        const p1 = document.getElementById(\"p\"+pagecount);"
            ctx.writein "        p1.style.display = \"none\";"
            ctx.writein "        const b1 = document.getElementById(\"sb\"+pagecount);"
            ctx.writein "        b1.style.display = \"none\";"
            ctx.writein "        const s1 = document.getElementById(\"s\"+pagecount);"
            ctx.writein "        s1.style.display = \"none\";"
            ctx.writein "        const c1 = document.getElementById(\"c\"+pagecount);"
            ctx.writein "        c1.style.display = \"none\";"
            ctx.writein "        pagecount++;"
            ctx.writein "        const p2 = document.getElementById(\"p\"+pagecount);"
            ctx.writein "        p2.style.display = \"block\";"
            ctx.writein "        if(sws.checked)"
            ctx.writein "        {"
            ctx.writein "            const b2 = document.getElementById(\"sb\"+pagecount);"
            ctx.writein "            b2.style.display = \"block\";"
            ctx.writein "            const s2 = document.getElementById(\"s\"+pagecount);"
            ctx.writein "            s2.style.display = \"block\";"
            ctx.writein "        }"
            ctx.writein "        else"
            ctx.writein "        {"
            ctx.writein "            const b2 = document.getElementById(\"sb\"+pagecount);"
            ctx.writein "            b2.style.display = \"none\";"
            ctx.writein "            const s2 = document.getElementById(\"s\"+pagecount);"
            ctx.writein "            s2.style.display = \"none\";"
            ctx.writein "        }"
            ctx.writein "        if(swc.checked)"
            ctx.writein "        {"
            ctx.writein "            const c2 = document.getElementById(\"c\"+pagecount);"
            ctx.writein "            c2.style.display = \"block\";"
            ctx.writein "        }"
            ctx.writein "        else"
            ctx.writein "        {"
            ctx.writein "            const c2 = document.getElementById(\"c\"+pagecount);"
            ctx.writein "            c2.style.display = \"none\";"
            ctx.writein "        }"
            ctx.writein "        const audioPlayer = document.getElementById(\"audioPlayer\");"
            ctx.writein "        if(audioList[pagecount-1] != \"\" && swa.checked)"
            ctx.writein "        {"
            ctx.writein ("            audioPlayer.src = " + audioDirectoryLiteral + " + audioList[pagecount-1];")
            ctx.writein "            audioPlayer.play();"
            ctx.writein "        }"
            ctx.writein "        autoAnimationMap['page'+pagecount]();"
            ctx.writein "    }"
            ctx.writein "}"
    /// <summary>
    /// 前のページへの遷移を制御するJavaScriptコードの生成
    /// </summary>
    member this.jsDrawPrev(audioDir:string) =
        context.switchJSMain <| fun ctx ->
            let audioDirectoryLiteral = JsonSerializer.Serialize(audioDir + "/")
            ctx.writein "function drawPrev()"
            ctx.writein "{"
            ctx.writein "    resetAll();"
            ctx.writein "    if(pagecount>1)"
            ctx.writein "    {"
            ctx.writein "        const swc = document.getElementById(\"switchCharacter\");"
            ctx.writein "        const sws = document.getElementById(\"switchSubtitle\");"
            ctx.writein "        const swa = document.getElementById(\"switchAudio\");"
            ctx.writein "        const p1 = document.getElementById(\"p\"+pagecount);"
            ctx.writein "        p1.style.display = \"none\";"
            ctx.writein "        const b1 = document.getElementById(\"sb\"+pagecount);"
            ctx.writein "        b1.style.display = \"none\";"
            ctx.writein "        const s1 = document.getElementById(\"s\"+pagecount);"
            ctx.writein "        s1.style.display = \"none\";"
            ctx.writein "        const c1 = document.getElementById(\"c\"+pagecount);"
            ctx.writein "        c1.style.display = \"none\";"
            ctx.writein "        pagecount--;"
            ctx.writein "        const p2 = document.getElementById(\"p\"+pagecount);"
            ctx.writein "        p2.style.display = \"block\";"
            ctx.writein "        if(sws.checked)"
            ctx.writein "        {"
            ctx.writein "            const b2 = document.getElementById(\"sb\"+pagecount);"
            ctx.writein "            b2.style.display = \"block\";"
            ctx.writein "            const s2 = document.getElementById(\"s\"+pagecount);"
            ctx.writein "            s2.style.display = \"block\";"
            ctx.writein "        }"
            ctx.writein "        else"
            ctx.writein "        {"
            ctx.writein "            const b2 = document.getElementById(\"sb\"+pagecount);"
            ctx.writein "            b2.style.display = \"none\";"
            ctx.writein "            const s2 = document.getElementById(\"s\"+pagecount);"
            ctx.writein "            s2.style.display = \"none\";"
            ctx.writein "        }"
            ctx.writein "        if(swc.checked)"
            ctx.writein "        {"
            ctx.writein "            const c2 = document.getElementById(\"c\"+pagecount);"
            ctx.writein "            c2.style.display = \"block\";"
            ctx.writein "        }"
            ctx.writein "        else"
            ctx.writein "        {"
            ctx.writein "            const c2 = document.getElementById(\"c\"+pagecount);"
            ctx.writein "            c2.style.display = \"none\";"
            ctx.writein "        }"
            ctx.writein "        const audioPlayer = document.getElementById(\"audioPlayer\");"
            ctx.writein "        if(audioList[pagecount-1] != \"\" && swa.checked)"
            ctx.writein "        {"
            ctx.writein ("            audioPlayer.src = " + audioDirectoryLiteral + " + audioList[pagecount-1];")
            ctx.writein "            audioPlayer.play();"
            ctx.writein "        }"
            ctx.writein "    }"
            ctx.writein "}"

    /// キャラクターのデフォルト表示・非表示設定
    /// 字幕のデフォルト表示・非表示設定
    /// 音声のデフォルト表示・非表示設定
    /// デフォルトの設定

[<AutoOpen>]
module HtmlWebExtensions =
    let private phpAttributeCode (name:string) (value:PHPdata) =
        let validName = HtmlEncoding.attributeName name
        validName + "=\"<?php echo htmlspecialchars((string)(" + value.code + "), ENT_QUOTES | ENT_SUBSTITUTE, 'UTF-8'); ?>\""

    type html with
        /// <summary>
        /// 内部要素のないタグ
        /// </summary>
        member this.taga (t:string,lst:list<string*PHPdata>) =
            this.Context.writei("<"+t+" ")
            this.Context.indentInc()
            for a,s in lst do
                this.Context.write (phpAttributeCode a s + " ")
            this.Context.indentDec()
            this.Context.writen  " />"
        /// <summary>
        /// 内部要素のあるタグ
        /// </summary>
        member this.tagb0 (t:string,lst:list<string*PHPdata>) = fun code ->
            if lst.Length=0 then
                this.Context.write ("<"+t+">")
            else
                this.Context.write ("<"+t+" ")
                this.Context.indentInc()
                for a,s in lst do
                    this.Context.write (phpAttributeCode a s + " ")
                this.Context.indentDec()
                this.Context.write ">"
            code()
            this.Context.writen ("</"+t+">")
        /// <summary>
        /// 内部要素のあるタグ
        /// </summary>
        member this.tagb (t:string,lst:list<string*PHPdata>) = fun code ->
            if lst.Length=0 then
                this.Context.writein ("<"+t+">")
            else
                this.Context.writei ("<"+t+" ")
                this.Context.indentInc()
                for a,s in lst do
                    this.Context.writei (phpAttributeCode a s + " ")
                this.Context.indentDec()
                this.Context.writen ">"
            code()
            this.Context.writein ("</"+t+">")
        /// <summary>
        /// 見出し（h1）要素を生成する
        /// </summary>
        /// <param name="t">見出しに表示する内容</param>
        member this.h1 (t:int0) = fun code ->
            this.tagb "h1" <| fun () -> this.Context.php.echo t.code
            code()
        /// <summary>
        /// 見出し（h1）要素を生成する
        /// </summary>
        /// <param name="t">見出しに表示する内容</param>
        /// <param name="atr">文字の太さ、色を定義するスタイル情報</param>
        member this.h1 (t:int0,s:Style) = fun code ->
            this.tagb ("h1",[s.atr]) <| fun () -> this.Context.php.echo t.code
            code()

        member this.h2 (t:int0) = fun code ->
            this.tagb "h2" <| fun () -> this.Context.php.echo t.code
            code()
        member this.h2 (t:int0,s:Style) = fun code ->
            this.tagb ("h2",[s.atr]) <| fun () -> this.Context.php.echo t.code
            code()

        member this.h3 (t:int0) = fun code ->
            this.tagb "h3" <| fun () -> this.Context.php.echo t.code
            code()
        member this.h3 (t:int0,s:Style) = fun code ->
            this.tagb ("h3",[s.atr]) <| fun () -> this.Context.php.echo t.code
            code()

        member this.h4 (t:int0) = fun code ->
            this.tagb "h4" <| fun () -> this.Context.php.echo t.code
            code()
        member this.h4 (t:int0,s:Style) = fun code ->
            this.tagb ("h4",[s.atr]) <| fun () -> this.Context.php.echo t.code
            code()

        member this.h5 (t:int0) = fun code ->
            this.tagb "h5" <| fun () -> this.Context.php.echo t.code
            code()
        member this.h5 (t:int0,s:Style) = fun code ->
            this.tagb ("h5",[s.atr]) <| fun () -> this.Context.php.echo t.code
            code()
        /// <summary>
        /// 見出し（h1）要素を生成する
        /// </summary>
        /// <param name="t">見出しに表示する内容</param>
        member this.h1 (t:double0) = fun code ->
            this.tagb "h1" <| fun () -> this.Context.php.echo t.code
            code()
        /// <summary>
        /// 見出し（h1）要素を生成する
        /// </summary>
        /// <param name="t">見出しに表示する内容</param>
        /// <param name="atr">文字の太さ、色を定義するスタイル情報</param>
        member this.h1 (t:double0,s:Style) = fun code ->
            this.tagb ("h1",[s.atr]) <| fun () -> this.Context.php.echo t.code
            code()

        member this.h2 (t:double0) = fun code ->
            this.tagb "h2" <| fun () -> this.Context.php.echo t.code
            code()
        member this.h2 (t:double0,s:Style) = fun code ->
            this.tagb ("h2",[s.atr]) <| fun () -> this.Context.php.echo t.code
            code()

        member this.h3 (t:double0) = fun code ->
            this.tagb "h3" <| fun () -> this.Context.php.echo t.code
            code()
        member this.h3 (t:double0,s:Style) = fun code ->
            this.tagb ("h3",[s.atr]) <| fun () -> this.Context.php.echo t.code
            code()

        member this.h4 (t:double0) = fun code ->
            this.tagb "h4" <| fun () -> this.Context.php.echo t.code
            code()
        member this.h4 (t:double0,s:Style) = fun code ->
            this.tagb ("h4",[s.atr]) <| fun () -> this.Context.php.echo t.code
            code()

        member this.h5 (t:double0) = fun code ->
            this.tagb "h5" <| fun () -> this.Context.php.echo t.code
            code()
        member this.h5 (t:double0,s:Style) = fun code ->
            this.tagb ("h5",[s.atr]) <| fun () -> this.Context.php.echo t.code
            code()
        /// <summary>
        /// フォーム送信用のsubmitボタンを生成する
        /// <para>
        /// nameとvalueの型違いに対応したオーバーロードを提供する
        /// </para>
        /// </summary>
        member this.submit(name:string,value:PHPdata) =
            this.taga("input",["type",PHPdata "submit"; "name",PHPdata name; "value",value])
        member this.submit(name:PHPdata,value:string) = this.taga("input",["type",PHPdata "submit"; "name", name; "value",PHPdata value])
        /// <summary>
        /// フォーム送信用のsubmitボタンを生成する
        /// </summary>
        /// <param name="name">name属性に設定する文字列</param>
        /// <param name="value">value属性に設定する文字列</param>
        member this.submit(name:string,value:string) =
            this.taga("input",[Atr("type", "submit"); Atr("name", name); Atr("value", value)])
        /// <summary>
        /// 送信先URLを指定したsubmitボタンを生成する
        /// </summary>
        /// <param name="url">formaction属性に設定するURL</param>
        /// <param name="name">name属性に設定するPHPデータ</param>
        /// <param name="value">value属性に設定する文字列</param>
        member this.submit(url:string,name:PHPdata,value:string) = this.taga("input",["type",PHPdata "submit"; "name", name; "value",PHPdata value; "formaction",PHPdata url])
        /// <summary>
        /// 無効化されたsubmitボタンを生成する
        /// </summary>
        /// <param name="name">name属性に設定するPHPデータ</param>
        /// <param name="value">value属性に設定するPHPデータ</param>
        member this.submit_disabled(name:PHPdata,value:PHPdata) = this.taga("input",["type",PHPdata "submit"; "name", name; "value",value; "disabled",PHPdata "disabled"])
        member this.submit_disabled(name:string,value:PHPdata) = this.taga("input",["type",PHPdata "submit"; "name",PHPdata name; "value",value; "disabled",PHPdata "disabled"])
        member this.submit_disabled(name:PHPdata,value:string) = this.taga("input",["type",PHPdata "submit"; "name", name; "value",PHPdata value; "disabled",PHPdata "disabled"])
        /// <summary>
        /// li要素を生成する
        /// </summary>
        /// <param name="a">li要素に設定する属性のリスト</param>
        member this.item (a:list<string*PHPdata>) = fun code -> this.tagb ("li",a) code
        /// <summary>
        /// a要素を生成する
        /// </summary>
        /// <param name="url">href属性に設定するPHPデータ</param>
        member this.link(url:PHPdata) = fun code -> this.tagb ("a",["href",url]) code
        /// <summary>
        /// a要素を生成する
        /// </summary>
        /// <param name="url">href属性に設定するPHPデータ</param>
        /// <param name="s">文字の太さ、色を定義するスタイル情報</param>
        member this.link(url:PHPdata, s:Style) = fun code ->
            this.tagb ("a",["style",PHPdata s.code0; "href",url]) code
        /// <summary>
        /// select要素を生成する
        /// </summary>
        /// <param name="x">name属性に設定するPHPデータ</param>
        member this.select(x:PHPdata) = fun code -> this.tagb ("select",["name",x;]) code
        /// <summary>
        /// 無効化されたselsect要素を生成する
        /// </summary>
        member this.select_disabled(x:PHPdata) = fun code -> this.tagb ("select",["name",x; "disabled",PHPdata "disabled"]) code
        /// <summary>
        /// 任意のHTMLタグの開始タグと終了タグを生成する
        /// </summary>
        /// <param name="t">タグ名</param>
        /// <param name="code">タグ内部の内容を生成する関数</param>
        member this.splitTag t code =
            let b (lst:list<string*PHPdata>) =
                if lst.Length=0 then
                    this.Context.writein ("<"+t+">")
                else
                    this.Context.writein ("<"+t+" ")
                    for a,s in lst do
                        this.Context.writein (phpAttributeCode a s + " ")
                    this.Context.writein ">"
            code b
            this.Context.writein ("</"+t+">")
        /// <summary>
        /// select要素を生成
        /// </summary>
        member this.Select = this.splitTag "select"
        /// <summary>
        /// tr要素を生成
        /// </summary>
        member this.Tr = this.splitTag "tr"
        /// <summary>
        /// div要素を生成する
        /// </summary>
        /// <param name="a">属性リスト</param>
        member this.div (a:list<string*PHPdata>) = fun code -> this.tagb ("div",a) code
        /// <summary>
        /// CSSdataの内容に応じてHTML要素を生成する
        /// </summary>
        /// <param name="a">生成する要素を指定するCSSデータ</param>
        member this.div (a:CSSdata) = fun code ->
            match a.label with
            |HTMLTag s -> this.tagb s code
            |CSSClass s -> this.tagb ("div",[Atr("class",s)]) code
            |CSSID s -> this.tagb ("div",[Atr("id",s)]) code
            |_ -> ()
        /// <summary>
        /// CSSdataの内容に応じてHTML要素を生成する
        /// </summary>
        /// <param name="a">生成対象を指定するCSSデータ</param>
        /// <param name="atr">追加する属性のリスト</param>
        member this.div (a:CSSdata,atr:list<Atr>) = fun code ->
            match a.label with
            |HTMLTag s -> this.tagb s code
            |CSSClass s -> this.tagb ("div",[Atr("class",s)]@atr) code
            |CSSID s -> this.tagb ("div",[Atr("id",s)]@atr) code
            |_ -> ()
        /// <summary>
        /// CSSdataに基づいてarticle要素を生成する
        /// </summary>
        /// <param name="a">要素に適用するCSSデータ</param>
        member this.article (a:CSSdata) = fun code ->
            match a.label with
            |CSSClass s -> this.tagb ("article",[Atr("class",s)]) code
            |CSSID s -> this.tagb ("article",[Atr("id",s)]) code
            |_ -> ()
        /// <summary>
        /// CSSdataに基づいてaside要素を生成する
        /// </summary>
        member this.aside (a:CSSdata) = fun code ->
            match a.label with
            |CSSClass s -> this.tagb ("aside",[Atr("class",s)]) code
            |CSSID s -> this.tagb ("aside",[Atr("id",s)]) code
            |_ -> ()
        /// <summary>
        /// CSSdataに基づいてpara要素を生成する
        /// </summary>
        member this.para (a:CSSdata) = fun code ->
            match a.label with
            |CSSClass s -> this.tagb ("p",[Atr("class",s)]) code
            |CSSID s -> this.tagb ("p",[Atr("id",s)]) code
            |_ -> ()
        /// <summary>
        /// CSSdataに基づいてsection要素を生成する
        /// </summary>
        member this.section (a:CSSdata) = fun code ->
            match a.label with
            |CSSClass s -> this.tagb ("section",[Atr("class",s)]) code
            |CSSID s -> this.tagb ("section",[Atr("id",s)]) code
            |_ -> ()
        /// <summary>
        /// CSSdataに基づいてspan要素を生成する
        /// </summary>
        member this.span (a:CSSdata) = fun code ->
            match a.label with
            // |CSSClass s -> this.tagb0 ("span",["class",s]) code
            // |CSSID s -> this.tagb0 ("span",["id"
            |CSSClass s -> this.tagb ("span",[Atr("class",s)]) code
            |CSSID s -> this.tagb ("span",[Atr("id",s)]) code
            |_ -> ()

        /// <summary>
        /// チェックボックス（チェックされたとき1、チェックされていないとき0を送信）
        /// </summary>
        member this.checkbox(name:PHPdata) =
            this.taga("input",["type",PHPdata "hidden"; "name", name; "value",PHPdata "0";])
            this.taga("input",["type",PHPdata "checkbox"; "name", name; "value",PHPdata "1";])
        /// <summary>
        /// チェックボックス（チェックされたとき1、チェックされていないとき0を送信）
        /// </summary>
        member this.checkbox_disabled(name:PHPdata) =
            this.taga("input",["type",PHPdata "hidden"; "name", name; "value",PHPdata "0";])
            this.taga("input",["type",PHPdata "checkbox"; "name", name; "value",PHPdata "1"; "disabled",PHPdata "disabled"])
        /// <summary>
        /// チェックボックス（チェックされたとき1、チェックされていないとき0を送信）
        /// </summary>
        member this.checkbox_checked(name:PHPdata) =
            this.taga("input",["type",PHPdata "hidden"; "name", name; "value",PHPdata "0";])
            this.taga("input",["type",PHPdata "checkbox"; "name", name; "value",PHPdata "1"; "checked",PHPdata "checked";])
        /// <summary>
        /// チェックボックス（チェックされたとき1、チェックされていないとき0を送信）
        /// </summary>
        member this.checkbox_checked_disabled(name:PHPdata) =
            this.taga("input",["type",PHPdata "hidden"; "name", name; "value",PHPdata "0";])
            this.taga("input",["type",PHPdata "checkbox"; "name", name; "value",PHPdata "1"; "checked",PHPdata "checked"; "disabled",PHPdata "disabled"])
        /// <summary>
        /// 指定位置に数式テキストを描画する
        /// </summary>
        /// <param name="s">適用するスタイル</param>
        /// <param name="p">表示位置</param>
        /// <param name="text">表示する数式</param>
        member this.Mathtext (s:Style) (p:position) (text:PHPdata) =
            let s1 = Style [{Key = "margin-left"; Value=InvariantFormat.number p.x+"px"}
                            {Key = "margin-top"; Value=InvariantFormat.number p.y+"px"}
                            {Key = "position"; Value = "absolute";}]
            this.tagb ("div", [(s1+s).atr]) <| fun () ->
                this.Context.writein ("\\(" + text.code + "\\)")


        /// <summary>
        /// コードブロックを生成
        /// </summary>
        member this.code (style:list<string*PHPdata>) = fun cd ->
            this.tagb0 ("pre",style) <| fun () ->
                this.tagb0 ("code",[]) <| fun () ->
                    cd()

        member this.code (style:list<string*PHPdata>, cd:PHPdata) =
            this.tagb0 ("pre",style) <| fun () ->
                this.tagb0 ("code",[]) <| fun () ->
                    this.Context.write cd.phpcode

        member this.code (style:list<string*string>) = this.code (style |> List.map (fun (a,b) -> a,PHPdata b))

        member this.code (style:list<string*string>, cd:PHPdata) = this.code (style |> List.map (fun (a,b) -> a,PHPdata b),cd)

        member this.code (cd:PHPdata) = this.code (([] : (string * PHPdata) list),cd)
        /// <summary>
        /// 罫線指定付きの表を生成
        /// </summary>
        /// <param name="caption">表のタイトル</param>
        /// <param name="borderH">水平罫線の設定</param>
        /// <param name="borderV">垂直罫線の設定</param>
        /// <param name="tlist">表データ</param>
        member this.listTable (caption:string) = fun (borderH:list<BorderH>) (borderV:list<BorderV>) (tlist:list<list<string>>) ->
            this.tagb("div",[Atr("class", "fig")]) <| fun () ->
                this.tagb ("span",[Atr("class", "caption")]) <| fun () ->
                    this.Context.writein (caption)
                this.tagb("table",[Atr("class", "tab")]) <| fun () ->
                    for j in 0..tlist.Length-1 do
                        this.tagb ("tr",[Atr("class",match borderV[j] with |TrTB -> "trtb" |TrT -> "trt" |TrB -> "trb" |TrN -> "trn")]) <| fun () ->
                            for i in 0..tlist[j].Length-1 do
                                this.tagb ("td",[Atr("class",
                                    match borderH[i] with
                                    |TdL -> "tdl"
                                    |TdC -> "tdc"
                                    |TdR -> "tdr"
                                    |TdJ -> "tdj"
                                    |TdLL -> "tdlL"
                                    |TdCL -> "tdcL"
                                    |TdRL -> "tdrL"
                                    |TdJL -> "tdjL"
                                    |TdLR -> "tdlR"
                                    |TdCR -> "tdcR"
                                    |TdRR -> "tdrR"
                                    |TdJR -> "tdjR"
                                    |TdLLR -> "tdlLR"
                                    |TdCLR -> "tdcLR"
                                    |TdRLR -> "tdrLR"
                                    |TdJLR -> "tdjLR")]) <| fun () ->
                                    this.Context.writein (tlist[j][i])
        /// <summary>
        /// num0式を評価し、インラインMathJax文字列を返す
        /// </summary>
        member this.inlineMath(text:int0) =
            AnimationRendering.inlineMath this.Context (text :> INum0)
        /// <summary>
        /// num0式を評価し、インラインMathJax文字列を返す
        /// </summary>
        member this.inlineMath(text:double0) =
            AnimationRendering.inlineMath this.Context (text :> INum0)
        /// <summary>
        /// num0式を評価し、インラインMathJax文字列を返す
        /// </summary>
        member this.inlineMath(text:complex0) =
            AnimationRendering.inlineMath this.Context (text :> INum0)
        /// <summary>
        /// num0式を評価し、MathJax形式で出力する
        /// </summary>
        member this.eq(text:int0) =
            this.Context.writein ("\\("+text.Expr.evalL this.Context + "\\)")
        /// <summary>
        /// num0式を評価し、MathJax形式で出力する
        /// </summary>
        member this.eq(text:double0) =
            this.Context.writein ("\\("+text.Expr.evalL this.Context + "\\)")
        /// <summary>
        /// num0式を評価し、MathJax形式で出力する
        /// </summary>
        member this.eq(text:complex0) =
            this.Context.writein ("\\("+text.Expr.evalL this.Context + "\\)")


        /// <summary>
        /// 指定位置・サイズでテキストブロックを生成
        /// </summary>
        /// <param name="s">適用するスタイル</param>
        /// <param name="p">表示位置</param>
        /// <param name="width, height">ブロックのサイズ</param>
        /// <param name="text">表示する文字列のリスト</param>
        member this.blockText (s:Style) (p:position) (width:float,height:float) (text:list<string>) =
            let padding = 5
            let s1 = Style [size.width (InvariantFormat.number width+"px")
                            size.height (InvariantFormat.number height+"px")
                            {Key = "margin-left"; Value = InvariantFormat.number p.x+"px";}
                            {Key = "margin-top"; Value = InvariantFormat.number p.y+"px";}
                            {Key = "position"; Value = "absolute";}
                            {Key = "overflow-wrap"; Value = "break-word";}]
            this.tagb ("div", [(s1+s).atr]) <| fun () ->
                text |> List.iter (fun s -> this.Context.writein (s+"<br>"))
                this.Context.writein ("\r\n")
            {Left = p.x;
            Right = p.x+double width+2.0*double padding;
            Top = p.y;
            Bottom = p.y+double height+2.0*double padding;}

[<AutoOpen>]
module HtmlGenerationExtensions2 =
    type HtmlGenerationContext with
        
        /// <summary>
        /// 指定位置に画像を表示する
        /// </summary>
        /// <param name="s">適用するスタイル</param>
        /// <param name="p">表示位置</param>
        /// <param name="filename">表示する画像のファイル名</param>
        member this.image (s:Style,p:position) = fun (filename:string) ->
            let sourceUrl = this.ImportAsset filename
            let st = Style [{Key="position"; Value="absolute"}; {Key="margin-left"; Value=InvariantFormat.number p.x+"px"}; {Key="margin-top"; Value=InvariantFormat.number p.y+"px"}] + s
            this.html.taga ("img", [st.atr; Atr("src", sourceUrl)])
        member this.image (s:Style, id:string) = fun (filename:string) ->
            let sourceUrl = this.ImportAsset filename
            this.html.taga ("img", [Atr("id",id); s.atr; Atr("src", sourceUrl)])
        member this.image (s:Style) = fun (filename:string) ->
            let sourceUrl = this.ImportAsset filename
            this.html.taga ("img", [s.atr; Atr("src", sourceUrl)])
        member this.image (filename:string) =
            let sourceUrl = this.ImportAsset filename
            this.html.taga ("img", [Atr("src", sourceUrl)])
        /// <summary>
        /// 指定位置に動画を表示する
        /// </summary>
        /// <param name="s">適用するスタイル</param>
        /// <param name="p">表示位置</param>
        /// <param name="filename">表示する動画のファイル名</param>
        member this.video (s:Style,p:position) = fun (filename:string) ->
            let sourceUrl = this.ImportAsset filename
            let st = Style [{Key="margin-left"; Value=InvariantFormat.number p.x+"px"}; {Key="margin-top"; Value=InvariantFormat.number p.y+"px"}] + s
            this.html.tagv ("video", [st.atr; Atr("src", sourceUrl); Atr("controls")])
            this.html.tage "video"
        member this.video (s:Style) = fun (filename:string) ->
            let sourceUrl = this.ImportAsset filename
            this.html.tagv ("video", [s.atr; Atr("src", sourceUrl); Atr("controls")])
            this.html.tage "video"
        /// <summary>
        /// キャラクター付き解説ページ
        /// </summary>
        member this.page (c:list<CharacterImage>) (audio:Audio,audioFile:option<string>,scriptColor:string) code2 =
            this.slide position.Origin <| fun p ->
                let animationCounter = this.AnimationCount
                // 音声ファイル追加
                this.AddAudioFile(
                    match audioFile with |Some t -> t |None -> "")
                // 字幕枠
                let subtitleBackgroundStyle =
                    "width: 1880px; height: 160px; " +
                    (if this.SubtitleEnabled then "display: block; " else "display: none; ") +
                    "position: absolute; z-index: 1; margin-top: 880px; padding: 20px; background-color: #aaaaff; font-family: 'Noto Sans JP'; font-size: 48px; font-weight: 800; text-shadow: 0 1px 0 #fff, 1px 0 0 #fff, 0 -1px 0 #fff, -1px -1px 0 #fff, 1px -1px 0 #fff, -1px 1px 0 #fff, 1px 1px 0 #fff"
                this.html.tagb ("div", [Atr("id", "sb" + animationCounter.ToString()); Atr("style", subtitleBackgroundStyle)]) <| fun () ->
                    ()
                // キャラクター画像
                this.html.tagb (
                    "div",
                    [Atr("id", "c" + animationCounter.ToString())
                     Atr("style", if this.CharacterEnabled then "display: block" else "display: none")]) <| fun () ->
                    for ci in c do
                        let sourceUrl = this.ImportAsset ci.CharacterImageFile
                        this.html.taga ("img", [Atr("src", sourceUrl); Atr("style", ci.CharacterImageStyle)])
                // 字幕
                let subtitleStyle =
                    "width: 1880px; height: 160px; " +
                    (if this.SubtitleEnabled then "display: block; " else "display: none; ") +
                    "position: absolute; z-index: 5; margin-top: 880px; padding: 20px; font-family: 'Noto Sans JP'; color: " + scriptColor + "; font-size: 48px; font-weight: 800; text-shadow: 0 1px 0 #fff, 1px 0 0 #fff, 0 -1px 0 #fff, -1px 0 0 #fff, -1px -1px 0 #fff, 1px -1px 0 #fff, -1px 1px 0 #fff, 1px 1px 0 #fff"
                this.html.tagb ("div", [Atr("id", "s" + animationCounter.ToString()); Atr("style", subtitleStyle)])
                    <| fun () -> this.BodyContext.writein audio.Subtitle
                this.switchAutoAnimation <| fun ctx ->
                    ctx.writein ("page"+animationCounter.ToString()+": () => {")
                // メインコンテンツ
                this.html.tagb ("div", [Atr("style", "width: 1920px; height: 880px; position: absolute; z-index: 0")]) <| fun () ->
                    code2 p
                this.switchAutoAnimation <| fun ctx ->
                    ctx.writein "},"
                match this.TryLastAnimationButton() with
                | Some(fStartName,fResetName,btnx,btny) ->
                    this.startButton2 ("startButton"+fStartName) (Style[position.position "absolute"; margin.left (btnx.ToString()+"px"); margin.top (btny.ToString()+"px"); position.index 1000;]) ("animationStartMap['"+fStartName+"']()")
                    this.resetButton2 ("resetButton"+fStartName) (Style[position.position "absolute"; margin.left (btnx.ToString()+"px"); margin.top ((btny+25).ToString()+"px"); position.index 1000;]) ("animationResetMap['"+fResetName+"']()")
                | None -> ()
                this.ClearAnimationButtons()
        /// <summary>
        /// 指定位置にスライドを生成
        /// </summary>
        /// <param name="p">スライドの表示位置</param>
        member this.slide (p:position)  code =
                let animationCounter = this.NextAnimationNumber()
                this.html.tagb (
                    "div",
                    [Atr("id", "p" + animationCounter.ToString())
                     Atr("style", "display: " + (if animationCounter=1 then "block" else "none") + "; position: absolute")]) <| fun wr ->
                    code p
        /// <summary>
        /// 前のページへ移動するボタンを生成
        /// </summary>
        member this.prevButton() =
                this.html.tagb ("button", [Atr("id", "prevButton"); Atr("style", "position: absolute; z-index: 100"); Atr("onclick", "drawPrev()")]) <| fun () ->
                    this.BodyContext.writein "前へ"
        /// <summary>
        /// 次のページへ移動するボタンを生成
        /// </summary>
        member this.nextButton() =
                this.html.tagb ("button", [Atr("id", "nextButton"); Atr("style", "position: absolute; margin-left: 75px; z-index: 100"); Atr("onclick", "drawNext()")]) <| fun () ->
                    this.BodyContext.writein "次へ"
        /// <summary>
        /// アニメーションを開始するボタンを生成
        /// </summary>
        member this.startButton2(id:string) (s:Style) (c:string) =
                this.html.tagb ("button", [Atr("id",id); Atr("onclick",c)]@[s.atr]) <| fun () ->
                    this.BodyContext.writein "Start"
        /// <summary>
        /// アニメーションをリセットするボタンを生成
        /// </summary>
        member this.resetButton2(id:string) (s:Style) (c:string) =
                this.html.tagb ("button", [Atr("id",id); Atr("onclick",c)]@[s.atr]) <| fun () ->
                    this.BodyContext.writein "Reset"
        /// <summary>
        /// キャラクター表示を制御するチェックボックスを生成
        /// </summary>
        member this.switchCharacter() =
            let checkedAttribute = if this.CharacterEnabled then [Atr("checked")] else []
            this.html.taga (
                "input",
                [Atr("type", "checkbox"); Atr("id", "switchCharacter"); Atr("style", "position: absolute; margin-top: 6px; margin-left: 150px; z-index: 100"); Atr("onclick", "setCharacter()")]
                @ checkedAttribute)
            this.html.tagb ("label", [Atr("style", "position: absolute; margin-top: 0px; margin-left: 165px; z-index: 100")]) <| fun () ->
                this.BodyContext.writein "キャラクター"
        /// <summary>
        /// 字幕表示を制御するチェックボックスを生成
        /// </summary>
        member this.switchSubtitle() =
            let checkedAttribute = if this.SubtitleEnabled then [Atr("checked")] else []
            this.html.taga (
                "input",
                [Atr("type", "checkbox"); Atr("id", "switchSubtitle"); Atr("style", "position: absolute; margin-top: 6px; margin-left: 270px; z-index: 100"); Atr("onclick", "setSubtitle()")]
                @ checkedAttribute)
            this.html.tagb ("label", [Atr("style", "position: absolute; margin-top: 0px; margin-left: 285px; z-index: 100")]) <| fun () ->
                this.BodyContext.writein "字幕"
        /// <summary>
        /// 音声再生を制御するチェックボックスを生成
        /// </summary>
        member this.switchAudio() =
            let checkedAttribute = if this.VoiceEnabled then [Atr("checked")] else []
            this.html.taga (
                "input",
                [Atr("type", "checkbox"); Atr("id", "switchAudio"); Atr("style", "position: absolute; margin-top: 6px; margin-left: 330px; z-index: 100"); Atr("onclick", "setSubtitle()")]
                @ checkedAttribute)
            this.html.tagb ("label", [Atr("style", "position: absolute; margin-top: 0px; margin-left: 345px; z-index: 100")]) <| fun () ->
                this.BodyContext.writein "音声"
        member this.audioPlayer() =
                this.html.tagb ("audio", [Atr("id", "audioPlayer")]) ignore
        /// <summary>
        /// 指定位置に画像を表示
        /// </summary>
        member this.imageA (s:Style) = fun (p:position) (filename:string) ->
            let s1 = Style [{Key = "margin-left"; Value = InvariantFormat.number p.x+"px";}
                            {Key = "margin-top"; Value = InvariantFormat.number p.y+"px";}
                            {Key = "position"; Value = "absolute";}]
            let sourceUrl = this.ImportAsset filename
            this.html.taga ("img", [(s1+s).atr; Atr("src", sourceUrl)])

/// <summary>
/// 図形アニメーションを管理するクラス
/// </summary>
/// <param name="figcounter">図形の識別番号</param>
/// <param name="originX, originY">描画の基準座標</param>
/// <param name="canvasX, canvasY">キャンパスのサイズ</param>
[<AutoOpen>]
module CompilationEnvironmentAnimationExtensions =
    type HtmlGenerationContext with
        member this.slideAnimation = ContextSlideAnimation(this)

type FigureAnimation(context:HtmlGenerationContext,figcounter:int,originX:int,originY:int,canvasX:int,canvasY:int) =
    let padding = 10.0
    /// アニメーションの実行順序リスト
    let mutable animeFlow:list<string*string*AnimationSetting*bool> = []
    let mutable counter = 0
    member _.Padding with get() = padding
    member _.id with get() = "fa"+figcounter.ToString()+"_"+counter.ToString()
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
    member this.animationLine s = AnimationLine(context,s,canvasX,canvasY)
    member this.animationArc s = AnimationArc(context,s,canvasX,canvasY)
    member this.animationText s = AnimationText(context,s,originX,originY,canvasX,canvasY)
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
            context.BodyContext.writein str
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
                    ctx.writein ("    repeat(" + idstart + ", " + setting.FrameTime.ToString() + ", " + setting.FrameNumber.ToString() + ");")
                else
                    ctx.writein ("    repeatSeq(" + idstart + ", " + setting.FrameTime.ToString() + ", " + setting.FrameNumber.ToString() + ", () => {")
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

[<AutoOpen>]
module dochtml =
    let private htmlpresentationCore
        (dir:string)
        (filename:string)
        (title:string)
        (cssfile:option<string>)
        (pagesizeX:option<int>,pagesizeY:option<int>)
        isPageAnimation
        code =
        // ディレクトリ作成
        // コンテンツディレクトリ
        use context = new HtmlGenerationContext(dir, filename)
        context.switchJSAnimationStart <| fun ctx ->
            ctx.writein "const animationStartMap = {"
        context.switchJSAnimationReset <| fun ctx ->
            ctx.writein "const animationResetMap = {"
        context.switchAutoAnimation <| fun ctx ->
            ctx.writein "const autoAnimationMap = {"
        context.switchAnimationSeq <| fun ctx ->
            ctx.writein "function repeatSeq(fn, interval, Nt, onComplete)"
            ctx.writein "{"
            ctx.writein "    let t = 0;"
            ctx.writein "    function run()"
            ctx.writein "    {"
            ctx.writein "        if (t < Nt)"
            ctx.writein "        {"
            ctx.writein "            fn(t);"
            ctx.writein "            t++;"
            ctx.writein "            setTimeout(run, interval);"
            ctx.writein "        }"
            ctx.writein "        else"
            ctx.writein "        {"
            ctx.writein "            onComplete();"
            ctx.writein "        }"
            ctx.writein "    }"
            ctx.writein "    run();"
            ctx.writein "}"
            ctx.writein "function repeat(fn, interval, Nt)"
            ctx.writein "{"
            ctx.writein "    let t = 0;"
            ctx.writein "    function run()"
            ctx.writein "    {"
            ctx.writein "        if(t == Nt)"
            ctx.writein "        {"
            ctx.writein "            t = 0;"
            ctx.writein "        }"
            ctx.writein "        fn(t);"
            ctx.writein "        t++;"
            ctx.writein "        setTimeout(run, interval);"
            ctx.writein "    }"
            ctx.writein "    run();"
            ctx.writein "}"
        code context
        if isPageAnimation then
            context.slideAnimation.writeAudioList()
            context.slideAnimation.jsSetCharacter()
            context.slideAnimation.jsSetSubtitle()
            context.slideAnimation.jsDrawNext(context.ContentsUrlPrefix)
            context.slideAnimation.jsDrawPrev(context.ContentsUrlPrefix)
        // head、body要素書き込みストリームを閉じてhead、body要素のコード取得
        let codeDraw = context.switchJSMain <| fun ctx ->
            ctx.allCodes
        let codeBody = context.switchBody <| fun ctx ->
            ctx.allCodes
        // html書き込みストリーム作成
        context.switchMain <| fun ctx ->
            ctx.writein "<!DOCTYPE html>"
            // html要素
            ctx.html.tagb ("html", [Atr("lang", "ja")]) <| fun () ->
                // head要素
                ctx.html.tagb "head" <| fun () ->
                    // titleタグ
                    ctx.writein ("<title>"+title+"</title>")
                    // metaタグ
                    ctx.writein "<meta charset=\"UTF-8\">"
                    //追加（5/29）viewportタブ
                    match pagesizeX with
                    |None ->
                        ctx.writein "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0, maximum-scale=1.0, minimum-scale=1.0\">"
                    |Some width ->
                        ctx.writein ("<meta name=\"viewport\" content=\"width=" + width.ToString() + "\">")
                    // titleタグ
                    ctx.html.tagb "title" <| fun () ->
                        ctx.writein filename
                    // MathJax
                    ctx.html.tagb (
                        "script",
                        [Atr("type", "text/javascript")
                         Atr("id", "MathJax-script")
                         Atr("async")
                         Atr("src", "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml.js")]) ignore
                    for asset in
                        ["animationSeq.js"; "animationSeqReset.js"; "animationStart.js"; "animationReset.js"; "autoAnimation.js"] do
                        ctx.html.tagb (
                            "script",
                            [Atr("type", "text/javascript"); Atr("src", context.AssetUrl(asset))]) ignore
                    // scriptタグ
                    ctx.html.tagb "script" <| fun () ->
                        match codeDraw with |Some s -> ctx.writein s |None -> ()
                    // webフォント取得
                    ctx.writein "<link rel=\"preconnect\" href=\"https://fonts.googleapis.com\">"
                    ctx.writein "<link rel=\"preconnect\" href=\"https://fonts.gstatic.com\" crossorigin>"
                    ctx.writein "<link href=\"https://fonts.googleapis.com/css2?family=Noto+Sans+JP:wght@100..900&display=swap\" rel=\"stylesheet\">"
                    match cssfile with
                    | Some stylesheet -> ctx.html.taga ("link", [Atr("rel", "stylesheet"); Atr("href", stylesheet)])
                    | None -> ()
                // body要素
                match pagesizeX,pagesizeY with
                |None,None ->
                    let s0 = Style [area.backGroundColor "#ffffff"]
                    ctx.html.tagb ("body", [s0.atr]) <| fun () ->
                        match codeBody with |Some s -> ctx.writein s |None -> ()
                |Some x,None ->
                    let s0 = Style [area.backGroundColor "#aaaaaa"]
                    ctx.html.tagb ("body", [s0.atr]) <| fun () ->
                        let s1 = Style [
                            area.backGroundColor "#ffffff"
                            margin.left "auto"
                            margin.right "auto"
                            size.width (x.ToString()+"px")]
                        ctx.html.tagb ("div", [s1.atr]) <| fun () ->
                            match codeBody with |Some s -> ctx.writein s |None -> ()
                |None,Some y->
                    let s0 = Style [area.backGroundColor "#aaaaaa"]
                    ctx.html.tagb ("body", [s0.atr]) <| fun () ->
                        let s1 = Style [
                            area.backGroundColor "#ffffff"
                            margin.left "auto"
                            margin.right "auto"
                            size.height (y.ToString()+"px")]
                        ctx.html.tagb ("div", [s1.atr]) <| fun () ->
                            match codeBody with |Some s -> ctx.writein s |None -> ()
                |Some x,Some y ->
                    let s0 = Style [area.backGroundColor "#aaaaaa"]
                    ctx.html.tagb ("body", [s0.atr]) <| fun () ->
                        let s1 = Style [
                            area.backGroundColor "#ffffff"
                            margin.left "auto"
                            margin.right "auto"
                            size.width (x.ToString()+"px")
                            size.height (y.ToString()+"px")]
                        ctx.html.tagb ("div", [s1.atr]) <| fun () ->
                            match codeBody with |Some s -> ctx.writein s |None -> ()

                context.switchJSAnimationStart <| fun ctx ->
                    ctx.writein "test: () => {}"
                    ctx.writein "};"
                context.switchJSAnimationReset <| fun ctx ->
                    ctx.writein "test: () => {}"
                    ctx.writein "};"
                    ctx.writein ""
                    ctx.writein "function resetAll(){"
                    ctx.writein "    for (const key in animationResetMap) {"
                    ctx.writein "        if (typeof animationResetMap[key] === \"function\") {"
                    ctx.writein "            animationResetMap[key]();"
                    ctx.writein "        }"
                    ctx.writein "    }"
                    ctx.writein "}"
                context.switchAutoAnimation <| fun ctx ->
                    ctx.writein "test: () => {}"
                    ctx.writein "};"
                // bodyタグ一時コード削除
                context.switchBody <| fun c -> c.delete()
                // JavaScript関数一時コード削除
                context.switchJSMain <| fun c -> c.delete()

    /// 全体がキャンバスの無制限レイアウト
    let htmlpresentation
        (dir:string)
        (filename:string)
        (title:string)
        (cssfile:option<string>)
        pagesize
        isPageAnimation
        code =
        htmlpresentationCore
            dir
            filename
            title
            cssfile
            pagesize
            isPageAnimation
            code

    let freeCanvas outputdir filename (title:string) cssfile code =
        htmlpresentation outputdir filename title cssfile (None, None) false <| fun ctx ->
            ctx.html.canvas <| Style [size.width "0px"; size.height "0px"] <| fun () -> code ctx

    /// 全体がキャンバスの無制限レイアウト
    let freePage outputdir filename (title:string) cssfile code =
        htmlpresentation outputdir filename title cssfile (None, None) false code

    /// 固定幅レイアウト
    let fixedWidthPage outputdir filename (title:string) pageWidth cssfile code =
        htmlpresentation outputdir filename title cssfile (Some pageWidth, None) false code

    let fixedPage outputdir filename (title:string) pageWidth pageHeight cssfile code =
        htmlpresentationCore outputdir filename title cssfile (Some pageWidth, Some pageHeight) true <| fun ctx ->
            code ctx
            ctx.prevButton()
            ctx.nextButton()
            ctx.switchCharacter()
            ctx.switchSubtitle()
            ctx.switchAudio()
            ctx.audioPlayer()

[<AutoOpen>]
module htmlexpr2 =
    type HtmlGenerationContext with
        
        member this.html = html this.BodyContext
        
        /// <summary>
        /// 手動操作型のアニメーション領域を生成
        /// </summary>
        /// <param name="s">アニメーションの領域設定</param>
        /// <param name="p">表示位置</param>
        /// <param name="buttonX, buttonY">操作ボタンの配置座標</param>
        member this.animationManual (s:ViewBoxStyle) (p:position) (buttonX:int,buttonY:int) code =
            let f =
                FigureAnimation(
                    this, this.NextFigureNumber(),
                    s.mX,s.mY,s.sX,s.sY)
            this.switchBody <| fun ctx ->
                ctx.writein  ("<svg viewBox=\"0 0 "+s.sX.ToString()+" "+s.sY.ToString()+"\" ")
                ctx.writein  ("width=\""+CssLength.pixelsInt s.sX+"\" ")
                ctx.writein  ("height=\""+CssLength.pixelsInt s.sY+"\" ")
                ctx.writein  "xmlns=\"http://www.w3.org/2000/svg\" "
                ctx.writein  ("style=\"margin-left: "+CssLength.pixelsInt s.mX+"; ")
                ctx.writein  ("margin-top: "+CssLength.pixelsInt s.mY+"; ")
                ctx.writein  "position: absolute;"
                ctx.writein  ("background-color: "+s.backgroundColor+";")
                ctx.writein  "\">"
                code(f,p)
                ctx.writein  "</svg>"
            let asc = this.nextAnimationGroup()
            let fnameStart = f.jsStartControll asc
            let fnameReset = f.jsResetControll asc
            this.addAnimationButton(fnameStart,fnameReset,buttonX,buttonY)

        /// <summary>
        /// 自動再生型のアニメーション領域を生成する
        /// </summary>
        member this.animationAuto (s:ViewBoxStyle) (p:position) code =
            let f =
                FigureAnimation(
                    this, this.NextFigureNumber(),
                    s.mX,s.mY,s.sX,s.sY)
            this.switchBody <| fun ctx ->
                ctx.writein  ("<svg viewBox=\"0 0 "+s.sX.ToString()+" "+s.sY.ToString()+"\" ")
                ctx.writein  ("width=\""+CssLength.pixelsInt s.sX+"\" ")
                ctx.writein  ("height=\""+CssLength.pixelsInt s.sY+"\" ")
                ctx.writein  "xmlns=\"http://www.w3.org/2000/svg\" "
                ctx.writein  ("style=\"margin-left: "+CssLength.pixelsInt s.mX+"; ")
                ctx.writein  ("margin-top: "+CssLength.pixelsInt s.mY+"; ")
                ctx.writein  "position: absolute;"
                ctx.writein  ("background-color: "+s.backgroundColor+";")
                ctx.writein  "\">"
                code(f,p)
                ctx.writein  "</svg>"
            let asc = this.nextAnimationGroup()
            let fnameStart = f.jsStartControll asc
            let fnameReset = f.jsResetControll asc
            this.addAutoAnimation(fnameStart,fnameReset)
