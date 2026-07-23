//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

open System
open System.IO

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

/// <summary>
/// 線分アニメーションを生成するクラス
/// </summary>
/// <param name="s">線の太さ、色を定義するスタイル情報</param>
/// <param name="canvasX">描画領域の横幅</param>
/// <param name="canvasY">描画領域の縦幅</param>
type AnimationLine(environment:CompilationEnvironment,s:Style,canvasX:int,canvasY:int) =
    let id = environment.htmlio.nextContentsID()
    let s0 = Style ([{Key="visibility";Value="hidden"}]@s.list)
    let s1 = Style ([{Key="visibility";Value="visible"}]@s.list)
    do
        environment.html.taga ("line", [Atr("id",id);]@[s0.atr])
    /// <summary>
    /// 割り当てられたidを取得する
    /// </summary>
    member this.ID with get() = id
    /// <summary>
    /// 指定したLineオブジェクトをキャンパスに追加する
    /// </summary>
    /// <param name="f">描画対象となる線分</param>
    member this.P (f:Line) =
        let t = double0(Var(Dt,"t",NaN), context=environment.RequireGenerationContext())
        environment.htmlio.switchAnimationSeq <| fun environment ->
            writein (environment.RequireGenerationContext()) ("    var e = document.getElementById(\""+id+"\");")
            writein (environment.RequireGenerationContext()) ("    var x1 = " + (f.Start.X t).code + ";")
            writein (environment.RequireGenerationContext()) ("    var y1 = " + (canvasY - f.Start.Y t).code + ";")
            writein (environment.RequireGenerationContext()) ("    var x2 = " + (f.End.X t).code + ";")
            writein (environment.RequireGenerationContext()) ("    var y2 = " + (canvasY - f.End.Y t).code + ";")
            writein (environment.RequireGenerationContext()) ("    e.setAttribute(\"style\"," + "\"" + s1.code0 + "\");")
            writein (environment.RequireGenerationContext()) "    e.setAttribute(\"x1\", x1);"
            writein (environment.RequireGenerationContext()) "    e.setAttribute(\"y1\", y1);"
            writein (environment.RequireGenerationContext()) "    e.setAttribute(\"x2\", x2);"
            writein (environment.RequireGenerationContext()) "    e.setAttribute(\"y2\", y2);"
        environment.htmlio.switchJSAnimationSeqReset <| fun environment ->
            writein (environment.RequireGenerationContext()) ("    var e = document.getElementById(\""+id+"\");")
            writein (environment.RequireGenerationContext()) ("    e.setAttribute(\"style\"," + "\"" + s0.code0 + "\");")

/// <summary>
/// 円アニメーションを生成するクラス
/// </summary>
/// <param name="s">線の太さ、色を定義するスタイル情報</param>
/// <param name="canvasX">描画領域の横幅</param>
/// <param name="canvasY">描画領域の縦幅</param>
type AnimationEllipse(environment:CompilationEnvironment,s:Style,canvasX:int,canvasY:int) =
    let id = environment.htmlio.nextContentsID()
    let s0 = Style ([{Key="visibility";Value="hidden"}]@s.list)
    let s1 = Style ([{Key="visibility";Value="visible"}]@s.list)
    do
        environment.html.taga ("ellipse", [Atr("id",id);]@[s0.atr])
    /// <summary>
    /// 割り当てられたidを取得する
    /// </summary>
    member this.ID with get() = id
    /// <summary>
    /// 指定したEllipseオブジェクトをキャンパスに追加する
    /// </summary>
    /// <param name="e">描画対象となる円</param>
    member this.P (e:Ellipse) =
        let t = double0(Var(Dt,"t",NaN), context=environment.RequireGenerationContext())
        environment.htmlio.switchAnimationSeq <| fun environment ->
            writein (environment.RequireGenerationContext()) ("    var e = document.getElementById(\""+id+"\");")
            writein (environment.RequireGenerationContext()) ("    var cx = " + (e.center.X t).code + ";")
            writein (environment.RequireGenerationContext()) ("    var cy = " + (canvasY - e.center.Y t).code + ";")
            writein (environment.RequireGenerationContext()) ("    var rx = " + (e.radiusX t).code + ";")
            writein (environment.RequireGenerationContext()) ("    var ry = " + (e.radiusY t).code + ";")
            writein (environment.RequireGenerationContext()) ("    e.setAttribute(\"style\"," + "\"" + s1.code0 + "\");")
            writein (environment.RequireGenerationContext()) "    e.setAttribute(\"cx\", cx);"
            writein (environment.RequireGenerationContext()) "    e.setAttribute(\"cy\", cy);"
            writein (environment.RequireGenerationContext()) "    e.setAttribute(\"rx\", rx);"
            writein (environment.RequireGenerationContext()) "    e.setAttribute(\"ry\", ry);"
        environment.htmlio.switchJSAnimationSeqReset <| fun environment ->
            writein (environment.RequireGenerationContext()) ("    var e = document.getElementById(\""+id+"\");")
            writein (environment.RequireGenerationContext()) ("    e.setAttribute(\"style\"," + "\"" + s0.code0 + "\");")

/// <summary>
/// 円弧アニメーションを生成するクラス
/// </summary>
/// <param name="s">線の太さ、色を定義するスタイル情報</param>
/// <param name="canvasX">描画領域の横幅</param>
/// <param name="canvasY">描画領域の縦幅</param>
type AnimationArc(environment:CompilationEnvironment,s:Style,canvasX:int,canvasY:int) =
    let id = environment.htmlio.nextContentsID()
    let s0 = Style ([{Key="visibility";Value="hidden"}]@s.list)
    let s1 = Style ([{Key="visibility";Value="visible"}]@s.list)
    do
        environment.html.taga ("path", [Atr("id",id);]@[s0.atr])
    /// <summary>
    /// 割り当てられたidを取得する
    /// </summary>
    member this.ID with get() = id
    /// <summary>
    /// 指定したArcオブジェクトをキャンパスに追加する
    /// </summary>
    /// <param name="e">描画対象となる円弧</param>
    member this.P (e:Arc) =
        let t = double0(Var(Dt,"t",NaN), context=environment.RequireGenerationContext())
        environment.htmlio.switchAnimationSeq <| fun environment ->
            writein (environment.RequireGenerationContext()) ("    var e = document.getElementById(\""+id+"\");")
            let a1 = Math.PI * e.angle1 t / 180
            let x1 = e.center.X t + e.radius t * asm.cos a1
            let y1 = e.center.Y t + e.radius t * asm.sin a1
            writein (environment.RequireGenerationContext()) ("    var x1 = " + x1.code+";")
            writein (environment.RequireGenerationContext()) ("    var y1 = " + (canvasY - y1).code+";")
            let a2 = Math.PI * e.angle2 t / 180 - 1E-4
            let x2 = e.center.X t + e.radius t * asm.cos a2
            let y2 = e.center.Y t + e.radius t * asm.sin a2
            writein (environment.RequireGenerationContext()) ("    var x2 = " + x2.code+";")
            writein (environment.RequireGenerationContext()) ("    var y2 = " + (canvasY - y2).code+";")
            writein (environment.RequireGenerationContext()) ("    var a1 = " + (e.angle1 t).code+";")
            writein (environment.RequireGenerationContext()) ("    var a2 = " + (e.angle2 t).code+";")
            writein (environment.RequireGenerationContext()) ("    var radiusX = " + (e.radius t).code + ";")
            writein (environment.RequireGenerationContext()) ("    var radiusY = " + (e.radius t).code + ";")
            writein (environment.RequireGenerationContext()) "    var da = a2 - a1;"
            writein (environment.RequireGenerationContext()) "    if(da < 0.0) {da = a2 + 360 - a1;}"
            writein (environment.RequireGenerationContext()) "    var largerOrSmaller = 0;"
            writein (environment.RequireGenerationContext()) "    if(da > 180.0) {largerOrSmaller = 1;}"
            writein (environment.RequireGenerationContext()) ("    d = \"M \" + x1 + \" \" + y1 + \" A \" + radiusX + \" \" + radiusY + \" 0 \" + largerOrSmaller + \" 0 \" + x2 + \" \" + y2 " + ";")
            writein (environment.RequireGenerationContext()) ("    e.setAttribute(\"style\"," + "\"" + s1.code0 + "\");")
            writein (environment.RequireGenerationContext()) ("    e.setAttribute(\"d\", " + "d" + ");")
        environment.htmlio.switchJSAnimationSeqReset <| fun environment ->
            writein (environment.RequireGenerationContext()) ("    var e = document.getElementById(\""+id+"\");")
            writein (environment.RequireGenerationContext()) ("    e.setAttribute(\"style\"," + "\"" + s0.code0 + "\");")

/// <summary>
/// テキスト・数式アニメーションを生成するクラス
/// </summary>
/// <param name="s">線の太さ、色を定義するスタイル情報</param>
/// <param name="canvasX">描画領域の横幅</param>
/// <param name="canvasY">描画領域の縦幅</param>
type AnimationText(environment:CompilationEnvironment,s:Style,originX:int,originY:int,canvasX:int,canvasY:int) =
    let id = environment.htmlio.nextContentsID()
    let ss = Style ([{Key="position";Value="absolute"}]@s.list)
    let ss0 = Style ([{Key="display";Value="none"}]@ss.list)
    let ss1 = Style ([{Key="display";Value="block"}]@ss.list)
    do
        environment.html.tagb ("div", [Atr("id",id); ss0.atr]) <| fun () -> ()
    /// <summary>
    /// 割り当てられたidを取得
    /// </summary>
    member this.ID with get() = id
    /// <summary>
    /// 指定したTextオブジェクトをキャンパスに追加する
    /// </summary>
    /// <param name="e">対象となるテキスト</param>
    member this.P (e:Text) =
        let t = double0(Var(Dt,"t",NaN), context=environment.RequireGenerationContext())
        environment.htmlio.switchAnimationSeq <| fun environment ->
            writein (environment.RequireGenerationContext()) ("    var e = document.getElementById(\""+id+"\");")
            writein (environment.RequireGenerationContext()) ("    e.setAttribute(\"style\"," + "\"" + ss1.code0 + "\");")
            writein (environment.RequireGenerationContext()) ("    e.innerHTML = \"" + e.str + "\";")
            writein (environment.RequireGenerationContext()) ("    var x = " + (originX + e.center.X t).code+ ";")
            writein (environment.RequireGenerationContext()) ("    var y = " + (originY + canvasY - e.center.Y t).code+ ";")
            writein (environment.RequireGenerationContext()) "    x = x - e.offsetWidth/2;"
            writein (environment.RequireGenerationContext()) "    y = y - e.offsetHeight/2;"
            writein (environment.RequireGenerationContext()) ("    e.setAttribute(\"style\"," + "\"" + ss1.code0 + " margin-left: \"+String(x)+\"px; margin-top: \"+String(y)+\"px; \");")
        environment.htmlio.switchJSAnimationSeqReset <| fun environment ->
            writein (environment.RequireGenerationContext()) ("    var e = document.getElementById(\""+id+"\");")
            writein (environment.RequireGenerationContext()) ("    e.setAttribute(\"style\"," + "\"" + ss0.code0 + "\");")
    /// <summary>
    /// 指定したMathTextオブジェクトをキャンパスに追加する
    /// </summary>
    /// <param name="e">対象となる数式</param>
    member this.P (e:MathText<'a>) =
        let t = double0(Var(Dt,"t",NaN), context=environment.RequireGenerationContext())
        environment.htmlio.switchAnimationSeq <| fun environment ->
            writein (environment.RequireGenerationContext()) ("    var e = document.getElementById(\""+id+"\");")
            writein (environment.RequireGenerationContext()) ("    e.setAttribute(\"style\"," + "\"" + ss1.code0 + "\");")
            writein (environment.RequireGenerationContext()) ("    e.innerHTML = \"\\\\(" + (e.eq :> INum0).Code + "\\\\)\";")
            writein (environment.RequireGenerationContext()) "    MathJax.typeset();"
            writein (environment.RequireGenerationContext()) ("    var x =" + (originX + e.center.X t).code+ ";")
            writein (environment.RequireGenerationContext()) ("    var y =" + (originY + canvasY - e.center.Y t).code+ ";")
            writein (environment.RequireGenerationContext()) "    x = x - e.offsetWidth/2;"
            writein (environment.RequireGenerationContext()) "    y = y - e.offsetHeight/2;"
            writein (environment.RequireGenerationContext()) ("    e.setAttribute(\"style\"," + "\"" + ss1.code0 + " margin-left: \"+String(x)+\"px; margin-top: \"+String(y)+\"px; \");")
        environment.htmlio.switchJSAnimationSeqReset <| fun environment ->
            writein (environment.RequireGenerationContext()) ("    var e = document.getElementById(\""+id+"\");")
            writein (environment.RequireGenerationContext()) ("    e.setAttribute(\"style\"," + "\"" + ss0.code0 + "\");")

/// <summary>
/// 多角形アニメーションを生成するクラス
/// </summary>
/// <param name="s">線の太さ、色を定義するスタイル情報</param>
/// <param name="canvasX">描画領域の横幅</param>
/// <param name="canvasY">描画領域の縦幅</param>
type AnimationPolygon(environment:CompilationEnvironment,s:Style,canvasX:int,canvasY:int) =
    let id = environment.htmlio.nextContentsID()
    let s0 = Style ([{Key="visibility";Value="hidden"}]@s.list)
    let s1 = Style ([{Key="visibility";Value="visible"}]@s.list)
    do
        environment.html.taga ("polygon", [Atr("id", id);] @ [s.atr])
    /// <summary>
    /// 割り当てられたidを取得する
    /// </summary>
    member this.ID with get() = id
    /// <summary>
    /// 指定した頂点座標のリストを多角形としてキャンパスに追加する
    /// </summary>
    /// <param name="apex">多角形を構成する頂点座標のリスト</param>
    member this.P (apex:list<tposition>) =
        let t = double0(Var(Dt,"t",NaN), context=environment.RequireGenerationContext())
        environment.htmlio.switchAnimationSeq <| fun environment ->
            writein (environment.RequireGenerationContext()) ("    var e = document.getElementById(\"" + id + "\");")
            writein (environment.RequireGenerationContext()) "    var p = \"\";"
            for p in apex do
                writein (environment.RequireGenerationContext()) ("    var x = " + (p.X t).code + ";")
                writein (environment.RequireGenerationContext()) ("    var y = " + (canvasY - p.Y t).code + ";")
                writein (environment.RequireGenerationContext()) "    p = p + String(x) + \",\" + String(y) + \" \";"
            writein (environment.RequireGenerationContext()) ("    e.setAttribute(\"style\"," + "\"" + s1.code0 + "\");")
            writein (environment.RequireGenerationContext()) "    e.setAttribute(\"points\", p);"
        environment.htmlio.switchJSAnimationSeqReset <| fun environment ->
            writein (environment.RequireGenerationContext()) ("    var e = document.getElementById(\""+id+"\");")
            writein (environment.RequireGenerationContext()) ("    e.setAttribute(\"style\"," + "\"" + s0.code0 + "\");")

/// <summary>
/// スライドアニメーション全体を管轄するクラス
/// </summary>
type ContextSlideAnimation internal (environment:CompilationEnvironment) =
    let context = environment.RequireGenerationContext()
    /// <summary>
    /// 登録された音声ファイルの一覧を書きだす
    /// </summary>
    member this.writeAudioList() =
        environment.htmlio.switchJSMain <| fun environment ->
            let audioFiles = context.AudioFiles
            writein (environment.RequireGenerationContext()) "const audioList = ["
            for i in 0..audioFiles.Length-1 do
                writein (environment.RequireGenerationContext()) ("    \""+audioFiles[i] + "\"" + if i<audioFiles.Length-1 then "," else "")
            writein (environment.RequireGenerationContext()) "];"
    /// <summary>
    /// キャラクター表示を制御するJavaScriptコードの生成
    /// </summary>
    member this.jsSetCharacter() =
        environment.htmlio.switchJSMain <| fun environment ->
            writein (environment.RequireGenerationContext()) "let pagecount = 1;"
            writein (environment.RequireGenerationContext()) "function setCharacter()"
            writein (environment.RequireGenerationContext()) "{"
            writein (environment.RequireGenerationContext()) "        const swc = document.getElementById(\"switchCharacter\");"
            writein (environment.RequireGenerationContext()) "        const c = document.getElementById(\"c\"+pagecount);"
            writein (environment.RequireGenerationContext()) "        if(swc.checked)"
            writein (environment.RequireGenerationContext()) "        {"
            writein (environment.RequireGenerationContext()) "            c.style.display = \"block\";"
            writein (environment.RequireGenerationContext()) "        }"
            writein (environment.RequireGenerationContext()) "        else"
            writein (environment.RequireGenerationContext()) "        {"
            writein (environment.RequireGenerationContext()) "            c.style.display = \"none\";"
            writein (environment.RequireGenerationContext()) "        }"
            writein (environment.RequireGenerationContext()) "}"
    /// <summary>
    /// 字幕表示を制御するJavaScriptコードの生成
    /// </summary>
    member this.jsSetSubtitle() =
        environment.htmlio.switchJSMain <| fun environment ->
            writein (environment.RequireGenerationContext()) "function setSubtitle()"
            writein (environment.RequireGenerationContext()) "{"
            writein (environment.RequireGenerationContext()) "        const sws = document.getElementById(\"switchSubtitle\");"
            writein (environment.RequireGenerationContext()) "        const b2 = document.getElementById(\"sb\"+pagecount);"
            writein (environment.RequireGenerationContext()) "        const s2 = document.getElementById(\"s\"+pagecount);"
            writein (environment.RequireGenerationContext()) "        if(sws.checked)"
            writein (environment.RequireGenerationContext()) "        {"
            writein (environment.RequireGenerationContext()) "            b2.style.display = \"block\";"
            writein (environment.RequireGenerationContext()) "            s2.style.display = \"block\";"
            writein (environment.RequireGenerationContext()) "        }"
            writein (environment.RequireGenerationContext()) "        else"
            writein (environment.RequireGenerationContext()) "        {"
            writein (environment.RequireGenerationContext()) "            b2.style.display = \"none\";"
            writein (environment.RequireGenerationContext()) "            s2.style.display = \"none\";"
            writein (environment.RequireGenerationContext()) "        }"
            writein (environment.RequireGenerationContext()) "}"
    /// <summary>
    /// 次のページへの遷移を制御するJavaScriptコードの生成
    /// </summary>
    member this.jsDrawNext(audioDir:string) =
        environment.htmlio.switchJSMain <| fun environment ->
            let animationCount = context.AnimationCount
            writein (environment.RequireGenerationContext()) "function drawNext()"
            writein (environment.RequireGenerationContext()) "{"
            writein (environment.RequireGenerationContext()) "    resetAll();"
            writein (environment.RequireGenerationContext()) ("    if(pagecount<"+animationCount.ToString()+")")
            writein (environment.RequireGenerationContext()) "    {"
            writein (environment.RequireGenerationContext()) "        const swc = document.getElementById(\"switchCharacter\");"
            writein (environment.RequireGenerationContext()) "        const sws = document.getElementById(\"switchSubtitle\");"
            writein (environment.RequireGenerationContext()) "        const swa = document.getElementById(\"switchAudio\");"
            writein (environment.RequireGenerationContext()) "        "
            writein (environment.RequireGenerationContext()) "        const p1 = document.getElementById(\"p\"+pagecount);"
            writein (environment.RequireGenerationContext()) "        p1.style.display = \"none\";"
            writein (environment.RequireGenerationContext()) "        const b1 = document.getElementById(\"sb\"+pagecount);"
            writein (environment.RequireGenerationContext()) "        b1.style.display = \"none\";"
            writein (environment.RequireGenerationContext()) "        const s1 = document.getElementById(\"s\"+pagecount);"
            writein (environment.RequireGenerationContext()) "        s1.style.display = \"none\";"
            writein (environment.RequireGenerationContext()) "        const c1 = document.getElementById(\"c\"+pagecount);"
            writein (environment.RequireGenerationContext()) "        c1.style.display = \"none\";"
            writein (environment.RequireGenerationContext()) "        pagecount++;"
            writein (environment.RequireGenerationContext()) "        const p2 = document.getElementById(\"p\"+pagecount);"
            writein (environment.RequireGenerationContext()) "        p2.style.display = \"block\";"
            writein (environment.RequireGenerationContext()) "        if(sws.checked)"
            writein (environment.RequireGenerationContext()) "        {"
            writein (environment.RequireGenerationContext()) "            const b2 = document.getElementById(\"sb\"+pagecount);"
            writein (environment.RequireGenerationContext()) "            b2.style.display = \"block\";"
            writein (environment.RequireGenerationContext()) "            const s2 = document.getElementById(\"s\"+pagecount);"
            writein (environment.RequireGenerationContext()) "            s2.style.display = \"block\";"
            writein (environment.RequireGenerationContext()) "        }"
            writein (environment.RequireGenerationContext()) "        else"
            writein (environment.RequireGenerationContext()) "        {"
            writein (environment.RequireGenerationContext()) "            const b2 = document.getElementById(\"sb\"+pagecount);"
            writein (environment.RequireGenerationContext()) "            b2.style.display = \"none\";"
            writein (environment.RequireGenerationContext()) "            const s2 = document.getElementById(\"s\"+pagecount);"
            writein (environment.RequireGenerationContext()) "            s2.style.display = \"none\";"
            writein (environment.RequireGenerationContext()) "        }"
            writein (environment.RequireGenerationContext()) "        if(swc.checked)"
            writein (environment.RequireGenerationContext()) "        {"
            writein (environment.RequireGenerationContext()) "            const c2 = document.getElementById(\"c\"+pagecount);"
            writein (environment.RequireGenerationContext()) "            c2.style.display = \"block\";"
            writein (environment.RequireGenerationContext()) "        }"
            writein (environment.RequireGenerationContext()) "        else"
            writein (environment.RequireGenerationContext()) "        {"
            writein (environment.RequireGenerationContext()) "            const c2 = document.getElementById(\"c\"+pagecount);"
            writein (environment.RequireGenerationContext()) "            c2.style.display = \"none\";"
            writein (environment.RequireGenerationContext()) "        }"
            writein (environment.RequireGenerationContext()) "        const audioPlayer = document.getElementById(\"audioPlayer\");"
            writein (environment.RequireGenerationContext()) "        if(audioList[pagecount-1] != \"\" && swa.checked)"
            writein (environment.RequireGenerationContext()) "        {"
            writein (environment.RequireGenerationContext()) ("            audioPlayer.src = \""+audioDir+"/\" + audioList[pagecount-1];")
            writein (environment.RequireGenerationContext()) "            audioPlayer.play();"
            writein (environment.RequireGenerationContext()) "        }"
            writein (environment.RequireGenerationContext()) "        autoAnimationMap['page'+pagecount]();"
            writein (environment.RequireGenerationContext()) "    }"
            writein (environment.RequireGenerationContext()) "}"
    /// <summary>
    /// 前のページへの遷移を制御するJavaScriptコードの生成
    /// </summary>
    member this.jsDrawPrev(audioDir:string) =
        environment.htmlio.switchJSMain <| fun environment ->
            writein (environment.RequireGenerationContext()) "function drawPrev()"
            writein (environment.RequireGenerationContext()) "{"
            writein (environment.RequireGenerationContext()) "    resetAll();"
            writein (environment.RequireGenerationContext()) "    if(pagecount>1)"
            writein (environment.RequireGenerationContext()) "    {"
            writein (environment.RequireGenerationContext()) "        const swc = document.getElementById(\"switchCharacter\");"
            writein (environment.RequireGenerationContext()) "        const sws = document.getElementById(\"switchSubtitle\");"
            writein (environment.RequireGenerationContext()) "        const swa = document.getElementById(\"switchAudio\");"
            writein (environment.RequireGenerationContext()) "        const p1 = document.getElementById(\"p\"+pagecount);"
            writein (environment.RequireGenerationContext()) "        p1.style.display = \"none\";"
            writein (environment.RequireGenerationContext()) "        const b1 = document.getElementById(\"sb\"+pagecount);"
            writein (environment.RequireGenerationContext()) "        b1.style.display = \"none\";"
            writein (environment.RequireGenerationContext()) "        const s1 = document.getElementById(\"s\"+pagecount);"
            writein (environment.RequireGenerationContext()) "        s1.style.display = \"none\";"
            writein (environment.RequireGenerationContext()) "        const c1 = document.getElementById(\"c\"+pagecount);"
            writein (environment.RequireGenerationContext()) "        c1.style.display = \"none\";"
            writein (environment.RequireGenerationContext()) "        pagecount--;"
            writein (environment.RequireGenerationContext()) "        const p2 = document.getElementById(\"p\"+pagecount);"
            writein (environment.RequireGenerationContext()) "        p2.style.display = \"block\";"
            writein (environment.RequireGenerationContext()) "        if(sws.checked)"
            writein (environment.RequireGenerationContext()) "        {"
            writein (environment.RequireGenerationContext()) "            const b2 = document.getElementById(\"sb\"+pagecount);"
            writein (environment.RequireGenerationContext()) "            b2.style.display = \"block\";"
            writein (environment.RequireGenerationContext()) "            const s2 = document.getElementById(\"s\"+pagecount);"
            writein (environment.RequireGenerationContext()) "            s2.style.display = \"block\";"
            writein (environment.RequireGenerationContext()) "        }"
            writein (environment.RequireGenerationContext()) "        else"
            writein (environment.RequireGenerationContext()) "        {"
            writein (environment.RequireGenerationContext()) "            const b2 = document.getElementById(\"sb\"+pagecount);"
            writein (environment.RequireGenerationContext()) "            b2.style.display = \"none\";"
            writein (environment.RequireGenerationContext()) "            const s2 = document.getElementById(\"s\"+pagecount);"
            writein (environment.RequireGenerationContext()) "            s2.style.display = \"none\";"
            writein (environment.RequireGenerationContext()) "        }"
            writein (environment.RequireGenerationContext()) "        if(swc.checked)"
            writein (environment.RequireGenerationContext()) "        {"
            writein (environment.RequireGenerationContext()) "            const c2 = document.getElementById(\"c\"+pagecount);"
            writein (environment.RequireGenerationContext()) "            c2.style.display = \"block\";"
            writein (environment.RequireGenerationContext()) "        }"
            writein (environment.RequireGenerationContext()) "        else"
            writein (environment.RequireGenerationContext()) "        {"
            writein (environment.RequireGenerationContext()) "            const c2 = document.getElementById(\"c\"+pagecount);"
            writein (environment.RequireGenerationContext()) "            c2.style.display = \"none\";"
            writein (environment.RequireGenerationContext()) "        }"
            writein (environment.RequireGenerationContext()) "        const audioPlayer = document.getElementById(\"audioPlayer\");"
            writein (environment.RequireGenerationContext()) "        if(audioList[pagecount-1] != \"\" && swa.checked)"
            writein (environment.RequireGenerationContext()) "        {"
            writein (environment.RequireGenerationContext()) ("            audioPlayer.src = \""+audioDir+"/\" + audioList[pagecount-1];")
            writein (environment.RequireGenerationContext()) "            audioPlayer.play();"
            writein (environment.RequireGenerationContext()) "        }"
            writein (environment.RequireGenerationContext()) "    }"
            writein (environment.RequireGenerationContext()) "}"

    /// キャラクターのデフォルト表示・非表示設定
    /// 字幕のデフォルト表示・非表示設定
    /// 音声のデフォルト表示・非表示設定
    /// デフォルトの設定

[<AutoOpen>]
module HtmlWebExtensions =
    type html with
        /// <summary>
        /// 内部要素のないタグ
        /// </summary>
        member this.taga (t:string,lst:list<string*PHPdata>) =
            writei this.GenerationContext ("<"+t+" ")
            this.GenerationContext.CurrentProgram.indentInc()
            for a,s in lst do
                write this.GenerationContext (a + " = <?php echo \"\\\"\"." + s.code + " . \"\\\"\"; ?> ")
            this.GenerationContext.CurrentProgram.indentDec()
            writen this.GenerationContext " />"
        /// <summary>
        /// 内部要素のあるタグ
        /// </summary>
        member this.tagb0 (t:string,lst:list<string*PHPdata>) = fun code ->
            if lst.Length=0 then
                write this.GenerationContext ("<"+t+">")
            else
                write this.GenerationContext ("<"+t+" ")
                this.GenerationContext.CurrentProgram.indentInc()
                for a,s in lst do
                    write this.GenerationContext (a + " = <?php echo \"\\\"\"." + s.code + " . \"\\\"\"; ?> ")
                this.GenerationContext.CurrentProgram.indentDec()
                write this.GenerationContext ">"
            code()
            writen this.GenerationContext ("</"+t+">")
        /// <summary>
        /// 内部要素のあるタグ
        /// </summary>
        member this.tagb (t:string,lst:list<string*PHPdata>) = fun code ->
            if lst.Length=0 then
                writein this.GenerationContext ("<"+t+">")
            else
                writei this.GenerationContext ("<"+t+" ")
                this.GenerationContext.CurrentProgram.indentInc()
                for a,s in lst do
                    writei this.GenerationContext (a + " = <?php echo \"\\\"\"." + s.code + " . \"\\\"\"; ?> ")
                this.GenerationContext.CurrentProgram.indentDec()
                writen this.GenerationContext ">"
            code()
            writein this.GenerationContext ("</"+t+">")
        /// <summary>
        /// 見出し（h1）要素を生成する
        /// </summary>
        /// <param name="t">見出しに表示する内容</param>
        member this.h1 (t:int0) = fun code ->
            this.tagb "h1" <| fun () -> this.Environment.php.echo t.code
            code()
        /// <summary>
        /// 見出し（h1）要素を生成する
        /// </summary>
        /// <param name="t">見出しに表示する内容</param>
        /// <param name="atr">文字の太さ、色を定義するスタイル情報</param>
        member this.h1 (t:int0,s:Style) = fun code ->
            this.tagb ("h1",[s.atr]) <| fun () -> this.Environment.php.echo t.code
            code()

        member this.h2 (t:int0) = fun code ->
            this.tagb "h2" <| fun () -> this.Environment.php.echo t.code
            code()
        member this.h2 (t:int0,s:Style) = fun code ->
            this.tagb ("h2",[s.atr]) <| fun () -> this.Environment.php.echo t.code
            code()

        member this.h3 (t:int0) = fun code ->
            this.tagb "h3" <| fun () -> this.Environment.php.echo t.code
            code()
        member this.h3 (t:int0,s:Style) = fun code ->
            this.tagb ("h3",[s.atr]) <| fun () -> this.Environment.php.echo t.code
            code()

        member this.h4 (t:int0) = fun code ->
            this.tagb "h4" <| fun () -> this.Environment.php.echo t.code
            code()
        member this.h4 (t:int0,s:Style) = fun code ->
            this.tagb ("h4",[s.atr]) <| fun () -> this.Environment.php.echo t.code
            code()

        member this.h5 (t:int0) = fun code ->
            this.tagb "h5" <| fun () -> this.Environment.php.echo t.code
            code()
        member this.h5 (t:int0,s:Style) = fun code ->
            this.tagb ("h5",[s.atr]) <| fun () -> this.Environment.php.echo t.code
            code()
        /// <summary>
        /// 見出し（h1）要素を生成する
        /// </summary>
        /// <param name="t">見出しに表示する内容</param>
        member this.h1 (t:double0) = fun code ->
            this.tagb "h1" <| fun () -> this.Environment.php.echo t.code
            code()
        /// <summary>
        /// 見出し（h1）要素を生成する
        /// </summary>
        /// <param name="t">見出しに表示する内容</param>
        /// <param name="atr">文字の太さ、色を定義するスタイル情報</param>
        member this.h1 (t:double0,s:Style) = fun code ->
            this.tagb ("h1",[s.atr]) <| fun () -> this.Environment.php.echo t.code
            code()

        member this.h2 (t:double0) = fun code ->
            this.tagb "h2" <| fun () -> this.Environment.php.echo t.code
            code()
        member this.h2 (t:double0,s:Style) = fun code ->
            this.tagb ("h2",[s.atr]) <| fun () -> this.Environment.php.echo t.code
            code()

        member this.h3 (t:double0) = fun code ->
            this.tagb "h3" <| fun () -> this.Environment.php.echo t.code
            code()
        member this.h3 (t:double0,s:Style) = fun code ->
            this.tagb ("h3",[s.atr]) <| fun () -> this.Environment.php.echo t.code
            code()

        member this.h4 (t:double0) = fun code ->
            this.tagb "h4" <| fun () -> this.Environment.php.echo t.code
            code()
        member this.h4 (t:double0,s:Style) = fun code ->
            this.tagb ("h4",[s.atr]) <| fun () -> this.Environment.php.echo t.code
            code()

        member this.h5 (t:double0) = fun code ->
            this.tagb "h5" <| fun () -> this.Environment.php.echo t.code
            code()
        member this.h5 (t:double0,s:Style) = fun code ->
            this.tagb ("h5",[s.atr]) <| fun () -> this.Environment.php.echo t.code
            code()
        /// <summary>
        /// フォーム送信用のsubmitボタンを生成する
        /// <para>
        /// nameとvalueの型違いに対応したオーバーロードを提供する
        /// </para>
        /// </summary>
        member this.submit(name:string,value:PHPdata) = this.taga("input",[Atr("type","\"submit\""); Atr("name","\""+name+"\""); Atr("value",value.code)])
        member this.submit(name:PHPdata,value:string) = this.taga("input",["type",PHPdata "submit"; "name", name; "value",PHPdata value])
        /// <summary>
        /// フォーム送信用のsubmitボタンを生成する
        /// </summary>
        /// <param name="name">name属性に設定する文字列</param>
        /// <param name="value">value属性に設定する文字列</param>
        member this.submit(name:string,value:string) = this.taga("input",[Atr("type","\"submit\""); Atr("name","\""+name+"\""); Atr("value","\""+value+"\"")])
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
        member this.link(url:PHPdata, s:Style) = fun code -> this.tagb ("a",[s.atr; Atr("href","\""+url.code+"\"")]) code
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
                    writein this.GenerationContext ("<"+t+">")
                else
                    writein this.GenerationContext ("<"+t+" ")
                    for a,s in lst do
                        writein this.GenerationContext (a + "=" + s.code + " ")
                    writein this.GenerationContext ">"
            code b
            writein this.GenerationContext ("</"+t+">")
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
                writein this.GenerationContext ("\\(" + text.code + "\\)")
        /// <summary>
        /// 指定位置に画像を表示する
        /// </summary>
        /// <param name="s">適用するスタイル</param>
        /// <param name="p">表示位置</param>
        /// <param name="filename">表示する画像のファイル名</param>
        member this.image (s:Style,p:position) = fun (filename:string) ->
            let f = Path.GetFileName filename
            if File.Exists filename then
                if Directory.Exists (this.GenerationContext.ContentsDirectory) then
                    File.Copy(filename, this.GenerationContext.ContentsDirectory + "\\" + f, true)
                else
                    printfn "directory not exist: %s" (this.GenerationContext.ContentsDirectory)
            else
                printfn "image file not exist: %s" filename
            let st = Style [{Key="position"; Value="absolute"}; {Key="margin-left"; Value=InvariantFormat.number p.x+"px"}; {Key="margin-top"; Value=InvariantFormat.number p.y+"px"}] + s
            this.taga ("img", [st.atr;Atr("src", Path.GetFileName (this.GenerationContext.ContentsDirectory) + "\\" + f)])
        member this.image (s:Style, id:string) = fun (filename:string) ->
            let f = Path.GetFileName filename
            if File.Exists filename then
                if Directory.Exists (this.GenerationContext.ContentsDirectory) then
                    File.Copy(filename, this.GenerationContext.ContentsDirectory + "\\" + f, true)
                else
                    printfn "directory not exist: %s" (this.GenerationContext.ContentsDirectory)
            else
                printfn "image file not exist: %s" filename
            this.taga ("img", [Atr("id",id); s.atr;Atr("src", Path.GetFileName (this.GenerationContext.ContentsDirectory) + "\\" + f)])
        member this.image (s:Style) = fun (filename:string) ->
            let f = Path.GetFileName filename
            if File.Exists filename then
                if Directory.Exists (this.GenerationContext.ContentsDirectory) then
                    File.Copy(filename, this.GenerationContext.ContentsDirectory + "\\" + f, true)
                else
                    printfn "directory not exist: %s" (this.GenerationContext.ContentsDirectory)
            else
                printfn "image file not exist: %s" filename
            this.taga ("img", [s.atr;Atr("src", Path.GetFileName (this.GenerationContext.ContentsDirectory) + "\\" + f)])
        member this.image (filename:string) =
            let f = Path.GetFileName filename
            if File.Exists filename then
                if Directory.Exists (this.GenerationContext.ContentsDirectory) then
                    File.Copy(filename, this.GenerationContext.ContentsDirectory + "\\" + f, true)
                else
                    printfn "directory not exist: %s" (this.GenerationContext.ContentsDirectory)
            else
                printfn "image file not exist: %s" filename
            this.taga ("img", [Atr("src", Path.GetFileName (this.GenerationContext.ContentsDirectory) + "\\" + f)])
        /// <summary>
        /// 指定位置に動画を表示する
        /// </summary>
        /// <param name="s">適用するスタイル</param>
        /// <param name="p">表示位置</param>
        /// <param name="filename">表示する動画のファイル名</param>
        member this.video (s:Style,p:position) = fun (filename:string) ->
            let f = Path.GetFileName filename
            if File.Exists filename then
                if Directory.Exists (this.GenerationContext.ContentsDirectory) then
                    File.Copy(filename, this.GenerationContext.ContentsDirectory + "\\" + f, true)
                else
                    printfn "directory not exist: %s" (this.GenerationContext.ContentsDirectory)
            else
                printfn "video file not exist: %s" filename
            let st = Style [{Key="margin-left"; Value=InvariantFormat.number p.x+"px"}; {Key="margin-top"; Value=InvariantFormat.number p.y+"px"}] + s
            this.tagv ("video", [st.atr;Atr("src", this.GenerationContext.ContentsDirectory + "\\" + f); Atr("controls", "")])
            this.tage "video"
        member this.video (s:Style) = fun (filename:string) ->
            let f = Path.GetFileName filename
            if File.Exists filename then
                if Directory.Exists (this.GenerationContext.ContentsDirectory) then
                    File.Copy(filename, this.GenerationContext.ContentsDirectory + "\\" + f, true)
                else
                    printfn "directory not exist: %s" (this.GenerationContext.ContentsDirectory)
            else
                printfn "video file not exist: %s" filename
            this.tagv ("video", [s.atr;Atr("src", this.GenerationContext.ContentsDirectory + "\\" + f); Atr("controls", "")])
            this.tage "video"

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
                    write this.GenerationContext cd.phpcode

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
            this.tagb("div",[Atr("class","\"fig\"")]) <| fun () ->
                this.tagb ("span",[Atr("class","\"caption\"")]) <| fun () ->
                    writein this.GenerationContext (caption)
                this.tagb("table",[Atr("class","\"tab\"")]) <| fun () ->
                    for j in 0..tlist.Length-1 do
                        this.tagb ("tr",[Atr("class",match borderV[j] with |TrTB -> "\"trtb\"" |TrT -> "\"trt\"" |TrB -> "\"trb\"" |TrN -> "\"trn\"")]) <| fun () ->
                            for i in 0..tlist[j].Length-1 do
                                this.tagb ("td",[Atr("class",
                                    match borderH[i] with
                                    |TdL -> "\"tdl\""
                                    |TdC -> "\"tdc\""
                                    |TdR -> "\"tdr\""
                                    |TdJ -> "\"tdj\""
                                    |TdLL -> "\"tdlL\""
                                    |TdCL -> "\"tdcL\""
                                    |TdRL -> "\"tdrL\""
                                    |TdJL -> "\"tdjL\""
                                    |TdLR -> "\"tdlR\""
                                    |TdCR -> "\"tdcR\""
                                    |TdRR -> "\"tdrR\""
                                    |TdJR -> "\"tdjR\""
                                    |TdLLR -> "\"tdlLR\""
                                    |TdCLR -> "\"tdcLR\""
                                    |TdRLR -> "\"tdrLR\""
                                    |TdJLR -> "\"tdjLR\"")]) <| fun () ->
                                    writein this.GenerationContext (tlist[j][i])
        /// <summary>
        /// num0式を評価し、MathJax形式で出力する
        /// </summary>
        member this.eq(text:int0) =
            writein this.GenerationContext ("\\("+text.Expr.evalL this.GenerationContext.CurrentProgram + "\\)")
        /// <summary>
        /// num0式を評価し、MathJax形式で出力する
        /// </summary>
        member this.eq(text:double0) =
            writein this.GenerationContext ("\\("+text.Expr.evalL this.GenerationContext.CurrentProgram + "\\)")
        /// <summary>
        /// num0式を評価し、MathJax形式で出力する
        /// </summary>
        member this.eq(text:complex0) =
            writein this.GenerationContext ("\\("+text.Expr.evalL this.GenerationContext.CurrentProgram + "\\)")

        /// <summary>
        /// キャラクター付き解説ページ
        /// </summary>
        member this.page (c:list<CharacterImage>) (audio:Audio,audioFile:option<string>,scriptColor:string) code2 =
            this.slide position.Origin <| fun p ->
                let animationCounter = this.GenerationContext.AnimationCount
                let contentsDirectory = this.GenerationContext.ContentsDirectory
                // 音声ファイル追加
                this.GenerationContext.AddAudioFile(
                    match audioFile with |Some t -> t |None -> "")
                // 字幕枠
                this.tag "div" ("id = \"sb"+animationCounter.ToString()+"\" style=\"width: 1880px; height: 160px; " + (if this.GenerationContext.SubtitleEnabled then "display: block; " else "display: none; ") + "position: absolute; z-index: 1; margin-top: 880px; padding: 20px; background-color: #aaaaff; font-family: 'Noto Sans JP'; font-size: 36pt; font-weight: 800; text-shadow: 0 1px 0 #fff, 1px 0 0 #fff, 0 -1px 0 #fff, -1px -1px 0 #fff, 1px -1px 0 #fff, -1px 1px 0 #fff, 1px 1px 0 #fff \";") <| fun () ->
                    ()
                // キャラクター画像
                this.tag "div" ("id = \"c"+animationCounter.ToString()+"\"" + "style=\"" + (if this.GenerationContext.CharacterEnabled then "display: block; " else "display: none; ") + "\"") <| fun () ->
                    for ci in c do
                        if File.Exists ci.CharacterImageFile then
                            if Directory.Exists contentsDirectory then
                                File.Copy(ci.CharacterImageFile, contentsDirectory+"\\"+Path.GetFileName ci.CharacterImageFile, true)
                                this.tag_ "img" <| "src=\"" + Path.GetFileName contentsDirectory + "/" + Path.GetFileName ci.CharacterImageFile + "\" style=\"" + ci.CharacterImageStyle + "\""
                            else
                                printfn "directory not exist: %s" contentsDirectory
                        else
                            printfn "character image file not exist: %s" ci.CharacterImageFile
                // 字幕
                this.tag "div" ("id = \"s"+animationCounter.ToString()+"\" style=\"width: 1880px; height: 160px; " + (if this.GenerationContext.SubtitleEnabled then "display: block; " else "display: none; ") + "position: absolute; z-index: 5; margin-top: 880px; padding: 20px; font-family: 'Noto Sans JP'; color: "+scriptColor+"; font-size: 36pt; font-weight: 800; text-shadow: 0 1px 0 #fff, 1px 0 0 #fff, 0 -1px 0 #fff, -1px 0 0 #fff, -1px -1px 0 #fff, 1px -1px 0 #fff, -1px 1px 0 #fff, 1px 1px 0 #fff ;\"")
                    <| fun () -> writein this.GenerationContext audio.Subtitle
                this.Environment.htmlio.switchAutoAnimation <| fun environment ->
                    writein (environment.RequireGenerationContext()) ("page"+animationCounter.ToString()+": () => {")
                // メインコンテンツ
                this.tag "div" "style=\"width: 1920px; height: 880px; position: absolute; z-index: 0;\"" <| fun () ->
                    code2 p
                this.Environment.htmlio.switchAutoAnimation <| fun environment ->
                    writein (environment.RequireGenerationContext()) "},"
                match this.GenerationContext.TryLastAnimationButton() with
                | Some(fStartName,fResetName,btnx,btny) ->
                    this.startButton2 ("startButton"+fStartName) (Style[position.position "absolute"; margin.left (btnx.ToString()+"px"); margin.top (btny.ToString()+"px"); position.index 1000;]) ("animationStartMap['"+fStartName+"']()")
                    this.resetButton2 ("resetButton"+fStartName) (Style[position.position "absolute"; margin.left (btnx.ToString()+"px"); margin.top ((btny+25).ToString()+"px"); position.index 1000;]) ("animationResetMap['"+fResetName+"']()")
                | None -> ()
                this.GenerationContext.ClearAnimationButtons()
        /// <summary>
        /// 指定位置にスライドを生成
        /// </summary>
        /// <param name="p">スライドの表示位置</param>
        member this.slide (p:position)  code =
                let animationCounter = this.GenerationContext.NextAnimationNumber()
                this.tagb ("div", "id=\"p"+animationCounter.ToString()+"\" style=\"display: "+(if animationCounter=1 then "block" else "none")+"; position: absolute;\"") <| fun wr ->
                    code p
        /// <summary>
        /// 前のページへ移動するボタンを生成
        /// </summary>
        member this.prevButton() =
                this.tagb ("button", "id=\"prevButton\" style=\"position: absolute; z-index: 100;\" onclick=\"drawPrev()\"") <| fun () ->
                    writein this.GenerationContext "前へ"
        /// <summary>
        /// 次のページへ移動するボタンを生成
        /// </summary>
        member this.nextButton() =
                this.tagb ("button", "id=\"nextButton\" style=\"position: absolute; margin-left: 75px; z-index: 100;\" onclick=\"drawNext()\"") <| fun () ->
                    writein this.GenerationContext "次へ"
        /// <summary>
        /// アニメーションを開始するボタンを生成
        /// </summary>
        member this.startButton2(id:string) (s:Style) (c:string) =
                this.tagb ("button", [Atr("id",id); Atr("onclick",c)]@[s.atr]) <| fun () ->
                    writein this.GenerationContext "Start"
        /// <summary>
        /// アニメーションをリセットするボタンを生成
        /// </summary>
        member this.resetButton2(id:string) (s:Style) (c:string) =
                this.tagb ("button", [Atr("id",id); Atr("onclick",c)]@[s.atr]) <| fun () ->
                    writein this.GenerationContext "Reset"
        /// <summary>
        /// キャラクター表示を制御するチェックボックスを生成
        /// </summary>
        member this.switchCharacter() =
            this.taga ("input", "type=\"checkbox\" id=\"switchCharacter\" style=\"position: absolute; margin-top: 6px; margin-left: 150px; z-index: 100;\"  onclick=\"setCharacter()\" " + if this.GenerationContext.CharacterEnabled then "checked" else "")
            this.tagb ("label", "style=\"position: absolute; margin-top: 0px; margin-left: 165px; z-index: 100;\"") <| fun () ->
                writein this.GenerationContext "キャラクター"
        /// <summary>
        /// 字幕表示を制御するチェックボックスを生成
        /// </summary>
        member this.switchSubtitle() =
            this.taga ("input", "type=\"checkbox\" id=\"switchSubtitle\" style=\"position: absolute; margin-top: 6px; margin-left: 270px; z-index: 100;\" onclick=\"setSubtitle()\" " + if this.GenerationContext.SubtitleEnabled then "checked" else "")
            this.tagb ("label", "style=\"position: absolute; margin-top: 0px; margin-left: 285px; z-index: 100;\"") <| fun () ->
                writein this.GenerationContext "字幕"
        /// <summary>
        /// 音声再生を制御するチェックボックスを生成
        /// </summary>
        member this.switchAudio() =
            this.taga ("input", "type=\"checkbox\" id=\"switchAudio\" style=\"position: absolute; margin-top: 6px; margin-left: 330px; z-index: 100;\" onclick=\"setSubtitle()\" " + if this.GenerationContext.VoiceEnabled then "checked" else "")
            this.tagb ("label", "style=\"position: absolute; margin-top: 0px; margin-left: 345px; z-index: 100;\"") <| fun () ->
                writein this.GenerationContext "音声"
        member this.audioPlayer() =
                this.tagb ("audio", "id=\"audioPlayer\"")  <| fun () -> ()
        /// <summary>
        /// 指定位置に画像を表示
        /// </summary>
        member this.imageA (s:Style) = fun (p:position) (filename:string) ->
            let s1 = Style [{Key = "margin-left"; Value = InvariantFormat.number p.x+"px";}
                            {Key = "margin-top"; Value = InvariantFormat.number p.y+"px";}
                            {Key = "position"; Value = "absolute";}]
            let f = Path.GetFileName filename
            if File.Exists filename then
                if Directory.Exists (this.GenerationContext.ContentsDirectory) then
                    File.Copy(filename, this.GenerationContext.ContentsDirectory + "\\" + f, true)
                else
                    printfn "directory not exist: %s" (this.GenerationContext.ContentsDirectory)
            else
                printfn "image file not exist: %s" filename
            this.taga ("img", [(s1+s).atr])
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
                text |> List.iter (fun s -> writein this.GenerationContext (s+"<br>"))
                writein this.GenerationContext ("\r\n")
            {Left = p.x;
            Right = p.x+double width+2.0*double padding;
            Top = p.y;
            Bottom = p.y+double height+2.0*double padding;}
/// <summary>
/// 図形アニメーションを管理するクラス
/// </summary>
/// <param name="figcounter">図形の識別番号</param>
/// <param name="originX, originY">描画の基準座標</param>
/// <param name="canvasX, canvasY">キャンパスのサイズ</param>
[<AutoOpen>]
module CompilationEnvironmentAnimationExtensions =
    type CompilationEnvironment with
        member this.slideAnimation = ContextSlideAnimation(this)

type FigureAnimation(environment:CompilationEnvironment,figcounter:int,originX:int,originY:int,canvasX:int,canvasY:int) =
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
        let idstart,idreset = environment.htmlio.nextAnimationSeqID()
        environment.htmlio.switchAnimationSeq <| fun environment ->
            writein (environment.RequireGenerationContext()) ("function "+idstart+"(t){")
        environment.htmlio.switchJSAnimationSeqReset <| fun environment ->
            writein (environment.RequireGenerationContext()) ("function "+idreset+"(){")
        setFigure setting
        environment.htmlio.switchAnimationSeq <| fun environment ->
            writein (environment.RequireGenerationContext()) "}"
        environment.htmlio.switchJSAnimationSeqReset <| fun environment ->
            writein (environment.RequireGenerationContext()) "}"
        animeFlow <- animeFlow@[idstart,idreset,setting,false]
    /// <summary>
    /// アニメーションをループする
    /// </summary>
    member this.loop (setting:AnimationSetting) (setFigure:AnimationSetting->unit) =
        // アニメーションシーケンスIDを発行
        let idstart,idreset = environment.htmlio.nextAnimationSeqID()
        environment.htmlio.switchAnimationSeq <| fun environment ->
            writein (environment.RequireGenerationContext()) ("function "+idstart+"(t){")
        environment.htmlio.switchJSAnimationSeqReset <| fun environment ->
            writein (environment.RequireGenerationContext()) ("function "+idreset+"(){")
        setFigure setting
        environment.htmlio.switchAnimationSeq <| fun environment ->
            writein (environment.RequireGenerationContext()) "}"
        environment.htmlio.switchJSAnimationSeqReset <| fun environment ->
            writein (environment.RequireGenerationContext()) "}"
        animeFlow <- animeFlow@[idstart,idreset,setting,true]
    /// <summary>
    /// キャンバスアニメーションを指定して図形アニメーションを生成
    /// </summary>
    /// <param name="s">アニメーション設定</param>
    member this.animationEllipse s = AnimationEllipse(environment,s,canvasX,canvasY)
    member this.animationLine s = AnimationLine(environment,s,canvasX,canvasY)
    member this.animationArc s = AnimationArc(environment,s,canvasX,canvasY)
    member this.animationText s = AnimationText(environment,s,originX,originY,canvasX,canvasY)
    member this.animationPolygon s = AnimationPolygon(environment,s,canvasX,canvasY)
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
        environment.html.taga ("line", [s.atr]@c)
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
        environment.html.taga ("ellipse", [s.atr]@c)
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
        environment.html.taga ("path", [s.atr]@[Atr("d",d)])
    /// <summary>
    /// 多角形を描画
    /// </summary>
    /// <param name="apex">多角形を構成する頂点のリスト</param>
    member this.polygon (s:Style) (apex:list<position>) =
        let pp =
            apex
            |> List.map (fun p -> InvariantFormat.number p.x + "," + InvariantFormat.number (double canvasY-p.y))
            |> fun s -> String.Join(",",s)
        environment.html.taga ("polygon", [s.atr]@[Atr("points",pp)])
    /// <summary>
    /// 折れ線を描画
    /// </summary>
    /// <param name="apex">折れ線を構成する頂点のリスト</param>
    member this.polyline (s:Style) (apex:list<position>) =
        let pp =
            apex
            |> List.map (fun p -> InvariantFormat.number p.x + "," + InvariantFormat.number (double canvasY-p.y))
            |> fun s -> String.Join(",",s)
        environment.html.taga ("polyline", [s.atr]@[Atr("points",pp)])
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
        environment.html.taga ("rect", [s.atr]@c)
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
        environment.html.tagb ("div", [ss.atr]) <| fun () ->
            writein (environment.RequireGenerationContext()) str
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
        environment.html.tagb ("div", [ss.atr]) <| fun () ->
            writein (environment.RequireGenerationContext()) ("\\(" + e.Expr.evalH (environment.RequireGenerationContext().CurrentProgram) + "\\)")
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
        environment.html.tagb ("div", [ss.atr]) <| fun () ->
            writein (environment.RequireGenerationContext()) ("\\(" + e.Expr.evalH (environment.RequireGenerationContext().CurrentProgram) + "\\)")
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
        environment.html.tagb ("div", [ss.atr]) <| fun () ->
            writein (environment.RequireGenerationContext()) ("\\(" + e.Expr.evalH (environment.RequireGenerationContext().CurrentProgram) + "\\)")
    /// <summary>
    /// 画像を表示
    /// </summary>
    /// <param name="filename">画像のファイル名</param>
    member this.image (s:Style) (center:position) (filename:string) =
        let f = Path.GetFileName filename
        File.Copy(filename, environment.RequireGenerationContext().ContentsDirectory + "\\" + f, true)
        let c = [
            {Key="display";Value="block"}
            {Key="position";Value="absolute"}
            {Key="margin-left";Value=InvariantFormat.number (double originX+center.x)+"px"}
            {Key="margin-top";Value=InvariantFormat.number (double originY+double canvasY-center.y)+"px"}]
        let ss = Style (s.list@c)
        environment.html.taga ("img", [ss.atr; Atr("src",environment.RequireGenerationContext().ContentsDirectory + "\\" + f)])
    /// <summary>
    /// 開始ボタンの制御用JavaScriptコードを生成
    /// </summary>
    /// <param name="buttonIndex">対象となるボタンの識別子</param>
    member this.jsStartControll(buttonIndex:string) =
        let fname = "start" + buttonIndex
        environment.htmlio.switchJSAnimationStart <| fun environment ->
            writein (environment.RequireGenerationContext()) (fname+": () => {")
            for idstart,_,setting,isLoop in animeFlow do
                if isLoop then
                    writein (environment.RequireGenerationContext()) ("    repeat(" + idstart + ", " + setting.FrameTime.ToString() + ", " + setting.FrameNumber.ToString() + ");")
                else
                    writein (environment.RequireGenerationContext()) ("    repeatSeq(" + idstart + ", " + setting.FrameTime.ToString() + ", " + setting.FrameNumber.ToString() + ", () => {")
            for _,_,_,isLoop in animeFlow do
                if isLoop then
                    ()
                else
                    writein (environment.RequireGenerationContext()) "    });"
            writein (environment.RequireGenerationContext()) "},"
        fname
    /// <summary>
    /// リセットボタンの制御用JavaScriptコードを生成
    /// </summary>
    member this.jsResetControll(buttonIndex:string) =
        let fname = "reset" + buttonIndex
        environment.htmlio.switchJSAnimationReset <| fun environment ->
            writein (environment.RequireGenerationContext()) (fname+": () => {")
            for _,idreset,_,_ in animeFlow do
                writein (environment.RequireGenerationContext()) ("    " + idreset + "();")
            writein (environment.RequireGenerationContext()) "},"
        fname
    /// <summary>
    /// アニメーション用のJavaScriptコードを生成
    /// </summary>
    member _.jsAnimation codejs =
        environment.htmlio.switchBody <| fun environment ->
            writein (environment.RequireGenerationContext()) "var t = 0;"
            writein (environment.RequireGenerationContext()) "var dt = 1;"
            writein (environment.RequireGenerationContext()) "window.onload=function(){"
            writein (environment.RequireGenerationContext()) "    var timer;"
            writein (environment.RequireGenerationContext()) "    var delay = 33;"
            writein (environment.RequireGenerationContext()) "    var loop = function(){"
            writein (environment.RequireGenerationContext()) "        t = t + dt;"
            writein (environment.RequireGenerationContext()) "        if(t >= 100){t = 0;}"
            writein (environment.RequireGenerationContext()) "        clearTimeout(timer);"
            writein (environment.RequireGenerationContext()) "        timer=setTimeout(loop,delay);"
            writein (environment.RequireGenerationContext()) "    }"
            writein (environment.RequireGenerationContext()) "    loop();"
            writein (environment.RequireGenerationContext()) "}"
            writein (environment.RequireGenerationContext()) codejs

[<AutoOpen>]
module dochtml =
    let private htmlpresentationCore
        (movieSetting:MovieSetting)
        (dir:string)
        (filename:string)
        (title:string)
        (cssfile:option<string>)
        (pagesizeX:option<int>,pagesizeY:option<int>)
        isPageAnimation
        code =
        // ディレクトリ作成
        if not <| Directory.Exists (dir + "\\" + "contents_" + filename) then
            ignore <| Directory.CreateDirectory(dir + "\\" + "contents_" + filename)
        // コンテンツディレクトリ
        makeProgramWithMovieSetting movieSetting
            [
                // メインファイル
                dir, filename + ".html", HTML
                // HTML本体のコード
                dir, filename+"_body", HTML
                // JavaScriptのコード
                dir, filename+"_js", JavaScript
                // スライドアニメーション用javascriptファイル名
                dir  + "\\" + "contents_" + filename, "animationSeq.js", JavaScript
                // スライドアニメーション(アニメーション開始)用javascript
                dir  + "\\" + "contents_" + filename, "animationStart.js", JavaScript
                // スライドアニメーション(アニメーションリセット)用javascript
                dir  + "\\" + "contents_" + filename, "animationSeqReset.js", JavaScript
                // スライドアニメーション(アニメーションリセット)用javascript
                dir  + "\\" + "contents_" + filename, "animationReset.js", JavaScript
                // オートアニメーション実行用javascript
                dir  + "\\" + "contents_" + filename, "autoAnimation.js", JavaScript
            ]
            <| fun context ->
                let environment = CompilationEnvironment(Some context)
                context.ContentsDirectory <-
                    dir + "\\" + "contents_" + filename
                environment.htmlio.switchJSAnimationStart <| fun environment ->
                    writein (environment.RequireGenerationContext()) "const animationStartMap = {"
                environment.htmlio.switchJSAnimationReset <| fun environment ->
                    writein (environment.RequireGenerationContext()) "const animationResetMap = {"
                environment.htmlio.switchAutoAnimation <| fun environment ->
                    writein (environment.RequireGenerationContext()) "const autoAnimationMap = {"
                environment.htmlio.switchAnimationSeq <| fun environment ->
                    writein (environment.RequireGenerationContext()) "function repeatSeq(fn, interval, Nt, onComplete)"
                    writein (environment.RequireGenerationContext()) "{"
                    writein (environment.RequireGenerationContext()) "    let t = 0;"
                    writein (environment.RequireGenerationContext()) "    function run()"
                    writein (environment.RequireGenerationContext()) "    {"
                    writein (environment.RequireGenerationContext()) "        if (t < Nt)"
                    writein (environment.RequireGenerationContext()) "        {"
                    writein (environment.RequireGenerationContext()) "            fn(t);"
                    writein (environment.RequireGenerationContext()) "            t++;"
                    writein (environment.RequireGenerationContext()) "            setTimeout(run, interval);"
                    writein (environment.RequireGenerationContext()) "        }"
                    writein (environment.RequireGenerationContext()) "        else"
                    writein (environment.RequireGenerationContext()) "        {"
                    writein (environment.RequireGenerationContext()) "            onComplete();"
                    writein (environment.RequireGenerationContext()) "        }"
                    writein (environment.RequireGenerationContext()) "    }"
                    writein (environment.RequireGenerationContext()) "    run();"
                    writein (environment.RequireGenerationContext()) "}"
                    writein (environment.RequireGenerationContext()) "function repeat(fn, interval, Nt)"
                    writein (environment.RequireGenerationContext()) "{"
                    writein (environment.RequireGenerationContext()) "    let t = 0;"
                    writein (environment.RequireGenerationContext()) "    function run()"
                    writein (environment.RequireGenerationContext()) "    {"
                    writein (environment.RequireGenerationContext()) "        if(t == Nt)"
                    writein (environment.RequireGenerationContext()) "        {"
                    writein (environment.RequireGenerationContext()) "            t = 0;"
                    writein (environment.RequireGenerationContext()) "        }"
                    writein (environment.RequireGenerationContext()) "        fn(t);"
                    writein (environment.RequireGenerationContext()) "        t++;"
                    writein (environment.RequireGenerationContext()) "        setTimeout(run, interval);"
                    writein (environment.RequireGenerationContext()) "    }"
                    writein (environment.RequireGenerationContext()) "    run();"
                    writein (environment.RequireGenerationContext()) "}"
                environment.htmlio.switchBody <| fun environment ->
                    code environment
                if isPageAnimation then
                    environment.slideAnimation.writeAudioList()
                    environment.slideAnimation.jsSetCharacter()
                    environment.slideAnimation.jsSetSubtitle()
                    environment.slideAnimation.jsDrawNext("contents_" + filename)
                    environment.slideAnimation.jsDrawPrev("contents_" + filename)
                // head、body要素書き込みストリームを閉じてhead、body要素のコード取得
                let codeDraw = environment.htmlio.switchJSMain <| fun environment ->
                    environment.RequireGenerationContext().CurrentProgram.allCodes
                let codeBody = environment.htmlio.switchBody <| fun environment ->
                    environment.RequireGenerationContext().CurrentProgram.allCodes
                // html書き込みストリーム作成
                environment.htmlio.switchMain <| fun environment ->
                    writein (environment.RequireGenerationContext()) "<!DOCTYPE html>"
                    // html要素
                    environment.html.tagb ("html", "lang=\"ja\"") <| fun () ->
                        // head要素
                        environment.html.tagb ("head", "") <| fun () ->
                            // titleタグ
                            writein (environment.RequireGenerationContext()) ("<title>"+title+"</title>")
                            // metaタグ
                            writein (environment.RequireGenerationContext()) "<meta charset=\"UTF-8\">"
                            //追加（5/29）viewportタブ
                            match pagesizeX with
                            |None ->
                                writein (environment.RequireGenerationContext()) "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0, maximum-scale=1.0, minimum-scale=1.0\">"
                            |Some width ->
                                writein (environment.RequireGenerationContext()) ("<meta name=\"viewport\" content=\"width=" + width.ToString() + "\">")
                            // titleタグ
                            environment.html.tagb ("title", "") <| fun () ->
                                writein (environment.RequireGenerationContext()) filename
                            // MathJax
                            environment.html.tagb ("script", "type=\"text/javascript\" id=\"MathJax-script\" async src=\"https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml.js\"") <| fun () -> ()
                            environment.html.tagb ("script", "type=\"text/javascript\" src=\"" + "contents_" + filename + "/animationSeq.js\"") <| fun () -> ()
                            environment.html.tagb ("script", "type=\"text/javascript\" src=\"" + "contents_" + filename + "/animationSeqReset.js\"") <| fun () -> ()
                            environment.html.tagb ("script", "type=\"text/javascript\" src=\"" + "contents_" + filename + "/animationStart.js\"") <| fun () -> ()
                            environment.html.tagb ("script", "type=\"text/javascript\" src=\"" + "contents_" + filename + "/animationReset.js\"") <| fun () -> ()
                            environment.html.tagb ("script", "type=\"text/javascript\" src=\"" + "contents_" + filename + "/autoAnimation.js\"") <| fun () -> ()
                            // scriptタグ
                            environment.html.tagb ("script", "") <| fun () ->
                                writein (environment.RequireGenerationContext()) codeDraw
                            // webフォント取得
                            writein (environment.RequireGenerationContext()) "<link rel=\"preconnect\" href=\"https://fonts.googleapis.com\">"
                            writein (environment.RequireGenerationContext()) "<link rel=\"preconnect\" href=\"https://fonts.gstatic.com\" crossorigin>"
                            writein (environment.RequireGenerationContext()) "<link href=\"https://fonts.googleapis.com/css2?family=Noto+Sans+JP:wght@100..900&display=swap\" rel=\"stylesheet\">"
                            match cssfile with |Some x -> writein (environment.RequireGenerationContext()) ("<link rel=\"stylesheet\" href=\""+x+"\" />") |None -> ()
                        // body要素
                        match pagesizeX,pagesizeY with
                        |None,None ->
                            let s0 = Style [area.backGroundColor "#ffffff"]
                            environment.html.tagb ("body", [s0.atr]) <| fun () ->
                                writein (environment.RequireGenerationContext()) codeBody
                        |Some x,None ->
                            let s0 = Style [area.backGroundColor "#aaaaaa"]
                            environment.html.tagb ("body", [s0.atr]) <| fun () ->
                                let s1 = Style [
                                    area.backGroundColor "#ffffff"
                                    margin.left "auto"
                                    margin.right "auto"
                                    size.width (x.ToString()+"px")]
                                environment.html.tagb ("div", [s1.atr]) <| fun () ->
                                    writein (environment.RequireGenerationContext()) codeBody
                        |None,Some y->
                            let s0 = Style [area.backGroundColor "#aaaaaa"]
                            environment.html.tagb ("body", [s0.atr]) <| fun () ->
                                let s1 = Style [
                                    area.backGroundColor "#ffffff"
                                    margin.left "auto"
                                    margin.right "auto"
                                    size.height (y.ToString()+"px")]
                                environment.html.tagb ("div", [s1.atr]) <| fun () ->
                                    writein (environment.RequireGenerationContext()) codeBody
                        |Some x,Some y ->
                            let s0 = Style [area.backGroundColor "#aaaaaa"]
                            environment.html.tagb ("body", [s0.atr]) <| fun () ->
                                let s1 = Style [
                                    area.backGroundColor "#ffffff"
                                    margin.left "auto"
                                    margin.right "auto"
                                    size.width (x.ToString()+"px")
                                    size.height (y.ToString()+"px")]
                                environment.html.tagb ("div", [s1.atr]) <| fun () ->
                                    writein (environment.RequireGenerationContext()) codeBody

                environment.htmlio.switchJSAnimationStart <| fun environment ->
                    writein (environment.RequireGenerationContext()) "test: () => {}"
                    writein (environment.RequireGenerationContext()) "};"
                environment.htmlio.switchJSAnimationReset <| fun environment ->
                    writein (environment.RequireGenerationContext()) "test: () => {}"
                    writein (environment.RequireGenerationContext()) "};"
                    writein (environment.RequireGenerationContext()) ""
                    writein (environment.RequireGenerationContext()) "function resetAll(){"
                    writein (environment.RequireGenerationContext()) "    for (const key in animationResetMap) {"
                    writein (environment.RequireGenerationContext()) "        if (typeof animationResetMap[key] === \"function\") {"
                    writein (environment.RequireGenerationContext()) "            animationResetMap[key]();"
                    writein (environment.RequireGenerationContext()) "        }"
                    writein (environment.RequireGenerationContext()) "    }"
                    writein (environment.RequireGenerationContext()) "}"
                environment.htmlio.switchAutoAnimation <| fun environment ->
                    writein (environment.RequireGenerationContext()) "test: () => {}"
                    writein (environment.RequireGenerationContext()) "};"
                let context = environment.RequireGenerationContext()
                for i in 0..7 do
                    context.Programs[i].close()
                // bodyタグ一時コード削除
                context.Programs[1].delete()
                // JavaScript関数一時コード削除
                context.Programs[2].delete()

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
            MovieSetting.Default
            dir
            filename
            title
            cssfile
            pagesize
            isPageAnimation
            code

    let freeCanvas outputdir filename (title:string) cssfile code =
        htmlpresentation outputdir filename title cssfile (None, None) false <| fun environment ->
            environment.html.canvas <| Style [size.width "0px"; size.height "0px"] <| fun () -> code environment

    /// 全体がキャンバスの無制限レイアウト
    let freePage outputdir filename (title:string) cssfile code =
        htmlpresentation outputdir filename title cssfile (None, None) false code

    /// 固定幅レイアウト
    let fixedWidthPage outputdir filename (title:string) pageWidth cssfile code =
        htmlpresentation outputdir filename title cssfile (Some pageWidth, None) false code

    let fixedPage outputdir filename (title:string) pageWidth pageHeight setting cssfile code =
        htmlpresentationCore setting outputdir filename title cssfile (Some pageWidth, Some pageHeight) true <| fun environment ->
            code environment
            environment.html.prevButton()
            environment.html.nextButton()
            environment.html.switchCharacter()
            environment.html.switchSubtitle()
            environment.html.switchAudio()
            environment.html.audioPlayer()

[<AutoOpen>]
module htmlexpr2 =
    type html with
        /// <summary>
        /// 手動操作型のアニメーション領域を生成
        /// </summary>
        /// <param name="s">アニメーションの領域設定</param>
        /// <param name="p">表示位置</param>
        /// <param name="buttonX, buttonY">操作ボタンの配置座標</param>
        member this.animationManual (s:ViewBoxStyle) (p:position) (buttonX:int,buttonY:int) code =
            let environment = this.Environment
            let context = environment.RequireGenerationContext()
            let f =
                FigureAnimation(
                    environment, context.NextFigureNumber(),
                    s.mX,s.mY,s.sX,s.sY)
            environment.htmlio.switchBody <| fun environment ->
                let context = environment.RequireGenerationContext()
                writein context ("<svg viewBox=\"0 0 "+s.sX.ToString()+" "+s.sY.ToString()+"\" ")
                writein context ("width=\""+s.sX.ToString()+"px\" ")
                writein context ("heigth=\""+s.sY.ToString()+"px\" ")
                writein context "xmlns=\"http://www.w3.org/2000/svg\" "
                writein context ("style=\"margin-left: "+s.mX.ToString()+"; ")
                writein context ("margin-top: "+s.mY.ToString()+"; ")
                writein context "position: absolute;"
                writein context ("background-color: "+s.backgroundColor+";")
                writein context "\">"
                code(f,p)
                writein context "</svg>"
            let asc = environment.htmlio.nextAnimationGroup()
            let fnameStart = f.jsStartControll asc
            let fnameReset = f.jsResetControll asc
            environment.htmlio.addAnimationButton(fnameStart,fnameReset,buttonX,buttonY)

        /// <summary>
        /// 自動再生型のアニメーション領域を生成する
        /// </summary>
        member this.animationAuto (s:ViewBoxStyle) (p:position) code =
            let environment = this.Environment
            let context = environment.RequireGenerationContext()
            let f =
                FigureAnimation(
                    environment, context.NextFigureNumber(),
                    s.mX,s.mY,s.sX,s.sY)
            environment.htmlio.switchBody <| fun environment ->
                let context = environment.RequireGenerationContext()
                writein context ("<svg viewBox=\"0 0 "+s.sX.ToString()+" "+s.sY.ToString()+"\" ")
                writein context ("width=\""+s.sX.ToString()+"px\" ")
                writein context ("heigth=\""+s.sY.ToString()+"px\" ")
                writein context "xmlns=\"http://www.w3.org/2000/svg\" "
                writein context ("style=\"margin-left: "+s.mX.ToString()+"; ")
                writein context ("margin-top: "+s.mY.ToString()+"; ")
                writein context "position: absolute;"
                writein context ("background-color: "+s.backgroundColor+";")
                writein context "\">"
                code(f,p)
                writein context "</svg>"
            let asc = environment.htmlio.nextAnimationGroup()
            let fnameStart = f.jsStartControll asc
            let fnameReset = f.jsResetControll asc
            environment.htmlio.addAutoAnimation(fnameStart,fnameReset)
