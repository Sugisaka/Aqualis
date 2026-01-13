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
    
type MovieSetting = {
    /// キャラクター表示
    Character:Switch;
    /// 字幕表示
    Subtitle:Switch;
    /// 音声再生
    Voice:Switch}
    
type tposition = {
    /// x座標：時間（フレーム番号）の関数
    X:num0->num0; 
    /// y座標：時間（フレーム番号）の関数
    Y:num0->num0}
    
type Line = {
    /// 始点
    Start:tposition;
    /// 終点
    End:tposition;}
    
type Ellipse = {
    /// 中心座標
    center:tposition; 
    /// 半径(x)
    radiusX:num0->num0; 
    /// 半径(y)
    radiusY:num0->num0;}
    
type Arc = {
    /// 円弧の中心座標
    center:tposition;
    /// 開始角（度数法, 反時計回りに描画）
    angle1:num0->num0;
    /// 終了角（度数法, 反時計回りに描画）
    angle2:num0->num0;
    /// 円弧の半径
    radius:num0->num0;}
    
type Text = {
    /// 中心座標
    center:tposition;
    /// 表示するテキスト
    str:string; }
    
type MathText = {
    /// 中心座標
    center:tposition;
    /// 表示する数式
    eq:num0; }
    
type AnimationLine(s:Style,canvasX:int,canvasY:int) =
    let id = nextContentsID()
    let s0 = Style ([{Key="visibility";Value="hidden"}]@s.list)
    let s1 = Style ([{Key="visibility";Value="visible"}]@s.list)
    do
        html.taga ("line", [Atr("id",id);]@[s0.atr])
    member this.ID with get() = id
    member this.P (f:Line) = 
        let t = num0(Var(Dt,"t",NaN))
        switchAnimationSeq <| fun () ->
            writein("    var e = document.getElementById(\""+id+"\");")
            writein("    var x1 = " + (f.Start.X t).code + ";")
            writein("    var y1 = " + (canvasY - f.Start.Y t).code + ";")
            writein("    var x2 = " + (f.End.X t).code + ";")
            writein("    var y2 = " + (canvasY - f.End.Y t).code + ";")
            writein("    e.setAttribute(\"style\"," + "\"" + s1.code0 + "\");")
            writein "    e.setAttribute(\"x1\", x1);"
            writein "    e.setAttribute(\"y1\", y1);"
            writein "    e.setAttribute(\"x2\", x2);"
            writein "    e.setAttribute(\"y2\", y2);"
        switchJSAnimationSeqReset <| fun () ->
            writein("    var e = document.getElementById(\""+id+"\");")
            writein("    e.setAttribute(\"style\"," + "\"" + s0.code0 + "\");")
            
type AnimationEllipse(s:Style,canvasX:int,canvasY:int) =
    let id = nextContentsID()
    let s0 = Style ([{Key="visibility";Value="hidden"}]@s.list)
    let s1 = Style ([{Key="visibility";Value="visible"}]@s.list)
    do
        html.taga ("ellipse", [Atr("id",id);]@[s0.atr])
    member this.ID with get() = id
    member this.P (e:Ellipse) = 
        let t = num0(Var(Dt,"t",NaN))
        switchAnimationSeq <| fun () ->
            writein("    var e = document.getElementById(\""+id+"\");")
            writein("    var cx = " + (e.center.X t).code + ";")
            writein("    var cy = " + (canvasY - e.center.Y t).code + ";")
            writein("    var rx = " + (e.radiusX t).code + ";")
            writein("    var ry = " + (e.radiusY t).code + ";")
            writein("    e.setAttribute(\"style\"," + "\"" + s1.code0 + "\");")
            writein "    e.setAttribute(\"cx\", cx);"
            writein "    e.setAttribute(\"cy\", cy);"
            writein "    e.setAttribute(\"rx\", rx);"
            writein "    e.setAttribute(\"ry\", ry);"
        switchJSAnimationSeqReset <| fun () ->
            writein("    var e = document.getElementById(\""+id+"\");")
            writein("    e.setAttribute(\"style\"," + "\"" + s0.code0 + "\");")
            
type AnimationArc(s:Style,canvasX:int,canvasY:int) =
    let id = nextContentsID()
    let s0 = Style ([{Key="visibility";Value="hidden"}]@s.list)
    let s1 = Style ([{Key="visibility";Value="visible"}]@s.list)
    do
        html.taga ("path", [Atr("id",id);]@[s0.atr])
    member this.ID with get() = id
    member this.P (e:Arc) = 
        let t = num0(Var(Dt,"t",NaN))
        switchAnimationSeq <| fun () ->
            writein ("    var e = document.getElementById(\""+id+"\");")
            let a1 = Math.PI * e.angle1 t / 180
            let x1 = e.center.X t + e.radius t * asm.cos a1
            let y1 = e.center.Y t + e.radius t * asm.sin a1
            writein("    var x1 = " + x1.code+";")
            writein("    var y1 = " + (canvasY - y1).code+";")
            let a2 = Math.PI * e.angle2 t / 180 - 1E-4
            let x2 = e.center.X t + e.radius t * asm.cos a2
            let y2 = e.center.Y t + e.radius t * asm.sin a2
            writein("    var x2 = " + x2.code+";")
            writein("    var y2 = " + (canvasY - y2).code+";")
            writein("    var a1 = " + (e.angle1 t).code+";")
            writein("    var a2 = " + (e.angle2 t).code+";")
            writein("    var radiusX = " + (e.radius t).code + ";")
            writein("    var radiusY = " + (e.radius t).code + ";")
            writein "    var da = a2 - a1;"
            writein "    if(da < 0.0) {da = a2 + 360 - a1;}"
            writein "    var largerOrSmaller = 0;"
            writein "    if(da > 180.0) {largerOrSmaller = 1;}"
            writein("    d = \"M \" + x1 + \" \" + y1 + \" A \" + radiusX + \" \" + radiusY + \" 0 \" + largerOrSmaller + \" 0 \" + x2 + \" \" + y2 " + ";")
            writein("    e.setAttribute(\"style\"," + "\"" + s1.code0 + "\");")
            writein("    e.setAttribute(\"d\", " + "d" + ");")
        switchJSAnimationSeqReset <| fun () ->
            writein("    var e = document.getElementById(\""+id+"\");")
            writein("    e.setAttribute(\"style\"," + "\"" + s0.code0 + "\");")
            
type AnimationText(s:Style,originX:int,originY:int,canvasX:int,canvasY:int) =
    let id = nextContentsID()
    let ss = Style ([{Key="position";Value="absolute"}]@s.list)
    let ss0 = Style ([{Key="display";Value="none"}]@ss.list)
    let ss1 = Style ([{Key="display";Value="block"}]@ss.list)
    do
        html.tagb ("div", "id = \"" + id + "\" " + ss0.code) <| fun () -> ()
    member this.ID with get() = id
    member this.P (e:Text) = 
        let t = num0(Var(Dt,"t",NaN))
        switchAnimationSeq <| fun () ->
            writein ("    var e = document.getElementById(\""+id+"\");")
            writein ("    e.setAttribute(\"style\"," + "\"" + ss1.code0 + "\");")
            writein ("    e.innerHTML = \"" + e.str + "\";")
            writein ("    var x = " + (originX + e.center.X t).code+ ";")
            writein ("    var y = " + (originY + canvasY - e.center.Y t).code+ ";")
            writein  "    x = x - e.offsetWidth/2;"
            writein  "    y = y - e.offsetHeight/2;"
            writein ("    e.setAttribute(\"style\"," + "\"" + ss1.code0 + " margin-left: \"+String(x)+\"px; margin-top: \"+String(y)+\"px; \");")
        switchJSAnimationSeqReset <| fun () ->
            writein ("    var e = document.getElementById(\""+id+"\");")
            writein ("    e.setAttribute(\"style\"," + "\"" + ss0.code0 + "\");")
            
    member this.P (e:MathText) = 
        let t = num0(Var(Dt,"t",NaN))
        switchAnimationSeq <| fun () ->
            writein ("    var e = document.getElementById(\""+id+"\");")
            writein ("    e.setAttribute(\"style\"," + "\"" + ss1.code0 + "\");")
            writein ("    e.innerHTML = \"\\\\(" + e.eq.code + "\\\\)\";")
            writein  "    MathJax.typeset();"
            writein ("    var x =" + (originX + e.center.X t).code+ ";")
            writein ("    var y =" + (originY + canvasY - e.center.Y t).code+ ";")
            writein  "    x = x - e.offsetWidth/2;"
            writein  "    y = y - e.offsetHeight/2;"
            writein ("    e.setAttribute(\"style\"," + "\"" + ss1.code0 + " margin-left: \"+String(x)+\"px; margin-top: \"+String(y)+\"px; \");")
        switchJSAnimationSeqReset <| fun () ->
            writein ("    var e = document.getElementById(\""+id+"\");")
            writein ("    e.setAttribute(\"style\"," + "\"" + ss0.code0 + "\");")
            
type AnimationPolygon(s:Style,canvasX:int,canvasY:int) =
    let id = nextContentsID()
    let s0 = Style ([{Key="visibility";Value="hidden"}]@s.list)
    let s1 = Style ([{Key="visibility";Value="visible"}]@s.list)
    do
        html.taga ("polygon", [Atr("id", id);] @ [s.atr])
    member this.ID with get() = id
    member this.P (apex:list<tposition>) = 
        let t = num0(Var(Dt,"t",NaN))
        switchAnimationSeq <| fun () ->
            writein ("    var e = document.getElementById(\"" + id + "\");")
            writein  "    var p = \"\";"
            for p in apex do
                writein ("    var x = " + (p.X t).code + ";")
                writein ("    var y = " + (canvasY - p.Y t).code + ";")
                writein  "    p = p + String(x) + \",\" + String(y) + \" \";"
            writein ("    e.setAttribute(\"style\"," + "\"" + s1.code0 + "\");")
            writein "    e.setAttribute(\"points\", p);"
        switchJSAnimationSeqReset <| fun () ->
            writein ("    var e = document.getElementById(\""+id+"\");")
            writein ("    e.setAttribute(\"style\"," + "\"" + s0.code0 + "\");")
            
type SlideAnimation =
    static member writeAudioList() =
        switchJSMain <| fun () ->
            writein "const audioList = ["
            for i in 0..audioList.Length-1 do
                writein ("    \""+audioList[i] + "\"" + if i<audioList.Length-1 then "," else "")
            writein "];"
    static member jsSetCharacter() =
        switchJSMain <| fun () ->
            writein "let pagecount = 1;"
            writein"function setCharacter()"
            writein"{"
            writein"        const swc = document.getElementById(\"switchCharacter\");"
            writein"        const c = document.getElementById(\"c\"+pagecount);"
            writein"        if(swc.checked)"
            writein"        {"
            writein"            c.style.display = \"block\";"
            writein"        }"
            writein"        else"
            writein"        {"
            writein"            c.style.display = \"none\";"
            writein"        }"
            writein"}"
    static member jsSetSubtitle() =
        switchJSMain <| fun () ->
            writein "function setSubtitle()"
            writein "{"
            writein "        const sws = document.getElementById(\"switchSubtitle\");"
            writein "        const b2 = document.getElementById(\"sb\"+pagecount);"
            writein "        const s2 = document.getElementById(\"s\"+pagecount);"
            writein "        if(sws.checked)"
            writein "        {"
            writein "            b2.style.display = \"block\";"
            writein "            s2.style.display = \"block\";"
            writein "        }"
            writein "        else"
            writein "        {"
            writein "            b2.style.display = \"none\";"
            writein "            s2.style.display = \"none\";"
            writein "        }"
            writein "}"
    static member jsDrawNext() =
        switchJSMain <| fun () ->
            writein "function drawNext()"
            writein "{"
            writein "    resetAll();"
            writein("    if(pagecount<"+anicounter.ToString()+")")
            writein "    {"
            writein "        const swc = document.getElementById(\"switchCharacter\");"
            writein "        const sws = document.getElementById(\"switchSubtitle\");"
            writein "        const swa = document.getElementById(\"switchAudio\");"
            writein "        "
            writein "        const p1 = document.getElementById(\"p\"+pagecount);"
            writein "        p1.style.display = \"none\";"
            writein "        const b1 = document.getElementById(\"sb\"+pagecount);"
            writein "        b1.style.display = \"none\";"
            writein "        const s1 = document.getElementById(\"s\"+pagecount);"
            writein "        s1.style.display = \"none\";"
            writein "        const c1 = document.getElementById(\"c\"+pagecount);"
            writein "        c1.style.display = \"none\";"
            writein "        pagecount++;"
            writein "        const p2 = document.getElementById(\"p\"+pagecount);"
            writein "        p2.style.display = \"block\";"
            writein "        if(sws.checked)"
            writein "        {"
            writein "            const b2 = document.getElementById(\"sb\"+pagecount);"
            writein "            b2.style.display = \"block\";"
            writein "            const s2 = document.getElementById(\"s\"+pagecount);"
            writein "            s2.style.display = \"block\";"
            writein "        }"
            writein "        else"
            writein "        {"
            writein "            const b2 = document.getElementById(\"sb\"+pagecount);"
            writein "            b2.style.display = \"none\";"
            writein "            const s2 = document.getElementById(\"s\"+pagecount);"
            writein "            s2.style.display = \"none\";"
            writein "        }"
            writein "        if(swc.checked)"
            writein "        {"
            writein "            const c2 = document.getElementById(\"c\"+pagecount);"
            writein "            c2.style.display = \"block\";"
            writein "        }"
            writein "        else"
            writein "        {"
            writein "            const c2 = document.getElementById(\"c\"+pagecount);"
            writein "            c2.style.display = \"none\";"
            writein "        }"
            writein "        const audioPlayer = document.getElementById(\"audioPlayer\");"
            writein "        if(audioList[pagecount-1] != \"\" && swa.checked)"
            writein "        {"
            writein "            audioPlayer.src = audioList[pagecount-1];"
            writein "            audioPlayer.play();"
            writein "        }"
            writein "        autoAnimationMap['page'+pagecount]();"
            writein "    }"
            writein "}"
    static member jsDrawPrev() =
        switchJSMain <| fun () ->
            writein "function drawPrev()"
            writein "{"
            writein "    resetAll();"
            writein "    if(pagecount>1)"
            writein "    {"
            writein "        const swc = document.getElementById(\"switchCharacter\");"
            writein "        const sws = document.getElementById(\"switchSubtitle\");"
            writein "        const swa = document.getElementById(\"switchAudio\");"
            writein "        const p1 = document.getElementById(\"p\"+pagecount);"
            writein "        p1.style.display = \"none\";"
            writein "        const b1 = document.getElementById(\"sb\"+pagecount);"
            writein "        b1.style.display = \"none\";"
            writein "        const s1 = document.getElementById(\"s\"+pagecount);"
            writein "        s1.style.display = \"none\";"
            writein "        const c1 = document.getElementById(\"c\"+pagecount);"
            writein "        c1.style.display = \"none\";"
            writein "        pagecount--;"
            writein "        const p2 = document.getElementById(\"p\"+pagecount);"
            writein "        p2.style.display = \"block\";"
            writein "        if(sws.checked)"
            writein "        {"
            writein "            const b2 = document.getElementById(\"sb\"+pagecount);"
            writein "            b2.style.display = \"block\";"
            writein "            const s2 = document.getElementById(\"s\"+pagecount);"
            writein "            s2.style.display = \"block\";"
            writein "        }"
            writein "        else"
            writein "        {"
            writein "            const b2 = document.getElementById(\"sb\"+pagecount);"
            writein "            b2.style.display = \"none\";"
            writein "            const s2 = document.getElementById(\"s\"+pagecount);"
            writein "            s2.style.display = \"none\";"
            writein "        }"
            writein "        if(swc.checked)"
            writein "        {"
            writein "            const c2 = document.getElementById(\"c\"+pagecount);"
            writein "            c2.style.display = \"block\";"
            writein "        }"
            writein "        else"
            writein "        {"
            writein "            const c2 = document.getElementById(\"c\"+pagecount);"
            writein "            c2.style.display = \"none\";"
            writein "        }"
            writein "        const audioPlayer = document.getElementById(\"audioPlayer\");"
            writein "        if(audioList[pagecount-1] != \"\" && swa.checked)"
            writein "        {"
            writein "            audioPlayer.src = audioList[pagecount-1];"
            writein "            audioPlayer.play();"
            writein "        }"
            writein "    }"
            writein "}"
            
[<AutoOpen>]
module movieSetting =
    /// キャラクターのデフォルト表示・非表示設定
    let mutable character = true
    /// 字幕のデフォルト表示・非表示設定
    let mutable subtitle = true
    /// 音声のデフォルト表示・非表示設定
    let mutable voice = true
    
    let setDefault(s:MovieSetting) = 
        match s.Character with |ON -> character <- true |OFF -> character <- false
        match s.Subtitle  with |ON -> subtitle  <- true |OFF -> subtitle  <- false
        match s.Voice     with |ON -> voice     <- true |OFF -> voice     <- false
        
[<AutoOpen>]
module htmlexpr =
    type html with
        static member htmlfile (dir:string,filename:string) code =
            makeProgram [dir,filename,HTML] <| fun () ->
                code()
                programList[prIndex].close()
        /// 内部要素のないタグ
        static member taga (t:string,lst:list<string*PHPdata>) =
            writein("<"+t+" ")
            programList[prIndex].indentInc()
            for a,s in lst do
                writein(a + " = <?php echo \"\\\"\"." + s.code + " . \"\\\"\"; ?>")
            programList[prIndex].indentDec()
            writein " />"
        /// 内部要素のあるタグ
        static member tagb0 (t:string,lst:list<string*PHPdata>) = fun code ->
            if lst.Length=0 then
                write("<"+t+">")
            else
                writen("<"+t+" ")
                programList[prIndex].indentInc()
                for a,s in lst do
                    writen(a + " = <?php echo \"\\\"\"." + s.code + " . \"\\\"\"; ?>")
                programList[prIndex].indentDec()
                write ">"
            code()
            writen ("</"+t+">")
        /// 内部要素のあるタグ
        static member tagb (t:string,lst:list<string*PHPdata>) = fun code ->
            if lst.Length=0 then
                writein("<"+t+">")
            else
                writein("<"+t+" ")
                programList[prIndex].indentInc()
                for a,s in lst do
                    writein(a + " = <?php echo \"\\\"\"." + s.code + " . \"\\\"\"; ?>")
                programList[prIndex].indentDec()
                writein ">"
            code()
            writein ("</"+t+">")
            
        static member h1 (t:num0) = fun code ->
            html.tagb "h1" <| fun () -> php.echo t.code
            code()
        static member h1 (t:num0,atr:Style) = fun code ->
            html.tagb ("h1",atr) <| fun () -> php.echo t.code
            code()
            
        static member h2 (t:num0) = fun code ->
            html.tagb "h2" <| fun () -> php.echo t.code
            code()
        static member h2 (t:num0,atr:Style) = fun code ->
            html.tagb ("h2",atr) <| fun () -> php.echo t.code
            code()
            
        static member h3 (t:num0) = fun code ->
            html.tagb "h3" <| fun () -> php.echo t.code
            code()
        static member h3 (t:num0,atr:Style) = fun code ->
            html.tagb ("h3",atr) <| fun () -> php.echo t.code
            code()
            
        static member h4 (t:num0) = fun code ->
            html.tagb "h4" <| fun () -> php.echo t.code
            code()
        static member h4 (t:num0,atr:Style) = fun code ->
            html.tagb ("h4",atr) <| fun () -> php.echo t.code
            code()

        static member h5 (t:num0) = fun code ->
            html.tagb "h5" <| fun () -> php.echo t.code
            code()
        static member h5 (t:num0,atr:Style) = fun code ->
            html.tagb ("h5",atr) <| fun () -> php.echo t.code
            code()
            
        static member submit(name:string,value:PHPdata) = html.taga("input",["type","\"submit\""; "name","\""+name+"\""; "value",(value.code)])
        static member submit(name:PHPdata,value:string) = html.taga("input",["type",PHPdata "submit"; "name", name; "value",PHPdata value])
        static member submit(name:string,value:string) = html.taga("input",["type","\"submit\""; "name","\""+name+"\""; "value","\""+value+"\""])
        static member submit(url:string,name:PHPdata,value:string) = html.taga("input",["type",PHPdata "submit"; "name", name; "value",PHPdata value; "formaction",PHPdata url])
        static member submit_disabled(name:PHPdata,value:PHPdata) = html.taga("input",["type",PHPdata "submit"; "name", name; "value",value; "disabled",PHPdata "disabled"])
        static member submit_disabled(name:string,value:PHPdata) = html.taga("input",["type",PHPdata "submit"; "name",PHPdata name; "value",value; "disabled",PHPdata "disabled"])
        static member submit_disabled(name:PHPdata,value:string) = html.taga("input",["type",PHPdata "submit"; "name", name; "value",PHPdata value; "disabled",PHPdata "disabled"])
        static member item (a:list<string*PHPdata>) = fun code -> html.tagb ("li",a) code
        static member link(url:PHPdata) = fun code -> html.tagb ("a",["href",url]) code
        static member link(url:PHPdata, s:Style) = fun code -> html.tagb ("a",[s.atr; Atr("href","\""+url.code+"\"")]) code
        static member select(x:PHPdata) = fun code -> html.tagb ("select",["name",x;]) code
        static member select_disabled(x:PHPdata) = fun code -> html.tagb ("select",["name",x; "disabled",PHPdata "disabled"]) code
        static member splitTag t code = 
            let b (lst:list<string*PHPdata>) =
                if lst.Length=0 then
                    writein ("<"+t+">")
                else
                    writein("<"+t+" ")
                    for a,s in lst do
                        writein(a + "=" + s.code + " ")
                    writein ">"
            code b 
            writein ("</"+t+">")
        static member Select = html.splitTag "select" 
        static member Tr = html.splitTag "tr" 
        static member div (a:list<string*PHPdata>) = fun code -> html.tagb ("div",a) code
        static member div (a:CSSdata) = fun code -> 
            match a.label with
            |HTMLTag s -> html.tagb s code
            |CSSClass s -> html.tagb ("div",["class",s]) code
            |CSSID s -> html.tagb ("div",["id",s]) code
            |_ -> ()
        static member div (a:CSSdata,atr:list<string*string>) = fun code -> 
            match a.label with
            |HTMLTag s -> html.tagb s code
            |CSSClass s -> html.tagb ("div",["class",s]@atr) code
            |CSSID s -> html.tagb ("div",["id",s]@atr) code
            |_ -> ()
            
        static member article (a:CSSdata) = fun code -> 
            match a.label with
            |CSSClass s -> html.tagb ("article",["class",s]) code
            |CSSID s -> html.tagb ("article",["id",s]) code
            |_ -> ()
            
        static member aside (a:CSSdata) = fun code -> 
            match a.label with
            |CSSClass s -> html.tagb ("aside",["class",s]) code
            |CSSID s -> html.tagb ("aside",["id",s]) code
            |_ -> ()
            
        static member para (a:CSSdata) = fun code -> 
            match a.label with
            |CSSClass s -> html.tagb ("p",["class",s]) code
            |CSSID s -> html.tagb ("p",["id",s]) code
            |_ -> ()
            
        static member section (a:CSSdata) = fun code -> 
            match a.label with
            |CSSClass s -> html.tagb ("section",["class",s]) code
            |CSSID s -> html.tagb ("section",["id",s]) code
            |_ -> ()
            
        static member span (a:CSSdata) = fun code -> 
            match a.label with
            |CSSClass s -> html.tagb0 ("span",["class",s]) code
            |CSSID s -> html.tagb0 ("span",["id",s]) code
            |_ -> ()
            
        /// チェックボックス（チェックされたとき1、チェックされていないとき0を送信）
        static member checkbox(name:PHPdata) = 
            html.taga("input",["type",PHPdata "hidden"; "name", name; "value",PHPdata "0";])
            html.taga("input",["type",PHPdata "checkbox"; "name", name; "value",PHPdata "1";])
        /// チェックボックス（チェックされたとき1、チェックされていないとき0を送信）
        static member checkbox_disabled(name:PHPdata) =
            html.taga("input",["type",PHPdata "hidden"; "name", name; "value",PHPdata "0";])
            html.taga("input",["type",PHPdata "checkbox"; "name", name; "value",PHPdata "1"; "disabled",PHPdata "disabled"])
        /// チェックボックス（チェックされたとき1、チェックされていないとき0を送信）
        static member checkbox_checked(name:PHPdata) = 
            html.taga("input",["type",PHPdata "hidden"; "name", name; "value",PHPdata "0";])
            html.taga("input",["type",PHPdata "checkbox"; "name", name; "value",PHPdata "1"; "checked",PHPdata "checked";])
        /// チェックボックス（チェックされたとき1、チェックされていないとき0を送信）
        static member checkbox_checked_disabled(name:PHPdata) =
            html.taga("input",["type",PHPdata "hidden"; "name", name; "value",PHPdata "0";])
            html.taga("input",["type",PHPdata "checkbox"; "name", name; "value",PHPdata "1"; "checked",PHPdata "checked"; "disabled",PHPdata "disabled"])
        static member Mathtext (s:Style) (p:position) (text:PHPdata) =
            let s1 = Style [{Key = "margin-left"; Value=p.x.ToString()+"px"}
                            {Key = "margin-top"; Value=p.y.ToString()+"px"}
                            {Key = "position"; Value = "absolute";}]
            html.tagb ("div", s1+s) <| fun () ->
                writein ("\\(" + text.code + "\\)")
        static member image (s:Style,p:position) = fun (filename:string) ->
            let f = Path.GetFileName filename
            if File.Exists filename then
                if Directory.Exists contentsDir then
                    File.Copy(filename, contentsDir + "\\" + f, true)
                else
                    printfn "directory not exist: %s" contentsDir
            else
                printfn "image file not exist: %s" filename
            let st = Style [{Key="margin-left"; Value=p.x.ToString()+"px"}; {Key="margin-top"; Value=p.y.ToString()+"px"}] + s
            html.taga ("img", [st.atr;Atr("src",contentsDir + "\\" + f)])
        static member image (s:Style, id:string) = fun (filename:string) ->
            let f = Path.GetFileName filename
            if File.Exists filename then
                if Directory.Exists contentsDir then
                    File.Copy(filename, contentsDir + "\\" + f, true)
                else
                    printfn "directory not exist: %s" contentsDir
            else
                printfn "image file not exist: %s" filename
            html.taga ("img", [Atr("id",id); s.atr;Atr("src",contentsDir + "\\" + f)])
        static member image (s:Style) = fun (filename:string) ->
            let f = Path.GetFileName filename
            if File.Exists filename then
                if Directory.Exists contentsDir then
                    File.Copy(filename, contentsDir + "\\" + f, true)
                else
                    printfn "directory not exist: %s" contentsDir
            else
                printfn "image file not exist: %s" filename
            html.taga ("img", [s.atr;Atr("src",contentsDir + "\\" + f)])
        static member image (filename:string) =
            let f = Path.GetFileName filename
            if File.Exists filename then
                if Directory.Exists contentsDir then
                    File.Copy(filename, contentsDir + "\\" + f, true)
                else
                    printfn "directory not exist: %s" contentsDir
            else
                printfn "image file not exist: %s" filename
            html.taga ("img", [Atr("src",contentsDir + "\\" + f)])
        static member video (s:Style,p:position) = fun (filename:string) ->
            let f = Path.GetFileName filename
            if File.Exists filename then
                if Directory.Exists contentsDir then
                    File.Copy(filename, contentsDir + "\\" + f, true)
                else
                    printfn "directory not exist: %s" contentsDir
            else
                printfn "video file not exist: %s" filename
            let st = Style [{Key="margin-left"; Value=p.x.ToString()+"px"}; {Key="margin-top"; Value=p.y.ToString()+"px"}] + s
            html.tagv ("video", [st.atr;Atr("src", contentsDir + "\\" + f); Atr("controls", "")])
            html.tage "video"
        static member video (s:Style) = fun (filename:string) ->
            let f = Path.GetFileName filename
            if File.Exists filename then
                if Directory.Exists contentsDir then
                    File.Copy(filename, contentsDir + "\\" + f, true)
                else
                    printfn "directory not exist: %s" contentsDir
            else
                printfn "video file not exist: %s" filename
            html.tagv ("video", [s.atr;Atr("src", contentsDir + "\\" + f); Atr("controls", "")])
            html.tage "video"

        static member code (style:list<string*PHPdata>) = fun cd ->
            html.tagb0 ("pre",style) <| fun () ->
                html.tagb0 ("code",[]) <| fun () ->
                    cd()

        static member code (style:list<string*PHPdata>, cd:PHPdata) =
            html.tagb0 ("pre",style) <| fun () ->
                html.tagb0 ("code",[]) <| fun () ->
                    write cd.phpcode
                    
        static member code (style:list<string*string>) = html.code (style |> List.map (fun (a,b) -> a,PHPdata b))
        
        static member code (style:list<string*string>, cd:PHPdata) = html.code (style |> List.map (fun (a,b) -> a,PHPdata b),cd)
        
        static member code (cd:PHPdata) = html.code ([],cd)
        
        static member listTable (caption:string) = fun (borderH:list<BorderH>) (borderV:list<BorderV>) (tlist:list<list<string>>) ->
            html.tagb("div",["class","\"fig\""]) <| fun () ->
                html.tagb ("span",["class","\"caption\""]) <| fun () ->
                    writein(caption)
                html.tagb("table",["class","\"tab\""]) <| fun () ->
                    for j in 0..tlist.Length-1 do
                        html.tagb ("tr",["class",match borderV[j] with |TrTB -> "\"trtb\"" |TrT -> "\"trt\"" |TrB -> "\"trb\"" |TrN -> "\"trn\""]) <| fun () ->
                            for i in 0..tlist[j].Length-1 do
                                html.tagb ("td",["class",
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
                                    |TdJLR -> "\"tdjLR\""]) <| fun () ->
                                    writein <| tlist[j][i]
        static member eq(text:num0) =
            writein ("\\("+text.Expr.evalL programList[prIndex] + "\\)")
            
        static member eqB (s:Style) = fun (p:position) (size:int) (color:string) (text:num0) ->
            let s1 = Style [{Key = "margin-left"; Value = p.x.ToString()+"px";}
                            {Key = "margin-top"; Value = p.y.ToString()+"px";}
                            {Key = "position"; Value = "absolute";}
                            {Key = "font-size"; Value = size.ToString()+"px";}
                            {Key = "color"; Value = color.ToString();}]
            html.tagb ("div", s1+s) <| fun () ->
                writein ("\\("+text.Expr.evalL programList[prIndex]+"\\)")
                
        static member eqD (s:Style) = fun (p:position) (size:int) (color:string) (text:list<num0>) ->
            let s1 = Style [{Key = "margin-left"; Value = p.x.ToString()+"px";}
                            {Key = "margin-top"; Value = p.y.ToString()+"px";}
                            {Key = "position"; Value = "absolute";}
                            {Key = "font-size"; Value = size.ToString()+"px";}
                            {Key = "color"; Value = color.ToString();}]
            html.tagb ("div", s1+s) <| fun () ->
                writein ("\\(\\begin{align}"+String.Join("\\\\",text |> List.map(fun t -> t.Expr.evalL programList[prIndex]))+"\\end{align}\\)")
                
        /// キャラクター付き解説ページ
        static member page (c:list<Character*string>) (audio:Audio) code2 =
            html.slide position.Origin <| fun p ->
                // 音声ファイル追加
                match audio.Source with
                |Silent ->
                    audioList <- audioList@[""]
                |AudioFile f ->
                    audioList <- audioList@[DirectoryInfo(contentsDir).Name+"/"+Path.GetFileName f]
                    if File.Exists f then
                        if Directory.Exists contentsDir then
                            File.Copy(f, contentsDir+"\\"+Path.GetFileName f, true)
                        else
                            printfn "directory not exist: %s" contentsDir
                    else
                        printfn "audio file not exist: %s" f
                // 指定されたファイルが存在し、それらが現在書き込み中のファイルでない場合
                |AudioDir f when File.Exists f ->
                    let audiodir = Path.GetDirectoryName f
                    let scriptfile = Path.GetFileNameWithoutExtension f
                    let scripts = File.ReadAllLines f
                    // 目的の台詞が何行目に書かれているかで音声ファイル番号を特定
                    let n = Array.tryFindIndex (fun s -> s=audio.Subtitle.Hatsuon) scripts
                    match n with
                    |None ->
                        // 指定した台詞がファイルに書かれていない場合
                        printfn "Append script \"%s\" to %s" audio.Subtitle.Hatsuon f
                        // 台詞テキストファイルに追記
                        let wr = new StreamWriter(f,true)
                        wr.Write audio.Subtitle.Hatsuon
                        wr.Close()
                        audioList <- audioList@[""]
                    |Some m ->
                        //ファイル番号先頭数字はゼロ埋め
                        let rec find (p:string) =
                            let f = audiodir+"\\"+m.ToString(p)+"-"+scriptfile+".wav"
                            if p="0000" then
                                // ファイル数が4桁以上はあり得ない→音声ファイルがないと判断
                                printfn "Error: audio file not exist: %s" <| audiodir+"\\"+m.ToString()+"-"+scriptfile+".wav"
                            elif File.Exists f then
                                // ファイルが見つかった場合の処理
                                audioList <- audioList@[DirectoryInfo(contentsDir).Name+"/"+Path.GetFileName f]
                                File.Copy(f, contentsDir+"\\"+Path.GetFileName f, true)
                                printfn "Copy: %s -> %s" f <| contentsDir+"\\"+Path.GetFileName f
                            else
                                // ファイルが見つからなかった場合は音声ファイルの番号桁数を上げる
                                find (p+"0")
                        find "0"
                // 指定されたファイルが存在しない場合
                |AudioDir f ->
                    // 台詞テキストファイルに書き込み
                    let wr = new StreamWriter(f)
                    wr.Write audio.Subtitle.Hatsuon
                    wr.Close()
                    audioList <- audioList@[""]
                    
                // メインコンテンツ
                // html.tag "div" "style=\"width: 1920px; height: 880px; position: absolute; z-index: 0;\"" <| fun () ->
                //     code2 p
                // 字幕枠
                html.tag "div" ("id = \"sb"+anicounter.ToString()+"\" style=\"width: 1880px; height: 160px; " + (if subtitle then "display: block; " else "display: none; ") + "position: absolute; z-index: 1; margin-top: 880px; padding: 20px; background-color: #aaaaff; font-family: 'Noto Sans JP'; font-size: 36pt; font-weight: 800; text-shadow: 0 1px 0 #fff, 1px 0 0 #fff, 0 -1px 0 #fff, -1px 0 0 #fff, -1px -1px 0 #fff, 1px -1px 0 #fff, -1px 1px 0 #fff, 1px 1px 0 #fff \";") <| fun () ->
                    ()
                // キャラクター画像
                html.tag "div" ("id = \"c"+anicounter.ToString()+"\"" + "style=\"" + (if character then "display: block; " else "display: none; ") + "\"") <| fun () ->
                    for c,file in c do
                        if File.Exists file then
                            if Directory.Exists contentsDir then
                                match c with
                                |Tale ->
                                    File.Copy(file, contentsDir+"\\"+Path.GetFileName file, true)
                                    html.tag_ "img" <| "src=\""+contentsDir+"/"+Path.GetFileName file+"\" style=\"position: absolute; margin-left: 0px; margin-top: -122px; width: 850px; object-position: right 204px top 426px; z-index: 2;\""
                                |Dang ->
                                    File.Copy(file, contentsDir+"\\"+Path.GetFileName file, true)
                                    html.tag_ "img" <| "src=\""+contentsDir+"/"+Path.GetFileName file+"\" style=\"position: absolute; margin-left: 1462px; margin-top: 727px; width: 450px; z-index: 3;\""
                                |Armi ->
                                    File.Copy(file, contentsDir+"\\"+Path.GetFileName file, true)
                                    html.tag_ "img" <| "src=\""+contentsDir+"/"+Path.GetFileName file+"\" style=\"position: absolute; margin-left: 1503px; margin-top: 638px; width: 360px; z-index: 4;\""
                            else
                                printfn "directory not exist: %s" contentsDir
                        else
                            printfn "character image file not exist: %s" file
                // 字幕
                match audio.Speaker with
                |Character.Tale -> 
                    html.tag "div" ("id = \"s"+anicounter.ToString()+"\" style=\"width: 1880px; height: 160px; " + (if subtitle then "display: block; " else "display: none; ") + "position: absolute; z-index: 5; margin-top: 880px; padding: 20px; font-family: 'Noto Sans JP'; color: #d11aff; font-size: 36pt; font-weight: 800; text-shadow: 0 1px 0 #fff, 1px 0 0 #fff, 0 -1px 0 #fff, -1px 0 0 #fff, -1px -1px 0 #fff, 1px -1px 0 #fff, -1px 1px 0 #fff, 1px 1px 0 #fff ;\"")
                        <| fun () -> writein audio.Subtitle.Subtitle
                |Character.Dang -> 
                    html.tag "div" ("id = \"s"+anicounter.ToString()+"\" style=\"width: 1880px; height: 160px; " + (if subtitle then "display: block; " else "display: none; ") + "position: absolute; z-index: 5; margin-top: 880px; padding: 20px; font-family: 'Noto Sans JP'; color: #455eff; font-size: 36pt; font-weight: 800; text-shadow: 0 1px 0 #fff, 1px 0 0 #fff, 0 -1px 0 #fff, -1px 0 0 #fff, -1px -1px 0 #fff, 1px -1px 0 #fff, -1px 1px 0 #fff, 1px 1px 0 #fff ;\"")
                        <| fun () -> writein audio.Subtitle.Subtitle
                |Character.Armi -> 
                    html.tag "div" ("id = \"s"+anicounter.ToString()+"\" style=\"width: 1880px; height: 160px; " + (if subtitle then "display: block; " else "display: none; ") + "position: absolute; z-index: 5; margin-top: 880px; padding: 20px; font-family: 'Noto Sans JP'; color: #ff8800; font-size: 36pt; font-weight: 800; text-shadow: 0 1px 0 #fff, 1px 0 0 #fff, 0 -1px 0 #fff, -1px 0 0 #fff, -1px -1px 0 #fff, 1px -1px 0 #fff, -1px 1px 0 #fff, 1px 1px 0 #fff ;\"")
                        <| fun () -> writein audio.Subtitle.Subtitle
                switchAutoAnimation <| fun () ->
                    writein("page"+anicounter.ToString()+": () => {")
                // メインコンテンツ
                html.tag "div" "style=\"width: 1920px; height: 880px; position: absolute; z-index: 0;\"" <| fun () ->
                    code2 p
                switchAutoAnimation <| fun () ->
                    writein "},"
                if animationButtonList.Length > 0 then
                    let fStartName,fResetName,btnx,btny = animationButtonList[animationButtonList.Length-1]
                    html.startButton2 ("startButton"+fStartName) (Style[position.position "absolute"; margin.left (btnx.ToString()+"px"); margin.top (btny.ToString()+"px"); position.index 1000;]) ("animationStartMap['"+fStartName+"']()")
                    html.resetButton2 ("resetButton"+fStartName) (Style[position.position "absolute"; margin.left (btnx.ToString()+"px"); margin.top ((btny+25).ToString()+"px"); position.index 1000;]) ("animationResetMap['"+fResetName+"']()")
                animationButtonList <- []
                
        static member slide (p:position)  code =
                anicounter <- anicounter + 1
                html.tagb ("div", "id=\"p"+anicounter.ToString()+"\" style=\"display: "+(if anicounter=1 then "block" else "none")+"; position: absolute;\"") <| fun wr ->
                    code p
        static member prevButton() =
                html.tagb ("button", "id=\"prevButton\" style=\"position: absolute; z-index: 100;\" onclick=\"drawPrev()\"") <| fun () ->
                    writein "前へ"
        static member nextButton() =
                html.tagb ("button", "id=\"nextButton\" style=\"position: absolute; margin-left: 75px; z-index: 100;\" onclick=\"drawNext()\"") <| fun () ->
                    writein "次へ"
        static member startButton2(id:string) (s:Style) (c:string) =
                html.tagb ("button", [Atr("id",id); Atr("onclick",c)]@[s.atr]) <| fun () ->
                    writein "Start"
        static member resetButton2(id:string) (s:Style) (c:string) =
                html.tagb ("button", [Atr("id",id); Atr("onclick",c)]@[s.atr]) <| fun () ->
                    writein "Reset"
        static member switchCharacter() =
            html.taga ("input", "type=\"checkbox\" id=\"switchCharacter\" style=\"position: absolute; margin-top: 6px; margin-left: 150px; z-index: 100;\"  onclick=\"setCharacter()\" " + if character then "checked" else "")
            html.tagb ("label", "style=\"position: absolute; margin-top: 0px; margin-left: 165px; z-index: 100;\"") <| fun () ->
                writein "キャラクター"
        static member switchSubtitle() =
            html.taga ("input", "type=\"checkbox\" id=\"switchSubtitle\" style=\"position: absolute; margin-top: 6px; margin-left: 270px; z-index: 100;\" onclick=\"setSubtitle()\" " + if subtitle then "checked" else "")
            html.tagb ("label", "style=\"position: absolute; margin-top: 0px; margin-left: 285px; z-index: 100;\"") <| fun () ->
                writein "字幕"
        static member switchAudio() =
            html.taga ("input", "type=\"checkbox\" id=\"switchAudio\" style=\"position: absolute; margin-top: 6px; margin-left: 330px; z-index: 100;\" onclick=\"setSubtitle()\" " + if voice then "checked" else "")
            html.tagb ("label", "style=\"position: absolute; margin-top: 0px; margin-left: 345px; z-index: 100;\"") <| fun () ->
                writein "音声"
        static member audioPlayer() =
                html.tagb ("audio", "id=\"audioPlayer\"")  <| fun () -> ()
        static member imageA (s:Style) = fun (p:position) (filename:string) ->
            let s1 = Style [{Key = "margin-left"; Value = p.x.ToString()+"px";}
                            {Key = "margin-top"; Value = p.y.ToString()+"px";}
                            {Key = "position"; Value = "absolute";}]
            let f = Path.GetFileName filename
            if File.Exists filename then
                if Directory.Exists contentsDir then
                    File.Copy(filename, contentsDir + "\\" + f, true)
                else
                    printfn "directory not exist: %s" contentsDir
            else
                printfn "image file not exist: %s" filename
            html.taga ("img", s1+s)
        static member blockText (s:Style) (p:position) (width:float,height:float) (text:list<string>) =
            let padding = 5
            let s1 = Style [size.width (width.ToString()+"px")
                            size.height (height.ToString()+"px")
                            {Key = "margin-left"; Value = p.x.ToString()+"px";}
                            {Key = "margin-top"; Value = p.y.ToString()+"px";}
                            {Key = "position"; Value = "absolute";}
                            {Key = "overflow-wrap"; Value = "break-word";}]
            html.tagb ("div", s1+s) <| fun () ->
                text |> List.iter (fun s -> writein (s+"<br>"))
                writein("\r\n")
            {Left = p.x;
            Right = p.x+double width+2.0*double padding;
            Top = p.y;
            Bottom = p.y+double height+2.0*double padding;}
            
type FigureAnimation(figcounter:int,originX:int,originY:int,canvasX:int,canvasY:int) =
    let padding = 10.0
    let mutable animeFlow:list<string*string*AnimationSetting*bool> = []
    let mutable counter = 0
    member _.Padding with get() = padding
    member _.id with get() = "fa"+figcounter.ToString()+"_"+counter.ToString()
    
    member this.seq (setting:AnimationSetting) (setFigure:AnimationSetting->unit) =
        // アニメーションシーケンスIDを発行
        let idstart,idreset = nextAnimationSeqID()
        switchAnimationSeq <| fun () ->
            writein ("function "+idstart+"(t){")
        switchJSAnimationSeqReset <| fun () ->
            writein ("function "+idreset+"(){")
        setFigure setting
        switchAnimationSeq <| fun () ->
            writein "}"
        switchJSAnimationSeqReset <| fun () ->
            writein "}"
        animeFlow <- animeFlow@[idstart,idreset,setting,false]
    member this.loop (setting:AnimationSetting) (setFigure:AnimationSetting->unit) =
        // アニメーションシーケンスIDを発行
        let idstart,idreset = nextAnimationSeqID()
        switchAnimationSeq <| fun () ->
            writein ("function "+idstart+"(t){")
        switchJSAnimationSeqReset <| fun () ->
            writein ("function "+idreset+"(){")
        setFigure setting
        switchAnimationSeq <| fun () ->
            writein "}"
        switchJSAnimationSeqReset <| fun () ->
            writein "}"
        animeFlow <- animeFlow@[idstart,idreset,setting,true]
        
    member this.animationEllipse s = AnimationEllipse(s,canvasX,canvasY)
    member this.animationLine s = AnimationLine(s,canvasX,canvasY)
    member this.animationArc s = AnimationArc(s,canvasX,canvasY)
    member this.animationText s = AnimationText(s,originX,originY,canvasX,canvasY)
    member this.animationPolygon s = AnimationPolygon(s,canvasX,canvasY)
    member this.line (s:Style) (startP:position) (endP:position) =
        let c = [
            Atr("x1",startP.x.ToString())
            Atr("y1",(double canvasY-startP.y).ToString())
            Atr("x2",endP.x.ToString())
            Atr("y2",(double canvasY-endP.y).ToString())]
        html.taga ("line", [s.atr]@c)
    member this.ellipse (s:Style) (center:position) (radiusX:float,radiusY:float) =
        let c = [
            Atr("cx",center.x.ToString())
            Atr("cy",(double canvasY-center.y).ToString())
            Atr("rx",radiusX.ToString())
            Atr("ry",radiusY.ToString())]
        html.taga ("ellipse", [s.atr]@c)
    member this.circle (s:Style) (center:position) (radius:float) =
        this.ellipse s center (radius,radius)
    member this.ellipseArc (s:Style) (center:position) (radiusX:float,radiusY:float) (theta1:float,theta2:float) =
        let x1 = center.x + radiusX * cos theta1
        let y1 = center.y + radiusY * sin theta1
        let x2 = center.x + radiusX * cos theta2
        let y2 = center.y + radiusY * sin theta2
        let d = 
            if theta2-theta1 < Math.PI then
                "M " + x1.ToString() + " " + (float canvasY-y1).ToString() + " A " + radiusX.ToString() + " " + radiusY.ToString() + " 0 0 0 " + x2.ToString() + " " + (float canvasY-y2).ToString()
            else
                "M " + x1.ToString() + " " + (float canvasY-y1).ToString() + " A " + radiusX.ToString() + " " + radiusY.ToString() + " 0 1 0 " + x2.ToString() + " " + (float canvasY-y2).ToString()
        html.taga ("path", [s.atr]@[Atr("d",d)])
    member this.polygon (s:Style) (apex:list<position>) =
        let pp =
            apex
            |> List.map (fun p -> p.x.ToString() + "," + (double canvasY-p.y).ToString())
            |> fun s -> String.Join(",",s)
        html.taga ("polygon", [s.atr]@[Atr("points",pp)])
    member this.polyline (s:Style) (apex:list<position>) =
        let pp =
            apex
            |> List.map (fun p -> p.x.ToString() + "," + (double canvasY-p.y).ToString())
            |> fun s -> String.Join(",",s)
        html.taga ("polyline", [s.atr]@[Atr("points",pp)])
    member this.rect (s:Style) (center:position) (sx:float,sy:float) =
        let c = [
            Atr("x",(center.x-0.5*sx).ToString())
            Atr("y",(double canvasY-center.y-0.5*sy).ToString())
            Atr("width",sx.ToString())
            Atr("height",sy.ToString())]
        html.taga ("rect", [s.atr]@c)
    member this.text (s:Style) (center:position) (str:string) =
        let c = [
            {Key="display";Value="block"}
            {Key="position";Value="absolute"}
            {Key="margin-left";Value=(double originX+center.x).ToString()+"px"}
            {Key="margin-top";Value=(double originY+double canvasY-center.y).ToString()+"px"}]
        let ss = Style (s.list@c)
        html.tagb ("div", ss.code) <| fun () ->
            writein str
    member this.eq (s:Style) (center:position) (e:num0) =
        let c = [
            {Key="display";Value="block"}
            {Key="position";Value="absolute"}
            {Key="margin-left";Value=(double originX+center.x).ToString()+"px"}
            {Key="margin-top";Value=(double originY+double canvasY-center.y).ToString()+"px"}]
        let ss = Style (s.list@c)
        html.tagb ("div", ss.code) <| fun () ->
            writein ("\\(" + e.Expr.evalH programList[prIndex] + "\\)")
    member this.image (s:Style) (center:position) (filename:string) =
        let f = Path.GetFileName filename
        File.Copy(filename, contentsDir + "\\" + f, true)
        let c = [
            {Key="display";Value="block"}
            {Key="position";Value="absolute"}
            {Key="margin-left";Value=(double originX+center.x).ToString()+"px"}
            {Key="margin-top";Value=(double originY+double canvasY-center.y).ToString()+"px"}]
        let ss = Style (s.list@c)
        html.taga ("img", [Atr ss.code; Atr("src",contentsDir + "\\" + f)])
    member this.jsStartControll(buttonIndex:string) =
        let fname = "start" + buttonIndex
        switchJSAnimationStart <| fun () ->
            writein(fname+": () => {")
            for idstart,_,setting,isLoop in animeFlow do
                if isLoop then
                    writein("    repeat(" + idstart + ", " + setting.FrameTime.ToString() + ", " + setting.FrameNumber.ToString() + ");")
                else
                    writein("    repeatSeq(" + idstart + ", " + setting.FrameTime.ToString() + ", " + setting.FrameNumber.ToString() + ", () => {")
            for _,_,_,isLoop in animeFlow do
                if isLoop then
                    ()
                else
                    writein "    });"
            writein "},"
        fname
    member this.jsResetControll(buttonIndex:string) =
        let fname = "reset" + buttonIndex
        switchJSAnimationReset <| fun () ->
            writein(fname+": () => {")
            for _,idreset,_,_ in animeFlow do
                writein("    " + idreset + "();")
            writein "},"
        fname
    static member jsAnimation codejs =
        switchBody <| fun () ->
            writein "var t = 0;"
            writein "var dt = 1;"
            writein "window.onload=function(){"
            writein "    var timer;"
            writein "    var delay = 33;"
            writein "    var loop = function(){"
            writein "        t = t + dt;"
            writein "        if(t >= 100){t = 0;}"
            writein "        clearTimeout(timer);"
            writein "        timer=setTimeout(loop,delay);"
            writein "    }"
            writein "    loop();"
            writein "}"
            writein codejs
            
[<AutoOpen>]
module dochtml =
    let htmlpresentation (dir:string) (filename:string) (title:string) (cssfile:option<string>) (pagesizeX:option<int>,pagesizeY:option<int>) isPageAnimation code =
        // ディレクトリ作成
        if not <| Directory.Exists (dir + "\\" + "contents_" + filename) then
            ignore <| Directory.CreateDirectory(dir + "\\" + "contents_" + filename)
        // コンテンツディレクトリ
        contentsDir <- dir + "\\" + "contents_" + filename
        makeProgram
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
            <| fun () ->
                switchJSAnimationStart <| fun () ->
                    writein "const animationStartMap = {"
                switchJSAnimationReset <| fun () ->
                    writein "const animationResetMap = {"
                switchAutoAnimation <| fun () ->
                    writein "const autoAnimationMap = {"
                switchAnimationSeq <| fun () ->
                    writein "function repeatSeq(fn, interval, Nt, onComplete)"
                    writein "{"
                    writein "    let t = 0;"
                    writein "    function run()"
                    writein "    {"
                    writein "        if (t < Nt)"
                    writein "        {"
                    writein "            fn(t);"
                    writein "            t++;"
                    writein "            setTimeout(run, interval);"
                    writein "        }"
                    writein "        else"
                    writein "        {"
                    writein "            onComplete();"
                    writein "        }"
                    writein "    }"
                    writein "    run();"
                    writein "}"
                    writein "function repeat(fn, interval, Nt)"
                    writein "{"
                    writein "    let t = 0;"
                    writein "    function run()"
                    writein "    {"
                    writein "        if(t == Nt)"
                    writein "        {"
                    writein "            t = 0;"
                    writein "        }"
                    writein "        fn(t);"
                    writein "        t++;"
                    writein "        setTimeout(run, interval);"
                    writein "    }"
                    writein "    run();"
                    writein "}"
                switchBody <| fun () ->
                    code()
                if isPageAnimation then
                    SlideAnimation.writeAudioList()
                    SlideAnimation.jsSetCharacter()
                    SlideAnimation.jsSetSubtitle()
                    SlideAnimation.jsDrawNext()
                    SlideAnimation.jsDrawPrev()
                // head、body要素書き込みストリームを閉じてhead、body要素のコード取得
                let codeDraw = switchJSMain <| fun () ->
                    programList[prIndex].allCodes
                let codeBody = switchBody <| fun () ->
                    programList[prIndex].allCodes
                // html書き込みストリーム作成
                switchMain <| fun () ->
                    writein "<!DOCTYPE html>"
                    // html要素
                    html.tagb ("html", "lang=\"ja\"") <| fun () ->
                        // head要素
                        html.tagb ("head", "") <| fun () ->
                            // titleタグ
                            writein("<title>"+title+"</title>")
                            // metaタグ
                            writein "<meta charset=\"UTF-8\">"    
                            //追加（5/29）viewportタブ
                            writein "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0, maximum-scale=1.0, minimum-scale=1.0\">"
                            // titleタグ
                            html.tagb ("title", "") <| fun () ->
                                writein filename
                            // MathJax
                            html.tagb ("script", "type=\"text/javascript\" id=\"MathJax-script\" async src=\"https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml.js\"") <| fun () -> ()
                            html.tagb ("script", "type=\"text/javascript\" src=\"" + "contents_" + filename + "/animationSeq.js\"") <| fun () -> ()
                            html.tagb ("script", "type=\"text/javascript\" src=\"" + "contents_" + filename + "/animationSeqReset.js\"") <| fun () -> ()
                            html.tagb ("script", "type=\"text/javascript\" src=\"" + "contents_" + filename + "/animationStart.js\"") <| fun () -> ()
                            html.tagb ("script", "type=\"text/javascript\" src=\"" + "contents_" + filename + "/animationReset.js\"") <| fun () -> ()
                            html.tagb ("script", "type=\"text/javascript\" src=\"" + "contents_" + filename + "/autoAnimation.js\"") <| fun () -> ()
                            // scriptタグ
                            html.tagb ("script", "") <| fun () ->
                                writein codeDraw
                            // webフォント取得
                            writein "<link rel=\"preconnect\" href=\"https://fonts.googleapis.com\">"
                            writein "<link rel=\"preconnect\" href=\"https://fonts.gstatic.com\" crossorigin>"
                            writein "<link href=\"https://fonts.googleapis.com/css2?family=Noto+Sans+JP:wght@100..900&display=swap\" rel=\"stylesheet\">"
                            match cssfile with |Some x -> writein ("<link rel=\"stylesheet\" href=\""+x+"\" />") |None -> ()
                        // body要素
                        match pagesizeX,pagesizeY with
                        |None,None ->
                            let s0 = Style [area.backGroundColor "#ffffff"]
                            html.tagb ("body", s0) <| fun () ->
                                writein codeBody
                        |Some x,None ->
                            let s0 = Style [area.backGroundColor "#aaaaaa"]
                            html.tagb ("body", s0) <| fun () ->
                                let s1 = Style [
                                    area.backGroundColor "#ffffff"
                                    margin.left "auto"
                                    margin.right "auto"
                                    size.width (x.ToString()+"px")]
                                html.tagb ("div", s1) <| fun () ->
                                    writein codeBody
                        |None,Some y->
                            let s0 = Style [area.backGroundColor "#aaaaaa"]
                            html.tagb ("body", s0) <| fun () ->
                                let s1 = Style [
                                    area.backGroundColor "#ffffff"
                                    margin.left "auto"
                                    margin.right "auto"
                                    size.height (y.ToString()+"px")]
                                html.tagb ("div", s1) <| fun () ->
                                    writein codeBody
                        |Some x,Some y ->
                            let s0 = Style [area.backGroundColor "#aaaaaa"]
                            html.tagb ("body", s0) <| fun () ->
                                let s1 = Style [
                                    area.backGroundColor "#ffffff"
                                    margin.left "auto"
                                    margin.right "auto"
                                    size.width (x.ToString()+"px")
                                    size.height (y.ToString()+"px")]
                                html.tagb ("div", s1) <| fun () ->
                                    writein codeBody
                                    
                switchJSAnimationStart <| fun () ->
                    writein "test: () => {}"
                    writein "};"
                switchJSAnimationReset <| fun () ->
                    writein "test: () => {}"
                    writein "};"
                    writein ""
                    writein "function resetAll(){"
                    writein "    for (const key in animationResetMap) {"
                    writein "        if (typeof animationResetMap[key] === \"function\") {"
                    writein "            animationResetMap[key]();"
                    writein "        }"
                    writein "    }"
                    writein "}"
                switchAutoAnimation <| fun () ->
                    writein "test: () => {}"
                    writein "};"
                for i in 0..7 do
                    programList[i].close()
                // bodyタグ一時コード削除
                programList[1].delete()
                // JavaScript関数一時コード削除
                programList[2].delete()
                
    /// 全体がキャンバスの無制限レイアウト
    let freeCanvas outputdir filename (title:string) cssfile code =
        htmlpresentation outputdir filename title cssfile (None, None) false <| fun () ->
            html.canvas <| Style [size.width "0px"; size.height "0px"] <| code
            
    /// 全体がキャンバスの無制限レイアウト
    let freePage outputdir filename (title:string) cssfile code =
        htmlpresentation outputdir filename title cssfile (None, None) false code
        
    /// 固定幅レイアウト
    let fixedWidthPage outputdir filename (title:string) pageWidth cssfile code =
        htmlpresentation outputdir filename title cssfile (Some pageWidth, None) false code
        
    let fixedPage outputdir filename (title:string) pageWidth pageHeight setting cssfile code =
        setDefault setting
        htmlpresentation outputdir filename title cssfile (Some pageWidth, Some pageHeight) true <| fun () ->
            code()
            html.prevButton()
            html.nextButton()
            html.switchCharacter()
            html.switchSubtitle()
            html.switchAudio()
            html.audioPlayer()
            
[<AutoOpen>]
module htmlexpr2 =
    type html with
        static member animationManual (s:ViewBoxStyle) (p:position) (buttonX:int,buttonY:int) code =
            figcounter <- figcounter + 1
            let f = FigureAnimation(figcounter,s.mX,s.mY,s.sX,s.sY)
            switchBody <| fun () ->
                writein ("<svg viewBox=\"0 0 "+s.sX.ToString()+" "+s.sY.ToString()+"\" ")
                writein ("width=\""+s.sX.ToString()+"px\" ")
                writein ("heigth=\""+s.sY.ToString()+"px\" ")
                writein "xmlns=\"http://www.w3.org/2000/svg\" "
                writein ("style=\"margin-left: "+s.mX.ToString()+"; ")
                writein ("margin-top: "+s.mY.ToString()+"; ")
                writein "position: absolute;"
                writein ("background-color: "+s.backgroundColor+";")
                writein "\">"
                code(f,p)
                writein "</svg>"
            let asc = nextAnimationGroup()
            let fnameStart = f.jsStartControll asc
            let fnameReset = f.jsResetControll asc
            addAnimationButton (fnameStart,fnameReset,buttonX,buttonY)
            
        static member animationAuto (s:ViewBoxStyle) (p:position) code =
            figcounter <- figcounter + 1
            let f = FigureAnimation(figcounter,s.mX,s.mY,s.sX,s.sY)
            switchBody <| fun () ->
                writein ("<svg viewBox=\"0 0 "+s.sX.ToString()+" "+s.sY.ToString()+"\" ")
                writein ("width=\""+s.sX.ToString()+"px\" ")
                writein ("heigth=\""+s.sY.ToString()+"px\" ")
                writein "xmlns=\"http://www.w3.org/2000/svg\" "
                writein ("style=\"margin-left: "+s.mX.ToString()+"; ")
                writein ("margin-top: "+s.mY.ToString()+"; ")
                writein "position: absolute;"
                writein ("background-color: "+s.backgroundColor+";")
                writein "\">"
                code(f,p)
                writein "</svg>"
            let asc = nextAnimationGroup()
            let fnameStart = f.jsStartControll asc
            let fnameReset = f.jsResetControll asc
            addAutoAnimation(fnameStart,fnameReset)
