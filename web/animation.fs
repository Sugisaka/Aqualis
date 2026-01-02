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
        switchJS <| fun () ->
            codewrite("    var e = document.getElementById(\""+id+"\");")
            codewrite("    var x1 = " + (f.Start.X t).code + ";")
            codewrite("    var y1 = " + (canvasY - f.Start.Y t).code + ";")
            codewrite("    var x2 = " + (f.End.X t).code + ";")
            codewrite("    var y2 = " + (canvasY - f.End.Y t).code + ";")
            codewrite("    e.setAttribute(\"style\"," + "\"" + s1.code0 + "\");")
            codewrite "    e.setAttribute(\"x1\", x1);"
            codewrite "    e.setAttribute(\"y1\", y1);"
            codewrite "    e.setAttribute(\"x2\", x2);"
            codewrite "    e.setAttribute(\"y2\", y2);"
        switchJSAnimationSeqReset <| fun () ->
            codewrite("    var e = document.getElementById(\""+id+"\");")
            codewrite("    e.setAttribute(\"style\"," + "\"" + s0.code0 + "\");")
            
type AnimationEllipse(s:Style,canvasX:int,canvasY:int) =
    let id = nextContentsID()
    let s0 = Style ([{Key="visibility";Value="hidden"}]@s.list)
    let s1 = Style ([{Key="visibility";Value="visible"}]@s.list)
    do
        html.taga ("ellipse", [Atr("id",id);]@[s0.atr])
    member this.ID with get() = id
    member this.P (e:Ellipse) = 
        let t = num0(Var(Dt,"t",NaN))
        switchJS <| fun () ->
            codewrite ("    var e = document.getElementById(\""+id+"\");")
            codewrite ("    var cx = " + (e.center.X t).code + ";")
            codewrite ("    var cy = " + (canvasY - e.center.Y t).code + ";")
            codewrite ("    var rx = " + (e.radiusX t).code + ";")
            codewrite ("    var ry = " + (e.radiusY t).code + ";")
            codewrite ("    e.setAttribute(\"style\"," + "\"" + s1.code0 + "\");")
            codewrite "    e.setAttribute(\"cx\", cx);"
            codewrite "    e.setAttribute(\"cy\", cy);"
            codewrite "    e.setAttribute(\"rx\", rx);"
            codewrite "    e.setAttribute(\"ry\", ry);"
        switchJSAnimationSeqReset <| fun () ->
            codewrite("    var e = document.getElementById(\""+id+"\");")
            codewrite("    e.setAttribute(\"style\"," + "\"" + s0.code0 + "\");")
            
type AnimationArc(s:Style,canvasX:int,canvasY:int) =
    let id = nextContentsID()
    let s0 = Style ([{Key="visibility";Value="hidden"}]@s.list)
    let s1 = Style ([{Key="visibility";Value="visible"}]@s.list)
    do
        html.taga ("path", [Atr("id",id);]@[s0.atr])
    member this.ID with get() = id
    member this.P (e:Arc) = 
        let t = num0(Var(Dt,"t",NaN))
        switchJS <| fun () ->
            codewrite ("    var e = document.getElementById(\""+id+"\");")
            let a1 = Math.PI * e.angle1 t / 180
            let x1 = e.center.X t + e.radius t * asm.cos a1
            let y1 = e.center.Y t + e.radius t * asm.sin a1
            codewrite("    var x1 = " + x1.code+";")
            codewrite("    var y1 = " + (canvasY - y1).code+";")
            let a2 = Math.PI * e.angle2 t / 180 - 1E-4
            let x2 = e.center.X t + e.radius t * asm.cos a2
            let y2 = e.center.Y t + e.radius t * asm.sin a2
            codewrite("    var x2 = " + x2.code+";")
            codewrite("    var y2 = " + (canvasY - y2).code+";")
            codewrite("    var a1 = " + (e.angle1 t).code+";")
            codewrite("    var a2 = " + (e.angle2 t).code+";")
            codewrite("    var radiusX = " + (e.radius t).code + ";")
            codewrite("    var radiusY = " + (e.radius t).code + ";")
            codewrite "    var da = a2 - a1;"
            codewrite "    if(da < 0.0) {da = a2 + 360 - a1;}"
            codewrite "    var largerOrSmaller = 0;"
            codewrite "    if(da > 180.0) {largerOrSmaller = 1;}"
            codewrite("    d = \"M \" + x1 + \" \" + y1 + \" A \" + radiusX + \" \" + radiusY + \" 0 \" + largerOrSmaller + \" 0 \" + x2 + \" \" + y2 " + ";")
            codewrite("    e.setAttribute(\"style\"," + "\"" + s1.code0 + "\");")
            codewrite("    e.setAttribute(\"d\", " + "d" + ");")
        switchJSAnimationSeqReset <| fun () ->
            codewrite("    var e = document.getElementById(\""+id+"\");")
            codewrite("    e.setAttribute(\"style\"," + "\"" + s0.code0 + "\");")
            
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
        switchJS <| fun () ->
            codewrite ("    var e = document.getElementById(\""+id+"\");")
            codewrite ("    e.setAttribute(\"style\"," + "\"" + ss1.code0 + "\");")
            codewrite ("    e.innerHTML = \"" + e.str + "\";")
            codewrite ("    var x = " + (originX + e.center.X t).code+ ";")
            codewrite ("    var y = " + (originY + canvasY - e.center.Y t).code+ ";")
            codewrite  "    x = x - e.offsetWidth/2;"
            codewrite  "    y = y - e.offsetHeight/2;"
            codewrite ("    e.setAttribute(\"style\"," + "\"" + ss1.code0 + " margin-left: \"+String(x)+\"px; margin-top: \"+String(y)+\"px; \");")
        switchJSAnimationSeqReset <| fun () ->
            codewrite ("    var e = document.getElementById(\""+id+"\");")
            codewrite ("    e.setAttribute(\"style\"," + "\"" + ss0.code0 + "\");")
            
    member this.P (e:MathText) = 
        let t = num0(Var(Dt,"t",NaN))
        switchJS <| fun () ->
            codewrite ("    var e = document.getElementById(\""+id+"\");")
            codewrite ("    e.setAttribute(\"style\"," + "\"" + ss1.code0 + "\");")
            codewrite ("    e.innerHTML = \"\\\\(" + e.eq.code + "\\\\)\";")
            codewrite  "    MathJax.typeset();"
            codewrite ("    var x =" + (originX + e.center.X t).code+ ";")
            codewrite ("    var y =" + (originY + canvasY - e.center.Y t).code+ ";")
            codewrite  "    x = x - e.offsetWidth/2;"
            codewrite  "    y = y - e.offsetHeight/2;"
            codewrite ("    e.setAttribute(\"style\"," + "\"" + ss1.code0 + " margin-left: \"+String(x)+\"px; margin-top: \"+String(y)+\"px; \");")
        switchJSAnimationSeqReset <| fun () ->
            codewrite ("    var e = document.getElementById(\""+id+"\");")
            codewrite ("    e.setAttribute(\"style\"," + "\"" + ss0.code0 + "\");")
            
type AnimationPolygon(s:Style,canvasX:int,canvasY:int) =
    let id = nextContentsID()
    let s0 = Style ([{Key="visibility";Value="hidden"}]@s.list)
    let s1 = Style ([{Key="visibility";Value="visible"}]@s.list)
    do
        html.taga ("polygon", [Atr("id", id);] @ [s.atr])
    member this.ID with get() = id
    member this.P (apex:list<tposition>) = 
        let t = num0(Var(Dt,"t",NaN))
        switchJS <| fun () ->
            codewrite ("    var e = document.getElementById(\"" + id + "\");")
            codewrite  "    var p = \"\";"
            for p in apex do
                codewrite ("    var x = " + (p.X t).code + ";")
                codewrite ("    var y = " + (canvasY - p.Y t).code + ";")
                codewrite  "    p = p + String(x) + \",\" + String(y) + \" \";"
            codewrite ("    e.setAttribute(\"style\"," + "\"" + s1.code0 + "\");")
            codewrite "    e.setAttribute(\"points\", p);"
        switchJSAnimationSeqReset <| fun () ->
            codewrite ("    var e = document.getElementById(\""+id+"\");")
            codewrite ("    e.setAttribute(\"style\"," + "\"" + s0.code0 + "\");")
            
type SlideAnimation =
    static member writeAudioList() =
        switchDraw <| fun () ->
            codewrite "const audioList = ["
            for i in 0..audioList.Length-1 do
                codewrite ("    \""+audioList[i] + "\"" + if i<audioList.Length-1 then "," else "")
            codewrite "];"
    static member jsSetCharacter() =
        switchDraw <| fun () ->
            codewrite "let pagecount = 1;"
            codewrite"function setCharacter()"
            codewrite"{"
            codewrite"        const swc = document.getElementById(\"switchCharacter\");"
            codewrite"        const c = document.getElementById(\"c\"+pagecount);"
            codewrite"        if(swc.checked)"
            codewrite"        {"
            codewrite"            c.style.display = \"block\";"
            codewrite"        }"
            codewrite"        else"
            codewrite"        {"
            codewrite"            c.style.display = \"none\";"
            codewrite"        }"
            codewrite"}"
    static member jsSetSubtitle() =
        switchDraw <| fun () ->
            codewrite "function setSubtitle()"
            codewrite "{"
            codewrite "        const sws = document.getElementById(\"switchSubtitle\");"
            codewrite "        const b2 = document.getElementById(\"sb\"+pagecount);"
            codewrite "        const s2 = document.getElementById(\"s\"+pagecount);"
            codewrite "        if(sws.checked)"
            codewrite "        {"
            codewrite "            b2.style.display = \"block\";"
            codewrite "            s2.style.display = \"block\";"
            codewrite "        }"
            codewrite "        else"
            codewrite "        {"
            codewrite "            b2.style.display = \"none\";"
            codewrite "            s2.style.display = \"none\";"
            codewrite "        }"
            codewrite "}"
    static member jsDrawNext() =
        switchDraw <| fun () ->
            codewrite "function drawNext()"
            codewrite "{"
            codewrite "    resetAll();"
            codewrite("    if(pagecount<"+anicounter.ToString()+")")
            codewrite "    {"
            codewrite "        const swc = document.getElementById(\"switchCharacter\");"
            codewrite "        const sws = document.getElementById(\"switchSubtitle\");"
            codewrite "        const swa = document.getElementById(\"switchAudio\");"
            codewrite "        "
            codewrite "        const p1 = document.getElementById(\"p\"+pagecount);"
            codewrite "        p1.style.display = \"none\";"
            codewrite "        const b1 = document.getElementById(\"sb\"+pagecount);"
            codewrite "        b1.style.display = \"none\";"
            codewrite "        const s1 = document.getElementById(\"s\"+pagecount);"
            codewrite "        s1.style.display = \"none\";"
            codewrite "        const c1 = document.getElementById(\"c\"+pagecount);"
            codewrite "        c1.style.display = \"none\";"
            codewrite "        pagecount++;"
            codewrite "        const p2 = document.getElementById(\"p\"+pagecount);"
            codewrite "        p2.style.display = \"block\";"
            codewrite "        if(sws.checked)"
            codewrite "        {"
            codewrite "            const b2 = document.getElementById(\"sb\"+pagecount);"
            codewrite "            b2.style.display = \"block\";"
            codewrite "            const s2 = document.getElementById(\"s\"+pagecount);"
            codewrite "            s2.style.display = \"block\";"
            codewrite "        }"
            codewrite "        else"
            codewrite "        {"
            codewrite "            const b2 = document.getElementById(\"sb\"+pagecount);"
            codewrite "            b2.style.display = \"none\";"
            codewrite "            const s2 = document.getElementById(\"s\"+pagecount);"
            codewrite "            s2.style.display = \"none\";"
            codewrite "        }"
            codewrite "        if(swc.checked)"
            codewrite "        {"
            codewrite "            const c2 = document.getElementById(\"c\"+pagecount);"
            codewrite "            c2.style.display = \"block\";"
            codewrite "        }"
            codewrite "        else"
            codewrite "        {"
            codewrite "            const c2 = document.getElementById(\"c\"+pagecount);"
            codewrite "            c2.style.display = \"none\";"
            codewrite "        }"
            codewrite "        const audioPlayer = document.getElementById(\"audioPlayer\");"
            codewrite "        if(audioList[pagecount-1] != \"\" && swa.checked)"
            codewrite "        {"
            codewrite "            audioPlayer.src = audioList[pagecount-1];"
            codewrite "            audioPlayer.play();"
            codewrite "        }"
            codewrite "    }"
            codewrite "}"
    static member jsDrawPrev() =
        switchDraw <| fun () ->
            codewrite "function drawPrev()"
            codewrite "{"
            codewrite "    resetAll();"
            codewrite "    if(pagecount>1)"
            codewrite "    {"
            codewrite "        const swc = document.getElementById(\"switchCharacter\");"
            codewrite "        const sws = document.getElementById(\"switchSubtitle\");"
            codewrite "        const swa = document.getElementById(\"switchAudio\");"
            codewrite "        const p1 = document.getElementById(\"p\"+pagecount);"
            codewrite "        p1.style.display = \"none\";"
            codewrite "        const b1 = document.getElementById(\"sb\"+pagecount);"
            codewrite "        b1.style.display = \"none\";"
            codewrite "        const s1 = document.getElementById(\"s\"+pagecount);"
            codewrite "        s1.style.display = \"none\";"
            codewrite "        const c1 = document.getElementById(\"c\"+pagecount);"
            codewrite "        c1.style.display = \"none\";"
            codewrite "        pagecount--;"
            codewrite "        const p2 = document.getElementById(\"p\"+pagecount);"
            codewrite "        p2.style.display = \"block\";"
            codewrite "        if(sws.checked)"
            codewrite "        {"
            codewrite "            const b2 = document.getElementById(\"sb\"+pagecount);"
            codewrite "            b2.style.display = \"block\";"
            codewrite "            const s2 = document.getElementById(\"s\"+pagecount);"
            codewrite "            s2.style.display = \"block\";"
            codewrite "        }"
            codewrite "        else"
            codewrite "        {"
            codewrite "            const b2 = document.getElementById(\"sb\"+pagecount);"
            codewrite "            b2.style.display = \"none\";"
            codewrite "            const s2 = document.getElementById(\"s\"+pagecount);"
            codewrite "            s2.style.display = \"none\";"
            codewrite "        }"
            codewrite "        if(swc.checked)"
            codewrite "        {"
            codewrite "            const c2 = document.getElementById(\"c\"+pagecount);"
            codewrite "            c2.style.display = \"block\";"
            codewrite "        }"
            codewrite "        else"
            codewrite "        {"
            codewrite "            const c2 = document.getElementById(\"c\"+pagecount);"
            codewrite "            c2.style.display = \"none\";"
            codewrite "        }"
            codewrite "        const audioPlayer = document.getElementById(\"audioPlayer\");"
            codewrite "        if(audioList[pagecount-1] != \"\" && swa.checked)"
            codewrite "        {"
            codewrite "            audioPlayer.src = audioList[pagecount-1];"
            codewrite "            audioPlayer.play();"
            codewrite "        }"
            codewrite "    }"
            codewrite "}"
            
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
            makeProgram [dir,filename,HTML] code
        /// 内部要素のないタグ
        static member taga (t:string,lst:list<string*exprString>) =
            codewrite("<"+t+" ")
            programList[prIndex].indentInc()
            for a,s in lst do
                codewrite(a + " = <?php echo \"\\\"\" . " + s.toString(" . ",StrQuotation) + " . \"\\\"\"; ?>")
            programList[prIndex].indentDec()
            codewrite " />"
        /// 内部要素のあるタグ
        static member tagb (t:string,lst:list<string*exprString>) = fun code ->
            if lst.Length=0 then
                codewrite("<"+t+">")
            else
                codewrite("<"+t+" ")
                programList[prIndex].indentInc()
                for a,s in lst do
                    codewrite(a + " = <?php echo \"\\\"\" . " + s.toString(" . ",StrQuotation) + " . \"\\\"\"; ?>")
                programList[prIndex].indentDec()
                codewrite ">"
            code()
            codewrite ("</"+t+">")
            
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
            
        static member submit(name:string,value:num0) = html.taga("input",["type","\"submit\""; "name","\""+name+"\""; "value",(value.code)])
        static member submit(name:num0,value:string) = html.taga("input",["type",Str "submit"; "name", "\\\""++name++"\\\""; "value",Str value])
        static member submit(url:string,name:num0,value:string) = html.taga("input",["type",Str "submit"; "name", "\\\""++name++"\\\""; "value",Str value; "formaction",Str url])
        static member submit(name:exprString,value:string) = html.taga("input",["type","\"submit\""; "name",name.toString(" . ",StrQuotation); "value","\""+value+"\""])
        static member submit_disabled(name:string,value:num0) = html.taga("input",["type",Str "submit"; "name",Str name; "value",Nvr value.Expr; "disabled",Str "disabled"])
        static member submit_disabled(name:num0,value:string) = html.taga("input",["type",Str "submit"; "name", "\\\""++name++"\\\""; "value",Str value; "disabled",Str "disabled"])
        static member submit_disabled(name:exprString,value:string) = html.taga("input",["type","\"submit\""; "name",name.toString(" . ",StrQuotation); "value","\""+value+"\""; "disabled","\"disabled\""])
        static member item (a:list<string*exprString>) = fun code -> html.tagb ("li",a) code
        static member link(url:num0) = fun code -> html.tagb ("a",["href","\""+url.code+"\"";]) code
        static member link(url:num0, s:Style) = fun code -> html.tagb ("a",[s.atr; Atr("href","\""+url.code+"\"")]) code
        static member select(x:num0) = fun code -> html.tagb ("select",["name",x.code;]) code
        static member select(x:exprString) = fun code -> html.tagb ("select",["name",x.toString(" . ",StrQuotation);]) code
        static member select_disabled(x:num0) = fun code -> html.tagb ("select",["name",x.code; "disabled","\"disabled\""]) code
        static member splitTag t code = 
            let b (lst:list<string*num0>) =
                if lst.Length=0 then
                    codewrite ("<"+t+">")
                else
                    codewrite("<"+t+" ")
                    for a,s in lst do
                        codewrite(a + "=" + s.code + " ")
                    codewrite ">"
            code b 
            codewrite ("</"+t+">")
        static member Select = html.splitTag "select" 
        static member Tr = html.splitTag "tr" 
        static member div (a:list<string*exprString>) = fun code -> html.tagb ("div",a) code
        /// チェックボックス（チェックされたとき1、チェックされていないとき0を送信）
        static member checkbox(name:num0) = 
            html.taga("input",["type",Str "hidden"; "name",Nvr name.Expr; "value",Str "0";])
            html.taga("input",["type",Str "checkbox"; "name",Nvr name.Expr; "value",Str "1";])
        /// チェックボックス（チェックされたとき1、チェックされていないとき0を送信）
        static member checkbox_disabled(name:num0) =
            html.taga("input",["type",Str "hidden"; "name",Nvr name.Expr; "value",Str "0";])
            html.taga("input",["type",Str "checkbox"; "name",Nvr name.Expr; "value",Str "1"; "disabled",Str "disabled"])
        /// チェックボックス（チェックされたとき1、チェックされていないとき0を送信）
        static member checkbox_checked(name:num0) = 
            html.taga("input",["type",Str "hidden"; "name",Nvr name.Expr; "value",Str "0";])
            html.taga("input",["type",Str "checkbox"; "name",Nvr name.Expr; "value",Str "1"; "checked",Str "checked";])
        /// チェックボックス（チェックされたとき1、チェックされていないとき0を送信）
        static member checkbox_checked_disabled(name:num0) =
            html.taga("input",["type",Str "hidden"; "name",Nvr name.Expr; "value",Str "0";])
            html.taga("input",["type",Str "checkbox"; "name",Nvr name.Expr; "value",Str "1"; "checked",Str "checked"; "disabled",Str "disabled"])
        static member Mathtext (s:Style) (p:position) (text:num0) =
            let s1 = Style [{Key = "margin-left"; Value=p.x.ToString()+"px"}
                            {Key = "margin-top"; Value=p.y.ToString()+"px"}
                            {Key = "position"; Value = "absolute";}]
            html.tagb ("div", s1+s) <| fun () ->
                codewrite ("\\(" + text.Expr.evalL programList[prIndex] + "\\)")
        static member image (s:Style,p:position) = fun (filename:string) ->
            let f = Path.GetFileName filename
            if File.Exists filename then
                if Directory.Exists (outputDir + "\\" + contentsDir) then
                    File.Copy(filename, outputDir + "\\" + contentsDir + "\\" + f, true)
                else
                    printfn "directory not exist: %s" (outputDir + "\\" + contentsDir)
            else
                printfn "image file not exist: %s" filename
            let st = Style [{Key="margin-left"; Value=p.x.ToString()+"px"}; {Key="margin-top"; Value=p.y.ToString()+"px"}] + s
            html.taga ("img", [st.atr;Atr("src",contentsDir + "\\" + f)])
        static member image (s:Style, id:string) = fun (filename:string) ->
            let f = Path.GetFileName filename
            if File.Exists filename then
                if Directory.Exists (outputDir + "\\" + contentsDir) then
                    File.Copy(filename, outputDir + "\\" + contentsDir + "\\" + f, true)
                else
                    printfn "directory not exist: %s" (outputDir + "\\" + contentsDir)
            else
                printfn "image file not exist: %s" filename
            html.taga ("img", [Atr("id",id); s.atr;Atr("src",contentsDir + "\\" + f)])
        static member image (s:Style) = fun (filename:string) ->
            let f = Path.GetFileName filename
            if File.Exists filename then
                if Directory.Exists (outputDir + "\\" + contentsDir) then
                    File.Copy(filename, outputDir + "\\" + contentsDir + "\\" + f, true)
                else
                    printfn "directory not exist: %s" (outputDir + "\\" + contentsDir)
            else
                printfn "image file not exist: %s" filename
            html.taga ("img", [s.atr;Atr("src",contentsDir + "\\" + f)])
        static member video (s:Style,p:position) = fun (filename:string) ->
            let f = Path.GetFileName filename
            if File.Exists filename then
                if Directory.Exists (outputDir + "\\" + contentsDir) then
                    File.Copy(filename, outputDir + "\\" + contentsDir + "\\" + f, true)
                else
                    printfn "directory not exist: %s" (outputDir + "\\" + contentsDir)
            else
                printfn "video file not exist: %s" filename
            let st = Style [{Key="margin-left"; Value=p.x.ToString()+"px"}; {Key="margin-top"; Value=p.y.ToString()+"px"}] + s
            html.tagv ("video", [st.atr;Atr("src", contentsDir + "\\" + f); Atr("controls", "")])
            html.tage "video"
        static member video (s:Style) = fun (filename:string) ->
            let f = Path.GetFileName filename
            if File.Exists filename then
                if Directory.Exists (outputDir + "\\" + contentsDir) then
                    File.Copy(filename, outputDir + "\\" + contentsDir + "\\" + f, true)
                else
                    printfn "directory not exist: %s" (outputDir + "\\" + contentsDir)
            else
                printfn "video file not exist: %s" filename
            html.tagv ("video", [s.atr;Atr("src", contentsDir + "\\" + f); Atr("controls", "")])
            html.tage "video"
        static member listTable (caption:string) = fun (borderH:list<BorderH>) (borderV:list<BorderV>) (tlist:list<list<string>>) ->
            html.tagb("div",["class","\"fig\""]) <| fun () ->
                html.tagb ("span",["class","\"caption\""]) <| fun () ->
                    codewrite(caption)
                html.tagb("table",["class","\"tab\""]) <| fun () ->
                    for j in 0..tlist.Length-1 do
                        html.tagb ("tr",["class",match borderV[j] with |TB -> "\"trtb\"" |T -> "\"trt\"" |B -> "\"trb\"" |N -> "\"trn\""]) <| fun () ->
                            for i in 0..tlist[j].Length-1 do
                                html.tagb ("td",["class",match borderH[i] with |L -> "\"tdl\"" |BorderH.C -> "\"tdc\"" |BorderH.R -> "\"tdr\"" |J -> "\"tdj\"" |Ll -> "\"tdlL\"" |Cl -> "\"tdcL\"" |Rl -> "\"tdrL\"" |Jl -> "\"tdjL\"" |Lr -> "\"tdlR\"" |Cr -> "\"tdcR\"" |Rr -> "\"tdrR\"" |Jr -> "\"tdjR\"" |Llr -> "\"tdlLR\"" |Clr -> "\"tdcLR\"" |Rlr -> "\"tdrLR\"" |Jlr -> "\"tdjLR\""]) <| fun () ->
                                    codewrite <| tlist[j][i]
        static member eq(text:num0) =
            codewrite ("\\("+text.Expr.evalL programList[prIndex] + "\\)")
            
        /// キャラクター付き解説ページ
        static member page (c:list<Character*string>) (audio:Audio) code2 =
            html.slide position.Origin <| fun p ->
                // 音声ファイル追加
                match audio.Source with
                |Silent ->
                    audioList <- audioList@[""]
                |AudioFile f ->
                    audioList <- audioList@[contentsDir+"/"+Path.GetFileName f]
                    if File.Exists f then
                        if Directory.Exists (outputDir + "\\" + contentsDir) then
                            File.Copy(f, outputDir + "\\" + contentsDir+"\\"+Path.GetFileName f, true)
                        else
                            printfn "directory not exist: %s" (outputDir + "\\" + contentsDir)
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
                                audioList <- audioList@[contentsDir+"/"+Path.GetFileName f]
                                File.Copy(f, outputDir + "\\" + contentsDir+"\\"+Path.GetFileName f, true)
                                printfn "Copy: %s -> %s" f <| outputDir + "\\" + contentsDir+"\\"+Path.GetFileName f
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
                html.tag "div" "style=\"width: 1920px; height: 880px; position: absolute; z-index: 0;\"" <| fun () ->
                    code2 p
                // 字幕枠
                html.tag "div" ("id = \"sb"+anicounter.ToString()+"\" style=\"width: 1880px; height: 160px; " + (if subtitle then "display: block; " else "display: none; ") + "position: absolute; z-index: 1; margin-top: 880px; padding: 20px; background-color: #aaaaff; font-family: 'Noto Sans JP'; font-size: 36pt; font-weight: 800; text-shadow: 0 1px 0 #fff, 1px 0 0 #fff, 0 -1px 0 #fff, -1px 0 0 #fff, -1px -1px 0 #fff, 1px -1px 0 #fff, -1px 1px 0 #fff, 1px 1px 0 #fff \";") <| fun () ->
                    ()
                // キャラクター画像
                html.tag "div" ("id = \"c"+anicounter.ToString()+"\"" + "style=\"" + (if character then "display: block; " else "display: none; ") + "\"") <| fun () ->
                    for c,file in c do
                        if File.Exists file then
                            if Directory.Exists (outputDir + "\\" + contentsDir) then
                                match c with
                                |Tale ->
                                    File.Copy(file, outputDir + "\\" + contentsDir+"\\"+Path.GetFileName file, true)
                                    html.tag_ "img" <| "src=\""+contentsDir+"/"+Path.GetFileName file+"\" style=\"position: absolute; margin-left: 0px; margin-top: -122px; width: 850px; object-position: right 204px top 426px; z-index: 2;\""
                                |Dang ->
                                    File.Copy(file, outputDir + "\\" + contentsDir+"\\"+Path.GetFileName file, true)
                                    html.tag_ "img" <| "src=\""+contentsDir+"/"+Path.GetFileName file+"\" style=\"position: absolute; margin-left: 1462px; margin-top: 727px; width: 450px; z-index: 3;\""
                                |Armi ->
                                    File.Copy(file, outputDir + "\\" + contentsDir+"\\"+Path.GetFileName file, true)
                                    html.tag_ "img" <| "src=\""+contentsDir+"/"+Path.GetFileName file+"\" style=\"position: absolute; margin-left: 1503px; margin-top: 638px; width: 360px; z-index: 4;\""
                            else
                                printfn "directory not exist: %s" (outputDir + "\\" + contentsDir)
                        else
                            printfn "character image file not exist: %s" file
                // 字幕
                match audio.Speaker with
                |Character.Tale -> 
                    html.tag "div" ("id = \"s"+anicounter.ToString()+"\" style=\"width: 1880px; height: 160px; " + (if subtitle then "display: block; " else "display: none; ") + "position: absolute; z-index: 5; margin-top: 880px; padding: 20px; font-family: 'Noto Sans JP'; color: #d11aff; font-size: 36pt; font-weight: 800; text-shadow: 0 1px 0 #fff, 1px 0 0 #fff, 0 -1px 0 #fff, -1px 0 0 #fff, -1px -1px 0 #fff, 1px -1px 0 #fff, -1px 1px 0 #fff, 1px 1px 0 #fff ;\"")
                        <| fun () -> codewrite audio.Subtitle.Subtitle
                |Character.Dang -> 
                    html.tag "div" ("id = \"s"+anicounter.ToString()+"\" style=\"width: 1880px; height: 160px; " + (if subtitle then "display: block; " else "display: none; ") + "position: absolute; z-index: 5; margin-top: 880px; padding: 20px; font-family: 'Noto Sans JP'; color: #455eff; font-size: 36pt; font-weight: 800; text-shadow: 0 1px 0 #fff, 1px 0 0 #fff, 0 -1px 0 #fff, -1px 0 0 #fff, -1px -1px 0 #fff, 1px -1px 0 #fff, -1px 1px 0 #fff, 1px 1px 0 #fff ;\"")
                        <| fun () -> codewrite audio.Subtitle.Subtitle
                |Character.Armi -> 
                    html.tag "div" ("id = \"s"+anicounter.ToString()+"\" style=\"width: 1880px; height: 160px; " + (if subtitle then "display: block; " else "display: none; ") + "position: absolute; z-index: 5; margin-top: 880px; padding: 20px; font-family: 'Noto Sans JP'; color: #ff8800; font-size: 36pt; font-weight: 800; text-shadow: 0 1px 0 #fff, 1px 0 0 #fff, 0 -1px 0 #fff, -1px 0 0 #fff, -1px -1px 0 #fff, 1px -1px 0 #fff, -1px 1px 0 #fff, 1px 1px 0 #fff ;\"")
                        <| fun () -> codewrite audio.Subtitle.Subtitle
                // メインコンテンツ
                html.tag "div" "style=\"width: 1920px; height: 880px; position: absolute; z-index: 0;\"" <| fun () ->
                    code2 p
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
                    codewrite "前へ"
        static member nextButton() =
                html.tagb ("button", "id=\"nextButton\" style=\"position: absolute; margin-left: 75px; z-index: 100;\" onclick=\"drawNext()\"") <| fun () ->
                    codewrite "次へ"
        static member startButton2(id:string) (s:Style) (c:string) =
                html.tagb ("button", [Atr("id",id); Atr("onclick",c)]@[s.atr]) <| fun () ->
                    codewrite "Start"
        static member resetButton2(id:string) (s:Style) (c:string) =
                html.tagb ("button", [Atr("id",id); Atr("onclick",c)]@[s.atr]) <| fun () ->
                    codewrite "Reset"
        static member switchCharacter() =
            html.taga ("input", "type=\"checkbox\" id=\"switchCharacter\" style=\"position: absolute; margin-top: 6px; margin-left: 150px; z-index: 100;\"  onclick=\"setCharacter()\" " + if character then "checked" else "")
            html.tagb ("label", "style=\"position: absolute; margin-top: 0px; margin-left: 165px; z-index: 100;\"") <| fun () ->
                codewrite "キャラクター"
        static member switchSubtitle() =
            html.taga ("input", "type=\"checkbox\" id=\"switchSubtitle\" style=\"position: absolute; margin-top: 6px; margin-left: 270px; z-index: 100;\" onclick=\"setSubtitle()\" " + if subtitle then "checked" else "")
            html.tagb ("label", "style=\"position: absolute; margin-top: 0px; margin-left: 285px; z-index: 100;\"") <| fun () ->
                codewrite "字幕"
        static member switchAudio() =
            html.taga ("input", "type=\"checkbox\" id=\"switchAudio\" style=\"position: absolute; margin-top: 6px; margin-left: 330px; z-index: 100;\" onclick=\"setSubtitle()\" " + if voice then "checked" else "")
            html.tagb ("label", "style=\"position: absolute; margin-top: 0px; margin-left: 345px; z-index: 100;\"") <| fun () ->
                codewrite "音声"
        static member audioPlayer() =
                html.tagb ("audio", "id=\"audioPlayer\"")  <| fun () -> ()
        static member imageA (s:Style) = fun (p:position) (filename:string) ->
            let s1 = Style [{Key = "margin-left"; Value = p.x.ToString()+"px";}
                            {Key = "margin-top"; Value = p.y.ToString()+"px";}
                            {Key = "position"; Value = "absolute";}]
            let f = Path.GetFileName filename
            if File.Exists filename then
                if Directory.Exists (outputDir + "\\" + contentsDir) then
                    File.Copy(filename, outputDir + "\\" + contentsDir + "\\" + f, true)
                else
                    printfn "directory not exist: %s" (outputDir + "\\" + contentsDir)
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
                text |> List.iter (fun s -> codewrite (s+"<br>"))
                codewrite("\r\n")
            {Left = p.x;
            Right = p.x+double width+2.0*double padding;
            Top = p.y;
            Bottom = p.y+double height+2.0*double padding;}
            
type FigureAnimation(figcounter:int,outputdir:string,projectname:string,originX:int,originY:int,canvasX:int,canvasY:int) =
    let padding = 10.0
    let mutable animeFlow:list<string*string*AnimationSetting> = []
    let mutable counter = 0
    member _.Padding with get() = padding
    member _.id with get() = "fa"+figcounter.ToString()+"_"+counter.ToString()
    
    member this.seq (setting:AnimationSetting) (setFigure:AnimationSetting->unit) =
        // アニメーションシーケンスIDを発行
        let idstart,idreset = nextAnimationSeqID()
        switchJS <| fun () ->
            codewrite ("function "+idstart+"(t){")
        switchJSAnimationSeqReset <| fun () ->
            codewrite ("function "+idreset+"(){")
        setFigure setting
        switchJS <| fun () ->
            codewrite "}"
        switchJSAnimationSeqReset <| fun () ->
            codewrite "}"
        animeFlow <- animeFlow@[idstart,idreset,setting]
        
    member this.animationEllipse s = AnimationEllipse(s,canvasX,canvasY)
    member this.animationLine s = AnimationLine(s,canvasX,canvasY)
    member this.animationArc s = AnimationArc(s,canvasX,canvasY)
    member this.animationText s = AnimationText(s,originX,originY,canvasX,canvasY)
    member this.animationPolygon s = AnimationPolygon(s,canvasX,canvasY)
    member this.line (s:Style) (startP:tposition) (endP:tposition) =
        // コンテンツIDを発行
        let id = nextContentsID()
        html.taga ("line", [Atr("id",id)]@[s.atr])
        let t = num0(Var(Dt,"t",NaN))
        switchJS <| fun () ->
            codewrite ("    var e = document.getElementById(\""+id+"\");")
            codewrite ("    e.setAttribute(\"x1\", " + (startP.X t).code + ");")
            codewrite ("    e.setAttribute(\"y1\", " + (startP.Y t).code + ");")
            codewrite ("    e.setAttribute(\"x2\", " + (endP.X t).code + ");")
            codewrite ("    e.setAttribute(\"y2\", " + (endP.Y t).code + ");")
    member this.ellipse1 (s:Style) (center:tposition) (radiusX:int) (radiusY:int) =
        // コンテンツIDを発行
        let id = nextContentsID()
        html.taga ("ellipse", [Atr("id",id)]@[s.atr])
        let t = num0(Var(Dt,"t",NaN))
        switchJS <| fun () ->
            codewrite ("    var e = document.getElementById(\""+id+"\");")
            codewrite ("    e.setAttribute(\"cx\", " + (center.X t).code + ");")
            codewrite ("    e.setAttribute(\"cy\", " + (center.Y t).code + ");")
            codewrite ("    e.setAttribute(\"rx\", \"" + radiusX.ToString() + "\");")
            codewrite ("    e.setAttribute(\"ry\", \"" + radiusY.ToString() + "\");")
            
    member this.ellipse2 (s:Style) (startP:tposition) (center:tposition) (radiusX:int) (radiusY:int) (theta:num0->num0) (sweep:int) (interval:int) =
        // コンテンツIDを発行
        let id = nextContentsID()
        html.taga ("path", [Atr("id",id)]@[s.atr])
        let t = num0(Var(Dt,"t",NaN))
        switchJS <| fun () ->
            codewrite ("    var e = document.getElementById(\""+id+"\");")
            codewrite ("    var x1 = " + (startP.X t).code + ";")
            codewrite ("    var y1 = " + (startP.Y t).code + ";")
            codewrite ("    var radiusX = " + radiusX.ToString() + ";")
            codewrite ("    var radiusY = " + radiusY.ToString() + ";")
            codewrite ("    var theta = " + (theta t).code + " / (" + interval.ToString() + " - 0.99);")
            codewrite ("    var x2 = " + (center.X t).code + "+" + radiusX.ToString() + "*" + "Math.cos(theta);")
            codewrite ("    var y2 = " + (center.Y t).code + "+"  + radiusY.ToString() +  "*" + "Math.sin(theta);")
            codewrite "    var d = \"\" ;"
            codewrite ("    if (6.283185307179586*t/" + interval.ToString() + "< Math.PI) {" )
            codewrite ("       d = \"M \" + x1 + \" \" + y1 + \" A \" + radiusX + \" \" + radiusY + \" 0 \" + 0 + \" \" + " + sweep.ToString() + " + \" \" + x2 + \" \" + y2 " + ";")
            codewrite "    } else {"
            codewrite ("       d = \"M \" + x1 + \" \" + y1 + \" A \" + radiusX + \" \" + radiusY + \" 0 \" + 1 + \" \" + " + sweep.ToString() + " + \" \" + x2 + \" \" + y2 " + ";")
            codewrite "    }"
            codewrite ("    e.setAttribute(\"d\", " + "d" + ");")
            
    member this.polygon1 (s:Style) (apex:list<tposition>) =
        // コンテンツIDを発行
        let id = nextContentsID()
        html.taga ("polygon", [Atr("id", id)] @ [s.atr])
        let t = num0(Var(Dt,"t",NaN))
        switchJS <| fun () ->
            codewrite ("var e = document.getElementById(\"" + id + "\");")
        let pp =
            apex
            |> List.map (fun (p:tposition) ->

                "(" + (p.X t).code + ") + \",\" + (" + (p.Y t).code + ")"
            )
        let pointsJS = String.concat " + \" \" + " pp
        switchJS <| fun () ->
            codewrite ("e.setAttribute(\"points\", " + pointsJS + ");")
            
    member this.text (s:Style) (center:tposition) (radius:tposition) (angleS:int) (angleE:int) (str:string) (interval:int) =
        // コンテンツIDを発行
        let id = nextContentsID()
        html.taga ("text", [Atr("id",id)]@[s.atr])
        let t = num0(Var(Dt,"t",NaN))
        let lines = str.Split "\n" |> Array.toList
        switchJS <| fun () ->
            codewrite("var e = document.getElementById(\""+id+"\");")
            codewrite("var cx =" + (center.X t).code+ ";")
            codewrite("var cy =" + (center.Y t).code+ ";")
            codewrite("var theta1 = " + angleS.ToString() + "* Math.PI / 180.0;")
            codewrite("var theta2 = " + angleE.ToString() + "* Math.PI / 180.0;")
            codewrite "var delta = theta2 - theta1;"
            codewrite "if (delta < 0) {delta += 2*Math.PI}"
            codewrite "if (delta > Math.PI) {delta -= 2*Math.PI}"
            codewrite("if (delta != 0) {var theta = theta1 + delta * (t / " + interval.ToString() + ")}")
            codewrite "if (delta == 0) {var theta = theta1}"
            codewrite("var x = cx + " + (radius.X t).code + "* Math.cos(theta);")
            codewrite("var y = cy - " + (radius.Y t).code + "* Math.sin(theta);")
            codewrite "e.setAttribute(\"x\", x);"
            codewrite "e.setAttribute(\"y\", y);"
            codewrite("e.innerHTML = " + (String.concat " + " (lines |> List.mapi (fun i s -> "\"<tspan dy='" + string (if i=0 then 0.0 else 1.2) + "em'>" + s + "</tspan>\""))) + ";")
            
    member this.rect (s:Style) (startP:tposition) (sx:int) (sy:int) =
        // コンテンツIDを発行
        let id = nextContentsID()
        html.taga ("rect", [Atr("id",id)]@[s.atr])
        let t = num0(Var(Dt,"t",NaN))
        switchJS <| fun () ->
            codewrite ("var e = document.getElementById(\""+id+"\");")
            codewrite ("e.setAttribute(\"x\", " + (startP.X t).code + ");")
            codewrite ("e.setAttribute(\"y\", " + (startP.Y t).code + ");")
            codewrite ("e.setAttribute(\"width\", \"" + sx.ToString() + "\");")
            codewrite ("e.setAttribute(\"height\", \"" + sy.ToString() + "\");")
            
    member this.image1 (s:Style) (filename:string) (startP:tposition) (endP:tposition) (interval:int) =
        // コンテンツIDを発行
        let id = nextContentsID()
        let f = Path.GetFileName filename
        File.Copy(filename,outputdir+"\\"+projectname+"\\"+contentsDir+"\\"+f, true)
        counter <- counter + 1
        html.taga ("image", [Atr("id", id); Atr("xlink:href", contentsDir + "/" + f); s.atr])
        let t = num0(Var(Dt,"t",NaN))
        switchJS <| fun () ->
            codewrite ("var e = document.getElementById(\""+id+"\");")
            codewrite "if (t == 0) e.style.visibility = 'visible';"
            codewrite ("e.setAttribute(\"x\", " + (startP.X t).code + ");")
            codewrite ("e.setAttribute(\"y\", " + (startP.Y t).code + ");")
            
    member this.jsStartControll(buttonIndex:string) =
        let fname = "start" + buttonIndex
        switchJSAnimationStart <| fun () ->
            codewrite(fname+": () => {")
            for idstart,idreset,setting in animeFlow do
                codewrite("    repeatSeq(" + idstart + ", " + setting.FrameTime.ToString() + ", " + setting.FrameNumber.ToString() + ", () => {")
            for _ in animeFlow do
                codewrite "    });"
            codewrite "},"
        fname
        
    member this.jsResetControll(buttonIndex:string) =
        let fname = "reset" + buttonIndex
        switchJSAnimationReset <| fun () ->
            codewrite(fname+": () => {")
            for idstart,idreset,setting in animeFlow do
                codewrite("    " + idreset + "();")
            codewrite "},"
        fname
        
    static member jsAnimation codejs =
        switchBody <| fun () ->
            codewrite "var t = 0;"
            codewrite "var dt = 1;"
            codewrite "window.onload=function(){"
            codewrite "    var timer;"
            codewrite "    var delay = 33;"
            codewrite "    var loop = function(){"
            codewrite "        t = t + dt;"
            codewrite "        if(t >= 100){t = 0;}"
            codewrite "        clearTimeout(timer);"
            codewrite "        timer=setTimeout(loop,delay);"
            codewrite "    }"
            codewrite "    loop();"
            codewrite "}"
            codewrite codejs
            
    member this.Rect (s:Style) = fun (startP:tposition) (sx:int) (sy:int) ->
        counter <- counter + 1
        html.taga ("rect", [Atr("id",this.id)]@[s.atr])
        let t = num0(Var(Dt,"t",NaN))
        switchJS <| fun () ->
            codewrite("var e = document.getElementById(\""+this.id+"\");")
            codewrite("e.setAttribute(\"x\", " + (startP.X t).code + ");")
            codewrite("e.setAttribute(\"y\", " + (startP.Y t).code + ");")
            codewrite("e.setAttribute(\"width\", \"" + sx.ToString() + "\");")
            codewrite("e.setAttribute(\"height\", \"" + sy.ToString() + "\");")
            
    member this.polygon (s:Style) = fun (apex:list<tposition>) ->
        counter <- counter + 1
        html.taga ("polygon", [Atr("id", this.id)] @ [s.atr])
        let t = num0(Var(Dt,"t",NaN))
        switchJS <| fun () ->
            codewrite("var e = document.getElementById(\"" + this.id + "\");")
            let pp =
                apex
                |> List.map (fun (p:tposition) ->
                
                    "(" + (p.X t).code + ") + \",\" + (" + (p.Y t).code + ")"
                )
            let pointsJS = String.concat " + \" \" + " pp
            codewrite("e.setAttribute(\"points\", " + pointsJS + ");")
            
    member this.image (s:Style) = fun (startP:tposition) (endP:tposition) (filename:string) ->
        let f = Path.GetFileName filename
        File.Copy(filename,outputdir+"\\"+projectname+"\\"+contentsDir+"\\"+f, true)
        counter <- counter + 1
        html.taga ("image", [Atr("id", this.id); Atr("xlink:href", contentsDir + "/" + f); s.atr])
        let t = num0(Var(Dt,"t",NaN))
        switchJS <| fun () ->
            codewrite("var e = document.getElementById(\""+this.id+"\");")
            codewrite("e.setAttribute(\"x\", " + (startP.X t).code + ");")
            codewrite("e.setAttribute(\"y\", " + (startP.Y t).code + ");")
            
    member this.image2 (id:string) (s:Style) (startP:tposition) (endP:tposition) (filename:string) =
        let f = Path.GetFileName filename
        File.Copy(filename,outputdir+"\\"+projectname+"\\"+contentsDir+"\\"+f)
        counter <- counter + 1
        html.taga ("image", [Atr("id", id); Atr("xlink:href", contentsDir + "/" + f); s.atr])
        let t = num0(Var(Dt,"t",NaN))
        switchJS <| fun () ->
            codewrite("var e = document.getElementById(\""+id+"\");")
            codewrite("e.setAttribute(\"x\", " + (startP.X t).code + ");")
            codewrite("e.setAttribute(\"y\", " + (startP.Y t).code + ");")
            
    member this.image3 (s:Style) (startP:tposition) (filename:string) =
        let f = Path.GetFileName filename
        File.Copy(filename,outputdir+"\\"+projectname+"\\"+contentsDir+"\\"+f)
        counter <- counter + 1
        html.taga ("image", [Atr("id", this.id); Atr("xlink:href", contentsDir + "/" + f); s.atr])
        let t = num0(Var(Dt,"t",NaN))
        switchJS <| fun () ->
            codewrite("var e = document.getElementById(\""+this.id+"\");")
            codewrite("e.setAttribute(\"x\", " + (startP.X t).code + ");")
            codewrite("e.setAttribute(\"y\", " + (startP.Y t).code + ");")
            
[<AutoOpen>]
module dochtml =
    let htmlpresentation dir filename (lang:Language) (pagesizeX:option<int>,pagesizeY:option<int>) isPageAnimation code =
        // ディレクトリ作成
        if not <| Directory.Exists (dir + "\\" + filename) then
            ignore <| Directory.CreateDirectory(dir + "\\" + filename)
        if not <| Directory.Exists (dir + "\\" + filename + "\\" + "contents_" + filename) then
            ignore <| Directory.CreateDirectory(dir + "\\" + filename + "\\" + "contents_" + filename)
        // 出力先ディレクトリ
        outputDir <- dir + "\\" + filename
        // コンテンツディレクトリ
        contentsDir <- "contents_" + filename
        makeProgram
            [
                // メインファイル
                dir + "\\" + filename, filename + ".html", lang
                // HTML本体のコード
                dir + "\\" + filename, filename+"_body", lang
                // スライドアニメーション用javascriptファイル名
                filename  + "\\" + "contents_" + filename, "animationSeq.js", JavaScript
                // draw関数のコード
                dir + "\\" + filename, filename+"_draw", JavaScript
                // スライドアニメーション(アニメーション開始)用javascript
                filename  + "\\" + "contents_" + filename, "animationStart.js", JavaScript
                // スライドアニメーション(アニメーションリセット)用javascript
                filename  + "\\" + "contents_" + filename, "animationSeqReset.js", JavaScript
                // スライドアニメーション(アニメーションリセット)用javascript
                filename  + "\\" + "contents_" + filename, "animationReset.js", JavaScript
            ]
            <| fun () ->
                switchJSAnimationStart <| fun () ->
                    codewrite "const animationStartMap = {"
                switchJSAnimationReset <| fun () ->
                    codewrite "const animationResetMap = {"
                code()
                if isPageAnimation then
                    SlideAnimation.writeAudioList()
                    SlideAnimation.jsSetCharacter()
                    SlideAnimation.jsSetSubtitle()
                    SlideAnimation.jsDrawNext()
                    SlideAnimation.jsDrawPrev()
                // head、body要素書き込みストリームを閉じてhead、body要素のコード取得
                let codeDraw = switchDraw <| fun () ->
                    programList[prIndex].allCodes
                let codeBody = switchBody <| fun () ->
                    programList[prIndex].allCodes
                // html書き込みストリーム作成
                switchMain <| fun () ->
                    codewrite "<!DOCTYPE html>"
                    // html要素
                    html.tagb ("html", "lang=\"ja\"") <| fun () ->
                        // head要素
                        html.tagb ("head", "") <| fun () ->
                            // metaタグ
                            codewrite "<meta charset=\"UTF-8\">"    
                            //追加（5/29）viewportタブ
                            codewrite "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0, maximum-scale=1.0, minimum-scale=1.0\">"
                            // titleタグ
                            html.tagb ("title", "") <| fun () ->
                                codewrite filename
                            // MathJax
                            html.tagb ("script", "type=\"text/javascript\" id=\"MathJax-script\" async src=\"https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml.js\"") <| fun () -> ()
                            html.tagb ("script", "type=\"text/javascript\" src=\"" + "contents_" + filename + "/animationSeq.js\"") <| fun () -> ()
                            html.tagb ("script", "type=\"text/javascript\" src=\"" + "contents_" + filename + "/animationSeqReset.js\"") <| fun () -> ()
                            html.tagb ("script", "type=\"text/javascript\" src=\"" + "contents_" + filename + "/animationStart.js\"") <| fun () -> ()
                            html.tagb ("script", "type=\"text/javascript\" src=\"" + "contents_" + filename + "/animationReset.js\"") <| fun () -> ()
                            // scriptタグ
                            html.tagb ("script", "") <| fun () ->
                                codewrite codeDraw
                                // if codejs <> "" then
                                //     FigureAnimation.jsAnimation codejs
                            // webフォント取得
                            codewrite "<link rel=\"preconnect\" href=\"https://fonts.googleapis.com\">"
                            codewrite "<link rel=\"preconnect\" href=\"https://fonts.gstatic.com\" crossorigin>"
                            codewrite "<link href=\"https://fonts.googleapis.com/css2?family=Noto+Sans+JP:wght@100..900&display=swap\" rel=\"stylesheet\">"
                        // body要素
                        let s0 = Style [area.backGroundColor "#aaaaaa"]
                        match pagesizeX,pagesizeY with
                        |None,None ->
                            let s = Style [
                                font.family "'Noto Sans JP', sans-serif"
                                font.weight "500"
                                font.size 16]
                            html.tagb ("body", [Atr("onload","draw()");s.atr]) <| fun () ->
                                codewrite codeBody
                        |Some x,None ->
                            html.tagb ("body", s0) <| fun () ->
                                let s1 = Style [
                                    area.backGroundColor "#ffffff"
                                    margin.left "auto"
                                    margin.right "auto"
                                    size.width (x.ToString()+"px")]
                                html.tagb ("div", s1) <| fun () ->
                                    codewrite codeBody
                        |None,Some y->
                            html.tagb ("body", s0) <| fun () ->
                                let s1 = Style [
                                    area.backGroundColor "#ffffff"
                                    margin.left "auto"
                                    margin.right "auto"
                                    size.height (y.ToString()+"px")]
                                html.tagb ("div", s1) <| fun () ->
                                    codewrite codeBody
                        |Some x,Some y ->
                            html.tagb ("body", s0) <| fun () ->
                                let s1 = Style [
                                    area.backGroundColor "#ffffff"
                                    margin.left "auto"
                                    margin.right "auto"
                                    size.width (x.ToString()+"px")
                                    size.height (y.ToString()+"px")]
                                html.tagb ("div", s1) <| fun () ->
                                    codewrite codeBody
                                    
                switchJSAnimationStart <| fun () ->
                    codewrite "test: () => {}"
                    codewrite "};"
                switchJSAnimationReset <| fun () ->
                    codewrite "test: () => {}"
                    codewrite "};"
                    codewrite ""
                    codewrite "function resetAll(){"
                    codewrite "    for (const key in animationResetMap) {"
                    codewrite "        if (typeof animationResetMap[key] === \"function\") {"
                    codewrite "            animationResetMap[key]();"
                    codewrite "        }"
                    codewrite "    }"
                    codewrite "}"
                for i in 0..6 do
                    programList[i].close()
                // bodyタグ一時コード削除
                programList[1].delete()
                // draw関数一時コード削除
                programList[3].delete()
                    
    let group (t:string) d code = code d
    
    /// 全体がキャンバスの無制限レイアウト
    let freeCanvas lang outputdir filename code =
        htmlpresentation outputdir filename lang (None, None) false <| fun () ->
            html.canvas <| Style [size.width "0px"; size.height "0px"] <| code
            
    /// 全体がキャンバスの無制限レイアウト
    let freePage lang outputdir filename code =
        htmlpresentation outputdir filename lang (None, None) false code
        
    /// 固定幅レイアウト
    let fixedWidthPage lang outputdir filename pageWidth code =
        htmlpresentation outputdir filename lang (Some pageWidth, None) false code
        
    let fixedPage lang outputdir filename pageWidth pageHeight setting code =
        setDefault setting
        htmlpresentation outputdir filename lang (Some pageWidth, Some pageHeight) true <| fun () ->
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
        static member animation (outputdir:string) (projectname:string) (s:ViewBoxStyle) (p:position) (buttonX:int,buttonY:int) code =
            figcounter <- figcounter + 1
            let f = FigureAnimation(figcounter,outputdir,projectname,s.mX,s.mY,s.sX,s.sY)
            switchJS <| fun () ->
                codewrite "function repeatSeq(fn, interval, Nt, onComplete)"
                codewrite "{"
                codewrite "    let t = 0;"
                codewrite "    function run()"
                codewrite "    {"
                codewrite "        if (t < Nt)"
                codewrite "        {"
                codewrite "            fn(t);"
                codewrite "            t++;"
                codewrite "            setTimeout(run, interval);"
                codewrite "        }"
                codewrite "        else"
                codewrite "        {"
                codewrite "            onComplete();"
                codewrite "        }"
                codewrite "    }"
                codewrite "    run();"
                codewrite "}"
            switchBody <| fun () ->
                codewrite ("<svg viewBox=\"0 0 "+s.sX.ToString()+" "+s.sY.ToString()+"\" ")
                codewrite ("width=\""+s.sX.ToString()+"px\" ")
                codewrite ("heigth=\""+s.sY.ToString()+"px\" ")
                codewrite "xmlns=\"http://www.w3.org/2000/svg\" "
                codewrite ("style=\"margin-left: "+s.mX.ToString()+"; ")
                codewrite ("margin-top: "+s.mY.ToString()+"; ")
                codewrite "position: absolute;"
                codewrite ("background-color: "+s.backgroundColor+";")
                codewrite "\">"
                code(f,p)
                codewrite "</svg>"
            let asc = nextAnimationGroup()
            let fnameStart = f.jsStartControll asc
            let fnameReset = f.jsResetControll asc
            addAnimationButton (fnameStart,fnameReset,buttonX,buttonY)
