//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    open System

    type CSS = {Key:string; Value:string}

    type Atr(s:string,t:string) =
        new(s:string) = Atr(s,"")
        // new(s:Style) =
        //     let h:string = s.code
        //     Atr h
        member _.name with get() = s
        member _.value with get() = t
        member _.code with get() =
            match t with
            |"" -> s
            |_ -> s+" = \""+t+"\""
        static member list(s:list<Atr>) =
            String.concat " " (
                s
                |> List.map (fun (s:Atr) -> s.code)
                |> List.filter (fun s -> s<>""))

    and Style(s:list<CSS>) =
        member _.list with get() = s
        member _.code0 with get() =
            s
            |> List.map (fun s -> s.Key+": "+s.Value)
            |> fun s -> String.concat "; " s
        // member _.code with get() =
        //     s
        //     |> List.map (fun s -> s.Key+": "+s.Value)
        //     |> fun s -> String.concat "; " s
        //     |> fun s -> if s = "" then "" else "style = \""+s+"\""
        member this.atr with get() = Atr("style", this.code0)
        static member (+) (a:Style,b:Style) = Style(a.list@b.list)
        static member blank = Style []

    [<AutoOpen>]
    module style =
        let zindex(n:int) = {Key="z-index"; Value=n.ToString()}
        module area =
            let backGroundColor (s:string) = {Key="background-color"; Value=s}
            let backGroundSize (s:string) = {Key="background-size"; Value=s}
            let backGroundImage (filename:string) = {Key="background-image"; Value="url("+filename+")"}
            let opacity (s:string) = {Key="background-opacity"; Value=s}
        module font =
            let size (s:int) = {Key="font-size"; Value=s.ToString()+"px"}
            let color (s:string) = {Key="color"; Value=s}
            let weight (s:string) = {Key="font-weight"; Value=s.ToString()}
            let family (s:string) = {Key="font-family"; Value=s}
            let lineHeight (s:int) = {Key="line-height"; Value=s.ToString()+"px"}
            let style (s:string) = {Key="font-style"; Value=s}
        module size =
            let width (s:string) = {Key="width"; Value=s}
            let height (s:string) = {Key="height"; Value=s}
            let maxWidth (s:string) = { Key = "max-width"; Value = s }
        module margin =
            let left (s:string) = {Key="margin-left"; Value=s}
            let right (s:string) = {Key="margin-right"; Value=s}
            let top (s:string) = {Key="margin-top"; Value=s}
            let bottom (s:string) = {Key="margin-bottom"; Value=s}
            let all (s:int) = {Key="margin"; Value=s.ToString()+"px"}
            let custom (s:string) = {Key="margin"; Value=s}
        module padding =
            let left (s:int) = {Key="padding-left"; Value=s.ToString()+"px"}
            let right (s:int) = {Key="padding-right"; Value=s.ToString()+"px"}
            let top (s:int) = {Key="padding-top"; Value=s.ToString()+"px"}
            let bottom (s:int) = {Key="padding-bottom"; Value=s.ToString()+"px"}
            let all (s:int) = {Key="padding"; Value=s.ToString()+"px"}
            let paddingVH (v:int,h:int) = {Key="padding"; Value=v.ToString()+"px"+h.ToString()+"px"}
        module border =
            let style (s:string) = {Key="border"; Value=s}
            let color (s:string) = {Key="border-color"; Value=s}
            module width =
                let top (s:int) = {Key="border-top-width"; Value=s.ToString()+"px"}
                let bottom (s:int) = {Key="border-bottom-width"; Value=s.ToString()+"px"}
                let left (s:int) = {Key="border-left-width"; Value=s.ToString()+"px"}
                let right (s:int) = {Key="border-right-width"; Value=s.ToString()+"px"}
        module stroke =
            let color (s:string) = {Key="stroke"; Value=s}
            let width (s:float) = {Key="stroke-width"; Value=InvariantFormat.number s+"px"}
            let dasharray (s:list<int>) = {Key="stroke-dasharray"; Value=String.Join(" ",s |> List.map (fun i -> i.ToString()))}
            let opacity(s:float) = {Key="stroke-opacity"; Value=InvariantFormat.number s}
        module fill =
            let color (s:string) = {Key="fill"; Value=s}
            let opacity(s:float) = {Key="fill-opacity"; Value=InvariantFormat.number s}
        module align =
            module items =
                let center = {Key="align-items"; Value="center"}
            let justifyContent (s:string) = {Key="justify-content"; Value=s}
            let text (s:string) = {Key="text-align"; Value=s}
            let vertical (s:string) = {Key="vertical-align"; Value=s}
            let textDecoration (s:string) = {Key = "text-decoration"; Value = s}
            let float (s:string) = {Key = "float"; Value = s}
        module display =
            let flex = {Key="display"; Value="flex"}
            let display (s:string) = {Key="display"; Value= s}
            let gap (s:string) = {Key="gap"; Value=s}
            let visibility (s:string) = {Key="visibility"; Value= s}
        module list =
            let listStyle (s:string) = {Key="list-style"; Value=s}
        module bidi =
            let unicodeBidi (s:string) = {Key="unicode-bidi"; Value=s}
        module overflow =
            let clipMargin (s:string) = {Key = "overflow-clip-margin"; Value = s}
            let overflow (s:string) = {Key = "overflow"; Value = s}
        module cursor =
            let custom (s:string) = { Key = "cursor"; Value = s }
        module objectFit =
            let custom (s:string) = {Key = "object-fit"; Value = s}
        module flex =
            let wrap (s:string) = {Key="flex-wrap"; Value=s}
        module position =
            let position (s:string) = {Key="position"; Value=s}
            let index (s:int) = {Key="z-index"; Value=s.ToString()}
        module space =
            let space (s:string) = {Key = "white-space"; Value = s.ToString();}

    type Anchor = {Left:double; Right:double; Top:double; Bottom:double;}

    type position(xx:float,yy:float) =
        new(ix:int,iy:int) =
            position(float ix,float iy)
        member this.x with get() = xx
        member this.y with get() = yy
        member this.shift(x,y) = position(xx+x,yy+y)
        member this.shiftX(x) = this.shift(x,0)
        member this.shiftY(y) = this.shift(0,y)
        member this.origin = this.shift(0,0)
        static member Origin with get() = position(0,0)
        static member (+) (p1:position,p2:position) = position(p1.x+p2.x, p1.y+p2.y)
        static member (-) (p1:position,p2:position) = position(p1.x-p2.x, p1.y-p2.y)

    type html internal (program:program, environment:Aqualis option) =
        let write(s:string) = program.codewrite s
        let writei(s:string) = program.codewritei s
        let writen(s:string) = program.codewriten s
        let writein(s:string) = program.codewritein s
        new(program:program) = html(program, None)
        member internal _.Environment =
            environment
            |> Option.defaultWith (fun () -> invalidOp "This HTML writer is not associated with a CompilationEnvironment.")
        member internal this.GenerationContext = this.Environment.RequireGenerationContext()
        member this.head title = fun code ->
            writein "<!doctype html>"
            writein "<html lang=\"ja\">"
            writein "<meta http-equiv=\"content-language\" content=\"ja\">"
            writein "<head>"
            writein("    <title>"+title+"</title>")
            writein "    <meta charset=\"utf-8\">"
            writein "    <meta name='viewport' content='width=device-width, initial-scale=1.0, maximum-scale=1.0, minimum-scale=1.0'>"
            writein "    <script type='text/javascript' id='MathJax-script' async src='https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml.js'></script>"
            writein "    <link rel='preconnect' href='https://fonts.googleapis.com'>"
            writein "    <link rel='preconnect' href='https://fonts.gstatic.com' crossorigin>"
            writein "    <link href='https://fonts.googleapis.com/css2?family=Noto+Sans+JP:wght@500;600;700&display=swap' rel='stylesheet'>"
            writein "    <link rel='stylesheet' href='style.css' />"
            writein "</head>"
            writein "<body>"
            code()
            writein "</body>"
            writein "</html>"
        member this.head (title,refresh:int) = fun code ->
            writein "<!doctype html>"
            writein "<html lang=\"ja\">"
            writein "<meta http-equiv=\"content-language\" content=\"ja\">"
            writein "<head>"
            writein("    <title>"+title+"</title>")
            writein "    <meta charset=\"utf-8\">"
            writein "    <meta name='viewport' content='width=device-width, initial-scale=1.0, maximum-scale=1.0, minimum-scale=1.0'>"
            writein "    <script type='text/javascript' id='MathJax-script' async src='https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml.js'></script>"
            writein "    <link rel='preconnect' href='https://fonts.googleapis.com'>"
            writein "    <link rel='preconnect' href='https://fonts.gstatic.com' crossorigin>"
            writein "    <link href='https://fonts.googleapis.com/css2?family=Noto+Sans+JP:wght@500;600;700&display=swap' rel='stylesheet'>"
            writein "    <link rel='stylesheet' href='style.css' />"
            writein("    <meta http-equiv=\"refresh\" content=\""+refresh.ToString()+"\">")
            writein "</head>"
            writein "<body>"
            code()
            writein "</body>"
            writein "</html>"
        member this.head (title,cssfile,jsfile,refresh:int) = fun code ->
            writein "<!doctype html>"
            writein "<html lang=\"ja\">"
            writein "<meta http-equiv=\"content-language\" content=\"ja\">"
            writein "<head>"
            writein("    <title>"+title+"</title>")
            writein "    <meta charset=\"utf-8\">"
            writein "    <meta name='viewport' content='width=device-width, initial-scale=1.0, maximum-scale=1.0, minimum-scale=1.0'>"
            writein "    <script type='text/javascript' id='MathJax-script' async src='https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml.js'></script>"
            writein "    <link rel='preconnect' href='https://fonts.googleapis.com'>"
            writein "    <link rel='preconnect' href='https://fonts.gstatic.com' crossorigin>"
            writein "    <link href='https://fonts.googleapis.com/css2?family=Noto+Sans+JP:wght@500;600;700&display=swap' rel='stylesheet'>"
            writein("    <link rel='stylesheet' href='"+cssfile+"'>")
            writein("    <script type='text/javascript' src='"+jsfile+"'></script>")
            writein("    <meta http-equiv=\"refresh\" content=\""+refresh.ToString()+"\">")
            writein "</head>"
            writein "<body>"
            code()
            writein "</body>"
            writein "</html>"
        member this.head (title,cssfile,jsfile) = fun code ->
            writein "<!doctype html>"
            writein "<html lang=\"ja\">"
            writein "<meta http-equiv=\"content-language\" content=\"ja\">"
            writein "<head>"
            writein("    <title>"+title+"</title>")
            writein "    <meta charset=\"utf-8\">"
            writein "    <meta name='viewport' content='width=device-width, initial-scale=1.0, maximum-scale=1.0, minimum-scale=1.0'>"
            writein "    <script type='text/javascript' id='MathJax-script' async src='https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml.js'></script>"
            writein "    <link rel='preconnect' href='https://fonts.googleapis.com'>"
            writein "    <link rel='preconnect' href='https://fonts.gstatic.com' crossorigin>"
            writein "    <link href='https://fonts.googleapis.com/css2?family=Noto+Sans+JP:wght@500;600;700&display=swap' rel='stylesheet'>"
            writein("    <link rel='stylesheet' href='"+cssfile+"' />")
            writein("    <script type='text/javascript' src='"+jsfile+"'></script>")
            writein "</head>"
            writein "<body>"
            code()
            writein "</body>"
            writein "</html>"
        member this.head (title,cssfile) = fun code ->
            writein "<!doctype html>"
            writein "<html lang=\"ja\">"
            writein "<meta http-equiv=\"content-language\" content=\"ja\">"
            writein "<head>"
            writein("    <title>"+title+"</title>")
            writein "    <meta charset=\"utf-8\">"
            writein "    <meta name='viewport' content='width=device-width, initial-scale=1.0, maximum-scale=1.0, minimum-scale=1.0'>"
            writein "    <script type='text/javascript' id='MathJax-script' async src='https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml.js'></script>"
            writein "    <link rel='preconnect' href='https://fonts.googleapis.com'>"
            writein "    <link rel='preconnect' href='https://fonts.gstatic.com' crossorigin>"
            writein "    <link href='https://fonts.googleapis.com/css2?family=Noto+Sans+JP:wght@500;600;700&display=swap' rel='stylesheet'>"
            writein("    <link rel='stylesheet' href='"+cssfile+"' />")
            writein "</head>"
            writein "<body>"
            code()
            writein "</body>"
            writein "</html>"
        // /// 内部要素のないタグ
        // member this.taga (t:string,s:Style) =
        //     writein("<"+t+" "+s.code+" />")
        /// 内部要素のないタグ
        member this.taga (t:string,atr:list<Atr>) =
            writein("<"+t+" "+Atr.list atr+" />")
        // 内部要素のないタグ
        // member this.taga (t:string,lst:list<string*string>) =
        //     writein("<"+t+" ")
        //     for a,s in lst do
        //         writein(a + "=" + s + " ")
        //     writein " />"
        /// 内部要素のないタグ
        member this.taga (t:string) =
            writein("<"+t+" ")
            writein " />"
        /// 内部要素のないタグ
        member this.taga (t:string,a:string) =
            writein("<"+t+" "+a+" />")
        // /// 内部要素のあるタグ
        // member this.tagb (t:string,atr:Style) = fun code ->
        //     let a = atr.code
        //     if a = "" then
        //         writein("<"+t+">")
        //     else
        //         writein("<"+t+" "+a+" >")
        //     code()
        //     writein ("</"+t+">")
        /// 内部要素のあるタグ
        member this.tagb (t:string,atr:list<Atr>) = fun code ->
            let a = Atr.list atr
            if a = "" then
                writein("<"+t+">")
            else
                writein("<"+t+" "+a+" >")
            code()
            writein ("</"+t+">")

        // /// 内部要素のあるタグ
        // member this.tagb (t:string,lst:list<string*string>) = fun code ->
        //     if lst.Length=0 then
        //         writein("<"+t+">")
        //     else
        //         writein("<"+t+" ")
        //         for a,s in lst do
        //             writein(a + "=\"" + s + "\" ")
        //         writein ">"
        //     code()
        //     writein ("</"+t+">")
        /// 内部要素のあるタグ
        member this.tagb (t:string,a:string) = fun code ->
            if a="" then
                writein("<"+t+">")
            else
                writein("<"+t+" "+a+">")
            code()
            writein ("</"+t+">")
        /// 内部要素のあるタグ
        member this.tagb (t:string) = fun code ->
            writein("<"+t+">")
            code()
            writein ("</"+t+">")

        // /// 内部要素のあるタグ
        // member this.tagb0 (t:string,lst:list<string*string>) = fun code ->
        //     if lst.Length=0 then
        //         write("<"+t+">")
        //     else
        //         writen("<"+t+" ")
        //         for a,s in lst do
        //             writen(a + " = \"" + s + "\"")
        //         write ">"
        //     code()
        //     writen ("</"+t+">")

        member this.tagv (t:string,atr:list<Atr>) =
            writein("<" + t + " " + Atr.list atr + ">")

        member this.tage (t:string) =
            writein("</" + t + ">")

        member this.h1 (t:string) = fun code ->
            this.tagb "h1" <| fun () -> writein t
            code()

        member this.h1 (t:string,s:Style) = fun code ->
            this.tagb ("h1",[s.atr]) <| fun () -> writein t
            code()

        member this.h2 (t:string) = fun code ->
            this.tagb "h2" <| fun () -> writein t
            code()

        member this.h2 (t:string,s:Style) = fun code ->
            this.tagb ("h2",[s.atr]) <| fun () -> writein t
            code()
        member this.h3 (t:string) = fun code ->
            this.tagb "h3" <| fun () -> writein t
            code()
        member this.h3 (t:string,s:Style) = fun code ->
            this.tagb ("h3",[s.atr]) <| fun () -> writein t
            code()
        member this.h4 (t:string) = fun code ->
            this.tagb "h4" <| fun () -> writein t
            code()
        member this.h4 (t:string,s:Style) = fun code ->
            this.tagb ("h4",[s.atr]) <| fun () -> writein t
            code()
        member this.h5 (t:string) = fun code ->
            this.tagb "h5" <| fun () -> writein t
            code()
        member this.h5 (t:string,s:Style) = fun code ->
            this.tagb ("h5",[s.atr]) <| fun () -> writein t
            code()
        member this.form (action:string) = fun code -> this.tagb ("form",[Atr("method","post"); Atr("action",action);]) code
        member this.form_fileUpload (action:string) = fun code -> this.tagb ("form",[Atr("method","post"); Atr("enctype","multipart/form-data"); Atr("action",action);]) code
        member this.submit(url:string,name:string,value:string) = this.taga("input",[Atr("type","submit"); Atr("name",name); Atr("value",value); Atr("formaction",url)])
        // member this.table_ code = this.tagb "table" code
        member this.table (a:list<Atr>) = fun code -> this.tagb ("table",a) code
        member this.tableData (lst:list<list<string>>) = fun (p:position) (size:int) ->
            writein ("<table style =\"margin-left: "+InvariantFormat.number p.x+"px; margin-top: "+InvariantFormat.number p.y+"px; font-size: "+size.ToString()+"px; position: absolute;\">")
            for m in 0..lst.Length-1 do
                writein "<tr>"
                for s in lst[m] do
                    writein "<td>"
                    writein s
                    writein "</td>"
                writein "</tr>"
            writein "</table>"
            writein "</div>"
        // member this.tr code = this.tagb "tr" code
        member this.tr (a:list<Atr>) = fun code -> this.tagb ("tr",a) code
        member this.th (a:list<Atr>) code = this.tagb ("th",a) code
        member this.td (a:list<Atr>) code = this.tagb ("td",a) code
        // member this.td (a:list<string*string>) = fun code -> this.tagb ("td",a) code
        member this.strong(t:string) = this.tagb "strong" <| fun () -> writein t
        // member this.enumerate code = this.tagb "ol" code
        member this.enumerate (a:list<Atr>) = fun code -> this.tagb ("ol",a) code
        // member this.enumerate (a:Style) = fun code -> this.tagb ("ol",a) code
        member this.enumerateList (a:list<Atr>) (c:list<unit->unit>) =
            this.tagb "ol" <| fun () ->
                for x in c do
                    this.item a x
        member this.itemize code = this.tagb "ul" code
        member this.itemize (a:list<Atr>) = fun code -> this.tagb ("ul",a) code
        // member this.itemize (a:Style) = fun code -> this.tagb ("ul",a) code
        member this.itemizeList (a:list<Atr>) (c:list<unit->unit>) =
            this.tagb "ul" <| fun () ->
                for x in c do
                    this.item a x
        // member this.item code = this.tagb "li" code
        // member this.item (a:Style) = fun code -> this.tagb ("li",a) code
        member this.item (a:list<Atr>) = fun code -> this.tagb ("li",a) code
        member this.para code = this.tagb "p" code
        member this.para (a:list<Atr>) = this.tagb ("p",a)
        member this.para (t:string) = this.tagb "p" <| fun () -> writein(t)
        member this.span(cls:string,t) = this.tagb ("span",[Atr("class",cls)]) <| fun () -> writein(t)
        member this.span(cls:string) = fun code -> this.tagb ("span",[Atr("class",cls)]) code
        member this.span(cls:string, s:Style) = fun code -> this.tagb ("span",[s.atr; Atr("class",cls)]) code
        member this.link(url:string) = fun code -> this.tagb ("a",[Atr("href",url);]) code
        member this.link(url:string, s:Style) = fun code -> this.tagb ("a",[s.atr; Atr("href",url)]) code
        member this.link_newtab(url:string) = fun code -> this.tagb ("a",[Atr("href",url); Atr("target","_blank")]) code
        member this.select_disabled(x:string) = fun code -> this.tagb ("select",[Atr("name",x); Atr("disabled","disabled")]) code
        member this.time(datatime:string, s:Style) = fun code -> this.tagb ("time",[s.atr; Atr("datatime",datatime)]) code
        member this.article(cls:string) = fun code -> this.tagb ("article", [Atr("class", cls)]) code
        member this.aside (cls:string, s:Style) = fun code -> this.tagb ("aside", [s.atr; Atr("class", cls)]) code
        member this.aside (a:list<Atr>) = fun code -> this.tagb ("aside",a) code
        member this.section(cls:string, s:Style) = fun code -> this.tagb ("section", [s.atr; Atr("class", cls)]) code
        member this.option(value:string) = fun code -> this.tagb ("option",[Atr("value",value);]) code
        member this.option_selected(value:string) = fun code -> this.tagb ("option",[Atr("value",value);Atr("selected","selected");]) code
        // member this.div (a:list<Atr>) = fun code -> this.tagb ("div",a) code
        member this.button(value:string,onclick:string) = this.taga("input",[Atr("type","button"); Atr("value",value); Atr("onclick",onclick);])
        member this.bold code = this.tagb "b" code
        member this.latexTag (tagname:string) code =
            writein("\\begin{"+tagname+"}")
            code()
            writein("\\end{"+tagname+"}")
        member this.eq (q:string) = "\\("+q+"\\)"
        member this.align code =
            writein "\\[\\begin{align}"
            code()
            writein "\\end{align}\\]"
        member this.footer code = this.tagb ("footer", [Atr("class","footer")]) <| fun () -> code()
        member this.footer (s:Style) = fun code -> this.tagb ("footer", [s.atr]) <| fun () -> code()
        member this.br() = writein "<br>"
        member this.hr() = writein "<hr>"
        member this.setjs filename =
            this.tagb ("script",[Atr("src",filename)]) <| fun () -> ()
        member this.title (s:Style) (p:position) (text:string) =
            let s1 = Style [{Key = "margin-left"; Value = InvariantFormat.number p.x+"px";}
                            {Key = "margin-top"; Value = InvariantFormat.number p.y+"px";}
                            {Key = "position"; Value = "absolute";}
                            {Key = "font-family"; Value = "'Noto Sans JP'";}
                            {Key = "color"; Value = "black";}
                            {Key = "font-weight"; Value = "bold";}
                            {Key = "white-space"; Value = "nowrap";}
                            {Key = "font-size"; Value = "90px";}]
            this.tagb ("div",[(s1+s).atr]) <| fun () ->
                writein text
        member this.contents (s:Style) (p:position) (text:string) =
            let s1 = Style [{Key = "margin-left"; Value = InvariantFormat.number p.x+"px";}
                            {Key = "margin-top"; Value = InvariantFormat.number p.y+"px";}
                            {Key = "position"; Value = "absolute";}
                            {Key = "font-family"; Value = "'Noto Sans JP'";}
                            {Key = "color"; Value = "black";}
                            {Key = "white-space"; Value = "nowrap";}
                            {Key = "font-size"; Value = "40px";}
                            {Key = "border-left-style"; Value= "solid";}
                            {Key = "border-left-width"; Value= "25px";}
                            {Key = "border-left-color"; Value= "#1e6eff";}
                            {Key = "padding-left"; Value="10px";}]
            this.tagb ("div",[(s1+s).atr]) <| fun () ->
                writein text
        member this.subtitle1 (s:Style) (p:position) (text:string) =
            let s1 = Style [{Key = "margin-left"; Value = InvariantFormat.number p.x+"px";}
                            {Key = "margin-top"; Value = InvariantFormat.number p.y+"px";}
                            {Key = "position"; Value = "absolute";}
                            {Key = "font-family"; Value = "'Noto Sans JP'";}
                            {Key = "color"; Value = "black";}
                            {Key = "white-space"; Value = "nowrap";}
                            {Key = "font-size"; Value = "40px";}
                            {Key = "border-left-style"; Value= "solid";}
                            {Key = "border-left-width"; Value= "15px";}
                            {Key = "border-left-color"; Value= "#1e6eff";}
                            {Key = "padding-left"; Value="10px";}]
            this.tagb ("div",[(s1+s).atr]) <| fun () ->
                writein text
        member this.subtitle2 (s:Style) (p:position) (text:string) =
            let s1 = Style [{Key = "margin-left"; Value = InvariantFormat.number p.x+"px";}
                            {Key = "margin-top"; Value = InvariantFormat.number p.y+"px";}
                            {Key = "position"; Value = "absolute";}
                            {Key = "font-family"; Value = "'Noto Sans JP'";}
                            {Key = "color"; Value = "black";}
                            {Key = "white-space"; Value = "nowrap";}
                            {Key = "font-size"; Value = "30px";}
                            {Key = "border-left-style"; Value= "solid";}
                            {Key = "border-left-width"; Value= "15px";}
                            {Key = "border-left-color"; Value= "#1e6eff";}
                            {Key = "border-bottom-style"; Value= "solid";}
                            {Key = "border-bottom-width"; Value= "2px";}
                            {Key = "border-bottom-color"; Value= "#1e6eff";}
                            {Key = "padding-left"; Value="10px";}
                            {Key = "display"; Value="inline-block";}]
            this.tagb ("div",[(s1+s).atr]) <| fun () ->
                writein text
        member this.div (s:Style) = fun (p:position) code ->
            let s1 = Style [{Key = "margin-left"; Value = InvariantFormat.number p.x+"px";}
                            {Key = "margin-top"; Value = InvariantFormat.number p.y+"px";}
                            {Key = "position"; Value = "absolute";}]
            this.tagb ("div", [(s1+s).atr]) code
        member this.text (s:Style) = fun (p:position) (text:string) ->
            let s1 = Style [{Key = "margin-left"; Value = InvariantFormat.number p.x+"px";}
                            {Key = "margin-top"; Value = InvariantFormat.number p.y+"px";}
                            {Key = "position"; Value = "absolute";}]
            this.tagb ("div", [(s1+s).atr]) <| fun () ->
                writein text

        member this.canvas (s:Style) code =
            this.tagb ("div", [s.atr]) <| fun () ->
                code ()

        member this.div (cls:string, s:Style) = fun code ->
            this.tagb ("div", [s.atr; Atr("class", cls)]) code

        member this.div (s:list<Atr>) = fun code ->
            this.tagb ("div", s) code

        member this.tag (tagname:string) (s:string) code =
            this.tagb (tagname, s) code

        member this.tag_ (tagname:string) (s:string) =
            this.taga (tagname, s)

        member this.fig (p:position) code =
            let f = figure(this.taga)
            code(f,p)
            let sx,sy,mx,my = f.setWriteMode()
            writein (
                "<svg viewBox=\"0 0 "+InvariantFormat.number sx+" "+InvariantFormat.number sy+"\" "+
                "width=\""+InvariantFormat.number sx+"px\" "+
                "heigth=\""+InvariantFormat.number sy+"px\" "+
                "xmlns=\"http://www.w3.org/2000/svg\" "+
                "style=\"margin-left: "+InvariantFormat.number mx+"; "+
                "margin-top: "+InvariantFormat.number my+"; "+
                "position: absolute;"+
                "\">")
            code(f,p)
            writein "</svg>"

        member this.blockTextcode (s:Style) (p:position) (width:float,height:float) (borderWidth:float,borderStyle:string,borderColor:string) (text:list<string>) =
            let padding = 5
            let s1 = Style [size.width (InvariantFormat.number width+"px")
                            size.height (InvariantFormat.number height+"px")
                            font.family "'Noto Sans Mono',monospace"
                            {Key = "margin-left"; Value = InvariantFormat.number p.x + "px";}
                            {Key = "margin-top"; Value = InvariantFormat.number p.y + "px";}
                            {Key = "position"; Value = "absolute";}
                            {Key = "overflow-wrap"; Value = "break-word";}
                            {Key = "border-width"; Value = InvariantFormat.number borderWidth + "px";}
                            {Key = "border-style"; Value = borderStyle;}
                            {Key = "border-width"; Value = borderColor;}]
            this.tagb ("div", [(s1+s).atr])
                <| fun () ->
                    text |> List.iter (fun s -> writein (s+"<br>"))
                    writein ""
            {Left = p.x;
            Right = p.x+double width+2.0*double padding+2.0*double borderWidth;
            Top = p.y;
            Bottom = p.y+double height+2.0*double padding+2.0*double borderWidth;}

        member this.textFrame (s:Style) = fun (p:position) (size:int) (color:string) code ->
            let s1 = Style [{Key = "margin-left"; Value = InvariantFormat.number p.x+"px";}
                            {Key = "margin-top"; Value = InvariantFormat.number p.y+"px";}
                            {Key = "position"; Value = "absolute";}
                            {Key = "font-size"; Value = size.ToString()+"px";}
                            {Key = "color"; Value = color.ToString();}]
            this.tagb ("div", [(s1+s).atr]) <| fun () ->
                code()

        member this.equationFrame (s:Style) = fun (p:position) (size:int) (color:string) code ->
            this.textFrame s p size color <| fun () ->
                writein "\\("
                code()
                writein "\\)"

        member this.alignFrame (s:Style) = fun (p:position) (size:int) (color:string) code ->
            this.textFrame s p size color <| fun () ->
                writein "\\["
                writein "\\begin{align}"
                code()
                writein "\\end{align}"
                writein "\\]"

        member this.line (s:Style) (pp:list<position>) =
            this.fig (position(0.0,0.0)) <| fun (f,p) ->
                f.polyLine s pp
        member this.polyLine (s:Style) (pp:list<position>) =
            this.fig (position(0.0,0.0)) <| fun (f,p) ->
                f.polyLine s pp
        member this.arrow (lineStyle:Style,arrowStyle:Style,width,arrowsize) (pp:list<position>) =
            this.fig (position(0.0,0.0)) <| fun (f,p) ->
                f.polyLineTriangleArrow (lineStyle,arrowStyle,width,arrowsize) pp
        member this.circle (s:Style) (ps:position) (r:int) =
            this.fig (position(0.0,0.0)) <| fun (f,p) ->
                f.ellipse s (p + ps) r r
        member this.rectangle (s:Style) (ps:position) (w:int,h:int) =
            this.fig (position(0.0,0.0)) <| fun (f,p) ->
                f.rect s (p + ps) w h

        member this.graph (px0:double,py0:double) (sizeX:double,sizeY:double) (x1:double,x2:double) (y1:double,y2:double) code =
            this.fig (position(px0,py0)) <| fun (f,p) ->
                if x1*x2<0.0 then
                    let x0 = (0.0-x1)/(x2-x1)*sizeX
                    f.triangleArrow Style[stroke.color "#000000"; fill.color "#000000";] (3.0,20) (p+position(x0,sizeY)) (p+position(x0,0.0))
                if y1*y2<0.0 then
                    let y0 = sizeY-(0.0-y1)/(y2-y1)*sizeY
                    f.triangleArrow Style[stroke.color "#000000"; fill.color "#000000";] (3.0,20) (p+position(0.0,y0)) (p+position(sizeX,y0))
                code(f,p)
        member this.graphEq (px0:double,py0:double) (sizeX:double,sizeY:double) (x1:double,x2:double,N:int) (y1:double,y2:double) (fn:list<Style*(double->double)>) =
            this.graph (px0,py0) (sizeX,sizeY) (x1,x2) (y1,y2) <| fun (f,p) ->
                for s,fc in fn do
                    let pol =
                        [
                            for i in 0..N do
                                let x = x1 + (x2-x1)*double i/double N
                                let y = fc x
                                let X = (x-x1)/(x2-x1)*sizeX
                                let Y = sizeY-(y-y1)/(y2-y1)*sizeY
                                p+position(X,Y)
                        ]
                    f.polyLine s pol
        member this.graphEqs (px0:double,py0:double) (sizeX:double,sizeY:double) (x1:double,x2:double,N:int) (y1:double,y2:double) (fn:list<Style*(double->double)>) code =
            let T (p:position) = position((p.x-x1)/(x2-x1)*sizeX, sizeY-(p.y-y1)/(y2-y1)*sizeY)
            this.graph (px0,py0) (sizeX,sizeY) (x1,x2) (y1,y2) <| fun (f,p) ->
                for s,fc in fn do
                    let pol =
                        [
                            for i in 0..N do
                                let x = x1 + (x2-x1)*double i/double N
                                let y = fc x
                                p + T (position(x,y))
                        ]
                    f.polyLine s pol
                let line (s:Style) (pp:list<position>) =
                    f.polyLine s (pp |> List.map (fun ps -> p + T ps))
                let arrow (lineStyle:Style,arrowStyle:Style,lineWidth,arrowSize) (pp:list<position>) =
                    f.polyLineTriangleArrow (lineStyle,arrowStyle,lineWidth,arrowSize) (pp |> List.map (fun ps -> p + T ps))
                let circle (s:Style) (ps:position) (r:int) =
                    f.ellipse s (p + T ps) r r
                let rectangle (s:Style) (ps:position) (w:float,h:float) =
                    let Ws = int <| w/(x2-x1)*sizeX
                    let Hs = int <| h/(y2-y1)*sizeY
                    f.rect s (p + T ps) Ws Hs
                let text (s:Style) (ps:position) text = ()
                code(line,arrow,circle,rectangle,text)
            let line (s:Style) (pp:list<position>) = ()
            let arrow (lineStyle:Style,arrowStyle:Style,lineWidth,arrowSize) (pp:list<position>) = ()
            let circle (s:Style) (ps:position) (r:int) = ()
            let rectangle (s:Style) (ps:position) (w:float,h:float) = ()
            let text (s:Style) (ps:position) text =
                this.text (Style[font.family "'Noto Sans JP', sans-serif";font.weight "600"; zindex 3]+s) (position(px0,py0) + T ps) text
            code(line,arrow,circle,rectangle,text)

    and figure(writeTag:(string * list<Atr>) -> unit) =
        let padding = 10.0
        let mutable xmin:option<double> = None
        let mutable xmax:option<double> = None
        let mutable ymin:option<double> = None
        let mutable ymax:option<double> = None
        let mutable writeMode = false
        member _.Padding with get() = padding
        member _.Xmin with get() = match xmin with |None -> 0.0 |Some v -> v
        member _.Xmax with get() = match xmax with |None -> 0.0 |Some v -> v
        member _.Ymin with get() = match ymin with |None -> 0.0 |Some v -> v
        member _.Ymax with get() = match ymax with |None -> 0.0 |Some v -> v
        member this.setWriteMode() =
            writeMode <- true
            let sizeX = this.Xmax-this.Xmin+2.0*padding
            let sizeY = this.Ymax-this.Ymin+2.0*padding
            let marginX = this.Xmin-padding
            let marginY = this.Ymin-padding
            sizeX,sizeY,marginX,marginY

        member private _.updateRange(p:position) =
            match xmin with
            |None ->
                xmin <- Some p.x
            |Some xx when p.x<xx ->
                xmin <- Some p.x
            |_ -> ()
            match ymin with
            |None ->
                ymin <- Some p.y
            |Some yy when p.y<yy ->
                ymin <- Some p.y
            |_ -> ()

            match xmax with
            |None ->
                xmax <- Some p.x
            |Some xx when p.x>xx ->
                xmax <- Some p.x
            |_ -> ()
            match ymax with
            |None ->
                ymax <- Some p.y
            |Some yy when p.y>yy ->
                ymax <- Some p.y
            |_ -> ()

        member this.line (s:Style) = fun (startP:position) (endP:position) ->
            if writeMode then
                writeTag ("line", [
                    Atr("x1",InvariantFormat.number (startP.x-this.Xmin+this.Padding));
                    Atr("y1",InvariantFormat.number (startP.y-this.Ymin+this.Padding));
                    Atr("x2",InvariantFormat.number (endP.x-this.Xmin+this.Padding));
                    Atr("y2",InvariantFormat.number (endP.y-this.Ymin+this.Padding));]@[s.atr])
            else
                this.updateRange startP
                this.updateRange endP

        member this.line (id:string) = fun (s:Style) (startP:position) (endP:position) ->
            if writeMode then
                writeTag ("line", [
                    Atr("id",id);
                    Atr("x1",InvariantFormat.number (startP.x-this.Xmin+this.Padding));
                    Atr("y1",InvariantFormat.number (startP.y-this.Ymin+this.Padding));
                    Atr("x2",InvariantFormat.number (endP.x-this.Xmin+this.Padding));
                    Atr("y2",InvariantFormat.number (endP.y-this.Ymin+this.Padding));]@[s.atr])
            else
                this.updateRange startP
                this.updateRange endP

        member this.rect (s:Style) (startP:position) (sx:int) (sy:int) =
            if writeMode then
                writeTag ("rect", [
                    Atr("x", InvariantFormat.number (startP.x-this.Xmin+this.Padding));
                    Atr("y", InvariantFormat.number (startP.y-this.Ymin+this.Padding));
                    Atr("width",InvariantFormat.number sx)
                    Atr("height", InvariantFormat.number sy)]@[s.atr])
            else
                this.updateRange startP
                this.updateRange(startP.shift(sx,sy))

        member this.ellipse (s:Style) (center:position) (radiusX:int) (radiusY:int) =
            if writeMode then
                writeTag ("ellipse", [
                    Atr("cx", InvariantFormat.number (center.x-this.Xmin+this.Padding));
                    Atr("cy", InvariantFormat.number (center.y-this.Ymin+this.Padding));
                    Atr("rx", InvariantFormat.number radiusX);
                    Atr("ry", InvariantFormat.number radiusY);]@[s.atr])
            else
                this.updateRange(center.shiftX -radiusX)
                this.updateRange(center.shiftX radiusX)
                this.updateRange(center.shiftY -radiusY)
                this.updateRange(center.shiftY radiusY)

        member this.polygon (s:Style) (apex:list<position>) =
            if writeMode then
                let pp = String.concat " " <| List.map (fun (p:position) -> InvariantFormat.number (p.x-this.Xmin+this.Padding)+","+InvariantFormat.number (p.y-this.Ymin+this.Padding)) apex
                writeTag ("polygon", [Atr("points",pp)]@[s.atr])
            else
                for q in apex do
                    this.updateRange q

        member this.polyLine (s:Style) (apex:list<position>) =
            if writeMode then
                let pp = String.concat " " <| List.map (fun (p:position) -> InvariantFormat.number (p.x-this.Xmin+this.Padding)+","+InvariantFormat.number (p.y-this.Ymin+this.Padding)) apex
                writeTag ("polyline", [Atr("points", pp)]@[s.atr])
            else
                for q in apex do
                    this.updateRange q

        member this.triangleArrow (s:Style) (lineWidth:float,arrowSize:float) (startP:position) (endP:position) =
            let pi = 3.14159265358979
            let t0 = atan2 (startP.y-endP.y) (startP.x-endP.x)
            let q1x = endP.x + arrowSize*cos(t0-15.0*pi/180.0)
            let q1y = endP.y + arrowSize*sin(t0-15.0*pi/180.0)
            let q2x = endP.x + arrowSize*cos(t0+15.0*pi/180.0)
            let q2y = endP.y + arrowSize*sin(t0+15.0*pi/180.0)
            let ux,uy =
                let c = lineWidth/sqrt((endP.x-startP.x)*(endP.x-startP.x)+(endP.y-startP.y)*(endP.y-startP.y))
                endP.x + (startP.x-endP.x)*c,
                endP.y + (startP.y-endP.y)*c
            if writeMode then
                this.line (s+Style[stroke.width lineWidth]) startP (position(ux,uy))
            else
                this.updateRange startP
                this.updateRange endP
                this.updateRange(position(q1x,q1y))
                this.updateRange(position(q2x,q2y))
            this.polygon s [position(q1x,q1y);endP;position(q2x,q2y)]

        member this.polyLineTriangleArrow (lineStyle:Style,arrowStyle:Style,lineWidth:float,arrowSize:float) (pp:list<position>) =
            let pi = 3.14159265358979
            let startP = pp[pp.Length-2]
            let endP = pp[pp.Length-1]
            let t0 = atan2 (startP.y-endP.y) (startP.x-endP.x)
            let q1x = endP.x + arrowSize*cos(t0-15.0*pi/180.0)
            let q1y = endP.y + arrowSize*sin(t0-15.0*pi/180.0)
            let q2x = endP.x + arrowSize*cos(t0+15.0*pi/180.0)
            let q2y = endP.y + arrowSize*sin(t0+15.0*pi/180.0)
            let ux,uy =
                let c = lineWidth/sqrt((endP.x-startP.x)*(endP.x-startP.x)+(endP.y-startP.y)*(endP.y-startP.y))
                endP.x + (startP.x-endP.x)*c,
                endP.y + (startP.y-endP.y)*c
            if writeMode then
                this.polyLine (lineStyle+Style[stroke.width lineWidth]) <| (List.map (fun i -> pp[i]) [0..pp.Length-2])@[position(ux,uy)]
            else
                for p in pp do
                    this.updateRange p
                this.updateRange(position(q1x,q1y))
                this.updateRange(position(q2x,q2y))
            this.polygon arrowStyle [position(q1x,q1y);endP;position(q2x,q2y)]

        member this.lineArrow (s:Style,lineWidth:float,arrowSize:float) (startP:position) (endP:position) =
            let pi = 3.14159265358979
            let t0 = atan2 (startP.y-endP.y) (startP.x-endP.x)
            let q1x = endP.x + arrowSize*cos(t0-15.0*pi/180.0)
            let q1y = endP.y + arrowSize*sin(t0-15.0*pi/180.0)
            let q2x = endP.x + arrowSize*cos(t0+15.0*pi/180.0)
            let q2y = endP.y + arrowSize*sin(t0+15.0*pi/180.0)
            let ux,uy =
                let c = lineWidth/sqrt((endP.x-startP.x)*(endP.x-startP.x)+(endP.y-startP.y)*(endP.y-startP.y))
                endP.x + (startP.x-endP.x)*c,
                endP.y + (startP.y-endP.y)*c
            if writeMode then
                this.line (s+Style[stroke.width lineWidth]) startP (position(ux,uy))
            else
                this.updateRange startP
                this.updateRange endP
                this.updateRange(position(q1x,q1y))
                this.updateRange(position(q2x,q2y))
            this.polyLine s [position(q1x,q1y);endP;position(q2x,q2y)]

    [<AutoOpen>]
    module CompilationEnvironmentHtmlExtensions =
        type Aqualis with
            member this.html = html((this.RequireGenerationContext()).CurrentProgram, Some this)
