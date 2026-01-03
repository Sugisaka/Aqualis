namespace Aqualis
    
    open System
    
    type CSS = {Key:string; Value:string}
    
    type Atr(s:string) =
        new(s:string,t:string) = Atr (s+" = \""+t+"\"")
        new(s:Style) = 
            let h:string = s.code
            Atr h
        member _.code with get() = s
        static member list(s:list<Atr>) = String.concat " " (List.map (fun (s:Atr) -> s.code) s) 

    and Style(s:list<CSS>) =
        member _.list with get() = s
        member _.code0 with get() =
            s 
            |> List.map (fun s -> s.Key+": "+s.Value) 
            |> fun s -> String.concat "; " s + ";"
        member _.code with get() =
            s 
            |> List.map (fun s -> s.Key+": "+s.Value) 
            |> fun s -> String.concat "; " s
            |> fun s -> "style = \""+s+"\""
        member this.atr with get() = Atr this.code
        static member (+) (a:Style,b:Style) = Style(a.list@b.list)
        static member blank = Style []
        
    [<AutoOpen>]
    module style =
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
            let width (s:float) = {Key="stroke-width"; Value=s.ToString()+"px"}
            let dasharray (s:float) = {Key="stroke-dasharray"; Value=s.ToString()+"px"}
            let strokeOpacity(s:float) = {Key="stroke-opacity"; Value=s.ToString()}
            let fill (s:string) = {Key="fill"; Value=s}
            let fillOpacity(s:float) = {Key="fill-opacity"; Value=s.ToString()}
            let dash (pattern:string) = {Key="stroke-dasharray"; Value=pattern}
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
        
    type html =
        static member head title = fun code ->
            codewritein "<!doctype html>"
            codewritein "<html lang=\"ja\">"
            codewritein "<meta http-equiv=\"content-language\" content=\"ja\">"
            codewritein "<head>"
            codewritein("    <title>"+title+"</title>")
            codewritein "    <meta charset=\"utf-8\">"
            codewritein "    <meta name='viewport' content='width=device-width, initial-scale=1.0, maximum-scale=1.0, minimum-scale=1.0'>"
            codewritein "    <script type='text/javascript' id='MathJax-script' async src='https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml.js'></script>"
            codewritein "    <link rel='preconnect' href='https://fonts.googleapis.com'>"
            codewritein "    <link rel='preconnect' href='https://fonts.gstatic.com' crossorigin>"
            codewritein "    <link href='https://fonts.googleapis.com/css2?family=Noto+Sans+JP:wght@500;600;700&display=swap' rel='stylesheet'>"
            codewritein "    <link rel='stylesheet' href='style.css' />"
            codewritein "</head>"
            codewritein "<body>"
            code()
            codewritein "</body>"
            codewritein "</html>"
        static member head (title,refresh:int) = fun code ->
            codewritein "<!doctype html>"
            codewritein "<html lang=\"ja\">"
            codewritein "<meta http-equiv=\"content-language\" content=\"ja\">"
            codewritein "<head>"
            codewritein("    <title>"+title+"</title>")
            codewritein "    <meta charset=\"utf-8\">"
            codewritein "    <meta name='viewport' content='width=device-width, initial-scale=1.0, maximum-scale=1.0, minimum-scale=1.0'>"
            codewritein "    <script type='text/javascript' id='MathJax-script' async src='https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml.js'></script>"
            codewritein "    <link rel='preconnect' href='https://fonts.googleapis.com'>"
            codewritein "    <link rel='preconnect' href='https://fonts.gstatic.com' crossorigin>"
            codewritein "    <link href='https://fonts.googleapis.com/css2?family=Noto+Sans+JP:wght@500;600;700&display=swap' rel='stylesheet'>"
            codewritein "    <link rel='stylesheet' href='style.css' />"
            codewritein("    <meta http-equiv=\"refresh\" content=\""+refresh.ToString()+"\">")
            codewritein "</head>"
            codewritein "<body>"
            code()
            codewritein "</body>"
            codewritein "</html>"
        static member head (title,cssfile,jsfile,refresh:int) = fun code ->
            codewritein "<!doctype html>"
            codewritein "<html lang=\"ja\">"
            codewritein "<meta http-equiv=\"content-language\" content=\"ja\">"
            codewritein "<head>"
            codewritein("    <title>"+title+"</title>")
            codewritein "    <meta charset=\"utf-8\">"
            codewritein "    <meta name='viewport' content='width=device-width, initial-scale=1.0, maximum-scale=1.0, minimum-scale=1.0'>"
            codewritein "    <script type='text/javascript' id='MathJax-script' async src='https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml.js'></script>"
            codewritein "    <link rel='preconnect' href='https://fonts.googleapis.com'>"
            codewritein "    <link rel='preconnect' href='https://fonts.gstatic.com' crossorigin>"
            codewritein "    <link href='https://fonts.googleapis.com/css2?family=Noto+Sans+JP:wght@500;600;700&display=swap' rel='stylesheet'>"
            codewritein("    <link rel='stylesheet' href='"+cssfile+"'>")
            codewritein("    <script type='text/javascript' src='"+jsfile+"'></script>")
            codewritein("    <meta http-equiv=\"refresh\" content=\""+refresh.ToString()+"\">")
            codewritein "</head>"
            codewritein "<body>"
            code()
            codewritein "</body>"
            codewritein "</html>"
        static member head (title,cssfile,jsfile) = fun code ->
            codewritein "<!doctype html>"
            codewritein "<html lang=\"ja\">"
            codewritein "<meta http-equiv=\"content-language\" content=\"ja\">"
            codewritein "<head>"
            codewritein("    <title>"+title+"</title>")
            codewritein "    <meta charset=\"utf-8\">"
            codewritein "    <meta name='viewport' content='width=device-width, initial-scale=1.0, maximum-scale=1.0, minimum-scale=1.0'>"
            codewritein "    <script type='text/javascript' id='MathJax-script' async src='https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml.js'></script>"
            codewritein "    <link rel='preconnect' href='https://fonts.googleapis.com'>"
            codewritein "    <link rel='preconnect' href='https://fonts.gstatic.com' crossorigin>"
            codewritein "    <link href='https://fonts.googleapis.com/css2?family=Noto+Sans+JP:wght@500;600;700&display=swap' rel='stylesheet'>"
            codewritein("    <link rel='stylesheet' href='"+cssfile+"' />")
            codewritein("    <script type='text/javascript' src='"+jsfile+"'></script>")
            codewritein "</head>"
            codewritein "<body>"
            code()
            codewritein "</body>"
            codewritein "</html>"
        static member head (title,cssfile) = fun code ->
            codewritein "<!doctype html>"
            codewritein "<html lang=\"ja\">"
            codewritein "<meta http-equiv=\"content-language\" content=\"ja\">"
            codewritein "<head>"
            codewritein("    <title>"+title+"</title>")
            codewritein "    <meta charset=\"utf-8\">"
            codewritein "    <meta name='viewport' content='width=device-width, initial-scale=1.0, maximum-scale=1.0, minimum-scale=1.0'>"
            codewritein "    <script type='text/javascript' id='MathJax-script' async src='https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml.js'></script>"
            codewritein "    <link rel='preconnect' href='https://fonts.googleapis.com'>"
            codewritein "    <link rel='preconnect' href='https://fonts.gstatic.com' crossorigin>"
            codewritein "    <link href='https://fonts.googleapis.com/css2?family=Noto+Sans+JP:wght@500;600;700&display=swap' rel='stylesheet'>"
            codewritein("    <link rel='stylesheet' href='"+cssfile+"' />")
            codewritein "</head>"
            codewritein "<body>"
            code()
            codewritein "</body>"
            codewritein "</html>"
        /// 内部要素のないタグ
        static member taga (t:string,s:Style) =
            codewritein("<"+t+" "+s.code+" />")
        /// 内部要素のないタグ
        static member taga (t:string,atr:list<Atr>) =
            codewritein("<"+t+" "+Atr.list atr+" />")
        /// 内部要素のないタグ
        static member taga (t:string,lst:list<string*string>) =
            codewritein("<"+t+" ")
            for a,s in lst do
                codewritein(a + "=" + s + " ")
            codewritein " />"
        /// 内部要素のないタグ
        static member taga (t:string) =
            codewritein("<"+t+" ")
            codewritein " />"
        /// 内部要素のないタグ
        static member taga (t:string,a:string) =
            codewritein("<"+t+" "+a+" />")
        /// 内部要素のあるタグ
        static member tagb (t:string,atr:Style) = fun code ->
            let a = atr.code
            if a = "" then
                codewritein("<"+t+">")
            else
                codewritein("<"+t+" "+a+" >")
            code()
            codewritein ("</"+t+">")
        /// 内部要素のあるタグ
        static member tagb (t:string,atr:list<Atr>) = fun code ->
            let a = Atr.list atr
            if a = "" then
                codewritein("<"+t+">")
            else
                codewritein("<"+t+" "+a+" >")
            code()
            codewritein ("</"+t+">")
            
        /// 内部要素のあるタグ
        static member tagb (t:string,lst:list<string*string>) = fun code ->
            if lst.Length=0 then
                codewritein("<"+t+">")
            else
                codewritein("<"+t+" ")
                for a,s in lst do
                    codewritein(a + "=\"" + s + "\" ")
                codewritein(">")
            code()
            codewritein ("</"+t+">")
        /// 内部要素のあるタグ
        static member tagb (t:string,a:string) = fun code ->
            if a="" then
                codewritein("<"+t+">")
            else
                codewritein("<"+t+" "+a+">")
            code()
            codewritein ("</"+t+">")
        /// 内部要素のあるタグ
        static member tagb (t:string) = fun code ->
            codewritein("<"+t+">")
            code()
            codewritein ("</"+t+">")
            
        static member tagv (t:string,atr:list<Atr>) =
            codewritein("<" + t + " " + Atr.list atr + ">")
            
        static member tage (t:string) =
            codewritein("</" + t + ">") 
            
        static member h1 (t:string) = fun code ->
            html.tagb "h1" <| fun () -> codewritein t
            code()
            
        static member h1 (t:string,atr:Style) = fun code ->
            html.tagb ("h1",atr) <| fun () -> codewritein t
            code()
            
        static member h2 (t:string) = fun code ->
            html.tagb "h2" <| fun () -> codewritein t
            code()
            
        static member h2 (t:string,atr:Style) = fun code ->
            html.tagb ("h2",atr) <| fun () -> codewritein t
            code()
        static member h3 (t:string) = fun code ->
            html.tagb "h3" <| fun () -> codewritein t
            code()
        static member h3 (t:string,atr:Style) = fun code ->
            html.tagb ("h3",atr) <| fun () -> codewritein t
            code()
        static member h4 (t:string) = fun code ->
            html.tagb "h4" <| fun () -> codewritein t
            code()
        static member h4 (t:string,atr:Style) = fun code ->
            html.tagb ("h4",atr) <| fun () -> codewritein t
            code()
        static member h5 (t:string) = fun code ->
            html.tagb "h5" <| fun () -> codewritein t
            code()
        static member h5 (t:string,atr:Style) = fun code ->
            html.tagb ("h5",atr) <| fun () -> codewritein t
            code()
        static member linkstyle1 (t:string,atr:Style) = fun code ->
            html.tagb ("linkstyle1",atr) <| fun () -> codewritein t
            code()

        static member linkstyle2 (t:string,atr:Style) = fun code ->
            html.tagb ("linkstyle2",atr) <| fun () -> codewritein t
            code()

        static member linkstyle3 (t:string,atr:Style) = fun code ->
            html.tagb ("linkstyle3",atr) <| fun () -> codewritein t
            code()

        static member linkstyle4 (t:string,atr:Style) = fun code ->
            html.tagb ("linkstyle4",atr) <| fun () -> codewritein t
            code()

        static member boxA (atr:Style) = fun code ->
            html.tagb ("boxA",atr) <| fun () ->
            code()

        static member boxA_inner (atr:Style) = fun code ->
            html.tagb ("boxA_inner",atr) <| fun () ->
            code()

        static member box4 (atr:Style) = fun code ->
            html.tagb ("box4",atr) <| fun () ->
            code()

        static member box5 (atr:Style) = fun code ->
            html.tagb ("box5",atr) <| fun () ->
            code()

        static member box6_1 (atr:Style) = fun code ->
            html.tagb ("box6_1",atr) <| fun () ->
            code()

        static member box6_2 (atr:Style) = fun code ->
            html.tagb ("box6_2",atr) <| fun () ->
            code()

        static member box6_3 (atr:Style) = fun code ->
            html.tagb ("box6_3",atr) <| fun () ->
            code()

        static member box6_4 (atr:Style) = fun code ->
            html.tagb ("box6_4",atr) <| fun () ->
            code()

        static member box7 (atr:Style) = fun code ->
            html.tagb ("box7",atr) <| fun () ->
            code()

        static member box7_1 (atr:Style) = fun code ->
            html.tagb ("box7_1",atr) <| fun () ->
            code()

        static member box7_2 (atr:Style) = fun code ->
            html.tagb ("box7_2",atr) <| fun () ->
            code()

        static member box8 (atr:Style) = fun code ->
            html.tagb ("box8",atr) <| fun () ->
            code()

        static member p (atr:Style) = fun code ->
            html.tagb ("p",atr) <| fun () ->
            code()

        static member text (atr:Style) = fun code ->
            html.tagb ("text",atr) <| fun () ->
            code()

        static member tag_style1 (atr:Style) = fun code ->
            html.tagb ("tag_style1",atr) <| fun () ->
            code()

        static member tag_style2 (atr:Style) = fun code ->
            html.tagb ("tag_style2",atr) <| fun () ->
            code()

        static member img_style (atr:Style) = fun code ->
            html.tagb ("img_style",atr) <| fun () ->
            code()

        static member img_style2 (atr:Style) = fun code ->
            html.tagb ("img_style2",atr) <| fun () ->
            code()

        static member video_style (atr:Style) = fun code ->
            html.tagb ("video_style",atr) <| fun () ->
            code()

        static member news (atr:Style) = fun code ->
            html.tagb ("news",atr) <| fun () ->
            code()

        static member time (atr:Style) = fun code ->
            html.tagb ("time",atr) <| fun () ->
            code()

        static member menu (atr:Style) = fun code ->
            html.tagb ("menu",atr) <| fun () ->
            code()

        static member ul_style (atr:Style) = fun code ->
            html.tagb ("ul_style",atr) <| fun () ->
            code()

        static member ul_style2 (atr:Style) = fun code ->
            html.tagb ("ul_style2",atr) <| fun () ->
            code()

        static member li_style1 (atr:Style) = fun code ->
            html.tagb ("li_style1",atr) <| fun () ->
            code()

        static member li_style2 (atr:Style) = fun code ->
            html.tagb ("li_style2",atr) <| fun () ->
            code()

        static member bread (atr:Style) = fun code ->
            html.tagb ("bread",atr) <| fun () ->
            code()

        static member bread_ol (atr:Style) = fun code ->
            html.tagb ("bread_ol",atr) <| fun () ->
            code()

        static member bread_li (atr:Style) = fun code ->
            html.tagb ("bread_li",atr) <| fun () ->
            code()

        static member author (atr:Style) = fun code ->
            html.tagb ("author",atr) <| fun () ->
            code()

        static member p_style1 (atr:Style) = fun code ->
            html.tagb ("p_style1",atr) <| fun () ->
            code()

        static member p_style2 (atr:Style) = fun code ->
            html.tagb ("p_style2",atr) <| fun () ->
            code()

        static member p_style3 (atr:Style) = fun code ->
            html.tagb ("p_style3",atr) <| fun () ->
            code()

        static member ul (atr:Style) = fun code ->
            html.tagb ("ul",atr) <| fun () ->
            code()

        static member li (atr:Style) = fun code ->
            html.tagb ("li",atr) <| fun () ->
            code()

        static member card (atr:Style) = fun code ->
            html.tagb ("card",atr) <| fun () ->
            code()

        static member card_box (atr:Style) = fun code ->
            html.tagb ("card_box",atr) <| fun () ->
            code()
        static member card_box_wrap (atr:Style) = fun code ->
            html.tagb ("card_box_wrap",atr) <| fun () ->
            code()

        static member form (action:string) = fun code -> html.tagb ("form",["method","post"; "action",action;]) code
        static member form_fileUpload (action:string) = fun code -> html.tagb ("form",["method","post"; "enctype","multipart/form-data"; "action",action;]) code
        static member submit(url:string,name:string,value:string) = html.taga("input",["type","submit"; "name",name; "value",value; "formaction",url])
        static member table_ code = html.tagb "table" code
        static member table (a:list<string*string>) = fun code -> html.tagb ("table",a) code
        static member tr_ = fun code -> html.tagb "tr" code
        static member tr (a:list<string*string>) = fun code -> html.tagb ("tr",a) code
        static member th = fun code -> html.tagb "th" code
        static member td_ = fun code -> html.tagb "td" code
        static member td (a:list<string*string>) = fun code -> html.tagb ("td",a) code
        static member strong(t:string) = html.tagb "strong" <| fun () -> codewritein t
        static member enumerate_ code = html.tagb "ol" code
        static member ol (atr:Style) = fun code -> html.tagb ("ol",atr) code
        static member itemize_ code = html.tagb "ul" code
        static member itemize (a:list<string*string>) = fun code -> html.tagb ("ul",a) code
        static member item code = html.tagb "li" code
        static member item_ code = html.tagb "li" code
        static member item (a:list<string*string>) = fun code -> html.tagb ("li",a) code
        static member para_ code = html.tagb "p" code
        static member para (a:list<string*string>) = html.tagb ("p",a)
        static member para (t:string) = html.tagb "p" <| fun () -> codewritein(t)
        static member span(cls:string,t) = html.tagb ("span",["class",cls]) <| fun () -> codewritein(t)
        static member span(cls:string) = fun code -> html.tagb ("span",["class",cls]) code
        static member span(cls:string, s:Style) = fun code -> html.tagb ("span",[s.atr; Atr("class",cls)]) code
        static member link(url:string) = fun code -> html.tagb ("a",["href",url;]) code
        static member link(url:string, s:Style) = fun code -> html.tagb ("a",[s.atr; Atr("href",url)]) code
        static member link_newtab(url:string) = fun code -> html.tagb ("a",["href",url; "target","_blank"]) code
        static member select_disabled(x:string) = fun code -> html.tagb ("select",["name",x; "disabled","disabled"]) code
        static member time(datatime:string, s:Style) = fun code -> html.tagb ("time",[s.atr; Atr("datatime",datatime)]) code
        static member article(cls:string) = fun code -> html.tagb ("article", ["class", cls]) code
        static member aside (cls:string, s:Style) = fun code -> html.tagb ("aside", [s.atr; Atr("class", cls)]) code
        static member aside (a:list<string*string>) = fun code -> html.tagb ("aside",a) code
        static member section(cls:string, s:Style) = fun code -> html.tagb ("section", [s.atr; Atr("class", cls)]) code

        static member option(value:string) = fun code -> html.tagb ("option",["value",value;]) code
        static member option_selected(value:string) = fun code -> html.tagb ("option",["value",value;"selected","selected";]) code
        static member div (a:list<string*string>) = fun code -> html.tagb ("div",a) code
        static member button(value:string,onclick:string) = html.taga("input",["type","button"; "value",value; "onclick",onclick;])
        static member bold code = html.tagb "b" code
        static member latexTag (tagname:string) code =
            codewritein("\\begin{"+tagname+"}")
            code()
            codewritein("\\end{"+tagname+"}")
        static member eq q = "\\("+q+"\\)"
        static member align code =
            codewritein "\\[\\begin{align}"
            code()
            codewritein "\\end{align}\\]"
        static member footer code = html.tagb ("footer", ["class","footer"]) <| fun () -> code()
        static member footer (s:Style) = fun code -> html.tagb ("footer", s) <| fun () -> code()
        static member br() = codewritein "<br>"
        static member hr() = codewritein "<hr>"
        static member setjs filename =
            html.tagb ("script",[Atr("src",filename)]) <| fun () -> ()

        static member title (s:Style) (p:position) (text:string) =
            let s1 = Style [{Key = "margin-left"; Value = p.x.ToString()+"px";}
                            {Key = "margin-top"; Value = p.y.ToString()+"px";}
                            {Key = "position"; Value = "absolute";}
                            {Key = "font-family"; Value = "'Noto Sans JP'";}
                            {Key = "color"; Value = "black";}
                            {Key = "font-weight"; Value = "bold";}
                            {Key = "white-space"; Value = "nowrap";}
                            {Key = "font-size"; Value = "90px";}]
            html.tagb ("div",s1+s) <| fun () ->
                codewritein text

        static member contents (s:Style) (p:position) (text:string) =
            let s1 = Style [{Key = "margin-left"; Value = p.x.ToString()+"px";}
                            {Key = "margin-top"; Value = p.y.ToString()+"px";}
                            {Key = "position"; Value = "absolute";}
                            {Key = "font-family"; Value = "'Noto Sans JP'";}
                            {Key = "color"; Value = "black";}
                            {Key = "white-space"; Value = "nowrap";}
                            {Key = "font-size"; Value = "40px";}
                            {Key = "border-left-style"; Value= "solid";}
                            {Key = "border-left-width"; Value= "25px";}
                            {Key = "border-left-color"; Value= "#1e6eff";}
                            {Key = "padding-left"; Value="10px";}]
            html.tagb ("div",s1+s) <| fun () ->
                codewritein text

        static member subtitle1 (s:Style) (p:position) (text:string) =
            let s1 = Style [{Key = "margin-left"; Value = p.x.ToString()+"px";}
                            {Key = "margin-top"; Value = p.y.ToString()+"px";}
                            {Key = "position"; Value = "absolute";}
                            {Key = "font-family"; Value = "'Noto Sans JP'";}
                            {Key = "color"; Value = "black";}
                            {Key = "white-space"; Value = "nowrap";}
                            {Key = "font-size"; Value = "40px";}
                            {Key = "border-left-style"; Value= "solid";}
                            {Key = "border-left-width"; Value= "15px";}
                            {Key = "border-left-color"; Value= "#1e6eff";}
                            {Key = "padding-left"; Value="10px";}]
            html.tagb ("div",s1+s) <| fun () ->
                codewritein text

        static member subtitle2 (s:Style) (p:position) (text:string) =
            let s1 = Style [{Key = "margin-left"; Value = p.x.ToString()+"px";}
                            {Key = "margin-top"; Value = p.y.ToString()+"px";}
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
            html.tagb ("div",s1+s) <| fun () ->
                codewritein text
                

                
        static member textA (s:Style) = fun (p:position) (text:string) ->
            // html.writeTag "div" ("style = \"font-size:"+s.size.ToString()+ "px; margin-left:"+p.x.ToString()+"px; margin-top:"+p.y.ToString()+"px; color: "+s.color+"; white-space: nowrap; position: absolute;\"") <| fun () ->
            let s1 = Style [{Key = "margin-left"; Value = p.x.ToString()+"px";}
                            {Key = "margin-top"; Value = p.y.ToString()+"px";}
                            {Key = "position"; Value = "absolute";}
                            {Key = "font-size"; Value = "30px";}]
            html.tagb ("div", s1+s) <| fun () ->
                codewritein text

        static member textB (s:Style) = fun (p:position) (text:string) ->
            // html.writeTag "div" ("style = \"font-size:"+s.size.ToString()+ "px; margin-left:"+p.x.ToString()+"px; margin-top:"+p.y.ToString()+"px; color: "+s.color+"; white-space: nowrap; position: absolute;\"") <| fun () ->
            let s1 = Style [{Key = "margin-left"; Value = p.x.ToString()+"px";}
                            {Key = "margin-top"; Value = p.y.ToString()+"px";}
                            {Key = "position"; Value = "absolute";}
                            {Key = "font-size"; Value = "40px";}]
            html.tagb ("div", s1+s) <| fun () ->
                codewritein text
        static member textC (s:Style) = fun (p:position) (size:string) (c:string) (text:string) ->
            // html.writeTag "div" ("style = \"font-size:"+s.size.ToString()+ "px; margin-left:"+p.x.ToString()+"px; margin-top:"+p.y.ToString()+"px; color: "+s.color+"; white-space: nowrap; position: absolute;\"") <| fun () ->
            let s1 = Style [{Key = "margin-left"; Value = p.x.ToString()+"px";}
                            {Key = "margin-top"; Value = p.y.ToString()+"px";}
                            {Key = "position"; Value = "absolute";}
                            {Key = "font-size"; Value = size.ToString()+"px";}
                            {Key = "color"; Value = c.ToString();}]
            html.tagb ("div", s1+s) <| fun () ->
                codewritein text
                

            
        static member para code =
            html.tagb "p" code
            
        static member canvas (s:Style) code =
            html.tagb ("div", s) <| fun () ->
                code ()
                
        static member div (cls:string, s:Style) = fun code ->
            html.tagb ("div", [s.atr; Atr("class", cls)]) code
            
        static member div (s:list<Atr>) = fun code ->
            html.tagb ("div", s) code
            
        static member tag (tagname:string) (s:string) code =
            html.tagb (tagname, s) code
            
        static member tag_ (tagname:string) (s:string) =
            html.taga (tagname, s)

        static member fig (p:position) code =
            let f = figure()
            code(f,p)
            let sx,sy,mx,my = f.setWriteMode()
            codewritein (
                "<svg viewBox=\"0 0 "+sx.ToString()+" "+sy.ToString()+"\" "+
                "width=\""+sx.ToString()+"px\" "+
                "heigth=\""+sy.ToString()+"px\" "+
                "xmlns=\"http://www.w3.org/2000/svg\" "+
                "style=\"margin-left: "+mx.ToString()+"; "+
                "margin-top: "+my.ToString()+"; "+
                "position: absolute;"+
                "\">")
            code(f,p)
            codewritein "</svg>"
            
        static member blockTextcode (s:Style) (p:position) (width:float,height:float) (text:list<string>) =
            let padding = 5
            let s1 = Style [size.width (width.ToString()+"px")
                            size.height (height.ToString()+"px")
                            font.family "'Noto Sans Mono',monospace"
                            {Key = "margin-left"; Value = p.x.ToString()+"px";}
                            {Key = "margin-top"; Value = p.y.ToString()+"px";}
                            {Key = "position"; Value = "absolute";}
                            {Key = "overflow-wrap"; Value = "break-word";}]
            html.tagb ("div", s1+s)
                <| fun () ->
                    text |> List.iter (fun s -> codewritein (s+"<br>"))
                    codewritein ""
            {Left = p.x;
            Right = p.x+double width+2.0*double padding;
            Top = p.y;
            Bottom = p.y+double height+2.0*double padding;}
            
        static member table (lst:list<list<string>>) = fun (p:position) (size:int) ->
            codewritein ("<table style =\"margin-left: "+p.x.ToString()+"px; margin-top: "+p.y.ToString()+"px; font-size: "+size.ToString()+"px; position: absolute;\">")
            for m in 0..lst.Length-1 do
                codewritein "<tr>"
                for s in lst[m] do
                    codewritein "<td>"
                    codewritein s
                    codewritein "</td>"
                codewritein "</tr>"
            codewritein "</table>"
            codewritein "</div>"
            
    and figure() =
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
                html.taga ("line", [
                    Atr("x1",(startP.x-this.Xmin+this.Padding).ToString());
                    Atr("y1",(startP.y-this.Ymin+this.Padding).ToString());
                    Atr("x2",(endP.x-this.Xmin+this.Padding).ToString());
                    Atr("y2",(endP.y-this.Ymin+this.Padding).ToString());]@[s.atr])
            else
                this.updateRange startP
                this.updateRange endP

        member this.line (id:string) = fun (s:Style) (startP:position) (endP:position) ->
            if writeMode then
                html.taga ("line", [
                    Atr("id",id);
                    Atr("x1",(startP.x-this.Xmin+this.Padding).ToString());
                    Atr("y1",(startP.y-this.Ymin+this.Padding).ToString());
                    Atr("x2",(endP.x-this.Xmin+this.Padding).ToString());
                    Atr("y2",(endP.y-this.Ymin+this.Padding).ToString());]@[s.atr])
            else
                this.updateRange startP
                this.updateRange endP
        member this.Rect (s:Style) (startP:position) (sx:int) (sy:int) =
            if writeMode then
                html.taga ("rect", [
                    Atr("x", (startP.x-this.Xmin+this.Padding).ToString());
                    Atr("y", (startP.y-this.Ymin+this.Padding).ToString());
                    Atr("width",sx.ToString())
                    Atr("height", sy.ToString())]@[s.atr])
            else
                this.updateRange startP
                this.updateRange(startP.shift(sx,sy))

        member this.ellipse (s:Style) (center:position) (radiusX:int) (radiusY:int) =
            if writeMode then
                html.taga ("circle", [
                    Atr("cx", (center.x-this.Xmin+this.Padding).ToString());
                    Atr("cy", (center.y-this.Ymin+this.Padding).ToString());
                    Atr("rx", radiusX.ToString());
                    Atr("ry", radiusY.ToString());]@[s.atr])
            else
                this.updateRange(center.shiftX(-radiusX))
                this.updateRange(center.shiftX(radiusX))
                this.updateRange(center.shiftY(-radiusY))
                this.updateRange(center.shiftY(radiusY))

        member this.polygon (s:Style) (apex:list<position>) =
            if writeMode then
                let pp = String.concat " " <| List.map (fun (p:position) -> (p.x-this.Xmin+this.Padding).ToString()+","+(p.y-this.Ymin+this.Padding).ToString()) apex
                html.taga ("polygon", [Atr("points",pp)]@[s.atr])
            else
                for q in apex do
                    this.updateRange q
        member this.polyline (s:Style) (apex:list<position>) =
            if writeMode then
                let pp = String.concat " " <| List.map (fun (p:position) -> (p.x-this.Xmin+this.Padding).ToString()+","+(p.y-this.Ymin+this.Padding).ToString()) apex
                html.taga ("polygon", [Atr("points", pp)]@[s.atr])
            else
                for q in apex do
                    this.updateRange q
        member this.trianglearrow (s:Style) (lineWidth:float) (startP:position) (endP:position) =
            let r = 30.0
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
            if writeMode then
                this.line s startP (position(ux,uy))
            else
                this.updateRange(startP)
                this.updateRange(endP)
                this.updateRange(position(q1x,q1y))
                this.updateRange(position(q2x,q2y))
            this.polygon s [position(q1x,q1y);endP;position(q2x,q2y)]
        member this.linearrow (s:Style) (startP:position) (endP:position) (lineWidth:float) =
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
            if writeMode then
                this.line (s+Style[stroke.width lineWidth]) startP (position(ux,uy))
            else
                this.updateRange(startP)
                this.updateRange(endP)
                this.updateRange(position(q1x,q1y))
                this.updateRange(position(q2x,q2y))
            this.polyline s [position(q1x,q1y);endP;position(q2x,q2y)]
