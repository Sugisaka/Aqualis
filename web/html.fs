namespace Aqualis

open System.IO

type html =
    static member htmlfile (dir:string,filename:string) code =
        makeProgram [dir,filename,HTML] code
    static member head title = fun code ->
        programList[prIndex].codewrite "<!doctype html>"
        programList[prIndex].codewrite "<html lang=\"ja\">"
        programList[prIndex].codewrite "<meta http-equiv=\"content-language\" content=\"ja\">"
        programList[prIndex].codewrite "<head>"
        programList[prIndex].codewrite("    <title>"+title+"</title>")
        programList[prIndex].codewrite "    <meta charset=\"utf-8\">"
        programList[prIndex].codewrite "    <meta name='viewport' content='width=device-width, initial-scale=1.0, maximum-scale=1.0, minimum-scale=1.0'>"
        programList[prIndex].codewrite "    <script type='text/javascript' id='MathJax-script' async src='https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml.js'></script>"
        programList[prIndex].codewrite "    <link rel='preconnect' href='https://fonts.googleapis.com'>"
        programList[prIndex].codewrite "    <link rel='preconnect' href='https://fonts.gstatic.com' crossorigin>"
        programList[prIndex].codewrite "    <link href='https://fonts.googleapis.com/css2?family=Noto+Sans+JP:wght@500;600;700&display=swap' rel='stylesheet'>"
        programList[prIndex].codewrite "    <link rel='stylesheet' href='style.css' />"
        programList[prIndex].codewrite "</head>"
        programList[prIndex].codewrite "<body>"
        code()
        programList[prIndex].codewrite "</body>"
        programList[prIndex].codewrite "</html>"
    static member head (title,refresh:int) = fun code ->
        programList[prIndex].codewrite "<!doctype html>"
        programList[prIndex].codewrite "<html lang=\"ja\">"
        programList[prIndex].codewrite "<meta http-equiv=\"content-language\" content=\"ja\">"
        programList[prIndex].codewrite "<head>"
        programList[prIndex].codewrite("    <title>"+title+"</title>")
        programList[prIndex].codewrite "    <meta charset=\"utf-8\">"
        programList[prIndex].codewrite "    <meta name='viewport' content='width=device-width, initial-scale=1.0, maximum-scale=1.0, minimum-scale=1.0'>"
        programList[prIndex].codewrite "    <script type='text/javascript' id='MathJax-script' async src='https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml.js'></script>"
        programList[prIndex].codewrite "    <link rel='preconnect' href='https://fonts.googleapis.com'>"
        programList[prIndex].codewrite "    <link rel='preconnect' href='https://fonts.gstatic.com' crossorigin>"
        programList[prIndex].codewrite "    <link href='https://fonts.googleapis.com/css2?family=Noto+Sans+JP:wght@500;600;700&display=swap' rel='stylesheet'>"
        programList[prIndex].codewrite "    <link rel='stylesheet' href='style.css' />"
        programList[prIndex].codewrite("    <meta http-equiv=\"refresh\" content=\""+refresh.ToString()+"\">")
        programList[prIndex].codewrite "</head>"
        programList[prIndex].codewrite "<body>"
        code()
        programList[prIndex].codewrite "</body>"
        programList[prIndex].codewrite "</html>"
    static member head (title,cssfile,jsfile,refresh:int) = fun code ->
        programList[prIndex].codewrite("<!doctype html>")
        programList[prIndex].codewrite("<html lang=\"ja\">")
        programList[prIndex].codewrite("<meta http-equiv=\"content-language\" content=\"ja\">")
        programList[prIndex].codewrite("<head>")
        programList[prIndex].codewrite("    <title>"+title+"</title>")
        programList[prIndex].codewrite("    <meta charset=\"utf-8\">")
        programList[prIndex].codewrite("    <meta name='viewport' content='width=device-width, initial-scale=1.0, maximum-scale=1.0, minimum-scale=1.0'>")
        programList[prIndex].codewrite("    <script type='text/javascript' id='MathJax-script' async src='https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml.js'></script>")
        programList[prIndex].codewrite("    <link rel='preconnect' href='https://fonts.googleapis.com'>")
        programList[prIndex].codewrite("    <link rel='preconnect' href='https://fonts.gstatic.com' crossorigin>")
        programList[prIndex].codewrite("    <link href='https://fonts.googleapis.com/css2?family=Noto+Sans+JP:wght@500;600;700&display=swap' rel='stylesheet'>")
        programList[prIndex].codewrite("    <link rel='stylesheet' href='"+cssfile+"'>")
        programList[prIndex].codewrite("    <script type='text/javascript' src='"+jsfile+"'></script>")
        programList[prIndex].codewrite("    <meta http-equiv=\"refresh\" content=\""+refresh.ToString()+"\">")
        programList[prIndex].codewrite("</head>")
        programList[prIndex].codewrite("<body>")
        code()
        programList[prIndex].codewrite("</body>")
        programList[prIndex].codewrite("</html>")
    static member head (title,cssfile,jsfile) = fun code ->
        programList[prIndex].codewrite("<!doctype html>")
        programList[prIndex].codewrite("<html lang=\"ja\">")
        programList[prIndex].codewrite("<meta http-equiv=\"content-language\" content=\"ja\">")
        programList[prIndex].codewrite("<head>")
        programList[prIndex].codewrite("    <title>"+title+"</title>")
        programList[prIndex].codewrite("    <meta charset=\"utf-8\">")
        programList[prIndex].codewrite("    <meta name='viewport' content='width=device-width, initial-scale=1.0, maximum-scale=1.0, minimum-scale=1.0'>")
        programList[prIndex].codewrite("    <script type='text/javascript' id='MathJax-script' async src='https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml.js'></script>")
        programList[prIndex].codewrite("    <link rel='preconnect' href='https://fonts.googleapis.com'>")
        programList[prIndex].codewrite("    <link rel='preconnect' href='https://fonts.gstatic.com' crossorigin>")
        programList[prIndex].codewrite("    <link href='https://fonts.googleapis.com/css2?family=Noto+Sans+JP:wght@500;600;700&display=swap' rel='stylesheet'>")
        programList[prIndex].codewrite("    <link rel='stylesheet' href='"+cssfile+"' />")
        programList[prIndex].codewrite("    <script type='text/javascript' src='"+jsfile+"'></script>")
        programList[prIndex].codewrite("</head>")
        programList[prIndex].codewrite("<body>")
        code()
        programList[prIndex].codewrite("</body>")
        programList[prIndex].codewrite("</html>")
    static member head (title,cssfile) = fun code ->
        programList[prIndex].codewrite("<!doctype html>")
        programList[prIndex].codewrite("<html lang=\"ja\">")
        programList[prIndex].codewrite("<meta http-equiv=\"content-language\" content=\"ja\">")
        programList[prIndex].codewrite("<head>")
        programList[prIndex].codewrite("    <title>"+title+"</title>")
        programList[prIndex].codewrite("    <meta charset=\"utf-8\">")
        programList[prIndex].codewrite("    <meta name='viewport' content='width=device-width, initial-scale=1.0, maximum-scale=1.0, minimum-scale=1.0'>")
        programList[prIndex].codewrite("    <script type='text/javascript' id='MathJax-script' async src='https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml.js'></script>")
        programList[prIndex].codewrite("    <link rel='preconnect' href='https://fonts.googleapis.com'>")
        programList[prIndex].codewrite("    <link rel='preconnect' href='https://fonts.gstatic.com' crossorigin>")
        programList[prIndex].codewrite("    <link href='https://fonts.googleapis.com/css2?family=Noto+Sans+JP:wght@500;600;700&display=swap' rel='stylesheet'>")
        programList[prIndex].codewrite("    <link rel='stylesheet' href='"+cssfile+"' />")
        programList[prIndex].codewrite("</head>")
        programList[prIndex].codewrite("<body>")
        code()
        programList[prIndex].codewrite("</body>")
        programList[prIndex].codewrite("</html>")
        
    /// 内部要素のないタグ
    static member taga (t:string,s:Style) =
        programList[prIndex].codewrite("<"+t+" "+s.code+" />")
    /// 内部要素のないタグ
    static member taga (t:string,atr:list<Atr>) =
        programList[prIndex].codewrite("<"+t+" "+Atr.list atr+" />")
    /// 内部要素のないタグ
    static member taga (t:string,lst:list<string*exprString>) =
        programList[prIndex].codewrite("<"+t+" ")
        for (a,s) in lst do
            programList[prIndex].codewrite(a + "=" + s.toString(" . ",StrQuotation) + " ")
        programList[prIndex].codewrite " />"
    /// 内部要素のないタグ
    static member taga (t:string,lst:list<string*string>) =
        programList[prIndex].codewrite("<"+t+" ")
        for (a,s) in lst do
            programList[prIndex].codewrite(a + "=" + s + " ")
        programList[prIndex].codewrite " />"
    /// 内部要素のないタグ
    static member taga (t:string) =
        programList[prIndex].codewrite("<"+t+" ")
        programList[prIndex].codewrite " />"
    /// 内部要素のないタグ
    static member taga (t:string,a:string) =
        programList[prIndex].codewrite("<"+t+" "+a+" />")
    /// 内部要素のあるタグ
    static member tagb (t:string,atr:Style) = fun code ->
        let a = atr.code
        if a = "" then
            programList[prIndex].codewrite("<"+t+">")
        else
            programList[prIndex].codewrite("<"+t+" "+a+" >")
        code()
        programList[prIndex].codewrite ("</"+t+">")
    /// 内部要素のあるタグ
    static member tagb (t:string,atr:list<Atr>) = fun code ->
        let a = Atr.list atr
        if a = "" then
            programList[prIndex].codewrite("<"+t+">")
        else
            programList[prIndex].codewrite("<"+t+" "+a+" >")
        code()
        programList[prIndex].codewrite ("</"+t+">")
    /// 内部要素のあるタグ
    static member tagb (t:string,lst:list<string*num0>) = fun code ->
        if lst.Length=0 then
            programList[prIndex].codewrite("<"+t+">")
        else
            programList[prIndex].codewrite("<"+t+" ")
            for (a,s) in lst do
                programList[prIndex].codewrite(a + "=" + s.code + " ")
            programList[prIndex].codewrite ">"
        code()
        programList[prIndex].codewrite ("</"+t+">")
    /// 内部要素のあるタグ
    static member tagb (t:string,lst:list<string*string>) = fun code ->
        if lst.Length=0 then
            programList[prIndex].codewrite("<"+t+">")
        else
            programList[prIndex].codewrite("<"+t+" ")
            for (a,s) in lst do
                programList[prIndex].codewrite(a + "=" + s + " ")
            programList[prIndex].codewrite(">")
        code()
        programList[prIndex].codewrite ("</"+t+">")
    /// 内部要素のあるタグ
    static member tagb (t:string,a:string) = fun code ->
        if a="" then
            programList[prIndex].codewrite("<"+t+">")
        else
            programList[prIndex].codewrite("<"+t+" "+a+">")
        code()
        programList[prIndex].codewrite ("</"+t+">")
    /// 内部要素のあるタグ
    static member tagb (t:string) = fun code ->
        programList[prIndex].codewrite("<"+t+">")
        code()
        programList[prIndex].codewrite ("</"+t+">")
    static member h1 (t:num0) = fun code ->
        html.tagb "h1" <| fun () -> php.echo (t.Expr.eval (programList[prIndex]))
        code()
    static member h1 (t:string) = fun code ->
        html.tagb "h1" <| fun () -> programList[prIndex].codewrite t
        code()
    static member h1 (t:num0,atr:Style) = fun code ->
        html.tagb ("h1",atr) <| fun () -> php.echo (t.Expr.eval (programList[prIndex]))
        code()
    static member h1 (t:string,atr:Style) = fun code ->
        html.tagb ("h1",atr) <| fun () -> programList[prIndex].codewrite t
        code()
        
    static member h2 (t:num0) = fun code ->
        html.tagb "h2" <| fun () -> php.echo (t.Expr.eval (programList[prIndex]))
        code()
    static member h2 (t:string) = fun code ->
        html.tagb "h2" <| fun () -> programList[prIndex].codewrite t
        code()
    static member h2 (t:num0,atr:Style) = fun code ->
        html.tagb ("h2",atr) <| fun () -> php.echo (t.Expr.eval (programList[prIndex]))
        code()
    static member h2 (t:string,atr:Style) = fun code ->
        html.tagb ("h2",atr) <| fun () -> programList[prIndex].codewrite t
        code()
        
    static member h3 (t:num0) = fun code ->
        html.tagb "h3" <| fun () -> php.echo (t.Expr.eval (programList[prIndex]))
        code()
    static member h3 (t:string) = fun code ->
        html.tagb "h3" <| fun () -> programList[prIndex].codewrite(t)
        code()
    static member h3 (t:num0,atr:Style) = fun code ->
        html.tagb ("h3",atr) <| fun () -> php.echo (t.Expr.eval (programList[prIndex]))
        code()
    static member h3 (t:string,atr:Style) = fun code ->
        html.tagb ("h3",atr) <| fun () -> programList[prIndex].codewrite(t)
        code()

    static member form (action:string) = fun code -> html.tagb ("form",["method","\"post\""; "action","\""+action+"\"";]) code
    static member form_fileUpload (action:string) = fun code -> html.tagb ("form",["method","\"post\""; "enctype","\"multipart/form-data\""; "action","\""+action+"\"";]) code
    static member submit(name:string,value:num0) = html.taga("input",["type","\"submit\""; "name","\""+name+"\""; "value",(value.Expr.eval (programList[prIndex]))])
    static member submit(name:num0,value:string) = html.taga("input",["type",Str "submit"; "name", "\\\""++name++"\\\""; "value",Str value])
    static member submit(url:string,name:num0,value:string) = html.taga("input",["type",Str "submit"; "name", "\\\""++name++"\\\""; "value",Str value; "formaction",Str url])
    static member submit(name:string,value:string) = html.taga("input",["type","\"submit\""; "name","\""+name+"\""; "value","\""+value+"\""])
    static member submit(url:string,name:string,value:string) = html.taga("input",["type","\"submit\""; "name","\""+name+"\""; "value","\""+value+"\""; "formaction","\""+url+"\""])
    static member submit_disabled(name:string,value:num0) = html.taga("input",["type",Str "submit"; "name",Str name; "value",Nvr value.Expr; "disabled",Str "disabled"])
    static member submit_disabled(name:num0,value:string) = html.taga("input",["type",Str "submit"; "name", "\\\""++name++"\\\""; "value",Str value; "disabled",Str "disabled"])
    static member submit_disabled(name:string,value:string) = html.taga("input",["type","\"submit\""; "name","\""+name+"\""; "value","\""+value+"\""; "disabled","\"disabled\""])
    static member table_ code = html.tagb "table" code
    static member table (a:list<string*string>) code = html.tagb ("table",a) code
    static member tr_ = fun code -> html.tagb "tr" code
    static member tr (a:list<string*string>) = fun code -> html.tagb ("tr",a) code
    static member th = fun code -> html.tagb "th" code
    static member td_ = fun code -> html.tagb "td" code
    static member td (a:list<string*string>) = fun code -> html.tagb ("td",a) code
    static member strong(t:string) = html.tagb "strong" <| fun () -> programList[prIndex].codewrite(t)
    // static member image (s:Style) = fun filename -> programList[prIndex].codewrite("<img src=\""+filename+"\">")
    static member enumerate_ code = html.tagb "ol" code
    static member itemize_ code = html.tagb "ul" code
    static member itemize (a:list<string*string>) = fun code -> html.tagb ("ul",a) code
    static member item code = html.tagb "li" code
    static member item_ code = html.tagb "li" code
    static member item (a:list<string*num0>) = fun code -> html.tagb ("li",a) code
    static member item (a:list<string*string>) = fun code -> html.tagb ("li",a) code
    static member para_ code = html.tagb "p" code
    static member para (a:list<string*string>) = html.tagb ("p",a)
    static member para (t:string) = html.tagb "p" <| fun () -> programList[prIndex].codewrite(t)
    static member span(cls:string,t) = html.tagb ("span",["class","\""+cls+"\""]) <| fun () -> programList[prIndex].codewrite(t)
    static member span(cls:string) = fun code -> html.tagb ("span",["class","\""+cls+"\""]) code
    static member link(url:num0) = fun code -> html.tagb ("a",["href","\""+url.code+"\"";]) code
    static member link(url:string) = fun code -> html.tagb ("a",["href","\""+url+"\"";]) code
    static member link_newtab(url:string) = fun code -> html.tagb ("a",["href","\""+url+"\""; "target","\"_blank\""]) code
    static member select(x:num0) = fun code -> html.tagb ("select",["name",x.code;]) code
    static member select(x:string) = fun code -> html.tagb ("select",["name","\""+x+"\"";]) code
    static member select_disabled(x:num0) = fun code -> html.tagb ("select",["name",x.code; "disabled","\"disabled\""]) code
    static member select_disabled(x:string) = fun code -> html.tagb ("select",["name","\""+x+"\""; "disabled","\"disabled\""]) code
    static member splitTag t code = 
        let b (lst:list<string*num0>) =
            if lst.Length=0 then
                programList[prIndex].codewrite ("<"+t+">")
            else
                programList[prIndex].codewrite("<"+t+" ")
                for (a,s) in lst do
                    programList[prIndex].codewrite(a + "=" + s.code + " ")
                programList[prIndex].codewrite ">"
        code b 
        programList[prIndex].codewrite ("</"+t+">")
    static member Select = html.splitTag "select" 
    static member Tr = html.splitTag "tr" 
    static member option(value:string) = fun code -> html.tagb ("option",["value","\""+value+"\"";]) code
    static member option_selected(value:string) = fun code -> html.tagb ("option",["value","\""+value+"\"";"selected","\"selected\"";]) code
    static member div (a:list<string*num0>) = fun code -> html.tagb ("div",a) code
    static member div (a:list<string*string>) = fun code -> html.tagb ("div",a) code
    static member button(value:string,onclick:string) = html.taga("input",["type","\"button\""; "value","\""+value+"\""; "onclick","\""+onclick+"\"";])
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
    static member bold code = html.tagb "b" code
    static member align code =
        programList[prIndex].codewrite "\\[\\begin{align}"
        code()
        programList[prIndex].codewrite "\\end{align}\\]"
    static member eq q = "\\("+q+"\\)"

    static member latexTag (tagname:string) code =
        programList[prIndex].codewrite("\\begin{"+tagname+"}")
        code()
        programList[prIndex].codewrite("\\end{"+tagname+"}")
    static member listTable (caption:string) = fun (borderH:list<BorderH>) (borderV:list<BorderV>) (tlist:list<list<string>>) ->
        html.tagb("div",["class","\"fig\""]) <| fun () ->
            html.tagb ("span",["class","\"caption\""]) <| fun () ->
                programList[prIndex].codewrite(caption)
            html.tagb("table",["class","\"tab\""]) <| fun () ->
                for j in 0..tlist.Length-1 do
                    html.tagb ("tr",["class",match borderV[j] with |TB -> "\"trtb\"" |T -> "\"trt\"" |B -> "\"trb\"" |N -> "\"trn\""]) <| fun () ->
                        for i in 0..tlist[j].Length-1 do
                            html.tagb ("td",["class",match borderH[i] with |L -> "\"tdl\"" |BorderH.C -> "\"tdc\"" |BorderH.R -> "\"tdr\"" |J -> "\"tdj\"" |Ll -> "\"tdlL\"" |Cl -> "\"tdcL\"" |Rl -> "\"tdrL\"" |Jl -> "\"tdjL\"" |Lr -> "\"tdlR\"" |Cr -> "\"tdcR\"" |Rr -> "\"tdrR\"" |Jr -> "\"tdjR\"" |Llr -> "\"tdlLR\"" |Clr -> "\"tdcLR\"" |Rlr -> "\"tdrLR\"" |Jlr -> "\"tdjLR\""]) <| fun () ->
                                programList[prIndex].codewrite <| tlist[j][i]
                                
    static member footer code = html.tagb ("footer", ["class","footer"]) <| fun () -> code()
    static member footer (s:Style) = fun code -> html.tagb ("footer", s) <| fun () -> code()
    static member br() = programList[prIndex].codewrite "<br>"
    static member hr() = programList[prIndex].codewrite "<hr>"
    
    // /// タグの生成
    // static member programList[prIndex].codewriteTag tagname attr code =
    //     programList[prIndex].codewrite("<"+tagname+(if attr="" then "" else " "+attr)+">")
    //     code()
    //     programList[prIndex].codewrite("</"+tagname+">")
        
    // /// タグの生成
    // static member programList[prIndex].codewriteTag_ tagname attr =
    //     programList[prIndex].codewrite("<"+tagname+(if attr="" then "" else " "+attr)+"/>")
        
    static member setjs filename =
        html.tagb ("script",[Atr("src",filename)]) <| fun () -> ()
        
    static member title (s:Style) (p:position) (text:string) =
        let s1 = Style [{Key = "margin-left"; Value = p.x.ToString()+"px";}
                        {Key = "margin-top"; Value = p.y.ToString()+"px";}
                        {Key = "position"; Value = "absolute";}
                        {Key = "font-family"; Value = "'Noto Sans JP'";}
                        {Key = "color"; Value = "black";}
                        {Key = "font-weight"; Value = "bold";}
                        {Key = "white-space"; Value = "nowrap";}]
        html.tagb ("div",s1+s) <| fun () ->
            programList[prIndex].codewrite text
            
    static member Mathtext (s:Style) (p:position) (text:num0) =
        let s1 = Style [{Key = "margin-left"; Value=p.x.ToString()+"px"}
                        {Key = "margin-top"; Value=p.y.ToString()+"px"}
                        {Key = "white-space"; Value="nowrap"}]
        html.tagb ("div", s1+s) <| fun () ->
            programList[prIndex].codewrite ("\\(" + text.Expr.evalL programList[prIndex] + "\\)")
            
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
        
    static member para code =
        html.tagb "p" code
        
    static member canvas (s:Style) code =
        html.tagb ("div", s) <| fun () ->
            let c = canvas()
            code c
            
    static member div (s:Style) = fun code ->
        html.tagb ("div", s) code
        
    static member div (s:list<Atr>) = fun code ->
        html.tagb ("div", s) code
        
    static member tag (tagname:string) (s:string) code =
        html.tagb (tagname, s) code
        
    static member tag_ (tagname:string) (s:string) =
        html.taga (tagname, s)
        
    static member eq(text:num0) =
        programList[prIndex].codewrite ("\\("+text.Expr.evalL programList[prIndex] + "\\)")
        
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
            html.tag "div" <| "id = \"sb"+anicounter.ToString()+"\" style=\"width: 1880px; height: 160px; position: absolute; z-index: 1; margin-top: 880px; padding: 20px; background-color: #aaaaff; font-family: 'Noto Sans JP'; font-size: 36pt; font-weight: 800; text-shadow: 0 1px 0 #fff, 1px 0 0 #fff, 0 -1px 0 #fff, -1px 0 0 #fff, -1px -1px 0 #fff, 1px -1px 0 #fff, -1px 1px 0 #fff, 1px 1px 0 #fff \";" <| fun () ->
                ()
            // キャラクター画像
            html.tag "div" <| "id = \"c"+anicounter.ToString()+"\"" <| fun () ->
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
                html.tag "div" <| "id = \"s"+anicounter.ToString()+"\" style=\"width: 1880px; height: 160px; position: absolute; z-index: 5; margin-top: 880px; padding: 20px; font-family: 'Noto Sans JP'; color: #d11aff; font-size: 36pt; font-weight: 800; text-shadow: 0 1px 0 #fff, 1px 0 0 #fff, 0 -1px 0 #fff, -1px 0 0 #fff, -1px -1px 0 #fff, 1px -1px 0 #fff, -1px 1px 0 #fff, 1px 1px 0 #fff ;\""
                    <| fun () -> programList[prIndex].codewrite audio.Subtitle.Subtitle
            |Character.Dang -> 
                html.tag "div" <| "id = \"s"+anicounter.ToString()+"\" style=\"width: 1880px; height: 160px; position: absolute; z-index: 5; margin-top: 880px; padding: 20px; font-family: 'Noto Sans JP'; color: #455eff; font-size: 36pt; font-weight: 800; text-shadow: 0 1px 0 #fff, 1px 0 0 #fff, 0 -1px 0 #fff, -1px 0 0 #fff, -1px -1px 0 #fff, 1px -1px 0 #fff, -1px 1px 0 #fff, 1px 1px 0 #fff ;\""
                    <| fun () -> programList[prIndex].codewrite audio.Subtitle.Subtitle
            |Character.Armi -> 
                html.tag "div" <| "id = \"s"+anicounter.ToString()+"\" style=\"width: 1880px; height: 160px; position: absolute; z-index: 5; margin-top: 880px; padding: 20px; font-family: 'Noto Sans JP'; color: #ff8800; font-size: 36pt; font-weight: 800; text-shadow: 0 1px 0 #fff, 1px 0 0 #fff, 0 -1px 0 #fff, -1px 0 0 #fff, -1px -1px 0 #fff, 1px -1px 0 #fff, -1px 1px 0 #fff, 1px 1px 0 #fff ;\""
                    <| fun () -> programList[prIndex].codewrite audio.Subtitle.Subtitle
                    
    static member fig (p:position) code =
        let f = figure()
        code(f,p)
        let sx,sy,mx,my = f.setWriteMode()
        programList[prIndex].codewrite (
            "<svg viewBox=\"0 0 "+sx.ToString()+" "+sy.ToString()+"\" "+
            "width=\""+sx.ToString()+"px\" "+
            "heigth=\""+sy.ToString()+"px\" "+
            "xmlns=\"http://www.w3.org/2000/svg\" "+
            "style=\"margin-left: "+mx.ToString()+"; "+
            "margin-top: "+my.ToString()+"; "+
            "position: absolute;"+
            "\">")
        code(f,p)
        programList[prIndex].codewrite "</svg>"
        
    static member animation (s:ViewBoxStyle) (p:position) code =
        figcounter <- figcounter + 1
        let f = FigureAnimation figcounter
        programList[prIndex].codewrite (
            "<svg viewBox=\"0 0 "+s.sX.ToString()+" "+s.sY.ToString()+"\" "+
            "width=\""+s.sX.ToString()+"px\" "+
            "heigth=\""+s.sY.ToString()+"px\" "+
            "xmlns=\"http://www.w3.org/2000/svg\" "+
            "style=\"margin-left: "+s.mX.ToString()+"; "+
            "margin-top: "+s.mY.ToString()+"; "+
            "position: absolute;"+
            "background-color: "+s.backgroundColor+";"+
            "\">")
        code(f,p)
        programList[prIndex].codewrite "</svg>"
        
    static member slide (p:position)  code =
            anicounter <- anicounter + 1
            html.tagb ("div", "id=\"p"+anicounter.ToString()+"\" style=\"visibility: "+(if anicounter=1 then "visible" else "hidden")+"; position: absolute;\"") <| fun wr ->
                code p
                
    static member prevButton() =
            html.tagb ("button", "id=\"prevButton\" style=\"position: absolute; z-index: 100;\" onclick=\"drawPrev()\"") <| fun () ->
                programList[prIndex].codewrite "前へ"
    static member nextButton() =
            html.tagb ("button", "id=\"nextButton\" style=\"position: absolute; margin-left: 75px; z-index: 100;\" onclick=\"drawNext()\"") <| fun () ->
                programList[prIndex].codewrite "次へ"
                
    static member switchCharacter() =
        html.taga ("input", "type=\"checkbox\" id=\"switchCharacter\" style=\"position: absolute; margin-top: 6px; margin-left: 150px; z-index: 100;\"  onclick=\"setCharacter()\" checked")
        html.tagb ("label", "style=\"position: absolute; margin-top: 0px; margin-left: 165px; z-index: 100;\"") <| fun () ->
            programList[prIndex].codewrite "キャラクター"
    static member switchSubtitle() =
        html.taga ("input", "type=\"checkbox\" id=\"switchSubtitle\" style=\"position: absolute; margin-top: 6px; margin-left: 270px; z-index: 100;\" onclick=\"setSubtitle()\" checked")
        html.tagb ("label", "style=\"position: absolute; margin-top: 0px; margin-left: 285px; z-index: 100;\"") <| fun () ->
            programList[prIndex].codewrite "字幕"
    static member switchAudio() =
        html.taga ("input", "type=\"checkbox\" id=\"switchAudio\" style=\"position: absolute; margin-top: 6px; margin-left: 330px; z-index: 100;\" onclick=\"setSubtitle()\" checked")
        html.tagb ("label", "style=\"position: absolute; margin-top: 0px; margin-left: 345px; z-index: 100;\"") <| fun () ->
            programList[prIndex].codewrite "音声"
    static member audioPlayer() =
            html.tagb ("audio", "id=\"audioPlayer\"")  <| fun () -> ()
            
and SlideAnimation =
    static member writeAudioList() =
        switchDraw <| fun () ->
            programList[prIndex].codewrite "const audioList = ["
            for i in 0..audioList.Length-1 do
                programList[prIndex].codewrite ("    \""+audioList[i] + "\"" + if i<audioList.Length-1 then "," else "")
            programList[prIndex].codewrite "];"
    static member jsSetCharacter() =
        switchDraw <| fun () ->
            programList[prIndex].codewrite "let pagecount = 1;"
            programList[prIndex].codewrite"function setCharacter()"
            programList[prIndex].codewrite"{"
            programList[prIndex].codewrite"        const swc = document.getElementById(\"switchCharacter\");"
            programList[prIndex].codewrite"        const c = document.getElementById(\"c\"+pagecount);"
            programList[prIndex].codewrite"        if(swc.checked)"
            programList[prIndex].codewrite"        {"
            programList[prIndex].codewrite"            c.style.visibility = \"visible\";"
            programList[prIndex].codewrite"        }"
            programList[prIndex].codewrite"        else"
            programList[prIndex].codewrite"        {"
            programList[prIndex].codewrite"            c.style.visibility = \"hidden\";"
            programList[prIndex].codewrite"        }"
            programList[prIndex].codewrite"}"
    static member jsSetSubtitle() =
        switchDraw <| fun () ->
            programList[prIndex].codewrite "function setSubtitle()"
            programList[prIndex].codewrite "{"
            programList[prIndex].codewrite "        const sws = document.getElementById(\"switchSubtitle\");"
            programList[prIndex].codewrite "        const b2 = document.getElementById(\"sb\"+pagecount);"
            programList[prIndex].codewrite "        const s2 = document.getElementById(\"s\"+pagecount);"
            programList[prIndex].codewrite "        if(sws.checked)"
            programList[prIndex].codewrite "        {"
            programList[prIndex].codewrite "            b2.style.visibility = \"visible\";"
            programList[prIndex].codewrite "            s2.style.visibility = \"visible\";"
            programList[prIndex].codewrite "        }"
            programList[prIndex].codewrite "        else"
            programList[prIndex].codewrite "        {"
            programList[prIndex].codewrite "            b2.style.visibility = \"hidden\";"
            programList[prIndex].codewrite "            s2.style.visibility = \"hidden\";"
            programList[prIndex].codewrite "        }"
            programList[prIndex].codewrite "}"
    static member jsDrawNext() =
        switchDraw <| fun () ->
            programList[prIndex].codewrite "function drawNext()"
            programList[prIndex].codewrite "{"
            programList[prIndex].codewrite("    if(pagecount<"+anicounter.ToString()+")")
            programList[prIndex].codewrite "    {"
            programList[prIndex].codewrite "        const swc = document.getElementById(\"switchCharacter\");"
            programList[prIndex].codewrite "        const sws = document.getElementById(\"switchSubtitle\");"
            programList[prIndex].codewrite "        const swa = document.getElementById(\"switchAudio\");"
            programList[prIndex].codewrite "        "
            programList[prIndex].codewrite "        const p1 = document.getElementById(\"p\"+pagecount);"
            programList[prIndex].codewrite "        p1.style.visibility = \"hidden\";"
            programList[prIndex].codewrite "        const b1 = document.getElementById(\"sb\"+pagecount);"
            programList[prIndex].codewrite "        b1.style.visibility = \"hidden\";"
            programList[prIndex].codewrite "        const s1 = document.getElementById(\"s\"+pagecount);"
            programList[prIndex].codewrite "        s1.style.visibility = \"hidden\";"
            programList[prIndex].codewrite "        const c1 = document.getElementById(\"c\"+pagecount);"
            programList[prIndex].codewrite "        c1.style.visibility = \"hidden\";"
            programList[prIndex].codewrite "        pagecount++;"
            programList[prIndex].codewrite "        const p2 = document.getElementById(\"p\"+pagecount);"
            programList[prIndex].codewrite "        p2.style.visibility = \"visible\";"
            programList[prIndex].codewrite "        if(sws.checked)"
            programList[prIndex].codewrite "        {"
            programList[prIndex].codewrite "            const b2 = document.getElementById(\"sb\"+pagecount);"
            programList[prIndex].codewrite "            b2.style.visibility = \"visible\";"
            programList[prIndex].codewrite "            const s2 = document.getElementById(\"s\"+pagecount);"
            programList[prIndex].codewrite "            s2.style.visibility = \"visible\";"
            programList[prIndex].codewrite "        }"
            programList[prIndex].codewrite "        else"
            programList[prIndex].codewrite "        {"
            programList[prIndex].codewrite "            const b2 = document.getElementById(\"sb\"+pagecount);"
            programList[prIndex].codewrite "            b2.style.visibility = \"hidden\";"
            programList[prIndex].codewrite "            const s2 = document.getElementById(\"s\"+pagecount);"
            programList[prIndex].codewrite "            s2.style.visibility = \"hidden\";"
            programList[prIndex].codewrite "        }"
            programList[prIndex].codewrite "        if(swc.checked)"
            programList[prIndex].codewrite "        {"
            programList[prIndex].codewrite "            const c2 = document.getElementById(\"c\"+pagecount);"
            programList[prIndex].codewrite "            c2.style.visibility = \"visible\";"
            programList[prIndex].codewrite "        }"
            programList[prIndex].codewrite "        else"
            programList[prIndex].codewrite "        {"
            programList[prIndex].codewrite "            const c2 = document.getElementById(\"c\"+pagecount);"
            programList[prIndex].codewrite "            c2.style.visibility = \"hidden\";"
            programList[prIndex].codewrite "        }"
            programList[prIndex].codewrite "        const audioPlayer = document.getElementById(\"audioPlayer\");"
            programList[prIndex].codewrite "        if(audioList[pagecount-1] != \"\" && swa.checked)"
            programList[prIndex].codewrite "        {"
            programList[prIndex].codewrite "            audioPlayer.src = audioList[pagecount-1];"
            programList[prIndex].codewrite "            audioPlayer.play();"
            programList[prIndex].codewrite "        }"
            programList[prIndex].codewrite "    }"
            programList[prIndex].codewrite "}"
    static member jsDrawPrev() =
        switchDraw <| fun () ->
            programList[prIndex].codewrite "function drawPrev()"
            programList[prIndex].codewrite "{"
            programList[prIndex].codewrite "    if(pagecount>1)"
            programList[prIndex].codewrite "    {"
            programList[prIndex].codewrite "        const swc = document.getElementById(\"switchCharacter\");"
            programList[prIndex].codewrite "        const sws = document.getElementById(\"switchSubtitle\");"
            programList[prIndex].codewrite "        const swa = document.getElementById(\"switchAudio\");"
            programList[prIndex].codewrite "        const p1 = document.getElementById(\"p\"+pagecount);"
            programList[prIndex].codewrite "        p1.style.visibility = \"hidden\";"
            programList[prIndex].codewrite "        const b1 = document.getElementById(\"sb\"+pagecount);"
            programList[prIndex].codewrite "        b1.style.visibility = \"hidden\";"
            programList[prIndex].codewrite "        const s1 = document.getElementById(\"s\"+pagecount);"
            programList[prIndex].codewrite "        s1.style.visibility = \"hidden\";"
            programList[prIndex].codewrite "        const c1 = document.getElementById(\"c\"+pagecount);"
            programList[prIndex].codewrite "        c1.style.visibility = \"hidden\";"
            programList[prIndex].codewrite "        pagecount--;"
            programList[prIndex].codewrite "        const p2 = document.getElementById(\"p\"+pagecount);"
            programList[prIndex].codewrite "        p2.style.visibility = \"visible\";"
            programList[prIndex].codewrite "        if(sws.checked)"
            programList[prIndex].codewrite "        {"
            programList[prIndex].codewrite "            const b2 = document.getElementById(\"sb\"+pagecount);"
            programList[prIndex].codewrite "            b2.style.visibility = \"visible\";"
            programList[prIndex].codewrite "            const s2 = document.getElementById(\"s\"+pagecount);"
            programList[prIndex].codewrite "            s2.style.visibility = \"visible\";"
            programList[prIndex].codewrite "        }"
            programList[prIndex].codewrite "        else"
            programList[prIndex].codewrite "        {"
            programList[prIndex].codewrite "            const b2 = document.getElementById(\"sb\"+pagecount);"
            programList[prIndex].codewrite "            b2.style.visibility = \"hidden\";"
            programList[prIndex].codewrite "            const s2 = document.getElementById(\"s\"+pagecount);"
            programList[prIndex].codewrite "            s2.style.visibility = \"hidden\";"
            programList[prIndex].codewrite "        }"
            programList[prIndex].codewrite "        if(swc.checked)"
            programList[prIndex].codewrite "        {"
            programList[prIndex].codewrite "            const c2 = document.getElementById(\"c\"+pagecount);"
            programList[prIndex].codewrite "            c2.style.visibility = \"visible\";"
            programList[prIndex].codewrite "        }"
            programList[prIndex].codewrite "        else"
            programList[prIndex].codewrite "        {"
            programList[prIndex].codewrite "            const c2 = document.getElementById(\"c\"+pagecount);"
            programList[prIndex].codewrite "            c2.style.visibility = \"hidden\";"
            programList[prIndex].codewrite "        }"
            programList[prIndex].codewrite "        const audioPlayer = document.getElementById(\"audioPlayer\");"
            programList[prIndex].codewrite "        if(audioList[pagecount-1] != \"\" && swa.checked)"
            programList[prIndex].codewrite "        {"
            programList[prIndex].codewrite "            audioPlayer.src = audioList[pagecount-1];"
            programList[prIndex].codewrite "            audioPlayer.play();"
            programList[prIndex].codewrite "        }"
            programList[prIndex].codewrite "    }"
            programList[prIndex].codewrite "}"
            
and canvas() =
    
    member _.Mathtext (s:Style) (p:position) (text:num0) =
        switchBody <| fun () ->
            // html.pr.codewriteTag "div" ("style = \"font-size:"+s.size.ToString()+ "px;margin-left:"+p.x.ToString()+"px; margin-top:"+p.y.ToString()+"px; white-space: nowrap;\" id=\"contents\"") <| fun () ->
            let s1 = Style [{Key = "margin-left"; Value = p.x.ToString()+"px";}
                            {Key = "margin-top"; Value = p.y.ToString()+"px";}
                            {Key = "position"; Value = "absolute";}]
            html.tagb ("div", s1+s) <| fun () ->
                programList[prIndex].codewrite ("\\(" + text.Expr.evalL programList[prIndex] + "\\)")
            
    member _.image (s:Style) (p:position) (filename:string) =
        switchBody <| fun () ->
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
        
    member _.text (s:Style) = fun (p:position) (text:string) ->
        switchBody <| fun () ->
            // html.pr.codewriteTag "div" ("style = \"font-size:"+s.size.ToString()+ "px; margin-left:"+p.x.ToString()+"px; margin-top:"+p.y.ToString()+"px; color: "+s.color+"; white-space: nowrap; position: absolute;\"") <| fun () ->
            let s1 = Style [{Key = "margin-left"; Value = p.x.ToString()+"px";}
                            {Key = "margin-top"; Value = p.y.ToString()+"px";}
                            {Key = "position"; Value = "absolute";}]
            html.tagb ("div", s1+s) <| fun () ->
                programList[prIndex].codewrite text
                
    // member _.text (s:list<Atr>) = fun (p:position) (id:string) (text:string) ->
    //     // html.pr.codewriteTag "div" ("id = \""+id+"\" style = \"font-size:"+s.size.ToString()+ "px; margin-left:"+p.x.ToString()+"px; margin-top:"+p.y.ToString()+"px; color: "+s.color+"; white-space: nowrap; position: absolute;\"") <| fun () ->
    //     html.pr.codewriteTag "div" (Atr.list s) <| fun () ->
    //         programList[prIndex].codewrite text
            
    member _.blockText (s:Style) (p:position) (width:float,height:float) (text:list<string>) =
        switchBody <| fun () ->
            let padding = 5
            // html.pr.codewriteTag "div" 
            //     ("style = \"font-size: "+s.size.ToString()+"px;"+
            //         "margin-left: "+p.x.ToString()+"px; "+
            //         "margin-top: "+p.y.ToString()+"px; "+
            //         "position: absolute; "+
            //         (match s.weight with |Some c -> "font-weight: "+c+"; " |None -> "")+
            //         (match s.textcolor with |Some c -> "color: "+c+"; " |None -> "")+
            //         (match s.bgcolor with |Some c -> "background-color: "+c+"; " |None -> "")+
            //         "width: "+width.ToString()+"px; "+
            //         "height: "+height.ToString()+"px; "+
            //         (match s.border with |Some c -> "border: "+c+"; " |None -> "")+
            //         (match s.align with 
            //             |Center -> "display: flex; align-items: center; justify-content: center; "
            //             |Left -> "")+
            //         "overflow-wrap: break-word; "+
            //         "font-family: 'Noto Sans JP',monospace; "+
            //         "padding: "+padding.ToString()+"px; "+
            //         "line-height: 14px;\"") <| fun () ->
            
            let s1 = Style [size.width (width.ToString()+"px")
                            size.height (height.ToString()+"px")
                            {Key = "margin-left"; Value = p.x.ToString()+"px";}
                            {Key = "margin-top"; Value = p.y.ToString()+"px";}
                            {Key = "position"; Value = "absolute";}
                            {Key = "overflow-wrap"; Value = "break-word";}]
            html.tagb ("div", s1+s) <| fun () ->
                text |> List.iter (fun s -> programList[prIndex].codewrite (s+"<br>"))
                programList[prIndex].codewrite("\r\n")
            {Left = p.x;
             Right = p.x+double width+2.0*double padding;
             Top = p.y;
             Bottom = p.y+double height+2.0*double padding;}
             
    member _.blockTextcode (s:Style) (p:position) (width:float,height:float) (text:list<string>) =
        switchBody <| fun () ->
            let padding = 5
            // html.pr.codewriteTag "div" 
            //     ("style = \"font-size: "+s.size.ToString()+"px;"+
            //         "margin-left: "+p.x.ToString()+"px; "+
            //         "margin-top: "+p.y.ToString()+"px; "+
            //         "position: absolute; "+
            //         (match s.weight with |Some c -> "font-weight: "+c+"; " |None -> "")+
            //         (match s.textcolor with |Some c -> "color: "+c+"; " |None -> "")+
            //         (match s.bgcolor with |Some c -> "background-color: "+c+"; " |None -> "")+
            //         "width: "+width.ToString()+"px; "+
            //         "height: "+height.ToString()+"px; "+
            //         (match s.border with |Some c -> "border: "+c+"; " |None -> "")+
            //         (match s.align with 
            //             |Center -> "display: flex; align-items: center; justify-content: center; "
            //             |Left -> "")+
            //         "overflow-wrap: break-word; "+
            //         "font-family: 'Noto Sans Mono',monospace; "+
            //         "padding: "+padding.ToString()+"px; "+
            //         "line-height: 14px;\"")
            let s1 = Style [size.width (width.ToString()+"px")
                            size.height (height.ToString()+"px")
                            font.family "'Noto Sans Mono',monospace"
                            {Key = "margin-left"; Value = p.x.ToString()+"px";}
                            {Key = "margin-top"; Value = p.y.ToString()+"px";}
                            {Key = "position"; Value = "absolute";}
                            {Key = "overflow-wrap"; Value = "break-word";}]
            html.tagb ("div", s1+s)
                <| fun () ->
                    text |> List.iter (fun s -> programList[prIndex].codewrite (s+"<br>"))
                    programList[prIndex].codewrite "\r\n"
            {Left = p.x;
             Right = p.x+double width+2.0*double padding;
             Top = p.y;
             Bottom = p.y+double height+2.0*double padding;}
             
    member _.table (lst:list<list<string>>) (p:position) (size:int) =
        switchBody <| fun () ->
            programList[prIndex].codewrite ("<table style =\"margin-left: "+p.x.ToString()+"px; margin-top: "+p.y.ToString()+"px; font-size: "+size.ToString()+"px; position: absolute;\">")
            for m in 0..lst.Length-1 do
                programList[prIndex].codewrite "<tr>"
                for s in lst[m] do
                    programList[prIndex].codewrite "<td>"
                    programList[prIndex].codewrite s
                    programList[prIndex].codewrite "</td>"
                programList[prIndex].codewrite "</tr>"
            programList[prIndex].codewrite "</table>"
            programList[prIndex].codewrite "</div>"
            
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
    member this.line (s:Style) (startP:position) (endP:position) =
        if writeMode then
            html.taga ("line", [
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
            this.updateRange startP
            this.updateRange endP
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
            this.updateRange startP
            this.updateRange endP
            this.updateRange(position(q1x,q1y))
            this.updateRange(position(q2x,q2y))
        this.polyline s [position(q1x,q1y);endP;position(q2x,q2y)]
        
and tposition = {X:num0->num0; Y:num0->num0}

and FigureAnimation(figcounter:int) =
    let padding = 10.0
    let mutable counter = 0
    member _.Padding with get() = padding
    member _.id with get() = "fa"+figcounter.ToString()+"_"+counter.ToString()
    member this.line (s:Style) (startP:tposition) (endP:tposition) =
        counter <- counter + 1
        html.taga ("line", [Atr("id",this.id)]@[s.atr])
        let t = num0(Var(It 4, "t", NaN))
        switchJS <| fun () ->
            programList[prIndex].codewrite("var e = document.getElementById(\""+this.id+"\");")
            programList[prIndex].codewrite("e.setAttribute(\"x1\", " + (startP.X t).Expr.eval (programList[prIndex]) + ");")
            programList[prIndex].codewrite("e.setAttribute(\"y1\", " + (startP.Y t).Expr.eval (programList[prIndex]) + ");")
            programList[prIndex].codewrite("e.setAttribute(\"x2\", " + (endP.X t).Expr.eval (programList[prIndex]) + ");")
            programList[prIndex].codewrite("e.setAttribute(\"y2\", " + (endP.Y t).Expr.eval (programList[prIndex]) + ");")
    static member jsAnimation codejs =
        switchBody <| fun () ->
            programList[prIndex].codewrite "var t = 0;"
            programList[prIndex].codewrite "var dt = 1;"
            programList[prIndex].codewrite "window.onload=function(){"
            programList[prIndex].codewrite "    var timer;"
            programList[prIndex].codewrite "    var delay = 33;"
            programList[prIndex].codewrite "    var loop = function(){"
            programList[prIndex].codewrite "        t = t + dt;"
            programList[prIndex].codewrite "        if(t >= 100){t = 0;}"
            programList[prIndex].codewrite codejs
            programList[prIndex].codewrite "        clearTimeout(timer);"
            programList[prIndex].codewrite "        timer=setTimeout(loop,delay);"
            programList[prIndex].codewrite "    }"
            programList[prIndex].codewrite "    loop();"
            programList[prIndex].codewrite "}"
            
[<AutoOpen>]
module dochtml =
    let htmlpresentation dir filename (pagesizeX:option<int>,pagesizeY:option<int>) isPageAnimation code =
        // ディレクトリ作成
        if not <| Directory.Exists (dir + "\\" + filename) then
            ignore <| Directory.CreateDirectory(dir + "\\" + filename)
        if not <| Directory.Exists (dir + "\\" + filename + "\\" + "contents_" + filename) then
            ignore <| Directory.CreateDirectory(dir + "\\" + filename + "\\" + "contents_" + filename)
        // head、body要素書き込みストリーム作成
        // head、body要素書き込み
        /// draw関数のコード出力先ファイル名
        let filename_main = filename + "//" + filename + ".html"
        /// draw関数のコード出力先ファイル名
        let filename_draw = filename + "\\" + filename+"_draw"
        /// HTML本体のコード出力先ファイル名
        let filename_body = filename + "\\" + filename+"_body"
        /// スライドアニメーション用javascriptファイル名
        let filename_js = filename + "\\" + filename+"_js"
        makeProgram 
            [
                dir,filename_main,HTML;
                dir,filename_body,HTML;
                dir,filename_js,JavaScript;
                dir,filename_draw,JavaScript;
            ]
            <| fun () ->
                // 出力先ディレクトリ
                outputDir <- dir + "\\" + filename
                // コンテンツディレクトリ
                contentsDir <- "contents_" + filename
                code()
                if isPageAnimation then
                    SlideAnimation.writeAudioList()
                    SlideAnimation.jsSetCharacter()
                    SlideAnimation.jsSetSubtitle()
                    SlideAnimation.jsDrawNext()
                    SlideAnimation.jsDrawPrev()
                // head、body要素書き込みストリームを閉じてhead、body要素のコード取得
                let codeDraw = 
                    switchDraw <| fun () ->
                        programList[prIndex].close()
                        programList[prIndex].allCodes
                let codeJS = 
                    switchJS <| fun () ->
                        programList[prIndex].close()
                        programList[prIndex].allCodes
                let codeBody = 
                    switchBody <| fun () ->
                        programList[prIndex].close()
                        programList[prIndex].allCodes
                // html書き込みストリーム作成
                switchMain <| fun () ->
                    programList[prIndex].codewrite "<!DOCTYPE html>"
                    // html要素
                    html.tagb ("html", "lang=\"ja\"") <| fun () ->
                        // head要素
                        html.tagb ("head", "") <| fun () ->
                            // metaタグ
                            programList[prIndex].codewrite "<meta charset=\"UTF-8\">"    
                            // titleタグ
                            html.tagb ("title", "") <| fun () ->
                                programList[prIndex].codewrite filename
                            // MathJax
                            html.tagb ("script", "type=\"text/javascript\" id=\"MathJax-script\" async src=\"https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml.js\"") <| fun () -> ()
                            // scriptタグ
                            html.tagb ("script", "") <| fun () ->
                                programList[prIndex].codewrite codeDraw
                                if codeJS <> "" then
                                    FigureAnimation.jsAnimation codeJS
                            // webフォント取得
                            programList[prIndex].codewrite "<link rel=\"preconnect\" href=\"https://fonts.googleapis.com\">"
                            programList[prIndex].codewrite "<link rel=\"preconnect\" href=\"https://fonts.gstatic.com\" crossorigin>"
                            programList[prIndex].codewrite "<link href=\"https://fonts.googleapis.com/css2?family=Noto+Sans+JP:wght@100..900&display=swap\" rel=\"stylesheet\">"
                        // body要素
                        let s0 = Style [area.backGroundColor "#aaaaaa"]
                        match pagesizeX,pagesizeY with
                        |None,None ->
                            let s = Style [
                                font.family "'Noto Sans JP', sans-serif"
                                font.weight "500"
                                font.size 16]
                            html.tagb ("body", [Atr("onload","draw()");s.atr]) <| fun () ->
                                programList[prIndex].codewrite codeBody
                        |Some x,None ->
                            html.tagb ("body", s0) <| fun () ->
                                let s1 = Style [
                                    area.backGroundColor "#ffffff"
                                    margin.left "auto"
                                    margin.right "auto"
                                    size.width (x.ToString()+"px")]
                                html.tagb ("div", s1) <| fun () ->
                                    programList[prIndex].codewrite codeBody
                        |None,Some y->
                            html.tagb ("body", s0) <| fun () ->
                                let s1 = Style [
                                    area.backGroundColor "#ffffff"
                                    margin.left "auto"
                                    margin.right "auto"
                                    size.height (y.ToString()+"px")]
                                html.tagb ("div", s1) <| fun () ->
                                    programList[prIndex].codewrite codeBody
                        |Some x,Some y ->
                            html.tagb ("body", s0) <| fun () ->
                                let s1 = Style [
                                    area.backGroundColor "#ffffff"
                                    margin.left "auto"
                                    margin.right "auto"
                                    size.width (x.ToString()+"px")
                                    size.height (y.ToString()+"px")]
                                html.tagb ("div", s1) <| fun () ->
                                    programList[prIndex].codewrite codeBody
                                    
    let group (t:string) d code = code d
    
    /// 全体がキャンバスの無制限レイアウト
    let freeCanvas outputdir filename code =
        htmlpresentation outputdir filename (None, None) false <| fun () ->
            html.canvas <| Style [size.width "0px"; size.height "0px"] <| code
            
    /// 全体がキャンバスの無制限レイアウト
    let freePage outputdir filename code =
        htmlpresentation outputdir filename (None, None) false code
        
    /// 固定幅レイアウト
    let fixedWidthPage outputdir filename pageWidth code =
        htmlpresentation outputdir filename (Some pageWidth, None) false code
        
    let fixedPage outputdir filename pageWidth pageHeight code =
        htmlpresentation outputdir filename (Some pageWidth, Some pageHeight) true <| fun () ->
            code()
            html.prevButton()
            html.nextButton()
            html.switchCharacter()
            html.switchSubtitle()
            html.switchAudio()
            html.audioPlayer()
