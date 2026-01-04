// 
// Copyright (c) 2026 Jun-ichiro Sugisaka
// 
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
// 
namespace Aqualis
    
    open System
    open System.IO
    
    type Label = |WriteLabel of StreamWriter |ReadLabel of list<string*string>

    type TeXWriter(figlabel:Label,equlabel:Label,tablabel:Label,codelabel:Label,lang:Language,figdir:string) =
        let mutable equnum = 0
        let mutable fignum = 0
        let mutable tabnum = 0
        let mutable codenum = 0
        let mutable ftncounter = 0
        let mutable ftnnum = 0
        let mutable secnum = 0
        let mutable ssecnum = 0
        let mutable sssecnum = 0
        let mutable licheck = 0
        
        member _.write s = codewritein s
        
        member this.tag (tagname:string) code =
            match lang with
            |HTML ->
                codewritein("<"+tagname+">")
                code()
                codewritein("</"+tagname+">")
            |LaTeX ->
                codewritein("\\begin{"+tagname+"}")
                code()
                codewritein("\\end{"+tagname+"}")
            |_ -> ()
            
        member this.block lst code =
            match lang with
            |HTML ->
                codewritein "<div "
                for a,name in lst do
                    codewritein(a+"=\""+name+"\" ")
                codewritein ">"
                code()
                codewritein "</div>"
            |_ -> ()
        member this.form (name:string) code =
            match lang with
            |HTML ->
                codewritein("<form name=\""+name+"\">")
                code()
                codewritein "</form>"
            |_ -> ()
        member this.radioButton (name:string) lst =
            match lang with
            |HTML ->
                this.form ("f_"+name) <| fun () ->
                    for (a,b,c,d) in lst do
                        codewritein("<input type=\"radio\" name=\""+name+"\" value=\""+a+"\""+(if c then " checked" else "")+" "+d+">"+b)
            |_ -> ()
        member this.title txt =
            match lang with
            |HTML ->
                codewritein("<h1>"+txt+"</h1>")
            |LaTeX ->
                codewritein("\\MyTitle{"+txt+"}")
            |_ -> ()
        member this.section title code =
            secnum <- secnum + 1
            ssecnum <- 0
            sssecnum <- 0
            match lang with
            |HTML ->
                codewritein("<h2>" + secnum.ToString() + " "+title+"</h2>")
            |LaTeX ->
                codewritein("\\section{"+title+"}")
            |_ -> ()
            code()
        member this.subsection title code =
            ssecnum <- ssecnum + 1
            sssecnum <- 0
            match lang with
            |HTML ->
                codewritein("<h3>" + secnum.ToString() + "." + ssecnum.ToString() + " " + title+"</h3>")
            |LaTeX ->
                codewritein("\\subsection{"+title+"}")
            |_ -> ()
            code()
        member this.subsection_ title code =
            match lang with
            |HTML ->
                codewritein("<h3>" + title + "</h3>")
            |LaTeX ->
                codewritein("\\subsection*{"+title+"}")
            |_ -> ()
            code()
        member this.subsubsection title code =
            sssecnum <- sssecnum + 1
            match lang with
            |HTML ->
                codewritein("<h4>" + secnum.ToString() + "." + ssecnum.ToString() + "." + sssecnum.ToString() + " " + title+"</h4>")
            |LaTeX ->
                codewritein("\\subsubsection{"+title+"}")
            |_ -> ()
            code()
        member this.subsubsection_ title code =
            match lang with
            |HTML ->
                codewritein("<h4>" + title + "</h4>")
            |LaTeX ->
                codewritein("\\subsubsection*{"+title+"}")
            |_ -> ()
            code()
        member this.para code =
            match lang with
            |HTML ->
                ftncounter <- ftncounter + 1
                let filename = "footnote"+ftncounter.ToString()
                let wr = new StreamWriter(filename)
                let addfootnote(txt:string) =
                    ftnnum <- ftnnum + 1
                    codewritein("<sup><a name=\"xft"+ftnnum.ToString()+"\"><a href=\"#ft"+ftnnum.ToString()+"\">"+ftnnum.ToString()+")</a></a></sup>")
                    wr.WriteLine("<a name=\"ft"+ftnnum.ToString()+"\">"+ftnnum.ToString()+") <a href=\"#xft"+ftnnum.ToString()+"\">↑</a>　"+txt+"</a><br/>")
                codewritein "<p>"
                code addfootnote
                codewritein "</p>"
                wr.Close()
                codewritein "<div class=\"footnote\">"
                codewritein(File.ReadAllText filename)
                codewritein "</div>"
                File.Delete filename
            |LaTeX ->
                codewritein "\\par"
                code this.footnote
            |_ -> 
                ()
        member this.footnote(txt:string) =
            codewritein("\\footnote{"+txt+"}")
        member this.url(txt:string) =
            match lang with
            |LaTeX -> txt.Replace("_","\\_")
            |_ -> txt
        member this.table label tbalign elalign caption (lst:list<list<string>>) =
            match lang with
            |HTML ->
                codewritein "<div class=\"fig\">"
                codewritein("<span class=\"caption\">"+this.tabref(label)+"&emsp;"+caption+"</span>")
                codewritein "<table class=\"tab\">"
                for m in 0..lst.Length-1 do
                    codewritein "<tr>"
                    for s in lst[m] do
                        codewritein "<td>"
                        codewritein s
                        codewritein "</td>"
                    codewritein "</tr>"
                codewritein "</table>"
                codewritein "</div>"
            |LaTeX ->
                codewritein("\\begin{table}["+tbalign+"]")
                codewritein(" \\caption{"+caption+"}")
                codewritein(" \\label{"+label+"}")
                codewritein " \\centering"
                codewritein("  \\begin{tabular}{"+elalign+"}")
                for m in 0..lst.Length-1 do
                    if m=0 || m=1 then codewritein "\\hline "
                    for n in 0..lst[m].Length-1 do
                        codewritein(lst[m][n])
                        if n<lst[m].Length-1 then codewritein "&"
                    codewritein "\\\\"
                    if m=lst.Length-1 then codewritein "\\hline "
                codewritein "  \\end{tabular}"
                codewritein "\\end{table}"
            |_ -> ()
            match tablabel with
            |WriteLabel wr ->
                tabnum <- tabnum + 1
                wr.WriteLine(label + "," + tabnum.ToString())
            |ReadLabel _ ->
                ()
        member this.figref label =
            match lang,figlabel with
            |_,WriteLabel _ ->
                ""
            |LaTeX,ReadLabel _ ->
                "\\ref{"+label+"}"
            |HTML,ReadLabel lst ->
                let p = List.tryFind (fun (f:string,i:string) -> f=label) lst
                match p with
                |None ->
                    "図??"
                |Some(_,n) ->
                    "<a href=\"#"+label+"\">図"+n.ToString()+"</a>"
            |_ -> ""
        member this.figref_nolink(label) =
            match lang,figlabel with
            |_,WriteLabel _ ->
                ""
            |LaTeX,ReadLabel _ ->
                "\\ref{"+label+"}"
            |HTML,ReadLabel lst ->
                let p = List.tryFind (fun (f:string,i:string) -> f=label) lst
                match p with
                |None ->
                    "図??"
                |Some(_,n) ->
                    "図"+n.ToString()
            |_ -> ""
        member this.tabref label =
            match lang,tablabel with
            |_,WriteLabel _ ->
                ""
            |LaTeX,ReadLabel _ ->
                "\\ref{"+label+"}"
            |HTML,ReadLabel lst ->
                let p = List.tryFind (fun (f:string,i:string) -> f=label) lst
                match p with
                |None ->
                    "??"
                |Some(_,n) ->
                    n.ToString()
            |_ -> ""
        member this.equref label =
            match lang,equlabel with
            |_,WriteLabel _ ->
                ""
            |LaTeX,ReadLabel _ ->
                "\\ref{"+label+"}"
            |HTML,ReadLabel lst ->
                let p = List.tryFind (fun (f:string,i:string) -> f=label) lst
                match p with
                |None ->
                    "??"
                |Some(_,n) ->
                    n.ToString()
            |_ -> ""
        member this.coderef label =
            match lang,codelabel with
            |_,WriteLabel _ ->
                ""
            |LaTeX,ReadLabel _ ->
                "ソースコード\\ref{"+label+"}"
            |HTML,ReadLabel lst ->
                let p = List.tryFind (fun (f:string,i:string) -> f=label) lst
                match p with
                |None ->
                    "ソースコード??"
                |Some(_,n) ->
                    "ソースコード"+n.ToString()
            |_ -> ""
        member this.figure (filename:string) (caption:string) =
            match lang with
            |HTML ->
                codewritein "<div class=\"fig\">"
                codewritein("  <a name=\""+filename+"\">")
                codewritein("    <img src =\""+figdir+"/"+filename+".svg\" alt=\""+caption+"\">")
                codewritein("  </a>")
                codewritein("  <div class=\"caption\">"+this.figref_nolink filename+"&emsp;"+caption+"</div>")
                codewritein "</div>"
            |LaTeX ->
                codewritein("\\inputfigure{"+filename+"}{"+caption+"}")
            |_ -> ()
            match figlabel with
            |WriteLabel wr ->
                fignum <- fignum + 1
                wr.WriteLine(filename + "," + fignum.ToString())
            |ReadLabel lst ->
                ()
        member this.graphics (filename:string) =
            match lang with
            |HTML ->
                codewritein "<div class=\"fig\">"
                codewritein("<img src =\""+figdir+"/"+filename+".svg\">")
                codewritein "</div>"
            |LaTeX ->
                codewritein "\\begin{center}"
                codewritein("   \\includegraphics{"+figdir+"/"+filename+"}")
                codewritein "\\end{center}"
            |_ -> ()
            match figlabel with
            |WriteLabel wr ->
                fignum <- fignum + 1
                wr.WriteLine(filename + "," + fignum.ToString())
            |ReadLabel lst ->
                ()
        member this.enumerate code =
            match lang with
            |HTML ->
                codewritein "<ol>"
                code()
                codewritein "</ol>"
            |LaTeX ->
                codewritein "\\begin{enumerate}"
                code()
                codewritein "\\end{enumerate}"
            |_ -> ()
        member this.itemize code =
            match lang with
            |HTML ->
                codewritein "<ul>"
                code()
                codewritein "</ul>"
            |LaTeX ->
                codewritein "\\begin{itemize}"
                code()
                codewritein "\\end{itemize}"
            |_ -> ()
        member this.item code =
            match lang with
            |HTML ->
                codewritein "<li>"
                let c = code()
                codewritein "</li>"
                c
            |LaTeX ->
                codewritein "\\item"
                let c = code()
                c
            |_ -> code()
        member this.itemchk code =
            licheck <- licheck + 1
            match lang with
            |HTML ->
                codewritein("<li><input type=\"checkbox\" id=\"lichk"+licheck.ToString()+"\" onchange=\"switchCheck('lichk"+licheck.ToString()+"','txtlichk"+licheck.ToString()+"');\"><span id=\"txtlichk"+licheck.ToString()+"\">")
                let c = code()
                codewritein "</span></li>"
                c
            |LaTeX ->
                codewritein "\\item"
                let c = code()
                c
            |_ -> code()
        member this.eq(txt:string) =
            match lang with
            |HTML ->
                "\\("+txt+"\\)"
            |LaTeX ->
                "$"+txt+"$"
            |_ -> ""
        member this.eqwr(txt:string) =
            codewritein(this.eq txt)
        member this.align (code:unit->unit) =
            match lang with
            |HTML ->
                codewritein "\\["
                codewritein "\\begin{align}"
                code()
                codewritein "\\end{align}"
                codewritein "\\]"
            |LaTeX ->
                codewritein "\\begin{align}"
                code()
                codewritein "\\end{align}"
            |_ -> ()
        member this.code (caption,label) (c:unit->unit) =
            match lang with
            |HTML ->
                codenum <- codenum + 1
                match codelabel with
                |WriteLabel wr ->
                    if label<>"" then wr.WriteLine(label + "," + codenum.ToString())
                |ReadLabel lst ->
                    ()
                codewritein "<div class=\"fig\">"
                codewritein("<span class=\"caption\">ソースコード"+codenum.ToString()+"&emsp;"+caption+"</span>")
                codewritein "<div class=\"sourcecode\">"
                c()
                codewritein "</div>"
                codewritein "</div>"
            |LaTeX ->
                codewritein("\\begin{lstlisting} [caption="+caption+",label="+label+"]")
                c()
                codewritein "\\end{lstlisting}"
            |_ -> ()
        member this.equation (label:string) = fun (code:unit->unit) ->
            match lang with
            |HTML ->
                code()
                equnum <- equnum + 1
                codewritein("\\tag{"+equnum.ToString()+"}")
                match equlabel with
                |WriteLabel wr ->
                    if label<>"" then wr.WriteLine(label + "," + equnum.ToString())
                |ReadLabel lst ->
                    ()
            |LaTeX ->
                code()
                if label<>"" then codewritein("\\label{"+label+"}")
            |_ -> ()
        member this.equation_nonumber (code:unit->unit) =
            match lang with
            |HTML ->
                code()
                codewritein "\\nonumber"
            |LaTeX ->
                code()
                codewritein "\\nonumber"
            |_ -> ()
        member this.eqbr() =
            codewritein "\\\\"
        member this.br with get() =
            match lang with
            |LaTeX ->
                codewritein "\\\\"
            |HTML ->
                codewritein "<br/>"
            |_ ->
                ()
        /// code内部で使用する改行
        member this.codebr with get() =
            match lang with
            |LaTeX ->
                codewritein ""
            |HTML ->
                codewritein "<br/>"
            |_ ->
                ()
        member this.bf(txt:string) =
            match lang with
            |LaTeX ->
                codewritein("\\textbf{"+txt+"}")
            |HTML ->
                codewritein("<strong>"+txt+"</strong>")
            |_ ->
                ()
        member this.numunit (n:int) = fun (u:string) ->
            match lang with
            |LaTeX ->
                "\\SI{"+n.ToString()+"}{"+u+"}"
            |HTML ->
                n.ToString()+" "+u
            |_ -> ""
        member this.numunitbr (n:int) = fun (u:string) ->
            match lang with
            |LaTeX ->
                "\\SI{"+n.ToString()+"}{["+u+"]}"
            |HTML ->
                n.ToString()+" ["+u+"]"
            |_ -> ""
        member this.numunit (n:float) = fun (u:string) ->
            match lang with
            |LaTeX ->
                "\\SI{"+n.ToString()+"}{"+u+"}"
            |HTML ->
                n.ToString()+" "+u
            |_ -> ""
        member this.numunitbr (n:float) = fun (u:string) ->
            match lang with
            |LaTeX ->
                "\\SI{"+n.ToString()+"}{["+u+"]}"
            |HTML ->
                n.ToString()+" ["+u+"]"
            |_ -> ""
        member this.unit (u:string) =
            match lang with
            |LaTeX ->
                "\\si{"+u+"}"
            |HTML ->
                u
            |_ -> ""
        member this.link txt url =
            match lang with
            |LaTeX -> 
                txt
            |HTML -> 
                "<a href=\""+url+"\">"+txt+"</a>"
            |_ -> 
                ""
        member this.pdflink(text,filename) =
            match lang with
            |HTML ->
                codewritein("    <div class='pdflink'><a href='"+filename+".pdf' target='_blank'>"+text+"</a></div>")
            |_ -> 
                ()
        member this.bold(txt:string) = match lang with |LaTeX -> "\\boldsymbol{"+txt+"}" |HTML -> "\\boldsymbol{"+txt+"}" |_ -> ""
        member this.lt with get() = match lang with |LaTeX -> "<" |HTML -> "\\lt" |_ -> ""
        member this.gt with get() = match lang with |LaTeX -> ">" |HTML -> "\\gt" |_ -> ""
        member this.kilo with get() = match lang with |LaTeX -> "\\kilo" |HTML -> "\\mathrm{k}" |_ -> ""
        member this.centi with get() = match lang with |LaTeX -> "\\centi" |HTML -> "\\mathrm{c}" |_ -> ""
        member this.milli with get() = match lang with |LaTeX -> "\\milli" |HTML -> "\\mathrm{m}" |_ -> ""
        member this.micro with get() = match lang with |LaTeX -> "\\micro" |HTML -> "\\mathrm{\\mu}" |_ -> ""
        member this.pico with get() = match lang with |LaTeX -> "\\pico" |HTML -> "\\mathrm{p}" |_ -> ""
        member this.mega with get() = match lang with |LaTeX -> "\\mega" |HTML -> "\\mathrm{M}" |_ -> ""
        member this.second with get() = match lang with |LaTeX -> "\\second" |HTML -> "\\mathrm{s}" |_ -> ""
        member this.rad with get() = match lang with |LaTeX -> "\\mathrm{rad}" |HTML -> "\\mathrm{rad}" |_ -> ""
        member this.minute with get() = match lang with |LaTeX -> "\\minute" |HTML -> "\\mathrm{min}" |_ -> ""
        member this.hour with get() = match lang with |LaTeX -> "\\hour" |HTML -> "\\mathrm{h}" |_ -> ""
        member this.day with get() = match lang with |LaTeX -> "\\day" |HTML -> "\\mathrm{day}" |_ -> ""
        member this.metre with get() = match lang with |LaTeX -> "\\metre" |HTML -> "\\mathrm{m}" |_ -> ""
        member this.arcminute with get() = match lang with |LaTeX -> "\\arcminute" |HTML -> "'" |_ -> ""
        member this.arcsecond with get() = match lang with |LaTeX -> "\\arcsecond" |HTML -> "''" |_ -> ""
        member this.degree with get() = match lang with |LaTeX -> "\\degree" |HTML -> "^\\circ" |_ -> ""
        member this.mole with get() = match lang with |LaTeX -> "\\mole" |HTML -> "\\mathrm{mol}" |_ -> ""
        member this.candela with get() = match lang with |LaTeX -> "\\candela" |HTML -> "\\mathrm{cd}" |_ -> ""
        member this.liter with get() = match lang with |LaTeX -> "\\liter" |HTML -> "\\mathrm{L}" |_ -> ""
        member this.tonne with get() = match lang with |LaTeX -> "\\tonne" |HTML -> "\\mathrm{t}" |_ -> ""
        member this.kilogram with get() = match lang with |LaTeX -> "\\kilogram" |HTML -> "\\mathrm{kg}" |_ -> ""
        member this.degreeCelsius with get() = match lang with |LaTeX -> "\\degreeCelsius" |HTML -> "\\mathrm{^\\circ C}" |_ -> ""
        member this.ampere with get() = match lang with |LaTeX -> "\\ampere" |HTML -> "\\mathrm{A}" |_ -> ""
        member this.henry with get() = match lang with |LaTeX -> "\\henry" |HTML -> "\\mathrm{H}" |_ -> ""
        member this.hertz with get() = match lang with |LaTeX -> "\\hertz" |HTML -> "\\mathrm{Hz}" |_ -> ""
        member this.newton with get() = match lang with |LaTeX -> "\\newton" |HTML -> "\\mathrm{N}" |_ -> ""
        member this.pascal with get() = match lang with |LaTeX -> "\\pascal" |HTML -> "\\mathrm{Pa}" |_ -> ""
        member this.watt with get() = match lang with |LaTeX -> "\\watt" |HTML -> "\\mathrm{W}" |_ -> ""
        member this.joule with get() = match lang with |LaTeX -> "\\joule" |HTML -> "\\mathrm{J}" |_ -> ""
        member this.coulomb with get() = match lang with |LaTeX -> "\\coulomb" |HTML -> "\\mathrm{C}" |_ -> ""
        member this.siemens with get() = match lang with |LaTeX -> "\\siemens" |HTML -> "\\mathrm{S}" |_ -> ""
        member this.weber with get() = match lang with |LaTeX -> "\\weber" |HTML -> "\\mathrm{Wb}" |_ -> ""
        member this.tesla with get() = match lang with |LaTeX -> "\\tesla" |HTML -> "\\mathrm{T}" |_ -> ""
        member this.kelvin with get() = match lang with |LaTeX -> "\\kelvin" |HTML -> "\\mathrm{K}" |_ -> ""
        member this.ohm with get() = match lang with |LaTeX -> "\\ohm" |HTML -> "\\mathrm{\\Omega}" |_ -> ""
        member this.volt with get() = match lang with |LaTeX -> "\\volt" |HTML -> "\\mathrm{V}" |_ -> ""
        member this.farad with get() = match lang with |LaTeX -> "\\farad" |HTML -> "\\mathrm{F}" |_ -> ""
        member this.vpp with get() = match lang with |LaTeX -> "\\volt_{pp}" |HTML -> "\\mathrm{V_{pp}}" |_ -> ""
        member this.div with get() = match lang with |LaTeX -> "\\mathrm{div}" |HTML -> "\\mathrm{div}" |_ -> ""
        member this.per with get() = match lang with |LaTeX -> "/" |HTML -> "/" |_ -> ""
        member this.squared with get() = match lang with |LaTeX -> "\\squared" |HTML -> "^2" |_ -> ""
        member this.cubed with get() = match lang with |LaTeX -> "\\cubed" |HTML -> "^3" |_ -> ""
        member this.percent with get() = match lang with |LaTeX -> "\\%" |HTML -> "%" |_ -> ""
        member this.pow(n:int) = match lang with |LaTeX -> "^"+(if n<0 then "{"+n.ToString()+"}" else n.ToString()) |HTML -> "^"+(if n<0 then "{"+n.ToString()+"}" else n.ToString()) |_ -> ""
        
    [<AutoOpen>]
    module Aqualis_doc =
        /// <summary>
        /// 自動採点サイト(HTML)を出力
        /// </summary>
        let Document (lang:Language) (outputdir:string) (filename:string) (title:string,titlelong:string) (figdir:string) code =
            for i in 1..2 do
                let figlabel = 
                    if i=1 then
                        WriteLabel(new StreamWriter(filename+"_figlabel"))
                    else
                        let rd = new StreamReader(filename+"_figlabel")
                        let rec read lst =
                            match rd.ReadLine() with
                            |null ->
                                rd.Close()
                                ReadLabel lst
                            |t ->
                                let k = t.Split([|','|],StringSplitOptions.RemoveEmptyEntries)
                                read ((k[0],k[1])::lst)
                        read []
                let equlabel = 
                    if i=1 then
                        WriteLabel(new StreamWriter(filename+"_equlabel"))
                    else
                        let rd = new StreamReader(filename+"_equlabel")
                        let rec read lst =
                            match rd.ReadLine() with
                            |null ->
                                rd.Close()
                                ReadLabel lst
                            |t ->
                                let k = t.Split([|','|],StringSplitOptions.RemoveEmptyEntries)
                                read ((k[0],k[1])::lst)
                        read []
                let tablabel = 
                    if i=1 then
                        WriteLabel(new StreamWriter(filename+"_tablabel"))
                    else
                        let rd = new StreamReader(filename+"_tablabel")
                        let rec read lst =
                            match rd.ReadLine() with
                            |null ->
                                rd.Close()
                                ReadLabel lst
                            |t ->
                                let k = t.Split([|','|],StringSplitOptions.RemoveEmptyEntries)
                                read ((k[0],k[1])::lst)
                        read []
                let codelabel = 
                    if i=1 then
                        WriteLabel(new StreamWriter(filename+"_codelabel"))
                    else
                        let rd = new StreamReader(filename+"_codelabel")
                        let rec read lst =
                            match rd.ReadLine() with
                            |null ->
                                rd.Close()
                                ReadLabel lst
                            |t ->
                                let k = t.Split([|','|],StringSplitOptions.RemoveEmptyEntries)
                                read ((k[0],k[1])::lst)
                        read []
                match lang with
                |HTML ->
                    makeProgram [outputdir,filename+".html",HTML] <| fun () ->
                        codewritein "<!DOCTYPE html>"
                        codewritein "<html lang='ja'>"
                        codewritein "    <head>"
                        codewritein "        <meta charset='utf-8'>"
                        codewritein "        <script type=\"text/javascript\" id=\"MathJax-script\" async src=\"https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml.js\"></script>"
                        codewritein "        <script src=\"animation.js\"></script>"
                        codewritein "        <script>"
                        codewritein "        function switchCheck(chkid,txtid)"
                        codewritein "        {"
                        codewritein "            let chk = document.getElementById(chkid);"
                        codewritein "            let txt = document.getElementById(txtid);"
                        codewritein "            if (chk.checked)"
                        codewritein "            {"
                        codewritein "                txt.style.color = '#B2D5E6';"
                        codewritein "            }"
                        codewritein "            else"
                        codewritein "            {"
                        codewritein "                txt.style.color = 'black';"
                        codewritein "            }"
                        codewritein "        }"
                        codewritein "        function switchDisplay(f1,f2)"
                        codewritein "        {"
                        codewritein "            let el1 = document.getElementById(f1);"
                        codewritein "            let el2 = document.getElementById(f2);"
                        codewritein "            if (el1.style.display == \"block\")"
                        codewritein "            {"
                        codewritein "                el1.style.display =\"none\";"
                        codewritein "                el2.style.display =\"block\";"
                        codewritein "            }"
                        codewritein "            else"
                        codewritein "            {"
                        codewritein "                el1.style.display =\"block\";"
                        codewritein "                el2.style.display =\"none\";"
                        codewritein "            }"
                        codewritein "        }"
                        codewritein "        </script>"
                        codewritein("        <title>"+title+"</title>")
                        codewritein "        <meta name='viewport' content='width=device-width, initial-scale=1.0, maximum-scale=1.0, minimum-scale=1.0'>"
                        codewritein "        <link rel=\"preconnect\" href=\"https://fonts.googleapis.com\">"
                        codewritein "        <link rel=\"preconnect\" href=\"https://fonts.gstatic.com\" crossorigin>"
                        codewritein "        <link href=\"https://fonts.googleapis.com/css2?family=Noto+Sans+JP:wght@500;600;700&display=swap\" rel=\"stylesheet\">"
                        codewritein "        <link rel='stylesheet' href='style.css' />"
                        codewritein "    </head>"
                        codewritein "    <body>"
                        codewritein("    <span class='headtitle'>"+titlelong+"</span>")
                        code(TeXWriter(figlabel,equlabel,tablabel,codelabel,lang,figdir))
                        codewritein "    </body>"
                        codewritein "</html>"
                        programList[prIndex].close()
                |LaTeX ->
                    makeProgram [outputdir,filename+".tex",LaTeX] <| fun () ->
                        codewritein "\\documentclass[a4paper]{ltjsarticle}"
                        codewritein ""
                        codewritein "\\usepackage{luatexja}"
                        codewritein "\\usepackage{graphicx}"
                        codewritein "\\usepackage{amsmath}"
                        codewritein "\\usepackage{amssymb}"
                        codewritein "\\usepackage{siunitx}"
                        codewritein "\\usepackage{listings,jvlisting}"
                        codewritein "\\usepackage{url}"
                        codewritein "\\usepackage{upgreek}"
                        codewritein "\\usepackage[no-math]{luatexja-fontspec}"
                        codewritein "\\usepackage[haranoaji,deluxe,match,nfssonly]{luatexja-preset}"
                        codewritein ""
                        codewritein "\\newcommand{\\inputfigure}[2]{"
                        codewritein "\\begin{figure}[ht]"
                        codewritein "\\begin{center}"
                        codewritein("\\includegraphics{"+figdir+"/#1}")
                        codewritein "\\end{center}"
                        codewritein "\\caption{#2}"
                        codewritein "\\label{#1}"
                        codewritein "\\end{figure}"
                        codewritein "}"
                        codewritein ""
                        codewritein "\\makeatletter"
                        codewritein "\\def\\thesection{\\arabic{section}.}"
                        codewritein "\\def\\thesubsection{\\thesection\\arabic{subsection}}"
                        codewritein "\\def\\thesubsubsection{\\thesubsection.\\arabic{subsubsection}}"
                        codewritein "\\def\\section{\\@startsection {section}{1}{\\z@}{2.8ex plus .5ex minus .2ex}{1.2ex plus.5ex}{\\reset@font\\Large\\textbf}}"
                        codewritein "\\def\\subsection{\\@startsection {subsection}{1}{\\z@}{1.9ex plus .3ex minus .1ex}{0.5ex plus.2ex}{\\reset@font\\large\\textbf}}"
                        codewritein "\\def\\subsubsection{\\@startsection {subsubsection}{1}{\\z@}{1.3ex plus .3ex minus .1ex}{0.2ex plus .1ex}{\\reset@font\\normalsize\\textbf}}"
                        codewritein "\\makeatother"
                        codewritein ""
                        codewritein "%%paper WxH=210x297"
                        codewritein "\\oddsidemargin=-0.4mm"
                        codewritein "\\topmargin=4.6mm"
                        codewritein "\\headheight=0mm"
                        codewritein "\\headsep=0mm"
                        codewritein "\\footskip=15mm"
                        codewritein "\\textwidth=160mm"
                        codewritein "\\textheight=237mm"
                        codewritein "\\topsep=6pt"
                        codewritein "\\parindent=0mm"
                        codewritein "\\unitlength=1.00mm"
                        codewritein ""
                        codewritein "\\newcommand{\\MyTitle}[1]{\\begin{center} {\\Huge\\textbf{#1}} \\end{center}}"
                        codewritein "\\newcommand{\\MyChapter}[1]{\\vskip 5mm \\noindent {\\Large\\textbf{#1}} \\vskip 2mm}"
                        codewritein "\\newcommand{\\MySection}[1]{\\vskip 3mm \\noindent {\\large\\textbf{#1}} \\vskip 1mm}"
                        codewritein ""
                        codewritein "\\pagestyle{empty}"
                        codewritein ""
                        codewritein "\\lstset{"
                        codewritein "  basicstyle={\\ttfamily},"
                        codewritein "  identifierstyle={\\small},"
                        codewritein "  commentstyle={\\smallitshape},"
                        codewritein "  keywordstyle={\\small\\bfseries},"
                        codewritein "  ndkeywordstyle={\\small},"
                        codewritein "  stringstyle={\\small\\ttfamily},"
                        codewritein "  frame={tb},"
                        codewritein "  breaklines=true,"
                        codewritein "  columns=[l]{fullflexible},"
                        codewritein "  numbers=left,"
                        codewritein "  numberstyle={\\scriptsize},"
                        codewritein "}"
                        codewritein ""
                        codewritein "\\renewcommand{\\lstlistingname}{ソースコード}"
                        codewritein ""
                        codewritein "\\begin{document}"
                        code(TeXWriter(figlabel,equlabel,tablabel,codelabel,lang,figdir))
                        codewritein "\\end{document}"
                        programList[prIndex].close()
                |_ -> ()
                match figlabel with
                |WriteLabel wr -> wr.Close()
                |_ -> ()
                match equlabel with
                |WriteLabel wr -> wr.Close()
                |_ -> ()
                match tablabel with
                |WriteLabel wr -> wr.Close()
                |_ -> ()
                match codelabel with
                |WriteLabel wr -> wr.Close()
                |_ -> ()
            File.Delete(filename+"_figlabel")
            File.Delete(filename+"_equlabel")
            File.Delete(filename+"_tablabel")
            File.Delete(filename+"_codelabel")
