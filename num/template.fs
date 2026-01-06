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
        
        member _.write s = writein s
        
        member this.tag (tagname:string) code =
            match lang with
            |HTML ->
                writein("<"+tagname+">")
                code()
                writein("</"+tagname+">")
            |LaTeX ->
                writein("\\begin{"+tagname+"}")
                code()
                writein("\\end{"+tagname+"}")
            |_ -> ()
            
        member this.block lst code =
            match lang with
            |HTML ->
                writein "<div "
                for a,name in lst do
                    writein(a+"=\""+name+"\" ")
                writein ">"
                code()
                writein "</div>"
            |_ -> ()
        member this.form (name:string) code =
            match lang with
            |HTML ->
                writein("<form name=\""+name+"\">")
                code()
                writein "</form>"
            |_ -> ()
        member this.radioButton (name:string) lst =
            match lang with
            |HTML ->
                this.form ("f_"+name) <| fun () ->
                    for (a,b,c,d) in lst do
                        writein("<input type=\"radio\" name=\""+name+"\" value=\""+a+"\""+(if c then " checked" else "")+" "+d+">"+b)
            |_ -> ()
        member this.title txt =
            match lang with
            |HTML ->
                writein("<h1>"+txt+"</h1>")
            |LaTeX ->
                writein("\\MyTitle{"+txt+"}")
            |_ -> ()
        member this.section title code =
            secnum <- secnum + 1
            ssecnum <- 0
            sssecnum <- 0
            match lang with
            |HTML ->
                writein("<h2>" + secnum.ToString() + " "+title+"</h2>")
            |LaTeX ->
                writein("\\section{"+title+"}")
            |_ -> ()
            code()
        member this.subsection title code =
            ssecnum <- ssecnum + 1
            sssecnum <- 0
            match lang with
            |HTML ->
                writein("<h3>" + secnum.ToString() + "." + ssecnum.ToString() + " " + title+"</h3>")
            |LaTeX ->
                writein("\\subsection{"+title+"}")
            |_ -> ()
            code()
        member this.subsection_ title code =
            match lang with
            |HTML ->
                writein("<h3>" + title + "</h3>")
            |LaTeX ->
                writein("\\subsection*{"+title+"}")
            |_ -> ()
            code()
        member this.subsubsection title code =
            sssecnum <- sssecnum + 1
            match lang with
            |HTML ->
                writein("<h4>" + secnum.ToString() + "." + ssecnum.ToString() + "." + sssecnum.ToString() + " " + title+"</h4>")
            |LaTeX ->
                writein("\\subsubsection{"+title+"}")
            |_ -> ()
            code()
        member this.subsubsection_ title code =
            match lang with
            |HTML ->
                writein("<h4>" + title + "</h4>")
            |LaTeX ->
                writein("\\subsubsection*{"+title+"}")
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
                    writein("<sup><a name=\"xft"+ftnnum.ToString()+"\"><a href=\"#ft"+ftnnum.ToString()+"\">"+ftnnum.ToString()+")</a></a></sup>")
                    wr.WriteLine("<a name=\"ft"+ftnnum.ToString()+"\">"+ftnnum.ToString()+") <a href=\"#xft"+ftnnum.ToString()+"\">↑</a>　"+txt+"</a><br/>")
                writein "<p>"
                code addfootnote
                writein "</p>"
                wr.Close()
                writein "<div class=\"footnote\">"
                writein(File.ReadAllText filename)
                writein "</div>"
                File.Delete filename
            |LaTeX ->
                writein "\\par"
                code this.footnote
            |_ -> 
                ()
        member this.footnote(txt:string) =
            writein("\\footnote{"+txt+"}")
        member this.url(txt:string) =
            match lang with
            |LaTeX -> txt.Replace("_","\\_")
            |_ -> txt
        member this.table label tbalign elalign caption (lst:list<list<string>>) =
            match lang with
            |HTML ->
                writein "<div class=\"fig\">"
                writein("<span class=\"caption\">"+this.tabref(label)+"&emsp;"+caption+"</span>")
                writein "<table class=\"tab\">"
                for m in 0..lst.Length-1 do
                    writein "<tr>"
                    for s in lst[m] do
                        writein "<td>"
                        writein s
                        writein "</td>"
                    writein "</tr>"
                writein "</table>"
                writein "</div>"
            |LaTeX ->
                writein("\\begin{table}["+tbalign+"]")
                writein(" \\caption{"+caption+"}")
                writein(" \\label{"+label+"}")
                writein " \\centering"
                writein("  \\begin{tabular}{"+elalign+"}")
                for m in 0..lst.Length-1 do
                    if m=0 || m=1 then writein "\\hline "
                    for n in 0..lst[m].Length-1 do
                        writein(lst[m][n])
                        if n<lst[m].Length-1 then writein "&"
                    writein "\\\\"
                    if m=lst.Length-1 then writein "\\hline "
                writein "  \\end{tabular}"
                writein "\\end{table}"
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
                writein "<div class=\"fig\">"
                writein("  <a name=\""+filename+"\">")
                writein("    <img src =\""+figdir+"/"+filename+".svg\" alt=\""+caption+"\">")
                writein("  </a>")
                writein("  <div class=\"caption\">"+this.figref_nolink filename+"&emsp;"+caption+"</div>")
                writein "</div>"
            |LaTeX ->
                writein("\\inputfigure{"+filename+"}{"+caption+"}")
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
                writein "<div class=\"fig\">"
                writein("<img src =\""+figdir+"/"+filename+".svg\">")
                writein "</div>"
            |LaTeX ->
                writein "\\begin{center}"
                writein("   \\includegraphics{"+figdir+"/"+filename+"}")
                writein "\\end{center}"
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
                writein "<ol>"
                code()
                writein "</ol>"
            |LaTeX ->
                writein "\\begin{enumerate}"
                code()
                writein "\\end{enumerate}"
            |_ -> ()
        member this.itemize code =
            match lang with
            |HTML ->
                writein "<ul>"
                code()
                writein "</ul>"
            |LaTeX ->
                writein "\\begin{itemize}"
                code()
                writein "\\end{itemize}"
            |_ -> ()
        member this.item code =
            match lang with
            |HTML ->
                writein "<li>"
                let c = code()
                writein "</li>"
                c
            |LaTeX ->
                writein "\\item"
                let c = code()
                c
            |_ -> code()
        member this.itemchk code =
            licheck <- licheck + 1
            match lang with
            |HTML ->
                writein("<li><input type=\"checkbox\" id=\"lichk"+licheck.ToString()+"\" onchange=\"switchCheck('lichk"+licheck.ToString()+"','txtlichk"+licheck.ToString()+"');\"><span id=\"txtlichk"+licheck.ToString()+"\">")
                let c = code()
                writein "</span></li>"
                c
            |LaTeX ->
                writein "\\item"
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
            writein(this.eq txt)
        member this.align (code:unit->unit) =
            match lang with
            |HTML ->
                writein "\\["
                writein "\\begin{align}"
                code()
                writein "\\end{align}"
                writein "\\]"
            |LaTeX ->
                writein "\\begin{align}"
                code()
                writein "\\end{align}"
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
                writein "<div class=\"fig\">"
                writein("<span class=\"caption\">ソースコード"+codenum.ToString()+"&emsp;"+caption+"</span>")
                writein "<div class=\"sourcecode\">"
                c()
                writein "</div>"
                writein "</div>"
            |LaTeX ->
                writein("\\begin{lstlisting} [caption="+caption+",label="+label+"]")
                c()
                writein "\\end{lstlisting}"
            |_ -> ()
        member this.equation (label:string) = fun (code:unit->unit) ->
            match lang with
            |HTML ->
                code()
                equnum <- equnum + 1
                writein("\\tag{"+equnum.ToString()+"}")
                match equlabel with
                |WriteLabel wr ->
                    if label<>"" then wr.WriteLine(label + "," + equnum.ToString())
                |ReadLabel lst ->
                    ()
            |LaTeX ->
                code()
                if label<>"" then writein("\\label{"+label+"}")
            |_ -> ()
        member this.equation_nonumber (code:unit->unit) =
            match lang with
            |HTML ->
                code()
                writein "\\nonumber"
            |LaTeX ->
                code()
                writein "\\nonumber"
            |_ -> ()
        member this.eqbr() =
            writein "\\\\"
        member this.br with get() =
            match lang with
            |LaTeX ->
                writein "\\\\"
            |HTML ->
                writein "<br/>"
            |_ ->
                ()
        /// code内部で使用する改行
        member this.codebr with get() =
            match lang with
            |LaTeX ->
                writein ""
            |HTML ->
                writein "<br/>"
            |_ ->
                ()
        member this.bf(txt:string) =
            match lang with
            |LaTeX ->
                writein("\\textbf{"+txt+"}")
            |HTML ->
                writein("<strong>"+txt+"</strong>")
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
                writein("    <div class='pdflink'><a href='"+filename+".pdf' target='_blank'>"+text+"</a></div>")
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
                        writein "<!DOCTYPE html>"
                        writein "<html lang='ja'>"
                        writein "    <head>"
                        writein "        <meta charset='utf-8'>"
                        writein "        <script type=\"text/javascript\" id=\"MathJax-script\" async src=\"https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml.js\"></script>"
                        writein "        <script src=\"animation.js\"></script>"
                        writein "        <script>"
                        writein "        function switchCheck(chkid,txtid)"
                        writein "        {"
                        writein "            let chk = document.getElementById(chkid);"
                        writein "            let txt = document.getElementById(txtid);"
                        writein "            if (chk.checked)"
                        writein "            {"
                        writein "                txt.style.color = '#B2D5E6';"
                        writein "            }"
                        writein "            else"
                        writein "            {"
                        writein "                txt.style.color = 'black';"
                        writein "            }"
                        writein "        }"
                        writein "        function switchDisplay(f1,f2)"
                        writein "        {"
                        writein "            let el1 = document.getElementById(f1);"
                        writein "            let el2 = document.getElementById(f2);"
                        writein "            if (el1.style.display == \"block\")"
                        writein "            {"
                        writein "                el1.style.display =\"none\";"
                        writein "                el2.style.display =\"block\";"
                        writein "            }"
                        writein "            else"
                        writein "            {"
                        writein "                el1.style.display =\"block\";"
                        writein "                el2.style.display =\"none\";"
                        writein "            }"
                        writein "        }"
                        writein "        </script>"
                        writein("        <title>"+title+"</title>")
                        writein "        <meta name='viewport' content='width=device-width, initial-scale=1.0, maximum-scale=1.0, minimum-scale=1.0'>"
                        writein "        <link rel=\"preconnect\" href=\"https://fonts.googleapis.com\">"
                        writein "        <link rel=\"preconnect\" href=\"https://fonts.gstatic.com\" crossorigin>"
                        writein "        <link href=\"https://fonts.googleapis.com/css2?family=Noto+Sans+JP:wght@500;600;700&display=swap\" rel=\"stylesheet\">"
                        writein "        <link rel='stylesheet' href='style.css' />"
                        writein "    </head>"
                        writein "    <body>"
                        writein("    <span class='headtitle'>"+titlelong+"</span>")
                        code(TeXWriter(figlabel,equlabel,tablabel,codelabel,lang,figdir))
                        writein "    </body>"
                        writein "</html>"
                        programList[prIndex].close()
                |LaTeX ->
                    makeProgram [outputdir,filename+".tex",LaTeX; outputdir,filename+"_temp.tex",LaTeX] <| fun () ->
                        prIndex <- 1
                        code(TeXWriter(figlabel,equlabel,tablabel,codelabel,lang,figdir))
                        prIndex <- 0
                        writein "\\documentclass[a4paper]{ltjsarticle}"
                        writein ""
                        writein "\\usepackage{luatexja}"
                        writein "\\usepackage{graphicx}"
                        writein "\\usepackage{amsmath}"
                        writein "\\usepackage{amssymb}"
                        writein "\\usepackage{siunitx}"
                        writein "\\usepackage{listings,jvlisting}"
                        writein "\\usepackage{url}"
                        writein "\\usepackage{upgreek}"
                        writein "\\usepackage[no-math]{luatexja-fontspec}"
                        writein "\\usepackage[haranoaji,deluxe,match,nfssonly]{luatexja-preset}"
                        for p in programList[1].hlist.list do
                            writein p
                        writein ""
                        writein "\\newcommand{\\inputfigure}[2]{"
                        writein "\\begin{figure}[ht]"
                        writein "\\begin{center}"
                        writein("\\includegraphics{"+figdir+"/#1}")
                        writein "\\end{center}"
                        writein "\\caption{#2}"
                        writein "\\label{#1}"
                        writein "\\end{figure}"
                        writein "}"
                        writein ""
                        writein "\\makeatletter"
                        writein "\\def\\thesection{\\arabic{section}.}"
                        writein "\\def\\thesubsection{\\thesection\\arabic{subsection}}"
                        writein "\\def\\thesubsubsection{\\thesubsection.\\arabic{subsubsection}}"
                        writein "\\def\\section{\\@startsection {section}{1}{\\z@}{2.8ex plus .5ex minus .2ex}{1.2ex plus.5ex}{\\reset@font\\Large\\textbf}}"
                        writein "\\def\\subsection{\\@startsection {subsection}{1}{\\z@}{1.9ex plus .3ex minus .1ex}{0.5ex plus.2ex}{\\reset@font\\large\\textbf}}"
                        writein "\\def\\subsubsection{\\@startsection {subsubsection}{1}{\\z@}{1.3ex plus .3ex minus .1ex}{0.2ex plus .1ex}{\\reset@font\\normalsize\\textbf}}"
                        writein "\\makeatother"
                        writein ""
                        writein "%%paper WxH=210x297"
                        writein "\\oddsidemargin=-0.4mm"
                        writein "\\topmargin=4.6mm"
                        writein "\\headheight=0mm"
                        writein "\\headsep=0mm"
                        writein "\\footskip=15mm"
                        writein "\\textwidth=160mm"
                        writein "\\textheight=237mm"
                        writein "\\topsep=6pt"
                        writein "\\parindent=0mm"
                        writein "\\unitlength=1.00mm"
                        writein ""
                        writein "\\newcommand{\\MyTitle}[1]{\\begin{center} {\\Huge\\textbf{#1}} \\end{center}}"
                        writein "\\newcommand{\\MyChapter}[1]{\\vskip 5mm \\noindent {\\Large\\textbf{#1}} \\vskip 2mm}"
                        writein "\\newcommand{\\MySection}[1]{\\vskip 3mm \\noindent {\\large\\textbf{#1}} \\vskip 1mm}"
                        writein ""
                        writein "\\pagestyle{empty}"
                        writein ""
                        writein "\\lstset{"
                        writein "  basicstyle={\\ttfamily},"
                        writein "  identifierstyle={\\small},"
                        writein "  commentstyle={\\smallitshape},"
                        writein "  keywordstyle={\\small\\bfseries},"
                        writein "  ndkeywordstyle={\\small},"
                        writein "  stringstyle={\\small\\ttfamily},"
                        writein "  frame={tb},"
                        writein "  breaklines=true,"
                        writein "  columns=[l]{fullflexible},"
                        writein "  numbers=left,"
                        writein "  numberstyle={\\scriptsize},"
                        writein "}"
                        writein ""
                        writein "\\renewcommand{\\lstlistingname}{ソースコード}"
                        writein ""
                        writein "\\begin{document}"
                        writein(programList[1].allCodes)
                        writein "\\end{document}"
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
