// 
// Copyright (c) 2026 Jun-ichiro Sugisaka
// 
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
// 
namespace Aqualis
    
    open System
    
    type eqmode() =
        ///<summary>改行</summary>
        member _.eqReturn() =
            match programList[prIndex].language with
            |LaTeX ->
                writein "\\\\"
            |_ ->
                ()
                
        ///<summary>数式番号なし</summary>
        member _.eqNonumber() =
            match programList[prIndex].language with
            |LaTeX ->
                writein "\\nonumber"
            |_ ->
                ()
                
        ///<summary>改行</summary>
        member _.eqLabel(lb:string) =
            match programList[prIndex].language with
            |LaTeX ->
                writein("\\label{"+lb+"}")
            |_ ->
                ()
                
        ///<summary>数式番号なし改行</summary>
        member this.nnReturn() =
            this.eqNonumber()
            this.eqReturn()
            
        ///<summary>空白の左辺</summary>
        member _.nl with get() = num0(Var(Zt,"",NaN))
        
    ///<summary>変数宣言</summary>
    type doc () =
        
        ///<summary>段落</summary>
        static member para code =
            match programList[prIndex].language with
            |LaTeX ->
                writein "\\par"
                code()
            |_ ->
                code()
                
        ///<summary>テキスト</summary>
        static member text (s:string) =
            match programList[prIndex].language with
            |LaTeX ->
                writein s
            |_ ->
                ! s
                
        ///<summary>図の挿入</summary>
        static member inputfigure (filename:string) (caption:string) =
            programList[prIndex].hlist.add "\\usepackage{graphicx}"
            match programList[prIndex].language with
            |LaTeX ->
                writein "\\begin{figure}[htbp]"
                writein "\\begin{center}"
                writein("\\includegraphics{"+filename+"}")
                writein "\\end{center}"
                writein("\\caption{"+caption+"}")
                writein("\\label{"+filename+"}")
                writein "\\end{figure}"
            |_ ->
                ! (filename+": "+caption)
                
        ///<summary>番号付き箇条書き</summary>
        static member enumerate (slst:(unit->unit)list) =
            match programList[prIndex].language with
            |LaTeX ->
                writein "\\begin{enumerate}"
                for s in slst do
                    writein "\\item"
                    s()
                writein "\\end{enumerate}"
            |_ ->
                for s in slst do
                    s()
                    
        ///<summary>番号なし箇条書き</summary>
        static member itemize (slst:(unit->unit)list) =
            match programList[prIndex].language with
            |LaTeX ->
                writein "\\begin{itemize}"
                for s in slst do
                    writein "\\item"
                    s()
                writein "\\end{itemize}"
            |_ ->
                for s in slst do
                    s()
                    
        ///<summary>数式</summary>
        static member eq code =
            let e = eqmode()
            match programList[prIndex].language with
            |LaTeX ->
                writein "\\begin{align}"
                code e
                writein "\\end{align}"
            |HTML ->
                writein "\\["
                writein "\\begin{align}"
                code e
                writein "\\end{align}"
                writein "\\]"
            |_ ->
                code e
                
        ///<summary>変数（変数リストに追加しない）</summary>
        static member var (tp,name:string) =
            num0(Var(tp,name,NaN))

        ///<summary>単独の数式</summary>
        static member f (a:num0) = a.code
            
        ///<summary>単独の数式</summary>
        static member f (a:bool0) = a.code
            
        ///<summary>単独の数式(インライン)</summary>
        static member fi (a:num0) = "$"+a.code+"$"
            
        ///<summary>単独の数式(インライン)</summary>
        static member fi (a:bool0) = "$"+a.code+"$"
            
        ///<summary>総和</summary>
        static member sum (a:num0,i:num0) = fun (b:num0) (c:num0) ->
            match programList[prIndex].language with
            |LaTeX ->
                let ta = a.code
                let ti = i.code
                let tb = b.code
                let tc = c.code
                num0(Var(c.etype,"\\sum_{"+ta+"="+ti+"}^{"+tb+"} "+tc,NaN))
            |_ ->
                num0 NaN
                
        ///<summary>総和</summary>
        static member sum (a:num0,i:int) = fun (b:num0) (c:num0) ->
            doc.sum (a,I i) b c
            
        ///<summary>総和</summary>
        static member sum (a:num0) = fun (b:num0) (c:num0) ->
            match programList[prIndex].language with
            |LaTeX ->
                let ta = a.code
                let tb = b.code
                let tc = c.code
                num0(Var(c.etype,"\\sum_{"+ta+"}^{"+tb+"} "+tc,NaN))
            |_ ->
                num0 NaN
                
        ///<summary>積分</summary>
        static member integral (a:num0,b:num0) = fun (eq:num0) (x:num0) ->
            match programList[prIndex].language with
            |LaTeX ->
                let ta = a.code
                let tb = b.code
                let te = eq.code
                let tx = x.code
                num0(Var(x.etype,"\\int_{"+ta+"}^{"+tb+"} "+te+"\\mathrm{d}"+tx,NaN))
            |_ ->
                num0 NaN
                
        ///<summary>積分</summary>
        static member integral (a:int,b:num0) = fun (eq:num0) (x:num0) ->
            doc.integral (I a,b) eq x

        ///<summary>積分</summary>
        static member integral (a:num0,b:int) = fun (eq:num0) (x:num0) ->
            doc.integral (a,I b) eq x
            
        ///<summary>積分</summary>
        static member integral (a:int,b:int) = fun (eq:num0) (x:num0) ->
            doc.integral (I a,I b) eq x
            
        ///<summary>微分</summary>
        static member diff (f:num0) (x:num0) =
            match programList[prIndex].language with
            |LaTeX ->
                let tf = f.code
                let tx = x.code
                num0(Var(f.etype,"\\frac{\\mathrm{d}"+tf+"}^{\\mathrm{d}"+tx+"}",NaN))
            |_ ->
                num0 NaN
                
        ///<summary>偏微分</summary>
        static member pdiff (f:num0) (x:num0) =
            match programList[prIndex].language with
            |LaTeX ->
                let tf = f.code
                let tx = x.code
                num0(Var(f.etype,"\\frac{\\partial "+tf+"}^{\\partial "+tx+"}",NaN))
            |_ ->
                num0 NaN
                
        ///<summary>場合分け</summary>
        static member cases (lst:(num0*string)list) =
            match programList[prIndex].language with
            |LaTeX ->
                let c = 
                    lst
                    |> List.map (fun (f,x) -> f.code + " & \\left(" + x + "\\right)\n")
                    |> fun s -> String.Join ("\\\\",s)
                num0(Var(Nt,"\\begin{dcases}\n" + c + "\\end{dcases}",NaN))
            |_ ->
                num0 NaN
                
        ///<summary>場合分け</summary>
        static member cases (lst:(num0*num0)list) =
            match programList[prIndex].language with
            |LaTeX ->
                let c = 
                    lst
                    |> List.map (fun (f,x) -> f.code + " & " + x.code + "\n")
                    |> fun s -> String.Join ("\\\\",s)
                num0(Var(Nt,"\\begin{dcases}\n" + c + "\\end{dcases}",NaN))
            |_ ->
                num0 NaN
                
        ///<summary>場合分け</summary>
        static member cases (lst:(num0*bool0)list) =
            match programList[prIndex].language with
            |LaTeX ->
                let c = 
                    lst
                    |> List.map (fun (f,x) -> f.code + " & \\left(" + x.code + "\\right)\n")
                    |> fun s -> String.Join ("\\\\",s)
                num0(Var(Nt,"\\begin{dcases}"+"\n"+c+"\\end{dcases}",NaN))
            |_ ->
                num0 NaN
              
        ///<summary>括弧「()」</summary>
        static member par1 (v:num0) = num0(Var(v.etype,"\\left("+v.code+"\\right)",NaN))
        
        ///<summary>括弧「[]」</summary>
        static member par2 (v:num0) = num0(Var(v.etype,"\\left["+v.code+"\\right]",NaN))
        
        ///<summary>括弧「[]」+下付き・上付き文字</summary>
        static member par2 (v:num0,a:num0,b:num0) = num0(Var(v.etype,"\\left["+v.code+"\\right]_{"+a.code+"}^{"+b.code+"}",NaN))
        
        ///<summary>括弧「[]」+下付き・上付き文字</summary>
        static member par2 (v:num0,a:int,b:num0) = 
            doc.par2 (v,I a,b)
        
        ///<summary>括弧「[]」+下付き・上付き文字</summary>
        static member par2 (v:num0,a:num0,b:int) = 
            doc.par2 (v,a,I b)
        
        ///<summary>括弧「[]」+下付き・上付き文字</summary>
        static member par2 (v:num0,a:int,b:int) = 
            doc.par2 (v,I a,I b)
        
        ///<summary>括弧「{}」</summary>
        static member par3 (v:num0) = num0(Var(v.etype,"\\left\\{"+v.code+"\\right\\}",NaN))
