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
            match (GenerationScope.currentProgram()).language with
            |LaTeX ->
                writein "\\\\"
            |_ ->
                ()

        ///<summary>数式番号なし</summary>
        member _.eqNonumber() =
            match (GenerationScope.currentProgram()).language with
            |LaTeX ->
                writein "\\nonumber"
            |_ ->
                ()

        ///<summary>改行</summary>
        member _.eqLabel(lb:string) =
            match (GenerationScope.currentProgram()).language with
            |LaTeX ->
                writein("\\label{"+lb+"}")
            |_ ->
                ()

        ///<summary>数式番号なし改行</summary>
        member this.nnReturn() =
            this.eqNonumber()
            this.eqReturn()

        ///<summary>空白の左辺</summary>
        member _.nl with get() = complex0(Var(Zt,"",NaN))

    ///<summary>変数宣言</summary>
    type doc () =

        ///<summary>段落</summary>
        static member para code =
            match (GenerationScope.currentProgram()).language with
            |LaTeX ->
                writein "\\par"
                code()
            |_ ->
                code()

        ///<summary>テキスト</summary>
        static member text (s:string) =
            match (GenerationScope.currentProgram()).language with
            |LaTeX ->
                writein s
            |_ ->
                ! s

        ///<summary>図の挿入</summary>
        static member inputfigure (filename:string) (caption:string) =
            (GenerationScope.currentProgram()).hlist.add "\\usepackage{graphicx}"
            match (GenerationScope.currentProgram()).language with
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
            match (GenerationScope.currentProgram()).language with
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
            match (GenerationScope.currentProgram()).language with
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
            match (GenerationScope.currentProgram()).language with
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
            Var(tp,name,NaN)

        ///<summary>単独の数式</summary>
        static member f (a:int0) = a.code
        static member f (a:double0) = a.code
        static member f (a:complex0) = a.code

        ///<summary>単独の数式</summary>
        static member f (a:bool0) = a.code

        ///<summary>単独の数式(インライン)</summary>
        static member fi (a:int0) = "$"+a.code+"$"
        static member fi (a:double0) = "$"+a.code+"$"
        static member fi (a:complex0) = "$"+a.code+"$"

        ///<summary>単独の数式(インライン)</summary>
        static member fi (a:bool0) = "$"+a.code+"$"

        ///<summary>総和</summary>
        static member sum (a:int0,i:int0,b:int0,c:double0) =
            match (GenerationScope.currentProgram()).language with
            |LaTeX|HTML|HTMLSequenceDiagram ->
                let ta = a.code
                let ti = i.code
                let tb = b.code
                let tc =
                    match c.Expr with
                    |Add _ |Sub _ ->
                        "\\left[" + c.code + "\\right]"
                    |_ ->
                        c.code
                double0(Var(c.etype,"\\sum_{"+ta+"="+ti+"}^{"+tb+"} "+tc,NaN))
            |_ ->
                double0 NaN
        ///<summary>総和</summary>
        static member sum (a:int0,i:int0,b:int0,c:complex0) =
            match (GenerationScope.currentProgram()).language with
            |LaTeX|HTML|HTMLSequenceDiagram ->
                let ta = a.code
                let ti = i.code
                let tb = b.code
                let tc =
                    match c.Expr with
                    |Add _ |Sub _ ->
                        "\\left[" + c.code + "\\right]"
                    |_ ->
                        c.code
                complex0(Var(c.etype,"\\sum_{"+ta+"="+ti+"}^{"+tb+"} "+tc,NaN))
            |_ ->
                complex0 NaN

        ///<summary>総和</summary>
        static member sum (a:int0,b:int0,c:double0) =
            match (GenerationScope.currentProgram()).language with
            |LaTeX|HTML|HTMLSequenceDiagram ->
                let ta = a.code
                let tb = b.code
                let tc =
                    match c.Expr with
                    |Add _ |Sub _ ->
                        "\\left[" + c.code + "\\right]"
                    |_ ->
                        c.code
                double0(Var(c.etype,"\\sum_{"+ta+"}^{"+tb+"} "+tc,NaN))
            |_ ->
                double0 NaN
        ///<summary>総和</summary>
        static member sum (a:int0,b:int0,c:complex0) =
            match (GenerationScope.currentProgram()).language with
            |LaTeX|HTML|HTMLSequenceDiagram ->
                let ta = a.code
                let tb = b.code
                let tc =
                    match c.Expr with
                    |Add _ |Sub _ ->
                        "\\left[" + c.code + "\\right]"
                    |_ ->
                        c.code
                complex0(Var(c.etype,"\\sum_{"+ta+"}^{"+tb+"} "+tc,NaN))
            |_ ->
                complex0 NaN

        ///<summary>積分</summary>
        static member integral (a:double0,b:double0,eq:double0,x:double0) =
            match (GenerationScope.currentProgram()).language with
            |LaTeX|HTML|HTMLSequenceDiagram ->
                let ta = a.code
                let tb = b.code
                let te =
                    match eq.Expr with
                    |Add _ |Sub _ ->
                        "\\left[" + eq.code + "\\right]"
                    |_ ->
                        eq.code
                let tx = x.code
                double0(Var(eq.etype,"\\int_{"+ta+"}^{"+tb+"} "+te+"\\mathrm{d}"+tx,NaN))
            |_ ->
                double0 NaN
        ///<summary>積分</summary>
        static member integral (a:double0,b:double0,eq:complex0,x:double0) =
            match (GenerationScope.currentProgram()).language with
            |LaTeX|HTML|HTMLSequenceDiagram ->
                let ta = a.code
                let tb = b.code
                let te =
                    match eq.Expr with
                    |Add _ |Sub _ ->
                        "\\left[" + eq.code + "\\right]"
                    |_ ->
                        eq.code
                let tx = x.code
                complex0(Var(eq.etype,"\\int_{"+ta+"}^{"+tb+"} "+te+"\\mathrm{d}"+tx,NaN))
            |_ ->
                complex0 NaN

        ///<summary>積分</summary>
        static member integral (a:int,b:double0,eq:double0,x:double0) =
            doc.integral (D a,b,eq, x)

        ///<summary>積分</summary>
        static member integral (a:double0,b:int,eq:double0,x:double0) =
            doc.integral (a,D b,eq, x)

        ///<summary>積分</summary>
        static member integral (a:int,b:int,eq:double0,x:double0) =
            doc.integral (D a,D b,eq, x)

        ///<summary>微分</summary>
        static member diff (f:double0,x:double0) =
            match (GenerationScope.currentProgram()).language with
            |LaTeX|HTML|HTMLSequenceDiagram ->
                let tf = f.code
                let tx = x.code
                double0(Var(f.etype,"\\frac{\\mathrm{d}"+tf+"}^{\\mathrm{d}"+tx+"}",NaN))
            |_ ->
                double0 NaN

        ///<summary>偏微分</summary>
        static member pdiff (f:double0,x:double0) =
            match (GenerationScope.currentProgram()).language with
            |LaTeX|HTML|HTMLSequenceDiagram ->
                let tf = f.code
                let tx = x.code
                double0(Var(f.etype,"\\frac{\\partial "+tf+"}^{\\partial "+tx+"}",NaN))
            |_ ->
                double0 NaN

        ///<summary>場合分け</summary>
        static member cases (lst:(double0*string)list) =
            match (GenerationScope.currentProgram()).language with
            |LaTeX|HTML|HTMLSequenceDiagram ->
                let c =
                    lst
                    |> List.map (fun (f,x) -> f.code + " & \\left(" + x + "\\right)\n")
                    |> fun s -> String.Join ("\\\\",s)
                double0(Var(Nt,"\\begin{dcases}\n" + c + "\\end{dcases}",NaN))
            |_ ->
                double0 NaN

        ///<summary>場合分け</summary>
        static member cases (lst:(double0*double0)list) =
            match (GenerationScope.currentProgram()).language with
            |LaTeX|HTML|HTMLSequenceDiagram ->
                let c =
                    lst
                    |> List.map (fun (f,x) -> f.code + " & " + x.code + "\n")
                    |> fun s -> String.Join ("\\\\",s)
                double0(Var(Nt,"\\begin{dcases}\n" + c + "\\end{dcases}",NaN))
            |_ ->
                double0 NaN

        ///<summary>場合分け</summary>
        static member cases (lst:(double0*bool0)list) =
            match (GenerationScope.currentProgram()).language with
            |LaTeX|HTML|HTMLSequenceDiagram ->
                let c =
                    lst
                    |> List.map (fun (f,x) -> f.code + " & \\left(" + x.code + "\\right)\n")
                    |> fun s -> String.Join ("\\\\",s)
                double0(Var(Nt,"\\begin{dcases}"+"\n"+c+"\\end{dcases}",NaN))
            |_ ->
                double0 NaN

        ///<summary>括弧「()」</summary>
        static member par1 (v:double0) = double0(Var(v.etype,"\\left("+v.code+"\\right)",NaN))

        ///<summary>括弧「[]」</summary>
        static member par2 (v:double0) = double0(Var(v.etype,"\\left["+v.code+"\\right]",NaN))

        ///<summary>括弧「[]」+下付き・上付き文字</summary>
        static member par2 (v:double0,a:double0,b:double0) = double0(Var(v.etype,"\\left["+v.code+"\\right]_{"+a.code+"}^{"+b.code+"}",NaN))

        ///<summary>括弧「[]」+下付き・上付き文字</summary>
        static member par2 (v:double0,a:int,b:double0) =
            doc.par2 (v,D a,b)

        ///<summary>括弧「[]」+下付き・上付き文字</summary>
        static member par2 (v:double0,a:double0,b:int) =
            doc.par2 (v,a,D b)

        ///<summary>括弧「[]」+下付き・上付き文字</summary>
        static member par2 (v:double0,a:int,b:int) =
            doc.par2 (v,D a,D b)

        ///<summary>括弧「{}」</summary>
        static member par3 (v:double0) = double0(Var(v.etype,"\\left\\{"+v.code+"\\right\\}",NaN))
