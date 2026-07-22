//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    open System

    type eqmode(environment:CompilationEnvironment) =
        let context = environment.RequireGenerationContext()
        let program = context.CurrentProgram
        ///<summary>改行</summary>
        member _.eqReturn() =
            match program.language with
            |LaTeX ->
                program.codewritein "\\\\"
            |_ ->
                ()

        ///<summary>数式番号なし</summary>
        member _.eqNonumber() =
            match program.language with
            |LaTeX ->
                program.codewritein "\\nonumber"
            |_ ->
                ()

        ///<summary>改行</summary>
        member _.eqLabel(lb:string) =
            match program.language with
            |LaTeX ->
                program.codewritein("\\label{"+lb+"}")
            |_ ->
                ()

        ///<summary>数式番号なし改行</summary>
        member this.nnReturn() =
            this.eqNonumber()
            this.eqReturn()

        ///<summary>空白の左辺</summary>
        member _.nl with get() = complex0(Var(Zt,"",NaN))

    ///<summary>変数宣言</summary>
    type ContextDoc internal (environment:CompilationEnvironment) =
        let context = environment.RequireGenerationContext()
        let program = context.CurrentProgram

        ///<summary>段落</summary>
        member this.para code =
            match program.language with
            |LaTeX ->
                program.codewritein "\\par"
                code()
            |_ ->
                code()

        ///<summary>テキスト</summary>
        member this.text (s:string) =
            match program.language with
            |LaTeX ->
                program.codewritein s
            |_ ->
                environment.group.comment s

        ///<summary>図の挿入</summary>
        member this.inputfigure (filename:string) (caption:string) =
            program.hlist.add "\\usepackage{graphicx}"
            match program.language with
            |LaTeX ->
                program.codewritein "\\begin{figure}[htbp]"
                program.codewritein "\\begin{center}"
                program.codewritein("\\includegraphics{"+filename+"}")
                program.codewritein "\\end{center}"
                program.codewritein("\\caption{"+caption+"}")
                program.codewritein("\\label{"+filename+"}")
                program.codewritein "\\end{figure}"
            |_ ->
                environment.group.comment (filename+": "+caption)

        ///<summary>番号付き箇条書き</summary>
        member this.enumerate (slst:(unit->unit)list) =
            match program.language with
            |LaTeX ->
                program.codewritein "\\begin{enumerate}"
                for s in slst do
                    program.codewritein "\\item"
                    s()
                program.codewritein "\\end{enumerate}"
            |_ ->
                for s in slst do
                    s()

        ///<summary>番号なし箇条書き</summary>
        member this.itemize (slst:(unit->unit)list) =
            match program.language with
            |LaTeX ->
                program.codewritein "\\begin{itemize}"
                for s in slst do
                    program.codewritein "\\item"
                    s()
                program.codewritein "\\end{itemize}"
            |_ ->
                for s in slst do
                    s()

        ///<summary>数式</summary>
        member this.eq code =
            let e = eqmode(environment)
            match program.language with
            |LaTeX ->
                program.codewritein "\\begin{align}"
                code e
                program.codewritein "\\end{align}"
            |HTML ->
                program.codewritein "\\["
                program.codewritein "\\begin{align}"
                code e
                program.codewritein "\\end{align}"
                program.codewritein "\\]"
            |_ ->
                code e

        ///<summary>変数（変数リストに追加しない）</summary>
        member this.var (tp,name:string) =
            Var(tp,name,NaN)

        ///<summary>単独の数式</summary>
        member this.f (a:int0) = a.code
        member this.f (a:double0) = a.code
        member this.f (a:complex0) = a.code

        ///<summary>単独の数式</summary>
        member this.f (a:bool0) = a.code

        ///<summary>単独の数式(インライン)</summary>
        member this.fi (a:int0) = "$"+a.code+"$"
        member this.fi (a:double0) = "$"+a.code+"$"
        member this.fi (a:complex0) = "$"+a.code+"$"

        ///<summary>単独の数式(インライン)</summary>
        member this.fi (a:bool0) = "$"+a.code+"$"

        ///<summary>総和</summary>
        member this.sum (a:int0,i:int0,b:int0,c:double0) =
            match program.language with
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
        member this.sum (a:int0,i:int0,b:int0,c:complex0) =
            match program.language with
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
        member this.sum (a:int0,b:int0,c:double0) =
            match program.language with
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
        member this.sum (a:int0,b:int0,c:complex0) =
            match program.language with
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
        member this.integral (a:double0,b:double0,eq:double0,x:double0) =
            match program.language with
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
        member this.integral (a:double0,b:double0,eq:complex0,x:double0) =
            match program.language with
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
        member this.integral (a:int,b:double0,eq:double0,x:double0) =
            this.integral (D a,b,eq, x)

        ///<summary>積分</summary>
        member this.integral (a:double0,b:int,eq:double0,x:double0) =
            this.integral (a,D b,eq, x)

        ///<summary>積分</summary>
        member this.integral (a:int,b:int,eq:double0,x:double0) =
            this.integral (D a,D b,eq, x)

        ///<summary>微分</summary>
        member this.diff (f:double0,x:double0) =
            match program.language with
            |LaTeX|HTML|HTMLSequenceDiagram ->
                let tf = f.code
                let tx = x.code
                double0(Var(f.etype,"\\frac{\\mathrm{d}"+tf+"}^{\\mathrm{d}"+tx+"}",NaN))
            |_ ->
                double0 NaN

        ///<summary>偏微分</summary>
        member this.pdiff (f:double0,x:double0) =
            match program.language with
            |LaTeX|HTML|HTMLSequenceDiagram ->
                let tf = f.code
                let tx = x.code
                double0(Var(f.etype,"\\frac{\\partial "+tf+"}^{\\partial "+tx+"}",NaN))
            |_ ->
                double0 NaN

        ///<summary>場合分け</summary>
        member this.cases (lst:(double0*string)list) =
            match program.language with
            |LaTeX|HTML|HTMLSequenceDiagram ->
                let c =
                    lst
                    |> List.map (fun (f,x) -> f.code + " & \\left(" + x + "\\right)\n")
                    |> fun s -> String.Join ("\\\\",s)
                double0(Var(Nt,"\\begin{dcases}\n" + c + "\\end{dcases}",NaN))
            |_ ->
                double0 NaN

        ///<summary>場合分け</summary>
        member this.cases (lst:(double0*double0)list) =
            match program.language with
            |LaTeX|HTML|HTMLSequenceDiagram ->
                let c =
                    lst
                    |> List.map (fun (f,x) -> f.code + " & " + x.code + "\n")
                    |> fun s -> String.Join ("\\\\",s)
                double0(Var(Nt,"\\begin{dcases}\n" + c + "\\end{dcases}",NaN))
            |_ ->
                double0 NaN

        ///<summary>場合分け</summary>
        member this.cases (lst:(double0*bool0)list) =
            match program.language with
            |LaTeX|HTML|HTMLSequenceDiagram ->
                let c =
                    lst
                    |> List.map (fun (f,x) -> f.code + " & \\left(" + x.code + "\\right)\n")
                    |> fun s -> String.Join ("\\\\",s)
                double0(Var(Nt,"\\begin{dcases}"+"\n"+c+"\\end{dcases}",NaN))
            |_ ->
                double0 NaN

        ///<summary>括弧「()」</summary>
        member this.par1 (v:double0) = double0(Var(v.etype,"\\left("+v.code+"\\right)",NaN))

        ///<summary>括弧「[]」</summary>
        member this.par2 (v:double0) = double0(Var(v.etype,"\\left["+v.code+"\\right]",NaN))

        ///<summary>括弧「[]」+下付き・上付き文字</summary>
        member this.par2 (v:double0,a:double0,b:double0) = double0(Var(v.etype,"\\left["+v.code+"\\right]_{"+a.code+"}^{"+b.code+"}",NaN))

        ///<summary>括弧「[]」+下付き・上付き文字</summary>
        member this.par2 (v:double0,a:int,b:double0) =
            this.par2 (v,D a,b)

        ///<summary>括弧「[]」+下付き・上付き文字</summary>
        member this.par2 (v:double0,a:double0,b:int) =
            this.par2 (v,a,D b)

        ///<summary>括弧「[]」+下付き・上付き文字</summary>
        member this.par2 (v:double0,a:int,b:int) =
            this.par2 (v,D a,D b)

        ///<summary>括弧「{}」</summary>
        member this.par3 (v:double0) = double0(Var(v.etype,"\\left\\{"+v.code+"\\right\\}",NaN))

    [<AutoOpen>]
    module CompilationEnvironmentDocExtensions =
        type CompilationEnvironment with
            member this.doc = ContextDoc(this)
