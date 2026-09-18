//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    open System

    /// Controls line breaks, labels, and numbering within generated equations.
    type eqmode(context:Aqualis) =
        /// Emits a LaTeX line break; has no effect for other targets.
        member _.eqReturn() =
            match context.language with
            |LaTeX ->
                context.codewritein "\\\\"
            |_ ->
                ()

        /// Suppresses numbering of the current LaTeX equation line.
        member _.eqNonumber() =
            match context.language with
            |LaTeX ->
                context.codewritein "\\nonumber"
            |_ ->
                ()

        /// Emits a LaTeX equation label; has no effect for other targets.
        member _.eqLabel(lb:string) =
            match context.language with
            |LaTeX ->
                context.codewritein("\\label{"+lb+"}")
            |_ ->
                ()

        /// Suppresses numbering and then emits a LaTeX line break.
        member this.nnReturn() =
            this.eqNonumber()
            this.eqReturn()

        /// Gets an empty complex expression for the left side of an equation.
        member _.nl with get() = complex0(Var(Zt,"",NaN))

    /// Emits mathematical document markup through the active generation context.
    type ContextDoc internal (context:Aqualis) =

        /// Emits a LaTeX paragraph marker, then invokes the content callback.
        /// For other targets, it only invokes the callback.
        member this.para code =
            match context.language with
            |LaTeX ->
                context.codewritein "\\par"
                code()
            |_ ->
                code()

        /// Emits text in LaTeX, or a comment for other target languages.
        member this.text (s:string) =
            match context.language with
            |LaTeX ->
                context.codewritein s
            |_ ->
                context.group.comment s

        /// Emits a LaTeX figure with a caption and filename-based label.
        member this.inputfigure (filename:string) (caption:string) =
            context.hlist.add "\\usepackage{graphicx}"
            match context.language with
            |LaTeX ->
                context.codewritein "\\begin{figure}[htbp]"
                context.codewritein "\\begin{center}"
                context.codewritein("\\includegraphics{"+filename+"}")
                context.codewritein "\\end{center}"
                context.codewritein("\\caption{"+caption+"}")
                context.codewritein("\\label{"+filename+"}")
                context.codewritein "\\end{figure}"
            |_ ->
                context.group.comment (filename+": "+caption)

        /// Emits a LaTeX numbered list, or invokes its items directly for other targets.
        member this.enumerate (slst:(unit->unit)list) =
            match context.language with
            |LaTeX ->
                context.codewritein "\\begin{enumerate}"
                for s in slst do
                    context.codewritein "\\item"
                    s()
                context.codewritein "\\end{enumerate}"
            |_ ->
                for s in slst do
                    s()

        /// Emits a LaTeX bulleted list, or invokes its items directly for other targets.
        member this.itemize (slst:(unit->unit)list) =
            match context.language with
            |LaTeX ->
                context.codewritein "\\begin{itemize}"
                for s in slst do
                    context.codewritein "\\item"
                    s()
                context.codewritein "\\end{itemize}"
            |_ ->
                for s in slst do
                    s()

        /// Runs the callback inside an aligned equation in LaTeX or HTML.
        member this.eq code =
            let e = eqmode(context)
            match context.language with
            |LaTeX ->
                context.codewritein "\\begin{align}"
                code e
                context.codewritein "\\end{align}"
            |HTML ->
                context.codewritein "\\["
                context.codewritein "\\begin{align}"
                code e
                context.codewritein "\\end{align}"
                context.codewritein "\\]"
            |_ ->
                code e

        /// Creates a symbolic variable without registering it in the variable list.
        member this.var (tp,name:string) =
            Var(tp,name,NaN)

        /// Returns the generated code of the supplied expression.
        member this.f (a:int0) = a.code
        /// Gets the rendered double-precision expression.
        member this.f (a:double0) = a.code
        /// Gets the rendered complex expression.
        member this.f (a:complex0) = a.code

        /// Returns the generated code of the supplied expression.
        member this.f (a:bool0) = a.code

        /// Wraps the generated expression code in inline math delimiters.
        member this.fi (a:int0) = "$"+a.code+"$"
        /// Wraps a double-precision expression in inline TeX math delimiters.
        member this.fi (a:double0) = "$"+a.code+"$"
        /// Wraps a complex expression in inline TeX math delimiters.
        member this.fi (a:complex0) = "$"+a.code+"$"

        /// Wraps the generated expression code in inline math delimiters.
        member this.fi (a:bool0) = "$"+a.code+"$"

        /// Creates sum notation for LaTeX and HTML output; other targets receive
        /// a placeholder expression with NaN as its numeric value.
        member this.sum (a:int0,i:int0,b:int0,c:double0) =
            match context.language with
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
        /// Creates sum notation for LaTeX and HTML output.
        member this.sum (a:int0,i:int0,b:int0,c:complex0) =
            match context.language with
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

        /// Creates sum notation for LaTeX and HTML output.
        member this.sum (a:int0,b:int0,c:double0) =
            match context.language with
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
        /// Creates sum notation for LaTeX and HTML output.
        member this.sum (a:int0,b:int0,c:complex0) =
            match context.language with
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

        /// Creates definite-integral notation for LaTeX and HTML output; other
        /// targets receive a placeholder expression with NaN as its numeric value.
        member this.integral (a:double0,b:double0,eq:double0,x:double0) =
            match context.language with
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
        /// Creates definite-integral notation for LaTeX and HTML output.
        member this.integral (a:double0,b:double0,eq:complex0,x:double0) =
            match context.language with
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

        /// Creates definite-integral notation for LaTeX and HTML output.
        member this.integral (a:int,b:double0,eq:double0,x:double0) =
            this.integral (D a,b,eq, x)

        /// Creates definite-integral notation for LaTeX and HTML output.
        member this.integral (a:double0,b:int,eq:double0,x:double0) =
            this.integral (a,D b,eq, x)

        /// Creates definite-integral notation for LaTeX and HTML output.
        member this.integral (a:int,b:int,eq:double0,x:double0) =
            this.integral (D a,D b,eq, x)

        /// Creates ordinary-derivative notation for LaTeX and HTML output.
        member this.diff (f:double0,x:double0) =
            match context.language with
            |LaTeX|HTML|HTMLSequenceDiagram ->
                let tf = f.code
                let tx = x.code
                double0(Var(f.etype,"\\frac{\\mathrm{d}"+tf+"}^{\\mathrm{d}"+tx+"}",NaN))
            |_ ->
                double0 NaN

        /// Creates partial-derivative notation for LaTeX and HTML output.
        member this.pdiff (f:double0,x:double0) =
            match context.language with
            |LaTeX|HTML|HTMLSequenceDiagram ->
                let tf = f.code
                let tx = x.code
                double0(Var(f.etype,"\\frac{\\partial "+tf+"}^{\\partial "+tx+"}",NaN))
            |_ ->
                double0 NaN

        /// Creates a cases expression from values and their conditions.
        member this.cases (lst:(double0*string)list) =
            match context.language with
            |LaTeX|HTML|HTMLSequenceDiagram ->
                let c =
                    lst
                    |> List.map (fun (f,x) -> f.code + " & \\left(" + x + "\\right)\n")
                    |> fun s -> String.Join ("\\\\",s)
                double0(Var(Nt,"\\begin{dcases}\n" + c + "\\end{dcases}",NaN))
            |_ ->
                double0 NaN

        /// Creates a cases expression from values and their conditions.
        member this.cases (lst:(double0*double0)list) =
            match context.language with
            |LaTeX|HTML|HTMLSequenceDiagram ->
                let c =
                    lst
                    |> List.map (fun (f,x) -> f.code + " & " + x.code + "\n")
                    |> fun s -> String.Join ("\\\\",s)
                double0(Var(Nt,"\\begin{dcases}\n" + c + "\\end{dcases}",NaN))
            |_ ->
                double0 NaN

        /// Creates a cases expression from values and their conditions.
        member this.cases (lst:(double0*bool0)list) =
            match context.language with
            |LaTeX|HTML|HTMLSequenceDiagram ->
                let c =
                    lst
                    |> List.map (fun (f,x) -> f.code + " & \\left(" + x.code + "\\right)\n")
                    |> fun s -> String.Join ("\\\\",s)
                double0(Var(Nt,"\\begin{dcases}"+"\n"+c+"\\end{dcases}",NaN))
            |_ ->
                double0 NaN

        /// Wraps an expression in scalable parentheses.
        member this.par1 (v:double0) = double0(Var(v.etype,"\\left("+v.code+"\\right)",NaN))

        /// Wraps an expression in scalable square brackets.
        member this.par2 (v:double0) = double0(Var(v.etype,"\\left["+v.code+"\\right]",NaN))

        /// Wraps an expression in square brackets with subscript and superscript.
        member this.par2 (v:double0,a:double0,b:double0) = double0(Var(v.etype,"\\left["+v.code+"\\right]_{"+a.code+"}^{"+b.code+"}",NaN))

        /// Wraps an expression in square brackets with subscript and superscript.
        member this.par2 (v:double0,a:int,b:double0) =
            this.par2 (v,D a,b)

        /// Wraps an expression in square brackets with subscript and superscript.
        member this.par2 (v:double0,a:double0,b:int) =
            this.par2 (v,a,D b)

        /// Wraps an expression in square brackets with subscript and superscript.
        member this.par2 (v:double0,a:int,b:int) =
            this.par2 (v,D a,D b)

        /// Wraps an expression in scalable braces.
        member this.par3 (v:double0) = double0(Var(v.etype,"\\left\\{"+v.code+"\\right\\}",NaN))

    /// Adds document generation access to Aqualis.
    [<AutoOpen>]
    module CompilationEnvironmentDocExtensions =
        type Aqualis with
            /// Gets document-markup helpers bound to this generation context.
            member this.doc = ContextDoc(this)
