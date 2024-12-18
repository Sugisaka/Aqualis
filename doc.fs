(*
Copyright (c) 2022 Jun-ichiro Sugisaka

This software is released under the MIT License.
http://opensource.org/licenses/mit-license.php
*)
namespace Aqualis
    
    open Aqualis_base
    
    type eqmode() =
        ///<summary>改行</summary>
        member _.eqReturn() =
            let q = p.param
            match p.lang with
            |LaTeX ->
                q.codewrite("\\\\")
            |_ ->
                ()
                
        ///<summary>数式番号なし</summary>
        member _.eqNonumber() =
            let q = p.param
            match p.lang with
            |LaTeX ->
                q.codewrite("\\nonumber")
            |_ ->
                ()
                
        ///<summary>改行</summary>
        member _.eqLabel(lb:string) =
            let q = p.param
            match p.lang with
            |LaTeX ->
                q.codewrite("\\label{"+lb+"}")
            |_ ->
                ()
                
        ///<summary>数式番号なし改行</summary>
        member this.nnReturn() =
            this.eqNonumber()
            this.eqReturn()
            
        ///<summary>空白の左辺</summary>
        member _.nl with get() = Var(Zt,"")
        
    ///<summary>変数宣言</summary>
    type doc () =
        
        ///<summary>段落</summary>
        static member para code =
            let q = p.param
            match p.lang with
            |LaTeX ->
                q.codewrite("\\par")
                code()
            |_ ->
                code()
                
        ///<summary>テキスト</summary>
        static member text (s:string) =
            let q = p.param
            match p.lang with
            |LaTeX ->
                q.codewrite(s)
            |_ ->
                ! s
                
        ///<summary>図の挿入</summary>
        static member inputfigure (filename:string) (caption:string) =
            let q = p.param
            p.incld("\\usepackage{graphicx}")
            match p.lang with
            |LaTeX ->
                q.codewrite("\\begin{figure}[htbp]")
                q.codewrite("\\begin{center}")
                q.codewrite("\\includegraphics{"+filename+"}")
                q.codewrite("\\end{center}")
                q.codewrite("\\caption{"+caption+"}")
                q.codewrite("\\label{"+filename+"}")
                q.codewrite("\\end{figure}")
            |_ ->
                ! (filename+": "+caption)
                
        ///<summary>番号付き箇条書き</summary>
        static member enumerate (slst:(unit->unit)list) =
            let q = p.param
            match p.lang with
            |LaTeX ->
                q.codewrite("\\begin{enumerate}")
                for s in slst do
                    q.codewrite("\\item")
                    s()
                q.codewrite("\\end{enumerate}")
            |_ ->
                for s in slst do
                    s()
                    
        ///<summary>番号なし箇条書き</summary>
        static member itemize (slst:(unit->unit)list) =
            let q = p.param
            match p.lang with
            |LaTeX ->
                q.codewrite("\\begin{itemize}")
                for s in slst do
                    q.codewrite("\\item")
                    s()
                q.codewrite("\\end{itemize}")
            |_ ->
                for s in slst do
                    s()
                    
        ///<summary>数式</summary>
        static member eq code =
            let q = p.param
            let e = eqmode()
            match p.lang with
            |LaTeX ->
                AqualisCompiler.set_EquationSimplify OFF
                q.codewrite("\\begin{align}")
                code e
                q.codewrite("\\end{align}")
                AqualisCompiler.set_EquationSimplify ON
            |HTML ->
                AqualisCompiler.set_EquationSimplify OFF
                q.codewrite("\\[")
                q.codewrite("\\begin{align}")
                code e
                q.codewrite("\\end{align}")
                q.codewrite("\\]")
                AqualisCompiler.set_EquationSimplify ON
            |_ ->
                code e
                
        ///<summary>変数（変数リストに追加しない）</summary>
        static member var (tp,name:string) =
            Var(tp,name)

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
            match p.lang with
            |LaTeX ->
                p.param.frac_style <- 1
                let ta = a.code
                let ti = i.code
                p.param.frac_style <- 0
                let tb = b.code
                let tc = c.code
                Var(c.etype,"\\sum_{"+ta+"="+ti+"}^{"+tb+"} "+tc)
            |_ ->
                NaN
                
        ///<summary>総和</summary>
        static member sum (a:num0,i:int) = fun (b:num0) (c:num0) ->
            doc.sum (a,i.I) b c
            
        ///<summary>総和</summary>
        static member sum (a:num0) = fun (b:num0) (c:num0) ->
            match p.lang with
            |LaTeX ->
                p.param.frac_style <- 1
                let ta = a.code
                p.param.frac_style <- 0
                let tb = b.code
                let tc = c.code
                Var(c.etype,"\\sum_{"+ta+"}^{"+tb+"} "+tc)
            |_ ->
                NaN
                
        ///<summary>積分</summary>
        static member integral (a:num0,b:num0) = fun (eq:num0) (x:num0) ->
            match p.lang with
            |LaTeX ->
                p.param.frac_style <- 1
                let ta = a.code
                let tb = b.code
                p.param.frac_style <- 0
                let te = eq.code
                let tx = x.code
                Var(x.etype,"\\int_{"+ta+"}^{"+tb+"} "+te+"\\mathrm{d}"+tx)
            |_ ->
                NaN
                
        ///<summary>積分</summary>
        static member integral (a:int,b:num0) = fun (eq:num0) (x:num0) ->
            doc.integral (a.I,b) eq x

        ///<summary>積分</summary>
        static member integral (a:num0,b:int) = fun (eq:num0) (x:num0) ->
            doc.integral (a,b.I) eq x
            
        ///<summary>積分</summary>
        static member integral (a:int,b:int) = fun (eq:num0) (x:num0) ->
            doc.integral (a.I,b.I) eq x
            
        ///<summary>微分</summary>
        static member diff (f:num0) (x:num0) =
            match p.lang with
            |LaTeX ->
                let tf = f.code
                let tx = x.code
                Var(f.etype,"\\frac{\\mathrm{d}"+tf+"}^{\\mathrm{d}"+tx+"}")
            |_ ->
                NaN
                
        ///<summary>偏微分</summary>
        static member pdiff (f:num0) (x:num0) =
            match p.lang with
            |LaTeX ->
                let tf = f.code
                let tx = x.code
                Var(f.etype,"\\frac{\\partial "+tf+"}^{\\partial "+tx+"}")
            |_ ->
                NaN
                
        ///<summary>場合分け</summary>
        static member cases (lst:(num0*string)list) =
            let q = p.param
            match p.lang with
            |LaTeX ->
                let c = 
                    [0..lst.Length-1]
                    |> List.fold (fun acc i ->
                        let (f,x) = lst[i]
                        let tf = f.code
                        (acc+tf+" & \\left("+x+"\\right)"+(if i=lst.Length-1 then "" else "\\\\")+"\n")) ""
                Var(Nt,"\\begin{dcases}"+"\n"+c+"\\end{dcases}")
            |_ ->
                NaN
                
        ///<summary>場合分け</summary>
        static member cases (lst:(num0*num0)list) =
            let q = p.param
            match p.lang with
            |LaTeX ->
                let c = 
                    [0..lst.Length-1]
                    |> List.fold (fun acc i ->
                        let (f,x) = lst[i]
                        let tf = f.code
                        let tx = x.code
                        (acc+tf+" & "+tx+(if i=lst.Length-1 then "" else "\\\\"))+"\n") ""
                Var(Nt,"\\begin{dcases}"+"\n"+c+"\\end{dcases}")
            |_ ->
                NaN
                
        ///<summary>場合分け</summary>
        static member cases (lst:(num0*bool0)list) =
            let q = p.param
            match p.lang with
            |LaTeX ->
                let c = 
                    [0..lst.Length-1]
                    |> List.fold (fun acc i ->
                        let (f,x) = lst[i]
                        let tf = f.code
                        let tx = x.code
                        (acc+tf+" & \\left("+tx+"\\right)"+(if i=lst.Length-1 then "" else "\\\\")+"\n")) ""
                Var(Nt,"\\begin{dcases}"+"\n"+c+"\\end{dcases}")
            |_ ->
                NaN
                
        ///<summary>括弧「()」</summary>
        static member par1 (v:num0) = Par(v.etype,"\\left(","\\right)",v)
        
        ///<summary>括弧「[]」</summary>
        static member par2 (v:num0) = Par(v.etype,"\\left[","\\right]",v)
        
        ///<summary>括弧「[]」+下付き・上付き文字</summary>
        static member par2 (v:num0,a:num0,b:num0) = Par(v.etype,"\\left[","\\right]_{"+a.code+"}^{"+b.code+"}",v)
        
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
        static member par3 (v:num0) = Par(v.etype,"\\left\\{","\\right\\}",v)