(*
Copyright (c) 2022 Jun-ichiro Sugisaka

This software is released under the MIT License.
http://opensource.org/licenses/mit-license.php
*)
namespace Aqualis
    
    open Aqualis_base

    ///<summary>変数宣言</summary>
    type doc () =
        
        ///<summary>段落</summary>
        static member para code =
            let q = p.param
            match p.lang with
              |T ->
                q.codewrite("\\par")
                code()
              |_ ->
                code()
                
        ///<summary>テキスト</summary>
        static member text (s:string) =
            let q = p.param
            match p.lang with
              |T ->
                q.codewrite(s)
              |_ ->
                ! s
                
        ///<summary>番号付き箇条書き</summary>
        static member enumerate (slst:(unit->unit)list) =
            let q = p.param
            match p.lang with
              |T ->
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
              |T ->
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
            match p.lang with
              |T ->
                AqualisCompiler.set_EquationSimplify OFF
                q.codewrite("\\begin{align*}")
                code()
                q.codewrite("\\end{align*}")
                AqualisCompiler.set_EquationSimplify ON
              |_ ->
                code()
                
        ///<summary>改行</summary>
        static member eqReturn() =
            let q = p.param
            match p.lang with
              |T ->
                q.codewrite("\\\\")
              |_ ->
                ()
                
        ///<summary>数式番号なし</summary>
        static member eqNonumber() =
            let q = p.param
            match p.lang with
              |T ->
                q.codewrite("\\nonumber")
              |_ ->
                ()
                
        ///<summary>改行</summary>
        static member eqLabel(lb:string) =
            let q = p.param
            match p.lang with
              |T ->
                q.codewrite("\\label{"+lb+"}")
              |_ ->
                ()
                
        ///<summary>変数（変数リストに追加しない）</summary>
        static member var (tp,name:string) =
            Var(name)

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
              |T ->
                let ta = a.code
                let ti = i.code
                let tb = b.code
                let tc = c.code
                Expr.Var("\\sum_{"+ta+"="+ti+"}^{"+tb+"} "+tc)
              |_ ->
                Expr.NaN
                
        ///<summary>総和</summary>
        static member sum (a:num0,i:int) = fun (b:num0) (c:num0) ->
            doc.sum (a,i.I) b c
            
        ///<summary>総和</summary>
        static member sum (a:num0) = fun (b:num0) (c:num0) ->
            match p.lang with
              |T ->
                let ta = a.code
                let tb = b.code
                let tc = c.code
                Expr.Var("\\sum_{"+ta+"}^{"+tb+"} "+tc)
              |_ ->
                Expr.NaN
                
        ///<summary>積分</summary>
        static member integral (a:num0,b:num0) = fun (eq:num0) (x:num0) ->
            match p.lang with
              |T ->
                let ta = a.code
                let tb = b.code
                let te = eq.code
                let tx = x.code
                Var("\\int_{"+ta+"}^{"+tb+"} "+te+"\\mathrm{d}"+tx)
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
              |T ->
                let tf = f.code
                let tx = x.code
                Var("\\frac{\\mathrm{d}"+tf+"}^{\\mathrm{d}"+tx+"}")
              |_ ->
                NaN
                
        ///<summary>偏微分</summary>
        static member pdiff (f:num0) (x:num0) =
            match p.lang with
              |T ->
                let tf = f.code
                let tx = x.code
                Var("\\frac{\\partial "+tf+"}^{\\partial "+tx+"}")
              |_ ->
                NaN
                
        ///<summary>場合分け</summary>
        static member cases (lst:(num0*string)list) =
            let q = p.param
            match p.lang with
              |T ->
                let c = 
                    [0..lst.Length-1]
                    |> List.fold (fun acc i ->
                        let (f,x) = lst[i]
                        let tf = f.code
                        (acc+tf+" & \\left("+x+"\\right)"+(if i=lst.Length-1 then "" else "\\\\")+"\n")) ""
                Var("\\begin{dcases}"+"\n"+c+"\\end{dcases}")
              |_ ->
                NaN
                
        ///<summary>場合分け</summary>
        static member cases (lst:(num0*num0)list) =
            let q = p.param
            match p.lang with
              |T ->
                let c = 
                    [0..lst.Length-1]
                    |> List.fold (fun acc i ->
                        let (f,x) = lst[i]
                        let tf = f.code
                        let tx = x.code
                        (acc+tf+" & "+tx+(if i=lst.Length-1 then "" else "\\\\"))+"\n") ""
                Var("\\begin{dcases}"+"\n"+c+"\\end{dcases}")
              |_ ->
                NaN
                
        ///<summary>場合分け</summary>
        static member cases (lst:(num0*bool0)list) =
            let q = p.param
            match p.lang with
              |T ->
                let c = 
                    [0..lst.Length-1]
                    |> List.fold (fun acc i ->
                        let (f,x) = lst[i]
                        let tf = f.code
                        let tx = x.code
                        (acc+tf+" & \\left("+tx+"\\right)"+(if i=lst.Length-1 then "" else "\\\\")+"\n")) ""
                Var("\\begin{dcases}"+"\n"+c+"\\end{dcases}")
              |_ ->
                NaN
                