(*
Copyright (c) 2022 Jun-ichiro Sugisaka

This software is released under the MIT License.
http://opensource.org/licenses/mit-license.php
*)
namespace Aqualis
    
    open System
    open System.IO
    open System.Text
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
        static member enumerate (slst:string list) =
            let q = p.param
            match p.lang with
              |T ->
                q.codewrite("\\begin{enumerate}")
                for s in slst do
                    q.codewrite("\\item")
                    q.codewrite(s)
                q.codewrite("\\end{enumerate}")
              |_ ->
                for s in slst do
                    ! s
                    
        ///<summary>番号なし箇条書き</summary>
        static member itemize (slst:string list) =
            let q = p.param
            match p.lang with
              |T ->
                q.codewrite("\\begin{itemize}")
                for s in slst do
                    q.codewrite("\\item")
                    q.codewrite(s)
                q.codewrite("\\end{itemize}")
              |_ ->
                for s in slst do
                    ! s
                    
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
                
        ///<summary>ラベル付き数式</summary>
        static member eqlb (label:string) = fun code ->
            let q = p.param
            match p.lang with
              |T ->
                AqualisCompiler.set_EquationSimplify OFF
                q.codewrite("\\begin{align}")
                code()
                if not (label="") then q.codewrite("\\label{"+label+"}")
                q.codewrite("\\end{align}")
                AqualisCompiler.set_EquationSimplify ON
              |_ ->
                code()
                
        ///<summary>総和</summary>
        static member sum (a:num0,i:num0) = fun (b:num0) (c:num0) ->
            match p.lang with
              |T ->
                let (_,ta,_) = a.code.str
                let (_,ti,_) = i.code.str
                let (_,tb,_) = b.code.str
                let (t,tc,_) = c.code.str
                num0.Code(t,"\\sum_{"+ta+"="+ti+"}^{"+tb+"} "+tc,[])
              |_ ->
                num0.NaN
                
        ///<summary>総和</summary>
        static member sum (a:num0,i:int) = fun (b:num0) (c:num0) ->
            doc.sum (a,I i) b c
            
        ///<summary>総和</summary>
        static member sum (a:num0) = fun (b:num0) (c:num0) ->
            match p.lang with
              |T ->
                let (_,ta,_) = a.code.str
                let (_,tb,_) = b.code.str
                let (t,tc,_) = c.code.str
                num0.Code(t,"\\sum_{"+ta+"}^{"+tb+"} "+tc,[])
              |_ ->
                num0.NaN
                
        ///<summary>積分</summary>
        static member integral (a:num0,b:num0) = fun (eq:num0) (x:num0) ->
            match p.lang with
              |T ->
                let (_,ta,_) = a.code.str
                let (_,tb,_) = b.code.str
                let (t,te,_) = eq.code.str
                let (_,tx,_) = x.code.str
                num0.Code(t,"\\int_{"+ta+"}^{"+tb+"} "+te+"\\mathrm{d}"+tx,[])
              |_ ->
                num0.NaN
                
        ///<summary>積分</summary>
        static member integral (a:int,b:num0) = fun (eq:num0) (x:num0) ->
            doc.integral (I a,b) eq x

        ///<summary>積分</summary>
        static member integral (a:num0,b:int) = fun (eq:num0) (x:num0) ->
            doc.integral (a,I b) eq x
            
        ///<summary>積分</summary>
        static member integral (a:int,b:int) = fun (eq:num0) (x:num0) ->
            doc.integral (I a, I b) eq x
            
        ///<summary>微分</summary>
        static member diff (f:num0) (x:num0) =
            match p.lang with
              |T ->
                let (t,tf,_) = f.code.str
                let (_,tx,_) = x.code.str
                num0.Code(t,"\\frac{\\mathrm{d}"+tf+"}^{\\mathrm{d}"+tx+"}",[])
              |_ ->
                num0.NaN
                
        ///<summary>偏微分</summary>
        static member pdiff (f:num0) (x:num0) =
            match p.lang with
              |T ->
                let (t,tf,_) = f.code.str
                let (_,tx,_) = x.code.str
                num0.Code(t,"\\frac{\\partial "+tf+"}^{\\partial "+tx+"}",[])
              |_ ->
                num0.NaN

        ///<summary>場合分け</summary>
        static member cases (lst:(num0*num0)list) =
            let q = p.param
            match p.lang with
              |T ->
                let (et,c) = 
                    [0..lst.Length-1]
                    |> List.fold (fun (t,acc) i ->
                        let (f,x) = lst[i]
                        let (s,tf,_) = f.code.str
                        let (_,tx,_) = x.code.str
                        let et = match (s,t) with |Zt,_|_,Zt -> Zt |Dt,_|_,Dt -> Dt |_ -> It 4
                        et,(acc+tf+" & "+tx+(if i=lst.Length-1 then "" else "\\\\"))+"\n") (It 4,"")
                num0.Code(et,"\\begin{dcases}"+"\n"+c+"\\end{dcases}",[])
              |_ ->
                num0.NaN
                
        ///<summary>場合分け</summary>
        static member cases (lst:(num0*bool0)list) =
            let q = p.param
            match p.lang with
              |T ->
                let (et,c) = 
                    [0..lst.Length-1]
                    |> List.fold (fun (t,acc) i ->
                        let (f,x) = lst[i]
                        let (s,tf,_) = f.code.str
                        let (_,tx,_) = x.code.str
                        let et = match (s,t) with |Zt,_|_,Zt -> Zt |Dt,_|_,Dt -> Dt |_ -> It 4
                        et,(acc+tf+" & \\left("+tx+"\\right)"+(if i=lst.Length-1 then "" else "\\\\")+"\n")) (It 4,"")
                num0.Code(et,"\\begin{dcases}"+"\n"+c+"\\end{dcases}",[])
              |_ ->
                num0.NaN
                