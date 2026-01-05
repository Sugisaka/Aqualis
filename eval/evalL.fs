// 
// Copyright (c) 2026 Jun-ichiro Sugisaka
// 
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
// 
namespace Aqualis
    
    [<AutoOpen>]
    module exprEvalL =
        
        open System
        
        type expr with
            
            static member substL (x:expr) (y:expr) (c:program) =
                c.codewritein "\\begin{align}"
                c.codewritein (x.evalL c  + " \\leftarrow " + y.evalL c)
                c.codewritein "\\end{align}"

                
            static member equivL (x:expr) (y:expr) (c:program) =
                c.codewritein (x.evalL c  + " = " + y.evalL c)
                
            static member equivAlignL (x:expr) (y:expr) (c:program) =
                c.codewritein (x.evalL c  + " =& " + y.evalL c)
                
            static member forLoopL (c:program) (n1:expr,n2:expr) code =
                let iname,returnVar = c.i0.getVar()
                let i = Var(It 4, iname, NaN)
                let n1_ = n1.evalL c
                let n2_ = n2.evalL c
                c.codewritein("for $" + i.evalL c + "=" + n1_ + "\\cdots " + n2_ + "$\\\\")
                c.indentInc()
                code i
                c.indentDec()
                c.codewritein "end\\\\"
                returnVar()
                
            ///<summary>無限ループ</summary>
            static member loopL (c:program) code =
                let iname,returnVar = c.i0.getVar()
                let i = Var(It 4, iname, NaN)
                let label = gotoLabel.nextGotoLabel()
                let exit() = c.codewritein("goto " + label)
                expr.substL i (Int 1) c
                c.codewritein "do"
                c.indentInc()
                code(exit,i)
                expr.substL i (Add(It 4, i, Int 1)) c
                c.indentDec()
                c.codewritein "end do"
                c.codewritein(label + " continue")
                returnVar()
                
            ///<summary>条件を満たす間ループ</summary>
            static member whiledoL (c:program) (cond:expr) = fun code ->
                c.codewritein("while " + cond.evalL c + "\\\\")
                c.indentInc()
                code()
                c.indentDec()
                c.codewritein "end\\\\"
                
            ///<summary>指定した範囲でループ</summary>
            static member rangeL (c:program) (i1:expr) = fun (i2:expr) -> fun code -> 
                match i1,i2 with
                |Int a, Int b when a>b -> 
                    let iname,returnVar = c.i0.getVar()
                    let i = Var(It 4, iname, NaN)
                    c.comment("for $" + i.evalL c + "=" + i1.evalL c + "\\cdots " + i2.evalL c + "$\\\\")
                    c.indentInc()
                    code i
                    c.indentDec()
                    c.comment "end\\\\"
                    returnVar()
                |i1,i2 ->
                    let iname,returnVar = c.i0.getVar()
                    let i = Var(It 4, iname, NaN)
                    c.codewritein("for $" + i.evalL c + "=" + i1.evalL c + "\\cdots " + i2.evalL c + "$\\\\")
                    c.indentInc()
                    code i
                    c.indentDec()
                    c.codewritein "end\\\\"
                    returnVar()
                    
            ///<summary>指定した範囲でループ(途中脱出可)</summary>
            static member range_exitL (c:program) (i1:expr) = fun (i2:expr) -> fun code -> 
                match i1,i2 with
                |Int a, Int b when a>b -> 
                    let iname,returnVar = c.i0.getVar()
                    let i = Var(It 4, iname, NaN)
                    let label = gotoLabel.nextGotoLabel()
                    let exit() = c.codewritein("goto " + label)
                    c.comment("for $" + i.evalL c + "=" + i1.evalL c + "\\cdots " + i2.evalL c + "$\\\\")
                    c.indentInc()
                    code(exit,i)
                    c.indentDec()
                    c.comment "end"
                    c.comment(label + " continue")
                    returnVar()
                |_ ->
                    let iname,returnVar = c.i0.getVar()
                    let i = Var(It 4, iname, NaN)
                    let label = gotoLabel.nextGotoLabel()
                    let exit() = c.codewritein("goto " + label)
                    c.codewritein("for $" + i.evalL c + "=" + i1.evalL c + "\\cdots " + i2.evalL c + "$\\\\")
                    c.indentInc()
                    code(exit,i)
                    c.indentDec()
                    c.codewritein "end"
                    c.codewritein(label + " continue")
                    returnVar()
                    
            static member branchL (c:program) code =
                let ifcode (cond:expr) code =
                    let cond = cond.evalL c
                    c.codewritein("if " + cond)
                    c.indentInc()
                    code()
                    c.indentDec()
                let elseifcode (cond:expr) code =
                    let cond = cond.evalL c
                    c.codewritein("else if " + cond)
                    c.indentInc()
                    code()
                    c.indentDec()
                let elsecode code =
                    c.codewritein "else"
                    c.indentInc()
                    code()
                    c.indentDec()
                code(ifcode,elseifcode,elsecode)
                c.codewritein "endif"
                
            member this.evalL(c:program) =
                let par (s:string) (pl:int) =
                    match pl%3 with
                    |2 -> "\\left\\{" + s + "\\right\\}"
                    |1 -> "\\left[" + s + "\\right]"
                    |_ -> "\\left(" + s + "\\right)"
                let rec eval (u:expr) (pl:int) : string*int =
                    match u with
                    |False -> "false",pl
                    |True -> "true",pl
                    |Eq(x,y) -> 
                        let x,nx = eval x pl
                        let y,ny = eval y pl
                        x + " = " + y, max nx ny
                    |NEq(x,y) -> 
                        let x,nx = eval x pl
                        let y,ny = eval y pl
                        x + " \\neq " + y, max nx ny
                    |Greater(x,y) ->
                        let x,nx = eval x pl
                        let y,ny = eval y pl
                        x + " > " + y, max nx ny
                    |GreaterEq(x,y) ->
                        let x,nx = eval x pl
                        let y,ny = eval y pl
                        x + " \\geq " + y, max nx ny
                    |Less(x,y) ->
                        let x,nx = eval x pl
                        let y,ny = eval y pl
                        x + " < " + y, max nx ny
                    |LessEq(x,y) ->
                        let x,nx = eval x pl
                        let y,ny = eval y pl
                        x + " \\leq " + y, max nx ny
                    |AND x -> 
                        x 
                        |> List.map (fun v -> 
                            match v with 
                            |OR _ |AND _ ->
                                let v,nv = eval v pl 
                                par v nv
                            |_ -> 
                                let v,_ = eval v pl
                                v)
                        |> fun lst -> String.Join(" \\cap ", lst),1
                    |OR x -> 
                        x 
                        |> List.map (fun v -> 
                            match v with 
                            |OR _ |AND _ ->
                                let v,nv = eval v pl 
                                par v nv
                            |_ -> 
                                let v,_ = eval v pl
                                v)
                        |> fun lst -> String.Join(" \\cup ", lst),1
                    |Int x -> c.numFormat.ItoS x, pl
                    |Dbl x -> c.numFormat.DtoS x, pl
                    |Cpx (0.0,1.0) -> "uj", pl
                    |Cpx (re,im) -> eval (Dbl re + Cpx(0.0,1.0) * Dbl im) pl
                    |Var (_,s,_) -> s, pl
                    |Inv(_,x) -> 
                        match x with
                        |Add _|Sub _ ->
                            let x,nx = eval x pl
                            "-" + par x nx, nx+1
                        |_ ->
                            let x,nx = eval x pl
                            "-" + x, nx
                    |Add(_,x,y) -> 
                        let x,nx = eval x pl
                        let y,ny = eval y pl
                        x + "+" + y, max nx ny
                    |Sub(_,x,y) -> 
                        match x,y with
                        |x,(Add _|Sub _) ->
                            let x,_  = eval x pl
                            let y,ny = eval y pl
                            x + "-" + par y ny, ny+1
                        |_ ->
                            let x,ny = eval x pl
                            let y,nx = eval y pl
                            x + "-" + y, max nx ny
                    |Mul(_,x,y) ->
                        match x,y with
                        |(Add _|Sub _),(Int _| Dbl _) ->
                            let x,nx = eval x pl
                            let y,ny = eval y pl
                            y + par x (nx+1), max (nx+1) ny
                        |_,(Int _| Dbl _) ->
                            let x,nx = eval x pl
                            let y,ny = eval y pl
                            x + y, max nx ny
                        |(Add _|Sub _),(Add _|Sub _) ->
                            let x,nx = eval x pl
                            let y,ny = eval y pl
                            par x nx + par y ny, max (nx+1) (ny+1)
                        |(Add _|Sub _),_ ->
                            let x,nx = eval x pl
                            let y,ny = eval y pl
                            par x nx + y, max (nx+1) ny
                        |_,(Add _|Sub _) ->
                            let x,nx = eval x pl
                            let y,ny = eval y pl
                            x + par y ny, max nx (ny+1)
                        |_ ->
                            let x,nx = eval x pl
                            let y,ny = eval y pl
                            x + " " + y, max nx ny
                    |Div(It 4,x,y) ->
                        eval (Floor(x/y)) pl
                    |Div(_,x,y) ->
                        let x,nx = eval x pl
                        let y,ny = eval y pl
                        "\\frac{\\displaystyle " + x + "}{\\displaystyle " + y + "}", max nx ny
                    |Mod(_,x,y) ->
                        let x,nx = eval x 0
                        let y,ny = eval y 0
                        "\\bmod(" + x + "," + y + ")", pl
                    |Pow(_,x,y) ->
                        let x,nx = eval x pl
                        let y,ny = eval y pl
                        x + " ** " + y, pl
                    |Exp(_,x) ->
                        let x,nx = eval x pl
                        "\\exp" + par x nx, nx+1
                    |Sin(_,x) ->
                        let x,nx = eval x pl
                        "\\sin" + par x nx, nx+1
                    |Cos(_,x) ->
                        let x,nx = eval x pl
                        "\\cos" + par x nx, nx+1
                    |Tan(_,x) ->
                        let x,nx = eval x pl
                        "\\tan" + par x nx, nx+1
                    |Asin(_,x) ->
                        let x,nx = eval x pl
                        "\\arcsin" + par x nx, nx+1
                    |Acos(_,x) ->
                        let x,nx = eval x pl
                        "\\arccos" + par x nx, nx+1
                    |Atan(_,x) ->
                        let x,nx = eval x pl
                        "\\arctan" + par x nx, nx+1
                    |Atan2(x,y) ->
                        let x,nx = eval x pl
                        let y,ny = eval y pl
                        "\\arctan" + par (x + "," + y) (max nx ny), max nx ny + 1
                    |Abs(_,x) ->
                        let x,nx = eval x 0
                        "\\left|" + x + "\\right|", pl
                    |Log(_,x) ->
                        let x,nx = eval x pl
                        "\\log" + par x nx, nx+1
                    |Log10(_,x) ->
                        let x,nx = eval x pl
                        "\\log_{10}" + par x nx, nx+1
                    |Sqrt(_,x) ->
                        let x,nx = eval x 0
                        "\\sqrt{" + x + "}", pl
                    |ToInt x ->
                        let x,nx = eval x pl
                        "int" + par x nx, nx+1
                    |ToDbl x ->
                        let x,nx = eval x pl
                        "double" + par x nx, nx+1
                    |Floor x ->
                        let x,nx = eval x 0
                        "\\lfloor " + x + "\\rfloor", pl
                    |Ceil x ->
                        let x,nx = eval x 0
                        "\\lceil " + x+ "\\rceil", pl
                    |Re x ->
                        let x,nx = eval x pl
                        "\\mathrm{Re}" + par x nx, nx+1
                    |Im x ->
                        let x,nx = eval x pl
                        "\\mathrm{Im}" + par x nx, nx+1
                    |Conj x ->
                        let x,nx = eval x 0
                        "\\bar{" + x + "}", pl
                    |Idx1 (_,name,i) ->
                        let i,ni = eval i 0
                        name + "_{" + i + "}", pl
                    |Idx2 (_,name,i,j) ->
                        let i,ni = eval i 0
                        let j,nj = eval j 0
                        name + "_{" + i + "," + j + "}", pl
                    |Idx3 (_,name,i,j,k) ->
                        let i,ni = eval i 0
                        let j,nj = eval j 0
                        let k,nk = eval k 0
                        name + "_{" + i + "," + j + "," + k + "}", pl
                    |Let (t,y,f) -> 
                        let x =
                            match t with
                            |It 4 -> Var (t, (fun (a,_) -> a) (c.i0.getVar()), y)
                            |Dt   -> Var (t, (fun (a,_) -> a) (c.d0.getVar()), y)
                            |Zt   -> Var (t, (fun (a,_) -> a) (c.z0.getVar()), y)
                            |_    -> NaN
                        match y with
                        |NaN -> ()
                        |_ -> expr.substL x y c
                        eval (f x) pl
                    |Sum(t, n1, n2, f) ->
                        // 合計値格納用変数
                        eval (Let(t, Int 0, fun u ->
                            expr.forLoopF c (n1,n2) <| fun i ->
                                // 加算・代入処理
                                expr.substL u (Add(t,u, f i)) c
                            u)) pl
                    |IfEl(cond,n1,n2) -> 
                        eval (Let(n1.etype, NaN, fun x -> 
                            expr.branchL c <| fun (ifcode,_,elsecode) ->
                                ifcode cond <| fun () ->
                                    expr.substL x n1 c
                                elsecode <| fun () ->
                                    expr.substL x n2 c
                            x)) pl
                    |NaN -> "NaN", pl
                let t,_ = eval this 0
                t