// 
// Copyright (c) 2026 Jun-ichiro Sugisaka
// 
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
// 
namespace Aqualis
    
    [<AutoOpen>]
    module exprEvalJ =
        
        open System
        
        type expr with
            
            static member substJ (x:expr) (y:expr) (c:program) =
                c.codewritein (x.evalJ c  + " = " + y.evalJ c + ";")
                
            static member equivJ (x:expr) (y:expr) (c:program) =
                printfn "JavaScriptでこの文は使用できません"
                
            static member equivAlignJ (x:expr) (y:expr) (c:program) =
                printfn "JavaScriptでこの文は使用できません"
                
            static member forLoopJ (c:program) (n1:expr,n2:expr) code =
                let iname,returnVar = c.i0.getVar()
                let i = Var(It 4, iname, NaN)
                let n1_ = n1.evalJ c
                let n2_ = n2.evalJ c
                if isParMode then programList[prIndex].varPrivate.setVar(It 4,A0,iname,"")
                c.codewritein("for(" + i.evalJ c + " = " + n1_ + "; " + i.evalJ c + " <= " + n2_ + "; " + i.evalJ c + "++)")
                c.codewritein "{"
                c.indentInc()
                code i
                c.indentDec()
                c.codewritein "}"
                returnVar()
                
            ///<summary>無限ループ</summary>
            static member loopJ (c:program) code =
                let iname,returnVar = c.i0.getVar()
                let i = Var(It 4, iname, NaN)
                let label = "_" + gotoLabel.nextGotoLabel()
                let exit() = c.codewritein("goto "+label+";")
                expr.substJ i (Int 1) c
                if isParMode then programList[prIndex].varPrivate.setVar(It 4,A0,iname,"")
                c.codewritein "for(;;)"
                c.codewritein "{"
                c.indentInc()
                code(exit,i)
                expr.substJ i (Add(It 4, i, Int 1)) c
                c.indentDec()
                c.codewritein "}"
                c.codewritein(label+":;")
                returnVar()
                
            ///<summary>条件を満たす間ループ</summary>
            static member whiledoJ (c:program) (cond:expr) = fun code ->
                c.codewritein("while(" + cond.evalJ c + ")")
                c.codewritein "{"
                c.indentInc()
                code()
                c.indentDec()
                c.codewritein "}"
                
            ///<summary>指定した範囲でループ</summary>
            static member rangeJ (c:program) (i1:expr) = fun (i2:expr) -> fun code -> 
                match i1.simp,i2.simp with
                |Int a, Int b when a>b -> 
                    let iname,returnVar = c.i0.getVar()
                    let i = Var(It 4, iname, NaN)
                    if isParMode then programList[prIndex].varPrivate.setVar(It 4,A0,iname,"")
                    c.comment("for(" + i.evalJ c + "=" + i1.evalJ c + "; " + i.evalJ c + "<=" + i2.evalJ c + "; " + i.evalJ c + "++)")
                    c.comment "{"
                    c.indentInc()
                    code i
                    c.indentDec()
                    c.comment "}"
                    returnVar()
                |i1,i2 ->
                    let iname,returnVar = c.i0.getVar()
                    let i = Var(It 4, iname, NaN)
                    if isParMode then programList[prIndex].varPrivate.setVar(It 4,A0,iname,"")
                    c.codewritein("for(" + i.evalJ c + "=" + i1.evalJ c + "; " + i.evalJ c + "<=" + i2.evalJ c + "; " + i.evalJ c + "++)")
                    c.codewritein "{"
                    c.indentInc()
                    code i
                    c.indentDec()
                    c.codewritein "}"
                    returnVar()
                    
            ///<summary>指定した範囲でループ(途中脱出可)</summary>
            static member range_exitJ (c:program) (i1:expr) = fun (i2:expr) -> fun code -> 
                match i1.simp,i2.simp with
                |Int a, Int b when a>b -> 
                    let iname,returnVar = c.i0.getVar()
                    let i = Var(It 4, iname, NaN)
                    let label = gotoLabel.nextGotoLabel()
                    let exit() = c.codewritein("goto "+label+"")
                    if isParMode then programList[prIndex].varPrivate.setVar(It 4,A0,iname,"")
                    c.comment("for(" + i.evalJ c + "=" + i1.evalJ c + "; " + i.evalJ c + "<=" + i2.evalJ c + "; " + i.evalJ c + "++)")
                    c.comment "{"
                    c.indentInc()
                    code(exit,i)
                    c.indentDec()
                    c.comment "}"
                    c.comment(label+":;")
                    returnVar()
                |i1,i2 ->
                    let iname,returnVar = c.i0.getVar()
                    let i = Var(It 4, iname, NaN)
                    let label = gotoLabel.nextGotoLabel()
                    let exit() = c.codewritein("goto "+label+"")
                    if isParMode then programList[prIndex].varPrivate.setVar(It 4,A0,iname,"")
                    c.codewritein("for(" + i.evalJ c + "=" + i1.evalJ c + "; " + i.evalJ c + "<=" + i2.evalJ c + "; " + i.evalJ c + "++)")
                    c.codewritein "{"
                    c.indentInc()
                    code(exit,i)
                    c.indentDec()
                    c.codewritein "}"
                    c.codewritein(label+":;")
                    returnVar()
                    
            static member branchJ (c:program) code =
                let ifcode (cond:expr) code =
                    let cond = cond.evalJ c
                    c.codewritein("if(" + cond + ")")
                    c.codewritein "{"
                    c.indentInc()
                    code()
                    c.indentDec()
                    c.codewritein "}"
                let elseifcode (cond:expr) code =
                    let cond = cond.evalJ c
                    c.codewritein("else if(" + cond + ")")
                    c.codewritein "{"
                    c.indentInc()
                    code()
                    c.indentDec()
                    c.codewritein "}"
                let elsecode code =
                    c.codewritein "else"
                    c.codewritein "{"
                    c.indentInc()
                    code()
                    c.indentDec()
                    c.codewritein "}"
                code(ifcode,elseifcode,elsecode)
                
            member this.evalJ(c:program) =
                match this.simp with
                |False -> "false"
                |True -> "true"
                |Eq(x,y) -> x.evalJ c + " == " + y.evalJ c
                |NEq(x,y) -> x.evalJ c + " =/ " + y.evalJ c
                |Greater(x,y) -> x.evalJ c + " > " + y.evalJ c
                |GreaterEq(x,y) -> x.evalJ c + " >= " + y.evalJ c
                |Less(x,y) -> x.evalJ c + " < " + y.evalJ c
                |LessEq(x,y) -> x.evalJ c + " <= " + y.evalJ c
                |AND x -> 
                    x 
                    |> List.map (fun v -> match v with |OR _ |AND _ -> "(" + v.evalJ c + ")" |_ -> v.evalJ c)
                    |> fun lst -> String.Join(" && ", lst)
                |OR x -> 
                    x 
                    |> List.map (fun v -> match v with |OR _ |AND _ -> "(" + v.evalJ c + ")" |_ -> v.evalJ c)
                    |> fun lst -> String.Join(" || ", lst)
                |Int x -> c.numFormat.ItoS x
                |Dbl x -> c.numFormat.DtoS x
                |Cpx (0.0,1.0) -> "uj"
                |Cpx (re,im) -> (Add(Zt, Dbl re, Mul(Zt, Cpx(0.0,1.0), Dbl im))).evalJ c
                |Var (_,s,x) -> s
                |Inv(_,x) -> 
                    match x with
                    |Add _|Sub _ -> "-(" + x.evalJ c + ")"
                    |_ -> "-" + x.evalJ c
                |Add(_,x,y) -> x.evalJ c + "+" + y.evalJ c
                |Sub(_,x,y) -> 
                    match x,y with
                    |x,(Add _|Sub _) -> x.evalJ c + "-(" + y.evalJ c + ")"
                    |_ -> x.evalJ c + "-" + y.evalJ c
                |Mul(_,x,y) ->
                    match x,y with
                    |(Add _|Sub _),(Add _|Sub _) -> "(" + x.evalJ c + ")*(" + y.evalJ c + ")"
                    |(Add _|Sub _),_ -> "(" + x.evalJ c + ")*" + y.evalJ c
                    |_,(Add _|Sub _) -> x.evalJ c + "*(" + y.evalJ c + ")"
                    |_ -> x.evalJ c + "*" + y.evalJ c
                |Div(Dt,x,y) when x.etype = It 4 && y.etype = It 4 -> 
                    (ToDbl x/ToDbl y).evalJ c
                |Div(_,x,y) ->
                    match x,y with
                    |(Add _|Sub _),(Add _|Sub _|Mul _|Div _) -> "(" + x.evalJ c + ")/(" + y.evalJ c + ")"
                    |(Add _|Sub _),_ -> "(" + x.evalJ c + ")/" + y.evalJ c
                    |_,(Add _|Sub _|Mul _|Div _) -> x.evalJ c + "/(" + y.evalJ c + ")"
                    |_ -> x.evalJ c + "/" + y.evalJ c
                |Mod(_,x,y) -> x.evalJ c + "%" + y.evalJ c
                |Pow(_,x,y) -> "Math.pow(" + x.evalJ c + "," + y.evalJ c + ")"
                |Exp(_,x) -> "Math.exp(" + x.evalJ c + ")"
                |Sin(_,x) -> "Math.sin(" + x.evalJ c + ")"
                |Cos(_,x) -> "Math.cos(" + x.evalJ c + ")"
                |Tan(_,x) -> "Math.tan(" + x.evalJ c + ")"
                |Asin(_,x) -> "Math.asin(" + x.evalJ c + ")"
                |Acos(_,x) -> "Math.acos(" + x.evalJ c + ")"
                |Atan(_,x) -> "Math.atan(" + x.evalJ c + ")"
                |Atan2(x,y) -> "Math.atan2(" + x.evalJ c + "," + y.evalJ c + ")"
                |Abs(_,x) -> "Math.abs(" + x.evalJ c + ")"
                |Log(_,x) -> "Math.log(" + x.evalJ c + ")"
                |Log10(_,x) -> "Math.log10(" + x.evalJ c + ")"
                |Sqrt(_,x) -> "Math.sqrt(" + x.evalJ c + ")"
                |ToInt x -> 
                    match x with
                    |Add _|Sub _ |Mul _ |Div _ ->
                        "(int)(" + x.evalJ c + ")"
                    |_ ->
                        "(int)" + x.evalJ c
                |ToDbl x -> 
                    match x with
                    |Add _|Sub _ |Mul _ |Div _ ->
                        "(double)(" + x.evalJ c + ")"
                    |_ ->
                        "(double)" + x.evalJ c
                |Floor x -> "Math.floor(" + x.evalJ c + ")"
                |Ceil x -> "Math.ceil(" + x.evalJ c + ")"
                |Re x -> "Math.creal(" + x.evalJ c + ")"
                |Im x -> "Math.cimag(" + x.evalJ c + ")"
                |Conj x -> "Math.conj(" + x.evalJ c + ")"
                |Idx1 (_,name,i) -> name + "[" + i.evalJ c + "]"
                |Idx2 (_,name,i,j) ->
                    printfn "JavaScriptでは2次元配列の代わりに1次元配列を使用します"
                    "NaN"
                |Idx3 (_,name,i,j,k) -> 
                    printfn "JavaScriptでは3次元配列の代わりに1次元配列を使用します"
                    "NaN"
                |Let (t,y,f) -> 
                    let x =
                        match t with
                        |It 4 -> Var (t, (fun (a,_) -> a) (c.i0.getVar()), y)
                        |Dt   -> Var (t, (fun (a,_) -> a) (c.d0.getVar()), y)
                        |Zt   -> Var (t, (fun (a,_) -> a) (c.z0.getVar()), y)
                        |_    -> NaN
                    match y with
                    |NaN -> ()
                    |_ -> expr.substJ x y c
                    (f x).evalJ c
                |Sum(t, n1, n2, f) ->
                    // 合計値格納用変数
                    (Let(t, Int 0, fun u ->
                        expr.forLoopJ c (n1,n2) <| fun i ->
                            // 加算・代入処理
                            expr.substJ u (Add(t,u, f i)) c
                        u)).evalJ c
                |IfEl(cond,n1,n2) -> 
                    (Let(n1.etype, NaN, fun x -> 
                        expr.branchJ c <| fun (ifcode,_,elsecode) ->
                            ifcode cond <| fun () ->
                                expr.substJ x n1 c
                            elsecode <| fun () ->
                                expr.substJ x n2 c
                        x)).evalJ c
                |NaN -> "NaN"
