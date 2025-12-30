namespace Aqualis
    
    [<AutoOpen>]
    module exprEvalPh =
        
        open System
        
        type expr with
            
            static member substPh (x:expr) (y:expr) (c:program) =
                c.codewrite ("<?php " + x.evalPh c + " = " + "\"" + x.evalPh c + "\"" + "; ?>")
                
            static member equivPh (x:expr) (y:expr) (c:program) =
                printfn "PHPでこの文は使用できません"
                
            static member equivAlignPh (x:expr) (y:expr) (c:program) =
                printfn "PHPでこの文は使用できません"
                
            static member forLoopPh (c:program) (n1:expr,n2:expr) code =
                let iname,returnVar = c.i0.getVar()
                let i = Var(It 4, iname, NaN)
                let n1_ = n1.evalPh c
                let n2_ = n2.evalPh c
                if isParMode then pr.varPrivate.setVar(It 4,A0,iname,"")
                c.codewrite("for(" + i.evalPh c + " = " + n1_ + "; " + i.evalPh c + " <= " + n2_ + "; " + i.evalPh c + "++)")
                c.codewrite "{"
                c.indentInc()
                code i
                c.indentDec()
                c.codewrite "}"
                returnVar()
                
            ///<summary>無限ループ</summary>
            static member loopPh (c:program) code =
                let iname,returnVar = c.i0.getVar()
                let i = Var(It 4, iname, NaN)
                let label = "_" + gotoLabel.nextGotoLabel()
                let exit() = c.codewrite("goto "+label+";")
                expr.substPh i (Int 1) c
                if isParMode then pr.varPrivate.setVar(It 4,A0,iname,"")
                c.codewrite "for(;;):"
                c.indentInc()
                code(exit,i)
                expr.substPh i (Add(It 4, i, Int 1)) c
                c.indentDec()
                c.codewrite "endfor;"
                c.codewrite(label+":;")
                returnVar()
                
            ///<summary>条件を満たす間ループ</summary>
            static member whiledoPh (c:program) (cond:expr) = fun code ->
                c.codewrite("while(" + cond.evalPh c + "):")
                c.indentInc()
                code()
                c.indentDec()
                c.codewrite "endwhile;"
                
            ///<summary>指定した範囲でループ</summary>
            static member rangePh (c:program) (i1:expr) = fun (i2:expr) -> fun code -> 
                match i1.simp,i2.simp with
                |Int a, Int b when a>b -> 
                    let iname,returnVar = c.i0.getVar()
                    let i = Var(It 4, iname, NaN)
                    if isParMode then pr.varPrivate.setVar(It 4,A0,iname,"")
                    c.comment("for(" + i.evalPh c + "=" + i1.evalPh c + "; " + i.evalPh c + "<=" + i2.evalPh c + "; " + i.evalPh c + "++):")
                    c.indentInc()
                    code i
                    c.indentDec()
                    c.comment "endfor;"
                    returnVar()
                |i1,i2 ->
                    let iname,returnVar = c.i0.getVar()
                    let i = Var(It 4, iname, NaN)
                    if isParMode then pr.varPrivate.setVar(It 4,A0,iname,"")
                    c.codewrite("for(" + i.evalPh c + "=" + i1.evalPh c + "; " + i.evalPh c + "<=" + i2.evalPh c + "; " + i.evalPh c + "++):")
                    c.indentInc()
                    code i
                    c.indentDec()
                    c.codewrite "endfor;"
                    returnVar()
                    
            ///<summary>指定した範囲でループ(途中脱出可)</summary>
            static member range_exitPh (c:program) (i1:expr) = fun (i2:expr) -> fun code -> 
                match i1.simp,i2.simp with
                |Int a, Int b when a>b -> 
                    let iname,returnVar = c.i0.getVar()
                    let i = Var(It 4, iname, NaN)
                    let label = gotoLabel.nextGotoLabel()
                    let exit() = c.codewrite("goto "+label+"")
                    if isParMode then pr.varPrivate.setVar(It 4,A0,iname,"")
                    c.comment("for(" + i.evalPh c + "=" + i1.evalPh c + "; " + i.evalPh c + "<=" + i2.evalPh c + "; " + i.evalPh c + "++):")
                    c.indentInc()
                    code(exit,i)
                    c.indentDec()
                    c.comment "endfor;"
                    c.comment(label+":")
                    returnVar()
                |i1,i2 ->
                    let iname,returnVar = c.i0.getVar()
                    let i = Var(It 4, iname, NaN)
                    let label = gotoLabel.nextGotoLabel()
                    let exit() = c.codewrite("goto "+label+"")
                    if isParMode then pr.varPrivate.setVar(It 4,A0,iname,"")
                    c.codewrite("for(" + i.evalPh c + "=" + i1.evalPh c + "; " + i.evalPh c + "<=" + i2.evalPh c + "; " + i.evalPh c + "++):")
                    c.indentInc()
                    code(exit,i)
                    c.indentDec()
                    c.codewrite "endfor;"
                    c.codewrite(label+":")
                    returnVar()
                    
            static member branchPh (c:program) code =
                let ifcode (cond:expr) code =
                    let cond = cond.evalPh c
                    c.codewrite("if(" + cond + "):")
                    c.indentInc()
                    code()
                    c.indentDec()
                let elseifcode (cond:expr) code =
                    let cond = cond.evalPh c
                    c.codewrite("else if(" + cond + "):")
                    c.indentInc()
                    code()
                    c.indentDec()
                let elsecode code =
                    c.codewrite "else:"
                    c.indentInc()
                    code()
                    c.indentDec()
                code(ifcode,elseifcode,elsecode)
                c.codewrite "endif;"
                
            member this.evalPh(c:program) =
                match this.simp with
                |False -> "false"
                |True -> "true"
                |Eq(x,y) -> x.evalPh c + " == " + y.evalPh c
                |NEq(x,y) -> x.evalPh c + " =/ " + y.evalPh c
                |Greater(x,y) -> x.evalPh c + " > " + y.evalPh c
                |GreaterEq(x,y) -> x.evalPh c + " >= " + y.evalPh c
                |Less(x,y) -> x.evalPh c + " < " + y.evalPh c
                |LessEq(x,y) -> x.evalPh c + " <= " + y.evalPh c
                |AND x -> 
                    x 
                    |> List.map (fun v -> match v with |OR _ |AND _ -> "(" + v.evalPh c + ")" |_ -> v.evalPh c)
                    |> fun lst -> String.Join(" && ", lst)
                |OR x -> 
                    x 
                    |> List.map (fun v -> match v with |OR _ |AND _ -> "(" + v.evalPh c + ")" |_ -> v.evalPh c)
                    |> fun lst -> String.Join(" || ", lst)
                |Int x -> c.numFormat.ItoS x
                |Dbl x -> c.numFormat.DtoS x
                |Cpx (0.0,1.0) -> "uj"
                |Cpx (re,im) -> (Add(Zt, Dbl re, Mul(Zt, Cpx(0.0,1.0), Dbl im))).evalPh c
                |Var (_,s,x) -> s
                |Inv(_,x) -> 
                    match x with
                    |Add _|Sub _ -> "-(" + x.evalPh c + ")"
                    |_ -> "-" + x.evalPh c
                |Add(_,x,y) -> x.evalPh c + "+" + y.evalPh c
                |Sub(_,x,y) -> 
                    match x,y with
                    |x,(Add _|Sub _) -> x.evalPh c + "-(" + y.evalPh c + ")"
                    |_ -> x.evalPh c + "-" + y.evalPh c
                |Mul(_,x,y) ->
                    match x,y with
                    |(Add _|Sub _),(Add _|Sub _) -> "(" + x.evalPh c + ")*(" + y.evalPh c + ")"
                    |(Add _|Sub _),_ -> "(" + x.evalPh c + ")*" + y.evalPh c
                    |_,(Add _|Sub _) -> x.evalPh c + "*(" + y.evalPh c + ")"
                    |_ -> x.evalPh c + "*" + y.evalPh c
                |Div(It 4,x,y) when x.etype = It 4 && y.etype = It 4 -> 
                    "intdiv(" + x.evalPh c + ", " + y.evalPh c + ")"
                |Div(_,x,y) ->
                    match x,y with
                    |(Add _|Sub _),(Add _|Sub _|Mul _|Div _) -> "(" + x.evalPh c + ")/(" + y.evalPh c + ")"
                    |(Add _|Sub _),_ -> "(" + x.evalPh c + ")/" + y.evalPh c
                    |_,(Add _|Sub _|Mul _|Div _) -> x.evalPh c + "/(" + y.evalPh c + ")"
                    |_ -> x.evalPh c + "/" + y.evalPh c
                |Mod(_,x,y) -> x.evalPh c + "%" + y.evalPh c
                |Pow(Zt,x,y) -> "cpow(" + x.evalPh c + "," + y.evalPh c + ")"
                |Pow(_,x,y) -> "pow(" + x.evalPh c + "," + y.evalPh c + ")"
                |Exp(Zt,x) -> "cexp(" + x.evalPh c + ")"
                |Exp(_,x) -> "exp(" + x.evalPh c + ")"
                |Sin(Zt,x) -> "csin(" + x.evalPh c + ")"
                |Sin(_,x) -> "sin(" + x.evalPh c + ")"
                |Cos(Zt,x) -> "ccos(" + x.evalPh c + ")"
                |Cos(_,x) -> "cos(" + x.evalPh c + ")"
                |Tan(Zt,x) -> "ctan(" + x.evalPh c + ")"
                |Tan(_,x) -> "tan(" + x.evalPh c + ")"
                |Asin(Zt,x) -> "casin(" + x.evalPh c + ")"
                |Asin(_,x) -> "asin(" + x.evalPh c + ")"
                |Acos(Zt,x) -> "cacos(" + x.evalPh c + ")"
                |Acos(_,x) -> "acos(" + x.evalPh c + ")"
                |Atan(Zt,x) -> "catan(" + x.evalPh c + ")"
                |Atan(_,x) -> "atan(" + x.evalPh c + ")"
                |Atan2(x,y) -> "atan2(" + x.evalPh c + "," + y.evalPh c + ")"
                |Abs(Dt,x) when x.etype=Zt -> "cabs(" + x.evalPh c + ")"
                |Abs(Dt,x) -> "fabs(" + x.evalPh c + ")"
                |Abs(_,x) -> "abs(" + x.evalPh c + ")"
                |Log(Zt,x) -> "clog(" + x.evalPh c + ")"
                |Log(_,x) -> "log(" + x.evalPh c + ")"
                |Log10(_,x) -> "log10(" + x.evalPh c + ")"
                |Sqrt(Zt,x) -> "csqrt(" + x.evalPh c + ")"
                |Sqrt(_,x) -> "sqrt(" + x.evalPh c + ")"
                |ToInt x -> 
                    match x with
                    |Add _|Sub _ |Mul _ |Div _ ->
                        "(int)(" + x.evalPh c + ")"
                    |_ ->
                        "(int)" + x.evalPh c
                |ToDbl x -> 
                    match x with
                    |Add _|Sub _ |Mul _ |Div _ ->
                        "(double)(" + x.evalPh c + ")"
                    |_ ->
                        "(double)" + x.evalPh c
                |Floor x -> "floor(" + x.evalPh c + ")"
                |Ceil x -> "ceil(" + x.evalPh c + ")"
                |Re x -> "creal(" + x.evalPh c + ")"
                |Im x -> "cimag(" + x.evalPh c + ")"
                |Conj x -> "conj(" + x.evalPh c + ")"
                |Idx1 (_,name,i) -> name + "[" + i.evalPh c + "]"
                |Idx2 (_,name,i,j) ->
                    printfn "Ph言語では2次元配列の代わりに1次元配列を使用します"
                    "NaN"
                |Idx3 (_,name,i,j,k) -> 
                    printfn "Ph言語では3次元配列の代わりに1次元配列を使用します"
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
                    |_ -> expr.substPh x y c
                    (f x).evalPh c
                |Sum(t, n1, n2, f) ->
                    // 合計値格納用変数
                    (Let(t, Int 0, fun u ->
                        expr.forLoopPh c (n1,n2) <| fun i ->
                            // 加算・代入処理
                            expr.substPh u (Add(t,u, f i)) c
                        u)).evalPh c
                |IfEl(cond,n1,n2) -> 
                    (Let(n1.etype, NaN, fun x -> 
                        expr.branchPh c <| fun (ifcode,_,elsecode) ->
                            ifcode cond <| fun () ->
                                expr.substPh x n1 c
                            elsecode <| fun () ->
                                expr.substPh x n2 c
                        x)).evalPh c
                |NaN -> "NaN"
