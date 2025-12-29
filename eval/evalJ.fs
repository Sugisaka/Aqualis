namespace Aqualis
    
    [<AutoOpen>]
    module exprEvalJ =
        
        open System
        
        type expr with
            
            static member substJ (x:expr) (y:expr) (c:program) =
                c.cwriter.codewrite (x.evalJ c  + " = " + y.evalJ c + ";")
                
            static member equivJ (x:expr) (y:expr) (c:program) =
                printfn "JavaScriptでこの文は使用できません"
                
            static member equivAlignJ (x:expr) (y:expr) (c:program) =
                printfn "JavaScriptでこの文は使用できません"
                
            static member forLoopJ (c:program) (n1:expr,n2:expr) code =
                let iname,returnVar = c.i0.getVar()
                let i = Var(It 4, iname, NaN)
                let n1_ = n1.evalJ c
                let n2_ = n2.evalJ c
                if isParMode then pr.varPrivate.setVar(It 4,A0,iname,"")
                c.cwriter.codewrite("for(" + i.evalJ c + " = " + n1_ + "; " + i.evalJ c + " <= " + n2_ + "; " + i.evalJ c + "++)")
                c.cwriter.codewrite "{"
                c.cwriter.indent.inc()
                code i
                c.cwriter.indent.dec()
                c.cwriter.codewrite "}"
                returnVar()
                
            ///<summary>無限ループ</summary>
            static member loopJ (c:program) code =
                let iname,returnVar = c.i0.getVar()
                let i = Var(It 4, iname, NaN)
                let label = "_" + gotoLabel.nextGotoLabel()
                let exit() = c.cwriter.codewrite("goto "+label+";")
                expr.substJ i (Int 1) c
                if isParMode then pr.varPrivate.setVar(It 4,A0,iname,"")
                c.cwriter.codewrite "for(;;)"
                c.cwriter.codewrite "{"
                c.cwriter.indent.inc()
                code(exit,i)
                expr.substJ i (Add(It 4, i, Int 1)) c
                c.cwriter.indent.dec()
                c.cwriter.codewrite "}"
                c.cwriter.codewrite(label+":;")
                returnVar()
                
            ///<summary>条件を満たす間ループ</summary>
            static member whiledoJ (c:program) (cond:expr) = fun code ->
                c.cwriter.codewrite("while(" + cond.evalJ c + ")")
                c.cwriter.codewrite "{"
                c.cwriter.indent.inc()
                code()
                c.cwriter.indent.dec()
                c.cwriter.codewrite "}"
                
            ///<summary>指定した範囲でループ</summary>
            static member rangeJ (c:program) (i1:expr) = fun (i2:expr) -> fun code -> 
                match i1.simp,i2.simp with
                |Int a, Int b when a>b -> 
                    let iname,returnVar = c.i0.getVar()
                    let i = Var(It 4, iname, NaN)
                    if isParMode then pr.varPrivate.setVar(It 4,A0,iname,"")
                    c.cwriter.comment("for(" + i.evalJ c + "=" + i1.evalJ c + "; " + i.evalJ c + "<=" + i2.evalJ c + "; " + i.evalJ c + "++)")
                    c.cwriter.comment "{"
                    c.cwriter.indent.inc()
                    code i
                    c.cwriter.indent.dec()
                    c.cwriter.comment "}"
                    returnVar()
                |i1,i2 ->
                    let iname,returnVar = c.i0.getVar()
                    let i = Var(It 4, iname, NaN)
                    if isParMode then pr.varPrivate.setVar(It 4,A0,iname,"")
                    c.cwriter.codewrite("for(" + i.evalJ c + "=" + i1.evalJ c + "; " + i.evalJ c + "<=" + i2.evalJ c + "; " + i.evalJ c + "++)")
                    c.cwriter.codewrite "{"
                    c.cwriter.indent.inc()
                    code i
                    c.cwriter.indent.dec()
                    c.cwriter.codewrite "}"
                    returnVar()
                    
            ///<summary>指定した範囲でループ(途中脱出可)</summary>
            static member range_exitJ (c:program) (i1:expr) = fun (i2:expr) -> fun code -> 
                match i1.simp,i2.simp with
                |Int a, Int b when a>b -> 
                    let iname,returnVar = c.i0.getVar()
                    let i = Var(It 4, iname, NaN)
                    let label = gotoLabel.nextGotoLabel()
                    let exit() = c.cwriter.codewrite("goto "+label+"")
                    if isParMode then pr.varPrivate.setVar(It 4,A0,iname,"")
                    c.cwriter.comment("for(" + i.evalJ c + "=" + i1.evalJ c + "; " + i.evalJ c + "<=" + i2.evalJ c + "; " + i.evalJ c + "++)")
                    c.cwriter.comment "{"
                    c.cwriter.indent.inc()
                    code(exit,i)
                    c.cwriter.indent.dec()
                    c.cwriter.comment "}"
                    c.cwriter.comment(label+":;")
                    returnVar()
                |i1,i2 ->
                    let iname,returnVar = c.i0.getVar()
                    let i = Var(It 4, iname, NaN)
                    let label = gotoLabel.nextGotoLabel()
                    let exit() = c.cwriter.codewrite("goto "+label+"")
                    if isParMode then pr.varPrivate.setVar(It 4,A0,iname,"")
                    c.cwriter.codewrite("for(" + i.evalJ c + "=" + i1.evalJ c + "; " + i.evalJ c + "<=" + i2.evalJ c + "; " + i.evalJ c + "++)")
                    c.cwriter.codewrite "{"
                    c.cwriter.indent.inc()
                    code(exit,i)
                    c.cwriter.indent.dec()
                    c.cwriter.codewrite "}"
                    c.cwriter.codewrite(label+":;")
                    returnVar()
                    
            static member branchJ (c:program) code =
                let ifcode (cond:expr) code =
                    let cond = cond.evalJ c
                    c.cwriter.codewrite("if(" + cond + ")")
                    c.cwriter.codewrite "{"
                    c.cwriter.indent.inc()
                    code()
                    c.cwriter.indent.dec()
                    c.cwriter.codewrite "}"
                let elseifcode (cond:expr) code =
                    let cond = cond.evalJ c
                    c.cwriter.codewrite("else if(" + cond + ")")
                    c.cwriter.codewrite "{"
                    c.cwriter.indent.inc()
                    code()
                    c.cwriter.indent.dec()
                    c.cwriter.codewrite "}"
                let elsecode code =
                    c.cwriter.codewrite "else"
                    c.cwriter.codewrite "{"
                    c.cwriter.indent.inc()
                    code()
                    c.cwriter.indent.dec()
                    c.cwriter.codewrite "}"
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
                |Pow(Zt,x,y) -> "cpow(" + x.evalJ c + "," + y.evalJ c + ")"
                |Pow(_,x,y) -> "pow(" + x.evalJ c + "," + y.evalJ c + ")"
                |Exp(Zt,x) -> "cexp(" + x.evalJ c + ")"
                |Exp(_,x) -> "exp(" + x.evalJ c + ")"
                |Sin(Zt,x) -> "csin(" + x.evalJ c + ")"
                |Sin(_,x) -> "sin(" + x.evalJ c + ")"
                |Cos(Zt,x) -> "ccos(" + x.evalJ c + ")"
                |Cos(_,x) -> "cos(" + x.evalJ c + ")"
                |Tan(Zt,x) -> "ctan(" + x.evalJ c + ")"
                |Tan(_,x) -> "tan(" + x.evalJ c + ")"
                |Asin(Zt,x) -> "casin(" + x.evalJ c + ")"
                |Asin(_,x) -> "asin(" + x.evalJ c + ")"
                |Acos(Zt,x) -> "cacos(" + x.evalJ c + ")"
                |Acos(_,x) -> "acos(" + x.evalJ c + ")"
                |Atan(Zt,x) -> "catan(" + x.evalJ c + ")"
                |Atan(_,x) -> "atan(" + x.evalJ c + ")"
                |Atan2(x,y) -> "atan2(" + x.evalJ c + "," + y.evalJ c + ")"
                |Abs(Dt,x) when x.etype=Zt -> "cabs(" + x.evalJ c + ")"
                |Abs(Dt,x) -> "fabs(" + x.evalJ c + ")"
                |Abs(_,x) -> "abs(" + x.evalJ c + ")"
                |Log(Zt,x) -> "clog(" + x.evalJ c + ")"
                |Log(_,x) -> "log(" + x.evalJ c + ")"
                |Log10(_,x) -> "log10(" + x.evalJ c + ")"
                |Sqrt(Zt,x) -> "csqrt(" + x.evalJ c + ")"
                |Sqrt(_,x) -> "sqrt(" + x.evalJ c + ")"
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
                |Floor x -> "floor(" + x.evalJ c + ")"
                |Ceil x -> "ceil(" + x.evalJ c + ")"
                |Re x -> "creal(" + x.evalJ c + ")"
                |Im x -> "cimag(" + x.evalJ c + ")"
                |Conj x -> "conj(" + x.evalJ c + ")"
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
