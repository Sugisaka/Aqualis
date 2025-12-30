namespace Aqualis
    
    [<AutoOpen>]
    module exprEvalC =
        
        open System
        
        type expr with
            
            static member substC (x:expr) (y:expr) (c:program) =
                c.codewrite (x.evalC c  + " = " + y.evalC c + ";")
                
            static member equivC (x:expr) (y:expr) (c:program) =
                printfn "C99でこの文は使用できません"
                
            static member equivAlignC (x:expr) (y:expr) (c:program) =
                printfn "C99でこの文は使用できません"
                
            static member forLoopC (c:program) (n1:expr,n2:expr) code =
                let iname,returnVar = c.i0.getVar()
                let i = Var(It 4, iname, NaN)
                let n1_ = n1.evalC c
                let n2_ = n2.evalC c
                if isParMode then pr.varPrivate.setVar(It 4,A0,iname,"")
                c.codewrite("for(" + i.evalC c + " = " + n1_ + "; " + i.evalC c + " <= " + n2_ + "; " + i.evalC c + "++)")
                c.codewrite "{"
                c.indentInc()
                code i
                c.indentDec()
                c.codewrite "}"
                returnVar()
                
            ///<summary>無限ループ</summary>
            static member loopC (c:program) code =
                let iname,returnVar = c.i0.getVar()
                let i = Var(It 4, iname, NaN)
                let label = "_" + gotoLabel.nextGotoLabel()
                let exit() = c.codewrite("goto "+label+";")
                expr.substC i (Int 1) c
                if isParMode then pr.varPrivate.setVar(It 4,A0,iname,"")
                c.codewrite "for(;;)"
                c.codewrite "{"
                c.indentInc()
                code(exit,i)
                expr.substC i (Add(It 4, i, Int 1)) c
                c.indentDec()
                c.codewrite "}"
                c.codewrite(label+":;")
                returnVar()
                
            ///<summary>条件を満たす間ループ</summary>
            static member whiledoC (c:program) (cond:expr) = fun code ->
                c.codewrite("while(" + cond.evalC c + ")")
                c.codewrite "{"
                c.indentInc()
                code()
                c.indentDec()
                c.codewrite "}"
                
            ///<summary>指定した範囲でループ</summary>
            static member rangeC (c:program) (i1:expr) = fun (i2:expr) -> fun code -> 
                match i1.simp,i2.simp with
                |Int a, Int b when a>b -> 
                    let iname,returnVar = c.i0.getVar()
                    let i = Var(It 4, iname, NaN)
                    if isParMode then pr.varPrivate.setVar(It 4,A0,iname,"")
                    c.comment("for(" + i.evalC c + "=" + i1.evalC c + "; " + i.evalC c + "<=" + i2.evalC c + "; " + i.evalC c + "++)")
                    c.comment "{"
                    c.indentInc()
                    code i
                    c.indentDec()
                    c.comment "}"
                    returnVar()
                |i1,i2 ->
                    let iname,returnVar = c.i0.getVar()
                    let i = Var(It 4, iname, NaN)
                    if isParMode then pr.varPrivate.setVar(It 4,A0,iname,"")
                    c.codewrite("for(" + i.evalC c + "=" + i1.evalC c + "; " + i.evalC c + "<=" + i2.evalC c + "; " + i.evalC c + "++)")
                    c.codewrite "{"
                    c.indentInc()
                    code i
                    c.indentDec()
                    c.codewrite "}"
                    returnVar()
                    
            ///<summary>指定した範囲でループ(途中脱出可)</summary>
            static member range_exitC (c:program) (i1:expr) = fun (i2:expr) -> fun code -> 
                match i1.simp,i2.simp with
                |Int a, Int b when a>b -> 
                    let iname,returnVar = c.i0.getVar()
                    let i = Var(It 4, iname, NaN)
                    let label = gotoLabel.nextGotoLabel()
                    let exit() = c.codewrite("goto "+label+"")
                    if isParMode then pr.varPrivate.setVar(It 4,A0,iname,"")
                    c.comment("for(" + i.evalC c + "=" + i1.evalC c + "; " + i.evalC c + "<=" + i2.evalC c + "; " + i.evalC c + "++)")
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
                    let exit() = c.codewrite("goto "+label+"")
                    if isParMode then pr.varPrivate.setVar(It 4,A0,iname,"")
                    c.codewrite("for(" + i.evalC c + "=" + i1.evalC c + "; " + i.evalC c + "<=" + i2.evalC c + "; " + i.evalC c + "++)")
                    c.codewrite "{"
                    c.indentInc()
                    code(exit,i)
                    c.indentDec()
                    c.codewrite "}"
                    c.codewrite(label+":;")
                    returnVar()
                    
            static member branchC (c:program) code =
                let ifcode (cond:expr) code =
                    let cond = cond.evalC c
                    c.codewrite("if(" + cond + ")")
                    c.codewrite "{"
                    c.indentInc()
                    code()
                    c.indentDec()
                    c.codewrite "}"
                let elseifcode (cond:expr) code =
                    let cond = cond.evalC c
                    c.codewrite("else if(" + cond + ")")
                    c.codewrite "{"
                    c.indentInc()
                    code()
                    c.indentDec()
                    c.codewrite "}"
                let elsecode code =
                    c.codewrite "else"
                    c.codewrite "{"
                    c.indentInc()
                    code()
                    c.indentDec()
                    c.codewrite "}"
                code(ifcode,elseifcode,elsecode)
                
            member this.evalC(c:program) =
                match this.simp with
                |False -> "false"
                |True -> "true"
                |Eq(x,y) -> x.evalC c + " == " + y.evalC c
                |NEq(x,y) -> x.evalC c + " =/ " + y.evalC c
                |Greater(x,y) -> x.evalC c + " > " + y.evalC c
                |GreaterEq(x,y) -> x.evalC c + " >= " + y.evalC c
                |Less(x,y) -> x.evalC c + " < " + y.evalC c
                |LessEq(x,y) -> x.evalC c + " <= " + y.evalC c
                |AND x -> 
                    x 
                    |> List.map (fun v -> match v with |OR _ |AND _ -> "(" + v.evalC c + ")" |_ -> v.evalC c)
                    |> fun lst -> String.Join(" && ", lst)
                |OR x -> 
                    x 
                    |> List.map (fun v -> match v with |OR _ |AND _ -> "(" + v.evalC c + ")" |_ -> v.evalC c)
                    |> fun lst -> String.Join(" || ", lst)
                |Int x -> c.numFormat.ItoS x
                |Dbl x -> c.numFormat.DtoS x
                |Cpx (0.0,1.0) -> "uj"
                |Cpx (re,im) -> (Add(Zt, Dbl re, Mul(Zt, Cpx(0.0,1.0), Dbl im))).evalC c
                |Var (_,s,x) -> s
                |Inv(_,x) -> 
                    match x with
                    |Add _|Sub _ -> "-(" + x.evalC c + ")"
                    |_ -> "-" + x.evalC c
                |Add(_,x,y) -> x.evalC c + "+" + y.evalC c
                |Sub(_,x,y) -> 
                    match x,y with
                    |x,(Add _|Sub _) -> x.evalC c + "-(" + y.evalC c + ")"
                    |_ -> x.evalC c + "-" + y.evalC c
                |Mul(_,x,y) ->
                    match x,y with
                    |(Add _|Sub _),(Add _|Sub _) -> "(" + x.evalC c + ")*(" + y.evalC c + ")"
                    |(Add _|Sub _),_ -> "(" + x.evalC c + ")*" + y.evalC c
                    |_,(Add _|Sub _) -> x.evalC c + "*(" + y.evalC c + ")"
                    |_ -> x.evalC c + "*" + y.evalC c
                |Div(Dt,x,y) when x.etype = It 4 && y.etype = It 4 -> 
                    (ToDbl x/ToDbl y).evalC c
                |Div(_,x,y) ->
                    match x,y with
                    |(Add _|Sub _),(Add _|Sub _|Mul _|Div _) -> "(" + x.evalC c + ")/(" + y.evalC c + ")"
                    |(Add _|Sub _),_ -> "(" + x.evalC c + ")/" + y.evalC c
                    |_,(Add _|Sub _|Mul _|Div _) -> x.evalC c + "/(" + y.evalC c + ")"
                    |_ -> x.evalC c + "/" + y.evalC c
                |Mod(_,x,y) -> x.evalC c + "%" + y.evalC c
                |Pow(Zt,x,y) -> "cpow(" + x.evalC c + "," + y.evalC c + ")"
                |Pow(_,x,y) -> "pow(" + x.evalC c + "," + y.evalC c + ")"
                |Exp(Zt,x) -> "cexp(" + x.evalC c + ")"
                |Exp(_,x) -> "exp(" + x.evalC c + ")"
                |Sin(Zt,x) -> "csin(" + x.evalC c + ")"
                |Sin(_,x) -> "sin(" + x.evalC c + ")"
                |Cos(Zt,x) -> "ccos(" + x.evalC c + ")"
                |Cos(_,x) -> "cos(" + x.evalC c + ")"
                |Tan(Zt,x) -> "ctan(" + x.evalC c + ")"
                |Tan(_,x) -> "tan(" + x.evalC c + ")"
                |Asin(Zt,x) -> "casin(" + x.evalC c + ")"
                |Asin(_,x) -> "asin(" + x.evalC c + ")"
                |Acos(Zt,x) -> "cacos(" + x.evalC c + ")"
                |Acos(_,x) -> "acos(" + x.evalC c + ")"
                |Atan(Zt,x) -> "catan(" + x.evalC c + ")"
                |Atan(_,x) -> "atan(" + x.evalC c + ")"
                |Atan2(x,y) -> "atan2(" + x.evalC c + "," + y.evalC c + ")"
                |Abs(Dt,x) when x.etype=Zt -> "cabs(" + x.evalC c + ")"
                |Abs(Dt,x) -> "fabs(" + x.evalC c + ")"
                |Abs(_,x) -> "abs(" + x.evalC c + ")"
                |Log(Zt,x) -> "clog(" + x.evalC c + ")"
                |Log(_,x) -> "log(" + x.evalC c + ")"
                |Log10(_,x) -> "log10(" + x.evalC c + ")"
                |Sqrt(Zt,x) -> "csqrt(" + x.evalC c + ")"
                |Sqrt(_,x) -> "sqrt(" + x.evalC c + ")"
                |ToInt x -> 
                    match x with
                    |Add _|Sub _ |Mul _ |Div _ ->
                        "(int)(" + x.evalC c + ")"
                    |_ ->
                        "(int)" + x.evalC c
                |ToDbl x -> 
                    match x with
                    |Add _|Sub _ |Mul _ |Div _ ->
                        "(double)(" + x.evalC c + ")"
                    |_ ->
                        "(double)" + x.evalC c
                |Floor x -> "floor(" + x.evalC c + ")"
                |Ceil x -> "ceil(" + x.evalC c + ")"
                |Re x -> "creal(" + x.evalC c + ")"
                |Im x -> "cimag(" + x.evalC c + ")"
                |Conj x -> "conj(" + x.evalC c + ")"
                |Idx1 (_,name,i) -> name + "[" + i.evalC c + "]"
                |Idx2 (_,name,i,j) ->
                    printfn "C言語では2次元配列の代わりに1次元配列を使用します"
                    "NaN"
                |Idx3 (_,name,i,j,k) -> 
                    printfn "C言語では3次元配列の代わりに1次元配列を使用します"
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
                    |_ -> expr.substC x y c
                    (f x).evalC c
                |Sum(t, n1, n2, f) ->
                    // 合計値格納用変数
                    (Let(t, Int 0, fun u ->
                        expr.forLoopC c (n1,n2) <| fun i ->
                            // 加算・代入処理
                            expr.substC u (Add(t,u, f i)) c
                        u)).evalC c
                |IfEl(cond,n1,n2) -> 
                    (Let(n1.etype, NaN, fun x -> 
                        expr.branchC c <| fun (ifcode,_,elsecode) ->
                            ifcode cond <| fun () ->
                                expr.substC x n1 c
                            elsecode <| fun () ->
                                expr.substC x n2 c
                        x)).evalC c
                |NaN -> "NaN"
