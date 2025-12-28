namespace Aqualis
    
    [<AutoOpen>]
    module exprEvalPy =
        
        open System
        
        type expr with
            
            static member substPy (x:expr) (y:expr) (c:program) =
                c.cwriter.codewrite (x.evalPy c  + " = " + y.evalPy c)
                
            static member equivPy (x:expr) (y:expr) (c:program) =
                printfn "Pythonでこの文は使用できません"
                
            static member equivAlignPy (x:expr) (y:expr) (c:program) =
                printfn "Pythonでこの文は使用できません"
                
            static member forLoopPy (c:program) (n1:expr,n2:expr) code =
                let iname,returnVar = c.i0.getVar()
                let i = Var(It 4, iname, NaN)
                let n1_ = n1.evalPy c
                let n2_ = (Add(It 4, n2, Int 1)).evalPy c
                c.cwriter.codewrite("for " + i.evalPy c + " in range(" + n1_ + ", " + n2_ + ", 1):")
                c.cwriter.indent.inc()
                code i
                c.cwriter.indent.dec()
                returnVar()
                
            ///<summary>無限ループ</summary>
            static member loopPy (c:program) code =
                let iname,returnVar = c.i0.getVar()
                let i = Var(It 4, iname, NaN)
                let label = gotoLabel.nextGotoLabel()
                let exit() = c.cwriter.codewrite("goto "+label)
                expr.substPy i (Int 1) c
                c.cwriter.codewrite "while True:"
                c.cwriter.indent.inc()
                code(exit,i)
                c.cwriter.codewrite("flag = " + label)
                expr.substPy i (Add(It 4, i, Int 1)) c
                c.cwriter.indent.dec()
                if label = "10" then
                    gotoLabel.exit_reset()
                else 
                    c.cwriter.codewrite("if flag < " + label + ":")
                    c.cwriter.indent.inc()
                    c.cwriter.codewrite "break"
                    c.cwriter.indent.dec()
                returnVar()
                
            ///<summary>条件を満たす間ループ</summary>
            static member whiledoPy (c:program) (cond:expr) = fun code ->
                c.cwriter.codewrite("while(" + cond.evalPy c + ")")
                c.cwriter.indent.inc()
                code()
                c.cwriter.indent.dec()
                
            ///<summary>指定した範囲でループ</summary>
            static member rangePy (c:program) (i1:expr) = fun (i2:expr) -> fun code -> 
                match i1.simp,i2.simp with
                |Int a, Int b when a>b -> 
                    let iname,returnVar = c.i0.getVar()
                    let i = Var(It 4, iname, NaN)
                    c.cwriter.comment("for " + i.evalPy c + " in range("+i1.evalPy c + ", " + (Add(It 4,i2,Int 1)).evalPy c + ", 1):")
                    c.cwriter.indent.inc()
                    code i
                    c.cwriter.indent.dec()
                    returnVar()
                |_ ->
                    let iname,returnVar = c.i0.getVar()
                    let i = Var(It 4, iname, NaN)
                    c.cwriter.codewrite("for " + i.evalPy c + " in range("+i1.evalPy c + ", " + (Add(It 4,i2,Int 1)).evalPy c + ", 1):")
                    c.cwriter.indent.inc()
                    code i
                    c.cwriter.indent.dec()
                    returnVar()
                    
            ///<summary>指定した範囲でループ(途中脱出可)</summary>
            static member range_exitPy (c:program) (i1:expr) = fun (i2:expr) -> fun code -> 
                match i1.simp,i2.simp with
                |Int a, Int b when a>b -> 
                    let iname,returnVar = c.i0.getVar()
                    let i = Var(It 4, iname, NaN)
                    let label = gotoLabel.nextGotoLabel()
                    let exit() = c.cwriter.comment("goto "+label)
                    c.cwriter.comment("for " + i.evalPy c + " in range(" + i1.evalPy c + ", " + (Add(It 4,i2,Int 1)).evalPy c + ", 1):")
                    c.cwriter.indent.inc()
                    code(exit,i)
                    c.cwriter.indent.dec()
                    if label = "10" then
                        gotoLabel.exit_reset()
                    else 
                        c.cwriter.comment("if flag < "+label)
                        c.cwriter.indent.inc()
                        c.cwriter.comment "break"
                        c.cwriter.indent.dec()
                    returnVar()
                |_ ->
                    let iname,returnVar = c.i0.getVar()
                    let i = Var(It 4, iname, NaN)
                    let label = gotoLabel.nextGotoLabel()
                    let exit() = c.cwriter.codewrite("goto "+label)
                    c.cwriter.codewrite("for " + i.evalPy c + " in range(" + i1.evalPy c + ", " + (Add(It 4,i2,Int 1)).evalPy c + ", 1):")
                    c.cwriter.indent.inc()
                    code(exit,i)
                    c.cwriter.indent.dec()
                    if label = "10" then
                        gotoLabel.exit_reset()
                    else 
                        c.cwriter.codewrite("if flag < "+label+":")
                        c.cwriter.indent.inc()
                        c.cwriter.codewrite "break"
                        c.cwriter.indent.dec()
                    returnVar()
                    
            static member branchPy (c:program) code =
                let ifcode (cond:expr) code =
                    let cond = cond.evalPy c
                    c.cwriter.codewrite("if " + cond + ":")
                    c.cwriter.indent.inc()
                    code()
                    c.cwriter.indent.dec()
                let elseifcode (cond:expr) code =
                    let cond = cond.evalPy c
                    c.cwriter.codewrite("elif " + cond + ":")
                    c.cwriter.indent.inc()
                    code()
                    c.cwriter.indent.dec()
                let elsecode code =
                    c.cwriter.codewrite "else:"
                    c.cwriter.indent.inc()
                    code()
                    c.cwriter.indent.dec()
                code(ifcode,elseifcode,elsecode)
                
            member this.evalPy(c:program) =
                match this.simp with
                |False -> "false"
                |True -> "true"
                |Eq(x,y) -> x.evalPy c + " == " + y.evalPy c
                |NEq(x,y) -> x.evalPy c + " =/ " + y.evalPy c
                |Greater(x,y) -> x.evalPy c + " > " + y.evalPy c
                |GreaterEq(x,y) -> x.evalPy c + " >= " + y.evalPy c
                |Less(x,y) -> x.evalPy c + " < " + y.evalPy c
                |LessEq(x,y) -> x.evalPy c + " <= " + y.evalPy c
                |AND x -> 
                    x 
                    |> List.map (fun v -> match v with |OR _ |AND _ -> "(" + v.evalPy c + ")" |_ -> v.evalPy c)
                    |> fun lst -> String.Join(" and ", lst)
                |OR x -> 
                    x 
                    |> List.map (fun v -> match v with |OR _ |AND _ -> "(" + v.evalPy c + ")" |_ -> v.evalPy c)
                    |> fun lst -> String.Join(" or ", lst)
                |Int x -> c.numFormat.ItoS x
                |Dbl x -> c.numFormat.DtoS x
                |Cpx (0.0,1.0) -> "uj"
                |Cpx (re,im) -> (Add(Zt, Dbl re, Mul(Zt, Cpx(0.0,1.0), Dbl im))).evalPy c
                |Var (_,s,x) -> s
                |Inv(_,x) -> 
                    match x with
                    |Add _|Sub _ -> "-(" + x.evalPy c + ")"
                    |_ -> "-" + x.evalPy c
                |Add(_,x,y) -> x.evalPy c + "+" + y.evalPy c
                |Sub(_,x,y) -> 
                    match x,y with
                    |x,(Add _|Sub _) -> x.evalPy c + "-(" + y.evalPy c + ")"
                    |_ -> x.evalPy c + "-" + y.evalPy c
                |Mul(_,x,y) ->
                    match x,y with
                    |(Add _|Sub _),(Add _|Sub _) -> "(" + x.evalPy c + ")*(" + y.evalPy c + ")"
                    |(Add _|Sub _),_ -> "(" + x.evalPy c + ")*" + y.evalPy c
                    |_,(Add _|Sub _) -> x.evalPy c + "*(" + y.evalPy c + ")"
                    |_ -> x.evalPy c + "*" + y.evalPy c
                |Div(Dt,x,y) when x.etype = It 4 && y.etype = It 4 -> 
                    (ToDbl x/ToDbl y).evalPy c
                |Div(_,x,y) ->
                    match x,y with
                    |(Add _|Sub _),(Add _|Sub _|Mul _|Div _) -> "(" + x.evalPy c + ")/(" + y.evalPy c + ")"
                    |(Add _|Sub _),_ -> "(" + x.evalPy c + ")/" + y.evalPy c
                    |_,(Add _|Sub _|Mul _|Div _) -> x.evalPy c + "/(" + y.evalPy c + ")"
                    |_ -> x.evalPy c + "/" + y.evalPy c
                |Mod(_,x,y) -> "divmod(" + x.evalPy c + "," + y.evalPy c + ")"
                |Pow(_,x,y) ->
                    match x,y with
                    |(Add _|Sub _|Mul _|Div _),(Add _|Sub _|Mul _|Div _) -> 
                        "(" + x.evalPy c + ")**(" + y.evalPy c + ")"
                    |(Add _|Sub _|Mul _|Div _),_ -> 
                        "(" + x.evalPy c + ")**" + y.evalPy c + ""
                    |_,(Add _|Sub _|Mul _|Div _) -> 
                        "" + x.evalPy c + "**(" + y.evalPy c + ")"
                    |_ -> 
                        x.evalPy c + "**" + y.evalPy c
                |Exp(Zt,x) -> "cmath.exp(" + x.evalPy c + ")"
                |Sin(Zt,x) -> "cmath.sin(" + x.evalPy c + ")"
                |Cos(Zt,x) -> "cmath.cos(" + x.evalPy c + ")"
                |Tan(Zt,x) -> "cmath.tan(" + x.evalPy c + ")"
                |Asin(Zt,x) -> "cmath.asin(" + x.evalPy c + ")"
                |Acos(Zt,x) -> "cmath.acos(" + x.evalPy c + ")"
                |Atan(Zt,x) -> "cmath.atan(" + x.evalPy c + ")"
                |Abs(Zt,x) -> "cmath.abs(" + x.evalPy c + ")"
                |Log(Zt,x) -> "cmath.log(" + x.evalPy c + ")"
                |Log10(Zt,x) -> "cmath.log10(" + x.evalPy c + ")"
                |Sqrt(Zt,x) -> "cmath.sqrt(" + x.evalPy c + ")"
                |Exp(_,x) -> "math.exp(" + x.evalPy c + ")"
                |Sin(_,x) -> "math.sin(" + x.evalPy c + ")"
                |Cos(_,x) -> "math.cos(" + x.evalPy c + ")"
                |Tan(_,x) -> "math.tan(" + x.evalPy c + ")"
                |Asin(_,x) -> "math.asin(" + x.evalPy c + ")"
                |Acos(_,x) -> "math.acos(" + x.evalPy c + ")"
                |Atan(_,x) -> "math.atan(" + x.evalPy c + ")"
                |Atan2(x,y) -> "math.atan2(" + x.evalPy c + "," + y.evalPy c + ")"
                |Abs(_,x) -> "abs(" + x.evalPy c + ")"
                |Log(_,x) -> "math.log(" + x.evalPy c + ")"
                |Log10(_,x) -> "math.log10(" + x.evalPy c + ")"
                |Sqrt(_,x) -> "math.sqrt(" + x.evalPy c + ")"
                |ToInt x -> "int(" + x.evalPy c + ")"
                |ToDbl x -> "float(" + x.evalPy c + ")"
                |Floor x -> "math.floor(" + x.evalPy c + ")"
                |Ceil x -> "math.ceil(" + x.evalPy c + ")"
                |Re x -> "(" + x.evalPy c + ").real"
                |Im x -> "(" + x.evalPy c + ").imag"
                |Conj x -> "conjugate(" + x.evalPy c + ")"
                |Idx1 (_,name,i) -> name + "[" + i.evalPy c + "]"
                |Idx2 (_,name,i,j) -> name + "[" + i.evalPy c + "," + j.evalPy c + "]"
                |Idx3 (_,name,i,j,k) -> name + "[" + i.evalPy c + "," + j.evalPy c + "," + k.evalPy c + "]"
                |Let (t,y,f) -> 
                    let x =
                        match t with
                        |It 4 -> Var (t, (fun (a,_) -> a) (c.i0.getVar()), y)
                        |Dt   -> Var (t, (fun (a,_) -> a) (c.d0.getVar()), y)
                        |Zt   -> Var (t, (fun (a,_) -> a) (c.z0.getVar()), y)
                        |_    -> NaN
                    match y with
                    |NaN -> ()
                    |_ -> expr.substPy x y c
                    (f x).evalPy c
                |Sum(t, n1, n2, f) ->
                    // 合計値格納用変数
                    (Let(t, Int 0, fun u ->
                        expr.forLoopPy c (n1,n2) <| fun i ->
                            // 加算・代入処理
                            expr.substPy u (Add(t,u, f i)) c
                        u)).evalPy c
                |IfEl(cond,n1,n2) -> 
                    (Let(n1.etype, NaN, fun x -> 
                        expr.branchPy c <| fun (ifcode,_,elsecode) ->
                            ifcode cond <| fun () ->
                                expr.substPy x n1 c
                            elsecode <| fun () ->
                                expr.substPy x n2 c
                        x)).evalPy c
                |NaN -> "NaN"
