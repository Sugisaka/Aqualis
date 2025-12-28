namespace Aqualis
    
    [<AutoOpen>]
    module exprEvalF =
        
        open System
        
        type expr with
            
            static member substF (x:expr) (y:expr) (c:program) =
                c.cwriter.codewrite (x.evalF c  + " = " + y.evalF c)
                
            static member equivF (x:expr) (y:expr) (c:program) =
                printfn "Fortranでこの文は使用できません"
                
            static member equivAlignF (x:expr) (y:expr) (c:program) =
                printfn "Fortranでこの文は使用できません"
                
            static member forLoopF (c:program) (n1:expr,n2:expr) code =
                let iname,returnVar = c.i0.getVar()
                let i = Var(It 4, iname, NaN)
                let n1_ = n1.evalF c
                let n2_ = n2.evalF c
                if isParMode then pr.varPrivate.setVar(It 4,A0,iname,"")
                c.cwriter.codewrite("do " + i.evalF c + "=" + n1_ + "," + n2_)
                c.cwriter.indent.inc()
                code i
                c.cwriter.indent.dec()
                c.cwriter.codewrite "end do"
                returnVar()
                
            ///<summary>無限ループ</summary>
            static member loopF (c:program) code =
                let iname,returnVar = c.i0.getVar()
                let i = Var(It 4, iname, NaN)
                let label = gotoLabel.nextGotoLabel()
                let exit() = c.cwriter.codewrite("goto "+label)
                expr.substF i (Int 1) c
                c.cwriter.codewrite "do"
                c.cwriter.indent.inc()
                code(exit,i)
                expr.substF i (Add(It 4, i, Int 1)) c
                c.cwriter.indent.dec()
                c.cwriter.codewrite "end do"
                c.cwriter.codewrite(label+" continue")
                returnVar()
                
            ///<summary>条件を満たす間ループ</summary>
            static member whiledoF (c:program) (cond:expr) = fun code ->
                c.cwriter.codewrite("do while(" + cond.evalF c + ")")
                c.cwriter.indent.inc()
                code()
                c.cwriter.indent.dec()
                c.cwriter.codewrite "end do"
                
            ///<summary>指定した範囲でループ</summary>
            static member rangeF (c:program) (i1:expr) = fun (i2:expr) -> fun code -> 
                match i1.simp,i2.simp with
                |Int a, Int b when a>b -> 
                    let iname,returnVar = c.i0.getVar()
                    let i = Var(It 4, iname, NaN)
                    if isParMode then pr.varPrivate.setVar(It 4,A0,iname,"")
                    c.cwriter.comment("do " + i.evalF c + "=" + i1.evalF c + "," + i2.evalF c)
                    c.cwriter.indent.inc()
                    code i
                    c.cwriter.indent.dec()
                    c.cwriter.comment "end do"
                    returnVar()
                |i1,i2 ->
                    let iname,returnVar = c.i0.getVar()
                    let i = Var(It 4, iname, NaN)
                    if isParMode then pr.varPrivate.setVar(It 4,A0,iname,"")
                    c.cwriter.codewrite("do " + i.evalF c + "=" + i1.evalF c + "," + i2.evalF c)
                    c.cwriter.indent.inc()
                    code i
                    c.cwriter.indent.dec()
                    c.cwriter.codewrite "end do"
                    returnVar()
                    
            ///<summary>指定した範囲でループ(途中脱出可)</summary>
            static member range_exitF (c:program) (i1:expr) = fun (i2:expr) -> fun code -> 
                match i1.simp,i2.simp with
                |Int a, Int b when a>b -> 
                    let iname,returnVar = c.i0.getVar()
                    let i = Var(It 4, iname, NaN)
                    let label = gotoLabel.nextGotoLabel()
                    let exit() = c.cwriter.codewrite("goto "+label)
                    if isParMode then pr.varPrivate.setVar(It 4,A0,iname,"")
                    c.cwriter.comment("do " + i.evalF c + "=" + i1.evalF c + "," + i2.evalF c)
                    c.cwriter.indent.inc()
                    code(exit,i)
                    c.cwriter.indent.dec()
                    c.cwriter.comment("end do")
                    c.cwriter.comment(label+" continue")
                    returnVar()
                |i1,i2 ->
                    let iname,returnVar = c.i0.getVar()
                    let i = Var(It 4, iname, NaN)
                    let label = gotoLabel.nextGotoLabel()
                    let exit() = c.cwriter.codewrite("goto "+label)
                    if isParMode then pr.varPrivate.setVar(It 4,A0,iname,"")
                    c.cwriter.codewrite("do " + i.evalF c + "=" + i1.evalF c + "," + i2.evalF c)
                    c.cwriter.indent.inc()
                    code(exit,i)
                    c.cwriter.indent.dec()
                    c.cwriter.codewrite("end do")
                    c.cwriter.codewrite(label+" continue")
                    returnVar()
                    
            static member branchF (c:program) code =
                let ifcode (cond:expr) code =
                    let cond = cond.evalF c
                    c.cwriter.codewrite("if(" + cond + ") then")
                    c.cwriter.indent.inc()
                    code()
                    c.cwriter.indent.dec()
                let elseifcode (cond:expr) code =
                    let cond = cond.evalF c
                    c.cwriter.codewrite("else if(" + cond + ") then")
                    c.cwriter.indent.inc()
                    code()
                    c.cwriter.indent.dec()
                let elsecode code =
                    c.cwriter.codewrite "else"
                    c.cwriter.indent.inc()
                    code()
                    c.cwriter.indent.dec()
                code(ifcode,elseifcode,elsecode)
                c.cwriter.codewrite "endif"
                
            member this.evalF(c:program) =
                match this.simp with
                |False -> "false"
                |True -> "true"
                |Eq(x,y) -> x.evalF c + " == " + y.evalF c
                |NEq(x,y) -> x.evalF c + " =/ " + y.evalF c
                |Greater(x,y) -> x.evalF c + " > " + y.evalF c
                |GreaterEq(x,y) -> x.evalF c + " >= " + y.evalF c
                |Less(x,y) -> x.evalF c + " < " + y.evalF c
                |LessEq(x,y) -> x.evalF c + " <= " + y.evalF c
                |AND x -> 
                    x 
                    |> List.map (fun v -> match v with |OR _ -> "(" + v.evalF c + ")" |_ -> v.evalF c)
                    |> fun lst -> String.Join(" .and. ", lst)
                |OR x -> 
                    x 
                    |> List.map (fun v -> match v with |AND _ -> "(" + v.evalF c + ")" |_ -> v.evalF c)
                    |> fun lst -> String.Join(" .or. ", lst)
                |Int x -> c.numFormat.ItoS x
                |Dbl x -> c.numFormat.DtoS x
                |Cpx (0.0,1.0) -> "uj"
                |Cpx (re,im) -> (Add(Zt, Dbl re, Mul(Zt, Cpx(0.0,1.0), Dbl im))).evalF c
                |Var (_,s,x) -> s
                |Inv(_,x) -> 
                    match x with
                    |Add _|Sub _ -> "-(" + x.evalF c + ")"
                    |_ -> "-" + x.evalF c
                |Add(_,x,y) -> x.evalF c + "+" + y.evalF c
                |Sub(_,x,y) -> 
                    match x,y with
                    |x,(Add _|Sub _) -> x.evalF c + "-(" + y.evalF c + ")"
                    |_ -> x.evalF c + "-" + y.evalF c
                |Mul(_,x,y) ->
                    match x,y with
                    |(Add _|Sub _),(Add _|Sub _) -> "(" + x.evalF c + ")*(" + y.evalF c + ")"
                    |(Add _|Sub _),_ -> "(" + x.evalF c + ")*" + y.evalF c
                    |_,(Add _|Sub _) -> x.evalF c + "*(" + y.evalF c + ")"
                    |_ -> x.evalF c + "*" + y.evalF c
                |Div(Dt,x,y) when x.etype = It 4 && y.etype = It 4 -> 
                    (ToDbl x/ToDbl y).evalF c
                |Div(_,x,y) -> 
                    match x,y with
                    |(Add _|Sub _),(Add _|Sub _|Mul _|Div _) -> "(" + x.evalF c + ")/(" + y.evalF c + ")"
                    |(Add _|Sub _),_ -> "(" + x.evalF c + ")/" + y.evalF c
                    |_,(Add _|Sub _|Mul _|Div _) -> 
                        x.evalF c + "/(" + y.evalF c + ")"
                    |_ -> x.evalF c + "/" + y.evalF c
                |Mod(_,x,y) -> "mod(" + x.evalF c + "," + y.evalF c + ")"
                |Pow(_,x,y) -> 
                    match x,y with
                    |(Add _|Sub _|Mul _|Div _),(Add _|Sub _|Mul _|Div _) -> 
                        "(" + x.evalF c + ")**(" + y.evalF c + ")"
                    |(Add _|Sub _|Mul _|Div _),_ -> 
                        "(" + x.evalF c + ")**" + y.evalF c + ""
                    |_,(Add _|Sub _|Mul _|Div _) -> 
                        "" + x.evalF c + "**(" + y.evalF c + ")"
                    |_ -> 
                        x.evalF c + "**" + y.evalF c
                |Exp(_,x) -> "exp(" + x.evalF c + ")"
                |Sin(_,x) -> "sin(" + x.evalF c + ")"
                |Cos(_,x) -> "cos(" + x.evalF c + ")"
                |Tan(_,x) -> "tan(" + x.evalF c + ")"
                |Asin(_,x) -> "asin(" + x.evalF c + ")"
                |Acos(_,x) -> "acos(" + x.evalF c + ")"
                |Atan(_,x) -> "atan(" + x.evalF c + ")"
                |Atan2(x,y) -> "atan2(" + x.evalF c + "," + y.evalF c + ")"
                |Abs(_,x) -> "abs(" + x.evalF c + ")"
                |Log(_,x) -> "log(" + x.evalF c + ")"
                |Log10(_,x) -> "log10(" + x.evalF c + ")"
                |Sqrt(_,x) -> "sqrt(" + x.evalF c + ")"
                |ToInt x -> "int(" + x.evalF c + ")"
                |ToDbl x -> "dble(" + x.evalF c + ")"
                |Floor x -> "floor(" + x.evalF c + ")"
                |Ceil x -> "ceiling(" + x.evalF c + ")"
                |Re x -> "real(" + x.evalF c + ")"
                |Im x -> "aimag(" + x.evalF c + ")"
                |Conj x -> "conjg(" + x.evalF c + ")"
                |Idx1 (_,name,i) -> name + "(" + i.evalF c + ")"
                |Idx2 (_,name,i,j) -> name + "(" + i.evalF c + "," + j.evalF c + ")"
                |Idx3 (_,name,i,j,k) -> name + "(" + i.evalF c + "," + j.evalF c + "," + k.evalF c + ")"
                |Let (t,y,f) -> 
                    let x =
                        match t with
                        |It 4 -> Var (t, (fun (a,_) -> a) (c.i0.getVar()), y)
                        |Dt   -> Var (t, (fun (a,_) -> a) (c.d0.getVar()), y)
                        |Zt   -> Var (t, (fun (a,_) -> a) (c.z0.getVar()), y)
                        |_    -> NaN
                    match y with
                    |NaN -> ()
                    |_ -> expr.substF x y c
                    (f x).evalF c
                |Sum(t, n1, n2, f) ->
                    // 合計値格納用変数
                    (Let(t, Int 0, fun u ->
                        expr.forLoopF c (n1,n2) <| fun i ->
                            // 加算・代入処理
                            expr.substF u (Add(t,u, f i)) c
                        u)).evalF c
                |IfEl(cond,n1,n2) -> 
                    (Let(n1.etype, NaN, fun x -> 
                        expr.branchF c <| fun (ifcode,_,elsecode) ->
                            ifcode cond <| fun () ->
                                expr.substF x n1 c
                            elsecode <| fun () ->
                                expr.substF x n2 c
                        x)).evalF c
                |NaN -> "NaN"
