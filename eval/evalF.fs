//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    /// Expression output operations for Fortran.
    [<AutoOpen>]
    module exprEvalF =

        open System

        /// The DO variable must represent the value immediately after the 32-bit endpoint.
        let private wideCounter (c:Aqualis) name =
            let mutable candidate = name + "_wide"
            while varGenerator.isVarExist(c,candidate,It 8,A0) do candidate <- candidate + "_"
            c.cvar.setUniqVar(It 8,A0,candidate,"")
            if c.ParallelMode then c.varPrivate.setVar(It 8,A0,candidate,"")
            candidate

        type expr with

            /// Emits an assignment for Fortran.
            static member substF (x:expr) (y:expr) (c:Aqualis) =
                c.codewritein (x.evalF c  + " = " + y.evalF c)

            /// Reports that equation display is unsupported for this target language.
            static member equivF (x:expr) (y:expr) (c:Aqualis) =
                UnsupportedOperation.codeGeneration "Fortran" "equation display"

            /// Reports that aligned equation display is unsupported for this target language.
            static member equivAlignF (x:expr) (y:expr) (c:Aqualis) =
                UnsupportedOperation.codeGeneration "Fortran" "aligned equation display"

            /// Emits a counted loop for Fortran.
            static member forLoopF (c:Aqualis) (n1:expr,n2:expr) code =
                let iname,returnVar = c.i0.getVar()
                let i = Var(It 4, iname, NaN)
                let n1_ = n1.evalF c
                let n2_ = n2.evalF c
                let control = wideCounter c iname
                if c.ParallelMode then c.varPrivate.setVar(It 4,A0,iname,"")
                c.codewritein("do " + control + "=" + n1_ + "," + n2_)
                c.indentInc()
                c.codewritein(i.evalF c + " = int(" + control + ")")
                code i
                c.indentDec()
                c.codewritein "end do"
                returnVar()

            /// Emits an unbounded loop for Fortran.
            static member loopF (c:Aqualis) code =
                let iname,returnVar = c.i0.getVar()
                let i = Var(It 4, iname, NaN)
                let label = c.GotoLabels.nextGotoLabel()
                let exit() = c.codewritein("goto "+label)
                expr.substF i (Int 1) c
                c.codewritein "do"
                c.indentInc()
                code(exit,i)
                expr.substF i (Add(It 4, i, Int 1)) c
                c.indentDec()
                c.codewritein "end do"
                c.codewritein(label+" continue")
                returnVar()

            /// Emits a conditional loop for Fortran.
            static member whiledoF (c:Aqualis) (cond:expr) = fun code ->
                let statements = cond.requiresStatements true
                c.codewritein(if statements then "do" else "do while(" + cond.evalF c + ")")
                c.indentInc()
                if statements then
                    let value = cond.evalF c
                    c.codewritein("if (.not.(" + value + ")) exit")
                code()
                c.indentDec()
                c.codewritein "end do"

            /// Emits an inclusive range loop for Fortran.
            static member rangeF (c:Aqualis) (counter:option<string>) (i1:expr) = fun (i2:expr) -> fun code ->
                match i1.simp,i2.simp with
                |Int a, Int b when a>b && not c.ParallelMode ->
                    let iname,returnVar = match counter with |None -> c.i0.getVar() |Some s -> c.i0.getVar (s,It 4,A0)
                    let i = Var(It 4, iname, NaN)
                    if c.ParallelMode then c.varPrivate.setVar(It 4,A0,iname,"")
                    c.captureCode(fun () -> code i) |> ignore
                    returnVar()
                |i1,i2 ->
                    let iname,returnVar = match counter with |None -> c.i0.getVar() |Some s -> c.i0.getVar (s,It 4,A0)
                    let i = Var(It 4, iname, NaN)
                    if c.ParallelMode then c.varPrivate.setVar(It 4,A0,iname,"")
                    let control = wideCounter c iname
                    c.codewritein("do " + control + "=" + i1.evalF c + "," + i2.evalF c)
                    c.indentInc()
                    c.codewritein(i.evalF c + " = int(" + control + ")")
                    code i
                    c.indentDec()
                    c.codewritein "end do"
                    returnVar()

            /// Emits an inclusive range loop with an exit action for Fortran.
            static member range_exitF (c:Aqualis) (counter:option<string>) (i1:expr) = fun (i2:expr) -> fun code ->
                match i1.simp,i2.simp with
                |Int a, Int b when a>b && not c.ParallelMode ->
                    let iname,returnVar = match counter with |None -> c.i0.getVar() |Some s -> c.i0.getVar (s,It 4,A0)
                    let i = Var(It 4, iname, NaN)
                    let exit() = ()
                    if c.ParallelMode then c.varPrivate.setVar(It 4,A0,iname,"")
                    c.captureCode(fun () -> code(exit,i)) |> ignore
                    returnVar()
                |i1,i2 ->
                    let iname,returnVar = match counter with |None -> c.i0.getVar() |Some s -> c.i0.getVar (s,It 4,A0)
                    let i = Var(It 4, iname, NaN)
                    let label = c.GotoLabels.nextGotoLabel()
                    let exit() = c.codewritein("goto "+label)
                    if c.ParallelMode then c.varPrivate.setVar(It 4,A0,iname,"")
                    let control = wideCounter c iname
                    c.codewritein("do " + control + "=" + i1.evalF c + "," + i2.evalF c)
                    c.indentInc()
                    c.codewritein(i.evalF c + " = int(" + control + ")")
                    code(exit,i)
                    c.indentDec()
                    c.codewritein "end do"
                    c.codewritein(label+" continue")
                    returnVar()

            /// Emits a branch callback for Fortran.
            static member branchF (c:Aqualis) code =
                let mutable nestedElseCount = 0
                let ifcode (cond:expr) code =
                    let cond = cond.evalF c
                    c.codewritein("if(" + cond + ") then")
                    c.indentInc()
                    code()
                    c.indentDec()
                let elseifcode (cond:expr) code =
                    // A logical condition can emit short-circuit branches. Keep those
                    // statements inside the preceding branch's else clause.
                    c.codewritein "else"
                    c.indentInc()
                    nestedElseCount <- nestedElseCount + 1
                    let cond = cond.evalF c
                    c.codewritein("if(" + cond + ") then")
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
                for _ in 1..nestedElseCount do
                    c.indentDec()
                    c.codewritein "endif"

            /// Renders an expression for Fortran.
            member this.evalF(c:Aqualis) =
                let complexArgument (value:expr) =
                    if value.etype=Zt then value.evalF c
                    else "cmplx(" + value.evalF c + ",kind=kind(0.0d0))"
                let comparisonOperand (value:expr) =
                    let rendered = value.evalF c
                    if value.etype = Bt then "(" + rendered + ")" else rendered
                match this.simp with
                |False -> ".false."
                |True -> ".true."
                |Eq(x,y) -> comparisonOperand x + (if x.etype = Bt && y.etype = Bt then " .eqv. " else " == ") + comparisonOperand y
                |NEq(x,y) -> comparisonOperand x + (if x.etype = Bt && y.etype = Bt then " .neqv. " else " /= ") + comparisonOperand y
                |Greater(x,y) -> comparisonOperand x + " > " + comparisonOperand y
                |GreaterEq(x,y) -> comparisonOperand x + " >= " + comparisonOperand y
                |Less(x,y) -> comparisonOperand x + " < " + comparisonOperand y
                |LessEq(x,y) -> comparisonOperand x + " <= " + comparisonOperand y
                |AND x when x.Length > 1 -> (expr.shortCircuit true x).evalF c
                |AND x ->
                    x
                    |> List.map (fun v -> match v with |OR _ -> "(" + v.evalF c + ")" |_ -> v.evalF c)
                    |> fun lst -> String.Join(" .and. ", lst)
                |OR x when x.Length > 1 -> (expr.shortCircuit false x).evalF c
                |OR x ->
                    x
                    |> List.map (fun v -> match v with |AND _ -> "(" + v.evalF c + ")" |_ -> v.evalF c)
                    |> fun lst -> String.Join(" .or. ", lst)
                |Int x when x = Int32.MinValue -> "(-2147483647-1)"
                |Int x -> c.numFormat.ItoS x
                |Dbl x -> c.numFormat.DtoS x
                |Cpx (0.0,1.0) -> "(0d0,1d0)"
                |Cpx (re,im) when not (Double.IsFinite re && Double.IsFinite im) ->
                    "cmplx(" + c.numFormat.DtoS re + "," + c.numFormat.DtoS im + ",kind=kind(0.0d0))"
                |Cpx (re,im) -> "(" + c.numFormat.DtoS re + "," + c.numFormat.DtoS im + ")"
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
                    |_,(Add _|Sub _|Div _) -> x.evalF c + "*(" + y.evalF c + ")"
                    |_ -> x.evalF c + "*" + y.evalF c
                |Div(Dt,x,y) when x.etype = It 4 && y.etype = It 4 ->
                    (ToDbl x/ToDbl y).evalF c
                |Div(It 4,x,y) -> "aqualis_int_div(" + x.evalF c + "," + y.evalF c + ")"
                |Div(_,x,y) ->
                    match x,y with
                    |(Add _|Sub _),(Add _|Sub _|Mul _|Div _) -> "(" + x.evalF c + ")/(" + y.evalF c + ")"
                    |(Add _|Sub _),_ -> "(" + x.evalF c + ")/" + y.evalF c
                    |_,(Add _|Sub _|Mul _|Div _) ->
                        x.evalF c + "/(" + y.evalF c + ")"
                    |_ -> x.evalF c + "/" + y.evalF c
                |Mod(_,x,y) -> "mod(" + x.evalF c + "," + y.evalF c + ")"
                |Pow(t,x,y) ->
                    let baseValue =
                        if t=Zt && x.etype<>Zt then
                            "cmplx(" + x.evalF c + ",kind=kind(0.0d0))"
                        elif t=Dt && (match x.etype with It _ -> true | _ -> false) then
                            "dble(" + x.evalF c + ")"
                        else
                            match x with
                            |Add _|Sub _|Mul _|Div _|Inv _|Pow _ -> "(" + x.evalF c + ")"
                            |_ -> x.evalF c
                    let exponentValue =
                        match y with
                        |Add _|Sub _|Mul _|Div _|Inv _ -> "(" + y.evalF c + ")"
                        |Int value when value < 0 -> "(" + y.evalF c + ")"
                        |Dbl value when value < 0.0 -> "(" + y.evalF c + ")"
                        |_ -> y.evalF c
                    baseValue + "**" + exponentValue
                |Exp(Zt,x) -> "exp(" + complexArgument x + ")"
                |Exp(_,x) -> "exp(" + x.evalF c + ")"
                |Sin(Zt,x) -> "sin(" + complexArgument x + ")"
                |Sin(_,x) -> "sin(" + x.evalF c + ")"
                |Cos(Zt,x) -> "cos(" + complexArgument x + ")"
                |Cos(_,x) -> "cos(" + x.evalF c + ")"
                |Tan(Zt,x) -> "tan(" + complexArgument x + ")"
                |Tan(_,x) -> "tan(" + x.evalF c + ")"
                |Asin(Zt,x) -> "asin(" + complexArgument x + ")"
                |Asin(_,x) -> "asin(" + x.evalF c + ")"
                |Acos(Zt,x) -> "acos(" + complexArgument x + ")"
                |Acos(_,x) -> "acos(" + x.evalF c + ")"
                |Atan(Zt,x) -> "atan(" + complexArgument x + ")"
                |Atan(_,x) -> "atan(" + x.evalF c + ")"
                |Atan2(x,y) -> "atan2(" + x.evalF c + "," + y.evalF c + ")"
                |Abs(Dt,x) when x.etype = It 4 -> "abs(dble(" + x.evalF c + "))"
                |Abs(_,x) -> "abs(" + x.evalF c + ")"
                |Log(Zt,x) -> "log(" + complexArgument x + ")"
                |Log(_,x) -> "log(" + x.evalF c + ")"
                |Log10(Zt,x) -> "(log(" + complexArgument x + ")/log(10.0d0))"
                |Log10(_,x) -> "log10(" + x.evalF c + ")"
                |Sqrt(Zt,x) -> "sqrt(" + complexArgument x + ")"
                |Sqrt(_,x) -> "sqrt(" + x.evalF c + ")"
                |ToInt x -> "int(" + x.evalF c + ")"
                |ToDbl x -> "dble(" + x.evalF c + ")"
                |Floor x -> "floor(" + x.evalF c + ")"
                |Ceil x -> "ceiling(" + x.evalF c + ")"
                |Re x -> "real(" + x.evalF c + ",kind=kind(0.0d0))"
                |Im x -> "aimag(" + x.evalF c + ")"
                |Conj x -> "conjg(" + x.evalF c + ")"
                |Idx1 (_,name,i) -> name + "(" + i.evalF c + ")"
                |Idx2 (_,name,i,j) -> name + "(" + i.evalF c + "," + j.evalF c + ")"
                |Idx3 (_,name,i,j,k) -> name + "(" + i.evalF c + "," + j.evalF c + "," + k.evalF c + ")"
                |Let (t,y,x,f) ->
                    // let x =
                    //     match t with
                    //     |It 4 -> Var (t, (fun (a,_) -> a) (c.i0.getVar()), y)
                    //     |Dt   -> Var (t, (fun (a,_) -> a) (c.d0.getVar()), y)
                    //     |Zt   -> Var (t, (fun (a,_) -> a) (c.z0.getVar()), y)
                    //     |_    -> NaN
                    // match y with
                    // |NaN -> ()
                    // |_ -> expr.substF x y c
                    (f x).evalF c
                |Sum(t, n1, n2, f) ->
                    let v =
                        match t with
                        |It 4 -> Var (t, (fun (a,_) -> a) (c.i0.getVar()), NaN)
                        |Dt   -> Var (t, (fun (a,_) -> a) (c.d0.getVar()), NaN)
                        |Zt   -> Var (t, (fun (a,_) -> a) (c.z0.getVar()), NaN)
                        |_    -> NaN
                    expr.substF v (Int 0) c
                    // 合計値格納用変数
                    (Let(t, Int 0, v, fun u ->
                        expr.forLoopF c (n1,n2) <| fun i ->
                            // 加算・代入処理
                            expr.substF u (Add(t,u, f i)) c
                        u)).evalF c
                |IfEl(cond,n1,n2) ->
                    let v =
                        match n1.etype%%n2.etype with
                        |It 4 -> Var (It 4, (fun (a,_) -> a) (c.i0.getVar()), NaN)
                        |Dt   -> Var (Dt, (fun (a,_) -> a) (c.d0.getVar()), NaN)
                        |Zt   -> Var (Zt, (fun (a,_) -> a) (c.z0.getVar()), NaN)
                        |_    -> NaN
                    (Let(n1.etype%%n2.etype, NaN, v, fun x ->
                        expr.branchF c <| fun (ifcode,_,elsecode) ->
                            ifcode cond <| fun () ->
                                expr.substF x n1 c
                            elsecode <| fun () ->
                                expr.substF x n2 c
                        x)).evalF c
                |NaN -> "NaN"
