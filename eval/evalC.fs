//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    /// Expression output operations for C99.
    [<AutoOpen>]
    module exprEvalC =

        open System

        /// Keep the public index 32-bit while the control variable can step past INT_MAX.
        let private wideCounter (c:Aqualis) name =
            let mutable candidate = name + "_wide"
            while varGenerator.isVarExist(c,candidate,It 8,A0) do candidate <- candidate + "_"
            c.cvar.setUniqVar(It 8,A0,candidate,"")
            if c.ParallelMode then c.varPrivate.setVar(It 8,A0,candidate,"")
            c.hlist.add "<stdint.h>"
            candidate

        type expr with

            /// Emits an assignment for C99.
            static member substC (x:expr) (y:expr) (c:Aqualis) =
               c.codewritein (x.evalC c  + " = " + y.evalC c + ";")

            /// Reports that equation display is unsupported for this target language.
            static member equivC (x:expr) (y:expr) (c:Aqualis) =
                UnsupportedOperation.codeGeneration "C99" "equation display"

            /// Reports that aligned equation display is unsupported for this target language.
            static member equivAlignC (x:expr) (y:expr) (c:Aqualis) =
                UnsupportedOperation.codeGeneration "C99" "aligned equation display"

            /// Emits a counted loop for C99.
            static member forLoopC (c:Aqualis) (n1:expr,n2:expr) code =
                let iname,returnVar = c.i0.getVar()
                let i = Var(It 4, iname, NaN)
                let n1_ = n1.evalC c
                let n2_ = n2.evalC c
                let control = wideCounter c iname
                if c.ParallelMode then c.varPrivate.setVar(It 4,A0,iname,"")
                c.codewritein("for(" + control + " = " + n1_ + "; " + control + " <= " + n2_ + "; " + control + "++)")
                c.codewritein "{"
                c.indentInc()
                c.codewritein(i.evalC c + " = (int)" + control + ";")
                code i
                c.indentDec()
                c.codewritein "}"
                returnVar()

            /// Emits an unbounded loop for C99.
            static member loopC (c:Aqualis) code =
                let iname,returnVar = c.i0.getVar()
                let i = Var(It 4, iname, NaN)
                let label = "_" + c.GotoLabels.nextGotoLabel()
                let exit() = c.codewritein("goto "+label+";")
                expr.substC i (Int 1) c
                if c.ParallelMode then c.varPrivate.setVar(It 4,A0,iname,"")
                c.codewritein "for(;;)"
                c.codewritein "{"
                c.indentInc()
                code(exit,i)
                expr.substC i (Add(It 4, i, Int 1)) c
                c.indentDec()
                c.codewritein "}"
                c.codewritein(label+":;")
                returnVar()

            /// Emits a conditional loop for C99.
            static member whiledoC (c:Aqualis) (cond:expr) = fun code ->
                c.codewritein("while(" + (if cond.emitsStatements then "true" else cond.evalC c) + ")")
                c.codewritein "{"
                c.indentInc()
                if cond.emitsStatements then
                    let value = cond.evalC c
                    c.codewritein("if (!(" + value + ")) break;")
                code()
                c.indentDec()
                c.codewritein "}"

            /// Emits an inclusive range loop for C99.
            static member rangeC (c:Aqualis) (counter:option<string>) (i1:expr) = fun (i2:expr) -> fun code ->
                match i1.simp,i2.simp with
                |Int a, Int b when a>b ->
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
                    c.codewritein("for(" + control + "=" + i1.evalC c + "; " + control + "<=" + i2.evalC c + "; " + control + "++)")
                    c.codewritein "{"
                    c.indentInc()
                    c.codewritein(i.evalC c + " = (int)" + control + ";")
                    code i
                    c.indentDec()
                    c.codewritein "}"
                    returnVar()

            /// Emits an inclusive range loop with an exit action for C99.
            static member range_exitC (c:Aqualis) (counter:option<string>) (i1:expr) = fun (i2:expr) -> fun code ->
                match i1.simp,i2.simp with
                |Int a, Int b when a>b ->
                    let iname,returnVar = match counter with |None -> c.i0.getVar() |Some s -> c.i0.getVar (s,It 4,A0)
                    let i = Var(It 4, iname, NaN)
                    let exit() = ()
                    if c.ParallelMode then c.varPrivate.setVar(It 4,A0,iname,"")
                    c.captureCode(fun () -> code(exit,i)) |> ignore
                    returnVar()
                |i1,i2 ->
                    let iname,returnVar = match counter with |None -> c.i0.getVar() |Some s -> c.i0.getVar (s,It 4,A0)
                    let i = Var(It 4, iname, NaN)
                    let label = "_" + c.GotoLabels.nextGotoLabel()
                    let exit() = c.codewritein("goto "+label+";")
                    if c.ParallelMode then c.varPrivate.setVar(It 4,A0,iname,"")
                    let control = wideCounter c iname
                    c.codewritein("for(" + control + "=" + i1.evalC c + "; " + control + "<=" + i2.evalC c + "; " + control + "++)")
                    c.codewritein "{"
                    c.indentInc()
                    c.codewritein(i.evalC c + " = (int)" + control + ";")
                    code(exit,i)
                    c.indentDec()
                    c.codewritein "}"
                    c.codewritein(label+":;")
                    returnVar()

            /// Emits a branch callback for C99.
            static member branchC (c:Aqualis) code =
                let mutable nestedElseCount = 0
                let ifcode (cond:expr) code =
                    let cond = cond.evalC c
                    c.codewritein("if(" + cond + ")")
                    c.codewritein "{"
                    c.indentInc()
                    code()
                    c.indentDec()
                    c.codewritein "}"
                let elseifcode (cond:expr) code =
                    let nested = cond.emitsStatements
                    if nested then
                        c.codewritein "else"
                        c.codewritein "{"
                        c.indentInc()
                        nestedElseCount <- nestedElseCount + 1
                    let cond = cond.evalC c
                    c.codewritein((if nested then "if(" else "else if(") + cond + ")")
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
                let result = code(ifcode,elseifcode,elsecode)
                for _ in 1..nestedElseCount do
                    c.indentDec()
                    c.codewritein "}"
                result

            /// Renders an expression for C99.
            member this.evalC(c:Aqualis) =
                let comparisonOperand (value:expr) =
                    let rendered = value.evalC c
                    if value.etype = Bt then "(" + rendered + ")" else rendered
                match this.simp with
                |False -> "false"
                |True -> "true"
                |Eq(x,y) -> comparisonOperand x + " == " + comparisonOperand y
                |NEq(x,y) -> comparisonOperand x + " != " + comparisonOperand y
                |Greater(x,y) -> comparisonOperand x + " > " + comparisonOperand y
                |GreaterEq(x,y) -> comparisonOperand x + " >= " + comparisonOperand y
                |Less(x,y) -> comparisonOperand x + " < " + comparisonOperand y
                |LessEq(x,y) -> comparisonOperand x + " <= " + comparisonOperand y
                |AND x when x |> List.skip (min 1 x.Length) |> List.exists (fun v -> v.emitsStatements) -> (expr.shortCircuit true x).evalC c
                |AND x ->
                    x
                    |> List.map (fun v -> match v with |OR _ |AND _ -> "(" + v.evalC c + ")" |_ -> v.evalC c)
                    |> fun lst -> String.Join(" && ", lst)
                |OR x when x |> List.skip (min 1 x.Length) |> List.exists (fun v -> v.emitsStatements) -> (expr.shortCircuit false x).evalC c
                |OR x ->
                    x
                    |> List.map (fun v -> match v with |OR _ |AND _ -> "(" + v.evalC c + ")" |_ -> v.evalC c)
                    |> fun lst -> String.Join(" || ", lst)
                |Int x when x = Int32.MinValue -> "(-2147483647-1)"
                |Int x -> c.numFormat.ItoS x
                |Dbl x -> c.numFormat.DtoS x
                |Cpx (0.0,1.0) -> "uj"
                |Cpx (re,im) when not (Double.IsFinite re && Double.IsFinite im) ->
                    "aqualis_complex_literal(" + c.numFormat.DtoS re + "," + c.numFormat.DtoS im + ")"
                |Cpx (re,im) -> "(" + c.numFormat.DtoS re + "+uj*" + c.numFormat.DtoS im + ")"
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
                    |_,(Add _|Sub _|Div _|Mod _) -> x.evalC c + "*(" + y.evalC c + ")"
                    |_ -> x.evalC c + "*" + y.evalC c
                |Div(Dt,x,y) when x.etype = It 4 && y.etype = It 4 ->
                    (ToDbl x/ToDbl y).evalC c
                |Div(_,x,y) ->
                    match x,y with
                    |(Add _|Sub _),(Add _|Sub _|Mul _|Div _|Mod _) -> "(" + x.evalC c + ")/(" + y.evalC c + ")"
                    |(Add _|Sub _),_ -> "(" + x.evalC c + ")/" + y.evalC c
                    |_,(Add _|Sub _|Mul _|Div _|Mod _) -> x.evalC c + "/(" + y.evalC c + ")"
                    |_ -> x.evalC c + "/" + y.evalC c
                |Mod(_,x,y) ->
                    let left =
                        match x with
                        |Add _|Sub _ -> "(" + x.evalC c + ")"
                        |_ -> x.evalC c
                    let right =
                        match y with
                        |Add _|Sub _|Mul _|Div _|Mod _ -> "(" + y.evalC c + ")"
                        |_ -> y.evalC c
                    left + "%" + right
                |Pow(Zt,x,y) -> "cpow(" + x.evalC c + "," + y.evalC c + ")"
                |Pow(It _,x,y) -> "aqualis_int_power(" + x.evalC c + "," + y.evalC c + ")"
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
                |Log10(Zt,x) -> "(clog(" + x.evalC c + ")/log(10.0))"
                |Log10(_,x) -> "log10(" + x.evalC c + ")"
                |Sqrt(Zt,x) -> "csqrt(" + x.evalC c + ")"
                |Sqrt(_,x) -> "sqrt(" + x.evalC c + ")"
                |ToInt x ->
                    match x with
                    |Add _|Sub _ |Mul _ |Div _ |Mod _ ->
                        "(int)(" + x.evalC c + ")"
                    |_ ->
                        "(int)" + x.evalC c
                |ToDbl x ->
                    match x with
                    |Add _|Sub _ |Mul _ |Div _ |Mod _ ->
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
                    UnsupportedOperation.codeGeneration "C99" "two-dimensional array indexing"
                |Idx3 (_,name,i,j,k) ->
                    UnsupportedOperation.codeGeneration "C99" "three-dimensional array indexing"
                |Let (t,y,x,f) ->
                    // let x =
                    //     match t with
                    //     |It 4 -> Var (t, (fun (a,_) -> a) (c.i0.getVar()), y)
                    //     |Dt   -> Var (t, (fun (a,_) -> a) (c.d0.getVar()), y)
                    //     |Zt   -> Var (t, (fun (a,_) -> a) (c.z0.getVar()), y)
                    //     |_    -> NaN
                    // match y with
                    // |NaN -> ()
                    // |_ -> expr.substC x y c
                    (f x).evalC c
                |Sum(t, n1, n2, f) ->
                    let v =
                        match t with
                        |It 4 -> Var (t, (fun (a,_) -> a) (c.i0.getVar()), NaN)
                        |Dt   -> Var (t, (fun (a,_) -> a) (c.d0.getVar()), NaN)
                        |Zt   -> Var (t, (fun (a,_) -> a) (c.z0.getVar()), NaN)
                        |_    -> NaN
                    expr.substC v (Int 0) c
                    // 合計値格納用変数
                    (Let(t, Int 0, v, fun u ->
                        expr.forLoopC c (n1,n2) <| fun i ->
                            // 加算・代入処理
                            expr.substC u (Add(t,u, f i)) c
                        u)).evalC c
                |IfEl(cond,n1,n2) ->
                    let v =
                        match n1.etype%%n2.etype with
                        |It 4 -> Var (It 4, (fun (a,_) -> a) (c.i0.getVar()), NaN)
                        |Dt   -> Var (Dt, (fun (a,_) -> a) (c.d0.getVar()), NaN)
                        |Zt   -> Var (Zt, (fun (a,_) -> a) (c.z0.getVar()), NaN)
                        |_    -> NaN
                    (Let(n1.etype%%n2.etype, NaN, v, fun x ->
                        expr.branchC c <| fun (ifcode,_,elsecode) ->
                            ifcode cond <| fun () ->
                                expr.substC x n1 c
                            elsecode <| fun () ->
                                expr.substC x n2 c
                        x)).evalC c
                |NaN -> "NaN"
