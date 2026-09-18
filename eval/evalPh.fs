//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    /// Expression output operations for PHP.
    [<AutoOpen>]
    module exprEvalPh =

        open System

        type expr with

            /// Emits an assignment for PHP.
            static member substPh (x:expr) (y:expr) (c:Aqualis) =
                let target =
                    match x with
                    |Var(_,name,_) -> name
                    |_ -> x.evalPh c
                c.codewritein ("<?php ", target + " = " + y.evalPh c + "; ?>")

            /// Reports that equation display is unsupported for this target language.
            static member equivPh (x:expr) (y:expr) (c:Aqualis) =
                UnsupportedOperation.codeGeneration "PHP" "equation display"

            /// Reports that aligned equation display is unsupported for this target language.
            static member equivAlignPh (x:expr) (y:expr) (c:Aqualis) =
                UnsupportedOperation.codeGeneration "PHP" "aligned equation display"

            /// Emits a counted loop for PHP.
            static member forLoopPh (c:Aqualis) (n1:expr,n2:expr) code =
                let iname,returnVar = c.i0.getVar()
                let i = Var(It 4, iname, NaN)
                let n1_ = n1.evalPh c
                let n2_ = n2.evalPh c
                if c.ParallelMode then c.varPrivate.setVar(It 4,A0,iname,"")
                c.codewritein("<?php ", "for(" + i.evalPh c + " = " + n1_ + "; " + i.evalPh c + " <= " + n2_ + "; " + i.evalPh c + "++): ?>")
                c.indentInc()
                code i
                c.indentDec()
                c.codewritein("<?php ", "endfor; ?>")
                returnVar()

            /// Emits an unbounded loop for PHP.
            static member loopPh (c:Aqualis) code =
                let iname,returnVar = c.i0.getVar()
                let i = Var(It 4, iname, NaN)
                let label = "_" + c.GotoLabels.nextGotoLabel()
                let exit() = c.codewritein("<?php ", "goto "+label+"; ?>")
                expr.substPh i (Int 1) c
                if c.ParallelMode then c.varPrivate.setVar(It 4,A0,iname,"")
                c.codewritein("<?php ", "for(;;): ?>")
                c.indentInc()
                code(exit,i)
                expr.substPh i (Add(It 4, i, Int 1)) c
                c.indentDec()
                c.codewritein("<?php ", "endfor; ?>")
                c.codewritein("<?php ", label+":; ?>")
                returnVar()

            /// Emits a conditional loop for PHP.
            static member whiledoPh (c:Aqualis) (cond:expr) = fun code ->
                c.codewritein("<?php ", "while(" + cond.evalPh c + "): ?>")
                c.indentInc()
                code()
                c.indentDec()
                c.codewritein("<?php ", "endwhile; ?>")

            /// Emits an inclusive range loop for PHP.
            static member rangePh (c:Aqualis) (counter:option<string>) (i1:expr) = fun (i2:expr) -> fun code ->
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
                    c.codewritein("<?php ", "for(" + i.evalPh c + "=" + i1.evalPh c + "; " + i.evalPh c + "<=" + i2.evalPh c + "; " + i.evalPh c + "++): ?>")
                    c.indentInc()
                    code i
                    c.indentDec()
                    c.codewritein("<?php ", "endfor; ?>")
                    returnVar()

            /// Emits an inclusive range loop with an exit action for PHP.
            static member range_exitPh (c:Aqualis) (counter:option<string>) (i1:expr) = fun (i2:expr) -> fun code ->
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
                    let exit() = c.codewritein("<?php ", "goto "+label+"; ?>")
                    if c.ParallelMode then c.varPrivate.setVar(It 4,A0,iname,"")
                    c.codewritein("<?php ", "for(" + i.evalPh c + "=" + i1.evalPh c + "; " + i.evalPh c + "<=" + i2.evalPh c + "; " + i.evalPh c + "++): ?>")
                    c.indentInc()
                    code(exit,i)
                    c.indentDec()
                    c.codewritein("<?php ", "endfor; ?>")
                    c.codewritein("<?php ", label+":; ?>")
                    returnVar()

            /// Emits a branch callback for PHP.
            static member branchPh (c:Aqualis) code =
                let ifcode (cond:expr) code =
                    let cond = cond.evalPh c
                    c.codewritein ("<?php ", "if(" + cond + "): ?>")
                    c.indentInc()
                    code()
                    c.indentDec()
                let elseifcode (cond:expr) code =
                    let cond = cond.evalPh c
                    c.codewritein ("<?php ", "elseif(" + cond + "): ?>")
                    c.indentInc()
                    code()
                    c.indentDec()
                let elsecode code =
                    c.codewritein("<?php ", "else: ?>")
                    c.indentInc()
                    code()
                    c.indentDec()
                code(ifcode,elseifcode,elsecode)
                c.codewritein("<?php ", "endif; ?>")

            /// Renders an expression for PHP.
            member this.evalPh(c:Aqualis) =
                match this.simp with
                |False -> "false"
                |True -> "true"
                |Eq(x,y)
                |NEq(x,y)
                |Greater(x,y)
                |GreaterEq(x,y)
                |Less(x,y)
                |LessEq(x,y) when x.etype = Zt || y.etype = Zt ->
                    UnsupportedOperation.codeGeneration "PHP" "complex-number comparisons"
                |Eq(x,y) -> x.evalPh c + " == " + y.evalPh c
                |NEq(x,y) -> x.evalPh c + " != " + y.evalPh c
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
                |Cpx _ -> UnsupportedOperation.codeGeneration "PHP" "complex-number literals"
                |Var (Zt,_,_) -> UnsupportedOperation.codeGeneration "PHP" "complex-number values"
                |Var (_,s,x) -> s
                |Inv(Zt,_)
                |Add(Zt,_,_)
                |Sub(Zt,_,_)
                |Mul(Zt,_,_)
                |Div(Zt,_,_)
                |Mod(Zt,_,_)
                |Pow(Zt,_,_) ->
                    UnsupportedOperation.codeGeneration "PHP" "complex-number arithmetic"
                |Exp(Zt,_)
                |Sin(Zt,_)
                |Cos(Zt,_)
                |Tan(Zt,_)
                |Asin(Zt,_)
                |Acos(Zt,_)
                |Atan(Zt,_)
                |Log(Zt,_)
                |Log10(Zt,_)
                |Sqrt(Zt,_) ->
                    UnsupportedOperation.codeGeneration "PHP" "complex-number functions"
                |Abs(_,x) when x.etype = Zt ->
                    UnsupportedOperation.codeGeneration "PHP" "the absolute-value operation for complex numbers"
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
                    |_,(Add _|Sub _|Div _|Mod _) -> x.evalPh c + "*(" + y.evalPh c + ")"
                    |_ -> x.evalPh c + "*" + y.evalPh c
                |Div(It 4,x,y) when x.etype = It 4 && y.etype = It 4 ->
                    "intdiv(" + x.evalPh c + ", " + y.evalPh c + ")"
                |Div(_,x,y) ->
                    match x,y with
                    |(Add _|Sub _),(Add _|Sub _|Mul _|Div _|Mod _) -> "(" + x.evalPh c + ")/(" + y.evalPh c + ")"
                    |(Add _|Sub _),_ -> "(" + x.evalPh c + ")/" + y.evalPh c
                    |_,(Add _|Sub _|Mul _|Div _|Mod _) -> x.evalPh c + "/(" + y.evalPh c + ")"
                    |_ -> x.evalPh c + "/" + y.evalPh c
                |Mod(_,x,y) ->
                    let left =
                        match x with
                        |Add _|Sub _ -> "(" + x.evalPh c + ")"
                        |_ -> x.evalPh c
                    let right =
                        match y with
                        |Add _|Sub _|Mul _|Div _|Mod _ -> "(" + y.evalPh c + ")"
                        |_ -> y.evalPh c
                    left + "%" + right
                |Pow(_,x,y) -> "pow(" + x.evalPh c + "," + y.evalPh c + ")"
                |Exp(_,x) -> "exp(" + x.evalPh c + ")"
                |Sin(_,x) -> "sin(" + x.evalPh c + ")"
                |Cos(_,x) -> "cos(" + x.evalPh c + ")"
                |Tan(_,x) -> "tan(" + x.evalPh c + ")"
                |Asin(_,x) -> "asin(" + x.evalPh c + ")"
                |Acos(_,x) -> "acos(" + x.evalPh c + ")"
                |Atan(_,x) -> "atan(" + x.evalPh c + ")"
                |Atan2(x,y) -> "atan2(" + x.evalPh c + "," + y.evalPh c + ")"
                |Abs(_,x) -> "abs(" + x.evalPh c + ")"
                |Log(_,x) -> "log(" + x.evalPh c + ")"
                |Log10(_,x) -> "log10(" + x.evalPh c + ")"
                |Sqrt(_,x) -> "sqrt(" + x.evalPh c + ")"
                |ToInt x ->
                    match x with
                    |Add _|Sub _ |Mul _ |Div _ |Mod _ ->
                        "(int)(" + x.evalPh c + ")"
                    |_ ->
                        "(int)" + x.evalPh c
                |ToDbl x ->
                    match x with
                    |Add _|Sub _ |Mul _ |Div _ |Mod _ ->
                        "(float)(" + x.evalPh c + ")"
                    |_ ->
                        "(float)" + x.evalPh c
                |Floor x -> "floor(" + x.evalPh c + ")"
                |Ceil x -> "ceil(" + x.evalPh c + ")"
                |Re _ -> UnsupportedOperation.codeGeneration "PHP" "the real-part operation (Re)"
                |Im _ -> UnsupportedOperation.codeGeneration "PHP" "the imaginary-part operation (Im)"
                |Conj _ -> UnsupportedOperation.codeGeneration "PHP" "the complex-conjugate operation (Conj)"
                |Idx1 (Zt,_,_) -> UnsupportedOperation.codeGeneration "PHP" "complex-number array values"
                |Idx1 (_,name,i) -> name + "[" + i.evalPh c + "]"
                |Idx2 (_,name,i,j) ->
                    UnsupportedOperation.codeGeneration "PHP" "two-dimensional array indexing"
                |Idx3 (_,name,i,j,k) ->
                    UnsupportedOperation.codeGeneration "PHP" "three-dimensional array indexing"
                |Let (Zt,_,_,_)
                |Sum(Zt,_,_,_) ->
                    UnsupportedOperation.codeGeneration "PHP" "complex-number expressions"
                |IfEl(_,n1,n2) when n1.etype = Zt || n2.etype = Zt ->
                    UnsupportedOperation.codeGeneration "PHP" "complex-number expressions"
                |Let (t,y,x,f) ->
                    // let x =
                    //     match t with
                    //     |It 4 -> Var (t, (fun (a,_) -> a) (c.i0.getVar()), y)
                    //     |Dt   -> Var (t, (fun (a,_) -> a) (c.d0.getVar()), y)
                    //     |Zt   -> Var (t, (fun (a,_) -> a) (c.z0.getVar()), y)
                    //     |_    -> NaN
                    // match y with
                    // |NaN -> ()
                    // |_ -> expr.substPh x y c
                    (f x).evalPh c
                |Sum(t, n1, n2, f) ->
                    let v =
                        match t with
                        |It 4 -> Var (t, (fun (a,_) -> a) (c.i0.getVar()), NaN)
                        |Dt   -> Var (t, (fun (a,_) -> a) (c.d0.getVar()), NaN)
                        |Zt   -> Var (t, (fun (a,_) -> a) (c.z0.getVar()), NaN)
                        |_    -> NaN
                    expr.substPh v (Int 0) c
                    // 合計値格納用変数
                    (Let(t, Int 0, v, fun u ->
                        expr.forLoopPh c (n1,n2) <| fun i ->
                            // 加算・代入処理
                            expr.substPh u (Add(t,u, f i)) c
                        u)).evalPh c
                |IfEl(cond,n1,n2) ->
                    let v =
                        match n1.etype with
                        |It 4 -> Var (It 4, (fun (a,_) -> a) (c.i0.getVar()), NaN)
                        |Dt   -> Var (Dt, (fun (a,_) -> a) (c.d0.getVar()), NaN)
                        |Zt   -> Var (Zt, (fun (a,_) -> a) (c.z0.getVar()), NaN)
                        |_    -> NaN
                    (Let(n1.etype, NaN, v, fun x ->
                        expr.branchPh c <| fun (ifcode,_,elsecode) ->
                            ifcode cond <| fun () ->
                                expr.substPh x n1 c
                            elsecode <| fun () ->
                                expr.substPh x n2 c
                        x)).evalPh c
                |NaN -> "NaN"
