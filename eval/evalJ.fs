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

        let private notSupported operation : 'T =
            raise (
                NotSupportedException(
                    $"JavaScript code generation does not support {operation}."))

        type expr with

            static member substJ (x:expr) (y:expr) (c:program) =
                c.codewritein (x.evalJ c  + " = " + y.evalJ c + ";")

            static member equivJ (_:expr) (_:expr) (_:program) =
                notSupported "equation display"

            static member equivAlignJ (_:expr) (_:expr) (_:program) =
                notSupported "aligned equation display"

            static member forLoopJ (context:GenerationContext) (n1:expr,n2:expr) code =
                let c = context.CurrentProgram
                let iname,returnVar = c.i0.getVar()
                let i = Var(It 4, iname, NaN)
                let n1_ = n1.evalJ c
                let n2_ = n2.evalJ c
                if context.IsParallelMode then c.varPrivate.setVar(It 4,A0,iname,"")
                c.codewritein("for(" + i.evalJ c + " = " + n1_ + "; " + i.evalJ c + " <= " + n2_ + "; " + i.evalJ c + "++)")
                c.codewritein "{"
                c.indentInc()
                code i
                c.indentDec()
                c.codewritein "}"
                returnVar()

            ///<summary>無限ループ</summary>
            static member loopJ (context:GenerationContext) code =
                let c = context.CurrentProgram
                let iname,returnVar = c.i0.getVar()
                let i = Var(It 4, iname, NaN)
                let exit() = c.codewritein "break;"
                expr.substJ i (Int 1) c
                if context.IsParallelMode then c.varPrivate.setVar(It 4,A0,iname,"")
                c.codewritein "for(;;)"
                c.codewritein "{"
                c.indentInc()
                code(exit,i)
                expr.substJ i (Add(It 4, i, Int 1)) c
                c.indentDec()
                c.codewritein "}"
                returnVar()

            ///<summary>条件を満たす間ループ</summary>
            static member whiledoJ (context:GenerationContext) (cond:expr) = fun code ->
                let c = context.CurrentProgram
                c.codewritein("while(" + cond.evalJ c + ")")
                c.codewritein "{"
                c.indentInc()
                code()
                c.indentDec()
                c.codewritein "}"

            ///<summary>指定した範囲でループ</summary>
            static member rangeJ (context:GenerationContext) (counter:option<string>) (i1:expr) = fun (i2:expr) -> fun code ->
                let c = context.CurrentProgram
                match i1.simp,i2.simp with
                |Int a, Int b when a>b ->
                    let iname,returnVar = match counter with |None -> c.i0.getVar() |Some s -> c.i0.getVar (s,It 4,A0)
                    let i = Var(It 4, iname, NaN)
                    if context.IsParallelMode then c.varPrivate.setVar(It 4,A0,iname,"")
                    c.comment("for(" + i.evalJ c + "=" + i1.evalJ c + "; " + i.evalJ c + "<=" + i2.evalJ c + "; " + i.evalJ c + "++)")
                    c.comment "{"
                    c.indentInc()
                    code i
                    c.indentDec()
                    c.comment "}"
                    returnVar()
                |i1,i2 ->
                    let iname,returnVar = match counter with |None -> c.i0.getVar() |Some s -> c.i0.getVar (s,It 4,A0)
                    let i = Var(It 4, iname, NaN)
                    if context.IsParallelMode then c.varPrivate.setVar(It 4,A0,iname,"")
                    c.codewritein("for(" + i.evalJ c + "=" + i1.evalJ c + "; " + i.evalJ c + "<=" + i2.evalJ c + "; " + i.evalJ c + "++)")
                    c.codewritein "{"
                    c.indentInc()
                    code i
                    c.indentDec()
                    c.codewritein "}"
                    returnVar()

            ///<summary>指定した範囲でループ(途中脱出可)</summary>
            static member range_exitJ (context:GenerationContext) (counter:option<string>) (i1:expr) = fun (i2:expr) -> fun code ->
                let c = context.CurrentProgram
                match i1.simp,i2.simp with
                |Int a, Int b when a>b ->
                    let iname,returnVar = match counter with |None -> c.i0.getVar() |Some s -> c.i0.getVar (s,It 4,A0)
                    let i = Var(It 4, iname, NaN)
                    let exit() = c.comment "break;"
                    if context.IsParallelMode then c.varPrivate.setVar(It 4,A0,iname,"")
                    c.comment("for(" + i.evalJ c + "=" + i1.evalJ c + "; " + i.evalJ c + "<=" + i2.evalJ c + "; " + i.evalJ c + "++)")
                    c.comment "{"
                    c.indentInc()
                    code(exit,i)
                    c.indentDec()
                    c.comment "}"
                    returnVar()
                |i1,i2 ->
                    let iname,returnVar = match counter with |None -> c.i0.getVar() |Some s -> c.i0.getVar (s,It 4,A0)
                    let i = Var(It 4, iname, NaN)
                    let exit() = c.codewritein "break;"
                    if context.IsParallelMode then c.varPrivate.setVar(It 4,A0,iname,"")
                    c.codewritein("for(" + i.evalJ c + "=" + i1.evalJ c + "; " + i.evalJ c + "<=" + i2.evalJ c + "; " + i.evalJ c + "++)")
                    c.codewritein "{"
                    c.indentInc()
                    code(exit,i)
                    c.indentDec()
                    c.codewritein "}"
                    returnVar()

            static member branchJ (context:GenerationContext) code =
                let c = context.CurrentProgram
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
                |Eq(x,y)
                |NEq(x,y)
                |Greater(x,y)
                |GreaterEq(x,y)
                |Less(x,y)
                |LessEq(x,y) when x.etype = Zt || y.etype = Zt ->
                    notSupported "complex-number comparisons"
                |Eq(x,y) -> x.evalJ c + " == " + y.evalJ c
                |NEq(x,y) -> x.evalJ c + " != " + y.evalJ c
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
                |Cpx _ -> notSupported "complex-number literals"
                |Var (Zt,_,_) -> notSupported "complex-number values"
                |Var (_,s,x) -> s
                |Inv(Zt,_)
                |Add(Zt,_,_)
                |Sub(Zt,_,_)
                |Mul(Zt,_,_)
                |Div(Zt,_,_)
                |Mod(Zt,_,_)
                |Pow(Zt,_,_) ->
                    notSupported "complex-number arithmetic"
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
                    notSupported "complex-number functions"
                |Abs(_,x) when x.etype = Zt ->
                    notSupported "the absolute-value operation for complex numbers"
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
                |ToInt x -> "Math.trunc(" + x.evalJ c + ")"
                |ToDbl x -> "Number(" + x.evalJ c + ")"
                |Floor x -> "Math.floor(" + x.evalJ c + ")"
                |Ceil x -> "Math.ceil(" + x.evalJ c + ")"
                |Re _ -> notSupported "the real-part operation (Re)"
                |Im _ -> notSupported "the imaginary-part operation (Im)"
                |Conj _ -> notSupported "the complex-conjugate operation (Conj)"
                |Idx1 (Zt,_,_) -> notSupported "complex-number array values"
                |Idx1 (_,name,i) -> name + "[" + i.evalJ c + "]"
                |Idx2 _ -> notSupported "two-dimensional array indexing"
                |Idx3 _ -> notSupported "three-dimensional array indexing"
                |Let (Zt,_,_)
                |Sum(Zt,_,_,_) ->
                    notSupported "complex-number expressions"
                |IfEl(_,n1,n2) when n1.etype = Zt || n2.etype = Zt ->
                    notSupported "complex-number expressions"
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
                        expr.forLoopJ (GenerationContext.ForInternalProgram c) (n1,n2) <| fun i ->
                            // 加算・代入処理
                            expr.substJ u (Add(t,u, f i)) c
                        u)).evalJ c
                |IfEl(cond,n1,n2) ->
                    (Let(n1.etype, NaN, fun x ->
                        expr.branchJ (GenerationContext.ForInternalProgram c) <| fun (ifcode,_,elsecode) ->
                            ifcode cond <| fun () ->
                                expr.substJ x n1 c
                            elsecode <| fun () ->
                                expr.substJ x n2 c
                        x)).evalJ c
                |NaN -> "NaN"
