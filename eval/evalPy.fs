// 
// Copyright (c) 2026 Jun-ichiro Sugisaka
// 
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
// 
namespace Aqualis
    
    /// Expression output operations for Python.
    [<AutoOpen>]
    module exprEvalPy =
        
        open System

        /// Computes a quotient truncated toward zero.
        let private truncatingQuotient dividend divisor =
            let magnitude =
                "abs(" + dividend + ") // abs(" + divisor + ")"
            let sameSign =
                "(" + dividend + " < 0) == (" + divisor + " < 0)"
            "(" + magnitude + " if " + sameSign + " else -(" + magnitude + "))"

        /// Computes integer division truncated toward zero.
        let private truncatingIntegerDivision dividend divisor =
            let dividendName = "_aqualis_dividend"
            let divisorName = "_aqualis_divisor"
            "(lambda " + dividendName + ", " + divisorName + ": " +
            truncatingQuotient dividendName divisorName + ")(" +
            dividend + ", " + divisor + ")"

        /// Computes the matching integer remainder.
        let private truncatingIntegerRemainder dividend divisor =
            let dividendName = "_aqualis_dividend"
            let divisorName = "_aqualis_divisor"
            "(lambda " + dividendName + ", " + divisorName + ": " +
            dividendName + " - " + truncatingQuotient dividendName divisorName +
            " * " + divisorName + ")(" + dividend + ", " + divisor + ")"

        /// Writes a Python suite with an indented callback body.
        let private writeSuite (c:Aqualis) (code:unit -> unit) =
            let emitted,_ = c.captureCode code
            if String.IsNullOrWhiteSpace emitted then c.codewritein "pass"
            else c.codewriten emitted
        
        type expr with
            
            /// Emits an assignment for Python.
            static member substPy (x:expr) (y:expr) (c:Aqualis) =
                c.codewritein (x.evalPy c  + " = " + y.evalPy c)
                
            /// Reports that equation display is unsupported for this target language.
            static member equivPy (x:expr) (y:expr) (c:Aqualis) =
                UnsupportedOperation.codeGeneration "Python" "equation display"

            /// Reports that aligned equation display is unsupported for this target language.
            static member equivAlignPy (x:expr) (y:expr) (c:Aqualis) =
                UnsupportedOperation.codeGeneration "Python" "aligned equation display"
                
            /// Emits a counted loop for Python.
            static member forLoopPy (c:Aqualis) (n1:expr,n2:expr) code =
                let iname,returnVar = c.i0.getVar()
                let i = Var(It 4, iname, NaN)
                let n1_ = n1.evalPy c
                let n2_ = (Add(It 4, n2, Int 1)).evalPy c
                c.codewritein("for " + i.evalPy c + " in range(" + n1_ + ", " + n2_ + ", 1):")
                c.indentInc()
                writeSuite c (fun () -> code i)
                c.indentDec()
                returnVar()
                
            /// Emits an unbounded loop for Python.
            static member loopPy (c:Aqualis) code =
                let iname,returnVar = c.i0.getVar()
                let i = Var(It 4, iname, NaN)
                let exitType = "_AqualisLoopExit" + c.GotoLabels.nextGotoLabel()
                let exit() = c.codewritein("raise " + exitType + "()")
                c.codewritein("class " + exitType + "(Exception):")
                c.indentInc()
                c.codewritein "pass"
                c.indentDec()
                expr.substPy i (Int 1) c
                c.codewritein "try:"
                c.indentInc()
                c.codewritein "while True:"
                c.indentInc()
                code(exit,i)
                expr.substPy i (Add(It 4, i, Int 1)) c
                c.indentDec()
                c.indentDec()
                c.codewritein("except " + exitType + ":")
                c.indentInc()
                c.codewritein "pass"
                c.indentDec()
                returnVar()
                
            /// Emits a conditional loop for Python.
            static member whiledoPy (c:Aqualis) (cond:expr) = fun code ->
                c.codewritein("while(" + cond.evalPy c + "):")
                c.indentInc()
                writeSuite c code
                c.indentDec()
                
            /// Emits an inclusive range loop for Python.
            static member rangePy (c:Aqualis) (counter:option<string>) (i1:expr) = fun (i2:expr) -> fun code ->
                match i1.simp,i2.simp with
                |Int a, Int b when a>b -> 
                    let iname,returnVar = match counter with |None -> c.i0.getVar() |Some s -> c.i0.getVar (s,It 4,A0)
                    let i = Var(It 4, iname, NaN)
                    c.captureCode(fun () -> code i) |> ignore
                    returnVar()
                |_ ->
                    let iname,returnVar = match counter with |None -> c.i0.getVar() |Some s -> c.i0.getVar (s,It 4,A0)
                    let i = Var(It 4, iname, NaN)
                    c.codewritein("for " + i.evalPy c + " in range("+i1.evalPy c + ", " + (Add(It 4,i2,Int 1)).evalPy c + ", 1):")
                    c.indentInc()
                    writeSuite c (fun () -> code i)
                    c.indentDec()
                    returnVar()
                    
            /// Emits an inclusive range loop with an exit action for Python.
            static member range_exitPy (c:Aqualis) (counter:option<string>) (i1:expr) = fun (i2:expr) -> fun code ->
                match i1.simp,i2.simp with
                |Int a, Int b when a>b -> 
                    let iname,returnVar = match counter with |None -> c.i0.getVar() |Some s -> c.i0.getVar (s,It 4,A0)
                    let i = Var(It 4, iname, NaN)
                    let exit() = ()
                    c.captureCode(fun () -> code(exit,i)) |> ignore
                    returnVar()
                |_ ->
                    let iname,returnVar = match counter with |None -> c.i0.getVar() |Some s -> c.i0.getVar (s,It 4,A0)
                    let i = Var(It 4, iname, NaN)
                    let exitType = "_AqualisLoopExit" + c.GotoLabels.nextGotoLabel()
                    let exit() = c.codewritein("raise " + exitType + "()")
                    c.codewritein("class " + exitType + "(Exception):")
                    c.indentInc()
                    c.codewritein "pass"
                    c.indentDec()
                    c.codewritein "try:"
                    c.indentInc()
                    c.codewritein("for " + i.evalPy c + " in range(" + i1.evalPy c + ", " + (Add(It 4,i2,Int 1)).evalPy c + ", 1):")
                    c.indentInc()
                    writeSuite c (fun () -> code(exit,i))
                    c.indentDec()
                    c.indentDec()
                    c.codewritein("except " + exitType + ":")
                    c.indentInc()
                    c.codewritein "pass"
                    c.indentDec()
                    returnVar()
                    
            /// Emits a branch callback for Python.
            static member branchPy (c:Aqualis) code =
                let ifcode (cond:expr) code =
                    let cond = cond.evalPy c
                    c.codewritein("if " + cond + ":")
                    c.indentInc()
                    writeSuite c code
                    c.indentDec()
                let elseifcode (cond:expr) code =
                    let cond = cond.evalPy c
                    c.codewritein("elif " + cond + ":")
                    c.indentInc()
                    writeSuite c code
                    c.indentDec()
                let elsecode code =
                    c.codewritein "else:"
                    c.indentInc()
                    writeSuite c code
                    c.indentDec()
                code(ifcode,elseifcode,elsecode)
                
            /// Renders an expression for Python.
            member this.evalPy(c:Aqualis) =
                match this.simp with
                |False -> "False"
                |True -> "True"
                |Eq(x,y) -> x.evalPy c + " == " + y.evalPy c
                |NEq(x,y) -> x.evalPy c + " != " + y.evalPy c
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
                |Cpx (0.0,1.0) -> "1j"
                |Cpx (re,im) when not (Double.IsFinite re && Double.IsFinite im) ->
                    "complex(" + c.numFormat.DtoS re + "," + c.numFormat.DtoS im + ")"
                |Cpx (re,im) -> "(" + c.numFormat.DtoS re + "+1j*" + c.numFormat.DtoS im + ")"
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
                |Div(It _,x,y) ->
                    truncatingIntegerDivision (x.evalPy c) (y.evalPy c)
                |Div(_,x,y) ->
                    match x,y with
                    |(Add _|Sub _),(Add _|Sub _|Mul _|Div _) -> "(" + x.evalPy c + ")/(" + y.evalPy c + ")"
                    |(Add _|Sub _),_ -> "(" + x.evalPy c + ")/" + y.evalPy c
                    |_,(Add _|Sub _|Mul _|Div _) -> x.evalPy c + "/(" + y.evalPy c + ")"
                    |_ -> x.evalPy c + "/" + y.evalPy c
                |Mod(_,x,y) ->
                    truncatingIntegerRemainder (x.evalPy c) (y.evalPy c)
                |Pow(t,x,y) ->
                    let baseValue =
                        if t=Zt && x.etype<>Zt then
                            "complex(" + x.evalPy c + ")"
                        else
                            match x with
                            |Add _|Sub _|Mul _|Div _|Inv _ -> "(" + x.evalPy c + ")"
                            |_ -> x.evalPy c
                    let exponentValue =
                        match y with
                        |Add _|Sub _|Mul _|Div _|Inv _ -> "(" + y.evalPy c + ")"
                        |Int value when value < 0 -> "(" + y.evalPy c + ")"
                        |Dbl value when value < 0.0 -> "(" + y.evalPy c + ")"
                        |_ -> y.evalPy c
                    if t=Dt then "numpy.power(float(" + x.evalPy c + ")," + y.evalPy c + ")"
                    else baseValue + "**" + exponentValue
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
                |Conj x -> "(" + x.evalPy c + ").conjugate()"
                |Idx1 (_,name,i) -> name + "[" + i.evalPy c + "]"
                |Idx2 (_,name,i,j) -> name + "[" + i.evalPy c + "," + j.evalPy c + "]"
                |Idx3 (_,name,i,j,k) -> name + "[" + i.evalPy c + "," + j.evalPy c + "," + k.evalPy c + "]"
                |Let (t,y,x,f) -> 
                    // let x =
                    //     match t with
                    //     |It 4 -> Var (t, (fun (a,_) -> a) (c.i0.getVar()), y)
                    //     |Dt   -> Var (t, (fun (a,_) -> a) (c.d0.getVar()), y)
                    //     |Zt   -> Var (t, (fun (a,_) -> a) (c.z0.getVar()), y)
                    //     |_    -> NaN
                    // match y with
                    // |NaN -> ()
                    // |_ -> expr.substPy x y c
                    (f x).evalPy c
                |Sum(t, n1, n2, f) ->
                    let v =
                        match t with
                        |It 4 -> Var (t, (fun (a,_) -> a) (c.i0.getVar()), NaN)
                        |Dt   -> Var (t, (fun (a,_) -> a) (c.d0.getVar()), NaN)
                        |Zt   -> Var (t, (fun (a,_) -> a) (c.z0.getVar()), NaN)
                        |_    -> NaN
                    expr.substPy v (Int 0) c
                    // 合計値格納用変数
                    (Let(t, Int 0, v, fun u ->
                        expr.forLoopPy c (n1,n2) <| fun i ->
                            // 加算・代入処理
                            expr.substPy u (Add(t,u, f i)) c
                        u)).evalPy c
                |IfEl(cond,n1,n2) -> 
                    let v =
                        match n1.etype with
                        |It 4 -> Var (It 4, (fun (a,_) -> a) (c.i0.getVar()), NaN)
                        |Dt   -> Var (Dt, (fun (a,_) -> a) (c.d0.getVar()), NaN)
                        |Zt   -> Var (Zt, (fun (a,_) -> a) (c.z0.getVar()), NaN)
                        |_    -> NaN
                    (Let(n1.etype, NaN, v, fun x -> 
                        expr.branchPy c <| fun (ifcode,_,elsecode) ->
                            ifcode cond <| fun () ->
                                expr.substPy x n1 c
                            elsecode <| fun () ->
                                expr.substPy x n2 c
                        x)).evalPy c
                |NaN -> "NaN"
