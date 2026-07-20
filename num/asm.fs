//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    open System

    ///<summary>数学関数</summary>
    type asm =
        ///<summary>虚数単位</summary>
        static member uj with get() =
            if GenerationContext.TryCurrent.IsNone then
                complex0(Cpx(0.0,1.0))
            else
                match (GenerationScope.currentProgram()).language with
                |Fortran ->
                    (GenerationScope.currentProgram()).var.setUniqVar(Zt,A0,"uj","(0d0,1d0)")
                    complex0(Var(Zt,"uj",NaN))
                |C99 ->
                    //#defineで定義済み
                    complex0(Var(Zt,"uj",NaN))
                |LaTeX ->
                    (GenerationScope.currentProgram()).var.setUniqVar(Zt,A0,"\\mathrm{j}","(0d0,1d0)")
                    complex0(Var(Zt,"\\mathrm{j}",NaN))
                |HTML ->
                    (GenerationScope.currentProgram()).var.setUniqVar(Zt,A0,"\\mathrm{j}","(0d0,1d0)")
                    complex0(Var(Zt,"\\mathrm{j}",NaN))
                |HTMLSequenceDiagram ->
                    (GenerationScope.currentProgram()).var.setUniqVar(Zt,A0,"\\mathrm{j}","(0d0,1d0)")
                    complex0(Var(Zt,"\\mathrm{j}",NaN))
                |Python ->
                    complex0(Var(Zt,"1.0j",NaN))
                |JavaScript ->
                    complex0(Cpx(0.0,1.0))
                |PHP ->
                    complex0(Cpx(0.0,1.0))
                |Numeric ->
                    complex0(Cpx(0.0,1.0))
        ///<summary>円周率</summary>
        static member pi with get() =
            if GenerationContext.TryCurrent.IsNone then
                double0(Dbl Math.PI)
            else
                match (GenerationScope.currentProgram()).language with
                |Fortran ->
                    (GenerationScope.currentProgram()).var.setUniqVar(Dt,A0,"pi","3.14159265358979d0")
                    double0(Var(Dt,"pi",NaN))
                |C99 ->
                    (GenerationScope.currentProgram()).var.setUniqVar(Dt,A0,"pi","3.14159265358979")
                    double0(Var(Dt,"pi",NaN))
                |LaTeX ->
                    (GenerationScope.currentProgram()).var.setUniqVar(Dt,A0,"\\pi","3.14159265358979")
                    double0(Var(Dt,"\\pi",NaN))
                |HTML ->
                    (GenerationScope.currentProgram()).var.setUniqVar(Dt,A0,"\\pi","3.14159265358979")
                    double0(Var(Dt,"\\pi",NaN))
                |HTMLSequenceDiagram ->
                    (GenerationScope.currentProgram()).var.setUniqVar(Dt,A0,"\\pi","3.14159265358979")
                    double0(Var(Dt,"\\pi",NaN))
                |Python ->
                    (GenerationScope.currentProgram()).var.setUniqVar(Dt,A0,"pi","3.14159265358979")
                    double0(Var(Dt,"pi",NaN))
                |JavaScript ->
                    double0(Dbl Math.PI)
                |PHP ->
                    (GenerationScope.currentProgram()).var.setUniqVar(Dt,A0,"pi","3.14159265358979")
                    double0(Var(Dt,"pi",NaN))
                |Numeric ->
                    double0(Dbl Math.PI)
        ///<summary>2πj</summary>
        static member j2p with get() = 2*asm.pi*asm.uj
        static member todouble(x:int0) = double0(ToDbl x.Expr)
        static member toint(x:double0) = int0(ToInt x.Expr)
        ///<summary>累乗</summary>
        static member pow(x:int0, y:int0) = int0(Pow(It 4,x.Expr,y.Expr))
        static member pow(x:int0, y:double0) = double0(Pow(Dt,x.Expr,y.Expr))
        static member pow(x:int0, y:complex0) = complex0(Pow(Zt,x.Expr,y.Expr))
        static member pow(x:double0, y:int0) = double0(Pow(Dt,x.Expr,y.Expr))
        static member pow(x:double0, y:double0) = double0(Pow(Dt,x.Expr,y.Expr))
        static member pow(x:double0, y:complex0) = complex0(Pow(Zt,x.Expr,y.Expr))
        static member pow(x:complex0, y:int0) = double0(Pow(Zt,x.Expr,y.Expr))
        static member pow(x:complex0, y:double0) = double0(Pow(Zt,x.Expr,y.Expr))
        static member pow(x:complex0, y:complex0) = complex0(Pow(Zt,x.Expr,y.Expr))
        static member pow(x:int0, y:int) = asm.pow(x,I y)
        static member pow(x:double0, y:int) = asm.pow(x,I y)
        static member pow(x:complex0, y:int) = asm.pow(x,I y)
        static member pow(x:int0, y:double) = asm.pow(x,D y)
        static member pow(x:double0, y:double) = asm.pow(x,D y)
        static member pow(x:complex0, y:double) = asm.pow(x,D y)
        static member pow(x:int, y:int0) = asm.pow(I x,y)
        static member pow(x:int, y:double0) = asm.pow(I x,y)
        static member pow(x:int, y:complex0) = asm.pow(I x,y)
        static member pow(x:double, y:int0) = asm.pow(D x,y)
        static member pow(x:double, y:double0) = asm.pow(D x,y)
        static member pow(x:double, y:complex0) = asm.pow(D x,y)

        ///<summary>指数関数</summary>
        static member exp (v:int0) = double0(Exp(v.etype,v.Expr))
        static member exp (v:double0) = double0(Exp(v.etype,v.Expr))
        static member exp (v:complex0) = complex0(Exp(v.etype,v.Expr))
        ///<summary>正弦関数</summary>
        static member sin (v:int0) = double0(Sin(v.etype,v.Expr))
        static member sin (v:double0) = double0(Sin(v.etype,v.Expr))
        static member sin (v:complex0) = complex0(Sin(v.etype,v.Expr))
        ///<summary>余弦関数</summary>
        static member cos (v:int0) = double0(Cos(v.etype,v.Expr))
        static member cos (v:double0) = double0(Cos(v.etype,v.Expr))
        static member cos (v:complex0) = complex0(Cos(v.etype,v.Expr))
        ///<summary>正接関数</summary>
        static member tan (v:int0) = double0(Tan(v.etype,v.Expr))
        static member tan (v:double0) = double0(Tan(v.etype,v.Expr))
        static member tan (v:complex0) = complex0(Tan(v.etype,v.Expr))
        ///<summary>逆正弦関数</summary>
        static member asin (v:int0) = double0(Asin(v.etype,v.Expr))
        static member asin (v:double0) = double0(Asin(v.etype,v.Expr))
        static member asin (v:complex0) = complex0(Asin(v.etype,v.Expr))
        ///<summary>逆余弦関数</summary>
        static member acos (v:int0) = double0(Acos(v.etype,v.Expr))
        static member acos (v:double0) = double0(Acos(v.etype,v.Expr))
        static member acos (v:complex0) = complex0(Acos(v.etype,v.Expr))
        ///<summary>逆正接関数</summary>
        static member atan (v:int0) = double0(Atan(v.etype,v.Expr))
        static member atan (v:double0) = double0(Atan(v.etype,v.Expr))
        static member atan (v:complex0) = complex0(Atan(v.etype,v.Expr))
        ///<summary>逆正接関数</summary>
        static member atan2(x:int0, y:int0) = double0(Atan2(x.Expr,y.Expr))
        static member atan2(x:double0, y:int0) = double0(Atan2(x.Expr,y.Expr))
        static member atan2(x:int0, y:double0) = double0(Atan2(x.Expr,y.Expr))
        static member atan2(x:double0, y:double0) = double0(Atan2(x.Expr,y.Expr))
        ///<summary>絶対値</summary>
        static member abs (v:int0) = double0(Abs(Dt,v.Expr))
        static member abs (v:double0) = double0(Abs(Dt,v.Expr))
        static member abs (v:complex0) = double0(Abs(Dt,v.Expr))
        ///<summary>自然対数</summary>
        static member log (v:int0) = double0(Log(v.etype,v.Expr))
        static member log (v:double0) = double0(Log(v.etype,v.Expr))
        static member log (v:complex0) = complex0(Log(v.etype,v.Expr))
        ///<summary>常用対数</summary>
        static member log10 (v:int0) = double0(Log10(v.etype,v.Expr))
        static member log10 (v:double0) = double0(Log10(v.etype,v.Expr))
        static member log10 (v:complex0) = complex0(Log10(v.etype,v.Expr))
        ///<summary>平方根</summary>
        static member sqrt (v:int0) = double0(Sqrt(v.etype,v.Expr))
        static member sqrt (v:double0) = double0(Sqrt(v.etype,v.Expr))
        static member sqrt (v:complex0) = complex0(Sqrt(v.etype,v.Expr))
        ///<summary>小数点以下切り捨て</summary>
        static member floor (v:double0) = asm.toint(double0(Floor v.Expr))
        ///<summary>小数点以下切り上げ</summary>
        static member ceil (v:double0) = asm.toint(double0(Ceil v.Expr))
        ///<summary>共役複素数</summary>
        static member conj (v:complex0) = complex0(Conj v.Expr)
        static member iSum (n1:int0, n2:int0) = fun (f:int0->int0) -> int0(Sum(It 4, n1.Expr, n2.Expr, fun (x:expr) -> (f(int0 x)).Expr))
        static member iSum (n1:int, n2:int0) = fun (f:int0->int0) -> int0(Sum(It 4, Int n1, n2.Expr, fun (x:expr) -> (f(int0 x)).Expr))
        static member iSum (n1:int0, n2:int) = fun (f:int0->int0) -> int0(Sum(It 4, n1.Expr, Int n2, fun (x:expr) -> (f(int0 x)).Expr))
        static member iSum (n1:int, n2:int) = fun (f:int0->int0) -> int0(Sum(It 4, Int n1, Int n2, fun (x:expr) -> (f(int0 x)).Expr))
        static member dSum (n1:int0, n2:int0) = fun (f:int0->double0) -> double0(Sum(Dt, n1.Expr, n2.Expr, fun (x:expr) -> (f(int0 x)).Expr))
        static member dSum (n1:int, n2:int0) = fun (f:int0->double0) -> double0(Sum(Dt, Int n1, n2.Expr, fun (x:expr) -> (f(int0 x)).Expr))
        static member dSum (n1:int0, n2:int) = fun (f:int0->double0) -> double0(Sum(Dt, n1.Expr, Int n2, fun (x:expr) -> (f(int0 x)).Expr))
        static member dSum (n1:int, n2:int) = fun (f:int0->double0) -> double0(Sum(Dt, Int n1, Int n2, fun (x:expr) -> (f(int0 x)).Expr))
        static member zSum (n1:int0, n2:int0) = fun (f:int0->complex0) -> complex0(Sum(Zt, n1.Expr, n2.Expr, fun (x:expr) -> (f(int0 x)).Expr))
        static member zSum (n1:int, n2:int0) = fun (f:int0->complex0) -> complex0(Sum(Zt, Int n1, n2.Expr, fun (x:expr) -> (f(int0 x)).Expr))
        static member zSum (n1:int0, n2:int) = fun (f:int0->complex0) -> complex0(Sum(Zt, n1.Expr, Int n2, fun (x:expr) -> (f(int0 x)).Expr))
        static member zSum (n1:int, n2:int) = fun (f:int0->complex0) -> complex0(Sum(Zt, Int n1, Int n2, fun (x:expr) -> (f(int0 x)).Expr))
        static member iLet (x:int0) = fun (f:int0->int0) -> int0(Let(It 4, x.Expr, fun (x:expr) -> (f(int0 x)).Expr))
        static member iLet (x:int) = fun (f:int0->int0) -> asm.iLet (I x) f
        static member dLet (x:double0) = fun (f:double0->double0) -> double0(Let(Dt, x.Expr, fun (x:expr) -> (f(double0 x)).Expr))
        static member dLet (x:double) = fun (f:double0->double0) -> asm.dLet (D x) f
        static member dLet (x:int0) = fun (f:double0->double0) -> double0(Let(Dt, x.Expr, fun (x:expr) -> (f(double0 x)).Expr))
        static member dLet (x:int) = fun (f:double0->double0) -> asm.dLet (I x) f
        static member zLet (x:complex0) = fun (f:complex0->complex0) -> complex0(Let(Zt, x.Expr, fun (x:expr) -> (f(complex0 x)).Expr))
        static member zLet (x:double0) = fun (f:complex0->complex0) -> complex0(Let(Zt, x.Expr, fun (x:expr) -> (f(complex0 x)).Expr))
        static member zLet (x:double*double) = fun (f:complex0->complex0) -> asm.zLet (Z x) f
        static member zLet (x:double) = fun (f:complex0->complex0) -> asm.zLet (D x) f
        static member zLet (x:int0) = fun (f:complex0->complex0) -> complex0(Let(Zt, x.Expr, fun (x:expr) -> (f(complex0 x)).Expr))
        static member zLet (x:int) = fun (f:complex0->complex0) -> asm.zLet (I x) f
        static member diff (f:double0,x:double0) = double0(expr.diff f.Expr x.Expr (GenerationScope.currentProgram()))
        static member diff (f:complex0,x:double0) = complex0(expr.diff f.Expr x.Expr (GenerationScope.currentProgram()))
        static member diff (f:double0,x:complex0) = complex0(expr.diff f.Expr x.Expr (GenerationScope.currentProgram()))
        
    [<AutoOpen>]
    module num0_op =
        type int0 with
            ///<summary>インクリメント</summary>
            member x.inc() = x <== x + 1
            ///<summary>デクリメント</summary>
            member x.dec() = x <== x - 1
        type complex0 with
            ///<summary>実部</summary>
            member x.re with get() = double0(Re x.Expr)
            ///<summary>虚部</summary>
            member x.im with get() = double0(Im x.Expr)
            ///<summary>複素共役</summary>
            member x.conj with get() = complex0(Conj x.Expr)
            ///<summary>絶対値</summary>
            member x.abs with get() = asm.abs x
            ///<summary>絶対値の2乗</summary>
            member x.pow with get() = asm.pow(asm.abs x,2)
            ///<summary>偏角</summary>
            member x.pha with get() = asm.atan2(x.im,x.re)
