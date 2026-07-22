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
        static member todouble(x:int0) = double0(ToDbl x.Expr, ?context=x.Context)
        static member toint(x:double0) = int0(ToInt x.Expr, ?context=x.Context)
        ///<summary>累乗</summary>
        static member pow(x:int0, y:int0) = int0(Pow(It 4,x.Expr,y.Expr), ?context=NumericContext.binary x y)
        static member pow(x:int0, y:double0) = double0(Pow(Dt,x.Expr,y.Expr), ?context=NumericContext.binary x y)
        static member pow(x:int0, y:complex0) = complex0(Pow(Zt,x.Expr,y.Expr), ?context=NumericContext.binary x y)
        static member pow(x:double0, y:int0) = double0(Pow(Dt,x.Expr,y.Expr), ?context=NumericContext.binary x y)
        static member pow(x:double0, y:double0) = double0(Pow(Dt,x.Expr,y.Expr), ?context=NumericContext.binary x y)
        static member pow(x:double0, y:complex0) = complex0(Pow(Zt,x.Expr,y.Expr), ?context=NumericContext.binary x y)
        static member pow(x:complex0, y:int0) = double0(Pow(Zt,x.Expr,y.Expr), ?context=NumericContext.binary x y)
        static member pow(x:complex0, y:double0) = double0(Pow(Zt,x.Expr,y.Expr), ?context=NumericContext.binary x y)
        static member pow(x:complex0, y:complex0) = complex0(Pow(Zt,x.Expr,y.Expr), ?context=NumericContext.binary x y)
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
        static member exp (v:int0) = double0(Exp(v.etype,v.Expr), ?context=v.Context)
        static member exp (v:double0) = double0(Exp(v.etype,v.Expr), ?context=v.Context)
        static member exp (v:complex0) = complex0(Exp(v.etype,v.Expr), ?context=v.Context)
        ///<summary>正弦関数</summary>
        static member sin (v:int0) = double0(Sin(v.etype,v.Expr), ?context=v.Context)
        static member sin (v:double0) = double0(Sin(v.etype,v.Expr), ?context=v.Context)
        static member sin (v:complex0) = complex0(Sin(v.etype,v.Expr), ?context=v.Context)
        ///<summary>余弦関数</summary>
        static member cos (v:int0) = double0(Cos(v.etype,v.Expr), ?context=v.Context)
        static member cos (v:double0) = double0(Cos(v.etype,v.Expr), ?context=v.Context)
        static member cos (v:complex0) = complex0(Cos(v.etype,v.Expr), ?context=v.Context)
        ///<summary>正接関数</summary>
        static member tan (v:int0) = double0(Tan(v.etype,v.Expr), ?context=v.Context)
        static member tan (v:double0) = double0(Tan(v.etype,v.Expr), ?context=v.Context)
        static member tan (v:complex0) = complex0(Tan(v.etype,v.Expr), ?context=v.Context)
        ///<summary>逆正弦関数</summary>
        static member asin (v:int0) = double0(Asin(v.etype,v.Expr), ?context=v.Context)
        static member asin (v:double0) = double0(Asin(v.etype,v.Expr), ?context=v.Context)
        static member asin (v:complex0) = complex0(Asin(v.etype,v.Expr), ?context=v.Context)
        ///<summary>逆余弦関数</summary>
        static member acos (v:int0) = double0(Acos(v.etype,v.Expr), ?context=v.Context)
        static member acos (v:double0) = double0(Acos(v.etype,v.Expr), ?context=v.Context)
        static member acos (v:complex0) = complex0(Acos(v.etype,v.Expr), ?context=v.Context)
        ///<summary>逆正接関数</summary>
        static member atan (v:int0) = double0(Atan(v.etype,v.Expr), ?context=v.Context)
        static member atan (v:double0) = double0(Atan(v.etype,v.Expr), ?context=v.Context)
        static member atan (v:complex0) = complex0(Atan(v.etype,v.Expr), ?context=v.Context)
        ///<summary>逆正接関数</summary>
        static member atan2(x:int0, y:int0) = double0(Atan2(x.Expr,y.Expr), ?context=NumericContext.binary x y)
        static member atan2(x:double0, y:int0) = double0(Atan2(x.Expr,y.Expr), ?context=NumericContext.binary x y)
        static member atan2(x:int0, y:double0) = double0(Atan2(x.Expr,y.Expr), ?context=NumericContext.binary x y)
        static member atan2(x:double0, y:double0) = double0(Atan2(x.Expr,y.Expr), ?context=NumericContext.binary x y)
        ///<summary>絶対値</summary>
        static member abs (v:int0) = double0(Abs(Dt,v.Expr), ?context=v.Context)
        static member abs (v:double0) = double0(Abs(Dt,v.Expr), ?context=v.Context)
        static member abs (v:complex0) = double0(Abs(Dt,v.Expr), ?context=v.Context)
        ///<summary>自然対数</summary>
        static member log (v:int0) = double0(Log(v.etype,v.Expr), ?context=v.Context)
        static member log (v:double0) = double0(Log(v.etype,v.Expr), ?context=v.Context)
        static member log (v:complex0) = complex0(Log(v.etype,v.Expr), ?context=v.Context)
        ///<summary>常用対数</summary>
        static member log10 (v:int0) = double0(Log10(v.etype,v.Expr), ?context=v.Context)
        static member log10 (v:double0) = double0(Log10(v.etype,v.Expr), ?context=v.Context)
        static member log10 (v:complex0) = complex0(Log10(v.etype,v.Expr), ?context=v.Context)
        ///<summary>平方根</summary>
        static member sqrt (v:int0) = double0(Sqrt(v.etype,v.Expr), ?context=v.Context)
        static member sqrt (v:double0) = double0(Sqrt(v.etype,v.Expr), ?context=v.Context)
        static member sqrt (v:complex0) = complex0(Sqrt(v.etype,v.Expr), ?context=v.Context)
        ///<summary>小数点以下切り捨て</summary>
        static member floor (v:double0) = int0(ToInt(Floor v.Expr), ?context=v.Context)
        ///<summary>小数点以下切り上げ</summary>
        static member ceil (v:double0) = int0(ToInt(Ceil v.Expr), ?context=v.Context)
        ///<summary>共役複素数</summary>
        static member conj (v:complex0) = complex0(Conj v.Expr, ?context=v.Context)
        static member iSum (n1:int0, n2:int0) = fun (f:int0->int0) ->
            let context = NumericContext.binary n1 n2
            int0(Sum(It 4, n1.Expr, n2.Expr, fun value -> (f(int0(value, ?context=context))).Expr), ?context=context)
        static member iSum (n1:int, n2:int0) = asm.iSum(int0(Int n1), n2)
        static member iSum (n1:int0, n2:int) = asm.iSum(n1, int0(Int n2))
        static member iSum (n1:int, n2:int) = asm.iSum(int0(Int n1), int0(Int n2))
        static member dSum (n1:int0, n2:int0) = fun (f:int0->double0) ->
            let context = NumericContext.binary n1 n2
            double0(Sum(Dt, n1.Expr, n2.Expr, fun value -> (f(int0(value, ?context=context))).Expr), ?context=context)
        static member dSum (n1:int, n2:int0) = asm.dSum(int0(Int n1), n2)
        static member dSum (n1:int0, n2:int) = asm.dSum(n1, int0(Int n2))
        static member dSum (n1:int, n2:int) = asm.dSum(int0(Int n1), int0(Int n2))
        static member zSum (n1:int0, n2:int0) = fun (f:int0->complex0) ->
            let context = NumericContext.binary n1 n2
            complex0(Sum(Zt, n1.Expr, n2.Expr, fun value -> (f(int0(value, ?context=context))).Expr), ?context=context)
        static member zSum (n1:int, n2:int0) = asm.zSum(int0(Int n1), n2)
        static member zSum (n1:int0, n2:int) = asm.zSum(n1, int0(Int n2))
        static member zSum (n1:int, n2:int) = asm.zSum(int0(Int n1), int0(Int n2))
        static member iLet (x:int0) = fun (f:int0->int0) ->
            int0(Let(It 4, x.Expr, fun value -> (f(int0(value, ?context=x.Context))).Expr), ?context=x.Context)
        static member iLet (x:int) = fun (f:int0->int0) -> asm.iLet (I x) f
        static member dLet (x:double0) = fun (f:double0->double0) ->
            double0(Let(Dt, x.Expr, fun value -> (f(double0(value, ?context=x.Context))).Expr), ?context=x.Context)
        static member dLet (x:double) = fun (f:double0->double0) -> asm.dLet (D x) f
        static member dLet (x:int0) = fun (f:double0->double0) ->
            double0(Let(Dt, x.Expr, fun value -> (f(double0(value, ?context=x.Context))).Expr), ?context=x.Context)
        static member dLet (x:int) = fun (f:double0->double0) -> asm.dLet (I x) f
        static member zLet (x:complex0) = fun (f:complex0->complex0) ->
            complex0(Let(Zt, x.Expr, fun value -> (f(complex0(value, ?context=x.Context))).Expr), ?context=x.Context)
        static member zLet (x:double0) = fun (f:complex0->complex0) ->
            complex0(Let(Zt, x.Expr, fun value -> (f(complex0(value, ?context=x.Context))).Expr), ?context=x.Context)
        static member zLet (x:double*double) = fun (f:complex0->complex0) -> asm.zLet (Z x) f
        static member zLet (x:double) = fun (f:complex0->complex0) -> asm.zLet (D x) f
        static member zLet (x:int0) = fun (f:complex0->complex0) ->
            complex0(Let(Zt, x.Expr, fun value -> (f(complex0(value, ?context=x.Context))).Expr), ?context=x.Context)
        static member zLet (x:int) = fun (f:complex0->complex0) -> asm.zLet (I x) f
        static member diff (f:double0,x:double0) =
            let context = NumericContext.binary f x |> GenerationContextMerge.requireTarget
            double0(expr.diff f.Expr x.Expr context.CurrentProgram, context=context)
        static member diff (f:complex0,x:double0) =
            let context = NumericContext.binary f x |> GenerationContextMerge.requireTarget
            complex0(expr.diff f.Expr x.Expr context.CurrentProgram, context=context)
        static member diff (f:double0,x:complex0) =
            let context = NumericContext.binary f x |> GenerationContextMerge.requireTarget
            complex0(expr.diff f.Expr x.Expr context.CurrentProgram, context=context)
        
    [<AutoOpen>]
    module num0_op =
        type int0 with
            ///<summary>インクリメント</summary>
            member x.inc() = x <== x + 1
            ///<summary>デクリメント</summary>
            member x.dec() = x <== x - 1
        type double0 with
            ///<summary>近い整数値に変換</summary>
            member this.round with get() = asm.toint <| asm.floor this+0.5
        type complex0 with
            ///<summary>実部</summary>
            member x.re with get() = double0(Re x.Expr, ?context=x.Context)
            ///<summary>虚部</summary>
            member x.im with get() = double0(Im x.Expr, ?context=x.Context)
            ///<summary>複素共役</summary>
            member x.conj with get() = complex0(Conj x.Expr, ?context=x.Context)

            ///<summary>絶対値</summary>
            member x.abs with get() = asm.abs x
            ///<summary>絶対値の2乗</summary>
            member x.pow with get() = asm.pow(asm.abs x,2)
            ///<summary>偏角</summary>
            member x.pha with get() = asm.atan2(x.im,x.re)

    /// Context-dependent mathematical constants. Functions with operands continue
    /// to derive their context from those operands through NumericContext.
    type ContextAsm internal (environment:CompilationEnvironment) =
        let context() = environment.RequireGenerationContext()

        member _.uj =
            let ctx = context()
            match ctx.CurrentProgram.language with
            |Fortran ->
                ctx.CurrentProgram.var.setUniqVar(Zt,A0,"uj","(0d0,1d0)")
                complex0(Var(Zt,"uj",NaN), context=ctx)
            |C99 -> complex0(Var(Zt,"uj",NaN), context=ctx)
            |LaTeX|HTML|HTMLSequenceDiagram ->
                ctx.CurrentProgram.var.setUniqVar(Zt,A0,"\\mathrm{j}","(0d0,1d0)")
                complex0(Var(Zt,"\\mathrm{j}",NaN), context=ctx)
            |Python -> complex0(Var(Zt,"1.0j",NaN), context=ctx)
            |JavaScript|PHP|Numeric -> complex0(Cpx(0.0,1.0))

        member _.pi =
            let ctx = context()
            match ctx.CurrentProgram.language with
            |Fortran ->
                ctx.CurrentProgram.var.setUniqVar(Dt,A0,"pi","3.14159265358979d0")
                double0(Var(Dt,"pi",NaN), context=ctx)
            |C99|Python|PHP ->
                ctx.CurrentProgram.var.setUniqVar(Dt,A0,"pi","3.14159265358979")
                double0(Var(Dt,"pi",NaN), context=ctx)
            |LaTeX|HTML|HTMLSequenceDiagram ->
                ctx.CurrentProgram.var.setUniqVar(Dt,A0,"\\pi","3.14159265358979")
                double0(Var(Dt,"\\pi",NaN), context=ctx)
            |JavaScript|Numeric -> double0(Dbl Math.PI)

    [<AutoOpen>]
    module CompilationEnvironmentAsmExtensions =
        type CompilationEnvironment with
            member this.asm = ContextAsm(this)
