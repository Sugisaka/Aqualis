namespace Aqualis
    
    open System
    
    ///<summary>数学関数</summary>
    type asm =
        ///<summary>虚数単位</summary>
        static member uj with get() =
            match pr.language with
            |Fortran   -> 
                pr.var.setUniqVar(Zt,A0,"uj","(0d0,1d0)")
                num0(Var(Zt,"uj",NaN))
            |C99 -> 
                //#defineで定義済み
                num0(Var(Zt,"uj",NaN))
            |LaTeX ->
                pr.var.setUniqVar(Zt,A0,"\\mathrm{j}","(0d0,1d0)")
                num0(Var(Zt,"\\mathrm{j}",NaN))
            |HTML ->
                pr.var.setUniqVar(Zt,A0,"\\mathrm{j}","(0d0,1d0)")
                num0(Var(Zt,"\\mathrm{j}",NaN))
            |Python -> 
                num0(Var(Zt,"1.0j",NaN))
            |JavaScript -> 
                num0(Cpx(0.0,1.0))
            |PHP -> 
                num0(Cpx(0.0,1.0))
            |Numeric -> 
                num0(Cpx(0.0,1.0))
        ///<summary>円周率</summary>
        static member pi with get() = 
            match pr.language with
            |Fortran   ->
                pr.var.setUniqVar(Dt,A0,"pi","3.14159265358979d0")
                num0(Var(Dt,"pi",NaN))
            |C99 ->
                pr.var.setUniqVar(Dt,A0,"pi","3.14159265358979")
                num0(Var(Dt,"pi",NaN))
            |LaTeX   ->
                pr.var.setUniqVar(Dt,A0,"\\pi","3.14159265358979")
                num0(Var(Dt,"\\pi",NaN))
            |HTML   ->
                pr.var.setUniqVar(Dt,A0,"\\pi","3.14159265358979")
                num0(Var(Dt,"\\pi",NaN))
            |Python ->
                pr.var.setUniqVar(Dt,A0,"pi","3.14159265358979")
                num0(Var(Dt,"pi",NaN))
            |JavaScript ->
                pr.var.setUniqVar(Dt,A0,"pi","3.14159265358979")
                num0(Var(Dt,"pi",NaN))
            |PHP ->
                pr.var.setUniqVar(Dt,A0,"pi","3.14159265358979")
                num0(Var(Dt,"pi",NaN))
            |Numeric -> 
                num0(Dbl Math.PI)
        ///<summary>2πj</summary>
        static member j2p with get() = 2*asm.pi*asm.uj
        static member todouble(x:num0) = num0(ToDbl x.Expr)
        static member toint(x:num0) = num0(ToInt x.Expr)
        ///<summary>累乗</summary>
        static member pow(x:num0, y:num0) = num0(Pow(x%%y,x.Expr,y.Expr))
        static member pow(x:num0, y:int) = asm.pow(x,I y)
        static member pow(x:num0, y:double) = asm.pow(x,D y)
        static member pow(x:int, y:num0) = asm.pow(I x,y)
        static member pow(x:double, y:num0) = asm.pow(D x,y)
        ///<summary>指数関数</summary>
        static member exp (v:num0) = num0(Exp(v.etype,v.Expr))
        ///<summary>正弦関数</summary>
        static member sin (v:num0) = num0(Sin(v.etype,v.Expr))
        ///<summary>余弦関数</summary>
        static member cos (v:num0) = num0(Cos(v.etype,v.Expr))
        static member tan (v:num0) = num0(Tan(v.etype,v.Expr))
        ///<summary>逆正弦関数</summary>
        static member asin (v:num0) = num0(Asin(v.etype,v.Expr))
        ///<summary>逆余弦関数</summary>
        static member acos (v:num0) = num0(Acos(v.etype,v.Expr))
        ///<summary>逆正接関数</summary>
        static member atan (v:num0) = num0(Atan(v.etype,v.Expr))
        ///<summary>逆正接関数</summary>
        static member atan2(x:num0, y:num0) = num0(Atan2(x.Expr,y.Expr))
        ///<summary>絶対値</summary>
        static member abs (v:num0) = num0(Abs(Dt,v.Expr))
        ///<summary>自然対数</summary>
        static member log (v:num0) = num0(Log(v.etype,v.Expr))
        ///<summary>常用対数</summary>
        static member log10 (v:num0) = num0(Log10(v.etype,v.Expr))
        ///<summary>平方根</summary>
        static member sqrt (v:num0) = num0(Sqrt(v.etype,v.Expr))
        ///<summary>小数点以下切り捨て</summary>
        static member floor (v:num0) = num0(Floor v.Expr)
        ///<summary>小数点以下切り上げ</summary>
        static member ceil (v:num0) = num0(Ceil v.Expr)
        ///<summary>共役複素数</summary>
        static member conj (v:num0) = num0(Conj v.Expr)
        static member iSum (n1:num0,n2:num0) = fun (f:num0->num0) -> num0(Sum(It 4, n1.Expr, n2.Expr, fun (x:expr) -> (f(num0 x)).Expr))
        static member iSum (n1:int,n2:num0) = fun (f:num0->num0) -> num0(Sum(It 4, Int n1, n2.Expr, fun (x:expr) -> (f(num0 x)).Expr))
        static member iSum (n1:num0,n2:int) = fun (f:num0->num0) -> num0(Sum(It 4, n1.Expr, Int n2, fun (x:expr) -> (f(num0 x)).Expr))
        static member iSum (n1:int,n2:int) = fun (f:num0->num0) -> num0(Sum(It 4, Int n1, Int n2, fun (x:expr) -> (f(num0 x)).Expr))
        static member dSum (n1:num0,n2:num0) = fun (f:num0->num0) -> num0(Sum(Dt, n1.Expr, n2.Expr, fun (x:expr) -> (f(num0 x)).Expr))
        static member dSum (n1:int,n2:num0) = fun (f:num0->num0) -> num0(Sum(Dt, Int n1, n2.Expr, fun (x:expr) -> (f(num0 x)).Expr))
        static member dSum (n1:num0,n2:int) = fun (f:num0->num0) -> num0(Sum(Dt, n1.Expr, Int n2, fun (x:expr) -> (f(num0 x)).Expr))
        static member dSum (n1:int,n2:int) = fun (f:num0->num0) -> num0(Sum(Dt, Int n1, Int n2, fun (x:expr) -> (f(num0 x)).Expr))
        static member zSum (n1:num0,n2:num0) = fun (f:num0->num0) -> num0(Sum(Zt, n1.Expr, n2.Expr, fun (x:expr) -> (f(num0 x)).Expr))
        static member zSum (n1:int,n2:num0) = fun (f:num0->num0) -> num0(Sum(Zt, Int n1, n2.Expr, fun (x:expr) -> (f(num0 x)).Expr))
        static member zSum (n1:num0,n2:int) = fun (f:num0->num0) -> num0(Sum(Zt, n1.Expr, Int n2, fun (x:expr) -> (f(num0 x)).Expr))
        static member zSum (n1:int,n2:int) = fun (f:num0->num0) -> num0(Sum(Zt, Int n1, Int n2, fun (x:expr) -> (f(num0 x)).Expr))
        static member iLet (x:num0) = fun (f:num0->num0) -> num0(Let(It 4, x.Expr, fun (x:expr) -> (f(num0 x)).Expr))
        static member iLet (x:int) = asm.iLet (I x)
        static member dLet (x:num0) = fun (f:num0->num0) -> num0(Let(Dt, x.Expr, fun (x:expr) -> (f(num0 x)).Expr))
        static member dLet (x:int) = asm.dLet (I x)
        static member dLet (x:double) = asm.dLet (D x)
        static member zLet (x:num0) = fun (f:num0->num0) -> num0(Let(Zt, x.Expr, fun (x:expr) -> (f(num0 x)).Expr))
        static member zLet (x:int) = asm.zLet (I x)
        static member zLet (x:double) = asm.zLet (D x)
        static member zLet (x:double*double) = asm.zLet (Z x)
        static member diff (f:num0) (x:num0) = num0(expr.diff f.Expr x.Expr pr)
        
    [<AutoOpen>]
    module num0_op =
        type num0 with
            ///<summary>実部</summary>
            member x.re with get() = num0(Re x.Expr)
            ///<summary>虚部</summary>
            member x.im with get() = num0(Im x.Expr)
            ///<summary>複素共役</summary>
            member x.conj with get() = num0(Conj x.Expr)
            ///<summary>絶対値</summary>
            member x.abs with get() = asm.abs x
            ///<summary>絶対値の2乗</summary>
            member x.pow with get() = asm.pow(asm.abs x,2)
            ///<summary>偏角</summary>
            member x.pha with get() = asm.atan2(x.im,x.re)
            ///<summary>インクリメント</summary>
            member x.inc() = x <== x + 1
            ///<summary>デクリメント</summary>
            member x.dec() = x <== x - 1
