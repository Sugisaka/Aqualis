(*
Copyright (c) 2022 Jun-ichiro Sugisaka

This software is released under the MIT License.
http://opensource.org/licenses/mit-license.php
*)
namespace Aqualis
    
    open Aqualis_base
    
    ///<summary>変数</summary>
    type aqvar(x:Expr) =
        member _.expr with get() = x
        member _.code with get() = x.code
        
    ///<summary>スカラー変数</summary>
    type base0(x:Expr) =
        inherit aqvar(x)
        
    ///<summary>文字列型</summary>
    type str(x:string) =
        inherit base0(Str_c x)
        
    ///<summary>数値型</summary>
    type num0(e:Etype,x:Expr) =
        inherit base0(x)
        new(n:int) = num0(It 4, Int_c n)
        new(n:double) = num0(Dt, Dbl_c n)
        member this.etype with get() = e
        static member (~-) (x:num0) = num0(x.etype,-x.expr)
        static member ( + ) (x:num0,y:num0) = num0(Etype.prior(x.etype,y.etype), x.expr + y.expr)
        static member ( + ) (x:num0,y:double) = x+num0(y)
        static member ( + ) (x:num0,y:int) = x+num0(y)
        static member ( + ) (x:double,y:num0) = num0(x)+y
        static member ( + ) (x:int,y:num0) = num0(x)+y
        
        static member ( - ) (x:num0,y:num0) = num0(Etype.prior(x.etype,y.etype), x.expr - y.expr)
        static member ( - ) (x:num0,y:double) = x-num0(y)
        static member ( - ) (x:num0,y:int) = x-num0(y)
        static member ( - ) (x:double,y:num0) = num0(x)-y
        static member ( - ) (x:int,y:num0) = num0(x)-y
        
        static member ( * ) (x:num0,y:num0) = num0(Etype.prior(x.etype,y.etype), x.expr * y.expr)
        static member ( * ) (x:num0,y:double) = x*num0(y)
        static member ( * ) (x:num0,y:int) = x*num0(y)
        static member ( * ) (x:double,y:num0) = num0(x)*y
        static member ( * ) (x:int,y:num0) = num0(x)*y
        
        static member ( / ) (x:num0,y:num0) = 
            match x.etype,y.etype with
            |It _,It _ ->
                let xx:num0 = asm.todouble(x)
                let yy:num0 = asm.todouble(y)
                num0(Dt, xx.expr / yy.expr)
            |It _,_ ->
                let xx:num0 = asm.todouble(x)
                num0(Dt, xx.expr / y.expr)
            |_,It _ ->
                let yy:num0 = asm.todouble(y)
                num0(Dt, x.expr / yy.expr)
            |_ ->
                num0(Etype.prior(x.etype,y.etype), x.expr / y.expr)
        static member ( / ) (x:num0,y:double) = x/num0(y)
        static member ( / ) (x:num0,y:int) = x/num0(double y)
        static member ( / ) (x:double,y:num0) = num0(x)/y
        static member ( / ) (x:int,y:num0) = num0(double x)/y
        
        static member ( ./ ) (x:num0,y:num0) = num0(Etype.prior(x.etype,y.etype), x.expr ./ y.expr)
        static member ( ./ ) (x:num0,y:double) = x./num0(y)
        static member ( ./ ) (x:num0,y:int) = x./num0(y)
        static member ( ./ ) (x:double,y:num0) = num0(x)./y
        static member ( ./ ) (x:int,y:num0) = num0(x)./y
        
        static member ( % ) (x:num0,y:num0) = num0(Etype.prior(x.etype,y.etype), x.expr % y.expr)
        static member ( % ) (x:num0,y:double) = x%num0(y)
        static member ( % ) (x:num0,y:int) = x%num0(y)
        static member ( % ) (x:double,y:num0) = num0(x)%y
        static member ( % ) (x:int,y:num0) = num0(x)%y

        static member (=.) (x:num0,y:num0) = bool0(x.expr =. y.expr)
        static member (=.) (x:int,y:num0) = bool0((Int_c x) =. y.expr)
        static member (=.) (x:double,y:num0) = bool0((Dbl_c x) =. y.expr)
        static member (=.) (x:num0,y:int) = bool0(x.expr =. (Int_c y))
        static member (=.) (x:num0,y:double) = bool0(x.expr =. (Dbl_c y))
        
        static member (.=/) (x:num0,y:num0) = bool0(x.expr .=/ y.expr)
        static member (.=/) (x:int,y:num0) = bool0((Int_c x) .=/ y.expr)
        static member (.=/) (x:double,y:num0) = bool0((Dbl_c x) .=/ y.expr)
        static member (.=/) (x:num0,y:int) = bool0(x.expr .=/ (Int_c y))
        static member (.=/) (x:num0,y:double) = bool0(x.expr .=/ (Dbl_c y))
        
        static member (.<) (x:num0,y:num0) = bool0(x.expr .< y.expr)
        static member (.<) (x:int,y:num0) = bool0((Int_c x) .< y.expr)
        static member (.<) (x:double,y:num0) = bool0((Dbl_c x) .< y.expr)
        static member (.<) (x:num0,y:int) = bool0(x.expr .< (Int_c y))
        static member (.<) (x:num0,y:double) = bool0(x.expr .< (Dbl_c y))
        
        static member (.<=) (x:num0,y:num0) = bool0(x.expr .<= y.expr)
        static member (.<=) (x:int,y:num0) = bool0((Int_c x) .<= y.expr)
        static member (.<=) (x:double,y:num0) = bool0((Dbl_c x) .<= y.expr)
        static member (.<=) (x:num0,y:int) = bool0(x.expr .<= (Int_c y))
        static member (.<=) (x:num0,y:double) = bool0(x.expr .<= (Dbl_c y))
        
        static member (.>) (x:num0,y:num0) = bool0(x.expr .> y.expr)
        static member (.>) (x:int,y:num0) = bool0((Int_c x) .> y.expr)
        static member (.>) (x:double,y:num0) = bool0((Dbl_c x) .> y.expr)
        static member (.>) (x:num0,y:int) = bool0(x.expr .> (Int_c y))
        static member (.>) (x:num0,y:double) = bool0(x.expr .> (Dbl_c y))
        
        static member (.>=) (x:num0,y:num0) = bool0(x.expr .>= y.expr)
        static member (.>=) (x:int,y:num0) = bool0((Int_c x) .>= y.expr)
        static member (.>=) (x:double,y:num0) = bool0((Dbl_c x) .>= y.expr)
        static member (.>=) (x:num0,y:int) = bool0(x.expr .>= (Int_c y))
        static member (.>=) (x:num0,y:double) = bool0(x.expr .>= (Dbl_c y))
        
        static member (<==) (x:num0,y:num0) = 
            match x.etype,y.etype with
            |Dt,Zt -> printfn "Warning: complex型からdouble型への代入です：%s←%s" x.code y.code
            |It _,Dt -> printfn "Warning: double型からint型への代入です：%s←%s" x.code y.code
            |It 2,It 4 -> printfn "Warning: int型からbyte型への代入です：%s←%s" x.code y.code
            |_ -> ()
            x.expr <== y.expr
        static member (<==) (x:num0,y:int) = x <== num0(y)
        static member (<==) (x:num0,y:double) = x <== num0(y)
        static member (<==) (x:num0,(re:double,im:double)) = x <== re+asm.uj*im
        static member (<==) (x:list<num0>,y:num0) = for v in x do v <== y
        static member (<==) (x:list<num0>,y:int) = for v in x do v <== y
        static member (<==) (x:list<num0>,y:double) = for v in x do v <== y
        static member (<==) (x:list<num0>,y:double*double) = for v in x do v <== y
        member this.clear() = this <== 0
        
        ///<summary>実部</summary>
        member x.re with get() = num0(Dt, match p.lang with |F -> Formula("real("+x.code+")") |C -> Formula("creal("+x.code+")") |_ -> Formula("Re["+x.code+"]"))
        ///<summary>虚部</summary>
        member x.im with get() = num0(Dt, match p.lang with |F -> Formula("aimag("+x.code+")") |C -> Formula("cimag("+x.code+")") |_ -> Formula("Im["+x.code+"]"))
        ///<summary>絶対値</summary>
        member x.abs with get() = asm.abs(x)
        ///<summary>絶対値の2乗</summary>
        member x.pow with get() = asm.pow(asm.abs(x),2)
        ///<summary>偏角</summary>
        member x.pha with get() = asm.atan2(x.im,x.re)
        ///<summary>インクリメント</summary>
        member x.inc with get() = x <== x + 1
        ///<summary>デクリメント</summary>
        member x.dec with get() = x <== x - 1
        
    and bool0(x:Expr) =
        inherit aqvar(x)
        static member (=.) (x:bool0,y:num0) = bool0(x.expr =. y.expr)
        static member (=.) (x:bool0,y:int) = bool0(x.expr =. (Int_c y))
        static member (=.) (x:bool0,y:double) = bool0(x.expr =. (Dbl_c y))
        
        static member (.<) (x:bool0,y:num0) = bool0(x.expr .< y.expr)
        static member (.<) (x:bool0,y:int) = bool0(x.expr .< (Int_c y))
        static member (.<) (x:bool0,y:double) = bool0(x.expr .< (Dbl_c y))
        
        static member (.<=) (x:bool0,y:num0) = bool0(x.expr .<= y.expr)
        static member (.<=) (x:bool0,y:int) = bool0(x.expr .<= (Int_c y))
        static member (.<=) (x:bool0,y:double) = bool0(x.expr .<= (Dbl_c y))
        
        static member (.>) (x:bool0,y:num0) = bool0(x.expr .> y.expr)
        static member (.>) (x:bool0,y:int) = bool0(x.expr .> (Int_c y))
        static member (.>) (x:bool0,y:double) = bool0(x.expr .> (Dbl_c y))
        
        static member (.>=) (x:bool0,y:num0) = bool0(x.expr .>= y.expr)
        static member (.>=) (x:bool0,y:int) = bool0(x.expr .>= (Int_c y))
        static member (.>=) (x:bool0,y:double) = bool0(x.expr .>= (Dbl_c y))
        
    ///<summary>数学関数</summary>
    and asm =
        ///<summary>虚数単位</summary>
        static member uj with get() =
            match p.lang with
            |F   -> 
                p.var.setUniqVar(Zt,A0,"uj","(0d0,1d0)")
                num0(Zt, Var "uj")
            |C -> 
                //#defineで定義済み
                num0(Zt, Var "uj")
            |T   ->
                p.var.setUniqVar(Zt,A0,"\\mathrm{j}","(0d0,1d0)")
                num0(Zt, Var "\\mathrm{j}")
            |H   ->
                p.var.setUniqVar(Zt,A0,"&ImaginaryI;","(0d0,1d0)")
                num0(Zt, Var "<mi>&ImaginaryI;</mi>")
        ///<summary>円周率</summary>
        static member pi with get() = 
            match p.lang with
            |F   ->
                p.var.setUniqVar(Dt,A0,"pi","3.14159265358979d0")
                num0(Dt, Var "pi")
            |C ->
                p.var.setUniqVar(Dt,A0,"pi","3.14159265358979")
                num0(Dt, Var "pi")
            |T   ->
                p.var.setUniqVar(Dt,A0,"\\pi","3.14159265358979")
                num0(Dt, Var "\\pi")
            |H   ->
                p.var.setUniqVar(Dt,A0,"&pi;","3.14159265358979")
                num0(Dt, Var "<mi>&pi;</mi>")
        ///<summary>2πj</summary>
        static member j2p with get() = 2*asm.pi*asm.uj
        
        static member todouble(x:num0) = num0(Dt, match p.lang with |F -> Formula("dble("+x.code+")") |_ -> Formula("(double)("+x.code+")"))
        static member toint(x:num0) = num0(It 4, match p.lang with |F -> Formula("int("+x.code+")") |_ -> Formula("(int)("+x.code+")"))
        
        ///<summary>累乗</summary>
        static member pow(x:num0, y:num0) = num0(Etype.prior(x.etype,y.etype),Expr.pow(x.expr,y.expr))
        static member pow(x:num0, y:int) = num0(Etype.prior(x.etype,It 4),Expr.pow(x.expr,Int_c y))
        static member pow(x:num0, y:double) = num0(Etype.prior(x.etype,Dt),Expr.pow(x.expr,Dbl_c y))
        static member pow(x:int, y:num0) = num0(Etype.prior(It 4,y.etype),Expr.pow(Int_c x,y.expr))
        static member pow(x:double, y:num0) = num0(Etype.prior(Dt,y.etype),Expr.pow(Dbl_c x,y.expr))
        
        ///<summary>指数関数</summary>
        static member exp (v:num0) = 
            let v = match v.etype with |It _ -> asm.todouble(v) |_ -> v
            let e = match p.lang,v.etype with |C,Zt -> Formula("cexp("+v.code+")") |_ -> Formula("exp("+v.code+")")
            num0(Etype.prior(Dt,v.etype), e)
        
        ///<summary>正弦関数</summary>
        static member sin (v:num0) = 
            let v = match v.etype with |It _ -> asm.todouble(v) |_ -> v
            let e = match p.lang,v.etype with |C,Zt -> Formula("csin("+v.code+")") |_ -> Formula("sin("+v.code+")")
            num0(Etype.prior(Dt,v.etype), e)

        ///<summary>余弦関数</summary>
        static member cos (v:num0) = 
            let v = match v.etype with |It _ -> asm.todouble(v) |_ -> v
            let e = match p.lang,v.etype with |C,Zt -> Formula("ccos("+v.code+")") |_ -> Formula("cos("+v.code+")")
            num0(Etype.prior(Dt,v.etype), e)
        static member tan (v:num0) = 
            let v = match v.etype with |It _ -> asm.todouble(v) |_ -> v
            let e = match p.lang,v.etype with |C,Zt -> Formula("ctan("+v.code+")") |_ -> Formula("tan("+v.code+")")
            num0(Etype.prior(Dt,v.etype), e)

        ///<summary>逆正弦関数</summary>
        static member asin (v:num0) = 
            let v = match v.etype with |It _ -> asm.todouble(v) |_ -> v
            let e = match p.lang,v.etype with |C,Zt -> Formula("casin("+v.code+")") |_ -> Formula("asin("+v.code+")")
            num0(Etype.prior(Dt,v.etype), e)
        ///<summary>逆余弦関数</summary>
        static member acos (v:num0) = 
            let v = match v.etype with |It _ -> asm.todouble(v) |_ -> v
            let e = match p.lang,v.etype with |C,Zt -> Formula("cacos("+v.code+")") |_ -> Formula("acos("+v.code+")")
            num0(Etype.prior(Dt,v.etype), e)
        ///<summary>逆正接関数</summary>
        static member atan (v:num0) = 
            let v = match v.etype with |It _ -> asm.todouble(v) |_ -> v
            let e = match p.lang,v.etype with |C,Zt -> Formula("catan("+v.code+")") |_ -> Formula("atan("+v.code+")")
            num0(Etype.prior(Dt,v.etype), e)
        ///<summary>逆正接関数</summary>
        static member atan2(x:num0, y:num0) = 
            let x = match x.etype with |It _ -> asm.todouble(x) |_ -> x
            let y = match y.etype with |It _ -> asm.todouble(y) |_ -> y
            let e = match p.lang with |F|C|T|H -> Formula("atan2("+x.code+","+y.code+")")
            num0(Etype.prior(x.etype,y.etype), e)
        
        ///<summary>絶対値</summary>
        static member abs (v:num0) = 
            let e = match p.lang,v.etype with |C,Zt -> Formula("cabs("+v.code+")") |C,Dt -> Formula("fabs("+v.code+")") |_ -> Formula("abs("+v.code+")")
            num0(Dt, e)
        ///<summary>自然対数</summary>
        static member log (v:num0) = 
            let v = match v.etype with |It _ -> asm.todouble(v) |_ -> v
            let e = match p.lang,v.etype with |C,Zt -> Formula("clog("+v.code+")") |_ -> Formula("log("+v.code+")")
            num0(Etype.prior(Dt,v.etype), e)
        ///<summary>常用対数</summary>
        static member log10 (v:num0) = 
            let v = match v.etype with |It _ -> asm.todouble(v) |_ -> v
            let e = match p.lang,v.etype with |C,Zt -> Formula("clog10("+v.code+")") |_ -> Formula("log10("+v.code+")")
            num0(Etype.prior(Dt,v.etype), e)
        ///<summary>平方根</summary>
        static member sqrt (v:num0) = 
            let v = match v.etype with |It _ -> asm.todouble(v) |_ -> v
            let e = match p.lang,v.etype with |C,Zt -> Formula("csqrt("+v.code+")") |_ -> Formula("sqrt("+v.code+")")
            num0(Etype.prior(Dt,v.etype), e)
        ///<summary>小数点以下切り捨て</summary>
        static member floor (v:num0) = 
            let v = match v.etype with |It _ -> asm.todouble(v) |_ -> v
            num0(It 4, match p.lang with |F|C|T|H -> Formula("floor("+v.code+")"))
        
        ///<summary>小数点以下切り上げ</summary>
        static member ceil (v:num0) = 
            let e = match p.lang with |F -> Formula("ceiling("+v.code+")") |C|T|H -> Formula("ceil("+v.code+")")
            num0(It 4, e)
        ///<summary>共役複素数</summary>
        static member conj (v:num0) = 
            let e = match p.lang with |F -> Formula("conjg("+v.code+")") |C|T|H -> Formula("conj("+v.code+")")
            num0(Zt, e)
        
        
    [<AutoOpen>]
    module noperator =
        let AND (lst:list<bool0>) = bool0(And <| List.map (fun (x:bool0) -> x.expr) lst)
        let OR (lst:list<bool0>) = bool0(Or <| List.map (fun (x:bool0) -> x.expr) lst)
        
        type System.Int32 with
            ///<summary>整数をint0型に置換</summary>
            member this.I with get() =
                num0(It 4, Int_c this)
                
        type System.Double with
            ///<summary>小数をdouble0型に置換</summary>
            member this.D with get() =
                num0(Dt, Dbl_c this)