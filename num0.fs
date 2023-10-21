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
                asm.todouble(x) / asm.todouble(y)
            |It _,_ ->
                asm.todouble(x) / y
            |_,It _ ->
                x / asm.todouble(y)
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

        static member (.=) (x:num0,y:num0) = bool0(x.expr .= y.expr)
        static member (.=) (x:int,y:num0) = bool0((Int_c x) .= y.expr)
        static member (.=) (x:double,y:num0) = bool0((Dbl_c x) .= y.expr)
        static member (.=) (x:num0,y:int) = bool0(x.expr .= (Int_c y))
        static member (.=) (x:num0,y:double) = bool0(x.expr .= (Dbl_c y))
        
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
        static member (.=) (x:bool0,y:num0) = bool0(x.expr .= y.expr)
        static member (.=) (x:bool0,y:int) = bool0(x.expr .= (Int_c y))
        static member (.=) (x:bool0,y:double) = bool0(x.expr .= (Dbl_c y))
        
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
                p.var.setUniqVar(Zt,A0,"\\mathrm{j}","(0d0,1d0)")
                num0(Zt, Var "\\mathrm{j}")
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
                p.var.setUniqVar(Dt,A0,"\\pi","3.14159265358979")
                num0(Dt, Var "\\pi")
        ///<summary>2πj</summary>
        static member j2p with get() = 2*asm.pi*asm.uj
        
        static member todouble(x:num0) = 
            let rec dbl (e:Expr) =
                match p.lang,e with
                |_,Int_c x  -> Dbl_c (double x)
                |_,Dbl_c x  -> Dbl_c x
                |_,Add(a,b) -> (dbl a)+(dbl b)
                |_,Sub(a,b) -> (dbl a)-(dbl b)
                |_,Mul(a,b) -> (dbl a)*(dbl b)
                |_,Div(a,b) -> (dbl a)/(dbl b)
                |F,_        -> Formula("dble("+e.code+")")
                |T,_        -> Formula("\\mathrm{double}("+e.code+")")
                |H,_        -> Formula("\\mathrm{double}("+e.code+")")
                |_,_        -> Formula("(double)("+e.code+")")
            num0(Dt, dbl x.expr)
        static member toint(x:num0) =
            let rec it (e:Expr) =
                match p.lang,e with
                |_,Int_c x  -> Int_c x
                |_,Dbl_c x  -> Int_c (int(floor x))
                |_,Add(a,b) -> (it a)+(it b)
                |_,Sub(a,b) -> (it a)-(it b)
                |_,Mul(a,b) -> (it a)*(it b)
                |_,Div(a,b) -> (it a)/(it b)
                |F,_        -> Formula("int("+e.code+")")
                |T,_        -> Formula("\\mathrm{integer}("+e.code+")")
                |H,_        -> Formula("\\mathrm{integer}("+e.code+")")
                |_,_        -> Formula("(int)("+e.code+")")
            num0(It 4, it x.expr)
        ///<summary>累乗</summary>
        static member pow(x:num0, y:num0) = num0(Etype.prior(x.etype,y.etype),Expr.pow(x.expr,y.expr))
        static member pow(x:num0, y:int) = num0(Etype.prior(x.etype,It 4),Expr.pow(x.expr,Int_c y))
        static member pow(x:num0, y:double) = num0(Etype.prior(x.etype,Dt),Expr.pow(x.expr,Dbl_c y))
        static member pow(x:int, y:num0) = num0(Etype.prior(It 4,y.etype),Expr.pow(Int_c x,y.expr))
        static member pow(x:double, y:num0) = num0(Etype.prior(Dt,y.etype),Expr.pow(Dbl_c x,y.expr))
        
        ///<summary>指数関数</summary>
        static member exp (v:num0) = 
            let v = match v.etype with |It _ -> asm.todouble(v) |_ -> v
            let e = match p.lang,v.etype with |C,Zt -> Formula("cexp("+v.code+")") |T,_|H,_ -> Formula("\\exp("+v.code+")") |_ -> Formula("exp("+v.code+")")
            num0(Etype.prior(Dt,v.etype), e)
        
        ///<summary>正弦関数</summary>
        static member sin (v:num0) = 
            let v = match v.etype with |It _ -> asm.todouble(v) |_ -> v
            let e = match p.lang,v.etype with |C,Zt -> Formula("csin("+v.code+")") |T,_|H,_ -> Formula("\\sin("+v.code+")") |_ -> Formula("sin("+v.code+")")
            num0(Etype.prior(Dt,v.etype), e)

        ///<summary>余弦関数</summary>
        static member cos (v:num0) = 
            let v = match v.etype with |It _ -> asm.todouble(v) |_ -> v
            let e = match p.lang,v.etype with |C,Zt -> Formula("ccos("+v.code+")") |T,_|H,_ -> Formula("\\cos("+v.code+")") |_ -> Formula("cos("+v.code+")")
            num0(Etype.prior(Dt,v.etype), e)
        static member tan (v:num0) = 
            let v = match v.etype with |It _ -> asm.todouble(v) |_ -> v
            let e = match p.lang,v.etype with |C,Zt -> Formula("ctan("+v.code+")") |T,_|H,_ -> Formula("\\tan("+v.code+")") |_ -> Formula("tan("+v.code+")")
            num0(Etype.prior(Dt,v.etype), e)

        ///<summary>逆正弦関数</summary>
        static member asin (v:num0) = 
            let v = match v.etype with |It _ -> asm.todouble(v) |_ -> v
            let e = match p.lang,v.etype with |C,Zt -> Formula("casin("+v.code+")") |T,_|H,_ -> Formula("\\arcsin("+v.code+")") |_ -> Formula("asin("+v.code+")")
            num0(Etype.prior(Dt,v.etype), e)
        ///<summary>逆余弦関数</summary>
        static member acos (v:num0) = 
            let v = match v.etype with |It _ -> asm.todouble(v) |_ -> v
            let e = match p.lang,v.etype with |C,Zt -> Formula("cacos("+v.code+")") |T,_|H,_ -> Formula("\\arccos("+v.code+")") |_ -> Formula("acos("+v.code+")")
            num0(Etype.prior(Dt,v.etype), e)
        ///<summary>逆正接関数</summary>
        static member atan (v:num0) = 
            let v = match v.etype with |It _ -> asm.todouble(v) |_ -> v
            let e = match p.lang,v.etype with |C,Zt -> Formula("catan("+v.code+")") |T,_|H,_ -> Formula("\\arctan("+v.code+")") |_ -> Formula("atan("+v.code+")")
            num0(Etype.prior(Dt,v.etype), e)
        ///<summary>逆正接関数</summary>
        static member atan2(x:num0, y:num0) = 
            let x = match x.etype with |It _ -> asm.todouble(x) |_ -> x
            let y = match y.etype with |It _ -> asm.todouble(y) |_ -> y
            let e = match p.lang with |F|C -> Formula("atan2("+x.code+","+y.code+")") |T|H -> Formula("\\arctan(\\frac{"+y.code+"}{"+x.code+"})")
            num0(Etype.prior(x.etype,y.etype), e)
        
        ///<summary>絶対値</summary>
        static member abs (v:num0) = 
            let e = match p.lang,v.etype with |C,Zt -> Formula("cabs("+v.code+")") |C,Dt -> Formula("fabs("+v.code+")") |T,_|H,_ -> Formula("\\left|"+v.code+"\\right|") |_ -> Formula("abs("+v.code+")")
            num0(Dt, e)
        ///<summary>自然対数</summary>
        static member log (v:num0) = 
            let v = match v.etype with |It _ -> asm.todouble(v) |_ -> v
            let e = match p.lang,v.etype with |C,Zt -> Formula("clog("+v.code+")") |T,_|H,_ -> Formula("\\ln("+v.code+")") |_ -> Formula("log("+v.code+")")
            num0(Etype.prior(Dt,v.etype), e)
        ///<summary>常用対数</summary>
        static member log10 (v:num0) = 
            let v = match v.etype with |It _ -> asm.todouble(v) |_ -> v
            let e = match p.lang,v.etype with |C,Zt -> Formula("clog10("+v.code+")") |T,_|H,_ -> Formula("\\log_{10}("+v.code+")") |_ -> Formula("log10("+v.code+")")
            num0(Etype.prior(Dt,v.etype), e)
        ///<summary>平方根</summary>
        static member sqrt (v:num0) = 
            let v = match v.etype with |It _ -> asm.todouble(v) |_ -> v
            let e = match p.lang,v.etype with |C,Zt -> Formula("csqrt("+v.code+")") |T,_|H,_ -> Formula("\\sqrt{"+v.code+"}") |_ -> Formula("sqrt("+v.code+")")
            num0(Etype.prior(Dt,v.etype), e)
        ///<summary>小数点以下切り捨て</summary>
        static member floor (v:num0) = 
            let v = match v.etype with |It _ -> asm.todouble(v) |_ -> v
            num0(It 4, match p.lang with |F|C -> Formula("floor("+v.code+")") |T|H -> Formula("\\mathrm{floor}("+v.code+")"))
        
        ///<summary>小数点以下切り上げ</summary>
        static member ceil (v:num0) = 
            let e = match p.lang with |F -> Formula("ceiling("+v.code+")") |C -> Formula("ceil("+v.code+")") |T|H -> Formula("\\mathrm{ceil}("+v.code+")")
            num0(It 4, e)
        ///<summary>共役複素数</summary>
        static member conj (v:num0) = 
            let e = match p.lang,v.expr with |F,_ -> Formula("conjg("+v.code+")") |C,_ -> Formula("conj("+v.code+")") |(T|H),(Var _|Int_c _|Dbl_c _) -> Formula(v.code+"^*") |(T|H),_ -> Formula("\\left["+v.code+"\\right]^*")
            num0(Zt, e)        
            
    [<AutoOpen>]
    module noperator =
        let And (lst:list<bool0>) = bool0(AND <| List.map (fun (x:bool0) -> x.expr) lst)
        let Or (lst:list<bool0>) = bool0(OR <| List.map (fun (x:bool0) -> x.expr) lst)
        
        type System.Int32 with
            ///<summary>整数をint0型に置換</summary>
            member this.I with get() =
                num0(It 4, Int_c this)
                
        type System.Double with
            ///<summary>小数をdouble0型に置換</summary>
            member this.D with get() =
                num0(Dt, Dbl_c this)