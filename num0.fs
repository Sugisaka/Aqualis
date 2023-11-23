(*
Copyright (c) 2022 Jun-ichiro Sugisaka

This software is released under the MIT License.
http://opensource.org/licenses/mit-license.php
*)
namespace Aqualis
    
    open System
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
    type num0(x:Expr) =
        inherit base0(x)
        new(n:int) = num0(Int_c n)
        new(n:double) = num0(Dbl_c n)
        member this.etype with get() = x.etype
        static member (~-) (x:num0) = num0(-x.expr)
        static member ( + ) (x:num0,y:num0) = num0(x.expr + y.expr)
        static member ( + ) (x:num0,y:double) = x+num0(y)
        static member ( + ) (x:num0,y:int) = x+num0(y)
        static member ( + ) (x:double,y:num0) = num0(x)+y
        static member ( + ) (x:int,y:num0) = num0(x)+y
        
        static member ( - ) (x:num0,y:num0) = num0(x.expr - y.expr)
        static member ( - ) (x:num0,y:double) = x-num0(y)
        static member ( - ) (x:num0,y:int) = x-num0(y)
        static member ( - ) (x:double,y:num0) = num0(x)-y
        static member ( - ) (x:int,y:num0) = num0(x)-y
        
        static member ( * ) (x:num0,y:num0) = num0(x.expr * y.expr)
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
                num0(x.expr / y.expr)
        static member ( / ) (x:num0,y:double) = x/num0(y)
        static member ( / ) (x:num0,y:int) = x/num0(double y)
        static member ( / ) (x:double,y:num0) = num0(x)/y
        static member ( / ) (x:int,y:num0) = num0(double x)/y
        
        static member ( ./ ) (x:num0,y:num0) = num0(x.expr ./ y.expr)
        static member ( ./ ) (x:num0,y:double) = x./num0(y)
        static member ( ./ ) (x:num0,y:int) = x./num0(y)
        static member ( ./ ) (x:double,y:num0) = num0(x)./y
        static member ( ./ ) (x:int,y:num0) = num0(x)./y
        
        static member ( % ) (x:num0,y:num0) = num0(x.expr % y.expr)
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
        member x.re with get() = num0(match p.lang with |F -> Formula(Dt,"real("+x.code+")") |C -> Formula(Dt,"creal("+x.code+")") |_ -> Formula(Dt,"Re["+x.code+"]"))
        ///<summary>虚部</summary>
        member x.im with get() = num0(match p.lang with |F -> Formula(Dt,"aimag("+x.code+")") |C -> Formula(Dt,"cimag("+x.code+")") |_ -> Formula(Dt,"Im["+x.code+"]"))
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
        
        member this.eval with get() =
            let rec ev(cond:Expr) =
                match cond with
                |Eq(Int_c a,Int_c b) -> a=b
                |Eq(Int_c a,Dbl_c b) -> (double a)=b
                |Eq(Dbl_c a,Int_c b) -> a=(double b)
                |Eq(Dbl_c a,Dbl_c b) -> (double a)=(double b)
                |NEq(Int_c a,Int_c b) -> a<>b
                |NEq(Int_c a,Dbl_c b) -> (double a)<>b
                |NEq(Dbl_c a,Int_c b) -> a<>(double b)
                |NEq(Dbl_c a,Dbl_c b) -> (double a)<>(double b)
                |Greater(Int_c a,Int_c b) -> a>b
                |Greater(Int_c a,Dbl_c b) -> (double a)>b
                |Greater(Dbl_c a,Int_c b) -> a>(double b)
                |Greater(Dbl_c a,Dbl_c b) -> (double a)>(double b)
                |GreaterEq(Int_c a,Int_c b) -> a>=b
                |GreaterEq(Int_c a,Dbl_c b) -> (double a)>=b
                |GreaterEq(Dbl_c a,Int_c b) -> a>=(double b)
                |GreaterEq(Dbl_c a,Dbl_c b) -> (double a)>=(double b)
                |Less(Int_c a,Int_c b) -> a<b
                |Less(Int_c a,Dbl_c b) -> (double a)<b
                |Less(Dbl_c a,Int_c b) -> a<(double b)
                |Less(Dbl_c a,Dbl_c b) -> (double a)<(double b)
                |LessEq(Int_c a,Int_c b) -> a<=b
                |LessEq(Int_c a,Dbl_c b) -> (double a)<=b
                |LessEq(Dbl_c a,Int_c b) -> a<=(double b)
                |LessEq(Dbl_c a,Dbl_c b) -> (double a)<=(double b)
                |AND lst ->
                    List.fold (fun acc x -> acc && (ev x)) true lst
                |OR lst ->
                    List.fold (fun acc x -> acc || (ev x)) false lst
                |_ ->
                    printfn "%s" ("Error: condition invalid. "+cond.ToString())
                    false
            ev this.expr
            
    ///<summary>数学関数</summary>
    and asm =
        ///<summary>虚数単位</summary>
        static member uj with get() =
            match p.lang with
            |F   -> 
                p.var.setUniqVar(Zt,A0,"uj","(0d0,1d0)")
                num0(Var(Zt,"uj"))
            |C -> 
                //#defineで定義済み
                num0(Var(Zt,"uj"))
            |T   ->
                p.var.setUniqVar(Zt,A0,"\\mathrm{j}","(0d0,1d0)")
                num0(Var(Zt,"\\mathrm{j}"))
            |H   ->
                p.var.setUniqVar(Zt,A0,"\\mathrm{j}","(0d0,1d0)")
                num0(Var(Zt,"\\mathrm{j}"))
        ///<summary>円周率</summary>
        static member pi with get() = 
            if p.isEmpty then
                num0(Dbl_c Math.PI)
            else
                match p.lang with
                |F   ->
                    p.var.setUniqVar(Dt,A0,"pi","3.14159265358979d0")
                    num0(Var(Dt,"pi"))
                |C ->
                    p.var.setUniqVar(Dt,A0,"pi","3.14159265358979")
                    num0(Var(Dt,"pi"))
                |T   ->
                    p.var.setUniqVar(Dt,A0,"\\pi","3.14159265358979")
                    num0(Var(Dt,"\\pi"))
                |H   ->
                    p.var.setUniqVar(Dt,A0,"\\pi","3.14159265358979")
                    num0(Var(Dt,"\\pi"))
        ///<summary>2πj</summary>
        static member j2p with get() = 2*asm.pi*asm.uj
        
        static member todouble(x:num0) = 
            let rec dbl (e:Expr) =
                match p.lang,e with
                |_,Int_c x  -> Dbl_c (double x)
                |_,Dbl_c x  -> Dbl_c x
                |_,Add(_,a,b) -> (dbl a)+(dbl b)
                |_,Sub(_,a,b) -> (dbl a)-(dbl b)
                |_,Mul(_,a,b) -> (dbl a)*(dbl b)
                |_,Div(_,a,b) -> (dbl a)/(dbl b)
                |F,_          -> Formula(Dt,"dble("+e.code+")")
                |T,_          -> Formula(Dt,"\\mathrm{double}("+e.code+")")
                |H,_          -> Formula(Dt,"\\mathrm{double}("+e.code+")")
                |_,_          -> Formula(Dt,"(double)("+e.code+")")
            num0(dbl x.expr)
        static member toint(x:num0) =
            let rec it (e:Expr) =
                match p.lang,e with
                |_,Int_c x  -> Int_c x
                |_,Dbl_c x  -> Int_c (int(floor x))
                |_,Add(_,a,b) -> (it a)+(it b)
                |_,Sub(_,a,b) -> (it a)-(it b)
                |_,Mul(_,a,b) -> (it a)*(it b)
                |_,Div(_,a,b) -> (it a)/(it b)
                |F,_          -> Formula(Dt,"int("+e.code+")")
                |T,_          -> Formula(Dt,"\\mathrm{integer}("+e.code+")")
                |H,_          -> Formula(Dt,"\\mathrm{integer}("+e.code+")")
                |_,_          -> Formula(Dt,"(int)("+e.code+")")
            num0(it x.expr)
        ///<summary>累乗</summary>
        static member pow(x:num0, y:num0) = 
            match x.expr,y.expr with
            |Int_c x, Int_c y -> num0(Dbl_c <| Math.Pow(x,y))
            |Int_c x, Dbl_c y -> num0(Dbl_c <| Math.Pow(x,y))
            |Dbl_c x, Int_c y -> num0(Dbl_c <| Math.Pow(x,y))
            |Dbl_c x, Dbl_c y -> num0(Dbl_c <| Math.Pow(x,y))
            |_ ->
                num0(Expr.pow(x.expr,y.expr))
        static member pow(x:num0, y:int) = num0(Expr.pow(x.expr,Int_c y))
        static member pow(x:num0, y:double) = num0(Expr.pow(x.expr,Dbl_c y))
        static member pow(x:int, y:num0) = num0(Expr.pow(Int_c x,y.expr))
        static member pow(x:double, y:num0) = num0(Expr.pow(Dbl_c x,y.expr))
        
        ///<summary>指数関数</summary>
        static member exp (v:num0) = 
            match v.expr with
            |Int_c v -> num0(Dbl_c <| Math.Exp(v))
            |Dbl_c v -> num0(Dbl_c <| Math.Exp(v))
            |_ ->
                let v = match v.etype with |It _ -> asm.todouble(v) |_ -> v
                num0(Exp(Dt%%(v.etype),v.expr))
        
        ///<summary>正弦関数</summary>
        static member sin (v:num0) = 
            match v.expr with
            |Int_c v -> num0(Dbl_c <| Math.Sin(v))
            |Dbl_c v -> num0(Dbl_c <| Math.Sin(v))
            |_ ->
                let v = match v.etype with |It _ -> asm.todouble(v) |_ -> v
                num0(Sin(Dt%%(v.etype),v.expr))

        ///<summary>余弦関数</summary>
        static member cos (v:num0) = 
            match v.expr with
            |Int_c v -> num0(Dbl_c <| Math.Cos(v))
            |Dbl_c v -> num0(Dbl_c <| Math.Cos(v))
            |_ ->
                let v = match v.etype with |It _ -> asm.todouble(v) |_ -> v
                num0(Cos(Dt%%(v.etype),v.expr))
        static member tan (v:num0) = 
            match v.expr with
            |Int_c v -> num0(Dbl_c <| Math.Tan(v))
            |Dbl_c v -> num0(Dbl_c <| Math.Tan(v))
            |_ ->
                let v = match v.etype with |It _ -> asm.todouble(v) |_ -> v
                num0(Tan(Dt%%(v.etype),v.expr))

        ///<summary>逆正弦関数</summary>
        static member asin (v:num0) = 
            match v.expr with
            |Int_c v -> num0(Dbl_c <| Math.Asin(v))
            |Dbl_c v -> num0(Dbl_c <| Math.Asin(v))
            |_ ->
                let v = match v.etype with |It _ -> asm.todouble(v) |_ -> v
                num0(Asin(Dt%%(v.etype),v.expr))
                
        ///<summary>逆余弦関数</summary>
        static member acos (v:num0) = 
            match v.expr with
            |Int_c v -> num0(Dbl_c <| Math.Acos(v))
            |Dbl_c v -> num0(Dbl_c <| Math.Acos(v))
            |_ ->
                let v = match v.etype with |It _ -> asm.todouble(v) |_ -> v
                num0(Acos(Dt%%(v.etype),v.expr))
                
        ///<summary>逆正接関数</summary>
        static member atan (v:num0) = 
            match v.expr with
            |Int_c v -> num0(Dbl_c <| Math.Atan(v))
            |Dbl_c v -> num0(Dbl_c <| Math.Atan(v))
            |_ ->
                let v = match v.etype with |It _ -> asm.todouble(v) |_ -> v
                num0(Atan(Dt%%(v.etype),v.expr))
                
        ///<summary>逆正接関数</summary>
        static member atan2(x:num0, y:num0) = 
            match x.expr,y.expr with
            |Int_c x, Int_c y -> num0(Dbl_c <| Math.Atan2(x,y))
            |Int_c x, Dbl_c y -> num0(Dbl_c <| Math.Atan2(x,y))
            |Dbl_c x, Int_c y -> num0(Dbl_c <| Math.Atan2(x,y))
            |Dbl_c x, Dbl_c y -> num0(Dbl_c <| Math.Atan2(x,y))
            |_ ->
                let x = match x.etype with |It _ -> asm.todouble(x) |_ -> x
                let y = match y.etype with |It _ -> asm.todouble(y) |_ -> y
                num0(Atan2(x.expr,y.expr))
                
        ///<summary>絶対値</summary>
        static member abs (v:num0) = 
            match v.expr with
            |Int_c v -> num0(Dbl_c <| Math.Abs(v))
            |Dbl_c v -> num0(Dbl_c <| Math.Abs(v))
            |_       -> num0(Abs(Dt,v.expr))
                
        ///<summary>自然対数</summary>
        static member log (v:num0) = 
            match v.expr with
            |Int_c v -> num0(Dbl_c <| Math.Log(v))
            |Dbl_c v -> num0(Dbl_c <| Math.Log(v))
            |_ ->
                let v = match v.etype with |It _ -> asm.todouble(v) |_ -> v
                num0(Log(Dt%%(v.etype),v.expr))
                
        ///<summary>常用対数</summary>
        static member log10 (v:num0) = 
            match v.expr with
            |Int_c v -> num0(Dbl_c <| Math.Log10(v))
            |Dbl_c v -> num0(Dbl_c <| Math.Log10(v))
            |_ ->
                let v = match v.etype with |It _ -> asm.todouble(v) |_ -> v
                num0(Log10(Dt%%(v.etype),v.expr))
                
        ///<summary>平方根</summary>
        static member sqrt (v:num0) = 
            match v.expr with
            |Int_c v -> num0(Dbl_c <| Math.Sqrt(double v))
            |Dbl_c v -> num0(Dbl_c <| Math.Sqrt(v))
            |_ ->
                let v = match v.etype with |It _ -> asm.todouble(v) |_ -> v
                num0(Sqrt(Dt%%(v.etype),v.expr))
                
        ///<summary>小数点以下切り捨て</summary>
        static member floor (v:num0) = 
            match v.expr with
            |Int_c v -> num0(Int_c v)
            |Dbl_c v -> num0(Int_c <| int(floor(v)))
            |_ ->
                let v = match v.etype with |It _ -> asm.todouble(v) |_ -> v
                num0(match p.lang with |F|C -> Formula(v.etype,"floor("+v.code+")") |T|H -> Formula(v.etype,"\\mathrm{floor}("+v.code+")"))
                
        ///<summary>小数点以下切り上げ</summary>
        static member ceil (v:num0) = 
            match v.expr with
            |Int_c v -> num0(Int_c v)
            |Dbl_c v -> num0(Int_c <| int(ceil(v)))
            |_ ->
                let e = match p.lang with |F -> Formula(It 4,"ceiling("+v.code+")") |C -> Formula(It 4,"ceil("+v.code+")") |T|H -> Formula(It 4,"\\mathrm{ceil}("+v.code+")")
                num0(e)
                
        ///<summary>共役複素数</summary>
        static member conj (v:num0) = 
            match v.etype with
            |Zt ->
                let e = match p.lang,v.expr with |F,_ -> Formula(Zt,"conjg("+v.code+")") |C,_ -> Formula(Zt,"conj("+v.code+")") |(T|H),(Var _|Int_c _|Dbl_c _) -> Formula(Zt,v.code+"^*") |(T|H),_ -> Formula(Zt,"\\left["+v.code+"\\right]^*")
                num0(e)
            |_ -> v
            
    [<AutoOpen>]
    module noperator =
        let And (lst:list<bool0>) = bool0(AND <| List.map (fun (x:bool0) -> x.expr) lst)
        let Or (lst:list<bool0>) = bool0(OR <| List.map (fun (x:bool0) -> x.expr) lst)
        
        type System.Int32 with
            ///<summary>整数をint0型に置換</summary>
            member this.I with get() =
                num0(Int_c this)
                
        type System.Double with
            ///<summary>小数をdouble0型に置換</summary>
            member this.D with get() =
                num0(Dbl_c this)