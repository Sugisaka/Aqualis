(*
Copyright (c) 2022 Jun-ichiro Sugisaka

This software is released under the MIT License.
http://opensource.org/licenses/mit-license.php
*)
namespace Aqualis
    
    open System
    open System.IO
    open System.Text
    open Aqualis_base

    ///<summary>数学関数</summary>
    type asm =
        ///<summary>虚数単位</summary>
        static member uj with get() =
            match p.lang with
              |F   -> 
                p.param.vreg(Zt,A0,"uj","(0d0,1d0)")
                Var(Zt,"uj",[])
              |C89 //実部・虚部に分けて計算するので変数は不要
              |C99 -> 
                //#defineで定義済み
                Var(Zt,"uj",[])
              |T   ->
                p.param.vreg(Zt,A0,"uj","(0d0,1d0)")
                Var(Zt,"uj",[])
              |H   ->
                p.param.vreg(Zt,A0,"&ImaginaryI;","(0d0,1d0)")
                Var(Zt,"<mi>&ImaginaryI;</mi>",[])
              |NL ->
                NaN
        ///<summary>円周率</summary>
        static member pi with get() = 
            match p.lang with
              |F   ->
                p.param.vreg(Dt,A0,"pi","3.14159265358979d0")
                Var(Dt,"pi",[])
              |C89 ->
                p.param.vreg(Dt,A0,"pi","3.14159265358979")
                Var(Dt,"pi",[])
              |C99 ->
                p.param.vreg(Dt,A0,"pi","3.14159265358979")
                Var(Dt,"pi",[])
              |T   ->
                p.param.vreg(Dt,A0,"pi","3.14159265358979")
                Var(Dt,"pi",[])
              |H   ->
                p.param.vreg(Dt,A0,"&pi;","3.14159265358979")
                Var(Dt,"<mi>&pi;</mi>",[])
              |NL  ->
                Dbl_e <| Math.PI
        ///<summary>2πj</summary>
        static member j2p with get() = 2*asm.pi*asm.uj
        ///<summary>累乗</summary>
        static member pow(x:num0, y:num0) =
            let (<--) (x:num0) (y:num0) = num0.subst false x y
            let p = p.param
            match p.lang,x.etype,y.etype with
              |C89,Zt,_|C89,_,Zt ->
                let xx = x.code
                let yy = y.code
                let (_,_,c1) = xx.str
                let (_,_,c2) = yy.str
                let a = Var(p.getvar(Dt))
                let t = Var(p.getvar(Dt))
                let z = Var(p.getvar(Zt))
                a <-- asm.sqrt(xx.re*xx.re+xx.im*xx.im)
                t <-- asm.atan2(xx.im,xx.re)
                let ff:num0 = yy.re*t+yy.im*asm.log(a)
                z.re <-- asm.pow(a,yy.re)*asm.exp(-yy.im*t)*asm.cos(ff)
                z.im <-- asm.pow(a,yy.re)*asm.exp(-yy.im*t)*asm.sin(ff)
                Var(Zt,z.name,[z;t;a]@c2@c1)
              |_ ->
                match p.lang,x,y with
                  (* 0^0 *)
                  |_,(Int_e 0|Dbl_e 0.0),(Int_e 0|Dbl_e 0.0) -> num0.NaN
                  (* 0^y *)
                  |_,(Int_e 0|Dbl_e 0.0),_ -> Int_e 0
                  (* x^0 *)
                  |_,_,(Int_e 0|Dbl_e 0.0) -> Int_e 1
                  (* [整数定数]^[整数定数] *)
                  |(F|C89|C99),Int_e v1,Int_e v2 -> Dbl_e(double(v1)**double(v2))
                  (* [整数定数]^[小数定数] *)
                  |(F|C89|C99),Int_e v1,Dbl_e v2 -> Dbl_e((double v1)**v2)
                  (* [小数定数]^[整数定数] *)
                  |(F|C89|C99),Dbl_e v1,Int_e v2 -> Dbl_e(v1**(double v2))
                  (* [小数定数]^[小数定数] *)
                  |(F|C89|C99),Dbl_e v1,Dbl_e v2 -> Dbl_e(v1**v2)
                  (* [負の整数定数]^y *)
                  |_,Int_e v1,_ when v1<0   -> asm.pow(Par(x.etype,x),y)
                  (* [負の小数定数]^y *)
                  |_,Dbl_e v1,_ when v1<0.0 -> asm.pow(Par(x.etype,x),y)
                  (* x^[負の整数定数] *)
                  |_,_,Int_e v2 when v2<0   -> asm.pow(x,Par(y.etype,y))
                  (* x^[負の小数定数] *)
                  |_,_,Dbl_e v2 when v2<0.0 -> asm.pow(x,Par(y.etype,y))
                  (* x = [負号付] or (x1+x2+…) or (x1-x2) or (x1*x2) or (x1/x2) 
                     y = [負号付] or (y1+y2+…) or (y1-y2) or (y1*y2) or (y1/y2) *)
                  |_,(Inv _|Add _|Sub _|Mul _|Div _),(Inv _|Add _|Sub _|Mul _|Div _) -> asm.pow(Par(x.etype,x),Par(y.etype,y))
                  (* x = [負号付] or (x1+x2+…) or (x1-x2) or (x1*x2) or (x1/x2) *)
                  |_,(Inv _|Add _|Sub _|Mul _|Div _),_ -> asm.pow(Par(x.etype,x),y)
                  (* y = [負号付] or (y1+y2+…) or (y1-y2) or (y1*y2) or (y1/y2) *)
                  |_,_,(Inv _|Add _|Sub _|Mul _|Div _) -> asm.pow(x,Par(y.etype,y))
                  (* [複素数]^y or x^[複素数] *)
                  |_,Var(Zt,_,_),_|_,_,Var(Zt,_,_) -> Pow(Zt,x,y)
                  (* [小数]^y or x^[小数] *)
                  |_,Var(Dt,_,_),_|_,_,Var(Dt,_,_) -> Pow(Dt,x,y)
                  (* [整数]^[整数] *)
                  |_,Var(_ ,_,_),_|_,_,Var(_ ,_,_) -> Pow(It 4,x,y)
                  (* x^y *)
                  |_ -> Pow (num0.ptype(x,y),x,y)
        ///<summary>累乗</summary>
        static member pow(x:int , y:num0) = asm.pow(Int_e x,y)
        ///<summary>累乗</summary>
        static member pow(x:num0, y:int ) = asm.pow(x,Int_e y)
        ///<summary>累乗</summary>
        static member pow(x:num0, y:double) = asm.pow(x,Dbl_e y)
        ///<summary>累乗</summary>
        static member pow(x:double, y:num0) = asm.pow(Dbl_e x,y)
        ///<summary>累乗</summary>
        static member pow(x:num0, (re:double,im:double)) = asm.pow(x,re+asm.uj*(Dbl_e im))
        ///<summary>累乗</summary>
        static member pow((re:double,im:double), y:num0) = asm.pow(re+asm.uj*(Dbl_e im),y)
        ///<summary>指数関数</summary>
        static member exp (v:num0) = 
            let (<--) (x:num0) (y:num0) = num0.subst false x y
            let p = p.param
            match p.lang,v.etype with
              |C89,Zt ->
                let vv = v.code
                let (_,_,c) = vv.str
                let z = Var(p.getvar(Zt))
                z.re <-- asm.exp(vv.re)*asm.cos(vv.im)
                z.im <-- asm.exp(vv.re)*asm.sin(vv.im)
                Var(Zt,z.name,[z]@c)
              |_ ->
                match v with
                  |Int_e 0 -> Int_e 1
                  |Dbl_e 0.0 -> Dbl_e 1.0
                  |Int_e n -> Dbl_e(exp(double n))
                  |Dbl_e x -> Dbl_e(exp(x))
                  |_       -> Exp(v.etype,v)
        ///<summary>指数関数</summary>
        static member exp (v:int) =
            match p.lang with
              |H -> Exp(Dt,Int_e v)
              |_ -> Dbl_e(exp(double v))
        ///<summary>指数関数</summary>
        static member exp (v:double) =
            match p.lang with
              |H -> Exp(Dt,Dbl_e v)
              |_ -> Dbl_e(exp(v))
        ///<summary>正弦関数</summary>
        static member sin (v:num0) = 
            let (<--) (x:num0) (y:num0) = num0.subst false x y
            match p.lang,v.etype with
              |C89,Zt ->
                let p = p.param
                let vv = v.code
                let (_,_,c) = vv.str
                let z = Var(p.getvar(Zt))
                z.re <-- 0.5*(asm.exp(vv.im)+asm.exp(-vv.im))*asm.sin(vv.re)
                z.im <-- 0.5*(asm.exp(vv.im)-asm.exp(-vv.im))*asm.cos(vv.re)
                Var(Zt,z.name,[z]@c)
              |_ ->
                match v with
                  |Int_e 0 |Dbl_e 0.0 -> Dbl_e 0.0
                  |Int_e n -> Dbl_e(sin(double n))
                  |Dbl_e x -> Dbl_e(sin(x))
                  |_ -> Sin(num0.ptype(v,Dt),v)
        ///<summary>正弦関数</summary>
        static member sin (v:int) =
            match p.lang with
              |H -> Sin(Dt,Int_e v)
              |_ -> Dbl_e(sin(double v))
        ///<summary>正弦関数</summary>
        static member sin (v:double) =
            match p.lang with
              |H -> Sin(Dt,Dbl_e v)
              |_ -> Dbl_e(sin v)
        ///<summary>余弦関数</summary>
        static member cos (v:num0) = 
            let (<--) (x:num0) (y:num0) = num0.subst false x y
            match p.lang,v.etype with
              |C89,Zt ->
                let p = p.param
                let vv = v.code
                let (_,_,c) = vv.str
                let z = Var(p.getvar(Zt))
                z.re <--  0.5*(asm.exp(vv.im)+asm.exp(-vv.im))*asm.cos(vv.re)
                z.im <-- -0.5*(asm.exp(vv.im)-asm.exp(-vv.im))*asm.sin(vv.re)
                Var(Zt,z.name,[z]@c)
              |_ ->
                match v with
                  |Int_e 0 |Dbl_e 0.0 -> Dbl_e 1.0
                  |Int_e n -> Dbl_e(cos(double n))
                  |Dbl_e x -> Dbl_e(cos(x))
                  |_ -> Cos(num0.ptype(v,Dt),v)
        ///<summary>余弦関数</summary>
        static member cos (v:int) =
            match p.lang with
              |H -> Cos(Dt,Int_e v)
              |_ -> Dbl_e(cos(double v))
        ///<summary>余弦関数</summary>
        static member cos (v:double) =
            match p.lang with
              |H -> Cos(Dt,Dbl_e v)
              |_ -> Dbl_e(cos v)
        ///<summary>正接関数</summary>
        static member tan (v:num0) = 
            let (<--) (x:num0) (y:num0) = num0.subst false x y
            match p.lang,v.etype with
              |C89,Zt ->
                let p = p.param
                let vv = v.code
                let (_,_,c) = vv.str
                let vsre = Var(p.getvar(Dt))
                let vsim = Var(p.getvar(Dt))
                let vcre = Var(p.getvar(Dt))
                let vcim = Var(p.getvar(Dt))
                let a = Var(p.getvar(Dt))
                let b = Var(p.getvar(Zt))
                vsre <--  0.5*(asm.exp(vv.im)+asm.exp(-vv.im))*asm.sin(vv.re)
                vsim <--  0.5*(asm.exp(vv.im)-asm.exp(-vv.im))*asm.cos(vv.re)
                vcre <--  0.5*(asm.exp(vv.im)+asm.exp(-vv.im))*asm.cos(vv.re)
                vcim <-- -0.5*(asm.exp(vv.im)-asm.exp(-vv.im))*asm.sin(vv.re)
                a <-- vcre * vcre + vcim * vcim
                b.re <-- (vsre * vcre + vsim * vcim)/a
                b.im <-- (vsim * vcre - vsre * vcim)/a
                Var(Zt,b.name,[b;a;vcim;vcre;vsim;vsre]@c)
              |_ ->
                match v with
                  |Int_e 0 |Dbl_e 0.0 -> Dbl_e 0.0
                  |Int_e n -> Dbl_e(tan(double n))
                  |Dbl_e x -> Dbl_e(tan(x))
                  |_ -> Tan(num0.ptype(v,Dt),v)
        ///<summary>正接関数</summary>
        static member tan (v:int) =
            match p.lang with
              |H -> Tan(Dt,Int_e v)
              |_ -> Dbl_e(tan(double v))
        ///<summary>正接関数</summary>
        static member tan (v:double) =
            match p.lang with
              |H -> Tan(Dt,Dbl_e v)
              |_ -> Dbl_e(tan v)
        ///<summary>逆正弦関数</summary>
        static member asin (v:num0) = 
            match p.lang,v.etype with
              |C89,Zt ->
                NaN
              |_ ->
                match v with
                  |Int_e 0 -> Dbl_e 0.0
                  |Int_e n -> Dbl_e(asin(double n))
                  |Dbl_e x -> Dbl_e(asin(x))
                  |_ -> Asin(num0.ptype(v,Dt),v)
        ///<summary>逆正弦関数</summary>
        static member asin (v:int) =
            match p.lang with
              |H -> Asin(Dt,Int_e v)
              |_ -> Dbl_e(asin(double v))
        ///<summary>逆正弦関数</summary>
        static member asin (v:double) =
            match p.lang with
              |H -> Asin(Dt,Dbl_e v)
              |_ -> Dbl_e(asin v)
        ///<summary>逆余弦関数</summary>
        static member acos (v:num0) =
            match p.lang,v.etype with
              |C89,Zt ->
                NaN
              |_ ->
                match v with
                  |Int_e n -> Dbl_e(acos(double n))
                  |Dbl_e x -> Dbl_e(acos(x))
                  |_ -> Acos(num0.ptype(v,Dt),v)
        ///<summary>逆余弦関数</summary>
        static member acos (v:int) =
            match p.lang with
              |H -> Acos(Dt,Int_e v)
              |_ -> Dbl_e(acos(double v))
        ///<summary>逆余弦関数</summary>
        static member acos (v:double) =
            match p.lang with
              |H -> Acos(Dt,Dbl_e v)
              |_ -> Dbl_e(acos v)
        ///<summary>逆正接関数</summary>
        static member atan (v:num0) = 
            match p.lang,v.etype with
              |C89,Zt ->
                NaN
              |_ ->
                match v with
                  |Int_e 0 |Dbl_e 0.0 -> Dbl_e 0.0
                  |Int_e n -> Dbl_e(atan(double n))
                  |Dbl_e x -> Dbl_e(atan(x))
                  |_ -> Atan(num0.ptype(v,Dt),v)
        ///<summary>逆正接関数</summary>
        static member atan (v:int) = Dbl_e(atan(double v))
        ///<summary>逆正接関数</summary>
        static member atan (v:double) = Dbl_e(atan v)
        ///<summary>逆正接関数（atan2(y,x)=atan(y/x)）</summary>
        static member atan2 (u:num0,v:num0) = 
            match p.lang,u.etype,v.etype with
              |C89,Zt,_|C89,_,Zt ->
                NaN
              |_ ->
                match u,v with
                  |Int_e n,Int_e m -> Dbl_e(atan2 (double n) (double m))
                  |Dbl_e n,Dbl_e m -> Dbl_e(atan2 n m)
                  |Int_e n,_ -> Atan2(Dt,Dbl_e(double n),v)
                  |_,Int_e m -> Atan2(Dt,u,Dbl_e(double m))
                  |_ -> Atan2(Dt,u,v)
        ///<summary>逆正接関数（atan2(y,x)=atan(y/x)）</summary>
        static member atan2 (u:num0,v:int) = asm.atan2(u,Int_e v)
        ///<summary>逆正接関数（atan2(y,x)=atan(y/x)）</summary>
        static member atan2 (u:num0,v:double) = asm.atan2(u,Dbl_e v)
        ///<summary>逆正接関数（atan2(y,x)=atan(y/x)）</summary>
        static member atan2 (u:int,v:num0) = asm.atan2(Int_e u,v)
        ///<summary>逆正接関数（atan2(y,x)=atan(y/x)）</summary>
        static member atan2 (u:double,v:num0) = asm.atan2(Dbl_e u,v)
        ///<summary>逆正接関数（atan2(y,x)=atan(y/x)）</summary>
        static member atan2 (u:int,v:int) =
            match p.lang with
              |H -> Atan2(Dt,Int_e u,Int_e v)
              |_ -> Dbl_e(atan2 (double u) (double v))
        ///<summary>逆正接関数（atan2(y,x)=atan(y/x)）</summary>
        static member atan2 (u:int,v:double) =
            match p.lang with
              |H -> Atan2(Dt,Int_e u,Dbl_e v)
              |_ -> Dbl_e(atan2 (double u) v)
        ///<summary>逆正接関数（atan2(y,x)=atan(y/x)）</summary>
        static member atan2 (u:double,v:int) =
            match p.lang with
              |H -> Atan2(Dt,Dbl_e u,Int_e v)
              |_ -> Dbl_e(atan2 u (double v))
        ///<summary>逆正接関数（atan2(y,x)=atan(y/x)）</summary>
        static member atan2 (u:double,v:double) =
            match p.lang with
              |H -> Atan2(Dt,Dbl_e u,Dbl_e v)
              |_ -> Dbl_e(atan2 u v)
        ///<summary>絶対値関数</summary>
        static member abs (v:num0) =
            let (<--) (x:num0) (y:num0) = num0.subst false x y
            match p.lang,v.etype with
              |C89,Zt ->
                let p = p.param
                let vv = v.code
                let (_,_,c) = vv.str
                let d = Var(p.getvar(Dt))
                d <-- asm.sqrt(vv.re*vv.re+vv.im*vv.im)
                Var(Dt,d.name,d::c)
              |_ ->
                Abs((match v.etype with |Zt -> Dt |t -> t),v)
        ///<summary>絶対値関数</summary>
        static member abs (v:int) = Int_e(abs v)
        ///<summary>絶対値関数</summary>
        static member abs (v:double) = Dbl_e(abs v)
        ///<summary>対数関数（自然対数）</summary>
        static member log (v:num0) =
            let (<--) (x:num0) (y:num0) = num0.subst false x y
            match p.lang,v.etype with
              |C89,Zt ->
                let p = p.param
                let vv = v.code
                let (_,_,c) = vv.str
                let z = Var(p.getvar(Zt))
                z.re <-- asm.log(asm.sqrt(vv.re*vv.re+vv.im*vv.im))
                z.im <-- asm.atan2(vv.im,vv.re)
                Var(Zt,z.name,[z]@c)
              |_ ->
                Log(num0.ptype(v,Dt),v)
        ///<summary>対数関数（自然対数）</summary>
        static member log (v:int) =
            match p.lang with
              |H -> Log(Dt,Int_e v)
              |_ -> Dbl_e (log(double v))
        ///<summary>対数関数（自然対数）</summary>
        static member log (v:double) =
            match p.lang with
              |H -> Log(Dt,Dbl_e v)
              |_ -> Dbl_e (log v)
        ///<summary>対数関数（常用対数）</summary>
        static member log10 (v:num0) =
            match p.lang,v.etype with
              |_,Zt ->
                asm.log(v)/log(10.0)
              |_,It _ ->
                Log10(Dt,asm.todouble v)
              |_ ->
                Log10(num0.ptype(v,Dt),v)
        ///<summary>対数関数（常用対数）</summary>
        static member log10 (v:int) =
            match p.lang with
              |H -> Log10(Dt,Int_e v)
              |_ -> Dbl_e (log10(double v))
        ///<summary>対数関数（常用対数）</summary>
        static member log10 (v:double) =
            match p.lang with
              |H -> Log10(Dt,Dbl_e v)
              |_ -> Dbl_e (log10 v)
        ///<summary>平方根</summary>
        static member sqrt (v:num0) =
            let (<--) (x:num0) (y:num0) = num0.subst false x y
            match p.lang,v.etype with
              |C89,Zt ->
                let p = p.param
                let vv = v.code
                let (_,_,c) = vv.str
                let z = Var(p.getvar(Zt))
                let da = Var(p.getvar(Dt))
                let dt = Var(p.getvar(Dt))
                da <-- asm.sqrt(asm.sqrt(vv.re*vv.re+vv.im*vv.im))
                dt <-- asm.atan2(vv.im,vv.re)
                z.re <-- da*asm.cos(0.5*dt)
                z.im <-- da*asm.sin(0.5*dt)
                Var(Zt,z.name,[z;dt;da]@c)
              |_ ->
                Sqrt(num0.ptype(v,Dt),v)
        ///<summary>平方根</summary>
        static member sqrt (v:int) =
            match p.lang with
              |H -> Sqrt(Dt,Int_e v)
              |_ -> Dbl_e(sqrt(double v))
        ///<summary>平方根</summary>
        static member sqrt (v:double) =
            match p.lang with
              |H -> Sqrt(Dt,Dbl_e v)
              |_ -> Dbl_e(sqrt v)
        ///<summary>小数点以下切り捨て</summary>
        static member floor (v:num0) =
            Floor(Dt,v)
        ///<summary>小数点以下切り捨て</summary>
        static member floor (v:double) = asm.floor(D v)
        ///<summary>小数点以下切り上げ</summary>
        static member ceil (v:num0) =
            Ceiling(Dt,v)
        ///<summary>小数点以下切り上げ</summary>
        static member ceil (v:double) = asm.ceil(D v)
        ///<summary>整数に型変換</summary>
        static member toint (v:num0) = 
            match v with
              |Add _|Sub _|Mul _|Div _ -> 
                Par(It 4,ToInt(v))
              |_ -> 
                ToInt(v)
        ///<summary>倍精度浮動小数点型に変換</summary>
        static member todouble (v:num0) = 
            match v with
              |Add _|Sub _|Mul _|Div _ -> 
                Par(Dt,ToDbl(v))
              |_ -> 
                ToDbl(v)
        ///<summary>共役複素数</summary>
        static member conj (v:num0) =
            let (<--) (x:num0) (y:num0) = num0.subst false x y
            match p.lang,v.etype with
              |C89,Zt ->
                let p = p.param
                let vv = v.code
                let (_,_,c) = vv.str
                let z = Var(p.getvar(Zt))
                z.re <-- vv.re
                z.im <-- -vv.im
                Var(Zt,z.name,[z]@c)
              |_ ->
                Conj(v)
                