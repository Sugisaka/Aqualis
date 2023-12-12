(*
Copyright (c) 2022 Jun-ichiro Sugisaka

This software is released under the MIT License.
http://opensource.org/licenses/mit-license.php
*)
namespace Aqualis
    
    open System
    open Aqualis_base
    
    ///<summary>数学関数</summary>
    type asm =
        ///<summary>虚数単位</summary>
        static member uj with get() =
            match p.lang with
            |F   -> 
                p.var.setUniqVar(Zt,A0,"uj","(0d0,1d0)")
                Var(Zt,"uj")
            |C -> 
                //#defineで定義済み
                Var(Zt,"uj")
            |T   ->
                p.var.setUniqVar(Zt,A0,"\\mathrm{j}","(0d0,1d0)")
                Var(Zt,"\\mathrm{j}")
            |H   ->
                p.var.setUniqVar(Zt,A0,"\\mathrm{j}","(0d0,1d0)")
                Var(Zt,"\\mathrm{j}")
        ///<summary>円周率</summary>
        static member pi with get() = 
            if p.isEmpty then
                Dbl_c Math.PI
            else
                match p.lang with
                |F   ->
                    p.var.setUniqVar(Dt,A0,"pi","3.14159265358979d0")
                    Var(Dt,"pi")
                |C ->
                    p.var.setUniqVar(Dt,A0,"pi","3.14159265358979")
                    Var(Dt,"pi")
                |T   ->
                    p.var.setUniqVar(Dt,A0,"\\pi","3.14159265358979")
                    Var(Dt,"\\pi")
                |H   ->
                    p.var.setUniqVar(Dt,A0,"\\pi","3.14159265358979")
                    Var(Dt,"\\pi")
        ///<summary>2πj</summary>
        static member j2p with get() = 2*asm.pi*asm.uj
        
        static member todouble(x:num0) = 
            let rec dbl (e:num0) =
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
            dbl x
        static member toint(x:num0) =
            let rec it (e:num0) =
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
            it x
            
        static member sum (n1:num0) = fun (n2:num0) (code:num0->num0) ->
            let y = code(n1)
            Sum(y.etype,n1,n2,code)
            
        static member sum (n1:int) = fun (n2:int) (code:num0->num0) ->
            let y = code(Int_c n1)
            Sum(y.etype,Int_c n1,Int_c n2,code)
            
        ///<summary>累乗</summary>
        static member pow(x:num0, y:num0) = 
            match x,y with
            |Int_c x, Int_c y -> Dbl_c <| Math.Pow(x,y)
            |Int_c x, Dbl_c y -> Dbl_c <| Math.Pow(x,y)
            |Dbl_c x, Int_c y -> Dbl_c <| Math.Pow(x,y)
            |Dbl_c x, Dbl_c y -> Dbl_c <| Math.Pow(x,y)
            |_ -> num0.powr(x,y)
        static member pow(x:num0, y:int) = asm.pow(x,Int_c y)
        static member pow(x:num0, y:double) = asm.pow(x,Dbl_c y)
        static member pow(x:int, y:num0) = asm.pow(Int_c x,y)
        static member pow(x:double, y:num0) = asm.pow(Dbl_c x,y)
        
        ///<summary>指数関数</summary>
        static member exp (v:num0) = 
            match v with
            |Int_c v -> Dbl_c <| Math.Exp(v)
            |Dbl_c v -> Dbl_c <| Math.Exp(v)
            |_ ->
                let v = match v.etype with |It _ -> asm.todouble(v) |_ -> v
                Exp(Dt%%(v.etype),v)
        
        ///<summary>正弦関数</summary>
        static member sin (v:num0) = 
            match v with
            |Int_c v -> Dbl_c <| Math.Sin(v)
            |Dbl_c v -> Dbl_c <| Math.Sin(v)
            |_ ->
                let v = match v.etype with |It _ -> asm.todouble(v) |_ -> v
                Sin(Dt%%(v.etype),v)

        ///<summary>余弦関数</summary>
        static member cos (v:num0) = 
            match v with
            |Int_c v -> Dbl_c <| Math.Cos(v)
            |Dbl_c v -> Dbl_c <| Math.Cos(v)
            |_ ->
                let v = match v.etype with |It _ -> asm.todouble(v) |_ -> v
                Cos(Dt%%(v.etype),v)
        static member tan (v:num0) = 
            match v with
            |Int_c v -> Dbl_c <| Math.Tan(v)
            |Dbl_c v -> Dbl_c <| Math.Tan(v)
            |_ ->
                let v = match v.etype with |It _ -> asm.todouble(v) |_ -> v
                Tan(Dt%%(v.etype),v)

        ///<summary>逆正弦関数</summary>
        static member asin (v:num0) = 
            match v with
            |Int_c v -> Dbl_c <| Math.Asin(v)
            |Dbl_c v -> Dbl_c <| Math.Asin(v)
            |_ ->
                let v = match v.etype with |It _ -> asm.todouble(v) |_ -> v
                Asin(Dt%%(v.etype),v)
                
        ///<summary>逆余弦関数</summary>
        static member acos (v:num0) = 
            match v with
            |Int_c v -> Dbl_c <| Math.Acos(v)
            |Dbl_c v -> Dbl_c <| Math.Acos(v)
            |_ ->
                let v = match v.etype with |It _ -> asm.todouble(v) |_ -> v
                Acos(Dt%%(v.etype),v)
                
        ///<summary>逆正接関数</summary>
        static member atan (v:num0) = 
            match v with
            |Int_c v -> Dbl_c <| Math.Atan(v)
            |Dbl_c v -> Dbl_c <| Math.Atan(v)
            |_ ->
                let v = match v.etype with |It _ -> asm.todouble(v) |_ -> v
                Atan(Dt%%(v.etype),v)
                
        ///<summary>逆正接関数</summary>
        static member atan2(x:num0, y:num0) = 
            match x,y with
            |Int_c x, Int_c y -> Dbl_c <| Math.Atan2(x,y)
            |Int_c x, Dbl_c y -> Dbl_c <| Math.Atan2(x,y)
            |Dbl_c x, Int_c y -> Dbl_c <| Math.Atan2(x,y)
            |Dbl_c x, Dbl_c y -> Dbl_c <| Math.Atan2(x,y)
            |_ ->
                let x = match x.etype with |It _ -> asm.todouble(x) |_ -> x
                let y = match y.etype with |It _ -> asm.todouble(y) |_ -> y
                Atan2(x,y)
                
        ///<summary>絶対値</summary>
        static member abs (v:num0) = 
            match v with
            |Int_c v -> Dbl_c <| Math.Abs(v)
            |Dbl_c v -> Dbl_c <| Math.Abs(v)
            |_       -> Abs(Dt,v)
            
        ///<summary>自然対数</summary>
        static member log (v:num0) = 
            match v with
            |Int_c v -> Dbl_c <| Math.Log(v)
            |Dbl_c v -> Dbl_c <| Math.Log(v)
            |_ ->
                let v = match v.etype with |It _ -> asm.todouble(v) |_ -> v
                Log(Dt%%(v.etype),v)
                
        ///<summary>常用対数</summary>
        static member log10 (v:num0) = 
            match v with
            |Int_c v -> Dbl_c <| Math.Log10(v)
            |Dbl_c v -> Dbl_c <| Math.Log10(v)
            |_ ->
                let v = match v.etype with |It _ -> asm.todouble(v) |_ -> v
                Log10(Dt%%(v.etype),v)
                
        ///<summary>平方根</summary>
        static member sqrt (v:num0) = 
            match v with
            |Int_c v -> Dbl_c <| Math.Sqrt(double v)
            |Dbl_c v -> Dbl_c <| Math.Sqrt(v)
            |_ ->
                let v = match v.etype with |It _ -> asm.todouble(v) |_ -> v
                Sqrt(Dt%%(v.etype),v)
                
        ///<summary>小数点以下切り捨て</summary>
        static member floor (v:num0) = 
            match v with
            |Int_c v -> Int_c v
            |Dbl_c v -> Int_c <| int(floor(v))
            |_ ->
                let v = match v.etype with |It _ -> asm.todouble(v) |_ -> v
                match p.lang with
                |F|C -> Formula(v.etype,"floor("+v.code+")")
                |T|H -> Formula(v.etype,"\\mathrm{floor}("+v.code+")")
                
        ///<summary>小数点以下切り上げ</summary>
        static member ceil (v:num0) = 
            match v with
            |Int_c v -> Int_c v
            |Dbl_c v -> Int_c <| int(ceil(v))
            |_ ->
                match p.lang with
                |F -> Formula(It 4,"ceiling("+v.code+")")
                |C -> Formula(It 4,"ceil("+v.code+")")
                |T|H -> Formula(It 4,"\\mathrm{ceil}("+v.code+")")
                
        ///<summary>共役複素数</summary>
        static member conj (v:num0) = 
            match v.etype with
            |Zt ->
                match p.lang,v with
                |F,_ -> Formula(Zt,"conjg("+v.code+")")
                |C,_ -> Formula(Zt,"conj("+v.code+")")
                |(T|H),(Var _|Int_c _|Dbl_c _) -> Formula(Zt,v.code+"^*")
                |(T|H),_ -> Formula(Zt,"\\left["+v.code+"\\right]^*")
            |_ ->
                printfn "%s" <| v.ToString()
                v
                
        ///<summary>uの値を変数vに保存</summary>
        static member xlet (v:num0,u:num0) = Let(v.etype,v,u)
        
    [<AutoOpen>]
    module num0_op =
        ///<summary>数値型</summary>
        type num0 with
            ///<summary>実部</summary>
            member x.re with get() = 
                match p.lang with
                |F -> Formula(Dt,"real("+x.code+")")
                |C -> Formula(Dt,"creal("+x.code+")")
                |_ -> Formula(Dt,"Re["+x.code+"]")
            ///<summary>虚部</summary>
            member x.im with get() =
                match p.lang with
                |F -> Formula(Dt,"aimag("+x.code+")")
                |C -> Formula(Dt,"cimag("+x.code+")")
                |_ -> Formula(Dt,"Im["+x.code+"]")
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
            
        type bool0 with
            member this.eval with get() =
                let rec ev(cond:bool0) =
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
                ev this
                
        [<AutoOpen>]
        module noperator =
            let And (lst:list<bool0>) = 
                match List.tryFind (fun x -> bool0.equal(x,False)) lst with
                |None ->
                    AND <| List.map (fun (x:bool0) -> x) lst
                |Some _ -> False
            let Or (lst:list<bool0>) = 
                match List.tryFind (fun x -> bool0.equal(x,True)) lst with
                |None ->
                    OR <| List.map (fun (x:bool0) -> x) lst
                |Some _ -> True
                
            type System.Int32 with
                ///<summary>整数をint0型に置換</summary>
                member this.I with get() = Int_c this
                
            type System.Double with
                ///<summary>小数をdouble0型に置換</summary>
                member this.D with get() = Dbl_c this