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
        
    ///<summary>整数型</summary>
    type int0(x:Expr) =
        inherit base0(x)
        static member (~-) (x:int0) = int0(-x.expr)
        static member ( + ) (x:int0,y:int0) = int0( x.expr + y.expr)
        static member ( + ) (x:int0,y:int) = int0( x.expr + Int_c y)
        static member ( + ) (x:int,y:int0) = int0( Int_c x + y.expr)
        static member ( + ) (x:double,y:int0) = int0( Dbl_c x + y.expr)
        static member ( + ) (x:int0,y:double) = int0( x.expr + Dbl_c y)
        
        static member ( - ) (x:int0,y:int0) = int0( x.expr - y.expr)
        static member ( - ) (x:int0,y:int) = int0( x.expr - Int_c y)
        static member ( - ) (x:int,y:int0) = int0( Int_c x - y.expr)
        static member ( - ) (x:double,y:int0) = int0( Dbl_c x - y.expr)
        static member ( - ) (x:int0,y:double) = int0( x.expr - Dbl_c y)
        
        static member ( * ) (x:int0,y:int0) = int0( x.expr * y.expr)
        static member ( * ) (x:int0,y:int) = int0( x.expr * Int_c y)
        static member ( * ) (x:int,y:int0) = int0( Int_c x * y.expr)
        static member ( * ) (x:double,y:int0) = int0( Dbl_c x * y.expr)
        static member ( * ) (x:int0,y:double) = int0( x.expr * Dbl_c y)
        
        static member ( / ) (x:int0,y:int0) = float0( asm.todouble(x).expr / asm.todouble(y).expr)
        static member ( / ) (x:int0,y:int) = float0( asm.todouble(x).expr / Dbl_c (double y))
        static member ( / ) (x:int,y:int0) = float0( Dbl_c (double x) / asm.todouble(y).expr)
        static member ( / ) (x:double,y:int0) = float0( Dbl_c x / asm.todouble(y).expr)
        static member ( / ) (x:int0,y:double) = float0( asm.todouble(x).expr / Dbl_c y)
        
        static member (%) (x:int0,y:int0) = float0( asm.todouble(x).expr % asm.todouble(y).expr)
        static member (%) (x:int0,y:int) = float0( asm.todouble(x).expr % Dbl_c (double y))
        static member (%) (x:int,y:int0) = float0( Dbl_c (double x) % asm.todouble(y).expr)
        static member (%) (x:double,y:int0) = float0( Dbl_c x % asm.todouble(y).expr)
        static member (%) (x:int0,y:double) = float0( asm.todouble(x).expr % Dbl_c y)

        static member (/.) (x:int0,y:int0) = int0( x.expr /. y.expr)
        static member (/.) (x:int0,y:int) = int0( x.expr /. Int_c y)
        static member (/.) (x:int,y:int0) = int0( Int_c x /. y.expr)
        
        static member (=.) (x:int0,y:int0) = bool0(x.expr =. y.expr)
        static member (=.) (x:int,y:int0) = bool0((Int_c x) =. y.expr)
        static member (=.) (x:int0,y:int) = bool0(x.expr =. (Int_c y))
        static member (=.) (x:int0,y:double) = bool0(x.expr =. (Dbl_c y))
        static member (=.) (x:double,y:int0) = bool0((Dbl_c x) =. y.expr)
        
        static member (=/.) (x:int0,y:int0) = bool0(x.expr =/. y.expr)
        static member (=/.) (x:int,y:int0) = bool0((Int_c x) =/. y.expr)
        static member (=/.) (x:int0,y:int) = bool0(x.expr =/. (Int_c y))
        static member (=/.) (x:int0,y:double) = bool0(x.expr =/. (Dbl_c y))
        static member (=/.) (x:double,y:int0) = bool0((Dbl_c x) =/. y.expr)
        
        static member (<.) (x:int0,y:int0) = bool0(x.expr <. y.expr)
        static member (<.) (x:int,y:int0) = bool0((Int_c x) <. y.expr)
        static member (<.) (x:int0,y:int) = bool0(x.expr <. (Int_c y))
        static member (<.) (x:int0,y:double) = bool0(x.expr <. (Dbl_c y))
        static member (<.) (x:double,y:int0) = bool0((Dbl_c x) <. y.expr)
        
        static member (<=.) (x:int0,y:int0) = bool0(x.expr <=. y.expr)
        static member (<=.) (x:int,y:int0) = bool0((Int_c x) <=. y.expr)
        static member (<=.) (x:int0,y:int) = bool0(x.expr <=. (Int_c y))
        static member (<=.) (x:int0,y:double) = bool0(x.expr <=. (Dbl_c y))
        static member (<=.) (x:double,y:int0) = bool0((Dbl_c x) <=. y.expr)
        
        static member (>.) (x:int0,y:int0) = bool0(x.expr >. y.expr)
        static member (>.) (x:int,y:int0) = bool0((Int_c x) >. y.expr)
        static member (>.) (x:int0,y:int) = bool0(x.expr >. (Int_c y))
        static member (>.) (x:int0,y:double) = bool0(x.expr >. (Dbl_c y))
        static member (>.) (x:double,y:int0) = bool0((Dbl_c x) >. y.expr)
        
        static member (>=.) (x:int0,y:int0) = bool0(x.expr >=. y.expr)
        static member (>=.) (x:int,y:int0) = bool0((Int_c x) >=. y.expr)
        static member (>=.) (x:int0,y:int) = bool0(x.expr >=. (Int_c y))
        static member (>=.) (x:int0,y:double) = bool0(x.expr >=. (Dbl_c y))
        static member (>=.) (x:double,y:int0) = bool0((Dbl_c x) >=. y.expr)
        
        static member (<==) (x:int0,y:int0) = x.expr <== y.expr
        static member (<==) (x:int0,y:int) = x.expr <== Int_c y
        member this.clear() = this <== 0
        member this.inc with get() = this <== this + 1
        member this.dec with get() = this <== this - 1
                
    ///<summary>実数型</summary>
    and float0(x:Expr) =
        inherit base0(x)
        static member (~-) (x:float0) = float0(-x.expr)
        static member ( + ) (x:float0,y:int0) = float0(x.expr + y.expr)
        static member ( + ) (x:float0,y:int) = float0(x.expr + Int_c y)
        static member ( + ) (x:int0,y:float0) = float0(x.expr + y.expr)
        static member ( + ) (x:int,y:float0) = float0(Int_c x + y.expr)
        static member ( + ) (x:float0,y:float0) = float0(x.expr + y.expr)
        static member ( + ) (x:double,y:float0) = float0(Dbl_c x + y.expr)
        static member ( + ) (x:float0,y:double) = float0(x.expr + Dbl_c y)
        
        static member ( - ) (x:float0,y:int0) = float0(x.expr - y.expr)
        static member ( - ) (x:float0,y:int) = float0(x.expr - Int_c y)
        static member ( - ) (x:int0,y:float0) = float0(x.expr - y.expr)
        static member ( - ) (x:int,y:float0) = float0(Int_c x - y.expr)
        static member ( - ) (x:float0,y:float0) = float0(x.expr - y.expr)
        static member ( - ) (x:double,y:float0) = float0(Dbl_c x - y.expr)
        static member ( - ) (x:float0,y:double) = float0(x.expr - Dbl_c y)
        
        static member ( * ) (x:float0,y:int0) = float0(x.expr * y.expr)
        static member ( * ) (x:float0,y:int) = float0(x.expr * Int_c y)
        static member ( * ) (x:int0,y:float0) = float0(x.expr * y.expr)
        static member ( * ) (x:int,y:float0) = float0(Int_c x * y.expr)
        static member ( * ) (x:float0,y:float0) = float0(x.expr * y.expr)
        static member ( * ) (x:double,y:float0) = float0(Dbl_c x * y.expr)
        static member ( * ) (x:float0,y:double) = float0(x.expr * Dbl_c y)
        
        static member ( / ) (x:float0,y:int0) = float0(x.expr / y.expr)
        static member ( / ) (x:float0,y:int) = float0(x.expr / Int_c y)
        static member ( / ) (x:int0,y:float0) = float0(x.expr / y.expr)
        static member ( / ) (x:int,y:float0) = float0(Int_c x / y.expr)
        static member ( / ) (x:float0,y:float0) = float0(x.expr / y.expr)
        static member ( / ) (x:double,y:float0) = float0(Dbl_c x / y.expr)
        static member ( / ) (x:float0,y:double) = float0(x.expr / Dbl_c y)
        
        static member (=.) (x:int0,y:float0) = bool0(x.expr =. y.expr)
        static member (=.) (x:int,y:float0) = bool0((Int_c x) =. y.expr)
        static member (=.) (x:float0,y:int0) = bool0(x.expr =. y.expr)
        static member (=.) (x:float0,y:int) = bool0(x.expr =. (Int_c y))
        static member (=.) (x:float0,y:float0) = bool0(x.expr =. y.expr)
        static member (=.) (x:double,y:float0) = bool0((Dbl_c x) =. y.expr)
        static member (=.) (x:float0,y:double) = bool0(x.expr =. (Dbl_c y))
        
        static member (=/.) (x:int0,y:float0) = bool0(x.expr =/. y.expr)
        static member (=/.) (x:int,y:float0) = bool0((Int_c x) =/. y.expr)
        static member (=/.) (x:float0,y:int0) = bool0(x.expr =/. y.expr)
        static member (=/.) (x:float0,y:int) = bool0(x.expr =/. (Int_c y))
        static member (=/.) (x:float0,y:float0) = bool0(x.expr =/. y.expr)
        static member (=/.) (x:double,y:float0) = bool0((Dbl_c x) =/. y.expr)
        static member (=/.) (x:float0,y:double) = bool0(x.expr =/. (Dbl_c y))
        
        static member (<.) (x:int0,y:float0) = bool0(x.expr <. y.expr)
        static member (<.) (x:int,y:float0) = bool0((Int_c x) <. y.expr)
        static member (<.) (x:float0,y:int0) = bool0(x.expr <. y.expr)
        static member (<.) (x:float0,y:int) = bool0(x.expr <. (Int_c y))
        static member (<.) (x:float0,y:float0) = bool0(x.expr <. y.expr)
        static member (<.) (x:double,y:float0) = bool0((Dbl_c x) <. y.expr)
        static member (<.) (x:float0,y:double) = bool0(x.expr <. (Dbl_c y))
        
        static member (<=.) (x:int0,y:float0) = bool0(x.expr <=. y.expr)
        static member (<=.) (x:int,y:float0) = bool0((Int_c x) <=. y.expr)
        static member (<=.) (x:float0,y:int0) = bool0(x.expr <=. y.expr)
        static member (<=.) (x:float0,y:int) = bool0(x.expr <=. (Int_c y))
        static member (<=.) (x:float0,y:float0) = bool0(x.expr <=. y.expr)
        static member (<=.) (x:double,y:float0) = bool0((Dbl_c x) <=. y.expr)
        static member (<=.) (x:float0,y:double) = bool0(x.expr <=. (Dbl_c y))
        
        static member (>.) (x:int0,y:float0) = bool0(x.expr >. y.expr)
        static member (>.) (x:int,y:float0) = bool0((Int_c x) >. y.expr)
        static member (>.) (x:float0,y:int0) = bool0(x.expr >. y.expr)
        static member (>.) (x:float0,y:int) = bool0(x.expr >. (Int_c y))
        static member (>.) (x:float0,y:float0) = bool0(x.expr >. y.expr)
        static member (>.) (x:double,y:float0) = bool0((Dbl_c x) >. y.expr)
        static member (>.) (x:float0,y:double) = bool0(x.expr >. (Dbl_c y))
        
        static member (>=.) (x:int0,y:float0) = bool0(x.expr >=. y.expr)
        static member (>=.) (x:int,y:float0) = bool0((Int_c x) >=. y.expr)
        static member (>=.) (x:float0,y:int0) = bool0(x.expr >=. y.expr)
        static member (>=.) (x:float0,y:int) = bool0(x.expr >=. (Int_c y))
        static member (>=.) (x:float0,y:float0) = bool0(x.expr >=. y.expr)
        static member (>=.) (x:double,y:float0) = bool0((Dbl_c x) >=. y.expr)
        static member (>=.) (x:float0,y:double) = bool0(x.expr >=. (Dbl_c y))
        
        static member (<==) (x:float0,y:int0) = x.expr <== y.expr
        static member (<==) (x:float0,y:int) = x.expr <== Int_c y
        static member (<==) (x:float0,y:float0) = x.expr <== y.expr
        static member (<==) (x:float0,y:double) = x.expr <== Dbl_c y
        member this.clear() = this <== 0.0
                
    and bool0(x:Expr) =
        inherit aqvar(x)
        static member (=.) (x:bool0,y:int0) = bool0(x.expr =. y.expr)
        static member (=.) (x:bool0,y:int) = bool0(x.expr =. (Int_c y))
        static member (=.) (x:bool0,y:float0) = bool0(x.expr =. y.expr)
        static member (=.) (x:bool0,y:double) = bool0(x.expr =. (Dbl_c y))
        
        static member (<.) (x:bool0,y:int0) = bool0(x.expr <. y.expr)
        static member (<.) (x:bool0,y:int) = bool0(x.expr <. (Int_c y))
        static member (<.) (x:bool0,y:float0) = bool0(x.expr <. y.expr)
        static member (<.) (x:bool0,y:double) = bool0(x.expr <. (Dbl_c y))
        
        static member (<=.) (x:bool0,y:int0) = bool0(x.expr <=. y.expr)
        static member (<=.) (x:bool0,y:int) = bool0(x.expr <=. (Int_c y))
        static member (<=.) (x:bool0,y:float0) = bool0(x.expr <=. y.expr)
        static member (<=.) (x:bool0,y:double) = bool0(x.expr <=. (Dbl_c y))
        
        static member (>.) (x:bool0,y:int0) = bool0(x.expr >. y.expr)
        static member (>.) (x:bool0,y:int) = bool0(x.expr >. (Int_c y))
        static member (>.) (x:bool0,y:float0) = bool0(x.expr >. y.expr)
        static member (>.) (x:bool0,y:double) = bool0(x.expr >. (Dbl_c y))
        
        static member (>=.) (x:bool0,y:int0) = bool0(x.expr >=. y.expr)
        static member (>=.) (x:bool0,y:int) = bool0(x.expr >=. (Int_c y))
        static member (>=.) (x:bool0,y:float0) = bool0(x.expr >=. y.expr)
        static member (>=.) (x:bool0,y:double) = bool0(x.expr >=. (Dbl_c y))
        
    ///<summary>複素数型</summary>
    and complex0(x:Expr) =
        inherit base0(x)
        static member (~-) (x:complex0) = complex0(-x.expr)
        static member ( + ) (x:complex0,y:int0) = complex0(x.expr + y.expr)
        static member ( + ) (x:complex0,y:int) = complex0(x.expr + Int_c y)
        static member ( + ) (x:int0,y:complex0) = complex0(x.expr + y.expr)
        static member ( + ) (x:int,y:complex0) = complex0(Int_c x + y.expr)
        static member ( + ) (x:complex0,y:float0) = complex0(x.expr + y.expr)
        static member ( + ) (x:complex0,y:double) = complex0(x.expr + Dbl_c y)
        static member ( + ) (x:float0,y:complex0) = complex0(x.expr + y.expr)
        static member ( + ) (x:double,y:complex0) = complex0(Dbl_c x + y.expr)
        static member ( + ) (x:complex0,y:complex0) = complex0(x.expr + y.expr)
        
        static member ( - ) (x:complex0,y:int0) = complex0(x.expr - y.expr)
        static member ( - ) (x:complex0,y:int) = complex0(x.expr - Int_c y)
        static member ( - ) (x:int0,y:complex0) = complex0(x.expr - y.expr)
        static member ( - ) (x:int,y:complex0) = complex0(Int_c x - y.expr)
        static member ( - ) (x:complex0,y:float0) = complex0(x.expr - y.expr)
        static member ( - ) (x:complex0,y:double) = complex0(x.expr - Dbl_c y)
        static member ( - ) (x:float0,y:complex0) = complex0(x.expr - y.expr)
        static member ( - ) (x:double,y:complex0) = complex0(Dbl_c x - y.expr)
        static member ( - ) (x:complex0,y:complex0) = complex0(x.expr - y.expr)
        
        static member ( * ) (x:complex0,y:int0) = complex0(x.expr * y.expr)
        static member ( * ) (x:complex0,y:int) = complex0(x.expr * Int_c y)
        static member ( * ) (x:int0,y:complex0) = complex0(x.expr * y.expr)
        static member ( * ) (x:int,y:complex0) = complex0(Int_c x * y.expr)
        static member ( * ) (x:complex0,y:float0) = complex0(x.expr * y.expr)
        static member ( * ) (x:complex0,y:double) = complex0(x.expr * Dbl_c y)
        static member ( * ) (x:float0,y:complex0) = complex0(x.expr * y.expr)
        static member ( * ) (x:double,y:complex0) = complex0(Dbl_c x * y.expr)
        static member ( * ) (x:complex0,y:complex0) = complex0(x.expr * y.expr)
        
        static member ( / ) (x:complex0,y:int0) = complex0(x.expr / y.expr)
        static member ( / ) (x:complex0,y:int) = complex0(x.expr / Int_c y)
        static member ( / ) (x:int0,y:complex0) = complex0(x.expr / y.expr)
        static member ( / ) (x:int,y:complex0) = complex0(Int_c x / y.expr)
        static member ( / ) (x:complex0,y:float0) = complex0(x.expr / y.expr)
        static member ( / ) (x:complex0,y:double) = complex0(x.expr / Dbl_c y)
        static member ( / ) (x:float0,y:complex0) = complex0(x.expr / y.expr)
        static member ( / ) (x:double,y:complex0) = complex0(Dbl_c x / y.expr)
        static member ( / ) (x:complex0,y:complex0) = complex0(x.expr / y.expr)
        
        static member (<==) (x:complex0,y:int0) = x.expr <== y.expr
        static member (<==) (x:complex0,y:int) = x.expr <== Int_c y
        static member (<==) (x:complex0,y:float0) = x.expr <== y.expr
        static member (<==) (x:complex0,y:double) = x.expr <== Dbl_c y
        static member (<==) (x:complex0,y:complex0) = x.expr <== y.expr
        
        ///<summary>実部</summary>
        member x.re with get() = float0(match p.lang with |F -> Formula("real("+x.code+")") |C -> Formula("creal("+x.code+")") |_ -> Formula("Re["+x.code+"]"))
        ///<summary>虚部</summary>
        member x.im with get() = float0(match p.lang with |F -> Formula("aimag("+x.code+")") |C -> Formula("cimag("+x.code+")") |_ -> Formula("Im["+x.code+"]"))
        ///<summary>絶対値</summary>
        member x.abs with get() = asm.abs(x)
        ///<summary>絶対値の2乗</summary>
        member x.pow with get() = 
            let a:float0 = asm.abs(x)
            let b = int0(Int_c 2)
            asm.pow(a,b)
        ///<summary>偏角</summary>
        member x.pha with get() = asm.atan2(x.im,x.re)
        member this.clear() = this <== 0.0
                
    ///<summary>数学関数</summary>
    and asm =
        ///<summary>虚数単位</summary>
        static member uj with get() =
            match p.lang with
            |F   -> 
                p.param.var.setUniqVar(Zt,A0,"uj","(0d0,1d0)")
                complex0(Var "uj")
            |C -> 
                //#defineで定義済み
                complex0(Var "uj")
            |T   ->
                p.param.var.setUniqVar(Zt,A0,"\\mathrm{j}","(0d0,1d0)")
                complex0(Var "\\mathrm{j}")
            |H   ->
                p.param.var.setUniqVar(Zt,A0,"&ImaginaryI;","(0d0,1d0)")
                complex0(Var "<mi>&ImaginaryI;</mi>")
        ///<summary>円周率</summary>
        static member pi with get() = 
            match p.lang with
            |F   ->
                p.param.var.setUniqVar(Dt,A0,"pi","3.14159265358979d0")
                float0(Var "pi")
            |C ->
                p.param.var.setUniqVar(Dt,A0,"pi","3.14159265358979")
                float0(Var "pi")
            |T   ->
                p.param.var.setUniqVar(Dt,A0,"\\pi","3.14159265358979")
                float0(Var "\\pi")
            |H   ->
                p.param.var.setUniqVar(Dt,A0,"&pi;","3.14159265358979")
                float0(Var "<mi>&pi;</mi>")
        ///<summary>2πj</summary>
        static member j2p with get() = 2*asm.pi*asm.uj

        static member todouble(x:int0):float0 =  float0(match p.lang with |F -> Formula("dble("+x.code+")") |_ -> Formula("(double)("+x.code+")"))
        static member toint(x:float0) =  int0(match p.lang with |F -> Formula("int("+x.code+")") |_ -> Formula("(int)("+x.code+")"))

        ///<summary>累乗</summary>
        static member pow(x:int0, y:int0) = int0(Expr.pow(x.expr,y.expr))
        ///<summary>累乗</summary>
        static member pow(x:int, y:int0) = int0(Expr.pow(Int_c x,y.expr))
        ///<summary>累乗</summary>
        static member pow(x:int0, y:int) = int0(Expr.pow(x.expr,Int_c y))
        ///<summary>累乗</summary>
        static member pow(x:int0, y:float0) = float0(Expr.pow(x.expr,y.expr))
        ///<summary>累乗</summary>
        static member pow(x:int, y:float0) = float0(Expr.pow(Int_c x,y.expr))
        ///<summary>累乗</summary>
        static member pow(x:int0, y:double) = float0(Expr.pow(x.expr,Dbl_c y))
        ///<summary>累乗</summary>
        static member pow(x:int0, y:complex0) = complex0(Expr.pow(x.expr,y.expr))
        ///<summary>累乗</summary>
        static member pow(x:int, y:complex0) = complex0(Expr.pow(Int_c x,y.expr))
        ///<summary>累乗</summary>
        static member pow(x:float0, y:int0) = float0(Expr.pow(x.expr,y.expr))
        ///<summary>累乗</summary>
        static member pow(x:double, y:int0) = float0(Expr.pow(Dbl_c x,y.expr))
        ///<summary>累乗</summary>
        static member pow(x:float0, y:int) = float0(Expr.pow(x.expr,Int_c y))
        ///<summary>累乗</summary>
        static member pow(x:float0, y:float0) = float0(Expr.pow(x.expr,y.expr))
        ///<summary>累乗</summary>
        static member pow(x:double, y:float0) = float0(Expr.pow(Dbl_c x,y.expr))
        ///<summary>累乗</summary>
        static member pow(x:float0, y:double) = float0(Expr.pow(x.expr,Dbl_c y))
        ///<summary>累乗</summary>
        static member pow(x:float0, y:complex0) = complex0(Expr.pow(x.expr,y.expr))
        ///<summary>累乗</summary>
        static member pow(x:double, y:complex0) = complex0(Expr.pow(Dbl_c x,y.expr))
        ///<summary>累乗</summary>
        static member pow(x:complex0, y:int0) = complex0(Expr.pow(x.expr,y.expr))
        ///<summary>累乗</summary>
        static member pow(x:complex0, y:int) = complex0(Expr.pow(x.expr,Int_c y))
        ///<summary>累乗</summary>
        static member pow(x:complex0, y:float0) = complex0(Expr.pow(x.expr,y.expr))
        ///<summary>累乗</summary>
        static member pow(x:complex0, y:double) = complex0(Expr.pow(x.expr,Dbl_c y))
        ///<summary>累乗</summary>
        static member pow(x:complex0, y:complex0) = complex0(Expr.pow(x.expr,y.expr))

        ///<summary>指数関数</summary>
        static member exp (v:int0) = float0(match p.lang with |F|C|T|H -> Formula("exp("+asm.todouble(v).code+")"))
        ///<summary>指数関数</summary>
        static member exp (v:float0) = float0(match p.lang with |F|C|T|H -> Formula("exp("+v.code+")"))
        ///<summary>指数関数</summary>
        static member exp (v:complex0) = float0(match p.lang with |F|T|H -> Formula("exp("+v.code+")") |C -> Formula("cexp("+v.code+")"))
        ///<summary>指数関数</summary>
        static member exp (v:int) = asm.exp(int0(Int_c v))
        ///<summary>指数関数</summary>
        static member exp (v:double) = asm.exp(float0(Dbl_c v))

        ///<summary>正弦関数</summary>
        static member sin (v:int0) = float0(match p.lang with |F|C|T|H -> Formula("sin("+asm.todouble(v).code+")"))
        ///<summary>正弦関数</summary>
        static member sin (v:float0) = float0(match p.lang with |F|C|T|H -> Formula("sin("+v.code+")"))
        ///<summary>正弦関数</summary>
        static member sin (v:complex0) = float0(match p.lang with |F|T|H -> Formula("sin("+v.code+")") |C -> Formula("csin("+v.code+")"))
        ///<summary>正弦関数</summary>
        static member sin (v:int) = asm.sin(int0(Int_c v))
        ///<summary>正弦関数</summary>
        static member sin (v:double) = asm.sin(float0(Dbl_c v))

        ///<summary>余弦関数</summary>
        static member cos (v:int0) = float0(match p.lang with |F|C|T|H -> Formula("cos("+asm.todouble(v).code+")"))
        ///<summary>余弦関数</summary>
        static member cos (v:float0) = float0(match p.lang with |F|C|T|H -> Formula("cos("+v.code+")"))
        ///<summary>余弦関数</summary>
        static member cos (v:complex0) = float0(match p.lang with |F|T|H -> Formula("cos("+v.code+")") |C -> Formula("ccos("+v.code+")"))
        ///<summary>余弦関数</summary>
        static member cos (v:int) = asm.cos(int0(Int_c v))
        ///<summary>余弦関数</summary>
        static member cos (v:double) = asm.cos(float0(Dbl_c v))

        ///<summary>正接関数</summary>
        static member tan (v:int0) = float0(match p.lang with |F|C|T|H -> Formula("tan("+asm.todouble(v).code+")"))
        ///<summary>正接関数</summary>
        static member tan (v:float0) = float0(match p.lang with |F|C|T|H -> Formula("tan("+v.code+")"))
        ///<summary>正接関数</summary>
        static member tan (v:complex0) = float0(match p.lang with |F|T|H -> Formula("tan("+v.code+")") |C -> Formula("ctan("+v.code+")"))
        ///<summary>正接関数</summary>
        static member tan (v:int) = asm.tan(int0(Int_c v))
        ///<summary>正接関数</summary>
        static member tan (v:double) = asm.tan(float0(Dbl_c v))

        ///<summary>逆正弦関数</summary>
        static member asin (v:int0) = float0(match p.lang with |F|C|T|H -> Formula("asin("+asm.todouble(v).code+")"))
        ///<summary>逆正弦関数</summary>
        static member asin (v:float0) = float0(match p.lang with |F|C|T|H -> Formula("asin("+v.code+")"))
        ///<summary>逆正弦関数</summary>
        static member asin (v:complex0) = float0(match p.lang with |F|T|H -> Formula("asin("+v.code+")") |C -> Formula("casin("+v.code+")"))
        ///<summary>逆正弦関数</summary>
        static member asin (v:int) = asm.asin(int0(Int_c v))
        ///<summary>逆正弦関数</summary>
        static member asin (v:double) = asm.asin(float0(Dbl_c v))

        ///<summary>逆余弦関数</summary>
        static member acos (v:int0) = float0(match p.lang with |F|C|T|H -> Formula("acos("+asm.todouble(v).code+")"))
        ///<summary>逆余弦関数</summary>
        static member acos (v:float0) = float0(match p.lang with |F|C|T|H -> Formula("acos("+v.code+")"))
        ///<summary>逆余弦関数</summary>
        static member acos (v:complex0) = float0(match p.lang with |F|T|H -> Formula("acos("+v.code+")") |C -> Formula("cacos("+v.code+")"))
        ///<summary>逆余弦関数</summary>
        static member acos (v:int) = asm.acos(int0(Int_c v))
        ///<summary>逆余弦関数</summary>
        static member acos (v:double) = asm.acos(float0(Dbl_c v))

        ///<summary>逆正接関数</summary>
        static member atan (v:int0) = float0(match p.lang with |F|C|T|H -> Formula("atan("+asm.todouble(v).code+")"))
        ///<summary>逆正接関数</summary>
        static member atan (v:float0) = float0(match p.lang with |F|C|T|H -> Formula("atan("+v.code+")"))
        ///<summary>逆正接関数</summary>
        static member atan (v:complex0) = float0(match p.lang with |F|T|H -> Formula("atan("+v.code+")") |C -> Formula("catan("+v.code+")"))
        ///<summary>逆正接関数</summary>
        static member atan (v:int) = asm.atan(int0(Int_c v))
        ///<summary>逆正接関数</summary>
        static member atan (v:double) = asm.atan(float0(Dbl_c v))

        ///<summary>逆正接関数</summary>
        static member atan2(x:int0, y:int0) = float0(match p.lang with |F|C|T|H -> Formula("atan2("+asm.todouble(x).code+","+asm.todouble(y).code+")"))
        ///<summary>逆正接関数</summary>
        static member atan2(x:int0, y:float0) = float0(match p.lang with |F|C|T|H -> Formula("atan2("+asm.todouble(x).code+","+y.code+")"))
        ///<summary>逆正接関数</summary>
        static member atan2(x:float0, y:int0) = float0(match p.lang with |F|C|T|H -> Formula("atan2("+x.code+","+asm.todouble(y).code+")"))
        ///<summary>逆正接関数</summary>
        static member atan2(x:float0, y:float0) = float0(match p.lang with |F|C|T|H -> Formula("atan2("+x.code+","+y.code+")"))
        ///<summary>逆正接関数</summary>
        static member atan2(x:int, y:int0) = asm.atan2(int0(Int_c x),y)
        ///<summary>逆正接関数</summary>
        static member atan2(x:int0, y:int) = asm.atan2(x,int0(Int_c y))
        ///<summary>逆正接関数</summary>
        static member atan2(x:int, y:float0) = asm.atan2(int0(Int_c x),y)
        ///<summary>逆正接関数</summary>
        static member atan2(x:int0, y:double) = asm.atan2(x,float0(Dbl_c y))
        ///<summary>逆正接関数</summary>
        static member atan2(x:double, y:int0) = asm.atan2(float0(Dbl_c x),y)
        ///<summary>逆正接関数</summary>
        static member atan2(x:float0, y:int) = asm.atan2(x,int0(Int_c y))
        ///<summary>逆正接関数</summary>
        static member atan2(x:double, y:float0) = asm.atan2(float0(Dbl_c x),y)
        ///<summary>逆正接関数</summary>
        static member atan2(x:float0, y:double) = asm.atan2(x,float0(Dbl_c y))
        
        ///<summary>絶対値</summary>
        static member abs (v:int0) = float0(match p.lang with |F|C|T|H -> Formula("abs("+v.code+")"))
        ///<summary>絶対値</summary>
        static member abs (v:float0) = float0(match p.lang with |F|T|H -> Formula("abs("+v.code+")") |C -> Formula("fabs("+v.code+")"))
        ///<summary>絶対値</summary>
        static member abs (v:complex0) = float0(match p.lang with |F|T|H -> Formula("abs("+v.code+")") |C -> Formula("cabs("+v.code+")"))
        ///<summary>絶対値</summary>
        static member abs (v:int) = asm.abs(int0(Int_c v))
        ///<summary>絶対値</summary>
        static member abs (v:double) = asm.abs(float0(Dbl_c v))
        
        ///<summary>自然対数</summary>
        static member log (v:int0) = float0(match p.lang with |F|C|T|H -> Formula("log("+asm.todouble(v).code+")"))
        ///<summary>自然対数</summary>
        static member log (v:float0) = float0(match p.lang with |F|C|T|H -> Formula("log("+v.code+")"))
        ///<summary>自然対数</summary>
        static member log (v:complex0) = float0(match p.lang with |F|T|H -> Formula("log("+v.code+")") |C -> Formula("clog("+v.code+")"))
        ///<summary>自然対数</summary>
        static member log (v:int) = asm.log(int0(Int_c v))
        ///<summary>自然対数</summary>
        static member log (v:double) = asm.log(float0(Dbl_c v))
        
        ///<summary>常用対数</summary>
        static member log10 (v:int0) = float0(match p.lang with |F|C|T|H -> Formula("log10("+asm.todouble(v).code+")"))
        ///<summary>常用対数</summary>
        static member log10 (v:float0) = float0(match p.lang with |F|C|T|H -> Formula("log10("+v.code+")"))
        ///<summary>常用対数</summary>
        static member log10 (v:int) = asm.log10(int0(Int_c v))
        ///<summary>常用対数</summary>
        static member log10 (v:double) = asm.log10(float0(Dbl_c v))
        
        ///<summary>平方根</summary>
        static member sqrt (v:int0) = float0(match p.lang with |F|C|T|H -> Formula("sqrt("+asm.todouble(v).code+")"))
        ///<summary>平方根</summary>
        static member sqrt (v:float0) = float0(match p.lang with |F|C|T|H -> Formula("sqrt("+v.code+")"))
        ///<summary>平方根</summary>
        static member sqrt (v:complex0) = float0(match p.lang with |F|T|H -> Formula("sqrt("+v.code+")") |C -> Formula("csqrt("+v.code+")"))
        ///<summary>平方根</summary>
        static member sqrt (v:int) = asm.sqrt(int0(Int_c v))
        ///<summary>平方根</summary>
        static member sqrt (v:double) = asm.sqrt(float0(Dbl_c v))
        
        ///<summary>小数点以下切り捨て</summary>
        static member floor (v:float0) = int0(match p.lang with |F|C|T|H -> Formula("floor("+v.code+")"))
        ///<summary>小数点以下切り捨て</summary>
        static member floor (v:double) = asm.floor(float0(Dbl_c v))
        
        ///<summary>小数点以下切り上げ</summary>
        static member ceil (v:float0) = int0(match p.lang with |F -> Formula("ceiling("+v.code+")") |C|T|H -> Formula("ceil("+v.code+")"))
        ///<summary>小数点以下切り上げ</summary>
        static member ceil (v:double) = asm.ceil(float0(Dbl_c v))
        
        ///<summary>共役複素数</summary>
        static member conj (v:complex0) = complex0(match p.lang with |F -> Formula("conjg("+v.code+")") |C|T|H -> Formula("conj("+v.code+")"))
        
    [<AutoOpen>]
    module boperator =
        let AND (lst:list<bool0>) = bool0(And <| List.map (fun (x:bool0) -> x.expr) lst)
        let OR (lst:list<bool0>) = bool0(Or <| List.map (fun (x:bool0) -> x.expr) lst)

    ///<summary>数値型</summary>
    type num0(e:Etype,x:Expr) =
        inherit base0(x)
        member this.etype with get() = e
        static member (~-) (x:num0) = complex0(-x.expr)
        static member ( + ) (x:num0,y:num0) = num0(Etype.prior(x.etype,y.etype), x.expr + y.expr)
        static member ( + ) (x:num0,y:complex0) = num0(Etype.prior(x.etype,Zt), x.expr + y.expr)
        static member ( + ) (x:num0,y:float0) = num0(Etype.prior(x.etype,Dt), x.expr + y.expr)
        static member ( + ) (x:num0,y:int0) = num0(Etype.prior(x.etype,It 4), x.expr + y.expr)
        static member ( + ) (x:complex0,y:num0) = num0(Etype.prior(Zt,y.etype), x.expr + y.expr)
        static member ( + ) (x:float0,y:num0) = num0(Etype.prior(Dt,y.etype), x.expr + y.expr)
        static member ( + ) (x:int0,y:num0) = num0(Etype.prior(It 4,y.etype), x.expr + y.expr)
        static member ( + ) (x:num0,y:double) = x+float0(Dbl_c y)
        static member ( + ) (x:num0,y:int) = x+int0(Int_c y)
        static member ( + ) (x:double,y:num0) = float0(Dbl_c x)+y
        static member ( + ) (x:int,y:num0) = int0(Int_c x)+y
        
        static member ( - ) (x:num0,y:num0) = num0(Etype.prior(x.etype,y.etype), x.expr - y.expr)
        static member ( - ) (x:num0,y:complex0) = num0(Etype.prior(x.etype,Zt), x.expr - y.expr)
        static member ( - ) (x:num0,y:float0) = num0(Etype.prior(x.etype,Dt), x.expr - y.expr)
        static member ( - ) (x:num0,y:int0) = num0(Etype.prior(x.etype,It 4), x.expr - y.expr)
        static member ( - ) (x:complex0,y:num0) = num0(Etype.prior(Zt,y.etype), x.expr - y.expr)
        static member ( - ) (x:float0,y:num0) = num0(Etype.prior(Dt,y.etype), x.expr - y.expr)
        static member ( - ) (x:int0,y:num0) = num0(Etype.prior(It 4,y.etype), x.expr - y.expr)
        static member ( - ) (x:num0,y:double) = x-float0(Dbl_c y)
        static member ( - ) (x:num0,y:int) = x-int0(Int_c y)
        static member ( - ) (x:double,y:num0) = float0(Dbl_c x)-y
        static member ( - ) (x:int,y:num0) = int0(Int_c x)-y
        
        static member ( * ) (x:num0,y:num0) = num0(Etype.prior(x.etype,y.etype), x.expr * y.expr)
        static member ( * ) (x:num0,y:complex0) = num0(Etype.prior(x.etype,Zt), x.expr * y.expr)
        static member ( * ) (x:num0,y:float0) = num0(Etype.prior(x.etype,Dt), x.expr * y.expr)
        static member ( * ) (x:num0,y:int0) = num0(Etype.prior(x.etype,It 4), x.expr * y.expr)
        static member ( * ) (x:complex0,y:num0) = num0(Etype.prior(Zt,y.etype), x.expr * y.expr)
        static member ( * ) (x:float0,y:num0) = num0(Etype.prior(Dt,y.etype), x.expr * y.expr)
        static member ( * ) (x:int0,y:num0) = num0(Etype.prior(It 4,y.etype), x.expr * y.expr)
        static member ( * ) (x:num0,y:double) = x*float0(Dbl_c y)
        static member ( * ) (x:num0,y:int) = x*int0(Int_c y)
        static member ( * ) (x:double,y:num0) = float0(Dbl_c x)*y
        static member ( * ) (x:int,y:num0) = int0(Int_c x)*y
        
        static member ( / ) (x:num0,y:num0) = num0(Etype.prior(x.etype,y.etype), x.expr / y.expr)
        static member ( / ) (x:num0,y:complex0) = num0(Etype.prior(x.etype,Zt), x.expr / y.expr)
        static member ( / ) (x:num0,y:float0) = num0(Etype.prior(x.etype,Dt), x.expr / y.expr)
        static member ( / ) (x:num0,y:int0) = num0(Etype.prior(x.etype,It 4), x.expr / y.expr)
        static member ( / ) (x:complex0,y:num0) = num0(Etype.prior(Zt,y.etype), x.expr / y.expr)
        static member ( / ) (x:float0,y:num0) = num0(Etype.prior(Dt,y.etype), x.expr / y.expr)
        static member ( / ) (x:int0,y:num0) = num0(Etype.prior(It 4,y.etype), x.expr / y.expr)
        static member ( / ) (x:num0,y:double) = x/float0(Dbl_c y)
        static member ( / ) (x:num0,y:int) = x/int0(Int_c y)
        static member ( / ) (x:double,y:num0) = float0(Dbl_c x)/y
        static member ( / ) (x:int,y:num0) = int0(Int_c x)/y
        
        static member (<==) (x:num0,y:num0) = 
            match x.etype,y.etype with
            |Dt,It _ -> printfn "Warning: complex型からint型への代入です：%s←%s" x.code y.code
            |Dt,Zt -> printfn "Warning: complex型からdouble型への代入です：%s←%s" x.code y.code
            |It _,Dt -> printfn "Warning: double型からint型への代入です：%s←%s" x.code y.code
            |It 2,It 4 -> printfn "Warning: int型からbyte型への代入です：%s←%s" x.code y.code
            |_ -> ()
            x.expr <== y.expr
        static member (<==) (x:num0,y:complex0) =
            match x.etype with
            |Dt -> printfn "Warning: complex型からdouble型への代入です：%s←%s" x.code y.code
            |It _ -> printfn "Warning: complex型からint型への代入です：%s←%s" x.code y.code
            |_ -> ()
            x.expr <== y.expr
        static member (<==) (x:num0,y:float0) =
            match x.etype with
            |It _ -> printfn "Warning: double型からint型への代入です：%s←%s" x.code y.code
            |_ -> ()
            x.expr <== y.expr
        static member (<==) (x:num0,y:int0) = x.expr <== y.expr
        static member (<==) (x:complex0,y:num0) = x.expr <== y.expr
        static member (<==) (x:float0,y:num0) =
            match y.etype with
            |Zt -> printfn "Warning: complex型からdouble型への代入です：%s←%s" x.code y.code
            |_ -> ()
            x.expr <== y.expr
        static member (<==) (x:int0,y:num0) =
            match y.etype with
            |Dt -> printfn "Warning: double型からint型への代入です：%s←%s" x.code y.code
            |Zt -> printfn "Warning: complex型からint型への代入です：%s←%s" x.code y.code
            |_ -> ()
            x.expr <== y.expr
        static member (<==) (x:num0,y:double) =
            match x.etype with
            |It _ -> printfn "Warning: double型からint型への代入です：%s←%s" x.code <| y.ToString()
            |_ -> ()
            x.expr <== Dbl_c y
        static member (<==) (x:num0,y:int) = x.expr <== Int_c y

        member this.clear() = this <== 0.0
        
    type asm with
        static member pow(x:num0, y:num0) = int0(Expr.pow(x.expr,y.expr))
        static member pow(x:num0, y:complex0) = int0(Expr.pow(x.expr,y.expr))
        static member pow(x:complex0, y:num0) = int0(Expr.pow(x.expr,y.expr))
        static member pow(x:num0, y:float0) = int0(Expr.pow(x.expr,y.expr))
        static member pow(x:float0, y:num0) = int0(Expr.pow(x.expr,y.expr))
        static member pow(x:num0, y:int0) = int0(Expr.pow(x.expr,y.expr))
        static member pow(x:int0, y:num0) = int0(Expr.pow(x.expr,y.expr))

        ///<summary>指数関数</summary>
        static member exp (v:num0) = float0(match p.lang,v.etype with |C,Zt -> Formula("cexp("+v.code+")") |_ -> Formula("exp("+v.code+")"))

        ///<summary>正弦関数</summary>
        static member sin (v:num0) = float0(match p.lang,v.etype with |C,Zt -> Formula("csin("+v.code+")") |_ -> Formula("sin("+v.code+")"))

        ///<summary>余弦関数</summary>
        static member cos (v:num0) = float0(match p.lang,v.etype with |C,Zt -> Formula("ccos("+v.code+")") |_ -> Formula("cos("+v.code+")"))
        static member tan (v:num0) = float0(match p.lang,v.etype with |C,Zt -> Formula("ctan("+v.code+")") |_ -> Formula("tan("+v.code+")"))

        ///<summary>逆正弦関数</summary>
        static member asin (v:num0) = float0(match p.lang,v.etype with |C,Zt -> Formula("casin("+v.code+")") |_ -> Formula("asin("+v.code+")"))
        ///<summary>逆余弦関数</summary>
        static member acos (v:num0) = float0(match p.lang,v.etype with |C,Zt -> Formula("cacos("+v.code+")") |_ -> Formula("acos("+v.code+")"))
        ///<summary>逆正接関数</summary>
        static member atan (v:num0) = float0(match p.lang,v.etype with |C,Zt -> Formula("catan("+v.code+")") |_ -> Formula("atan("+v.code+")"))
        ///<summary>逆正接関数</summary>
        static member atan2(x:num0, y:num0) = float0(match p.lang with |F|C|T|H -> Formula("atan2("+x.code+","+y.code+")"))
        static member atan2(x:num0, y:float0) = float0(match p.lang with |F|C|T|H -> Formula("atan2("+x.code+","+y.code+")"))
        static member atan2(x:float0, y:num0) = float0(match p.lang with |F|C|T|H -> Formula("atan2("+x.code+","+y.code+")"))
        static member atan2(x:num0, y:int0) = float0(match p.lang with |F|C|T|H -> Formula("atan2("+x.code+","+y.code+")"))
        static member atan2(x:int0, y:num0) = float0(match p.lang with |F|C|T|H -> Formula("atan2("+x.code+","+y.code+")"))
        
        ///<summary>絶対値</summary>
        static member abs (v:num0) = float0(match p.lang,v.etype with |C,Zt -> Formula("cabs("+v.code+")") |C,Dt -> Formula("fabs("+v.code+")") |_ -> Formula("abs("+v.code+")"))
        ///<summary>自然対数</summary>
        static member log (v:num0) = float0(match p.lang,v.etype with |C,Zt -> Formula("clog("+v.code+")") |_ -> Formula("log("+v.code+")"))
        ///<summary>常用対数</summary>
        static member log10 (v:num0) = float0(match p.lang,v.etype with |C,Zt -> Formula("clog10("+v.code+")") |_ -> Formula("log10("+v.code+")"))
        ///<summary>平方根</summary>
        static member sqrt (v:num0) = float0(match p.lang,v.etype with |C,Zt -> Formula("csqrt("+v.code+")") |_ -> Formula("sqrt("+v.code+")"))
        ///<summary>小数点以下切り捨て</summary>
        static member floor (v:num0) = int0(match p.lang with |F|C|T|H -> Formula("floor("+v.code+")"))
        
        ///<summary>小数点以下切り上げ</summary>
        static member ceil (v:num0) = int0(match p.lang with |F|C|T|H -> Formula("ceil("+v.code+")"))
        ///<summary>共役複素数</summary>
        static member conj (v:num0) = complex0(match p.lang with |F -> Formula("conjg("+v.code+")") |C|T|H -> Formula("conj("+v.code+")"))
        
    type num0 with
        ///<summary>実部</summary>
        member x.re with get() = float0(match p.lang with |F -> Formula("real("+x.code+")") |C -> Formula("creal("+x.code+")") |_ -> Formula("Re["+x.code+"]"))
        ///<summary>虚部</summary>
        member x.im with get() = float0(match p.lang with |F -> Formula("aimag("+x.code+")") |C -> Formula("cimag("+x.code+")") |_ -> Formula("Im["+x.code+"]"))
        ///<summary>絶対値</summary>
        member x.abs with get() = asm.abs(x)
        ///<summary>絶対値の2乗</summary>
        member x.pow with get() = 
            let a:float0 = asm.abs(x)
            let b = int0(Int_c 2)
            asm.pow(a,b)
        ///<summary>偏角</summary>
        member x.pha with get() = asm.atan2(x.im,x.re)
        
    [<AutoOpen>]
    module coperator =
        type System.Int32 with
            ///<summary>整数をint0型に置換</summary>
            member this.nI with get() =
                num0(It 4, Int_c this)
        type System.Double with
            ///<summary>小数をdouble0型に置換</summary>
            member this.nD with get() =
                num0(Dt, Dbl_c this)