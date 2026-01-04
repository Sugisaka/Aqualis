// 
// Copyright (c) 2026 Jun-ichiro Sugisaka
// 
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
// 
namespace Aqualis
    
    open System
    
    [<AutoOpen>]
    module exprSimp =
        
        type expr with
            
            static member simpInv(x:expr) =
                match x with
                |Int v when v <= 0   -> Int -v
                |Dbl v when v <= 0.0 -> Dbl -v
                |Cpx(0.0,0.0) -> Cpx(0.0,0.0)
                |Inv(_,v) -> v
                |Sub(_,a,b) -> expr.simpSub(b,a)
                |_ -> Inv(x.etype,x)
                
            static member simpAdd(x:expr,y:expr) =
                match x,y with
                |_,Int 0|_,Dbl 0.0|_,Cpx(0.0,0.0) -> x
                |Int 0,_|Dbl 0.0,_|Cpx(0.0,0.0),_ -> y
                |Int v1,Int v2 -> Int(v1+v2)
                |Int v1,Dbl v2 -> Dbl(double v1+v2)
                |Int v1,Cpx(v2re,v2im) -> Cpx(double v1+v2re,v2im)
                |Dbl v1,Int v2 -> Dbl(v1+double v2)
                |Dbl v1,Dbl v2 -> Dbl(v1+v2)
                |Dbl v1,Cpx(v2re,v2im) -> Cpx(v1+v2re,v2im)
                |Cpx(v1re,v1im),Int v2 -> Cpx(v1re+double v2,v1im)
                |Cpx(v1re,v1im),Dbl v2 -> Cpx(v1re+v2,v1im)
                |Cpx(v1re,v1im),Cpx(v2re,v2im) -> Cpx(v1re+v2re,v1im+v2im)
                |Inv(_,x),Inv(_,y) -> (-(x+y)).simp
                |_,Inv(_,y) -> (x-y).simp
                |Inv(_,x),_ -> (y-x).simp
                |_ when expr.equal(x,y) -> 
                    (Int 2*x).simp
                |Mul(_,a,b),Mul(_,c,d) when expr.equal(a,c) -> 
                    ((b+d).simp*a).simp
                |Mul(_,a,b),Mul(_,c,d) when expr.equal(a,d) ->
                    ((b+c).simp*a).simp
                |Mul(_,a,b),Mul(_,c,d) when expr.equal(b,c) ->
                    ((a+d).simp*b).simp
                |Mul(_,a,b),Mul(_,c,d) when expr.equal(b,d) ->
                    ((a+c).simp*b).simp
                |Div(_,a,b),Div(_,c,d) when expr.equal(b,d) ->
                    ((a+c).simp/b).simp
                |Add(_,a,b),c ->
                    let a = a.simp
                    let b = b.simp
                    let c = c.simp
                    match a,b,c with
                    |_, (Int _|Dbl _|Cpx _), (Int _|Dbl _|Cpx _) -> ((b+c).simp+a).simp
                    |(Int _|Dbl _|Cpx _), _, (Int _|Dbl _|Cpx _) -> ((a+c).simp+b).simp
                    |(Int _|Dbl _|Cpx _), (Int _|Dbl _|Cpx _), _ -> ((a+b).simp+c).simp
                    |_ -> Add(x%%y,x,y)
                |a,Add(_,b,c) ->
                    let a = a.simp
                    let b = b.simp
                    let c = c.simp
                    match a,b,c with
                    |(Int _|Dbl _|Cpx _), (Int _|Dbl _|Cpx _), _ -> ((a+b).simp+c).simp
                    |(Int _|Dbl _|Cpx _), _, (Int _|Dbl _|Cpx _) -> ((a+c).simp+b).simp
                    |_, (Int _|Dbl _|Cpx _), (Int _|Dbl _|Cpx _) -> ((b+c).simp+a).simp
                    |_ -> Add(x%%y,x,y)
                |Sub(_,a,b),c ->
                    let a = a.simp
                    let b = b.simp
                    let c = c.simp
                    match a,b,c with
                    |_, (Int _|Dbl _|Cpx _), (Int _|Dbl _|Cpx _) -> ((c-b).simp+a).simp
                    |(Int _|Dbl _|Cpx _), _, (Int _|Dbl _|Cpx _) -> ((a+c).simp-b).simp
                    |(Int _|Dbl _|Cpx _), (Int _|Dbl _|Cpx _), _ -> ((a-b).simp+c).simp
                    |_ -> Add(x%%y,x,y)
                |a,Sub(_,b,c) ->
                    let a = a.simp
                    let b = b.simp
                    let c = c.simp
                    match a,b,c with
                    |(Int _|Dbl _|Cpx _), (Int _|Dbl _|Cpx _), _ -> ((a+b).simp-c).simp
                    |(Int _|Dbl _|Cpx _), _, (Int _|Dbl _|Cpx _) -> ((a-c).simp+b).simp
                    |_, (Int _|Dbl _|Cpx _), (Int _|Dbl _|Cpx _) -> ((b-c).simp+a).simp
                    |_ -> Add(x%%y,x,y)
                |_ ->
                    Add(x%%y, x, y)
                
            static member simpSub(x:expr,y:expr) =
                match x,y with
                |_,Int 0|_,Dbl 0.0 -> x
                |Int 0,_|Dbl 0.0,_ -> (-y).simp
                |Int v1,Int v2 -> if v1-v2 < 0 then Inv(It 4,Int(v2-v1)) else Int(v1-v2)
                |Int v1,Dbl v2 -> if double v1-v2 < 0.0 then Inv(Dt,Dbl(v2-double v1)) else Dbl(double v1-v2)
                |Int v1,Cpx(v2re,v2im) -> Cpx(double v1-v2re,-v2im)
                |Dbl v1,Int v2 -> Dbl(v1-double v2)
                |Dbl v1,Dbl v2 -> if v1-v2 < 0.0 then Inv(Dt,Dbl(v2-v1)) else Dbl(v1-v2)
                |Dbl v1,Cpx(v2re,v2im) -> Cpx(v1-v2re,-v2im)
                |Cpx(v1re,v1im),Int v2 -> Cpx(v1re-double v2,v1im)
                |Cpx(v1re,v1im),Dbl v2 -> Cpx(v1re-v2,v1im)
                |Cpx(v1re,v1im),Cpx(v2re,v2im) -> Cpx(v1re-v2re,v1im-v2im)
                |Inv(_,x),Inv(_,y) -> (y-x).simp
                |Inv(_,x),_ -> (-(x+y).simp).simp
                |_,Inv(_,y) -> (x+y).simp
                |v1,v2 when expr.equal(v1,v2) -> Int 0
                |Mul(_,a,b),Mul(_,c,d) when expr.equal(a,c) -> ((b-d).simp*a).simp
                |Mul(_,a,b),Mul(_,c,d) when expr.equal(a,d) -> ((b-c).simp*a).simp
                |Mul(_,a,b),Mul(_,c,d) when expr.equal(b,c) -> ((a-d).simp*b).simp
                |Mul(_,a,b),Mul(_,c,d) when expr.equal(b,d) -> ((a-c).simp*b).simp
                |Div(_,a,b),Div(_,c,d) when expr.equal(b,d) -> ((a-c).simp/b).simp
                |Add(_,a,b),c ->
                    let a = a.simp
                    let b = b.simp
                    let c = c.simp
                    match a,b,c with
                    |_, (Int _|Dbl _|Cpx _), (Int _|Dbl _|Cpx _) -> ((b-c).simp+a).simp
                    |(Int _|Dbl _|Cpx _), _, (Int _|Dbl _|Cpx _) -> ((a-c).simp+b).simp
                    |(Int _|Dbl _|Cpx _), (Int _|Dbl _|Cpx _), _ -> ((a+b).simp-c).simp
                    |_ -> Sub(x%%y,x,y)
                |a,Add(_,b,c) ->
                    let a = a.simp
                    let b = b.simp
                    let c = c.simp
                    match a,b,c with
                    |(Int _|Dbl _|Cpx _), (Int _|Dbl _|Cpx _), _ -> ((a-b).simp-c).simp
                    |(Int _|Dbl _|Cpx _), _, (Int _|Dbl _|Cpx _) -> ((a-c).simp-b).simp
                    |_, (Int _|Dbl _|Cpx _), (Int _|Dbl _|Cpx _) -> ((-(b+c).simp).simp+a).simp
                    |_ -> Sub(x%%y,x,y)
                |Sub(_,a,b),c ->
                    let a = a.simp
                    let b = b.simp
                    let c = c.simp
                    match a,b,c with
                    |_, (Int _|Dbl _|Cpx _), (Int _|Dbl _|Cpx _) -> ((-(b+c).simp).simp+a).simp
                    |(Int _|Dbl _|Cpx _), _, (Int _|Dbl _|Cpx _) -> ((a-c).simp-b).simp
                    |(Int _|Dbl _|Cpx _), (Int _|Dbl _|Cpx _), _ -> ((a-b).simp-c).simp
                    |_ -> Sub(x%%y,x,y)
                |a,Sub(_,b,c) ->
                    let a = a.simp
                    let b = b.simp
                    let c = c.simp
                    match a,b,c with
                    |(Int _|Dbl _|Cpx _), (Int _|Dbl _|Cpx _), _ -> ((a-b).simp+c).simp
                    |(Int _|Dbl _|Cpx _), _, (Int _|Dbl _|Cpx _) -> ((a+c).simp-b).simp
                    |_, (Int _|Dbl _|Cpx _), (Int _|Dbl _|Cpx _) -> ((c-b).simp+a).simp
                    |_ -> Sub(x%%y,x,y)
                |_ -> Sub(x%%y, x, y)
                
            static member simpMul(x:expr,y:expr) =
                match x,y with
                |Int 0,_|Dbl 0.0,_|Cpx (0.0,0.0),_|_,Int 0|_,Dbl 0.0|_,Cpx (0.0,0.0) -> Int 0
                |Int 1,_|Dbl 1.0,_|Cpx (1.0,0.0),_ -> y
                |_,Int 1|_,Dbl 1.0|_,Cpx (1.0,0.0) -> x
                |Int v1,Int v2 -> Int(v1*v2)
                |Int v1,Dbl v2 -> Dbl(double v1*v2)
                |Int v1,Cpx(v2re,v2im) -> Cpx(double v1*v2re,double v1*v2im)
                |Dbl v1,Int v2 -> Dbl(v1*double v2)
                |Dbl v1,Dbl v2 -> Dbl(v1*v2)
                |Dbl v1,Cpx(v2re,v2im) -> Cpx(v1*v2re,v1*v2im)
                |Cpx(v1re,v1im),Int v2 -> Cpx(v1re*double v2,v1im*double v2)
                |Cpx(v1re,v1im),Dbl v2 -> Cpx(v1re*v2,v1im*v2)
                |Cpx(v1re,v1im),Cpx(v2re,v2im) -> Cpx(v1re*v2re-v1im*v2im,v1re*v2im+v1im*v2re)
                |Inv(_,x),Inv(_,y) -> (x*y).simp
                |_,Inv(_,y) -> (-(x*y).simp).simp
                |Inv(_,x),_ -> (-(x*y).simp).simp
                |Mul(_,a,b),c ->
                    let a = a.simp
                    let b = b.simp
                    let c = c.simp
                    match a,b,c with
                    |_, (Int _|Dbl _|Cpx _), (Int _|Dbl _|Cpx _) -> ((b*c).simp*a).simp
                    |(Int _|Dbl _|Cpx _), _, (Int _|Dbl _|Cpx _) -> ((a*c).simp*b).simp
                    |(Int _|Dbl _|Cpx _), (Int _|Dbl _|Cpx _), _ -> ((a*b).simp*c).simp
                    |_ -> Mul(x%%y,x,y)
                |a,Mul(_,b,c) ->
                    let a = a.simp
                    let b = b.simp
                    let c = c.simp
                    match a,b,c with
                    |(Int _|Dbl _|Cpx _), (Int _|Dbl _|Cpx _), _ -> ((a*b).simp*c).simp
                    |(Int _|Dbl _|Cpx _), _, (Int _|Dbl _|Cpx _) -> ((a*c).simp*b).simp
                    |_, (Int _|Dbl _|Cpx _), (Int _|Dbl _|Cpx _) -> ((b*c).simp*a).simp
                    |_ -> Mul(x%%y,x,y)
                |Div(_,a,b),c ->
                    let a = a.simp
                    let b = b.simp
                    let c = c.simp
                    match a,b,c with
                    |_, (Int _|Dbl _|Cpx _), (Int _|Dbl _|Cpx _) -> ((c/b).simp*a).simp
                    |(Int _|Dbl _|Cpx _), _, (Int _|Dbl _|Cpx _) -> ((a*c).simp/b).simp
                    |(Int _|Dbl _|Cpx _), (Int _|Dbl _|Cpx _), _ -> ((a/b).simp*c).simp
                    |_ -> Mul(x%%y,x,y)
                |a,Div(_,b,c) ->
                    let a = a.simp
                    let b = b.simp
                    let c = c.simp
                    match a,b,c with
                    |(Int _|Dbl _|Cpx _), (Int _|Dbl _|Cpx _), _ -> ((a*b).simp/c).simp
                    |(Int _|Dbl _|Cpx _), _, (Int _|Dbl _|Cpx _) -> ((a/c).simp*b).simp
                    |_, (Int _|Dbl _|Cpx _), (Int _|Dbl _|Cpx _) -> ((b/c).simp*a).simp
                    |_ -> Mul(x%%y,x,y)
                |_ -> Mul(x%%y,x,y)
                
            static member simpDiv(x:expr,y:expr) =
                match x,y with
                |_,Int 0 |_,Dbl 0.0 |_,Cpx (0.0,0.0) ->
                    Console.WriteLine "Error: ゼロ割りを検出しました"
                    NaN
                |Int 0,_ -> Int 0
                |Dbl 0.0,_ -> Dbl 0.0
                |Cpx (0.0,0.0),_ -> Cpx (0.0,0.0)
                |_,Int 1 -> x
                |_,Dbl 1.0 -> x
                |_,Cpx (1.0,0.0) -> x
                |Int x,Int y when x%y=0 -> Int (x/y)
                |Int x,Int y -> Dbl (double x / double y)
                |Int x,Dbl y -> Dbl (double x / y)
                |Int x, Cpx (yre,yim) -> Cpx (yre,yim)
                |Dbl x, Int y -> Dbl (x / double y)
                |Dbl x, Dbl y -> Dbl (x/y)
                |Dbl x, Cpx (yre,yim) -> let d = yre*yre+yim*yim in Cpx (x*yre/d,-x*yim/d)
                |Cpx (xre,xim), Int y -> Cpx (xre/double y,xim/double y)
                |Cpx (xre,xim), Dbl y -> Cpx (xre/y,xim/y)
                |Cpx (xre,xim), Cpx (yre,yim) -> let d = yre*yre+yim*yim in Cpx((xre*yre+xim*yim)/d, (-xre*yim+xim*yre)/d)
                |_ when expr.equal(x,y) -> Int 1
                |Inv(_,x),Inv(_,y) -> (x/y).simp
                |_,Inv(_,y) -> (-(x/y).simp).simp
                |Inv(_,x),_ -> (-(x/y).simp).simp
                |Mul(_,a,b),c ->
                    let a = a.simp
                    let b = b.simp
                    let c = c.simp
                    match a,b,c with
                    |_, (Int _|Dbl _|Cpx _), (Int _|Dbl _|Cpx _) -> ((b/c).simp*a).simp
                    |(Int _|Dbl _|Cpx _), _, (Int _|Dbl _|Cpx _) -> ((a/c).simp*b).simp
                    |(Int _|Dbl _|Cpx _), (Int _|Dbl _|Cpx _), _ -> ((a*b).simp/c).simp
                    |_ -> Div(Dt%%x%%y,x,y)
                |a,Mul(_,b,c) ->
                    let a = a.simp
                    let b = b.simp
                    let c = c.simp
                    match a,b,c with
                    |(Int _|Dbl _|Cpx _), (Int _|Dbl _|Cpx _), _ -> ((a/b).simp/c).simp
                    |(Int _|Dbl _|Cpx _), _, (Int _|Dbl _|Cpx _) -> ((a/c).simp/b).simp
                    |_, (Int _|Dbl _|Cpx _), (Int _|Dbl _|Cpx _) -> (Dbl 1.0/(b*c).simp).simp*a
                    |_ -> Div(Dt%%x%%y,x,y)
                |Div(_,a,b),c ->
                    let a = a.simp
                    let b = b.simp
                    let c = c.simp
                    match a,b,c with
                    |_, (Int _|Dbl _|Cpx _), (Int _|Dbl _|Cpx _) -> ((Dbl 1.0/(b*c).simp).simp*a).simp
                    |(Int _|Dbl _|Cpx _), _, (Int _|Dbl _|Cpx _) -> ((a/c).simp/b).simp
                    |(Int _|Dbl _|Cpx _), (Int _|Dbl _|Cpx _), _ -> ((a/b).simp/c).simp
                    |_ -> Div(Dt%%x%%y,x,y)
                |a,Div(_,b,c) ->
                    let a = a.simp
                    let b = b.simp
                    let c = c.simp
                    match a,b,c with
                    |(Int _|Dbl _|Cpx _), (Int _|Dbl _|Cpx _), _ -> ((a/b).simp*c).simp
                    |(Int _|Dbl _|Cpx _), _, (Int _|Dbl _|Cpx _) -> ((a*c).simp/b).simp
                    |_, (Int _|Dbl _|Cpx _), (Int _|Dbl _|Cpx _) -> ((c/b).simp*a).simp
                    |_ -> Div(Dt%%x%%y,x,y)
                |_ -> Div(Dt%%x%%y,x,y)
                
            static member simpIntDiv(x:expr,y:expr) =
                match x,y with
                |_,Int 0 -> 
                    Console.WriteLine "Error: ゼロ割りを検出しました"
                    NaN
                |Int 0,_ -> Int 0
                |_,Int 1 -> x
                |Int x,Int y -> Int (x/y)
                |_ when expr.equal(x,y) -> Int 1
                |Inv(_,x),Inv(_,y) -> (x./y).simp
                |_,Inv(_,v2) -> (-(x./v2).simp).simp
                |Inv(_,v1),_ -> (-(v1./y).simp).simp
                |Mul(_,Int v2,u2),Int v1 when v2%v1=0 -> (Int(v2/v1)*u2).simp
                |Mul(_,v2,Int u2),Int v1 when u2%v1=0 -> (v2*Int(u2/v1)).simp
                |_ -> Div(It 4,x,y)
                
            static member simpMod(x:expr,y:expr) =
                match x,y with
                |_,Int 0 -> 
                    Console.WriteLine "Error: ゼロ割りを検出しました"
                    NaN
                |Int 0,_ -> Int 0
                |_,Int 1 -> Int 0
                |Int v1,Int v2 -> Int(v1 % v2)
                |_ -> Mod(It 4, x,y)
                
            static member simpPow(x:expr,y:expr) =
                match x,y with
                |(Int 0|Dbl 0.0),(Int 0|Dbl 0.0) -> 
                    Console.WriteLine "Error: 0の0乗は未定義です"
                    NaN
                |Int x,Int y when y>0 -> Int (List.fold (fun acc _ -> acc*x) 1 [1..y])
                |Int x, Int y -> Dbl (double x ** double y)
                |Int x, Dbl y -> Dbl (double x ** y)
                |Int _, Cpx _ -> expr.simpExp((y*expr.simpLog x).simp)
                |Dbl x, Int y -> Dbl (x ** double y)
                |Dbl x, Dbl y -> Dbl (x ** y)
                |Dbl _, Cpx _ -> expr.simpExp((y*expr.simpLog x).simp)
                |Cpx _, Int _ -> expr.simpExp((y*expr.simpLog x).simp)
                |Cpx _, Dbl _ -> expr.simpExp((y*expr.simpLog x).simp)
                |Cpx _, Cpx _ -> expr.simpExp((y*expr.simpLog x).simp)
                |(Int 0|Dbl 0.0),_ -> Int 0
                |_,(Int 0|Dbl 0.0) -> Int 1
                |_ -> Pow(x%%y.etype,x,y)
                
            static member simpExp(x:expr) =
                match x with
                |Int x -> Dbl (exp x)
                |Dbl x -> Dbl (exp x)
                |Cpx (re,im) -> let a = exp re in Cpx (a*cos im,a*sin im)
                |_ when x.etype=It 4 -> expr.simpExp(ToDbl x)
                |_ -> Exp(x.etype,x)
                    
            static member simpSin(x:expr) =
                match x with
                |Int x -> if sin (double x) < 0.0 then Inv(Dt,Dbl -(sin (double x))) else Dbl (sin (double x))
                |Dbl x -> if sin x < 0.0 then Inv(Dt,Dbl -(sin x)) else Dbl (sin x)
                |Cpx (re,im) -> 
                    let a1 = exp im
                    let a2 = exp -im
                    Cpx (sin re*(a1+a2)/2.0,cos re*(a1-a2)/2.0)
                |_ when x.etype=It 4 -> expr.simpSin(ToDbl x)
                |_ -> Sin(x.etype,x)
                    
            static member simpCos(x:expr) =
                match x with
                |Int x -> if cos (double x) < 0.0 then Inv(Dt,Dbl -(cos (double x))) else Dbl (cos (double x))
                |Dbl x -> if cos x < 0.0 then Inv(Dt,Dbl -(cos x)) else Dbl (cos x)
                |Cpx (re,im) -> 
                    let a1 = exp im
                    let a2 = exp -im
                    Cpx (cos re*(a1+a2)/2.0,-sin re*(a1-a2)/2.0)
                |_ when x.etype=It 4 -> expr.simpCos(ToDbl x)
                |_ -> Cos(x.etype,x)
                    
            static member simpTan(x:expr) =
                match x with
                |Int x -> if tan (double x) < 0.0 then Inv(Dt,Dbl -(tan (double x))) else Dbl (tan (double x))
                |Dbl x -> if tan x < 0.0 then Inv(Dt,Dbl -(tan x)) else Dbl (tan x)
                |Cpx _ -> expr.simpDiv(expr.simpSin x, expr.simpCos x)
                |_ when x.etype=It 4 -> expr.simpTan(ToDbl x)
                |_ -> Tan(x.etype,x)
                    
            static member simpAsin(x:expr) =
                match x with
                |Int x -> if asin (double x) < 0.0 then Inv(Dt,Dbl -(asin (double x))) else Dbl (asin (double x))
                |Dbl x -> if asin x < 0.0 then Inv(Dt,Dbl -(asin x)) else Dbl (asin x)
                |Cpx _ -> 
                    (-(Cpx(0.0,1.0)*expr.simpLog(((Cpx(0.0,1.0)*x).simp+expr.simpSqrt((Int 1-expr.simpPow(x,Int 2)).simp)).simp)).simp).simp
                |_ when x.etype=It 4 -> expr.simpAsin(ToDbl x)
                |_ -> Asin(x.etype,x)
                
            static member simpAcos(x:expr) =
                match x with
                |Int x -> if acos (double x) < 0.0 then Inv(Dt,Dbl -(acos (double x))) else Dbl (acos (double x))
                |Dbl x -> if acos x < 0.0 then Inv(Dt,Dbl -(acos x)) else Dbl (acos x)
                |Cpx _ -> 
                    (-(Cpx(0.0,1.0)*expr.simpLog((x+(Cpx(0.0,1.0)*expr.simpSqrt((Int 1-expr.simpPow(x,Int 2)).simp)).simp).simp)).simp).simp
                |_ when x.etype=It 4 -> expr.simpAcos(ToDbl x)
                |_ -> Acos(x.etype,x)
                
            static member simpAtan(x:expr) =
                match x with
                |Int x -> if atan (double x) < 0.0 then Inv(Dt,Dbl -(atan (double x))) else Dbl (atan (double x))
                |Dbl x -> if atan x < 0.0 then Inv(Dt,Dbl -(atan x)) else Dbl (atan x)
                |Cpx _ -> 
                    ((Int 1/(Int 2*Cpx(0.0,1.0)).simp).simp*expr.simpLog(((Int 1+(Cpx(0.0,1.0)*x).simp).simp/(Int 1-(Cpx(0.0,1.0)*x).simp).simp).simp)).simp
                |_ when x.etype=It 4 -> expr.simpAtan(ToDbl x)
                |_ -> Atan(x.etype,x)
                
            static member simpAtan2(x:expr,y:expr) =
                match x,y with
                |Int x,Int y -> if atan2 (double x) y < 0.0 then Inv(Dt,Dbl -(atan2 (double x) y)) else Dbl (atan2 (double x) y)
                |Int x,Dbl y -> if atan2 (double x) y < 0.0 then Inv(Dt,Dbl -(atan2 (double x) y)) else Dbl (atan2 (double x) y)
                |Dbl x,Int y -> if atan2 x y < 0.0 then Inv(Dt,Dbl -(atan2 x y)) else Dbl (atan2 x y)
                |Dbl x,Dbl y -> if atan2 x y < 0.0 then Inv(Dt,Dbl -(atan2 x y)) else Dbl (atan2 x y)
                |Cpx _,_ -> expr.simpAtan((y/x).simp)
                |_,Cpx _ -> expr.simpAtan((y/x).simp)
                |_ when x.etype=It 4 && y.etype=It 4 -> expr.simpAtan2(ToDbl x,ToDbl y)
                |_ when x.etype=It 4 -> expr.simpAtan2(ToDbl x, y)
                |_ when y.etype=It 4 -> expr.simpAtan2(x, ToDbl y)
                |_ -> Atan2(x,y)
                
            static member simpAbs(x:expr) =
                match x with
                |Int x -> Dbl (abs x)
                |Dbl x -> Dbl (abs x)
                |Cpx (re,im) -> 
                    (expr.simpPow(Dbl re,Int 2)+expr.simpPow(Dbl im,Int 2)).simp
                |_ -> Abs(Dt,x)
                
            static member simpLog(x:expr) =
                match x with
                |Int x -> if log (double x) < 0.0 then Inv(Dt,Dbl -(log (double x))) else Dbl (log (double x))
                |Dbl x -> if log x < 0.0 then Inv(Dt,Dbl -(log x)) else Dbl (log x)
                |Cpx (re,im) ->
                    (expr.simpLog(expr.simpAbs x)+(Cpx(0.0,1.0)*expr.simpAtan2(Dbl im,Dbl re)).simp).simp
                |_ when x.etype=It 4 -> expr.simpLog(ToDbl x)
                |_ -> Log(x.etype,x)
                
            static member simpLog10(x:expr) =
                match x with
                |Int x -> if log10 (double x) < 0.0 then Inv(Dt,Dbl -(log10 (double x))) else Dbl (log10 (double x))
                |Dbl x -> if log10 x < 0.0 then Inv(Dt,Dbl -(log10 x)) else Dbl (log10 x)
                |Cpx _ -> 
                    (expr.simpLog x/expr.simpLog(Dbl 10.0)).simp
                |_ when x.etype=It 4 -> expr.simpLog10(ToDbl x)
                |_ -> Log10(x.etype,x)
                
            static member simpSqrt(x:expr) =
                match x with
                |Int x ->
                    Dbl (sqrt (double x))
                |Dbl x when x>=0.0 ->
                    Dbl (sqrt x)
                |Dbl x ->
                    (Dbl (sqrt -x)*Cpx(0.0,1.0)).simp
                |Cpx (re,im) -> 
                    let a = expr.simpAbs x
                    (expr.simpSqrt(((a+Dbl re).simp/Int 2).simp)+
                     (Cpx(0.0,1.0)*(Int(if im>0.0 then 1 else -1)*expr.simpSqrt(((a-Dbl re).simp/Int 2).simp)).simp).simp).simp
                |_ when x.etype=It 4 -> expr.simpSqrt(ToDbl x)
                |_ -> Sqrt(x.etype,x)
                
            static member simpToInt(x:expr) =
                match x with
                |Int x -> Int x
                |Dbl x -> Int (int x)
                |_ -> ToInt x
                
            static member simpToDbl(x:expr) =
                match x with
                |Int x -> Dbl (double x)
                |Dbl x -> Dbl x
                |_ -> ToDbl x
                
            static member simpFloor(x:expr) =
                match x with
                |Int x -> Dbl (double x)
                |Dbl x -> Dbl (floor x)
                |_ -> Floor x
                
            static member simpCeil(x:expr) =
                match x with
                |Int x -> Dbl (double x)
                |Dbl x -> Dbl (ceil x)
                |_ -> Ceil x
                
            static member simpRe(x:expr) =
                match x with
                |Cpx (xre,_) -> if xre<0.0 then Inv(Dt,Dbl -xre) else Dbl xre
                |Dbl x -> Dbl x
                |Int x -> Int x
                |_ -> Re x
                
            static member simpIm(x:expr) =
                match x with
                |Cpx (_,xim) -> if xim<0.0 then Inv(Dt,Dbl -xim) else Dbl xim
                |Dbl _ -> Dbl 0.0
                |Int _ -> Int 0
                |_ -> Im x
                
            static member simpConj(x:expr) =
                match x with
                |Cpx (xre,xim) -> Cpx (xre,-xim)
                |Dbl x -> Dbl x
                |Int x -> Int x
                |_ -> Conj x
                
            static member simpEq(x:expr,y:expr) =
                match x,y with
                |Int x,Int y when x=y -> True
                |Int x,Int y -> False
                |Dbl x,Dbl y when x=y -> True
                |Dbl x,Dbl y -> False
                |Cpx (xre,xim),Cpx (yre,yim) when xre=yre && xim=yim -> True
                |Cpx (xre,xim),Cpx (yre,yim) -> False
                |_ -> Eq(x,y)
                
            static member simpNotEq(x:expr,y:expr) =
                match x,y with
                |Int x,Int y when x=y -> False
                |Int x,Int y -> True
                |Dbl x,Dbl y when x=y -> False
                |Dbl x,Dbl y -> True
                |Cpx (xre,xim),Cpx (yre,yim) when xre=yre && xim=yim -> False
                |Cpx (xre,xim),Cpx (yre,yim) -> True
                |_ -> NEq(x,y)
                
            static member simpLess(x:expr,y:expr) =
                match x,y with
                |Int x,Int y when x<y -> True
                |Int x,Int y -> False
                |Dbl x,Dbl y when x<y -> True
                |Dbl x,Dbl y -> False
                |Int x,Dbl y when (double x)<y -> True
                |Int x,Dbl y -> False
                |Dbl x,Int y when x>double y -> True
                |Dbl x,Int y -> False
                |_ -> Less(x,y)
                
            static member simpLessEq(x:expr,y:expr) =
                match x,y with
                |Int x,Int y when x<=y -> True
                |Int x,Int y -> False
                |Dbl x,Dbl y when x<=y -> True
                |Dbl x,Dbl y -> False
                |Int x,Dbl y when double x<=y -> True
                |Int x,Dbl y -> False
                |Dbl x,Int y when x>double y -> True
                |Dbl x,Int y -> False
                |_ -> LessEq(x,y)
                
            static member simpGreater(x:expr,y:expr) =
                match x,y with
                |Int x,Int y when x>y -> True
                |Int x,Int y -> False
                |Dbl x,Dbl y when x>y -> True
                |Dbl x,Dbl y -> False
                |Int x,Dbl y when double x>y -> True
                |Int x,Dbl y -> False
                |Dbl x,Int y when x>double y -> True
                |Dbl x,Int y -> False
                |_ -> Greater(x,y)
                
            static member simpGreaterEq(x:expr,y:expr) =
                match x,y with
                |Int x,Int y when x>y -> True
                |Int x,Int y -> False
                |Dbl x,Dbl y when x>y -> True
                |Dbl x,Dbl y -> False
                |Int x,Dbl y when double x>y -> True
                |Int x,Dbl y -> False
                |Dbl x,Int y when x>double y -> True
                |Dbl x,Int y -> False
                |_ -> GreaterEq(x,y)
                
            static member simpAND(x:list<expr>) = 
                let xx = x |> List.map (fun s -> s.simp) 
                let res =
                    xx |> List.fold (fun acc s -> 
                        match acc,s with
                        |None,True -> Some True
                        |Some True,True -> Some True
                        |Some False,True -> Some False
                        |_,False -> Some False
                        |_ -> Some NaN ) None
                match res with
                |Some True -> True
                |Some False -> False
                |Some _ -> AND xx
                |None -> AND xx
                
            static member simpOR(x:list<expr>) = 
                let xx = x |> List.map (fun s -> s.simp) 
                let res =
                    xx |> List.fold (fun acc s -> 
                        match acc,s with
                        |None,False -> Some False
                        |Some False,False -> Some False
                        |Some True,False -> Some True
                        |_,True -> Some True
                        |_ -> Some NaN ) None
                match res with
                |Some True -> True
                |Some False -> False
                |Some _ -> OR xx
                |None -> OR xx
                
            member this.simp with get() =
                match this with
                |Int n when n < 0 ->
                    Inv(It 4, Int -n)
                |Dbl x when x < 0.0 ->
                    Inv(Dt, Dbl -x)
                |Cpx(re,0.0) -> 
                    Dbl re
                |Add(_,x,y) -> 
                    let x = x.simp
                    let y = y.simp
                    expr.simpAdd(x,y)
                |Sub(_,x,y) ->
                    let x = x.simp
                    let y = y.simp
                    expr.simpSub(x,y)
                |Mul(_,x,y) ->
                    let x = x.simp
                    let y = y.simp
                    expr.simpMul(x,y)
                |Div(It _, x,y) ->
                    let x = x.simp
                    let y = y.simp
                    expr.simpIntDiv(x,y)
                |Div(_, x,y) ->
                    let x = x.simp
                    let y = y.simp
                    expr.simpDiv(x,y)
                |Inv(t,x) ->
                    let x = x.simp
                    expr.simpInv x
                |Mod(_, x,y) ->
                    let x = x.simp
                    let y = y.simp
                    expr.simpMod(x,y)
                |Pow(_, x,y) -> 
                    let x = x.simp
                    let y = y.simp
                    expr.simpPow(x,y)
                |Exp(_, x) -> 
                    let x = x.simp
                    expr.simpExp x
                |Sin(_, x) ->
                    let x = x.simp
                    expr.simpSin x
                |Cos(_, x) ->
                    let x = x.simp
                    expr.simpCos x
                |Tan(_, x) ->
                    let x = x.simp
                    expr.simpTan x
                |Asin(_, x) ->
                    let x = x.simp
                    expr.simpAsin x
                |Acos(_, x) ->
                    let x = x.simp
                    expr.simpAcos x
                |Atan(_, x) ->
                    let x = x.simp
                    expr.simpAtan x
                |Atan2(x,y) ->
                    let x = x.simp
                    let y = y.simp
                    expr.simpAtan2(x,y)
                |Abs(_, x) ->
                    let x = x.simp
                    expr.simpAbs x
                |Log(_, x) ->
                    let x = x.simp
                    expr.simpLog x
                |Log10(_, x) ->
                    let x = x.simp
                    expr.simpLog10 x
                |Sqrt(_, x)  ->
                    let x = x.simp
                    expr.simpSqrt x
                |ToInt x ->
                    let x = x.simp
                    expr.simpToInt x
                |ToDbl x ->
                    let x = x.simp
                    expr.simpToDbl x
                |Conj x ->
                    let x = x.simp
                    expr.simpConj x
                |Eq(x,y) ->
                    let x = x.simp
                    let y = y.simp
                    expr.simpEq(x,y)
                |NEq(x,y) ->
                    let x = x.simp
                    let y = y.simp
                    expr.simpNotEq(x,y)
                |Less(x,y) ->
                    let x = x.simp
                    let y = y.simp
                    expr.simpLess(x,y)
                |LessEq(x,y) ->
                    let x = x.simp
                    let y = y.simp
                    expr.simpLessEq(x,y)
                |Greater(x,y) ->
                    let x = x.simp
                    let y = y.simp
                    expr.simpGreater(x,y)
                |GreaterEq(x,y) ->
                    let x = x.simp
                    let y = y.simp
                    expr.simpGreaterEq(x,y)
                |AND x ->
                    let x = List.map (fun (v:expr) -> v.simp) x
                    expr.simpAND x
                |OR x ->
                    let x = List.map (fun (v:expr) -> v.simp) x
                    expr.simpOR x
                |IfEl (c,p,q) ->
                    match c.simp with
                    |True -> p.simp
                    |False -> q.simp
                    |_ -> this
                |_ ->
                    this
