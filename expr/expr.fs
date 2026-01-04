// 
// Copyright (c) 2026 Jun-ichiro Sugisaka
// 
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
// 
namespace Aqualis
    
    open System
    
    type expr =
        |False
        |True
        |Eq of expr*expr
        |NEq of expr*expr
        |Greater of expr*expr
        |GreaterEq of expr*expr
        |Less of expr*expr
        |LessEq of expr*expr
        |AND of expr list
        |OR of expr list
        |Int of int
        |Dbl of double
        |Cpx of double*double
        |Var of Etype*string*expr
        |Inv of Etype*expr
        |Add of Etype*expr*expr
        |Sub of Etype*expr*expr
        |Mul of Etype*expr*expr
        |Div of Etype*expr*expr
        |Mod of Etype*expr*expr
        |Pow of Etype*expr*expr
        |Exp of Etype*expr
        |Sin of Etype*expr
        |Cos of Etype*expr
        |Tan of Etype*expr
        |Asin of Etype*expr
        |Acos of Etype*expr
        |Atan of Etype*expr
        |Atan2 of expr*expr
        |Abs of Etype*expr
        |Log of Etype*expr
        |Log10 of Etype*expr
        |Sqrt of Etype*expr
        |ToInt of expr
        |ToDbl of expr
        |Floor of expr
        |Ceil of expr
        |Re of expr
        |Im of expr
        |Conj of expr
        |Idx1 of Etype*string*expr
        |Idx2 of Etype*string*expr*expr
        |Idx3 of Etype*string*expr*expr*expr
        |Let of Etype*expr*(expr->expr)
        |IfEl of expr*expr*expr
        |Sum of Etype*expr*expr*(expr->expr)
        |NaN
        
        member this.etype with get() =
            match this with
            |False -> Bt
            |True -> Bt
            |Eq _ -> Bt
            |NEq _ -> Bt
            |Greater _ -> Bt
            |GreaterEq _ -> Bt
            |Less _ -> Bt
            |LessEq _ -> Bt
            |AND _ -> Bt
            |OR _ -> Bt
            |Int _ -> It 4
            |Dbl _ -> Dt
            |Cpx _ -> Zt
            |Var (t,_,_) -> t
            |Inv (t,_) -> t
            |Add (t,_,_) -> t
            |Sub (t,_,_) -> t
            |Mul (t,_,_) -> t
            |Div (t,_,_) -> t
            |Mod (t,_,_) -> t
            |Pow (t,_,_) -> t
            |Exp (t,_) -> t
            |Sin (t,_) -> t
            |Cos (t,_) -> t
            |Tan (t,_) -> t
            |Asin (t,_) -> t
            |Acos (t,_) -> t
            |Atan (t,_) -> t
            |Atan2 _ -> Dt
            |Abs (t,_) -> t
            |Log (t,_) -> t
            |Log10 (t,_) -> t
            |Sqrt (t,_) -> t
            |ToInt _ -> It 4
            |ToDbl _ -> Dt
            |Floor _ -> Dt
            |Ceil _ -> Dt
            |Re _ -> Dt
            |Im _ -> Dt
            |Conj _ -> Zt
            |Idx1 (t,_,_) -> t
            |Idx2 (t,_,_,_) -> t
            |Idx3 (t,_,_,_,_) -> t
            |Let (t,_,_) -> t
            |Sum (t,_,_,_) -> t
            |IfEl (_,a,b) -> a.etype%%b.etype
            |NaN -> Nt
            
        static member ( %% ) (x:expr,y:expr) = x.etype%%y.etype
        static member ( %% ) (x:Etype,y:expr) = x%%y.etype
        static member ( %% ) (x:expr,y:Etype) = x.etype%%y
        
        static member ( + ) (x:expr,y:expr) = Add(x%%y,x,y)
        static member ( - ) (x:expr,y:expr) = Sub(x%%y,x,y)
        static member ( * ) (x:expr,y:expr) = Mul(x%%y,x,y)
        static member ( / ) (x:expr,y:expr) = Div(Dt%%x%%y,x,y)
        static member ( ./ ) (x:expr,y:expr) = Div(It 4,x,y)
        static member ( ~- ) (x:expr) = Inv(x.etype,x)
        static member internal equal(x:expr,y:expr) =
            match x,y with
            |Var(t1,u1,_),Var(t2,u2,_) when t1=t2 && u1=u2 -> true
            |Int u1,Int u2 when u1=u2 -> true
            |Dbl u1,Dbl u2 when u1=u2 -> true
            |Inv(t1,u1),Inv(t2,u2) when t1=t2 && expr.equal(u1,u2) -> true
            |Add(t1,u1,v1),Add(t2,u2,v2) when t1=t2 && (expr.equal(u1,u2) && expr.equal(v1,v2) || expr.equal(u1,v2) && expr.equal(u2,v1)) -> true
            |Sub(t1,u1,v1),Sub(t2,u2,v2) when t1=t2 && expr.equal(u1,u2) && expr.equal(v1,v2) -> true
            |Mul(t1,u1,v1),Mul(t2,u2,v2) when t1=t2 && (expr.equal(u1,u2) && expr.equal(v1,v2) || expr.equal(u1,v2) && expr.equal(u2,v1)) -> true
            |Div(t1,u1,v1),Div(t2,u2,v2) when t1=t2 && expr.equal(u1,u2) && expr.equal(v1,v2) -> true
            |Pow(t1,u1,v1),Pow(t2,u2,v2) when t1=t2 && expr.equal(u1,u2) && expr.equal(v1,v2) -> true
            |Exp(t1,u1),Exp(t2,u2) when t1=t2 && expr.equal(u1,u2) -> true
            |Sin(t1,u1),Sin(t2,u2) when t1=t2 && expr.equal(u1,u2) -> true
            |Cos(t1,u1),Cos(t2,u2) when t1=t2 && expr.equal(u1,u2) -> true
            |Tan(t1,u1),Tan(t2,u2) when t1=t2 && expr.equal(u1,u2) -> true
            |Asin(t1,u1),Asin(t2,u2) when t1=t2 && expr.equal(u1,u2) -> true
            |Acos(t1,u1),Acos(t2,u2) when t1=t2 && expr.equal(u1,u2) -> true
            |Atan(t1,u1),Atan(t2,u2) when t1=t2 && expr.equal(u1,u2) -> true
            |Atan2(u1,v1),Atan2(u2,v2) when expr.equal(u1,u2) && expr.equal(v1,v2) -> true
            |Abs(t1,u1),Abs(t2,u2) when t1=t2 && expr.equal(u1,u2) -> true
            |Log(t1,u1),Log(t2,u2) when t1=t2 && expr.equal(u1,u2) -> true
            |Log10(t1,u1),Log10(t2,u2) when t1=t2 && expr.equal(u1,u2) -> true
            |Sqrt(t1,u1),Sqrt(t2,u2) when t1=t2 && expr.equal(u1,u2) -> true
            |ToDbl u1,ToDbl u2 when expr.equal(u1,u2) -> true
            |ToInt u1,ToInt u2 when expr.equal(u1,u2) -> true
            |Floor u1,Floor u2 when expr.equal(u1,u2) -> true
            |Ceil u1,Ceil u2 when expr.equal(u1,u2) -> true
            |Re u1,Re u2 when expr.equal(u1,u2) -> true
            |Im u1,Im u2 when expr.equal(u1,u2) -> true
            |Conj u1,Conj u2 when expr.equal(u1,u2) -> true
            |Idx1(t1,u1,nA1),Idx1(t2,u2,nA2) when t1=t2 && u1=u2 && expr.equal(nA1,nA2) -> true
            |Idx2(t1,u1,nA1,nB1),Idx2(t2,u2,nA2,nB2) when t1=t2 && u1=u2 && expr.equal(nA1,nA2) && expr.equal(nB1,nB2) -> true
            |Idx3(t1,u1,nA1,nB1,nC1),Idx3(t2,u2,nA2,nB2,nC2) when t1=t2 && u1=u2 && expr.equal(nA1,nA2) && expr.equal(nB1,nB2) && expr.equal(nC1,nC2) -> true
            |NaN,NaN -> true
            |_ -> false
            
        override this.ToString() =
            let rec str (xx:expr,indent:int) =
                let indentStep = 0
                let ss0 = String(' ', 4*indent)
                match xx with
                |False -> ss0 + "False"
                |True -> ss0 + "True"
                |Eq(a,b) -> ss0 + "Eq(" + str(a, indent+indentStep) + ", " + str(b, indent+indentStep) + ") "
                |NEq(a,b) -> ss0 + "NEq(" + str(a, indent+indentStep) + ", " + str(b, indent+indentStep) + ") "
                |Greater(a,b) -> ss0 + "Greater(" + str(a, indent+indentStep) + ", " + str(b, indent+indentStep) + ") "
                |GreaterEq(a,b) -> ss0 + "GreaterEq(" + str(a, indent+indentStep) + ", " + str(b, indent+indentStep) + ") "
                |Less(a,b) -> ss0 + "Less(" + str(a, indent+indentStep) + ", " + str(b, indent+indentStep) + ") "
                |LessEq(a,b) -> ss0 + "LessEq(" + str(a, indent+indentStep) + ", " + str(b, indent+indentStep) + ") "
                |AND lst -> ss0 + "AND(" + String.Join(", ", lst |> List.map (fun p -> str(p, indent+indentStep))) + ") "
                |OR lst -> ss0 + "OR(" + String.Join(", ", lst |> List.map (fun p -> str(p, indent+indentStep))) + ") "
                |Int x -> ss0 + x.ToString()
                |Dbl x -> ss0 + x.ToString()
                |Cpx (re,im) -> ss0 + re.ToString() + "," + re.ToString() + ") "
                |Var (t,n,_) -> ss0 + n
                |Inv (t,x) -> ss0 + "Inv(" + str(x, indent+indentStep) + ") "
                |Add (t,a,b) -> ss0 + "Add(" + str(a, indent+indentStep) + ", " + str(b, indent+indentStep) + ") "
                |Sub (t,a,b) -> ss0 + "Sub(" + str(a, indent+indentStep) + ", " + str(b, indent+indentStep) + ") "
                |Mul (t,a,b) -> ss0 + "Mul(" + str(a, indent+indentStep) + ", " + str(b, indent+indentStep) + ") "
                |Div (t,a,b) -> ss0 + "Div(" + str(a, indent+indentStep) + ", " + str(b, indent+indentStep) + ") "
                |Mod (t,a,b) -> ss0 + "Mod(" + str(a, indent+indentStep) + ", " + str(b, indent+indentStep) + ") "
                |Pow (t,a,b) -> ss0 + "Pow(" + str(a, indent+indentStep) + ", " + str(b, indent+indentStep) + ") "
                |Exp (t,x) -> ss0 + "Exp(" + str(x, indent+indentStep) + ") "
                |Sin (t,x) -> ss0 + "Sin(" + str(x, indent+indentStep) + ") "
                |Cos (t,x) -> ss0 + "Cos(" + str(x, indent+indentStep) + ") "
                |Tan (t,x) -> ss0 + "Tan(" + str(x, indent+indentStep) + ") "
                |Asin (t,x) -> ss0 + "Asin(" + str(x, indent+indentStep) + ") "
                |Acos (t,x) -> ss0 + "Acos(" + str(x, indent+indentStep) + ") "
                |Atan (t,x) -> ss0 + "Atan(" + str(x, indent+indentStep) + ") "
                |Atan2 (a,b) -> ss0 + "Atan2(" + str(a, indent+indentStep) + ", " + str(b, indent+indentStep) + ") "
                |Abs (t,x) -> ss0 + "Abs(" + str(x, indent+indentStep) + ") "
                |Log (t,x) -> ss0 + "Log(" + str(x, indent+indentStep) + ") "
                |Log10 (t,x) -> ss0 + "Log10(" + str(x, indent+indentStep) + ") "
                |Sqrt (t,x) -> ss0 + "Sqrt(" + str(x, indent+indentStep) + ") "
                |ToInt x -> ss0 + "ToInt(" + str(x, indent+indentStep) + ") "
                |ToDbl x -> ss0 + "ToDbl(" + str(x, indent+indentStep) + ") "
                |Floor x -> ss0 + "Floor(" + str(x, indent+indentStep) + ") "
                |Ceil x -> ss0 + "Ceil(" + str(x, indent+indentStep) + ") "
                |Re x -> ss0 + "Re(" + str(x, indent+indentStep) + ") "
                |Im x -> ss0 + "Im(" + str(x, indent+indentStep) + ") "
                |Conj x -> ss0 + "Conj(" + str(x, indent+indentStep) + ") "
                |Idx1 (t,x,i) -> ss0 + "Idx1(" + x + ", " + str(i, indent+indentStep) + ") "
                |Idx2 (t,x,i,j) -> ss0 + "Idx2(" + x + ", " + str(i, indent+indentStep) + ", " + str(j, indent+indentStep) + ") "
                |Idx3 (t,x,i,j,k) -> ss0 + "Idx3(" + x + ", " + str(i, indent+indentStep) + ", " + str(j, indent+indentStep) + ", " + str(k, indent+indentStep) + ") "
                |Let (t,_,_) -> ss0 + "Let"
                |Sum (_,_,_,_) -> ss0 + "Sum"
                |IfEl (_,a,b) -> ss0 + "IfEl"
                |NaN -> ss0 + "NaN"
            str(this, 0)
