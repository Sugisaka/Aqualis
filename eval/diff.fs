// 
// Copyright (c) 2026 Jun-ichiro Sugisaka
// 
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
// 
namespace Aqualis
    
    [<AutoOpen>]
    module exprDiff =
        type expr with
            
            /// fをxで微分(gは変数生成用カウンタ)
            static member diff (f:expr) = fun (x:expr) (g:program) ->
                    match f,x with
                    |Int _,_ ->
                        Int 0
                    |Dbl _,_ ->
                        Int 0
                    |Var(_,vt,_),Var(_,xt,_) when vt = xt ->
                        Int 1
                    |Var _,_ ->
                        Int 0
                    |Inv(t,v),(Var _|Idx1 _|Idx2 _|Idx3 _) ->
                        Inv(t,expr.diff v x g)
                    |Add(t,v,u),(Var _|Idx1 _|Idx2 _|Idx3 _) ->
                        Add(t, expr.diff v x g, expr.diff u x g)
                    |Sub(t,v,u),(Var _|Idx1 _|Idx2 _|Idx3 _) ->
                        Sub(t, expr.diff v x g, expr.diff u x g)
                    |Mul(t,v,u),(Var _|Idx1 _|Idx2 _|Idx3 _) ->
                        Add(t, Mul(t, v, expr.diff u x g), Mul(t, expr.diff v x g, u))
                    |Div(t,v,u),(Var _|Idx1 _|Idx2 _|Idx3 _) ->
                        Sub(t, Div(t, expr.diff v x g, u), Mul(t, Div(t, v, Pow(u.etype, u, Int 2)), expr.diff u x g))
                    |Pow(t, v, Int 2),(Var _|Idx1 _|Idx2 _|Idx3 _) ->
                        Mul(t, Int 2, Mul(t, v, expr.diff v x g))
                    |Pow(t,v,Int u),(Var _|Idx1 _|Idx2 _|Idx3 _) ->
                        Mul(t, Mul(t, Int u, Pow(t, v, Int (u-1))), expr.diff v x g)
                    |Pow(t,v,Dbl u),(Var _|Idx1 _|Idx2 _|Idx3 _) ->
                        Mul(t, Mul(t, Dbl u, Pow(t,v,Dbl (u-1.0))), expr.diff v x g)
                    |Pow(t,v,u),(Var _|Idx1 _|Idx2 _|Idx3 _) ->
                        Mul(u%%v, Exp(u%%v, Mul(t, u, Log(v.etype,v))), expr.diff (Mul(u%%v, u, Log(v.etype,v))) x g)
                    |Exp(t,v),(Var _|Idx1 _|Idx2 _|Idx3 _) ->
                        Mul(t, Exp(t,v), expr.diff v x g)
                    |Sin(t,v),(Var _|Idx1 _|Idx2 _|Idx3 _) ->
                        Mul(t, Cos(t,v), expr.diff v x g)
                    |Cos(t,v),(Var _|Idx1 _|Idx2 _|Idx3 _) ->
                        Inv(t, Mul(t, Sin(t,v), expr.diff v x g))
                    |Tan(t,v),(Var _|Idx1 _|Idx2 _|Idx3 _) ->
                        Mul(t, Div(t, Int 1, Pow(t, Cos(t,v), Int 2)), expr.diff v x g)
                    |Asin(t,v),(Var _|Idx1 _|Idx2 _|Idx3 _) ->
                        Div(t, expr.diff v x g, Sqrt(v.etype, Sub(v.etype, Int 1, Pow(v.etype, v, Int 2))))
                    |Acos(t,v),(Var _|Idx1 _|Idx2 _|Idx3 _) ->
                        Inv(t, Div(t, expr.diff v x g, Sqrt(v.etype,Sub(v.etype, Int 1,Pow(v.etype,v,Int 2)))))
                    |Atan(t,v),(Var _|Idx1 _|Idx2 _|Idx3 _) ->
                        Div(t, expr.diff v x g, Add(t, Int 1, Pow(v.etype,v,Int 2)))
                    |Atan2 _,_ ->
                        printfn "atan2を微分できません"
                        NaN
                    |Abs(Zt,v),(Var _|Idx1 _|Idx2 _|Idx3 _) -> 
                        printfn "複素数の絶対値を微分できません"
                        NaN
                    |Abs(t,v),(Var _|Idx1 _|Idx2 _|Idx3 _) -> 
                        Mul(t, Div(t, v, Abs(t,v)), expr.diff v x g)
                    |Log(t,v),(Var _|Idx1 _|Idx2 _|Idx3 _) ->
                        Div(t, expr.diff v x g, v)
                    |Log10(t,v),(Var _|Idx1 _|Idx2 _|Idx3 _) ->
                        Div(t, Mul(t, Dbl (log10(2.718281828459045)), expr.diff v x g), v)
                    |Sqrt(t,v),(Var _|Idx1 _|Idx2 _|Idx3 _) ->
                        Div(t, Mul(t, Dbl 0.5, expr.diff v x g), Sqrt(t,v))
                    |Idx1(t1,u1,nA1),Idx1(t2,u2,nB1) ->
                        if t1 = t2 && u1 = u2 then
                            IfEl(Eq(nA1,nB1), Int 1, Int 0)
                        else
                            Int 0
                    |Idx2(t1,u1,nA1,nA2),Idx2(t2,u2,nB1,nB2) ->
                        if t1 = t2 && u1 = u2 then
                            IfEl(AND [Eq(nA1,nB1); Eq(nA2,nB2)], Int 1, Int 0)
                        else
                            Int 0
                    |Idx3(t1,u1,nA1,nA2,nA3),Idx3(t2,u2,nB1,nB2,nB3) ->
                        if t1 = t2 && u1 = u2 then
                            IfEl(AND [Eq(nA1,nB1); Eq(nA2,nB2); Eq(nA3,nB3)], Int 1, Int 0)
                        else
                            Int 0
                    |Idx1 _ ,(Var _|Idx2 _|Idx3 _) -> Int 0
                    |Idx2 _ ,(Var _|Idx1 _|Idx3 _) -> Int 0
                    |Idx3 _ ,(Var _|Idx1 _|Idx2 _) -> Int 0
                    |Sum(t, n1, n2, f),_ ->
                        Sum(t, n1, n2, fun n -> expr.diff (f n) x g)
                    |Let(t,u,f),_ ->
                        // 直接計算の場合でも変数を生成し、その変数に対し微分を行う
                        let vname,_ = g.d0.getVar()
                        let v = Var(Dt,vname,NaN)
                        expr.subst v u g
                        expr.diff (f v) x g + expr.diff (f v) v g * expr.diff u x g
                    |NaN,_ ->
                        printfn "NaNを微分できません"
                        NaN
                    |_ ->
                        printfn "Error 「%s」を変数以外のもの「%s」で微分できません" (f.ToString()) (x.ToString())
                        NaN
