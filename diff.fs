(*
Copyright (c) 2022 Jun-ichiro Sugisaka

This software is released under the MIT License.
http://opensource.org/licenses/mit-license.php
*)
namespace Aqualis
    
    open System
    open Aqualis_base

    [<AutoOpen>]
    module asmdiff =
        type asm with
            ///<summary>式の微分</summary>
            static member diff(f:num0,x:num0) =
                match x.expr with
                |Var(_,x) ->
                    let rec df(f:Expr) =
                        match f with
                        |Str_c _ ->
                            printfn "文字列を微分できません"
                            NaN
                        |Int_c _ -> Int_c 0
                        |Dbl_c _ -> Int_c 0
                        |Var(_,v) when v=x -> Int_c 1
                        |Var(_,_) -> Int_c 0
                        |Par(_,v) -> df v
                        |Inv(_,v) -> -(df v)
                        |Add(_,v,u) -> (df v)+(df u)
                        |Sub(_,v,u) -> (df v)-(df u)
                        |Mul(_,v,u) -> v*(df u)+(df v)*u
                        |Div(_,v,u) -> (df v)/u-v/(u*u)*(df u)
                        |Pow(_,Abs(_,v),Int_c 2) when v.etype=Zt ->
                            let u = asm.conj(num0(df v))
                            (Int_c 2)*v*u.expr
                        |Pow(t,v,Int_c u) -> Pow(t,v,Int_c (u-1))*(df v)
                        |Pow(t,v,Dbl_c u) -> Pow(t,v,Dbl_c (u-1.0))*(df v)
                        |Pow(_,v,u) -> Exp(u%%v,u*Log(v.etype,v))*(df(u*Log(v.etype,v)))
                        |Exp(t,v) -> Exp(t,v)*(df v)
                        |Sin(t,v) -> Cos(t,v)*(df v)
                        |Cos(t,v) -> -Sin(t,v)*(df v)
                        |Tan(t,v) -> (Int_c 1)/Pow(t,Cos(t,v),Int_c 2)*(df v)
                        |Asin(t,v) -> (df v)/Sqrt(v.etype,Int_c 1-v*v)
                        |Acos(t,v) -> -(df v)/Sqrt(v.etype,Int_c 1-v*v)
                        |Atan(t,v) -> (df v)/(Int_c 1+v*v)
                        |Atan2 _ ->
                            printfn "atan2を微分できません"
                            NaN
                        |Abs(t,v) -> 
                            let u = asm.conj(num0(df v))
                            v/Abs(t,v)*u.expr
                        |Log(t,v) -> (df v)/v
                        |Log10(t,v) -> (Dbl_c(log10(2.718281828459045)))*(df v)/v
                        |Sqrt(t,v) -> (Dbl_c 0.5)*(df v)/Sqrt(t,v)
                        |Formula _ ->
                            printfn "微分できない数式です"
                            NaN
                        |Eq _ ->
                            printfn "比較演算子を微分できません"
                            NaN
                        |NEq _ ->
                            printfn "比較演算子を微分できません"
                            NaN
                        |Greater _ ->
                            printfn "比較演算子を微分できません"
                            NaN
                        |GreaterEq _ ->
                            printfn "比較演算子を微分できません"
                            NaN
                        |Less _ ->
                            printfn "比較演算子を微分できません"
                            NaN
                        |LessEq _ ->
                            printfn "比較演算子を微分できません"
                            NaN
                        |AND _ ->
                            printfn "比較演算子を微分できません"
                            NaN
                        |OR _ ->
                            printfn "比較演算子を微分できません"
                            NaN
                        |Null ->
                            printfn "Nullを微分できません"
                            NaN
                        |NaN ->
                            printfn "NaNを微分できません"
                            NaN
                    num0(df(f.expr))
                |_ ->
                    printfn "Error 変数以外のもの「%s」で微分できません" (x.expr.ToString())
                    num0(NaN)