// 
// Copyright (c) 2026 Jun-ichiro Sugisaka
// 
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
// 
namespace Aqualis
    
    [<AutoOpen>]
    module exprEvalN =
        
        type expr with
            
            ///<summary>指定した範囲でループ</summary>
            static member rangeN (i1:expr) = fun (i2:expr) -> fun code -> 
                let ii1 = i1.simp
                let ii2 = i2.simp
                match ii1,ii2 with
                |Int a, Int b ->
                    for i in a..b do
                        code (Int i)
                |_ ->
                    printfn "ループ範囲「%s → %s」を評価できません。" <| ii1.ToString() <| ii2.simp.ToString()
            member this.eval() =
                match this with
                |True -> 
                    True
                |False -> 
                    False
                |Int x -> 
                    Int x
                |Dbl x -> 
                    Dbl x
                |Cpx (xre,xim) -> 
                    Cpx (xre,xim)
                |Div(Dt,x,y) when x.etype = It 4 && y.etype = It 4 -> 
                    (ToDbl x/ToDbl y).simp.eval()
                |Let (_,y,f) -> 
                    let x = y.simp.eval()
                    match x with
                    |Int _ ->
                        (f x).simp.eval()
                    |Dbl _ ->
                        (f x).simp.eval()
                    |Cpx _ ->
                        (f x).simp.eval()
                    |_ ->
                        printfn "「%s」を数値演算できません" <| y.simp.ToString()
                        NaN
                |IfEl (c,p,q) -> 
                    match c.simp.eval() with
                    |True ->
                        p.simp.eval()
                    |False ->
                        q.simp.eval()
                    |_ ->
                        printfn "条件式「%s」を評価できません" <| c.simp.ToString()
                        NaN
                |Sum(t, n1, n2, f) ->
                    match n1.simp.eval(), n2.simp.eval() with
                    |Int n1,Int n2 ->
                        [n1..n2] 
                        |> List.map (fun i -> (f (Int i)).simp.eval())
                        |> List.fold (fun x y -> (x+y).simp.eval() ) (Int 0)
                    |_ -> 
                        printfn "総和の範囲指定「 %s → %s 」が整数ではありません：" <| n1.simp.ToString() <| n2.simp.ToString()
                        NaN
                |x -> 
                    printfn "「%s」を数値演算できません" <| x.ToString()
                    NaN
