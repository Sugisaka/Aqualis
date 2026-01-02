namespace Aqualis
    
    [<AutoOpen>]
    module exprEvalN =
        
        type expr with
            
            member this.evalN() =
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
                    (ToDbl x/ToDbl y).simp.evalN()
                |Let (_,y,f) -> 
                    let x = y.simp.evalN()
                    match x with
                    |Int _ ->
                        (f x).simp.evalN()
                    |Dbl _ ->
                        (f x).simp.evalN()
                    |Cpx _ ->
                        (f x).simp.evalN()
                    |_ ->
                        printfn "「%s」を数値演算できません" <| y.simp.ToString()
                        NaN
                |IfEl (c,p,q) -> 
                    match c.simp.evalN() with
                    |True ->
                        p.simp.evalN()
                    |False ->
                        q.simp.evalN()
                    |_ ->
                        printfn "条件式「%s」を評価できません" <| c.simp.ToString()
                        NaN
                |Sum(t, n1, n2, f) ->
                    match n1.simp.evalN(), n2.simp.evalN() with
                    |Int n1,Int n2 ->
                        [n1..n2] 
                        |> List.map (fun i -> (f (Int i)).simp.evalN())
                        |> List.fold (fun x y -> (x+y).simp.evalN() ) (Int 0)
                    |_ -> 
                        printfn "総和の範囲指定「 %s → %s 」が整数ではありません：" <| n1.simp.ToString() <| n2.simp.ToString()
                        NaN
                |x -> 
                    printfn "「%s」を数値演算できません" <| x.ToString()
                    NaN
