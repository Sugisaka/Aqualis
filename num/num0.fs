// 
// Copyright (c) 2026 Jun-ichiro Sugisaka
// 
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
// 
namespace Aqualis
    
    open System
    
    ///<summary>exprStringの文字列変換時の処理</summary>
    type ExprConcatOption = 
        ///<summary>そのまま連結</summary>
        |Direct
        ///<summary>文字列をダブルクォーテーションで囲んで連結</summary>
        |StrQuotation
        ///<summary>文字列をダブルクォーテーションの文字列で囲んで連結</summary>
        |CodeStrQuotation
        

        
    ///<summary>数値と文字列の結合</summary>
    type reduceExprString = 
        |RStr of string
        |RNvr of expr
        
        member this.etype with get() =
            match this with
            |RStr t -> Structure "string"
            |RNvr t -> t.etype
            

    type bool0(x:expr) =
        member _.Expr with get() = x
        member _.code with get() = x.eval (programList[prIndex])
        static member (++) (x:string,y:bool0) = exprString x ++ exprString y
        static member (++) (x:bool0,y:string) = exprString x ++ exprString y
        static member (++) (x:bool0,y:bool0) = exprString x ++ exprString y
            
        static member (++) (x:bool0,y:exprString) = exprString x ++ y
        static member (.<) (v1:bool0,v2:num0) = 
            match v1.Expr with
            |Less(u1,u2) -> bool0(AND[Less(u1,u2);Less(u2,v2.Expr)])
            |LessEq(u1,u2) -> bool0(AND[Less(u1,u2);Less(u2,v2.Expr)])
            |Greater(u1,u2) ->bool0(AND[Less(u1,u2);Less(u2,v2.Expr)])
            |GreaterEq(u1,u2) ->bool0(AND[Less(u1,u2);Less(u2,v2.Expr)])
            |AND lst ->
                match lst with
                |[] -> bool0 NaN
                |_ ->
                    match lst[lst.Length-1] with
                    |Less(_,u2) |LessEq(_,u2) |Greater(_,u2) |GreaterEq(_,u2) -> 
                        bool0(AND <| lst@[Less(u2,v2.Expr)])
                    |_ -> bool0 NaN
            |_ ->
                bool0 NaN
        static member (.<) (v1:bool0,v2:double) = v1 .< num0(Dbl v2)
        static member (.<) (v1:bool0,v2:int) = v1 .< num0(Int v2)
        static member (.<=) (v1:bool0,v2:num0) = 
            match v1.Expr with
            |Less(u1,u2) -> bool0(AND[Less(u1,u2);LessEq(u2,v2.Expr)])
            |LessEq(u1,u2) -> bool0(AND[Less(u1,u2);LessEq(u2,v2.Expr)])
            |Greater(u1,u2) ->bool0(AND[Less(u1,u2);LessEq(u2,v2.Expr)])
            |GreaterEq(u1,u2) ->bool0(AND[Less(u1,u2);LessEq(u2,v2.Expr)])
            |AND lst ->
                match lst with
                |[] -> bool0 NaN
                |_ ->
                    match lst[lst.Length-1] with
                    |Less(_,u2) |LessEq(_,u2) |Greater(_,u2) |GreaterEq(_,u2) -> 
                        bool0(AND <| lst@[LessEq(u2,v2.Expr)])
                    |_ -> bool0 NaN
            |_ ->
                bool0 NaN
        static member (.<=) (v1:bool0,v2:double) = v1 .<= num0(Dbl v2)
        static member (.<=) (v1:bool0,v2:int) = v1 .<= num0(Int v2)
        static member (.>) (v1:bool0,v2:num0) = 
            match v1.Expr with
            |Less(u1,u2) -> bool0(AND[Less(u1,u2);Greater(u2,v2.Expr)])
            |LessEq(u1,u2) -> bool0(AND[Less(u1,u2);Greater(u2,v2.Expr)])
            |Greater(u1,u2) ->bool0(AND[Less(u1,u2);Greater(u2,v2.Expr)])
            |GreaterEq(u1,u2) ->bool0(AND[Less(u1,u2);Greater(u2,v2.Expr)])
            |AND lst ->
                match lst with
                |[] -> bool0 NaN
                |_ ->
                    match lst[lst.Length-1] with
                    |Less(_,u2) |LessEq(_,u2) |Greater(_,u2) |GreaterEq(_,u2) -> 
                        bool0(AND <| lst@[Greater(u2,v2.Expr)])
                    |_ -> bool0 NaN
            |_ ->
                bool0 NaN
        static member (.>) (v1:bool0,v2:double) = v1 .> num0(Dbl v2)
        static member (.>) (v1:bool0,v2:int) = v1 .> num0(Int v2)
        static member (.>=) (v1:bool0,v2:num0) = 
            match v1.Expr with
            |Less(u1,u2) -> bool0(AND[Less(u1,u2);GreaterEq(u2,v2.Expr)])
            |LessEq(u1,u2) -> bool0(AND[Less(u1,u2);GreaterEq(u2,v2.Expr)])
            |Greater(u1,u2) ->bool0(AND[Less(u1,u2);GreaterEq(u2,v2.Expr)])
            |GreaterEq(u1,u2) ->bool0(AND[Less(u1,u2);GreaterEq(u2,v2.Expr)])
            |AND lst ->
                match lst with
                |[] -> bool0 NaN
                |_ ->
                    match lst[lst.Length-1] with
                    |Less(_,u2) |LessEq(_,u2) |Greater(_,u2) |GreaterEq(_,u2) -> 
                        bool0(AND <| lst@[GreaterEq(u2,v2.Expr)])
                    |_ -> bool0 NaN
            |_ ->
                bool0 NaN
        static member (.>=) (v1:bool0,v2:double) = v1 .>= num0(Dbl v2)
        static member (.>=) (v1:bool0,v2:int) = v1 .>= num0(Int v2)
        
    ///<summary>変数（数値データ）クラス</summary>
    and num0(x:expr) =
        
        member this.Expr with get() = x
        
        member this.etype with get() = x.etype
        
        member this.code with get() = x.eval (programList[prIndex])
        
        ///<summary>優先度の高い型を選択</summary>
        static member ( %% ) (x:num0,y:num0) = 
            match x.etype,y.etype with
            |Zt,(Zt|Dt|It _) -> Zt
            |(Zt|Dt|It _),Zt -> Zt
            |Dt,(Dt|It _) -> Dt
            |(Dt|It _),Dt -> Dt
            |It a,It b -> It (if a>b then a else b)
            |_ -> Nt
            
        static member (++) (x:string,y:num0) = exprString x ++ exprString y
        static member (++) (x:num0,y:string) = exprString x ++ exprString y
        static member (++) (x:num0,y:num0) = exprString x ++ exprString y
            
        static member (++) (x:num0,y:exprString) = exprString x ++ y
        
        ///<summary>負号</summary>
        static member ( ~- ) (x:num0) = num0(Inv(x.etype,x.Expr))
        
        ///<summary>加算</summary>
        static member ( + ) (x:num0,y:num0) = num0(Add(x%%y, x.Expr, y.Expr))
        static member ( + ) (x:num0,y:double) = x + num0(Dbl y)
        static member ( + ) (x:num0,y:int) = x + num0(Int y)
        static member ( + ) (x:double,y:num0) = num0(Dbl x) + y
        static member ( + ) (x:int,y:num0) = num0(Int x) + y
        
        ///<summary>減算</summary>
        static member ( - ) (x:num0,y:num0) = num0(Sub(x%%y, x.Expr, y.Expr))
        static member ( - ) (x:num0,y:double) = x-num0(Dbl y)
        static member ( - ) (x:num0,y:int) = x-num0(Int y)
        static member ( - ) (x:double,y:num0) = num0(Dbl x)-y
        static member ( - ) (x:int,y:num0) = num0(Int x)-y
        
        ///<summary>乗算</summary>
        static member ( * ) (x:num0,y:num0) = num0(Mul(x%%y, x.Expr, y.Expr))
        static member ( * ) (x:num0,y:double) = x*num0(Dbl y)
        static member ( * ) (x:num0,y:int) = x*num0(Int y)
        static member ( * ) (x:double,y:num0) = num0(Dbl x)*y
        static member ( * ) (x:int,y:num0) = num0(Int x)*y
        
        ///<summary>除算</summary>
        static member ( / ) (x:num0,y:num0) = num0(Div(Dt%%x.etype%%y.etype, x.Expr, y.Expr))
        static member ( / ) (x:num0,y:double) = x/num0(Dbl y)
        static member ( / ) (x:num0,y:int) = x/num0(Dbl(double y))
        static member ( / ) (x:double,y:num0) = num0(Dbl x)/y
        static member ( / ) (x:int,y:num0) = num0(Dbl(double x))/y
        
        ///<summary>整数同士の除算(剰余無視)</summary>
        static member ( ./ ) (x:num0,y:num0) = num0(Div(It 4, x.Expr, y.Expr))
        static member ( ./ ) (x:num0,y:int) = x./num0(Int y)
        static member ( ./ ) (x:int,y:num0) = num0(Int x)./y
        
        ///<summary>剰余</summary>
        static member ( % ) (x:num0,y:num0) = num0(Mod(It 4, x.Expr, y.Expr))
        static member ( % ) (x:num0,y:int) = x % num0(Int y)
        static member ( % ) (x:int,y:num0) = num0(Int x) % y
        
        ///<summary>累乗</summary>
        static member powr(x:num0, y:num0) = num0(Pow(x%%y, x.Expr, y.Expr))
        static member ( .** ) (x:num0, y:num0) = num0(Pow(x%%y, x.Expr, y.Expr))
        
        ///<summary>等号</summary>
        static member (.=) (x:num0,y:num0) = bool0(Eq(x.Expr,y.Expr))
        static member (.=) (x:int,y:num0) = num0(Int x) .= y
        static member (.=) (x:double,y:num0) = num0(Dbl x) .= y
        static member (.=) (x:num0,y:int) = x .= num0(Int y)
        static member (.=) (x:num0,y:double) = x .= num0(Dbl y)
        ///<summary>不等号</summary>
        static member (.=/) (x:num0,y:num0) = bool0(NEq(x.Expr,y.Expr))
        static member (.=/) (x:int,y:num0) = num0(Int x) .=/ y
        static member (.=/) (x:double,y:num0) = num0(Dbl x) .=/ y
        static member (.=/) (x:num0,y:int) = x .=/ num0(Int y)
        static member (.=/) (x:num0,y:double) = x .=/ num0(Dbl y)
        ///<summary>比較（より小）</summary>
        static member (.<) (x:num0,y:num0) = bool0(Less(x.Expr,y.Expr))
        static member (.<) (x:int,y:num0) = num0(Int x) .< y
        static member (.<) (x:double,y:num0) = num0(Dbl x) .< y
        static member (.<) (x:num0,y:int) = x .< num0(Int y)
        static member (.<) (x:num0,y:double) = x .< num0(Dbl y)
        
        ///<summary>比較（以下）</summary>
        static member (.<=) (x:num0,y:num0) = bool0(LessEq(x.Expr,y.Expr))
        static member (.<=) (x:int,y:num0) = num0(Int x) .<= y
        static member (.<=) (x:double,y:num0) = num0(Dbl x) .<= y
        static member (.<=) (x:num0,y:int) = x .<= num0(Int y)
        static member (.<=) (x:num0,y:double) = x .<= num0(Dbl y)
                
        static member (.<=) (v1:bool0,v2:double) = v1 .<= num0(Dbl v2) 
        static member (.<=) (v1:bool0,v2:int) = v1 .<= num0(Int v2) 
        
        ///<summary>比較（より大）</summary>
        static member (.>) (x:num0,y:num0) = bool0(Greater(x.Expr,y.Expr))
        static member (.>) (x:int,y:num0) = num0(Int x) .> y
        static member (.>) (x:double,y:num0) = num0(Dbl x) .> y
        static member (.>) (x:num0,y:int) = x .> num0(Int y)
        static member (.>) (x:num0,y:double) = x .> num0(Dbl y)
        
        ///<summary>比較（以上）</summary>
        static member (.>=) (x:num0,y:num0) = bool0(GreaterEq(x.Expr,y.Expr))
        static member (.>=) (x:int,y:num0) = num0(Int x) .>= y
        static member (.>=) (x:double,y:num0) = num0(Dbl x) .>= y
        static member (.>=) (x:num0,y:int) = x .>= num0(Int y)
        static member (.>=) (x:num0,y:double) = x .>= num0(Dbl y)
        
        ///<summary>代入</summary>
        static member (<==) (x:num0,y:num0) = expr.subst x.Expr y.Expr (programList[prIndex])
        static member (<==) (x:num0,y:int) = x <== num0(Int y)
        static member (<==) (x:num0,y:double) = x <== num0(Dbl y)
        static member (<==) (x:num0,y:exprString) = 
            match programList[prIndex].language with 
            |PHP ->
                expr.subst x.Expr (Var(Nt,y.toString(".",StrQuotation),NaN)) (programList[prIndex])
            |_ ->
                printfn "この言語では文字列を含む値を代入できません"
        static member (<==) (x:num0,y:string) = x <== exprString y
        member this.clear() = this <== 0
        
        ///<summary>等式(TeX、HTMLのみ)</summary>
        static member (===) (x:num0,y:num0) = expr.equiv x.Expr y.Expr (programList[prIndex])
        static member (===) (x:num0,y:int) = x === num0(Int y)
        static member (===) (x:num0,y:double) = x === num0(Dbl y)
        
        ///<summary>等号揃等式(TeX、HTMLのみ)</summary>
        static member (=|=) (x:num0,y:num0) = expr.equivAlign x.Expr y.Expr (programList[prIndex])
        static member (=|=) (x:num0,y:int) = x =|= num0(Int y)
        static member (=|=) (x:num0,y:double) = x =|= num0(Dbl y)
        
    ///<summary>数値と文字列の結合</summary>
    and exprString(x:list<reduceExprString>) = 
        new(x:string) = exprString [RStr x]
        new(x:num0) = exprString [RNvr x.Expr]
        new(x:bool0) = exprString [RNvr x.Expr]
        
        member _.data with get() = x
        
        member this.toString(c:string,op:ExprConcatOption) =
            x
            |> List.map (function
                |RStr x ->
                    match op with
                    |Direct -> x
                    |StrQuotation -> "\""+x+"\""
                    |CodeStrQuotation -> "\\\""+x+"\\\""
                |RNvr x -> x.eval (programList[prIndex]))
            |> fun s -> String.Join(c,s)
        static member (++) (a:exprString,b:exprString) : exprString = exprString(a.data@b.data)
        static member (++) (a:string,b:exprString) = exprString a ++ b
        static member (++) (a:exprString,b:string) = a ++ exprString b
        static member (++) (a:exprString,b:num0) = a ++ exprString b
        static member (++) (a:exprString,b:bool0) = a ++ exprString b
        
    [<AutoOpen>]
    module strExpr =
        let st (x:string) = exprString x
        let nv (x:num0) = exprString x
