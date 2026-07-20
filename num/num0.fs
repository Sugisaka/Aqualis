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


    type bool0(x:expr, ?context:GenerationContext) =
        let context = defaultArg (context |> Option.map Some) GenerationContext.TryCurrent
        member _.Expr with get() = x
        member _.Context = context
        member _.code with get() =
            match context with
            |Some ctx -> x.eval ctx.CurrentProgram
            |None -> x.eval ((GenerationScope.currentProgram()))
        static member (++) (x:string,y:bool0) = exprString x ++ exprString y
        static member (++) (x:bool0,y:string) = exprString x ++ exprString y
        static member (++) (x:bool0,y:bool0) = exprString x ++ exprString y

        static member (++) (x:bool0,y:exprString) = exprString x ++ y
        static member (.<) (v1:bool0,v2:int0) =
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
        static member (.<) (v1:bool0,v2:double0) =
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
        static member (.<) (v1:bool0,v2:double) = v1 .< double0(Dbl v2)
        static member (.<) (v1:bool0,v2:int) = v1 .< int0(Int v2)
        static member (.<=) (v1:bool0,v2:int0) =
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
        static member (.<=) (v1:bool0,v2:double0) =
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
        static member (.<=) (v1:bool0,v2:double) = v1 .<= double0(Dbl v2)
        static member (.<=) (v1:bool0,v2:int) = v1 .<= int0(Int v2)
        static member (.>) (v1:bool0,v2:int0) =
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
        static member (.>) (v1:bool0,v2:double0) =
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
        static member (.>) (v1:bool0,v2:double) = v1 .> double0(Dbl v2)
        static member (.>) (v1:bool0,v2:int) = v1 .> int0(Int v2)
        static member (.>=) (v1:bool0,v2:int0) =
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
        static member (.>=) (v1:bool0,v2:double0) =
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
        static member (.>=) (v1:bool0,v2:double) = v1 .>= double0(Dbl v2)
        static member (.>=) (v1:bool0,v2:int) = v1 .>= int0(Int v2)

    ///<summary>変数（数値データ）クラス</summary>
    and int0(x:expr, ?context:GenerationContext) =
        let context =
            match context with
            |Some explicitContext -> Some explicitContext
            |None ->
                match x with
                |Int _ |Dbl _ |Cpx _ -> None
                |_ -> GenerationContext.TryCurrent

        member this.Expr with get() = x

        member _.Context = context

        member this.etype with get() = x.etype

        member this.code with get() =
            match context with
            |Some ctx -> x.eval ctx.CurrentProgram
            |None -> x.eval ((GenerationScope.currentProgram()))

        ///<summary>優先度の高い型を選択</summary>
        static member ( %% ) (x:int0,y:int0) =
            match x.etype,y.etype with
            |Zt,(Zt|Dt|It _) -> Zt
            |(Zt|Dt|It _),Zt -> Zt
            |Dt,(Dt|It _) -> Dt
            |(Dt|It _),Dt -> Dt
            |It a,It b -> It (if a>b then a else b)
            |_ -> Nt

        static member (++) (x:string,y:int0) = exprString x ++ exprString y
        static member (++) (x:int0,y:string) = exprString x ++ exprString y
        static member (++) (x:int0,y:int0) = exprString x ++ exprString y

        static member (++) (x:int0,y:exprString) = exprString x ++ y

        ///<summary>負号</summary>
        static member ( ~- ) (x:int0) = int0(Inv(x.etype,x.Expr))

        ///<summary>加算</summary>
        static member ( + ) (x:int0,y:int0) = int0(Add(x%%y, x.Expr, y.Expr))
        static member ( + ) (x:int0,y:double) = x + int0(Dbl y)
        static member ( + ) (x:int0,y:int) = x + int0(Int y)
        static member ( + ) (x:double,y:int0) = int0(Dbl x) + y
        static member ( + ) (x:int,y:int0) = int0(Int x) + y

        ///<summary>減算</summary>
        static member ( - ) (x:int0,y:int0) = int0(Sub(x%%y, x.Expr, y.Expr))
        static member ( - ) (x:int0,y:double) = x-int0(Dbl y)
        static member ( - ) (x:int0,y:int) = x-int0(Int y)
        static member ( - ) (x:double,y:int0) = int0(Dbl x)-y
        static member ( - ) (x:int,y:int0) = int0(Int x)-y

        ///<summary>乗算</summary>
        static member ( * ) (x:int0,y:int0) = int0(Mul(x%%y, x.Expr, y.Expr))
        static member ( * ) (x:int0,y:double) = x*int0(Dbl y)
        static member ( * ) (x:int0,y:int) = x*int0(Int y)
        static member ( * ) (x:double,y:int0) = int0(Dbl x)*y
        static member ( * ) (x:int,y:int0) = int0(Int x)*y

        ///<summary>除算</summary>
        static member ( / ) (x:int0,y:int0) = int0(Div(Dt%%x.etype%%y.etype, x.Expr, y.Expr))
        static member ( / ) (x:int0,y:double) = x/int0(Dbl y)
        static member ( / ) (x:int0,y:int) = x/int0(Dbl(double y))
        static member ( / ) (x:double,y:int0) = int0(Dbl x)/y
        static member ( / ) (x:int,y:int0) = int0(Dbl(double x))/y

        ///<summary>整数同士の除算(剰余無視)</summary>
        static member ( ./ ) (x:int0,y:int0) = int0(Div(It 4, x.Expr, y.Expr))
        static member ( ./ ) (x:int0,y:int) = x./int0(Int y)
        static member ( ./ ) (x:int,y:int0) = int0(Int x)./y

        ///<summary>剰余</summary>
        static member ( % ) (x:int0,y:int0) = int0(Mod(It 4, x.Expr, y.Expr))
        static member ( % ) (x:int0,y:int) = x % int0(Int y)
        static member ( % ) (x:int,y:int0) = int0(Int x) % y

        ///<summary>累乗</summary>
        static member powr(x:int0, y:int0) = int0(Pow(x%%y, x.Expr, y.Expr))
        static member ( .** ) (x:int0, y:int0) = int0(Pow(x%%y, x.Expr, y.Expr))

        ///<summary>等号</summary>
        static member (.=) (x:int0,y:int0) = bool0(Eq(x.Expr,y.Expr))
        static member (.=) (x:int,y:int0) = int0(Int x) .= y
        static member (.=) (x:double,y:int0) = int0(Dbl x) .= y
        static member (.=) (x:int0,y:int) = x .= int0(Int y)
        static member (.=) (x:int0,y:double) = x .= int0(Dbl y)
        ///<summary>不等号</summary>
        static member (.=/) (x:int0,y:int0) = bool0(NEq(x.Expr,y.Expr))
        static member (.=/) (x:int,y:int0) = int0(Int x) .=/ y
        static member (.=/) (x:double,y:int0) = int0(Dbl x) .=/ y
        static member (.=/) (x:int0,y:int) = x .=/ int0(Int y)
        static member (.=/) (x:int0,y:double) = x .=/ int0(Dbl y)
        ///<summary>比較（より小）</summary>
        static member (.<) (x:int0,y:int0) = bool0(Less(x.Expr,y.Expr))
        static member (.<) (x:int,y:int0) = int0(Int x) .< y
        static member (.<) (x:double,y:int0) = int0(Dbl x) .< y
        static member (.<) (x:int0,y:int) = x .< int0(Int y)
        static member (.<) (x:int0,y:double) = x .< int0(Dbl y)

        ///<summary>比較（以下）</summary>
        static member (.<=) (x:int0,y:int0) = bool0(LessEq(x.Expr,y.Expr))
        static member (.<=) (x:int,y:int0) = int0(Int x) .<= y
        static member (.<=) (x:double,y:int0) = int0(Dbl x) .<= y
        static member (.<=) (x:int0,y:int) = x .<= int0(Int y)
        static member (.<=) (x:int0,y:double) = x .<= int0(Dbl y)

        static member (.<=) (v1:bool0,v2:double) = v1 .<= int0(Dbl v2)
        static member (.<=) (v1:bool0,v2:int) = v1 .<= int0(Int v2)

        ///<summary>比較（より大）</summary>
        static member (.>) (x:int0,y:int0) = bool0(Greater(x.Expr,y.Expr))
        static member (.>) (x:int,y:int0) = int0(Int x) .> y
        static member (.>) (x:double,y:int0) = int0(Dbl x) .> y
        static member (.>) (x:int0,y:int) = x .> int0(Int y)
        static member (.>) (x:int0,y:double) = x .> int0(Dbl y)

        ///<summary>比較（以上）</summary>
        static member (.>=) (x:int0,y:int0) = bool0(GreaterEq(x.Expr,y.Expr))
        static member (.>=) (x:int,y:int0) = int0(Int x) .>= y
        static member (.>=) (x:double,y:int0) = int0(Dbl x) .>= y
        static member (.>=) (x:int0,y:int) = x .>= int0(Int y)
        static member (.>=) (x:int0,y:double) = x .>= int0(Dbl y)
        static member (~%%) (x:int0) = expr.equivAlign (Var(Nt,"",NaN)) x.Expr ((GenerationScope.currentProgram()))

        ///<summary>代入</summary>
        static member (<==) (x:int0,y:int0) =
            let context =
                match x.Context with
                |Some left ->
                    match y.Context with
                    |Some right when not (obj.ReferenceEquals(left, right)) ->
                        invalidOp "Values from different GenerationContext instances cannot be assigned."
                    |_ -> left
                |None ->
                    invalidOp "The assignment target is not associated with a GenerationContext."
            expr.subst x.Expr y.Expr context.CurrentProgram
        static member (<==) (x:int0,y:int) = x <== int0(Int y)
        static member (<==) (x:int0,y:exprString) =
            let context =
                x.Context
                |> Option.defaultWith (fun () ->
                    invalidOp "The assignment target is not associated with a GenerationContext.")
            match context.CurrentProgram.language with
            |PHP ->
                expr.subst x.Expr (Var(Nt,y.toString(".",StrQuotation),NaN)) context.CurrentProgram
            |_ ->
                printfn "この言語では文字列を含む値を代入できません"
        static member (<==) (x:int0,y:string) = x <== exprString y
        member this.clear() = this <== 0

        ///<summary>等式(TeX、HTMLのみ)</summary>
        static member (===) (x:int0,y:int0) = int0(Var(Nt,x.code+" = "+y.code,NaN)) //expr.equiv x.Expr y.Expr ((GenerationScope.currentProgram()))
        static member (===) (x:int0,y:int) = x === int0(Int y)
        static member (===) (x:int0,y:double) = x === int0(Dbl y)

        ///<summary>等号揃等式(TeX、HTMLのみ)</summary>
        static member (=|=) (x:int0,y:int0) = int0(Var(Nt,x.code+" =& "+y.code,NaN)) //expr.equivAlign x.Expr y.Expr ((GenerationScope.currentProgram()))
        static member (=|=) (x:int0,y:int) = x =|= int0(Int y)
        static member (=|=) (x:int0,y:double) = x =|= int0(Dbl y)
        ///<summary>数式揃位置(TeX、HTMLのみ)</summary>
        static member (.|) (x:int0,y:int0) = int0(Var(Nt,x.code+" & "+y.code,NaN))
        static member html (e:int0) = "\\("+e.code+"\\)"
        static member html (e:exprString) = e.data |> List.fold (fun acc a -> match a with |RStr x -> acc+x |RNvr x -> acc+"\\("+x.evalH (GenerationScope.currentProgram())+"\\)") ""
        static member html (e:list<int0>) = "\\[\\begin{align}"+String.Join("\\\\",e |> List.map (fun f -> f.code))+"\\end{align}\\]"
    ///<summary>変数（数値データ）クラス</summary>
    and double0(x:expr, ?context:GenerationContext) =
        let context =
            match context with
            |Some explicitContext -> Some explicitContext
            |None ->
                match x with
                |Int _ |Dbl _ |Cpx _ -> None
                |_ -> GenerationContext.TryCurrent

        member this.Expr with get() = x

        member _.Context = context

        member this.etype with get() = x.etype

        member this.code with get() =
            match context with
            |Some ctx -> x.eval ctx.CurrentProgram
            |None -> x.eval ((GenerationScope.currentProgram()))

        ///<summary>優先度の高い型を選択</summary>
        static member ( %% ) (x:double0,y:double0) =
            match x.etype,y.etype with
            |Zt,(Zt|Dt|It _) -> Zt
            |(Zt|Dt|It _),Zt -> Zt
            |Dt,(Dt|It _) -> Dt
            |(Dt|It _),Dt -> Dt
            |It a,It b -> It (if a>b then a else b)
            |_ -> Nt

        static member (++) (x:string,y:double0) = exprString x ++ exprString y
        static member (++) (x:double0,y:string) = exprString x ++ exprString y
        static member (++) (x:int0,y:double0) = exprString x ++ exprString y
        static member (++) (x:double0,y:int0) = exprString x ++ exprString y
        static member (++) (x:double0,y:double0) = exprString x ++ exprString y

        static member (++) (x:double0,y:exprString) = exprString x ++ y

        ///<summary>負号</summary>
        static member ( ~- ) (x:double0) = double0(Inv(x.etype,x.Expr))

        ///<summary>加算</summary>
        static member ( + ) (x:int0,y:double0) = double0(Add(Dt, x.Expr, y.Expr))
        static member ( + ) (x:double0,y:int0) = double0(Add(Dt, x.Expr, y.Expr))
        static member ( + ) (x:double0,y:double0) = double0(Add(Dt, x.Expr, y.Expr))
        static member ( + ) (x:double0,y:double) = x + double0(Dbl y)
        static member ( + ) (x:double0,y:int) = x + double0(Int y)
        static member ( + ) (x:double,y:double0) = double0(Dbl x) + y
        static member ( + ) (x:int,y:double0) = double0(Int x) + y

        ///<summary>減算</summary>
        static member ( - ) (x:int0,y:double0) = double0(Sub(Dt, x.Expr, y.Expr))
        static member ( - ) (x:double0,y:int0) = double0(Sub(Dt, x.Expr, y.Expr))
        static member ( - ) (x:double0,y:double0) = double0(Sub(Dt, x.Expr, y.Expr))
        static member ( - ) (x:double0,y:double) = x-double0(Dbl y)
        static member ( - ) (x:double0,y:int) = x-double0(Int y)
        static member ( - ) (x:double,y:double0) = double0(Dbl x)-y
        static member ( - ) (x:int,y:double0) = double0(Int x)-y

        ///<summary>乗算</summary>
        static member ( * ) (x:int0,y:double0) = double0(Mul(Dt, x.Expr, y.Expr))
        static member ( * ) (x:double0,y:int0) = double0(Mul(Dt, x.Expr, y.Expr))
        static member ( * ) (x:double0,y:double0) = double0(Mul(Dt, x.Expr, y.Expr))
        static member ( * ) (x:double0,y:double) = x*double0(Dbl y)
        static member ( * ) (x:double0,y:int) = x*double0(Int y)
        static member ( * ) (x:double,y:double0) = double0(Dbl x)*y
        static member ( * ) (x:int,y:double0) = double0(Int x)*y

        ///<summary>除算</summary>
        static member ( / ) (x:int0,y:double0) = double0(Div(Dt%%x.etype%%y.etype, x.Expr, y.Expr))
        static member ( / ) (x:double0,y:int0) = double0(Div(Dt%%x.etype%%y.etype, x.Expr, y.Expr))
        static member ( / ) (x:double0,y:double0) = double0(Div(Dt%%x.etype%%y.etype, x.Expr, y.Expr))
        static member ( / ) (x:double0,y:double) = x/double0(Dbl y)
        static member ( / ) (x:double0,y:int) = x/double0(Dbl(double y))
        static member ( / ) (x:double,y:double0) = double0(Dbl x)/y
        static member ( / ) (x:int,y:double0) = double0(Dbl(double x))/y

        ///<summary>累乗</summary>
        static member powr(x:int0, y:double0) = double0(Pow(Dt, x.Expr, y.Expr))
        static member powr(x:double0, y:int0) = double0(Pow(Dt, x.Expr, y.Expr))
        static member powr(x:double0, y:double0) = double0(Pow(Dt, x.Expr, y.Expr))
        static member ( .** ) (x:double0, y:double0) = double0(Pow(Dt, x.Expr, y.Expr))

        ///<summary>等号</summary>
        static member (.=) (x:double0,y:double0) = bool0(Eq(x.Expr,y.Expr))
        static member (.=) (x:int,y:double0) = double0(Int x) .= y
        static member (.=) (x:double,y:double0) = double0(Dbl x) .= y
        static member (.=) (x:double0,y:int) = x .= double0(Int y)
        static member (.=) (x:double0,y:double) = x .= double0(Dbl y)
        ///<summary>不等号</summary>
        static member (.=/) (x:double0,y:double0) = bool0(NEq(x.Expr,y.Expr))
        static member (.=/) (x:int,y:double0) = double0(Int x) .=/ y
        static member (.=/) (x:double,y:double0) = double0(Dbl x) .=/ y
        static member (.=/) (x:double0,y:int) = x .=/ double0(Int y)
        static member (.=/) (x:double0,y:double) = x .=/ double0(Dbl y)
        ///<summary>比較（より小）</summary>
        static member (.<) (x:int0,y:double0) = bool0(Less(x.Expr,y.Expr))
        static member (.<) (x:double0,y:int0) = bool0(Less(x.Expr,y.Expr))
        static member (.<) (x:double0,y:double0) = bool0(Less(x.Expr,y.Expr))
        static member (.<) (x:int,y:double0) = double0(Int x) .< y
        static member (.<) (x:double,y:double0) = double0(Dbl x) .< y
        static member (.<) (x:double0,y:int) = x .< double0(Int y)
        static member (.<) (x:double0,y:double) = x .< double0(Dbl y)

        ///<summary>比較（以下）</summary>
        static member (.<=) (x:int0,y:double0) = bool0(LessEq(x.Expr,y.Expr))
        static member (.<=) (x:double0,y:int0) = bool0(LessEq(x.Expr,y.Expr))
        static member (.<=) (x:double0,y:double0) = bool0(LessEq(x.Expr,y.Expr))
        static member (.<=) (x:int,y:double0) = double0(Int x) .<= y
        static member (.<=) (x:double,y:double0) = double0(Dbl x) .<= y
        static member (.<=) (x:double0,y:int) = x .<= double0(Int y)
        static member (.<=) (x:double0,y:double) = x .<= double0(Dbl y)

        static member (.<=) (v1:bool0,v2:double) = v1 .<= double0(Dbl v2)
        static member (.<=) (v1:bool0,v2:int) = v1 .<= double0(Int v2)

        ///<summary>比較（より大）</summary>
        static member (.>) (x:int0,y:double0) = bool0(Greater(x.Expr,y.Expr))
        static member (.>) (x:double0,y:int0) = bool0(Greater(x.Expr,y.Expr))
        static member (.>) (x:double0,y:double0) = bool0(Greater(x.Expr,y.Expr))
        static member (.>) (x:int,y:double0) = double0(Int x) .> y
        static member (.>) (x:double,y:double0) = double0(Dbl x) .> y
        static member (.>) (x:double0,y:int) = x .> double0(Int y)
        static member (.>) (x:double0,y:double) = x .> double0(Dbl y)

        ///<summary>比較（以上）</summary>
        static member (.>=) (x:int0,y:double0) = bool0(GreaterEq(x.Expr,y.Expr))
        static member (.>=) (x:double0,y:int0) = bool0(GreaterEq(x.Expr,y.Expr))
        static member (.>=) (x:double0,y:double0) = bool0(GreaterEq(x.Expr,y.Expr))
        static member (.>=) (x:int,y:double0) = double0(Int x) .>= y
        static member (.>=) (x:double,y:double0) = double0(Dbl x) .>= y
        static member (.>=) (x:double0,y:int) = x .>= double0(Int y)
        static member (.>=) (x:double0,y:double) = x .>= double0(Dbl y)

        ///<summary>代入</summary>
        static member (<==) (x:double0,y:double0) =
            let context =
                match x.Context with
                |Some left ->
                    match y.Context with
                    |Some right when not (obj.ReferenceEquals(left, right)) ->
                        invalidOp "Values from different GenerationContext instances cannot be assigned."
                    |_ -> left
                |None ->
                    invalidOp "The assignment target is not associated with a GenerationContext."
            expr.subst x.Expr y.Expr context.CurrentProgram
        static member (<==) (x:double0,y:int) = x <== double0(Int y)
        static member (<==) (x:double0,y:int0) = x <== double0 y.Expr
        static member (<==) (x:double0,y:double) = x <== double0(Dbl y)
        static member (<==) (x:double0,y:exprString) =
            let context =
                x.Context
                |> Option.defaultWith (fun () ->
                    invalidOp "The assignment target is not associated with a GenerationContext.")
            match context.CurrentProgram.language with
            |PHP ->
                expr.subst x.Expr (Var(Nt,y.toString(".",StrQuotation),NaN)) context.CurrentProgram
            |_ ->
                printfn "この言語では文字列を含む値を代入できません"
        static member (<==) (x:double0,y:string) = x <== exprString y
        member this.clear() = this <== 0

        ///<summary>等式(TeX、HTMLのみ)</summary>
        static member (===) (x:double0,y:double0) = double0(Var(Nt,x.code+" = "+y.code,NaN)) //expr.equiv x.Expr y.Expr ((GenerationScope.currentProgram()))
        static member (===) (x:double0,y:int) = x === double0(Int y)
        static member (===) (x:double0,y:double) = x === double0(Dbl y)

        ///<summary>等号揃等式(TeX、HTMLのみ)</summary>
        static member (=|=) (x:double0,y:double0) = double0(Var(Nt,x.code+" =& "+y.code,NaN)) //expr.equivAlign x.Expr y.Expr ((GenerationScope.currentProgram()))
        static member (=|=) (x:double0,y:int) = x =|= double0(Int y)
        static member (=|=) (x:double0,y:double) = x =|= double0(Dbl y)
        ///<summary>数式揃位置(TeX、HTMLのみ)</summary>
        static member (.|) (x:double0,y:double0) = double0(Var(Nt,x.code+" & "+y.code,NaN))
        static member html (e:double0) = "\\("+e.code+"\\)"
        static member html (e:exprString) = e.data |> List.fold (fun acc a -> match a with |RStr x -> acc+x |RNvr x -> acc+"\\("+x.evalH (GenerationScope.currentProgram())+"\\)") ""
        static member html (e:list<double0>) = "\\[\\begin{align}"+String.Join("\\\\",e |> List.map (fun f -> f.code))+"\\end{align}\\]"
        static member (~%%) (x:double0) = expr.equivAlign (Var(Nt,"",NaN)) x.Expr ((GenerationScope.currentProgram()))
        
    ///<summary>変数（数値データ）クラス</summary>
    and complex0(x:expr, ?context:GenerationContext) =
        let context =
            match context with
            |Some explicitContext -> Some explicitContext
            |None ->
                match x with
                |Int _ |Dbl _ |Cpx _ -> None
                |_ -> GenerationContext.TryCurrent

        member this.Expr with get() = x

        member _.Context = context

        member this.etype with get() = x.etype

        member this.code with get() =
            match context with
            |Some ctx -> x.eval ctx.CurrentProgram
            |None -> x.eval ((GenerationScope.currentProgram()))

        ///<summary>優先度の高い型を選択</summary>
        static member ( %% ) (x:complex0,y:complex0) =
            match x.etype,y.etype with
            |Zt,(Zt|Dt|It _) -> Zt
            |(Zt|Dt|It _),Zt -> Zt
            |Dt,(Dt|It _) -> Dt
            |(Dt|It _),Dt -> Dt
            |It a,It b -> It (if a>b then a else b)
            |_ -> Nt

        static member (++) (x:string,y:complex0) = exprString x ++ exprString y
        static member (++) (x:complex0,y:string) = exprString x ++ exprString y
        static member (++) (x:int0,y:complex0) = exprString x ++ exprString y
        static member (++) (x:double0,y:complex0) = exprString x ++ exprString y
        static member (++) (x:complex0,y:int0) = exprString x ++ exprString y
        static member (++) (x:complex0,y:double0) = exprString x ++ exprString y
        static member (++) (x:complex0,y:complex0) = exprString x ++ exprString y

        static member (++) (x:complex0,y:exprString) = exprString x ++ y

        ///<summary>負号</summary>
        static member ( ~- ) (x:complex0) = complex0(Inv(x.etype,x.Expr))

        ///<summary>加算</summary>
        static member ( + ) (x:int0,y:complex0) = complex0(Add(Zt, x.Expr, y.Expr))
        static member ( + ) (x:double0,y:complex0) = complex0(Add(Zt, x.Expr, y.Expr))
        static member ( + ) (x:complex0,y:int0) = complex0(Add(Zt, x.Expr, y.Expr))
        static member ( + ) (x:complex0,y:double0) = complex0(Add(Zt, x.Expr, y.Expr))
        static member ( + ) (x:complex0,y:complex0) = complex0(Add(Zt, x.Expr, y.Expr))
        static member ( + ) (x:complex0,y:double) = x + complex0(Dbl y)
        static member ( + ) (x:complex0,y:int) = x + complex0(Int y)
        static member ( + ) (x:double,y:complex0) = complex0(Dbl x) + y
        static member ( + ) (x:int,y:complex0) = complex0(Int x) + y

        ///<summary>減算</summary>
        static member ( - ) (x:int0,y:complex0) = complex0(Sub(Zt, x.Expr, y.Expr))
        static member ( - ) (x:double0,y:complex0) = complex0(Sub(Zt, x.Expr, y.Expr))
        static member ( - ) (x:complex0,y:int0) = complex0(Sub(Zt, x.Expr, y.Expr))
        static member ( - ) (x:complex0,y:double0) = complex0(Sub(Zt, x.Expr, y.Expr))
        static member ( - ) (x:complex0,y:complex0) = complex0(Sub(Zt, x.Expr, y.Expr))
        static member ( - ) (x:complex0,y:double) = x-complex0(Dbl y)
        static member ( - ) (x:complex0,y:int) = x-complex0(Int y)
        static member ( - ) (x:double,y:complex0) = complex0(Dbl x)-y
        static member ( - ) (x:int,y:complex0) = complex0(Int x)-y

        ///<summary>乗算</summary>
        static member ( * ) (x:complex0,y:int0) = complex0(Mul(Zt, x.Expr, y.Expr))
        static member ( * ) (x:complex0,y:double0) = complex0(Mul(Zt, x.Expr, y.Expr))
        static member ( * ) (x:complex0,y:complex0) = complex0(Mul(Zt, x.Expr, y.Expr))
        static member ( * ) (x:complex0,y:double) = x*complex0(Dbl y)
        static member ( * ) (x:complex0,y:int) = x*complex0(Int y)
        static member ( * ) (x:double,y:complex0) = complex0(Dbl x)*y
        static member ( * ) (x:double0,y:complex0) = complex0(x.Expr)*y
        static member ( * ) (x:int,y:complex0) = complex0(Int x)*y
        static member ( * ) (x:int0,y:complex0) = complex0(x.Expr)*y

        ///<summary>除算</summary>
        static member ( / ) (x:int0,y:complex0) = complex0(Div(Dt%%x.etype%%y.etype, x.Expr, y.Expr))
        static member ( / ) (x:double0,y:complex0) = complex0(Div(Dt%%x.etype%%y.etype, x.Expr, y.Expr))
        static member ( / ) (x:complex0,y:int0) = complex0(Div(Dt%%x.etype%%y.etype, x.Expr, y.Expr))
        static member ( / ) (x:complex0,y:double0) = complex0(Div(Dt%%x.etype%%y.etype, x.Expr, y.Expr))
        static member ( / ) (x:complex0,y:complex0) = complex0(Div(Dt%%x.etype%%y.etype, x.Expr, y.Expr))
        static member ( / ) (x:complex0,y:double) = x/complex0(Dbl y)
        static member ( / ) (x:complex0,y:int) = x/complex0(Dbl(double y))
        static member ( / ) (x:double,y:complex0) = complex0(Dbl x)/y
        static member ( / ) (x:int,y:complex0) = complex0(Dbl(double x))/y

        ///<summary>累乗</summary>
        static member powr(x:int0, y:complex0) = complex0(Pow(Zt, x.Expr, y.Expr))
        static member powr(x:double0, y:complex0) = complex0(Pow(Zt, x.Expr, y.Expr))
        static member powr(x:complex0, y:int0) = complex0(Pow(Zt, x.Expr, y.Expr))
        static member powr(x:complex0, y:double0) = complex0(Pow(Zt, x.Expr, y.Expr))
        static member powr(x:complex0, y:complex0) = complex0(Pow(Zt, x.Expr, y.Expr))
        static member ( .** ) (x:int0, y:complex0) = complex0(Pow(Zt, x.Expr, y.Expr))
        static member ( .** ) (x:double0, y:complex0) = complex0(Pow(Zt, x.Expr, y.Expr))
        static member ( .** ) (x:complex0, y:int0) = complex0(Pow(Zt, x.Expr, y.Expr))
        static member ( .** ) (x:complex0, y:double0) = complex0(Pow(Zt, x.Expr, y.Expr))
        static member ( .** ) (x:complex0, y:complex0) = complex0(Pow(Zt, x.Expr, y.Expr))

        ///<summary>代入</summary>
        static member (<==) (x:complex0,y:complex0) =
            let context =
                match x.Context with
                |Some left ->
                    match y.Context with
                    |Some right when not (obj.ReferenceEquals(left, right)) ->
                        invalidOp "Values from different GenerationContext instances cannot be assigned."
                    |_ -> left
                |None ->
                    invalidOp "The assignment target is not associated with a GenerationContext."
            expr.subst x.Expr y.Expr context.CurrentProgram
        static member (<==) (x:complex0,y:int) = x <== complex0(Int y)
        static member (<==) (x:complex0,y:int0) = x <== complex0 y.Expr
        static member (<==) (x:complex0,y:double) = x <== complex0(Dbl y)
        static member (<==) (x:complex0,y:double0) = x <== complex0 y.Expr
        static member (<==) (x:complex0,y:exprString) =
            let context =
                x.Context
                |> Option.defaultWith (fun () ->
                    invalidOp "The assignment target is not associated with a GenerationContext.")
            match context.CurrentProgram.language with
            |PHP ->
                expr.subst x.Expr (Var(Nt,y.toString(".",StrQuotation),NaN)) context.CurrentProgram
            |_ ->
                printfn "この言語では文字列を含む値を代入できません"
        static member (<==) (x:complex0,y:string) = x <== exprString y
        member this.clear() = this <== 0

        ///<summary>等式(TeX、HTMLのみ)</summary>
        static member (===) (x:complex0,y:complex0) = complex0(Var(Nt,x.code+" = "+y.code,NaN)) //expr.equiv x.Expr y.Expr ((GenerationScope.currentProgram()))
        static member (===) (x:complex0,y:int) = x === complex0(Int y)
        static member (===) (x:complex0,y:double) = x === complex0(Dbl y)

        ///<summary>等号揃等式(TeX、HTMLのみ)</summary>
        static member (=|=) (x:complex0,y:complex0) = complex0(Var(Nt,x.code+" =& "+y.code,NaN)) //expr.equivAlign x.Expr y.Expr ((GenerationScope.currentProgram()))
        static member (=|=) (x:complex0,y:int) = x =|= complex0(Int y)
        static member (=|=) (x:complex0,y:double) = x =|= complex0(Dbl y)
        ///<summary>数式揃位置(TeX、HTMLのみ)</summary>
        static member (.|) (x:complex0,y:complex0) = complex0(Var(Nt,x.code+" & "+y.code,NaN))
        static member html (e:complex0) = "\\("+e.code+"\\)"
        static member html (e:exprString) = e.data |> List.fold (fun acc a -> match a with |RStr x -> acc+x |RNvr x -> acc+"\\("+x.evalH (GenerationScope.currentProgram())+"\\)") ""
        static member html (e:list<complex0>) = "\\[\\begin{align}"+String.Join("\\\\",e |> List.map (fun f -> f.code))+"\\end{align}\\]"
        static member (~%%) (x:complex0) = expr.equivAlign (Var(Nt,"",NaN)) x.Expr ((GenerationScope.currentProgram()))

    ///<summary>数値と文字列の結合</summary>
    and exprString(x:list<reduceExprString>) =
        new(x:string) = exprString [RStr x]
        new(x:bool0) = exprString [RNvr x.Expr]
        new(x:int0) = exprString [RNvr x.Expr]
        new(x:double0) = exprString [RNvr x.Expr]
        new(x:complex0) = exprString [RNvr x.Expr]

        member _.data with get() = x

        member this.toString(c:string,op:ExprConcatOption) =
            x
            |> List.map (function
                |RStr x ->
                    match op with
                    |Direct -> x
                    |StrQuotation -> "\""+x+"\""
                    |CodeStrQuotation -> "\\\""+x+"\\\""
                |RNvr x -> x.eval ((GenerationScope.currentProgram())))
            |> fun s -> String.Join(c,s)
        static member (++) (a:exprString,b:exprString) : exprString = exprString(a.data@b.data)
        static member (++) (a:string,b:exprString) = exprString a ++ b
        static member (++) (a:exprString,b:string) = a ++ exprString b
        static member (++) (a:exprString,b:int0) = a ++ exprString b
        static member (++) (a:exprString,b:double0) = a ++ exprString b
        static member (++) (a:exprString,b:complex0) = a ++ exprString b
        static member (++) (a:exprString,b:bool0) = a ++ exprString b

    [<AutoOpen>]
    module strExpr =
        let st (x:string) = exprString x
        let iv (x:int0) = exprString x
        let dv (x:double0) = exprString x
        let zv (x:complex0) = exprString x
