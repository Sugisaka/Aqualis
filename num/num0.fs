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

    /// Common read-only representation of a scalar numeric expression.
    type INum0 =
        abstract member Code : string
        abstract member Expr : expr
        abstract member Etype : Etype
        abstract member Context : GenerationContext option

    module internal NumericContext =
        let unary (value:INum0) = value.Context
        let binary (left:INum0) (right:INum0) =
            GenerationContextMerge.merge left.Context right.Context
        let many (values:INum0 seq) =
            values |> Seq.map _.Context |> GenerationContextMerge.mergeMany

        let renderedBinary separator (left:INum0) (right:INum0) =
            let context = binary left right
            let program = GenerationContextMerge.requireTarget context |> fun (context:GenerationContext) -> context.CurrentProgram
            Var(Nt, left.Expr.eval(program) + separator + right.Expr.eval(program), NaN), context

    /// Shared storage and generation-context behavior for scalar numeric types.
    [<AbstractClass>]
    type NumericScalar<'Self>(x:expr, ?context:GenerationContext) =
        let context =
            match x with
            |Int _ |Dbl _ |Cpx _ -> None
            |_ -> context

        member _.Expr = x
        member _.Context = context
        member _.etype = x.etype
        member _.code =
            match context with
            |Some ctx -> x.eval ctx.CurrentProgram
            |None -> invalidOp "A symbolic value without a GenerationContext cannot be rendered as code."

        interface INum0 with
            member this.Code = this.code
            member this.Expr = this.Expr
            member this.Etype = this.etype
            member this.Context = this.Context


    type bool0(x:expr, ?context:GenerationContext) =
        let context =
            match x with
            |True |False -> None
            |_ -> context
        member _.Expr with get() = x
        member _.Context = context
        member _.code with get() =
            match context with
            |Some ctx -> x.eval ctx.CurrentProgram
            |None -> invalidOp "A symbolic value without a GenerationContext cannot be rendered as code."
        static member (++) (x:string,y:bool0) = exprString x ++ exprString y
        static member (++) (x:bool0,y:string) = exprString x ++ exprString y
        static member (++) (x:bool0,y:bool0) = exprString x ++ exprString y

        static member (++) (x:bool0,y:exprString) = exprString x ++ y
        static member (.<) (v1:bool0,v2:int0) =
            let result expression = bool0(expression, ?context=GenerationContextMerge.merge v1.Context v2.Context)
            match v1.Expr with
            |Less(u1,u2) -> result (AND[Less(u1,u2);Less(u2,v2.Expr)])
            |LessEq(u1,u2) -> result (AND[Less(u1,u2);Less(u2,v2.Expr)])
            |Greater(u1,u2) ->result (AND[Less(u1,u2);Less(u2,v2.Expr)])
            |GreaterEq(u1,u2) ->result (AND[Less(u1,u2);Less(u2,v2.Expr)])
            |AND lst ->
                match lst with
                |[] -> result NaN
                |_ ->
                    match lst[lst.Length-1] with
                    |Less(_,u2) |LessEq(_,u2) |Greater(_,u2) |GreaterEq(_,u2) ->
                        result (AND <| lst@[Less(u2,v2.Expr)])
                    |_ -> result NaN
            |_ ->
                result NaN
        static member (.<) (v1:bool0,v2:double0) =
            let result expression = bool0(expression, ?context=GenerationContextMerge.merge v1.Context v2.Context)
            match v1.Expr with
            |Less(u1,u2) -> result (AND[Less(u1,u2);Less(u2,v2.Expr)])
            |LessEq(u1,u2) -> result (AND[Less(u1,u2);Less(u2,v2.Expr)])
            |Greater(u1,u2) ->result (AND[Less(u1,u2);Less(u2,v2.Expr)])
            |GreaterEq(u1,u2) ->result (AND[Less(u1,u2);Less(u2,v2.Expr)])
            |AND lst ->
                match lst with
                |[] -> result NaN
                |_ ->
                    match lst[lst.Length-1] with
                    |Less(_,u2) |LessEq(_,u2) |Greater(_,u2) |GreaterEq(_,u2) ->
                        result (AND <| lst@[Less(u2,v2.Expr)])
                    |_ -> result NaN
            |_ ->
                result NaN
        static member (.<) (v1:bool0,v2:double) = v1 .< double0(Dbl v2)
        static member (.<) (v1:bool0,v2:int) = v1 .< int0(Int v2)
        static member (.<=) (v1:bool0,v2:int0) =
            let result expression = bool0(expression, ?context=GenerationContextMerge.merge v1.Context v2.Context)
            match v1.Expr with
            |Less(u1,u2) -> result (AND[Less(u1,u2);LessEq(u2,v2.Expr)])
            |LessEq(u1,u2) -> result (AND[Less(u1,u2);LessEq(u2,v2.Expr)])
            |Greater(u1,u2) ->result (AND[Less(u1,u2);LessEq(u2,v2.Expr)])
            |GreaterEq(u1,u2) ->result (AND[Less(u1,u2);LessEq(u2,v2.Expr)])
            |AND lst ->
                match lst with
                |[] -> result NaN
                |_ ->
                    match lst[lst.Length-1] with
                    |Less(_,u2) |LessEq(_,u2) |Greater(_,u2) |GreaterEq(_,u2) ->
                        result (AND <| lst@[LessEq(u2,v2.Expr)])
                    |_ -> result NaN
            |_ ->
                result NaN
        static member (.<=) (v1:bool0,v2:double0) =
            let result expression = bool0(expression, ?context=GenerationContextMerge.merge v1.Context v2.Context)
            match v1.Expr with
            |Less(u1,u2) -> result (AND[Less(u1,u2);LessEq(u2,v2.Expr)])
            |LessEq(u1,u2) -> result (AND[Less(u1,u2);LessEq(u2,v2.Expr)])
            |Greater(u1,u2) ->result (AND[Less(u1,u2);LessEq(u2,v2.Expr)])
            |GreaterEq(u1,u2) ->result (AND[Less(u1,u2);LessEq(u2,v2.Expr)])
            |AND lst ->
                match lst with
                |[] -> result NaN
                |_ ->
                    match lst[lst.Length-1] with
                    |Less(_,u2) |LessEq(_,u2) |Greater(_,u2) |GreaterEq(_,u2) ->
                        result (AND <| lst@[LessEq(u2,v2.Expr)])
                    |_ -> result NaN
            |_ ->
                result NaN
        static member (.<=) (v1:bool0,v2:double) = v1 .<= double0(Dbl v2)
        static member (.<=) (v1:bool0,v2:int) = v1 .<= int0(Int v2)
        static member (.>) (v1:bool0,v2:int0) =
            let result expression = bool0(expression, ?context=GenerationContextMerge.merge v1.Context v2.Context)
            match v1.Expr with
            |Less(u1,u2) -> result (AND[Less(u1,u2);Greater(u2,v2.Expr)])
            |LessEq(u1,u2) -> result (AND[Less(u1,u2);Greater(u2,v2.Expr)])
            |Greater(u1,u2) ->result (AND[Less(u1,u2);Greater(u2,v2.Expr)])
            |GreaterEq(u1,u2) ->result (AND[Less(u1,u2);Greater(u2,v2.Expr)])
            |AND lst ->
                match lst with
                |[] -> result NaN
                |_ ->
                    match lst[lst.Length-1] with
                    |Less(_,u2) |LessEq(_,u2) |Greater(_,u2) |GreaterEq(_,u2) ->
                        result (AND <| lst@[Greater(u2,v2.Expr)])
                    |_ -> result NaN
            |_ ->
                result NaN
        static member (.>) (v1:bool0,v2:double0) =
            let result expression = bool0(expression, ?context=GenerationContextMerge.merge v1.Context v2.Context)
            match v1.Expr with
            |Less(u1,u2) -> result (AND[Less(u1,u2);Greater(u2,v2.Expr)])
            |LessEq(u1,u2) -> result (AND[Less(u1,u2);Greater(u2,v2.Expr)])
            |Greater(u1,u2) ->result (AND[Less(u1,u2);Greater(u2,v2.Expr)])
            |GreaterEq(u1,u2) ->result (AND[Less(u1,u2);Greater(u2,v2.Expr)])
            |AND lst ->
                match lst with
                |[] -> result NaN
                |_ ->
                    match lst[lst.Length-1] with
                    |Less(_,u2) |LessEq(_,u2) |Greater(_,u2) |GreaterEq(_,u2) ->
                        result (AND <| lst@[Greater(u2,v2.Expr)])
                    |_ -> result NaN
            |_ ->
                result NaN
        static member (.>) (v1:bool0,v2:double) = v1 .> double0(Dbl v2)
        static member (.>) (v1:bool0,v2:int) = v1 .> int0(Int v2)
        static member (.>=) (v1:bool0,v2:int0) =
            let result expression = bool0(expression, ?context=GenerationContextMerge.merge v1.Context v2.Context)
            match v1.Expr with
            |Less(u1,u2) -> result (AND[Less(u1,u2);GreaterEq(u2,v2.Expr)])
            |LessEq(u1,u2) -> result (AND[Less(u1,u2);GreaterEq(u2,v2.Expr)])
            |Greater(u1,u2) ->result (AND[Less(u1,u2);GreaterEq(u2,v2.Expr)])
            |GreaterEq(u1,u2) ->result (AND[Less(u1,u2);GreaterEq(u2,v2.Expr)])
            |AND lst ->
                match lst with
                |[] -> result NaN
                |_ ->
                    match lst[lst.Length-1] with
                    |Less(_,u2) |LessEq(_,u2) |Greater(_,u2) |GreaterEq(_,u2) ->
                        result (AND <| lst@[GreaterEq(u2,v2.Expr)])
                    |_ -> result NaN
            |_ ->
                result NaN
        static member (.>=) (v1:bool0,v2:double0) =
            let result expression = bool0(expression, ?context=GenerationContextMerge.merge v1.Context v2.Context)
            match v1.Expr with
            |Less(u1,u2) -> result (AND[Less(u1,u2);GreaterEq(u2,v2.Expr)])
            |LessEq(u1,u2) -> result (AND[Less(u1,u2);GreaterEq(u2,v2.Expr)])
            |Greater(u1,u2) ->result (AND[Less(u1,u2);GreaterEq(u2,v2.Expr)])
            |GreaterEq(u1,u2) ->result (AND[Less(u1,u2);GreaterEq(u2,v2.Expr)])
            |AND lst ->
                match lst with
                |[] -> result NaN
                |_ ->
                    match lst[lst.Length-1] with
                    |Less(_,u2) |LessEq(_,u2) |Greater(_,u2) |GreaterEq(_,u2) ->
                        result (AND <| lst@[GreaterEq(u2,v2.Expr)])
                    |_ -> result NaN
            |_ ->
                result NaN
        static member (.>=) (v1:bool0,v2:double) = v1 .>= double0(Dbl v2)
        static member (.>=) (v1:bool0,v2:int) = v1 .>= int0(Int v2)

    ///<summary>変数（数値データ）クラス</summary>
    and int0(x:expr, ?context:GenerationContext) =
        inherit NumericScalar<int0>(x,?context=context)

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
        static member ( ~- ) (x:int0) = int0(Inv(x.etype,x.Expr), ?context=x.Context)

        ///<summary>加算</summary>
        static member ( + ) (x:int0,y:int0) = int0(Add(x%%y, x.Expr, y.Expr), ?context=NumericContext.binary x y)
        static member ( + ) (x:int0,y:double) = x + double0(Dbl y)
        static member ( + ) (x:int0,y:int) = x + int0(Int y)
        static member ( + ) (x:double,y:int0) = double0(Dbl x) + y
        static member ( + ) (x:int,y:int0) = int0(Int x) + y

        ///<summary>減算</summary>
        static member ( - ) (x:int0,y:int0) = int0(Sub(x%%y, x.Expr, y.Expr), ?context=NumericContext.binary x y)
        static member ( - ) (x:int0,y:double) = x-double0(Dbl y)
        static member ( - ) (x:int0,y:int) = x-int0(Int y)
        static member ( - ) (x:double,y:int0) = double0(Dbl x)-y
        static member ( - ) (x:int,y:int0) = int0(Int x)-y

        ///<summary>乗算</summary>
        static member ( * ) (x:int0,y:int0) = int0(Mul(x%%y, x.Expr, y.Expr), ?context=NumericContext.binary x y)
        static member ( * ) (x:int0,y:double) = x*double0(Dbl y)
        static member ( * ) (x:int0,y:int) = x*int0(Int y)
        static member ( * ) (x:double,y:int0) = double0(Dbl x)*y
        static member ( * ) (x:int,y:int0) = int0(Int x)*y

        ///<summary>除算</summary>
        static member ( / ) (x:int0,y:int0) = double0(Div(Dt%%x.etype%%y.etype, x.Expr, y.Expr), ?context=NumericContext.binary x y)
        static member ( / ) (x:int0,y:double) = x/int0(Dbl y)
        static member ( / ) (x:int0,y:int) = x/int0(Dbl(double y))
        static member ( / ) (x:double,y:int0) = double0(Dbl x)/y
        static member ( / ) (x:int,y:int0) = double0(Dbl(double x))/y

        ///<summary>整数同士の除算(剰余無視)</summary>
        static member ( ./ ) (x:int0,y:int0) = int0(Div(It 4, x.Expr, y.Expr), ?context=NumericContext.binary x y)
        static member ( ./ ) (x:int0,y:int) = x./int0(Int y)
        static member ( ./ ) (x:int,y:int0) = int0(Int x)./y

        ///<summary>剰余</summary>
        static member ( % ) (x:int0,y:int0) = int0(Mod(It 4, x.Expr, y.Expr), ?context=NumericContext.binary x y)
        static member ( % ) (x:int0,y:int) = x % int0(Int y)
        static member ( % ) (x:int,y:int0) = int0(Int x) % y

        ///<summary>累乗</summary>
        static member powr(x:int0, y:int0) = int0(Pow(x%%y, x.Expr, y.Expr), ?context=NumericContext.binary x y)
        static member ( .** ) (x:int0, y:int0) = int0(Pow(x%%y, x.Expr, y.Expr), ?context=NumericContext.binary x y)

        ///<summary>等号</summary>
        static member (.=) (x:int0,y:int0) = bool0(Eq(x.Expr,y.Expr), ?context=NumericContext.binary x y)
        static member (.=) (x:int,y:int0) = int0(Int x) .= y
        static member (.=) (x:double,y:int0) = int0(Dbl x) .= y
        static member (.=) (x:int0,y:int) = x .= int0(Int y)
        static member (.=) (x:int0,y:double) = x .= int0(Dbl y)
        ///<summary>不等号</summary>
        static member (.=/) (x:int0,y:int0) = bool0(NEq(x.Expr,y.Expr), ?context=NumericContext.binary x y)
        static member (.=/) (x:int,y:int0) = int0(Int x) .=/ y
        static member (.=/) (x:double,y:int0) = int0(Dbl x) .=/ y
        static member (.=/) (x:int0,y:int) = x .=/ int0(Int y)
        static member (.=/) (x:int0,y:double) = x .=/ int0(Dbl y)
        ///<summary>比較（より小）</summary>
        static member (.<) (x:int0,y:int0) = bool0(Less(x.Expr,y.Expr), ?context=NumericContext.binary x y)
        static member (.<) (x:int,y:int0) = int0(Int x) .< y
        static member (.<) (x:double,y:int0) = int0(Dbl x) .< y
        static member (.<) (x:int0,y:int) = x .< int0(Int y)
        static member (.<) (x:int0,y:double) = x .< int0(Dbl y)

        ///<summary>比較（以下）</summary>
        static member (.<=) (x:int0,y:int0) = bool0(LessEq(x.Expr,y.Expr), ?context=NumericContext.binary x y)
        static member (.<=) (x:int,y:int0) = int0(Int x) .<= y
        static member (.<=) (x:double,y:int0) = int0(Dbl x) .<= y
        static member (.<=) (x:int0,y:int) = x .<= int0(Int y)
        static member (.<=) (x:int0,y:double) = x .<= int0(Dbl y)

        static member (.<=) (v1:bool0,v2:double) = v1 .<= int0(Dbl v2)
        static member (.<=) (v1:bool0,v2:int) = v1 .<= int0(Int v2)

        ///<summary>比較（より大）</summary>
        static member (.>) (x:int0,y:int0) = bool0(Greater(x.Expr,y.Expr), ?context=NumericContext.binary x y)
        static member (.>) (x:int,y:int0) = int0(Int x) .> y
        static member (.>) (x:double,y:int0) = int0(Dbl x) .> y
        static member (.>) (x:int0,y:int) = x .> int0(Int y)
        static member (.>) (x:int0,y:double) = x .> int0(Dbl y)

        ///<summary>比較（以上）</summary>
        static member (.>=) (x:int0,y:int0) = bool0(GreaterEq(x.Expr,y.Expr), ?context=NumericContext.binary x y)
        static member (.>=) (x:int,y:int0) = int0(Int x) .>= y
        static member (.>=) (x:double,y:int0) = int0(Dbl x) .>= y
        static member (.>=) (x:int0,y:int) = x .>= int0(Int y)
        static member (.>=) (x:int0,y:double) = x .>= int0(Dbl y)
        static member (~%%) (x:int0) =
            let context = GenerationContextMerge.requireTarget x.Context
            expr.equivAlign (Var(Nt,"",NaN)) x.Expr context.CurrentProgram

        ///<summary>代入</summary>
        static member (<==) (x:int0,y:int0) =
            let context = GenerationContextMerge.requireTarget x.Context
            GenerationContextMerge.merge x.Context y.Context |> ignore
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
        static member (===) (x:int0,y:int0) =
            let value, context = NumericContext.renderedBinary " = " x y
            int0(value, ?context=context)
        static member (===) (x:int0,y:int) = x === int0(Int y)
        static member (===) (x:int0,y:double) = x === int0(Dbl y)

        ///<summary>等号揃等式(TeX、HTMLのみ)</summary>
        static member (=|=) (x:int0,y:int0) =
            let value, context = NumericContext.renderedBinary " =& " x y
            int0(value, ?context=context)
        static member (=|=) (x:int0,y:int) = x =|= int0(Int y)
        static member (=|=) (x:int0,y:double) = x =|= int0(Dbl y)
        ///<summary>数式揃位置(TeX、HTMLのみ)</summary>
        static member (.|) (x:int0,y:int0) =
            let value, context = NumericContext.renderedBinary " & " x y
            int0(value, ?context=context)
        static member html (e:int0) = "\\("+e.code+"\\)"
        static member html (e:exprString) =
            let program = GenerationContextMerge.requireTarget e.Context |> fun (context:GenerationContext) -> context.CurrentProgram
            e.data |> List.fold (fun acc a -> match a with |RStr x -> acc+x |RNvr x -> acc+"\\("+x.evalH program+"\\)") ""
        static member html (e:list<int0>) = "\\[\\begin{align}"+String.Join("\\\\",e |> List.map (fun f -> f.code))+"\\end{align}\\]"
    ///<summary>変数（数値データ）クラス</summary>
    and double0(x:expr, ?context:GenerationContext) =
        inherit NumericScalar<double0>(x,?context=context)

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
        static member ( ~- ) (x:double0) = double0(Inv(x.etype,x.Expr), ?context=x.Context)

        ///<summary>加算</summary>
        static member ( + ) (x:int0,y:double0):double0 = double0(Add(Dt, x.Expr, y.Expr), ?context=NumericContext.binary x y)
        static member ( + ) (x:double0,y:int0):double0 = double0(Add(Dt, x.Expr, y.Expr), ?context=NumericContext.binary x y)
        static member ( + ) (x:double0,y:double0) = double0(Add(Dt, x.Expr, y.Expr), ?context=NumericContext.binary x y)
        static member ( + ) (x:double0,y:double) = x + double0(Dbl y)
        static member ( + ) (x:double0,y:int) = x + double0(Int y)
        static member ( + ) (x:double,y:double0) = double0(Dbl x) + y
        static member ( + ) (x:int,y:double0) = double0(Int x) + y

        ///<summary>減算</summary>
        static member ( - ) (x:int0,y:double0):double0 = double0(Sub(Dt, x.Expr, y.Expr), ?context=NumericContext.binary x y)
        static member ( - ) (x:double0,y:int0):double0 = double0(Sub(Dt, x.Expr, y.Expr), ?context=NumericContext.binary x y)
        static member ( - ) (x:double0,y:double0) = double0(Sub(Dt, x.Expr, y.Expr), ?context=NumericContext.binary x y)
        static member ( - ) (x:double0,y:double) = x-double0(Dbl y)
        static member ( - ) (x:double0,y:int) = x-double0(Int y)
        static member ( - ) (x:double,y:double0) = double0(Dbl x)-y
        static member ( - ) (x:int,y:double0) = double0(Int x)-y

        ///<summary>乗算</summary>
        static member ( * ) (x:int0,y:double0):double0 = double0(Mul(Dt, x.Expr, y.Expr), ?context=NumericContext.binary x y)
        static member ( * ) (x:double0,y:int0):double0 = double0(Mul(Dt, x.Expr, y.Expr), ?context=NumericContext.binary x y)
        static member ( * ) (x:double0,y:double0) = double0(Mul(Dt, x.Expr, y.Expr), ?context=NumericContext.binary x y)
        static member ( * ) (x:double0,y:double) = x*double0(Dbl y)
        static member ( * ) (x:double0,y:int) = x*double0(Int y)
        static member ( * ) (x:double,y:double0) = double0(Dbl x)*y
        static member ( * ) (x:int,y:double0) = double0(Int x)*y

        ///<summary>除算</summary>
        static member ( / ) (x:int0,y:double0) = double0(Div(Dt%%x.etype%%y.etype, x.Expr, y.Expr), ?context=NumericContext.binary x y)
        static member ( / ) (x:double0,y:int0):double0 = double0(Div(Dt%%x.etype%%y.etype, x.Expr, y.Expr), ?context=NumericContext.binary x y)
        static member ( / ) (x:double0,y:double0) = double0(Div(Dt%%x.etype%%y.etype, x.Expr, y.Expr), ?context=NumericContext.binary x y)
        static member ( / ) (x:double0,y:double) = x/double0(Dbl y)
        static member ( / ) (x:double0,y:int) = x/double0(Dbl(double y))
        static member ( / ) (x:double,y:double0) = double0(Dbl x)/y
        static member ( / ) (x:int,y:double0) = double0(Dbl(double x))/y

        ///<summary>累乗</summary>
        static member powr(x:int0, y:double0) = double0(Pow(Dt, x.Expr, y.Expr), ?context=NumericContext.binary x y)
        static member powr(x:double0, y:int0) = double0(Pow(Dt, x.Expr, y.Expr), ?context=NumericContext.binary x y)
        static member powr(x:double0, y:int) = double0(Pow(Dt, x.Expr, Int y), ?context=x.Context)
        static member powr(x:int, y:double0) = double0(Pow(Dt, Int x, y.Expr), ?context=y.Context)
        static member powr(x:double0, y:double0) = double0(Pow(Dt, x.Expr, y.Expr), ?context=NumericContext.binary x y)
        static member powr(x:double0, y:double) = double0(Pow(Dt, x.Expr, Dbl y), ?context=x.Context)
        static member powr(x:double, y:double0) = double0(Pow(Dt, Dbl x, y.Expr), ?context=y.Context)
        static member ( .** ) (x:int0, y:double0) = double0(Pow(Dt, x.Expr, y.Expr), ?context=NumericContext.binary x y)
        static member ( .** ) (x:double0, y:int0) = double0(Pow(Dt, x.Expr, y.Expr), ?context=NumericContext.binary x y)
        static member ( .** ) (x:double0, y:int) = double0(Pow(Dt, x.Expr, Int y), ?context=x.Context)
        static member ( .** ) (x:int, y:double0) = double0(Pow(Dt, Int x, y.Expr), ?context=y.Context)
        static member ( .** ) (x:double0, y:double0) = double0(Pow(Dt, x.Expr, y.Expr), ?context=NumericContext.binary x y)
        static member ( .** ) (x:double0, y:double) = double0(Pow(Dt, x.Expr, Dbl y), ?context=x.Context)
        static member ( .** ) (x:double, y:double0) = double0(Pow(Dt, Dbl x, y.Expr), ?context=y.Context)

        ///<summary>等号</summary>
        static member (.=) (x:double0,y:double0) = bool0(Eq(x.Expr,y.Expr), ?context=NumericContext.binary x y)
        static member (.=) (x:int,y:double0) = double0(Int x) .= y
        static member (.=) (x:double,y:double0) = double0(Dbl x) .= y
        static member (.=) (x:double0,y:int) = x .= double0(Int y)
        static member (.=) (x:double0,y:double) = x .= double0(Dbl y)
        ///<summary>不等号</summary>
        static member (.=/) (x:double0,y:double0) = bool0(NEq(x.Expr,y.Expr), ?context=NumericContext.binary x y)
        static member (.=/) (x:int,y:double0) = double0(Int x) .=/ y
        static member (.=/) (x:double,y:double0) = double0(Dbl x) .=/ y
        static member (.=/) (x:double0,y:int) = x .=/ double0(Int y)
        static member (.=/) (x:double0,y:double) = x .=/ double0(Dbl y)
        ///<summary>比較（より小）</summary>
        static member (.<) (x:int0,y:double0) = bool0(Less(x.Expr,y.Expr), ?context=NumericContext.binary x y)
        static member (.<) (x:double0,y:int0) = bool0(Less(x.Expr,y.Expr), ?context=NumericContext.binary x y)
        static member (.<) (x:double0,y:double0) = bool0(Less(x.Expr,y.Expr), ?context=NumericContext.binary x y)
        static member (.<) (x:int,y:double0) = double0(Int x) .< y
        static member (.<) (x:double,y:double0) = double0(Dbl x) .< y
        static member (.<) (x:double0,y:int) = x .< double0(Int y)
        static member (.<) (x:double0,y:double) = x .< double0(Dbl y)

        ///<summary>比較（以下）</summary>
        static member (.<=) (x:int0,y:double0) = bool0(LessEq(x.Expr,y.Expr), ?context=NumericContext.binary x y)
        static member (.<=) (x:double0,y:int0) = bool0(LessEq(x.Expr,y.Expr), ?context=NumericContext.binary x y)
        static member (.<=) (x:double0,y:double0) = bool0(LessEq(x.Expr,y.Expr), ?context=NumericContext.binary x y)
        static member (.<=) (x:int,y:double0) = double0(Int x) .<= y
        static member (.<=) (x:double,y:double0) = double0(Dbl x) .<= y
        static member (.<=) (x:double0,y:int) = x .<= double0(Int y)
        static member (.<=) (x:double0,y:double) = x .<= double0(Dbl y)

        static member (.<=) (v1:bool0,v2:double) = v1 .<= double0(Dbl v2)
        static member (.<=) (v1:bool0,v2:int) = v1 .<= double0(Int v2)

        ///<summary>比較（より大）</summary>
        static member (.>) (x:int0,y:double0) = bool0(Greater(x.Expr,y.Expr), ?context=NumericContext.binary x y)
        static member (.>) (x:double0,y:int0) = bool0(Greater(x.Expr,y.Expr), ?context=NumericContext.binary x y)
        static member (.>) (x:double0,y:double0) = bool0(Greater(x.Expr,y.Expr), ?context=NumericContext.binary x y)
        static member (.>) (x:int,y:double0) = double0(Int x) .> y
        static member (.>) (x:double,y:double0) = double0(Dbl x) .> y
        static member (.>) (x:double0,y:int) = x .> double0(Int y)
        static member (.>) (x:double0,y:double) = x .> double0(Dbl y)

        ///<summary>比較（以上）</summary>
        static member (.>=) (x:int0,y:double0) = bool0(GreaterEq(x.Expr,y.Expr), ?context=NumericContext.binary x y)
        static member (.>=) (x:double0,y:int0) = bool0(GreaterEq(x.Expr,y.Expr), ?context=NumericContext.binary x y)
        static member (.>=) (x:double0,y:double0) = bool0(GreaterEq(x.Expr,y.Expr), ?context=NumericContext.binary x y)
        static member (.>=) (x:int,y:double0) = double0(Int x) .>= y
        static member (.>=) (x:double,y:double0) = double0(Dbl x) .>= y
        static member (.>=) (x:double0,y:int) = x .>= double0(Int y)
        static member (.>=) (x:double0,y:double) = x .>= double0(Dbl y)

        ///<summary>代入</summary>
        static member (<==) (x:double0,y:double0) =
            let context = GenerationContextMerge.requireTarget x.Context
            GenerationContextMerge.merge x.Context y.Context |> ignore
            expr.subst x.Expr y.Expr context.CurrentProgram
        static member (<==) (x:double0,y:int) = x <== double0(Int y)
        static member (<==) (x:double0,y:int0) = x <== double0(y.Expr, ?context=y.Context)
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
        static member (===) (x:double0,y:double0) =
            let value, context = NumericContext.renderedBinary " = " x y
            double0(value, ?context=context)
        static member (===) (x:double0,y:int) = x === double0(Int y)
        static member (===) (x:double0,y:double) = x === double0(Dbl y)

        ///<summary>等号揃等式(TeX、HTMLのみ)</summary>
        static member (=|=) (x:double0,y:double0) =
            let value, context = NumericContext.renderedBinary " =& " x y
            double0(value, ?context=context)
        static member (=|=) (x:double0,y:int) = x =|= double0(Int y)
        static member (=|=) (x:double0,y:double) = x =|= double0(Dbl y)
        ///<summary>数式揃位置(TeX、HTMLのみ)</summary>
        static member (.|) (x:double0,y:double0) =
            let value, context = NumericContext.renderedBinary " & " x y
            double0(value, ?context=context)
        static member html (e:double0) = "\\("+e.code+"\\)"
        static member html (e:exprString) =
            let program = GenerationContextMerge.requireTarget e.Context |> fun (context:GenerationContext) -> context.CurrentProgram
            e.data |> List.fold (fun acc a -> match a with |RStr x -> acc+x |RNvr x -> acc+"\\("+x.evalH program+"\\)") ""
        static member html (e:list<double0>) = "\\[\\begin{align}"+String.Join("\\\\",e |> List.map (fun f -> f.code))+"\\end{align}\\]"
        static member (~%%) (x:double0) =
            let context = GenerationContextMerge.requireTarget x.Context
            expr.equivAlign (Var(Nt,"",NaN)) x.Expr context.CurrentProgram
        
    ///<summary>変数（数値データ）クラス</summary>
    and complex0(x:expr, ?context:GenerationContext) =
        inherit NumericScalar<complex0>(x,?context=context)

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
        static member ( ~- ) (x:complex0) = complex0(Inv(x.etype,x.Expr), ?context=x.Context)

        ///<summary>加算</summary>
        static member ( + ) (x:int0,y:complex0) = complex0(Add(Zt, x.Expr, y.Expr), ?context=NumericContext.binary x y)
        static member ( + ) (x:double0,y:complex0) = complex0(Add(Zt, x.Expr, y.Expr), ?context=NumericContext.binary x y)
        static member ( + ) (x:complex0,y:int0) = complex0(Add(Zt, x.Expr, y.Expr), ?context=NumericContext.binary x y)
        static member ( + ) (x:complex0,y:double0) = complex0(Add(Zt, x.Expr, y.Expr), ?context=NumericContext.binary x y)
        static member ( + ) (x:complex0,y:complex0) = complex0(Add(Zt, x.Expr, y.Expr), ?context=NumericContext.binary x y)
        static member ( + ) (x:complex0,y:double) = x + complex0(Dbl y)
        static member ( + ) (x:complex0,y:int) = x + complex0(Int y)
        static member ( + ) (x:double,y:complex0) = complex0(Dbl x) + y
        static member ( + ) (x:int,y:complex0) = complex0(Int x) + y

        ///<summary>減算</summary>
        static member ( - ) (x:int0,y:complex0) = complex0(Sub(Zt, x.Expr, y.Expr), ?context=NumericContext.binary x y)
        static member ( - ) (x:double0,y:complex0) = complex0(Sub(Zt, x.Expr, y.Expr), ?context=NumericContext.binary x y)
        static member ( - ) (x:complex0,y:int0) = complex0(Sub(Zt, x.Expr, y.Expr), ?context=NumericContext.binary x y)
        static member ( - ) (x:complex0,y:double0) = complex0(Sub(Zt, x.Expr, y.Expr), ?context=NumericContext.binary x y)
        static member ( - ) (x:complex0,y:complex0) = complex0(Sub(Zt, x.Expr, y.Expr), ?context=NumericContext.binary x y)
        static member ( - ) (x:complex0,y:double) = x-complex0(Dbl y)
        static member ( - ) (x:complex0,y:int) = x-complex0(Int y)
        static member ( - ) (x:double,y:complex0) = complex0(Dbl x)-y
        static member ( - ) (x:int,y:complex0) = complex0(Int x)-y

        ///<summary>乗算</summary>
        static member ( * ) (x:complex0,y:int0) = complex0(Mul(Zt, x.Expr, y.Expr), ?context=NumericContext.binary x y)
        static member ( * ) (x:complex0,y:double0) = complex0(Mul(Zt, x.Expr, y.Expr), ?context=NumericContext.binary x y)
        static member ( * ) (x:complex0,y:complex0) = complex0(Mul(Zt, x.Expr, y.Expr), ?context=NumericContext.binary x y)
        static member ( * ) (x:complex0,y:double) = x*complex0(Dbl y)
        static member ( * ) (x:complex0,y:int) = x*complex0(Int y)
        static member ( * ) (x:double,y:complex0) = complex0(Dbl x)*y
        static member ( * ) (x:double0,y:complex0) = complex0(x.Expr, ?context=x.Context)*y
        static member ( * ) (x:int,y:complex0) = complex0(Int x)*y
        static member ( * ) (x:int0,y:complex0) = complex0(x.Expr, ?context=x.Context)*y

        ///<summary>除算</summary>
        static member ( / ) (x:int0,y:complex0) = complex0(Div(Dt%%x.etype%%y.etype, x.Expr, y.Expr), ?context=NumericContext.binary x y)
        static member ( / ) (x:double0,y:complex0) = complex0(Div(Dt%%x.etype%%y.etype, x.Expr, y.Expr), ?context=NumericContext.binary x y)
        static member ( / ) (x:complex0,y:int0) = complex0(Div(Dt%%x.etype%%y.etype, x.Expr, y.Expr), ?context=NumericContext.binary x y)
        static member ( / ) (x:complex0,y:double0) = complex0(Div(Dt%%x.etype%%y.etype, x.Expr, y.Expr), ?context=NumericContext.binary x y)
        static member ( / ) (x:complex0,y:complex0) = complex0(Div(Dt%%x.etype%%y.etype, x.Expr, y.Expr), ?context=NumericContext.binary x y)
        static member ( / ) (x:complex0,y:double) = x/complex0(Dbl y)
        static member ( / ) (x:complex0,y:int) = x/complex0(Dbl(double y))
        static member ( / ) (x:double,y:complex0) = complex0(Dbl x)/y
        static member ( / ) (x:int,y:complex0) = complex0(Dbl(double x))/y

        ///<summary>累乗</summary>
        static member powr(x:int0, y:complex0) = complex0(Pow(Zt, x.Expr, y.Expr), ?context=NumericContext.binary x y)
        static member powr(x:complex0, y:int0) = complex0(Pow(Zt, x.Expr, y.Expr), ?context=NumericContext.binary x y)
        static member powr(x:double0, y:complex0) = complex0(Pow(Zt, x.Expr, y.Expr), ?context=NumericContext.binary x y)
        static member powr(x:complex0, y:double0) = complex0(Pow(Zt, x.Expr, y.Expr), ?context=NumericContext.binary x y)
        static member powr(x:int, y:complex0) = complex0(Pow(Zt, Int x, y.Expr), ?context=y.Context)
        static member powr(x:complex0, y:int) = complex0(Pow(Zt, x.Expr, Int y), ?context=x.Context)
        static member powr(x:double, y:complex0) = complex0(Pow(Zt, Dbl x, y.Expr), ?context=y.Context)
        static member powr(x:complex0, y:double) = complex0(Pow(Zt, x.Expr, Dbl y), ?context=x.Context)
        static member powr(x:complex0, y:complex0) = complex0(Pow(Zt, x.Expr, y.Expr), ?context=NumericContext.binary x y)
        static member ( .** ) (x:int0, y:complex0) = complex0(Pow(Zt, x.Expr, y.Expr), ?context=NumericContext.binary x y)
        static member ( .** ) (x:complex0, y:int0) = complex0(Pow(Zt, x.Expr, y.Expr), ?context=NumericContext.binary x y)
        static member ( .** ) (x:double0, y:complex0) = complex0(Pow(Zt, x.Expr, y.Expr), ?context=NumericContext.binary x y)
        static member ( .** ) (x:complex0, y:double0) = complex0(Pow(Zt, x.Expr, y.Expr), ?context=NumericContext.binary x y)
        static member ( .** ) (x:int, y:complex0) = complex0(Pow(Zt, Int x, y.Expr), ?context=y.Context)
        static member ( .** ) (x:complex0, y:int) = complex0(Pow(Zt, x.Expr, Int y), ?context=x.Context)
        static member ( .** ) (x:double, y:complex0) = complex0(Pow(Zt, Dbl x, y.Expr), ?context=y.Context)
        static member ( .** ) (x:complex0, y:double) = complex0(Pow(Zt, x.Expr, Dbl y), ?context=x.Context)
        static member ( .** ) (x:complex0, y:complex0) = complex0(Pow(Zt, x.Expr, y.Expr), ?context=NumericContext.binary x y)

        ///<summary>代入</summary>
        static member (<==) (x:complex0,y:complex0) =
            let context = GenerationContextMerge.requireTarget x.Context
            GenerationContextMerge.merge x.Context y.Context |> ignore
            expr.subst x.Expr y.Expr context.CurrentProgram
        static member (<==) (x:complex0,y:int) = x <== complex0(Int y)
        static member (<==) (x:complex0,y:int0) = x <== complex0(y.Expr, ?context=y.Context)
        static member (<==) (x:complex0,y:double) = x <== complex0(Dbl y)
        static member (<==) (x:complex0,y:double0) = x <== complex0(y.Expr, ?context=y.Context)
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
        static member (===) (x:complex0,y:complex0) =
            let value, context = NumericContext.renderedBinary " = " x y
            complex0(value, ?context=context)
        static member (===) (x:complex0,y:int) = x === complex0(Int y)
        static member (===) (x:complex0,y:double) = x === complex0(Dbl y)

        ///<summary>等号揃等式(TeX、HTMLのみ)</summary>
        static member (=|=) (x:complex0,y:complex0) =
            let value, context = NumericContext.renderedBinary " =& " x y
            complex0(value, ?context=context)
        static member (=|=) (x:complex0,y:int) = x =|= complex0(Int y)
        static member (=|=) (x:complex0,y:double) = x =|= complex0(Dbl y)
        ///<summary>数式揃位置(TeX、HTMLのみ)</summary>
        static member (.|) (x:complex0,y:complex0) =
            let value, context = NumericContext.renderedBinary " & " x y
            complex0(value, ?context=context)
        static member html (e:complex0) = "\\("+e.code+"\\)"
        static member html (e:exprString) =
            let program = GenerationContextMerge.requireTarget e.Context |> fun (context:GenerationContext) -> context.CurrentProgram
            e.data |> List.fold (fun acc a -> match a with |RStr x -> acc+x |RNvr x -> acc+"\\("+x.evalH program+"\\)") ""
        static member html (e:list<complex0>) = "\\[\\begin{align}"+String.Join("\\\\",e |> List.map (fun f -> f.code))+"\\end{align}\\]"
        static member (~%%) (x:complex0) =
            let context = GenerationContextMerge.requireTarget x.Context
            expr.equivAlign (Var(Nt,"",NaN)) x.Expr context.CurrentProgram

    ///<summary>数値と文字列の結合</summary>
    and exprString(x:list<reduceExprString>, ?context:GenerationContext) =
        new(x:string) = exprString [RStr x]
        new(x:bool0) = exprString([RNvr x.Expr], ?context=x.Context)
        new(x:int0) = exprString([RNvr x.Expr], ?context=x.Context)
        new(x:double0) = exprString([RNvr x.Expr], ?context=x.Context)
        new(x:complex0) = exprString([RNvr x.Expr], ?context=x.Context)

        member _.data with get() = x
        member _.Context = context

        member this.toString(c:string,op:ExprConcatOption) =
            x
            |> List.map (function
                |RStr x ->
                    match op with
                    |Direct -> x
                    |StrQuotation -> "\""+x+"\""
                    |CodeStrQuotation -> "\\\""+x+"\\\""
                |RNvr x ->
                    let program = GenerationContextMerge.requireTarget context |> fun (context:GenerationContext) -> context.CurrentProgram
                    x.eval program)
            |> fun s -> String.Join(c,s)
        static member (++) (a:exprString,b:exprString) : exprString =
            exprString(a.data@b.data, ?context=GenerationContextMerge.merge a.Context b.Context)
        static member (++) (a:string,b:exprString) = exprString a ++ b
        static member (++) (a:exprString,b:string) = a ++ exprString b
        static member (++) (a:exprString,b:int0) = a ++ exprString b
        static member (++) (a:exprString,b:double0) = a ++ exprString b
        static member (++) (a:exprString,b:complex0) = a ++ exprString b
        static member (++) (a:exprString,b:bool0) = a ++ exprString b
        static member (++) (a:int,b:exprString) = int0(Int a) ++ b
        static member (++) (a:exprString,b:int) = a ++ int0(Int b)
        static member (++) (a:double,b:exprString) = double0(Dbl a) ++ b
        static member (++) (a:exprString,b:double) = a ++ double0(Dbl b)

    [<AutoOpen>]
    module strExpr =
        let st (x:string) = exprString x
        let iv (x:int0) = exprString x
        let dv (x:double0) = exprString x
        let zv (x:complex0) = exprString x
