//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    open System

    /// Controls how an expression string is quoted during concatenation.
    type ExprConcatOption =
        /// Appends text without additional quoting.
        |Direct
        /// Wraps text in double quotation marks.
        |StrQuotation
        /// Appends generated code for a quoted string literal.
        |CodeStrQuotation



    /// Represents a literal string or numeric expression in a concatenation.
    type reduceExprString =
        /// Literal string content.
        |RStr of string
        /// Numeric expression and its generation context.
        |RNvr of expr * Aqualis

        /// Gets the string or numeric type of the wrapped value.
        member this.etype with get() =
            match this with
            |RStr t -> Structure "string"
            |RNvr (value,_) -> value.etype

    /// Common read-only representation of a scalar numeric expression.
    type INum0 =
        /// Gets the expression rendered in its generation context.
        abstract member Code : string
        /// Gets the underlying expression tree.
        abstract member Expr : expr
        /// Gets the expression's numeric type.
        abstract member Etype : Etype
        /// Gets the owning generation context.
        abstract member Context : Aqualis

    /// Marker for scalar numeric expressions whose values are always real.
    type IReal0 =
        inherit INum0

    /// Combines generation contexts used by numeric expressions.
    module internal NumericContext =
        /// Gets the context of a unary numeric expression.
        let unary (value:INum0) = value.Context
        /// Merges the contexts of two numeric expressions.
        let binary (left:INum0) (right:INum0) =
            Aqualis.merge left.Context right.Context
        /// Merges the contexts of a sequence of numeric expressions.
        let many (values:INum0 seq) =
            values |> Seq.map _.Context |> Aqualis.mergeMany

        /// Renders two expressions with a separator in their merged context.
        let renderedBinary separator (left:INum0) (right:INum0) =
            let context = binary left right
            Var(Nt, left.Expr.eval(context) + separator + right.Expr.eval(context), NaN), context

    /// Shared storage and generation-context behavior for scalar numeric types.
    [<AbstractClass>]
    type NumericScalar<'Self>(x:expr, context:Aqualis) =
        // let context =
        //     match x with
        //     |Int _ |Dbl _ |Cpx _ -> None
        //     |_ -> context

        /// Gets the underlying expression tree.
        member _.Expr = x
        /// Gets the owning generation context.
        member _.Context = context
        /// Gets the expression's numeric type.
        member _.etype = x.etype
        /// Gets the expression rendered in its generation context.
        member _.code = x.eval context

        interface INum0 with
            /// Gets the rendered expression.
            member this.Code = this.code
            /// Gets the underlying expression tree.
            member this.Expr = this.Expr
            /// Gets the expression type.
            member this.Etype = this.etype
            /// Gets the owning generation context.
            member this.Context = this.Context


    /// Boolean expression associated with a generation context.
    type bool0(x:expr, context:Aqualis) =
        // let context =
        //     match x with
        //     |True |False -> None
        //     |_ -> context
        /// Gets the underlying Boolean expression.
        member _.Expr with get() = x
        /// Gets the owning generation context.
        member _.Context = context
        /// Gets the rendered Boolean expression.
        member _.code with get() = x.eval context
        /// Concatenates the operands for generated output.
        static member (++) (x:string,y:bool0) = exprString x ++ exprString y
        /// Concatenates the operands for generated output.
        static member (++) (x:bool0,y:string) = exprString x ++ exprString y
        /// Concatenates the operands for generated output.
        static member (++) (x:bool0,y:bool0) = exprString x ++ exprString y

        /// Concatenates the operands for generated output.
        static member (++) (x:bool0,y:exprString) = exprString x ++ y

        /// Adds a new comparison to a chained comparison expression.
        static member private appendComparison
            (v1:bool0,rightExpr:expr,rightContext:Aqualis,makeComparison:(expr*expr)->expr) =
            let result expression =
                bool0(expression, Aqualis.merge v1.Context rightContext)
            match v1.Expr with
            |((Less(_,middle)
              |LessEq(_,middle)
              |Greater(_,middle)
              |GreaterEq(_,middle)) as previous) ->
                result (AND [previous; makeComparison(middle,rightExpr)])
            |AND expressions ->
                match List.tryLast expressions with
                |Some (Less(_,middle)
                      |LessEq(_,middle)
                      |Greater(_,middle)
                      |GreaterEq(_,middle)) ->
                    result (AND (expressions @ [makeComparison(middle,rightExpr)]))
                |_ ->
                    result NaN
            |_ ->
                result NaN

        /// Appends a less-than comparison to a chained comparison.
        static member (.<) (v1:bool0,v2:int0) =
            bool0.appendComparison(v1,v2.Expr,v2.Context,Less)
        /// Appends a less-than comparison to a chained comparison.
        static member (.<) (v1:bool0,v2:double0) =
            bool0.appendComparison(v1,v2.Expr,v2.Context,Less)
        /// Appends a less-than comparison to a chained comparison.
        static member (.<) (v1:bool0,v2:double) = v1 .< double0(Dbl v2)
        /// Appends a less-than comparison to a chained comparison.
        static member (.<) (v1:bool0,v2:int) = v1 .< int0(Int v2)
        /// Appends a less-than-or-equal comparison to a chained comparison.
        static member (.<=) (v1:bool0,v2:int0) =
            bool0.appendComparison(v1,v2.Expr,v2.Context,LessEq)
        /// Appends a less-than-or-equal comparison to a chained comparison.
        static member (.<=) (v1:bool0,v2:double0) =
            bool0.appendComparison(v1,v2.Expr,v2.Context,LessEq)
        /// Appends a less-than-or-equal comparison to a chained comparison.
        static member (.<=) (v1:bool0,v2:double) = v1 .<= double0(Dbl v2)
        /// Appends a less-than-or-equal comparison to a chained comparison.
        static member (.<=) (v1:bool0,v2:int) = v1 .<= int0(Int v2)
        /// Appends a greater-than comparison to a chained comparison.
        static member (.>) (v1:bool0,v2:int0) =
            bool0.appendComparison(v1,v2.Expr,v2.Context,Greater)
        /// Appends a greater-than comparison to a chained comparison.
        static member (.>) (v1:bool0,v2:double0) =
            bool0.appendComparison(v1,v2.Expr,v2.Context,Greater)
        /// Appends a greater-than comparison to a chained comparison.
        static member (.>) (v1:bool0,v2:double) = v1 .> double0(Dbl v2)
        /// Appends a greater-than comparison to a chained comparison.
        static member (.>) (v1:bool0,v2:int) = v1 .> int0(Int v2)
        /// Appends a greater-than-or-equal comparison to a chained comparison.
        static member (.>=) (v1:bool0,v2:int0) =
            bool0.appendComparison(v1,v2.Expr,v2.Context,GreaterEq)
        /// Appends a greater-than-or-equal comparison to a chained comparison.
        static member (.>=) (v1:bool0,v2:double0) =
            bool0.appendComparison(v1,v2.Expr,v2.Context,GreaterEq)
        /// Appends a greater-than-or-equal comparison to a chained comparison.
        static member (.>=) (v1:bool0,v2:double) = v1 .>= double0(Dbl v2)
        /// Appends a greater-than-or-equal comparison to a chained comparison.
        static member (.>=) (v1:bool0,v2:int) = v1 .>= int0(Int v2)

    ///<summary>変数（数値データ）クラス</summary>
    and int0(x:expr, context:Aqualis) =
        inherit NumericScalar<int0>(x,context)

        interface IReal0

        new (x:expr) = int0(x,Aqualis.BlankWriter Numeric)
        /// Gets the owning generation context.
        member _.Context with get() = context
        ///<summary>優先度の高い型を選択</summary>
        static member ( %% ) (x:int0,y:int0) =
            match x.etype,y.etype with
            |Zt,(Zt|Dt|It _) -> Zt
            |(Zt|Dt|It _),Zt -> Zt
            |Dt,(Dt|It _) -> Dt
            |(Dt|It _),Dt -> Dt
            |It a,It b -> It (if a>b then a else b)
            |_ -> Nt

        /// Concatenates the operands for generated output.
        static member (++) (x:string,y:int0) = exprString x ++ exprString y
        /// Concatenates the operands for generated output.
        static member (++) (x:int0,y:string) = exprString x ++ exprString y
        /// Concatenates the operands for generated output.
        static member (++) (x:int0,y:int0) = exprString x ++ exprString y

        /// Concatenates the operands for generated output.
        static member (++) (x:int0,y:exprString) = exprString x ++ y

        ///<summary>負号</summary>
        static member ( ~- ) (x:int0) = int0(Inv(x.etype,x.Expr), x.Context)

        ///<summary>加算</summary>
        static member ( + ) (x:int0,y:int0) = int0(Add(x%%y, x.Expr, y.Expr), NumericContext.binary x y)
        /// Adds the operands.
        static member ( + ) (x:int0,y:double) = x + double0(Dbl y)
        /// Adds the operands.
        static member ( + ) (x:int0,y:int) = x + int0(Int y)
        /// Adds the operands.
        static member ( + ) (x:double,y:int0) = double0(Dbl x) + y
        /// Adds the operands.
        static member ( + ) (x:int,y:int0) = int0(Int x) + y

        ///<summary>減算</summary>
        static member ( - ) (x:int0,y:int0) = int0(Sub(x%%y, x.Expr, y.Expr), NumericContext.binary x y)
        /// Subtracts the right operand from the left operand.
        static member ( - ) (x:int0,y:double) = x-double0(Dbl y)
        /// Subtracts the right operand from the left operand.
        static member ( - ) (x:int0,y:int) = x-int0(Int y)
        /// Subtracts the right operand from the left operand.
        static member ( - ) (x:double,y:int0) = double0(Dbl x)-y
        /// Subtracts the right operand from the left operand.
        static member ( - ) (x:int,y:int0) = int0(Int x)-y

        ///<summary>乗算</summary>
        static member ( * ) (x:int0,y:int0) = int0(Mul(x%%y, x.Expr, y.Expr), NumericContext.binary x y)
        /// Multiplies the operands.
        static member ( * ) (x:int0,y:double) = x*double0(Dbl y)
        /// Multiplies the operands.
        static member ( * ) (x:int0,y:int) = x*int0(Int y)
        /// Multiplies the operands.
        static member ( * ) (x:double,y:int0) = double0(Dbl x)*y
        /// Multiplies the operands.
        static member ( * ) (x:int,y:int0) = int0(Int x)*y

        ///<summary>除算</summary>
        static member ( / ) (x:int0,y:int0) = double0(Div(Dt%%x.etype%%y.etype, x.Expr, y.Expr), NumericContext.binary x y)
        /// Divides the left operand by the right operand.
        static member ( / ) (x:int0,y:double) = x/int0(Dbl y)
        /// Divides the left operand by the right operand.
        static member ( / ) (x:int0,y:int) = x/int0(Dbl(double y))
        /// Divides the left operand by the right operand.
        static member ( / ) (x:double,y:int0) = double0(Dbl x)/y
        /// Divides the left operand by the right operand.
        static member ( / ) (x:int,y:int0) = double0(Dbl(double x))/y

        ///<summary>整数同士の除算(剰余無視)</summary>
        static member ( ./ ) (x:int0,y:int0) = int0(Div(It 4, x.Expr, y.Expr), NumericContext.binary x y)
        /// Builds a division expression with integer result semantics.
        static member ( ./ ) (x:int0,y:int) = x./int0(Int y)
        /// Builds a division expression with integer result semantics.
        static member ( ./ ) (x:int,y:int0) = int0(Int x)./y

        ///<summary>剰余</summary>
        static member ( % ) (x:int0,y:int0) = int0(Mod(It 4, x.Expr, y.Expr), NumericContext.binary x y)
        /// Computes the remainder of integer division.
        static member ( % ) (x:int0,y:int) = x % int0(Int y)
        /// Computes the remainder of integer division.
        static member ( % ) (x:int,y:int0) = int0(Int x) % y

        ///<summary>累乗</summary>
        static member powr(x:int0, y:int0) = int0(Pow(x%%y, x.Expr, y.Expr), NumericContext.binary x y)
        /// Raises the first operand to the power of the second operand.
        static member ( .** ) (x:int0, y:int0) = int0(Pow(x%%y, x.Expr, y.Expr), NumericContext.binary x y)

        ///<summary>等号</summary>
        static member (.=) (x:int0,y:int0) = bool0(Eq(x.Expr,y.Expr), NumericContext.binary x y)
        /// Tests whether the operands are equal.
        static member (.=) (x:int,y:int0) = int0(Int x) .= y
        /// Tests whether the operands are equal.
        static member (.=) (x:double,y:int0) = int0(Dbl x) .= y
        /// Tests whether the operands are equal.
        static member (.=) (x:int0,y:int) = x .= int0(Int y)
        /// Tests whether the operands are equal.
        static member (.=) (x:int0,y:double) = x .= int0(Dbl y)
        ///<summary>不等号</summary>
        static member (.=/) (x:int0,y:int0) = bool0(NEq(x.Expr,y.Expr), NumericContext.binary x y)
        /// Tests whether the operands are unequal.
        static member (.=/) (x:int,y:int0) = int0(Int x) .=/ y
        /// Tests whether the operands are unequal.
        static member (.=/) (x:double,y:int0) = int0(Dbl x) .=/ y
        /// Tests whether the operands are unequal.
        static member (.=/) (x:int0,y:int) = x .=/ int0(Int y)
        /// Tests whether the operands are unequal.
        static member (.=/) (x:int0,y:double) = x .=/ int0(Dbl y)
        ///<summary>比較（より小）</summary>
        static member (.<) (x:int0,y:int0) = bool0(Less(x.Expr,y.Expr), NumericContext.binary x y)
        /// Tests whether the left operand is less than the right operand.
        static member (.<) (x:int,y:int0) = int0(Int x) .< y
        /// Tests whether the left operand is less than the right operand.
        static member (.<) (x:double,y:int0) = int0(Dbl x) .< y
        /// Tests whether the left operand is less than the right operand.
        static member (.<) (x:int0,y:int) = x .< int0(Int y)
        /// Tests whether the left operand is less than the right operand.
        static member (.<) (x:int0,y:double) = x .< int0(Dbl y)

        ///<summary>比較（以下）</summary>
        static member (.<=) (x:int0,y:int0) = bool0(LessEq(x.Expr,y.Expr), NumericContext.binary x y)
        /// Tests whether the left operand is less than or equal to the right operand.
        static member (.<=) (x:int,y:int0) = int0(Int x) .<= y
        /// Tests whether the left operand is less than or equal to the right operand.
        static member (.<=) (x:double,y:int0) = int0(Dbl x) .<= y
        /// Tests whether the left operand is less than or equal to the right operand.
        static member (.<=) (x:int0,y:int) = x .<= int0(Int y)
        /// Tests whether the left operand is less than or equal to the right operand.
        static member (.<=) (x:int0,y:double) = x .<= int0(Dbl y)

        /// Appends a less-than-or-equal comparison to a chained comparison.
        static member (.<=) (v1:bool0,v2:double) = v1 .<= int0(Dbl v2)
        /// Appends a less-than-or-equal comparison to a chained comparison.
        static member (.<=) (v1:bool0,v2:int) = v1 .<= int0(Int v2)

        ///<summary>比較（より大）</summary>
        static member (.>) (x:int0,y:int0) = bool0(Greater(x.Expr,y.Expr), NumericContext.binary x y)
        /// Tests whether the left operand is greater than the right operand.
        static member (.>) (x:int,y:int0) = int0(Int x) .> y
        /// Tests whether the left operand is greater than the right operand.
        static member (.>) (x:double,y:int0) = int0(Dbl x) .> y
        /// Tests whether the left operand is greater than the right operand.
        static member (.>) (x:int0,y:int) = x .> int0(Int y)
        /// Tests whether the left operand is greater than the right operand.
        static member (.>) (x:int0,y:double) = x .> int0(Dbl y)

        ///<summary>比較（以上）</summary>
        static member (.>=) (x:int0,y:int0) = bool0(GreaterEq(x.Expr,y.Expr), NumericContext.binary x y)
        /// Tests whether the left operand is greater than or equal to the right operand.
        static member (.>=) (x:int,y:int0) = int0(Int x) .>= y
        /// Tests whether the left operand is greater than or equal to the right operand.
        static member (.>=) (x:double,y:int0) = int0(Dbl x) .>= y
        /// Tests whether the left operand is greater than or equal to the right operand.
        static member (.>=) (x:int0,y:int) = x .>= int0(Int y)
        /// Tests whether the left operand is greater than or equal to the right operand.
        static member (.>=) (x:int0,y:double) = x .>= int0(Dbl y)
        /// Renders the expression as an aligned equation in its generation context.
        static member (~%%) (x:int0) =
            expr.equivAlign (Var(Nt,"",NaN)) x.Expr x.Context

        ///<summary>代入</summary>
        static member (<==) (x:int0,y:int0) =
            Aqualis.merge x.Context y.Context |> ignore
            expr.subst x.Expr y.Expr x.Context
        /// Assigns the right-hand value or array to the left-hand destination.
        static member (<==) (x:int0,y:int) = x <== int0(Int y)
        /// Assigns the right-hand value or array to the left-hand destination.
        static member (<==) (x:int0,y:exprString) =
            // let context =
            //     x.Context
            //     |> Option.defaultWith (fun () ->
            //         invalidOp "The assignment target is not associated with a program.")
            match x.Context.language with
            |PHP ->
                expr.subst x.Expr (Var(Nt,y.toPhpString(".",x.Context),NaN)) x.Context
            |_ ->
                UnsupportedOperation.codeGeneration
                    (x.Context.language.ToString())
                    "assigning a string expression to an integer value"
        /// Assigns the right-hand value or array to the left-hand destination.
        static member (<==) (x:int0,y:string) = x <== exprString y
        /// Assigns zero to this numeric variable.
        member this.clear() = this <== 0

        ///<summary>等式(TeX、HTMLのみ)</summary>
        static member (===) (x:int0,y:int0) =
            int0(Eq(x.Expr,y.Expr), NumericContext.binary x y)
        /// Constructs an equality expression for document output.
        static member (===) (x:int0,y:int) = x === int0(Int y)
        /// Constructs an equality expression for document output.
        static member (===) (x:int0,y:double) = x === int0(Dbl y)

        ///<summary>等号揃等式(TeX、HTMLのみ)</summary>
        static member (=|=) (x:int0,y:int0) =
            let value, context = NumericContext.renderedBinary " =& " x y
            int0(value, context)
        /// Joins equations with an alignment marker for document output.
        static member (=|=) (x:int0,y:int) = x =|= int0(Int y)
        /// Joins equations with an alignment marker for document output.
        static member (=|=) (x:int0,y:double) = x =|= int0(Dbl y)
        ///<summary>数式揃位置(TeX、HTMLのみ)</summary>
        static member (.|) (x:int0,y:int0) =
            let value, context = NumericContext.renderedBinary " & " x y
            int0(value, context)
        /// Formats the expression as inline MathJax.
        static member html (e:int0) = "\\("+e.code+"\\)"
        /// Formats the expression as inline MathJax.
        static member html (e:exprString) =
            e.data |> List.fold (fun acc a -> match a with |RStr x -> acc+x |RNvr (x,_) -> acc+"\\("+x.evalH e.Context+"\\)") ""
        /// Formats the expressions as a MathJax align environment.
        static member html (e:list<int0>) = "\\[\\begin{align}"+String.Join("\\\\",e |> List.map (fun f -> f.code))+"\\end{align}\\]"
    ///<summary>変数（数値データ）クラス</summary>
    and double0(x:expr, context:Aqualis) =
        inherit NumericScalar<double0>(x,context)

        interface IReal0

        new (x:expr) = double0(x,Aqualis.BlankWriter Numeric)

        /// Gets the owning generation context.
        member _.Context with get() = context
        ///<summary>優先度の高い型を選択</summary>
        static member ( %% ) (x:double0,y:double0) =
            match x.etype,y.etype with
            |Zt,(Zt|Dt|It _) -> Zt
            |(Zt|Dt|It _),Zt -> Zt
            |Dt,(Dt|It _) -> Dt
            |(Dt|It _),Dt -> Dt
            |It a,It b -> It (if a>b then a else b)
            |_ -> Nt

        /// Concatenates the operands for generated output.
        static member (++) (x:string,y:double0) = exprString x ++ exprString y
        /// Concatenates the operands for generated output.
        static member (++) (x:double0,y:string) = exprString x ++ exprString y
        /// Concatenates the operands for generated output.
        static member (++) (x:int0,y:double0) = exprString x ++ exprString y
        /// Concatenates the operands for generated output.
        static member (++) (x:double0,y:int0) = exprString x ++ exprString y
        /// Concatenates the operands for generated output.
        static member (++) (x:double0,y:double0) = exprString x ++ exprString y

        /// Concatenates the operands for generated output.
        static member (++) (x:double0,y:exprString) = exprString x ++ y

        ///<summary>負号</summary>
        static member ( ~- ) (x:double0) = double0(Inv(x.etype,x.Expr), x.Context)

        ///<summary>加算</summary>
        static member ( + ) (x:int0,y:double0):double0 = double0(Add(Dt, x.Expr, y.Expr), NumericContext.binary x y)
        /// Adds the operands.
        static member ( + ) (x:double0,y:int0):double0 = double0(Add(Dt, x.Expr, y.Expr), NumericContext.binary x y)
        /// Adds the operands.
        static member ( + ) (x:double0,y:double0) = double0(Add(Dt, x.Expr, y.Expr), NumericContext.binary x y)
        /// Adds the operands.
        static member ( + ) (x:double0,y:double) = x + double0(Dbl y)
        /// Adds the operands.
        static member ( + ) (x:double0,y:int) = x + double0(Int y)
        /// Adds the operands.
        static member ( + ) (x:double,y:double0) = double0(Dbl x) + y
        /// Adds the operands.
        static member ( + ) (x:int,y:double0) = double0(Int x) + y

        ///<summary>減算</summary>
        static member ( - ) (x:int0,y:double0):double0 = double0(Sub(Dt, x.Expr, y.Expr), NumericContext.binary x y)
        /// Subtracts the right operand from the left operand.
        static member ( - ) (x:double0,y:int0):double0 = double0(Sub(Dt, x.Expr, y.Expr), NumericContext.binary x y)
        /// Subtracts the right operand from the left operand.
        static member ( - ) (x:double0,y:double0) = double0(Sub(Dt, x.Expr, y.Expr), NumericContext.binary x y)
        /// Subtracts the right operand from the left operand.
        static member ( - ) (x:double0,y:double) = x-double0(Dbl y)
        /// Subtracts the right operand from the left operand.
        static member ( - ) (x:double0,y:int) = x-double0(Int y)
        /// Subtracts the right operand from the left operand.
        static member ( - ) (x:double,y:double0) = double0(Dbl x)-y
        /// Subtracts the right operand from the left operand.
        static member ( - ) (x:int,y:double0) = double0(Int x)-y

        ///<summary>乗算</summary>
        static member ( * ) (x:int0,y:double0):double0 = double0(Mul(Dt, x.Expr, y.Expr), NumericContext.binary x y)
        /// Multiplies the operands.
        static member ( * ) (x:double0,y:int0):double0 = double0(Mul(Dt, x.Expr, y.Expr), NumericContext.binary x y)
        /// Multiplies the operands.
        static member ( * ) (x:double0,y:double0) = double0(Mul(Dt, x.Expr, y.Expr), NumericContext.binary x y)
        /// Multiplies the operands.
        static member ( * ) (x:double0,y:double) = x*double0(Dbl y)
        /// Multiplies the operands.
        static member ( * ) (x:double0,y:int) = x*double0(Int y)
        /// Multiplies the operands.
        static member ( * ) (x:double,y:double0) = double0(Dbl x)*y
        /// Multiplies the operands.
        static member ( * ) (x:int,y:double0) = double0(Int x)*y

        ///<summary>除算</summary>
        static member ( / ) (x:int0,y:double0) = double0(Div(Dt%%x.etype%%y.etype, x.Expr, y.Expr), NumericContext.binary x y)
        /// Divides the left operand by the right operand.
        static member ( / ) (x:double0,y:int0):double0 = double0(Div(Dt%%x.etype%%y.etype, x.Expr, y.Expr), NumericContext.binary x y)
        /// Divides the left operand by the right operand.
        static member ( / ) (x:double0,y:double0) = double0(Div(Dt%%x.etype%%y.etype, x.Expr, y.Expr), NumericContext.binary x y)
        /// Divides the left operand by the right operand.
        static member ( / ) (x:double0,y:double) = x/double0(Dbl y)
        /// Divides the left operand by the right operand.
        static member ( / ) (x:double0,y:int) = x/double0(Dbl(double y))
        /// Divides the left operand by the right operand.
        static member ( / ) (x:double,y:double0) = double0(Dbl x)/y
        /// Divides the left operand by the right operand.
        static member ( / ) (x:int,y:double0) = double0(Dbl(double x))/y

        ///<summary>累乗</summary>
        static member powr(x:int0, y:double0) = double0(Pow(Dt, x.Expr, y.Expr), NumericContext.binary x y)
        /// Raises the first operand to the power of the second operand.
        static member powr(x:double0, y:int0) = double0(Pow(Dt, x.Expr, y.Expr), NumericContext.binary x y)
        /// Raises the first operand to the power of the second operand.
        static member powr(x:double0, y:int) = double0(Pow(Dt, x.Expr, Int y), x.Context)
        /// Raises the first operand to the power of the second operand.
        static member powr(x:int, y:double0) = double0(Pow(Dt, Int x, y.Expr), y.Context)
        /// Raises the first operand to the power of the second operand.
        static member powr(x:double0, y:double0) = double0(Pow(Dt, x.Expr, y.Expr), NumericContext.binary x y)
        /// Raises the first operand to the power of the second operand.
        static member powr(x:double0, y:double) = double0(Pow(Dt, x.Expr, Dbl y), x.Context)
        /// Raises the first operand to the power of the second operand.
        static member powr(x:double, y:double0) = double0(Pow(Dt, Dbl x, y.Expr), y.Context)
        /// Raises the first operand to the power of the second operand.
        static member ( .** ) (x:int0, y:double0) = double0(Pow(Dt, x.Expr, y.Expr), NumericContext.binary x y)
        /// Raises the first operand to the power of the second operand.
        static member ( .** ) (x:double0, y:int0) = double0(Pow(Dt, x.Expr, y.Expr), NumericContext.binary x y)
        /// Raises the first operand to the power of the second operand.
        static member ( .** ) (x:double0, y:int) = double0(Pow(Dt, x.Expr, Int y), x.Context)
        /// Raises the first operand to the power of the second operand.
        static member ( .** ) (x:int, y:double0) = double0(Pow(Dt, Int x, y.Expr), y.Context)
        /// Raises the first operand to the power of the second operand.
        static member ( .** ) (x:double0, y:double0) = double0(Pow(Dt, x.Expr, y.Expr), NumericContext.binary x y)
        /// Raises the first operand to the power of the second operand.
        static member ( .** ) (x:double0, y:double) = double0(Pow(Dt, x.Expr, Dbl y), x.Context)
        /// Raises the first operand to the power of the second operand.
        static member ( .** ) (x:double, y:double0) = double0(Pow(Dt, Dbl x, y.Expr), y.Context)

        ///<summary>等号</summary>
        static member (.=) (x:double0,y:double0) = bool0(Eq(x.Expr,y.Expr), NumericContext.binary x y)
        /// Tests whether the operands are equal.
        static member (.=) (x:int,y:double0) = double0(Int x) .= y
        /// Tests whether the operands are equal.
        static member (.=) (x:double,y:double0) = double0(Dbl x) .= y
        /// Tests whether the operands are equal.
        static member (.=) (x:double0,y:int) = x .= double0(Int y)
        /// Tests whether the operands are equal.
        static member (.=) (x:double0,y:double) = x .= double0(Dbl y)
        ///<summary>不等号</summary>
        static member (.=/) (x:double0,y:double0) = bool0(NEq(x.Expr,y.Expr), NumericContext.binary x y)
        /// Tests whether the operands are unequal.
        static member (.=/) (x:int,y:double0) = double0(Int x) .=/ y
        /// Tests whether the operands are unequal.
        static member (.=/) (x:double,y:double0) = double0(Dbl x) .=/ y
        /// Tests whether the operands are unequal.
        static member (.=/) (x:double0,y:int) = x .=/ double0(Int y)
        /// Tests whether the operands are unequal.
        static member (.=/) (x:double0,y:double) = x .=/ double0(Dbl y)
        ///<summary>比較（より小）</summary>
        static member (.<) (x:int0,y:double0) = bool0(Less(x.Expr,y.Expr), NumericContext.binary x y)
        /// Tests whether the left operand is less than the right operand.
        static member (.<) (x:double0,y:int0) = bool0(Less(x.Expr,y.Expr), NumericContext.binary x y)
        /// Tests whether the left operand is less than the right operand.
        static member (.<) (x:double0,y:double0) = bool0(Less(x.Expr,y.Expr), NumericContext.binary x y)
        /// Tests whether the left operand is less than the right operand.
        static member (.<) (x:int,y:double0) = double0(Int x) .< y
        /// Tests whether the left operand is less than the right operand.
        static member (.<) (x:double,y:double0) = double0(Dbl x) .< y
        /// Tests whether the left operand is less than the right operand.
        static member (.<) (x:double0,y:int) = x .< double0(Int y)
        /// Tests whether the left operand is less than the right operand.
        static member (.<) (x:double0,y:double) = x .< double0(Dbl y)

        ///<summary>比較（以下）</summary>
        static member (.<=) (x:int0,y:double0) = bool0(LessEq(x.Expr,y.Expr), NumericContext.binary x y)
        /// Tests whether the left operand is less than or equal to the right operand.
        static member (.<=) (x:double0,y:int0) = bool0(LessEq(x.Expr,y.Expr), NumericContext.binary x y)
        /// Tests whether the left operand is less than or equal to the right operand.
        static member (.<=) (x:double0,y:double0) = bool0(LessEq(x.Expr,y.Expr), NumericContext.binary x y)
        /// Tests whether the left operand is less than or equal to the right operand.
        static member (.<=) (x:int,y:double0) = double0(Int x) .<= y
        /// Tests whether the left operand is less than or equal to the right operand.
        static member (.<=) (x:double,y:double0) = double0(Dbl x) .<= y
        /// Tests whether the left operand is less than or equal to the right operand.
        static member (.<=) (x:double0,y:int) = x .<= double0(Int y)
        /// Tests whether the left operand is less than or equal to the right operand.
        static member (.<=) (x:double0,y:double) = x .<= double0(Dbl y)

        /// Appends a less-than-or-equal comparison to a chained comparison.
        static member (.<=) (v1:bool0,v2:double) = v1 .<= double0(Dbl v2)
        /// Appends a less-than-or-equal comparison to a chained comparison.
        static member (.<=) (v1:bool0,v2:int) = v1 .<= double0(Int v2)

        ///<summary>比較（より大）</summary>
        static member (.>) (x:int0,y:double0) = bool0(Greater(x.Expr,y.Expr), NumericContext.binary x y)
        /// Tests whether the left operand is greater than the right operand.
        static member (.>) (x:double0,y:int0) = bool0(Greater(x.Expr,y.Expr), NumericContext.binary x y)
        /// Tests whether the left operand is greater than the right operand.
        static member (.>) (x:double0,y:double0) = bool0(Greater(x.Expr,y.Expr), NumericContext.binary x y)
        /// Tests whether the left operand is greater than the right operand.
        static member (.>) (x:int,y:double0) = double0(Int x) .> y
        /// Tests whether the left operand is greater than the right operand.
        static member (.>) (x:double,y:double0) = double0(Dbl x) .> y
        /// Tests whether the left operand is greater than the right operand.
        static member (.>) (x:double0,y:int) = x .> double0(Int y)
        /// Tests whether the left operand is greater than the right operand.
        static member (.>) (x:double0,y:double) = x .> double0(Dbl y)

        ///<summary>比較（以上）</summary>
        static member (.>=) (x:int0,y:double0) = bool0(GreaterEq(x.Expr,y.Expr), NumericContext.binary x y)
        /// Tests whether the left operand is greater than or equal to the right operand.
        static member (.>=) (x:double0,y:int0) = bool0(GreaterEq(x.Expr,y.Expr), NumericContext.binary x y)
        /// Tests whether the left operand is greater than or equal to the right operand.
        static member (.>=) (x:double0,y:double0) = bool0(GreaterEq(x.Expr,y.Expr), NumericContext.binary x y)
        /// Tests whether the left operand is greater than or equal to the right operand.
        static member (.>=) (x:int,y:double0) = double0(Int x) .>= y
        /// Tests whether the left operand is greater than or equal to the right operand.
        static member (.>=) (x:double,y:double0) = double0(Dbl x) .>= y
        /// Tests whether the left operand is greater than or equal to the right operand.
        static member (.>=) (x:double0,y:int) = x .>= double0(Int y)
        /// Tests whether the left operand is greater than or equal to the right operand.
        static member (.>=) (x:double0,y:double) = x .>= double0(Dbl y)

        ///<summary>代入</summary>
        static member (<==) (x:double0,y:double0) =
            Aqualis.merge x.Context y.Context |> ignore
            expr.subst x.Expr y.Expr x.Context
        /// Assigns the right-hand value or array to the left-hand destination.
        static member (<==) (x:double0,y:int) = x <== double0(Int y)
        /// Assigns the right-hand value or array to the left-hand destination.
        static member (<==) (x:double0,y:int0) = x <== double0(y.Expr, y.Context)
        /// Assigns the right-hand value or array to the left-hand destination.
        static member (<==) (x:double0,y:double) = x <== double0(Dbl y)
        /// Assigns the right-hand value or array to the left-hand destination.
        static member (<==) (x:double0,y:exprString) =
            // let context =
            //     x.Context
            //     |> Option.defaultWith (fun () ->
            //         invalidOp "The assignment target is not associated with a program.")
            match x.Context.language with
            |PHP ->
                expr.subst x.Expr (Var(Nt,y.toPhpString(".",x.Context),NaN)) x.Context
            |_ ->
                UnsupportedOperation.codeGeneration
                    (x.Context.language.ToString())
                    "assigning a string expression to a floating-point value"
        /// Assigns the right-hand value or array to the left-hand destination.
        static member (<==) (x:double0,y:string) = x <== exprString y
        /// Assigns zero to this numeric variable.
        member this.clear() = this <== 0

        ///<summary>等式(TeX、HTMLのみ)</summary>
        static member (===) (x:double0,y:double0) =
            double0(Eq(x.Expr,y.Expr), NumericContext.binary x y)
        /// Constructs an equality expression for document output.
        static member (===) (x:double0,y:int) = x === double0(Int y)
        /// Constructs an equality expression for document output.
        static member (===) (x:double0,y:double) = x === double0(Dbl y)

        ///<summary>等号揃等式(TeX、HTMLのみ)</summary>
        static member (=|=) (x:double0,y:double0) =
            let value, context = NumericContext.renderedBinary " =& " x y
            double0(value, context)
        /// Joins equations with an alignment marker for document output.
        static member (=|=) (x:double0,y:int) = x =|= double0(Int y)
        /// Joins equations with an alignment marker for document output.
        static member (=|=) (x:double0,y:double) = x =|= double0(Dbl y)
        ///<summary>数式揃位置(TeX、HTMLのみ)</summary>
        static member (.|) (x:double0,y:double0) =
            let value, context = NumericContext.renderedBinary " & " x y
            double0(value, context)
        /// Formats the expression as inline MathJax.
        static member html (e:double0) = "\\("+e.code+"\\)"
        /// Formats the expression as inline MathJax.
        static member html (e:exprString) =
            e.data |> List.fold (fun acc a -> match a with |RStr x -> acc+x |RNvr (x,_) -> acc+"\\("+x.evalH e.Context+"\\)") ""
        /// Formats the expressions as a MathJax align environment.
        static member html (e:list<double0>) = "\\[\\begin{align}"+String.Join("\\\\",e |> List.map (fun f -> f.code))+"\\end{align}\\]"
        /// Renders the expression as an aligned equation in its generation context.
        static member (~%%) (x:double0) =
            expr.equivAlign (Var(Nt,"",NaN)) x.Expr x.Context
        
    ///<summary>変数（数値データ）クラス</summary>
    and complex0(x:expr, context:Aqualis) =
        inherit NumericScalar<complex0>(x,context)

        new (x:expr) = complex0(x,Aqualis.BlankWriter Numeric)

        /// Gets the owning generation context.
        member _.Context with get() = context
        ///<summary>優先度の高い型を選択</summary>
        static member ( %% ) (x:complex0,y:complex0) =
            match x.etype,y.etype with
            |Zt,(Zt|Dt|It _) -> Zt
            |(Zt|Dt|It _),Zt -> Zt
            |Dt,(Dt|It _) -> Dt
            |(Dt|It _),Dt -> Dt
            |It a,It b -> It (if a>b then a else b)
            |_ -> Nt

        /// Concatenates the operands for generated output.
        static member (++) (x:string,y:complex0) = exprString x ++ exprString y
        /// Concatenates the operands for generated output.
        static member (++) (x:complex0,y:string) = exprString x ++ exprString y
        /// Concatenates the operands for generated output.
        static member (++) (x:int0,y:complex0) = exprString x ++ exprString y
        /// Concatenates the operands for generated output.
        static member (++) (x:double0,y:complex0) = exprString x ++ exprString y
        /// Concatenates the operands for generated output.
        static member (++) (x:complex0,y:int0) = exprString x ++ exprString y
        /// Concatenates the operands for generated output.
        static member (++) (x:complex0,y:double0) = exprString x ++ exprString y
        /// Concatenates the operands for generated output.
        static member (++) (x:complex0,y:complex0) = exprString x ++ exprString y

        /// Concatenates the operands for generated output.
        static member (++) (x:complex0,y:exprString) = exprString x ++ y

        ///<summary>負号</summary>
        static member ( ~- ) (x:complex0) = complex0(Inv(x.etype,x.Expr), x.Context)

        ///<summary>加算</summary>
        static member ( + ) (x:int0,y:complex0) = complex0(Add(Zt, x.Expr, y.Expr), NumericContext.binary x y)
        /// Adds the operands.
        static member ( + ) (x:double0,y:complex0) = complex0(Add(Zt, x.Expr, y.Expr), NumericContext.binary x y)
        /// Adds the operands.
        static member ( + ) (x:complex0,y:int0) = complex0(Add(Zt, x.Expr, y.Expr), NumericContext.binary x y)
        /// Adds the operands.
        static member ( + ) (x:complex0,y:double0) = complex0(Add(Zt, x.Expr, y.Expr), NumericContext.binary x y)
        /// Adds the operands.
        static member ( + ) (x:complex0,y:complex0) = complex0(Add(Zt, x.Expr, y.Expr), NumericContext.binary x y)
        /// Adds the operands.
        static member ( + ) (x:complex0,y:double) = x + complex0(Dbl y)
        /// Adds the operands.
        static member ( + ) (x:complex0,y:int) = x + complex0(Int y)
        /// Adds the operands.
        static member ( + ) (x:double,y:complex0) = complex0(Dbl x) + y
        /// Adds the operands.
        static member ( + ) (x:int,y:complex0) = complex0(Int x) + y

        ///<summary>減算</summary>
        static member ( - ) (x:int0,y:complex0) = complex0(Sub(Zt, x.Expr, y.Expr), NumericContext.binary x y)
        /// Subtracts the right operand from the left operand.
        static member ( - ) (x:double0,y:complex0) = complex0(Sub(Zt, x.Expr, y.Expr), NumericContext.binary x y)
        /// Subtracts the right operand from the left operand.
        static member ( - ) (x:complex0,y:int0) = complex0(Sub(Zt, x.Expr, y.Expr), NumericContext.binary x y)
        /// Subtracts the right operand from the left operand.
        static member ( - ) (x:complex0,y:double0) = complex0(Sub(Zt, x.Expr, y.Expr), NumericContext.binary x y)
        /// Subtracts the right operand from the left operand.
        static member ( - ) (x:complex0,y:complex0) = complex0(Sub(Zt, x.Expr, y.Expr), NumericContext.binary x y)
        /// Subtracts the right operand from the left operand.
        static member ( - ) (x:complex0,y:double) = x-complex0(Dbl y)
        /// Subtracts the right operand from the left operand.
        static member ( - ) (x:complex0,y:int) = x-complex0(Int y)
        /// Subtracts the right operand from the left operand.
        static member ( - ) (x:double,y:complex0) = complex0(Dbl x)-y
        /// Subtracts the right operand from the left operand.
        static member ( - ) (x:int,y:complex0) = complex0(Int x)-y

        ///<summary>乗算</summary>
        static member ( * ) (x:complex0,y:int0) = complex0(Mul(Zt, x.Expr, y.Expr), NumericContext.binary x y)
        /// Multiplies the operands.
        static member ( * ) (x:complex0,y:double0) = complex0(Mul(Zt, x.Expr, y.Expr), NumericContext.binary x y)
        /// Multiplies the operands.
        static member ( * ) (x:complex0,y:complex0) = complex0(Mul(Zt, x.Expr, y.Expr), NumericContext.binary x y)
        /// Multiplies the operands.
        static member ( * ) (x:complex0,y:double) = x*complex0(Dbl y)
        /// Multiplies the operands.
        static member ( * ) (x:complex0,y:int) = x*complex0(Int y)
        /// Multiplies the operands.
        static member ( * ) (x:double,y:complex0) = complex0(Dbl x)*y
        /// Multiplies the operands.
        static member ( * ) (x:double0,y:complex0) = complex0(x.Expr, x.Context)*y
        /// Multiplies the operands.
        static member ( * ) (x:int,y:complex0) = complex0(Int x)*y
        /// Multiplies the operands.
        static member ( * ) (x:int0,y:complex0) = complex0(x.Expr, x.Context)*y

        ///<summary>除算</summary>
        static member ( / ) (x:int0,y:complex0) = complex0(Div(Dt%%x.etype%%y.etype, x.Expr, y.Expr), NumericContext.binary x y)
        /// Divides the left operand by the right operand.
        static member ( / ) (x:double0,y:complex0) = complex0(Div(Dt%%x.etype%%y.etype, x.Expr, y.Expr), NumericContext.binary x y)
        /// Divides the left operand by the right operand.
        static member ( / ) (x:complex0,y:int0) = complex0(Div(Dt%%x.etype%%y.etype, x.Expr, y.Expr), NumericContext.binary x y)
        /// Divides the left operand by the right operand.
        static member ( / ) (x:complex0,y:double0) = complex0(Div(Dt%%x.etype%%y.etype, x.Expr, y.Expr), NumericContext.binary x y)
        /// Divides the left operand by the right operand.
        static member ( / ) (x:complex0,y:complex0) = complex0(Div(Dt%%x.etype%%y.etype, x.Expr, y.Expr), NumericContext.binary x y)
        /// Divides the left operand by the right operand.
        static member ( / ) (x:complex0,y:double) = x/complex0(Dbl y)
        /// Divides the left operand by the right operand.
        static member ( / ) (x:complex0,y:int) = x/complex0(Dbl(double y))
        /// Divides the left operand by the right operand.
        static member ( / ) (x:double,y:complex0) = complex0(Dbl x)/y
        /// Divides the left operand by the right operand.
        static member ( / ) (x:int,y:complex0) = complex0(Dbl(double x))/y

        ///<summary>累乗</summary>
        static member powr(x:int0, y:complex0) = complex0(Pow(Zt, x.Expr, y.Expr), NumericContext.binary x y)
        /// Raises the first operand to the power of the second operand.
        static member powr(x:complex0, y:int0) = complex0(Pow(Zt, x.Expr, y.Expr), NumericContext.binary x y)
        /// Raises the first operand to the power of the second operand.
        static member powr(x:double0, y:complex0) = complex0(Pow(Zt, x.Expr, y.Expr), NumericContext.binary x y)
        /// Raises the first operand to the power of the second operand.
        static member powr(x:complex0, y:double0) = complex0(Pow(Zt, x.Expr, y.Expr), NumericContext.binary x y)
        /// Raises the first operand to the power of the second operand.
        static member powr(x:int, y:complex0) = complex0(Pow(Zt, Int x, y.Expr), y.Context)
        /// Raises the first operand to the power of the second operand.
        static member powr(x:complex0, y:int) = complex0(Pow(Zt, x.Expr, Int y), x.Context)
        /// Raises the first operand to the power of the second operand.
        static member powr(x:double, y:complex0) = complex0(Pow(Zt, Dbl x, y.Expr), y.Context)
        /// Raises the first operand to the power of the second operand.
        static member powr(x:complex0, y:double) = complex0(Pow(Zt, x.Expr, Dbl y), x.Context)
        /// Raises the first operand to the power of the second operand.
        static member powr(x:complex0, y:complex0) = complex0(Pow(Zt, x.Expr, y.Expr), NumericContext.binary x y)
        /// Raises the first operand to the power of the second operand.
        static member ( .** ) (x:int0, y:complex0) = complex0(Pow(Zt, x.Expr, y.Expr), NumericContext.binary x y)
        /// Raises the first operand to the power of the second operand.
        static member ( .** ) (x:complex0, y:int0) = complex0(Pow(Zt, x.Expr, y.Expr), NumericContext.binary x y)
        /// Raises the first operand to the power of the second operand.
        static member ( .** ) (x:double0, y:complex0) = complex0(Pow(Zt, x.Expr, y.Expr), NumericContext.binary x y)
        /// Raises the first operand to the power of the second operand.
        static member ( .** ) (x:complex0, y:double0) = complex0(Pow(Zt, x.Expr, y.Expr), NumericContext.binary x y)
        /// Raises the first operand to the power of the second operand.
        static member ( .** ) (x:int, y:complex0) = complex0(Pow(Zt, Int x, y.Expr), y.Context)
        /// Raises the first operand to the power of the second operand.
        static member ( .** ) (x:complex0, y:int) = complex0(Pow(Zt, x.Expr, Int y), x.Context)
        /// Raises the first operand to the power of the second operand.
        static member ( .** ) (x:double, y:complex0) = complex0(Pow(Zt, Dbl x, y.Expr), y.Context)
        /// Raises the first operand to the power of the second operand.
        static member ( .** ) (x:complex0, y:double) = complex0(Pow(Zt, x.Expr, Dbl y), x.Context)
        /// Raises the first operand to the power of the second operand.
        static member ( .** ) (x:complex0, y:complex0) = complex0(Pow(Zt, x.Expr, y.Expr), NumericContext.binary x y)

        ///<summary>代入</summary>
        static member (<==) (x:complex0,y:complex0) =
            Aqualis.merge x.Context y.Context |> ignore
            expr.subst x.Expr y.Expr x.Context
        /// Assigns the right-hand value or array to the left-hand destination.
        static member (<==) (x:complex0,y:int) = x <== complex0(Int y)
        /// Assigns the right-hand value or array to the left-hand destination.
        static member (<==) (x:complex0,y:int0) = x <== complex0(y.Expr, y.Context)
        /// Assigns the right-hand value or array to the left-hand destination.
        static member (<==) (x:complex0,y:double) = x <== complex0(Dbl y)
        /// Assigns the right-hand value or array to the left-hand destination.
        static member (<==) (x:complex0,y:double0) = x <== complex0(y.Expr, y.Context)
        /// Assigns the right-hand value or array to the left-hand destination.
        static member (<==) (x:complex0,y:exprString) =
            // let context =
            //     x.Context
            //     |> Option.defaultWith (fun () ->
            //         invalidOp "The assignment target is not associated with a program.")
            match x.Context.language with
            |PHP ->
                expr.subst x.Expr (Var(Nt,y.toPhpString(".",x.Context),NaN)) x.Context
            |_ ->
                UnsupportedOperation.codeGeneration
                    (x.Context.language.ToString())
                    "assigning a string expression to a complex value"
        /// Assigns the right-hand value or array to the left-hand destination.
        static member (<==) (x:complex0,y:string) = x <== exprString y
        /// Assigns zero to this numeric variable.
        member this.clear() = this <== 0

        ///<summary>等式(TeX、HTMLのみ)</summary>
        static member (===) (x:complex0,y:complex0) =
            complex0(Eq(x.Expr,y.Expr), NumericContext.binary x y)
        /// Constructs an equality expression for document output.
        static member (===) (x:complex0,y:int) = x === complex0(Int y)
        /// Constructs an equality expression for document output.
        static member (===) (x:complex0,y:double) = x === complex0(Dbl y)

        ///<summary>等号揃等式(TeX、HTMLのみ)</summary>
        static member (=|=) (x:complex0,y:complex0) =
            let value, context = NumericContext.renderedBinary " =& " x y
            complex0(value, context)
        /// Joins equations with an alignment marker for document output.
        static member (=|=) (x:complex0,y:int) = x =|= complex0(Int y)
        /// Joins equations with an alignment marker for document output.
        static member (=|=) (x:complex0,y:double) = x =|= complex0(Dbl y)
        ///<summary>数式揃位置(TeX、HTMLのみ)</summary>
        static member (.|) (x:complex0,y:complex0) =
            let value, context = NumericContext.renderedBinary " & " x y
            complex0(value, context)
        /// Formats the expression as inline MathJax.
        static member html (e:complex0) = "\\("+e.code+"\\)"
        /// Formats the expression as inline MathJax.
        static member html (e:exprString) =
            e.data |> List.fold (fun acc a -> match a with |RStr x -> acc+x |RNvr (x,_) -> acc+"\\("+x.evalH e.Context+"\\)") ""
        /// Formats the expressions as a MathJax align environment.
        static member html (e:list<complex0>) = "\\[\\begin{align}"+String.Join("\\\\",e |> List.map (fun f -> f.code))+"\\end{align}\\]"
        /// Renders the expression as an aligned equation in its generation context.
        static member (~%%) (x:complex0) =
            expr.equivAlign (Var(Nt,"",NaN)) x.Expr x.Context

    /// Concatenation of literal text and numeric expressions.
    and exprString(x:list<reduceExprString>, context:Aqualis) =
        new(x:string) = exprString ([RStr x],Aqualis.BlankWriter Numeric)
        new(x:bool0) = exprString([RNvr(x.Expr,x.Context)], x.Context)
        new(x:int0) = exprString([RNvr(x.Expr,x.Context)], x.Context)
        new(x:double0) = exprString([RNvr(x.Expr,x.Context)], x.Context)
        new(x:complex0) = exprString([RNvr(x.Expr,x.Context)], x.Context)

        /// Gets the segments of this expression string.
        member _.data with get() = x
        /// Gets the owning generation context.
        member _.Context = context

        /// Renders expression segments with the requested separator and quoting mode.
        member this.toString(c:string,op:ExprConcatOption) =
            x
            |> List.map (function
                |RStr x ->
                    match op with
                    |Direct -> x
                    |StrQuotation -> "\""+x+"\""
                    |CodeStrQuotation -> "\\\""+x+"\\\""
                |RNvr (x,_) ->
                    x.eval context)
            |> fun s -> String.Join(c,s)

        /// Renders segments as PHP string literals or expressions for the target context.
        member internal this.toPhpString(separator:string,target:Aqualis) =
            x
            |> List.map (function
                |RStr value -> PhpEncoding.stringLiteral value
                |RNvr (value,valueContext) ->
                    Aqualis.merge target valueContext |> ignore
                    value.eval target)
            |> fun values -> String.Join(separator,values)
        /// Concatenates the operands for generated output.
        static member (++) (a:exprString,b:exprString) : exprString =
            exprString(a.data@b.data, Aqualis.merge a.Context b.Context)
        /// Concatenates the operands for generated output.
        static member (++) (a:string,b:exprString) = exprString a ++ b
        /// Concatenates the operands for generated output.
        static member (++) (a:exprString,b:string) = a ++ exprString b
        /// Concatenates the operands for generated output.
        static member (++) (a:exprString,b:int0) = a ++ exprString b
        /// Concatenates the operands for generated output.
        static member (++) (a:exprString,b:double0) = a ++ exprString b
        /// Concatenates the operands for generated output.
        static member (++) (a:exprString,b:complex0) = a ++ exprString b
        /// Concatenates the operands for generated output.
        static member (++) (a:exprString,b:bool0) = a ++ exprString b
        /// Concatenates the operands for generated output.
        static member (++) (a:int,b:exprString) = int0(Int a) ++ b
        /// Concatenates the operands for generated output.
        static member (++) (a:exprString,b:int) = a ++ int0(Int b)
        /// Concatenates the operands for generated output.
        static member (++) (a:double,b:exprString) = double0(Dbl a) ++ b
        /// Concatenates the operands for generated output.
        static member (++) (a:exprString,b:double) = a ++ double0(Dbl b)

    [<AutoOpen>]
    /// Adds inline MathJax output methods to generation contexts.
    module ExprStringOutputExtensions =
        type Aqualis with
            /// Writes text and embedded expressions as inline MathJax.
            member this.writein(value:exprString) = this.writein(double0.html value)
            /// Writes an integer expression as inline MathJax.
            member this.writein(value:int0) = this.writein(exprString(value))
            /// Writes a double-precision expression as inline MathJax.
            member this.writein(value:double0) = this.writein(exprString(value))
            /// Writes a complex expression as inline MathJax.
            member this.writein(value:complex0) = this.writein(exprString(value))
            
    [<AutoOpen>]
    /// Adds conversion to double-precision expressions for real scalars.
    module Real0Extensions =
        type IReal0 with
            /// Views this real scalar expression as a double-precision expression.
            member this.ToDouble0 = double0(this.Expr, this.Context)

    [<AutoOpen>]
    /// Adds conversion to complex expressions for numeric scalars.
    module Num0Extensions =
        type INum0 with
            /// Views this scalar numeric expression as a complex expression.
            member this.ToComplex0 = complex0(this.Expr, this.Context)

    [<AutoOpen>]
    /// Constructs expression strings from literal and numeric values.
    module strExpr =
        /// Wraps a literal string as an expression string.
        let st (x:string) = exprString x
        /// Wraps an integer expression as an expression string.
        let iv (x:int0) = exprString x
        /// Wraps a double-precision expression as an expression string.
        let dv (x:double0) = exprString x
        /// Wraps a complex expression as an expression string.
        let zv (x:complex0) = exprString x
