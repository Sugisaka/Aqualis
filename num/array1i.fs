//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    /// One-dimensional integer expression array.
    /// One-dimensional array of symbolic integer expressions.
    type int1 (typ:Etype,x:Expr1, context:Aqualis) as this =
        inherit NumericArray1<int0,int1>(typ,x,context)

        interface IReal1

        new (typ,x) = int1(typ,x,Aqualis.BlankWriter Numeric)
        new (context:Aqualis,typ,size,name,para) =
            context.cvar.setVar(typ,size,name,para)
            int1(typ,Var1(size,name),context=context)
        new(a:int0,f:int0->int0) = int1(It 4,Arx1(a,fun i -> (f i).Expr))
        new(a:int ,f:int0->int0) = int1(It 4,Arx1(I a,fun i -> (f i).Expr))
        override _.WrapScalar(value,resultContext) = int0(value,resultContext)
        override _.Create(elementType,value,resultContext) = int1(elementType,value,resultContext)
        override _.AssignAt(index,value) = this[index] <== int0(value,this.Context)
        override _.clear() = this.AssignScalar(I 0)
        override _.sizeinit() = this.size1 <== -1

        /// Builds a division expression with integer result semantics.
        static member (./) (x:int1,y:int1) =
            base1.sizeMismatchError(x,y)
            int1(x.etype%%y.etype,Arx1(x.size1,fun i -> Div(It 4,x[i].Expr,y[i].Expr)),Aqualis.merge x.Context y.Context)
        /// Builds a division expression with integer result semantics.
        static member (./) (x:int0,y:int1) = int1(x.etype%%y.etype,Arx1(y.size1,fun i -> Div(It 4,x.Expr,y[i].Expr)),Aqualis.merge x.Context y.Context)
        /// Builds a division expression with integer result semantics.
        static member (./) (x:int,y:int1) = I x ./ y
        /// Builds a division expression with integer result semantics.
        static member (./) (x:int1,y:int0) = int1(x.etype%%y.etype,Arx1(x.size1,fun i -> Div(It 4,x[i].Expr,y.Expr)),Aqualis.merge x.Context y.Context)
        /// Builds a division expression with integer result semantics.
        static member (./) (x:int1,y:int) = x ./ I y

        /// Assigns the right-hand value or array to the left-hand destination.
        static member (<==) (x:int1,y:int1) = x.AssignArray y
        /// Assigns the right-hand value or array to the left-hand destination.
        static member (<==) (x:int1,y:int0) = x.AssignScalar y
        /// Assigns the right-hand value or array to the left-hand destination.
        static member (<==) (x:int1,y:int) = x.AssignScalar(I y)

    [<AutoOpen>]
    /// Numeric operations for integer 1D arrays.
    module asm_int1 =
        type asm with
            /// Raises the first operand to the power of the second operand.
            static member pow(x:int1,y:int0) = double1(Dt,Arx1(x.size1,fun i -> asm.pow(x[i],y).Expr),Aqualis.merge x.Context y.Context)
            /// Raises the first operand to the power of the second operand.
            static member pow(x:int1,y:double0) = double1(Dt,Arx1(x.size1,fun i -> asm.pow(x[i],y).Expr),Aqualis.merge x.Context y.Context)
            /// Raises the first operand to the power of the second operand.
            static member pow(x:int1,y:complex0) = complex1(Zt,Arx1(x.size1,fun i -> asm.pow(x[i],y).Expr),Aqualis.merge x.Context y.Context)
            /// Raises the first operand to the power of the second operand.
            static member pow(x:int1,y:int) = asm.pow(x,I y)
            /// Raises the first operand to the power of the second operand.
            static member pow(x:int1,y:double) = asm.pow(x,D y)
            /// Computes the sine of the operand.
            static member sin(x:int1) = double1(Dt,Arx1(x.size1,fun i -> asm.sin(x[i]).Expr),x.Context)
            /// Computes the cosine of the operand.
            static member cos(x:int1) = double1(Dt,Arx1(x.size1,fun i -> asm.cos(x[i]).Expr),x.Context)
            /// Computes the tangent of the operand.
            static member tan(x:int1) = double1(Dt,Arx1(x.size1,fun i -> asm.tan(x[i]).Expr),x.Context)
            /// Computes the inverse sine of the operand.
            static member asin(x:int1) = double1(Dt,Arx1(x.size1,fun i -> asm.asin(x[i]).Expr),x.Context)
            /// Computes the inverse cosine of the operand.
            static member acos(x:int1) = double1(Dt,Arx1(x.size1,fun i -> asm.acos(x[i]).Expr),x.Context)
            /// Computes the inverse tangent of the operand.
            static member atan(x:int1) = double1(Dt,Arx1(x.size1,fun i -> asm.atan(x[i]).Expr),x.Context)
            /// Computes the two-argument inverse tangent.
            static member atan2(x:int1,y:int1) = double1(Dt,Arx1(x.size1,fun i -> asm.atan2(x[i],y[i]).Expr),Aqualis.merge x.Context y.Context)
            /// Computes the exponential of the operand.
            static member exp(x:int1) = double1(Dt,Arx1(x.size1,fun i -> asm.exp(x[i]).Expr),x.Context)
            /// Computes the absolute value or complex magnitude of the operand.
            static member abs(x:int1) = double1(Dt,Arx1(x.size1,fun i -> asm.abs(x[i]).Expr),x.Context)
            /// Computes the natural logarithm of the operand.
            static member log(x:int1) = double1(Dt,Arx1(x.size1,fun i -> asm.log(x[i]).Expr),x.Context)
            /// Computes the base-10 logarithm of the operand.
            static member log10(x:int1) = double1(Dt,Arx1(x.size1,fun i -> asm.log10(x[i]).Expr),x.Context)
            /// Computes the square root of the operand.
            static member sqrt(x:int1) = double1(Dt,Arx1(x.size1,fun i -> asm.sqrt(x[i]).Expr),x.Context)
