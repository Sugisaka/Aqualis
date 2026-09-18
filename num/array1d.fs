//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    /// One-dimensional real expression array.
    /// One-dimensional array of symbolic real expressions.
    type double1 (typ:Etype,x:Expr1, context:Aqualis) as this =
        inherit NumericArray1<double0,double1>(typ,x,context)

        interface IReal1

        new (typ,x) = double1(typ,x,Aqualis.BlankWriter Numeric)
        new (context:Aqualis,typ,size,name,para) =
            context.cvar.setVar(typ,size,name,para)
            double1(typ,Var1(size,name),context=context)
        new(a:int0,f:int0->double0) = double1(Dt,Arx1(a,fun i -> (f i).Expr))
        new(a:int ,f:int0->double0) = double1(Dt,Arx1(I a,fun i -> (f i).Expr))

        override _.WrapScalar(value,resultContext) = double0(value,resultContext)
        override _.Create(elementType,value,resultContext) = double1(elementType,value,resultContext)
        override _.AssignAt(index,value) = this[index] <== double0(value,this.Context)
        override _.clear() = this.AssignScalar(D 0.0)
        override _.sizeinit() = this.size1 <== -1

        /// Assigns the right-hand value or array to the left-hand destination.
        static member (<==) (x:double1,y:double1) = x.AssignArray y
        /// Assigns the right-hand value or array to the left-hand destination.
        static member (<==) (x:double1,y:double0) = x.AssignScalar y
        /// Assigns the right-hand value or array to the left-hand destination.
        static member (<==) (x:double1,y:int0) = x.AssignScalar((y :> IReal0).ToDouble0)
        /// Assigns the right-hand value or array to the left-hand destination.
        static member (<==) (x:double1,y:double) = x.AssignScalar(D y)
        /// Assigns the right-hand value or array to the left-hand destination.
        static member (<==) (x:double1,y:int) = x.AssignScalar(I y)

    /// Numeric operations for double-precision 1D arrays.
    [<AutoOpen>]
    module asm_double1 =
        type asm with
            /// Raises the first operand to the power of the second operand.
            static member pow(x:double1,y:int0) = double1(x.etype%%y.etype,Arx1(x.size1,fun i -> asm.pow(x[i],y).Expr),Aqualis.merge x.Context y.Context)
            /// Raises the first operand to the power of the second operand.
            static member pow(x:double1,y:double0) = double1(x.etype%%y.etype,Arx1(x.size1,fun i -> asm.pow(x[i],y).Expr),Aqualis.merge x.Context y.Context)
            /// Raises the first operand to the power of the second operand.
            static member pow(x:double1,y:int) = asm.pow(x,I y)
            /// Raises the first operand to the power of the second operand.
            static member pow(x:double1,y:double) = asm.pow(x,D y)
            /// Computes the sine of the operand.
            static member sin(x:double1) = double1(x.etype,Arx1(x.size1,fun i -> asm.sin(x[i]).Expr),x.Context)
            /// Computes the cosine of the operand.
            static member cos(x:double1) = double1(x.etype,Arx1(x.size1,fun i -> asm.cos(x[i]).Expr),x.Context)
            /// Computes the tangent of the operand.
            static member tan(x:double1) = double1(x.etype,Arx1(x.size1,fun i -> asm.tan(x[i]).Expr),x.Context)
            /// Computes the inverse sine of the operand.
            static member asin(x:double1) = double1(x.etype,Arx1(x.size1,fun i -> asm.asin(x[i]).Expr),x.Context)
            /// Computes the inverse cosine of the operand.
            static member acos(x:double1) = double1(x.etype,Arx1(x.size1,fun i -> asm.acos(x[i]).Expr),x.Context)
            /// Computes the inverse tangent of the operand.
            static member atan(x:double1) = double1(x.etype,Arx1(x.size1,fun i -> asm.atan(x[i]).Expr),x.Context)
            /// Computes the two-argument inverse tangent.
            static member atan2(x:double1,y:double1) = double1(Dt,Arx1(x.size1,fun i -> asm.atan2(x[i],y[i]).Expr),Aqualis.merge x.Context y.Context)
            /// Computes the exponential of the operand.
            static member exp(x:double1) = double1(x.etype,Arx1(x.size1,fun i -> asm.exp(x[i]).Expr),x.Context)
            /// Computes the absolute value or complex magnitude of the operand.
            static member abs(x:double1) = double1(x.etype,Arx1(x.size1,fun i -> asm.abs(x[i]).Expr),x.Context)
            /// Computes the natural logarithm of the operand.
            static member log(x:double1) = double1(x.etype,Arx1(x.size1,fun i -> asm.log(x[i]).Expr),x.Context)
            /// Computes the base-10 logarithm of the operand.
            static member log10(x:double1) = double1(x.etype,Arx1(x.size1,fun i -> asm.log10(x[i]).Expr),x.Context)
            /// Computes the square root of the operand.
            static member sqrt(x:double1) = double1(x.etype,Arx1(x.size1,fun i -> asm.sqrt(x[i]).Expr),x.Context)
            /// Rounds the operand down to an integer value.
            static member floor(x:double1) = double1(x.etype,Arx1(x.size1,fun i -> asm.floor(x[i]).Expr),x.Context)
            /// Rounds the operand up to an integer value.
            static member ceil(x:double1) = double1(x.etype,Arx1(x.size1,fun i -> asm.ceil(x[i]).Expr),x.Context)
