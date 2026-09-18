//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    /// One-dimensional complex expression array.
    /// One-dimensional array of symbolic complex expressions.
    type complex1 (typ:Etype,x:Expr1, context:Aqualis) as this =
        inherit NumericArray1<complex0,complex1>(typ,x,context)

        new (typ,x) = complex1(typ,x,Aqualis.BlankWriter Numeric)
        new (context:Aqualis,typ,size,name,para) =
            context.cvar.setVar(typ,size,name,para)
            complex1(typ,Var1(size,name),context=context)
        new(a:int0,f:int0->complex0) = complex1(Zt,Arx1(a,fun i -> (f i).Expr))
        new(a:int ,f:int0->complex0) = complex1(Zt,Arx1(I a,fun i -> (f i).Expr))

        override _.WrapScalar(value,resultContext) = complex0(value,resultContext)
        override _.Create(elementType,value,resultContext) = complex1(elementType,value,resultContext)
        override _.AssignAt(index,value) = this[index] <== complex0(value,this.Context)
        override _.clear() = this.AssignScalar(complex0(Int 0))
        override _.sizeinit() = this.size1 <== -1

        /// Assigns the right-hand value or array to the left-hand destination.
        static member (<==) (x:complex1,y:complex1) = x.AssignArray y
        /// Assigns the right-hand value or array to the left-hand destination.
        static member (<==) (x:complex1,y:complex0) = x.AssignScalar y
        /// Assigns the right-hand value or array to the left-hand destination.
        static member (<==) (x:complex1,y:double0) = x.AssignScalar((y :> INum0).ToComplex0)
        /// Assigns the right-hand value or array to the left-hand destination.
        static member (<==) (x:complex1,y:int0) = x.AssignScalar((y :> INum0).ToComplex0)
        /// Assigns the right-hand value or array to the left-hand destination.
        static member (<==) (x:complex1,y:double) = x.AssignScalar(complex0(Dbl y))
        /// Assigns the right-hand value or array to the left-hand destination.
        static member (<==) (x:complex1,y:int) = x.AssignScalar(complex0(Int y))

    /// Numeric operations for complex 1D arrays.
    [<AutoOpen>]
    module asm_complex1 =
        type asm with
            /// Raises the first operand to the power of the second operand.
            static member pow(x:double1,y:complex0) = complex1(x.etype%%y.etype,Arx1(x.size1,fun i -> asm.pow(x[i],y).Expr),Aqualis.merge x.Context y.Context)
            /// Raises the first operand to the power of the second operand.
            static member pow(x:complex1,y:int0) = complex1(x.etype%%y.etype,Arx1(x.size1,fun i -> asm.pow(x[i],y).Expr),Aqualis.merge x.Context y.Context)
            /// Raises the first operand to the power of the second operand.
            static member pow(x:complex1,y:double0) = complex1(x.etype%%y.etype,Arx1(x.size1,fun i -> asm.pow(x[i],y).Expr),Aqualis.merge x.Context y.Context)
            /// Raises the first operand to the power of the second operand.
            static member pow(x:complex1,y:complex0) = complex1(x.etype%%y.etype,Arx1(x.size1,fun i -> asm.pow(x[i],y).Expr),Aqualis.merge x.Context y.Context)
            /// Raises the first operand to the power of the second operand.
            static member pow(x:complex1,y:int) = asm.pow(x,I y)
            /// Raises the first operand to the power of the second operand.
            static member pow(x:complex1,y:double) = asm.pow(x,D y)
            /// Computes the sine of the operand.
            static member sin(x:complex1) = complex1(x.etype,Arx1(x.size1,fun i -> asm.sin(x[i]).Expr),x.Context)
            /// Computes the cosine of the operand.
            static member cos(x:complex1) = complex1(x.etype,Arx1(x.size1,fun i -> asm.cos(x[i]).Expr),x.Context)
            /// Computes the tangent of the operand.
            static member tan(x:complex1) = complex1(x.etype,Arx1(x.size1,fun i -> asm.tan(x[i]).Expr),x.Context)
            /// Computes the inverse sine of the operand.
            static member asin(x:complex1) = complex1(x.etype,Arx1(x.size1,fun i -> asm.asin(x[i]).Expr),x.Context)
            /// Computes the inverse cosine of the operand.
            static member acos(x:complex1) = complex1(x.etype,Arx1(x.size1,fun i -> asm.acos(x[i]).Expr),x.Context)
            /// Computes the inverse tangent of the operand.
            static member atan(x:complex1) = complex1(x.etype,Arx1(x.size1,fun i -> asm.atan(x[i]).Expr),x.Context)
            /// Computes the exponential of the operand.
            static member exp(x:complex1) = complex1(x.etype,Arx1(x.size1,fun i -> asm.exp(x[i]).Expr),x.Context)
            /// Computes the absolute value or complex magnitude of the operand.
            static member abs(x:complex1) = double1(Dt,Arx1(x.size1,fun i -> asm.abs(x[i]).Expr),x.Context)
            /// Computes the natural logarithm of the operand.
            static member log(x:complex1) = complex1(x.etype,Arx1(x.size1,fun i -> asm.log(x[i]).Expr),x.Context)
            /// Computes the base-10 logarithm of the operand.
            static member log10(x:complex1) = complex1(x.etype,Arx1(x.size1,fun i -> asm.log10(x[i]).Expr),x.Context)
            /// Computes the square root of the operand.
            static member sqrt(x:complex1) = complex1(x.etype,Arx1(x.size1,fun i -> asm.sqrt(x[i]).Expr),x.Context)
            /// Computes the complex conjugate of the operand.
            static member conj(x:complex1) = complex1(x.etype,Arx1(x.size1,fun i -> asm.conj(x[i]).Expr),x.Context)

    /// Conversions for one-dimensional real arrays.
    [<AutoOpen>]
    module Real1Extensions =
        type IReal1 with
            /// Views this real one-dimensional expression array as a double-precision array.
            member this.ToDouble1 = double1(this.Etype, this.Expr, this.Context)

    /// Conversions for one-dimensional numeric arrays.
    [<AutoOpen>]
    module Num1Extensions =
        type INum1 with
            /// Views this one-dimensional numeric expression array as a complex array.
            member this.ToComplex1 = complex1(this.Etype, this.Expr, this.Context)
