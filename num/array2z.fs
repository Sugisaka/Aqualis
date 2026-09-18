namespace Aqualis

    /// Two-dimensional array of symbolic complex expressions.
    type complex2 (typ:Etype,x:Expr2,context:Aqualis) as this =
        inherit NumericArray2<complex0,complex1,complex2>(typ,x,context)
        new(typ,x) = complex2(typ,x,Aqualis.BlankWriter Numeric)
        new(context:Aqualis,typ,size,name,para)=
            context.cvar.setVar(typ,size,name,para)
            complex2(typ,Var2(size,name),context=context)
        new(a:int0,b:int0,f:int0*int0->complex0) = complex2(Zt,Arx2(a, b, fun ij -> (f ij).Expr))
        new(a:int ,b:int0,f:int0*int0->complex0) = complex2(Zt,Arx2(I a, b, fun ij -> (f ij).Expr))
        new(a:int0,b:int ,f:int0*int0->complex0) = complex2(Zt,Arx2(a, I b, fun ij -> (f ij).Expr))
        new(a:int ,b:int ,f:int0*int0->complex0) = complex2(Zt,Arx2(I a, I b, fun ij -> (f ij).Expr))
        override _.WrapScalar value=complex0(value,this.Context)
        override _.WrapRow value=complex1(typ,value,this.Context)
        override _.CreateWithContext(elementType,value,resultContext)=complex2(elementType,value,resultContext)
        override _.AssignAt(i,j,value)=this[i,j] <== complex0(value,this.Context)
        override _.clear()=this.AssignScalar(complex0(Int 0))
        override _.sizeinit()=this.size1<== -1;this.size2<== -1
        /// Assigns the right-hand value or array to the left-hand destination.
        static member (<==)(x:complex2,y:complex2)=x.AssignArray y
        /// Assigns the right-hand value or array to the left-hand destination.
        static member (<==)(x:complex2,y:complex0)=x.AssignScalar y
        /// Assigns the right-hand value or array to the left-hand destination.
        static member (<==)(x:complex2,y:double0)=x.AssignScalar((y :> INum0).ToComplex0)
        /// Assigns the right-hand value or array to the left-hand destination.
        static member (<==)(x:complex2,y:int0)=x.AssignScalar((y :> INum0).ToComplex0)
        /// Assigns the right-hand value or array to the left-hand destination.
        static member (<==)(x:complex2,y:double)=x.AssignScalar(complex0(Dbl y))
        /// Assigns the right-hand value or array to the left-hand destination.
        static member (<==)(x:complex2,y:int)=x.AssignScalar(complex0(Int y))

    /// Numeric operations for complex 2D arrays.
    [<AutoOpen>]
    module asm_complex2 =
        type asm with
            /// Raises the first operand to the power of the second operand.
            static member pow(x:complex2,y:int0)=complex2(x.etype%%y.etype,Arx2(x.size1,x.size2,fun(i,j)->asm.pow(x[i,j],y).Expr),Aqualis.merge x.Context y.Context)
            /// Computes the sine of the operand.
            static member sin(x:complex2)=complex2(x.etype,Arx2(x.size1,x.size2,fun(i,j)->asm.sin(x[i,j]).Expr),x.Context)
            /// Computes the cosine of the operand.
            static member cos(x:complex2)=complex2(x.etype,Arx2(x.size1,x.size2,fun(i,j)->asm.cos(x[i,j]).Expr),x.Context)
            /// Computes the tangent of the operand.
            static member tan(x:complex2)=complex2(x.etype,Arx2(x.size1,x.size2,fun(i,j)->asm.tan(x[i,j]).Expr),x.Context)
            /// Computes the inverse sine of the operand.
            static member asin(x:complex2)=complex2(x.etype,Arx2(x.size1,x.size2,fun(i,j)->asm.asin(x[i,j]).Expr),x.Context)
            /// Computes the inverse cosine of the operand.
            static member acos(x:complex2)=complex2(x.etype,Arx2(x.size1,x.size2,fun(i,j)->asm.acos(x[i,j]).Expr),x.Context)
            /// Computes the inverse tangent of the operand.
            static member atan(x:complex2)=complex2(x.etype,Arx2(x.size1,x.size2,fun(i,j)->asm.atan(x[i,j]).Expr),x.Context)
            /// Computes the exponential of the operand.
            static member exp(x:complex2)=complex2(x.etype,Arx2(x.size1,x.size2,fun(i,j)->asm.exp(x[i,j]).Expr),x.Context)
            /// Computes the absolute value or complex magnitude of the operand.
            static member abs(x:complex2)=double2(Dt,Arx2(x.size1,x.size2,fun(i,j)->asm.abs(x[i,j]).Expr),x.Context)
            /// Computes the natural logarithm of the operand.
            static member log(x:complex2)=complex2(x.etype,Arx2(x.size1,x.size2,fun(i,j)->asm.log(x[i,j]).Expr),x.Context)
            /// Computes the base-10 logarithm of the operand.
            static member log10(x:complex2)=complex2(x.etype,Arx2(x.size1,x.size2,fun(i,j)->asm.log10(x[i,j]).Expr),x.Context)
            /// Computes the square root of the operand.
            static member sqrt(x:complex2)=complex2(x.etype,Arx2(x.size1,x.size2,fun(i,j)->asm.sqrt(x[i,j]).Expr),x.Context)
            /// Computes the complex conjugate of the operand.
            static member conj(x:complex2)=complex2(x.etype,Arx2(x.size1,x.size2,fun(i,j)->asm.conj(x[i,j]).Expr),x.Context)

    /// Conversions for two-dimensional real arrays.
    [<AutoOpen>]
    module Real2Extensions =
        type IReal2 with
            /// Views this real two-dimensional expression array as a double-precision array.
            member this.ToDouble2 = double2(this.Etype, this.Expr, this.Context)

    /// Conversions for two-dimensional numeric arrays.
    [<AutoOpen>]
    module Num2Extensions =
        type INum2 with
            /// Views this two-dimensional numeric expression array as a complex array.
            member this.ToComplex2 = complex2(this.Etype, this.Expr, this.Context)
