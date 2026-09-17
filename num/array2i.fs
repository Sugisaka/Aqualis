namespace Aqualis

    /// Two-dimensional array of symbolic integer expressions.
    type int2 (typ:Etype,x:Expr2,context:Aqualis) as this =
        inherit NumericArray2<int0,int1,int2>(typ,x,context)
        interface IReal2
        new(typ,x) = int2(typ,x,Aqualis.BlankWriter Numeric)
        new(context:Aqualis,typ,size,name,para)=
            context.cvar.setVar(typ,size,name,para)
            int2(typ,Var2(size,name),context=context)
        new(a:int0,b:int0,f:int0*int0->int0) = int2(It 4,Arx2(a, b, fun ij -> (f ij).Expr))
        new(a:int ,b:int0,f:int0*int0->int0) = int2(It 4,Arx2(I a, b, fun ij -> (f ij).Expr))
        new(a:int0,b:int ,f:int0*int0->int0) = int2(It 4,Arx2(a, I b, fun ij -> (f ij).Expr))
        new(a:int ,b:int ,f:int0*int0->int0) = int2(It 4,Arx2(I a, I b, fun ij -> (f ij).Expr))
        override _.WrapScalar value=int0(value,this.Context)
        override _.WrapRow value=int1(typ,value,this.Context)
        override _.CreateWithContext(elementType,value,resultContext)=int2(elementType,value,resultContext)
        override _.AssignAt(i,j,value)=this[i,j] <== int0(value,this.Context)
        override _.clear()=this.AssignScalar(I 0)
        override _.sizeinit()=this.size1<== -1; this.size2<== -1
        /// Builds a division expression with integer result semantics.
        static member (./)(x:int2,y:int2)=base2.sizeMismatchError(x,y);int2(x.etype%%y.etype,Arx2(x.size1,x.size2,fun(i,j)->Div(It 4,x[i,j].Expr,y[i,j].Expr)),Aqualis.merge x.Context y.Context)
        /// Builds a division expression with integer result semantics.
        static member (./)(x:int0,y:int2)=int2(x.etype%%y.etype,Arx2(y.size1,y.size2,fun(i,j)->Div(It 4,x.Expr,y[i,j].Expr)),Aqualis.merge x.Context y.Context)
        /// Builds a division expression with integer result semantics.
        static member (./)(x:int,y:int2)=I x ./ y
        /// Builds a division expression with integer result semantics.
        static member (./)(x:int2,y:int0)=int2(x.etype%%y.etype,Arx2(x.size1,x.size2,fun(i,j)->Div(It 4,x[i,j].Expr,y.Expr)),Aqualis.merge x.Context y.Context)
        /// Builds a division expression with integer result semantics.
        static member (./)(x:int2,y:int)=x ./ I y
        /// Assigns the right-hand value or array to the left-hand destination.
        static member (<==)(x:int2,y:int2)=x.AssignArray y
        /// Assigns the right-hand value or array to the left-hand destination.
        static member (<==)(x:int2,y:int0)=x.AssignScalar y
        /// Assigns the right-hand value or array to the left-hand destination.
        static member (<==)(x:int2,y:int)=x.AssignScalar(I y)

    [<AutoOpen>]
    /// Numeric operations for integer two-dimensional arrays.
    module asm_int2 =
        type asm with
            /// Raises the first operand to the power of the second operand.
            static member pow(x:int2,y:int0)=double2(Dt,Arx2(x.size1,x.size2,fun(i,j)->asm.pow(x[i,j],y).Expr),Aqualis.merge x.Context y.Context)
            /// Computes the sine of the operand.
            static member sin(x:int2)=double2(Dt,Arx2(x.size1,x.size2,fun(i,j)->asm.sin(x[i,j]).Expr),x.Context)
            /// Computes the cosine of the operand.
            static member cos(x:int2)=double2(Dt,Arx2(x.size1,x.size2,fun(i,j)->asm.cos(x[i,j]).Expr),x.Context)
            /// Computes the tangent of the operand.
            static member tan(x:int2)=double2(Dt,Arx2(x.size1,x.size2,fun(i,j)->asm.tan(x[i,j]).Expr),x.Context)
            /// Computes the inverse sine of the operand.
            static member asin(x:int2)=double2(Dt,Arx2(x.size1,x.size2,fun(i,j)->asm.asin(x[i,j]).Expr),x.Context)
            /// Computes the inverse cosine of the operand.
            static member acos(x:int2)=double2(Dt,Arx2(x.size1,x.size2,fun(i,j)->asm.acos(x[i,j]).Expr),x.Context)
            /// Computes the inverse tangent of the operand.
            static member atan(x:int2)=double2(Dt,Arx2(x.size1,x.size2,fun(i,j)->asm.atan(x[i,j]).Expr),x.Context)
            /// Computes the two-argument inverse tangent.
            static member atan2(x:int2,y:int2)=double2(Dt,Arx2(x.size1,x.size2,fun(i,j)->asm.atan2(x[i,j],y[i,j]).Expr),Aqualis.merge x.Context y.Context)
            /// Computes the exponential of the operand.
            static member exp(x:int2)=double2(Dt,Arx2(x.size1,x.size2,fun(i,j)->asm.exp(x[i,j]).Expr),x.Context)
            /// Computes the absolute value or complex magnitude of the operand.
            static member abs(x:int2)=double2(Dt,Arx2(x.size1,x.size2,fun(i,j)->asm.abs(x[i,j]).Expr),x.Context)
            /// Computes the natural logarithm of the operand.
            static member log(x:int2)=double2(Dt,Arx2(x.size1,x.size2,fun(i,j)->asm.log(x[i,j]).Expr),x.Context)
            /// Computes the base-10 logarithm of the operand.
            static member log10(x:int2)=double2(Dt,Arx2(x.size1,x.size2,fun(i,j)->asm.log10(x[i,j]).Expr),x.Context)
            /// Computes the square root of the operand.
            static member sqrt(x:int2)=double2(Dt,Arx2(x.size1,x.size2,fun(i,j)->asm.sqrt(x[i,j]).Expr),x.Context)
