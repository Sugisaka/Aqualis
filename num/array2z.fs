namespace Aqualis

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
        static member (<==)(x:complex2,y:complex2)=x.AssignArray y
        static member (<==)(x:complex2,y:complex0)=x.AssignScalar y
        static member (<==)(x:complex2,y:double0)=x.AssignScalar(complex0 y.Expr)
        static member (<==)(x:complex2,y:int0)=x.AssignScalar(complex0 y.Expr)
        static member (<==)(x:complex2,y:double)=x.AssignScalar(complex0(Dbl y))
        static member (<==)(x:complex2,y:int)=x.AssignScalar(complex0(Int y))

    [<AutoOpen>]
    module asm_complex2 =
        type asm with
            static member pow(x:complex2,y:int0)=complex2(x.etype%%y.etype,Arx2(x.size1,x.size2,fun(i,j)->asm.pow(x[i,j],y).Expr))
            static member sin(x:complex2)=complex2(x.etype,Arx2(x.size1,x.size2,fun(i,j)->asm.sin(x[i,j]).Expr))
            static member cos(x:complex2)=complex2(x.etype,Arx2(x.size1,x.size2,fun(i,j)->asm.cos(x[i,j]).Expr))
            static member tan(x:complex2)=complex2(x.etype,Arx2(x.size1,x.size2,fun(i,j)->asm.tan(x[i,j]).Expr))
            static member asin(x:complex2)=complex2(x.etype,Arx2(x.size1,x.size2,fun(i,j)->asm.asin(x[i,j]).Expr))
            static member acos(x:complex2)=complex2(x.etype,Arx2(x.size1,x.size2,fun(i,j)->asm.acos(x[i,j]).Expr))
            static member atan(x:complex2)=complex2(x.etype,Arx2(x.size1,x.size2,fun(i,j)->asm.atan(x[i,j]).Expr))
            static member exp(x:complex2)=complex2(x.etype,Arx2(x.size1,x.size2,fun(i,j)->asm.exp(x[i,j]).Expr))
            static member abs(x:complex2)=double2(Dt,Arx2(x.size1,x.size2,fun(i,j)->asm.abs(x[i,j]).Expr))
            static member log(x:complex2)=complex2(x.etype,Arx2(x.size1,x.size2,fun(i,j)->asm.log(x[i,j]).Expr))
            static member log10(x:complex2)=complex2(x.etype,Arx2(x.size1,x.size2,fun(i,j)->asm.log10(x[i,j]).Expr))
            static member sqrt(x:complex2)=complex2(x.etype,Arx2(x.size1,x.size2,fun(i,j)->asm.sqrt(x[i,j]).Expr))
            static member conj(x:complex2)=complex2(x.etype,Arx2(x.size1,x.size2,fun(i,j)->asm.conj(x[i,j]).Expr))

    [<AutoOpen>]
    module Real2Extensions =
        type IReal2 with
            /// Views this real two-dimensional expression array as a double-precision array.
            member this.ToDouble2 = double2(this.Etype, this.Expr, this.Context)

    [<AutoOpen>]
    module Num2Extensions =
        type INum2 with
            /// Views this two-dimensional numeric expression array as a complex array.
            member this.ToComplex2 = complex2(this.Etype, this.Expr, this.Context)
