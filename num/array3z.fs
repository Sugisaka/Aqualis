namespace Aqualis

    type complex3(typ:Etype,x:Expr3,context:Aqualis) as this=
        inherit NumericArray3<complex0,complex1,complex2,complex3>(typ,x,context)
        new(typ,x) = complex3(typ,x,Aqualis.BlankWriter Numeric)
        new(context:Aqualis,typ,size,name,para)=
            context.cvar.setVar(typ,size,name,para)
            complex3(typ,Var3(size,name),context=context)
        new(a:int0,b:int0,c:int0,f:int0*int0*int0->complex0) = complex3(Zt,Arx3(a,b,c,fun ijk->(f ijk).Expr))
        new(a:int ,b:int0,c:int0,f:int0*int0*int0->complex0) = complex3(Zt,Arx3(I a,b,c,fun ijk->(f ijk).Expr))
        new(a:int0,b:int ,c:int0,f:int0*int0*int0->complex0) = complex3(Zt,Arx3(a,I b,c,fun ijk->(f ijk).Expr))
        new(a:int0,b:int0,c:int ,f:int0*int0*int0->complex0) = complex3(Zt,Arx3(a,b,I c,fun ijk->(f ijk).Expr))
        new(a:int0,b:int ,c:int ,f:int0*int0*int0->complex0) = complex3(Zt,Arx3(a,I b,I c,fun ijk->(f ijk).Expr))
        new(a:int ,b:int0,c:int ,f:int0*int0*int0->complex0) = complex3(Zt,Arx3(I a,b,I c,fun ijk->(f ijk).Expr))
        new(a:int ,b:int ,c:int0,f:int0*int0*int0->complex0) = complex3(Zt,Arx3(I a,I b,c,fun ijk->(f ijk).Expr))
        new(a:int ,b:int ,c:int ,f:int0*int0*int0->complex0) = complex3(Zt,Arx3(I a,I b,I c,fun ijk->(f ijk).Expr))
        override _.WrapScalar value=complex0(value,this.Context)
        override _.WrapRow value=complex1(typ,value,this.Context)
        override _.WrapMatrix value=complex2(typ,value,this.Context)
        override _.CreateWithContext(elementType,value,resultContext)=complex3(elementType,value,resultContext)
        override _.AssignAt(i,j,k,value)=this[i,j,k] <== complex0(value,this.Context)
        override _.clear()=this.AssignScalar(complex0(Int 0))
        override _.sizeinit()=this.size1<== -1;this.size2<== -1;this.size3<== -1
        static member (<==)(x:complex3,y:complex3)=x.AssignArray y
        static member (<==)(x:complex3,y:complex0)=x.AssignScalar y
        static member (<==)(x:complex3,y:double0)=x.AssignScalar(complex0 y.Expr)
        static member (<==)(x:complex3,y:int0)=x.AssignScalar(complex0 y.Expr)
        static member (<==)(x:complex3,y:double)=x.AssignScalar(complex0(Dbl y))
        static member (<==)(x:complex3,y:int)=x.AssignScalar(complex0(Int y))

    [<AutoOpen>]
    module asm_complex3=
        type asm with
            static member pow(x:complex3,y:int0)=complex3(x.etype%%y.etype,Arx3(x.size1,x.size2,x.size3,fun(i,j,k)->asm.pow(x[i,j,k],y).Expr))
            static member sin(x:complex3)=complex3(x.etype,Arx3(x.size1,x.size2,x.size3,fun(i,j,k)->asm.sin(x[i,j,k]).Expr))
            static member cos(x:complex3)=complex3(x.etype,Arx3(x.size1,x.size2,x.size3,fun(i,j,k)->asm.cos(x[i,j,k]).Expr))
            static member tan(x:complex3)=complex3(x.etype,Arx3(x.size1,x.size2,x.size3,fun(i,j,k)->asm.tan(x[i,j,k]).Expr))
            static member asin(x:complex3)=complex3(x.etype,Arx3(x.size1,x.size2,x.size3,fun(i,j,k)->asm.asin(x[i,j,k]).Expr))
            static member acos(x:complex3)=complex3(x.etype,Arx3(x.size1,x.size2,x.size3,fun(i,j,k)->asm.acos(x[i,j,k]).Expr))
            static member atan(x:complex3)=complex3(Dt,Arx3(x.size1,x.size2,x.size3,fun(i,j,k)->asm.atan(x[i,j,k]).Expr))
            static member exp(x:complex3)=complex3(x.etype,Arx3(x.size1,x.size2,x.size3,fun(i,j,k)->asm.exp(x[i,j,k]).Expr))
            static member abs(x:complex3)=complex3(x.etype,Arx3(x.size1,x.size2,x.size3,fun(i,j,k)->asm.abs(x[i,j,k]).Expr))
            static member log(x:complex3)=complex3(x.etype,Arx3(x.size1,x.size2,x.size3,fun(i,j,k)->asm.log(x[i,j,k]).Expr))
            static member log10(x:complex3)=complex3(x.etype,Arx3(x.size1,x.size2,x.size3,fun(i,j,k)->asm.log10(x[i,j,k]).Expr))
            static member sqrt(x:complex3)=complex3(x.etype,Arx3(x.size1,x.size2,x.size3,fun(i,j,k)->asm.sqrt(x[i,j,k]).Expr))
            static member conj(x:complex3)=complex3(x.etype,Arx3(x.size1,x.size2,x.size3,fun(i,j,k)->asm.conj(x[i,j,k]).Expr))

    [<AutoOpen>]
    module Real3Extensions =
        type IReal3 with
            /// Views this real three-dimensional expression array as a double-precision array.
            member this.ToDouble3 = double3(this.Etype, this.Expr, this.Context)

    [<AutoOpen>]
    module Num3Extensions =
        type INum3 with
            /// Views this three-dimensional numeric expression array as a complex array.
            member this.ToComplex3 = complex3(this.Etype, this.Expr, this.Context)
