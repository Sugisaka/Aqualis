namespace Aqualis

    type double3(typ:Etype,x:Expr3,context:Aqualis) as this=
        inherit NumericArray3<double0,double1,double2,double3>(typ,x,context)
        interface IReal3
        new(typ,x) = double3(typ,x,Aqualis.BlankWriter Numeric)
        new(context:Aqualis,typ,size,name,para)=
            context.cvar.setVar(typ,size,name,para)
            double3(typ,Var3(size,name),context=context)
        new(a:int0,b:int0,c:int0,f:int0*int0*int0->double0) = double3(Dt,Arx3(a,b,c,fun ijk->(f ijk).Expr))
        new(a:int ,b:int0,c:int0,f:int0*int0*int0->double0) = double3(Dt,Arx3(I a,b,c,fun ijk->(f ijk).Expr))
        new(a:int0,b:int ,c:int0,f:int0*int0*int0->double0) = double3(Dt,Arx3(a,I b,c,fun ijk->(f ijk).Expr))
        new(a:int0,b:int0,c:int ,f:int0*int0*int0->double0) = double3(Dt,Arx3(a,b,I c,fun ijk->(f ijk).Expr))
        new(a:int0,b:int ,c:int ,f:int0*int0*int0->double0) = double3(Dt,Arx3(a,I b,I c,fun ijk->(f ijk).Expr))
        new(a:int ,b:int0,c:int ,f:int0*int0*int0->double0) = double3(Dt,Arx3(I a,b,I c,fun ijk->(f ijk).Expr))
        new(a:int ,b:int ,c:int0,f:int0*int0*int0->double0) = double3(Dt,Arx3(I a,I b,c,fun ijk->(f ijk).Expr))
        new(a:int ,b:int ,c:int ,f:int0*int0*int0->double0) = double3(Dt,Arx3(I a,I b,I c,fun ijk->(f ijk).Expr))
        override _.WrapScalar value=double0(value,this.Context)
        override _.WrapRow value=double1(typ,value,this.Context)
        override _.WrapMatrix value=double2(typ,value,this.Context)
        override _.CreateWithContext(elementType,value,resultContext)=double3(elementType,value,resultContext)
        override _.AssignAt(i,j,k,value)=this[i,j,k] <== double0(value,this.Context)
        override _.clear()=this.AssignScalar(D 0.0)
        override _.sizeinit()=this.size1<== -1;this.size2<== -1;this.size3<== -1
        static member (<==)(x:double3,y:double3)=x.AssignArray y
        static member (<==)(x:double3,y:double0)=x.AssignScalar y
        static member (<==)(x:double3,y:int0)=x.AssignScalar(double0 y.Expr)
        static member (<==)(x:double3,y:double)=x.AssignScalar(D y)
        static member (<==)(x:double3,y:int)=x.AssignScalar(I y)

    [<AutoOpen>]
    module asm_double3=
        type asm with
            static member pow(x:double3,y:int0)=double3(x.etype%%y.etype,Arx3(x.size1,x.size2,x.size3,fun(i,j,k)->asm.pow(x[i,j,k],y).Expr))
            static member sin(x:double3)=double3(x.etype,Arx3(x.size1,x.size2,x.size3,fun(i,j,k)->asm.sin(x[i,j,k]).Expr))
            static member cos(x:double3)=double3(x.etype,Arx3(x.size1,x.size2,x.size3,fun(i,j,k)->asm.cos(x[i,j,k]).Expr))
            static member tan(x:double3)=double3(x.etype,Arx3(x.size1,x.size2,x.size3,fun(i,j,k)->asm.tan(x[i,j,k]).Expr))
            static member asin(x:double3)=double3(x.etype,Arx3(x.size1,x.size2,x.size3,fun(i,j,k)->asm.asin(x[i,j,k]).Expr))
            static member acos(x:double3)=double3(x.etype,Arx3(x.size1,x.size2,x.size3,fun(i,j,k)->asm.acos(x[i,j,k]).Expr))
            static member atan(x:double3)=double3(Dt,Arx3(x.size1,x.size2,x.size3,fun(i,j,k)->asm.atan(x[i,j,k]).Expr))
            static member atan2(x:double3,y:double3)=double3(Dt,Arx3(x.size1,x.size2,x.size3,fun(i,j,k)->asm.atan2(x[i,j,k],y[i,j,k]).Expr))
            static member exp(x:double3)=double3(x.etype,Arx3(x.size1,x.size2,x.size3,fun(i,j,k)->asm.exp(x[i,j,k]).Expr))
            static member abs(x:double3)=double3(x.etype,Arx3(x.size1,x.size2,x.size3,fun(i,j,k)->asm.abs(x[i,j,k]).Expr))
            static member log(x:double3)=double3(x.etype,Arx3(x.size1,x.size2,x.size3,fun(i,j,k)->asm.log(x[i,j,k]).Expr))
            static member log10(x:double3)=double3(x.etype,Arx3(x.size1,x.size2,x.size3,fun(i,j,k)->asm.log10(x[i,j,k]).Expr))
            static member sqrt(x:double3)=double3(x.etype,Arx3(x.size1,x.size2,x.size3,fun(i,j,k)->asm.sqrt(x[i,j,k]).Expr))
            static member floor(x:double3)=double3(x.etype,Arx3(x.size1,x.size2,x.size3,fun(i,j,k)->asm.floor(x[i,j,k]).Expr))
            static member ceil(x:double3)=double3(x.etype,Arx3(x.size1,x.size2,x.size3,fun(i,j,k)->asm.ceil(x[i,j,k]).Expr))
