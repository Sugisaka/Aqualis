namespace Aqualis

    type double2 (typ:Etype,x:Expr2,context:Aqualis) as this =
        inherit NumericArray2<double0,double1,double2>(typ,x,context)
        new(typ,x) = double2(typ,x,Aqualis.BlankWriter Numeric)
        new(context:Aqualis,typ,size,name,para)=
            context.cvar.setVar(typ,size,name,para)
            double2(typ,Var2(size,name),context=context)
        new(a:int0,b:int0,f:int0*int0->double0) = double2(Dt,Arx2(a, b, fun ij -> (f ij).Expr))
        new(a:int ,b:int0,f:int0*int0->double0) = double2(Dt,Arx2(I a, b, fun ij -> (f ij).Expr))
        new(a:int0,b:int ,f:int0*int0->double0) = double2(Dt,Arx2(a, I b, fun ij -> (f ij).Expr))
        new(a:int ,b:int ,f:int0*int0->double0) = double2(Dt,Arx2(I a, I b, fun ij -> (f ij).Expr))
        override _.WrapScalar value=double0(value,this.Context)
        override _.WrapRow value=double1(typ,value,this.Context)
        override _.CreateWithContext(elementType,value,resultContext)=double2(elementType,value,resultContext)
        override _.AssignAt(i,j,value)=this[i,j] <== double0(value,this.Context)
        override _.clear()=this.AssignScalar(D 0.0)
        override _.sizeinit()=this.size1<== -1;this.size2<== -1
        static member (<==)(x:double2,y:double2)=x.AssignArray y
        static member (<==)(x:double2,y:double0)=x.AssignScalar y
        static member (<==)(x:double2,y:int0)=x.AssignScalar(double0 y.Expr)
        static member (<==)(x:double2,y:double)=x.AssignScalar(D y)
        static member (<==)(x:double2,y:int)=x.AssignScalar(I y)

    [<AutoOpen>]
    module asm_double2 =
        type asm with
            static member pow(x:double2,y:int0)=double2(x.etype%%y.etype,Arx2(x.size1,x.size2,fun(i,j)->asm.pow(x[i,j],y).Expr))
            static member sin(x:double2)=double2(x.etype,Arx2(x.size1,x.size2,fun(i,j)->asm.sin(x[i,j]).Expr))
            static member cos(x:double2)=double2(x.etype,Arx2(x.size1,x.size2,fun(i,j)->asm.cos(x[i,j]).Expr))
            static member tan(x:double2)=double2(x.etype,Arx2(x.size1,x.size2,fun(i,j)->asm.tan(x[i,j]).Expr))
            static member asin(x:double2)=double2(x.etype,Arx2(x.size1,x.size2,fun(i,j)->asm.asin(x[i,j]).Expr))
            static member acos(x:double2)=double2(x.etype,Arx2(x.size1,x.size2,fun(i,j)->asm.acos(x[i,j]).Expr))
            static member atan(x:double2)=double2(x.etype,Arx2(x.size1,x.size2,fun(i,j)->asm.atan(x[i,j]).Expr))
            static member atan2(x:double2,y:double2)=double2(Dt,Arx2(x.size1,x.size2,fun(i,j)->asm.atan2(x[i,j],y[i,j]).Expr))
            static member exp(x:double2)=double2(x.etype,Arx2(x.size1,x.size2,fun(i,j)->asm.exp(x[i,j]).Expr))
            static member abs(x:double2)=double2(x.etype,Arx2(x.size1,x.size2,fun(i,j)->asm.abs(x[i,j]).Expr))
            static member log(x:double2)=double2(x.etype,Arx2(x.size1,x.size2,fun(i,j)->asm.log(x[i,j]).Expr))
            static member log10(x:double2)=double2(x.etype,Arx2(x.size1,x.size2,fun(i,j)->asm.log10(x[i,j]).Expr))
            static member sqrt(x:double2)=double2(x.etype,Arx2(x.size1,x.size2,fun(i,j)->asm.sqrt(x[i,j]).Expr))
            static member floor(x:double2)=double2(x.etype,Arx2(x.size1,x.size2,fun(i,j)->asm.floor(x[i,j]).Expr))
            static member ceil(x:double2)=double2(x.etype,Arx2(x.size1,x.size2,fun(i,j)->asm.ceil(x[i,j]).Expr))
