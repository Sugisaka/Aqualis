namespace Aqualis

    type double3(typ:Etype,x:Expr3,?context:GenerationContext) as this=
        inherit NumericArray3<double0,double1,double3>(typ,x,?context=context)
        new(typ,size,name,para)=
            (GenerationScope.currentProgram()).var.setVar(typ,size,name,para)
            double3(typ,Var3(size,name),?context=GenerationContext.TryCurrent)
        override _.WrapScalar value=double0 value
        override _.WrapRow value=double1(typ,value)
        override _.Create(elementType,value)=double3(elementType,value)
        override _.AssignAt(i,j,k,value)=this[i,j,k] <== double0 value

        static member fiarray(a:int0,b:int0,c:int0,f:int0*int0*int0->int0)=double3(It 4,Arx3(a,b,c,fun ijk->(f ijk).Expr))
        static member fdarray(a:int0,b:int0,c:int0,f:int0*int0*int0->int0)=double3(Dt,Arx3(a,b,c,fun ijk->(f ijk).Expr))
        static member fzarray(a:int0,b:int0,c:int0,f:int0*int0*int0->int0)=double3(Zt,Arx3(a,b,c,fun ijk->(f ijk).Expr))
        static member fiarray(a:int,b:int0,c:int0,f)=double3.fiarray(I a,b,c,f)
        static member fdarray(a:int,b:int0,c:int0,f)=double3.fdarray(I a,b,c,f)
        static member fzarray(a:int,b:int0,c:int0,f)=double3.fzarray(I a,b,c,f)
        static member fiarray(a:int0,b:int,c:int0,f)=double3.fiarray(a,I b,c,f)
        static member fdarray(a:int0,b:int,c:int0,f)=double3.fdarray(a,I b,c,f)
        static member fzarray(a:int0,b:int,c:int0,f)=double3.fzarray(a,I b,c,f)
        static member fiarray(a:int0,b:int0,c:int,f)=double3.fiarray(a,b,I c,f)
        static member fdarray(a:int0,b:int0,c:int,f)=double3.fdarray(a,b,I c,f)
        static member fzarray(a:int0,b:int0,c:int,f)=double3.fzarray(a,b,I c,f)
        static member fiarray(a:int,b:int,c:int0,f)=double3.fiarray(I a,I b,c,f)
        static member fdarray(a:int,b:int,c:int0,f)=double3.fdarray(I a,I b,c,f)
        static member fzarray(a:int,b:int,c:int0,f)=double3.fzarray(I a,I b,c,f)
        static member fiarray(a:int,b:int0,c:int,f)=double3.fiarray(I a,b,I c,f)
        static member fdarray(a:int,b:int0,c:int,f)=double3.fdarray(I a,b,I c,f)
        static member fzarray(a:int,b:int0,c:int,f)=double3.fzarray(I a,b,I c,f)
        static member fiarray(a:int0,b:int,c:int,f)=double3.fiarray(a,I b,I c,f)
        static member fdarray(a:int0,b:int,c:int,f)=double3.fdarray(a,I b,I c,f)
        static member fzarray(a:int0,b:int,c:int,f)=double3.fzarray(a,I b,I c,f)
        static member fiarray(a:int,b:int,c:int,f)=double3.fiarray(I a,I b,I c,f)
        static member fdarray(a:int,b:int,c:int,f)=double3.fdarray(I a,I b,I c,f)
        static member fzarray(a:int,b:int,c:int,f)=double3.fzarray(I a,I b,I c,f)

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
