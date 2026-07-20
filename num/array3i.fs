namespace Aqualis

    type int3(typ:Etype,x:Expr3,?context:GenerationContext) as this=
        inherit NumericArray3<int0,int1,int3>(typ,x,?context=context)
        new(typ,size,name,para)=
            (GenerationScope.currentProgram()).var.setVar(typ,size,name,para)
            int3(typ,Var3(size,name),?context=GenerationContext.TryCurrent)
        new(a:int0,b:int0,c:int0,f:int0*int0*int0->int0) = int3(It 4,Arx3(a,b,c,fun ijk->(f ijk).Expr))
        new(a:int ,b:int0,c:int0,f:int0*int0*int0->int0) = int3(It 4,Arx3(I a,b,c,fun ijk->(f ijk).Expr))
        new(a:int0,b:int ,c:int0,f:int0*int0*int0->int0) = int3(It 4,Arx3(a,I b,c,fun ijk->(f ijk).Expr))
        new(a:int0,b:int0,c:int ,f:int0*int0*int0->int0) = int3(It 4,Arx3(a,b,I c,fun ijk->(f ijk).Expr))
        new(a:int0,b:int ,c:int ,f:int0*int0*int0->int0) = int3(It 4,Arx3(a,I b,I c,fun ijk->(f ijk).Expr))
        new(a:int ,b:int0,c:int ,f:int0*int0*int0->int0) = int3(It 4,Arx3(I a,b,I c,fun ijk->(f ijk).Expr))
        new(a:int ,b:int ,c:int0,f:int0*int0*int0->int0) = int3(It 4,Arx3(I a,I b,c,fun ijk->(f ijk).Expr))
        new(a:int ,b:int ,c:int ,f:int0*int0*int0->int0) = int3(It 4,Arx3(I a,I b,I c,fun ijk->(f ijk).Expr))
        override _.WrapScalar value=int0 value
        override _.WrapRow value=int1(typ,value)
        override _.Create(elementType,value)=int3(elementType,value)
        override _.AssignAt(i,j,k,value)=this[i,j,k] <== int0 value
        override _.clear()=this.AssignScalar(I 0)
        override _.sizeinit()=this.size1<== -1;this.size2<== -1;this.size3<== -1
        static member (./)(x:int3,y:int3)=base3.sizeMismatchError(x,y);int3(x.etype%%y.etype,Arx3(x.size1,x.size2,x.size3,fun(i,j,k)->Div(It 4,x[i,j,k].Expr,y[i,j,k].Expr)))
        static member (./)(x:int0,y:int3)=int3(x.etype%%y.etype,Arx3(y.size1,y.size2,y.size3,fun(i,j,k)->Div(It 4,x.Expr,y[i,j,k].Expr)))
        static member (./)(x:int,y:int3)=I x ./ y
        static member (./)(x:int3,y:int0)=int3(x.etype%%y.etype,Arx3(x.size1,x.size2,x.size3,fun(i,j,k)->Div(It 4,x[i,j,k].Expr,y.Expr)))
        static member (./)(x:int3,y:int)=x ./ I y
        static member (<==)(x:int3,y:int3)=x.AssignArray y
        static member (<==)(x:int3,y:int0)=x.AssignScalar y
        static member (<==)(x:int3,y:int)=x.AssignScalar(I y)

    [<AutoOpen>]
    module asm_int3=
        type asm with
            static member pow(x:int3,y:int0)=int3(x.etype%%y.etype,Arx3(x.size1,x.size2,x.size3,fun(i,j,k)->asm.pow(x[i,j,k],y).Expr))
            static member sin(x:int3)=int3(x.etype,Arx3(x.size1,x.size2,x.size3,fun(i,j,k)->asm.sin(x[i,j,k]).Expr))
            static member cos(x:int3)=int3(x.etype,Arx3(x.size1,x.size2,x.size3,fun(i,j,k)->asm.cos(x[i,j,k]).Expr))
            static member tan(x:int3)=int3(x.etype,Arx3(x.size1,x.size2,x.size3,fun(i,j,k)->asm.tan(x[i,j,k]).Expr))
            static member asin(x:int3)=int3(x.etype,Arx3(x.size1,x.size2,x.size3,fun(i,j,k)->asm.asin(x[i,j,k]).Expr))
            static member acos(x:int3)=int3(x.etype,Arx3(x.size1,x.size2,x.size3,fun(i,j,k)->asm.acos(x[i,j,k]).Expr))
            static member atan(x:int3)=int3(Dt,Arx3(x.size1,x.size2,x.size3,fun(i,j,k)->asm.atan(x[i,j,k]).Expr))
            static member atan2(x:int3,y:int3)=int3(Dt,Arx3(x.size1,x.size2,x.size3,fun(i,j,k)->asm.atan2(x[i,j,k],y[i,j,k]).Expr))
            static member exp(x:int3)=int3(x.etype,Arx3(x.size1,x.size2,x.size3,fun(i,j,k)->asm.exp(x[i,j,k]).Expr))
            static member abs(x:int3)=int3(x.etype,Arx3(x.size1,x.size2,x.size3,fun(i,j,k)->asm.abs(x[i,j,k]).Expr))
            static member log(x:int3)=int3(x.etype,Arx3(x.size1,x.size2,x.size3,fun(i,j,k)->asm.log(x[i,j,k]).Expr))
            static member log10(x:int3)=int3(x.etype,Arx3(x.size1,x.size2,x.size3,fun(i,j,k)->asm.log10(x[i,j,k]).Expr))
            static member sqrt(x:int3)=int3(x.etype,Arx3(x.size1,x.size2,x.size3,fun(i,j,k)->asm.sqrt(x[i,j,k]).Expr))
