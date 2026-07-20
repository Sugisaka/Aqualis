namespace Aqualis

    type int2 (typ:Etype,x:Expr2,?context:GenerationContext) as this =
        inherit NumericArray2<int0,int1,int2>(typ,x,?context=context)
        new(typ,size,name,para)=
            (GenerationScope.currentProgram()).var.setVar(typ,size,name,para)
            int2(typ,Var2(size,name),?context=GenerationContext.TryCurrent)
        override _.WrapScalar value=int0 value
        override _.WrapRow value=int1(typ,value)
        override _.Create(elementType,value)=int2(elementType,value)
        override _.AssignAt(i,j,value)=this[i,j] <== int0 value

        static member fiarray(a:int0,b:int0,f:int0*int0->int0)=int2(It 4,Arx2(a,b,fun ij->(f ij).Expr))
        static member fdarray(a:int0,b:int0,f:int0*int0->double0)=int2(Dt,Arx2(a,b,fun ij->(f ij).Expr))
        static member fzarray(a:int0,b:int0,f:int0*int0->complex0)=int2(Zt,Arx2(a,b,fun ij->(f ij).Expr))
        static member fiarray(a:int,b:int0,f)=int2.fiarray(I a,b,f)
        static member fdarray(a:int,b:int0,f)=int2.fdarray(I a,b,f)
        static member fzarray(a:int,b:int0,f)=int2.fzarray(I a,b,f)
        static member fiarray(a:int0,b:int,f)=int2.fiarray(a,I b,f)
        static member fdarray(a:int0,b:int,f)=int2.fdarray(a,I b,f)
        static member fzarray(a:int0,b:int,f)=int2.fzarray(a,I b,f)
        static member fiarray(a:int,b:int,f)=int2.fiarray(I a,I b,f)
        static member fdarray(a:int,b:int,f)=int2.fdarray(I a,I b,f)
        static member fzarray(a:int,b:int,f)=int2.fzarray(I a,I b,f)

        override _.clear()=this.AssignScalar(I 0)
        override _.sizeinit()=this.size1<== -1; this.size2<== -1
        static member (./)(x:int2,y:int2)=base2.sizeMismatchError(x,y);int2(x.etype%%y.etype,Arx2(x.size1,x.size2,fun(i,j)->Div(It 4,x[i,j].Expr,y[i,j].Expr)))
        static member (./)(x:int0,y:int2)=int2(x.etype%%y.etype,Arx2(y.size1,y.size2,fun(i,j)->Div(It 4,x.Expr,y[i,j].Expr)))
        static member (./)(x:int,y:int2)=I x ./ y
        static member (./)(x:int2,y:int0)=int2(x.etype%%y.etype,Arx2(x.size1,x.size2,fun(i,j)->Div(It 4,x[i,j].Expr,y.Expr)))
        static member (./)(x:int2,y:int)=x ./ I y
        static member (<==)(x:int2,y:int2)=x.AssignArray y
        static member (<==)(x:int2,y:int0)=x.AssignScalar y
        static member (<==)(x:int2,y:int)=x.AssignScalar(I y)

    [<AutoOpen>]
    module asm_int2 =
        type asm with
            static member pow(x:int2,y:int0)=int2(x.etype%%y.etype,Arx2(x.size1,x.size2,fun(i,j)->asm.pow(x[i,j],y).Expr))
            static member sin(x:int2)=int2(x.etype,Arx2(x.size1,x.size2,fun(i,j)->asm.sin(x[i,j]).Expr))
            static member cos(x:int2)=int2(x.etype,Arx2(x.size1,x.size2,fun(i,j)->asm.cos(x[i,j]).Expr))
            static member tan(x:int2)=int2(x.etype,Arx2(x.size1,x.size2,fun(i,j)->asm.tan(x[i,j]).Expr))
            static member asin(x:int2)=int2(x.etype,Arx2(x.size1,x.size2,fun(i,j)->asm.asin(x[i,j]).Expr))
            static member acos(x:int2)=int2(x.etype,Arx2(x.size1,x.size2,fun(i,j)->asm.acos(x[i,j]).Expr))
            static member atan(x:int2)=int2(x.etype,Arx2(x.size1,x.size2,fun(i,j)->asm.atan(x[i,j]).Expr))
            static member atan2(x:int2,y:int2)=int2(Dt,Arx2(x.size1,x.size2,fun(i,j)->asm.atan2(x[i,j],y[i,j]).Expr))
            static member exp(x:int2)=int2(x.etype,Arx2(x.size1,x.size2,fun(i,j)->asm.exp(x[i,j]).Expr))
            static member abs(x:int2)=int2(x.etype,Arx2(x.size1,x.size2,fun(i,j)->asm.abs(x[i,j]).Expr))
            static member log(x:int2)=int2(x.etype,Arx2(x.size1,x.size2,fun(i,j)->asm.log(x[i,j]).Expr))
            static member log10(x:int2)=int2(x.etype,Arx2(x.size1,x.size2,fun(i,j)->asm.log10(x[i,j]).Expr))
            static member sqrt(x:int2)=int2(x.etype,Arx2(x.size1,x.size2,fun(i,j)->asm.sqrt(x[i,j]).Expr))
