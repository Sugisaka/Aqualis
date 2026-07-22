//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    /// One-dimensional real expression array.
    type double1 (typ:Etype,x:Expr1, ?context:GenerationContext) as this =
        inherit NumericArray1<double0,double1>(typ,x,?context=context)

        new (context:GenerationContext,typ,size,name,para) =
            context.CurrentProgram.var.setVar(typ,size,name,para)
            double1(typ,Var1(size,name),context=context)
        new(a:int0,f:int0->double0) = double1(Dt,Arx1(a,fun i -> (f i).Expr))
        new(a:int ,f:int0->double0) = double1(Dt,Arx1(I a,fun i -> (f i).Expr))

        override _.WrapScalar(value,resultContext) = double0(value,?context=resultContext)
        override _.Create(elementType,value,resultContext) = double1(elementType,value,?context=resultContext)
        override _.AssignAt(index,value) = this[index] <== double0(value,?context=this.Context)
        override _.clear() = this.AssignScalar(D 0.0)
        override _.sizeinit() = this.size1 <== -1

        static member (<==) (x:double1,y:double1) = x.AssignArray y
        static member (<==) (x:double1,y:double0) = x.AssignScalar y
        static member (<==) (x:double1,y:int0) = x.AssignScalar(double0 y.Expr)
        static member (<==) (x:double1,y:double) = x.AssignScalar(D y)
        static member (<==) (x:double1,y:int) = x.AssignScalar(I y)

    [<AutoOpen>]
    module asm_double1 =
        type asm with
            static member pow(x:double1,y:int0) = double1(x.etype%%y.etype,Arx1(x.size1,fun i -> asm.pow(x[i],y).Expr))
            static member pow(x:double1,y:double0) = double1(x.etype%%y.etype,Arx1(x.size1,fun i -> asm.pow(x[i],y).Expr))
            static member pow(x:double1,y:complex0) = double1(x.etype%%y.etype,Arx1(x.size1,fun i -> asm.pow(x[i],y).Expr))
            static member pow(x:double1,y:int) = asm.pow(x,I y)
            static member pow(x:double1,y:double) = asm.pow(x,D y)
            static member sin(x:double1) = double1(x.etype,Arx1(x.size1,fun i -> asm.sin(x[i]).Expr))
            static member cos(x:double1) = double1(x.etype,Arx1(x.size1,fun i -> asm.cos(x[i]).Expr))
            static member tan(x:double1) = double1(x.etype,Arx1(x.size1,fun i -> asm.tan(x[i]).Expr))
            static member asin(x:double1) = double1(x.etype,Arx1(x.size1,fun i -> asm.asin(x[i]).Expr))
            static member acos(x:double1) = double1(x.etype,Arx1(x.size1,fun i -> asm.acos(x[i]).Expr))
            static member atan(x:double1) = double1(x.etype,Arx1(x.size1,fun i -> asm.atan(x[i]).Expr))
            static member atan2(x:double1,y:double1) = double1(Dt,Arx1(x.size1,fun i -> asm.atan2(x[i],y[i]).Expr))
            static member exp(x:double1) = double1(x.etype,Arx1(x.size1,fun i -> asm.exp(x[i]).Expr))
            static member abs(x:double1) = double1(x.etype,Arx1(x.size1,fun i -> asm.abs(x[i]).Expr))
            static member log(x:double1) = double1(x.etype,Arx1(x.size1,fun i -> asm.log(x[i]).Expr))
            static member log10(x:double1) = double1(x.etype,Arx1(x.size1,fun i -> asm.log10(x[i]).Expr))
            static member sqrt(x:double1) = double1(x.etype,Arx1(x.size1,fun i -> asm.sqrt(x[i]).Expr))
            static member floor(x:double1) = double1(x.etype,Arx1(x.size1,fun i -> asm.floor(x[i]).Expr))
            static member ceil(x:double1) = double1(x.etype,Arx1(x.size1,fun i -> asm.ceil(x[i]).Expr))
