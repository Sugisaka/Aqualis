//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    /// One-dimensional integer expression array.
    type int1 (typ:Etype,x:Expr1, context:Aqualis) as this =
        inherit NumericArray1<int0,int1>(typ,x,context)

        interface IReal1

        new (typ,x) = int1(typ,x,Aqualis.BlankWriter Numeric)
        new (context:Aqualis,typ,size,name,para) =
            context.cvar.setVar(typ,size,name,para)
            int1(typ,Var1(size,name),context=context)
        new(a:int0,f:int0->int0) = int1(It 4,Arx1(a,fun i -> (f i).Expr))
        new(a:int ,f:int0->int0) = int1(It 4,Arx1(I a,fun i -> (f i).Expr))
        override _.WrapScalar(value,resultContext) = int0(value,resultContext)
        override _.Create(elementType,value,resultContext) = int1(elementType,value,resultContext)
        override _.AssignAt(index,value) = this[index] <== int0(value,this.Context)
        override _.clear() = this.AssignScalar(I 0)
        override _.sizeinit() = this.size1 <== -1

        static member (./) (x:int1,y:int1) =
            base1.sizeMismatchError(x,y)
            Arx1(x.size1,fun i -> Div(It 4,x[i].Expr,y[i].Expr))
        static member (./) (x:int0,y:int1) = int1(x.etype%%y.etype,Arx1(y.size1,fun i -> Div(It 4,x.Expr,y[i].Expr)))
        static member (./) (x:int,y:int1) = I x ./ y
        static member (./) (x:int1,y:int0) = int1(x.etype%%y.etype,Arx1(x.size1,fun i -> Div(It 4,x[i].Expr,y.Expr)))
        static member (./) (x:int1,y:int) = x ./ I y

        static member (<==) (x:int1,y:int1) = x.AssignArray y
        static member (<==) (x:int1,y:int0) = x.AssignScalar y
        static member (<==) (x:int1,y:int) = x.AssignScalar(I y)

    [<AutoOpen>]
    module asm_int1 =
        type asm with
            static member pow(x:int1,y:int0) = double1(Dt,Arx1(x.size1,fun i -> asm.pow(x[i],y).Expr))
            static member pow(x:int1,y:double0) = double1(Dt,Arx1(x.size1,fun i -> asm.pow(x[i],y).Expr))
            static member pow(x:int1,y:complex0) = complex1(Zt,Arx1(x.size1,fun i -> asm.pow(x[i],y).Expr))
            static member pow(x:int1,y:int) = asm.pow(x,I y)
            static member pow(x:int1,y:double) = asm.pow(x,D y)
            static member sin(x:int1) = double1(Dt,Arx1(x.size1,fun i -> asm.sin(x[i]).Expr))
            static member cos(x:int1) = double1(Dt,Arx1(x.size1,fun i -> asm.cos(x[i]).Expr))
            static member tan(x:int1) = double1(Dt,Arx1(x.size1,fun i -> asm.tan(x[i]).Expr))
            static member asin(x:int1) = double1(Dt,Arx1(x.size1,fun i -> asm.asin(x[i]).Expr))
            static member acos(x:int1) = double1(Dt,Arx1(x.size1,fun i -> asm.acos(x[i]).Expr))
            static member atan(x:int1) = double1(Dt,Arx1(x.size1,fun i -> asm.atan(x[i]).Expr))
            static member atan2(x:int1,y:int1) = double1(Dt,Arx1(x.size1,fun i -> asm.atan2(x[i],y[i]).Expr))
            static member exp(x:int1) = double1(Dt,Arx1(x.size1,fun i -> asm.exp(x[i]).Expr))
            static member abs(x:int1) = double1(Dt,Arx1(x.size1,fun i -> asm.abs(x[i]).Expr))
            static member log(x:int1) = double1(Dt,Arx1(x.size1,fun i -> asm.log(x[i]).Expr))
            static member log10(x:int1) = double1(Dt,Arx1(x.size1,fun i -> asm.log10(x[i]).Expr))
            static member sqrt(x:int1) = double1(Dt,Arx1(x.size1,fun i -> asm.sqrt(x[i]).Expr))
