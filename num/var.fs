//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    open System

    ///<summary>変数宣言</summary>
    type ContextVar internal (environment:Aqualis) =
        let context() = environment.RequireGenerationContext()
        let nameFor (ctx:GenerationContext) name =
            match ctx.CurrentProgram.language with
            |PHP -> "$" + name
            |_ -> name

        member _.b0(name:string) =
            let ctx = context()
            let name = nameFor ctx name
            ctx.CurrentProgram.var.setUniqVarWarning(It 1,A0,name,"")
            int0(Var(It 1,name,NaN),context=ctx)

        member _.i0(name:string) =
            let ctx = context()
            let name = nameFor ctx name
            ctx.CurrentProgram.var.setUniqVarWarning(It 4,A0,name,"")
            int0(Var(It 4,name,NaN),context=ctx)

        member _.i0NoWarning(name:string,initial:int) =
            let ctx = context()
            let name = nameFor ctx name
            ctx.CurrentProgram.var.setUniqVar(It 4,A0,name,string initial)
            int0(Var(It 4,name,NaN),context=ctx)

        member _.d0(name:string) =
            let ctx = context()
            let name = nameFor ctx name
            ctx.CurrentProgram.var.setUniqVarWarning(Dt,A0,name,"")
            double0(Var(Dt,name,NaN),context=ctx)

        member _.z0(name:string) =
            let ctx = context()
            let name = nameFor ctx name
            ctx.CurrentProgram.var.setUniqVarWarning(Zt,A0,name,"")
            complex0(Var(Zt,name,NaN),context=ctx)
            
        member _.i1(name:string,size:int) =
            let ctx = context()
            let name = nameFor ctx name
            ctx.CurrentProgram.var.setUniqVarWarning(It 4,A1 size,name,"")
            int1(It 4,Var1(A1 size,name),context=ctx)

        member _.d1(name:string,size:int) =
            let ctx = context()
            let name = nameFor ctx name
            ctx.CurrentProgram.var.setUniqVarWarning(Dt,A1 size,name,"")
            double1(Dt,Var1(A1 size,name),context=ctx)

        member _.z1(name:string,size:int) =
            let ctx = context()
            let name = nameFor ctx name
            ctx.CurrentProgram.var.setUniqVarWarning(Zt,A1 size,name,"")
            complex1(Zt,Var1(A1 size,name),context=ctx)
            
        member _.i1(name:string) =
            let ctx = context()
            let name = nameFor ctx name
            int1(Dt,Var1(A1 0,name),context=ctx)
            
        member _.d1(name:string) =
            let ctx = context()
            let name = nameFor ctx name
            double1(Dt,Var1(A1 0,name),context=ctx)

        member _.z1(name:string) =
            let ctx = context()
            let name = nameFor ctx name
            complex1(Zt,Var1(A1 0,name),context=ctx)

        member _.ip1(name:string, values:int list) =
            let ctx = context()
            let name = nameFor ctx name
            let items = values |> List.map ctx.CurrentProgram.numFormat.ItoS |> String.concat ","
            let initial = if ctx.CurrentProgram.language = Fortran then "(/"+items+"/)" else "["+items+"]"
            ctx.CurrentProgram.var.setUniqVarWarning(It 4,A1 values.Length,name,initial)
            int1(It 4,Var1(A1 values.Length,name),context=ctx)
            
        member _.dp1(name:string, values:double list) =
            let ctx = context()
            let name = nameFor ctx name
            let items = values |> List.map ctx.CurrentProgram.numFormat.DtoS |> String.concat ","
            let initial = if ctx.CurrentProgram.language = Fortran then "(/"+items+"/)" else "["+items+"]"
            ctx.CurrentProgram.var.setUniqVarWarning(Dt,A1 values.Length,name,initial)
            double1(Dt,Var1(A1 values.Length,name),context=ctx)

        member _.zp1(name:string, values:(double*double) list) =
            let ctx = context()
            let name = nameFor ctx name
            let items = values |> List.map (fun (re,im) -> complex0(Cpx(re,im)).Expr.eval ctx.CurrentProgram) |> String.concat ","
            let initial = if ctx.CurrentProgram.language = Fortran then "(/"+items+"/)" else "["+items+"]"
            ctx.CurrentProgram.var.setUniqVarWarning(Zt,A1 values.Length,name,initial)
            complex1(Zt,Var1(A1 values.Length,name),context=ctx)

        member _.i2(name:string,size1:int,size2:int) =
            let ctx = context()
            let name = nameFor ctx name
            ctx.CurrentProgram.var.setUniqVarWarning(It 4,A2(size1,size2),name,"")
            int2(It 4,Var2(A2(size1,size2),name),context=ctx)

        member _.d2(name:string,size1:int,size2:int) =
            let ctx = context()
            let name = nameFor ctx name
            ctx.CurrentProgram.var.setUniqVarWarning(Dt,A2(size1,size2),name,"")
            double2(Dt,Var2(A2(size1,size2),name),context=ctx)

        member _.z2(name:string,size1:int,size2:int) =
            let ctx = context()
            let name = nameFor ctx name
            ctx.CurrentProgram.var.setUniqVarWarning(Zt,A2(size1,size2),name,"")
            complex2(Zt,Var2(A2(size1,size2),name),context=ctx)

        member _.i2(name:string) =
            let ctx = context()
            int2(It 4,Var2(A2(0,0),nameFor ctx name),context=ctx)
            
        member _.d2(name:string) =
            let ctx = context()
            double2(Dt,Var2(A2(0,0),nameFor ctx name),context=ctx)

        member _.z2(name:string) =
            let ctx = context()
            complex2(Zt,Var2(A2(0,0),nameFor ctx name),context=ctx)

        member _.i3(name:string,size1:int,size2:int,size3:int) =
            let ctx = context()
            let name = nameFor ctx name
            ctx.CurrentProgram.var.setUniqVarWarning(It 4,A3(size1,size2,size3),name,"")
            int3(It 4,Var3(A3(size1,size2,size3),name),context=ctx)

        member _.d3(name:string,size1:int,size2:int,size3:int) =
            let ctx = context()
            let name = nameFor ctx name
            ctx.CurrentProgram.var.setUniqVarWarning(Dt,A3(size1,size2,size3),name,"")
            double3(Dt,Var3(A3(size1,size2,size3),name),context=ctx)

        member _.z3(name:string,size1:int,size2:int,size3:int) =
            let ctx = context()
            let name = nameFor ctx name
            ctx.CurrentProgram.var.setUniqVarWarning(Zt,A3(size1,size2,size3),name,"")
            complex3(Zt,Var3(A3(size1,size2,size3),name),context=ctx)

        member _.i3(name:string) =
            let ctx = context()
            let name = nameFor ctx name
            ctx.CurrentProgram.var.setUniqVarWarning(It 4,A3(0,0,0),name,"")
            int3(It 4,Var3(A3(0,0,0),name),context=ctx)

        member _.d3(name:string) =
            let ctx = context()
            let name = nameFor ctx name
            ctx.CurrentProgram.var.setUniqVarWarning(Dt,A3(0,0,0),name,"")
            double3(Dt,Var3(A3(0,0,0),name),context=ctx)

        member _.z3(name:string) =
            let ctx = context()
            let name = nameFor ctx name
            ctx.CurrentProgram.var.setUniqVarWarning(Zt,A3(0,0,0),name,"")
            complex3(Zt,Var3(A3(0,0,0),name),context=ctx)
            
    [<AutoOpen>]
    module CompilationEnvironmentVarExtensions =
        type Aqualis with
            member this.var = ContextVar(this)
