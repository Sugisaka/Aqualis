namespace Aqualis

module internal TemporaryVariableScope =
    let useOne acquire createValue code =
        let name,release = acquire()
        try code (createValue name)
        finally release()

    let useMany count acquire createValue code =
        if count < 0 then invalidArg (nameof count) "The variable count cannot be negative."
        let rec acquireAll remaining acquired =
            if remaining = 0 then List.rev acquired
            else
                try
                    let item = acquire()
                    acquireAll (remaining - 1) (item::acquired)
                with _ ->
                    acquired |> List.iter (snd >> fun release -> release())
                    reraise()
        let acquired = acquireAll count []
        try acquired |> List.map (fst >> createValue) |> code
        finally acquired |> List.rev |> List.iter (snd >> fun release -> release())

type ContextCh internal (environment:CompilationEnvironment) =
    let context() = environment.RequireGenerationContext()

    member internal _.Environment = environment

    member _.i code =
        let ctx = context()
        TemporaryVariableScope.useOne ctx.CurrentProgram.i0.getVar
            (fun name -> int0(Var(It 4,name,NaN), context=ctx)) code

    member _.d code =
        let ctx = context()
        TemporaryVariableScope.useOne ctx.CurrentProgram.d0.getVar
            (fun name -> double0(Var(Dt,name,NaN), context=ctx)) code

    member _.z code =
        let ctx = context()
        TemporaryVariableScope.useOne ctx.CurrentProgram.z0.getVar
            (fun name -> complex0(Var(Zt,name,NaN), context=ctx)) code

    member _.I name code =
        let ctx = context()
        TemporaryVariableScope.useOne
            (fun () -> ctx.CurrentProgram.i0.getVar(ctx.CurrentProgram,name,It 4,A0))
            (fun variableName -> int0(Var(It 4,variableName,NaN), context=ctx)) code

    member _.D name code =
        let ctx = context()
        TemporaryVariableScope.useOne
            (fun () -> ctx.CurrentProgram.d0.getVar(ctx.CurrentProgram,name,Dt,A0))
            (fun variableName -> double0(Var(Dt,variableName,NaN), context=ctx)) code

    member _.Z name code =
        let ctx = context()
        TemporaryVariableScope.useOne
            (fun () -> ctx.CurrentProgram.z0.getVar(ctx.CurrentProgram,name,Zt,A0))
            (fun variableName -> complex0(Var(Zt,variableName,NaN), context=ctx)) code

    member _.ix count code =
        let ctx = context()
        TemporaryVariableScope.useMany count ctx.CurrentProgram.i0.getVar
            (fun name -> int0(Var(It 4,name,NaN), context=ctx)) code

    member _.dx count code =
        let ctx = context()
        TemporaryVariableScope.useMany count ctx.CurrentProgram.d0.getVar
            (fun name -> double0(Var(Dt,name,NaN), context=ctx)) code

    member _.zx count code =
        let ctx = context()
        TemporaryVariableScope.useMany count ctx.CurrentProgram.z0.getVar
            (fun name -> complex0(Var(Zt,name,NaN), context=ctx)) code

    member _.dd code =
        let ctx = context()
        TemporaryVariableScope.useMany 2 ctx.CurrentProgram.d0.getVar
            (fun name -> double0(Var(Dt,name,NaN), context=ctx))
            (function |[left;right] -> code(left,right) |_ -> failwith "unreachable")

    member _.dddd code =
        let ctx = context()
        TemporaryVariableScope.useMany 4 ctx.CurrentProgram.d0.getVar
            (fun name -> double0(Var(Dt,name,NaN), context=ctx))
            (function |[a;b;c;d] -> code(a,b,c,d) |_ -> failwith "unreachable")

    member _.ddd code =
        let ctx = context()
        TemporaryVariableScope.useMany 3 ctx.CurrentProgram.d0.getVar
            (fun name -> double0(Var(Dt,name,NaN), context=ctx))
            (function |[a;b;c] -> code(a,b,c) |_ -> failwith "unreachable")

    member _.f code =
        let ctx = context()
        let name,counter,release = ctx.CurrentProgram.f0.getVarAndCounter()
        try
            let initial = if ctx.CurrentProgram.language = Fortran then ctx.CurrentProgram.numFormat.ItoS(counter + 10) else ""
            ctx.CurrentProgram.var.setVar(Structure "file",A0,name,initial)
            code name
        finally release()

    member _.t variableType code =
        let ctx = context()
        TemporaryVariableScope.useOne ctx.CurrentProgram.t0.getVar
            (fun name ->
                ctx.CurrentProgram.var.setVar(Structure "string",variableType,name,"")
                name) code

    member this.ii code = this.i (fun left -> this.i (fun right -> code(left,right)))
    member this.iii code = this.i (fun first -> this.i (fun second -> this.i (fun third -> code(first,second,third))))
    member this.iiz code = this.i (fun first -> this.i (fun second -> this.z (fun third -> code(first,second,third))))
    member this.id code = this.i (fun first -> this.d (fun second -> code(first,second)))
    member this.idd code = this.i (fun first -> this.d (fun second -> this.d (fun third -> code(first,second,third))))
    member this.zz code = this.z (fun first -> this.z (fun second -> code(first,second)))
    member _.zzzz code =
        let ctx = context()
        TemporaryVariableScope.useMany 4 ctx.CurrentProgram.z0.getVar
            (fun name -> complex0(Var(Zt,name,NaN), context=ctx))
            (function |[a;b;c;d] -> code(a,b,c,d) |_ -> failwith "unreachable")

[<AutoOpen>]
module CompilationEnvironmentChExtensions =
    type CompilationEnvironment with
        member this.ch = ContextCh(this)
