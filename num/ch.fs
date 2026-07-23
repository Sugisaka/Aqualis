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

    member this.ii code = this.i (fun first -> this.i (fun second -> code(first,second)))
    member this.id code = this.i (fun first -> this.d (fun second -> code(first,second)))
    member this.iz code = this.i (fun first -> this.z (fun second -> code(first,second)))
    member this.dd code = this.d (fun first -> this.d (fun second -> code(first,second)))
    member this.dz code = this.d (fun first -> this.z (fun second -> code(first,second)))
    member this.zz code = this.z (fun first -> this.z (fun second -> code(first,second)))
    member this.iii code = this.i (fun first -> this.i (fun second -> this.i (fun third -> code(first,second,third))))
    member this.iid code = this.i (fun first -> this.i (fun second -> this.d (fun third -> code(first,second,third))))
    member this.iiz code = this.i (fun first -> this.i (fun second -> this.z (fun third -> code(first,second,third))))
    member this.idd code = this.i (fun first -> this.d (fun second -> this.d (fun third -> code(first,second,third))))
    member this.idz code = this.i (fun first -> this.d (fun second -> this.z (fun third -> code(first,second,third))))
    member this.izz code = this.i (fun first -> this.z (fun second -> this.z (fun third -> code(first,second,third))))
    member this.ddd code = this.d (fun first -> this.d (fun second -> this.d (fun third -> code(first,second,third))))
    member this.ddz code = this.d (fun first -> this.d (fun second -> this.z (fun third -> code(first,second,third))))
    member this.dzz code = this.d (fun first -> this.z (fun second -> this.z (fun third -> code(first,second,third))))
    member this.zzz code = this.z (fun first -> this.z (fun second -> this.z (fun third -> code(first,second,third))))
    member this.iiii code = this.i (fun first -> this.i (fun second -> this.i (fun third -> this.i (fun fourth -> code(first,second,third,fourth)))))
    member this.iiid code = this.i (fun first -> this.i (fun second -> this.i (fun third -> this.d (fun fourth -> code(first,second,third,fourth)))))
    member this.iiiz code = this.i (fun first -> this.i (fun second -> this.i (fun third -> this.z (fun fourth -> code(first,second,third,fourth)))))
    member this.iidd code = this.i (fun first -> this.i (fun second -> this.d (fun third -> this.d (fun fourth -> code(first,second,third,fourth)))))
    member this.iidz code = this.i (fun first -> this.i (fun second -> this.d (fun third -> this.z (fun fourth -> code(first,second,third,fourth)))))
    member this.iizz code = this.i (fun first -> this.i (fun second -> this.z (fun third -> this.z (fun fourth -> code(first,second,third,fourth)))))
    member this.iddd code = this.i (fun first -> this.d (fun second -> this.d (fun third -> this.d (fun fourth -> code(first,second,third,fourth)))))
    member this.iddz code = this.i (fun first -> this.d (fun second -> this.d (fun third -> this.z (fun fourth -> code(first,second,third,fourth)))))
    member this.idzz code = this.i (fun first -> this.d (fun second -> this.z (fun third -> this.z (fun fourth -> code(first,second,third,fourth)))))
    member this.izzz code = this.i (fun first -> this.z (fun second -> this.z (fun third -> this.z (fun fourth -> code(first,second,third,fourth)))))
    member this.dddd code = this.d (fun first -> this.d (fun second -> this.d (fun third -> this.d (fun fourth -> code(first,second,third,fourth)))))
    member this.dddz code = this.d (fun first -> this.d (fun second -> this.d (fun third -> this.z (fun fourth -> code(first,second,third,fourth)))))
    member this.ddzz code = this.d (fun first -> this.d (fun second -> this.z (fun third -> this.z (fun fourth -> code(first,second,third,fourth)))))
    member this.dzzz code = this.d (fun first -> this.z (fun second -> this.z (fun third -> this.z (fun fourth -> code(first,second,third,fourth)))))
    member this.zzzz code = this.z (fun first -> this.z (fun second -> this.z (fun third -> this.z (fun fourth -> code(first,second,third,fourth)))))

[<AutoOpen>]
module CompilationEnvironmentChExtensions =
    type CompilationEnvironment with
        member this.ch = ContextCh(this)
