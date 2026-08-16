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

type ContextCh internal (c:Aqualis) =

    member internal _.Environment = c

    member _.i code =
        TemporaryVariableScope.useOne c.i0.getVar
            (fun name -> int0(Var(It 4,name,NaN), context=c)) code

    member _.d code =
        TemporaryVariableScope.useOne c.d0.getVar
            (fun name -> double0(Var(Dt,name,NaN), context=c)) code

    member _.z code =
        TemporaryVariableScope.useOne c.z0.getVar
            (fun name -> complex0(Var(Zt,name,NaN), context=c)) code

    member _.I name code =
        TemporaryVariableScope.useOne
            (fun () -> c.i0.getVar(c,name,It 4,A0))
            (fun variableName -> int0(Var(It 4,variableName,NaN), context=c)) code

    member _.D name code =
        TemporaryVariableScope.useOne
            (fun () -> c.d0.getVar(c,name,Dt,A0))
            (fun variableName -> double0(Var(Dt,variableName,NaN), context=c)) code

    member _.Z name code =
        TemporaryVariableScope.useOne
            (fun () -> c.z0.getVar(c,name,Zt,A0))
            (fun variableName -> complex0(Var(Zt,variableName,NaN), context=c)) code

    member _.ix count code =
        TemporaryVariableScope.useMany count c.i0.getVar
            (fun name -> int0(Var(It 4,name,NaN), context=c)) code

    member _.dx count code =
        TemporaryVariableScope.useMany count c.d0.getVar
            (fun name -> double0(Var(Dt,name,NaN), context=c)) code

    member _.zx count code =
        TemporaryVariableScope.useMany count c.z0.getVar
            (fun name -> complex0(Var(Zt,name,NaN), context=c)) code

    member _.f code =
        let name,counter,release = c.f0.getVarAndCounter()
        try
            let initial = if c.language = Fortran then c.numFormat.ItoS(counter + 10) else ""
            c.cvar.setVar(Structure "file",A0,name,initial)
            code name
        finally release()

    member _.t variableType code =
        TemporaryVariableScope.useOne c.t0.getVar
            (fun name ->
                c.cvar.setVar(Structure "string",variableType,name,"")
                name) code

    member _.xLet (x:int0) = fun (f:int0->int0) ->
        let y = 
            match c.language with
            |Numeric ->
                x.Expr.simp.eval()
            |_ ->
                let vname,_ = c.i0.getVar()
                let v = int0(Var(It 4,vname,NaN),c)
                v <== x
                v.Expr
        int0(Let(It 4, x.Expr, y, fun value -> (f(int0(value, x.Context))).Expr), x.Context)
        
    member _.xLet (x:double0) = fun (f:double0->double0) ->
        let y = 
            match c.language with
            |Numeric ->
                x.Expr.simp.eval()
            |_ ->
                let vname,_ = c.d0.getVar()
                let v = double0(Var(Dt,vname,NaN),c)
                v <== x
                v.Expr
        double0(Let(Dt, x.Expr, y, fun value -> (f(double0(value, x.Context))).Expr), x.Context)
        
    member _.xLet (x:complex0) = fun (f:complex0->complex0) ->
        let y = 
            match c.language with
            |Numeric ->
                x.Expr.simp.eval()
            |_ ->
                let vname,_ = c.z0.getVar()
                let v = complex0(Var(Zt,vname,NaN),c)
                v <== x
                v.Expr
        complex0(Let(Zt, x.Expr, y, fun value -> (f(complex0(value, x.Context))).Expr), x.Context)
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
    type Aqualis with
        member this.ch = ContextCh(this)
