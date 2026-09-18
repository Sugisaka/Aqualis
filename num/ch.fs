namespace Aqualis

/// Acquires temporary variable names and guarantees their release after callbacks.
module internal TemporaryVariableScope =
    /// Runs a callback with one temporary variable, releasing it even on failure.
    let useOne acquire createValue code =
        let name,release = acquire()
        try code (createValue name)
        finally release()

    /// Runs a callback with the requested number of temporary variables and
    /// releases all acquired names, including when acquisition or execution fails.
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

/// Allocates temporary scalar and array variables for generated operations.
type ContextCh internal (c:Aqualis) =

    /// Gets the generation context that owns temporary variables.
    member internal _.Environment = c

    /// Provides temporary integer scalar variables to the callback and releases them afterward.
    member _.i code =
        TemporaryVariableScope.useOne c.i0.getVar
            (fun name -> int0(Var(It 4,name,NaN), context=c)) code

    /// Provides temporary real scalar variables to the callback and releases them afterward.
    member _.d code =
        TemporaryVariableScope.useOne c.d0.getVar
            (fun name -> double0(Var(Dt,name,NaN), context=c)) code

    /// Provides temporary complex scalar variables to the callback and releases them afterward.
    member _.z code =
        TemporaryVariableScope.useOne c.z0.getVar
            (fun name -> complex0(Var(Zt,name,NaN), context=c)) code

    /// Provides a named temporary integer variable to the callback.
    member _.I name code =
        TemporaryVariableScope.useOne
            (fun () -> c.i0.getVar(c,name,It 4,A0))
            (fun variableName -> int0(Var(It 4,variableName,NaN), context=c)) code

    /// Provides a named temporary real variable to the callback.
    member _.D name code =
        TemporaryVariableScope.useOne
            (fun () -> c.d0.getVar(c,name,Dt,A0))
            (fun variableName -> double0(Var(Dt,variableName,NaN), context=c)) code

    /// Provides a named temporary complex variable to the callback.
    member _.Z name code =
        TemporaryVariableScope.useOne
            (fun () -> c.z0.getVar(c,name,Zt,A0))
            (fun variableName -> complex0(Var(Zt,variableName,NaN), context=c)) code

    /// Provides the requested number of temporary integer scalars to the callback.
    member _.ix count code =
        TemporaryVariableScope.useMany count c.i0.getVar
            (fun name -> int0(Var(It 4,name,NaN), context=c)) code

    /// Provides the requested number of temporary real scalars to the callback.
    member _.dx count code =
        TemporaryVariableScope.useMany count c.d0.getVar
            (fun name -> double0(Var(Dt,name,NaN), context=c)) code

    /// Provides the requested number of temporary complex scalars to the callback.
    member _.zx count code =
        TemporaryVariableScope.useMany count c.z0.getVar
            (fun name -> complex0(Var(Zt,name,NaN), context=c)) code

    /// Provides a temporary file variable name to the callback and releases it afterward.
    member _.f code =
        let name,counter,release = c.f0.getVarAndCounter()
        try
            let initial = if c.language = Fortran then c.numFormat.ItoS(counter + 10) else ""
            c.cvar.setUniqVar(Structure "file",A0,name,initial)
            code name
        finally release()

    /// Provides a temporary string variable of the requested shape to the callback.
    member _.t variableType code =
        TemporaryVariableScope.useOne c.t0.getVar
            (fun name ->
                c.cvar.setUniqVar(Structure "string",variableType,name,"")
                name) code

    /// Builds a local symbolic binding around the supplied callback.
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
        
    /// Builds a local symbolic binding around the supplied callback.
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
        
    /// Builds a local symbolic binding around the supplied callback.
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
    /// Provides temporary integer, integer scalar variables to the callback and releases them afterward.
    member this.ii code = this.i (fun first -> this.i (fun second -> code(first,second)))
    /// Provides temporary integer, real scalar variables to the callback and releases them afterward.
    member this.id code = this.i (fun first -> this.d (fun second -> code(first,second)))
    /// Provides temporary integer, complex scalar variables to the callback and releases them afterward.
    member this.iz code = this.i (fun first -> this.z (fun second -> code(first,second)))
    /// Provides temporary real, real scalar variables to the callback and releases them afterward.
    member this.dd code = this.d (fun first -> this.d (fun second -> code(first,second)))
    /// Provides temporary real, complex scalar variables to the callback and releases them afterward.
    member this.dz code = this.d (fun first -> this.z (fun second -> code(first,second)))
    /// Provides temporary complex, complex scalar variables to the callback and releases them afterward.
    member this.zz code = this.z (fun first -> this.z (fun second -> code(first,second)))
    /// Provides temporary integer, integer, integer scalar variables to the callback and releases them afterward.
    member this.iii code = this.i (fun first -> this.i (fun second -> this.i (fun third -> code(first,second,third))))
    /// Provides temporary integer, integer, real scalar variables to the callback and releases them afterward.
    member this.iid code = this.i (fun first -> this.i (fun second -> this.d (fun third -> code(first,second,third))))
    /// Provides temporary integer, integer, complex scalar variables to the callback and releases them afterward.
    member this.iiz code = this.i (fun first -> this.i (fun second -> this.z (fun third -> code(first,second,third))))
    /// Provides temporary integer, real, real scalar variables to the callback and releases them afterward.
    member this.idd code = this.i (fun first -> this.d (fun second -> this.d (fun third -> code(first,second,third))))
    /// Provides temporary integer, real, complex scalar variables to the callback and releases them afterward.
    member this.idz code = this.i (fun first -> this.d (fun second -> this.z (fun third -> code(first,second,third))))
    /// Provides temporary integer, complex, complex scalar variables to the callback and releases them afterward.
    member this.izz code = this.i (fun first -> this.z (fun second -> this.z (fun third -> code(first,second,third))))
    /// Provides temporary real, real, real scalar variables to the callback and releases them afterward.
    member this.ddd code = this.d (fun first -> this.d (fun second -> this.d (fun third -> code(first,second,third))))
    /// Provides temporary real, real, complex scalar variables to the callback and releases them afterward.
    member this.ddz code = this.d (fun first -> this.d (fun second -> this.z (fun third -> code(first,second,third))))
    /// Provides temporary real, complex, complex scalar variables to the callback and releases them afterward.
    member this.dzz code = this.d (fun first -> this.z (fun second -> this.z (fun third -> code(first,second,third))))
    /// Provides temporary complex, complex, complex scalar variables to the callback and releases them afterward.
    member this.zzz code = this.z (fun first -> this.z (fun second -> this.z (fun third -> code(first,second,third))))
    /// Provides temporary integer, integer, integer, integer scalar variables to the callback and releases them afterward.
    member this.iiii code = this.i (fun first -> this.i (fun second -> this.i (fun third -> this.i (fun fourth -> code(first,second,third,fourth)))))
    /// Provides temporary integer, integer, integer, real scalar variables to the callback and releases them afterward.
    member this.iiid code = this.i (fun first -> this.i (fun second -> this.i (fun third -> this.d (fun fourth -> code(first,second,third,fourth)))))
    /// Provides temporary integer, integer, integer, complex scalar variables to the callback and releases them afterward.
    member this.iiiz code = this.i (fun first -> this.i (fun second -> this.i (fun third -> this.z (fun fourth -> code(first,second,third,fourth)))))
    /// Provides temporary integer, integer, real, real scalar variables to the callback and releases them afterward.
    member this.iidd code = this.i (fun first -> this.i (fun second -> this.d (fun third -> this.d (fun fourth -> code(first,second,third,fourth)))))
    /// Provides temporary integer, integer, real, complex scalar variables to the callback and releases them afterward.
    member this.iidz code = this.i (fun first -> this.i (fun second -> this.d (fun third -> this.z (fun fourth -> code(first,second,third,fourth)))))
    /// Provides temporary integer, integer, complex, complex scalar variables to the callback and releases them afterward.
    member this.iizz code = this.i (fun first -> this.i (fun second -> this.z (fun third -> this.z (fun fourth -> code(first,second,third,fourth)))))
    /// Provides temporary integer, real, real, real scalar variables to the callback and releases them afterward.
    member this.iddd code = this.i (fun first -> this.d (fun second -> this.d (fun third -> this.d (fun fourth -> code(first,second,third,fourth)))))
    /// Provides temporary integer, real, real, complex scalar variables to the callback and releases them afterward.
    member this.iddz code = this.i (fun first -> this.d (fun second -> this.d (fun third -> this.z (fun fourth -> code(first,second,third,fourth)))))
    /// Provides temporary integer, real, complex, complex scalar variables to the callback and releases them afterward.
    member this.idzz code = this.i (fun first -> this.d (fun second -> this.z (fun third -> this.z (fun fourth -> code(first,second,third,fourth)))))
    /// Provides temporary integer, complex, complex, complex scalar variables to the callback and releases them afterward.
    member this.izzz code = this.i (fun first -> this.z (fun second -> this.z (fun third -> this.z (fun fourth -> code(first,second,third,fourth)))))
    /// Provides temporary real, real, real, real scalar variables to the callback and releases them afterward.
    member this.dddd code = this.d (fun first -> this.d (fun second -> this.d (fun third -> this.d (fun fourth -> code(first,second,third,fourth)))))
    /// Provides temporary real, real, real, complex scalar variables to the callback and releases them afterward.
    member this.dddz code = this.d (fun first -> this.d (fun second -> this.d (fun third -> this.z (fun fourth -> code(first,second,third,fourth)))))
    /// Provides temporary real, real, complex, complex scalar variables to the callback and releases them afterward.
    member this.ddzz code = this.d (fun first -> this.d (fun second -> this.z (fun third -> this.z (fun fourth -> code(first,second,third,fourth)))))
    /// Provides temporary real, complex, complex, complex scalar variables to the callback and releases them afterward.
    member this.dzzz code = this.d (fun first -> this.z (fun second -> this.z (fun third -> this.z (fun fourth -> code(first,second,third,fourth)))))
    /// Provides temporary complex, complex, complex, complex scalar variables to the callback and releases them afterward.
    member this.zzzz code = this.z (fun first -> this.z (fun second -> this.z (fun third -> this.z (fun fourth -> code(first,second,third,fourth)))))

/// Adds temporary-variable helpers to Aqualis.
[<AutoOpen>]
module CompilationEnvironmentChExtensions =
    type Aqualis with
        ///<summary>一時変数生成</summary>
        member this.ch = ContextCh(this)
