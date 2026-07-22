namespace Aqualis

open System

type ContextOpenAcc internal (environment:CompilationEnvironment) =
    let context() = environment.RequireGenerationContext()

    member _.parallelize code =
        let ctx = context()
        let program = ctx.CurrentProgram
        ctx.IsOpenAccUsed <- true
        let copyIn = program.varCopyIn.list |> List.map (fun (_,_,name,_) -> name)
        let copyOut = program.varCopyOut.list |> List.map (fun (_,_,name,_) -> name)
        let dataClause =
            [
                if not copyIn.IsEmpty then yield "copyin(" + String.concat "," copyIn + ")"
                if not copyOut.IsEmpty then yield "copyout(" + String.concat "," copyOut + ")"
            ] |> String.concat " "
        match program.language with
        |Fortran ->
            if dataClause <> "" then program.codewritein ("!$acc data " + dataClause)
            program.codewritein "!$acc kernels"
            ctx.WithParallelMode(fun child -> code (CompilationEnvironment(Some child)))
            program.codewritein "!$acc end kernels"
            if dataClause <> "" then program.codewritein "!$acc end data"
        |C99 ->
            if dataClause <> "" then
                program.codewritein ("#pragma acc data " + dataClause)
                program.codewritein "{"
            program.codewritein "#pragma acc kernels"
            program.codewritein "{"
            ctx.WithParallelMode(fun child -> code (CompilationEnvironment(Some child)))
            program.codewritein "}"
            if dataClause <> "" then program.codewritein "}"
        |_ -> invalidOp "OpenACC generation is available only for Fortran and C99."

[<AutoOpen>]
module CompilationEnvironmentOpenAccExtensions =
    type CompilationEnvironment with
        member this.oacc = ContextOpenAcc(this)
