namespace Aqualis

open System

type ContextOmp internal (environment:Aqualis) =
    let context() = environment.RequireGenerationContext()

    let runParallel (ctx:GenerationContext) code =
        let program = ctx.CurrentProgram
        program.close()
        ctx.WithParallelMode(fun child -> code (Aqualis(Some child)))
        program.appendOpen()

    let privateClause (program:program) =
        match program.varPrivate.list with
        |[] -> ""
        |items ->
            items
            |> List.map (fun (_,_,name,_) -> name)
            |> String.concat ","
            |> sprintf " private(%s)"

    let emitParallelFor (threadCount:int option) (reduction:string option) code =
        let ctx = context()
        let program = ctx.CurrentProgram
        ctx.IsOpenMpUsed <- true
        match program.language with
        |Fortran -> program.mlist.add "omp_lib"
        |C99 -> program.hlist.add "<omp.h>"
        |_ -> ()
        runParallel ctx code
        let privatePart = privateClause program
        let threadPart = threadCount |> Option.map (sprintf " num_threads(%d)") |> Option.defaultValue ""
        let reductionPart = reduction |> Option.map (sprintf " reduction(%s)") |> Option.defaultValue ""
        match program.language with
        |Fortran ->
            program.codewritein ("!$omp parallel do" + privatePart + threadPart + reductionPart)
            program.codewritein "!$omp end parallel do"
        |C99 ->
            program.codewritein ("#pragma omp parallel for" + privatePart + threadPart + reductionPart)
        |_ -> ()
        program.varPrivate.clear()

    member _.parallelize code = emitParallelFor None None code
    member _.parallelize_th threadCount code = emitParallelFor (Some threadCount) None code

    member _.reduction(variable:INum0, operation:string) code =
        let ctx = context()
        GenerationContextMerge.merge (Some ctx) variable.Context |> ignore
        match operation with
        |"+"|"-"|"*" -> emitParallelFor None (Some(operation + ":" + variable.Code)) code
        |_ -> invalidArg (nameof operation) "OpenMP reduction supports +, -, and *."

    member _.reduction_th(threadCount:int, variable:INum0, operation:string) code =
        let ctx = context()
        GenerationContextMerge.merge (Some ctx) variable.Context |> ignore
        match operation with
        |"+"|"-"|"*" -> emitParallelFor (Some threadCount) (Some(operation + ":" + variable.Code)) code
        |_ -> invalidArg (nameof operation) "OpenMP reduction supports +, -, and *."

    member _.sections threadCount code =
        let ctx = context()
        let program = ctx.CurrentProgram
        ctx.IsOpenMpUsed <- true
        match program.language with
        |Fortran -> program.mlist.add "omp_lib"
        |C99 -> program.hlist.add "<omp.h>"
        |_ -> ()
        runParallel ctx code
        let privatePart = privateClause program
        match program.language with
        |Fortran ->
            program.codewritein ($"!$omp parallel{privatePart} num_threads({threadCount})")
            program.codewritein "!$omp sections"
            program.codewritein "!$omp end sections"
            program.codewritein "!$omp end parallel"
        |C99 ->
            program.codewritein ($"#pragma omp parallel{privatePart} num_threads({threadCount})")
            program.codewritein "{"
            program.codewritein "#pragma omp sections"
            program.codewritein "{"
            program.codewritein "}"
            program.codewritein "}"
        |_ -> ()
        program.varPrivate.clear()

    member _.section code =
        let ctx = context()
        match ctx.CurrentProgram.language with
        |Fortran ->
            ctx.CurrentProgram.codewritein "!$omp section"
            ctx.WithParallelMode(fun child -> code (Aqualis(Some child)))
        |C99 ->
            ctx.CurrentProgram.codewritein "#pragma omp section"
            ctx.CurrentProgram.codewritein "{"
            ctx.WithParallelMode(fun child -> code (Aqualis(Some child)))
            ctx.CurrentProgram.codewritein "}"
        |_ -> ()

    member _.thread_num =
        let ctx = context()
        match ctx.CurrentProgram.language with
        |Fortran|C99 -> int0(Var(It 4,"omp_get_thread_num()",NaN), context=ctx)
        |_ -> invalidOp "OpenMP thread numbers are available only for Fortran and C99."

    member _.max_threads =
        let ctx = context()
        match ctx.CurrentProgram.language with
        |Fortran|C99 -> int0(Var(It 4,"omp_get_max_threads()",NaN), context=ctx)
        |_ -> invalidOp "OpenMP thread counts are available only for Fortran and C99."

[<AutoOpen>]
module CompilationEnvironmentOmpExtensions =
    type Aqualis with
        member this.omp = ContextOmp(this)
