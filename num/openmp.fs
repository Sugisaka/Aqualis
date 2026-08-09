namespace Aqualis

open System

type ContextOmp internal (c:Aqualis) =

    let runParallel (ctx:Aqualis) code =
        ctx.close()
        ctx.WithParallelMode(fun child -> code child)
        ctx.appendOpen()

    let privateClause (c:Aqualis) =
        match c.varPrivate.list with
        |[] -> ""
        |items ->
            items
            |> List.map (fun (_,_,name,_) -> name)
            |> String.concat ","
            |> sprintf " private(%s)"

    let emitParallelFor (threadCount:int option) (reduction:string option) code =
        c.IsOpenMpUsed <- true
        match c.language with
        |Fortran -> c.mlist.add "omp_lib"
        |C99 -> c.hlist.add "<omp.h>"
        |_ -> ()
        runParallel c code
        let privatePart = privateClause c 
        let threadPart = threadCount |> Option.map (sprintf " num_threads(%d)") |> Option.defaultValue ""
        let reductionPart = reduction |> Option.map (sprintf " reduction(%s)") |> Option.defaultValue ""
        match c.language with
        |Fortran ->
            c.codewritein ("!$omp parallel do" + privatePart + threadPart + reductionPart)
            c.codewritein "!$omp end parallel do"
        |C99 ->
            c.codewritein ("#pragma omp parallel for" + privatePart + threadPart + reductionPart)
        |_ -> ()
        c.varPrivate.clear()

    member _.parallelize code = emitParallelFor None None code
    member _.parallelize_th threadCount code = emitParallelFor (Some threadCount) None code

    member _.reduction(variable:INum0, operation:string) code =
        Aqualis.merge c variable.Context |> ignore
        match operation with
        |"+"|"-"|"*" -> emitParallelFor None (Some(operation + ":" + variable.Code)) code
        |_ -> invalidArg (nameof operation) "OpenMP reduction supports +, -, and *."

    member _.reduction_th(threadCount:int, variable:INum0, operation:string) code =
        Aqualis.merge c variable.Context |> ignore
        match operation with
        |"+"|"-"|"*" -> emitParallelFor (Some threadCount) (Some(operation + ":" + variable.Code)) code
        |_ -> invalidArg (nameof operation) "OpenMP reduction supports +, -, and *."

    member _.sections threadCount code =
        c.IsOpenMpUsed <- true
        match c.language with
        |Fortran -> c.mlist.add "omp_lib"
        |C99 -> c.hlist.add "<omp.h>"
        |_ -> ()
        runParallel c code
        let privatePart = privateClause c
        match c.language with
        |Fortran ->
            c.codewritein ($"!$omp parallel{privatePart} num_threads({threadCount})")
            c.codewritein "!$omp sections"
            c.codewritein "!$omp end sections"
            c.codewritein "!$omp end parallel"
        |C99 ->
            c.codewritein ($"#pragma omp parallel{privatePart} num_threads({threadCount})")
            c.codewritein "{"
            c.codewritein "#pragma omp sections"
            c.codewritein "{"
            c.codewritein "}"
            c.codewritein "}"
        |_ -> ()
        c.varPrivate.clear()

    member _.section code =
        match c.language with
        |Fortran ->
            c.codewritein "!$omp section"
            c.WithParallelMode(fun child -> code child)
        |C99 ->
            c.codewritein "#pragma omp section"
            c.codewritein "{"
            c.WithParallelMode(fun child -> code child)
            c.codewritein "}"
        |_ -> ()

    member _.thread_num =
        match c.language with
        |Fortran|C99 -> int0(Var(It 4,"omp_get_thread_num()",NaN), context=c)
        |_ -> invalidOp "OpenMP thread numbers are available only for Fortran and C99."

    member _.max_threads =
        match c.language with
        |Fortran|C99 -> int0(Var(It 4,"omp_get_max_threads()",NaN), context=c)
        |_ -> invalidOp "OpenMP thread counts are available only for Fortran and C99."

[<AutoOpen>]
module CompilationEnvironmentOmpExtensions =
    type Aqualis with
        member this.omp = ContextOmp(this)
