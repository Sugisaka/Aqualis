namespace Aqualis

open System

type ContextOmp internal (c:Aqualis) =

    let ensureSupportedLanguage() =
        match c.language with
        |Fortran|C99 -> ()
        |_ -> invalidOp "OpenMP generation is available only for Fortran and C99."

    let validateThreadCount threadCount =
        if threadCount <= 0 then
            invalidArg (nameof threadCount) "The OpenMP thread count must be positive."

    let privateNames() =
        c.varPrivate.list
        |> List.map (fun (_,_,name,_) -> name)
        |> List.distinct
        |> List.rev

    let privateClause names =
        match names with
        |[] -> ""
        |items ->
            items
            |> String.concat ","
            |> sprintf " private(%s)"

    let splitPrivateNames (names:string list) =
        let maximumContentLength = 72
        let chunks = ResizeArray<string list>()
        let mutable currentChunk : string list = []
        let mutable currentLength = 0
        for name in names do
            let addedLength = name.Length + (if List.isEmpty currentChunk then 0 else 1)
            if not (List.isEmpty currentChunk) && currentLength + addedLength > maximumContentLength then
                chunks.Add(List.rev currentChunk)
                currentChunk <- [name]
                currentLength <- name.Length
            else
                currentChunk <- name::currentChunk
                currentLength <- currentLength + addedLength
        if not (List.isEmpty currentChunk) then
            chunks.Add(List.rev currentChunk)
        chunks |> Seq.toList

    let emitFortranDirective directive names suffix =
        match splitPrivateNames names with
        |[] ->
            c.codewritein (directive + suffix)
        |[singleLine] ->
            c.codewritein (
                directive + " private(" + String.concat "," singleLine + ")" + suffix)
        |firstLine::continuationLines ->
            c.codewritein (
                directive + " private(" + String.concat "," firstLine + ", &")
            continuationLines
            |> List.iteri (fun index namesOnLine ->
                let isLast = index = continuationLines.Length - 1
                let ending = if isLast then ")" + suffix else ", &"
                c.codewritein ("!$omp& " + String.concat "," namesOnLine + ending))

    let captureParallelBody code =
        let previousPrivateVariables = c.varPrivate.list
        c.varPrivate.clear()
        try
            let body,_ =
                c.captureCode(fun () ->
                    c.WithParallelMode(fun child -> code child))
            body, privateNames()
        finally
            c.varPrivate.clear()
            previousPrivateVariables
            |> List.rev
            |> List.iter c.varPrivate.setVar

    let validateRegion threadCount =
        ensureSupportedLanguage()
        threadCount |> Option.iter validateThreadCount
        if c.ParallelMode then
            invalidOp "Nested OpenMP regions are not supported."

    let registerOpenMpUsage() =
        c.IsOpenMpUsed <- true
        match c.language with
        |Fortran -> c.mlist.add "omp_lib"
        |C99 -> c.hlist.add "<omp.h>"
        |_ -> ()

    let emitParallelFor (threadCount:int option) (reduction:string option) code =
        validateRegion threadCount
        let body,names = captureParallelBody code
        registerOpenMpUsage()
        let threadPart = threadCount |> Option.map (sprintf " num_threads(%d)") |> Option.defaultValue ""
        let reductionPart = reduction |> Option.map (sprintf " reduction(%s)") |> Option.defaultValue ""
        match c.language with
        |Fortran ->
            emitFortranDirective "!$omp parallel do" names (threadPart + reductionPart)
            c.writeRaw body
            c.codewritein "!$omp end parallel do"
        |C99 ->
            c.codewritein ("#pragma omp parallel for" + privateClause names + threadPart + reductionPart)
            c.writeRaw body
        |_ -> ()

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
        validateRegion (Some threadCount)
        let body,names = captureParallelBody code
        registerOpenMpUsage()
        match c.language with
        |Fortran ->
            emitFortranDirective "!$omp parallel" names ($" num_threads({threadCount})")
            c.codewritein "!$omp sections"
            c.writeRaw body
            c.codewritein "!$omp end sections"
            c.codewritein "!$omp end parallel"
        |C99 ->
            c.codewritein ($"#pragma omp parallel{privateClause names} num_threads({threadCount})")
            c.codewritein "{"
            c.codewritein "#pragma omp sections"
            c.codewritein "{"
            c.writeRaw body
            c.codewritein "}"
            c.codewritein "}"
        |_ -> ()

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
        |_ -> invalidOp "OpenMP generation is available only for Fortran and C99."

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
        ///<summary>OpenMP</summary>
        member this.omp = ContextOmp(this)
