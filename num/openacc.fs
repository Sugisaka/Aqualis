namespace Aqualis

open System

type ContextOpenAcc internal (c:Aqualis) =

    member _.parallelize code =
        c.IsOpenAccUsed <- true
        let copyIn = c.varCopyIn.list |> List.map (fun (_,_,name,_) -> name)
        let copyOut = c.varCopyOut.list |> List.map (fun (_,_,name,_) -> name)
        let dataClause =
            [
                if not copyIn.IsEmpty then yield "copyin(" + String.concat "," copyIn + ")"
                if not copyOut.IsEmpty then yield "copyout(" + String.concat "," copyOut + ")"
            ] |> String.concat " "
        match c.language with
        |Fortran ->
            if dataClause <> "" then c.codewritein ("!$acc data " + dataClause)
            c.codewritein "!$acc kernels"
            c.WithParallelMode(fun child -> code child)
            c.codewritein "!$acc end kernels"
            if dataClause <> "" then c.codewritein "!$acc end data"
        |C99 ->
            if dataClause <> "" then
                c.codewritein ("#pragma acc data " + dataClause)
                c.codewritein "{"
            c.codewritein "#pragma acc kernels"
            c.codewritein "{"
            c.WithParallelMode(fun child -> code child)
            c.codewritein "}"
            if dataClause <> "" then c.codewritein "}"
        |_ -> invalidOp "OpenACC generation is available only for Fortran and C99."

[<AutoOpen>]
module CompilationEnvironmentOpenAccExtensions =
    type Aqualis with
        member this.oacc = ContextOpenAcc(this)
