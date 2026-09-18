namespace Aqualis

/// Adds collision-aware named-variable allocation to variable generators.
[<AutoOpen>]
module varControl2 =
    type varGenerator with
        /// Checks whether a name is occupied by any scalar, array, character,
        /// string, file, or custom variable in the generation context.
        static member isVarExist(program:Aqualis, name:string, typ:Etype, shape:VarType) =
            [
                program.i0, It 4, A0
                program.d0, Dt, A0
                program.z0, Zt, A0
                program.i1, It 4, A1 0
                program.d1, Dt, A1 0
                program.z1, Zt, A1 0
                program.i2, It 4, A2(0,0)
                program.d2, Dt, A2(0,0)
                program.z2, Zt, A2(0,0)
                program.i3, It 4, A3(0,0,0)
                program.d3, Dt, A3(0,0,0)
                program.z3, Zt, A3(0,0,0)
                program.c0, Structure "char", A0
                program.t0, Structure "string", A0
                program.f0, Structure "file", A0
            ]
            |> List.exists (fun (generator,itemType,itemShape) ->
                match generator.isVarExist name with
                |None,None,None,None -> false
                |None,Some _,None,None when itemType = typ && itemShape = shape -> false
                |_ -> true)
            |> fun exists -> exists || program.cvar.exists name

        /// Allocates or retrieves a named variable while handling collisions.
        member private this.getNamedVar(name:string, collision:bool, program:Aqualis option) =
            if collision then
                let replacement,release = this.getVar()
                let message = "Variable '" + name + "' is already in use; using '" + replacement + "' instead."
                let properties = Map ["variable", name; "replacement", replacement]
                match program with
                | Some context ->
                    context.ReportDiagnostic(
                        "AQL1001", Warning, message,
                        Some "variable allocation", properties)
                | None ->
                    Diagnostic.report
                        "AQL1001" Warning message None properties
                replacement,release
            else
                match
                    List.tryFind ((=) name) this.OfflineStrList,
                    List.tryFind (fun (index:int) -> this.varName index = name) this.OfflineNumList
                with
                |None,None ->
                    this.addOnlineStrList name
                |None,Some index ->
                    this.removeOfflineNumList index
                    this.addOnlineStrList name
                |Some existing,None ->
                    this.removeOfflineStrList existing
                    this.addOnlineStrList name
                |Some existing,Some index ->
                    this.removeOfflineNumList index
                    this.removeOfflineStrList existing
                    this.addOnlineStrList name
                let release() =
                    this.removeOnlineStrList name
                    this.addOfflineStrList name
                name,release

        /// Allocates a named variable in a context, substituting a generated name
        /// and reporting AQL1001 when the requested name collides.
        member this.getVar(program:Aqualis, name:string, typ:Etype, shape:VarType) =
            this.getNamedVar(name, varGenerator.isVarExist(program,name,typ,shape), Some program)

        /// Allocates a named variable without a generation context, reporting
        /// any collision through the ambient diagnostic scope.
        member this.getVar(name:string, _typ:Etype, _shape:VarType) =
            let collision =
                match this.isVarExist name with
                |None,None,None,None -> false
                |_ -> true
            this.getNamedVar(name, collision, None)
