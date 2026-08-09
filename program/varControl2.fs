namespace Aqualis

[<AutoOpen>]
module varControl2 =
    type varGenerator with
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

        member private this.getNamedVar(name:string, collision:bool) =
            if collision then
                let replacement,release = this.getVar()
                printfn "Variable %s is already in use; using %s instead." name replacement
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

        member this.getVar(program:Aqualis, name:string, typ:Etype, shape:VarType) =
            this.getNamedVar(name, varGenerator.isVarExist(program,name,typ,shape))

        member this.getVar(name:string, _typ:Etype, _shape:VarType) =
            let collision =
                match this.isVarExist name with
                |None,None,None,None -> false
                |_ -> true
            this.getNamedVar(name, collision)
