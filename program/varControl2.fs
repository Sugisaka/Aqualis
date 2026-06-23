// 
// Copyright (c) 2026 Jun-ichiro Sugisaka
// 
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
// 
namespace Aqualis
    
    [<AutoOpen>]
    module varControl2 =
        type varGenerator with
            static member isVarExist(v:string, typ:Etype, atp:VarType) =
                [
                    programList[prIndex].i0, It 4, A0;
                    programList[prIndex].d0, Dt, A0;
                    programList[prIndex].z0, Zt, A0;
                    programList[prIndex].i1, It 4, A1 0;
                    programList[prIndex].d1, Dt, A1 0;
                    programList[prIndex].z1, Zt, A1 0;
                    programList[prIndex].i2, It 4, A2(0,0);
                    programList[prIndex].d2, Dt, A2(0,0);
                    programList[prIndex].z2, Zt, A2(0,0);
                    programList[prIndex].i3, It 4, A3(0,0,0);
                    programList[prIndex].d3, Dt, A3(0,0,0);
                    programList[prIndex].z3, Zt, A3(0,0,0);
                    programList[prIndex].c0, Structure "char", A0;
                    programList[prIndex].t0, Structure "string", A0;
                    programList[prIndex].f0, Structure "file", A0;
                ]
                |> List.map(fun (u,tp,ap) -> 
                    match u.isVarExist v with
                    // 他で使用されていない
                    |None,None,None,None -> false
                    // 他で使用されているが、使用済みで型も同じ
                    |None,Some _,None,None when tp=typ && ap=atp -> false
                    // 他で使用されている
                    |_ -> true)
                |> List.contains true
                |> fun s -> s || programList[prIndex].var.exists v
                
            member this.getVar(v:string, tp:Etype, ap:VarType) =
                if varGenerator.isVarExist (v,tp,ap) then
                    let u,f = this.getVar()
                    printfn "変数%sは使用中のため、ここで使用できません。代わりに%sを使用します" v u
                    u,f
                else
                    match List.tryFind (fun x -> x=v) this.OfflineStrList, List.tryFind (fun (x:int) -> this.varName x = v) this.OfflineNumList with
                    |None,None ->
                        this.addOnlineStrList v
                        let returnVar() =
                            this.removeOnlineStrList v
                            this.addOfflineStrList v
                        v,returnVar
                    |None,Some n ->
                        this.removeOfflineNumList n
                        this.addOnlineStrList v
                        let returnVar() =
                            this.removeOnlineStrList v
                            this.addOfflineStrList v
                        v,returnVar
                    |Some n, None ->
                        this.removeOfflineStrList n
                        this.addOnlineStrList v
                        let returnVar() =
                            this.removeOnlineStrList v
                            this.addOfflineStrList v
                        v,returnVar
                    |Some n, Some m ->
                        this.removeOfflineNumList m
                        this.removeOfflineStrList n
                        this.addOnlineStrList v
                        let returnVar() =
                            this.removeOnlineStrList v
                            this.addOfflineStrList v
                        v,returnVar
