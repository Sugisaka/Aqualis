// 
// Copyright (c) 2026 Jun-ichiro Sugisaka
// 
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
// 
namespace Aqualis
    
    ///<summary>構造体sname_のメンバ変数を管理</summary>
    type structmember (sname_:string) =
        let gate = obj()
        let mutable memlist_:(Etype*VarType*string)list = []
        ///<summary>メンバ変数を追加</summary>
        member __.add(typ,vtp,name) =
            lock gate (fun () ->
                if List.exists(fun (typ_,vtp_,name_) -> typ_=typ && vtp_=vtp && name_=name) memlist_ = false then
                    memlist_ <- (typ,vtp,name)::memlist_
                    match vtp with
                    |A0 -> ()
                    |A1 _ -> memlist_ <- (It 4,A1 1,name+"_size")::memlist_
                    |A2 _ -> memlist_ <- (It 4,A1 2,name+"_size")::memlist_
                    |A3 _ -> memlist_ <- (It 4,A1 3,name+"_size")::memlist_)
                
        ///<summary>構造体名</summary>
        member __.sname with get() = sname_
        member __.memlist with get() = lock gate (fun () -> memlist_)
        
    ///<summary>構造体を管理</summary>
    type structure () =
        let gate = obj()
        ///<summary>定義された構造体リスト</summary>
        let mutable strlist:structmember list = []
        
        member this.clear() = lock gate (fun () -> strlist <- [])
        
        ///<summary>構造体を追加</summary>
        member __.addstructure sname =
            lock gate (fun () ->
                //構造体が未定義の場合はリストに追加
                if strlist |> List.exists(fun s -> s.sname=sname) = false then
                    strlist <- structmember sname::strlist)
        
        ///<summary>構造体メンバ変数を追加</summary>
        member this.addmember(sname,(typ,vtp,name)) =
            lock gate (fun () ->
                // 追加するメンバ変数の型が構造体の場合、その構造体定義も追加
                match typ with
                |Structure s ->
                    this.addstructure s
                |_ ->
                    ()
                match strlist |> List.tryFindIndex (fun s -> s.sname=sname) with
                |Some i ->
                    strlist.[i].add(typ,vtp,name)
                |None ->
                    ())
                
        ///<summary>構造体メンバがすべてそれ以前に定義された構造体となるようにソート</summary>
        member internal __.sort() =
            lock gate (fun () ->
                let structuresByName =
                    strlist
                    |> Seq.map (fun item -> item.sname, item)
                    |> dict

                // 1: visiting, 2: visited
                let states =
                    System.Collections.Generic.Dictionary<string,int>()
                let sorted = ResizeArray<structmember>()

                let dependencies (item:structmember) =
                    item.memlist
                    |> Seq.choose (fun (elementType,_,_) ->
                        match elementType with
                        |Structure dependencyName -> Some dependencyName
                        |_ -> None)
                    |> Seq.distinct
                    |> Seq.toList

                let rec visit path name =
                    match states.TryGetValue name with
                    |true,2 ->
                        ()
                    |true,1 ->
                        let cycleStart =
                            path
                            |> List.tryFindIndex ((=) name)
                            |> Option.defaultValue 0
                        let cycle =
                            (path |> List.skip cycleStart) @ [name]
                        invalidOp (
                            "Circular structure dependency detected: " +
                            String.concat " -> " cycle)
                    |_ ->
                        match structuresByName.TryGetValue name with
                        |false,_ ->
                            invalidOp (
                                $"Structure '{name}' is referenced but not defined.")
                        |true,item ->
                            states[name] <- 1
                            let currentPath = path @ [name]
                            for dependency in dependencies item do
                                visit currentPath dependency
                            states[name] <- 2
                            sorted.Add item

                for item in strlist do
                    visit [] item.sname

                sorted |> Seq.toList)
