namespace Aqualis
    
    ///<summary>構造体sname_のメンバ変数を管理</summary>
    type structmember (sname_:string) =
        let mutable memlist_:(Etype*VarType*string)list = []
        ///<summary>メンバ変数を追加</summary>
        member __.add(typ,vtp,name) =
            if List.exists(fun (typ_,vtp_,name_) -> typ_=typ && vtp_=vtp && name_=name) memlist_ = false then
                memlist_ <- (typ,vtp,name)::memlist_
                match vtp with
                |A0 -> ()
                |A1 _ -> memlist_ <- (It 4,A1 1,name+"_size")::memlist_
                |A2 _ -> memlist_ <- (It 4,A1 2,name+"_size")::memlist_
                |A3 _ -> memlist_ <- (It 4,A1 3,name+"_size")::memlist_
                
        ///<summary>構造体名</summary>
        member __.sname with get() = sname_
        member __.memlist with get() = memlist_
        
    ///<summary>構造体を管理</summary>
    type structure () =
        ///<summary>定義された構造体リスト</summary>
        let mutable strlist:structmember list = []
        
        member this.clear() = strlist <- []
        
        ///<summary>構造体を追加</summary>
        member __.addstructure sname =
            //構造体が未定義の場合はリストに追加
            if strlist |> List.exists(fun s -> s.sname=sname) = false then
                strlist <- structmember sname::strlist
        
        ///<summary>構造体メンバ変数を追加</summary>
        member this.addmember(sname,(typ,vtp,name)) =
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
                ()
                
        ///<summary>構造体メンバがすべてそれ以前に定義された構造体となるようにソート</summary>
        member internal __.sort() =
            //lst1：ソート済みリスト
            //lst2：未ソートリスト
            //lst0：検証対象のリスト
            let rec sort (lst1:structmember list) (lst2:structmember list) (lst0:structmember list) =
                //pに含まれる型名にp2に含まれていない型の構造体が入っているか検索
                let rec search1 (p1:(Etype*VarType*string)list) (p2:structmember list) res =
                    match p1 with
                    |(Structure sname,_,_)::b when List.exists (fun (n:structmember)->sname=n.sname) p2 = false ->
                        false
                    |a::b ->
                        search1 b p2 (res&&true)
                    |[] ->
                        res
                match lst0 with
                |[] when lst2.Length=0 ->
                    //lst0を最後まで検証し、未ソートリストlst2が空になったとき
                    //lst1：ソート済みのリスト
                    lst1
                |a::b when search1 a.memlist lst1 true ->
                    //aに中にlst1で未定義の構造体が入っていない場合
                    //lst1に追加して次を検証
                    sort (lst1@[a]) lst2 b
                |a::b ->
                    //aに中にlst1で未定義の構造体が入っていない場合
                    //lst2に追加して次を検証
                    sort lst1 (lst2@[a]) b
                |_ ->
                    //lst0を最後まで検証し、未ソートリストlst2の要素が残っているとき
                    //未ソート分をやり直し
                    sort lst1 [] lst2
            sort [] [] strlist
