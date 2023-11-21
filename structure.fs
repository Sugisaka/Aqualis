(*
Copyright (c) 2022 Jun-ichiro Sugisaka

This software is released under the MIT License.
http://opensource.org/licenses/mit-license.php
*)
namespace Aqualis
    
    open Aqualis_base

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
        member __.addstructure(sname) =
            //構造体が未定義の場合はリストに追加
            if (strlist |> List.exists(fun s -> s.sname=sname)) = false then
                strlist <- structmember(sname)::strlist
        
        ///<summary>構造体メンバ変数を追加</summary>
        member __.addmember(sname,(typ,vtp,name)) =
            match strlist |> List.tryFindIndex (fun s -> s.sname=sname) with
            |Some(i) ->
                strlist.[i].add(typ,vtp,name)
            |None ->
                ()
                  
        ///<summary>構造体メンバがすべてそれ以前に定義された構造体となるようにソート</summary>
        member private __.sort() =
            //lst1：ソート済みリスト
            //lst2：未ソートリスト
            //lst0：検証対象のリスト
            let rec sort (lst1:structmember list) (lst2:structmember list) (lst0:structmember list) =
                //pに含まれる型名にp2に含まれていない型の構造体が入っているか検索
                let rec search1 (p1:(Etype*VarType*string)list) (p2:structmember list) res =
                    match p1 with
                    |(Structure(sname),_,_)::b when List.exists (fun (n:structmember)->sname=n.sname) p2 = false ->
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
            
        ///<summary>構造体定義のコードを作成</summary>
        member this.Def_Structure()=
            match p.lang with
            |F ->
                for s in this.sort() do
                    p.hwrite("type "+s.sname+"\n")
                    p.indentInc()
                    for i in 0..s.memlist.Length-1 do
                        let (typ,vtp,name) = s.memlist.[s.memlist.Length-1-i]
                        p.hwrite(p.indentSpace + p.declare(typ,vtp,name,"")+"\n")
                    p.indentDec()
                    p.hwrite("end type "+s.sname+"\n")
            |C ->
                for s in this.sort() do
                    p.hwrite("typedef struct "+"_"+s.sname+"\n")
                    p.hwrite("{"+"\n")
                    p.indentInc()
                    for i in 0..s.memlist.Length-1 do
                        let (typ,vtp,name) = s.memlist.[s.memlist.Length-1-i]
                        p.hwrite(p.indentSpace + p.declare(typ,vtp,name,"")+"\n")
                    p.indentDec()
                    p.hwrite("} "+s.sname+";\n")
            |T ->
                for s in this.sort() do
                    p.hwrite("\\subsection{"+s.sname+"}")
                    p.hwrite("\\begin{itemize}\n")
                    p.indentInc()
                    for i in 0..s.memlist.Length-1 do
                        let (typ,vtp,name) = s.memlist.[s.memlist.Length-1-i]
                        p.hwrite(p.indentSpace + p.declare(typ,vtp,name,"")+"\n")
                    p.indentDec()
                    p.hwrite("\\end{itemize}\n")
            |H ->
                for s in this.sort() do
                    p.hwrite("<h3>"+s.sname+"</h3>\n")
                    p.hwrite("<ul>\n")
                    p.indentInc()
                    for i in 0..s.memlist.Length-1 do
                        let (typ,vtp,name) = s.memlist.[s.memlist.Length-1-i]
                        p.hwrite(p.indentSpace + p.declare(typ,vtp,name,"")+"\n")
                    p.indentDec()
                    p.hwrite("</ul>\n")
        ///<summary>構造体メンバへのアクセス</summary>
        static member mem(vname,name) =
            match p.lang with
            |F   -> vname+"%"+name
            |C -> vname+"."+name
            |T   -> vname+"."+name
            |H   -> vname+"."+name
            
        member this.i0 (sname, vname, name) =
            this.addmember(sname,(It 4,A0,name))
            num0(It 4,Var(It 4,structure.mem(vname,name)))
        member this.d0 (sname, vname, name) =
            this.addmember(sname,(Dt,A0,name))
            num0(Dt,Var(Dt,structure.mem(vname,name)))
        member this.z0 (sname, vname, name) =
            this.addmember(sname,(Zt,A0,name))
            num0(Zt,Var(Zt,structure.mem(vname,name)))
        member this.i1 (sname, vname, name, size1) =
            this.addmember(sname,(It 4,A1(size1),name))
            this.addmember(sname,(It 4,A1(1),name+"_size"))
            num1(It 4,Var1(A1(size1),structure.mem(vname,name)))
        member this.d1 (sname, vname, name, size1) =
            this.addmember(sname,(Dt,A1(size1),name))
            this.addmember(sname,(It 4,A1(1),name+"_size"))
            num1(Dt,Var1(A1(size1),structure.mem(vname,name)))
        member this.z1 (sname, vname, name, size1) =
            this.addmember(sname,(Zt,A1(size1),name))
            this.addmember(sname,(It 4,A1(1),name+"_size"))
            num1(Zt,Var1(A1(size1),structure.mem(vname,name)))
        member this.i2 (sname, vname, name, size1, size2) =
            this.addmember(sname,(It 4,A2(size1,size2),name))
            this.addmember(sname,(It 4,A1(2),name+"_size"))
            num2(It 4,Var2(A2(size1,size2),structure.mem(vname,name)))
        member this.d2 (sname, vname, name, size1, size2) =
            this.addmember(sname,(Dt,A2(size1,size2),name))
            this.addmember(sname,(It 4,A1(2),name+"_size"))
            num2(Dt,Var2(A2(size1,size2),structure.mem(vname,name)))
        member this.z2 (sname, vname, name, size1, size2) =
            this.addmember(sname,(Zt,A2(size1,size2),name))
            this.addmember(sname,(It 4,A1(2),name+"_size"))
            num2(Zt,Var2(A2(size1,size2),structure.mem(vname,name)))
        member this.i3 (sname, vname, name, size1, size2, size3) =
            this.addmember(sname,(It 4,A3(size1,size2,size3),name))
            this.addmember(sname,(It 4,A1(3),name+"_size"))
            num3(It 4,Var3(A3(size1,size2,size3),structure.mem(vname,name)))
        member this.d3 (sname, vname, name, size1, size2, size3) =
            this.addmember(sname,(Dt,A3(size1,size2,size3),name))
            this.addmember(sname,(It 4,A1(3),name+"_size"))
            num3(Dt,Var3(A3(size1,size2,size3),structure.mem(vname,name)))
        member this.z3 (sname, vname, name, size1, size2, size3) =
            this.addmember(sname,(Zt,A3(size1,size2,size3),name))
            this.addmember(sname,(It 4,A1(3),name+"_size"))
            num3(Zt,Var3(A3(size1,size2,size3),structure.mem(vname,name)))
        member this.i1 (sname, vname, name) = 
            this.addmember(sname,(It 4,A1(0),name))
            this.addmember(sname,(It 4,A1(1),name+"_size"))
            num1(It 4,Var1(A1(0),structure.mem(vname,name)))
        member this.d1 (sname, vname, name) = 
            this.addmember(sname,(Dt,A1(0),name))
            this.addmember(sname,(It 4,A1(1),name+"_size"))
            num1(Dt,Var1(A1(0),structure.mem(vname,name)))
        member this.z1 (sname, vname, name) = 
            this.addmember(sname,(Zt,A1(0),name))
            this.addmember(sname,(It 4,A1(1),name+"_size"))
            num1(Zt,Var1(A1(0),structure.mem(vname,name)))
        member this.i2 (sname, vname, name) = 
            this.addmember(sname,(It 4,A2(0,0),name))
            this.addmember(sname,(It 4,A1(2),name+"_size"))
            num2(It 4,Var2(A2(0,0),structure.mem(vname,name)))
        member this.d2 (sname, vname, name) = 
            this.addmember(sname,(Dt,A2(0,0),name))
            this.addmember(sname,(It 4,A1(2),name+"_size"))
            num2(Dt,Var2(A2(0,0),structure.mem(vname,name)))
        member this.z2 (sname, vname, name) = 
            this.addmember(sname,(Zt,A2(0,0),name))
            this.addmember(sname,(It 4,A1(2),name+"_size"))
            num2(Zt,Var2(A2(0,0),structure.mem(vname,name)))
        member this.i3 (sname, vname, name) = 
            this.addmember(sname,(It 4,A3(0,0,0),name))
            this.addmember(sname,(It 4,A1(3),name+"_size"))
            num3(It 4,Var3(A3(0,0,0),structure.mem(vname,name)))
        member this.d3 (sname, vname, name) = 
            this.addmember(sname,(Dt,A3(0,0,0),name))
            this.addmember(sname,(It 4,A1(3),name+"_size"))
            num3(Dt,Var3(A3(0,0,0),structure.mem(vname,name)))
        member this.z3 (sname, vname, name) = 
            this.addmember(sname,(Zt,A3(0,0,0),name))
            this.addmember(sname,(It 4,A1(3),name+"_size"))
            num3(Zt,Var3(A3(0,0,0),structure.mem(vname,name)))
            
        member this.reg(sname,name:string) =
            let str_ac = match p.lang with |F -> "%" |C |T |H -> "."
            //構造体のメンバの場合はリスト登録不要
            if name.Contains(str_ac)=false then
                //構造体の定義を追加
                this.addstructure(sname)
                //構造体変数の宣言
                let name_ = match p.lang with |H -> "<mi mathvariant=\"italic\">"+name+"</mi>" |_ -> name
                p.var.setVar(Structure(sname),A0,name_,"")
            
        member this.reg(sname,name:string,size1) =
            let str_ac = match p.lang with |F -> "%" |C |T |H -> "."
            //構造体のメンバの場合はリスト登録不要
            if name.Contains(str_ac)=false then
                //構造体の定義を追加
                this.addstructure(sname)
                //構造体変数の宣言
                let name_ = match p.lang with |H -> "<mi mathvariant=\"italic\">"+name+"</mi>" |_ -> name
                p.var.setVar(Structure(sname),A1(size1),name_,"")
            
        member this.reg(sname,name:string,size1,size2) =
            let str_ac = match p.lang with |F -> "%" |C |T |H -> "."
            //構造体のメンバの場合はリスト登録不要
            if name.Contains(str_ac)=false then
                //構造体の定義を追加
                this.addstructure(sname)
                //構造体変数の宣言
                let name_ = match p.lang with |H -> "<mi mathvariant=\"italic\">"+name+"</mi>" |_ -> name
                p.var.setVar(Structure(sname),A2(size1,size2),name_,"")
            
        member this.reg(sname,name:string,size1,size2,size3) =
            let str_ac = match p.lang with |F -> "%" |C |T |H -> "."
            //構造体のメンバの場合はリスト登録不要
            if name.Contains(str_ac)=false then
                //構造体の定義を追加
                this.addstructure(sname)
                //構造体変数の宣言
                let name_ = match p.lang with |H -> "<mi mathvariant=\"italic\">"+name+"</mi>" |_ -> name
                p.var.setVar(Structure(sname),A3(size1,size2,size3),name_,"")
                