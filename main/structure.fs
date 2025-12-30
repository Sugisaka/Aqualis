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
        member __.addstructure(sname) =
            //構造体が未定義の場合はリストに追加
            if (strlist |> List.exists(fun s -> s.sname=sname)) = false then
                strlist <- structmember(sname)::strlist
        
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
        member this.Def_Structure(writer:codeWriter) =
            match pr.language with
            |Fortran ->
                for s in this.sort() do
                    writer.codewrite("type "+s.sname+"\n")
                    writer.indent.inc()
                    for i in 0..s.memlist.Length-1 do
                        let typ,vtp,name = s.memlist.[s.memlist.Length-1-i]
                        writer.codewrite(pr.var.declare(typ,vtp,name,"",pr.numFormat)+"\n")
                    writer.indent.dec()
                    writer.codewrite("end type "+s.sname+"\n")
            |C99 ->
                for s in this.sort() do
                    writer.codewrite("typedef struct "+"_"+s.sname+"\n")
                    writer.codewrite("{"+"\n")
                    writer.indent.inc()
                    for i in 0..s.memlist.Length-1 do
                        let typ,vtp,name = s.memlist.[s.memlist.Length-1-i]
                        writer.codewrite(pr.var.declare(typ,vtp,name,"",pr.numFormat)+"\n")
                    writer.indent.dec()
                    writer.codewrite("} "+s.sname+";\n")
            |LaTeX ->
                for s in this.sort() do
                    writer.codewrite("\\subsection{"+s.sname+"}")
                    writer.codewrite "\\begin{itemize}\n"
                    writer.indent.inc()
                    for i in 0..s.memlist.Length-1 do
                        let typ,vtp,name = s.memlist.[s.memlist.Length-1-i]
                        writer.codewrite(pr.var.declare(typ,vtp,name,"",pr.numFormat)+"\n")
                    writer.indent.dec()
                    writer.codewrite "\\end{itemize}\n"
            |HTML ->
                for s in this.sort() do
                    writer.codewrite("<h3>"+s.sname+"</h3>\n")
                    writer.codewrite "<ul>\n"
                    writer.indent.inc()
                    for i in 0..s.memlist.Length-1 do
                        let typ,vtp,name = s.memlist.[s.memlist.Length-1-i]
                        writer.codewrite(pr.var.declare(typ,vtp,name,"",pr.numFormat)+"\n")
                    writer.indent.dec()
                    writer.codewrite "</ul>\n"
            |Python ->
                for s in this.sort() do
                    writer.codewrite("class "+s.sname+":\n")
                    writer.indent.inc()
                    for i in 0..s.memlist.Length-1 do
                        let typ,vtp,name = s.memlist.[s.memlist.Length-1-i]
                        writer.codewrite(pr.var.declare(typ,vtp,name,"",pr.numFormat)+"\n")
                    writer.indent.dec()
            |JavaScript ->
                ()
            |PHP ->
                ()
            |Numeric ->
                ()
            
        ///<summary>構造体メンバへのアクセス</summary>
        static member mem(vname,name) =
            match pr.language with
            |Fortran    -> vname+"%"+name
            |C99        -> vname+"."+name
            |LaTeX      -> vname+"."+name
            |HTML       -> vname+"."+name
            |Python     -> vname+"."+name
            |JavaScript -> vname+"."+name
            |PHP        -> vname+"."+name
            |Numeric    -> vname+"."+name
            
        member this.i0 (sname, vname, name) =
            this.addmember(sname,(It 4,A0,name))
            num0(Var(It 4,structure.mem(vname,name),NaN))
        member this.d0 (sname, vname, name) =
            this.addmember(sname,(Dt,A0,name))
            num0(Var(Dt,structure.mem(vname,name),NaN))
        member this.z0 (sname, vname, name) =
            this.addmember(sname,(Zt,A0,name))
            num0(Var(Zt,structure.mem(vname,name),NaN))
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
            let str_ac = match pr.language with |Fortran -> "%" |C99 |LaTeX |HTML |Python |JavaScript |PHP |Numeric -> "."
            //構造体のメンバの場合はリスト登録不要
            if name.Contains(str_ac)=false then
                //構造体の定義を追加
                this.addstructure(sname)
                //構造体変数の宣言
                let name_ = match pr.language with |HTML -> "<mi mathvariant=\"italic\">"+name+"</mi>" |_ -> name
                pr.var.setVar(Structure(sname),A0,name_,"")
            
        member this.reg(sname,name:string,size1) =
            let str_ac = match pr.language with |Fortran -> "%" |C99 |LaTeX |HTML |Python |JavaScript |PHP |Numeric -> "."
            //構造体のメンバの場合はリスト登録不要
            if name.Contains(str_ac)=false then
                //構造体の定義を追加
                this.addstructure(sname)
                //構造体変数の宣言
                let name_ = match pr.language with |HTML -> "<mi mathvariant=\"italic\">"+name+"</mi>" |_ -> name
                pr.var.setVar(Structure(sname),A1(size1),name_,"")
            
        member this.reg(sname,name:string,size1,size2) =
            let str_ac = match pr.language with |Fortran -> "%" |C99 |LaTeX |HTML |Python |JavaScript |PHP |Numeric -> "."
            //構造体のメンバの場合はリスト登録不要
            if name.Contains(str_ac)=false then
                //構造体の定義を追加
                this.addstructure(sname)
                //構造体変数の宣言
                let name_ = match pr.language with |HTML -> "<mi mathvariant=\"italic\">"+name+"</mi>" |_ -> name
                pr.var.setVar(Structure(sname),A2(size1,size2),name_,"")
            
        member this.reg(sname,name:string,size1,size2,size3) =
            let str_ac = match pr.language with |Fortran -> "%" |C99 |LaTeX |HTML |Python |JavaScript |PHP |Numeric -> "."
            //構造体のメンバの場合はリスト登録不要
            if name.Contains(str_ac)=false then
                //構造体の定義を追加
                this.addstructure(sname)
                //構造体変数の宣言
                let name_ = match pr.language with |HTML -> "<mi mathvariant=\"italic\">"+name+"</mi>" |_ -> name
                pr.var.setVar(Structure(sname),A3(size1,size2,size3),name_,"")
                