namespace Aqualis
    
    ///<summary>3次元配列変数</summary>
    type Expr3 =
        ///<summary>変数</summary>
        |Var3 of (VarType*string)
        ///<summary>部分配列</summary>
        |Arx3 of (num0*num0*num0*((num0*num0*num0)->num0))
        
    ///<summary>3次元配列</summary>
    type base3 (typ:Etype,x:Expr3) =
        ///<summary>変数を作成しリストに追加</summary>
        new (typ,size,name,para) = 
            programList[prIndex].var.setVar(typ,size,name,para)
            base3(typ,Var3(size,name))
        ///<summary>変数を作成しリストに追加</summary>
        new(sname,size,name) =
            programList[prIndex].var.setVar(Structure sname,size,name,"")
            base3(Structure sname,Var3(size,name))
        member _.Expr with get() = x
        member _.code with get() =
            match x with
            |Var3(_,x) -> x
            |_ -> "Error(property .code)"
        ///<summary>変数の要素数</summary>
        member __.size1 
          with get() =
            match x with
            |Var3(_,name) -> 
                match programList[prIndex].language with 
                |Fortran ->
                    num0(Var(It 4,name+"_size(1)",NaN))
                |C99 ->
                    num0(Var(It 4,name+"_size[0]",NaN))
                |LaTeX ->
                    num0(Var(It 4,"\\mathcal{S}_1["+name+"]",NaN))
                |HTML ->
                    num0(Var(It 4,"\\mathcal{S}_1["+name+"]",NaN))
                |HTMLSequenceDiagram ->
                    num0(Var(It 4,"\\mathcal{S}_1["+name+"]",NaN))
                |Python ->
                    num0(Var(It 4,name+"_size[0]",NaN))
                |JavaScript ->
                    num0(Var(It 4,name+"_size[0]",NaN))
                |PHP ->
                    num0(Var(It 4,name+"_size[0]",NaN))
                |Numeric ->
                    num0 NaN
            |Arx3(s1,_,_,_) -> s1
        ///<summary>変数の要素数</summary>
        member __.size2 
          with get() =
            match x with
            |Var3(_,name) -> 
                match programList[prIndex].language with 
                |Fortran ->
                    num0(Var(It 4,name+"_size(2)",NaN))
                |C99 ->
                    num0(Var(It 4,name+"_size[1]",NaN))
                |LaTeX ->
                    num0(Var(It 4,"\\mathcal{S}_2["+name+"]",NaN))
                |HTML ->
                    num0(Var(It 4,"\\mathcal{S}_2["+name+"]",NaN))
                |HTMLSequenceDiagram ->
                    num0(Var(It 4,"\\mathcal{S}_2["+name+"]",NaN))
                |Python ->
                    num0(Var(It 4,name+"_size[1]",NaN))
                |JavaScript ->
                    num0(Var(It 4,name+"_size[1]",NaN))
                |PHP ->
                    num0(Var(It 4,name+"_size[1]",NaN))
                |Numeric ->
                    num0 NaN
            |Arx3(_,s2,_,_) -> s2
        ///<summary>変数の要素数</summary>
        member __.size3 
          with get() =
            match x with
            |Var3(_,name) -> 
                match programList[prIndex].language with 
                |Fortran ->
                    num0(Var(It 4,name+"_size(3)",NaN))
                |C99 ->
                    num0(Var(It 4,name+"_size[2]",NaN))
                |LaTeX ->
                    num0(Var(It 4,"\\mathcal{S}_3["+name+"]",NaN))
                |HTML ->
                    num0(Var(It 4,"\\mathcal{S}_3["+name+"]",NaN))
                |HTMLSequenceDiagram ->
                    num0(Var(It 4,"\\mathcal{S}_3["+name+"]",NaN))
                |Python ->
                    num0(Var(It 4,name+"_size[2]",NaN))
                |JavaScript ->
                    num0(Var(It 4,name+"_size[2]",NaN))
                |PHP ->
                    num0(Var(It 4,name+"_size[2]",NaN))
                |Numeric ->
                    num0 NaN
            |Arx3(_,_,s3,_) -> s3
        ///<summary>インデクサ</summary>
        member this.Idx3(i:num0,j:num0,k:num0) =
            if debug.debugMode then
                match x with
                |Var3(_,name) ->
                    error.inc()
                    comment("***debug array3 access check: "+error.ID+"*****************************")
                    br.if1 (Or [this.size1 .= -1; this.size2 .= -1; this.size3 .= -1]) <| fun () -> 
                        print.t ("ERROR"+error.ID+" array "+name+" is not allocated")
                    br.if1 (Or [i .< _0; this.size1 .<= i]) <| fun () ->
                        print.w <| "ERROR" + error.ID + " array " + name + " illegal access. index " ++ i ++ " is out of range (1:" ++ this.size1 ++ ")"
                    br.if1 (Or [j .< _0; this.size2 .<= j]) <| fun () ->
                        print.w <| "ERROR" + error.ID + " array " + name + " illegal access. index " ++ j ++ " is out of range (1:" ++ this.size2 ++ ")"
                    br.if1 (Or [k .< _0; this.size3 .<= k]) <| fun () ->
                        print.w <| "ERROR" + error.ID + " array " + name + " illegal access. index " ++ k ++ " is out of range (1:" ++ this.size3 ++ ")"
                    comment "****************************************************"
                |_ -> ()
            match x,language() with
            |Var3(_,name),Fortran -> num0(Idx3(typ,name,(i+1).Expr,(j+1).Expr,(k+1).Expr))
            |Var3(_,name),C99 -> num0(Idx1(typ,name,(i + j * this.size1 + k * this.size1 * this.size2).Expr))
            |Var3(_,name),_ -> num0(Idx3(typ,name,i.Expr,j.Expr,k.Expr))
            |Arx3(_,_,_,f),_ -> f (i,j,k)
            
        member this.Idx3(i:num0,j:num0,k:int) = this.Idx3(i,j,I k)
        member this.Idx3(i:num0,j:int,k:num0) = this.Idx3(i,I j,k)
        member this.Idx3(i:num0,j:int,k:int) = this.Idx3(i,I j,I k)
        member this.Idx3(i:int,j:num0,k:num0) = this.Idx3(I i,j,k)
        member this.Idx3(i:int,j:num0,k:int) = this.Idx3(I i,j,I k)
        member this.Idx3(i:int,j:int,k:num0) = this.Idx3(I i,I j,k)
        member this.Idx3(i:int,j:int,k:int) = this.Idx3(I i,I j,I k)
        member this.Idx3(i:num0,j:num0,(a3:num0,b3:num0)) = Arx1(b3-a3+_1,  fun k -> this.Idx3(i,j,k+a3))
        member this.Idx3(i:num0,j:num0,(a3:num0,b3:int)) = Arx1(b3-a3+_1,  fun k -> this.Idx3(i,j,k+a3))
        member this.Idx3(i:num0,j:num0,(a3:int,b3:num0)) = Arx1(b3-a3+_1,  fun k -> this.Idx3(i,j,k+a3))
        member this.Idx3(i:num0,j:num0,(a3:int,b3:int)) = Arx1(b3-a3+_1,  fun k -> this.Idx3(i,j,k+a3))
        member this.Idx3(i:num0,j:num0,_:unit) = Arx1(this.size3,  fun k -> this.Idx3(i,j,k))
        member this.Idx3(i:num0,j:int,(a3:num0,b3:num0)) = Arx1(b3-a3+_1,  fun k -> this.Idx3(i,I j,k+a3))
        member this.Idx3(i:num0,j:int,(a3:num0,b3:int)) = Arx1(b3-a3+_1,  fun k -> this.Idx3(i,I j,k+a3))
        member this.Idx3(i:num0,j:int,(a3:int,b3:num0)) = Arx1(b3-a3+_1,  fun k -> this.Idx3(i,I j,k+a3))
        member this.Idx3(i:num0,j:int,(a3:int,b3:int)) = Arx1(b3-a3+_1,  fun k -> this.Idx3(i,I j,k+a3))
        member this.Idx3(i:num0,j:int,_:unit) = Arx1(this.size3,  fun k -> this.Idx3(i,I j,k))
        member this.Idx3(i:num0,(a2:num0,b2:num0),k:num0) = Arx1(b2-a2+_1,  fun j -> this.Idx3(i,j+a2,k))
        member this.Idx3(i:num0,(a2:num0,b2:num0),k:int) = Arx1(b2-a2+_1,  fun j -> this.Idx3(i,j+a2,I k))
        member this.Idx3(i:num0,(a2:num0,b2:num0),(a3:num0,b3:num0)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(i,j+a2,k+a3))
        member this.Idx3(i:num0,(a2:num0,b2:num0),(a3:num0,b3:int)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(i,j+a2,k+a3))
        member this.Idx3(i:num0,(a2:num0,b2:num0),(a3:int,b3:num0)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(i,j+a2,k+a3))
        member this.Idx3(i:num0,(a2:num0,b2:num0),(a3:int,b3:int)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(i,j+a2,k+a3))
        member this.Idx3(i:num0,(a2:num0,b2:num0),_:unit) = Arx2(b2-a2+_1, this.size3,  fun (j,k) -> this.Idx3(i,j+a2,k))
        member this.Idx3(i:num0,(a2:num0,b2:int),k:num0) = Arx1(b2-a2+_1,  fun j -> this.Idx3(i,j+a2,k))
        member this.Idx3(i:num0,(a2:num0,b2:int),k:int) = Arx1(b2-a2+_1,  fun j -> this.Idx3(i,j+a2,I k))
        member this.Idx3(i:num0,(a2:num0,b2:int),(a3:num0,b3:num0)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(i,j+a2,k+a3))
        member this.Idx3(i:num0,(a2:num0,b2:int),(a3:num0,b3:int)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(i,j+a2,k+a3))
        member this.Idx3(i:num0,(a2:num0,b2:int),(a3:int,b3:num0)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(i,j+a2,k+a3))
        member this.Idx3(i:num0,(a2:num0,b2:int),(a3:int,b3:int)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(i,j+a2,k+a3))
        member this.Idx3(i:num0,(a2:num0,b2:int),_:unit) = Arx2(b2-a2+_1, this.size3,  fun (j,k) -> this.Idx3(i,j+a2,k))
        member this.Idx3(i:num0,(a2:int,b2:num0),k:num0) = Arx1(b2-a2+_1,  fun j -> this.Idx3(i,j+a2,k))
        member this.Idx3(i:num0,(a2:int,b2:num0),k:int) = Arx1(b2-a2+_1,  fun j -> this.Idx3(i,j+a2,I k))
        member this.Idx3(i:num0,(a2:int,b2:num0),(a3:num0,b3:num0)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(i,j+a2,k+a3))
        member this.Idx3(i:num0,(a2:int,b2:num0),(a3:num0,b3:int)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(i,j+a2,k+a3))
        member this.Idx3(i:num0,(a2:int,b2:num0),(a3:int,b3:num0)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(i,j+a2,k+a3))
        member this.Idx3(i:num0,(a2:int,b2:num0),(a3:int,b3:int)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(i,j+a2,k+a3))
        member this.Idx3(i:num0,(a2:int,b2:num0),_:unit) = Arx2(b2-a2+_1, this.size3,  fun (j,k) -> this.Idx3(i,j+a2,k))
        member this.Idx3(i:num0,(a2:int,b2:int),k:num0) = Arx1(b2-a2+_1,  fun j -> this.Idx3(i,j+a2,k))
        member this.Idx3(i:num0,(a2:int,b2:int),k:int) = Arx1(b2-a2+_1,  fun j -> this.Idx3(i,j+a2,I k))
        member this.Idx3(i:num0,(a2:int,b2:int),(a3:num0,b3:num0)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(i,j+a2,k+a3))
        member this.Idx3(i:num0,(a2:int,b2:int),(a3:num0,b3:int)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(i,j+a2,k+a3))
        member this.Idx3(i:num0,(a2:int,b2:int),(a3:int,b3:num0)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(i,j+a2,k+a3))
        member this.Idx3(i:num0,(a2:int,b2:int),(a3:int,b3:int)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(i,j+a2,k+a3))
        member this.Idx3(i:num0,(a2:int,b2:int),_:unit) = Arx2(b2-a2+_1, this.size3,  fun (j,k) -> this.Idx3(i,j+a2,k))
        member this.Idx3(i:num0,_:unit,k:num0) = Arx1(this.size2,  fun j -> this.Idx3(i,j,k))
        member this.Idx3(i:num0,_:unit,k:int) = Arx1(this.size2,  fun j -> this.Idx3(i,j,I k))
        member this.Idx3(i:num0,_:unit,(a3:num0,b3:num0)) = Arx2(this.size2, b3-a3+_1,  fun (j,k) -> this.Idx3(i,j,k+a3))
        member this.Idx3(i:num0,_:unit,(a3:num0,b3:int)) = Arx2(this.size2, b3-a3+_1,  fun (j,k) -> this.Idx3(i,j,k+a3))
        member this.Idx3(i:num0,_:unit,(a3:int,b3:num0)) = Arx2(this.size2, b3-a3+_1,  fun (j,k) -> this.Idx3(i,j,k+a3))
        member this.Idx3(i:num0,_:unit,(a3:int,b3:int)) = Arx2(this.size2, b3-a3+_1,  fun (j,k) -> this.Idx3(i,j,k+a3))
        member this.Idx3(i:num0,_:unit,_:unit) = Arx2(this.size2, this.size3,  fun (j,k) -> this.Idx3(i,j,k))
        member this.Idx3(i:int,j:num0,(a3:num0,b3:num0)) = Arx1(b3-a3+_1,  fun k -> this.Idx3(I i,j,k+a3))
        member this.Idx3(i:int,j:num0,(a3:num0,b3:int)) = Arx1(b3-a3+_1,  fun k -> this.Idx3(I i,j,k+a3))
        member this.Idx3(i:int,j:num0,(a3:int,b3:num0)) = Arx1(b3-a3+_1,  fun k -> this.Idx3(I i,j,k+a3))
        member this.Idx3(i:int,j:num0,(a3:int,b3:int)) = Arx1(b3-a3+_1,  fun k -> this.Idx3(I i,j,k+a3))
        member this.Idx3(i:int,j:num0,_:unit) = Arx1(this.size3,  fun k -> this.Idx3(I i,j,k))
        member this.Idx3(i:int,j:int,(a3:num0,b3:num0)) = Arx1(b3-a3+_1,  fun k -> this.Idx3(I i,I j,k+a3))
        member this.Idx3(i:int,j:int,(a3:num0,b3:int)) = Arx1(b3-a3+_1,  fun k -> this.Idx3(I i,I j,k+a3))
        member this.Idx3(i:int,j:int,(a3:int,b3:num0)) = Arx1(b3-a3+_1,  fun k -> this.Idx3(I i,I j,k+a3))
        member this.Idx3(i:int,j:int,(a3:int,b3:int)) = Arx1(b3-a3+_1,  fun k -> this.Idx3(I i,I j,k+a3))
        member this.Idx3(i:int,j:int,_:unit) = Arx1(this.size3,  fun k -> this.Idx3(I i,I j,k))
        member this.Idx3(i:int,(a2:num0,b2:num0),k:num0) = Arx1(b2-a2+_1,  fun j -> this.Idx3(I i,j+a2,k))
        member this.Idx3(i:int,(a2:num0,b2:num0),k:int) = Arx1(b2-a2+_1,  fun j -> this.Idx3(I i,j+a2,I k))
        member this.Idx3(i:int,(a2:num0,b2:num0),(a3:num0,b3:num0)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(I i,j+a2,k+a3))
        member this.Idx3(i:int,(a2:num0,b2:num0),(a3:num0,b3:int)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(I i,j+a2,k+a3))
        member this.Idx3(i:int,(a2:num0,b2:num0),(a3:int,b3:num0)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(I i,j+a2,k+a3))
        member this.Idx3(i:int,(a2:num0,b2:num0),(a3:int,b3:int)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(I i,j+a2,k+a3))
        member this.Idx3(i:int,(a2:num0,b2:num0),_:unit) = Arx2(b2-a2+_1, this.size3,  fun (j,k) -> this.Idx3(I i,j+a2,k))
        member this.Idx3(i:int,(a2:num0,b2:int),k:num0) = Arx1(b2-a2+_1,  fun j -> this.Idx3(I i,j+a2,k))
        member this.Idx3(i:int,(a2:num0,b2:int),k:int) = Arx1(b2-a2+_1,  fun j -> this.Idx3(I i,j+a2,I k))
        member this.Idx3(i:int,(a2:num0,b2:int),(a3:num0,b3:num0)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(I i,j+a2,k+a3))
        member this.Idx3(i:int,(a2:num0,b2:int),(a3:num0,b3:int)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(I i,j+a2,k+a3))
        member this.Idx3(i:int,(a2:num0,b2:int),(a3:int,b3:num0)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(I i,j+a2,k+a3))
        member this.Idx3(i:int,(a2:num0,b2:int),(a3:int,b3:int)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(I i,j+a2,k+a3))
        member this.Idx3(i:int,(a2:num0,b2:int),_:unit) = Arx2(b2-a2+_1, this.size3,  fun (j,k) -> this.Idx3(I i,j+a2,k))
        member this.Idx3(i:int,(a2:int,b2:num0),k:num0) = Arx1(b2-a2+_1,  fun j -> this.Idx3(I i,j+a2,k))
        member this.Idx3(i:int,(a2:int,b2:num0),k:int) = Arx1(b2-a2+_1,  fun j -> this.Idx3(I i,j+a2,I k))
        member this.Idx3(i:int,(a2:int,b2:num0),(a3:num0,b3:num0)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(I i,j+a2,k+a3))
        member this.Idx3(i:int,(a2:int,b2:num0),(a3:num0,b3:int)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(I i,j+a2,k+a3))
        member this.Idx3(i:int,(a2:int,b2:num0),(a3:int,b3:num0)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(I i,j+a2,k+a3))
        member this.Idx3(i:int,(a2:int,b2:num0),(a3:int,b3:int)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(I i,j+a2,k+a3))
        member this.Idx3(i:int,(a2:int,b2:num0),_:unit) = Arx2(b2-a2+_1, this.size3,  fun (j,k) -> this.Idx3(I i,j+a2,k))
        member this.Idx3(i:int,(a2:int,b2:int),k:num0) = Arx1(b2-a2+_1,  fun j -> this.Idx3(I i,j+a2,k))
        member this.Idx3(i:int,(a2:int,b2:int),k:int) = Arx1(b2-a2+_1,  fun j -> this.Idx3(I i,j+a2,I k))
        member this.Idx3(i:int,(a2:int,b2:int),(a3:num0,b3:num0)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(I i,j+a2,k+a3))
        member this.Idx3(i:int,(a2:int,b2:int),(a3:num0,b3:int)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(I i,j+a2,k+a3))
        member this.Idx3(i:int,(a2:int,b2:int),(a3:int,b3:num0)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(I i,j+a2,k+a3))
        member this.Idx3(i:int,(a2:int,b2:int),(a3:int,b3:int)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(I i,j+a2,k+a3))
        member this.Idx3(i:int,(a2:int,b2:int),_:unit) = Arx2(b2-a2+_1, this.size3,  fun (j,k) -> this.Idx3(I i,j+a2,k))
        member this.Idx3(i:int,_:unit,k:num0) = Arx1(this.size2,  fun j -> this.Idx3(I i,j,k))
        member this.Idx3(i:int,_:unit,k:int) = Arx1(this.size2,  fun j -> this.Idx3(I i,j,I k))
        member this.Idx3(i:int,_:unit,(a3:num0,b3:num0)) = Arx2(this.size2, b3-a3+_1,  fun (j,k) -> this.Idx3(I i,j,k+a3))
        member this.Idx3(i:int,_:unit,(a3:num0,b3:int)) = Arx2(this.size2, b3-a3+_1,  fun (j,k) -> this.Idx3(I i,j,k+a3))
        member this.Idx3(i:int,_:unit,(a3:int,b3:num0)) = Arx2(this.size2, b3-a3+_1,  fun (j,k) -> this.Idx3(I i,j,k+a3))
        member this.Idx3(i:int,_:unit,(a3:int,b3:int)) = Arx2(this.size2, b3-a3+_1,  fun (j,k) -> this.Idx3(I i,j,k+a3))
        member this.Idx3(i:int,_:unit,_:unit) = Arx2(this.size2, this.size3,  fun (j,k) -> this.Idx3(I i,j,k))
        member this.Idx3((a1:num0,b1:num0),j:num0,k:num0) = Arx1(b1-a1+_1,  fun i -> this.Idx3(i+a1,j,k))
        member this.Idx3((a1:num0,b1:num0),j:num0,k:int) = Arx1(b1-a1+_1,  fun i -> this.Idx3(i+a1,j,I k))
        member this.Idx3((a1:num0,b1:num0),j:num0,(a3:num0,b3:num0)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,j,k+a3))
        member this.Idx3((a1:num0,b1:num0),j:num0,(a3:num0,b3:int)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,j,k+a3))
        member this.Idx3((a1:num0,b1:num0),j:num0,(a3:int,b3:num0)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,j,k+a3))
        member this.Idx3((a1:num0,b1:num0),j:num0,(a3:int,b3:int)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,j,k+a3))
        member this.Idx3((a1:num0,b1:num0),j:num0,_:unit) = Arx2(b1-a1+_1, this.size3,  fun (i,k) -> this.Idx3(i+a1,j,k))
        member this.Idx3((a1:num0,b1:num0),j:int,k:num0) = Arx1(b1-a1+_1,  fun i -> this.Idx3(i+a1,I j,k))
        member this.Idx3((a1:num0,b1:num0),j:int,k:int) = Arx1(b1-a1+_1,  fun i -> this.Idx3(i+a1,I j,I k))
        member this.Idx3((a1:num0,b1:num0),j:int,(a3:num0,b3:num0)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,I j,k+a3))
        member this.Idx3((a1:num0,b1:num0),j:int,(a3:num0,b3:int)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,I j,k+a3))
        member this.Idx3((a1:num0,b1:num0),j:int,(a3:int,b3:num0)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,I j,k+a3))
        member this.Idx3((a1:num0,b1:num0),j:int,(a3:int,b3:int)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,I j,k+a3))
        member this.Idx3((a1:num0,b1:num0),j:int,_:unit) = Arx2(b1-a1+_1, this.size3,  fun (i,k) -> this.Idx3(i+a1,I j,k))
        member this.Idx3((a1:num0,b1:num0),(a2:num0,b2:num0),k:num0) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,k))
        member this.Idx3((a1:num0,b1:num0),(a2:num0,b2:num0),k:int) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,I k))
        member this.Idx3((a1:num0,b1:num0),(a2:num0,b2:num0),(a3:num0,b3:num0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:num0,b1:num0),(a2:num0,b2:num0),(a3:num0,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:num0,b1:num0),(a2:num0,b2:num0),(a3:int,b3:num0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:num0,b1:num0),(a2:num0,b2:num0),(a3:int,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:num0,b1:num0),(a2:num0,b2:num0),_:unit) = Arx3(b1-a1+_1, b2-a2+_1, this.size3,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k))
        member this.Idx3((a1:num0,b1:num0),(a2:num0,b2:int),k:num0) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,k))
        member this.Idx3((a1:num0,b1:num0),(a2:num0,b2:int),k:int) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,I k))
        member this.Idx3((a1:num0,b1:num0),(a2:num0,b2:int),(a3:num0,b3:num0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:num0,b1:num0),(a2:num0,b2:int),(a3:num0,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:num0,b1:num0),(a2:num0,b2:int),(a3:int,b3:num0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:num0,b1:num0),(a2:num0,b2:int),(a3:int,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:num0,b1:num0),(a2:num0,b2:int),_:unit) = Arx3(b1-a1+_1, b2-a2+_1, this.size3,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k))
        member this.Idx3((a1:num0,b1:num0),(a2:int,b2:num0),k:num0) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,k))
        member this.Idx3((a1:num0,b1:num0),(a2:int,b2:num0),k:int) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,I k))
        member this.Idx3((a1:num0,b1:num0),(a2:int,b2:num0),(a3:num0,b3:num0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:num0,b1:num0),(a2:int,b2:num0),(a3:num0,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:num0,b1:num0),(a2:int,b2:num0),(a3:int,b3:num0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:num0,b1:num0),(a2:int,b2:num0),(a3:int,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:num0,b1:num0),(a2:int,b2:num0),_:unit) = Arx3(b1-a1+_1, b2-a2+_1, this.size3,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k))
        member this.Idx3((a1:num0,b1:num0),(a2:int,b2:int),k:num0) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,k))
        member this.Idx3((a1:num0,b1:num0),(a2:int,b2:int),k:int) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,I k))
        member this.Idx3((a1:num0,b1:num0),(a2:int,b2:int),(a3:num0,b3:num0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:num0,b1:num0),(a2:int,b2:int),(a3:num0,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:num0,b1:num0),(a2:int,b2:int),(a3:int,b3:num0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:num0,b1:num0),(a2:int,b2:int),(a3:int,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:num0,b1:num0),(a2:int,b2:int),_:unit) = Arx3(b1-a1+_1, b2-a2+_1, this.size3,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k))
        member this.Idx3((a1:num0,b1:num0),_:unit,k:num0) = Arx2(b1-a1+_1, this.size2,  fun (i,j) -> this.Idx3(i+a1,j,k))
        member this.Idx3((a1:num0,b1:num0),_:unit,k:int) = Arx2(b1-a1+_1, this.size2,  fun (i,j) -> this.Idx3(i+a1,j,I k))
        member this.Idx3((a1:num0,b1:num0),_:unit,(a3:num0,b3:num0)) = Arx3(b1-a1+_1, this.size2, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j,k+a3))
        member this.Idx3((a1:num0,b1:num0),_:unit,(a3:num0,b3:int)) = Arx3(b1-a1+_1, this.size2, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j,k+a3))
        member this.Idx3((a1:num0,b1:num0),_:unit,(a3:int,b3:num0)) = Arx3(b1-a1+_1, this.size2, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j,k+a3))
        member this.Idx3((a1:num0,b1:num0),_:unit,(a3:int,b3:int)) = Arx3(b1-a1+_1, this.size2, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j,k+a3))
        member this.Idx3((a1:num0,b1:num0),_:unit,_:unit) = Arx3(b1-a1+_1, this.size2, this.size3,  fun (i,j,k) -> this.Idx3(i+a1,j,k))
        member this.Idx3((a1:num0,b1:int),j:num0,k:num0) = Arx1(b1-a1+_1,  fun i -> this.Idx3(i+a1,j,k))
        member this.Idx3((a1:num0,b1:int),j:num0,k:int) = Arx1(b1-a1+_1,  fun i -> this.Idx3(i+a1,j,I k))
        member this.Idx3((a1:num0,b1:int),j:num0,(a3:num0,b3:num0)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,j,k+a3))
        member this.Idx3((a1:num0,b1:int),j:num0,(a3:num0,b3:int)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,j,k+a3))
        member this.Idx3((a1:num0,b1:int),j:num0,(a3:int,b3:num0)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,j,k+a3))
        member this.Idx3((a1:num0,b1:int),j:num0,(a3:int,b3:int)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,j,k+a3))
        member this.Idx3((a1:num0,b1:int),j:num0,_:unit) = Arx2(b1-a1+_1, this.size3,  fun (i,k) -> this.Idx3(i+a1,j,k))
        member this.Idx3((a1:num0,b1:int),j:int,k:num0) = Arx1(b1-a1+_1,  fun i -> this.Idx3(i+a1,I j,k))
        member this.Idx3((a1:num0,b1:int),j:int,k:int) = Arx1(b1-a1+_1,  fun i -> this.Idx3(i+a1,I j,I k))
        member this.Idx3((a1:num0,b1:int),j:int,(a3:num0,b3:num0)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,I j,k+a3))
        member this.Idx3((a1:num0,b1:int),j:int,(a3:num0,b3:int)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,I j,k+a3))
        member this.Idx3((a1:num0,b1:int),j:int,(a3:int,b3:num0)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,I j,k+a3))
        member this.Idx3((a1:num0,b1:int),j:int,(a3:int,b3:int)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,I j,k+a3))
        member this.Idx3((a1:num0,b1:int),j:int,_:unit) = Arx2(b1-a1+_1, this.size3,  fun (i,k) -> this.Idx3(i+a1,I j,k))
        member this.Idx3((a1:num0,b1:int),(a2:num0,b2:num0),k:num0) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,k))
        member this.Idx3((a1:num0,b1:int),(a2:num0,b2:num0),k:int) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,I k))
        member this.Idx3((a1:num0,b1:int),(a2:num0,b2:num0),(a3:num0,b3:num0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:num0,b1:int),(a2:num0,b2:num0),(a3:num0,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:num0,b1:int),(a2:num0,b2:num0),(a3:int,b3:num0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:num0,b1:int),(a2:num0,b2:num0),(a3:int,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:num0,b1:int),(a2:num0,b2:num0),_:unit) = Arx3(b1-a1+_1, b2-a2+_1, this.size3,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k))
        member this.Idx3((a1:num0,b1:int),(a2:num0,b2:int),k:num0) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,k))
        member this.Idx3((a1:num0,b1:int),(a2:num0,b2:int),k:int) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,I k))
        member this.Idx3((a1:num0,b1:int),(a2:num0,b2:int),(a3:num0,b3:num0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:num0,b1:int),(a2:num0,b2:int),(a3:num0,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:num0,b1:int),(a2:num0,b2:int),(a3:int,b3:num0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:num0,b1:int),(a2:num0,b2:int),(a3:int,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:num0,b1:int),(a2:num0,b2:int),_:unit) = Arx3(b1-a1+_1, b2-a2+_1, this.size3,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k))
        member this.Idx3((a1:num0,b1:int),(a2:int,b2:num0),k:num0) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,k))
        member this.Idx3((a1:num0,b1:int),(a2:int,b2:num0),k:int) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,I k))
        member this.Idx3((a1:num0,b1:int),(a2:int,b2:num0),(a3:num0,b3:num0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:num0,b1:int),(a2:int,b2:num0),(a3:num0,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:num0,b1:int),(a2:int,b2:num0),(a3:int,b3:num0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:num0,b1:int),(a2:int,b2:num0),(a3:int,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:num0,b1:int),(a2:int,b2:num0),_:unit) = Arx3(b1-a1+_1, b2-a2+_1, this.size3,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k))
        member this.Idx3((a1:num0,b1:int),(a2:int,b2:int),k:num0) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,k))
        member this.Idx3((a1:num0,b1:int),(a2:int,b2:int),k:int) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,I k))
        member this.Idx3((a1:num0,b1:int),(a2:int,b2:int),(a3:num0,b3:num0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:num0,b1:int),(a2:int,b2:int),(a3:num0,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:num0,b1:int),(a2:int,b2:int),(a3:int,b3:num0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:num0,b1:int),(a2:int,b2:int),(a3:int,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:num0,b1:int),(a2:int,b2:int),_:unit) = Arx3(b1-a1+_1, b2-a2+_1, this.size3,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k))
        member this.Idx3((a1:num0,b1:int),_:unit,k:num0) = Arx2(b1-a1+_1, this.size2,  fun (i,j) -> this.Idx3(i+a1,j,k))
        member this.Idx3((a1:num0,b1:int),_:unit,k:int) = Arx2(b1-a1+_1, this.size2,  fun (i,j) -> this.Idx3(i+a1,j,I k))
        member this.Idx3((a1:num0,b1:int),_:unit,(a3:num0,b3:num0)) = Arx3(b1-a1+_1, this.size2, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j,k+a3))
        member this.Idx3((a1:num0,b1:int),_:unit,(a3:num0,b3:int)) = Arx3(b1-a1+_1, this.size2, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j,k+a3))
        member this.Idx3((a1:num0,b1:int),_:unit,(a3:int,b3:num0)) = Arx3(b1-a1+_1, this.size2, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j,k+a3))
        member this.Idx3((a1:num0,b1:int),_:unit,(a3:int,b3:int)) = Arx3(b1-a1+_1, this.size2, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j,k+a3))
        member this.Idx3((a1:num0,b1:int),_:unit,_:unit) = Arx3(b1-a1+_1, this.size2, this.size3,  fun (i,j,k) -> this.Idx3(i+a1,j,k))
        member this.Idx3((a1:int,b1:num0),j:num0,k:num0) = Arx1(b1-a1+_1,  fun i -> this.Idx3(i+a1,j,k))
        member this.Idx3((a1:int,b1:num0),j:num0,k:int) = Arx1(b1-a1+_1,  fun i -> this.Idx3(i+a1,j,I k))
        member this.Idx3((a1:int,b1:num0),j:num0,(a3:num0,b3:num0)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,j,k+a3))
        member this.Idx3((a1:int,b1:num0),j:num0,(a3:num0,b3:int)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,j,k+a3))
        member this.Idx3((a1:int,b1:num0),j:num0,(a3:int,b3:num0)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,j,k+a3))
        member this.Idx3((a1:int,b1:num0),j:num0,(a3:int,b3:int)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,j,k+a3))
        member this.Idx3((a1:int,b1:num0),j:num0,_:unit) = Arx2(b1-a1+_1, this.size3,  fun (i,k) -> this.Idx3(i+a1,j,k))
        member this.Idx3((a1:int,b1:num0),j:int,k:num0) = Arx1(b1-a1+_1,  fun i -> this.Idx3(i+a1,I j,k))
        member this.Idx3((a1:int,b1:num0),j:int,k:int) = Arx1(b1-a1+_1,  fun i -> this.Idx3(i+a1,I j,I k))
        member this.Idx3((a1:int,b1:num0),j:int,(a3:num0,b3:num0)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,I j,k+a3))
        member this.Idx3((a1:int,b1:num0),j:int,(a3:num0,b3:int)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,I j,k+a3))
        member this.Idx3((a1:int,b1:num0),j:int,(a3:int,b3:num0)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,I j,k+a3))
        member this.Idx3((a1:int,b1:num0),j:int,(a3:int,b3:int)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,I j,k+a3))
        member this.Idx3((a1:int,b1:num0),j:int,_:unit) = Arx2(b1-a1+_1, this.size3,  fun (i,k) -> this.Idx3(i+a1,I j,k))
        member this.Idx3((a1:int,b1:num0),(a2:num0,b2:num0),k:num0) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,k))
        member this.Idx3((a1:int,b1:num0),(a2:num0,b2:num0),k:int) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,I k))
        member this.Idx3((a1:int,b1:num0),(a2:num0,b2:num0),(a3:num0,b3:num0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int,b1:num0),(a2:num0,b2:num0),(a3:num0,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int,b1:num0),(a2:num0,b2:num0),(a3:int,b3:num0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int,b1:num0),(a2:num0,b2:num0),(a3:int,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int,b1:num0),(a2:num0,b2:num0),_:unit) = Arx3(b1-a1+_1, b2-a2+_1, this.size3,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k))
        member this.Idx3((a1:int,b1:num0),(a2:num0,b2:int),k:num0) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,k))
        member this.Idx3((a1:int,b1:num0),(a2:num0,b2:int),k:int) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,I k))
        member this.Idx3((a1:int,b1:num0),(a2:num0,b2:int),(a3:num0,b3:num0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int,b1:num0),(a2:num0,b2:int),(a3:num0,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int,b1:num0),(a2:num0,b2:int),(a3:int,b3:num0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int,b1:num0),(a2:num0,b2:int),(a3:int,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int,b1:num0),(a2:num0,b2:int),_:unit) = Arx3(b1-a1+_1, b2-a2+_1, this.size3,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k))
        member this.Idx3((a1:int,b1:num0),(a2:int,b2:num0),k:num0) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,k))
        member this.Idx3((a1:int,b1:num0),(a2:int,b2:num0),k:int) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,I k))
        member this.Idx3((a1:int,b1:num0),(a2:int,b2:num0),(a3:num0,b3:num0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int,b1:num0),(a2:int,b2:num0),(a3:num0,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int,b1:num0),(a2:int,b2:num0),(a3:int,b3:num0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int,b1:num0),(a2:int,b2:num0),(a3:int,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int,b1:num0),(a2:int,b2:num0),_:unit) = Arx3(b1-a1+_1, b2-a2+_1, this.size3,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k))
        member this.Idx3((a1:int,b1:num0),(a2:int,b2:int),k:num0) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,k))
        member this.Idx3((a1:int,b1:num0),(a2:int,b2:int),k:int) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,I k))
        member this.Idx3((a1:int,b1:num0),(a2:int,b2:int),(a3:num0,b3:num0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int,b1:num0),(a2:int,b2:int),(a3:num0,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int,b1:num0),(a2:int,b2:int),(a3:int,b3:num0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int,b1:num0),(a2:int,b2:int),(a3:int,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int,b1:num0),(a2:int,b2:int),_:unit) = Arx3(b1-a1+_1, b2-a2+_1, this.size3,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k))
        member this.Idx3((a1:int,b1:num0),_:unit,k:num0) = Arx2(b1-a1+_1, this.size2,  fun (i,j) -> this.Idx3(i+a1,j,k))
        member this.Idx3((a1:int,b1:num0),_:unit,k:int) = Arx2(b1-a1+_1, this.size2,  fun (i,j) -> this.Idx3(i+a1,j,I k))
        member this.Idx3((a1:int,b1:num0),_:unit,(a3:num0,b3:num0)) = Arx3(b1-a1+_1, this.size2, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j,k+a3))
        member this.Idx3((a1:int,b1:num0),_:unit,(a3:num0,b3:int)) = Arx3(b1-a1+_1, this.size2, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j,k+a3))
        member this.Idx3((a1:int,b1:num0),_:unit,(a3:int,b3:num0)) = Arx3(b1-a1+_1, this.size2, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j,k+a3))
        member this.Idx3((a1:int,b1:num0),_:unit,(a3:int,b3:int)) = Arx3(b1-a1+_1, this.size2, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j,k+a3))
        member this.Idx3((a1:int,b1:num0),_:unit,_:unit) = Arx3(b1-a1+_1, this.size2, this.size3,  fun (i,j,k) -> this.Idx3(i+a1,j,k))
        member this.Idx3((a1:int,b1:int),j:num0,k:num0) = Arx1(b1-a1+_1,  fun i -> this.Idx3(i+a1,j,k))
        member this.Idx3((a1:int,b1:int),j:num0,k:int) = Arx1(b1-a1+_1,  fun i -> this.Idx3(i+a1,j,I k))
        member this.Idx3((a1:int,b1:int),j:num0,(a3:num0,b3:num0)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,j,k+a3))
        member this.Idx3((a1:int,b1:int),j:num0,(a3:num0,b3:int)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,j,k+a3))
        member this.Idx3((a1:int,b1:int),j:num0,(a3:int,b3:num0)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,j,k+a3))
        member this.Idx3((a1:int,b1:int),j:num0,(a3:int,b3:int)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,j,k+a3))
        member this.Idx3((a1:int,b1:int),j:num0,_:unit) = Arx2(b1-a1+_1, this.size3,  fun (i,k) -> this.Idx3(i+a1,j,k))
        member this.Idx3((a1:int,b1:int),j:int,k:num0) = Arx1(b1-a1+_1,  fun i -> this.Idx3(i+a1,I j,k))
        member this.Idx3((a1:int,b1:int),j:int,k:int) = Arx1(b1-a1+_1,  fun i -> this.Idx3(i+a1,I j,I k))
        member this.Idx3((a1:int,b1:int),j:int,(a3:num0,b3:num0)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,I j,k+a3))
        member this.Idx3((a1:int,b1:int),j:int,(a3:num0,b3:int)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,I j,k+a3))
        member this.Idx3((a1:int,b1:int),j:int,(a3:int,b3:num0)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,I j,k+a3))
        member this.Idx3((a1:int,b1:int),j:int,(a3:int,b3:int)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,I j,k+a3))
        member this.Idx3((a1:int,b1:int),j:int,_:unit) = Arx2(b1-a1+_1, this.size3,  fun (i,k) -> this.Idx3(i+a1,I j,k))
        member this.Idx3((a1:int,b1:int),(a2:num0,b2:num0),k:num0) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,k))
        member this.Idx3((a1:int,b1:int),(a2:num0,b2:num0),k:int) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,I k))
        member this.Idx3((a1:int,b1:int),(a2:num0,b2:num0),(a3:num0,b3:num0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int,b1:int),(a2:num0,b2:num0),(a3:num0,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int,b1:int),(a2:num0,b2:num0),(a3:int,b3:num0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int,b1:int),(a2:num0,b2:num0),(a3:int,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int,b1:int),(a2:num0,b2:num0),_:unit) = Arx3(b1-a1+_1, b2-a2+_1, this.size3,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k))
        member this.Idx3((a1:int,b1:int),(a2:num0,b2:int),k:num0) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,k))
        member this.Idx3((a1:int,b1:int),(a2:num0,b2:int),k:int) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,I k))
        member this.Idx3((a1:int,b1:int),(a2:num0,b2:int),(a3:num0,b3:num0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int,b1:int),(a2:num0,b2:int),(a3:num0,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int,b1:int),(a2:num0,b2:int),(a3:int,b3:num0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int,b1:int),(a2:num0,b2:int),(a3:int,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int,b1:int),(a2:num0,b2:int),_:unit) = Arx3(b1-a1+_1, b2-a2+_1, this.size3,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k))
        member this.Idx3((a1:int,b1:int),(a2:int,b2:num0),k:num0) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,k))
        member this.Idx3((a1:int,b1:int),(a2:int,b2:num0),k:int) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,I k))
        member this.Idx3((a1:int,b1:int),(a2:int,b2:num0),(a3:num0,b3:num0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int,b1:int),(a2:int,b2:num0),(a3:num0,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int,b1:int),(a2:int,b2:num0),(a3:int,b3:num0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int,b1:int),(a2:int,b2:num0),(a3:int,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int,b1:int),(a2:int,b2:num0),_:unit) = Arx3(b1-a1+_1, b2-a2+_1, this.size3,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k))
        member this.Idx3((a1:int,b1:int),(a2:int,b2:int),k:num0) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,k))
        member this.Idx3((a1:int,b1:int),(a2:int,b2:int),k:int) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,I k))
        member this.Idx3((a1:int,b1:int),(a2:int,b2:int),(a3:num0,b3:num0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int,b1:int),(a2:int,b2:int),(a3:num0,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int,b1:int),(a2:int,b2:int),(a3:int,b3:num0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int,b1:int),(a2:int,b2:int),(a3:int,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int,b1:int),(a2:int,b2:int),_:unit) = Arx3(b1-a1+_1, b2-a2+_1, this.size3,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k))
        member this.Idx3((a1:int,b1:int),_:unit,k:num0) = Arx2(b1-a1+_1, this.size2,  fun (i,j) -> this.Idx3(i+a1,j,k))
        member this.Idx3((a1:int,b1:int),_:unit,k:int) = Arx2(b1-a1+_1, this.size2,  fun (i,j) -> this.Idx3(i+a1,j,I k))
        member this.Idx3((a1:int,b1:int),_:unit,(a3:num0,b3:num0)) = Arx3(b1-a1+_1, this.size2, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j,k+a3))
        member this.Idx3((a1:int,b1:int),_:unit,(a3:num0,b3:int)) = Arx3(b1-a1+_1, this.size2, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j,k+a3))
        member this.Idx3((a1:int,b1:int),_:unit,(a3:int,b3:num0)) = Arx3(b1-a1+_1, this.size2, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j,k+a3))
        member this.Idx3((a1:int,b1:int),_:unit,(a3:int,b3:int)) = Arx3(b1-a1+_1, this.size2, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j,k+a3))
        member this.Idx3((a1:int,b1:int),_:unit,_:unit) = Arx3(b1-a1+_1, this.size2, this.size3,  fun (i,j,k) -> this.Idx3(i+a1,j,k))
        member this.Idx3(_:unit,j:num0,k:num0) = Arx1(this.size1,  fun i -> this.Idx3(i,j,k))
        member this.Idx3(_:unit,j:num0,k:int) = Arx1(this.size1,  fun i -> this.Idx3(i,j,I k))
        member this.Idx3(_:unit,j:num0,(a3:num0,b3:num0)) = Arx2(this.size1, b3-a3+_1,  fun (i,k) -> this.Idx3(i,j,k+a3))
        member this.Idx3(_:unit,j:num0,(a3:num0,b3:int)) = Arx2(this.size1, b3-a3+_1,  fun (i,k) -> this.Idx3(i,j,k+a3))
        member this.Idx3(_:unit,j:num0,(a3:int,b3:num0)) = Arx2(this.size1, b3-a3+_1,  fun (i,k) -> this.Idx3(i,j,k+a3))
        member this.Idx3(_:unit,j:num0,(a3:int,b3:int)) = Arx2(this.size1, b3-a3+_1,  fun (i,k) -> this.Idx3(i,j,k+a3))
        member this.Idx3(_:unit,j:num0,_:unit) = Arx2(this.size1, this.size3,  fun (i,k) -> this.Idx3(i,j,k))
        member this.Idx3(_:unit,j:int,k:num0) = Arx1(this.size1,  fun i -> this.Idx3(i,I j,k))
        member this.Idx3(_:unit,j:int,k:int) = Arx1(this.size1,  fun i -> this.Idx3(i,I j,I k))
        member this.Idx3(_:unit,j:int,(a3:num0,b3:num0)) = Arx2(this.size1, b3-a3+_1,  fun (i,k) -> this.Idx3(i,I j,k+a3))
        member this.Idx3(_:unit,j:int,(a3:num0,b3:int)) = Arx2(this.size1, b3-a3+_1,  fun (i,k) -> this.Idx3(i,I j,k+a3))
        member this.Idx3(_:unit,j:int,(a3:int,b3:num0)) = Arx2(this.size1, b3-a3+_1,  fun (i,k) -> this.Idx3(i,I j,k+a3))
        member this.Idx3(_:unit,j:int,(a3:int,b3:int)) = Arx2(this.size1, b3-a3+_1,  fun (i,k) -> this.Idx3(i,I j,k+a3))
        member this.Idx3(_:unit,j:int,_:unit) = Arx2(this.size1, this.size3,  fun (i,k) -> this.Idx3(i,I j,k))
        member this.Idx3(_:unit,(a2:num0,b2:num0),k:num0) = Arx2(this.size1, b2-a2+_1,  fun (i,j) -> this.Idx3(i,j+a2,k))
        member this.Idx3(_:unit,(a2:num0,b2:num0),k:int) = Arx2(this.size1, b2-a2+_1,  fun (i,j) -> this.Idx3(i,j+a2,I k))
        member this.Idx3(_:unit,(a2:num0,b2:num0),(a3:num0,b3:num0)) = Arx3(this.size1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i,j+a2,k+a3))
        member this.Idx3(_:unit,(a2:num0,b2:num0),(a3:num0,b3:int)) = Arx3(this.size1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i,j+a2,k+a3))
        member this.Idx3(_:unit,(a2:num0,b2:num0),(a3:int,b3:num0)) = Arx3(this.size1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i,j+a2,k+a3))
        member this.Idx3(_:unit,(a2:num0,b2:num0),(a3:int,b3:int)) = Arx3(this.size1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i,j+a2,k+a3))
        member this.Idx3(_:unit,(a2:num0,b2:num0),_:unit) = Arx3(this.size1, b2-a2+_1, this.size3,  fun (i,j,k) -> this.Idx3(i,j+a2,k))
        member this.Idx3(_:unit,(a2:num0,b2:int),k:num0) = Arx2(this.size1, b2-a2+_1,  fun (i,j) -> this.Idx3(i,j+a2,k))
        member this.Idx3(_:unit,(a2:num0,b2:int),k:int) = Arx2(this.size1, b2-a2+_1,  fun (i,j) -> this.Idx3(i,j+a2,I k))
        member this.Idx3(_:unit,(a2:num0,b2:int),(a3:num0,b3:num0)) = Arx3(this.size1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i,j+a2,k+a3))
        member this.Idx3(_:unit,(a2:num0,b2:int),(a3:num0,b3:int)) = Arx3(this.size1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i,j+a2,k+a3))
        member this.Idx3(_:unit,(a2:num0,b2:int),(a3:int,b3:num0)) = Arx3(this.size1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i,j+a2,k+a3))
        member this.Idx3(_:unit,(a2:num0,b2:int),(a3:int,b3:int)) = Arx3(this.size1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i,j+a2,k+a3))
        member this.Idx3(_:unit,(a2:num0,b2:int),_:unit) = Arx3(this.size1, b2-a2+_1, this.size3,  fun (i,j,k) -> this.Idx3(i,j+a2,k))
        member this.Idx3(_:unit,(a2:int,b2:num0),k:num0) = Arx2(this.size1, b2-a2+_1,  fun (i,j) -> this.Idx3(i,j+a2,k))
        member this.Idx3(_:unit,(a2:int,b2:num0),k:int) = Arx2(this.size1, b2-a2+_1,  fun (i,j) -> this.Idx3(i,j+a2,I k))
        member this.Idx3(_:unit,(a2:int,b2:num0),(a3:num0,b3:num0)) = Arx3(this.size1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i,j+a2,k+a3))
        member this.Idx3(_:unit,(a2:int,b2:num0),(a3:num0,b3:int)) = Arx3(this.size1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i,j+a2,k+a3))
        member this.Idx3(_:unit,(a2:int,b2:num0),(a3:int,b3:num0)) = Arx3(this.size1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i,j+a2,k+a3))
        member this.Idx3(_:unit,(a2:int,b2:num0),(a3:int,b3:int)) = Arx3(this.size1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i,j+a2,k+a3))
        member this.Idx3(_:unit,(a2:int,b2:num0),_:unit) = Arx3(this.size1, b2-a2+_1, this.size3,  fun (i,j,k) -> this.Idx3(i,j+a2,k))
        member this.Idx3(_:unit,(a2:int,b2:int),k:num0) = Arx2(this.size1, b2-a2+_1,  fun (i,j) -> this.Idx3(i,j+a2,k))
        member this.Idx3(_:unit,(a2:int,b2:int),k:int) = Arx2(this.size1, b2-a2+_1,  fun (i,j) -> this.Idx3(i,j+a2,I k))
        member this.Idx3(_:unit,(a2:int,b2:int),(a3:num0,b3:num0)) = Arx3(this.size1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i,j+a2,k+a3))
        member this.Idx3(_:unit,(a2:int,b2:int),(a3:num0,b3:int)) = Arx3(this.size1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i,j+a2,k+a3))
        member this.Idx3(_:unit,(a2:int,b2:int),(a3:int,b3:num0)) = Arx3(this.size1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i,j+a2,k+a3))
        member this.Idx3(_:unit,(a2:int,b2:int),(a3:int,b3:int)) = Arx3(this.size1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i,j+a2,k+a3))
        member this.Idx3(_:unit,(a2:int,b2:int),_:unit) = Arx3(this.size1, b2-a2+_1, this.size3,  fun (i,j,k) -> this.Idx3(i,j+a2,k))
        member this.Idx3(_:unit,_:unit,k:num0) = Arx2(this.size1, this.size2,  fun (i,j) -> this.Idx3(i,j,k))
        member this.Idx3(_:unit,_:unit,k:int) = Arx2(this.size1, this.size2,  fun (i,j) -> this.Idx3(i,j,I k))
        member this.Idx3(_:unit,_:unit,(a3:num0,b3:num0)) = Arx3(this.size1, this.size2, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i,j,k+a3))
        member this.Idx3(_:unit,_:unit,(a3:num0,b3:int)) = Arx3(this.size1, this.size2, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i,j,k+a3))
        member this.Idx3(_:unit,_:unit,(a3:int,b3:num0)) = Arx3(this.size1, this.size2, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i,j,k+a3))
        member this.Idx3(_:unit,_:unit,(a3:int,b3:int)) = Arx3(this.size1, this.size2, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i,j,k+a3))
        
        ///<summary>配列のメモリ割り当て</summary>
        member this.allocate(n1:num0,n2:num0,n3:num0) =
                match x with
                |Var3(size,name) ->
                    if debug.debugMode then
                        error.inc()
                        comment("***debug array1 allocate check: "+error.ID+"*****************************")
                        br.branch <| fun b ->
                            b.IF (this.size1 .=/ -1) <| fun () ->
                                print.t ("ERROR"+error.ID+" array "+name+" is already allocated")
                        comment "****************************************************"
                    match programList[prIndex].language with
                    |Fortran ->
                        match size with
                        |A3(0,0,0) ->
                            this.size1 <== n1
                            this.size2 <== n2
                            this.size3 <== n3
                            codewritein("allocate("+name+"(1:"+this.size1.Expr.eval (programList[prIndex])+",1:"+this.size2.Expr.eval (programList[prIndex])+",1:"+this.size3.Expr.eval (programList[prIndex])+")"+")"+"\n")
                        |_ -> 
                            codewritein("(Error:055-001 「"+name+"」は可変長3次元配列ではありません")
                    |C99 ->
                        match size with
                        |A3(0,0,0) ->
                            this.size1 <== n1
                            this.size2 <== n2
                            this.size3 <== n3
                            codewritein(name+" = "+"("+typ.tostring(language())+" *)"+"malloc("+"sizeof("+typ.tostring(language())+")*"+this.size1.Expr.eval (programList[prIndex])+"*"+this.size2.Expr.eval (programList[prIndex])+"*"+this.size3.Expr.eval (programList[prIndex])+");\n")
                        |_ -> 
                            codewritein("(Error:055-001 「"+name+"」は可変長3次元配列ではありません")
                    |LaTeX ->
                        match size,typ with
                        |A3(0,0,0),It _ ->
                            codewritein("$"+name+" \\in \\mathbb{Z}^{"+n1.Expr.eval (programList[prIndex])+"\\times"+n2.Expr.eval (programList[prIndex])+"\\times"+n3.Expr.eval (programList[prIndex])+"}$\\\\\n")
                        |A3(0,0,0),Dt   ->
                            codewritein("$"+name+" \\in \\mathbb{R}^{"+n1.Expr.eval (programList[prIndex])+"\\times"+n2.Expr.eval (programList[prIndex])+"\\times"+n3.Expr.eval (programList[prIndex])+"}$\\\\\n")
                        |A3(0,0,0),Zt   ->
                            codewritein("$"+name+" \\in \\mathbb{C}^{"+n1.Expr.eval (programList[prIndex])+"\\times"+n2.Expr.eval (programList[prIndex])+"\\times"+n3.Expr.eval (programList[prIndex])+"}$\\\\\n")
                        |_ -> 
                            codewritein("(Error:055-001 「"+name+"」は可変長3次元配列ではありません")
                    |HTML ->
                        match size,typ with
                        |A3(0,0,0),It _ ->
                            codewritein("\\("+name+" \\in \\mathbb{Z}^{"+n1.Expr.eval (programList[prIndex])+"\\times"+n2.Expr.eval (programList[prIndex])+"\\times"+n3.Expr.eval (programList[prIndex])+"}\\)<br>\n")
                        |A3(0,0,0),Dt   ->
                            codewritein("\\("+name+" \\in \\mathbb{R}^{"+n1.Expr.eval (programList[prIndex])+"\\times"+n2.Expr.eval (programList[prIndex])+"\\times"+n3.Expr.eval (programList[prIndex])+"}\\)<br>\n")
                        |A3(0,0,0),Zt   ->
                            codewritein("\\("+name+" \\in \\mathbb{C}^{"+n1.Expr.eval (programList[prIndex])+"\\times"+n2.Expr.eval (programList[prIndex])+"\\times"+n3.Expr.eval (programList[prIndex])+"}\\)<br>\n")
                        |_ -> 
                            codewritein("(Error:055-001 「"+name+"」は可変長3次元配列ではありません")
                    |HTMLSequenceDiagram ->
                        match size,typ with
                        |A3(0,0,0),It _ ->
                            codewritein("\\("+name+" \\in \\mathbb{Z}^{"+n1.Expr.eval (programList[prIndex])+"\\times"+n2.Expr.eval (programList[prIndex])+"\\times"+n3.Expr.eval (programList[prIndex])+"}\\)<br>\n")
                        |A3(0,0,0),Dt   ->
                            codewritein("\\("+name+" \\in \\mathbb{R}^{"+n1.Expr.eval (programList[prIndex])+"\\times"+n2.Expr.eval (programList[prIndex])+"\\times"+n3.Expr.eval (programList[prIndex])+"}\\)<br>\n")
                        |A3(0,0,0),Zt   ->
                            codewritein("\\("+name+" \\in \\mathbb{C}^{"+n1.Expr.eval (programList[prIndex])+"\\times"+n2.Expr.eval (programList[prIndex])+"\\times"+n3.Expr.eval (programList[prIndex])+"}\\)<br>\n")
                        |_ -> 
                            codewritein("(Error:055-001 「"+name+"」は可変長3次元配列ではありません")
                    |Python ->
                        match size with
                        |A3(0,0,0) ->
                            this.size1 <== n1
                            this.size2 <== n2
                            this.size3 <== n3
                            match typ with
                            |Structure sname -> codewritein(name+" = numpy.array([[["+sname+"() for _ in range(int("+this.size3.Expr.eval (programList[prIndex])+"))] for _ in range(int("+this.size2.Expr.eval (programList[prIndex])+"))] for _ in range(int("+this.size1.Expr.eval (programList[prIndex])+"))], dtype=object).reshape(int("+this.size1.Expr.eval (programList[prIndex])+"),int("+this.size2.Expr.eval (programList[prIndex])+"),int("+this.size3.Expr.eval (programList[prIndex])+"))\n")
                            |It _ |It 1      -> codewritein(name+" = numpy.zeros("+this.size1.Expr.eval (programList[prIndex])+"*"+this.size2.Expr.eval (programList[prIndex])+"*"+this.size3.Expr.eval (programList[prIndex])+", dtype=int).reshape("+this.size1.Expr.eval (programList[prIndex])+","+this.size2.Expr.eval (programList[prIndex])+","+this.size3.Expr.eval (programList[prIndex])+")"+"\n")
                            |Zt              -> codewritein(name+" = numpy.zeros("+this.size1.Expr.eval (programList[prIndex])+"*"+this.size2.Expr.eval (programList[prIndex])+"*"+this.size3.Expr.eval (programList[prIndex])+", dtype=numpy.complex128).reshape("+this.size1.Expr.eval (programList[prIndex])+","+this.size2.Expr.eval (programList[prIndex])+","+this.size3.Expr.eval (programList[prIndex])+")"+"\n")
                            |_               -> codewritein(name+" = numpy.zeros("+this.size1.Expr.eval (programList[prIndex])+"*"+this.size2.Expr.eval (programList[prIndex])+"*"+this.size3.Expr.eval (programList[prIndex])+").reshape("+this.size1.Expr.eval (programList[prIndex])+","+this.size2.Expr.eval (programList[prIndex])+","+this.size3.Expr.eval (programList[prIndex])+")"+"\n")
                        |_ -> 
                            codewritein("(Error:055-001 「"+name+"」は可変長3次元配列ではありません")
                    |JavaScript ->
                        match size with
                        |A3(0,0,0) ->
                            this.size1 <== n1
                            this.size2 <== n2
                            this.size3 <== n3
                            codewritein(name+" = "+"Array("+this.size1.Expr.eval (programList[prIndex])+"*"+this.size2.Expr.eval (programList[prIndex])+"*"+this.size3.Expr.eval (programList[prIndex])+");\n")
                        |_ -> 
                            codewritein("(Error:055-001 「"+name+"」は可変長3次元配列ではありません")
                    |PHP ->
                        match size with
                        |A3(0,0,0) ->
                            this.size1 <== n1
                            this.size2 <== n2
                            this.size3 <== n3
                            codewritein(name+" = [];\n")
                        |_ -> 
                            codewritein("(Error:055-001 「"+name+"」は可変長3次元配列ではありません")
                    |Numeric ->
                        ()
                |_ -> ()
                
        member this.allocate(n1:int,n2:num0,n3:num0) = this.allocate(I n1,n2,n3)
        member this.allocate(n1:num0,n2:int,n3:num0) = this.allocate(n1,I n2,n3)
        member this.allocate(n1:num0,n2:num0,n3:int) = this.allocate(n1,n2,I n3)
        member this.allocate(n1:int,n2:int,n3:num0) = this.allocate(I n1,I n2,n3)
        member this.allocate(n1:int,n2:num0,n3:int) = this.allocate(I n1,n2,I n3)
        member this.allocate(n1:num0,n2:int,n3:int) = this.allocate(n1,I n2,I n3)
        member this.allocate(n1:int,n2:int,n3:int) = this.allocate(I n1,I n2,I n3)
        
        ///<summary>配列のメモリ割り当て</summary>
        member this.deallocate() =
            if debug.debugMode then
                match x with
                |Var3(_,name) ->
                    error.inc()
                    comment("***debug array1 deallocate check: "+error.ID+"*****************************")
                    br.branch <| fun b ->
                        b.IF (this.size1 .= -1) <| fun () ->
                            print.t ("ERROR"+error.ID+" cannot deallocate array "+name)
                    comment "****************************************************"
                |_ -> ()
            match x with
            |Var3(size,name) ->
                match programList[prIndex].language with
                |Fortran ->
                    match size with
                    |A3(0,0,0) ->
                        this.size1 <== -1
                        this.size2 <== -1
                        this.size3 <== -1
                        codewritein("deallocate("+name+")"+"\n")
                    |_ -> ()
                |C99 ->
                    match size with
                    |A3(0,0,0) ->
                        this.size1 <== -1
                        this.size2 <== -1
                        this.size3 <== -1
                        codewritein("free("+name+");"+"\n")
                    |_ -> ()
                |LaTeX ->
                    match size with
                    |A3(0,0,0) ->
                        codewritein("deallocate($"+name+"$)\\\\\n")
                    |_ -> ()
                |HTML ->
                    match size with
                    |A3(0,0,0) ->
                        codewritein("\\("+name+"\\): deallocate<br/>\n")
                    |_ -> ()
                |HTMLSequenceDiagram ->
                    match size with
                    |A3(0,0,0) ->
                        codewritein("\\("+name+"\\): deallocate<br/>\n")
                    |_ -> ()
                |Python ->
                    match size with
                    |A3(0,0,0) ->
                        this.size1 <== -1
                        this.size2 <== -1
                        this.size3 <== -1
                        codewritein("del "+name+""+"\n")
                    |_ -> ()
                |JavaScript ->
                    match size with
                    |A3(0,0,0) ->
                        this.size1 <== -1
                        codewritein(name+"= null;"+"\n")
                    |_ -> ()
                |PHP ->
                    match size with
                    |A3(0,0,0) ->
                        this.size1 <== -1
                        codewritein("unset("+name+");"+"\n")
                    |_ -> ()
                |Numeric ->
                    ()
            |_ -> ()
            
        ///<summary>配列のクリア</summary>
        abstract member clear: unit -> unit
        default __.clear() = 
            printfn "WARNING: abstract clear method"
            
        ///<summary>配列サイズの初期化</summary>
        abstract member sizeinit: unit -> unit
        default __.sizeinit() = 
            printfn "WARNING: abstract sizeinit method"
            
        ///<summary>配列の全要素に対する処理</summary>
        member this.foreach code =
            iter.num this.size1 <| fun i -> 
                iter.num this.size2 <| fun j -> 
                    iter.num this.size3 <| fun k -> 
                        code(i,j,k)
                
        ///<summary>配列の全要素に対する処理</summary>
        member this.foreach_exit code =
            iter.num_exit this.size1 <| fun (ext1,i) -> 
                iter.num_exit this.size2 <| fun (ext2,j) -> 
                    iter.num_exit this.size2 <| fun (ext3,k) -> 
                        code(ext1,ext2,ext3,i,j,k)
                        
        static member sizeMismatchError(v1:base3,v2:base3) =
            if debug.debugMode then
                error.inc()
                comment("***debug array1 access check: "+error.ID+"*****************************")
                br.if1 (v1.size1 .=/ v2.size1) <| fun () -> 
                    print.t ("ERROR"+error.ID+" operator '<==' array size1 mismatch")
                br.if1 (v1.size2 .=/ v2.size2) <| fun () -> 
                    print.t ("ERROR"+error.ID+" operator '<==' array size2 mismatch")
                br.if1 (v1.size3 .=/ v2.size3) <| fun () -> 
                    print.t ("ERROR"+error.ID+" operator '<==' array size3 mismatch")
                comment "****************************************************"
                
    ///<summary>数値型1次元配列</summary>
    type num3 (typ:Etype,x:Expr3) =
        inherit base3(typ,x)
        new (typ,size,name,para) =
            programList[prIndex].var.setVar(typ,size,name,para)
            num3(typ,Var3(size,name))
        member this.etype with get() = typ
        member this.Item with get(i:num0,j:num0,k:num0) = this.Idx3(i,j,k)
        member this.Item with get(i:num0,j:num0,k:int) = this.Idx3(i,j,I k)
        member this.Item with get(i:num0,j:int,k:num0) = this.Idx3(i,I j,k)
        member this.Item with get(i:num0,j:int,k:int) = this.Idx3(i,I j,I k)
        member this.Item with get(i:int,j:num0,k:num0) = this.Idx3(I i,j,k)
        member this.Item with get(i:int,j:num0,k:int) = this.Idx3(I i,j,I k)
        member this.Item with get(i:int,j:int,k:num0) = this.Idx3(I i,I j,k)
        member this.Item with get(i:int,j:int,k:int) = this.Idx3(I i,I j,I k)
        member this.Item with get(i:num0,j:num0,(a3:num0,b3:num0)) = num1(typ,this.Idx3(i,j,(a3,b3)))
        member this.Item with get(i:num0,j:num0,(a3:num0,b3:int)) = num1(typ,this.Idx3(i,j,(a3,I b3)))
        member this.Item with get(i:num0,j:num0,(a3:int,b3:num0)) = num1(typ,this.Idx3(i,j,(I a3,b3)))
        member this.Item with get(i:num0,j:num0,(a3:int,b3:int)) = num1(typ,this.Idx3(i,j,(I a3,I b3)))
        member this.Item with get(i:num0,j:num0,_:unit) = num1(typ,this.Idx3(i,j,()))
        member this.Item with get(i:num0,j:int,(a3:num0,b3:num0)) = num1(typ,this.Idx3(i,I j,(a3,b3)))
        member this.Item with get(i:num0,j:int,(a3:num0,b3:int)) = num1(typ,this.Idx3(i,I j,(a3,I b3)))
        member this.Item with get(i:num0,j:int,(a3:int,b3:num0)) = num1(typ,this.Idx3(i,I j,(I a3,b3)))
        member this.Item with get(i:num0,j:int,(a3:int,b3:int)) = num1(typ,this.Idx3(i,I j,(I a3,I b3)))
        member this.Item with get(i:num0,j:int,_:unit) = num1(typ,this.Idx3(i,I j,()))
        member this.Item with get(i:num0,(a2:num0,b2:num0),k:num0) = num1(typ,this.Idx3(i,(a2,b2),k))
        member this.Item with get(i:num0,(a2:num0,b2:num0),k:int) = num1(typ,this.Idx3(i,(a2,b2),I k))
        member this.Item with get(i:num0,(a2:num0,b2:num0),(a3:num0,b3:num0)) = num2(typ,this.Idx3(i,(a2,b2),(a3,b3)))
        member this.Item with get(i:num0,(a2:num0,b2:num0),(a3:num0,b3:int)) = num2(typ,this.Idx3(i,(a2,b2),(a3,I b3)))
        member this.Item with get(i:num0,(a2:num0,b2:num0),(a3:int,b3:num0)) = num2(typ,this.Idx3(i,(a2,b2),(I a3,b3)))
        member this.Item with get(i:num0,(a2:num0,b2:num0),(a3:int,b3:int)) = num2(typ,this.Idx3(i,(a2,b2),(I a3,I b3)))
        member this.Item with get(i:num0,(a2:num0,b2:num0),_:unit) = num2(typ,this.Idx3(i,(a2,b2),()))
        member this.Item with get(i:num0,(a2:num0,b2:int),k:num0) = num1(typ,this.Idx3(i,(a2,I b2),k))
        member this.Item with get(i:num0,(a2:num0,b2:int),k:int) = num1(typ,this.Idx3(i,(a2,I b2),I k))
        member this.Item with get(i:num0,(a2:num0,b2:int),(a3:num0,b3:num0)) = num2(typ,this.Idx3(i,(a2,I b2),(a3,b3)))
        member this.Item with get(i:num0,(a2:num0,b2:int),(a3:num0,b3:int)) = num2(typ,this.Idx3(i,(a2,I b2),(a3,I b3)))
        member this.Item with get(i:num0,(a2:num0,b2:int),(a3:int,b3:num0)) = num2(typ,this.Idx3(i,(a2,I b2),(I a3,b3)))
        member this.Item with get(i:num0,(a2:num0,b2:int),(a3:int,b3:int)) = num2(typ,this.Idx3(i,(a2,I b2),(I a3,I b3)))
        member this.Item with get(i:num0,(a2:num0,b2:int),_:unit) = num2(typ,this.Idx3(i,(a2,I b2),()))
        member this.Item with get(i:num0,(a2:int,b2:num0),k:num0) = num1(typ,this.Idx3(i,(I a2,b2),k))
        member this.Item with get(i:num0,(a2:int,b2:num0),k:int) = num1(typ,this.Idx3(i,(I a2,b2),I k))
        member this.Item with get(i:num0,(a2:int,b2:num0),(a3:num0,b3:num0)) = num2(typ,this.Idx3(i,(I a2,b2),(a3,b3)))
        member this.Item with get(i:num0,(a2:int,b2:num0),(a3:num0,b3:int)) = num2(typ,this.Idx3(i,(I a2,b2),(a3,I b3)))
        member this.Item with get(i:num0,(a2:int,b2:num0),(a3:int,b3:num0)) = num2(typ,this.Idx3(i,(I a2,b2),(I a3,b3)))
        member this.Item with get(i:num0,(a2:int,b2:num0),(a3:int,b3:int)) = num2(typ,this.Idx3(i,(I a2,b2),(I a3,I b3)))
        member this.Item with get(i:num0,(a2:int,b2:num0),_:unit) = num2(typ,this.Idx3(i,(I a2,b2),()))
        member this.Item with get(i:num0,(a2:int,b2:int),k:num0) = num1(typ,this.Idx3(i,(I a2,I b2),k))
        member this.Item with get(i:num0,(a2:int,b2:int),k:int) = num1(typ,this.Idx3(i,(I a2,I b2),I k))
        member this.Item with get(i:num0,(a2:int,b2:int),(a3:num0,b3:num0)) = num2(typ,this.Idx3(i,(I a2,I b2),(a3,b3)))
        member this.Item with get(i:num0,(a2:int,b2:int),(a3:num0,b3:int)) = num2(typ,this.Idx3(i,(I a2,I b2),(a3,I b3)))
        member this.Item with get(i:num0,(a2:int,b2:int),(a3:int,b3:num0)) = num2(typ,this.Idx3(i,(I a2,I b2),(I a3,b3)))
        member this.Item with get(i:num0,(a2:int,b2:int),(a3:int,b3:int)) = num2(typ,this.Idx3(i,(I a2,I b2),(I a3,I b3)))
        member this.Item with get(i:num0,(a2:int,b2:int),_:unit) = num2(typ,this.Idx3(i,(I a2,I b2),()))
        member this.Item with get(i:num0,_:unit,k:num0) = num1(typ,this.Idx3(i,(),k))
        member this.Item with get(i:num0,_:unit,k:int) = num1(typ,this.Idx3(i,(),I k))
        member this.Item with get(i:num0,_:unit,(a3:num0,b3:num0)) = num2(typ,this.Idx3(i,(),(a3,b3)))
        member this.Item with get(i:num0,_:unit,(a3:num0,b3:int)) = num2(typ,this.Idx3(i,(),(a3,I b3)))
        member this.Item with get(i:num0,_:unit,(a3:int,b3:num0)) = num2(typ,this.Idx3(i,(),(I a3,b3)))
        member this.Item with get(i:num0,_:unit,(a3:int,b3:int)) = num2(typ,this.Idx3(i,(),(I a3,I b3)))
        member this.Item with get(i:num0,_:unit,_:unit) = num2(typ,this.Idx3(i,(),()))
        member this.Item with get(i:int,j:num0,(a3:num0,b3:num0)) = num1(typ,this.Idx3(I i,j,(a3,b3)))
        member this.Item with get(i:int,j:num0,(a3:num0,b3:int)) = num1(typ,this.Idx3(I i,j,(a3,I b3)))
        member this.Item with get(i:int,j:num0,(a3:int,b3:num0)) = num1(typ,this.Idx3(I i,j,(I a3,b3)))
        member this.Item with get(i:int,j:num0,(a3:int,b3:int)) = num1(typ,this.Idx3(I i,j,(I a3,I b3)))
        member this.Item with get(i:int,j:num0,_:unit) = num1(typ,this.Idx3(I i,j,()))
        member this.Item with get(i:int,j:int,(a3:num0,b3:num0)) = num1(typ,this.Idx3(I i,I j,(a3,b3)))
        member this.Item with get(i:int,j:int,(a3:num0,b3:int)) = num1(typ,this.Idx3(I i,I j,(a3,I b3)))
        member this.Item with get(i:int,j:int,(a3:int,b3:num0)) = num1(typ,this.Idx3(I i,I j,(I a3,b3)))
        member this.Item with get(i:int,j:int,(a3:int,b3:int)) = num1(typ,this.Idx3(I i,I j,(I a3,I b3)))
        member this.Item with get(i:int,j:int,_:unit) = num1(typ,this.Idx3(I i,I j,()))
        member this.Item with get(i:int,(a2:num0,b2:num0),k:num0) = num1(typ,this.Idx3(I i,(a2,b2),k))
        member this.Item with get(i:int,(a2:num0,b2:num0),k:int) = num1(typ,this.Idx3(I i,(a2,b2),I k))
        member this.Item with get(i:int,(a2:num0,b2:num0),(a3:num0,b3:num0)) = num2(typ,this.Idx3(I i,(a2,b2),(a3,b3)))
        member this.Item with get(i:int,(a2:num0,b2:num0),(a3:num0,b3:int)) = num2(typ,this.Idx3(I i,(a2,b2),(a3,I b3)))
        member this.Item with get(i:int,(a2:num0,b2:num0),(a3:int,b3:num0)) = num2(typ,this.Idx3(I i,(a2,b2),(I a3,b3)))
        member this.Item with get(i:int,(a2:num0,b2:num0),(a3:int,b3:int)) = num2(typ,this.Idx3(I i,(a2,b2),(I a3,I b3)))
        member this.Item with get(i:int,(a2:num0,b2:num0),_:unit) = num2(typ,this.Idx3(I i,(a2,b2),()))
        member this.Item with get(i:int,(a2:num0,b2:int),k:num0) = num1(typ,this.Idx3(I i,(a2,I b2),k))
        member this.Item with get(i:int,(a2:num0,b2:int),k:int) = num1(typ,this.Idx3(I i,(a2,I b2),I k))
        member this.Item with get(i:int,(a2:num0,b2:int),(a3:num0,b3:num0)) = num2(typ,this.Idx3(I i,(a2,I b2),(a3,b3)))
        member this.Item with get(i:int,(a2:num0,b2:int),(a3:num0,b3:int)) = num2(typ,this.Idx3(I i,(a2,I b2),(a3,I b3)))
        member this.Item with get(i:int,(a2:num0,b2:int),(a3:int,b3:num0)) = num2(typ,this.Idx3(I i,(a2,I b2),(I a3,b3)))
        member this.Item with get(i:int,(a2:num0,b2:int),(a3:int,b3:int)) = num2(typ,this.Idx3(I i,(a2,I b2),(I a3,I b3)))
        member this.Item with get(i:int,(a2:num0,b2:int),_:unit) = num2(typ,this.Idx3(I i,(a2,I b2),()))
        member this.Item with get(i:int,(a2:int,b2:num0),k:num0) = num1(typ,this.Idx3(I i,(I a2,b2),k))
        member this.Item with get(i:int,(a2:int,b2:num0),k:int) = num1(typ,this.Idx3(I i,(I a2,b2),I k))
        member this.Item with get(i:int,(a2:int,b2:num0),(a3:num0,b3:num0)) = num2(typ,this.Idx3(I i,(I a2,b2),(a3,b3)))
        member this.Item with get(i:int,(a2:int,b2:num0),(a3:num0,b3:int)) = num2(typ,this.Idx3(I i,(I a2,b2),(a3,I b3)))
        member this.Item with get(i:int,(a2:int,b2:num0),(a3:int,b3:num0)) = num2(typ,this.Idx3(I i,(I a2,b2),(I a3,b3)))
        member this.Item with get(i:int,(a2:int,b2:num0),(a3:int,b3:int)) = num2(typ,this.Idx3(I i,(I a2,b2),(I a3,I b3)))
        member this.Item with get(i:int,(a2:int,b2:num0),_:unit) = num2(typ,this.Idx3(I i,(I a2,b2),()))
        member this.Item with get(i:int,(a2:int,b2:int),k:num0) = num1(typ,this.Idx3(I i,(I a2,I b2),k))
        member this.Item with get(i:int,(a2:int,b2:int),k:int) = num1(typ,this.Idx3(I i,(I a2,I b2),I k))
        member this.Item with get(i:int,(a2:int,b2:int),(a3:num0,b3:num0)) = num2(typ,this.Idx3(I i,(I a2,I b2),(a3,b3)))
        member this.Item with get(i:int,(a2:int,b2:int),(a3:num0,b3:int)) = num2(typ,this.Idx3(I i,(I a2,I b2),(a3,I b3)))
        member this.Item with get(i:int,(a2:int,b2:int),(a3:int,b3:num0)) = num2(typ,this.Idx3(I i,(I a2,I b2),(I a3,b3)))
        member this.Item with get(i:int,(a2:int,b2:int),(a3:int,b3:int)) = num2(typ,this.Idx3(I i,(I a2,I b2),(I a3,I b3)))
        member this.Item with get(i:int,(a2:int,b2:int),_:unit) = num2(typ,this.Idx3(I i,(I a2,I b2),()))
        member this.Item with get(i:int,_:unit,k:num0) = num1(typ,this.Idx3(I i,(),k))
        member this.Item with get(i:int,_:unit,k:int) = num1(typ,this.Idx3(I i,(),I k))
        member this.Item with get(i:int,_:unit,(a3:num0,b3:num0)) = num2(typ,this.Idx3(I i,(),(a3,b3)))
        member this.Item with get(i:int,_:unit,(a3:num0,b3:int)) = num2(typ,this.Idx3(I i,(),(a3,I b3)))
        member this.Item with get(i:int,_:unit,(a3:int,b3:num0)) = num2(typ,this.Idx3(I i,(),(I a3,b3)))
        member this.Item with get(i:int,_:unit,(a3:int,b3:int)) = num2(typ,this.Idx3(I i,(),(I a3,I b3)))
        member this.Item with get(i:int,_:unit,_:unit) = num2(typ,this.Idx3(I i,(),()))
        member this.Item with get((a1:num0,b1:num0),j:num0,k:num0) = num1(typ,this.Idx3((a1,b1),j,k))
        member this.Item with get((a1:num0,b1:num0),j:num0,k:int) = num1(typ,this.Idx3((a1,b1),j,I k))
        member this.Item with get((a1:num0,b1:num0),j:num0,(a3:num0,b3:num0)) = num2(typ,this.Idx3((a1,b1),j,(a3,b3)))
        member this.Item with get((a1:num0,b1:num0),j:num0,(a3:num0,b3:int)) = num2(typ,this.Idx3((a1,b1),j,(a3,I b3)))
        member this.Item with get((a1:num0,b1:num0),j:num0,(a3:int,b3:num0)) = num2(typ,this.Idx3((a1,b1),j,(I a3,b3)))
        member this.Item with get((a1:num0,b1:num0),j:num0,(a3:int,b3:int)) = num2(typ,this.Idx3((a1,b1),j,(I a3,I b3)))
        member this.Item with get((a1:num0,b1:num0),j:num0,_:unit) = num2(typ,this.Idx3((a1,b1),j,()))
        member this.Item with get((a1:num0,b1:num0),j:int,k:num0) = num1(typ,this.Idx3((a1,b1),I j,k))
        member this.Item with get((a1:num0,b1:num0),j:int,k:int) = num1(typ,this.Idx3((a1,b1),I j,I k))
        member this.Item with get((a1:num0,b1:num0),j:int,(a3:num0,b3:num0)) = num2(typ,this.Idx3((a1,b1),I j,(a3,b3)))
        member this.Item with get((a1:num0,b1:num0),j:int,(a3:num0,b3:int)) = num2(typ,this.Idx3((a1,b1),I j,(a3,I b3)))
        member this.Item with get((a1:num0,b1:num0),j:int,(a3:int,b3:num0)) = num2(typ,this.Idx3((a1,b1),I j,(I a3,b3)))
        member this.Item with get((a1:num0,b1:num0),j:int,(a3:int,b3:int)) = num2(typ,this.Idx3((a1,b1),I j,(I a3,I b3)))
        member this.Item with get((a1:num0,b1:num0),j:int,_:unit) = num2(typ,this.Idx3((a1,b1),I j,()))
        member this.Item with get((a1:num0,b1:num0),(a2:num0,b2:num0),k:num0) = num2(typ,this.Idx3((a1,b1),(a2,b2),k))
        member this.Item with get((a1:num0,b1:num0),(a2:num0,b2:num0),k:int) = num2(typ,this.Idx3((a1,b1),(a2,b2),I k))
        member this.Item with get((a1:num0,b1:num0),(a2:num0,b2:num0),(a3:num0,b3:num0)) = num3(typ,this.Idx3((a1,b1),(a2,b2),(a3,b3)))
        member this.Item with get((a1:num0,b1:num0),(a2:num0,b2:num0),(a3:num0,b3:int)) = num3(typ,this.Idx3((a1,b1),(a2,b2),(a3,I b3)))
        member this.Item with get((a1:num0,b1:num0),(a2:num0,b2:num0),(a3:int,b3:num0)) = num3(typ,this.Idx3((a1,b1),(a2,b2),(I a3,b3)))
        member this.Item with get((a1:num0,b1:num0),(a2:num0,b2:num0),(a3:int,b3:int)) = num3(typ,this.Idx3((a1,b1),(a2,b2),(I a3,I b3)))
        member this.Item with get((a1:num0,b1:num0),(a2:num0,b2:num0),_:unit) = num3(typ,this.Idx3((a1,b1),(a2,b2),()))
        member this.Item with get((a1:num0,b1:num0),(a2:num0,b2:int),k:num0) = num2(typ,this.Idx3((a1,b1),(a2,I b2),k))
        member this.Item with get((a1:num0,b1:num0),(a2:num0,b2:int),k:int) = num2(typ,this.Idx3((a1,b1),(a2,I b2),I k))
        member this.Item with get((a1:num0,b1:num0),(a2:num0,b2:int),(a3:num0,b3:num0)) = num3(typ,this.Idx3((a1,b1),(a2,I b2),(a3,b3)))
        member this.Item with get((a1:num0,b1:num0),(a2:num0,b2:int),(a3:num0,b3:int)) = num3(typ,this.Idx3((a1,b1),(a2,I b2),(a3,I b3)))
        member this.Item with get((a1:num0,b1:num0),(a2:num0,b2:int),(a3:int,b3:num0)) = num3(typ,this.Idx3((a1,b1),(a2,I b2),(I a3,b3)))
        member this.Item with get((a1:num0,b1:num0),(a2:num0,b2:int),(a3:int,b3:int)) = num3(typ,this.Idx3((a1,b1),(a2,I b2),(I a3,I b3)))
        member this.Item with get((a1:num0,b1:num0),(a2:num0,b2:int),_:unit) = num3(typ,this.Idx3((a1,b1),(a2,I b2),()))
        member this.Item with get((a1:num0,b1:num0),(a2:int,b2:num0),k:num0) = num2(typ,this.Idx3((a1,b1),(I a2,b2),k))
        member this.Item with get((a1:num0,b1:num0),(a2:int,b2:num0),k:int) = num2(typ,this.Idx3((a1,b1),(I a2,b2),I k))
        member this.Item with get((a1:num0,b1:num0),(a2:int,b2:num0),(a3:num0,b3:num0)) = num3(typ,this.Idx3((a1,b1),(I a2,b2),(a3,b3)))
        member this.Item with get((a1:num0,b1:num0),(a2:int,b2:num0),(a3:num0,b3:int)) = num3(typ,this.Idx3((a1,b1),(I a2,b2),(a3,I b3)))
        member this.Item with get((a1:num0,b1:num0),(a2:int,b2:num0),(a3:int,b3:num0)) = num3(typ,this.Idx3((a1,b1),(I a2,b2),(I a3,b3)))
        member this.Item with get((a1:num0,b1:num0),(a2:int,b2:num0),(a3:int,b3:int)) = num3(typ,this.Idx3((a1,b1),(I a2,b2),(I a3,I b3)))
        member this.Item with get((a1:num0,b1:num0),(a2:int,b2:num0),_:unit) = num3(typ,this.Idx3((a1,b1),(I a2,b2),()))
        member this.Item with get((a1:num0,b1:num0),(a2:int,b2:int),k:num0) = num2(typ,this.Idx3((a1,b1),(I a2,I b2),k))
        member this.Item with get((a1:num0,b1:num0),(a2:int,b2:int),k:int) = num2(typ,this.Idx3((a1,b1),(I a2,I b2),I k))
        member this.Item with get((a1:num0,b1:num0),(a2:int,b2:int),(a3:num0,b3:num0)) = num3(typ,this.Idx3((a1,b1),(I a2,I b2),(a3,b3)))
        member this.Item with get((a1:num0,b1:num0),(a2:int,b2:int),(a3:num0,b3:int)) = num3(typ,this.Idx3((a1,b1),(I a2,I b2),(a3,I b3)))
        member this.Item with get((a1:num0,b1:num0),(a2:int,b2:int),(a3:int,b3:num0)) = num3(typ,this.Idx3((a1,b1),(I a2,I b2),(I a3,b3)))
        member this.Item with get((a1:num0,b1:num0),(a2:int,b2:int),(a3:int,b3:int)) = num3(typ,this.Idx3((a1,b1),(I a2,I b2),(I a3,I b3)))
        member this.Item with get((a1:num0,b1:num0),(a2:int,b2:int),_:unit) = num3(typ,this.Idx3((a1,b1),(I a2,I b2),()))
        member this.Item with get((a1:num0,b1:num0),_:unit,k:num0) = num2(typ,this.Idx3((a1,b1),(),k))
        member this.Item with get((a1:num0,b1:num0),_:unit,k:int) = num2(typ,this.Idx3((a1,b1),(),I k))
        member this.Item with get((a1:num0,b1:num0),_:unit,(a3:num0,b3:num0)) = num3(typ,this.Idx3((a1,b1),(),(a3,b3)))
        member this.Item with get((a1:num0,b1:num0),_:unit,(a3:num0,b3:int)) = num3(typ,this.Idx3((a1,b1),(),(a3,I b3)))
        member this.Item with get((a1:num0,b1:num0),_:unit,(a3:int,b3:num0)) = num3(typ,this.Idx3((a1,b1),(),(I a3,b3)))
        member this.Item with get((a1:num0,b1:num0),_:unit,(a3:int,b3:int)) = num3(typ,this.Idx3((a1,b1),(),(I a3,I b3)))
        member this.Item with get((a1:num0,b1:num0),_:unit,_:unit) = num3(typ,this.Idx3((a1,b1),(),()))
        member this.Item with get((a1:num0,b1:int),j:num0,k:num0) = num1(typ,this.Idx3((a1,I b1),j,k))
        member this.Item with get((a1:num0,b1:int),j:num0,k:int) = num1(typ,this.Idx3((a1,I b1),j,I k))
        member this.Item with get((a1:num0,b1:int),j:num0,(a3:num0,b3:num0)) = num2(typ,this.Idx3((a1,I b1),j,(a3,b3)))
        member this.Item with get((a1:num0,b1:int),j:num0,(a3:num0,b3:int)) = num2(typ,this.Idx3((a1,I b1),j,(a3,I b3)))
        member this.Item with get((a1:num0,b1:int),j:num0,(a3:int,b3:num0)) = num2(typ,this.Idx3((a1,I b1),j,(I a3,b3)))
        member this.Item with get((a1:num0,b1:int),j:num0,(a3:int,b3:int)) = num2(typ,this.Idx3((a1,I b1),j,(I a3,I b3)))
        member this.Item with get((a1:num0,b1:int),j:num0,_:unit) = num2(typ,this.Idx3((a1,I b1),j,()))
        member this.Item with get((a1:num0,b1:int),j:int,k:num0) = num1(typ,this.Idx3((a1,I b1),I j,k))
        member this.Item with get((a1:num0,b1:int),j:int,k:int) = num1(typ,this.Idx3((a1,I b1),I j,I k))
        member this.Item with get((a1:num0,b1:int),j:int,(a3:num0,b3:num0)) = num2(typ,this.Idx3((a1,I b1),I j,(a3,b3)))
        member this.Item with get((a1:num0,b1:int),j:int,(a3:num0,b3:int)) = num2(typ,this.Idx3((a1,I b1),I j,(a3,I b3)))
        member this.Item with get((a1:num0,b1:int),j:int,(a3:int,b3:num0)) = num2(typ,this.Idx3((a1,I b1),I j,(I a3,b3)))
        member this.Item with get((a1:num0,b1:int),j:int,(a3:int,b3:int)) = num2(typ,this.Idx3((a1,I b1),I j,(I a3,I b3)))
        member this.Item with get((a1:num0,b1:int),j:int,_:unit) = num2(typ,this.Idx3((a1,I b1),I j,()))
        member this.Item with get((a1:num0,b1:int),(a2:num0,b2:num0),k:num0) = num2(typ,this.Idx3((a1,I b1),(a2,b2),k))
        member this.Item with get((a1:num0,b1:int),(a2:num0,b2:num0),k:int) = num2(typ,this.Idx3((a1,I b1),(a2,b2),I k))
        member this.Item with get((a1:num0,b1:int),(a2:num0,b2:num0),(a3:num0,b3:num0)) = num3(typ,this.Idx3((a1,I b1),(a2,b2),(a3,b3)))
        member this.Item with get((a1:num0,b1:int),(a2:num0,b2:num0),(a3:num0,b3:int)) = num3(typ,this.Idx3((a1,I b1),(a2,b2),(a3,I b3)))
        member this.Item with get((a1:num0,b1:int),(a2:num0,b2:num0),(a3:int,b3:num0)) = num3(typ,this.Idx3((a1,I b1),(a2,b2),(I a3,b3)))
        member this.Item with get((a1:num0,b1:int),(a2:num0,b2:num0),(a3:int,b3:int)) = num3(typ,this.Idx3((a1,I b1),(a2,b2),(I a3,I b3)))
        member this.Item with get((a1:num0,b1:int),(a2:num0,b2:num0),_:unit) = num3(typ,this.Idx3((a1,I b1),(a2,b2),()))
        member this.Item with get((a1:num0,b1:int),(a2:num0,b2:int),k:num0) = num2(typ,this.Idx3((a1,I b1),(a2,I b2),k))
        member this.Item with get((a1:num0,b1:int),(a2:num0,b2:int),k:int) = num2(typ,this.Idx3((a1,I b1),(a2,I b2),I k))
        member this.Item with get((a1:num0,b1:int),(a2:num0,b2:int),(a3:num0,b3:num0)) = num3(typ,this.Idx3((a1,I b1),(a2,I b2),(a3,b3)))
        member this.Item with get((a1:num0,b1:int),(a2:num0,b2:int),(a3:num0,b3:int)) = num3(typ,this.Idx3((a1,I b1),(a2,I b2),(a3,I b3)))
        member this.Item with get((a1:num0,b1:int),(a2:num0,b2:int),(a3:int,b3:num0)) = num3(typ,this.Idx3((a1,I b1),(a2,I b2),(I a3,b3)))
        member this.Item with get((a1:num0,b1:int),(a2:num0,b2:int),(a3:int,b3:int)) = num3(typ,this.Idx3((a1,I b1),(a2,I b2),(I a3,I b3)))
        member this.Item with get((a1:num0,b1:int),(a2:num0,b2:int),_:unit) = num3(typ,this.Idx3((a1,I b1),(a2,I b2),()))
        member this.Item with get((a1:num0,b1:int),(a2:int,b2:num0),k:num0) = num2(typ,this.Idx3((a1,I b1),(I a2,b2),k))
        member this.Item with get((a1:num0,b1:int),(a2:int,b2:num0),k:int) = num2(typ,this.Idx3((a1,I b1),(I a2,b2),I k))
        member this.Item with get((a1:num0,b1:int),(a2:int,b2:num0),(a3:num0,b3:num0)) = num3(typ,this.Idx3((a1,I b1),(I a2,b2),(a3,b3)))
        member this.Item with get((a1:num0,b1:int),(a2:int,b2:num0),(a3:num0,b3:int)) = num3(typ,this.Idx3((a1,I b1),(I a2,b2),(a3,I b3)))
        member this.Item with get((a1:num0,b1:int),(a2:int,b2:num0),(a3:int,b3:num0)) = num3(typ,this.Idx3((a1,I b1),(I a2,b2),(I a3,b3)))
        member this.Item with get((a1:num0,b1:int),(a2:int,b2:num0),(a3:int,b3:int)) = num3(typ,this.Idx3((a1,I b1),(I a2,b2),(I a3,I b3)))
        member this.Item with get((a1:num0,b1:int),(a2:int,b2:num0),_:unit) = num3(typ,this.Idx3((a1,I b1),(I a2,b2),()))
        member this.Item with get((a1:num0,b1:int),(a2:int,b2:int),k:num0) = num2(typ,this.Idx3((a1,I b1),(I a2,I b2),k))
        member this.Item with get((a1:num0,b1:int),(a2:int,b2:int),k:int) = num2(typ,this.Idx3((a1,I b1),(I a2,I b2),I k))
        member this.Item with get((a1:num0,b1:int),(a2:int,b2:int),(a3:num0,b3:num0)) = num3(typ,this.Idx3((a1,I b1),(I a2,I b2),(a3,b3)))
        member this.Item with get((a1:num0,b1:int),(a2:int,b2:int),(a3:num0,b3:int)) = num3(typ,this.Idx3((a1,I b1),(I a2,I b2),(a3,I b3)))
        member this.Item with get((a1:num0,b1:int),(a2:int,b2:int),(a3:int,b3:num0)) = num3(typ,this.Idx3((a1,I b1),(I a2,I b2),(I a3,b3)))
        member this.Item with get((a1:num0,b1:int),(a2:int,b2:int),(a3:int,b3:int)) = num3(typ,this.Idx3((a1,I b1),(I a2,I b2),(I a3,I b3)))
        member this.Item with get((a1:num0,b1:int),(a2:int,b2:int),_:unit) = num3(typ,this.Idx3((a1,I b1),(I a2,I b2),()))
        member this.Item with get((a1:num0,b1:int),_:unit,k:num0) = num2(typ,this.Idx3((a1,I b1),(),k))
        member this.Item with get((a1:num0,b1:int),_:unit,k:int) = num2(typ,this.Idx3((a1,I b1),(),I k))
        member this.Item with get((a1:num0,b1:int),_:unit,(a3:num0,b3:num0)) = num3(typ,this.Idx3((a1,I b1),(),(a3,b3)))
        member this.Item with get((a1:num0,b1:int),_:unit,(a3:num0,b3:int)) = num3(typ,this.Idx3((a1,I b1),(),(a3,I b3)))
        member this.Item with get((a1:num0,b1:int),_:unit,(a3:int,b3:num0)) = num3(typ,this.Idx3((a1,I b1),(),(I a3,b3)))
        member this.Item with get((a1:num0,b1:int),_:unit,(a3:int,b3:int)) = num3(typ,this.Idx3((a1,I b1),(),(I a3,I b3)))
        member this.Item with get((a1:num0,b1:int),_:unit,_:unit) = num3(typ,this.Idx3((a1,I b1),(),()))
        member this.Item with get((a1:int,b1:num0),j:num0,k:num0) = num1(typ,this.Idx3((I a1,b1),j,k))
        member this.Item with get((a1:int,b1:num0),j:num0,k:int) = num1(typ,this.Idx3((I a1,b1),j,I k))
        member this.Item with get((a1:int,b1:num0),j:num0,(a3:num0,b3:num0)) = num2(typ,this.Idx3((I a1,b1),j,(a3,b3)))
        member this.Item with get((a1:int,b1:num0),j:num0,(a3:num0,b3:int)) = num2(typ,this.Idx3((I a1,b1),j,(a3,I b3)))
        member this.Item with get((a1:int,b1:num0),j:num0,(a3:int,b3:num0)) = num2(typ,this.Idx3((I a1,b1),j,(I a3,b3)))
        member this.Item with get((a1:int,b1:num0),j:num0,(a3:int,b3:int)) = num2(typ,this.Idx3((I a1,b1),j,(I a3,I b3)))
        member this.Item with get((a1:int,b1:num0),j:num0,_:unit) = num2(typ,this.Idx3((I a1,b1),j,()))
        member this.Item with get((a1:int,b1:num0),j:int,k:num0) = num1(typ,this.Idx3((I a1,b1),I j,k))
        member this.Item with get((a1:int,b1:num0),j:int,k:int) = num1(typ,this.Idx3((I a1,b1),I j,I k))
        member this.Item with get((a1:int,b1:num0),j:int,(a3:num0,b3:num0)) = num2(typ,this.Idx3((I a1,b1),I j,(a3,b3)))
        member this.Item with get((a1:int,b1:num0),j:int,(a3:num0,b3:int)) = num2(typ,this.Idx3((I a1,b1),I j,(a3,I b3)))
        member this.Item with get((a1:int,b1:num0),j:int,(a3:int,b3:num0)) = num2(typ,this.Idx3((I a1,b1),I j,(I a3,b3)))
        member this.Item with get((a1:int,b1:num0),j:int,(a3:int,b3:int)) = num2(typ,this.Idx3((I a1,b1),I j,(I a3,I b3)))
        member this.Item with get((a1:int,b1:num0),j:int,_:unit) = num2(typ,this.Idx3((I a1,b1),I j,()))
        member this.Item with get((a1:int,b1:num0),(a2:num0,b2:num0),k:num0) = num2(typ,this.Idx3((I a1,b1),(a2,b2),k))
        member this.Item with get((a1:int,b1:num0),(a2:num0,b2:num0),k:int) = num2(typ,this.Idx3((I a1,b1),(a2,b2),I k))
        member this.Item with get((a1:int,b1:num0),(a2:num0,b2:num0),(a3:num0,b3:num0)) = num3(typ,this.Idx3((I a1,b1),(a2,b2),(a3,b3)))
        member this.Item with get((a1:int,b1:num0),(a2:num0,b2:num0),(a3:num0,b3:int)) = num3(typ,this.Idx3((I a1,b1),(a2,b2),(a3,I b3)))
        member this.Item with get((a1:int,b1:num0),(a2:num0,b2:num0),(a3:int,b3:num0)) = num3(typ,this.Idx3((I a1,b1),(a2,b2),(I a3,b3)))
        member this.Item with get((a1:int,b1:num0),(a2:num0,b2:num0),(a3:int,b3:int)) = num3(typ,this.Idx3((I a1,b1),(a2,b2),(I a3,I b3)))
        member this.Item with get((a1:int,b1:num0),(a2:num0,b2:num0),_:unit) = num3(typ,this.Idx3((I a1,b1),(a2,b2),()))
        member this.Item with get((a1:int,b1:num0),(a2:num0,b2:int),k:num0) = num2(typ,this.Idx3((I a1,b1),(a2,I b2),k))
        member this.Item with get((a1:int,b1:num0),(a2:num0,b2:int),k:int) = num2(typ,this.Idx3((I a1,b1),(a2,I b2),I k))
        member this.Item with get((a1:int,b1:num0),(a2:num0,b2:int),(a3:num0,b3:num0)) = num3(typ,this.Idx3((I a1,b1),(a2,I b2),(a3,b3)))
        member this.Item with get((a1:int,b1:num0),(a2:num0,b2:int),(a3:num0,b3:int)) = num3(typ,this.Idx3((I a1,b1),(a2,I b2),(a3,I b3)))
        member this.Item with get((a1:int,b1:num0),(a2:num0,b2:int),(a3:int,b3:num0)) = num3(typ,this.Idx3((I a1,b1),(a2,I b2),(I a3,b3)))
        member this.Item with get((a1:int,b1:num0),(a2:num0,b2:int),(a3:int,b3:int)) = num3(typ,this.Idx3((I a1,b1),(a2,I b2),(I a3,I b3)))
        member this.Item with get((a1:int,b1:num0),(a2:num0,b2:int),_:unit) = num3(typ,this.Idx3((I a1,b1),(a2,I b2),()))
        member this.Item with get((a1:int,b1:num0),(a2:int,b2:num0),k:num0) = num2(typ,this.Idx3((I a1,b1),(I a2,b2),k))
        member this.Item with get((a1:int,b1:num0),(a2:int,b2:num0),k:int) = num2(typ,this.Idx3((I a1,b1),(I a2,b2),I k))
        member this.Item with get((a1:int,b1:num0),(a2:int,b2:num0),(a3:num0,b3:num0)) = num3(typ,this.Idx3((I a1,b1),(I a2,b2),(a3,b3)))
        member this.Item with get((a1:int,b1:num0),(a2:int,b2:num0),(a3:num0,b3:int)) = num3(typ,this.Idx3((I a1,b1),(I a2,b2),(a3,I b3)))
        member this.Item with get((a1:int,b1:num0),(a2:int,b2:num0),(a3:int,b3:num0)) = num3(typ,this.Idx3((I a1,b1),(I a2,b2),(I a3,b3)))
        member this.Item with get((a1:int,b1:num0),(a2:int,b2:num0),(a3:int,b3:int)) = num3(typ,this.Idx3((I a1,b1),(I a2,b2),(I a3,I b3)))
        member this.Item with get((a1:int,b1:num0),(a2:int,b2:num0),_:unit) = num3(typ,this.Idx3((I a1,b1),(I a2,b2),()))
        member this.Item with get((a1:int,b1:num0),(a2:int,b2:int),k:num0) = num2(typ,this.Idx3((I a1,b1),(I a2,I b2),k))
        member this.Item with get((a1:int,b1:num0),(a2:int,b2:int),k:int) = num2(typ,this.Idx3((I a1,b1),(I a2,I b2),I k))
        member this.Item with get((a1:int,b1:num0),(a2:int,b2:int),(a3:num0,b3:num0)) = num3(typ,this.Idx3((I a1,b1),(I a2,I b2),(a3,b3)))
        member this.Item with get((a1:int,b1:num0),(a2:int,b2:int),(a3:num0,b3:int)) = num3(typ,this.Idx3((I a1,b1),(I a2,I b2),(a3,I b3)))
        member this.Item with get((a1:int,b1:num0),(a2:int,b2:int),(a3:int,b3:num0)) = num3(typ,this.Idx3((I a1,b1),(I a2,I b2),(I a3,b3)))
        member this.Item with get((a1:int,b1:num0),(a2:int,b2:int),(a3:int,b3:int)) = num3(typ,this.Idx3((I a1,b1),(I a2,I b2),(I a3,I b3)))
        member this.Item with get((a1:int,b1:num0),(a2:int,b2:int),_:unit) = num3(typ,this.Idx3((I a1,b1),(I a2,I b2),()))
        member this.Item with get((a1:int,b1:num0),_:unit,k:num0) = num2(typ,this.Idx3((I a1,b1),(),k))
        member this.Item with get((a1:int,b1:num0),_:unit,k:int) = num2(typ,this.Idx3((I a1,b1),(),I k))
        member this.Item with get((a1:int,b1:num0),_:unit,(a3:num0,b3:num0)) = num3(typ,this.Idx3((I a1,b1),(),(a3,b3)))
        member this.Item with get((a1:int,b1:num0),_:unit,(a3:num0,b3:int)) = num3(typ,this.Idx3((I a1,b1),(),(a3,I b3)))
        member this.Item with get((a1:int,b1:num0),_:unit,(a3:int,b3:num0)) = num3(typ,this.Idx3((I a1,b1),(),(I a3,b3)))
        member this.Item with get((a1:int,b1:num0),_:unit,(a3:int,b3:int)) = num3(typ,this.Idx3((I a1,b1),(),(I a3,I b3)))
        member this.Item with get((a1:int,b1:num0),_:unit,_:unit) = num3(typ,this.Idx3((I a1,b1),(),()))
        member this.Item with get((a1:int,b1:int),j:num0,k:num0) = num1(typ,this.Idx3((I a1,I b1),j,k))
        member this.Item with get((a1:int,b1:int),j:num0,k:int) = num1(typ,this.Idx3((I a1,I b1),j,I k))
        member this.Item with get((a1:int,b1:int),j:num0,(a3:num0,b3:num0)) = num2(typ,this.Idx3((I a1,I b1),j,(a3,b3)))
        member this.Item with get((a1:int,b1:int),j:num0,(a3:num0,b3:int)) = num2(typ,this.Idx3((I a1,I b1),j,(a3,I b3)))
        member this.Item with get((a1:int,b1:int),j:num0,(a3:int,b3:num0)) = num2(typ,this.Idx3((I a1,I b1),j,(I a3,b3)))
        member this.Item with get((a1:int,b1:int),j:num0,(a3:int,b3:int)) = num2(typ,this.Idx3((I a1,I b1),j,(I a3,I b3)))
        member this.Item with get((a1:int,b1:int),j:num0,_:unit) = num2(typ,this.Idx3((I a1,I b1),j,()))
        member this.Item with get((a1:int,b1:int),j:int,k:num0) = num1(typ,this.Idx3((I a1,I b1),I j,k))
        member this.Item with get((a1:int,b1:int),j:int,k:int) = num1(typ,this.Idx3((I a1,I b1),I j,I k))
        member this.Item with get((a1:int,b1:int),j:int,(a3:num0,b3:num0)) = num2(typ,this.Idx3((I a1,I b1),I j,(a3,b3)))
        member this.Item with get((a1:int,b1:int),j:int,(a3:num0,b3:int)) = num2(typ,this.Idx3((I a1,I b1),I j,(a3,I b3)))
        member this.Item with get((a1:int,b1:int),j:int,(a3:int,b3:num0)) = num2(typ,this.Idx3((I a1,I b1),I j,(I a3,b3)))
        member this.Item with get((a1:int,b1:int),j:int,(a3:int,b3:int)) = num2(typ,this.Idx3((I a1,I b1),I j,(I a3,I b3)))
        member this.Item with get((a1:int,b1:int),j:int,_:unit) = num2(typ,this.Idx3((I a1,I b1),I j,()))
        member this.Item with get((a1:int,b1:int),(a2:num0,b2:num0),k:num0) = num2(typ,this.Idx3((I a1,I b1),(a2,b2),k))
        member this.Item with get((a1:int,b1:int),(a2:num0,b2:num0),k:int) = num2(typ,this.Idx3((I a1,I b1),(a2,b2),I k))
        member this.Item with get((a1:int,b1:int),(a2:num0,b2:num0),(a3:num0,b3:num0)) = num3(typ,this.Idx3((I a1,I b1),(a2,b2),(a3,b3)))
        member this.Item with get((a1:int,b1:int),(a2:num0,b2:num0),(a3:num0,b3:int)) = num3(typ,this.Idx3((I a1,I b1),(a2,b2),(a3,I b3)))
        member this.Item with get((a1:int,b1:int),(a2:num0,b2:num0),(a3:int,b3:num0)) = num3(typ,this.Idx3((I a1,I b1),(a2,b2),(I a3,b3)))
        member this.Item with get((a1:int,b1:int),(a2:num0,b2:num0),(a3:int,b3:int)) = num3(typ,this.Idx3((I a1,I b1),(a2,b2),(I a3,I b3)))
        member this.Item with get((a1:int,b1:int),(a2:num0,b2:num0),_:unit) = num3(typ,this.Idx3((I a1,I b1),(a2,b2),()))
        member this.Item with get((a1:int,b1:int),(a2:num0,b2:int),k:num0) = num2(typ,this.Idx3((I a1,I b1),(a2,I b2),k))
        member this.Item with get((a1:int,b1:int),(a2:num0,b2:int),k:int) = num2(typ,this.Idx3((I a1,I b1),(a2,I b2),I k))
        member this.Item with get((a1:int,b1:int),(a2:num0,b2:int),(a3:num0,b3:num0)) = num3(typ,this.Idx3((I a1,I b1),(a2,I b2),(a3,b3)))
        member this.Item with get((a1:int,b1:int),(a2:num0,b2:int),(a3:num0,b3:int)) = num3(typ,this.Idx3((I a1,I b1),(a2,I b2),(a3,I b3)))
        member this.Item with get((a1:int,b1:int),(a2:num0,b2:int),(a3:int,b3:num0)) = num3(typ,this.Idx3((I a1,I b1),(a2,I b2),(I a3,b3)))
        member this.Item with get((a1:int,b1:int),(a2:num0,b2:int),(a3:int,b3:int)) = num3(typ,this.Idx3((I a1,I b1),(a2,I b2),(I a3,I b3)))
        member this.Item with get((a1:int,b1:int),(a2:num0,b2:int),_:unit) = num3(typ,this.Idx3((I a1,I b1),(a2,I b2),()))
        member this.Item with get((a1:int,b1:int),(a2:int,b2:num0),k:num0) = num2(typ,this.Idx3((I a1,I b1),(I a2,b2),k))
        member this.Item with get((a1:int,b1:int),(a2:int,b2:num0),k:int) = num2(typ,this.Idx3((I a1,I b1),(I a2,b2),I k))
        member this.Item with get((a1:int,b1:int),(a2:int,b2:num0),(a3:num0,b3:num0)) = num3(typ,this.Idx3((I a1,I b1),(I a2,b2),(a3,b3)))
        member this.Item with get((a1:int,b1:int),(a2:int,b2:num0),(a3:num0,b3:int)) = num3(typ,this.Idx3((I a1,I b1),(I a2,b2),(a3,I b3)))
        member this.Item with get((a1:int,b1:int),(a2:int,b2:num0),(a3:int,b3:num0)) = num3(typ,this.Idx3((I a1,I b1),(I a2,b2),(I a3,b3)))
        member this.Item with get((a1:int,b1:int),(a2:int,b2:num0),(a3:int,b3:int)) = num3(typ,this.Idx3((I a1,I b1),(I a2,b2),(I a3,I b3)))
        member this.Item with get((a1:int,b1:int),(a2:int,b2:num0),_:unit) = num3(typ,this.Idx3((I a1,I b1),(I a2,b2),()))
        member this.Item with get((a1:int,b1:int),(a2:int,b2:int),k:num0) = num2(typ,this.Idx3((I a1,I b1),(I a2,I b2),k))
        member this.Item with get((a1:int,b1:int),(a2:int,b2:int),k:int) = num2(typ,this.Idx3((I a1,I b1),(I a2,I b2),I k))
        member this.Item with get((a1:int,b1:int),(a2:int,b2:int),(a3:num0,b3:num0)) = num3(typ,this.Idx3((I a1,I b1),(I a2,I b2),(a3,b3)))
        member this.Item with get((a1:int,b1:int),(a2:int,b2:int),(a3:num0,b3:int)) = num3(typ,this.Idx3((I a1,I b1),(I a2,I b2),(a3,I b3)))
        member this.Item with get((a1:int,b1:int),(a2:int,b2:int),(a3:int,b3:num0)) = num3(typ,this.Idx3((I a1,I b1),(I a2,I b2),(I a3,b3)))
        member this.Item with get((a1:int,b1:int),(a2:int,b2:int),(a3:int,b3:int)) = num3(typ,this.Idx3((I a1,I b1),(I a2,I b2),(I a3,I b3)))
        member this.Item with get((a1:int,b1:int),(a2:int,b2:int),_:unit) = num3(typ,this.Idx3((I a1,I b1),(I a2,I b2),()))
        member this.Item with get((a1:int,b1:int),_:unit,k:num0) = num2(typ,this.Idx3((I a1,I b1),(),k))
        member this.Item with get((a1:int,b1:int),_:unit,k:int) = num2(typ,this.Idx3((I a1,I b1),(),I k))
        member this.Item with get((a1:int,b1:int),_:unit,(a3:num0,b3:num0)) = num3(typ,this.Idx3((I a1,I b1),(),(a3,b3)))
        member this.Item with get((a1:int,b1:int),_:unit,(a3:num0,b3:int)) = num3(typ,this.Idx3((I a1,I b1),(),(a3,I b3)))
        member this.Item with get((a1:int,b1:int),_:unit,(a3:int,b3:num0)) = num3(typ,this.Idx3((I a1,I b1),(),(I a3,b3)))
        member this.Item with get((a1:int,b1:int),_:unit,(a3:int,b3:int)) = num3(typ,this.Idx3((I a1,I b1),(),(I a3,I b3)))
        member this.Item with get((a1:int,b1:int),_:unit,_:unit) = num3(typ,this.Idx3((I a1,I b1),(),()))
        member this.Item with get(_:unit,j:num0,k:num0) = num1(typ,this.Idx3((),j,k))
        member this.Item with get(_:unit,j:num0,k:int) = num1(typ,this.Idx3((),j,I k))
        member this.Item with get(_:unit,j:num0,(a3:num0,b3:num0)) = num2(typ,this.Idx3((),j,(a3,b3)))
        member this.Item with get(_:unit,j:num0,(a3:num0,b3:int)) = num2(typ,this.Idx3((),j,(a3,I b3)))
        member this.Item with get(_:unit,j:num0,(a3:int,b3:num0)) = num2(typ,this.Idx3((),j,(I a3,b3)))
        member this.Item with get(_:unit,j:num0,(a3:int,b3:int)) = num2(typ,this.Idx3((),j,(I a3,I b3)))
        member this.Item with get(_:unit,j:num0,_:unit) = num2(typ,this.Idx3((),j,()))
        member this.Item with get(_:unit,j:int,k:num0) = num1(typ,this.Idx3((),I j,k))
        member this.Item with get(_:unit,j:int,k:int) = num1(typ,this.Idx3((),I j,I k))
        member this.Item with get(_:unit,j:int,(a3:num0,b3:num0)) = num2(typ,this.Idx3((),I j,(a3,b3)))
        member this.Item with get(_:unit,j:int,(a3:num0,b3:int)) = num2(typ,this.Idx3((),I j,(a3,I b3)))
        member this.Item with get(_:unit,j:int,(a3:int,b3:num0)) = num2(typ,this.Idx3((),I j,(I a3,b3)))
        member this.Item with get(_:unit,j:int,(a3:int,b3:int)) = num2(typ,this.Idx3((),I j,(I a3,I b3)))
        member this.Item with get(_:unit,j:int,_:unit) = num2(typ,this.Idx3((),I j,()))
        member this.Item with get(_:unit,(a2:num0,b2:num0),k:num0) = num2(typ,this.Idx3((),(a2,b2),k))
        member this.Item with get(_:unit,(a2:num0,b2:num0),k:int) = num2(typ,this.Idx3((),(a2,b2),I k))
        member this.Item with get(_:unit,(a2:num0,b2:num0),(a3:num0,b3:num0)) = num3(typ,this.Idx3((),(a2,b2),(a3,b3)))
        member this.Item with get(_:unit,(a2:num0,b2:num0),(a3:num0,b3:int)) = num3(typ,this.Idx3((),(a2,b2),(a3,I b3)))
        member this.Item with get(_:unit,(a2:num0,b2:num0),(a3:int,b3:num0)) = num3(typ,this.Idx3((),(a2,b2),(I a3,b3)))
        member this.Item with get(_:unit,(a2:num0,b2:num0),(a3:int,b3:int)) = num3(typ,this.Idx3((),(a2,b2),(I a3,I b3)))
        member this.Item with get(_:unit,(a2:num0,b2:num0),_:unit) = num3(typ,this.Idx3((),(a2,b2),()))
        member this.Item with get(_:unit,(a2:num0,b2:int),k:num0) = num2(typ,this.Idx3((),(a2,I b2),k))
        member this.Item with get(_:unit,(a2:num0,b2:int),k:int) = num2(typ,this.Idx3((),(a2,I b2),I k))
        member this.Item with get(_:unit,(a2:num0,b2:int),(a3:num0,b3:num0)) = num3(typ,this.Idx3((),(a2,I b2),(a3,b3)))
        member this.Item with get(_:unit,(a2:num0,b2:int),(a3:num0,b3:int)) = num3(typ,this.Idx3((),(a2,I b2),(a3,I b3)))
        member this.Item with get(_:unit,(a2:num0,b2:int),(a3:int,b3:num0)) = num3(typ,this.Idx3((),(a2,I b2),(I a3,b3)))
        member this.Item with get(_:unit,(a2:num0,b2:int),(a3:int,b3:int)) = num3(typ,this.Idx3((),(a2,I b2),(I a3,I b3)))
        member this.Item with get(_:unit,(a2:num0,b2:int),_:unit) = num3(typ,this.Idx3((),(a2,I b2),()))
        member this.Item with get(_:unit,(a2:int,b2:num0),k:num0) = num2(typ,this.Idx3((),(I a2,b2),k))
        member this.Item with get(_:unit,(a2:int,b2:num0),k:int) = num2(typ,this.Idx3((),(I a2,b2),I k))
        member this.Item with get(_:unit,(a2:int,b2:num0),(a3:num0,b3:num0)) = num3(typ,this.Idx3((),(I a2,b2),(a3,b3)))
        member this.Item with get(_:unit,(a2:int,b2:num0),(a3:num0,b3:int)) = num3(typ,this.Idx3((),(I a2,b2),(a3,I b3)))
        member this.Item with get(_:unit,(a2:int,b2:num0),(a3:int,b3:num0)) = num3(typ,this.Idx3((),(I a2,b2),(I a3,b3)))
        member this.Item with get(_:unit,(a2:int,b2:num0),(a3:int,b3:int)) = num3(typ,this.Idx3((),(I a2,b2),(I a3,I b3)))
        member this.Item with get(_:unit,(a2:int,b2:num0),_:unit) = num3(typ,this.Idx3((),(I a2,b2),()))
        member this.Item with get(_:unit,(a2:int,b2:int),k:num0) = num2(typ,this.Idx3((),(I a2,I b2),k))
        member this.Item with get(_:unit,(a2:int,b2:int),k:int) = num2(typ,this.Idx3((),(I a2,I b2),I k))
        member this.Item with get(_:unit,(a2:int,b2:int),(a3:num0,b3:num0)) = num3(typ,this.Idx3((),(I a2,I b2),(a3,b3)))
        member this.Item with get(_:unit,(a2:int,b2:int),(a3:num0,b3:int)) = num3(typ,this.Idx3((),(I a2,I b2),(a3,I b3)))
        member this.Item with get(_:unit,(a2:int,b2:int),(a3:int,b3:num0)) = num3(typ,this.Idx3((),(I a2,I b2),(I a3,b3)))
        member this.Item with get(_:unit,(a2:int,b2:int),(a3:int,b3:int)) = num3(typ,this.Idx3((),(I a2,I b2),(I a3,I b3)))
        member this.Item with get(_:unit,(a2:int,b2:int),_:unit) = num3(typ,this.Idx3((),(I a2,I b2),()))
        member this.Item with get(_:unit,_:unit,k:num0) = num2(typ,this.Idx3((),(),k))
        member this.Item with get(_:unit,_:unit,k:int) = num2(typ,this.Idx3((),(),I k))
        member this.Item with get(_:unit,_:unit,(a3:num0,b3:num0)) = num3(typ,this.Idx3((),(),(a3,b3)))
        member this.Item with get(_:unit,_:unit,(a3:num0,b3:int)) = num3(typ,this.Idx3((),(),(a3,I b3)))
        member this.Item with get(_:unit,_:unit,(a3:int,b3:num0)) = num3(typ,this.Idx3((),(),(I a3,b3)))
        member this.Item with get(_:unit,_:unit,(a3:int,b3:int)) = num3(typ,this.Idx3((),(),(I a3,I b3)))
        
        //<summary>3次元配列生成(整数型)</summary> 
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="s3">第3要素数</param>
        ///<param name="f">(i,j,k)要素に対する要素値</param>
        static member fiarray(s1:num0,s2:num0,s3:num0,f:num0*num0*num0->num0) = num3(It 4,Arx3(s1,s2,s3,f))
        
        //<summary>3次元配列生成(倍精度浮動小数点型)</summary> 
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="s3">第3要素数</param>
        ///<param name="f">(i,j,k)要素に対する要素値</param>
        static member fdarray(s1:num0,s2:num0,s3:num0,f:num0*num0*num0->num0) = num3(Dt,Arx3(s1,s2,s3,f))
        
        //<summary>3次元配列生成(複素数型)</summary> 
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="s3">第3要素数</param>
        ///<param name="f">(i,j,k)要素に対する要素値</param>
        static member fzarray(s1:num0,s2:num0,s3:num0,f:num0*num0*num0->num0) = num3(Zt,Arx3(s1,s2,s3,f))
        
        //<summary>3次元配列生成(整数型)</summary> 
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="s3">第3要素数</param>
        ///<param name="f">(i,j,k)要素に対する要素値</param>
        static member fiarray(s1:int,s2:num0,s3:num0,f:num0*num0*num0->num0) = num3.fiarray(I s1,s2,s3,f)
        
        //<summary>3次元配列生成(倍精度浮動小数点型)</summary> 
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="s3">第3要素数</param>
        ///<param name="f">(i,j,k)要素に対する要素値</param>
        static member fdarray(s1:int,s2:num0,s3:num0,f:num0*num0*num0->num0) = num3.fdarray(I s1,s2,s3,f)
        
        //<summary>3次元配列生成(複素数型)</summary> 
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="s3">第3要素数</param>
        ///<param name="f">(i,j,k)要素に対する要素値</param>
        static member fzarray(s1:int,s2:num0,s3:num0,f:num0*num0*num0->num0) = num3.fzarray(I s1,s2,s3,f)
        
        //<summary>3次元配列生成(整数型)</summary> 
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="s3">第3要素数</param>
        ///<param name="f">(i,j,k)要素に対する要素値</param>
        static member fiarray(s1:num0,s2:int,s3:num0,f:num0*num0*num0->num0) = num3.fiarray(s1,I s2,s3,f)
        
        //<summary>3次元配列生成(倍精度浮動小数点型)</summary> 
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="s3">第3要素数</param>
        ///<param name="f">(i,j,k)要素に対する要素値</param>
        static member fdarray(s1:num0,s2:int,s3:num0,f:num0*num0*num0->num0) = num3.fdarray(s1,I s2,s3,f)
        
        //<summary>3次元配列生成(複素数型)</summary> 
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="s3">第3要素数</param>
        ///<param name="f">(i,j,k)要素に対する要素値</param>
        static member fzarray(s1:num0,s2:int,s3:num0,f:num0*num0*num0->num0) = num3.fzarray(s1,I s2,s3,f)
        
        //<summary>3次元配列生成(整数型)</summary> 
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="s3">第3要素数</param>
        ///<param name="f">(i,j,k)要素に対する要素値</param>
        static member fiarray(s1:num0,s2:num0,s3:int,f:num0*num0*num0->num0) = num3.fiarray(s1,s2,I s3,f)
        
        //<summary>3次元配列生成(倍精度浮動小数点型)</summary> 
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="s3">第3要素数</param>
        ///<param name="f">(i,j,k)要素に対する要素値</param>
        static member fdarray(s1:num0,s2:num0,s3:int,f:num0*num0*num0->num0) = num3.fdarray(s1,s2,I s3,f)
        
        //<summary>3次元配列生成(複素数型)</summary> 
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="s3">第3要素数</param>
        ///<param name="f">(i,j,k)要素に対する要素値</param>
        static member fzarray(s1:num0,s2:num0,s3:int,f:num0*num0*num0->num0) = num3.fzarray(s1,s2,I s3,f)
        
        //<summary>3次元配列生成(整数型)</summary> 
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="s3">第3要素数</param>
        ///<param name="f">(i,j,k)要素に対する要素値</param>
        static member fiarray(s1:int,s2:int,s3:num0,f:num0*num0*num0->num0) = num3.fiarray(I s1,I s2,s3,f)
        
        //<summary>3次元配列生成(倍精度浮動小数点型)</summary> 
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="s3">第3要素数</param>
        ///<param name="f">(i,j,k)要素に対する要素値</param>
        static member fdarray(s1:int,s2:int,s3:num0,f:num0*num0*num0->num0) = num3.fdarray(I s1,I s2,s3,f)
        
        //<summary>3次元配列生成(複素数型)</summary> 
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="s3">第3要素数</param>
        ///<param name="f">(i,j,k)要素に対する要素値</param>
        static member fzarray(s1:int,s2:int,s3:num0,f:num0*num0*num0->num0) = num3.fzarray(I s1,I s2,s3,f)
        
        //<summary>3次元配列生成(整数型)</summary> 
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="s3">第3要素数</param>
        ///<param name="f">(i,j,k)要素に対する要素値</param>
        static member fiarray(s1:int,s2:num0,s3:int,f:num0*num0*num0->num0) = num3.fiarray(I s1,s2,I s3,f)
        
        //<summary>3次元配列生成(倍精度浮動小数点型)</summary> 
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="s3">第3要素数</param>
        ///<param name="f">(i,j,k)要素に対する要素値</param>
        static member fdarray(s1:int,s2:num0,s3:int,f:num0*num0*num0->num0) = num3.fdarray(I s1,s2,I s3,f)
        
        //<summary>3次元配列生成(複素数型)</summary> 
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="s3">第3要素数</param>
        ///<param name="f">(i,j,k)要素に対する要素値</param>
        static member fzarray(s1:int,s2:num0,s3:int,f:num0*num0*num0->num0) = num3.fzarray(I s1,s2,I s3,f)
        
        //<summary>3次元配列生成(整数型)</summary> 
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="s3">第3要素数</param>
        ///<param name="f">(i,j,k)要素に対する要素値</param>
        static member fiarray(s1:num0,s2:int,s3:int,f:num0*num0*num0->num0) = num3.fiarray(s1,I s2,I s3,f)
        
        //<summary>3次元配列生成(倍精度浮動小数点型)</summary> 
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="s3">第3要素数</param>
        ///<param name="f">(i,j,k)要素に対する要素値</param>
        static member fdarray(s1:num0,s2:int,s3:int,f:num0*num0*num0->num0) = num3.fdarray(s1,I s2,I s3,f)
        
        //<summary>3次元配列生成(複素数型)</summary> 
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="s3">第3要素数</param>
        ///<param name="f">(i,j,k)要素に対する要素値</param>
        static member fzarray(s1:num0,s2:int,s3:int,f:num0*num0*num0->num0) = num3.fzarray(s1,I s2,I s3,f)
        
        //<summary>3次元配列生成(整数型)</summary> 
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="s3">第3要素数</param>
        ///<param name="f">(i,j,k)要素に対する要素値</param>
        static member fiarray(s1:int,s2:int,s3:int,f:num0*num0*num0->num0) = num3.fiarray(I s1,I s2,I s3,f)
        
        //<summary>3次元配列生成(倍精度浮動小数点型)</summary> 
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="s3">第3要素数</param>
        ///<param name="f">(i,j,k)要素に対する要素値</param>
        static member fdarray(s1:int,s2:int,s3:int,f:num0*num0*num0->num0) = num3.fdarray(I s1,I s2,I s3,f)
        
        //<summary>3次元配列生成(複素数型)</summary> 
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="s3">第3要素数</param>
        ///<param name="f">(i,j,k)要素に対する要素値</param>
        static member fzarray(s1:int,s2:int,s3:int,f:num0*num0*num0->num0) = num3.fzarray(I s1,I s2,I s3,f)
        
        //<summary>元の配列と同じサイズの配列生成</summary> 
        ///<param name="f">(i,j,k)要素に対する要素値</param>
        member this.farray(f:num0*num0*num0->num0) = num3(this.etype,Arx3(this.size1,this.size2,this.size3,f))
        
        //<summary>値を0で初期化</summary> 
        override this.clear() = 
            this <== I 0
            
        ///<summary>配列サイズ変数をメモリ未割当て状態に初期化</summary>
        override this.sizeinit() = 
            this.size1 <== -1
            
        static member sizeMismatchError(x:num3,y:num3) =
            if debug.debugMode then
                error.inc()
                comment("***debug array1 access check: "+error.ID+"*****************************")
                br.if1 (x.size1 .=/ y.size1) <| fun () -> 
                    print.t ("ERROR"+error.ID+" operator '+' array size1 mismatch")
                br.if1 (x.size2 .=/ y.size2) <| fun () -> 
                    print.t ("ERROR"+error.ID+" operator '+' array size2 mismatch")
                comment "****************************************************"
                
        static member (+) (x:num3,y:num3) =
            num3.sizeMismatchError(x,y)
            num3(x.etype%%y.etype,Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k]+y[i,j,k]))
        static member (+) (x:num0,y:num3) = num3(x.etype%%y.etype,Arx3(y.size1, y.size2, y.size3, fun (i,j,k) -> x+y[i,j,k]))
        static member (+) (x:int,y:num3) = num3(It 4%%y.etype,Arx3(y.size1, y.size2, y.size3, fun (i,j,k) -> x+y[i,j,k]))
        static member (+) (x:double,y:num3) = num3(Dt%%y.etype,Arx3(y.size1, y.size2, y.size3, fun (i,j,k) -> x+y[i,j,k]))
        static member (+) (x:num3,y:num0) = num3(x.etype%%y.etype,Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k]+y))
        static member (+) (x:num3,y:int) = num3(x.etype%%It 4,Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k]+y))
        static member (+) (x:num3,y:double) = num3(x.etype%%Dt,Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k]+y))
        
        static member (-) (x:num3,y:num3) =
            num3.sizeMismatchError(x,y)
            num3(x.etype%%y.etype,Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k]-y[i,j,k]))
        static member (-) (x:num0,y:num3) = num3(x.etype%%y.etype,Arx3(y.size1, y.size2, y.size3, fun (i,j,k) -> x-y[i,j,k]))
        static member (-) (x:int,y:num3) = num3(It 4%%y.etype,Arx3(y.size1, y.size2, y.size3, fun (i,j,k) -> x-y[i,j,k]))
        static member (-) (x:double,y:num3) = num3(Dt%%y.etype,Arx3(y.size1, y.size2, y.size3, fun (i,j,k) -> x-y[i,j,k]))
        static member (-) (x:num3,y:num0) = num3(x.etype%%y.etype,Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k]-y))
        static member (-) (x:num3,y:int) = num3(x.etype%%It 4,Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k]-y))
        static member (-) (x:num3,y:double) = num3(x.etype%%Dt,Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k]-y))
        
        static member ( * ) (x:num3,y:num3) =
            num3.sizeMismatchError(x,y)
            num3(x.etype%%y.etype,Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k]*y[i,j,k]))
        static member ( * ) (x:num0,y:num3) = num3(x.etype%%y.etype,Arx3(y.size1, y.size2, y.size3, fun (i,j,k) -> x*y[i,j,k]))
        static member ( * ) (x:int,y:num3) = num3(It 4%%y.etype,Arx3(y.size1, y.size2, y.size3, fun (i,j,k) -> x*y[i,j,k]))
        static member ( * ) (x:double,y:num3) = num3(Dt%%y.etype,Arx3(y.size1, y.size2, y.size3, fun (i,j,k) -> x*y[i,j,k]))
        static member ( * ) (x:num3,y:num0) = num3(x.etype%%y.etype,Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k]*y))
        static member ( * ) (x:num3,y:int) = num3(x.etype%%It 4,Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k]*y))
        static member ( * ) (x:num3,y:double) = num3(x.etype%%Dt,Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k]*y))
        
        static member (/) (x:num3,y:num3) =
            num3.sizeMismatchError(x,y)
            num3(x.etype%%y.etype,Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k]/y[i,j,k]))
        static member (/) (x:num0,y:num3) = num3(x.etype%%y.etype,Arx3(y.size1, y.size2, y.size3, fun (i,j,k) -> x/y[i,j,k]))
        static member (/) (x:int,y:num3) = num3(It 4%%y.etype,Arx3(y.size1, y.size2, y.size3, fun (i,j,k) -> x/y[i,j,k]))
        static member (/) (x:double,y:num3) = num3(Dt%%y.etype,Arx3(y.size1, y.size2, y.size3, fun (i,j,k) -> x/y[i,j,k]))
        static member (/) (x:num3,y:num0) = num3(x.etype%%y.etype,Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k]/y))
        static member (/) (x:num3,y:int) = num3(x.etype%%It 4,Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k]/y))
        static member (/) (x:num3,y:double) = num3(x.etype%%Dt,Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k]/y))

        static member (./) (x:num3,y:num3) =
            num3.sizeMismatchError(x,y)
            Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k]/y[i,j,k])
        static member (./) (x:num0,y:num3) = num3(x.etype%%y.etype,Arx3(y.size1, y.size2, y.size3, fun (i,j,k) -> x./y[i,j,k]))
        static member (./) (x:int,y:num3) = num3(It 4%%y.etype,Arx3(y.size1, y.size2, y.size3, fun (i,j,k) -> x./y[i,j,k]))
        static member (./) (x:num3,y:num0) = num3(x.etype%%y.etype,Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k]./y))
        static member (./) (x:num3,y:int) = num3(x.etype%%It 4,Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k]./y))
        
        static member (<==) (v1:num3,v2:num3) =
            if debug.debugMode then
                error.inc()
                comment("***debug array1 access check: "+error.ID+"*****************************")
                br.if1 (v1.size1 .=/ v2.size1) <| fun () -> 
                    print.t ("ERROR"+error.ID+" operator '<==' array size1 mismatch")
                br.if1 (v1.size2 .=/ v2.size2) <| fun () -> 
                    print.t ("ERROR"+error.ID+" operator '<==' array size2 mismatch")
                br.if1 (v1.size3 .=/ v2.size3) <| fun () -> 
                    print.t ("ERROR"+error.ID+" operator '<==' array size3 mismatch")
                comment("****************************************************")
            match v1.Expr,v2.Expr with
            |Var3(_,x),Var3(_,y) ->
                match programList[prIndex].language with
                |Fortran|LaTeX ->
                    codewritein(x + "=" + y)
                |C99 ->
                    iter.num v1.size1 <| fun i -> iter.num v1.size2 <| fun j -> iter.num v1.size3 <| fun k -> v1[i,j,k] <== v2[i,j,k]
                |HTML ->
                    codewritein(x + " \\leftarrow " + y)
                |HTMLSequenceDiagram ->
                    codewritein(x + " \\leftarrow " + y)
                |Python ->
                    codewritein(x + " = copy.deepcopy("+y+")")
                |JavaScript ->
                    iter.num v1.size1 <| fun i -> iter.num v1.size2 <| fun j -> iter.num v1.size3 <| fun k -> v1[i,j,k] <== v2[i,j,k]
                |PHP ->
                    iter.num v1.size1 <| fun i -> iter.num v1.size2 <| fun j -> iter.num v1.size3 <| fun k -> v1[i,j,k] <== v2[i,j,k]
                |Numeric -> ()
            |Var3(_,x),Arx3(_,_,_,f) ->
                match programList[prIndex].language with
                |Fortran|LaTeX|C99|HTML|HTMLSequenceDiagram|Python|JavaScript|PHP|Numeric -> iter.num v1.size1 <| fun i -> iter.num v1.size2 <| fun j -> iter.num v1.size3 <| fun k -> v1[i,j,k] <== v2[i,j,k]
            |Arx3(_,_,_,_),Var3(_,_) ->
                match programList[prIndex].language with
                |Fortran|LaTeX|C99|HTML|HTMLSequenceDiagram|Python|JavaScript|PHP|Numeric -> iter.num v1.size1 <| fun i -> iter.num v1.size2 <| fun j -> iter.num v1.size3 <| fun k -> v1[i,j,k] <== v2[i,j,k]
            |Arx3(_,_,_,_),Arx3(_,_,_,_) ->
                match programList[prIndex].language with
                |Fortran|LaTeX|C99|HTML|HTMLSequenceDiagram|Python|JavaScript|PHP|Numeric -> iter.num v1.size1 <| fun i -> iter.num v1.size2 <| fun j -> iter.num v1.size3 <| fun k -> v1[i,j,k] <== v2[i,j,k]
        static member (<==) (v1:num3,v2:num0) =
            match v1.Expr with
            |Var3(_,x) ->
                match programList[prIndex].language with
                |Fortran|LaTeX ->
                    codewritein(x + "=" + v2.Expr.eval (programList[prIndex]))
                |C99 ->
                    iter.num v1.size1 <| fun i -> iter.num v1.size2 <| fun j -> iter.num v1.size3 <| fun k -> v1[i,j,k] <== v2
                |HTML ->
                    codewritein(x + " \\leftarrow " + v2.Expr.eval (programList[prIndex]))
                |HTMLSequenceDiagram ->
                    codewritein(x + " \\leftarrow " + v2.Expr.eval (programList[prIndex]))
                |Python ->
                    match v1.etype with
                    |Structure sname -> codewritein(x + " = numpy.array([[["+sname+"() for _ in range(int("+v1.size3.Expr.eval (programList[prIndex])+"))] for _ in range(int("+v1.size2.Expr.eval (programList[prIndex])+"))] for _ in range(int("+v1.size1.Expr.eval (programList[prIndex])+"))], dtype=object).reshape(int("+v1.size1.Expr.eval (programList[prIndex])+"),int("+v1.size2.Expr.eval (programList[prIndex])+"),int("+v1.size3.Expr.eval (programList[prIndex])+"))\n")
                    |_               -> codewritein(x+"[:,:,:]="+v2.Expr.eval (programList[prIndex])+"\n")
                |JavaScript ->
                    iter.num v1.size1 <| fun i -> iter.num v1.size2 <| fun j -> iter.num v1.size3 <| fun k -> v1[i,j,k] <== v2
                |PHP ->
                    iter.num v1.size1 <| fun i -> iter.num v1.size2 <| fun j -> iter.num v1.size3 <| fun k -> v1[i,j,k] <== v2
                |Numeric -> ()
            |Arx3(_,_,_,_) ->
                match programList[prIndex].language with
                |Fortran|LaTeX|C99|HTML|HTMLSequenceDiagram|Python|JavaScript|PHP|Numeric -> iter.num v1.size1 <| fun i -> iter.num v1.size2 <| fun j -> iter.num v1.size3 <| fun k -> v1[i,j,k] <== v2
        static member (<==) (v1:num3,v2:double) =
            v1 <== D v2
        static member (<==) (v1:num3,v2:int) =
            v1 <== I v2
            
    [<AutoOpen>]
    module asm_num3 =
        type asm with
            static member pow(x:num3,y:num0) = num3(x.etype%%y.etype, Arx3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.pow(x[i,j,k],y)))
            static member sin(x:num3) = num3(x.etype, Arx3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.sin(x[i,j,k])))
            static member cos(x:num3) = num3(x.etype, Arx3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.cos(x[i,j,k])))
            static member tan(x:num3) = num3(x.etype, Arx3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.tan(x[i,j,k])))
            static member asin(x:num3) = num3(x.etype, Arx3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.asin(x[i,j,k])))
            static member acos(x:num3) = num3(x.etype, Arx3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.acos(x[i,j,k])))
            static member atan(x:num3) = num3(Dt, Arx3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.atan(x[i,j,k])))
            static member atan2(x:num3,y:num3) = num3(Dt, Arx3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.atan2(x[i,j,k],y[i,j,k])))
            static member exp(x:num3) = num3(x.etype, Arx3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.exp(x[i,j,k])))
            static member abs(x:num3) = num3(x.etype, Arx3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.abs(x[i,j,k])))
            static member log(x:num3) = num3(x.etype, Arx3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.log(x[i,j,k])))
            static member log10(x:num3) = num3(x.etype, Arx3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.log10(x[i,j,k])))
            static member sqrt(x:num3) = num3(x.etype, Arx3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.sqrt(x[i,j,k])))
            static member floor(x:num3) = num3(x.etype, Arx3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.floor(x[i,j,k])))
            static member ceil(typ,x:num3) = num3(x.etype, Arx3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.ceil(x[i,j,k])))
            static member conj(typ,x:num3) = num3(x.etype, Arx3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.conj(x[i,j,k])))
