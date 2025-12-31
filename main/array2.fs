namespace Aqualis
    
    ///<summary>2次元配列変数</summary>
    type Expr2 =
        ///<summary>変数</summary>
        |Var2 of (VarType*string)
        ///<summary>部分配列</summary>
        |Arx2 of (num0*num0*((num0*num0)->num0))
        
    ///<summary>2次元配列</summary>
    type base2 (typ:Etype,x:Expr2) =
        ///<summary>変数を作成しリストに追加</summary>
        new (typ,size,name,para) = 
            programList[prIndex].var.setVar(typ,size,name,para)
            base2(typ,Var2(size,name))
        ///<summary>変数を作成しリストに追加</summary>
        new(sname,size,name) =
            programList[prIndex].var.setVar(Structure sname,size,name,"")
            base2(Structure sname,Var2(size,name))
        member _.Expr with get() = x
        member _.code with get() =
            match x with
            |Var2(_,x) -> x
            |_ -> "Error(property .code)"
        ///<summary>変数の要素数</summary>
        member _.size1 
          with get() =
            match x with
            |Var2(_,name) -> 
                match programList[prIndex].language with 
                |Fortran -> num0(Var(It 4,name+"_size(1)",NaN))
                |C99 -> num0(Var(It 4,name+"_size[0]",NaN))
                |LaTeX -> num0(Var(It 4,"\\mathcal{S}_1["+name+"]",NaN))
                |HTML -> num0(Var(It 4,"\\mathcal{S}_1["+name+"]",NaN))
                |Python -> num0(Var(It 4,name+"_size[0]",NaN))
                |JavaScript -> num0(Var(It 4,name+"_size[0]",NaN))
                |PHP -> num0(Var(It 4,name+"_size[0]",NaN))
                |Numeric -> num0 NaN
            |Arx2(s1,_,_) -> s1
        ///<summary>変数の要素数</summary>
        member _.size2 
          with get() =
            match x with
            |Var2(_,name) -> 
                match programList[prIndex].language with 
                |Fortran -> num0(Var(It 4,name+"_size(2)",NaN))
                |C99 -> num0(Var(It 4,name+"_size[1]",NaN))
                |LaTeX -> num0(Var(It 4,"\\mathcal{S}_2["+name+"]",NaN))
                |HTML -> num0(Var(It 4,"\\mathcal{S}_2["+name+"]",NaN))
                |Python -> num0(Var(It 4,name+"_size[1]",NaN))
                |JavaScript -> num0(Var(It 4,name+"_size[1]",NaN))
                |PHP -> num0(Var(It 4,name+"_size[1]",NaN))
                |Numeric -> num0 NaN
            |Arx2(_,s2,_) -> s2
        ///<summary>インデクサ</summary>
        member this.Idx2(i:num0,j:num0) =
            if debug.debugMode then
                match x with
                |Var2(_,name) ->
                    error.inc()
                    programList[prIndex].comment("***debug array2 access check: "+error.ID+"*****************************")
                    br.branch <| fun b ->
                        b.IF (Or [this.size1 .= -1; this.size2 .= -1]) <| fun () -> 
                            print.t <| "ERROR" + error.ID + " array " + name + " is not allocated"
                        b.IF (Or [i .< _0; this.size1 .<= i]) <| fun () ->
                            print.w <| "ERROR" + error.ID + " array " + name + " illegal access. index " ++ i ++ " is out of range (1:" ++ this.size1 ++ ")"
                        b.IF (Or [j .< _0; this.size2 .<= j]) <| fun () ->
                            print.w <| "ERROR" + error.ID + " array " + name + " illegal access. index " ++ j ++ " is out of range (1:" ++ this.size2 ++ ")"
                    programList[prIndex].comment "****************************************************"
                |_ -> ()
            match x,language() with
            |Var2(_,name),Fortran -> num0(Idx2(typ,name,(i+1).Expr,(j+1).Expr))
            |Var2(_,name),C99 -> num0(Idx1(typ,name,(i + j * this.size1).Expr))
            |Var2(_,name),_ -> num0(Idx2(typ,name,i.Expr,j.Expr))
            |Arx2(_,_,f),_  -> f (i,j)
            
        member this.Idx2(i:num0,j:int) = this.Idx2(i,I j)
        member this.Idx2(i:int,j:num0) = this.Idx2(I i,j)
        member this.Idx2(i:int,j:int) = this.Idx2(I i,I j)
        member this.Idx2(i:num0,(a2:num0,b2:num0)) = Arx1(b2-a2+_1,  fun j -> this.Idx2(i,j+a2))
        member this.Idx2(i:num0,(a2:num0,b2:int)) = Arx1(b2-a2+_1,  fun j -> this.Idx2(i,j+a2))
        member this.Idx2(i:num0,(a2:int,b2:num0)) = Arx1(b2-a2+_1,  fun j -> this.Idx2(i,j+a2))
        member this.Idx2(i:num0,(a2:int,b2:int)) = Arx1(b2-a2+_1,  fun j -> this.Idx2(i,j+a2))
        member this.Idx2(i:num0,_:unit) = Arx1(this.size2,  fun j -> this.Idx2(i,j))
        member this.Idx2(i:int,(a2:num0,b2:num0)) = Arx1(b2-a2+_1,  fun j -> this.Idx2(I i,j+a2))
        member this.Idx2(i:int,(a2:num0,b2:int)) = Arx1(b2-a2+_1,  fun j -> this.Idx2(I i,j+a2))
        member this.Idx2(i:int,(a2:int,b2:num0)) = Arx1(b2-a2+_1,  fun j -> this.Idx2(I i,j+a2))
        member this.Idx2(i:int,(a2:int,b2:int)) = Arx1(b2-a2+_1,  fun j -> this.Idx2(I i,j+a2))
        member this.Idx2(i:int,_:unit) = Arx1(this.size2,  fun j -> this.Idx2(I i,j))
        member this.Idx2((a1:num0,b1:num0),j:num0) = Arx1(b1-a1+_1,  fun i -> this.Idx2(i+a1,j))
        member this.Idx2((a1:num0,b1:num0),j:int) = Arx1(b1-a1+_1,  fun i -> this.Idx2(i+a1,I j))
        member this.Idx2((a1:num0,b1:num0),(a2:num0,b2:num0)) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx2(i+a1,j+a2))
        member this.Idx2((a1:num0,b1:num0),(a2:num0,b2:int)) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx2(i+a1,j+a2))
        member this.Idx2((a1:num0,b1:num0),(a2:int,b2:num0)) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx2(i+a1,j+a2))
        member this.Idx2((a1:num0,b1:num0),(a2:int,b2:int)) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx2(i+a1,j+a2))
        member this.Idx2((a1:num0,b1:num0),_:unit) = Arx2(b1-a1+_1, this.size2,  fun (i,j) -> this.Idx2(i+a1,j))
        member this.Idx2((a1:num0,b1:int),j:num0) = Arx1(b1-a1+_1,  fun i -> this.Idx2(i+a1,j))
        member this.Idx2((a1:num0,b1:int),j:int) = Arx1(b1-a1+_1,  fun i -> this.Idx2(i+a1,I j))
        member this.Idx2((a1:num0,b1:int),(a2:num0,b2:num0)) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx2(i+a1,j+a2))
        member this.Idx2((a1:num0,b1:int),(a2:num0,b2:int)) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx2(i+a1,j+a2))
        member this.Idx2((a1:num0,b1:int),(a2:int,b2:num0)) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx2(i+a1,j+a2))
        member this.Idx2((a1:num0,b1:int),(a2:int,b2:int)) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx2(i+a1,j+a2))
        member this.Idx2((a1:num0,b1:int),_:unit) = Arx2(b1-a1+_1, this.size2,  fun (i,j) -> this.Idx2(i+a1,j))
        member this.Idx2((a1:int,b1:num0),j:num0) = Arx1(b1-a1+_1,  fun i -> this.Idx2(i+a1,j))
        member this.Idx2((a1:int,b1:num0),j:int) = Arx1(b1-a1+_1,  fun i -> this.Idx2(i+a1,I j))
        member this.Idx2((a1:int,b1:num0),(a2:num0,b2:num0)) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx2(i+a1,j+a2))
        member this.Idx2((a1:int,b1:num0),(a2:num0,b2:int)) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx2(i+a1,j+a2))
        member this.Idx2((a1:int,b1:num0),(a2:int,b2:num0)) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx2(i+a1,j+a2))
        member this.Idx2((a1:int,b1:num0),(a2:int,b2:int)) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx2(i+a1,j+a2))
        member this.Idx2((a1:int,b1:num0),_:unit) = Arx2(b1-a1+_1, this.size2,  fun (i,j) -> this.Idx2(i+a1,j))
        member this.Idx2((a1:int,b1:int),j:num0) = Arx1(b1-a1+_1,  fun i -> this.Idx2(i+a1,j))
        member this.Idx2((a1:int,b1:int),j:int) = Arx1(b1-a1+_1,  fun i -> this.Idx2(i+a1,I j))
        member this.Idx2((a1:int,b1:int),(a2:num0,b2:num0)) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx2(i+a1,j+a2))
        member this.Idx2((a1:int,b1:int),(a2:num0,b2:int)) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx2(i+a1,j+a2))
        member this.Idx2((a1:int,b1:int),(a2:int,b2:num0)) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx2(i+a1,j+a2))
        member this.Idx2((a1:int,b1:int),(a2:int,b2:int)) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx2(i+a1,j+a2))
        member this.Idx2((a1:int,b1:int),_:unit) = Arx2(b1-a1+_1, this.size2,  fun (i,j) -> this.Idx2(i+a1,j))
        member this.Idx2(_:unit,j:num0) = Arx1(this.size1,  fun i -> this.Idx2(i,j))
        member this.Idx2(_:unit,j:int) = Arx1(this.size1,  fun i -> this.Idx2(i,I j))
        member this.Idx2(_:unit,(a2:num0,b2:num0)) = Arx2(this.size1, b2-a2+_1,  fun (i,j) -> this.Idx2(i,j+a2))
        member this.Idx2(_:unit,(a2:num0,b2:int)) = Arx2(this.size1, b2-a2+_1,  fun (i,j) -> this.Idx2(i,j+a2))
        member this.Idx2(_:unit,(a2:int,b2:num0)) = Arx2(this.size1, b2-a2+_1,  fun (i,j) -> this.Idx2(i,j+a2))
        member this.Idx2(_:unit,(a2:int,b2:int)) = Arx2(this.size1, b2-a2+_1,  fun (i,j) -> this.Idx2(i,j+a2))
        
        ///<summary>配列のメモリ割り当て</summary>
        member this.allocate(n1:num0,n2:num0) =
                match x with
                |Var2(size,name) ->
                    if debug.debugMode then
                        error.inc()
                        programList[prIndex].comment("***debug array1 allocate check: "+error.ID+"*****************************")
                        br.branch <| fun b ->
                            b.IF (this.size1 .=/ -1) <| fun () ->
                                print.t ("ERROR"+error.ID+" array "+name+" is already allocated")
                        programList[prIndex].comment "****************************************************"
                    match programList[prIndex].language with
                    |Fortran ->
                        match size with
                        |A2(0,0) ->
                            this.size1 <== n1
                            this.size2 <== n2
                            programList[prIndex].codewrite("allocate("+name+"(1:"+this.size1.Expr.eval (programList[prIndex])+",1:"+this.size2.Expr.eval (programList[prIndex])+")"+")"+"\n")
                        |_ -> 
                            programList[prIndex].codewrite("(Error:055-001 「"+name+"」は可変長2次元配列ではありません")
                    |C99 ->
                        match size with
                        |A2(0,0) ->
                            this.size1 <== n1
                            this.size2 <== n2
                            programList[prIndex].codewrite(name+" = "+"("+typ.tostring(language())+" *)"+"malloc("+"sizeof("+typ.tostring(language())+")*"+this.size1.Expr.eval (programList[prIndex])+"*"+this.size2.Expr.eval (programList[prIndex])+");\n")
                        |_ -> 
                            programList[prIndex].codewrite("(Error:055-001 「"+name+"」は可変長2次元配列ではありません")
                    |LaTeX ->
                        match size,typ with
                        |A2(0,0),It _ ->
                            programList[prIndex].codewrite("$"+name+" \\in \\mathbb{Z}^{"+n1.Expr.eval (programList[prIndex])+"\\times"+n2.Expr.eval (programList[prIndex])+"}$\\\\\n")
                        |A2(0,0),Dt   ->
                            programList[prIndex].codewrite("$"+name+" \\in \\mathbb{R}^{"+n1.Expr.eval (programList[prIndex])+"\\times"+n2.Expr.eval (programList[prIndex])+"}$\\\\\n")
                        |A2(0,0),Zt   ->
                            programList[prIndex].codewrite("$"+name+" \\in \\mathbb{C}^{"+n1.Expr.eval (programList[prIndex])+"\\times"+n2.Expr.eval (programList[prIndex])+"}$\\\\\n")
                        |_ -> 
                            programList[prIndex].codewrite("(Error:055-001 「"+name+"」は可変長2次元配列ではありません")
                    |HTML ->
                        match size,typ with
                        |A2(0,0),It _ ->
                            programList[prIndex].codewrite("\\("+name+" \\in \\mathbb{Z}^{"+n1.Expr.eval (programList[prIndex])+"\\times"+n2.Expr.eval (programList[prIndex])+"}\\)<br>\n")
                        |A2(0,0),Dt   ->
                            programList[prIndex].codewrite("\\("+name+" \\in \\mathbb{R}^{"+n1.Expr.eval (programList[prIndex])+"\\times"+n2.Expr.eval (programList[prIndex])+"}\\)<br>\n")
                        |A2(0,0),Zt   ->
                            programList[prIndex].codewrite("\\("+name+" \\in \\mathbb{C}^{"+n1.Expr.eval (programList[prIndex])+"\\times"+n2.Expr.eval (programList[prIndex])+"}\\)<br>\n")
                        |_ -> 
                            programList[prIndex].codewrite("(Error:055-001 「"+name+"」は可変長2次元配列ではありません")
                    |Python ->
                        match size with
                        |A2(0,0) ->
                            this.size1 <== n1
                            this.size2 <== n2
                            match typ with
                            |Structure(sname) -> programList[prIndex].codewrite(name+" = numpy.array([["+sname+"() for _ in range(int("+this.size2.Expr.eval (programList[prIndex])+"))] for _ in range(int("+this.size1.Expr.eval (programList[prIndex])+"))], dtype=object).reshape(int("+this.size1.Expr.eval (programList[prIndex])+"),int("+this.size2.Expr.eval (programList[prIndex])+"))\n")
                            |It _ |It 1       -> programList[prIndex].codewrite(name+" = numpy.zeros("+this.size1.Expr.eval (programList[prIndex])+"*"+this.size2.Expr.eval (programList[prIndex])+", dtype=int).reshape(int("+this.size1.Expr.eval (programList[prIndex])+"),int("+this.size2.Expr.eval (programList[prIndex])+"))"+"\n")
                            |Zt               -> programList[prIndex].codewrite(name+" = numpy.zeros("+this.size1.Expr.eval (programList[prIndex])+"*"+this.size2.Expr.eval (programList[prIndex])+", dtype=numpy.complex128).reshape(int("+this.size1.Expr.eval (programList[prIndex])+"),int("+this.size2.Expr.eval (programList[prIndex])+"))"+"\n")
                            |_                -> programList[prIndex].codewrite(name+" = numpy.zeros("+this.size1.Expr.eval (programList[prIndex])+"*"+this.size2.Expr.eval (programList[prIndex])+").reshape(int("+this.size1.Expr.eval (programList[prIndex])+"),int("+this.size2.Expr.eval (programList[prIndex])+"))"+"\n")
                        |_ -> 
                            programList[prIndex].codewrite("(Error:055-001 「"+name+"」は可変長2次元配列ではありません")
                    |JavaScript ->
                        match size with
                        |A2(0,0) ->
                            this.size1 <== n1
                            this.size2 <== n2
                            programList[prIndex].codewrite(name+" = "+"Array("+this.size1.Expr.eval (programList[prIndex])+"*"+this.size2.Expr.eval (programList[prIndex])+");\n")
                        |_ -> 
                            programList[prIndex].codewrite("(Error:055-001 「"+name+"」は可変長2次元配列ではありません")
                    |PHP ->
                        match size with
                        |A2(0,0) ->
                            this.size1 <== n1
                            this.size2 <== n2
                            programList[prIndex].codewrite(name+" = [];\n")
                        |_ -> 
                            programList[prIndex].codewrite("(Error:055-001 「"+name+"」は可変長2次元配列ではありません")
                    |Numeric ->
                        ()
                |_ -> ()
                
        member this.allocate(n1:num0,n2:int) = this.allocate(n1,I n2)
        member this.allocate(n1:int,n2:num0) = this.allocate(I n1,n2)
        member this.allocate(n1:int,n2:int) = this.allocate(I n1,I n2)
        
        ///<summary>配列のメモリ割り当て</summary>
        member this.deallocate() =
            if debug.debugMode then
                match x with
                |Var2(_,name) ->
                    error.inc()
                    programList[prIndex].comment("***debug array1 deallocate check: "+error.ID+"*****************************")
                    br.branch <| fun b ->
                        b.IF (this.size1 .= -1) <| fun () ->
                            print.t ("ERROR"+error.ID+" cannot deallocate array "+name)
                    programList[prIndex].comment("****************************************************")
                |_ -> ()
            match x with
            |Var2(size,name) ->
                match programList[prIndex].language with
                |Fortran ->
                    match size with
                    |A2(0,0) ->
                        this.size1 <== -1
                        this.size2 <== -1
                        programList[prIndex].codewrite("deallocate("+name+")"+"\n")
                    |_ -> ()
                |C99 ->
                    match size with
                    |A2(0,0) ->
                        this.size1 <== -1
                        this.size2 <== -1
                        programList[prIndex].codewrite("free("+name+");"+"\n")
                    |_ -> ()
                |LaTeX ->
                    match size with
                    |A2(0,0) ->
                        programList[prIndex].codewrite("$"+name+"$: deallocate\\\\\n")
                    |_ -> ()
                |HTML ->
                    match size with
                    |A2(0,0) ->
                        programList[prIndex].codewrite("\\("+name+"\\): deallocate<br/>\n")
                    |_ -> ()
                |Python ->
                    match size with
                    |A2(0,0) ->
                        this.size1 <== -1
                        this.size2 <== -1
                        programList[prIndex].codewrite("del "+name+""+"\n")
                    |_ -> ()
                |JavaScript ->
                    match size with
                    |A2(0,0) ->
                        this.size1 <== -1
                        programList[prIndex].codewrite(name+"= null;"+"\n")
                    |_ -> ()
                |PHP ->
                    match size with
                    |A2(0,0) ->
                        this.size1 <== -1
                        programList[prIndex].codewrite("unset("+name+");"+"\n")
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
                    code(i,j)
                
        ///<summary>配列の全要素に対する処理</summary>
        member this.foreach_exit code =
            iter.num_exit this.size1 <| fun (ext1,i) -> 
                iter.num_exit this.size2 <| fun (ext2,j) -> 
                    code(ext1,ext2,i,j)
                
        static member sizeMismatchError(x:base2,y:base2) =
            if debug.debugMode then
                error.inc()
                programList[prIndex].comment("***debug array1 access check: "+error.ID+"*****************************")
                br.branch <| fun b ->
                    b.IF (x.size1 .=/ y.size1) <| fun () -> 
                        print.t ("ERROR"+error.ID+" array size (first index) mismatch")
                br.branch <| fun b ->
                    b.IF (x.size2 .=/ y.size2) <| fun () -> 
                        print.t ("ERROR"+error.ID+" array size (second index) mismatch")
                programList[prIndex].comment("****************************************************")
                
    ///<summary>数値型1次元配列</summary>
    type num2 (typ:Etype,x:Expr2) =
        inherit base2(typ,x)
        new (typ,size,name,para) =
            programList[prIndex].var.setVar(typ,size,name,para)
            num2(typ,Var2(size,name))
        member this.etype with get() = typ
        member this.Item with get(i:num0,j:num0) = this.Idx2(i,j)
        member this.Item with get(i:num0,j:int) = this.Idx2(i,I j)
        member this.Item with get(i:int,j:num0) = this.Idx2(I i,j)
        member this.Item with get(i:int,j:int) = this.Idx2(I i,I j)
        member this.Item with get(i:num0,(a2:num0,b2:num0)) = num1(typ,this.Idx2(i,(a2,b2)))
        member this.Item with get(i:num0,(a2:num0,b2:int)) = num1(typ,this.Idx2(i,(a2,I b2)))
        member this.Item with get(i:num0,(a2:int,b2:num0)) = num1(typ,this.Idx2(i,(I a2,b2)))
        member this.Item with get(i:num0,(a2:int,b2:int)) = num1(typ,this.Idx2(i,(I a2,I b2)))
        member this.Item with get(i:num0,_:unit) = num1(typ,this.Idx2(i,()))
        member this.Item with get(i:int,(a2:num0,b2:num0)) = num1(typ,this.Idx2(I i,(a2,b2)))
        member this.Item with get(i:int,(a2:num0,b2:int)) = num1(typ,this.Idx2(I i,(a2,I b2)))
        member this.Item with get(i:int,(a2:int,b2:num0)) = num1(typ,this.Idx2(I i,(I a2,b2)))
        member this.Item with get(i:int,(a2:int,b2:int)) = num1(typ,this.Idx2(I i,(I a2,I b2)))
        member this.Item with get(i:int,_:unit) = num1(typ,this.Idx2(I i,()))
        member this.Item with get((a1:num0,b1:num0),j:num0) = num1(typ,this.Idx2((a1,b1),j))
        member this.Item with get((a1:num0,b1:num0),j:int) = num1(typ,this.Idx2((a1,b1),I j))
        member this.Item with get((a1:num0,b1:num0),(a2:num0,b2:num0)) = num2(typ,this.Idx2((a1,b1),(a2,b2)))
        member this.Item with get((a1:num0,b1:num0),(a2:num0,b2:int)) = num2(typ,this.Idx2((a1,b1),(a2,I b2)))
        member this.Item with get((a1:num0,b1:num0),(a2:int,b2:num0)) = num2(typ,this.Idx2((a1,b1),(I a2,b2)))
        member this.Item with get((a1:num0,b1:num0),(a2:int,b2:int)) = num2(typ,this.Idx2((a1,b1),(I a2,I b2)))
        member this.Item with get((a1:num0,b1:num0),_:unit) = num2(typ,this.Idx2((a1,b1),()))
        member this.Item with get((a1:num0,b1:int),j:num0) = num1(typ,this.Idx2((a1,I b1),j))
        member this.Item with get((a1:num0,b1:int),j:int) = num1(typ,this.Idx2((a1,I b1),I j))
        member this.Item with get((a1:num0,b1:int),(a2:num0,b2:num0)) = num2(typ,this.Idx2((a1,I b1),(a2,b2)))
        member this.Item with get((a1:num0,b1:int),(a2:num0,b2:int)) = num2(typ,this.Idx2((a1,I b1),(a2,I b2)))
        member this.Item with get((a1:num0,b1:int),(a2:int,b2:num0)) = num2(typ,this.Idx2((a1,I b1),(I a2,b2)))
        member this.Item with get((a1:num0,b1:int),(a2:int,b2:int)) = num2(typ,this.Idx2((a1,I b1),(I a2,I b2)))
        member this.Item with get((a1:num0,b1:int),_:unit) = num2(typ,this.Idx2((a1,I b1),()))
        member this.Item with get((a1:int,b1:num0),j:num0) = num1(typ,this.Idx2((I a1,b1),j))
        member this.Item with get((a1:int,b1:num0),j:int) = num1(typ,this.Idx2((I a1,b1),I j))
        member this.Item with get((a1:int,b1:num0),(a2:num0,b2:num0)) = num2(typ,this.Idx2((I a1,b1),(a2,b2)))
        member this.Item with get((a1:int,b1:num0),(a2:num0,b2:int)) = num2(typ,this.Idx2((I a1,b1),(a2,I b2)))
        member this.Item with get((a1:int,b1:num0),(a2:int,b2:num0)) = num2(typ,this.Idx2((I a1,b1),(I a2,b2)))
        member this.Item with get((a1:int,b1:num0),(a2:int,b2:int)) = num2(typ,this.Idx2((I a1,b1),(I a2,I b2)))
        member this.Item with get((a1:int,b1:num0),_:unit) = num2(typ,this.Idx2((I a1,b1),()))
        member this.Item with get((a1:int,b1:int),j:num0) = num1(typ,this.Idx2((I a1,I b1),j))
        member this.Item with get((a1:int,b1:int),j:int) = num1(typ,this.Idx2((I a1,I b1),I j))
        member this.Item with get((a1:int,b1:int),(a2:num0,b2:num0)) = num2(typ,this.Idx2((I a1,I b1),(a2,b2)))
        member this.Item with get((a1:int,b1:int),(a2:num0,b2:int)) = num2(typ,this.Idx2((I a1,I b1),(a2,I b2)))
        member this.Item with get((a1:int,b1:int),(a2:int,b2:num0)) = num2(typ,this.Idx2((I a1,I b1),(I a2,b2)))
        member this.Item with get((a1:int,b1:int),(a2:int,b2:int)) = num2(typ,this.Idx2((I a1,I b1),(I a2,I b2)))
        member this.Item with get((a1:int,b1:int),_:unit) = num2(typ,this.Idx2((I a1,I b1),()))
        member this.Item with get(_:unit,j:num0) = num1(typ,this.Idx2((),j))
        member this.Item with get(_:unit,j:int) = num1(typ,this.Idx2((),I j))
        member this.Item with get(_:unit,(a2:num0,b2:num0)) = num2(typ,this.Idx2((),(a2,b2)))
        member this.Item with get(_:unit,(a2:num0,b2:int)) = num2(typ,this.Idx2((),(a2,I b2)))
        member this.Item with get(_:unit,(a2:int,b2:num0)) = num2(typ,this.Idx2((),(I a2,b2)))
        member this.Item with get(_:unit,(a2:int,b2:int)) = num2(typ,this.Idx2((),(I a2,I b2)))
        
        //<summary>2次元配列生成(整数型)</summary> 
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="f">(i,j)要素に対する要素値</param>
        static member fiarray(s1:num0,s2:num0,f:num0*num0->num0) = num2(It 4,Arx2(s1,s2,f))
        
        //<summary>2次元配列生成(倍精度浮動小数点型)</summary> 
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="f">(i,j)要素に対する要素値</param>
        static member fdarray(s1:num0,s2:num0,f:num0*num0->num0) = num2(Dt,Arx2(s1,s2,f))
        
        //<summary>2次元配列生成(複素数型)</summary> 
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="f">(i,j)要素に対する要素値</param>
        static member fzarray(s1:num0,s2:num0,f:num0*num0->num0) = num2(Zt,Arx2(s1,s2,f))
        
        //<summary>2次元配列生成(整数型)</summary> 
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="f">(i,j)要素に対する要素値</param>
        static member fiarray(s1:int,s2:num0,f:num0*num0->num0) = num2.fiarray(I s1,s2,f)
        
        //<summary>2次元配列生成(倍精度浮動小数点型)</summary> 
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="f">(i,j)要素に対する要素値</param>
        static member fdarray(s1:int,s2:num0,f:num0*num0->num0) = num2.fdarray(I s1,s2,f)
        
        //<summary>2次元配列生成(複素数型)</summary> 
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="f">(i,j)要素に対する要素値</param>
        static member fzarray(s1:int,s2:num0,f:num0*num0->num0) = num2.fzarray(I s1,s2,f)
        
        //<summary>2次元配列生成(整数型)</summary> 
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="f">(i,j)要素に対する要素値</param>
        static member fiarray(s1:num0,s2:int,f:num0*num0->num0) = num2.fiarray(s1,I s2,f)
        
        //<summary>2次元配列生成(倍精度浮動小数点型)</summary> 
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="f">(i,j)要素に対する要素値</param>
        static member fdarray(s1:num0,s2:int,f:num0*num0->num0) = num2.fdarray(s1,I s2,f)
        
        //<summary>2次元配列生成(複素数型)</summary> 
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="f">(i,j)要素に対する要素値</param>
        static member fzarray(s1:num0,s2:int,f:num0*num0->num0) = num2.fzarray(s1,I s2,f)

        //<summary>2次元配列生成(整数型)</summary> 
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="f">(i,j)要素に対する要素値</param>
        static member fiarray(s1:int,s2:int,f:num0*num0->num0) = num2.fiarray(I s1,I s2,f)
        
        //<summary>2次元配列生成(倍精度浮動小数点型)</summary> 
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="f">(i,j)要素に対する要素値</param>
        static member fdarray(s1:int,s2:int,f:num0*num0->num0) = num2.fdarray(I s1,I s2,f)
        
        //<summary>2次元配列生成(複素数型)</summary> 
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="f">(i,j)要素に対する要素値</param>
        static member fzarray(s1:int,s2:int,f:num0*num0->num0) = num2.fzarray(I s1,I s2,f)
        
        //<summary>元の配列と同じサイズの配列生成</summary> 
        ///<param name="f">(i,j)要素に対する要素値</param>
        member this.farray(f:num0*num0->num0) = num2(this.etype,Arx2(this.size1,this.size2,f))
        
        //<summary>値を0で初期化</summary> 
        override this.clear() = 
            this <== I 0
            
        ///<summary>配列サイズ変数をメモリ未割当て状態に初期化</summary>
        override this.sizeinit() = 
            this.size1 <== -1
            
        static member sizeMismatchError(x:num2,y:num2) =
            if debug.debugMode then
                error.inc()
                programList[prIndex].comment("***debug array1 access check: "+error.ID+"*****************************")
                br.if1 (x.size1 .=/ y.size1) <| fun () -> 
                    print.t ("ERROR"+error.ID+" array size1 mismatch")
                br.if1 (x.size2 .=/ y.size2) <| fun () -> 
                    print.t ("ERROR"+error.ID+" array size2 mismatch")
                programList[prIndex].comment("****************************************************")
                
        static member (+) (x:num2,y:num2) =
            num2.sizeMismatchError(x,y)
            num2(x.etype%%y.etype,Arx2(x.size1, x.size2, fun (i,j) -> x[i,j]+y[i,j]))
        static member (+) (x:num0,y:num2) = num2(x.etype%%y.etype,Arx2(y.size1, y.size2, fun (i,j) -> x+y[i,j]))
        static member (+) (x:int,y:num2) = num2((It 4)%%y.etype,Arx2(y.size1, y.size2, fun (i,j) -> x+y[i,j]))
        static member (+) (x:double,y:num2) = num2(Dt%%y.etype,Arx2(y.size1, y.size2, fun (i,j) -> x+y[i,j]))
        static member (+) (x:num2,y:num0) = num2(x.etype%%y.etype,Arx2(x.size1, x.size2, fun (i,j) -> x[i,j]+y))
        static member (+) (x:num2,y:int) = num2(x.etype%%(It 4),Arx2(x.size1, x.size2, fun (i,j) -> x[i,j]+y))
        static member (+) (x:num2,y:double) = num2(x.etype%%Dt,Arx2(x.size1, x.size2, fun (i,j) -> x[i,j]+y))
        
        static member (-) (x:num2,y:num2) =
            num2.sizeMismatchError(x,y)
            num2(x.etype%%y.etype,Arx2(x.size1, x.size2, fun (i,j) -> x[i,j]-y[i,j]))
        static member (-) (x:num0,y:num2) = num2(x.etype%%y.etype,Arx2(y.size1, y.size2, fun (i,j) -> x-y[i,j]))
        static member (-) (x:int,y:num2) = num2((It 4)%%y.etype,Arx2(y.size1, y.size2, fun (i,j) -> x-y[i,j]))
        static member (-) (x:double,y:num2) = num2(Dt%%y.etype,Arx2(y.size1, y.size2, fun (i,j) -> x-y[i,j]))
        static member (-) (x:num2,y:num0) = num2(x.etype%%y.etype,Arx2(x.size1, x.size2, fun (i,j) -> x[i,j]-y))
        static member (-) (x:num2,y:int) = num2(x.etype%%(It 4),Arx2(x.size1, x.size2, fun (i,j) -> x[i,j]-y))
        static member (-) (x:num2,y:double) = num2(x.etype%%Dt,Arx2(x.size1, x.size2, fun (i,j) -> x[i,j]-y))
        
        static member ( * ) (x:num2,y:num2) =
            num2.sizeMismatchError(x,y)
            num2(x.etype%%y.etype,Arx2(x.size1, x.size2, fun (i,j) -> x[i,j]*y[i,j]))
        static member ( * ) (x:num0,y:num2) = num2(x.etype%%y.etype,Arx2(y.size1, y.size2, fun (i,j) -> x*y[i,j]))
        static member ( * ) (x:int,y:num2) = num2((It 4)%%y.etype,Arx2(y.size1, y.size2, fun (i,j) -> x*y[i,j]))
        static member ( * ) (x:double,y:num2) = num2(Dt%%y.etype,Arx2(y.size1, y.size2, fun (i,j) -> x*y[i,j]))
        static member ( * ) (x:num2,y:num0) = num2(x.etype%%y.etype,Arx2(x.size1, x.size2, fun (i,j) -> x[i,j]*y))
        static member ( * ) (x:num2,y:int) = num2(x.etype%%(It 4),Arx2(x.size1, x.size2, fun (i,j) -> x[i,j]*y))
        static member ( * ) (x:num2,y:double) = num2(x.etype%%Dt,Arx2(x.size1, x.size2, fun (i,j) -> x[i,j]*y))
        
        static member (/) (x:num2,y:num2) =
            num2.sizeMismatchError(x,y)
            num2(x.etype%%y.etype,Arx2(x.size1, x.size2, fun (i,j) -> x[i,j]/y[i,j]))
        static member (/) (x:num0,y:num2) = num2(x.etype%%y.etype,Arx2(y.size1, y.size2, fun (i,j) -> x/y[i,j]))
        static member (/) (x:int,y:num2) = num2((It 4)%%y.etype,Arx2(y.size1, y.size2, fun (i,j) -> x/y[i,j]))
        static member (/) (x:double,y:num2) = num2(Dt%%y.etype,Arx2(y.size1, y.size2, fun (i,j) -> x/y[i,j]))
        static member (/) (x:num2,y:num0) = num2(x.etype%%y.etype,Arx2(x.size1, x.size2, fun (i,j) -> x[i,j]/y))
        static member (/) (x:num2,y:int) = num2(x.etype%%(It 4),Arx2(x.size1, x.size2, fun (i,j) -> x[i,j]/y))
        static member (/) (x:num2,y:double) = num2(x.etype%%Dt,Arx2(x.size1, x.size2, fun (i,j) -> x[i,j]/y))

        static member (./) (x:num2,y:num2) =
            num2.sizeMismatchError(x,y)
            num2(x.etype%%y.etype,Arx2(x.size1, x.size2, fun (i,j) -> x[i,j]/y[i,j]))
        static member (./) (x:num0,y:num2) = num2(x.etype%%y.etype,Arx2(y.size1, y.size2, fun (i,j) -> x./y[i,j]))
        static member (./) (x:int,y:num2) = num2((It 4)%%y.etype,Arx2(y.size1, y.size2, fun (i,j) -> x./y[i,j]))
        static member (./) (x:num2,y:num0) = num2(x.etype%%y.etype,Arx2(x.size1, x.size2, fun (i,j) -> x[i,j]./y))
        static member (./) (x:num2,y:int) = num2(x.etype%%(It 4),Arx2(x.size1, x.size2, fun (i,j) -> x[i,j]./y))
        
        static member (<==) (v1:num2,v2:num2) =
            if debug.debugMode then
                error.inc()
                programList[prIndex].comment("***debug array1 access check: "+error.ID+"*****************************")
                br.branch <| fun b ->
                    b.IF (v1.size1 .=/ v2.size1) <| fun () -> 
                        print.t ("ERROR"+error.ID+" operator '<==' array size mismatch")
                br.branch <| fun b ->
                    b.IF (v1.size2 .=/ v2.size2) <| fun () -> 
                        print.t ("ERROR"+error.ID+" operator '<==' array size mismatch")
                programList[prIndex].comment("****************************************************")
            match v1.Expr,v2.Expr with
            |Var2(_,x),Var2(_,y) ->
                match programList[prIndex].language with
                |Fortran|LaTeX ->
                    programList[prIndex].codewrite(x + "=" + y)
                |C99 ->
                    iter.num v1.size1 <| fun i -> iter.num v1.size2 <| fun j -> v1[i,j] <== v2[i,j]
                |HTML ->
                    programList[prIndex].codewrite(x + " \\leftarrow " + y)
                |Python ->
                    programList[prIndex].codewrite(x + " = copy.deepcopy("+y+")")
                |JavaScript ->
                    iter.num v1.size1 <| fun i -> iter.num v1.size2 <| fun j -> v1[i,j] <== v2[i,j]
                |PHP ->
                    iter.num v1.size1 <| fun i -> iter.num v1.size2 <| fun j -> v1[i,j] <== v2[i,j]
                |Numeric -> ()
            |Var2(_,x),Arx2(_,_,f) ->
                match programList[prIndex].language with
                |Fortran|LaTeX|C99|HTML|Python|JavaScript|PHP|Numeric -> iter.num v1.size1 <| fun i -> iter.num v1.size2 <| fun j -> v1[i,j] <== v2[i,j]
            |Arx2(_,_,_),Var2(_,_) ->
                match programList[prIndex].language with
                |Fortran|LaTeX|C99|HTML|Python|JavaScript|PHP|Numeric -> iter.num v1.size1 <| fun i -> iter.num v1.size2 <| fun j -> v1[i,j] <== v2[i,j]
            |Arx2(_,_,_),Arx2(_,_,_) ->
                match programList[prIndex].language with
                |Fortran|LaTeX|C99|HTML|Python|JavaScript|PHP|Numeric -> iter.num v1.size1 <| fun i -> iter.num v1.size2 <| fun j -> v1[i,j] <== v2[i,j]
        static member (<==) (v1:num2,v2:num0) =
            match v1.Expr with
            |Var2(_,x) ->
                match programList[prIndex].language with
                |Fortran|LaTeX ->
                    programList[prIndex].codewrite(x + "=" + v2.Expr.eval (programList[prIndex]))
                |C99 ->
                    iter.num v1.size1 <| fun i -> iter.num v1.size2 <| fun j -> v1[i,j] <== v2
                |HTML ->
                    programList[prIndex].codewrite(x + " \\leftarrow " + v2.Expr.eval (programList[prIndex]))
                |Python ->
                    match v1.etype with
                    |Structure sname -> programList[prIndex].codewrite(x+" = numpy.array([["+sname+"() for _ in range(int("+v1.size2.Expr.eval (programList[prIndex])+"))] for _ in range(int("+v1.size1.Expr.eval (programList[prIndex])+"))], dtype=object).reshape(int("+v1.size1.Expr.eval (programList[prIndex])+"),int("+v1.size2.Expr.eval (programList[prIndex])+"))\n")
                    |_               -> programList[prIndex].codewrite(x+"[:,:]="+v2.Expr.eval (programList[prIndex])+"\n")
                |JavaScript ->
                    iter.num v1.size1 <| fun i -> iter.num v1.size2 <| fun j -> v1[i,j] <== v2
                |PHP ->
                    iter.num v1.size1 <| fun i -> iter.num v1.size2 <| fun j -> v1[i,j] <== v2
                |Numeric -> ()
            |Arx2(_,_,_) ->
                match programList[prIndex].language with
                |Fortran|LaTeX|C99|HTML|Python|JavaScript|PHP|Numeric -> iter.num v1.size1 <| fun i -> iter.num v1.size2 <| fun j -> v1[i,j] <== v2
        static member (<==) (v1:num2,v2:int) =
            v1 <== I v2
        static member (<==) (v1:num2,v2:double) =
            v1 <== D v2
            
    [<AutoOpen>]
    module asm_num2 =
        type asm with
            static member pow(x:num2,y:num0) = num2(x.etype%%y.etype, Arx2(x.size1,x.size2,fun (i,j) -> asm.pow(x[i,j],y)))
            static member sin(x:num2) = num2(x.etype, Arx2(x.size1,x.size2,fun (i,j) -> asm.sin(x[i,j])))
            static member cos(x:num2) = num2(x.etype, Arx2(x.size1,x.size2,fun (i,j) -> asm.cos(x[i,j])))
            static member tan(x:num2) = num2(x.etype, Arx2(x.size1,x.size2,fun (i,j) -> asm.tan(x[i,j])))
            static member asin(x:num2) = num2(x.etype, Arx2(x.size1,x.size2,fun (i,j) -> asm.asin(x[i,j])))
            static member acos(x:num2) = num2(x.etype, Arx2(x.size1,x.size2,fun (i,j) -> asm.acos(x[i,j])))
            static member atan(x:num2) = num2(x.etype, Arx2(x.size1,x.size2,fun (i,j) -> asm.atan(x[i,j])))
            static member atan2(x:num2,y:num2) = num2(Dt, Arx2(x.size1,x.size2,fun (i,j) -> asm.atan2(x[i,j],y[i,j])))
            static member exp(x:num2) = num2(x.etype, Arx2(x.size1,x.size2,fun (i,j) -> asm.exp(x[i,j])))
            static member abs(x:num2) = num2(x.etype, Arx2(x.size1,x.size2,fun (i,j) -> asm.abs(x[i,j])))
            static member log(x:num2) = num2(x.etype, Arx2(x.size1,x.size2,fun (i,j) -> asm.log(x[i,j])))
            static member log10(x:num2) = num2(x.etype, Arx2(x.size1,x.size2,fun (i,j) -> asm.log10(x[i,j])))
            static member sqrt(x:num2) = num2(x.etype, Arx2(x.size1,x.size2,fun (i,j) -> asm.sqrt(x[i,j])))
            static member floor(x:num2) = num2(x.etype, Arx2(x.size1,x.size2,fun (i,j) -> asm.floor(x[i,j])))
            static member ceil(x:num2) = num2(x.etype, Arx2(x.size1,x.size2,fun (i,j) -> asm.ceil(x[i,j])))
            static member conj(x:num2) = num2(x.etype, Arx2(x.size1,x.size2,fun (i,j) -> asm.conj(x[i,j])))
