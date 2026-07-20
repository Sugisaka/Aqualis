//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    ///<summary>2次元配列変数</summary>
    type Expr2 =
        ///<summary>変数</summary>
        |Var2 of (VarType*string)
        ///<summary>部分配列</summary>
        |Arx2 of (int0*int0*((int0*int0)->expr))

    ///<summary>2次元配列</summary>
    type base2 (typ:Etype,x:Expr2) =
        ///<summary>変数を作成しリストに追加</summary>
        new (typ,size,name,para) =
            (GenerationScope.currentProgram()).var.setVar(typ,size,name,para)
            base2(typ,Var2(size,name))
        ///<summary>変数を作成しリストに追加</summary>
        new(sname,size,name) =
            (GenerationScope.currentProgram()).var.setVar(Structure sname,size,name,"")
            base2(Structure sname,Var2(size,name))
        member _.Etype with get() = typ
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
                match (GenerationScope.currentProgram()).language with
                |Fortran ->
                    int0(Var(It 4,name+"_size(1)",NaN))
                |C99 ->
                    int0(Var(It 4,name+"_size[0]",NaN))
                |LaTeX ->
                    int0(Var(It 4,"\\mathcal{S}_1["+name+"]",NaN))
                |HTML ->
                    int0(Var(It 4,"\\mathcal{S}_1["+name+"]",NaN))
                |HTMLSequenceDiagram ->
                    int0(Var(It 4,"\\mathcal{S}_1["+name+"]",NaN))
                |Python ->
                    int0(Var(It 4,name+"_size[0]",NaN))
                |JavaScript ->
                    int0(Var(It 4,name+"_size[0]",NaN))
                |PHP ->
                    int0(Var(It 4,name+"_size[0]",NaN))
                |Numeric ->
                    int0 NaN
            |Arx2(s1,_,_) -> s1
        ///<summary>変数の要素数</summary>
        member _.size2
          with get() =
            match x with
            |Var2(_,name) ->
                match (GenerationScope.currentProgram()).language with
                |Fortran ->
                    int0(Var(It 4,name+"_size(2)",NaN))
                |C99 ->
                    int0(Var(It 4,name+"_size[1]",NaN))
                |LaTeX ->
                    int0(Var(It 4,"\\mathcal{S}_2["+name+"]",NaN))
                |HTML ->
                    int0(Var(It 4,"\\mathcal{S}_2["+name+"]",NaN))
                |HTMLSequenceDiagram ->
                    int0(Var(It 4,"\\mathcal{S}_2["+name+"]",NaN))
                |Python ->
                    int0(Var(It 4,name+"_size[1]",NaN))
                |JavaScript ->
                    int0(Var(It 4,name+"_size[1]",NaN))
                |PHP ->
                    int0(Var(It 4,name+"_size[1]",NaN))
                |Numeric ->
                    int0 NaN
            |Arx2(_,s2,_) -> s2
        ///<summary>インデクサ</summary>
        member this.Idx2(i:int0,j:int0) =
            if (GenerationScope.debug()).debugMode then
                match x with
                |Var2(_,name) ->
                    (GenerationScope.errors()).inc()
                    !("***debug array2 access check: "+(GenerationScope.errors()).ID+"*****************************")
                    br.branch <| fun b ->
                        b.IF (Or [this.size1 .= -1; this.size2 .= -1]) <| fun () ->
                            print.s <| "ERROR" + (GenerationScope.errors()).ID + " array " + name + " is not allocated"
                        b.IF (Or [i .< _0; this.size1 .<= i]) <| fun () ->
                            print.tt <| "ERROR" + (GenerationScope.errors()).ID + " array " + name + " illegal access. index " ++ i ++ " is out of range (1:" ++ this.size1 ++ ")"
                        b.IF (Or [j .< _0; this.size2 .<= j]) <| fun () ->
                            print.tt <| "ERROR" + (GenerationScope.errors()).ID + " array " + name + " illegal access. index " ++ j ++ " is out of range (1:" ++ this.size2 ++ ")"
                    ! "****************************************************"
                |_ -> ()
            match x,language() with
            |Var2(_,name),Fortran -> Idx2(typ,name,(i+1).Expr,(j+1).Expr)
            |Var2(_,name),C99 -> Idx1(typ,name,(i + j * this.size1).Expr)
            |Var2(_,name),_ -> Idx2(typ,name,i.Expr,j.Expr)
            |Arx2(_,_,f),_  -> f (i,j)

        member this.Idx2(i:int0,j:int) = this.Idx2(i,I j)
        member this.Idx2(i:int,j:int0) = this.Idx2(I i,j)
        member this.Idx2(i:int,j:int) = this.Idx2(I i,I j)
        member this.Idx2(i:int0,(a2:int0,b2:int0)) = Arx1(b2-a2+_1,  fun j -> this.Idx2(i,j+a2))
        member this.Idx2(i:int0,(a2:int0,b2:int)) = Arx1(b2-a2+_1,  fun j -> this.Idx2(i,j+a2))
        member this.Idx2(i:int0,(a2:int,b2:int0)) = Arx1(b2-a2+_1,  fun j -> this.Idx2(i,j+a2))
        member this.Idx2(i:int0,(a2:int,b2:int)) = Arx1(b2-a2+_1,  fun j -> this.Idx2(i,j+a2))
        member this.Idx2(i:int0,_:unit) = Arx1(this.size2,  fun j -> this.Idx2(i,j))
        member this.Idx2(i:int,(a2:int0,b2:int0)) = Arx1(b2-a2+_1,  fun j -> this.Idx2(I i,j+a2))
        member this.Idx2(i:int,(a2:int0,b2:int)) = Arx1(b2-a2+_1,  fun j -> this.Idx2(I i,j+a2))
        member this.Idx2(i:int,(a2:int,b2:int0)) = Arx1(b2-a2+_1,  fun j -> this.Idx2(I i,j+a2))
        member this.Idx2(i:int,(a2:int,b2:int)) = Arx1(b2-a2+_1,  fun j -> this.Idx2(I i,j+a2))
        member this.Idx2(i:int,_:unit) = Arx1(this.size2,  fun j -> this.Idx2(I i,j))
        member this.Idx2((a1:int0,b1:int0),j:int0) = Arx1(b1-a1+_1,  fun i -> this.Idx2(i+a1,j))
        member this.Idx2((a1:int0,b1:int0),j:int) = Arx1(b1-a1+_1,  fun i -> this.Idx2(i+a1,I j))
        member this.Idx2((a1:int0,b1:int0),(a2:int0,b2:int0)) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx2(i+a1,j+a2))
        member this.Idx2((a1:int0,b1:int0),(a2:int0,b2:int)) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx2(i+a1,j+a2))
        member this.Idx2((a1:int0,b1:int0),(a2:int,b2:int0)) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx2(i+a1,j+a2))
        member this.Idx2((a1:int0,b1:int0),(a2:int,b2:int)) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx2(i+a1,j+a2))
        member this.Idx2((a1:int0,b1:int0),_:unit) = Arx2(b1-a1+_1, this.size2,  fun (i,j) -> this.Idx2(i+a1,j))
        member this.Idx2((a1:int0,b1:int),j:int0) = Arx1(b1-a1+_1,  fun i -> this.Idx2(i+a1,j))
        member this.Idx2((a1:int0,b1:int),j:int) = Arx1(b1-a1+_1,  fun i -> this.Idx2(i+a1,I j))
        member this.Idx2((a1:int0,b1:int),(a2:int0,b2:int0)) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx2(i+a1,j+a2))
        member this.Idx2((a1:int0,b1:int),(a2:int0,b2:int)) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx2(i+a1,j+a2))
        member this.Idx2((a1:int0,b1:int),(a2:int,b2:int0)) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx2(i+a1,j+a2))
        member this.Idx2((a1:int0,b1:int),(a2:int,b2:int)) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx2(i+a1,j+a2))
        member this.Idx2((a1:int0,b1:int),_:unit) = Arx2(b1-a1+_1, this.size2,  fun (i,j) -> this.Idx2(i+a1,j))
        member this.Idx2((a1:int,b1:int0),j:int0) = Arx1(b1-a1+_1,  fun i -> this.Idx2(i+a1,j))
        member this.Idx2((a1:int,b1:int0),j:int) = Arx1(b1-a1+_1,  fun i -> this.Idx2(i+a1,I j))
        member this.Idx2((a1:int,b1:int0),(a2:int0,b2:int0)) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx2(i+a1,j+a2))
        member this.Idx2((a1:int,b1:int0),(a2:int0,b2:int)) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx2(i+a1,j+a2))
        member this.Idx2((a1:int,b1:int0),(a2:int,b2:int0)) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx2(i+a1,j+a2))
        member this.Idx2((a1:int,b1:int0),(a2:int,b2:int)) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx2(i+a1,j+a2))
        member this.Idx2((a1:int,b1:int0),_:unit) = Arx2(b1-a1+_1, this.size2,  fun (i,j) -> this.Idx2(i+a1,j))
        member this.Idx2((a1:int,b1:int),j:int0) = Arx1(b1-a1+_1,  fun i -> this.Idx2(i+a1,j))
        member this.Idx2((a1:int,b1:int),j:int) = Arx1(b1-a1+_1,  fun i -> this.Idx2(i+a1,I j))
        member this.Idx2((a1:int,b1:int),(a2:int0,b2:int0)) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx2(i+a1,j+a2))
        member this.Idx2((a1:int,b1:int),(a2:int0,b2:int)) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx2(i+a1,j+a2))
        member this.Idx2((a1:int,b1:int),(a2:int,b2:int0)) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx2(i+a1,j+a2))
        member this.Idx2((a1:int,b1:int),(a2:int,b2:int)) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx2(i+a1,j+a2))
        member this.Idx2((a1:int,b1:int),_:unit) = Arx2(b1-a1+_1, this.size2,  fun (i,j) -> this.Idx2(i+a1,j))
        member this.Idx2(_:unit,j:int0) = Arx1(this.size1,  fun i -> this.Idx2(i,j))
        member this.Idx2(_:unit,j:int) = Arx1(this.size1,  fun i -> this.Idx2(i,I j))
        member this.Idx2(_:unit,(a2:int0,b2:int0)) = Arx2(this.size1, b2-a2+_1,  fun (i,j) -> this.Idx2(i,j+a2))
        member this.Idx2(_:unit,(a2:int0,b2:int)) = Arx2(this.size1, b2-a2+_1,  fun (i,j) -> this.Idx2(i,j+a2))
        member this.Idx2(_:unit,(a2:int,b2:int0)) = Arx2(this.size1, b2-a2+_1,  fun (i,j) -> this.Idx2(i,j+a2))
        member this.Idx2(_:unit,(a2:int,b2:int)) = Arx2(this.size1, b2-a2+_1,  fun (i,j) -> this.Idx2(i,j+a2))

        ///<summary>配列のメモリ割り当て</summary>
        member this.allocate(n1:int0,n2:int0) =
                match x with
                |Var2(size,name) ->
                    if (GenerationScope.debug()).debugMode then
                        (GenerationScope.errors()).inc()
                        !("***debug array1 allocate check: "+(GenerationScope.errors()).ID+"*****************************")
                        br.branch <| fun b ->
                            b.IF (this.size1 .=/ -1) <| fun () ->
                                print.s ("ERROR"+(GenerationScope.errors()).ID+" array "+name+" is already allocated")
                        ! "****************************************************"
                    match (GenerationScope.currentProgram()).language with
                    |Fortran ->
                        match size with
                        |A2(0,0) ->
                            this.size1 <== n1
                            this.size2 <== n2
                            writein("allocate("+name+"(1:"+this.size1.Expr.eval ((GenerationScope.currentProgram()))+",1:"+this.size2.Expr.eval ((GenerationScope.currentProgram()))+")"+")"+"\n")
                        |_ ->
                            writein("(Error:055-001 「"+name+"」は可変長2次元配列ではありません")
                    |C99 ->
                        match size with
                        |A2(0,0) ->
                            this.size1 <== n1
                            this.size2 <== n2
                            writein(name+" = "+"("+typ.tostring(language())+" *)"+"malloc("+"sizeof("+typ.tostring(language())+")*"+this.size1.Expr.eval ((GenerationScope.currentProgram()))+"*"+this.size2.Expr.eval ((GenerationScope.currentProgram()))+");\n")
                        |_ ->
                            writein("(Error:055-001 「"+name+"」は可変長2次元配列ではありません")
                    |LaTeX ->
                        match size,typ with
                        |A2(0,0),It _ ->
                            writein("$"+name+" \\in \\mathbb{Z}^{"+n1.Expr.eval ((GenerationScope.currentProgram()))+"\\times"+n2.Expr.eval ((GenerationScope.currentProgram()))+"}$\\\\\n")
                        |A2(0,0),Dt   ->
                            writein("$"+name+" \\in \\mathbb{R}^{"+n1.Expr.eval ((GenerationScope.currentProgram()))+"\\times"+n2.Expr.eval ((GenerationScope.currentProgram()))+"}$\\\\\n")
                        |A2(0,0),Zt   ->
                            writein("$"+name+" \\in \\mathbb{C}^{"+n1.Expr.eval ((GenerationScope.currentProgram()))+"\\times"+n2.Expr.eval ((GenerationScope.currentProgram()))+"}$\\\\\n")
                        |_ ->
                            writein("(Error:055-001 「"+name+"」は可変長2次元配列ではありません")
                    |HTML ->
                        match size,typ with
                        |A2(0,0),It _ ->
                            writein("\\("+name+" \\in \\mathbb{Z}^{"+n1.Expr.eval ((GenerationScope.currentProgram()))+"\\times"+n2.Expr.eval ((GenerationScope.currentProgram()))+"}\\)<br>\n")
                        |A2(0,0),Dt   ->
                            writein("\\("+name+" \\in \\mathbb{R}^{"+n1.Expr.eval ((GenerationScope.currentProgram()))+"\\times"+n2.Expr.eval ((GenerationScope.currentProgram()))+"}\\)<br>\n")
                        |A2(0,0),Zt   ->
                            writein("\\("+name+" \\in \\mathbb{C}^{"+n1.Expr.eval ((GenerationScope.currentProgram()))+"\\times"+n2.Expr.eval ((GenerationScope.currentProgram()))+"}\\)<br>\n")
                        |_ ->
                            writein("(Error:055-001 「"+name+"」は可変長2次元配列ではありません")
                    |HTMLSequenceDiagram ->
                        match size,typ with
                        |A2(0,0),It _ ->
                            writein("\\("+name+" \\in \\mathbb{Z}^{"+n1.Expr.eval ((GenerationScope.currentProgram()))+"\\times"+n2.Expr.eval ((GenerationScope.currentProgram()))+"}\\)<br>\n")
                        |A2(0,0),Dt   ->
                            writein("\\("+name+" \\in \\mathbb{R}^{"+n1.Expr.eval ((GenerationScope.currentProgram()))+"\\times"+n2.Expr.eval ((GenerationScope.currentProgram()))+"}\\)<br>\n")
                        |A2(0,0),Zt   ->
                            writein("\\("+name+" \\in \\mathbb{C}^{"+n1.Expr.eval ((GenerationScope.currentProgram()))+"\\times"+n2.Expr.eval ((GenerationScope.currentProgram()))+"}\\)<br>\n")
                        |_ ->
                            writein("(Error:055-001 「"+name+"」は可変長2次元配列ではありません")
                    |Python ->
                        match size with
                        |A2(0,0) ->
                            this.size1 <== n1
                            this.size2 <== n2
                            match typ with
                            |Structure sname -> writein(name+" = numpy.array([["+sname+"() for _ in range(int("+this.size2.Expr.eval ((GenerationScope.currentProgram()))+"))] for _ in range(int("+this.size1.Expr.eval ((GenerationScope.currentProgram()))+"))], dtype=object).reshape(int("+this.size1.Expr.eval ((GenerationScope.currentProgram()))+"),int("+this.size2.Expr.eval ((GenerationScope.currentProgram()))+"))\n")
                            |It _ |It 1      -> writein(name+" = numpy.zeros("+this.size1.Expr.eval ((GenerationScope.currentProgram()))+"*"+this.size2.Expr.eval ((GenerationScope.currentProgram()))+", dtype=int).reshape(int("+this.size1.Expr.eval ((GenerationScope.currentProgram()))+"),int("+this.size2.Expr.eval ((GenerationScope.currentProgram()))+"))"+"\n")
                            |Zt              -> writein(name+" = numpy.zeros("+this.size1.Expr.eval ((GenerationScope.currentProgram()))+"*"+this.size2.Expr.eval ((GenerationScope.currentProgram()))+", dtype=numpy.complex128).reshape(int("+this.size1.Expr.eval ((GenerationScope.currentProgram()))+"),int("+this.size2.Expr.eval ((GenerationScope.currentProgram()))+"))"+"\n")
                            |_               -> writein(name+" = numpy.zeros("+this.size1.Expr.eval ((GenerationScope.currentProgram()))+"*"+this.size2.Expr.eval ((GenerationScope.currentProgram()))+").reshape(int("+this.size1.Expr.eval ((GenerationScope.currentProgram()))+"),int("+this.size2.Expr.eval ((GenerationScope.currentProgram()))+"))"+"\n")
                        |_ ->
                            writein("(Error:055-001 「"+name+"」は可変長2次元配列ではありません")
                    |JavaScript ->
                        match size with
                        |A2(0,0) ->
                            this.size1 <== n1
                            this.size2 <== n2
                            writein(name+" = "+"Array("+this.size1.Expr.eval ((GenerationScope.currentProgram()))+"*"+this.size2.Expr.eval ((GenerationScope.currentProgram()))+");\n")
                        |_ ->
                            writein("(Error:055-001 「"+name+"」は可変長2次元配列ではありません")
                    |PHP ->
                        match size with
                        |A2(0,0) ->
                            this.size1 <== n1
                            this.size2 <== n2
                            writein(name+" = [];\n")
                        |_ ->
                            writein("(Error:055-001 「"+name+"」は可変長2次元配列ではありません")
                    |Numeric ->
                        ()
                |_ -> ()

        member this.allocate(n1:int0,n2:int) = this.allocate(n1,I n2)
        member this.allocate(n1:int,n2:int0) = this.allocate(I n1,n2)
        member this.allocate(n1:int,n2:int) = this.allocate(I n1,I n2)

        ///<summary>配列のメモリ割り当て</summary>
        member this.deallocate() =
            if (GenerationScope.debug()).debugMode then
                match x with
                |Var2(_,name) ->
                    (GenerationScope.errors()).inc()
                    !("***debug array1 deallocate check: "+(GenerationScope.errors()).ID+"*****************************")
                    br.branch <| fun b ->
                        b.IF (this.size1 .= -1) <| fun () ->
                            print.s ("ERROR"+(GenerationScope.errors()).ID+" cannot deallocate array "+name)
                    !("****************************************************")
                |_ -> ()
            match x with
            |Var2(size,name) ->
                match (GenerationScope.currentProgram()).language with
                |Fortran ->
                    match size with
                    |A2(0,0) ->
                        this.size1 <== -1
                        this.size2 <== -1
                        writein("deallocate("+name+")"+"\n")
                    |_ -> ()
                |C99 ->
                    match size with
                    |A2(0,0) ->
                        this.size1 <== -1
                        this.size2 <== -1
                        writein("free("+name+");"+"\n")
                    |_ -> ()
                |LaTeX ->
                    match size with
                    |A2(0,0) ->
                        writein("$"+name+"$: deallocate\\\\\n")
                    |_ -> ()
                |HTML ->
                    match size with
                    |A2(0,0) ->
                        writein("\\("+name+"\\): deallocate<br/>\n")
                    |_ -> ()
                |HTMLSequenceDiagram ->
                    match size with
                    |A2(0,0) ->
                        writein("\\("+name+"\\): deallocate<br/>\n")
                    |_ -> ()
                |Python ->
                    match size with
                    |A2(0,0) ->
                        this.size1 <== -1
                        this.size2 <== -1
                        writein("del "+name+""+"\n")
                    |_ -> ()
                |JavaScript ->
                    match size with
                    |A2(0,0) ->
                        this.size1 <== -1
                        writein(name+"= null;"+"\n")
                    |_ -> ()
                |PHP ->
                    match size with
                    |A2(0,0) ->
                        this.size1 <== -1
                        writein("unset("+name+");"+"\n")
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
        member this.Foreach (counterName1:string,counterName2:string) code =
            iter.num (this.size1,counterName1) <| fun i ->
                iter.num (this.size2,counterName2) <| fun j ->
                    code(i,j)

        ///<summary>配列の全要素に対する処理</summary>
        member this.foreach_exit code =
            iter.num_exit this.size1 <| fun (ext1,i) ->
                iter.num_exit this.size2 <| fun (ext2,j) ->
                    code(ext1,ext2,i,j)

        ///<summary>配列の全要素に対する処理</summary>
        member this.Foreach_exit (counterName1:string,counterName2:string) code =
            iter.num_exit (this.size1,counterName1) <| fun (ext1,i) ->
                iter.num_exit (this.size2,counterName1) <| fun (ext2,j) ->
                    code(ext1,ext2,i,j)

        static member sizeMismatchError(x:base2,y:base2) =
            if (GenerationScope.debug()).debugMode then
                (GenerationScope.errors()).inc()
                !("***debug array1 access check: "+(GenerationScope.errors()).ID+"*****************************")
                br.branch <| fun b ->
                    b.IF (x.size1 .=/ y.size1) <| fun () ->
                        print.s ("ERROR"+(GenerationScope.errors()).ID+" array size (first index) mismatch")
                br.branch <| fun b ->
                    b.IF (x.size2 .=/ y.size2) <| fun () ->
                        print.s ("ERROR"+(GenerationScope.errors()).ID+" array size (second index) mismatch")
                ! "****************************************************"

    /// Shared implementation for two-dimensional numeric arrays.
    [<AbstractClass>]
    type NumericArray2<'Scalar,'Row,'Self
        when 'Scalar :> INum0
        and 'Self :> NumericArray2<'Scalar,'Row,'Self>>
        (typ:Etype,x:Expr2,?context:GenerationContext) =
        inherit base2(typ,x)

        let context = defaultArg (context |> Option.map Some) GenerationContext.TryCurrent
        member _.Context = context
        member _.etype = typ
        abstract member WrapScalar: expr -> 'Scalar
        abstract member WrapRow: Expr1 -> 'Row
        abstract member Create: Etype * Expr2 -> 'Self
        abstract member AssignAt: int0 * int0 * expr -> unit

        member this.Item with get(i:int0,j:int0) = this.WrapScalar(this.Idx2(i,j))
        member this.Item with get(i:int0,j:int) = this.WrapScalar(this.Idx2(i,I j))
        member this.Item with get(i:int,j:int0) = this.WrapScalar(this.Idx2(I i,j))
        member this.Item with get(i:int,j:int) = this.WrapScalar(this.Idx2(I i,I j))
        member this.Item with get(i:int0,(a:int0,b:int0)) = this.WrapRow(this.Idx2(i,(a,b)))
        member this.Item with get(i:int0,(a:int0,b:int)) = this.WrapRow(this.Idx2(i,(a,I b)))
        member this.Item with get(i:int0,(a:int,b:int0)) = this.WrapRow(this.Idx2(i,(I a,b)))
        member this.Item with get(i:int0,(a:int,b:int)) = this.WrapRow(this.Idx2(i,(I a,I b)))
        member this.Item with get(i:int0,_:unit) = this.WrapRow(this.Idx2(i,()))
        member this.Item with get(i:int,(a:int0,b:int0)) = this.WrapRow(this.Idx2(I i,(a,b)))
        member this.Item with get(i:int,(a:int0,b:int)) = this.WrapRow(this.Idx2(I i,(a,I b)))
        member this.Item with get(i:int,(a:int,b:int0)) = this.WrapRow(this.Idx2(I i,(I a,b)))
        member this.Item with get(i:int,(a:int,b:int)) = this.WrapRow(this.Idx2(I i,(I a,I b)))
        member this.Item with get(i:int,_:unit) = this.WrapRow(this.Idx2(I i,()))
        member this.Item with get((a:int0,b:int0),j:int0) = this.WrapRow(this.Idx2((a,b),j))
        member this.Item with get((a:int0,b:int0),j:int) = this.WrapRow(this.Idx2((a,b),I j))
        member this.Item with get((a:int0,b:int0),(c:int0,d:int0)) = this.Create(typ,this.Idx2((a,b),(c,d)))
        member this.Item with get((a:int0,b:int0),(c:int0,d:int)) = this.Create(typ,this.Idx2((a,b),(c,I d)))
        member this.Item with get((a:int0,b:int0),(c:int,d:int0)) = this.Create(typ,this.Idx2((a,b),(I c,d)))
        member this.Item with get((a:int0,b:int0),(c:int,d:int)) = this.Create(typ,this.Idx2((a,b),(I c,I d)))
        member this.Item with get((a:int0,b:int0),_:unit) = this.Create(typ,this.Idx2((a,b),()))
        member this.Item with get((a:int0,b:int),j:int0) = this.WrapRow(this.Idx2((a,I b),j))
        member this.Item with get((a:int0,b:int),j:int) = this.WrapRow(this.Idx2((a,I b),I j))
        member this.Item with get((a:int0,b:int),(c:int0,d:int0)) = this.Create(typ,this.Idx2((a,I b),(c,d)))
        member this.Item with get((a:int0,b:int),(c:int0,d:int)) = this.Create(typ,this.Idx2((a,I b),(c,I d)))
        member this.Item with get((a:int0,b:int),(c:int,d:int0)) = this.Create(typ,this.Idx2((a,I b),(I c,d)))
        member this.Item with get((a:int0,b:int),(c:int,d:int)) = this.Create(typ,this.Idx2((a,I b),(I c,I d)))
        member this.Item with get((a:int0,b:int),_:unit) = this.Create(typ,this.Idx2((a,I b),()))
        member this.Item with get((a:int,b:int0),j:int0) = this.WrapRow(this.Idx2((I a,b),j))
        member this.Item with get((a:int,b:int0),j:int) = this.WrapRow(this.Idx2((I a,b),I j))
        member this.Item with get((a:int,b:int0),(c:int0,d:int0)) = this.Create(typ,this.Idx2((I a,b),(c,d)))
        member this.Item with get((a:int,b:int0),(c:int0,d:int)) = this.Create(typ,this.Idx2((I a,b),(c,I d)))
        member this.Item with get((a:int,b:int0),(c:int,d:int0)) = this.Create(typ,this.Idx2((I a,b),(I c,d)))
        member this.Item with get((a:int,b:int0),(c:int,d:int)) = this.Create(typ,this.Idx2((I a,b),(I c,I d)))
        member this.Item with get((a:int,b:int0),_:unit) = this.Create(typ,this.Idx2((I a,b),()))
        member this.Item with get((a:int,b:int),j:int0) = this.WrapRow(this.Idx2((I a,I b),j))
        member this.Item with get((a:int,b:int),j:int) = this.WrapRow(this.Idx2((I a,I b),I j))
        member this.Item with get((a:int,b:int),(c:int0,d:int0)) = this.Create(typ,this.Idx2((I a,I b),(c,d)))
        member this.Item with get((a:int,b:int),(c:int0,d:int)) = this.Create(typ,this.Idx2((I a,I b),(c,I d)))
        member this.Item with get((a:int,b:int),(c:int,d:int0)) = this.Create(typ,this.Idx2((I a,I b),(I c,d)))
        member this.Item with get((a:int,b:int),(c:int,d:int)) = this.Create(typ,this.Idx2((I a,I b),(I c,I d)))
        member this.Item with get((a:int,b:int),_:unit) = this.Create(typ,this.Idx2((I a,I b),()))
        member this.Item with get(_:unit,j:int0) = this.WrapRow(this.Idx2((),j))
        member this.Item with get(_:unit,j:int) = this.WrapRow(this.Idx2((),I j))
        member this.Item with get(_:unit,(a:int0,b:int0)) = this.Create(typ,this.Idx2((),(a,b)))
        member this.Item with get(_:unit,(a:int0,b:int)) = this.Create(typ,this.Idx2((),(a,I b)))
        member this.Item with get(_:unit,(a:int,b:int0)) = this.Create(typ,this.Idx2((),(I a,b)))
        member this.Item with get(_:unit,(a:int,b:int)) = this.Create(typ,this.Idx2((),(I a,I b)))

        member private this.New(elementType,body) = this.Create(elementType,Arx2(this.size1,this.size2,body))
        static member private Binary(x:NumericArray2<'Scalar,'Row,'Self>,y:NumericArray2<'Scalar,'Row,'Self>,make:Etype*expr*expr->expr) =
            base2.sizeMismatchError(x,y)
            x.New(x.etype%%y.etype,fun(i,j)->make(x.etype%%y.etype,(x[i,j]:>INum0).Expr,(y[i,j]:>INum0).Expr))
        static member private ScalarLeft(value:INum0,y:NumericArray2<'Scalar,'Row,'Self>,make:Etype*expr*expr->expr) =
            y.New(value.Etype%%y.etype,fun(i,j)->make(value.Etype%%y.etype,value.Expr,(y[i,j]:>INum0).Expr))
        static member private ScalarRight(x:NumericArray2<'Scalar,'Row,'Self>,value:INum0,make:Etype*expr*expr->expr) =
            x.New(x.etype%%value.Etype,fun(i,j)->make(x.etype%%value.Etype,(x[i,j]:>INum0).Expr,value.Expr))
        static member private PrimitiveLeft(elementType,value,y:NumericArray2<'Scalar,'Row,'Self>,make:Etype*expr*expr->expr) =
            y.New(elementType%%y.etype,fun(i,j)->make(elementType%%y.etype,value,(y[i,j]:>INum0).Expr))
        static member private PrimitiveRight(x:NumericArray2<'Scalar,'Row,'Self>,elementType,value,make:Etype*expr*expr->expr) =
            x.New(x.etype%%elementType,fun(i,j)->make(x.etype%%elementType,(x[i,j]:>INum0).Expr,value))

        static member (+)(x:NumericArray2<'Scalar,'Row,'Self>,y:NumericArray2<'Scalar,'Row,'Self>)=NumericArray2.Binary(x,y,fun(t,a,b)->Add(t,a,b))
        static member (+)(x:int0,y:NumericArray2<'Scalar,'Row,'Self>)=NumericArray2.ScalarLeft(x,y,fun(t,a,b)->Add(t,a,b))
        static member (+)(x:double0,y:NumericArray2<'Scalar,'Row,'Self>)=NumericArray2.ScalarLeft(x,y,fun(t,a,b)->Add(t,a,b))
        static member (+)(x:complex0,y:NumericArray2<'Scalar,'Row,'Self>)=NumericArray2.ScalarLeft(x,y,fun(t,a,b)->Add(t,a,b))
        static member (+)(x:int,y:NumericArray2<'Scalar,'Row,'Self>)=NumericArray2.PrimitiveLeft(It 4,Int x,y,fun(t,a,b)->Add(t,a,b))
        static member (+)(x:double,y:NumericArray2<'Scalar,'Row,'Self>)=NumericArray2.PrimitiveLeft(Dt,Dbl x,y,fun(t,a,b)->Add(t,a,b))
        static member (+)(x:NumericArray2<'Scalar,'Row,'Self>,y:int0)=NumericArray2.ScalarRight(x,y,fun(t,a,b)->Add(t,a,b))
        static member (+)(x:NumericArray2<'Scalar,'Row,'Self>,y:double0)=NumericArray2.ScalarRight(x,y,fun(t,a,b)->Add(t,a,b))
        static member (+)(x:NumericArray2<'Scalar,'Row,'Self>,y:complex0)=NumericArray2.ScalarRight(x,y,fun(t,a,b)->Add(t,a,b))
        static member (+)(x:NumericArray2<'Scalar,'Row,'Self>,y:int)=NumericArray2.PrimitiveRight(x,It 4,Int y,fun(t,a,b)->Add(t,a,b))
        static member (+)(x:NumericArray2<'Scalar,'Row,'Self>,y:double)=NumericArray2.PrimitiveRight(x,Dt,Dbl y,fun(t,a,b)->Add(t,a,b))

        static member (-)(x:NumericArray2<'Scalar,'Row,'Self>,y:NumericArray2<'Scalar,'Row,'Self>)=NumericArray2.Binary(x,y,fun(t,a,b)->Sub(t,a,b))
        static member (-)(x:int0,y:NumericArray2<'Scalar,'Row,'Self>)=NumericArray2.ScalarLeft(x,y,fun(t,a,b)->Sub(t,a,b))
        static member (-)(x:double0,y:NumericArray2<'Scalar,'Row,'Self>)=NumericArray2.ScalarLeft(x,y,fun(t,a,b)->Sub(t,a,b))
        static member (-)(x:complex0,y:NumericArray2<'Scalar,'Row,'Self>)=NumericArray2.ScalarLeft(x,y,fun(t,a,b)->Sub(t,a,b))
        static member (-)(x:int,y:NumericArray2<'Scalar,'Row,'Self>)=NumericArray2.PrimitiveLeft(It 4,Int x,y,fun(t,a,b)->Sub(t,a,b))
        static member (-)(x:double,y:NumericArray2<'Scalar,'Row,'Self>)=NumericArray2.PrimitiveLeft(Dt,Dbl x,y,fun(t,a,b)->Sub(t,a,b))
        static member (-)(x:NumericArray2<'Scalar,'Row,'Self>,y:int0)=NumericArray2.ScalarRight(x,y,fun(t,a,b)->Sub(t,a,b))
        static member (-)(x:NumericArray2<'Scalar,'Row,'Self>,y:double0)=NumericArray2.ScalarRight(x,y,fun(t,a,b)->Sub(t,a,b))
        static member (-)(x:NumericArray2<'Scalar,'Row,'Self>,y:complex0)=NumericArray2.ScalarRight(x,y,fun(t,a,b)->Sub(t,a,b))
        static member (-)(x:NumericArray2<'Scalar,'Row,'Self>,y:int)=NumericArray2.PrimitiveRight(x,It 4,Int y,fun(t,a,b)->Sub(t,a,b))
        static member (-)(x:NumericArray2<'Scalar,'Row,'Self>,y:double)=NumericArray2.PrimitiveRight(x,Dt,Dbl y,fun(t,a,b)->Sub(t,a,b))

        static member (*)(x:NumericArray2<'Scalar,'Row,'Self>,y:NumericArray2<'Scalar,'Row,'Self>)=NumericArray2.Binary(x,y,fun(t,a,b)->Mul(t,a,b))
        static member (*)(x:int0,y:NumericArray2<'Scalar,'Row,'Self>)=NumericArray2.ScalarLeft(x,y,fun(t,a,b)->Mul(t,a,b))
        static member (*)(x:double0,y:NumericArray2<'Scalar,'Row,'Self>)=NumericArray2.ScalarLeft(x,y,fun(t,a,b)->Mul(t,a,b))
        static member (*)(x:complex0,y:NumericArray2<'Scalar,'Row,'Self>)=NumericArray2.ScalarLeft(x,y,fun(t,a,b)->Mul(t,a,b))
        static member (*)(x:int,y:NumericArray2<'Scalar,'Row,'Self>)=NumericArray2.PrimitiveLeft(It 4,Int x,y,fun(t,a,b)->Mul(t,a,b))
        static member (*)(x:double,y:NumericArray2<'Scalar,'Row,'Self>)=NumericArray2.PrimitiveLeft(Dt,Dbl x,y,fun(t,a,b)->Mul(t,a,b))
        static member (*)(x:NumericArray2<'Scalar,'Row,'Self>,y:int0)=NumericArray2.ScalarRight(x,y,fun(t,a,b)->Mul(t,a,b))
        static member (*)(x:NumericArray2<'Scalar,'Row,'Self>,y:double0)=NumericArray2.ScalarRight(x,y,fun(t,a,b)->Mul(t,a,b))
        static member (*)(x:NumericArray2<'Scalar,'Row,'Self>,y:complex0)=NumericArray2.ScalarRight(x,y,fun(t,a,b)->Mul(t,a,b))
        static member (*)(x:NumericArray2<'Scalar,'Row,'Self>,y:int)=NumericArray2.PrimitiveRight(x,It 4,Int y,fun(t,a,b)->Mul(t,a,b))
        static member (*)(x:NumericArray2<'Scalar,'Row,'Self>,y:double)=NumericArray2.PrimitiveRight(x,Dt,Dbl y,fun(t,a,b)->Mul(t,a,b))

        static member (/)(x:NumericArray2<'Scalar,'Row,'Self>,y:NumericArray2<'Scalar,'Row,'Self>)=NumericArray2.Binary(x,y,fun(t,a,b)->Div(t,a,b))
        static member (/)(x:int0,y:NumericArray2<'Scalar,'Row,'Self>)=NumericArray2.ScalarLeft(x,y,fun(t,a,b)->Div(t,a,b))
        static member (/)(x:double0,y:NumericArray2<'Scalar,'Row,'Self>)=NumericArray2.ScalarLeft(x,y,fun(t,a,b)->Div(t,a,b))
        static member (/)(x:complex0,y:NumericArray2<'Scalar,'Row,'Self>)=NumericArray2.ScalarLeft(x,y,fun(t,a,b)->Div(t,a,b))
        static member (/)(x:int,y:NumericArray2<'Scalar,'Row,'Self>)=NumericArray2.PrimitiveLeft(It 4,Int x,y,fun(t,a,b)->Div(t,a,b))
        static member (/)(x:double,y:NumericArray2<'Scalar,'Row,'Self>)=NumericArray2.PrimitiveLeft(Dt,Dbl x,y,fun(t,a,b)->Div(t,a,b))
        static member (/)(x:NumericArray2<'Scalar,'Row,'Self>,y:int0)=NumericArray2.ScalarRight(x,y,fun(t,a,b)->Div(t,a,b))
        static member (/)(x:NumericArray2<'Scalar,'Row,'Self>,y:double0)=NumericArray2.ScalarRight(x,y,fun(t,a,b)->Div(t,a,b))
        static member (/)(x:NumericArray2<'Scalar,'Row,'Self>,y:complex0)=NumericArray2.ScalarRight(x,y,fun(t,a,b)->Div(t,a,b))
        static member (/)(x:NumericArray2<'Scalar,'Row,'Self>,y:int)=NumericArray2.PrimitiveRight(x,It 4,Int y,fun(t,a,b)->Div(t,a,b))
        static member (/)(x:NumericArray2<'Scalar,'Row,'Self>,y:double)=NumericArray2.PrimitiveRight(x,Dt,Dbl y,fun(t,a,b)->Div(t,a,b))

        member this.AssignArray(other:NumericArray2<'Scalar,'Row,'Self>) =
            let ctx =
                match context with
                |Some left ->
                    match other.Context with
                    |Some right when not(obj.ReferenceEquals(left,right))->invalidOp "Values from different GenerationContext instances cannot be assigned."
                    |_ -> left
                |None -> invalidOp "The assignment target is not associated with a GenerationContext."
            let writein text=ctx.CurrentProgram.codewritein text
            base2.sizeMismatchError(this,other)
            let elementwise()=iter.num this.size1 <| fun i->iter.num this.size2 <| fun j->this.AssignAt(i,j,(other[i,j]:>INum0).Expr)
            match this.Expr,other.Expr with
            |Var2(_,left),Var2(_,right)->
                match ctx.CurrentProgram.language with
                |Fortran|LaTeX->writein(left+"="+right)
                |HTML|HTMLSequenceDiagram->writein(left+" \\leftarrow "+right)
                |Python->writein(left+" = copy.deepcopy("+right+")")
                |C99|JavaScript|PHP->elementwise()
                |Numeric->()
            |_->elementwise()

        member this.AssignScalar(value:INum0)=
            let ctx=context|>Option.defaultWith(fun()->invalidOp "The assignment target is not associated with a GenerationContext.")
            match value.Context with
            |Some right when not(obj.ReferenceEquals(ctx,right))->invalidOp "Values from different GenerationContext instances cannot be assigned."
            |_->()
            let writein text=ctx.CurrentProgram.codewritein text
            let elementwise()=iter.num this.size1 <| fun i->iter.num this.size2 <| fun j->this.AssignAt(i,j,value.Expr)
            match this.Expr with
            |Var2(_,name)->
                match ctx.CurrentProgram.language with
                |Fortran|LaTeX->writein(name+"="+value.Expr.eval ctx.CurrentProgram)
                |HTML|HTMLSequenceDiagram->writein(name+" \\leftarrow "+value.Expr.eval ctx.CurrentProgram)
                |Python->
                    match typ with
                    |Structure sname->writein(name+" = numpy.array([["+sname+"() for _ in range(int("+this.size2.Expr.eval ctx.CurrentProgram+"))] for _ in range(int("+this.size1.Expr.eval ctx.CurrentProgram+"))], dtype=object).reshape(int("+this.size1.Expr.eval ctx.CurrentProgram+"),int("+this.size2.Expr.eval ctx.CurrentProgram+"))\n")
                    |_->writein(name+"[:,:]="+value.Expr.eval ctx.CurrentProgram+"\n")
                |C99|JavaScript|PHP->elementwise()
                |Numeric->()
            |Arx2 _->elementwise()
