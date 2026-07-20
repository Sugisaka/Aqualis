//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    ///<summary>1次元配列変数</summary>
    type Expr1 =
        ///<summary>変数</summary>
        |Var1 of (VarType*string)
        ///<summary>部分配列</summary>
        |Arx1 of (int0*(int0->expr))

    ///<summary>1次元配列</summary>
    type base1 (typ:Etype,x:Expr1) =
        ///<summary>変数を作成しリストに追加</summary>
        new (typ,size,name,para) =
            (GenerationScope.currentProgram()).var.setVar(typ,size,name,para)
            base1(typ,Var1(size,name))
        ///<summary>変数を作成しリストに追加</summary>
        new(sname,size,name) =
            (GenerationScope.currentProgram()).var.setVar(Structure sname,size,name,"")
            base1(Structure sname,Var1(size,name))
        member _.Etype with get() = typ
        member _.Expr with get() = x
        member _.code with get() =
            match x with
            |Var1(_,x) -> x
            |_ -> "Error(property .code)"
        ///<summary>変数の要素数</summary>
        member _.size1
          with get() =
            match x with
            |Var1(_,name) ->
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
            |Arx1(s,_) -> s
        ///<summary>インデクサ</summary>
        member this.Idx1(i:int0) =
            if (GenerationScope.debug()).debugMode then
                match x with
                |Var1(_,name) ->
                    (GenerationScope.errors()).inc()
                    !("***debug array1 access check: "+(GenerationScope.errors()).ID+"*****************************")
                    br.branch <| fun b ->
                        b.IF (this.size1 .= -1) <| fun () ->
                            print.t <| "ERROR" + (GenerationScope.errors()).ID + " array " + name + " is not allocated"
                        b.IF (Or [i .< _0; this.size1 .<= i]) <| fun () ->
                            print.cc <| "ERROR" + (GenerationScope.errors()).ID + " array " + name + " illegal access. index " ++ i ++ " is out of range (1:" ++ this.size1 ++ ")"
                    ! "****************************************************"
                |_ -> ()
            match x,language() with
            |Var1(_,name),Fortran -> Idx1(typ,name,(i+1).Expr)
            |Var1(_,name),_       -> Idx1(typ,name,i.Expr)
            |Arx1(_,f),_ -> f i
        member this.Idx1(i:int) = this.Idx1(I i)
        member this.Idx1(a:int0,b:int0) = Arx1(b-a+_1,fun i -> this.Idx1(i+a))
        member this.Idx1(a:int0,b:int)  = Arx1(b-a+_1,fun i -> this.Idx1(i+a))
        member this.Idx1(a:int ,b:int0) = Arx1(b-a+_1,fun i -> this.Idx1(i+a))
        member this.Idx1(a:int ,b:int)  = Arx1(b-a+_1,fun i -> this.Idx1(i+a))

        ///<summary>配列のメモリ割り当て</summary>
        member this.allocate(n1:int0) =
                match x with
                |Var1(size1,name) ->
                    if (GenerationScope.debug()).debugMode then
                        (GenerationScope.errors()).inc()
                        !("***debug array1 allocate check: "+(GenerationScope.errors()).ID+"*****************************")
                        br.branch <| fun b ->
                            b.IF (this.size1 .=/ -1) <| fun () ->
                                print.t ("ERROR"+(GenerationScope.errors()).ID+" array "+name+" is already allocated")
                        ! "****************************************************"
                    match (GenerationScope.currentProgram()).language with
                    |Fortran ->
                        match size1 with
                        |A1 0 ->
                            this.size1 <== n1
                            writein("allocate("+name+"(1:"+this.size1.Expr.eval ((GenerationScope.currentProgram()))+")"+")"+"\n")
                        |_ ->
                            writein("(Error:055-001 「"+name+"」は可変長1次元配列ではありません")
                    |C99 ->
                        match size1 with
                        |A1 0 ->
                            this.size1 <== n1
                            writein(name+" = "+"("+typ.tostring (GenerationScope.currentProgram()).language+" *)"+"malloc("+"sizeof("+typ.tostring (GenerationScope.currentProgram()).language+")*"+this.size1.Expr.eval ((GenerationScope.currentProgram()))+");\n")
                        |_ ->
                            writein("(Error:055-001 「"+name+"」は可変長1次元配列ではありません")
                    |LaTeX ->
                        match size1,typ with
                        |A1 0,It _ ->
                            writein("$"+name+" \\in \\mathbb{Z}^{"+n1.Expr.eval ((GenerationScope.currentProgram()))+"}$\\\\\n")
                        |A1 0,Dt   ->
                            writein("$"+name+" \\in \\mathbb{R}^{"+n1.Expr.eval ((GenerationScope.currentProgram()))+"}$\\\\\n")
                        |A1 0,Zt   ->
                            writein("$"+name+" \\in \\mathbb{C}^{"+n1.Expr.eval ((GenerationScope.currentProgram()))+"}$\\\\\n")
                        |_ ->
                            writein("(Error:055-001 「"+name+"」は可変長1次元配列ではありません")
                    |HTML ->
                        match size1,typ with
                        |A1 0,It _ ->
                            writein("\\("+name+" \\in \\mathbb{Z}^{"+n1.Expr.eval ((GenerationScope.currentProgram()))+"}\\)<br>\n")
                        |A1 0,Dt   ->
                            writein("\\("+name+" \\in \\mathbb{R}^{"+n1.Expr.eval ((GenerationScope.currentProgram()))+"}\\)<br>\n")
                        |A1 0,Zt   ->
                            writein("\\("+name+" \\in \\mathbb{C}^{"+n1.Expr.eval ((GenerationScope.currentProgram()))+"}\\)<br>\n")
                        |_ ->
                            writein("(Error:055-001 「"+name+"」は可変長1次元配列ではありません")
                    |HTMLSequenceDiagram ->
                        match size1,typ with
                        |A1 0,It _ ->
                            writein("\\("+name+" \\in \\mathbb{Z}^{"+n1.Expr.eval ((GenerationScope.currentProgram()))+"}\\)<br>\n")
                        |A1 0,Dt   ->
                            writein("\\("+name+" \\in \\mathbb{R}^{"+n1.Expr.eval ((GenerationScope.currentProgram()))+"}\\)<br>\n")
                        |A1 0,Zt   ->
                            writein("\\("+name+" \\in \\mathbb{C}^{"+n1.Expr.eval ((GenerationScope.currentProgram()))+"}\\)<br>\n")
                        |_ ->
                            writein("(Error:055-001 「"+name+"」は可変長1次元配列ではありません")
                    |Python ->
                        match size1 with
                        |A1 0 ->
                            this.size1 <== n1
                            match typ with
                            |Structure sname -> writein(name+" = numpy.array(["+sname+"() for _ in range(int("+this.size1.Expr.eval ((GenerationScope.currentProgram()))+"))], dtype=object)\n")
                            |It _ |It 1      -> writein(name+" = numpy.zeros("+this.size1.Expr.eval ((GenerationScope.currentProgram()))+", dtype=int)\n")
                            |Zt              -> writein(name+" = numpy.zeros("+this.size1.Expr.eval ((GenerationScope.currentProgram()))+", dtype=numpy.complex128)\n")
                            |_               -> writein(name+" = "+"numpy.zeros("+this.size1.Expr.eval ((GenerationScope.currentProgram()))+")\n")
                        |_ ->
                            writein("(Error:055-001 「"+name+"」は可変長1次元配列ではありません")
                    |JavaScript ->
                        match size1 with
                        |A1 0 ->
                            this.size1 <== n1
                            writein(name+" = Array(" + this.size1.Expr.eval ((GenerationScope.currentProgram())) + ");\n")
                        |_ ->
                            writein("(Error:055-001 「"+name+"」は可変長1次元配列ではありません")
                    |PHP ->
                        match size1 with
                        |A1 0 ->
                            this.size1 <== n1
                            writein(name+" = [];\n")
                        |_ ->
                            writein("(Error:055-001 「"+name+"」は可変長1次元配列ではありません")
                    |Numeric ->
                        ()
                |_ -> ()
        member this.allocate(n1:int) = this.allocate(I n1)

        ///<summary>配列のメモリ割り当て</summary>
        member this.deallocate() =
            if (GenerationScope.debug()).debugMode then
                match x with
                |Var1(_,name) ->
                    (GenerationScope.errors()).inc()
                    !("***debug array1 deallocate check: "+(GenerationScope.errors()).ID+"*****************************")
                    br.branch <| fun b ->
                        b.IF (this.size1 .= -1) <| fun () ->
                            print.t ("ERROR"+(GenerationScope.errors()).ID+" cannot deallocate array "+name)
                    ! "****************************************************"
                |_ -> ()
            match x with
            |Var1(size,name) ->
                match (GenerationScope.currentProgram()).language with
                |Fortran ->
                    match size with
                    |A1 0 ->
                        this.size1 <== -1
                        writein("deallocate("+name+")"+"\n")
                    |_ -> ()
                |C99 ->
                    match size with
                    |A1 0 ->
                        this.size1 <== -1
                        writein("free("+name+");"+"\n")
                    |_ -> ()
                |LaTeX ->
                    match size with
                    |A1 0 ->
                        writein("$"+name+"$: deallocate\\\\\n")
                    |_ -> ()
                |HTML ->
                    match size with
                    |A1 0 ->
                        writein("\\("+name+"\\): deallocate<br/>\n")
                    |_ -> ()
                |HTMLSequenceDiagram ->
                    match size with
                    |A1 0 ->
                        writein("\\("+name+"\\): deallocate<br/>\n")
                    |_ -> ()
                |Python ->
                    match size with
                    |A1 0 ->
                        this.size1 <== -1
                        writein("del "+name+""+"\n")
                    |_ -> ()
                |JavaScript ->
                    match size with
                    |A1 0 ->
                        this.size1 <== -1
                        writein(name+"= null;"+"\n")
                    |_ -> ()
                |PHP ->
                    match size with
                    |A1 0 ->
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
                code i

        ///<summary>配列の全要素に対する処理</summary>
        member this.Foreach (counterName:string) code =
            iter.num (this.size1,counterName) <| fun i ->
                code i

        ///<summary>配列の全要素に対する処理</summary>
        member this.foreach_exit code =
            iter.num_exit this.size1 <| fun (ext,i) ->
                code(ext,i)

        ///<summary>配列の全要素に対する処理</summary>
        member this.Foreach_exit (counterName:string) code =
            iter.num_exit (this.size1,counterName) <| fun (ext,i) ->
                code(ext,i)

        static member sizeMismatchError(x:base1,y:base1) =
            if (GenerationScope.debug()).debugMode then
                (GenerationScope.errors()).inc()
                !("***debug array1 access check: "+(GenerationScope.errors()).ID+"*****************************")
                br.branch <| fun b ->
                    b.IF (x.size1 .=/ y.size1) <| fun () ->
                        print.t ("ERROR"+(GenerationScope.errors()).ID+" array size (first index) mismatch")
                ! "****************************************************"
