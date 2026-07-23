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
    type base2 (typ:Etype,x:Expr2, ?context:GenerationContext) =
        let requireContext() = GenerationContextMerge.requireTarget context
        let currentProgram() = (requireContext()).CurrentProgram
        let language() = currentProgram().language
        let writein text = currentProgram().codewritein text
        let comment text = currentProgram().comment text
        let environment() = Aqualis context
        let sizeValue value = int0(value, ?context=context)
        ///<summary>変数を作成しリストに追加</summary>
        new (context:GenerationContext,typ,size,name,para) =
            context.CurrentProgram.var.setVar(typ,size,name,para)
            base2(typ,Var2(size,name), context=context)
        ///<summary>変数を作成しリストに追加</summary>
        new(context:GenerationContext,sname,size,name) =
            context.CurrentProgram.var.setVar(Structure sname,size,name,"")
            base2(Structure sname,Var2(size,name), context=context)
        member internal _.GenerationContext = context
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
                match currentProgram().language with
                |Fortran ->
                    sizeValue(Var(It 4,name+"_size(1)",NaN))
                |C99 ->
                    sizeValue(Var(It 4,name+"_size[0]",NaN))
                |LaTeX ->
                    sizeValue(Var(It 4,"\\mathcal{S}_1["+name+"]",NaN))
                |HTML ->
                    sizeValue(Var(It 4,"\\mathcal{S}_1["+name+"]",NaN))
                |HTMLSequenceDiagram ->
                    sizeValue(Var(It 4,"\\mathcal{S}_1["+name+"]",NaN))
                |Python ->
                    sizeValue(Var(It 4,name+"_size[0]",NaN))
                |JavaScript ->
                    sizeValue(Var(It 4,name+"_size[0]",NaN))
                |PHP ->
                    sizeValue(Var(It 4,name+"_size[0]",NaN))
                |Numeric ->
                    sizeValue NaN
            |Arx2(s1,_,_) -> s1
        ///<summary>変数の要素数</summary>
        member _.size2
          with get() =
            match x with
            |Var2(_,name) ->
                match currentProgram().language with
                |Fortran ->
                    sizeValue(Var(It 4,name+"_size(2)",NaN))
                |C99 ->
                    sizeValue(Var(It 4,name+"_size[1]",NaN))
                |LaTeX ->
                    sizeValue(Var(It 4,"\\mathcal{S}_2["+name+"]",NaN))
                |HTML ->
                    sizeValue(Var(It 4,"\\mathcal{S}_2["+name+"]",NaN))
                |HTMLSequenceDiagram ->
                    sizeValue(Var(It 4,"\\mathcal{S}_2["+name+"]",NaN))
                |Python ->
                    sizeValue(Var(It 4,name+"_size[1]",NaN))
                |JavaScript ->
                    sizeValue(Var(It 4,name+"_size[1]",NaN))
                |PHP ->
                    sizeValue(Var(It 4,name+"_size[1]",NaN))
                |Numeric ->
                    sizeValue NaN
            |Arx2(_,s2,_) -> s2
        ///<summary>インデクサ</summary>
        member this.Idx2(i:int0,j:int0) =
            GenerationContextMerge.mergeMany [context;i.Context;j.Context] |> ignore
            if context |> Option.exists (fun value -> value.Debug.debugMode) then
                match x with
                |Var2(_,name) ->
                    (requireContext()).Errors.inc()
                    comment ("***debug array2 access check: "+(requireContext()).Errors.ID+"*****************************")
                    (environment()).br.branch <| fun b ->
                        b.IF (Or [this.size1 .= -1; this.size2 .= -1]) <| fun () ->
                            (environment()).print.s <| "ERROR" + (requireContext()).Errors.ID + " array " + name + " is not allocated"
                        b.IF (Or [i .< _0; this.size1 .<= i]) <| fun () ->
                            (environment()).print.tt <| "ERROR" + (requireContext()).Errors.ID + " array " + name + " illegal access. index " ++ i ++ " is out of range (1:" ++ this.size1 ++ ")"
                        b.IF (Or [j .< _0; this.size2 .<= j]) <| fun () ->
                            (environment()).print.tt <| "ERROR" + (requireContext()).Errors.ID + " array " + name + " illegal access. index " ++ j ++ " is out of range (1:" ++ this.size2 ++ ")"
                    comment "****************************************************"
                |_ -> ()
            let targetLanguage = context |> Option.map (fun value -> value.CurrentProgram.language) |> Option.defaultValue Numeric
            match x,targetLanguage with
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
                    if (requireContext()).Debug.debugMode then
                        (requireContext()).Errors.inc()
                        comment ("***debug array1 allocate check: "+(requireContext()).Errors.ID+"*****************************")
                        (environment()).br.branch <| fun b ->
                            b.IF (this.size1 .=/ -1) <| fun () ->
                                (environment()).print.s ("ERROR"+(requireContext()).Errors.ID+" array "+name+" is already allocated")
                        comment "****************************************************"
                    match currentProgram().language with
                    |Fortran ->
                        match size with
                        |A2(0,0) ->
                            this.size1 <== n1
                            this.size2 <== n2
                            writein("allocate("+name+"(1:"+this.size1.Expr.eval (currentProgram())+",1:"+this.size2.Expr.eval (currentProgram())+")"+")"+"\n")
                        |_ ->
                            writein("(Error:055-001 「"+name+"」は可変長2次元配列ではありません")
                    |C99 ->
                        match size with
                        |A2(0,0) ->
                            this.size1 <== n1
                            this.size2 <== n2
                            writein(name+" = "+"("+typ.tostring(language())+" *)"+"malloc("+"sizeof("+typ.tostring(language())+")*"+this.size1.Expr.eval (currentProgram())+"*"+this.size2.Expr.eval (currentProgram())+");\n")
                        |_ ->
                            writein("(Error:055-001 「"+name+"」は可変長2次元配列ではありません")
                    |LaTeX ->
                        match size,typ with
                        |A2(0,0),It _ ->
                            writein("$"+name+" \\in \\mathbb{Z}^{"+n1.Expr.eval (currentProgram())+"\\times"+n2.Expr.eval (currentProgram())+"}$\\\\\n")
                        |A2(0,0),Dt   ->
                            writein("$"+name+" \\in \\mathbb{R}^{"+n1.Expr.eval (currentProgram())+"\\times"+n2.Expr.eval (currentProgram())+"}$\\\\\n")
                        |A2(0,0),Zt   ->
                            writein("$"+name+" \\in \\mathbb{C}^{"+n1.Expr.eval (currentProgram())+"\\times"+n2.Expr.eval (currentProgram())+"}$\\\\\n")
                        |_ ->
                            writein("(Error:055-001 「"+name+"」は可変長2次元配列ではありません")
                    |HTML ->
                        match size,typ with
                        |A2(0,0),It _ ->
                            writein("\\("+name+" \\in \\mathbb{Z}^{"+n1.Expr.eval (currentProgram())+"\\times"+n2.Expr.eval (currentProgram())+"}\\)<br>\n")
                        |A2(0,0),Dt   ->
                            writein("\\("+name+" \\in \\mathbb{R}^{"+n1.Expr.eval (currentProgram())+"\\times"+n2.Expr.eval (currentProgram())+"}\\)<br>\n")
                        |A2(0,0),Zt   ->
                            writein("\\("+name+" \\in \\mathbb{C}^{"+n1.Expr.eval (currentProgram())+"\\times"+n2.Expr.eval (currentProgram())+"}\\)<br>\n")
                        |_ ->
                            writein("(Error:055-001 「"+name+"」は可変長2次元配列ではありません")
                    |HTMLSequenceDiagram ->
                        match size,typ with
                        |A2(0,0),It _ ->
                            writein("\\("+name+" \\in \\mathbb{Z}^{"+n1.Expr.eval (currentProgram())+"\\times"+n2.Expr.eval (currentProgram())+"}\\)<br>\n")
                        |A2(0,0),Dt   ->
                            writein("\\("+name+" \\in \\mathbb{R}^{"+n1.Expr.eval (currentProgram())+"\\times"+n2.Expr.eval (currentProgram())+"}\\)<br>\n")
                        |A2(0,0),Zt   ->
                            writein("\\("+name+" \\in \\mathbb{C}^{"+n1.Expr.eval (currentProgram())+"\\times"+n2.Expr.eval (currentProgram())+"}\\)<br>\n")
                        |_ ->
                            writein("(Error:055-001 「"+name+"」は可変長2次元配列ではありません")
                    |Python ->
                        match size with
                        |A2(0,0) ->
                            this.size1 <== n1
                            this.size2 <== n2
                            match typ with
                            |Structure sname -> writein(name+" = numpy.array([["+sname+"() for _ in range(int("+this.size2.Expr.eval (currentProgram())+"))] for _ in range(int("+this.size1.Expr.eval (currentProgram())+"))], dtype=object).reshape(int("+this.size1.Expr.eval (currentProgram())+"),int("+this.size2.Expr.eval (currentProgram())+"))\n")
                            |It _ |It 1      -> writein(name+" = numpy.zeros("+this.size1.Expr.eval (currentProgram())+"*"+this.size2.Expr.eval (currentProgram())+", dtype=int).reshape(int("+this.size1.Expr.eval (currentProgram())+"),int("+this.size2.Expr.eval (currentProgram())+"))"+"\n")
                            |Zt              -> writein(name+" = numpy.zeros("+this.size1.Expr.eval (currentProgram())+"*"+this.size2.Expr.eval (currentProgram())+", dtype=numpy.complex128).reshape(int("+this.size1.Expr.eval (currentProgram())+"),int("+this.size2.Expr.eval (currentProgram())+"))"+"\n")
                            |_               -> writein(name+" = numpy.zeros("+this.size1.Expr.eval (currentProgram())+"*"+this.size2.Expr.eval (currentProgram())+").reshape(int("+this.size1.Expr.eval (currentProgram())+"),int("+this.size2.Expr.eval (currentProgram())+"))"+"\n")
                        |_ ->
                            writein("(Error:055-001 「"+name+"」は可変長2次元配列ではありません")
                    |JavaScript ->
                        match size with
                        |A2(0,0) ->
                            this.size1 <== n1
                            this.size2 <== n2
                            writein(name+" = "+"Array("+this.size1.Expr.eval (currentProgram())+"*"+this.size2.Expr.eval (currentProgram())+");\n")
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
            if (requireContext()).Debug.debugMode then
                match x with
                |Var2(_,name) ->
                    (requireContext()).Errors.inc()
                    comment ("***debug array1 deallocate check: "+(requireContext()).Errors.ID+"*****************************")
                    (environment()).br.branch <| fun b ->
                        b.IF (this.size1 .= -1) <| fun () ->
                            (environment()).print.s ("ERROR"+(requireContext()).Errors.ID+" cannot deallocate array "+name)
                    comment ("****************************************************")
                |_ -> ()
            match x with
            |Var2(size,name) ->
                match currentProgram().language with
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
            (environment()).iter.num this.size1 <| fun i ->
                (environment()).iter.num this.size2 <| fun j ->
                    code(i,j)

        ///<summary>配列の全要素に対する処理</summary>
        member this.Foreach (counterName1:string,counterName2:string) code =
            (environment()).iter.num (this.size1,counterName1) <| fun i ->
                (environment()).iter.num (this.size2,counterName2) <| fun j ->
                    code(i,j)

        ///<summary>配列の全要素に対する処理</summary>
        member this.foreach_exit code =
            (environment()).iter.num_exit this.size1 <| fun (ext1,i) ->
                (environment()).iter.num_exit this.size2 <| fun (ext2,j) ->
                    code(ext1,ext2,i,j)

        ///<summary>配列の全要素に対する処理</summary>
        member this.Foreach_exit (counterName1:string,counterName2:string) code =
            (environment()).iter.num_exit (this.size1,counterName1) <| fun (ext1,i) ->
                (environment()).iter.num_exit (this.size2,counterName1) <| fun (ext2,j) ->
                    code(ext1,ext2,i,j)

        static member sizeMismatchError(x:base2,y:base2) =
            let context = GenerationContextMerge.merge x.GenerationContext y.GenerationContext
            context |> Option.iter (fun ctx ->
                let environment = Aqualis context
                if ctx.Debug.debugMode then
                    ctx.Errors.inc()
                    ctx.CurrentProgram.comment ("***debug array2 access check: "+ctx.Errors.ID+"*****************************")
                    environment.br.branch <| fun b ->
                        b.IF (x.size1 .=/ y.size1) <| fun () ->
                            environment.print.s ("ERROR"+ctx.Errors.ID+" array size (first index) mismatch")
                    environment.br.branch <| fun b ->
                        b.IF (x.size2 .=/ y.size2) <| fun () ->
                            environment.print.s ("ERROR"+ctx.Errors.ID+" array size (second index) mismatch")
                    ctx.CurrentProgram.comment "****************************************************")

    /// Shared implementation for two-dimensional numeric arrays.
    [<AbstractClass>]
    type NumericArray2<'Scalar,'Row,'Self
        when 'Scalar :> INum0
        and 'Self :> NumericArray2<'Scalar,'Row,'Self>>
        (typ:Etype,x:Expr2,?context:GenerationContext) =
        inherit base2(typ,x,?context=context)

        let context =
            match context,x with
            |Some value,_ -> Some value
            |None,Arx2(size1,size2,_) -> GenerationContextMerge.merge size1.Context size2.Context
            |None,Var2 _ -> None
        let environment() = Aqualis context

        member _.Context = context
        member _.etype = typ
        abstract member WrapScalar: expr -> 'Scalar
        abstract member WrapRow: Expr1 -> 'Row
        abstract member CreateWithContext: Etype * Expr2 * GenerationContext option -> 'Self
        abstract member AssignAt: int0 * int0 * expr -> unit
        member this.Create(elementType,value) = this.CreateWithContext(elementType,value,context)

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

        member private this.New(elementType,body,resultContext) =
            this.CreateWithContext(elementType,Arx2(this.size1,this.size2,body),resultContext)
        static member private Binary(x:NumericArray2<'Scalar,'Row,'Self>,y:NumericArray2<'Scalar,'Row,'Self>,make:Etype*expr*expr->expr) =
            base2.sizeMismatchError(x,y)
            let resultContext = GenerationContextMerge.merge x.Context y.Context
            x.New(x.etype%%y.etype,(fun (i:int0,j:int0)->make(x.etype%%y.etype,(x[i,j]:>INum0).Expr,(y[i,j]:>INum0).Expr)),resultContext)
        static member private ScalarLeft(value:INum0,y:NumericArray2<'Scalar,'Row,'Self>,make:Etype*expr*expr->expr) =
            let resultContext = GenerationContextMerge.merge value.Context y.Context
            y.New(value.Etype%%y.etype,(fun (i:int0,j:int0)->make(value.Etype%%y.etype,value.Expr,(y[i,j]:>INum0).Expr)),resultContext)
        static member private ScalarRight(x:NumericArray2<'Scalar,'Row,'Self>,value:INum0,make:Etype*expr*expr->expr) =
            let resultContext = GenerationContextMerge.merge x.Context value.Context
            x.New(x.etype%%value.Etype,(fun (i:int0,j:int0)->make(x.etype%%value.Etype,(x[i,j]:>INum0).Expr,value.Expr)),resultContext)
        static member private PrimitiveLeft(elementType,value,y:NumericArray2<'Scalar,'Row,'Self>,make:Etype*expr*expr->expr) =
            y.New(elementType%%y.etype,(fun (i:int0,j:int0)->make(elementType%%y.etype,value,(y[i,j]:>INum0).Expr)),y.Context)
        static member private PrimitiveRight(x:NumericArray2<'Scalar,'Row,'Self>,elementType,value,make:Etype*expr*expr->expr) =
            x.New(x.etype%%elementType,(fun (i:int0,j:int0)->make(x.etype%%elementType,(x[i,j]:>INum0).Expr,value)),x.Context)

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
            let ctx = GenerationContextMerge.requireTarget context
            GenerationContextMerge.merge context other.Context |> ignore
            let writein text=ctx.CurrentProgram.codewritein text
            base2.sizeMismatchError(this,other)
            let elementwise()=(environment()).iter.num this.size1 <| fun i->(environment()).iter.num this.size2 <| fun j->this.AssignAt(i,j,(other[i,j]:>INum0).Expr)
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
            let ctx = GenerationContextMerge.requireTarget context
            GenerationContextMerge.merge context value.Context |> ignore
            let writein text=ctx.CurrentProgram.codewritein text
            let elementwise()=(environment()).iter.num this.size1 <| fun i->(environment()).iter.num this.size2 <| fun j->this.AssignAt(i,j,value.Expr)
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
