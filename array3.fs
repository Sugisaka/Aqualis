(*
Copyright (c) 2022 Jun-ichiro Sugisaka

This software is released under the MIT License.
http://opensource.org/licenses/mit-license.php
*)
namespace Aqualis
    
    open Aqualis_base
    
    ///<summary>2次元配列変数</summary>
    type Expr3 =
        ///<summary>変数</summary>
        |Var3 of (VarType*string)
        ///<summary>部分配列</summary>
        |Arx3 of (int0*int0*int0*((int0*int0*int0)->Expr))
        
    ///<summary>2次元配列</summary>
    type base3 (x:Expr3) =
        ///<summary>変数を作成しリストに追加</summary>
        new (typ,size,name,para) = 
            p.param.var.setVar(typ,size,name,para)
            base3(Var3(size,name))
        ///<summary>変数を作成しリストに追加</summary>
        new(sname,size,name) =
            p.param.var.setVar(Structure(sname),size,name,"")
            base3(Var3(size,name))
        member _.expr with get() = x
        member _.code with get() =
            match x with
            |Var3(_,x) -> x
            |_ -> "Error(property .code)"
        ///<summary>変数の要素数</summary>
        member __.size1 
          with get() =
            let p = p.param
            match x with
            |Var3(_,name) -> 
                match p.lang with 
                |F |T -> int0(Var(name+"_size(1)"))
                |C -> int0(Var(name+"_size[0]"))
                |H -> int0(Var("<msub><mi mathvariant=\"script\">S</mi><mn>1</mn></msub><mo>&af;</mo><mo>[</mo><mi>"+name+"</mi><mo>]</mo>"))
            |Arx3(s1,_,_,_) -> s1
        ///<summary>変数の要素数</summary>
        member __.size2 
          with get() =
            let p = p.param
            match x with
            |Var3(_,name) -> 
                match p.lang with 
                |F |T -> int0(Var(name+"_size(2)"))
                |C -> int0(Var(name+"_size[1]"))
                |H -> int0(Var("<msub><mi mathvariant=\"script\">S</mi><mn>2</mn></msub><mo>&af;</mo><mo>[</mo><mi>"+name+"</mi><mo>]</mo>"))
            |Arx3(_,s2,_,_) -> s2
        ///<summary>変数の要素数</summary>
        member __.size3 
          with get() =
            let p = p.param
            match x with
            |Var3(_,name) -> 
                match p.lang with 
                |F |T -> int0(Var(name+"_size(3)"))
                |C -> int0(Var(name+"_size[2]"))
                |H -> int0(Var("<msub><mi mathvariant=\"script\">S</mi><mn>3</mn></msub><mo>&af;</mo><mo>[</mo><mi>"+name+"</mi><mo>]</mo>"))
            |Arx3(_,_,s3,_) -> s3
        ///<summary>インデクサ</summary>
        member this.Idx3(i:int0,j:int0,k:int0) =
            let p = p.param
            if p.debugmode then
                match x with
                |Var3(_,name) ->
                    p.error_code_counter_inc()
                    p.comment("***debug array1 access check: "+p.error_code_counter.ToString()+"*****************************")
                    br.if1 (OR [this.size1 =. -1; this.size2 =. -1; this.size3 =. -1]) <| fun () -> 
                        print.c ("ERROR"+p.error_code_counter.ToString()+" array "+name+" is not allocated")
                        print.br
                    br.if1 (OR [(i <. _1); (this.size1 <. i)]) <| fun () ->
                        print.c ("ERROR"+p.error_code_counter.ToString()+" array "+name+" illegal access. index ")
                        print.c i
                        print.c (" is out of range (1:")
                        print.c this.size1
                        print.c (")")
                        print.br
                    br.if1 (OR [(j <. _1); (this.size2 <. j)]) <| fun () ->
                        print.c ("ERROR"+p.error_code_counter.ToString()+" array "+name+" illegal access. index ")
                        print.c j
                        print.c (" is out of range (1:")
                        print.c this.size3
                        print.c (")")
                        print.br
                    br.if1 (OR [(k <. _1); (this.size3 <. k)]) <| fun () ->
                        print.c ("ERROR"+p.error_code_counter.ToString()+" array "+name+" illegal access. index ")
                        print.c k
                        print.c (" is out of range (1:")
                        print.c this.size3
                        print.c (")")
                        print.br
                    p.comment("****************************************************")
                |_ -> ()
            match x with
            |Var3(_,name) ->
                match p.lang with
                |F   -> Var(name+"("+i.code+","+j.code+","+k.code+")")
                |C -> Var(name+"["+((k-1)*this.size1*this.size2+(j-1)*this.size1+(i-1)).code+"]")
                |T   -> Var(name+"("+i.code+","+j.code+","+k.code+")")
                |H   -> Var(name+"<mo>&af;</mo><mo>[</mo>"+i.code+"<mo>]</mo>")
            |Arx3(_,_,_,f) ->
                f (i,j,k)
                
        member this.Idx3(i:int0,j:int0,k:int) = this.Idx3(i,j,k.I)
        member this.Idx3(i:int0,j:int,k:int0) = this.Idx3(i,j.I,k)
        member this.Idx3(i:int,j:int0,k:int0) = this.Idx3(i.I,j,k)
        member this.Idx3(i:int,j:int,k:int0) = this.Idx3(i,j.I,k)
        member this.Idx3(i:int,j:int0,k:int) = this.Idx3(i,j,k.I)
        member this.Idx3(i:int0,j:int,k:int) = this.Idx3(i,j.I,k.I)
        member this.Idx3(i:int,j:int,k:int) = this.Idx3(i.I,j.I,k.I)
        member this.Idx3((a1:int0,b1:int0),(a2:int0,b2:int0),(a3:int0,b3:int0)) = (Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1-1,j+a2-1,k+a3-1)))
        member this.Idx3((a1:int0,b1:int0),(a2:int0,b2:int0),_:unit) = Arx3(b1-a1+_1, b2-a2+_1, this.size3,  fun (i,j,k) -> this.Idx3(i+a1-1,j+a2-1,k))
        member this.Idx3((a1:int0,b1:int0),_:unit,(a3:int0,b3:int0)) = Arx3(b1-a1+_1, this.size2, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1-1,j,k+a3-1))
        member this.Idx3(_:unit,(a2:int0,b2:int0),(a3:int0,b3:int0)) = Arx3(this.size1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i,j+a2-1,k+a3-1))
        member this.Idx3((a1:int0,b1:int0),(a2:int0,b2:int0),k:int0) = Arx2(b1-a1+_1, b2-a2+_1, fun (i,j) -> this.Idx3(i+a1-1,j+a2-1,k))
        member this.Idx3((a1:int0,b1:int0),j:int0,(a3:int0,b3:int0)) = Arx2(b1-a1+_1, b3-a3+_1, fun (i,k) -> this.Idx3(i+a1-1,j,k+a3-1))
        member this.Idx3(i:int0,(a2:int0,b2:int0),(a3:int0,b3:int0)) = Arx2(b2-a2+_1,b3-a3+_1, fun (j,k) -> this.Idx3(i,j+a2-1,k+a3-1))
        member this.Idx3((a1:int0,b1:int0),j:int0,k:int0) = Arx1(b1-a1+_1, fun i -> this.Idx3(i+a1-1,j,k))
        member this.Idx3(i:int0,(a2:int0,b2:int0),k:int0) = Arx1(b2-a2+_1, fun j -> this.Idx3(i,j+a2-1,k))
        member this.Idx3(i:int0,j:int0,(a3:int0,b3:int0)) = Arx1(b3-a3+_1, fun k -> this.Idx3(i,j,k+a3-1))

        ///<summary>配列のメモリ割り当て</summary>
        member this.allocate(typ:Etype,n1:int0,n2:int0,n3:int0) =
                let p = p.param
                match x with
                |Var3(size,name) ->
                    if p.debugmode then
                        p.error_code_counter_inc()
                        p.comment("***debug array1 allocate check: "+p.error_code_counter.ToString()+"*****************************")
                        br.branch <| fun b ->
                            b.IF (this.size1 =/. -1) <| fun () ->
                                print.c ("ERROR"+p.error_code_counter.ToString()+" array "+name+" is already allocated")
                                print.br
                        p.comment("****************************************************")
                    match p.lang with
                    |F ->
                        match size with
                        |A1(0) ->
                            this.size1 <== n1
                            this.size2 <== n2
                            this.size3 <== n3
                            p.codewrite("allocate("+name+"(1:"+this.size1.code+",1:"+this.size2.code+",1:"+this.size3.code+")"+")"+"\n")
                        |_ -> 
                            p.codewrite("(Error:055-001 「"+name+"」は可変長1次元配列ではありません")
                    |C ->
                        match size with
                        |A1(0) ->
                            this.size1 <== n1
                            this.size3 <== n2
                            this.size3 <== n3
                            p.codewrite(name+"="+"("+typ.tostring(p.lang)+" *)"+"malloc("+"sizeof("+typ.tostring(p.lang)+")*"+this.size1.code+");\n")
                        |_ -> 
                            p.codewrite("(Error:055-001 「"+name+"」は可変長1次元配列ではありません")
                    |T ->
                        match size with
                        |A1(0) ->
                            this.size1 <== n1
                            this.size2 <== n2
                            this.size3 <== n3
                            p.codewrite("allocate($"+name+"(1:"+this.size1.code+",1:"+this.size2.code+",1:"+this.size3.code+")"+"$)"+"\n")
                        |_ -> 
                            p.codewrite("(Error:055-001 「"+name+"」は可変長1次元配列ではありません")
                    |H ->
                        match size with
                        |A1(0) ->
                            p.codewrite("<math>"+name+"<mo>:</mo><mi>allocate</mi><mo>(</mo>"+n1.code+"<mo>)</mo></math>"+"\n<br/>\n")
                        |_ -> 
                            p.codewrite("(Error:055-001 「"+name+"」は可変長1次元配列ではありません")
                |_ -> ()
                
        ///<summary>配列のメモリ割り当て</summary>
        member this.deallocate() =
            let p = p.param
            if p.debugmode then
                match x with
                |Var3(_,name) ->
                    p.error_code_counter_inc()
                    p.comment("***debug array1 deallocate check: "+p.error_code_counter.ToString()+"*****************************")
                    br.branch <| fun b ->
                        b.IF (this.size1 =. -1) <| fun () ->
                            print.c ("ERROR"+p.error_code_counter.ToString()+" cannot deallocate array "+name)
                            print.br
                    p.comment("****************************************************")
                |_ -> ()
            match x with
            |Var3(size,name) ->
                match p.lang with
                |F ->
                    match size with
                    |A1(0) ->
                        this.size1 <== -1
                        this.size2 <== -1
                        this.size3 <== -1
                        p.codewrite("deallocate("+name+")"+"\n")
                    |_ -> ()
                |C ->
                    match size with
                    |A1(0) ->
                        this.size1 <== -1
                        this.size2 <== -1
                        this.size3 <== -1
                        p.codewrite("free("+name+");"+"\n")
                    |_ -> ()
                |T ->
                    match size with
                    |A1(0) ->
                        this.size1 <== -1
                        this.size2 <== -1
                        this.size3 <== -1
                        p.codewrite("deallocate($"+name+"$)"+"\n")
                    |_ -> ()
                |H ->
                    match size with
                    |A1(0) ->
                        p.codewrite("<math>"+name+"<mo>:</mo><mi>deallocate</mi></math>"+"\n<br/>\n")
                    |_ -> ()
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
            let p = p.param
            if p.debugmode then
                p.error_code_counter_inc()
                p.comment("***debug array1 access check: "+p.error_code_counter.ToString()+"*****************************")
                br.if1 (v1.size1 =/. v2.size1) <| fun () -> 
                    print.c ("ERROR"+p.error_code_counter.ToString()+" operator '<==' array size1 mismatch")
                    print.br
                br.if1 (v1.size2 =/. v2.size2) <| fun () -> 
                    print.c ("ERROR"+p.error_code_counter.ToString()+" operator '<==' array size2 mismatch")
                    print.br
                br.if1 (v1.size3 =/. v2.size3) <| fun () -> 
                    print.c ("ERROR"+p.error_code_counter.ToString()+" operator '<==' array size3 mismatch")
                    print.br
                p.comment("****************************************************")
                
        static member subst (v1:Expr3,s11:int0,s12:int0,s13:int0,f1:int0->int0->int0->Expr,v2:Expr3,s21:int0,s22:int0,s23:int0,f2:int0->int0->int0->Expr) =
            let p = p.param
            if p.debugmode then
                p.error_code_counter_inc()
                p.comment("***debug array1 access check: "+p.error_code_counter.ToString()+"*****************************")
                br.if1 (s11 =/. s21) <| fun () -> 
                    print.c ("ERROR"+p.error_code_counter.ToString()+" operator '<==' array size1 mismatch")
                    print.br
                br.if1 (s12 =/. s22) <| fun () -> 
                    print.c ("ERROR"+p.error_code_counter.ToString()+" operator '<==' array size2 mismatch")
                    print.br
                br.if1 (s13 =/. s23) <| fun () -> 
                    print.c ("ERROR"+p.error_code_counter.ToString()+" operator '<==' array size3 mismatch")
                    print.br
                p.comment("****************************************************")
            match v1,v2 with
            |Var3(_,x),Var3(_,y) ->
                match p.lang with
                |F|T -> p.codewrite(x + "=" + y)
                |C -> iter.num s11 <| fun i -> iter.num s12 <| fun j -> iter.num s13 <| fun k -> (f1 i j k) <== (f2 i j k)
                |H   -> p.codewrite("<math>" + x + "<mo>&larr;</mo>" + y + "</math>\n<br/>\n")
            |Var3(_,x),Arx3(_,_,_,f) ->
                match p.lang with
                |F|T|C|H -> iter.num s11 <| fun i -> iter.num s12 <| fun j -> iter.num s13 <| fun k -> (f1 i j k) <== (f2 i j k)
            |Arx3(_,_,_,_),Var3(_,_) ->
                match p.lang with
                |F|T|C|H -> iter.num s11 <| fun i -> iter.num s12 <| fun j -> iter.num s13 <| fun k -> (f1 i j k) <== (f2 i j k)
            |Arx3(_,_,_,_),Arx3(_,_,_,_) ->
                match p.lang with
                |F|T|C|H -> iter.num s11 <| fun i -> iter.num s12 <| fun j -> iter.num s13 <| fun k -> (f1 i j k) <== (f2 i j k)
                
        static member subst (v1:Expr3,s11:int0,s12:int0,s13:int0,f1:int0->int0->int0->Expr,v2:Expr) =
            let p = p.param
            match v1 with
            |Var3(_,x) ->
                match p.lang with
                |F|T -> p.codewrite(x + "=" + v2.code)
                |C -> iter.num s11 <| fun i -> iter.num s12 <| fun j -> iter.num s13 <| fun k -> (f1 i j k) <== v2
                |H   -> p.codewrite("<math>" + x + "<mo>&larr;</mo>" + v2.code + "</math>\n<br/>\n")
            |Arx3(_,_,_,_) ->
                match p.lang with
                |F|T|C|H -> iter.num s11 <| fun i -> iter.num s12 <| fun j -> iter.num s13 <| fun k -> (f1 i j k) <== v2
                
    ///<summary>数値型1次元配列</summary>
    type int3 (x:Expr3) =
        inherit base3(x)
        new (size,name,para) =
            p.param.var.setVar(It 4,size,name,para)
            int3(Var3(size,name))
        member this.Item with get(i:int0,j:int0,k:int0) = int0(this.Idx3(i,j,k))
        member this.Item with get(i:int0,j:int0,k:int) = int0(this.Idx3(i,j,k.I))
        member this.Item with get(i:int0,j:int,k:int0) = int0(this.Idx3(i,j.I,k))
        member this.Item with get(i:int,j:int0,k:int0) = int0(this.Idx3(i.I,j,k))
        member this.Item with get(i:int,j:int,k:int0) = int0(this.Idx3(i.I,j.I,k))
        member this.Item with get(i:int,j:int0,k:int) = int0(this.Idx3(i.I,j,k.I))
        member this.Item with get(i:int0,j:int,k:int) = int0(this.Idx3(i,j.I,k.I))
        member this.Item with get(i:int,j:int,k:int) = int0(this.Idx3(i.I,j.I,k.I))
        member this.Item with get((a1:int0,b1:int0),(a2:int0,b2:int0),(a3:int0,b3:int0)) = int3(Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1-1,j+a2-1,k+a3-1)))
        member this.Item with get((a1:int0,b1:int0),(a2:int0,b2:int0),_:unit) = int3(Arx3(b1-a1+_1, b2-a2+_1, this.size3,  fun (i,j,k) -> this.Idx3(i+a1-1,j+a2-1,k)))
        member this.Item with get((a1:int0,b1:int0),_:unit,(a3:int0,b3:int0)) = int3(Arx3(b1-a1+_1, this.size2, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1-1,j,k+a3-1)))
        member this.Item with get(_:unit,(a2:int0,b2:int0),(a3:int0,b3:int0)) = int3(Arx3(this.size1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i,j+a2-1,k+a3-1)))
        member this.Item with get((a1:int0,b1:int0),(a2:int0,b2:int0),k:int0) = int2(Arx2(b1-a1+_1, b2-a2+_1, fun (i,j) -> this.Idx3(i+a1-1,j+a2-1,k)))
        member this.Item with get((a1:int0,b1:int0),j:int0,(a3:int0,b3:int0)) = int2(Arx2(b1-a1+_1, b3-a3+_1, fun (i,k) -> this.Idx3(i+a1-1,j,k+a3-1)))
        member this.Item with get(i:int0,(a2:int0,b2:int0),(a3:int0,b3:int0)) = int2(Arx2(b2-a2+_1,b3-a3+_1, fun (j,k) -> this.Idx3(i,j+a2-1,k+a3-1)))
        member this.Item with get((a1:int0,b1:int0),j:int0,k:int0) = int1(Arx1(b1-a1+_1, fun i -> this.Idx3(i+a1-1,j,k)))
        member this.Item with get(i:int0,(a2:int0,b2:int0),k:int0) = int1(Arx1(b2-a2+_1, fun j -> this.Idx3(i,j+a2-1,k)))
        member this.Item with get(i:int0,j:int0,(a3:int0,b3:int0)) = int1(Arx1(b3-a3+_1, fun k -> this.Idx3(i,j,k+a3-1)))
        
        static member ( + ) (x:int3,y:int3) =
            base3.sizeMismatchError(x,y)
            int3(Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr+y[i,j,k].expr))
        static member ( + ) (x:int3,y:int0) =
            int3(Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr+y.expr))
        static member ( + ) (x:int0,y:int3) =
            int3(Arx3(y.size1, y.size2, y.size3, fun (i,j,k) -> x.expr+y[i,j,k].expr))
        static member ( + ) (x:int3,y:int) =
            x + y.I
        static member ( + ) (x:int,y:int3) =
            x.I + y
        
        static member ( - ) (x:int3,y:int3) =
            base3.sizeMismatchError(x,y)
            int3(Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr-y[i,j,k].expr))
        static member ( - ) (x:int3,y:int0) =
            int3(Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr-y.expr))
        static member ( - ) (x:int0,y:int3) =
            int3(Arx3(y.size1, y.size2, y.size3, fun (i,j,k) -> x.expr-y[i,j,k].expr))
        static member ( - ) (x:int3,y:int) =
            x - y.I
        static member ( - ) (x:int,y:int3) =
            x.I - y
            
        static member ( * ) (x:int3,y:int3) =
            base3.sizeMismatchError(x,y)
            int3(Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr*y[i,j,k].expr))
        static member ( * ) (x:int3,y:int0) =
            int3(Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr*y.expr))
        static member ( * ) (x:int0,y:int3) =
            int3(Arx3(y.size1, y.size2, y.size3, fun (i,j,k) -> x.expr*y[i,j,k].expr))
        static member ( * ) (x:int3,y:int) =
            x * y.I
        static member ( * ) (x:int,y:int3) =
            x.I * y
            
        static member ( / ) (x:int3,y:int3) =
            base3.sizeMismatchError(x,y)
            int3(Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr/y[i,j,k].expr))
        static member ( / ) (x:int3,y:int0) =
            int3(Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr/y.expr))
        static member ( / ) (x:int0,y:int3) =
            int3(Arx3(y.size1, y.size2, y.size3, fun (i,j,k) -> x.expr/y[i,j,k].expr))
        static member ( / ) (x:int3,y:int) =
            x / y.I
        static member ( / ) (x:int,y:int3) =
            x.I / y
            
        static member ( /. ) (x:int3,y:int3) =
            base3.sizeMismatchError(x,y)
            int3(Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr/.y[i,j,k].expr))
        static member ( /. ) (x:int3,y:int0) =
            int3(Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr/.y.expr))
        static member ( /. ) (x:int0,y:int3) =
            int3(Arx3(y.size1, y.size2, y.size3, fun (i,j,k) -> x.expr/.y[i,j,k].expr))
        static member ( /. ) (x:int3,y:int) =
            x /. y.I
        static member ( /. ) (x:int,y:int3) =
            x.I /. y
            
        member this.allocate(n1:int0,n2:int0,n3:int0) = this.allocate(It 4,n1,n2,n3)
        member this.allocate(n1:int0,n2:int0,n3:int) = this.allocate(It 4,n1,n2,n3.I)
        member this.allocate(n1:int0,n2:int,n3:int0) = this.allocate(It 4,n1,n2.I,n3)
        member this.allocate(n1:int,n2:int0,n3:int0) = this.allocate(It 4,n1.I,n2,n3)
        member this.allocate(n1:int0,n2:int,n3:int) = this.allocate(It 4,n1,n2.I,n3.I)
        member this.allocate(n1:int,n2:int0,n3:int) = this.allocate(It 4,n1.I,n2,n3.I)
        member this.allocate(n1:int,n2:int,n3:int0) = this.allocate(It 4,n1.I,n2.I,n3)
        member this.allocate(n1:int,n2:int,n3:int) = this.allocate(It 4,n1.I,n2.I,n3.I)
        
        static member (<==) (v1:int3,v2:int3) =
            base3.subst(v1.expr,v1.size1,v1.size2,v1.size3,(fun i j k -> v1[i,j,k].expr),v2.expr,v2.size1,v2.size2,v2.size3,(fun i j k -> v2[i,j,k].expr))
        static member (<==) (v1:int3,v2:int0) =
            base3.subst(v1.expr,v1.size1,v1.size2,v1.size3,(fun i j k -> v1[i,j,k].expr),v2.expr)
        static member (<==) (x:int3,y:int) = x <== int0(Int_c y)
                
    ///<summary>数値型1次元配列</summary>
    and float3 (x:Expr3) =
        inherit base3(x)
        new (size,name,para) =
            p.param.var.setVar(It 4,size,name,para)
            float3(Var3(size,name))
        member this.Item with get(i:int0,j:int0,k:int0) = float0(this.Idx3(i,j,k))
        member this.Item with get(i:int0,j:int0,k:int) = float0(this.Idx3(i,j,k.I))
        member this.Item with get(i:int0,j:int,k:int0) = float0(this.Idx3(i,j.I,k))
        member this.Item with get(i:int,j:int0,k:int0) = float0(this.Idx3(i.I,j,k))
        member this.Item with get(i:int,j:int,k:int0) = float0(this.Idx3(i.I,j.I,k))
        member this.Item with get(i:int,j:int0,k:int) = float0(this.Idx3(i.I,j,k.I))
        member this.Item with get(i:int0,j:int,k:int) = float0(this.Idx3(i,j.I,k.I))
        member this.Item with get(i:int,j:int,k:int) = float0(this.Idx3(i.I,j.I,k.I))
        member this.Item with get((a1:int0,b1:int0),(a2:int0,b2:int0),(a3:int0,b3:int0)) = float3(Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1-1,j+a2-1,k+a3-1)))
        member this.Item with get((a1:int0,b1:int0),(a2:int0,b2:int0),_:unit) = float3(Arx3(b1-a1+_1, b2-a2+_1, this.size3,  fun (i,j,k) -> this.Idx3(i+a1-1,j+a2-1,k)))
        member this.Item with get((a1:int0,b1:int0),_:unit,(a3:int0,b3:int0)) = float3(Arx3(b1-a1+_1, this.size2, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1-1,j,k+a3-1)))
        member this.Item with get(_:unit,(a2:int0,b2:int0),(a3:int0,b3:int0)) = float3(Arx3(this.size1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i,j+a2-1,k+a3-1)))
        member this.Item with get((a1:int0,b1:int0),(a2:int0,b2:int0),k:int0) = float2(Arx2(b1-a1+_1, b2-a2+_1, fun (i,j) -> this.Idx3(i+a1-1,j+a2-1,k)))
        member this.Item with get((a1:int0,b1:int0),j:int0,(a3:int0,b3:int0)) = float2(Arx2(b1-a1+_1, b3-a3+_1, fun (i,k) -> this.Idx3(i+a1-1,j,k+a3-1)))
        member this.Item with get(i:int0,(a2:int0,b2:int0),(a3:int0,b3:int0)) = float2(Arx2(b2-a2+_1,b3-a3+_1, fun (j,k) -> this.Idx3(i,j+a2-1,k+a3-1)))
        member this.Item with get((a1:int0,b1:int0),j:int0,k:int0) = float1(Arx1(b1-a1+_1, fun i -> this.Idx3(i+a1-1,j,k)))
        member this.Item with get(i:int0,(a2:int0,b2:int0),k:int0) = float1(Arx1(b2-a2+_1, fun j -> this.Idx3(i,j+a2-1,k)))
        member this.Item with get(i:int0,j:int0,(a3:int0,b3:int0)) = float1(Arx1(b3-a3+_1, fun k -> this.Idx3(i,j,k+a3-1)))
        
        static member ( + ) (x:float3,y:float3) =
            base3.sizeMismatchError(x,y)
            float3(Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr+y[i,j,k].expr))
        static member ( + ) (x:float3,y:int3) =
            base3.sizeMismatchError(x,y)
            float3(Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr+y[i,j,k].expr))
        static member ( + ) (x:int3,y:float3) =
            base3.sizeMismatchError(x,y)
            float3(Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr+y[i,j,k].expr))
        static member ( + ) (x:float3,y:int0) =
            float3(Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr+y.expr))
        static member ( + ) (x:int0,y:float3) =
            float3(Arx3(y.size1, y.size2, y.size3, fun (i,j,k) -> x.expr+y[i,j,k].expr))
        static member ( + ) (x:float3,y:float0) =
            float3(Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr+y.expr))
        static member ( + ) (x:float0,y:float3) =
            float3(Arx3(y.size1, y.size2, y.size3, fun (i,j,k) -> x.expr+y[i,j,k].expr))
        static member ( + ) (x:float3,y:int) =
            x + y.I
        static member ( + ) (x:float3,y:double) =
            x + y.D
        static member ( + ) (x:int,y:float3) =
            x.I + y
        static member ( + ) (x:double,y:float3) =
            x.D + y
            
        static member ( - ) (x:float3,y:float3) =
            base3.sizeMismatchError(x,y)
            float3(Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr-y[i,j,k].expr))
        static member ( - ) (x:float3,y:int3) =
            base3.sizeMismatchError(x,y)
            float3(Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr-y[i,j,k].expr))
        static member ( - ) (x:int3,y:float3) =
            base3.sizeMismatchError(x,y)
            float3(Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr-y[i,j,k].expr))
        static member ( - ) (x:float3,y:int0) =
            float3(Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr-y.expr))
        static member ( - ) (x:int0,y:float3) =
            float3(Arx3(y.size1, y.size2, y.size3, fun (i,j,k) -> x.expr-y[i,j,k].expr))
        static member ( - ) (x:float3,y:float0) =
            float3(Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr-y.expr))
        static member ( - ) (x:float0,y:float3) =
            float3(Arx3(y.size1, y.size2, y.size3, fun (i,j,k) -> x.expr-y[i,j,k].expr))
        static member ( - ) (x:float3,y:int) =
            x - y.I
        static member ( - ) (x:float3,y:double) =
            x - y.D
        static member ( - ) (x:int,y:float3) =
            x.I - y
        static member ( - ) (x:double,y:float3) =
            x.D - y
            
        static member ( * ) (x:float3,y:float3) =
            base3.sizeMismatchError(x,y)
            float3(Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr*y[i,j,k].expr))
        static member ( * ) (x:float3,y:int3) =
            base3.sizeMismatchError(x,y)
            float3(Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr*y[i,j,k].expr))
        static member ( * ) (x:int3,y:float3) =
            base3.sizeMismatchError(x,y)
            float3(Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr*y[i,j,k].expr))
        static member ( * ) (x:float3,y:int0) =
            float3(Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr*y.expr))
        static member ( * ) (x:int0,y:float3) =
            float3(Arx3(y.size1, y.size2, y.size3, fun (i,j,k) -> x.expr*y[i,j,k].expr))
        static member ( * ) (x:float3,y:float0) =
            float3(Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr*y.expr))
        static member ( * ) (x:float0,y:float3) =
            float3(Arx3(y.size1, y.size2, y.size3, fun (i,j,k) -> x.expr*y[i,j,k].expr))
        static member ( * ) (x:float3,y:int) =
            x * y.I
        static member ( * ) (x:float3,y:double) =
            x * y.D
        static member ( * ) (x:int,y:float3) =
            x.I * y
        static member ( * ) (x:double,y:float3) =
            x.D * y
            
        static member ( / ) (x:float3,y:float3) =
            base3.sizeMismatchError(x,y)
            float3(Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr/y[i,j,k].expr))
        static member ( / ) (x:float3,y:int3) =
            base3.sizeMismatchError(x,y)
            float3(Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr/y[i,j,k].expr))
        static member ( / ) (x:int3,y:float3) =
            base3.sizeMismatchError(x,y)
            float3(Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr/y[i,j,k].expr))
        static member ( / ) (x:float3,y:int0) =
            float3(Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr/y.expr))
        static member ( / ) (x:int0,y:float3) =
            float3(Arx3(y.size1, y.size2, y.size3, fun (i,j,k) -> x.expr/y[i,j,k].expr))
        static member ( / ) (x:float3,y:float0) =
            float3(Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr/y.expr))
        static member ( / ) (x:float0,y:float3) =
            float3(Arx3(y.size1, y.size2, y.size3, fun (i,j,k) -> x.expr/y[i,j,k].expr))
        static member ( / ) (x:float3,y:int) =
            x / y.I
        static member ( / ) (x:float3,y:double) =
            x / y.D
        static member ( / ) (x:int,y:float3) =
            x.I / y
        static member ( / ) (x:double,y:float3) =
            x.D / y
        
        member this.allocate(n1:int0,n2:int0,n3:int0) = this.allocate(Dt,n1,n2,n3)
        member this.allocate(n1:int0,n2:int0,n3:int) = this.allocate(Dt,n1,n2,n3.I)
        member this.allocate(n1:int0,n2:int,n3:int0) = this.allocate(Dt,n1,n2.I,n3)
        member this.allocate(n1:int,n2:int0,n3:int0) = this.allocate(Dt,n1.I,n2,n3)
        member this.allocate(n1:int0,n2:int,n3:int) = this.allocate(Dt,n1,n2.I,n3.I)
        member this.allocate(n1:int,n2:int0,n3:int) = this.allocate(Dt,n1.I,n2,n3.I)
        member this.allocate(n1:int,n2:int,n3:int0) = this.allocate(Dt,n1.I,n2.I,n3)
        member this.allocate(n1:int,n2:int,n3:int) = this.allocate(Dt,n1.I,n2.I,n3.I)
        
        static member (<==) (v1:float3,v2:float3) =
            base3.subst(v1.expr,v1.size1,v1.size2,v1.size3,(fun i j k -> v1[i,j,k].expr),v2.expr,v2.size1,v2.size2,v2.size3,(fun i j k -> v2[i,j,k].expr))
        static member (<==) (v1:float3,v2:int3) =
            base3.subst(v1.expr,v1.size1,v1.size2,v1.size3,(fun i j k -> v1[i,j,k].expr),v2.expr,v2.size1,v2.size2,v2.size3,(fun i j k -> v2[i,j,k].expr))
        static member (<==) (v1:float3,v2:float0) =
            base3.subst(v1.expr,v1.size1,v1.size2,v1.size3,(fun i j k -> v1[i,j,k].expr),v2.expr)
        static member (<==) (v1:float3,v2:int0) =
            base3.subst(v1.expr,v1.size1,v1.size2,v1.size3,(fun i j k -> v1[i,j,k].expr),v2.expr)
        static member (<==) (v1:float3,v2:double) =
            v1 <== v2.D
        static member (<==) (v1:float3,v2:int) =
            v1 <== v2.I
                
    ///<summary>数値型1次元配列</summary>
    type complex3 (x:Expr3) =
        inherit base3(x)
        new (size,name,para) =
            p.param.var.setVar(It 4,size,name,para)
            complex3(Var3(size,name))
        member this.Item with get(i:int0,j:int0,k:int0) = complex0(this.Idx3(i,j,k))
        member this.Item with get(i:int0,j:int0,k:int) = complex0(this.Idx3(i,j,k.I))
        member this.Item with get(i:int0,j:int,k:int0) = complex0(this.Idx3(i,j.I,k))
        member this.Item with get(i:int,j:int0,k:int0) = complex0(this.Idx3(i.I,j,k))
        member this.Item with get(i:int,j:int,k:int0) = complex0(this.Idx3(i.I,j.I,k))
        member this.Item with get(i:int,j:int0,k:int) = complex0(this.Idx3(i.I,j,k.I))
        member this.Item with get(i:int0,j:int,k:int) = complex0(this.Idx3(i,j.I,k.I))
        member this.Item with get(i:int,j:int,k:int) = complex0(this.Idx3(i.I,j.I,k.I))
        member this.Item with get((a1:int0,b1:int0),(a2:int0,b2:int0),(a3:int0,b3:int0)) = complex3(Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1-1,j+a2-1,k+a3-1)))
        member this.Item with get((a1:int0,b1:int0),(a2:int0,b2:int0),_:unit) = complex3(Arx3(b1-a1+_1, b2-a2+_1, this.size3,  fun (i,j,k) -> this.Idx3(i+a1-1,j+a2-1,k)))
        member this.Item with get((a1:int0,b1:int0),_:unit,(a3:int0,b3:int0)) = complex3(Arx3(b1-a1+_1, this.size2, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1-1,j,k+a3-1)))
        member this.Item with get(_:unit,(a2:int0,b2:int0),(a3:int0,b3:int0)) = complex3(Arx3(this.size1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i,j+a2-1,k+a3-1)))
        member this.Item with get((a1:int0,b1:int0),(a2:int0,b2:int0),k:int0) = complex2(Arx2(b1-a1+_1, b2-a2+_1, fun (i,j) -> this.Idx3(i+a1-1,j+a2-1,k)))
        member this.Item with get((a1:int0,b1:int0),j:int0,(a3:int0,b3:int0)) = complex2(Arx2(b1-a1+_1, b3-a3+_1, fun (i,k) -> this.Idx3(i+a1-1,j,k+a3-1)))
        member this.Item with get(i:int0,(a2:int0,b2:int0),(a3:int0,b3:int0)) = complex2(Arx2(b2-a2+_1,b3-a3+_1, fun (j,k) -> this.Idx3(i,j+a2-1,k+a3-1)))
        member this.Item with get((a1:int0,b1:int0),j:int0,k:int0) = complex1(Arx1(b1-a1+_1, fun i -> this.Idx3(i+a1-1,j,k)))
        member this.Item with get(i:int0,(a2:int0,b2:int0),k:int0) = complex1(Arx1(b2-a2+_1, fun j -> this.Idx3(i,j+a2-1,k)))
        member this.Item with get(i:int0,j:int0,(a3:int0,b3:int0)) = complex1(Arx1(b3-a3+_1, fun k -> this.Idx3(i,j,k+a3-1)))
        member this.abs with get() = float3(Arx3(this.size1, this.size2, this.size3, fun (i,j,k) -> this[i,j,k].abs.expr))
        member this.pow with get() = float3(Arx3(this.size1, this.size2, this.size3, fun (i,j,k) -> this[i,j,k].pow.expr))
        member this.pha with get() = float3(Arx3(this.size1, this.size2, this.size3, fun (i,j,k) -> this[i,j,k].pha.expr))
        
        static member ( + ) (x:complex3,y:complex3) =
            base3.sizeMismatchError(x,y)
            float3(Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr+y[i,j,k].expr))
        static member ( + ) (x:complex3,y:float3) =
            base3.sizeMismatchError(x,y)
            float3(Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr+y[i,j,k].expr))
        static member ( + ) (x:float3,y:complex3) =
            base3.sizeMismatchError(x,y)
            float3(Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr+y[i,j,k].expr))
        static member ( + ) (x:complex3,y:int3) =
            base3.sizeMismatchError(x,y)
            float3(Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr+y[i,j,k].expr))
        static member ( + ) (x:int3,y:complex3) =
            base3.sizeMismatchError(x,y)
            float3(Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr+y[i,j,k].expr))
        static member ( + ) (x:complex3,y:int0) =
            float3(Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr+y.expr))
        static member ( + ) (x:complex3,y:float0) =
            float3(Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr+y.expr))
        static member ( + ) (x:complex3,y:complex0) =
            float3(Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr+y.expr))
        static member ( + ) (x:int0,y:complex3) =
            float3(Arx3(y.size1, y.size2, y.size3, fun (i,j,k) -> x.expr+y[i,j,k].expr))
        static member ( + ) (x:float0,y:complex3) =
            float3(Arx3(y.size1, y.size2, y.size3, fun (i,j,k) -> x.expr+y[i,j,k].expr))
        static member ( + ) (x:complex0,y:complex3) =
            float3(Arx3(y.size1, y.size2, y.size3, fun (i,j,k) -> x.expr+y[i,j,k].expr))
        static member ( + ) (x:complex3,y:int) =
            x + y.I
        static member ( + ) (x:complex3,y:double) =
            x + y.D
        static member ( + ) (x:int,y:complex3) =
            x.I + y
        static member ( + ) (x:double,y:complex3) =
            x.D + y
            
        static member ( - ) (x:complex3,y:complex3) =
            base3.sizeMismatchError(x,y)
            float3(Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr-y[i,j,k].expr))
        static member ( - ) (x:complex3,y:float3) =
            base3.sizeMismatchError(x,y)
            float3(Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr-y[i,j,k].expr))
        static member ( - ) (x:float3,y:complex3) =
            base3.sizeMismatchError(x,y)
            float3(Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr-y[i,j,k].expr))
        static member ( - ) (x:complex3,y:int3) =
            base3.sizeMismatchError(x,y)
            float3(Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr-y[i,j,k].expr))
        static member ( - ) (x:int3,y:complex3) =
            base3.sizeMismatchError(x,y)
            float3(Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr-y[i,j,k].expr))
        static member ( - ) (x:complex3,y:int0) =
            float3(Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr-y.expr))
        static member ( - ) (x:complex3,y:float0) =
            float3(Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr-y.expr))
        static member ( - ) (x:complex3,y:complex0) =
            float3(Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr-y.expr))
        static member ( - ) (x:int0,y:complex3) =
            float3(Arx3(y.size1, y.size2, y.size3, fun (i,j,k) -> x.expr-y[i,j,k].expr))
        static member ( - ) (x:float0,y:complex3) =
            float3(Arx3(y.size1, y.size2, y.size3, fun (i,j,k) -> x.expr-y[i,j,k].expr))
        static member ( - ) (x:complex0,y:complex3) =
            float3(Arx3(y.size1, y.size2, y.size3, fun (i,j,k) -> x.expr-y[i,j,k].expr))
        static member ( - ) (x:complex3,y:int) =
            x - y.I
        static member ( - ) (x:complex3,y:double) =
            x - y.D
        static member ( - ) (x:int,y:complex3) =
            x.I - y
        static member ( - ) (x:double,y:complex3) =
            x.D - y
            
        static member ( * ) (x:complex3,y:complex3) =
            base3.sizeMismatchError(x,y)
            float3(Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr*y[i,j,k].expr))
        static member ( * ) (x:complex3,y:float3) =
            base3.sizeMismatchError(x,y)
            float3(Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr*y[i,j,k].expr))
        static member ( * ) (x:float3,y:complex3) =
            base3.sizeMismatchError(x,y)
            float3(Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr*y[i,j,k].expr))
        static member ( * ) (x:complex3,y:int3) =
            base3.sizeMismatchError(x,y)
            float3(Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr*y[i,j,k].expr))
        static member ( * ) (x:int3,y:complex3) =
            base3.sizeMismatchError(x,y)
            float3(Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr*y[i,j,k].expr))
        static member ( * ) (x:complex3,y:int0) =
            float3(Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr*y.expr))
        static member ( * ) (x:complex3,y:float0) =
            float3(Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr*y.expr))
        static member ( * ) (x:complex3,y:complex0) =
            float3(Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr*y.expr))
        static member ( * ) (x:int0,y:complex3) =
            float3(Arx3(y.size1, y.size2, y.size3, fun (i,j,k) -> x.expr*y[i,j,k].expr))
        static member ( * ) (x:float0,y:complex3) =
            float3(Arx3(y.size1, y.size2, y.size3, fun (i,j,k) -> x.expr*y[i,j,k].expr))
        static member ( * ) (x:complex0,y:complex3) =
            float3(Arx3(y.size1, y.size2, y.size3, fun (i,j,k) -> x.expr*y[i,j,k].expr))
        static member ( * ) (x:complex3,y:int) =
            x * y.I
        static member ( * ) (x:complex3,y:double) =
            x * y.D
        static member ( * ) (x:int,y:complex3) =
            x.I * y
        static member ( * ) (x:double,y:complex3) =
            x.D * y
            
        static member ( / ) (x:complex3,y:complex3) =
            base3.sizeMismatchError(x,y)
            float3(Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr/y[i,j,k].expr))
        static member ( / ) (x:complex3,y:float3) =
            base3.sizeMismatchError(x,y)
            float3(Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr/y[i,j,k].expr))
        static member ( / ) (x:float3,y:complex3) =
            base3.sizeMismatchError(x,y)
            float3(Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr/y[i,j,k].expr))
        static member ( / ) (x:complex3,y:int3) =
            base3.sizeMismatchError(x,y)
            float3(Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr/y[i,j,k].expr))
        static member ( / ) (x:int3,y:complex3) =
            base3.sizeMismatchError(x,y)
            float3(Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr/y[i,j,k].expr))
        static member ( / ) (x:complex3,y:int0) =
            float3(Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr/y.expr))
        static member ( / ) (x:complex3,y:float0) =
            float3(Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr/y.expr))
        static member ( / ) (x:complex3,y:complex0) =
            float3(Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr/y.expr))
        static member ( / ) (x:int0,y:complex3) =
            float3(Arx3(y.size1, y.size2, y.size3, fun (i,j,k) -> x.expr/y[i,j,k].expr))
        static member ( / ) (x:float0,y:complex3) =
            float3(Arx3(y.size1, y.size2, y.size3, fun (i,j,k) -> x.expr/y[i,j,k].expr))
        static member ( / ) (x:complex0,y:complex3) =
            float3(Arx3(y.size1, y.size2, y.size3, fun (i,j,k) -> x.expr/y[i,j,k].expr))
        static member ( / ) (x:complex3,y:int) =
            x / y.I
        static member ( / ) (x:complex3,y:double) =
            x / y.D
        static member ( / ) (x:int,y:complex3) =
            x.I / y
        static member ( / ) (x:double,y:complex3) =
            x.D / y
            
        member this.allocate(n1:int0,n2:int0,n3:int0) = this.allocate(Zt,n1,n2,n3)
        member this.allocate(n1:int0,n2:int0,n3:int) = this.allocate(Zt,n1,n2,n3.I)
        member this.allocate(n1:int0,n2:int,n3:int0) = this.allocate(Zt,n1,n2.I,n3)
        member this.allocate(n1:int,n2:int0,n3:int0) = this.allocate(Zt,n1.I,n2,n3)
        member this.allocate(n1:int0,n2:int,n3:int) = this.allocate(Zt,n1,n2.I,n3.I)
        member this.allocate(n1:int,n2:int0,n3:int) = this.allocate(Zt,n1.I,n2,n3.I)
        member this.allocate(n1:int,n2:int,n3:int0) = this.allocate(Zt,n1.I,n2.I,n3)
        member this.allocate(n1:int,n2:int,n3:int) = this.allocate(Zt,n1.I,n2.I,n3.I)
        
        static member (<==) (v1:complex3,v2:complex3) =
            base3.subst(v1.expr,v1.size1,v1.size2,v1.size3,(fun i j k -> v1[i,j,k].expr),v2.expr,v2.size1,v2.size2,v2.size3,(fun i j k -> v2[i,j,k].expr))
        static member (<==) (v1:complex3,v2:float3) =
            base3.subst(v1.expr,v1.size1,v1.size2,v1.size3,(fun i j k -> v1[i,j,k].expr),v2.expr,v2.size1,v2.size2,v2.size3,(fun i j k -> v2[i,j,k].expr))
        static member (<==) (v1:complex3,v2:int3) =
            base3.subst(v1.expr,v1.size1,v1.size2,v1.size3,(fun i j k -> v1[i,j,k].expr),v2.expr,v2.size1,v2.size2,v2.size3,(fun i j k -> v2[i,j,k].expr))
        static member (<==) (v1:complex3,v2:complex0) =
            base3.subst(v1.expr,v1.size1,v1.size2,v1.size3,(fun i j k -> v1[i,j,k].expr),v2.expr)
        static member (<==) (v1:complex3,v2:float0) =
            base3.subst(v1.expr,v1.size1,v1.size2,v1.size3,(fun i j k -> v1[i,j,k].expr),v2.expr)
        static member (<==) (v1:complex3,v2:int0) =
            base3.subst(v1.expr,v1.size1,v1.size2,v1.size3,(fun i j k -> v1[i,j,k].expr),v2.expr)
        static member (<==) (v1:complex3,v2:double) =
            v1 <== v2.D
        static member (<==) (v1:complex3,v2:int) =
            v1 <== v2.I
                
    ///<summary>数値型1次元配列</summary>
    type num3 (e:Etype,x:Expr3) =
        inherit base3(x)
        new (typ,size,name,para) =
            p.param.var.setVar(typ,size,name,para)
            num3(typ,Var3(size,name))
        member this.etype with get() = e
        member this.Item with get(i:int0,j:int0,k:int0) = num0(e,this.Idx3(i,j,k))
        member this.Item with get(i:int0,j:int0,k:int) = num0(e,this.Idx3(i,j,k.I))
        member this.Item with get(i:int0,j:int,k:int0) = num0(e,this.Idx3(i,j.I,k))
        member this.Item with get(i:int,j:int0,k:int0) = num0(e,this.Idx3(i.I,j,k))
        member this.Item with get(i:int,j:int,k:int0) = num0(e,this.Idx3(i.I,j.I,k))
        member this.Item with get(i:int,j:int0,k:int) = num0(e,this.Idx3(i.I,j,k.I))
        member this.Item with get(i:int0,j:int,k:int) = num0(e,this.Idx3(i,j.I,k.I))
        member this.Item with get(i:int,j:int,k:int) = num0(e,this.Idx3(i.I,j.I,k.I))

                
        //<summary>値を0で初期化</summary> 
        override this.clear() = 
            this <== num0(It 4, Int_c 0)
            
        ///<summary>配列サイズ変数をメモリ未割当て状態に初期化</summary>
        override this.sizeinit() = 
            this.size1 <== -1
            
        static member sizeMismatchError(x:num3,y:num3) =
            let p = p.param
            if p.debugmode then
                p.error_code_counter_inc()
                p.comment("***debug array1 access check: "+p.error_code_counter.ToString()+"*****************************")
                br.if1 (x.size1 =/. y.size1) <| fun () -> 
                    print.c ("ERROR"+p.error_code_counter.ToString()+" operator '+' array size1 mismatch")
                    print.br
                br.if1 (x.size2 =/. y.size2) <| fun () -> 
                    print.c ("ERROR"+p.error_code_counter.ToString()+" operator '+' array size2 mismatch")
                    print.br
                p.comment("****************************************************")
                
        member this.allocate(n1:int0,n2:int0,n3:int0) = this.allocate(e,n1,n2,n3)
        member this.allocate(n1:int,n2:int0,n3:int0) = this.allocate(n1.I,n2,n3)
        member this.allocate(n1:int0,n2:int,n3:int0) = this.allocate(n1,n2.I,n3)
        member this.allocate(n1:int0,n2:int0,n3:int) = this.allocate(n1,n2,n3.I)
        member this.allocate(n1:int,n2:int,n3:int0) = this.allocate(n1.I,n2.I,n3)
        member this.allocate(n1:int,n2:int0,n3:int) = this.allocate(n1.I,n2,n3.I)
        member this.allocate(n1:int0,n2:int,n3:int) = this.allocate(n1,n2.I,n3.I)
        member this.allocate(n1:int,n2:int,n3:int) = this.allocate(n1.I,n2.I,n3.I)

        static member (+) (x:num3,y:num3) =
            num3.sizeMismatchError(x,y)
            Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr+y[i,j,k].expr)
        static member (+) (x:num0,y:num3) = Arx3(y.size1, y.size2, y.size3, fun (i,j,k) -> x.expr+y[i,j,k].expr)
        static member (+) (x:num3,y:num0) = Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr+y.expr)
        
        static member (-) (x:num3,y:num3) =
            num3.sizeMismatchError(x,y)
            Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr-y[i,j,k].expr)
        static member (-) (x:num0,y:num3) = Arx3(y.size1, y.size2, y.size3, fun (i,j,k) -> x.expr-y[i,j,k].expr)
        static member (-) (x:num3,y:num0) = Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr-y.expr)
        
        static member (*) (x:num3,y:num3) =
            num3.sizeMismatchError(x,y)
            Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr*y[i,j,k].expr)
        static member (*) (x:num0,y:num3) = Arx3(y.size1, y.size2, y.size3, fun (i,j,k) -> x.expr*y[i,j,k].expr)
        static member (*) (x:num3,y:num0) = Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr*y.expr)
        
        static member (/) (x:num3,y:num3) =
            num3.sizeMismatchError(x,y)
            Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr/y[i,j,k].expr)
        static member (/) (x:num0,y:num3) = Arx3(y.size1, y.size2, y.size3, fun (i,j,k) -> x.expr/y[i,j,k].expr)
        static member (/) (x:num3,y:num0) = Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr/.y.expr)

        static member (/.) (x:num3,y:num3) =
            num3.sizeMismatchError(x,y)
            Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr/y[i,j,k].expr)
        static member (/.) (x:num0,y:num3) = Arx3(y.size1, y.size2, y.size3, fun (i,j,k) -> x.expr/.y[i,j,k].expr)
        static member (/.) (x:num3,y:num0) = Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr/.y.expr)
        
        static member (<==) (v1:num3,v2:complex3) =
            base3.subst(v1.expr,v1.size1,v1.size2,v1.size3,(fun i j k -> v1[i,j,k].expr),v2.expr,v2.size1,v2.size2,v2.size3,(fun i j k -> v2[i,j,k].expr))
        static member (<==) (v1:num3,v2:float3) =
            base3.subst(v1.expr,v1.size1,v1.size2,v1.size3,(fun i j k -> v1[i,j,k].expr),v2.expr,v2.size1,v2.size2,v2.size3,(fun i j k -> v2[i,j,k].expr))
        static member (<==) (v1:num3,v2:int3) =
            base3.subst(v1.expr,v1.size1,v1.size2,v1.size3,(fun i j k -> v1[i,j,k].expr),v2.expr,v2.size1,v2.size2,v2.size3,(fun i j k -> v2[i,j,k].expr))
        static member (<==) (v1:num3,v2:num3) =
            base3.subst(v1.expr,v1.size1,v1.size2,v1.size3,(fun i j k -> v1[i,j,k].expr),v2.expr,v2.size1,v2.size2,v2.size3,(fun i j k -> v2[i,j,k].expr))
        static member (<==) (v1:num3,v2:complex0) =
            base3.subst(v1.expr,v1.size1,v1.size2,v1.size3,(fun i j k -> v1[i,j,k].expr),v2.expr)
        static member (<==) (v1:num3,v2:float0) =
            base3.subst(v1.expr,v1.size1,v1.size2,v1.size3,(fun i j k -> v1[i,j,k].expr),v2.expr)
        static member (<==) (v1:num3,v2:int0) =
            base3.subst(v1.expr,v1.size1,v1.size2,v1.size3,(fun i j k -> v1[i,j,k].expr),v2.expr)
        static member (<==) (v1:num3,v2:num0) =
            base3.subst(v1.expr,v1.size1,v1.size2,v1.size3,(fun i j k -> v1[i,j,k].expr),v2.expr)
        static member (<==) (v1:num3,v2:double) =
            v1 <== v2.D
        static member (<==) (v1:num3,v2:int) =
            v1 <== v2.I
            
    [<AutoOpen>]
    module asm_num3 =
        type asm with
            static member pow(x:num3,y:num0) = num3(Etype.prior(x.etype,y.etype), Arx3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.pow(x[i,j,k],y).expr))
            static member sin(x:num3) = num3(x.etype, Arx3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.sin(x[i,j,k]).expr))
            static member cos(x:num3) = num3(x.etype, Arx3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.cos(x[i,j,k]).expr))
            static member tan(x:num3) = num3(x.etype, Arx3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.tan(x[i,j,k]).expr))
            static member asin(x:num3) = num3(x.etype, Arx3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.asin(x[i,j,k]).expr))
            static member acos(x:num3) = num3(x.etype, Arx3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.acos(x[i,j,k]).expr))
            static member atan(x:num3) = num3(Dt, Arx3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.atan(x[i,j,k]).expr))
            static member atan2(x:num3,y:num3) = num3(Dt, Arx3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.atan2(x[i,j,k],y[i,j,k]).expr))
            static member exp(x:num3) = num3(x.etype, Arx3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.exp(x[i,j,k]).expr))
            static member abs(x:num3) = num3(x.etype, Arx3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.abs(x[i,j,k]).expr))
            static member log(x:num3) = num3(x.etype, Arx3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.log(x[i,j,k]).expr))
            static member log10(x:num3) = num3(x.etype, Arx3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.log10(x[i,j,k]).expr))
            static member sqrt(x:num3) = num3(x.etype, Arx3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.sqrt(x[i,j,k]).expr))
            static member floor(x:num3) = num3(x.etype, Arx3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.floor(x[i,j,k]).expr))
            static member ceil(typ,x:num3) = num3(x.etype, Arx3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.ceil(x[i,j,k]).expr))
            static member conj(typ,x:num3) = num3(x.etype, Arx3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.conj(x[i,j,k]).expr))
            