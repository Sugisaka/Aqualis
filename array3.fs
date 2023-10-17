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
        |Arx3 of (num0*num0*num0*((num0*num0*num0)->Expr))
        
    ///<summary>2次元配列</summary>
    type base3 (typ:Etype,x:Expr3) =
        ///<summary>変数を作成しリストに追加</summary>
        new (typ,size,name,para) = 
            p.var.setVar(typ,size,name,para)
            base3(typ,Var3(size,name))
        ///<summary>変数を作成しリストに追加</summary>
        new(sname,size,name) =
            p.var.setVar(Structure(sname),size,name,"")
            base3(Structure(sname),Var3(size,name))
        member _.expr with get() = x
        member _.code with get() =
            match x with
            |Var3(_,x) -> x
            |_ -> "Error(property .code)"
        ///<summary>変数の要素数</summary>
        member __.size1 
          with get() =
            match x with
            |Var3(_,name) -> 
                match p.lang with 
                |F |T -> num0(It 4,Var(name+"_size(1)"))
                |C -> num0(It 4,Var(name+"_size[0]"))
                |H -> num0(It 4,Var("<msub><mi mathvariant=\"script\">S</mi><mn>1</mn></msub><mo>&af;</mo><mo>[</mo><mi>"+name+"</mi><mo>]</mo>"))
            |Arx3(s1,_,_,_) -> s1
        ///<summary>変数の要素数</summary>
        member __.size2 
          with get() =
            match x with
            |Var3(_,name) -> 
                match p.lang with 
                |F |T -> num0(It 4,Var(name+"_size(2)"))
                |C -> num0(It 4,Var(name+"_size[1]"))
                |H -> num0(It 4,Var("<msub><mi mathvariant=\"script\">S</mi><mn>2</mn></msub><mo>&af;</mo><mo>[</mo><mi>"+name+"</mi><mo>]</mo>"))
            |Arx3(_,s2,_,_) -> s2
        ///<summary>変数の要素数</summary>
        member __.size3 
          with get() =
            match x with
            |Var3(_,name) -> 
                match p.lang with 
                |F |T -> num0(It 4,Var(name+"_size(3)"))
                |C -> num0(It 4,Var(name+"_size[2]"))
                |H -> num0(It 4,Var("<msub><mi mathvariant=\"script\">S</mi><mn>3</mn></msub><mo>&af;</mo><mo>[</mo><mi>"+name+"</mi><mo>]</mo>"))
            |Arx3(_,_,s3,_) -> s3
        ///<summary>インデクサ</summary>
        member this.Idx3(i:num0,j:num0,k:num0) =
            if p.debugMode then
                match x with
                |Var3(_,name) ->
                    p.errorIDinc()
                    p.comment("***debug array1 access check: "+p.errorID.ToString()+"*****************************")
                    br.if1 (Or [this.size1 .= -1; this.size2 .= -1; this.size3 .= -1]) <| fun () -> 
                        print.t ("ERROR"+p.errorID.ToString()+" array "+name+" is not allocated")
                    br.if1 (Or [(i .< _1); (this.size1 .< i)]) <| fun () ->
                        print.s [!.("ERROR"+p.errorID.ToString()+" array "+name+" illegal access. index ");i;!." is out of range (1:";this.size1;!.")"]
                    br.if1 (Or [(j .< _1); (this.size2 .< j)]) <| fun () ->
                        print.s [!.("ERROR"+p.errorID.ToString()+" array "+name+" illegal access. index ");j;!." is out of range (1:";this.size3;!.")"]
                    br.if1 (Or [(k .< _1); (this.size3 .< k)]) <| fun () ->
                        print.s [!.("ERROR"+p.errorID.ToString()+" array "+name+" illegal access. index ");k;!." is out of range (1:";this.size3;!.")"]
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
                
        member this.Idx3(i:num0,j:num0,k:int) = this.Idx3(i,j,k.I)
        member this.Idx3(i:num0,j:int,k:num0) = this.Idx3(i,j.I,k)
        member this.Idx3(i:int,j:num0,k:num0) = this.Idx3(i.I,j,k)
        member this.Idx3(i:int,j:int,k:num0) = this.Idx3(i,j.I,k)
        member this.Idx3(i:int,j:num0,k:int) = this.Idx3(i,j,k.I)
        member this.Idx3(i:num0,j:int,k:int) = this.Idx3(i,j.I,k.I)
        member this.Idx3(i:int,j:int,k:int) = this.Idx3(i.I,j.I,k.I)
        member this.Idx3((a1:num0,b1:num0),(a2:num0,b2:num0),(a3:num0,b3:num0)) = (Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1-1,j+a2-1,k+a3-1)))
        member this.Idx3((a1:num0,b1:num0),(a2:num0,b2:num0),_:unit) = Arx3(b1-a1+_1, b2-a2+_1, this.size3,  fun (i,j,k) -> this.Idx3(i+a1-1,j+a2-1,k))
        member this.Idx3((a1:num0,b1:num0),_:unit,(a3:num0,b3:num0)) = Arx3(b1-a1+_1, this.size2, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1-1,j,k+a3-1))
        member this.Idx3(_:unit,(a2:num0,b2:num0),(a3:num0,b3:num0)) = Arx3(this.size1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i,j+a2-1,k+a3-1))
        member this.Idx3((a1:num0,b1:num0),(a2:num0,b2:num0),k:num0) = Arx2(b1-a1+_1, b2-a2+_1, fun (i,j) -> this.Idx3(i+a1-1,j+a2-1,k))
        member this.Idx3((a1:num0,b1:num0),j:num0,(a3:num0,b3:num0)) = Arx2(b1-a1+_1, b3-a3+_1, fun (i,k) -> this.Idx3(i+a1-1,j,k+a3-1))
        member this.Idx3(i:num0,(a2:num0,b2:num0),(a3:num0,b3:num0)) = Arx2(b2-a2+_1,b3-a3+_1, fun (j,k) -> this.Idx3(i,j+a2-1,k+a3-1))
        member this.Idx3((a1:num0,b1:num0),j:num0,k:num0) = Arx1(b1-a1+_1, fun i -> this.Idx3(i+a1-1,j,k))
        member this.Idx3(i:num0,(a2:num0,b2:num0),k:num0) = Arx1(b2-a2+_1, fun j -> this.Idx3(i,j+a2-1,k))
        member this.Idx3(i:num0,j:num0,(a3:num0,b3:num0)) = Arx1(b3-a3+_1, fun k -> this.Idx3(i,j,k+a3-1))

        ///<summary>配列のメモリ割り当て</summary>
        member this.allocate(n1:num0,n2:num0,n3:num0) =
                match x with
                |Var3(size,name) ->
                    if p.debugMode then
                        p.errorIDinc()
                        p.comment("***debug array1 allocate check: "+p.errorID.ToString()+"*****************************")
                        br.branch <| fun b ->
                            b.IF (this.size1 .=/ -1) <| fun () ->
                                print.t ("ERROR"+p.errorID.ToString()+" array "+name+" is already allocated")
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
                            p.codewrite(name+"="+"("+typ.tostring(p.lang)+" *)"+"malloc("+"sizeof("+typ.tostring(p.lang)+")*"+this.size1.code+"*"+this.size2.code+"*"+this.size3.code+");\n")
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
                
        member this.allocate(n1:int,n2:num0,n3:num0) = this.allocate(n1.I,n2,n3)
        member this.allocate(n1:num0,n2:int,n3:num0) = this.allocate(n1,n2.I,n3)
        member this.allocate(n1:num0,n2:num0,n3:int) = this.allocate(n1,n2,n3.I)
        member this.allocate(n1:int,n2:int,n3:num0) = this.allocate(n1.I,n2.I,n3)
        member this.allocate(n1:int,n2:num0,n3:int) = this.allocate(n1.I,n2,n3.I)
        member this.allocate(n1:num0,n2:int,n3:int) = this.allocate(n1,n2.I,n3.I)
        member this.allocate(n1:int,n2:int,n3:int) = this.allocate(n1.I,n2.I,n3.I)
        
        ///<summary>配列のメモリ割り当て</summary>
        member this.deallocate() =
            if p.debugMode then
                match x with
                |Var3(_,name) ->
                    p.errorIDinc()
                    p.comment("***debug array1 deallocate check: "+p.errorID.ToString()+"*****************************")
                    br.branch <| fun b ->
                        b.IF (this.size1 .= -1) <| fun () ->
                            print.t ("ERROR"+p.errorID.ToString()+" cannot deallocate array "+name)
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
            if p.debugMode then
                p.errorIDinc()
                p.comment("***debug array1 access check: "+p.errorID.ToString()+"*****************************")
                br.if1 (v1.size1 .=/ v2.size1) <| fun () -> 
                    print.t ("ERROR"+p.errorID.ToString()+" operator '<==' array size1 mismatch")
                br.if1 (v1.size2 .=/ v2.size2) <| fun () -> 
                    print.t ("ERROR"+p.errorID.ToString()+" operator '<==' array size2 mismatch")
                br.if1 (v1.size3 .=/ v2.size3) <| fun () -> 
                    print.t ("ERROR"+p.errorID.ToString()+" operator '<==' array size3 mismatch")
                p.comment("****************************************************")
                
        static member subst (v1:Expr3,s11:num0,s12:num0,s13:num0,f1:num0->num0->num0->Expr,v2:Expr3,s21:num0,s22:num0,s23:num0,f2:num0->num0->num0->Expr) =
            if p.debugMode then
                p.errorIDinc()
                p.comment("***debug array1 access check: "+p.errorID.ToString()+"*****************************")
                br.if1 (s11 .=/ s21) <| fun () -> 
                    print.t ("ERROR"+p.errorID.ToString()+" operator '<==' array size1 mismatch")
                br.if1 (s12 .=/ s22) <| fun () -> 
                    print.t ("ERROR"+p.errorID.ToString()+" operator '<==' array size2 mismatch")
                br.if1 (s13 .=/ s23) <| fun () -> 
                    print.t ("ERROR"+p.errorID.ToString()+" operator '<==' array size3 mismatch")
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
                
        static member subst (v1:Expr3,s11:num0,s12:num0,s13:num0,f1:num0->num0->num0->Expr,v2:Expr) =
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
    type num3 (typ:Etype,x:Expr3) =
        inherit base3(typ,x)
        new (typ,size,name,para) =
            p.var.setVar(typ,size,name,para)
            num3(typ,Var3(size,name))
        member this.etype with get() = typ
        member this.Item with get(i:num0,j:num0,k:num0) = num0(typ,this.Idx3(i,j,k))
        member this.Item with get(i:num0,j:num0,k:int) = num0(typ,this.Idx3(i,j,k.I))
        member this.Item with get(i:num0,j:int,k:num0) = num0(typ,this.Idx3(i,j.I,k))
        member this.Item with get(i:int,j:num0,k:num0) = num0(typ,this.Idx3(i.I,j,k))
        member this.Item with get(i:int,j:int,k:num0) = num0(typ,this.Idx3(i.I,j.I,k))
        member this.Item with get(i:int,j:num0,k:int) = num0(typ,this.Idx3(i.I,j,k.I))
        member this.Item with get(i:num0,j:int,k:int) = num0(typ,this.Idx3(i,j.I,k.I))
        member this.Item with get(i:int,j:int,k:int) = num0(typ,this.Idx3(i.I,j.I,k.I))
        member this.Item with get((a1:num0,b1:num0),(a2:num0,b2:num0),(a3:num0,b3:num0)) = num3(typ,this.Idx3((a1,b1),(a2,b2),(a3,b3)))
        member this.Item with get((a1:num0,b1:num0),(a2:num0,b2:num0),_:unit) = num3(typ,this.Idx3((a1,b1),(a2,b2),()))
        member this.Item with get((a1:num0,b1:num0),_:unit,(a3:num0,b3:num0)) = num3(typ,this.Idx3((a1,b1),(),(a3,b3)))
        member this.Item with get(_:unit,(a2:num0,b2:num0),(a3:num0,b3:num0)) = num3(typ,this.Idx3((),(a2,b2),(a3,b3)))
        member this.Item with get((a1:num0,b1:num0),(a2:num0,b2:num0),k:num0) = num2(typ,this.Idx3((a1,b1),(a2,b2),k))
        member this.Item with get((a1:num0,b1:num0),j:num0,(a3:num0,b3:num0)) = num2(typ,this.Idx3((a1,b1),j,(a3,b3)))
        member this.Item with get(i:num0,(a2:num0,b2:num0),(a3:num0,b3:num0)) = num2(typ,this.Idx3(i,(a2,b2),(a3,b3)))
        member this.Item with get((a1:num0,b1:num0),j:num0,k:num0) = num1(typ,this.Idx3((a1,b1),j,k))
        member this.Item with get(i:num0,(a2:num0,b2:num0),k:num0) = num1(typ,this.Idx3(i,(a2,b2),k))
        member this.Item with get(i:num0,j:num0,(a3:num0,b3:num0)) = num1(typ,this.Idx3(i,j,(a3,b3)))

        //<summary>値を0で初期化</summary> 
        override this.clear() = 
            this <== num0(It 4, Int_c 0)
            
        ///<summary>配列サイズ変数をメモリ未割当て状態に初期化</summary>
        override this.sizeinit() = 
            this.size1 <== -1
            
        static member sizeMismatchError(x:num3,y:num3) =
            if p.debugMode then
                p.errorIDinc()
                p.comment("***debug array1 access check: "+p.errorID.ToString()+"*****************************")
                br.if1 (x.size1 .=/ y.size1) <| fun () -> 
                    print.t ("ERROR"+p.errorID.ToString()+" operator '+' array size1 mismatch")
                br.if1 (x.size2 .=/ y.size2) <| fun () -> 
                    print.t ("ERROR"+p.errorID.ToString()+" operator '+' array size2 mismatch")
                p.comment("****************************************************")
                


        static member (+) (x:num3,y:num3) =
            num3.sizeMismatchError(x,y)
            num3(Etype.prior(x.etype,y.etype),Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr+y[i,j,k].expr))
        static member (+) (x:num0,y:num3) = num3(Etype.prior(x.etype,y.etype),Arx3(y.size1, y.size2, y.size3, fun (i,j,k) -> x.expr+y[i,j,k].expr))
        static member (+) (x:num3,y:num0) = num3(Etype.prior(x.etype,y.etype),Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr+y.expr))
        
        static member (-) (x:num3,y:num3) =
            num3.sizeMismatchError(x,y)
            num3(Etype.prior(x.etype,y.etype),Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr-y[i,j,k].expr))
        static member (-) (x:num0,y:num3) = num3(Etype.prior(x.etype,y.etype),Arx3(y.size1, y.size2, y.size3, fun (i,j,k) -> x.expr-y[i,j,k].expr))
        static member (-) (x:num3,y:num0) = num3(Etype.prior(x.etype,y.etype),Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr-y.expr))
        
        static member (*) (x:num3,y:num3) =
            num3.sizeMismatchError(x,y)
            num3(Etype.prior(x.etype,y.etype),Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr*y[i,j,k].expr))
        static member (*) (x:num0,y:num3) = num3(Etype.prior(x.etype,y.etype),Arx3(y.size1, y.size2, y.size3, fun (i,j,k) -> x.expr*y[i,j,k].expr))
        static member (*) (x:num3,y:num0) = num3(Etype.prior(x.etype,y.etype),Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr*y.expr))
        
        static member (/) (x:num3,y:num3) =
            num3.sizeMismatchError(x,y)
            num3(Etype.prior(x.etype,y.etype),Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr/y[i,j,k].expr))
        static member (/) (x:num0,y:num3) = num3(Etype.prior(x.etype,y.etype),Arx3(y.size1, y.size2, y.size3, fun (i,j,k) -> x.expr/y[i,j,k].expr))
        static member (/) (x:num3,y:num0) = num3(Etype.prior(x.etype,y.etype),Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr./y.expr))

        static member (./) (x:num3,y:num3) =
            num3.sizeMismatchError(x,y)
            Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr/y[i,j,k].expr)
        static member (./) (x:num0,y:num3) = num3(Etype.prior(x.etype,y.etype),Arx3(y.size1, y.size2, y.size3, fun (i,j,k) -> x.expr./y[i,j,k].expr))
        static member (./) (x:num3,y:num0) = num3(Etype.prior(x.etype,y.etype),Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> x[i,j,k].expr./y.expr))
        
        static member (<==) (v1:num3,v2:num3) =
            if p.debugMode then
                p.errorIDinc()
                p.comment("***debug array1 access check: "+p.errorID.ToString()+"*****************************")
                br.if1 (v1.size1 .=/ v2.size1) <| fun () -> 
                    print.t ("ERROR"+p.errorID.ToString()+" operator '<==' array size1 mismatch")
                br.if1 (v1.size2 .=/ v2.size2) <| fun () -> 
                    print.t ("ERROR"+p.errorID.ToString()+" operator '<==' array size2 mismatch")
                br.if1 (v1.size3 .=/ v2.size3) <| fun () -> 
                    print.t ("ERROR"+p.errorID.ToString()+" operator '<==' array size3 mismatch")
                p.comment("****************************************************")
            match v1.expr,v2.expr with
            |Var3(_,x),Var3(_,y) ->
                match p.lang with
                |F|T -> p.codewrite(x + "=" + y)
                |C -> iter.num v1.size1 <| fun i -> iter.num v1.size2 <| fun j -> iter.num v1.size3 <| fun k -> v1[i,j,k] <== v2[i,j,k]
                |H   -> p.codewrite("<math>" + x + "<mo>&larr;</mo>" + y + "</math>\n<br/>\n")
            |Var3(_,x),Arx3(_,_,_,f) ->
                match p.lang with
                |F|T|C|H -> iter.num v1.size1 <| fun i -> iter.num v1.size2 <| fun j -> iter.num v1.size3 <| fun k -> v1[i,j,k] <== v2[i,j,k]
            |Arx3(_,_,_,_),Var3(_,_) ->
                match p.lang with
                |F|T|C|H -> iter.num v1.size1 <| fun i -> iter.num v1.size2 <| fun j -> iter.num v1.size3 <| fun k -> v1[i,j,k] <== v2[i,j,k]
            |Arx3(_,_,_,_),Arx3(_,_,_,_) ->
                match p.lang with
                |F|T|C|H -> iter.num v1.size1 <| fun i -> iter.num v1.size2 <| fun j -> iter.num v1.size3 <| fun k -> v1[i,j,k] <== v2[i,j,k]
        static member (<==) (v1:num3,v2:num0) =
            match v1.expr with
            |Var3(_,x) ->
                match p.lang with
                |F|T -> p.codewrite(x + "=" + v2.code)
                |C -> iter.num v1.size1 <| fun i -> iter.num v1.size2 <| fun j -> iter.num v1.size3 <| fun k -> v1[i,j,k] <== v2
                |H   -> p.codewrite("<math>" + x + "<mo>&larr;</mo>" + v2.code + "</math>\n<br/>\n")
            |Arx3(_,_,_,_) ->
                match p.lang with
                |F|T|C|H -> iter.num v1.size1 <| fun i -> iter.num v1.size2 <| fun j -> iter.num v1.size3 <| fun k -> v1[i,j,k] <== v2
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
            