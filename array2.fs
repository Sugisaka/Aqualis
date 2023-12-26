(*
Copyright (c) 2022 Jun-ichiro Sugisaka

This software is released under the MIT License.
http://opensource.org/licenses/mit-license.php
*)
namespace Aqualis
    
    open Aqualis_base
    
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
            p.var.setVar(typ,size,name,para)
            base2(typ,Var2(size,name))
        ///<summary>変数を作成しリストに追加</summary>
        new(sname,size,name) =
            p.var.setVar(Structure(sname),size,name,"")
            base2(Structure(sname),Var2(size,name))
        member _.expr with get() = x
        member _.code with get() =
            match x with
            |Var2(_,x) -> x
            |_ -> "Error(property .code)"
        ///<summary>変数の要素数</summary>
        member __.size1 
          with get() =
            match x with
            |Var2(_,name) -> 
                match p.lang with 
                |F -> Var(It 4,name+"_size(1)")
                |C -> Var(It 4,name+"_size[0]")
                |T -> Var(It 4,"\\mathcal{S}_1["+name+"]")
                |H -> Var(It 4,"\\mathcal{S}_1["+name+"]")
            |Arx2(s1,_,_) -> s1
        ///<summary>変数の要素数</summary>
        member __.size2 
          with get() =
            match x with
            |Var2(_,name) -> 
                match p.lang with 
                |F -> Var(It 4,name+"_size(2)")
                |C -> Var(It 4,name+"_size[1]")
                |T -> Var(It 4,"\\mathcal{S}_2["+name+"]")
                |H -> Var(It 4,"\\mathcal{S}_2["+name+"]")
            |Arx2(_,s2,_) -> s2
        ///<summary>インデクサ</summary>
        member this.Idx2(i:num0,j:num0) =
            if p.debugMode then
                match x with
                |Var2(_,name) ->
                    p.errorIDinc()
                    p.comment("***debug array1 access check: "+p.errorID.ToString()+"*****************************")
                    br.branch <| fun b ->
                        b.IF (Or [this.size1 .= -1; this.size2 .= -1]) <| fun () -> 
                            print.t ("ERROR"+p.errorID.ToString()+" array "+name+" is not allocated")
                        b.IF (Or [(i .< _1); (this.size1 .< i)]) <| fun () ->
                            print.s [!.("ERROR"+p.errorID.ToString()+" array "+name+" illegal access. index ");i;!." is out of range (1:";this.size1;!.")"]
                        b.IF (Or [(j .< _1); (this.size2 .< j)]) <| fun () ->
                            print.s [!.("ERROR"+p.errorID.ToString()+" array "+name+" illegal access. index ");j;!." is out of range (1:";this.size2;!.")"]
                    p.comment("****************************************************")
                |_ -> ()
            match x with
            |Var2(_,name) -> Idx2(typ,name,i,j)
            |Arx2(_,_,f) -> f (i,j)
            
        member this.Idx2(i:num0,j:int) = this.Idx2(i,j.I)
        member this.Idx2(i:int,j:num0) = this.Idx2(i.I,j)
        member this.Idx2(i:int,j:int) = this.Idx2(i.I,j.I)
        member this.Idx2((a1:num0,b1:num0),_:unit) = Arx2(b1-a1+_1, this.size2, fun (i,j) -> this.Idx2(i+a1-1,j))
        member this.Idx2((a1:int,b1:int),_:unit) = this.Idx2((a1.I,b1.I),())
        member this.Idx2(_:unit,(a2:num0,b2:num0)) = Arx2(this.size1,b2-a2+_1, fun (i,j) -> this.Idx2(i,j+a2-1))
        member this.Idx2(_:unit,(a2:int,b2:int)) = this.Idx2((),(a2.I,b2.I))
        member this.Idx2((a1:num0,b1:num0),(a2:num0,b2:num0)) = Arx2(b1-a1+_1, b2-a2+_1, fun (i,j) -> this.Idx2(i+a1-1,j+a2-1))
        member this.Idx2((a1:num0,b1:num0),(a2:int,b2:int)) = this.Idx2((a1,b1),(a2.I,b2.I))
        member this.Idx2((a1:int,b1:int),(a2:num0,b2:num0)) = this.Idx2((a1.I,b1.I),(a2,b2))
        member this.Idx2((a1:int ,b1:int),(a2:int ,b2:int)) = this.Idx2((a1.I,b1.I),(a2.I,b2.I))
        member this.Idx2(i:num0,_:unit) = Arx1(this.size2, fun j -> this.Idx2(i,j))
        member this.Idx2(_:unit,j:num0) = Arx1(this.size1, fun i -> this.Idx2(i,j))
        member this.Idx2(i:num0,(a2:num0,b2:num0)) = Arx1(b2-a2+_1, fun j -> this.Idx2(i,j+a2-1))
        member this.Idx2(i:int,(a2:num0,b2:num0)) = this.Idx2(i.I,(a2,b2))
        member this.Idx2((a1:num0,b1:num0),j:num0) = Arx1(b1-a1+_1, fun i -> this.Idx2(i+a1-1,j))
        member this.Idx2((a1:num0,b1:num0),j:int) = this.Idx2((a1,b1),j.I)

        ///<summary>配列のメモリ割り当て</summary>
        member this.allocate(n1:num0,n2:num0) =
                match x with
                |Var2(size,name) ->
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
                        |A2(0,0) ->
                            this.size1 <== n1
                            this.size2 <== n2
                            p.codewrite("allocate("+name+"(1:"+this.size1.code+",1:"+this.size2.code+")"+")"+"\n")
                        |_ -> 
                            p.codewrite("(Error:055-001 「"+name+"」は可変長2次元配列ではありません")
                    |C ->
                        match size with
                        |A2(0,0) ->
                            this.size1 <== n1
                            this.size2 <== n2
                            p.codewrite(name+"="+"("+typ.tostring(p.lang)+" *)"+"malloc("+"sizeof("+typ.tostring(p.lang)+")*"+this.size1.code+"*"+this.size2.code+");\n")
                        |_ -> 
                            p.codewrite("(Error:055-001 「"+name+"」は可変長2次元配列ではありません")
                    |T ->
                        match size with
                        |A2(0,0) ->
                            p.codewrite("$"+name+"$: allocate($"+n1.code+","+n2.code+"$)\\\\\n")
                        |_ -> 
                            p.codewrite("(Error:055-001 「"+name+"」は可変長2次元配列ではありません")
                    |H ->
                        match size with
                        |A2(0,0) ->
                            p.codewrite("\\("+name+"\\): allocate(\\("+n1.code+","+n2.code+"\\))<br/>\n")
                        |_ -> 
                            p.codewrite("(Error:055-001 「"+name+"」は可変長2次元配列ではありません")
                |_ -> ()
                
        member this.allocate(n1:num0,n2:int) = this.allocate(n1,n2.I)
        member this.allocate(n1:int,n2:num0) = this.allocate(n1.I,n2)
        member this.allocate(n1:int,n2:int) = this.allocate(n1.I,n2.I)
        
        ///<summary>配列のメモリ割り当て</summary>
        member this.deallocate() =
            if p.debugMode then
                match x with
                |Var2(_,name) ->
                    p.errorIDinc()
                    p.comment("***debug array1 deallocate check: "+p.errorID.ToString()+"*****************************")
                    br.branch <| fun b ->
                        b.IF (this.size1 .= -1) <| fun () ->
                            print.t ("ERROR"+p.errorID.ToString()+" cannot deallocate array "+name)
                    p.comment("****************************************************")
                |_ -> ()
            match x with
            |Var2(size,name) ->
                match p.lang with
                |F ->
                    match size with
                    |A2(0,0) ->
                        this.size1 <== -1
                        p.codewrite("deallocate("+name+")"+"\n")
                    |_ -> ()
                |C ->
                    match size with
                    |A2(0,0) ->
                        this.size1 <== -1
                        p.codewrite("free("+name+");"+"\n")
                    |_ -> ()
                |T ->
                    match size with
                    |A2(0,0) ->
                        p.codewrite("$"+name+"$: deallocate\\\\\n")
                    |_ -> ()
                |H ->
                    match size with
                    |A2(0,0) ->
                        p.codewrite("\\("+name+"\\): deallocate<br/>\n")
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
                    code(i,j)
                
        ///<summary>配列の全要素に対する処理</summary>
        member this.foreach_exit code =
            iter.num_exit this.size1 <| fun (ext1,i) -> 
                iter.num_exit this.size2 <| fun (ext2,j) -> 
                    code(ext1,ext2,i,j)
                
        static member sizeMismatchError(x:base2,y:base2) =
            if p.debugMode then
                p.errorIDinc()
                p.comment("***debug array1 access check: "+p.errorID.ToString()+"*****************************")
                br.branch <| fun b ->
                    b.IF (x.size1 .=/ y.size1) <| fun () -> 
                        print.t ("ERROR"+p.errorID.ToString()+" array size (first index) mismatch")
                br.branch <| fun b ->
                    b.IF (x.size2 .=/ y.size2) <| fun () -> 
                        print.t ("ERROR"+p.errorID.ToString()+" array size (second index) mismatch")
                p.comment("****************************************************")
                
        static member subst (v1:Expr2,s11:num0,s12:num0,f1:num0->num0->num0,v2:Expr2,s21:num0,s22:num0,f2:num0->num0->num0) =
            if p.debugMode then
                p.errorIDinc()
                p.comment("***debug array1 access check: "+p.errorID.ToString()+"*****************************")
                br.branch <| fun b ->
                    b.IF (s11 .=/ s21) <| fun () -> 
                        print.t ("ERROR"+p.errorID.ToString()+" operator '<==' array size mismatch")
                br.branch <| fun b ->
                    b.IF (s21 .=/ s22) <| fun () -> 
                        print.t ("ERROR"+p.errorID.ToString()+" operator '<==' array size mismatch")
                p.comment("****************************************************")
            match v1,v2 with
            |Var2(_,x),Var2(_,y) ->
                match p.lang with
                |F|T ->
                    p.codewrite(x + "=" + y)
                |C ->
                    iter.num s11 <| fun i -> iter.num s12 <| fun j -> (f1 i j) <== (f2 i j)
                |H ->
                    p.codewrite("\\[")
                    p.codewrite("\\begin{align}")
                    p.codewrite(x + " \\leftarrow " + y)
                    p.codewrite("\\end{align}")
                    p.codewrite("\\]")
            |Var2(_,x),Arx2(_,_,f) ->
                match p.lang with
                |F|T|C|H -> iter.num s11 <| fun i -> iter.num s12 <| fun j -> (f1 i j) <== (f2 i j)
            |Arx2(_,_,_),Var2(_,_) ->
                match p.lang with
                |F|T|C|H -> iter.num s11 <| fun i -> iter.num s12 <| fun j -> (f1 i j) <== (f2 i j)
            |Arx2(_,_,_),Arx2(_,_,_) ->
                match p.lang with
                |F|T|C|H -> iter.num s11 <| fun i -> iter.num s12 <| fun j -> (f1 i j) <== (f2 i j)
                
        static member subst (v1:Expr2,s11:num0,s12:num0,f1:num0->num0->num0,v2:num0) =
            match v1 with
            |Var2(_,x) ->
                match p.lang with
                |F|T ->
                    p.codewrite(x + "=" + v2.code)
                |C ->
                    iter.num s11 <| fun i -> iter.num s12 <| fun j -> (f1 i j) <== v2
                |H ->
                    p.codewrite("\\[")
                    p.codewrite("\\begin{align}")
                    p.codewrite(x + " \\leftarrow " + v2.code)
                    p.codewrite("\\end{align}")
                    p.codewrite("\\]")
            |Arx2(_,_,_) ->
                match p.lang with
                |F|T|C|H -> iter.num s11 <| fun i -> iter.num s12 <| fun j -> (f1 i j) <== v2
                
    ///<summary>数値型1次元配列</summary>
    type num2 (typ:Etype,x:Expr2) =
        inherit base2(typ,x)
        new (typ,size,name,para) =
            p.var.setVar(typ,size,name,para)
            num2(typ,Var2(size,name))
        member this.etype with get() = typ
        member this.Item with get(i:num0,j:num0) = this.Idx2(i,j)
        member this.Item with get(i:num0,j:int) = this.Idx2(i,j.I)
        member this.Item with get(i:int,j:num0) = this.Idx2(i.I,j)
        member this.Item with get(i:int,j:int) = this.Idx2(i.I,j.I)
        member this.Item with get((a1:num0,b1:num0),_:unit) = num2(typ,this.Idx2((a1,b1),()))
        member this.Item with get((a1:int,b1:num0),_:unit) = num2(typ,this.Idx2((I a1,b1),()))
        member this.Item with get((a1:num0,b1:int),_:unit) = num2(typ,this.Idx2((a1,I b1),()))
        member this.Item with get((a1:int,b1:int),_:unit) = num2(typ,this.Idx2((a1,b1),()))
        member this.Item with get(_:unit,(a2:num0,b2:num0)) = num2(typ,this.Idx2((),(a2,b2)))
        member this.Item with get(_:unit,(a2:int,b2:num0)) = num2(typ,this.Idx2((),(I a2,b2)))
        member this.Item with get(_:unit,(a2:num0,b2:int)) = num2(typ,this.Idx2((),(a2,I b2)))
        member this.Item with get(_:unit,(a2:int,b2:int)) = num2(typ,this.Idx2((),(a2,b2)))
        member this.Item with get((a1:num0,b1:num0),(a2:num0,b2:num0)) = num2(typ,this.Idx2((a1,b1),(a2,b2)))
        member this.Item with get((a1:num0,b1:num0),(a2:int,b2:int)) = num2(typ,this.Idx2((a1,b1),(a2,b2)))
        member this.Item with get((a1:int,b1:int),(a2:num0,b2:num0)) = num2(typ,this.Idx2((a1,b1),(a2,b2)))
        member this.Item with get((a1:int ,b1:int),(a2:int ,b2:int)) = num2(typ,this.Idx2((a1 ,b1),(a2 ,b2)))
        member this.Item with get(i:num0,_:unit) = num1(typ,this.Idx2(i,()))
        member this.Item with get(i:int,_:unit) = num1(typ,this.Idx2(I i,()))
        member this.Item with get(_:unit,j:num0) = num1(typ,this.Idx2((),j))
        member this.Item with get(_:unit,j:int) = num1(typ,this.Idx2((),I j))
        member this.Item with get(i:num0,(a2:num0,b2:num0)) = num1(typ,this.Idx2(i,(a2,b2)))
        member this.Item with get(i:num0,(a2:int,b2:num0)) = num1(typ,this.Idx2(i,(I a2,b2)))
        member this.Item with get(i:num0,(a2:num0,b2:int)) = num1(typ,this.Idx2(i,(a2,I b2)))
        member this.Item with get(i:num0,(a2:int,b2:int)) = num1(typ,this.Idx2(i,(I a2,I b2)))
        member this.Item with get(i:int,(a2:num0,b2:num0)) = num1(typ,this.Idx2(i,(a2,b2)))
        member this.Item with get(i:int,(a2:int,b2:num0)) = num1(typ,this.Idx2(i,(I a2,b2)))
        member this.Item with get(i:int,(a2:num0,b2:int)) = num1(typ,this.Idx2(i,(a2,I b2)))
        member this.Item with get(i:int,(a2:int,b2:int)) = num1(typ,this.Idx2(i,(I a2,I b2)))
        member this.Item with get((a1:num0,b1:num0),j:num0) = num1(typ,this.Idx2((a1,b1),j))
        member this.Item with get((a1:int,b1:num0),j:num0) = num1(typ,this.Idx2((I a1,b1),j))
        member this.Item with get((a1:num0,b1:int),j:num0) = num1(typ,this.Idx2((a1,I b1),j))
        member this.Item with get((a1:int,b1:int),j:num0) = num1(typ,this.Idx2((I a1,I b1),j))
        member this.Item with get((a1:num0,b1:num0),j:int) = num1(typ,this.Idx2((a1,b1),j))
        member this.Item with get((a1:int,b1:num0),j:int) = num1(typ,this.Idx2((I a1,b1),j))
        member this.Item with get((a1:num0,b1:int),j:int) = num1(typ,this.Idx2((a1,I b1),j))
        member this.Item with get((a1:int,b1:int),j:int) = num1(typ,this.Idx2((I a1,I b1),j))
        
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
            this <== Int_c 0
            
        ///<summary>配列サイズ変数をメモリ未割当て状態に初期化</summary>
        override this.sizeinit() = 
            this.size1 <== -1
            
        static member sizeMismatchError(x:num2,y:num2) =
            if p.debugMode then
                p.errorIDinc()
                p.comment("***debug array1 access check: "+p.errorID.ToString()+"*****************************")
                br.if1 (x.size1 .=/ y.size1) <| fun () -> 
                    print.t ("ERROR"+p.errorID.ToString()+" array size1 mismatch")
                br.if1 (x.size2 .=/ y.size2) <| fun () -> 
                    print.t ("ERROR"+p.errorID.ToString()+" array size2 mismatch")
                p.comment("****************************************************")
                
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
        
        static member (*) (x:num2,y:num2) =
            num2.sizeMismatchError(x,y)
            num2(x.etype%%y.etype,Arx2(x.size1, x.size2, fun (i,j) -> x[i,j]*y[i,j]))
        static member (*) (x:num0,y:num2) = num2(x.etype%%y.etype,Arx2(y.size1, y.size2, fun (i,j) -> x*y[i,j]))
        static member (*) (x:int,y:num2) = num2((It 4)%%y.etype,Arx2(y.size1, y.size2, fun (i,j) -> x*y[i,j]))
        static member (*) (x:double,y:num2) = num2(Dt%%y.etype,Arx2(y.size1, y.size2, fun (i,j) -> x*y[i,j]))
        static member (*) (x:num2,y:num0) = num2(x.etype%%y.etype,Arx2(x.size1, x.size2, fun (i,j) -> x[i,j]*y))
        static member (*) (x:num2,y:int) = num2(x.etype%%(It 4),Arx2(x.size1, x.size2, fun (i,j) -> x[i,j]*y))
        static member (*) (x:num2,y:double) = num2(x.etype%%Dt,Arx2(x.size1, x.size2, fun (i,j) -> x[i,j]*y))
        
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
            if p.debugMode then
                p.errorIDinc()
                p.comment("***debug array1 access check: "+p.errorID.ToString()+"*****************************")
                br.branch <| fun b ->
                    b.IF (v1.size1 .=/ v2.size1) <| fun () -> 
                        print.t ("ERROR"+p.errorID.ToString()+" operator '<==' array size mismatch")
                br.branch <| fun b ->
                    b.IF (v1.size2 .=/ v2.size2) <| fun () -> 
                        print.t ("ERROR"+p.errorID.ToString()+" operator '<==' array size mismatch")
                p.comment("****************************************************")
            match v1.expr,v2.expr with
            |Var2(_,x),Var2(_,y) ->
                match p.lang with
                |F|T ->
                    p.codewrite(x + "=" + y)
                |C ->
                    iter.num v1.size1 <| fun i -> iter.num v1.size2 <| fun j -> v1[i,j] <== v2[i,j]
                |H ->
                    p.codewrite("\\[")
                    p.codewrite("\\begin{align}")
                    p.codewrite(x + " \\leftarrow " + y)
                    p.codewrite("\\end{align}")
                    p.codewrite("\\]")
            |Var2(_,x),Arx2(_,_,f) ->
                match p.lang with
                |F|T|C|H -> iter.num v1.size1 <| fun i -> iter.num v1.size2 <| fun j -> v1[i,j] <== v2[i,j]
            |Arx2(_,_,_),Var2(_,_) ->
                match p.lang with
                |F|T|C|H -> iter.num v1.size1 <| fun i -> iter.num v1.size2 <| fun j -> v1[i,j] <== v2[i,j]
            |Arx2(_,_,_),Arx2(_,_,_) ->
                match p.lang with
                |F|T|C|H -> iter.num v1.size1 <| fun i -> iter.num v1.size2 <| fun j -> v1[i,j] <== v2[i,j]
        static member (<==) (v1:num2,v2:num0) =
            match v1.expr with
            |Var2(_,x) ->
                match p.lang with
                |F|T ->
                    p.codewrite(x + "=" + v2.code)
                |C ->
                    iter.num v1.size1 <| fun i -> iter.num v1.size2 <| fun j -> v1[i,j] <== v2
                |H ->
                    p.codewrite("\\[")
                    p.codewrite("\\begin{align}")
                    p.codewrite(x + " \\leftarrow " + v2.code)
                    p.codewrite("\\end{align}")
                    p.codewrite("\\]")
            |Arx2(_,_,_) ->
                match p.lang with
                |F|T|C|H -> iter.num v1.size1 <| fun i -> iter.num v1.size2 <| fun j -> v1[i,j] <== v2
        static member (<==) (v1:num2,v2:int) =
            v1 <== v2.I
        static member (<==) (v1:num2,v2:double) =
            v1 <== v2.D
            
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
            