﻿(*
Copyright (c) 2022 Jun-ichiro Sugisaka

This software is released under the MIT License.
http://opensource.org/licenses/mit-license.php
*)
namespace Aqualis
    
    open Aqualis_base
    
    ///<summary>1次元配列変数</summary>
    type Expr1 =
        ///<summary>変数</summary>
        |Var1 of (VarType*string)
        ///<summary>部分配列</summary>
        |Arx1 of (num0*(num0->num0))
        
    ///<summary>1次元配列</summary>
    type base1 (typ:Etype,x:Expr1) =
        ///<summary>変数を作成しリストに追加</summary>
        new (typ,size,name,para) = 
            p.var.setVar(typ,size,name,para)
            base1(typ,Var1(size,name))
        ///<summary>変数を作成しリストに追加</summary>
        new(sname,size,name) =
            p.var.setVar(Structure(sname),size,name,"")
            base1(Structure(sname),Var1(size,name))
        member _.expr with get() = x
        member _.code with get() =
            match x with
            |Var1(_,x) -> x
            |_ -> "Error(property .code)"
        ///<summary>変数の要素数</summary>
        member __.size1 
          with get() =
            match x with
            |Var1(_,name) -> 
                match p.lang with 
                |Fortran -> Var(It 4,name+"_size(1)")
                |C99 -> Var(It 4,name+"_size[0]")
                |LaTeX -> Var(It 4,"\\mathcal{S}_1["+name+"]")
                |HTML -> Var(It 4,"\\mathcal{S}_1["+name+"]")
                |Python -> Var(It 4,name+"_size[0]")
            |Arx1(s,_) -> s
        ///<summary>インデクサ</summary>
        member this.Idx1(i:num0) =
            if p.debugMode then
                match x with
                |Var1(_,name) ->
                    p.errorIDinc()
                    p.comment("***debug array1 access check: "+p.errorID.ToString()+"*****************************")
                    br.branch <| fun b ->
                        b.IF (this.size1 .= -1) <| fun () -> 
                            print.t ("ERROR"+p.errorID.ToString()+" array "+name+" is not allocated")
                        b.IF (Or [i .< _0; this.size1 .<= i]) <| fun () ->
                            print.s [!.("ERROR"+p.errorID.ToString()+" array "+name+" illegal access. index ");i;!." is out of range (1:";this.size1;!.")"]
                    p.comment("****************************************************")
                |_ -> ()
            match x with
            |Var1(_,name) -> Idx1(typ,name,i)
            |Arx1(_,f) -> f i
        member this.Idx1(i:int) = this.Idx1(i.I)
        member this.Idx1(a:num0,b:num0) = Arx1(b-a+_1,fun i -> this.Idx1(i+a))
        member this.Idx1(a:num0,b:int)  = Arx1(b-a+_1,fun i -> this.Idx1(i+a))
        member this.Idx1(a:int ,b:num0) = Arx1(b-a+_1,fun i -> this.Idx1(i+a))
        member this.Idx1(a:int ,b:int)  = Arx1(b-a+_1,fun i -> this.Idx1(i+a))
        
        ///<summary>配列のメモリ割り当て</summary>
        member this.allocate(n1:num0) =
                match x with
                |Var1(size1,name) ->
                    if p.debugMode then
                        p.errorIDinc()
                        p.comment("***debug array1 allocate check: "+p.errorID.ToString()+"*****************************")
                        br.branch <| fun b ->
                            b.IF (this.size1 .=/ -1) <| fun () ->
                                print.t ("ERROR"+p.errorID.ToString()+" array "+name+" is already allocated")
                        p.comment("****************************************************")
                    match p.lang with
                    |Fortran ->
                        match size1 with
                        |A1(0) ->
                            this.size1 <== n1
                            p.codewrite("allocate("+name+"(1:"+this.size1.code+")"+")"+"\n")
                        |_ -> 
                            p.codewrite("(Error:055-001 「"+name+"」は可変長1次元配列ではありません")
                    |C99 ->
                        match size1 with
                        |A1(0) ->
                            this.size1 <== n1
                            p.codewrite(name+"="+"("+typ.tostring(p.lang)+" *)"+"malloc("+"sizeof("+typ.tostring(p.lang)+")*"+this.size1.code+");\n")
                        |_ -> 
                            p.codewrite("(Error:055-001 「"+name+"」は可変長1次元配列ではありません")
                    |LaTeX ->
                        match size1 with
                        |A1(0) ->
                            p.codewrite("$"+name+"$: allocate($"+n1.code+"$)\\\\\n")
                        |_ -> 
                            p.codewrite("(Error:055-001 「"+name+"」は可変長1次元配列ではありません")
                    |HTML ->
                        match size1 with
                        |A1(0) ->
                            p.codewrite("\\("+name+"\\): allocate(\\("+n1.code+"\\))<br/>\n")
                        |_ -> 
                            p.codewrite("(Error:055-001 「"+name+"」は可変長1次元配列ではありません")
                    |Python ->
                        match size1 with
                        |A1(0) ->
                            this.size1 <== n1
                            match typ with
                            |Structure(sname)       -> p.codewrite(name+" = numpy.array(["+sname+"() for _ in range(int("+this.size1.code+"))], dtype=object)\n")
                            |It _ |It 1             -> p.codewrite(name+" = numpy.zeros("+this.size1.code+", dtype=int)\n")
                            |Zt                     -> p.codewrite(name+" = numpy.zeros("+this.size1.code+", dtype=numpy.complex128)\n")
                            |_                      -> p.codewrite(name+"="+"numpy.zeros("+this.size1.code+")\n")
                        |_ -> 
                            p.codewrite("(Error:055-001 「"+name+"」は可変長1次元配列ではありません")
                |_ -> ()
        member this.allocate(n1:int) = this.allocate(n1.I)
                
        ///<summary>配列のメモリ割り当て</summary>
        member this.deallocate() =
            if p.debugMode then
                match x with
                |Var1(_,name) ->
                    p.errorIDinc()
                    p.comment("***debug array1 deallocate check: "+p.errorID.ToString()+"*****************************")
                    br.branch <| fun b ->
                        b.IF (this.size1 .= -1) <| fun () ->
                            print.t ("ERROR"+p.errorID.ToString()+" cannot deallocate array "+name)
                    p.comment("****************************************************")
                |_ -> ()
            match x with
            |Var1(size1,name) ->
                match p.lang with
                |Fortran ->
                    match size1 with
                    |A1(0) ->
                        this.size1 <== -1
                        p.codewrite("deallocate("+name+")"+"\n")
                    |_ -> ()
                |C99 ->
                    match size1 with
                    |A1(0) ->
                        this.size1 <== -1
                        p.codewrite("free("+name+");"+"\n")
                    |_ -> ()
                |LaTeX ->
                    match size1 with
                    |A1(0) ->
                        p.codewrite("$"+name+"$: deallocate\\\\\n")
                    |_ -> ()
                |HTML ->
                    match size1 with
                    |A1(0) ->
                        p.codewrite("\\("+name+"\\): deallocate<br/>\n")
                    |_ -> ()
                |Python ->
                    match size1 with
                    |A1(0) ->
                        p.codewrite("del "+name+""+"\n")
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
                code(i)
                
        ///<summary>配列の全要素に対する処理</summary>
        member this.foreach_exit code =
            iter.num_exit this.size1 <| fun (ext,i) -> 
                code(ext,i)
                
        static member sizeMismatchError(x:base1,y:base1) =
            if p.debugMode then
                p.errorIDinc()
                p.comment("***debug array1 access check: "+p.errorID.ToString()+"*****************************")
                br.branch <| fun b ->
                    b.IF (x.size1 .=/ y.size1) <| fun () -> 
                        print.t ("ERROR"+p.errorID.ToString()+" array size (first index) mismatch")
                p.comment("****************************************************")
                
    ///<summary>数値型1次元配列</summary>
    type num1 (typ:Etype,x:Expr1) =
        inherit base1(typ,x)
        new (typ,size,name,para) =
            p.var.setVar(typ,size,name,para)
            num1(typ,Var1(size,name))
        member this.etype with get() = typ
        member this.Item with get(i:num0) = this.Idx1(i)
        member this.Item with get(i:int) = this.Idx1(i.I)
        member this.Item with get((a:num0,b:num0)) = num1(typ,this.Idx1(a,b))
        member this.Item with get((a:num0,b:int )) = num1(typ,this.Idx1(a,b) )
        member this.Item with get((a:int ,b:num0)) = num1(typ,this.Idx1(a,b))
        member this.Item with get((a:int ,b:int )) = num1(typ,this.Idx1(a,b) )
        
        //<summary>1次元配列生成(整数型)</summary> 
        ///<param name="s1">第1要素数</param>
        ///<param name="f">(i,j)要素に対する要素値</param>
        static member fiarray(s1:num0,f:num0->num0) = num1(It 4,Arx1(s1,f))
        
        //<summary>1次元配列生成(倍精度浮動小数点型)</summary> 
        ///<param name="s1">第1要素数</param>
        ///<param name="f">(i,j)要素に対する要素値</param>
        static member fdarray(s1:num0,f:num0->num0) = num1(Dt,Arx1(s1,f))
        
        //<summary>1次元配列生成(複素数型)</summary> 
        ///<param name="s1">第1要素数</param>
        ///<param name="f">(i,j)要素に対する要素値</param>
        static member fzarray(s1:num0,f:num0->num0) = num1(Zt,Arx1(s1,f))
        
        //<summary>1次元配列生成(整数型)</summary> 
        ///<param name="s1">第1要素数</param>
        ///<param name="f">(i,j)要素に対する要素値</param>
        static member fiarray(s1:int,f:num0->num0) = num1.fiarray(I s1,f)
        
        //<summary>1次元配列生成(倍精度浮動小数点型)</summary> 
        ///<param name="s1">第1要素数</param>
        ///<param name="f">(i,j)要素に対する要素値</param>
        static member fdarray(s1:int,f:num0->num0) = num1.fdarray(I s1,f)
        
        //<summary>1次元配列生成(複素数型)</summary> 
        ///<param name="s1">第1要素数</param>
        ///<param name="f">(i,j)要素に対する要素値</param>
        static member fzarray(s1:int,f:num0->num0) = num1.fzarray(I s1,f)
        
        //<summary>元の配列と同じサイズの配列生成</summary> 
        ///<param name="f">(i,j)要素に対する要素値</param>
        member this.farray(f:num0->num0) = num1(this.etype,Arx1(this.size1,f))
        
        //<summary>値を0で初期化</summary> 
        override this.clear() = 
            this <== Int_c 0
            
        ///<summary>配列サイズ変数をメモリ未割当て状態に初期化</summary>
        override this.sizeinit() = 
            this.size1 <== -1
            
        static member sizeMismatchError(x:num1,y:num1) =
            if p.debugMode then
                p.errorIDinc()
                p.comment("***debug array1 access check: "+p.errorID.ToString()+"*****************************")
                br.branch <| fun b ->
                    b.IF (x.size1 .=/ y.size1) <| fun () -> 
                        print.t ("ERROR"+p.errorID.ToString()+" operator '+' array size mismatch")
                p.comment("****************************************************")
                
        static member (+) (x:num1,y:num1) =
            num1.sizeMismatchError(x,y)
            num1(x.etype%%y.etype, Arx1(x.size1, fun i -> x[i]+y[i]))
        static member (+) (x:num0,y:num1) = num1(x.etype%%y.etype,Arx1(y.size1, fun (i:num0) -> x+y[i]))
        static member (+) (x:int,y:num1) = num1((It 4)%%y.etype,Arx1(y.size1, fun (i:num0) -> x+y[i]))
        static member (+) (x:double,y:num1) = num1(Dt%%y.etype,Arx1(y.size1, fun (i:num0) -> x+y[i]))
        static member (+) (x:num1,y:num0) = num1(x.etype%%y.etype,Arx1(x.size1, fun i -> x[i]+y))
        static member (+) (x:num1,y:int) = num1(x.etype%%(It 4),Arx1(x.size1, fun i -> x[i]+y))
        static member (+) (x:num1,y:double) = num1(x.etype%%Dt,Arx1(x.size1, fun i -> x[i]+y))
        
        static member (-) (x:num1,y:num1) =
            num1.sizeMismatchError(x,y)
            num1(x.etype%%y.etype,Arx1(x.size1, fun i -> x[i]-y[i]))
        static member (-) (x:num0,y:num1) = num1(x.etype%%y.etype,Arx1(y.size1, fun (i:num0) -> x-y[i]))
        static member (-) (x:int,y:num1) = num1((It 4)%%y.etype,Arx1(y.size1, fun (i:num0) -> x-y[i]))
        static member (-) (x:double,y:num1) = num1(Dt%%y.etype,Arx1(y.size1, fun (i:num0) -> x-y[i]))
        static member (-) (x:num1,y:num0) = num1(x.etype%%y.etype,Arx1(x.size1, fun i -> x[i]-y))
        static member (-) (x:num1,y:int) = num1(x.etype%%(It 4),Arx1(x.size1, fun i -> x[i]-y))
        static member (-) (x:num1,y:double) = num1(x.etype%%Dt,Arx1(x.size1, fun i -> x[i]-y))
        
        static member (*) (x:num1,y:num1) =
            num1.sizeMismatchError(x,y)
            num1(x.etype%%y.etype,Arx1(x.size1, fun i -> x[i]*y[i]))
        static member (*) (x:num0,y:num1) = num1(x.etype%%y.etype,Arx1(y.size1, fun (i:num0) -> x*y[i]))
        static member (*) (x:int,y:num1) = num1((It 4)%%y.etype,Arx1(y.size1, fun (i:num0) -> x*y[i]))
        static member (*) (x:double,y:num1) = num1(Dt%%y.etype,Arx1(y.size1, fun (i:num0) -> x*y[i]))
        static member (*) (x:num1,y:num0) = num1(x.etype%%y.etype,Arx1(x.size1, fun i -> x[i]*y))
        static member (*) (x:num1,y:int) = num1(x.etype%%(It 4),Arx1(x.size1, fun i -> x[i]*y))
        static member (*) (x:num1,y:double) = num1(x.etype%%Dt,Arx1(x.size1, fun i -> x[i]*y))
        
        static member (/) (x:num1,y:num1) =
            num1.sizeMismatchError(x,y)
            num1(x.etype%%y.etype,Arx1(x.size1, fun i -> x[i]/y[i]))
        static member (/) (x:num0,y:num1) = num1(x.etype%%y.etype,Arx1(y.size1, fun (i:num0) -> x/y[i]))
        static member (/) (x:int,y:num1) = num1((It 4)%%y.etype,Arx1(y.size1, fun (i:num0) -> x/y[i]))
        static member (/) (x:double,y:num1) = num1(Dt%%y.etype,Arx1(y.size1, fun (i:num0) -> x/y[i]))
        static member (/) (x:num1,y:num0) = num1(x.etype%%y.etype,Arx1(x.size1, fun i -> x[i]/y))
        static member (/) (x:num1,y:int) = num1(x.etype%%(It 4),Arx1(x.size1, fun i -> x[i]/y))
        static member (/) (x:num1,y:double) = num1(x.etype%%Dt,Arx1(x.size1, fun i -> x[i]/y))

        static member (./) (x:num1,y:num1) =
            num1.sizeMismatchError(x,y)
            Arx1(x.size1, fun i -> x[i]/y[i])
        static member (./) (x:num0,y:num1) = num1(x.etype%%y.etype,Arx1(y.size1, fun (i:num0) -> x./y[i]))
        static member (./) (x:int,y:num1) = num1((It 4)%%y.etype,Arx1(y.size1, fun (i:num0) -> x./y[i]))
        static member (./) (x:num1,y:num0) = num1(x.etype%%y.etype,Arx1(x.size1, fun i -> x[i]./y))
        static member (./) (x:num1,y:int) = num1(x.etype%%(It 4),Arx1(x.size1, fun i -> x[i]./y))
        
        static member (<==) (v1:num1,v2:num1) =
            if p.debugMode then
                p.errorIDinc()
                p.comment("***debug array1 access check: "+p.errorID.ToString()+"*****************************")
                br.branch <| fun b ->
                    b.IF (v1.size1 .=/ v2.size1) <| fun () -> 
                        print.t ("ERROR"+p.errorID.ToString()+" operator '<==' array size mismatch")
                p.comment("****************************************************")
            match v1.expr,v2.expr with
            |Var1(_,x),Var1(_,y) ->
                match p.lang with
                |Fortran|LaTeX ->
                    p.codewrite(x + "=" + y)
                |C99 ->
                    iter.num v1.size1 <| fun i -> v1[i] <== v2[i]
                |HTML ->
                    p.codewrite(x + " \\leftarrow " + y)
                |Python ->
                    p.codewrite(x + " = copy.deepcopy("+y+")")
            |Var1(_,x),Arx1(_,f) ->
                match p.lang with
                |Fortran|LaTeX|C99|HTML|Python -> iter.num v1.size1 <| fun i -> v1[i] <== v2[i]
            |Arx1(_,_),Var1(_,_) ->
                match p.lang with
                |Fortran|LaTeX|C99|HTML|Python -> iter.num v1.size1 <| fun i -> v1[i] <== v2[i]
            |Arx1(_,_),Arx1(_,_) ->
                match p.lang with
                |Fortran|LaTeX|C99|HTML|Python -> iter.num v1.size1 <| fun i -> v1[i] <== v2[i]

        static member (<==) (v1:num1,v2:num0) =
            match v1.expr with
            |Var1(_,x) ->
                match p.lang with
                |Fortran|LaTeX ->
                    p.codewrite(x + "=" + v2.code)
                |C99 ->
                    iter.num v1.size1 <| fun i -> v1[i] <== v2
                |HTML ->
                    p.codewrite(x + " \\leftarrow " + v2.code)
                |Python ->
                    match v1.etype with
                    |Structure(sname) -> p.codewrite(x+" = numpy.array(["+sname+"() for _ in range(int("+v1.size1.code+"))], dtype=object)\n")
                    |_                -> p.codewrite(x+"[:]="+v2.code+"\n")
            |Arx1(_,_) ->
                match p.lang with
                |Fortran|LaTeX|C99|HTML|Python -> iter.num v1.size1 <| fun i -> v1[i] <== v2
        static member (<==) (x:num1,y:double) = x <== y.D
        static member (<==) (x:num1,y:int) = x <== y.I
        
    [<AutoOpen>]
    module asm_num1 =
        type asm with
            static member pow(x:num1,y:num0) = num1(x.etype%%y.etype,Arx1(x.size1,fun i -> asm.pow(x[i],y)))
            static member pow(x:num1,y:int) = num1(x.etype%%(It 4),Arx1(x.size1,fun i -> asm.pow(x[i],y)))
            static member pow(x:num1,y:double) = num1(x.etype%%Dt,Arx1(x.size1,fun i -> asm.pow(x[i],y)))
            static member sin(x:num1) = num1(x.etype,Arx1(x.size1,fun i -> asm.sin(x[i])))
            static member cos(x:num1) = num1(x.etype,Arx1(x.size1,fun i -> asm.cos(x[i])))
            static member tan(x:num1) = num1(x.etype,Arx1(x.size1,fun i -> asm.tan(x[i])))
            static member asin(x:num1) = num1(x.etype,Arx1(x.size1,fun i -> asm.asin(x[i])))
            static member acos(x:num1) = num1(x.etype,Arx1(x.size1,fun i -> asm.acos(x[i])))
            static member atan(x:num1) = num1(x.etype,Arx1(x.size1,fun i -> asm.atan(x[i])))
            static member atan2(x:num1,y:num1) = num1(Dt,Arx1(x.size1,fun i -> asm.atan2(x[i],y[i])))
            static member exp(x:num1) = num1(x.etype,Arx1(x.size1,fun i -> asm.exp(x[i])))
            static member abs(x:num1) = num1(x.etype,Arx1(x.size1,fun i -> asm.abs(x[i])))
            static member log(x:num1) = num1(x.etype,Arx1(x.size1,fun i -> asm.log(x[i])))
            static member log10(x:num1) = num1(x.etype,Arx1(x.size1,fun i -> asm.log10(x[i])))
            static member sqrt(x:num1) = num1(x.etype,Arx1(x.size1,fun i -> asm.sqrt(x[i])))
            static member floor(x:num1) = num1(x.etype,Arx1(x.size1,fun i -> asm.floor(x[i])))
            static member ceil(x:num1) = num1(x.etype,Arx1(x.size1,fun i -> asm.ceil(x[i])))
            static member conj(x:num1) = num1(x.etype,Arx1(x.size1,fun i -> asm.conj(x[i])))