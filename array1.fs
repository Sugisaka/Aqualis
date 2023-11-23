(*
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
        |Arx1 of (num0*(num0->Expr))
        
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
                |F -> num0(Var(It 4,name+"_size(1)"))
                |C -> num0(Var(It 4,name+"_size[0]"))
                |T -> num0(Var(It 4,"\\mathcal{S}_1["+name+"]"))
                |H -> num0(Var(It 4,"\\mathcal{S}_1["+name+"]"))
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
                        b.IF (Or [(i .< _1); (this.size1 .< i)]) <| fun () ->
                            print.s [!.("ERROR"+p.errorID.ToString()+" array "+name+" illegal access. index ");i;!." is out of range (1:";this.size1;!.")"]
                    p.comment("****************************************************")
                |_ -> ()
            match x with
            |Var1(_,name) ->
                match p.lang with
                |F   -> Var(typ,name+"("+i.code+")")
                |C -> Var(typ,name+"["+(i-1).code+"]")
                |T   -> Var(typ,name+"("+i.code+")")
                |H   -> Var(typ,name+"["+i.code+"]")
            |Arx1(_,f) ->
                f i
        member this.Idx1(i:int) = this.Idx1(i.I)
        member this.Idx1(a:num0,b:num0) = Arx1(b-a+_1,fun i -> this.Idx1(i+a-1))
        member this.Idx1(a:num0,b:int)  = Arx1(b-a+_1,fun i -> this.Idx1(i+a-1))
        member this.Idx1(a:int ,b:num0) = Arx1(b-a+_1,fun i -> this.Idx1(i+a-1))
        member this.Idx1(a:int ,b:int)  = Arx1(b-a+_1,fun i -> this.Idx1(i+a-1))
        
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
                    |F ->
                        match size1 with
                        |A1(0) ->
                            this.size1 <== n1
                            p.codewrite("allocate("+name+"(1:"+this.size1.code+")"+")"+"\n")
                        |_ -> 
                            p.codewrite("(Error:055-001 「"+name+"」は可変長1次元配列ではありません")
                    |C ->
                        match size1 with
                        |A1(0) ->
                            this.size1 <== n1
                            p.codewrite(name+"="+"("+typ.tostring(p.lang)+" *)"+"malloc("+"sizeof("+typ.tostring(p.lang)+")*"+this.size1.code+");\n")
                        |_ -> 
                            p.codewrite("(Error:055-001 「"+name+"」は可変長1次元配列ではありません")
                    |T ->
                        match size1 with
                        |A1(0) ->
                            p.codewrite("$"+name+"$: allocate($"+n1.code+"$)\\\\\n")
                        |_ -> 
                            p.codewrite("(Error:055-001 「"+name+"」は可変長1次元配列ではありません")
                    |H ->
                        match size1 with
                        |A1(0) ->
                            p.codewrite("\\("+name+"\\): allocate(\\("+n1.code+"\\))<br/>\n")
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
                |F ->
                    match size1 with
                    |A1(0) ->
                        this.size1 <== -1
                        p.codewrite("deallocate("+name+")"+"\n")
                    |_ -> ()
                |C ->
                    match size1 with
                    |A1(0) ->
                        this.size1 <== -1
                        p.codewrite("free("+name+");"+"\n")
                    |_ -> ()
                |T ->
                    match size1 with
                    |A1(0) ->
                        p.codewrite("$"+name+"$: deallocate\\\\\n")
                    |_ -> ()
                |H ->
                    match size1 with
                    |A1(0) ->
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
                
        static member subst (v1:Expr1,s1:num0,f1:num0->Expr,v2:Expr1,s2:num0,f2:num0->Expr) =
            if p.debugMode then
                p.errorIDinc()
                p.comment("***debug array1 access check: "+p.errorID.ToString()+"*****************************")
                br.branch <| fun b ->
                    b.IF (s1 .=/ s2) <| fun () -> 
                        print.t ("ERROR"+p.errorID.ToString()+" operator '<==' array size mismatch")
                p.comment("****************************************************")
            match v1,v2 with
            |Var1(_,x),Var1(_,y) ->
                match p.lang with
                |F|T ->
                    p.codewrite(x + "=" + y)
                |C ->
                    iter.num s1 <| fun i -> f1 i <== f2 i
                |H   ->
                    p.codewrite("\\[")
                    p.codewrite("\\begin{align}")
                    p.codewrite(x + " \\leftarrow " + y)
                    p.codewrite("\\end{align}")
                    p.codewrite("\\]")
            |Var1(_,x),Arx1(_,f) ->
                match p.lang with
                |F|T|C|H -> iter.num s1 <| fun i -> f1 i <== f2 i
            |Arx1(_,_),Var1(_,_) ->
                match p.lang with
                |F|T|C|H -> iter.num s1 <| fun i -> f1 i <== f2 i
            |Arx1(_,_),Arx1(_,_) ->
                match p.lang with
                |F|T|C|H -> iter.num s1 <| fun i -> f1 i <== f2 i
        static member subst (v1:Expr1,s1:num0,f1:num0->Expr,v2:Expr) =
            match v1 with
            |Var1(_,x) ->
                match p.lang with
                |F|T ->
                    p.codewrite(x + "=" + v2.code)
                |C ->
                    iter.num s1 <| fun i -> f1 i <== v2
                |H ->
                    p.codewrite("\\[")
                    p.codewrite("\\begin{align}")
                    p.codewrite(x + " \\leftarrow " + v2.code)
                    p.codewrite("\\end{align}")
                    p.codewrite("\\]")
            |Arx1(_,_) ->
                match p.lang with
                |F|T|C|H -> iter.num s1 <| fun i -> f1 i <== v2
                
    ///<summary>数値型1次元配列</summary>
    type num1 (typ:Etype,x:Expr1) =
        inherit base1(typ,x)
        new (typ,size,name,para) =
            p.var.setVar(typ,size,name,para)
            num1(typ,Var1(size,name))
        member this.etype with get() = typ
        member this.Item with get(i:num0) = num0(this.Idx1(i))
        member this.Item with get(i:int) = num0(this.Idx1(i.I))
        member this.Item with get((a:num0,b:num0)) = num1(typ,this.Idx1(a,b))
        member this.Item with get((a:num0,b:int )) = num1(typ,this.Idx1(a,b) )
        member this.Item with get((a:int ,b:num0)) = num1(typ,this.Idx1(a,b))
        member this.Item with get((a:int ,b:int )) = num1(typ,this.Idx1(a,b) )

        //<summary>値を0で初期化</summary> 
        override this.clear() = 
            this <== num0(Int_c 0)
            
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
            num1(x.etype%%y.etype, Arx1(x.size1, fun i -> x[i].expr+y[i].expr))
        static member (+) (x:num0,y:num1) = num1(x.etype%%y.etype,Arx1(y.size1, fun (i:num0) -> x.expr+y[i].expr))
        static member (+) (x:num1,y:num0) = num1(x.etype%%y.etype,Arx1(x.size1, fun i -> x[i].expr+y.expr))
        
        static member (-) (x:num1,y:num1) =
            num1.sizeMismatchError(x,y)
            num1(x.etype%%y.etype,Arx1(x.size1, fun i -> x[i].expr-y[i].expr))
        static member (-) (x:num0,y:num1) = num1(x.etype%%y.etype,Arx1(y.size1, fun (i:num0) -> x.expr-y[i].expr))
        static member (-) (x:num1,y:num0) = num1(x.etype%%y.etype,Arx1(x.size1, fun i -> x[i].expr-y.expr))
        
        static member (*) (x:num1,y:num1) =
            num1.sizeMismatchError(x,y)
            num1(x.etype%%y.etype,Arx1(x.size1, fun i -> x[i].expr*y[i].expr))
        static member (*) (x:num0,y:num1) = num1(x.etype%%y.etype,Arx1(y.size1, fun (i:num0) -> x.expr*y[i].expr))
        static member (*) (x:num1,y:num0) = num1(x.etype%%y.etype,Arx1(x.size1, fun i -> x[i].expr*y.expr))
        
        static member (/) (x:num1,y:num1) =
            num1.sizeMismatchError(x,y)
            num1(x.etype%%y.etype,Arx1(x.size1, fun i -> x[i].expr/y[i].expr))
        static member (/) (x:num0,y:num1) = num1(x.etype%%y.etype,Arx1(y.size1, fun (i:num0) -> x.expr/y[i].expr))
        static member (/) (x:num1,y:num0) = num1(x.etype%%y.etype,Arx1(x.size1, fun i -> x[i].expr./y.expr))

        static member (./) (x:num1,y:num1) =
            num1.sizeMismatchError(x,y)
            Arx1(x.size1, fun i -> x[i].expr/y[i].expr)
        static member (./) (x:num0,y:num1) = num1(x.etype%%y.etype,Arx1(y.size1, fun (i:num0) -> x.expr./y[i].expr))
        static member (./) (x:num1,y:num0) = num1(x.etype%%y.etype,Arx1(x.size1, fun i -> x[i].expr./y.expr))
        
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
                |F|T ->
                    p.codewrite(x + "=" + y)
                |C ->
                    iter.num v1.size1 <| fun i -> v1[i] <== v2[i]
                |H ->
                    p.codewrite("\\[")
                    p.codewrite("\\begin{align}")
                    p.codewrite(x + " \\leftarrow " + y)
                    p.codewrite("\\end{align}")
                    p.codewrite("\\]")
            |Var1(_,x),Arx1(_,f) ->
                match p.lang with
                |F|T|C|H -> iter.num v1.size1 <| fun i -> v1[i] <== v2[i]
            |Arx1(_,_),Var1(_,_) ->
                match p.lang with
                |F|T|C|H -> iter.num v1.size1 <| fun i -> v1[i] <== v2[i]
            |Arx1(_,_),Arx1(_,_) ->
                match p.lang with
                |F|T|C|H -> iter.num v1.size1 <| fun i -> v1[i] <== v2[i]

        static member (<==) (v1:num1,v2:num0) =
            match v1.expr with
            |Var1(_,x) ->
                match p.lang with
                |F|T ->
                    p.codewrite(x + "=" + v2.code)
                |C ->
                    iter.num v1.size1 <| fun i -> v1[i] <== v2
                |H ->
                    p.codewrite("\\[")
                    p.codewrite("\\begin{align}")
                    p.codewrite(x + " \\leftarrow " + v2.code)
                    p.codewrite("\\end{align}")
                    p.codewrite("\\]")
            |Arx1(_,_) ->
                match p.lang with
                |F|T|C|H -> iter.num v1.size1 <| fun i -> v1[i] <== v2
        static member (<==) (x:num1,y:double) = x <== y.D
        static member (<==) (x:num1,y:int) = x <== y.I
        
    [<AutoOpen>]
    module asm_num1 =
        type asm with
            static member pow(x:num1,y:num0) = num1(x.etype%%y.etype,Arx1(x.size1,fun i -> asm.pow(x[i],y).expr))
            static member sin(x:num1) = num1(x.etype,Arx1(x.size1,fun i -> asm.sin(x[i]).expr))
            static member cos(x:num1) = num1(x.etype,Arx1(x.size1,fun i -> asm.cos(x[i]).expr))
            static member tan(x:num1) = num1(x.etype,Arx1(x.size1,fun i -> asm.tan(x[i]).expr))
            static member asin(x:num1) = num1(x.etype,Arx1(x.size1,fun i -> asm.asin(x[i]).expr))
            static member acos(x:num1) = num1(x.etype,Arx1(x.size1,fun i -> asm.acos(x[i]).expr))
            static member atan(x:num1) = num1(x.etype,Arx1(x.size1,fun i -> asm.atan(x[i]).expr))
            static member atan2(x:num1,y:num1) = num1(Dt,Arx1(x.size1,fun i -> asm.atan2(x[i],y[i]).expr))
            static member exp(x:num1) = num1(x.etype,Arx1(x.size1,fun i -> asm.exp(x[i]).expr))
            static member abs(x:num1) = num1(x.etype,Arx1(x.size1,fun i -> asm.abs(x[i]).expr))
            static member log(x:num1) = num1(x.etype,Arx1(x.size1,fun i -> asm.log(x[i]).expr))
            static member log10(x:num1) = num1(x.etype,Arx1(x.size1,fun i -> asm.log10(x[i]).expr))
            static member sqrt(x:num1) = num1(x.etype,Arx1(x.size1,fun i -> asm.sqrt(x[i]).expr))
            static member floor(x:num1) = num1(x.etype,Arx1(x.size1,fun i -> asm.floor(x[i]).expr))
            static member ceil(x:num1) = num1(x.etype,Arx1(x.size1,fun i -> asm.ceil(x[i]).expr))
            static member conj(x:num1) = num1(x.etype,Arx1(x.size1,fun i -> asm.conj(x[i]).expr))