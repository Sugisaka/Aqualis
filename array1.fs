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
        |Arx1 of (int0*(int0->Expr))
        
    ///<summary>1次元配列</summary>
    type base1 (x:Expr1) =
        ///<summary>変数を作成しリストに追加</summary>
        new (typ,size,name,para) = 
            p.param.var.setVar(typ,size,name,para)
            base1(Var1(size,name))
        ///<summary>変数を作成しリストに追加</summary>
        new(sname,size,name) =
            p.param.var.setVar(Structure(sname),size,name,"")
            base1(Var1(size,name))
        member _.expr with get() = x
        member _.code with get() =
            match x with
            |Var1(_,x) -> x
            |_ -> "Error(property .code)"
        ///<summary>変数の要素数</summary>
        member __.size1 
          with get() =
            let p = p.param
            match x with
            |Var1(_,name) -> 
                match p.lang with 
                |F |T -> int0(Var(name+"_size(1)"))
                |C -> int0(Var(name+"_size[0]"))
                |H -> int0(Var("<msub><mi mathvariant=\"script\">S</mi><mn>1</mn></msub><mo>&af;</mo><mo>[</mo><mi>"+name+"</mi><mo>]</mo>"))
            |Arx1(s,_) -> s
        ///<summary>インデクサ</summary>
        member this.Idx1(i:int0) =
            let p = p.param
            if p.debugmode then
                match x with
                |Var1(_,name) ->
                    p.error_code_counter_inc()
                    p.comment("***debug array1 access check: "+p.error_code_counter.ToString()+"*****************************")
                    br.branch <| fun b ->
                        b.IF (this.size1 =. -1) <| fun () -> 
                            print.c ("ERROR"+p.error_code_counter.ToString()+" array "+name+" is not allocated")
                        b.IF (OR [(i <. _1); (this.size1 <. i)]) <| fun () ->
                            print.c ("ERROR"+p.error_code_counter.ToString()+" array "+name+" illegal access. index ")
                            print.c i
                            print.c " is out of range (1:"
                            print.c this.size1
                            print.c ")"
                            print.br
                    p.comment("****************************************************")
                |_ -> ()
            match x with
            |Var1(_,name) ->
                match p.lang with
                |F   -> Var(name+"("+i.code+")")
                |C -> Var(name+"["+(i-1).code+"]")
                |T   -> Var(name+"("+i.code+")")
                |H   -> Var(name+"<mo>&af;</mo><mo>[</mo>"+i.code+"<mo>]</mo>")
            |Arx1(_,f) ->
                f i                
        member this.Idx1(i:int) = this.Idx1(i.I)
        member this.Idx1(a:int0,b:int0) = Arx1(b-a+_1,fun i -> this.Idx1(i+a-1))
        member this.Idx1(a:int0,b:int)  = Arx1(b-a+_1,fun i -> this.Idx1(i+a-1))
        member this.Idx1(a:int ,b:int0) = Arx1(b-a+_1,fun i -> this.Idx1(i+a-1))
        member this.Idx1(a:int ,b:int)  = Arx1(b-a+_1,fun i -> this.Idx1(i+a-1))
        
        ///<summary>配列のメモリ割り当て</summary>
        member this.allocate(typ:Etype,n1:int0) =
                let p = p.param
                match x with
                |Var1(size1,name) ->
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
                            this.size1 <== n1
                            p.codewrite("allocate($"+name+"(1:"+this.size1.code+")"+"$)"+"\n")
                        |_ -> 
                            p.codewrite("(Error:055-001 「"+name+"」は可変長1次元配列ではありません")
                    |H ->
                        match size1 with
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
                |Var1(_,name) ->
                    p.error_code_counter_inc()
                    p.comment("***debug array1 deallocate check: "+p.error_code_counter.ToString()+"*****************************")
                    br.branch <| fun b ->
                        b.IF (this.size1 =. -1) <| fun () ->
                            print.c ("ERROR"+p.error_code_counter.ToString()+" cannot deallocate array "+name)
                            print.br
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
                        this.size1 <== -1
                        p.codewrite("deallocate($"+name+"$)"+"\n")
                    |_ -> ()
                |H ->
                    match size1 with
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
                code(i)
                
        ///<summary>配列の全要素に対する処理</summary>
        member this.foreach_exit code =
            iter.num_exit this.size1 <| fun (ext,i) -> 
                code(ext,i)
                
        static member sizeMismatchError(x:base1,y:base1) =
            let p = p.param
            if p.debugmode then
                p.error_code_counter_inc()
                p.comment("***debug array1 access check: "+p.error_code_counter.ToString()+"*****************************")
                br.branch <| fun b ->
                    b.IF (x.size1 =/. y.size1) <| fun () -> 
                        print.c ("ERROR"+p.error_code_counter.ToString()+" array size (first index) mismatch")
                        print.br
                p.comment("****************************************************")
                
        static member subst (v1:Expr1,s1:int0,f1:int0->Expr,v2:Expr1,s2:int0,f2:int0->Expr) =
            let p = p.param
            if p.debugmode then
                p.error_code_counter_inc()
                p.comment("***debug array1 access check: "+p.error_code_counter.ToString()+"*****************************")
                br.branch <| fun b ->
                    b.IF (s1 =/. s2) <| fun () -> 
                        print.c ("ERROR"+p.error_code_counter.ToString()+" operator '<==' array size mismatch")
                        print.br
                p.comment("****************************************************")
            match v1,v2 with
            |Var1(_,x),Var1(_,y) ->
                match p.lang with
                |F|T -> p.codewrite(x + "=" + y)
                |C -> iter.num s1 <| fun i -> f1 i <== f2 i
                |H   -> p.codewrite("<math>" + x + "<mo>&larr;</mo>" + y + "</math>\n<br/>\n")
            |Var1(_,x),Arx1(_,f) ->
                match p.lang with
                |F|T|C|H -> iter.num s1 <| fun i -> f1 i <== f2 i
            |Arx1(_,_),Var1(_,_) ->
                match p.lang with
                |F|T|C|H -> iter.num s1 <| fun i -> f1 i <== f2 i
            |Arx1(_,_),Arx1(_,_) ->
                match p.lang with
                |F|T|C|H -> iter.num s1 <| fun i -> f1 i <== f2 i
        static member subst (v1:Expr1,s1:int0,f1:int0->Expr,v2:Expr) =
            let p = p.param
            match v1 with
            |Var1(_,x) ->
                match p.lang with
                |F|T -> p.codewrite(x + "=" + v2.code)
                |C -> iter.num s1 <| fun i -> f1 i <== v2
                |H   -> p.codewrite("<math>" + x + "<mo>&larr;</mo>" + v2.code + "</math>\n<br/>\n")
            |Arx1(_,_) ->
                match p.lang with
                |F|T|C|H -> iter.num s1 <| fun i -> f1 i <== v2
                
    ///<summary>数値型1次元配列</summary>
    type int1 (x:Expr1) =
        inherit base1(x)
        new (size,name,para) =
            p.param.var.setVar(It 4,size,name,para)
            int1(Var1(size,name))
        member this.allocate(n1:int0) = this.allocate(It 4,n1)
        member this.allocate(n1:int) = this.allocate(n1.I)
        member this.Item with get(i:int0) = int0(this.Idx1(i))
        
        static member ( + ) (x:int1,y:int1) =
            base1.sizeMismatchError(x,y)
            int1(Arx1(x.size1, fun i -> x[i].expr+y[i].expr))
        static member ( + ) (x:int1,y:int0) =
            int1(Arx1(x.size1, fun i -> x[i].expr+y.expr))
        static member ( + ) (x:int0,y:int1) =
            int1(Arx1(y.size1, fun i -> x.expr+y[i].expr))
        static member ( + ) (x:int,y:int1) = x.I + y
        static member ( + ) (x:int1,y:int) = x + y.I
        
        static member ( - ) (x:int1,y:int1) =
            base1.sizeMismatchError(x,y)
            int1(Arx1(x.size1, fun i -> x[i].expr-y[i].expr))
        static member ( - ) (x:int1,y:int0) =
            int1(Arx1(x.size1, fun i -> x[i].expr-y.expr))
        static member ( - ) (x:int0,y:int1) =
            int1(Arx1(y.size1, fun i -> x.expr-y[i].expr))
        static member ( - ) (x:int,y:int1) = x.I - y
        static member ( - ) (x:int1,y:int) = x - y.I
        
        static member ( * ) (x:int1,y:int1) =
            base1.sizeMismatchError(x,y)
            int1(Arx1(x.size1, fun i -> x[i].expr*y[i].expr))
        static member ( * ) (x:int1,y:int0) =
            int1(Arx1(x.size1, fun i -> x[i].expr*y.expr))
        static member ( * ) (x:int0,y:int1) =
            int1(Arx1(y.size1, fun i -> x.expr*y[i].expr))
        static member ( * ) (x:int,y:int1) = x.I * y
        static member ( * ) (x:int1,y:int) = x * y.I
        
        static member ( / ) (x:int1,y:int1) =
            base1.sizeMismatchError(x,y)
            float1(Arx1(x.size1, fun i -> x[i].expr/y[i].expr))
        static member ( / ) (x:int1,y:int0) =
            float1(Arx1(x.size1, fun i -> x[i].expr/y.expr))
        static member ( / ) (x:int0,y:int1) =
            float1(Arx1(y.size1, fun i -> x.expr/y[i].expr))
        static member ( / ) (x:int,y:int1) = x.I / y
        static member ( / ) (x:int1,y:int) = x / y.I
        
        static member ( /. ) (x:int1,y:int1) =
            base1.sizeMismatchError(x,y)
            int1(Arx1(x.size1, fun i -> x[i].expr/.y[i].expr))
        static member ( /. ) (x:int1,y:int0) =
            int1(Arx1(x.size1, fun i -> x[i].expr/.y.expr))
        static member ( /. ) (x:int0,y:int1) =
            int1(Arx1(y.size1, fun i -> x.expr/.y[i].expr))
        static member ( /. ) (x:int,y:int1) = x.I /. y
        static member ( /. ) (x:int1,y:int) = x /. y.I
            
        member this.Item with get(i:int) = int0(this.Idx1(i))
        member this.Item with get((a:int0,b:int0)) = int1(this.Idx1(a,b))
        member this.Item with get((a:int0,b:int )) = int1(this.Idx1(a,b))
        member this.Item with get((a:int ,b:int0)) = int1(this.Idx1(a,b))
        member this.Item with get((a:int ,b:int)) = int1(this.Idx1(a,b))
        
        static member (<==) (v1:int1,v2:int1) = base1.subst(v1.expr,v1.size1,(fun i -> v1[i].expr),v2.expr,v2.size1,(fun i -> v2[i].expr))
        static member (<==) (v1:int1,v2:int0) = base1.subst(v1.expr,v1.size1,(fun i -> v1[i].expr),v2.expr)
        static member (<==) (v1:int1,v2:int) = v1 <== v2.I
                
    ///<summary>数値型1次元配列</summary>
    and float1 (x:Expr1) =
        inherit base1(x)
        new (size,name,para) =
            p.param.var.setVar(It 4,size,name,para)
            float1(Var1(size,name))
        member this.allocate(n1:int0) = this.allocate(Dt,n1)
        member this.allocate(n1:int) = this.allocate(n1.I)
        member this.Item with get(i:int0) = float0(this.Idx1(i))
        
        static member ( + ) (x:float1,y:float1) =
            base1.sizeMismatchError(x,y)
            float1(Arx1(x.size1, fun i -> x[i].expr+y[i].expr))
        static member ( + ) (x:float1,y:int1) =
            base1.sizeMismatchError(x,y)
            float1(Arx1(x.size1, fun i -> x[i].expr+y[i].expr))
        static member ( + ) (x:int1,y:float1) =
            base1.sizeMismatchError(x,y)
            float1(Arx1(x.size1, fun i -> x[i].expr+y[i].expr))
        static member ( + ) (x:float1,y:float0) =
            float1(Arx1(x.size1, fun i -> x[i].expr+y.expr))
        static member ( + ) (x:float1,y:int0) =
            float1(Arx1(x.size1, fun i -> x[i].expr+y.expr))
        static member ( + ) (x:float0,y:float1) =
            float1(Arx1(y.size1, fun i -> x.expr+y[i].expr))
        static member ( + ) (x:int0,y:float1) =
            float1(Arx1(y.size1, fun i -> x.expr+y[i].expr))
        static member ( + ) (x:float1,y:double) =
            x+y.D
        static member ( + ) (x:float1,y:int) =
            x+y.I
        static member ( + ) (x:double,y:float1) =
            x.D+y
        static member ( + ) (x:int,y:float1) =
            x.I+y
            
        static member ( - ) (x:float1,y:float1) =
            base1.sizeMismatchError(x,y)
            float1(Arx1(x.size1, fun i -> x[i].expr-y[i].expr))
        static member ( - ) (x:float1,y:int1) =
            base1.sizeMismatchError(x,y)
            float1(Arx1(x.size1, fun i -> x[i].expr-y[i].expr))
        static member ( - ) (x:int1,y:float1) =
            base1.sizeMismatchError(x,y)
            float1(Arx1(x.size1, fun i -> x[i].expr-y[i].expr))
        static member ( - ) (x:float1,y:float0) =
            float1(Arx1(x.size1, fun i -> x[i].expr-y.expr))
        static member ( - ) (x:float1,y:int0) =
            float1(Arx1(x.size1, fun i -> x[i].expr-y.expr))
        static member ( - ) (x:float0,y:float1) =
            float1(Arx1(y.size1, fun i -> x.expr-y[i].expr))
        static member ( - ) (x:int0,y:float1) =
            float1(Arx1(y.size1, fun i -> x.expr-y[i].expr))
        static member ( - ) (x:float1,y:double) =
            x-y.D
        static member ( - ) (x:float1,y:int) =
            x-y.I
        static member ( - ) (x:double,y:float1) =
            x.D-y
        static member ( - ) (x:int,y:float1) =
            x.I-y
        
        static member ( * ) (x:float1,y:float1) =
            base1.sizeMismatchError(x,y)
            float1(Arx1(x.size1, fun i -> x[i].expr*y[i].expr))
        static member ( * ) (x:float1,y:int1) =
            base1.sizeMismatchError(x,y)
            float1(Arx1(x.size1, fun i -> x[i].expr*y[i].expr))
        static member ( * ) (x:int1,y:float1) =
            base1.sizeMismatchError(x,y)
            float1(Arx1(x.size1, fun i -> x[i].expr*y[i].expr))
        static member ( * ) (x:float1,y:float0) =
            float1(Arx1(x.size1, fun i -> x[i].expr*y.expr))
        static member ( * ) (x:float1,y:int0) =
            float1(Arx1(x.size1, fun i -> x[i].expr*y.expr))
        static member ( * ) (x:float0,y:float1) =
            float1(Arx1(y.size1, fun i -> x.expr*y[i].expr))
        static member ( * ) (x:int0,y:float1) =
            float1(Arx1(y.size1, fun i -> x.expr*y[i].expr))
        static member ( * ) (x:float1,y:double) =
            x*y.D
        static member ( * ) (x:float1,y:int) =
            x*y.I
        static member ( * ) (x:double,y:float1) =
            x.D*y
        static member ( * ) (x:int,y:float1) =
            x.I*y
        
        static member ( / ) (x:float1,y:float1) =
            base1.sizeMismatchError(x,y)
            float1(Arx1(x.size1, fun i -> x[i].expr/y[i].expr))
        static member ( / ) (x:float1,y:int1) =
            base1.sizeMismatchError(x,y)
            float1(Arx1(x.size1, fun i -> x[i].expr/y[i].expr))
        static member ( / ) (x:int1,y:float1) =
            base1.sizeMismatchError(x,y)
            float1(Arx1(x.size1, fun i -> x[i].expr/y[i].expr))
        static member ( / ) (x:float1,y:float0) =
            float1(Arx1(x.size1, fun i -> x[i].expr/y.expr))
        static member ( / ) (x:float1,y:int0) =
            float1(Arx1(x.size1, fun i -> x[i].expr/y.expr))
        static member ( / ) (x:float0,y:float1) =
            float1(Arx1(y.size1, fun i -> x.expr/y[i].expr))
        static member ( / ) (x:int0,y:float1) =
            float1(Arx1(y.size1, fun i -> x.expr/y[i].expr))
        static member ( / ) (x:float1,y:double) =
            x/y.D
        static member ( / ) (x:float1,y:int) =
            x/y.I
        static member ( / ) (x:double,y:float1) =
            x.D/y
        static member ( / ) (x:int,y:float1) =
            x.I/y
        
        member this.Item with get(i:int) = float0(this.Idx1(i))
        member this.Item with get((a:int0,b:int0)) = float1(this.Idx1(a,b))
        member this.Item with get((a:int0,b:int )) = float1(this.Idx1(a,b))
        member this.Item with get((a:int ,b:int0)) = float1(this.Idx1(a,b))
        member this.Item with get((a:int ,b:int)) = float1(this.Idx1(a,b))
        
        static member (<==) (v1:float1,v2:float1) = int1.subst(v1.expr,v1.size1,(fun i -> v1[i].expr),v2.expr,v2.size1,(fun i -> v2[i].expr))
        static member (<==) (v1:float1,v2:int1) = int1.subst(v1.expr,v1.size1,(fun i -> v1[i].expr),v2.expr,v2.size1,(fun i -> v2[i].expr))
        static member (<==) (v1:float1,v2:float0) = int1.subst(v1.expr,v1.size1,(fun i -> v1[i].expr),v2.expr)
        static member (<==) (v1:float1,v2:int0) = int1.subst(v1.expr,v1.size1,(fun i -> v1[i].expr),v2.expr)
        static member (<==) (x:float1,y:double) = x <== y.D
        static member (<==) (x:float1,y:int) = x <== y.I
                
    ///<summary>数値型1次元配列</summary>
    type complex1 (x:Expr1) =
        inherit base1(x)
        new (size,name,para) =
            p.param.var.setVar(It 4,size,name,para)
            complex1(Var1(size,name))
        member this.Item with get(i:int0) = complex0(this.Idx1(i))
        member this.Item with get(i:int) = complex0(this.Idx1(i))
        member this.Item with get((a:int0,b:int0)) = complex1(this.Idx1(a,b))
        member this.Item with get((a:int0,b:int )) = complex1(this.Idx1(a,b))
        member this.Item with get((a:int ,b:int0)) = complex1(this.Idx1(a,b))
        member this.Item with get((a:int ,b:int)) = complex1(this.Idx1(a,b))
        
        member this.abs with get() = float1(Arx1(this.size1,fun i -> this[i].abs.expr))
        member this.pow with get() = float1(Arx1(this.size1,fun i -> this[i].pow.expr))
        member this.pha with get() = float1(Arx1(this.size1,fun i -> this[i].pha.expr))
        member this.allocate(n1:int0) = this.allocate(Zt,n1)
        member this.allocate(n1:int) = this.allocate(n1.I)
        
        static member ( + ) (x:complex1,y:complex1) =
            base1.sizeMismatchError(x,y)
            float1(Arx1(x.size1, fun i -> x[i].expr+y[i].expr))
        static member ( + ) (x:complex1,y:float1) =
            base1.sizeMismatchError(x,y)
            float1(Arx1(x.size1, fun i -> x[i].expr+y[i].expr))
        static member ( + ) (x:complex1,y:int1) =
            base1.sizeMismatchError(x,y)
            float1(Arx1(x.size1, fun i -> x[i].expr+y[i].expr))
        static member ( + ) (x:float1,y:complex1) =
            base1.sizeMismatchError(x,y)
            float1(Arx1(x.size1, fun i -> x[i].expr+y[i].expr))
        static member ( + ) (x:int1,y:complex1) =
            base1.sizeMismatchError(x,y)
            float1(Arx1(x.size1, fun i -> x[i].expr+y[i].expr))
        static member ( + ) (x:complex1,y:complex0) =
            float1(Arx1(x.size1, fun i -> x[i].expr+y.expr))
        static member ( + ) (x:complex1,y:float0) =
            float1(Arx1(x.size1, fun i -> x[i].expr+y.expr))
        static member ( + ) (x:complex1,y:int0) =
            float1(Arx1(x.size1, fun i -> x[i].expr+y.expr))
        static member ( + ) (x:complex0,y:complex1) =
            float1(Arx1(y.size1, fun i -> x.expr+y[i].expr))
        static member ( + ) (x:float0,y:complex1) =
            float1(Arx1(y.size1, fun i -> x.expr+y[i].expr))
        static member ( + ) (x:int0,y:complex1) =
            float1(Arx1(y.size1, fun i -> x.expr+y[i].expr))
        static member ( + ) (x:complex1,y:double) = x + y.D
        static member ( + ) (x:complex1,y:int) = x + y.I
        static member ( + ) (x:double,y:complex1) = x.D + y
        static member ( + ) (x:int,y:complex1) = x.I + y
        
        static member ( - ) (x:complex1,y:complex1) =
            base1.sizeMismatchError(x,y)
            float1(Arx1(x.size1, fun i -> x[i].expr-y[i].expr))
        static member ( - ) (x:complex1,y:float1) =
            base1.sizeMismatchError(x,y)
            float1(Arx1(x.size1, fun i -> x[i].expr-y[i].expr))
        static member ( - ) (x:complex1,y:int1) =
            base1.sizeMismatchError(x,y)
            float1(Arx1(x.size1, fun i -> x[i].expr-y[i].expr))
        static member ( - ) (x:float1,y:complex1) =
            base1.sizeMismatchError(x,y)
            float1(Arx1(x.size1, fun i -> x[i].expr-y[i].expr))
        static member ( - ) (x:int1,y:complex1) =
            base1.sizeMismatchError(x,y)
            float1(Arx1(x.size1, fun i -> x[i].expr-y[i].expr))
        static member ( - ) (x:complex1,y:complex0) =
            float1(Arx1(x.size1, fun i -> x[i].expr-y.expr))
        static member ( - ) (x:complex1,y:float0) =
            float1(Arx1(x.size1, fun i -> x[i].expr-y.expr))
        static member ( - ) (x:complex1,y:int0) =
            float1(Arx1(x.size1, fun i -> x[i].expr-y.expr))
        static member ( - ) (x:complex0,y:complex1) =
            float1(Arx1(y.size1, fun i -> x.expr-y[i].expr))
        static member ( - ) (x:float0,y:complex1) =
            float1(Arx1(y.size1, fun i -> x.expr-y[i].expr))
        static member ( - ) (x:int0,y:complex1) =
            float1(Arx1(y.size1, fun i -> x.expr-y[i].expr))
        static member ( - ) (x:complex1,y:double) = x - y.D
        static member ( - ) (x:complex1,y:int) = x - y.I
        static member ( - ) (x:double,y:complex1) = x.D - y
        static member ( - ) (x:int,y:complex1) = x.I - y
        
        static member ( * ) (x:complex1,y:complex1) =
            base1.sizeMismatchError(x,y)
            float1(Arx1(x.size1, fun i -> x[i].expr*y[i].expr))
        static member ( * ) (x:complex1,y:float1) =
            base1.sizeMismatchError(x,y)
            float1(Arx1(x.size1, fun i -> x[i].expr*y[i].expr))
        static member ( * ) (x:complex1,y:int1) =
            base1.sizeMismatchError(x,y)
            float1(Arx1(x.size1, fun i -> x[i].expr*y[i].expr))
        static member ( * ) (x:float1,y:complex1) =
            base1.sizeMismatchError(x,y)
            float1(Arx1(x.size1, fun i -> x[i].expr*y[i].expr))
        static member ( * ) (x:int1,y:complex1) =
            base1.sizeMismatchError(x,y)
            float1(Arx1(x.size1, fun i -> x[i].expr*y[i].expr))
        static member ( * ) (x:complex1,y:complex0) =
            float1(Arx1(x.size1, fun i -> x[i].expr*y.expr))
        static member ( * ) (x:complex1,y:float0) =
            float1(Arx1(x.size1, fun i -> x[i].expr*y.expr))
        static member ( * ) (x:complex1,y:int0) =
            float1(Arx1(x.size1, fun i -> x[i].expr*y.expr))
        static member ( * ) (x:complex0,y:complex1) =
            float1(Arx1(y.size1, fun i -> x.expr*y[i].expr))
        static member ( * ) (x:float0,y:complex1) =
            float1(Arx1(y.size1, fun i -> x.expr*y[i].expr))
        static member ( * ) (x:int0,y:complex1) =
            float1(Arx1(y.size1, fun i -> x.expr*y[i].expr))
        static member ( * ) (x:complex1,y:double) = x * y.D
        static member ( * ) (x:complex1,y:int) = x * y.I
        static member ( * ) (x:double,y:complex1) = x.D * y
        static member ( * ) (x:int,y:complex1) = x.I * y
        
        static member ( / ) (x:complex1,y:complex1) =
            base1.sizeMismatchError(x,y)
            float1(Arx1(x.size1, fun i -> x[i].expr/y[i].expr))
        static member ( / ) (x:complex1,y:float1) =
            base1.sizeMismatchError(x,y)
            float1(Arx1(x.size1, fun i -> x[i].expr/y[i].expr))
        static member ( / ) (x:complex1,y:int1) =
            base1.sizeMismatchError(x,y)
            float1(Arx1(x.size1, fun i -> x[i].expr/y[i].expr))
        static member ( / ) (x:float1,y:complex1) =
            base1.sizeMismatchError(x,y)
            float1(Arx1(x.size1, fun i -> x[i].expr/y[i].expr))
        static member ( / ) (x:int1,y:complex1) =
            base1.sizeMismatchError(x,y)
            float1(Arx1(x.size1, fun i -> x[i].expr/y[i].expr))
        static member ( / ) (x:complex1,y:complex0) =
            float1(Arx1(x.size1, fun i -> x[i].expr/y.expr))
        static member ( / ) (x:complex1,y:float0) =
            float1(Arx1(x.size1, fun i -> x[i].expr/y.expr))
        static member ( / ) (x:complex1,y:int0) =
            float1(Arx1(x.size1, fun i -> x[i].expr/y.expr))
        static member ( / ) (x:complex0,y:complex1) =
            float1(Arx1(y.size1, fun i -> x.expr/y[i].expr))
        static member ( / ) (x:float0,y:complex1) =
            float1(Arx1(y.size1, fun i -> x.expr/y[i].expr))
        static member ( / ) (x:int0,y:complex1) =
            float1(Arx1(y.size1, fun i -> x.expr/y[i].expr))
        static member ( / ) (x:complex1,y:double) = x / y.D
        static member ( / ) (x:complex1,y:int) = x / y.I
        static member ( / ) (x:double,y:complex1) = x.D / y
        static member ( / ) (x:int,y:complex1) = x.I / y
        
        static member (<==) (v1:complex1,v2:complex1) = int1.subst(v1.expr,v1.size1,(fun i -> v1[i].expr),v2.expr,v2.size1,(fun i -> v2[i].expr))
        static member (<==) (v1:complex1,v2:float1) = int1.subst(v1.expr,v1.size1,(fun i -> v1[i].expr),v2.expr,v2.size1,(fun i -> v2[i].expr))
        static member (<==) (v1:complex1,v2:int1) = int1.subst(v1.expr,v1.size1,(fun i -> v1[i].expr),v2.expr,v2.size1,(fun i -> v2[i].expr))
        static member (<==) (v1:complex1,v2:complex0) = int1.subst(v1.expr,v1.size1,(fun i -> v1[i].expr),v2.expr)
        static member (<==) (v1:complex1,v2:float0) = int1.subst(v1.expr,v1.size1,(fun i -> v1[i].expr),v2.expr)
        static member (<==) (v1:complex1,v2:int0) = int1.subst(v1.expr,v1.size1,(fun i -> v1[i].expr),v2.expr)
        static member (<==) (x:complex1,y:double) = x <== y.D
        static member (<==) (x:complex1,y:int) = x <== y.I
                
    ///<summary>数値型1次元配列</summary>
    type num1 (e:Etype,x:Expr1) =
        inherit base1(x)
        new (typ,size,name,para) =
            p.param.var.setVar(typ,size,name,para)
            num1(typ,Var1(size,name))
        member this.etype with get() = e
        member this.Item with get(i:int0) = num0(e,this.Idx1(i))
        member this.Item with get(i:int) = num0(e,this.Idx1(i.I))
        
        //<summary>値を0で初期化</summary> 
        override this.clear() = 
            this <== num0(It 4,Int_c 0)
            
        ///<summary>配列サイズ変数をメモリ未割当て状態に初期化</summary>
        override this.sizeinit() = 
            this.size1 <== -1
            
        static member sizeMismatchError(x:num1,y:num1) =
            let p = p.param
            if p.debugmode then
                p.error_code_counter_inc()
                p.comment("***debug array1 access check: "+p.error_code_counter.ToString()+"*****************************")
                br.branch <| fun b ->
                    b.IF (x.size1 =/. y.size1) <| fun () -> 
                        print.c ("ERROR"+p.error_code_counter.ToString()+" operator '+' array size mismatch")
                        print.br
                p.comment("****************************************************")
                
        member this.allocate(n1:int0) = this.allocate(e,n1)
        member this.allocate(n1:int) = this.allocate(n1.I)
        
        static member (+) (x:num1,y:num1) =
            num1.sizeMismatchError(x,y)
            Arx1(x.size1, fun i -> x[i].expr+y[i].expr)
        static member (+) (x:num0,y:num1) = Arx1(y.size1, fun (i:int0) -> x.expr+y[i].expr)
        static member (+) (x:num1,y:num0) = Arx1(x.size1, fun i -> x[i].expr+y.expr)
        
        static member (-) (x:num1,y:num1) =
            num1.sizeMismatchError(x,y)
            Arx1(x.size1, fun i -> x[i].expr-y[i].expr)
        static member (-) (x:num0,y:num1) = Arx1(y.size1, fun (i:int0) -> x.expr-y[i].expr)
        static member (-) (x:num1,y:num0) = Arx1(x.size1, fun i -> x[i].expr-y.expr)
        
        static member (*) (x:num1,y:num1) =
            num1.sizeMismatchError(x,y)
            Arx1(x.size1, fun i -> x[i].expr*y[i].expr)
        static member (*) (x:num0,y:num1) = Arx1(y.size1, fun (i:int0) -> x.expr*y[i].expr)
        static member (*) (x:num1,y:num0) = Arx1(x.size1, fun i -> x[i].expr*y.expr)
        
        static member (/) (x:num1,y:num1) =
            num1.sizeMismatchError(x,y)
            Arx1(x.size1, fun i -> x[i].expr/y[i].expr)
        static member (/) (x:num0,y:num1) = Arx1(y.size1, fun (i:int0) -> x.expr/y[i].expr)
        static member (/) (x:num1,y:num0) = Arx1(x.size1, fun i -> x[i].expr/.y.expr)

        static member (/.) (x:num1,y:num1) =
            num1.sizeMismatchError(x,y)
            Arx1(x.size1, fun i -> x[i].expr/y[i].expr)
        static member (/.) (x:num0,y:num1) = Arx1(y.size1, fun (i:int0) -> x.expr/.y[i].expr)
        static member (/.) (x:num1,y:num0) = Arx1(x.size1, fun i -> x[i].expr/.y.expr)
        
        static member (<==) (v1:num1,v2:complex1) = int1.subst(v1.expr,v1.size1,(fun i -> v1[i].expr),v2.expr,v2.size1,(fun i -> v2[i].expr))
        static member (<==) (v1:num1,v2:float1) = int1.subst(v1.expr,v1.size1,(fun i -> v1[i].expr),v2.expr,v2.size1,(fun i -> v2[i].expr))
        static member (<==) (v1:num1,v2:int1) = int1.subst(v1.expr,v1.size1,(fun i -> v1[i].expr),v2.expr,v2.size1,(fun i -> v2[i].expr))
        static member (<==) (v1:num1,v2:num1) = int1.subst(v1.expr,v1.size1,(fun i -> v1[i].expr),v2.expr,v2.size1,(fun i -> v2[i].expr))
        static member (<==) (v1:num1,v2:complex0) = int1.subst(v1.expr,v1.size1,(fun i -> v1[i].expr),v2.expr)
        static member (<==) (v1:num1,v2:float0) = int1.subst(v1.expr,v1.size1,(fun i -> v1[i].expr),v2.expr)
        static member (<==) (v1:num1,v2:int0) = int1.subst(v1.expr,v1.size1,(fun i -> v1[i].expr),v2.expr)
        static member (<==) (v1:num1,v2:num0) = int1.subst(v1.expr,v1.size1,(fun i -> v1[i].expr),v2.expr)
        static member (<==) (x:num1,y:double) = x <== y.D
        static member (<==) (x:num1,y:int) = x <== y.I
        
    [<AutoOpen>]
    module asm_num1 =
        type asm with
            static member pow(x:num1,y:num0) = num1(Etype.prior(x.etype,y.etype),Arx1(x.size1,fun i -> asm.pow(x[i],y).expr))
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