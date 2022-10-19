(*
Copyright (c) 2022 Jun-ichiro Sugisaka

This software is released under the MIT License.
http://opensource.org/licenses/mit-license.php
*)
namespace Aqualis
    
    open System
    open System.IO
    open System.Text
    open Aqualis_base

    ///<summary>1次元配列</summary>
    type base1 (typ:Etype,size1:VarType,name:string) =
        ///<summary>変数を作成しリストに追加</summary>
        new (typ,size,name,para) = 
            p.param.vreg(typ,size,name,para)
            base1(typ,size,name)
        ///<summary>変数を作成しリストに追加</summary>
        new(sname,size,name) =
            p.param.vreg(Structure(sname),size,name,"")
            base1(Structure(sname),size,name)
        ///<summary>変数名</summary>
        member __.name 
          with get() =
            name
        ///<summary>変数の型</summary>
        member __.etype
          with get() =
            typ
        ///<summary>変数の次元</summary>
        member __.vtype
          with get() =
            size1
        ///<summary>変数の要素数</summary>
        member __.size1 
          with get() =
            let p = p.param
            match p.lang with 
              |F |T -> 
                Var(It 4,name+"_size(1)",[]) 
              |C89 |C99 -> 
                Var(It 4,name+"_size[0]",[])
              |H -> 
                Var(It 4,"<msub><mi mathvariant=\"script\">S</mi><mn>1</mn></msub><mo>&af;</mo><mo>[</mo><mi>"+name+"</mi><mo>]</mo>",[]) 
              |NL -> 
                NaN
        ///<summary>インデクサ</summary>
        member this.Idx1(i:num0) =
            let p = p.param
            if p.debugmode then
                p.error_code_counter_inc()
                p.comment("***debug array1 access check: "+p.error_code_counter.ToString()+"*****************************")
                br.branch <| fun b ->
                    b.IF (this.size1 .= -1) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" array "+name+" is not allocated")]
                    b.IF (Or[(i.<(I 1)); (this.size1.<i)]) <| fun () ->
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" array "+name+" illegal access. index ");i;Str_e(" is out of range (1:");this.size1;Str_e(")")]
                p.comment("****************************************************")
            match p.lang with
              |F   -> name+"("+i.name+")"
              |C89 -> name+"["+(i-1).name+"]"
              |C99 -> name+"["+(i-1).name+"]"
              |T   -> name+"("+i.name+")"
              |H   -> name+"<mo>&af;</mo><mo>[</mo>"+i.name+"<mo>]</mo>"
              |NL   -> ""
              
        ///<summary>インデクサ</summary>
        member this.Idx1(i:int ) = this.Idx1(I i)
        
        ///<summary>配列のメモリ割り当て</summary>
        member this.allocate(n1:num0) =
            tbinder.i ("allocate "+name) n1 <| fun () ->
                let p = p.param
                let alloc (n1:num0) =
                    if p.debugmode then
                        p.error_code_counter_inc()
                        p.comment("***debug array1 allocate check: "+p.error_code_counter.ToString()+"*****************************")
                        br.branch <| fun b ->
                            b.IF (this.size1 .=/ -1) <| fun () ->
                                print.s[Str_e("ERROR"+p.error_code_counter.ToString()+" array "+name+" is already allocated")]
                        p.comment("****************************************************")
                    match p.lang with
                      |F ->
                        match size1 with
                          |A1(0) ->
                            this.size1 <== n1
                            p.codewrite("allocate("+name+"(1:"+this.size1.name+")"+")"+"\n")
                          |_ -> 
                            p.codewrite("(Error:055-001 「"+name+"」は可変長1次元配列ではありません")
                      |C89 ->
                        match size1 with
                          |A1(0) ->
                            this.size1 <== n1
                            p.codewrite(name+"="+"("+typ.tostring()+" *)"+"malloc("+"sizeof("+typ.tostring()+")*"+this.size1.name+");\n")
                          |_ -> 
                            p.codewrite("(Error:055-001 「"+name+"」は可変長1次元配列ではありません")
                      |C99 ->
                        match size1 with
                          |A1(0) ->
                            this.size1 <== n1
                            p.codewrite(name+"="+"("+typ.tostring()+" *)"+"malloc("+"sizeof("+typ.tostring()+")*"+this.size1.name+");\n")
                          |_ -> 
                            p.codewrite("(Error:055-001 「"+name+"」は可変長1次元配列ではありません")
                      |T ->
                        match size1 with
                          |A1(0) ->
                            this.size1 <== n1
                            p.codewrite("allocate($"+name+"(1:"+this.size1.name+")"+"$)"+"\n")
                          |_ -> 
                            p.codewrite("(Error:055-001 「"+name+"」は可変長1次元配列ではありません")
                      |H ->
                        match size1 with
                          |A1(0) ->
                            p.codewrite("<math>"+name+"<mo>:</mo><mi>allocate</mi><mo>(</mo>"+n1.name+"<mo>)</mo></math>"+"\n<br/>\n")
                          |_ -> 
                            p.codewrite("(Error:055-001 「"+name+"」は可変長1次元配列ではありません")
                      |NL ->
                        ()
                match p.lang with
                  |H ->
                    alloc n1
                  |_ ->
                    match n1 with
                        |Int_e n1_ ->
                          if n1_>0 then
                              alloc n1
                          else
                              alloc _0
                        |_ ->
                          br.branch <| fun b ->
                            b.IF (n1.>0) <| fun () ->
                                alloc n1
                            b.EL <| fun () ->
                                alloc _0
        ///<summary>配列の全要素に対する処理</summary>
        member this.foreach code =
            iter.num this.size1 <| fun i -> 
                code(i)
        ///<summary>配列の全要素に対する処理</summary>
        member this.foreach_exit code =
            iter.num_exit this.size1 <| fun (ext,i) -> 
                code(ext,i)
        ///<summary>配列のメモリ割り当て</summary>
        member this.allocate(n1:int) =
            this.allocate(I n1)
        ///<summary>配列のメモリ割り当て</summary>
        member this.deallocate() =
            let p = p.param
            if p.debugmode then
                p.error_code_counter_inc()
                p.comment("***debug array1 deallocate check: "+p.error_code_counter.ToString()+"*****************************")
                br.branch <| fun b ->
                    b.IF (this.size1 .= -1) <| fun () ->
                        print.s[Str_e("ERROR"+p.error_code_counter.ToString()+" cannot deallocate array "+name)]
                p.comment("****************************************************")
            match p.lang with
              |F ->
                match size1 with
                  |A1(0) ->
                    this.size1 <== -1
                    p.codewrite("deallocate("+name+")"+"\n")
                  |_ -> ()
              |C89 ->
                match size1 with
                  |A1(0) ->
                    this.size1 <== -1
                    p.codewrite("free("+name+");"+"\n")
                  |_ -> ()
              |C99 ->
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
              |NL ->
                ()
        ///<summary>配列のクリア</summary>
        abstract member clear: unit -> unit
        default __.clear() = 
            printfn "WARNING: abstract clear method"
        ///<summary>配列サイズの初期化</summary>
        abstract member sizeinit: unit -> unit
        default __.sizeinit() = 
            printfn "WARNING: abstract sizeinit method"
            
    ///<summary>数値型1次元配列</summary>
    type num1 (typ:Etype,size:VarType,name:string) =
        inherit base1(typ,size,name)
        new (typ,size,name,para) =
            p.param.vreg(typ,size,name,para)
            num1(typ,size,name)
        member this.Item with get(i:num0) = Var(typ,this.Idx1(i),[])
        member this.Item with get(i:int) = Var(typ,this.Idx1(i),[])
        member this.Item with get((a:num0,b:num0)) = ax1(b-a+_1,fun i -> this[i+a-1])
        member this.Item with get((a:num0,b:int )) = ax1(b-a+_1,fun i -> this[i+a-1])
        member this.Item with get((a:int ,b:num0)) = ax1(b-a+_1,fun i -> this[i+a-1])
        member this.Item with get((a:int ,b:int)) = ax1(b-a+_1,fun i -> this[i+a-1])
        static member (<==) (v1:num1,v2:num1) =
            let p = p.param
            if p.debugmode then
                p.error_code_counter_inc()
                p.comment("***debug array1 access check: "+p.error_code_counter.ToString()+"*****************************")
                br.branch <| fun b ->
                    b.IF (v1.size1 .=/ v2.size1) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '<==' array size mismatch: "+v1.name+","+v2.name)]
                p.comment("****************************************************")
            match p.lang with
              |F|T ->
                let u1 = v1.name
                let u2 = v2.name
                p.codewrite(u1 + "=" + u2)
              |C89|C99 ->
                iter.num v1.size1 <| fun i ->
                    v1.[i] <== v2.[i]
              |H ->
                let u1 = v1.name
                let u2 = v2.name
                p.codewrite("<math>" + u1 + "<mo>&larr;</mo>" + u2 + "</math>\n<br/>\n")
              |NL ->
                ()
        static member (<==) (v1:num1,v2:num0) =
            let p = p.param
            match p.lang with
              |F|T ->
                let u1 = v1.name
                let (_,u2,_) = v2.code.str
                p.codewrite(u1 + "=" + u2)
              |C89|C99 ->
                iter.num v1.size1 <| fun i ->
                    v1.[i] <== v2
              |H ->
                let u1 = v1.name
                let (_,u2,_) = v2.code.str
                p.codewrite("<math>" + u1 + "<mo>&larr;</mo>" + u2 + "</math>\n<br/>\n")
              |NL ->
                ()
        static member (<==) (v1:num1,v2:int) =
            v1 <== (Int_e v2)
        static member (<==) (v1:num1,v2:double) =
            v1 <== (Dbl_e v2)
        //<summary>値を0で初期化</summary> 
        override this.clear() = 
            this <== 0
        ///<summary>配列サイズ変数をメモリ未割当て状態に初期化</summary>
        override this.sizeinit() = 
            this.size1 <== -1
        /// <summary>
        /// この変数を関数内変数に変換
        /// </summary>
        /// <param name="cm">コメント</param>
        member __.farg (cm:string) = fun code ->
            let p = p.param
            p.addarg (typ,size,name,cm) <| fun (t,v,n) -> code(num1(t,v,n))
        static member (+) (x:num1,y:num1) =
            let p = p.param
            if p.debugmode then
                p.error_code_counter_inc()
                p.comment("***debug array1 access check: "+p.error_code_counter.ToString()+"*****************************")
                br.branch <| fun b ->
                    b.IF (x.size1 .=/ y.size1) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '+' array size mismatch")]
                p.comment("****************************************************")
            ax1(x.size1, fun i -> x.[i]+y.[i])
        static member (+) (x:num0,y:num1) = ax1(y.size1, fun i -> x+y.[i])
        static member (+) (x:num1,y:num0) = ax1(x.size1, fun i -> x.[i]+y)
        static member (+) (x:int,y:num1) = (I x) + y
        static member (+) (x:num1,y:int) = x + (I y)
        static member (+) (x:double,y:num1) = (D x) + y
        static member (+) (x:num1,y:double) = x + (D y)
        static member (+) (x:double*double,y:num1) = x + y
        static member (+) (x:num1,y:double*double) = x + y
        
        static member (-) (x:num1,y:num1) =
            let p = p.param
            if p.debugmode then
                p.error_code_counter_inc()
                p.comment("***debug array1 access check: "+p.error_code_counter.ToString()+"*****************************")
                br.branch <| fun b ->
                    b.IF (x.size1 .=/ y.size1) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '-' array size mismatch")]
                p.comment("****************************************************")
            ax1(x.size1, fun i -> x.[i]-y.[i])
        static member (-) (x:num0,y:num1) = ax1(y.size1, fun i -> x-y.[i])
        static member (-) (x:num1,y:num0) = ax1(x.size1, fun i -> x.[i]-y)
        static member (-) (x:int,y:num1) = (I x) - y
        static member (-) (x:num1,y:int) = x - (I y)
        static member (-) (x:double,y:num1) = (D x) - y
        static member (-) (x:num1,y:double) = x - (D y)
        static member (-) (x:double*double,y:num1) = x - y
        static member (-) (x:num1,y:double*double) = x - y
        
        static member (*) (x:num1,y:num1) =
            let p = p.param
            if p.debugmode then
                p.error_code_counter_inc()
                p.comment("***debug array1 access check: "+p.error_code_counter.ToString()+"*****************************")
                br.branch <| fun b ->
                    b.IF (x.size1 .=/ y.size1) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '*' array size mismatch")]
                p.comment("****************************************************")
            ax1(x.size1, fun i -> x.[i]*y.[i])
        static member (*) (x:num0,y:num1) = ax1(y.size1, fun i -> x*y.[i])
        static member (*) (x:num1,y:num0) = ax1(x.size1, fun i -> x.[i]*y)
        static member (*) (x:int,y:num1) = (I x) * y
        static member (*) (x:num1,y:int) = x * (I y)
        static member (*) (x:double,y:num1) = (D x) * y
        static member (*) (x:num1,y:double) = x * (D y)
        static member (*) (x:double*double,y:num1) = x * y
        static member (*) (x:num1,y:double*double) = x * y
        
        static member (/) (x:num1,y:num1) =
            let p = p.param
            if p.debugmode then
                p.error_code_counter_inc()
                p.comment("***debug array1 access check: "+p.error_code_counter.ToString()+"*****************************")
                br.branch <| fun b ->
                    b.IF (x.size1 .=/ y.size1) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '/' array size mismatch")]
                p.comment("****************************************************")
            ax1(x.size1, fun i -> x.[i]/y.[i])
        static member (/) (x:num0,y:num1) = ax1(y.size1, fun i -> x/y.[i])
        static member (/) (x:num1,y:num0) = ax1(x.size1, fun i -> x.[i]/y)
        static member (/) (x:int,y:num1) = (I x) / y
        static member (/) (x:num1,y:int) = x / (I y)
        static member (/) (x:double,y:num1) = (D x) / y
        static member (/) (x:num1,y:double) = x / (D y)
        static member (/) (x:double*double,y:num1) = x / y
        static member (/) (x:num1,y:double*double) = x / y
            
    and ax1(n:num0,f:num0->num0) =
        member _.size1 = n
        member _.Item with get(i:num0) = f(i)
        member _.Item with get(i:int) = f(I i)
        static member (+) (x:ax1,y:ax1) =
            let p = p.param
            if p.debugmode then
                p.error_code_counter_inc()
                p.comment("***debug array1 access check: "+p.error_code_counter.ToString()+"*****************************")
                br.branch <| fun b ->
                    b.IF (x.size1 .=/ y.size1) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '+' array size mismatch")]
                p.comment("****************************************************")
            ax1(x.size1,fun i ->x.[i]+y.[i])
        static member (+) (x:ax1,y:num1) =
            let p = p.param
            if p.debugmode then
                p.error_code_counter_inc()
                p.comment("***debug array1 access check: "+p.error_code_counter.ToString()+"*****************************")
                br.branch <| fun b ->
                    b.IF (x.size1 .=/ y.size1) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '+' array size mismatch")]
                p.comment("****************************************************")
            ax1(x.size1,fun i ->x.[i]+y.[i])
        static member (+) (x:num1,y:ax1) =
            let p = p.param
            if p.debugmode then
                p.error_code_counter_inc()
                p.comment("***debug array1 access check: "+p.error_code_counter.ToString()+"*****************************")
                br.branch <| fun b ->
                    b.IF (x.size1 .=/ y.size1) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '+' array size mismatch")]
                p.comment("****************************************************")
            ax1(x.size1,fun i ->x.[i]+y.[i])
        static member (+) (x:ax1,y:int) =
            ax1(x.size1,fun i ->x.[i]+y)
        static member (+) (x:ax1,y:double) =
            ax1(x.size1,fun i ->x.[i]+y)
        static member (+) (x:ax1,y:double*double) =
            ax1(x.size1,fun i ->x.[i]+y)
        static member (+) (x:int,y:ax1) =
            ax1(y.size1,fun i ->x+y.[i])
        static member (+) (x:double,y:ax1) =
            ax1(y.size1,fun i ->x+y.[i])
        static member (+) (x:double*double,y:ax1) =
            ax1(y.size1,fun i ->x+y.[i])
        static member (-) (x:ax1,y:ax1) =
            let p = p.param
            if p.debugmode then
                p.error_code_counter_inc()
                p.comment("***debug array1 access check: "+p.error_code_counter.ToString()+"*****************************")
                br.branch <| fun b ->
                    b.IF (x.size1 .=/ y.size1) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '-' array size mismatch")]
                p.comment("****************************************************")
            ax1(x.size1,fun i ->x.[i]-y.[i])
        static member (-) (x:ax1,y:num1) =
            let p = p.param
            if p.debugmode then
                p.error_code_counter_inc()
                p.comment("***debug array1 access check: "+p.error_code_counter.ToString()+"*****************************")
                br.branch <| fun b ->
                    b.IF (x.size1 .=/ y.size1) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '-' array size mismatch")]
                p.comment("****************************************************")
            ax1(x.size1,fun i ->x.[i]-y.[i])
        static member (-) (x:num1,y:ax1) =
            let p = p.param
            if p.debugmode then
                p.error_code_counter_inc()
                p.comment("***debug array1 access check: "+p.error_code_counter.ToString()+"*****************************")
                br.branch <| fun b ->
                    b.IF (x.size1 .=/ y.size1) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '-' array size mismatch")]
                p.comment("****************************************************")
            ax1(x.size1,fun i ->x.[i]-y.[i])
        static member (-) (x:ax1,y:int) =
            ax1(x.size1,fun i ->x.[i]-y)
        static member (-) (x:ax1,y:double) =
            ax1(x.size1,fun i ->x.[i]-y)
        static member (-) (x:ax1,y:double*double) =
            ax1(x.size1,fun i ->x.[i]-y)
        static member (-) (x:int,y:ax1) =
            ax1(y.size1,fun i ->x-y.[i])
        static member (-) (x:double,y:ax1) =
            ax1(y.size1,fun i ->x-y.[i])
        static member (-) (x:double*double,y:ax1) =
            ax1(y.size1,fun i ->x-y.[i])
        static member (*) (x:ax1,y:ax1) =
            let p = p.param
            if p.debugmode then
                p.error_code_counter_inc()
                p.comment("***debug array1 access check: "+p.error_code_counter.ToString()+"*****************************")
                br.branch <| fun b ->
                    b.IF (x.size1 .=/ y.size1) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '*' array size mismatch")]
                p.comment("****************************************************")
            ax1(x.size1,fun i ->x.[i]*y.[i])
        static member (*) (x:ax1,y:num1) =
            let p = p.param
            if p.debugmode then
                p.error_code_counter_inc()
                p.comment("***debug array1 access check: "+p.error_code_counter.ToString()+"*****************************")
                br.branch <| fun b ->
                    b.IF (x.size1 .=/ y.size1) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '*' array size mismatch")]
                p.comment("****************************************************")
            ax1(x.size1,fun i ->x.[i]*y.[i])
        static member (*) (x:num1,y:ax1) =
            let p = p.param
            if p.debugmode then
                p.error_code_counter_inc()
                p.comment("***debug array1 access check: "+p.error_code_counter.ToString()+"*****************************")
                br.branch <| fun b ->
                    b.IF (x.size1 .=/ y.size1) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '*' array size mismatch")]
                p.comment("****************************************************")
            ax1(x.size1,fun i ->x.[i]*y.[i])
        static member (*) (x:ax1,y:int) =
            ax1(x.size1,fun i ->x.[i]*y)
        static member (*) (x:ax1,y:double) =
            ax1(x.size1,fun i ->x.[i]*y)
        static member (*) (x:ax1,y:double*double) =
            ax1(x.size1,fun i ->x.[i]*y)
        static member (*) (x:int,y:ax1) =
            ax1(y.size1,fun i ->x*y.[i])
        static member (*) (x:double,y:ax1) =
            ax1(y.size1,fun i ->x*y.[i])
        static member (*) (x:double*double,y:ax1) =
            ax1(y.size1,fun i ->x*y.[i])
        static member (/) (x:ax1,y:ax1) =
            let p = p.param
            if p.debugmode then
                p.error_code_counter_inc()
                p.comment("***debug array1 access check: "+p.error_code_counter.ToString()+"*****************************")
                br.branch <| fun b ->
                    b.IF (x.size1 .=/ y.size1) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '/' array size mismatch")]
                p.comment("****************************************************")
            ax1(x.size1,fun i ->x.[i]/y.[i])
        static member (/) (x:ax1,y:num1) =
            let p = p.param
            if p.debugmode then
                p.error_code_counter_inc()
                p.comment("***debug array1 access check: "+p.error_code_counter.ToString()+"*****************************")
                br.branch <| fun b ->
                    b.IF (x.size1 .=/ y.size1) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '/' array size mismatch")]
                p.comment("****************************************************")
            ax1(x.size1,fun i ->x.[i]/y.[i])
        static member (/) (x:num1,y:ax1) =
            let p = p.param
            if p.debugmode then
                p.error_code_counter_inc()
                p.comment("***debug array1 access check: "+p.error_code_counter.ToString()+"*****************************")
                br.branch <| fun b ->
                    b.IF (x.size1 .=/ y.size1) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '/' array size mismatch")]
                p.comment("****************************************************")
            ax1(x.size1,fun i ->x.[i]/y.[i])
        static member (/) (x:ax1,y:int) =
            ax1(x.size1,fun i ->x.[i]/y)
        static member (/) (x:ax1,y:double) =
            ax1(x.size1,fun i ->x.[i]/y)
        static member (/) (x:ax1,y:double*double) =
            ax1(x.size1,fun i ->x.[i]/y)
        static member (/) (x:int,y:ax1) =
            ax1(y.size1,fun i ->x/y.[i])
        static member (/) (x:double,y:ax1) =
            ax1(y.size1,fun i ->x/y.[i])
        static member (/) (x:double*double,y:ax1) =
            ax1(y.size1,fun i ->x/y.[i])
            
        static member (<==) (x:num1,y:ax1) =
            let p = p.param
            if p.debugmode then
                p.error_code_counter_inc()
                p.comment("***debug array1 access check: "+p.error_code_counter.ToString()+"*****************************")
                br.branch <| fun b ->
                    b.IF (x.size1 .=/ y.size1) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '<==' array size mismatch")]
                p.comment("****************************************************")
            iter.num x.size1 <| fun i ->
                x.[i] <== y.[i]
        static member (<==) (x:ax1,y:num1) =
            let p = p.param
            if p.debugmode then
                p.error_code_counter_inc()
                p.comment("***debug array1 access check: "+p.error_code_counter.ToString()+"*****************************")
                br.branch <| fun b ->
                    b.IF (x.size1 .=/ y.size1) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '<==' array size mismatch")]
                p.comment("****************************************************")
            iter.num x.size1 <| fun i ->
                x.[i] <== y.[i]
        static member (<==) (x:ax1,y:ax1) =
            let p = p.param
            if p.debugmode then
                p.error_code_counter_inc()
                p.comment("***debug array1 access check: "+p.error_code_counter.ToString()+"*****************************")
                br.branch <| fun b ->
                    b.IF (x.size1 .=/ y.size1) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '<==' array size mismatch")]
                p.comment("****************************************************")
            iter.num x.size1 <| fun i ->
                x.[i] <== y.[i]
        member this.abs
            with get() =
                ax1(this.size1,fun i ->this.[i].abs)
        member this.pow
            with get() =
                ax1(this.size1,fun i ->this.[i].pow)
        member this.pha
            with get() =
                ax1(this.size1,fun i ->this.[i].pha)

    [<AutoOpen>]
    module asm_ax1 =
        type asm with
            static member pow(x:ax1,y:num0) =
                ax1(x.size1,fun i -> asm.pow(x.[i],y))
            static member pow(x:ax1,y:int) =
                ax1(x.size1,fun i -> asm.pow(x.[i],y))
            static member pow(x:ax1,y:double) =
                ax1(x.size1,fun i -> asm.pow(x.[i],y))
            static member pow(x:ax1,y:double*double) =
                ax1(x.size1,fun i -> asm.pow(x.[i],y))
            static member sin(x:ax1) =
                ax1(x.size1,fun i -> asm.sin(x.[i]))
            static member cos(x:ax1) =
                ax1(x.size1,fun i -> asm.cos(x.[i]))
            static member tan(x:ax1) =
                ax1(x.size1,fun i -> asm.tan(x.[i]))
            static member asin(x:ax1) =
                ax1(x.size1,fun i -> asm.asin(x.[i]))
            static member acos(x:ax1) =
                ax1(x.size1,fun i -> asm.acos(x.[i]))
            static member atan(x:ax1) =
                ax1(x.size1,fun i -> asm.atan(x.[i]))
            static member atan2(x:ax1,y:ax1) =
                ax1(x.size1,fun i -> asm.atan2(x.[i],y.[i]))
            static member exp(x:ax1) =
                ax1(x.size1,fun i -> asm.exp(x.[i]))
            static member abs(x:ax1) =
                ax1(x.size1,fun i -> asm.abs(x.[i]))
            static member log(x:ax1) =
                ax1(x.size1,fun i -> asm.log(x.[i]))
            static member log10(x:ax1) =
                ax1(x.size1,fun i -> asm.log10(x.[i]))
            static member sqrt(x:ax1) =
                ax1(x.size1,fun i -> asm.sqrt(x.[i]))
            static member floor(x:ax1) =
                ax1(x.size1,fun i -> asm.floor(x.[i]))
            static member ceil(x:ax1) =
                ax1(x.size1,fun i -> asm.ceil(x.[i]))
            static member toint(x:ax1) =
                ax1(x.size1,fun i -> asm.toint(x.[i]))
            static member todouble(x:ax1) =
                ax1(x.size1,fun i -> asm.todouble(x.[i]))
            static member conj(x:ax1) =
                ax1(x.size1,fun i -> asm.conj(x.[i]))
            static member pow(x:num1,y:num0) =
                ax1(x.size1,fun i -> asm.pow(x.[i],y))
            static member pow(x:num1,y:int) =
                ax1(x.size1,fun i -> asm.pow(x.[i],y))
            static member pow(x:num1,y:double) =
                ax1(x.size1,fun i -> asm.pow(x.[i],y))
            static member pow(x:num1,y:double*double) =
                ax1(x.size1,fun i -> asm.pow(x.[i],y))
            static member sin(x:num1) =
                ax1(x.size1,fun i -> asm.sin(x.[i]))
            static member cos(x:num1) =
                ax1(x.size1,fun i -> asm.cos(x.[i]))
            static member tan(x:num1) =
                ax1(x.size1,fun i -> asm.tan(x.[i]))
            static member asin(x:num1) =
                ax1(x.size1,fun i -> asm.asin(x.[i]))
            static member acos(x:num1) =
                ax1(x.size1,fun i -> asm.acos(x.[i]))
            static member atan(x:num1) =
                ax1(x.size1,fun i -> asm.atan(x.[i]))
            static member atan2(x:num1,y:ax1) =
                ax1(x.size1,fun i -> asm.atan2(x.[i],y.[i]))
            static member exp(x:num1) =
                ax1(x.size1,fun i -> asm.exp(x.[i]))
            static member abs(x:num1) =
                ax1(x.size1,fun i -> asm.abs(x.[i]))
            static member log(x:num1) =
                ax1(x.size1,fun i -> asm.log(x.[i]))
            static member log10(x:num1) =
                ax1(x.size1,fun i -> asm.log10(x.[i]))
            static member sqrt(x:num1) =
                ax1(x.size1,fun i -> asm.sqrt(x.[i]))
            static member floor(x:num1) =
                ax1(x.size1,fun i -> asm.floor(x.[i]))
            static member ceil(x:num1) =
                ax1(x.size1,fun i -> asm.ceil(x.[i]))
            static member toint(x:num1) =
                ax1(x.size1,fun i -> asm.toint(x.[i]))
            static member todouble(x:num1) =
                ax1(x.size1,fun i -> asm.todouble(x.[i]))
            static member conj(x:num1) =
                ax1(x.size1,fun i -> asm.conj(x.[i]))
                
    [<AutoOpen>]
    module num1_ax1 =
        type num1 with
            member this.abs
                with get() =
                    ax1(this.size1,fun i -> this.[i].abs)
            member this.pow
                with get() =
                    ax1(this.size1,fun i -> this.[i].pow)
            member this.pha
                with get() =
                    ax1(this.size1,fun i -> this.[i].pha)
            
    [<AutoOpen>]
    module tbinder_num1 =
        ///<summary>数値型の型に従って処理を分岐</summary>
        type tbinder with
            /// <summary>
            /// 指定された式または変数eqが整数型の場合のみcodeを実行
            /// </summary>
            /// <param name="eq"></param>
            static member i(eq:num1) = fun code ->
                match eq.etype with
                  |It _ -> code()
                  |_ -> ()
            /// <summary>
            /// 指定された式または変数eqが実数型の場合のみcodeを実行
            /// </summary>
            /// <param name="eq"></param>
            static member d(eq:num1) = fun code ->
                match eq.etype with
                  |Dt -> code()
                  |_ -> ()
            /// <summary>
            /// 指定された式または変数eqが複素数型の場合のみcodeを実行
            /// </summary>
            /// <param name="eq"></param>
            static member z(eq:num1) = fun code ->
                match eq.etype with
                  |Zt -> code()
                  |_ -> ()
                  