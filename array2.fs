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

    ///<summary>2次元配列</summary>
    type base2 (typ:Etype,size1:VarType,name:string) =
        new (typ,size,name,para) = 
            p.param.vreg(typ,size,name,para)
            base2(typ,size,name)
        new(sname,size,name) =
            p.param.vreg(Structure(sname),size,name,"")
            base2(Structure(sname),size,name)
        member __.name 
          with get() =
            name
        member __.etype
          with get() =
            typ
        member __.vtype
          with get() =
            size1
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
        member __.size2
          with get() =
            let p = p.param
            match p.lang with 
              |F |T -> 
                Var(It 4,name+"_size(2)",[]) 
              |C89 |C99 -> 
                Var(It 4,name+"_size[1]",[])
              |H -> 
                Var(It 4,"<msub><mi mathvariant=\"script\">S</mi><mn>2</mn></msub><mo>&af;</mo><mo>[</mo><mi>"+name+"</mi><mo>]</mo>",[]) 
              |NL ->
                NaN
        member this.Idx2(i:num0,j:num0) =
            let p = p.param
            if p.debugmode then
                p.error_code_counter_inc()
                p.comment("***debug error check: "+p.error_code_counter.ToString()+"*****************************")
                br.branch <| fun b ->
                    b.IF <| (this.size1 .= -1) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" array "+name+" is not allocated")]
                    b.IF <| Or[i.< 1; this.size1.<i] <| fun () ->
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" array "+name+" illegal access. 1st index ");i;Str_e(" is out of range (1:");this.size1;Str_e(")")]
                    b.IF <| Or[j.< 1; this.size2.<j] <| fun () ->
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" array "+name+" illegal access. 2nd index ");j;Str_e(" is out of range (1:");this.size2;Str_e(")")]
                p.comment("****************************************************")
            match p.lang with
              |F   -> name+"("+i.name+","+j.name+")"
              |C89 -> name+"["+((j-1)*this.size1+(i-1)).name+"]"
              |C99 -> name+"["+((j-1)*this.size1+(i-1)).name+"]"
              |T   -> name+"("+i.name+","+j.name+")"
              |H   -> name+"<mo>&af;</mo><mo>[</mo>"+i.name+j.name+"<mo>]</mo>"
              |NL  -> ""
        member this.Idx2(i:int ,j:int ) = this.Idx2(I i,I j)
        member this.Idx2(i:num0,j:int ) = this.Idx2(i,I j)
        member this.Idx2(i:int ,j:num0) = this.Idx2(I i,j)
        member this.foreach code =
            iter.num this.size1 <| fun i -> 
                iter.num this.size2 <| fun j -> 
                    code(i,j)
        member this.foreach1 code =
            iter.num this.size1 <| fun i -> 
                code(i)
        member this.foreach2 code =
            iter.num this.size2 <| fun j -> 
                code(j)
        member this.foreach_exit code =
            iter.num_exit this.size1 <| fun (ext,i) -> 
                iter.num this.size2 <| fun j -> 
                    code(ext,i,j)
        member this.foreach1_exit code =
            iter.num_exit this.size1 <| fun (ext,i) -> 
                code(ext,i)
        member this.foreach2_exit code =
            iter.num_exit this.size2 <| fun (ext,j) -> 
                code(ext,j)
        member this.allocate(n1:num0,n2:num0) =
            tbinder.i ("allocate "+name) n1 <| fun () ->
            tbinder.i ("allocate "+name) n2 <| fun () ->
                let p = p.param
                let alloc (n1:num0) (n2:num0) =
                    if p.debugmode then
                        p.error_code_counter_inc()
                        p.comment("***debug array2 allocate check: "+p.error_code_counter.ToString()+"*****************************")
                        br.branch <| fun b ->
                            b.IF (this.size1 .=/ I(-1)) <| fun () ->
                                print.s[Str_e("ERROR"+p.error_code_counter.ToString()+" array "+name+" is already allocated")]
                        p.comment("****************************************************")
                    match p.lang with
                      |F ->
                        match size1 with
                          |A2(0,0) ->
                            this.size1 <== n1
                            this.size2 <== n2
                            p.codewrite("allocate("+name+"(1:"+this.size1.name+",1:"+this.size2.name+")"+")"+"\n")
                          |_ -> 
                            p.codewrite("(Error:055-001 「"+name+"」は可変長1次元配列ではありません")
                      |C89 ->
                        match size1 with
                          |A2(0,0) ->
                            this.size1 <== n1
                            this.size2 <== n2
                            p.codewrite(name+"="+"("+typ.tostring()+" *)"+"malloc("+"sizeof("+typ.tostring()+")*"+this.size1.name+"*"+this.size2.name+");\n")
                          |_ -> 
                            p.codewrite("(Error:055-001 「"+name+"」は可変長1次元配列ではありません")
                      |C99 ->
                        match size1 with
                          |A2(0,0) ->
                            this.size1 <== n1
                            this.size2 <== n2
                            p.codewrite(name+"="+"("+typ.tostring()+" *)"+"malloc("+"sizeof("+typ.tostring()+")*"+this.size1.name+"*"+this.size2.name+");\n")
                          |_ -> 
                            p.codewrite("(Error:055-001 「"+name+"」は可変長1次元配列ではありません")
                      |T ->
                        match size1 with
                          |A2(0,0) ->
                            this.size1 <== n1
                            this.size2 <== n2
                            p.codewrite("allocate($"+name+"(1:"+this.size1.name+",1:"+this.size2.name+")"+"$)"+"\n")
                          |_ -> 
                            p.codewrite("(Error:055-001 「"+name+"」は可変長1次元配列ではありません")
                      |H ->
                        match size1 with
                          |A2(0,0) ->
                            p.codewrite("<math>"+name+"<mo>:</mo><mi>allocate</mi><mo>(</mo>"+n1.name+"<mo>,</mo>"+n2.name+"<mo>)</mo></math>"+"\n<br/>\n")
                          |_ -> 
                            p.codewrite("(Error:055-001 「"+name+"」は可変長1次元配列ではありません")
                      |NL ->
                        ()
                        
                match p.lang with
                  |H ->
                    alloc n1 n2
                  |_ ->
                    match (n1,n2) with
                        |(Int_e n1_,Int_e n2_) ->
                            if n1_>0 && n2_>0 then
                                alloc n1 n2
                            else
                                alloc _0 _0
                        |_ ->
                          br.branch <| fun b ->
                            b.IF (And[n1.>I 0; n2.>I 0]) <| fun () ->
                                alloc n1 n2
                            b.EL <| fun () ->
                                alloc _0 _0
        member this.allocate(n1:num0,n2:int) =
            this.allocate(n1,I n2)
        member this.allocate(n1:int,n2:num0) =
            this.allocate(I n1,n2)
        member this.allocate(n1:int,n2:int) =
            this.allocate(I n1,I n2)
            
        member this.deallocate() =
            let p = p.param
            if p.debugmode then
                p.error_code_counter_inc()
                p.comment("***debug array2 deallocate check: "+p.error_code_counter.ToString()+"*****************************")
                br.branch <| fun b ->
                    b.IF (this.size1 .= -1) <| fun () ->
                        print.s[Str_e("ERROR"+p.error_code_counter.ToString()+" cannot deallocate array "+name)]
                p.comment("****************************************************")
            match p.lang with
              |F ->
                match size1 with
                  |A2(0,0) ->
                    this.size1 <== -1
                    this.size2 <== -1
                    p.codewrite("deallocate("+name+")"+"\n")
                  |_ -> ()
              |C89 ->
                match size1 with
                  |A2(0,0) ->
                    this.size1 <== -1
                    this.size2 <== -1
                    p.codewrite("free("+name+");"+"\n")
                  |_ -> ()
              |C99 ->
                match size1 with
                  |A2(0,0) ->
                    this.size1 <== -1
                    this.size2 <== -1
                    p.codewrite("free("+name+");"+"\n")
                  |_ -> ()
              |T ->
                match size1 with
                  |A2(0,0) ->
                    this.size1 <== -1
                    this.size2 <== -1
                    p.codewrite("deallocate($"+name+"$)"+"\n")
                  |_ -> ()
              |H ->
                match size1 with
                  |A2(0,0) ->
                    p.codewrite("<math>"+name+"<mo>:</mo><mi>deallocate</mi></math>"+"\n<br/>\n")
                  |_ -> ()
              |NL ->
                ()
        abstract member clear: unit -> unit
        default __.clear() = 
            printfn "WARNING: abstract clear method"
        abstract member sizeinit: unit -> unit
        default __.sizeinit() = 
            printfn "WARNING: abstract sizeinit method"
                  
    ///<summary>数値型2次元配列</summary>
    type num2 (typ:Etype,size:VarType,name:string) =
        inherit base2(typ,size,name)
        new (typ,size,name,para) =
            p.param.vreg(typ,size,name,para)
            num2(typ,size,name)
        member this.Item with get(i:num0,j:num0) = Var(typ,this.Idx2(i,j),[])
        member this.Item with get(i:int,j:num0) = Var(typ,this.Idx2(i,j),[])
        member this.Item with get(i:num0,j:int) = Var(typ,this.Idx2(i,j),[])
        member this.Item with get(i:int,j:int) = Var(typ,this.Idx2(i,j),[])
        member this.Item with get(_:unit,j:num0) = ax1(this.size1,fun i -> this.[i,j])
        member this.Item with get(_:unit,j:int) = ax1(this.size1,fun i -> this.[i,j])
        member this.Item with get(i:num0,_:unit) = ax1(this.size2,fun j -> this.[i,j])
        member this.Item with get(i:int,_:unit) = ax1(this.size2,fun j -> this.[i,j])
        member this.Item with get((a:num0,b:num0),j:num0) = ax1(b-a+_1,fun i -> this.[i+a-1,j])
        member this.Item with get((a:num0,b:int ),j:num0) = ax1(b-a+_1,fun i -> this.[i+a-1,j])
        member this.Item with get((a:int ,b:num0),j:num0) = ax1(b-a+_1,fun i -> this.[i+a-1,j])
        member this.Item with get((a:int ,b:int ),j:num0) = ax1(b-a+_1,fun i -> this.[i+a-1,j])
        member this.Item with get((a:num0,b:num0),j:int ) = ax1(b-a+_1,fun i -> this.[i+a-1,j])
        member this.Item with get((a:num0,b:int ),j:int ) = ax1(b-a+_1,fun i -> this.[i+a-1,j])
        member this.Item with get((a:int ,b:num0),j:int ) = ax1(b-a+_1,fun i -> this.[i+a-1,j])
        member this.Item with get((a:int ,b:int ),j:int ) = ax1(b-a+_1,fun i -> this.[i+a-1,j])
        member this.Item with get(i:num0,(a:num0,b:num0)) = ax1(b-a+_1,fun j -> this.[i,j+a-1])
        member this.Item with get(i:num0,(a:num0,b:int )) = ax1(b-a+_1,fun j -> this.[i,j+a-1])
        member this.Item with get(i:num0,(a:int ,b:num0)) = ax1(b-a+_1,fun j -> this.[i,j+a-1])
        member this.Item with get(i:num0,(a:int ,b:int )) = ax1(b-a+_1,fun j -> this.[i,j+a-1])
        member this.Item with get(i:int ,(a:num0,b:num0)) = ax1(b-a+_1,fun j -> this.[i,j+a-1])
        member this.Item with get(i:int ,(a:num0,b:int )) = ax1(b-a+_1,fun j -> this.[i,j+a-1])
        member this.Item with get(i:int ,(a:int ,b:num0)) = ax1(b-a+_1,fun j -> this.[i,j+a-1])
        member this.Item with get(i:int ,(a:int ,b:int )) = ax1(b-a+_1,fun j -> this.[i,j+a-1])
        member this.Item with get((a:num0,b:num0),_:unit) = ax2(b-a+_1,this.size2,fun (i,j) -> this.[i+a-1,j])
        member this.Item with get((a:num0,b:int ),_:unit) = ax2(b-a+_1,this.size2,fun (i,j) -> this.[i+a-1,j])
        member this.Item with get((a:int ,b:num0),_:unit) = ax2(b-a+_1,this.size2,fun (i,j) -> this.[i+a-1,j])
        member this.Item with get((a:int ,b:int ),_:unit) = ax2(b-a+_1,this.size2,fun (i,j) -> this.[i+a-1,j])
        member this.Item with get(_:unit,(c:num0,d:num0)) = ax2(this.size1,d-c+_1,fun (i,j) -> this.[i,j+c-1])
        member this.Item with get(_:unit,(c:num0,d:int )) = ax2(this.size1,d-c+_1,fun (i,j) -> this.[i,j+c-1])
        member this.Item with get(_:unit,(c:int ,d:num0)) = ax2(this.size1,d-c+_1,fun (i,j) -> this.[i,j+c-1])
        member this.Item with get(_:unit,(c:int ,d:int )) = ax2(this.size1,d-c+_1,fun (i,j) -> this.[i,j+c-1])
        member this.Item with get((a:num0,b:num0),(c:num0,d:num0)) = ax2(b-a+_1,d-c+_1,fun (i,j) -> this.[i+a-1,j+c-1])
        member this.Item with get((a:num0,b:int ),(c:num0,d:num0)) = ax2(b-a+_1,d-c+_1,fun (i,j) -> this.[i+a-1,j+c-1])
        member this.Item with get((a:int ,b:num0),(c:num0,d:num0)) = ax2(b-a+_1,d-c+_1,fun (i,j) -> this.[i+a-1,j+c-1])
        member this.Item with get((a:int ,b:int ),(c:num0,d:num0)) = ax2(b-a+_1,d-c+_1,fun (i,j) -> this.[i+a-1,j+c-1])
        member this.Item with get((a:num0,b:num0),(c:num0,d:int )) = ax2(b-a+_1,d-c+_1,fun (i,j) -> this.[i+a-1,j+c-1])
        member this.Item with get((a:num0,b:int ),(c:num0,d:int )) = ax2(b-a+_1,d-c+_1,fun (i,j) -> this.[i+a-1,j+c-1])
        member this.Item with get((a:int ,b:num0),(c:num0,d:int )) = ax2(b-a+_1,d-c+_1,fun (i,j) -> this.[i+a-1,j+c-1])
        member this.Item with get((a:int ,b:int ),(c:num0,d:int )) = ax2(b-a+_1,d-c+_1,fun (i,j) -> this.[i+a-1,j+c-1])
        member this.Item with get((a:num0,b:num0),(c:int ,d:num0)) = ax2(b-a+_1,d-c+_1,fun (i,j) -> this.[i+a-1,j+c-1])
        member this.Item with get((a:num0,b:int ),(c:int ,d:num0)) = ax2(b-a+_1,d-c+_1,fun (i,j) -> this.[i+a-1,j+c-1])
        member this.Item with get((a:int ,b:num0),(c:int ,d:num0)) = ax2(b-a+_1,d-c+_1,fun (i,j) -> this.[i+a-1,j+c-1])
        member this.Item with get((a:int ,b:int ),(c:int ,d:num0)) = ax2(b-a+_1,d-c+_1,fun (i,j) -> this.[i+a-1,j+c-1])
        member this.Item with get((a:num0,b:num0),(c:int ,d:int )) = ax2(b-a+_1,d-c+_1,fun (i,j) -> this.[i+a-1,j+c-1])
        member this.Item with get((a:num0,b:int ),(c:int ,d:int )) = ax2(b-a+_1,d-c+_1,fun (i,j) -> this.[i+a-1,j+c-1])
        member this.Item with get((a:int ,b:num0),(c:int ,d:int )) = ax2(b-a+_1,d-c+_1,fun (i,j) -> this.[i+a-1,j+c-1])
        member this.Item with get((a:int ,b:int ),(c:int ,d:int )) = ax2(b-a+_1,d-c+_1,fun (i,j) -> this.[i+a-1,j+c-1])
        static member (<==) (v1:num2,v2:num2) =
            let p = p.param
            if p.debugmode then
                p.error_code_counter_inc()
                p.comment("***debug array1 access check: "+p.error_code_counter.ToString()+"*****************************")
                br.branch <| fun b ->
                    b.IF (v1.size1 .=/ v2.size1) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '<==' array size 1 mismatch: "+v1.name+","+v2.name)]
                br.branch <| fun b ->
                    b.IF (v1.size2 .=/ v2.size2) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '<==' array size 2 mismatch: "+v1.name+","+v2.name)]
                p.comment("****************************************************")
            match p.lang with
              |F|T ->
                let u1 = v1.name
                let u2 = v2.name
                num0.type_subst_warning(v1.etype,v2.etype)
                p.codewrite(u1 + " = " + u2)
              |C89|C99 ->
                iter.num v1.size1 <| fun i ->
                    iter.num v1.size2 <| fun j ->
                        v1.[i,j] <== v2.[i,j]
              |H ->
                let u1 = v1.name
                let u2 = v2.name
                num0.type_subst_warning(v1.etype,v2.etype)
                p.codewrite("<math>" + u1 + "<mo>&larr;</mo>" + u2 + "</math>\n<br/>\n")
              |NL ->
                ()
        static member (<==) (v1:num2,v2:num0) =
            let p = p.param
            match p.lang with
              |F|T ->
                let u1 = v1.name
                let (t2,u2,c2) = v2.code.str
                num0.type_subst_warning(v1.etype,t2)
                p.codewrite(u1 + " = " + u2)
                //一時変数を削除
                c2 |> List.iter (fun c -> match c with |Var(t,n,_) -> p.dispose(t,n) |_ -> ())
              |C89|C99 ->
                iter.num v1.size1 <| fun i ->
                    iter.num v1.size2 <| fun j ->
                        v1.[i,j] <== v2
              |H ->
                let u1 = v1.name
                let (t2,u2,c2) = v2.code.str
                num0.type_subst_warning(v1.etype,t2)
                p.codewrite("<math>" + u1 + "<mo>&larr;</mo>" + u2 + "</math>\n<br/>\n")
                //一時変数を削除
                c2 |> List.iter (fun c -> match c with |Var(t,n,_) -> p.dispose(t,n) |_ -> ())
              |NL ->
                ()
        static member (<==) (v1:num2,v2:int) =
            v1 <== (Int_e v2)
        static member (<==) (v1:num2,v2:double) =
            v1 <== (Dbl_e v2)
        ///<summary>値を0で初期化</summary> 
        override this.clear() = 
            this <== 0
        ///<summary>配列サイズ変数をメモリ未割当て状態に初期化</summary>
        override this.sizeinit() = 
            this.size1 <== -1
            this.size2 <== -1
        /// <summary>
        /// この変数を関数内変数に変換
        /// </summary>
        /// <param name="cm">コメント</param>
        member __.farg (cm:string) = fun code ->
            let p = p.param
            p.addarg (typ,size,name,cm) <| fun (t,v,n) -> code(num2(t,v,n))
        static member (+) (x:num2,y:num2) =
            let p = p.param
            if p.debugmode then
                p.error_code_counter_inc()
                p.comment("***debug array1 access check: "+p.error_code_counter.ToString()+"*****************************")
                br.branch <| fun b ->
                    b.IF (x.size1 .=/ y.size1) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '+' array size 1 mismatch")]
                br.branch <| fun b ->
                    b.IF (x.size2 .=/ y.size2) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '+' array size 2 mismatch")]
                p.comment("****************************************************")
            ax2(x.size1,x.size2, fun (i,j) -> x.[i,j]+y.[i,j])
        static member (+) (x:num0,y:num2) = ax2(y.size1,y.size2, fun (i,j) -> x+y.[i,j])
        static member (+) (x:num2,y:num0) = ax2(x.size1,x.size2, fun (i,j) -> x.[i,j]+y)
        static member (+) (x:int,y:num2) = (I x) + y
        static member (+) (x:num2,y:int) = x + (I y)
        static member (+) (x:double,y:num2) = (D x) + y
        static member (+) (x:num2,y:double) = x + (D y)
        static member (+) (x:double*double,y:num2) = x + y
        static member (+) (x:num2,y:double*double) = x + y
        
        static member (-) (x:num2,y:num2) =
            let p = p.param
            if p.debugmode then
                p.error_code_counter_inc()
                p.comment("***debug array1 access check: "+p.error_code_counter.ToString()+"*****************************")
                br.branch <| fun b ->
                    b.IF (x.size1 .=/ y.size1) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '-' array size 1 mismatch")]
                br.branch <| fun b ->
                    b.IF (x.size2 .=/ y.size2) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '-' array size 2 mismatch")]
                p.comment("****************************************************")
            ax2(x.size1,x.size2, fun (i,j) -> x.[i,j]-y.[i,j])
        static member (-) (x:num0,y:num2) = ax2(y.size1,y.size2, fun (i,j) -> x-y.[i,j])
        static member (-) (x:num2,y:num0) = ax2(x.size1,x.size2, fun (i,j) -> x.[i,j]-y)
        static member (-) (x:int,y:num2) = (I x) - y
        static member (-) (x:num2,y:int) = x - (I y)
        static member (-) (x:double,y:num2) = (D x) - y
        static member (-) (x:num2,y:double) = x - (D y)
        static member (-) (x:double*double,y:num2) = x - y
        static member (-) (x:num2,y:double*double) = x - y
        
        static member (*) (x:num2,y:num2) =
            let p = p.param
            if p.debugmode then
                p.error_code_counter_inc()
                p.comment("***debug array1 access check: "+p.error_code_counter.ToString()+"*****************************")
                br.branch <| fun b ->
                    b.IF (x.size1 .=/ y.size1) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '*' array size 1 mismatch")]
                br.branch <| fun b ->
                    b.IF (x.size2 .=/ y.size2) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '*' array size 2 mismatch")]
                p.comment("****************************************************")
            ax2(x.size1,x.size2, fun (i,j) -> x.[i,j]*y.[i,j])
        static member (*) (x:num0,y:num2) = ax2(y.size1,y.size2, fun (i,j) -> x*y.[i,j])
        static member (*) (x:num2,y:num0) = ax2(x.size1,x.size2, fun (i,j) -> x.[i,j]*y)
        static member (*) (x:int,y:num2) = (I x) * y
        static member (*) (x:num2,y:int) = x * (I y)
        static member (*) (x:double,y:num2) = (D x) * y
        static member (*) (x:num2,y:double) = x * (D y)
        static member (*) (x:double*double,y:num2) = x * y
        static member (*) (x:num2,y:double*double) = x * y
        
        static member (/) (x:num2,y:num2) =
            let p = p.param
            if p.debugmode then
                p.error_code_counter_inc()
                p.comment("***debug array1 access check: "+p.error_code_counter.ToString()+"*****************************")
                br.branch <| fun b ->
                    b.IF (x.size1 .=/ y.size1) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '/' array size 1 mismatch")]
                br.branch <| fun b ->
                    b.IF (x.size2 .=/ y.size2) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '/' array size 2 mismatch")]
                p.comment("****************************************************")
            ax2(x.size1,x.size2, fun (i,j) -> x.[i,j]/y.[i,j])
        static member (/) (x:num0,y:num2) = ax2(y.size1,y.size2, fun (i,j) -> x/y.[i,j])
        static member (/) (x:num2,y:num0) = ax2(x.size1,x.size2, fun (i,j) -> x.[i,j]/y)
        static member (/) (x:int,y:num2) = (I x) / y
        static member (/) (x:num2,y:int) = x / (I y)
        static member (/) (x:double,y:num2) = (D x) / y
        static member (/) (x:num2,y:double) = x / (D y)
        static member (/) (x:double*double,y:num2) = x / y
        static member (/) (x:num2,y:double*double) = x / y
            
    and ax2(n:num0,m:num0,f:(num0*num0)->num0) =
        member _.size1 = n
        member _.size2 = m
        member _.Item with get(i:num0,j:num0) = f(i,j)
        member _.Item with get(i:int,j:int) = f(I i,I j)
        member _.etype with get() = (f(_1,_1)).etype
        static member (+) (x:ax2,y:ax2) =
            let p = p.param
            if p.debugmode then
                p.error_code_counter_inc()
                p.comment("***debug array1 access check: "+p.error_code_counter.ToString()+"*****************************")
                br.branch <| fun b ->
                    b.IF (x.size1 .=/ y.size1) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '+' array size 1 mismatch")]
                br.branch <| fun b ->
                    b.IF (x.size2 .=/ y.size2) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '+' array size 2 mismatch")]
                p.comment("****************************************************")
            ax2(x.size1,x.size2,fun (i,j) ->x.[i,j]+y.[i,j])
        static member (+) (x:ax2,y:num2) =
            let p = p.param
            if p.debugmode then
                p.error_code_counter_inc()
                p.comment("***debug array1 access check: "+p.error_code_counter.ToString()+"*****************************")
                br.branch <| fun b ->
                    b.IF (x.size1 .=/ y.size1) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '+' array size 1 mismatch")]
                br.branch <| fun b ->
                    b.IF (x.size2 .=/ y.size2) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '+' array size 2 mismatch")]
                p.comment("****************************************************")
            ax2(x.size1,x.size2,fun (i,j) ->x.[i,j]+y.[i,j])
        static member (+) (x:num2,y:ax2) =
            let p = p.param
            if p.debugmode then
                p.error_code_counter_inc()
                p.comment("***debug array1 access check: "+p.error_code_counter.ToString()+"*****************************")
                br.branch <| fun b ->
                    b.IF (x.size1 .=/ y.size1) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '+' array size 1 mismatch")]
                br.branch <| fun b ->
                    b.IF (x.size2 .=/ y.size2) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '+' array size 2 mismatch")]
                p.comment("****************************************************")
            ax2(x.size1,x.size2,fun (i,j) ->x.[i,j]+y.[i,j])
        static member (+) (x:ax2,y:int) =
            ax2(x.size1,x.size2,fun (i,j) ->x.[i,j]+y)
        static member (+) (x:ax2,y:double) =
            ax2(x.size1,x.size2,fun (i,j) ->x.[i,j]+y)
        static member (+) (x:ax2,y:double*double) =
            ax2(x.size1,x.size2,fun (i,j) ->x.[i,j]+y)
        static member (+) (x:int,y:ax2) =
            ax2(y.size1,y.size2,fun (i,j) ->x+y.[i,j])
        static member (+) (x:double,y:ax2) =
            ax2(y.size1,y.size2,fun (i,j) ->x+y.[i,j])
        static member (+) (x:double*double,y:ax2) =
            ax2(y.size1,y.size2,fun (i,j) ->x+y.[i,j])
        static member (-) (x:ax2,y:ax2) =
            let p = p.param
            if p.debugmode then
                p.error_code_counter_inc()
                p.comment("***debug array1 access check: "+p.error_code_counter.ToString()+"*****************************")
                br.branch <| fun b ->
                    b.IF (x.size1 .=/ y.size1) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '-' array size 1 mismatch")]
                br.branch <| fun b ->
                    b.IF (x.size2 .=/ y.size2) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '-' array size 2 mismatch")]
                p.comment("****************************************************")
            ax2(x.size1,x.size2,fun (i,j) ->x.[i,j]-y.[i,j])
        static member (-) (x:ax2,y:num2) =
            let p = p.param
            if p.debugmode then
                p.error_code_counter_inc()
                p.comment("***debug array1 access check: "+p.error_code_counter.ToString()+"*****************************")
                br.branch <| fun b ->
                    b.IF (x.size1 .=/ y.size1) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '-' array size 1 mismatch")]
                br.branch <| fun b ->
                    b.IF (x.size2 .=/ y.size2) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '-' array size 2 mismatch")]
                p.comment("****************************************************")
            ax2(x.size1,x.size2,fun (i,j) ->x.[i,j]-y.[i,j])
        static member (-) (x:num2,y:ax2) =
            let p = p.param
            if p.debugmode then
                p.error_code_counter_inc()
                p.comment("***debug array1 access check: "+p.error_code_counter.ToString()+"*****************************")
                br.branch <| fun b ->
                    b.IF (x.size1 .=/ y.size1) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '-' array size 1 mismatch")]
                br.branch <| fun b ->
                    b.IF (x.size2 .=/ y.size2) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '-' array size 2 mismatch")]
                p.comment("****************************************************")
            ax2(x.size1,x.size2,fun (i,j) ->x.[i,j]-y.[i,j])
        static member (-) (x:ax2,y:int) =
            ax2(x.size1,x.size2,fun (i,j) ->x.[i,j]-y)
        static member (-) (x:ax2,y:double) =
            ax2(x.size1,x.size2,fun (i,j) ->x.[i,j]-y)
        static member (-) (x:ax2,y:double*double) =
            ax2(x.size1,x.size2,fun (i,j) ->x.[i,j]-y)
        static member (-) (x:int,y:ax2) =
            ax2(y.size1,y.size2,fun (i,j) ->x-y.[i,j])
        static member (-) (x:double,y:ax2) =
            ax2(y.size1,y.size2,fun (i,j) ->x-y.[i,j])
        static member (-) (x:double*double,y:ax2) =
            ax2(y.size1,y.size2,fun (i,j) ->x-y.[i,j])
        static member (*) (x:ax2,y:ax2) =
            let p = p.param
            if p.debugmode then
                p.error_code_counter_inc()
                p.comment("***debug array1 access check: "+p.error_code_counter.ToString()+"*****************************")
                br.branch <| fun b ->
                    b.IF (x.size1 .=/ y.size1) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '*' array size 1 mismatch")]
                br.branch <| fun b ->
                    b.IF (x.size2 .=/ y.size2) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '*' array size 2 mismatch")]
                p.comment("****************************************************")
            ax2(x.size1,x.size2,fun (i,j) ->x.[i,j]*y.[i,j])
        static member (*) (x:ax2,y:num2) =
            let p = p.param
            if p.debugmode then
                p.error_code_counter_inc()
                p.comment("***debug array1 access check: "+p.error_code_counter.ToString()+"*****************************")
                br.branch <| fun b ->
                    b.IF (x.size1 .=/ y.size1) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '*' array size 1 mismatch")]
                br.branch <| fun b ->
                    b.IF (x.size2 .=/ y.size2) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '*' array size 2 mismatch")]
                p.comment("****************************************************")
            ax2(x.size1,x.size2,fun (i,j) ->x.[i,j]*y.[i,j])
        static member (*) (x:num2,y:ax2) =
            let p = p.param
            if p.debugmode then
                p.error_code_counter_inc()
                p.comment("***debug array1 access check: "+p.error_code_counter.ToString()+"*****************************")
                br.branch <| fun b ->
                    b.IF (x.size1 .=/ y.size1) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '*' array size 1 mismatch")]
                br.branch <| fun b ->
                    b.IF (x.size2 .=/ y.size2) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '*' array size 2 mismatch")]
                p.comment("****************************************************")
            ax2(x.size1,x.size2,fun (i,j) ->x.[i,j]*y.[i,j])
        static member (*) (x:ax2,y:int) =
            ax2(x.size1,x.size2,fun (i,j) ->x.[i,j]*y)
        static member (*) (x:ax2,y:double) =
            ax2(x.size1,x.size2,fun (i,j) ->x.[i,j]*y)
        static member (*) (x:ax2,y:double*double) =
            ax2(x.size1,x.size2,fun (i,j) ->x.[i,j]*y)
        static member (*) (x:int,y:ax2) =
            ax2(y.size1,y.size2,fun (i,j) ->x*y.[i,j])
        static member (*) (x:double,y:ax2) =
            ax2(y.size1,y.size2,fun (i,j) ->x*y.[i,j])
        static member (*) (x:double*double,y:ax2) =
            ax2(y.size1,y.size2,fun (i,j) ->x*y.[i,j])
        static member (/) (x:ax2,y:ax2) =
            let p = p.param
            if p.debugmode then
                p.error_code_counter_inc()
                p.comment("***debug array1 access check: "+p.error_code_counter.ToString()+"*****************************")
                br.branch <| fun b ->
                    b.IF (x.size1 .=/ y.size1) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '/' array size 1 mismatch")]
                br.branch <| fun b ->
                    b.IF (x.size2 .=/ y.size2) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '/' array size 2 mismatch")]
                p.comment("****************************************************")
            ax2(x.size1,x.size2,fun (i,j) ->x.[i,j]/y.[i,j])
        static member (/) (x:ax2,y:num2) =
            let p = p.param
            if p.debugmode then
                p.error_code_counter_inc()
                p.comment("***debug array1 access check: "+p.error_code_counter.ToString()+"*****************************")
                br.branch <| fun b ->
                    b.IF (x.size1 .=/ y.size1) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '/' array size 1 mismatch")]
                br.branch <| fun b ->
                    b.IF (x.size2 .=/ y.size2) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '/' array size 2 mismatch")]
                p.comment("****************************************************")
            ax2(x.size1,x.size2,fun (i,j) ->x.[i,j]/y.[i,j])
        static member (/) (x:num2,y:ax2) =
            let p = p.param
            if p.debugmode then
                p.error_code_counter_inc()
                p.comment("***debug array1 access check: "+p.error_code_counter.ToString()+"*****************************")
                br.branch <| fun b ->
                    b.IF (x.size1 .=/ y.size1) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '/' array size 1 mismatch")]
                br.branch <| fun b ->
                    b.IF (x.size2 .=/ y.size2) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '/' array size 2 mismatch")]
                p.comment("****************************************************")
            ax2(x.size1,x.size2,fun (i,j) ->x.[i,j]/y.[i,j])
        static member (/) (x:ax2,y:int) =
            ax2(x.size1,x.size2,fun (i,j) ->x.[i,j]/y)
        static member (/) (x:ax2,y:double) =
            ax2(x.size1,x.size2,fun (i,j) ->x.[i,j]/y)
        static member (/) (x:ax2,y:double*double) =
            ax2(x.size1,x.size2,fun (i,j) ->x.[i,j]/y)
        static member (/) (x:int,y:ax2) =
            ax2(y.size1,y.size2,fun (i,j) ->x/y.[i,j])
        static member (/) (x:double,y:ax2) =
            ax2(y.size1,y.size2,fun (i,j) ->x/y.[i,j])
        static member (/) (x:double*double,y:ax2) =
            ax2(y.size1,y.size2,fun (i,j) ->x/y.[i,j])

        static member (<==) (x:num2,y:ax2) =
            let p = p.param
            if p.debugmode then
                p.error_code_counter_inc()
                p.comment("***debug array1 access check: "+p.error_code_counter.ToString()+"*****************************")
                br.branch <| fun b ->
                    b.IF (x.size1 .=/ y.size1) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '<==' array size 1 mismatch")]
                br.branch <| fun b ->
                    b.IF (x.size2 .=/ y.size2) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '<==' array size 2 mismatch")]
                p.comment("****************************************************")
            iter.num x.size1 <| fun i ->
                iter.num x.size2 <| fun j ->
                    x.[i,j] <== y.[i,j]
        static member (<==) (x:ax2,y:num2) =
            let p = p.param
            if p.debugmode then
                p.error_code_counter_inc()
                p.comment("***debug array1 access check: "+p.error_code_counter.ToString()+"*****************************")
                br.branch <| fun b ->
                    b.IF (x.size1 .=/ y.size1) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '<==' array size 1 mismatch")]
                br.branch <| fun b ->
                    b.IF (x.size2 .=/ y.size2) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '<==' array size 2 mismatch")]
                p.comment("****************************************************")
            iter.num x.size1 <| fun i ->
                iter.num x.size2 <| fun j ->
                    x.[i,j] <== y.[i,j]
        static member (<==) (x:ax2,y:ax2) =
            let p = p.param
            if p.debugmode then
                p.error_code_counter_inc()
                p.comment("***debug array1 access check: "+p.error_code_counter.ToString()+"*****************************")
                br.branch <| fun b ->
                    b.IF (x.size1 .=/ y.size1) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '<==' array size 1 mismatch")]
                br.branch <| fun b ->
                    b.IF (x.size2 .=/ y.size2) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '<==' array size 2 mismatch")]
                p.comment("****************************************************")
            iter.num x.size1 <| fun i ->
                iter.num x.size2 <| fun j ->
                    x.[i,j] <== y.[i,j]
        member this.abs
            with get() =
                ax2(this.size1,this.size2,fun (i,j) -> this.[i,j].abs)
        member this.pow
            with get() =
                ax2(this.size1,this.size2,fun (i,j) -> this.[i,j].pow)
        member this.pha
            with get() =
                ax2(this.size1,this.size2,fun (i,j) -> this.[i,j].pha)

    [<AutoOpen>]
    module asm_ax2 =
        type asm with
            static member pow(x:ax2,y:num0) =
                ax2(x.size1,x.size2,fun (i,j) -> asm.pow(x.[i,j],y))
            static member pow(x:ax2,y:int) =
                ax2(x.size1,x.size2,fun (i,j) -> asm.pow(x.[i,j],y))
            static member pow(x:ax2,y:double) =
                ax2(x.size1,x.size2,fun (i,j) -> asm.pow(x.[i,j],y))
            static member pow(x:ax2,y:double*double) =
                ax2(x.size1,x.size2,fun (i,j) -> asm.pow(x.[i,j],y))
            static member sin(x:ax2) =
                ax2(x.size1,x.size2,fun (i,j) -> asm.sin(x.[i,j]))
            static member cos(x:ax2) =
                ax2(x.size1,x.size2,fun (i,j) -> asm.cos(x.[i,j]))
            static member tan(x:ax2) =
                ax2(x.size1,x.size2,fun (i,j) -> asm.tan(x.[i,j]))
            static member asin(x:ax2) =
                ax2(x.size1,x.size2,fun (i,j) -> asm.asin(x.[i,j]))
            static member acos(x:ax2) =
                ax2(x.size1,x.size2,fun (i,j) -> asm.acos(x.[i,j]))
            static member atan(x:ax2) =
                ax2(x.size1,x.size2,fun (i,j) -> asm.atan(x.[i,j]))
            static member atan2(x:ax2,y:ax2) =
                ax2(x.size1,x.size2,fun (i,j) -> asm.atan2(x.[i,j],y.[i,j]))
            static member exp(x:ax2) =
                ax2(x.size1,x.size2,fun (i,j) -> asm.exp(x.[i,j]))
            static member abs(x:ax2) =
                ax2(x.size1,x.size2,fun (i,j) -> asm.abs(x.[i,j]))
            static member log(x:ax2) =
                ax2(x.size1,x.size2,fun (i,j) -> asm.log(x.[i,j]))
            static member log10(x:ax2) =
                ax2(x.size1,x.size2,fun (i,j) -> asm.log10(x.[i,j]))
            static member sqrt(x:ax2) =
                ax2(x.size1,x.size2,fun (i,j) -> asm.sqrt(x.[i,j]))
            static member floor(x:ax2) =
                ax2(x.size1,x.size2,fun (i,j) -> asm.floor(x.[i,j]))
            static member ceil(x:ax2) =
                ax2(x.size1,x.size2,fun (i,j) -> asm.ceil(x.[i,j]))
            static member toint(x:ax2) =
                ax2(x.size1,x.size2,fun (i,j) -> asm.toint(x.[i,j]))
            static member todouble(x:ax2) =
                ax2(x.size1,x.size2,fun (i,j) -> asm.todouble(x.[i,j]))
            static member conj(x:ax2) =
                ax2(x.size1,x.size2,fun (i,j) -> asm.conj(x.[i,j]))
            static member pow(x:num2,y:num0) =
                ax2(x.size1,x.size2,fun (i,j) -> asm.pow(x.[i,j],y))
            static member pow(x:num2,y:int) =
                ax2(x.size1,x.size2,fun (i,j) -> asm.pow(x.[i,j],y))
            static member pow(x:num2,y:double) =
                ax2(x.size1,x.size2,fun (i,j) -> asm.pow(x.[i,j],y))
            static member pow(x:num2,y:double*double) =
                ax2(x.size1,x.size2,fun (i,j) -> asm.pow(x.[i,j],y))
            static member sin(x:num2) =
                ax2(x.size1,x.size2,fun (i,j) -> asm.sin(x.[i,j]))
            static member cos(x:num2) =
                ax2(x.size1,x.size2,fun (i,j) -> asm.cos(x.[i,j]))
            static member tan(x:num2) =
                ax2(x.size1,x.size2,fun (i,j) -> asm.tan(x.[i,j]))
            static member asin(x:num2) =
                ax2(x.size1,x.size2,fun (i,j) -> asm.asin(x.[i,j]))
            static member acos(x:num2) =
                ax2(x.size1,x.size2,fun (i,j) -> asm.acos(x.[i,j]))
            static member atan(x:num2) =
                ax2(x.size1,x.size2,fun (i,j) -> asm.atan(x.[i,j]))
            static member atan2(x:num2,y:ax2) =
                ax2(x.size1,x.size2,fun (i,j) -> asm.atan2(x.[i,j],y.[i,j]))
            static member exp(x:num2) =
                ax2(x.size1,x.size2,fun (i,j) -> asm.exp(x.[i,j]))
            static member abs(x:num2) =
                ax2(x.size1,x.size2,fun (i,j) -> asm.abs(x.[i,j]))
            static member log(x:num2) =
                ax2(x.size1,x.size2,fun (i,j) -> asm.log(x.[i,j]))
            static member log10(x:num2) =
                ax2(x.size1,x.size2,fun (i,j) -> asm.log10(x.[i,j]))
            static member sqrt(x:num2) =
                ax2(x.size1,x.size2,fun (i,j) -> asm.sqrt(x.[i,j]))
            static member floor(x:num2) =
                ax2(x.size1,x.size2,fun (i,j) -> asm.floor(x.[i,j]))
            static member ceil(x:num2) =
                ax2(x.size1,x.size2,fun (i,j) -> asm.ceil(x.[i,j]))
            static member toint(x:num2) =
                ax2(x.size1,x.size2,fun (i,j) -> asm.toint(x.[i,j]))
            static member todouble(x:num2) =
                ax2(x.size1,x.size2,fun (i,j) -> asm.todouble(x.[i,j]))
            static member conj(x:num2) =
                ax2(x.size1,x.size2,fun (i,j) -> asm.conj(x.[i,j]))
                
    [<AutoOpen>]
    module num2_ax2 =
        type num2 with
            member this.abs
                with get() =
                    ax2(this.size1,this.size2,fun (i,j) -> this.[i,j].abs)
            member this.pow
                with get() =
                    ax2(this.size1,this.size2,fun (i,j) -> this.[i,j].pow)
            member this.pha
                with get() =
                    ax2(this.size1,this.size2,fun (i,j) -> this.[i,j].pha)
            
    [<AutoOpen>]
    module tbinder_num2 =
        ///<summary>数値型の型に従って処理を分岐</summary>
        type tbinder with
            /// <summary>
            /// 指定された式または変数eqが整数型の場合のみcodeを実行
            /// </summary>
            /// <param name="eq"></param>
            static member i(eq:num2) = fun code ->
                match eq.etype with
                  |It _ -> code()
                  |_ -> ()
            /// <summary>
            /// 指定された式または変数eqが実数型の場合のみcodeを実行
            /// </summary>
            /// <param name="eq"></param>
            static member d(eq:num2) = fun code ->
                match eq.etype with
                  |Dt -> code()
                  |_ -> ()
            /// <summary>
            /// 指定された式または変数eqが複素数型の場合のみcodeを実行
            /// </summary>
            /// <param name="eq"></param>
            static member z(eq:num2) = fun code ->
                match eq.etype with
                  |Zt -> code()
                  |_ -> ()
                  
            /// <summary>
            /// 指定された式または変数eqが整数型の場合のみcodeを実行
            /// </summary>
            /// <param name="eq"></param>
            static member i(eq:ax2) = fun code ->
                match eq.etype with
                  |It _ -> code()
                  |_ -> ()
            /// <summary>
            /// 指定された式または変数eqが実数型の場合のみcodeを実行
            /// </summary>
            /// <param name="eq"></param>
            static member d(eq:ax2) = fun code ->
                match eq.etype with
                  |Dt -> code()
                  |_ -> ()
            /// <summary>
            /// 指定された式または変数eqが複素数型の場合のみcodeを実行
            /// </summary>
            /// <param name="eq"></param>
            static member z(eq:ax2) = fun code ->
                match eq.etype with
                  |Zt -> code()
                  |_ -> ()
                  