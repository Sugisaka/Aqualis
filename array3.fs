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

    ///<summary>3次元配列</summary>
    type base3 (typ:Etype,size1:VarType,name:string) =
        new (typ,size,name,para) = 
            p.param.vreg(typ,size,name,para)
            base3(typ,size,name)
        new(sname,size,name) =
            p.param.vreg(Structure(sname),size,name,"")
            base3(Structure(sname),size,name)
        member __.name 
          with get() =
            name
        member internal __.etype
          with get() =
            typ
        member internal __.vtype
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
        member __.size3
          with get() =
            let p = p.param
            match p.lang with 
              |F |T -> 
                Var(It 4,name+"_size(3)",[]) 
              |C89 |C99 -> 
                Var(It 4,name+"_size[2]",[])
              |H -> 
                Var(It 4,"<msub><mi mathvariant=\"script\">S</mi><mn>3</mn></msub><mo>&af;</mo><mo>[</mo><mi>"+name+"</mi><mo>]</mo>",[]) 
              |NL ->
                NaN
        member this.Idx3(i:num0,j:num0,k:num0) =
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
                    b.IF <| Or[k.< 1; this.size3.<k] <| fun () ->
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" array "+name+" illegal access. 3rd index ");k;Str_e(" is out of range (1:");this.size3;Str_e(")")]
                p.comment("****************************************************")
            match p.lang with
              |F   -> name+"("+i.name+","+j.name+","+k.name+")"
              |C89 -> name+"["+((k-1)*this.size1*this.size2+(j-1)*this.size1+(i-1)).name+"]"
              |C99 -> name+"["+((k-1)*this.size1*this.size2+(j-1)*this.size1+(i-1)).name+"]"
              |T   -> name+"("+i.name+","+j.name+","+k.name+")"
              |H   -> name+"<mo>&af;</mo><mo>[</mo>"+i.name+j.name+k.name+"<mo>]</mo>"
              |NL  -> ""
        member this.Idx3(i:int ,j:int ,k:int ) = this.Idx3(I i,I j,I k)
        member this.Idx3(i:num0,j:int ,k:int ) = this.Idx3(i,I j,I k)
        member this.Idx3(i:int ,j:num0,k:int ) = this.Idx3(I i,j,I k)
        member this.Idx3(i:int ,j:int ,k:num0) = this.Idx3(I i,I j,k)
        member this.Idx3(i:num0,j:num0,k:int ) = this.Idx3(i,j,I k)
        member this.Idx3(i:num0,j:int ,k:num0) = this.Idx3(i,I j,k)
        member this.Idx3(i:int ,j:num0,k:num0) = this.Idx3(I i,j,k)
        member this.foreach code =
            iter.num this.size1 <| fun i -> 
                iter.num this.size2 <| fun j -> 
                    iter.num this.size3 <| fun k -> 
                        code(i,j,k)
        member this.foreach1 code =
            iter.num this.size1 <| fun i -> 
                code(i)
        member this.foreach2 code =
            iter.num this.size2 <| fun j -> 
                code(j)
        member this.foreach3 code =
            iter.num this.size3 <| fun k -> 
                code(k)
        member this.foreach_exit code =
            iter.num_exit this.size1 <| fun (ext,i) -> 
                iter.num this.size2 <| fun j -> 
                    iter.num this.size3 <| fun k -> 
                        code(ext,i,j,k)
        member this.foreach1_exit code =
            iter.num_exit this.size1 <| fun (ext,i) -> 
                code(ext,i)
        member this.foreach2_exit code =
            iter.num_exit this.size2 <| fun (ext,j) -> 
                code(ext,j)
        member this.foreach3_exit code =
            iter.num_exit this.size3 <| fun (ext,k) -> 
                code(ext,k)
        member this.allocate_check() =
              let p = p.param
              p.comment("***debug array3 allocate check: "+p.error_code_counter.ToString()+"*****************************")
              br.branch <| fun b ->
                  b.IF <| (this.size1 .=/ I(-1)) <| fun () ->
                      print.s[Str_e("ERROR"+p.error_code_counter.ToString()+" array "+name+" is already allocated")]
              p.comment("****************************************************")
        member this.allocate(n1:num0,n2:num0,n3:num0) =
            tbinder.i ("allocate "+name) n1 <| fun () ->
            tbinder.i ("allocate "+name) n2 <| fun () ->
            tbinder.i ("allocate "+name) n3 <| fun () ->
                let p = p.param
                let alloc (n1:num0) (n2:num0) (n3:num0) =
                    if p.debugmode then
                        p.error_code_counter_inc()
                        p.comment("***debug array3 allocate check: "+p.error_code_counter.ToString()+"*****************************")
                        br.branch <| fun b ->
                            b.IF <| (this.size1 .=/ -1) <| fun () ->
                                print.s[Str_e("ERROR"+p.error_code_counter.ToString()+" array "+name+" is already allocated")]
                        p.comment("****************************************************")
                    match p.lang with
                      |F ->
                        match size1 with
                          |A3(0,0,0) ->
                            this.size1 <== n1
                            this.size2 <== n2
                            this.size3 <== n3
                            p.codewrite("allocate("+name+"(1:"+this.size1.name+",1:"+this.size2.name+",1:"+this.size3.name+")"+")"+"\n")
                          |_ -> 
                            p.codewrite("(Error:055-001 「"+name+"」は可変長1次元配列ではありません")
                      |C89 ->
                        match size1 with
                          |A3(0,0,0) ->
                            this.size1 <== n1
                            this.size2 <== n2
                            this.size3 <== n3
                            p.codewrite(name+"="+"("+typ.tostring()+" *)"+"malloc("+"sizeof("+typ.tostring()+")*"+this.size1.name+"*"+this.size2.name+"*"+this.size3.name+");\n")
                          |_ -> 
                            p.codewrite("(Error:055-001 「"+name+"」は可変長1次元配列ではありません")
                      |C99 ->
                        match size1 with
                          |A3(0,0,0) ->
                            this.size1 <== n1
                            this.size2 <== n2
                            this.size3 <== n3
                            p.codewrite(name+"="+"("+typ.tostring()+" *)"+"malloc("+"sizeof("+typ.tostring()+")*"+this.size1.name+"*"+this.size2.name+"*"+this.size3.name+");\n")
                          |_ -> 
                            p.codewrite("(Error:055-001 「"+name+"」は可変長1次元配列ではありません")
                      |T ->
                        match size1 with
                          |A3(0,0,0) ->
                            this.size1 <== n1
                            this.size2 <== n2
                            this.size3 <== n3
                            p.codewrite("allocate($"+name+"(1:"+this.size1.name+",1:"+this.size2.name+",1:"+this.size3.name+")"+"$)"+"\n")
                          |_ -> 
                            p.codewrite("(Error:055-001 「"+name+"」は可変長1次元配列ではありません")
                      |H ->
                        match size1 with
                          |A3(0,0,0) ->
                            p.codewrite("<math>"+name+"<mo>:</mo><mi>allocate</mi><mo>(</mo>"+n1.name+"<mo>,</mo>"+n2.name+"<mo>,</mo>"+n3.name+"<mo>)</mo></math>"+"\n<br/>\n")
                          |_ -> 
                            p.codewrite("(Error:055-001 「"+name+"」は可変長1次元配列ではありません")
                      |NL ->
                        ()
                            
                match p.lang with
                  |H ->
                    alloc n1 n2 n3
                  |_ ->
                    match (n1,n2,n3) with
                        |(Int_e n1_,Int_e n2_,Int_e n3_) ->
                            if n1_>0 && n2_>0 && n3_>0 then
                                alloc n1 n2 n3
                            else
                                alloc _0 _0 _0
                        |_ ->
                          br.branch <| fun b ->
                            b.IF <| And[n1.>I 0; n2.>I 0; n3.>I 0] <| fun () ->
                                alloc n1 n2 n3
                            b.EL <| fun () ->
                                alloc _0 _0 _0
        member this.allocate(n1:num0,n2:num0,n3:int) =
            this.allocate(n1,n2,I n3)
        member this.allocate(n1:num0,n2:int,n3:num0) =
            this.allocate(n1,I n2,n3)
        member this.allocate(n1:num0,n2:int,n3:int) =
            this.allocate(n1,I n2,I n3)
        member this.allocate(n1:int,n2:num0,n3:num0) =
            this.allocate(I n1,n2,n3)
        member this.allocate(n1:int,n2:num0,n3:int) =
            this.allocate(I n1,n2,I n3)
        member this.allocate(n1:int,n2:int,n3:num0) =
            this.allocate(I n1,I n2,n3)
        member this.allocate(n1:int,n2:int,n3:int) =
            this.allocate(I n1,I n2,I n3)
            
        member this.deallocate_check() =
            let p = p.param
            p.comment("***debug array3 deallocate check: "+p.error_code_counter.ToString()+"*****************************")
            br.branch <| fun b ->
                b.IF (this.size1 .= -1) <| fun () ->
                    print.s[Str_e("ERROR"+p.error_code_counter.ToString()+" cannot deallocate array "+name)]
            p.comment("****************************************************")
        member this.deallocate() =
            let p = p.param
            if p.debugmode then
                p.error_code_counter_inc()
                this.deallocate_check()
            match p.lang with
              |F ->
                match size1 with
                  |A3(0,0,0) ->
                    this.size1 <== -1
                    this.size2 <== -1
                    this.size3 <== -1
                    p.codewrite("deallocate("+name+")"+"\n")
                  |_ -> ()
              |C89 ->
                match size1 with
                  |A3(0,0,0) ->
                    this.size1 <== -1
                    this.size2 <== -1
                    this.size3 <== -1
                    p.codewrite("free("+name+");"+"\n")
                  |_ -> ()
              |C99 ->
                match size1 with
                  |A3(0,0,0) ->
                    this.size1 <== -1
                    this.size2 <== -1
                    this.size3 <== -1
                    p.codewrite("free("+name+");"+"\n")
                  |_ -> ()
              |T ->
                match size1 with
                  |A3(0,0,0) ->
                    this.size1 <== -1
                    this.size2 <== -1
                    this.size3 <== -1
                    p.codewrite("deallocate($"+name+"$)"+"\n")
                  |_ -> ()
              |H ->
                match size1 with
                  |A3(0,0,0) ->
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
            
    ///<summary>数値型3次元配列</summary>
    type num3 (typ:Etype,size1:VarType,name:string) =
        inherit base3(typ,size1,name)
        new (typ,size1,name,para) = 
            p.param.vreg(typ,size1,name,para)
            num3(typ,size1,name)
        member this.Item with get(i:int ,j:int ,k:int ) = Var(typ,this.Idx3(I i,I j,I k),[])
        member this.Item with get(i:num0,j:int ,k:int ) = Var(typ,this.Idx3(i,I j,I k),[])
        member this.Item with get(i:int ,j:num0,k:int ) = Var(typ,this.Idx3(I i,j,I k),[])
        member this.Item with get(i:int ,j:int ,k:num0) = Var(typ,this.Idx3(I i,I j,k),[])
        member this.Item with get(i:num0,j:num0,k:int ) = Var(typ,this.Idx3(i,j,I k),[])
        member this.Item with get(i:num0,j:int ,k:num0) = Var(typ,this.Idx3(i,I j,k),[])
        member this.Item with get(i:int ,j:num0,k:num0) = Var(typ,this.Idx3(I i,j,k),[])
        member this.Item with get(i:num0,j:num0,k:num0) = Var(typ,this.Idx3(i,j,k),[])
        member this.Item with get(_:unit,j:num0,k:num0) = ax1(this.size1,fun i -> this.[i,j,k])
        member this.Item with get(_:unit,j:num0,k:int ) = ax1(this.size1,fun i -> this.[i,j,k])
        member this.Item with get(_:unit,j:int ,k:num0) = ax1(this.size1,fun i -> this.[i,j,k])
        member this.Item with get(_:unit,j:int ,k:int ) = ax1(this.size1,fun i -> this.[i,j,k])
        member this.Item with get(i:num0,_:unit,k:num0) = ax1(this.size2,fun j -> this.[i,j,k])
        member this.Item with get(i:num0,_:unit,k:int ) = ax1(this.size2,fun j -> this.[i,j,k])
        member this.Item with get(i:int ,_:unit,k:num0) = ax1(this.size2,fun j -> this.[i,j,k])
        member this.Item with get(i:int ,_:unit,k:int ) = ax1(this.size2,fun j -> this.[i,j,k])
        member this.Item with get(i:num0,j:num0,_:unit) = ax1(this.size3,fun k -> this.[i,j,k])
        member this.Item with get(i:num0,j:int ,_:unit) = ax1(this.size3,fun k -> this.[i,j,k])
        member this.Item with get(i:int ,j:num0,_:unit) = ax1(this.size3,fun k -> this.[i,j,k])
        member this.Item with get(i:int ,j:int ,_:unit) = ax1(this.size3,fun k -> this.[i,j,k])
        member this.Item with get((a:num0,b:num0),j:num0,k:num0) = ax1(b-a+_1,fun i -> this.[i+a-1,j,k])
        member this.Item with get((a:num0,b:num0),j:num0,k:int ) = ax1(b-a+_1,fun i -> this.[i+a-1,j,k])
        member this.Item with get((a:num0,b:num0),j:int ,k:num0) = ax1(b-a+_1,fun i -> this.[i+a-1,j,k])
        member this.Item with get((a:num0,b:num0),j:int ,k:int ) = ax1(b-a+_1,fun i -> this.[i+a-1,j,k])
        member this.Item with get((a:num0,b:int ),j:num0,k:num0) = ax1(b-a+_1,fun i -> this.[i+a-1,j,k])
        member this.Item with get((a:num0,b:int ),j:num0,k:int ) = ax1(b-a+_1,fun i -> this.[i+a-1,j,k])
        member this.Item with get((a:num0,b:int ),j:int ,k:num0) = ax1(b-a+_1,fun i -> this.[i+a-1,j,k])
        member this.Item with get((a:num0,b:int ),j:int ,k:int ) = ax1(b-a+_1,fun i -> this.[i+a-1,j,k])
        member this.Item with get((a:int ,b:num0),j:num0,k:num0) = ax1(b-a+_1,fun i -> this.[i+a-1,j,k])
        member this.Item with get((a:int ,b:num0),j:num0,k:int ) = ax1(b-a+_1,fun i -> this.[i+a-1,j,k])
        member this.Item with get((a:int ,b:num0),j:int ,k:num0) = ax1(b-a+_1,fun i -> this.[i+a-1,j,k])
        member this.Item with get((a:int ,b:num0),j:int ,k:int ) = ax1(b-a+_1,fun i -> this.[i+a-1,j,k])
        member this.Item with get((a:int ,b:int ),j:num0,k:num0) = ax1(b-a+_1,fun i -> this.[i+a-1,j,k])
        member this.Item with get((a:int ,b:int ),j:num0,k:int ) = ax1(b-a+_1,fun i -> this.[i+a-1,j,k])
        member this.Item with get((a:int ,b:int ),j:int ,k:num0) = ax1(b-a+_1,fun i -> this.[i+a-1,j,k])
        member this.Item with get((a:int ,b:int ),j:int ,k:int ) = ax1(b-a+_1,fun i -> this.[i+a-1,j,k])
        member this.Item with get(i:num0,(a:num0,b:num0),k:num0) = ax1(b-a+_1,fun j -> this.[i,j+a-1,k])
        member this.Item with get(i:num0,(a:num0,b:num0),k:int ) = ax1(b-a+_1,fun j -> this.[i,j+a-1,k])
        member this.Item with get(i:int ,(a:num0,b:num0),k:num0) = ax1(b-a+_1,fun j -> this.[i,j+a-1,k])
        member this.Item with get(i:int ,(a:num0,b:num0),k:int ) = ax1(b-a+_1,fun j -> this.[i,j+a-1,k])
        member this.Item with get(i:num0,(a:num0,b:int ),k:num0) = ax1(b-a+_1,fun j -> this.[i,j+a-1,k])
        member this.Item with get(i:num0,(a:num0,b:int ),k:int ) = ax1(b-a+_1,fun j -> this.[i,j+a-1,k])
        member this.Item with get(i:int ,(a:num0,b:int ),k:num0) = ax1(b-a+_1,fun j -> this.[i,j+a-1,k])
        member this.Item with get(i:int ,(a:num0,b:int ),k:int ) = ax1(b-a+_1,fun j -> this.[i,j+a-1,k])
        member this.Item with get(i:num0,(a:int ,b:num0),k:num0) = ax1(b-a+_1,fun j -> this.[i,j+a-1,k])
        member this.Item with get(i:num0,(a:int ,b:num0),k:int ) = ax1(b-a+_1,fun j -> this.[i,j+a-1,k])
        member this.Item with get(i:int ,(a:int ,b:num0),k:num0) = ax1(b-a+_1,fun j -> this.[i,j+a-1,k])
        member this.Item with get(i:int ,(a:int ,b:num0),k:int ) = ax1(b-a+_1,fun j -> this.[i,j+a-1,k])
        member this.Item with get(i:num0,(a:int ,b:int ),k:num0) = ax1(b-a+_1,fun j -> this.[i,j+a-1,k])
        member this.Item with get(i:num0,(a:int ,b:int ),k:int ) = ax1(b-a+_1,fun j -> this.[i,j+a-1,k])
        member this.Item with get(i:int ,(a:int ,b:int ),k:num0) = ax1(b-a+_1,fun j -> this.[i,j+a-1,k])
        member this.Item with get(i:int ,(a:int ,b:int ),k:int ) = ax1(b-a+_1,fun j -> this.[i,j+a-1,k])
        member this.Item with get(i:num0,j:num0,(a:num0,b:num0)) = ax1(b-a+_1,fun k -> this.[i,j,k+a-1])
        member this.Item with get(i:num0,j:int ,(a:num0,b:num0)) = ax1(b-a+_1,fun k -> this.[i,j,k+a-1])
        member this.Item with get(i:int ,j:num0,(a:num0,b:num0)) = ax1(b-a+_1,fun k -> this.[i,j,k+a-1])
        member this.Item with get(i:int ,j:int ,(a:num0,b:num0)) = ax1(b-a+_1,fun k -> this.[i,j,k+a-1])
        member this.Item with get(i:num0,j:num0,(a:num0,b:int )) = ax1(b-a+_1,fun k -> this.[i,j,k+a-1])
        member this.Item with get(i:num0,j:int ,(a:num0,b:int )) = ax1(b-a+_1,fun k -> this.[i,j,k+a-1])
        member this.Item with get(i:int ,j:num0,(a:num0,b:int )) = ax1(b-a+_1,fun k -> this.[i,j,k+a-1])
        member this.Item with get(i:int ,j:int ,(a:num0,b:int )) = ax1(b-a+_1,fun k -> this.[i,j,k+a-1])
        member this.Item with get(i:num0,j:num0,(a:int ,b:num0)) = ax1(b-a+_1,fun k -> this.[i,j,k+a-1])
        member this.Item with get(i:num0,j:int ,(a:int ,b:num0)) = ax1(b-a+_1,fun k -> this.[i,j,k+a-1])
        member this.Item with get(i:int ,j:num0,(a:int ,b:num0)) = ax1(b-a+_1,fun k -> this.[i,j,k+a-1])
        member this.Item with get(i:int ,j:int ,(a:int ,b:num0)) = ax1(b-a+_1,fun k -> this.[i,j,k+a-1])
        member this.Item with get(i:num0,j:num0,(a:int ,b:int )) = ax1(b-a+_1,fun k -> this.[i,j,k+a-1])
        member this.Item with get(i:num0,j:int ,(a:int ,b:int )) = ax1(b-a+_1,fun k -> this.[i,j,k+a-1])
        member this.Item with get(i:int ,j:num0,(a:int ,b:int )) = ax1(b-a+_1,fun k -> this.[i,j,k+a-1])
        member this.Item with get(i:int ,j:int ,(a:int ,b:int )) = ax1(b-a+_1,fun k -> this.[i,j,k+a-1])
        member this.Item with get(_:unit,_:unit,k:num0) = ax2(this.size1,this.size2,fun (i,j) -> this.[i,j,k])
        member this.Item with get(_:unit,_:unit,k:int ) = ax2(this.size1,this.size2,fun (i,j) -> this.[i,j,k])
        member this.Item with get(_:unit,j:num0,_:unit) = ax2(this.size1,this.size3,fun (i,k) -> this.[i,j,k])
        member this.Item with get(_:unit,j:int ,_:unit) = ax2(this.size1,this.size3,fun (i,k) -> this.[i,j,k])
        member this.Item with get(i:num0,_:unit,_:unit) = ax2(this.size2,this.size3,fun (j,k) -> this.[i,j,k])
        member this.Item with get(i:int ,_:unit,_:unit) = ax2(this.size2,this.size3,fun (j,k) -> this.[i,j,k])
        member this.Item with get((a:num0,b:num0),_:unit,k:num0) = ax2(b-a+_1,this.size2,fun (i,j) -> this.[i+a-1,j,k])
        member this.Item with get((a:num0,b:int ),_:unit,k:num0) = ax2(b-a+_1,this.size2,fun (i,j) -> this.[i+a-1,j,k])
        member this.Item with get((a:int ,b:num0),_:unit,k:num0) = ax2(b-a+_1,this.size2,fun (i,j) -> this.[i+a-1,j,k])
        member this.Item with get((a:int ,b:int ),_:unit,k:num0) = ax2(b-a+_1,this.size2,fun (i,j) -> this.[i+a-1,j,k])
        member this.Item with get((a:num0,b:num0),_:unit,k:int ) = ax2(b-a+_1,this.size2,fun (i,j) -> this.[i+a-1,j,k])
        member this.Item with get((a:num0,b:int ),_:unit,k:int ) = ax2(b-a+_1,this.size2,fun (i,j) -> this.[i+a-1,j,k])
        member this.Item with get((a:int ,b:num0),_:unit,k:int ) = ax2(b-a+_1,this.size2,fun (i,j) -> this.[i+a-1,j,k])
        member this.Item with get((a:int ,b:int ),_:unit,k:int ) = ax2(b-a+_1,this.size2,fun (i,j) -> this.[i+a-1,j,k])
        member this.Item with get(_:unit,(c:num0,d:num0),k:num0) = ax2(this.size1,d-c+_1,fun (i,j) -> this.[i,j+c-1,k])
        member this.Item with get(_:unit,(c:num0,d:int ),k:num0) = ax2(this.size1,d-c+_1,fun (i,j) -> this.[i,j+c-1,k])
        member this.Item with get(_:unit,(c:int ,d:num0),k:num0) = ax2(this.size1,d-c+_1,fun (i,j) -> this.[i,j+c-1,k])
        member this.Item with get(_:unit,(c:int ,d:int ),k:num0) = ax2(this.size1,d-c+_1,fun (i,j) -> this.[i,j+c-1,k])
        member this.Item with get(_:unit,(c:num0,d:num0),k:int ) = ax2(this.size1,d-c+_1,fun (i,j) -> this.[i,j+c-1,k])
        member this.Item with get(_:unit,(c:num0,d:int ),k:int ) = ax2(this.size1,d-c+_1,fun (i,j) -> this.[i,j+c-1,k])
        member this.Item with get(_:unit,(c:int ,d:num0),k:int ) = ax2(this.size1,d-c+_1,fun (i,j) -> this.[i,j+c-1,k])
        member this.Item with get(_:unit,(c:int ,d:int ),k:int ) = ax2(this.size1,d-c+_1,fun (i,j) -> this.[i,j+c-1,k])
        member this.Item with get((a:num0,b:num0),j:num0,_:unit) = ax2(b-a+_1,this.size3,fun (i,k) -> this.[i+a-1,j,k])
        member this.Item with get((a:num0,b:int ),j:num0,_:unit) = ax2(b-a+_1,this.size3,fun (i,k) -> this.[i+a-1,j,k])
        member this.Item with get((a:int ,b:num0),j:num0,_:unit) = ax2(b-a+_1,this.size3,fun (i,k) -> this.[i+a-1,j,k])
        member this.Item with get((a:int ,b:int ),j:num0,_:unit) = ax2(b-a+_1,this.size3,fun (i,k) -> this.[i+a-1,j,k])
        member this.Item with get((a:num0,b:num0),j:int ,_:unit) = ax2(b-a+_1,this.size3,fun (i,k) -> this.[i+a-1,j,k])
        member this.Item with get((a:num0,b:int ),j:int ,_:unit) = ax2(b-a+_1,this.size3,fun (i,k) -> this.[i+a-1,j,k])
        member this.Item with get((a:int ,b:num0),j:int ,_:unit) = ax2(b-a+_1,this.size3,fun (i,k) -> this.[i+a-1,j,k])
        member this.Item with get((a:int ,b:int ),j:int ,_:unit) = ax2(b-a+_1,this.size3,fun (i,k) -> this.[i+a-1,j,k])
        member this.Item with get(_:unit,j:num0,(c:num0,d:num0)) = ax2(this.size1,d-c+_1,fun (i,k) -> this.[i,j,k+c-1])
        member this.Item with get(_:unit,j:num0,(c:num0,d:int )) = ax2(this.size1,d-c+_1,fun (i,k) -> this.[i,j,k+c-1])
        member this.Item with get(_:unit,j:num0,(c:int ,d:num0)) = ax2(this.size1,d-c+_1,fun (i,k) -> this.[i,j,k+c-1])
        member this.Item with get(_:unit,j:num0,(c:int ,d:int )) = ax2(this.size1,d-c+_1,fun (i,k) -> this.[i,j,k+c-1])
        member this.Item with get(_:unit,j:int ,(c:num0,d:num0)) = ax2(this.size1,d-c+_1,fun (i,k) -> this.[i,j,k+c-1])
        member this.Item with get(_:unit,j:int ,(c:num0,d:int )) = ax2(this.size1,d-c+_1,fun (i,k) -> this.[i,j,k+c-1])
        member this.Item with get(_:unit,j:int ,(c:int ,d:num0)) = ax2(this.size1,d-c+_1,fun (i,k) -> this.[i,j,k+c-1])
        member this.Item with get(_:unit,j:int ,(c:int ,d:int )) = ax2(this.size1,d-c+_1,fun (i,k) -> this.[i,j,k+c-1])
        member this.Item with get(i:num0,(a:num0,b:num0),_:unit) = ax2(b-a+_1,this.size3,fun (j,k) -> this.[i,j+a-1,k])
        member this.Item with get(i:num0,(a:num0,b:int ),_:unit) = ax2(b-a+_1,this.size3,fun (j,k) -> this.[i,j+a-1,k])
        member this.Item with get(i:num0,(a:int ,b:num0),_:unit) = ax2(b-a+_1,this.size3,fun (j,k) -> this.[i,j+a-1,k])
        member this.Item with get(i:num0,(a:int ,b:int ),_:unit) = ax2(b-a+_1,this.size3,fun (j,k) -> this.[i,j+a-1,k])
        member this.Item with get(i:int ,(a:num0,b:num0),_:unit) = ax2(b-a+_1,this.size3,fun (j,k) -> this.[i,j+a-1,k])
        member this.Item with get(i:int ,(a:num0,b:int ),_:unit) = ax2(b-a+_1,this.size3,fun (j,k) -> this.[i,j+a-1,k])
        member this.Item with get(i:int ,(a:int ,b:num0),_:unit) = ax2(b-a+_1,this.size3,fun (j,k) -> this.[i,j+a-1,k])
        member this.Item with get(i:int ,(a:int ,b:int ),_:unit) = ax2(b-a+_1,this.size3,fun (j,k) -> this.[i,j+a-1,k])
        member this.Item with get(i:num0,_:unit,(c:num0,d:num0)) = ax2(this.size2,d-c+_1,fun (j,k) -> this.[i,j,k+c-1])
        member this.Item with get(i:num0,_:unit,(c:num0,d:int )) = ax2(this.size2,d-c+_1,fun (j,k) -> this.[i,j,k+c-1])
        member this.Item with get(i:num0,_:unit,(c:int ,d:num0)) = ax2(this.size2,d-c+_1,fun (j,k) -> this.[i,j,k+c-1])
        member this.Item with get(i:num0,_:unit,(c:int ,d:int )) = ax2(this.size2,d-c+_1,fun (j,k) -> this.[i,j,k+c-1])
        member this.Item with get(i:int ,_:unit,(c:num0,d:num0)) = ax2(this.size2,d-c+_1,fun (j,k) -> this.[i,j,k+c-1])
        member this.Item with get(i:int ,_:unit,(c:num0,d:int )) = ax2(this.size2,d-c+_1,fun (j,k) -> this.[i,j,k+c-1])
        member this.Item with get(i:int ,_:unit,(c:int ,d:num0)) = ax2(this.size2,d-c+_1,fun (j,k) -> this.[i,j,k+c-1])
        member this.Item with get(i:int ,_:unit,(c:int ,d:int )) = ax2(this.size2,d-c+_1,fun (j,k) -> this.[i,j,k+c-1])
        member this.Item with get((a:num0,b:num0),(c:num0,d:num0),k:num0) = ax2(b-a+_1,d-c+_1,fun (i,j) -> this.[i+a-1,j+c-1,k])
        member this.Item with get((a:num0,b:int ),(c:num0,d:num0),k:num0) = ax2(b-a+_1,d-c+_1,fun (i,j) -> this.[i+a-1,j+c-1,k])
        member this.Item with get((a:int ,b:num0),(c:num0,d:num0),k:num0) = ax2(b-a+_1,d-c+_1,fun (i,j) -> this.[i+a-1,j+c-1,k])
        member this.Item with get((a:int ,b:int ),(c:num0,d:num0),k:num0) = ax2(b-a+_1,d-c+_1,fun (i,j) -> this.[i+a-1,j+c-1,k])
        member this.Item with get((a:num0,b:num0),(c:num0,d:int ),k:num0) = ax2(b-a+_1,d-c+_1,fun (i,j) -> this.[i+a-1,j+c-1,k])
        member this.Item with get((a:num0,b:int ),(c:num0,d:int ),k:num0) = ax2(b-a+_1,d-c+_1,fun (i,j) -> this.[i+a-1,j+c-1,k])
        member this.Item with get((a:int ,b:num0),(c:num0,d:int ),k:num0) = ax2(b-a+_1,d-c+_1,fun (i,j) -> this.[i+a-1,j+c-1,k])
        member this.Item with get((a:int ,b:int ),(c:num0,d:int ),k:num0) = ax2(b-a+_1,d-c+_1,fun (i,j) -> this.[i+a-1,j+c-1,k])
        member this.Item with get((a:num0,b:num0),(c:int ,d:num0),k:num0) = ax2(b-a+_1,d-c+_1,fun (i,j) -> this.[i+a-1,j+c-1,k])
        member this.Item with get((a:num0,b:int ),(c:int ,d:num0),k:num0) = ax2(b-a+_1,d-c+_1,fun (i,j) -> this.[i+a-1,j+c-1,k])
        member this.Item with get((a:int ,b:num0),(c:int ,d:num0),k:num0) = ax2(b-a+_1,d-c+_1,fun (i,j) -> this.[i+a-1,j+c-1,k])
        member this.Item with get((a:int ,b:int ),(c:int ,d:num0),k:num0) = ax2(b-a+_1,d-c+_1,fun (i,j) -> this.[i+a-1,j+c-1,k])
        member this.Item with get((a:num0,b:num0),(c:int ,d:int ),k:num0) = ax2(b-a+_1,d-c+_1,fun (i,j) -> this.[i+a-1,j+c-1,k])
        member this.Item with get((a:num0,b:int ),(c:int ,d:int ),k:num0) = ax2(b-a+_1,d-c+_1,fun (i,j) -> this.[i+a-1,j+c-1,k])
        member this.Item with get((a:int ,b:num0),(c:int ,d:int ),k:num0) = ax2(b-a+_1,d-c+_1,fun (i,j) -> this.[i+a-1,j+c-1,k])
        member this.Item with get((a:int ,b:int ),(c:int ,d:int ),k:num0) = ax2(b-a+_1,d-c+_1,fun (i,j) -> this.[i+a-1,j+c-1,k])
        member this.Item with get((a:num0,b:num0),(c:num0,d:num0),k:int ) = ax2(b-a+_1,d-c+_1,fun (i,j) -> this.[i+a-1,j+c-1,k])
        member this.Item with get((a:num0,b:int ),(c:num0,d:num0),k:int ) = ax2(b-a+_1,d-c+_1,fun (i,j) -> this.[i+a-1,j+c-1,k])
        member this.Item with get((a:int ,b:num0),(c:num0,d:num0),k:int ) = ax2(b-a+_1,d-c+_1,fun (i,j) -> this.[i+a-1,j+c-1,k])
        member this.Item with get((a:int ,b:int ),(c:num0,d:num0),k:int ) = ax2(b-a+_1,d-c+_1,fun (i,j) -> this.[i+a-1,j+c-1,k])
        member this.Item with get((a:num0,b:num0),(c:num0,d:int ),k:int ) = ax2(b-a+_1,d-c+_1,fun (i,j) -> this.[i+a-1,j+c-1,k])
        member this.Item with get((a:num0,b:int ),(c:num0,d:int ),k:int ) = ax2(b-a+_1,d-c+_1,fun (i,j) -> this.[i+a-1,j+c-1,k])
        member this.Item with get((a:int ,b:num0),(c:num0,d:int ),k:int ) = ax2(b-a+_1,d-c+_1,fun (i,j) -> this.[i+a-1,j+c-1,k])
        member this.Item with get((a:int ,b:int ),(c:num0,d:int ),k:int ) = ax2(b-a+_1,d-c+_1,fun (i,j) -> this.[i+a-1,j+c-1,k])
        member this.Item with get((a:num0,b:num0),(c:int ,d:num0),k:int ) = ax2(b-a+_1,d-c+_1,fun (i,j) -> this.[i+a-1,j+c-1,k])
        member this.Item with get((a:num0,b:int ),(c:int ,d:num0),k:int ) = ax2(b-a+_1,d-c+_1,fun (i,j) -> this.[i+a-1,j+c-1,k])
        member this.Item with get((a:int ,b:num0),(c:int ,d:num0),k:int ) = ax2(b-a+_1,d-c+_1,fun (i,j) -> this.[i+a-1,j+c-1,k])
        member this.Item with get((a:int ,b:int ),(c:int ,d:num0),k:int ) = ax2(b-a+_1,d-c+_1,fun (i,j) -> this.[i+a-1,j+c-1,k])
        member this.Item with get((a:num0,b:num0),(c:int ,d:int ),k:int ) = ax2(b-a+_1,d-c+_1,fun (i,j) -> this.[i+a-1,j+c-1,k])
        member this.Item with get((a:num0,b:int ),(c:int ,d:int ),k:int ) = ax2(b-a+_1,d-c+_1,fun (i,j) -> this.[i+a-1,j+c-1,k])
        member this.Item with get((a:int ,b:num0),(c:int ,d:int ),k:int ) = ax2(b-a+_1,d-c+_1,fun (i,j) -> this.[i+a-1,j+c-1,k])
        member this.Item with get((a:int ,b:int ),(c:int ,d:int ),k:int ) = ax2(b-a+_1,d-c+_1,fun (i,j) -> this.[i+a-1,j+c-1,k])
        member this.Item with get((a:num0,b:num0),j:num0,(c:num0,d:num0)) = ax2(b-a+_1,d-c+_1,fun (i,k) -> this.[i+a-1,j,k+c-1])
        member this.Item with get((a:num0,b:int ),j:num0,(c:num0,d:num0)) = ax2(b-a+_1,d-c+_1,fun (i,k) -> this.[i+a-1,j,k+c-1])
        member this.Item with get((a:int ,b:num0),j:num0,(c:num0,d:num0)) = ax2(b-a+_1,d-c+_1,fun (i,k) -> this.[i+a-1,j,k+c-1])
        member this.Item with get((a:int ,b:int ),j:num0,(c:num0,d:num0)) = ax2(b-a+_1,d-c+_1,fun (i,k) -> this.[i+a-1,j,k+c-1])
        member this.Item with get((a:num0,b:num0),j:num0,(c:num0,d:int )) = ax2(b-a+_1,d-c+_1,fun (i,k) -> this.[i+a-1,j,k+c-1])
        member this.Item with get((a:num0,b:int ),j:num0,(c:num0,d:int )) = ax2(b-a+_1,d-c+_1,fun (i,k) -> this.[i+a-1,j,k+c-1])
        member this.Item with get((a:int ,b:num0),j:num0,(c:num0,d:int )) = ax2(b-a+_1,d-c+_1,fun (i,k) -> this.[i+a-1,j,k+c-1])
        member this.Item with get((a:int ,b:int ),j:num0,(c:num0,d:int )) = ax2(b-a+_1,d-c+_1,fun (i,k) -> this.[i+a-1,j,k+c-1])
        member this.Item with get((a:num0,b:num0),j:num0,(c:int ,d:num0)) = ax2(b-a+_1,d-c+_1,fun (i,k) -> this.[i+a-1,j,k+c-1])
        member this.Item with get((a:num0,b:int ),j:num0,(c:int ,d:num0)) = ax2(b-a+_1,d-c+_1,fun (i,k) -> this.[i+a-1,j,k+c-1])
        member this.Item with get((a:int ,b:num0),j:num0,(c:int ,d:num0)) = ax2(b-a+_1,d-c+_1,fun (i,k) -> this.[i+a-1,j,k+c-1])
        member this.Item with get((a:int ,b:int ),j:num0,(c:int ,d:num0)) = ax2(b-a+_1,d-c+_1,fun (i,k) -> this.[i+a-1,j,k+c-1])
        member this.Item with get((a:num0,b:num0),j:num0,(c:int ,d:int )) = ax2(b-a+_1,d-c+_1,fun (i,k) -> this.[i+a-1,j,k+c-1])
        member this.Item with get((a:num0,b:int ),j:num0,(c:int ,d:int )) = ax2(b-a+_1,d-c+_1,fun (i,k) -> this.[i+a-1,j,k+c-1])
        member this.Item with get((a:int ,b:num0),j:num0,(c:int ,d:int )) = ax2(b-a+_1,d-c+_1,fun (i,k) -> this.[i+a-1,j,k+c-1])
        member this.Item with get((a:int ,b:int ),j:num0,(c:int ,d:int )) = ax2(b-a+_1,d-c+_1,fun (i,k) -> this.[i+a-1,j,k+c-1])
        member this.Item with get((a:num0,b:num0),j:int ,(c:num0,d:num0)) = ax2(b-a+_1,d-c+_1,fun (i,k) -> this.[i+a-1,j,k+c-1])
        member this.Item with get((a:num0,b:int ),j:int ,(c:num0,d:num0)) = ax2(b-a+_1,d-c+_1,fun (i,k) -> this.[i+a-1,j,k+c-1])
        member this.Item with get((a:int ,b:num0),j:int ,(c:num0,d:num0)) = ax2(b-a+_1,d-c+_1,fun (i,k) -> this.[i+a-1,j,k+c-1])
        member this.Item with get((a:int ,b:int ),j:int ,(c:num0,d:num0)) = ax2(b-a+_1,d-c+_1,fun (i,k) -> this.[i+a-1,j,k+c-1])
        member this.Item with get((a:num0,b:num0),j:int ,(c:num0,d:int )) = ax2(b-a+_1,d-c+_1,fun (i,k) -> this.[i+a-1,j,k+c-1])
        member this.Item with get((a:num0,b:int ),j:int ,(c:num0,d:int )) = ax2(b-a+_1,d-c+_1,fun (i,k) -> this.[i+a-1,j,k+c-1])
        member this.Item with get((a:int ,b:num0),j:int ,(c:num0,d:int )) = ax2(b-a+_1,d-c+_1,fun (i,k) -> this.[i+a-1,j,k+c-1])
        member this.Item with get((a:int ,b:int ),j:int ,(c:num0,d:int )) = ax2(b-a+_1,d-c+_1,fun (i,k) -> this.[i+a-1,j,k+c-1])
        member this.Item with get((a:num0,b:num0),j:int ,(c:int ,d:num0)) = ax2(b-a+_1,d-c+_1,fun (i,k) -> this.[i+a-1,j,k+c-1])
        member this.Item with get((a:num0,b:int ),j:int ,(c:int ,d:num0)) = ax2(b-a+_1,d-c+_1,fun (i,k) -> this.[i+a-1,j,k+c-1])
        member this.Item with get((a:int ,b:num0),j:int ,(c:int ,d:num0)) = ax2(b-a+_1,d-c+_1,fun (i,k) -> this.[i+a-1,j,k+c-1])
        member this.Item with get((a:int ,b:int ),j:int ,(c:int ,d:num0)) = ax2(b-a+_1,d-c+_1,fun (i,k) -> this.[i+a-1,j,k+c-1])
        member this.Item with get((a:num0,b:num0),j:int ,(c:int ,d:int )) = ax2(b-a+_1,d-c+_1,fun (i,k) -> this.[i+a-1,j,k+c-1])
        member this.Item with get((a:num0,b:int ),j:int ,(c:int ,d:int )) = ax2(b-a+_1,d-c+_1,fun (i,k) -> this.[i+a-1,j,k+c-1])
        member this.Item with get((a:int ,b:num0),j:int ,(c:int ,d:int )) = ax2(b-a+_1,d-c+_1,fun (i,k) -> this.[i+a-1,j,k+c-1])
        member this.Item with get((a:int ,b:int ),j:int ,(c:int ,d:int )) = ax2(b-a+_1,d-c+_1,fun (i,k) -> this.[i+a-1,j,k+c-1])
        member this.Item with get(i:num0,(a:num0,b:num0),(c:num0,d:num0)) = ax2(b-a+_1,d-c+_1,fun (j,k) -> this.[i,j+a-1,k+c-1])
        member this.Item with get(i:num0,(a:num0,b:int ),(c:num0,d:num0)) = ax2(b-a+_1,d-c+_1,fun (j,k) -> this.[i,j+a-1,k+c-1])
        member this.Item with get(i:num0,(a:int ,b:num0),(c:num0,d:num0)) = ax2(b-a+_1,d-c+_1,fun (j,k) -> this.[i,j+a-1,k+c-1])
        member this.Item with get(i:num0,(a:int ,b:int ),(c:num0,d:num0)) = ax2(b-a+_1,d-c+_1,fun (j,k) -> this.[i,j+a-1,k+c-1])
        member this.Item with get(i:num0,(a:num0,b:num0),(c:num0,d:int )) = ax2(b-a+_1,d-c+_1,fun (j,k) -> this.[i,j+a-1,k+c-1])
        member this.Item with get(i:num0,(a:num0,b:int ),(c:num0,d:int )) = ax2(b-a+_1,d-c+_1,fun (j,k) -> this.[i,j+a-1,k+c-1])
        member this.Item with get(i:num0,(a:int ,b:num0),(c:num0,d:int )) = ax2(b-a+_1,d-c+_1,fun (j,k) -> this.[i,j+a-1,k+c-1])
        member this.Item with get(i:num0,(a:int ,b:int ),(c:num0,d:int )) = ax2(b-a+_1,d-c+_1,fun (j,k) -> this.[i,j+a-1,k+c-1])
        member this.Item with get(i:num0,(a:num0,b:num0),(c:int ,d:num0)) = ax2(b-a+_1,d-c+_1,fun (j,k) -> this.[i,j+a-1,k+c-1])
        member this.Item with get(i:num0,(a:num0,b:int ),(c:int ,d:num0)) = ax2(b-a+_1,d-c+_1,fun (j,k) -> this.[i,j+a-1,k+c-1])
        member this.Item with get(i:num0,(a:int ,b:num0),(c:int ,d:num0)) = ax2(b-a+_1,d-c+_1,fun (j,k) -> this.[i,j+a-1,k+c-1])
        member this.Item with get(i:num0,(a:int ,b:int ),(c:int ,d:num0)) = ax2(b-a+_1,d-c+_1,fun (j,k) -> this.[i,j+a-1,k+c-1])
        member this.Item with get(i:num0,(a:num0,b:num0),(c:int ,d:int )) = ax2(b-a+_1,d-c+_1,fun (j,k) -> this.[i,j+a-1,k+c-1])
        member this.Item with get(i:num0,(a:num0,b:int ),(c:int ,d:int )) = ax2(b-a+_1,d-c+_1,fun (j,k) -> this.[i,j+a-1,k+c-1])
        member this.Item with get(i:num0,(a:int ,b:num0),(c:int ,d:int )) = ax2(b-a+_1,d-c+_1,fun (j,k) -> this.[i,j+a-1,k+c-1])
        member this.Item with get(i:num0,(a:int ,b:int ),(c:int ,d:int )) = ax2(b-a+_1,d-c+_1,fun (j,k) -> this.[i,j+a-1,k+c-1])
        member this.Item with get(i:int ,(a:num0,b:num0),(c:num0,d:num0)) = ax2(b-a+_1,d-c+_1,fun (j,k) -> this.[i,j+a-1,k+c-1])
        member this.Item with get(i:int ,(a:num0,b:int ),(c:num0,d:num0)) = ax2(b-a+_1,d-c+_1,fun (j,k) -> this.[i,j+a-1,k+c-1])
        member this.Item with get(i:int ,(a:int ,b:num0),(c:num0,d:num0)) = ax2(b-a+_1,d-c+_1,fun (j,k) -> this.[i,j+a-1,k+c-1])
        member this.Item with get(i:int ,(a:int ,b:int ),(c:num0,d:num0)) = ax2(b-a+_1,d-c+_1,fun (j,k) -> this.[i,j+a-1,k+c-1])
        member this.Item with get(i:int ,(a:num0,b:num0),(c:num0,d:int )) = ax2(b-a+_1,d-c+_1,fun (j,k) -> this.[i,j+a-1,k+c-1])
        member this.Item with get(i:int ,(a:num0,b:int ),(c:num0,d:int )) = ax2(b-a+_1,d-c+_1,fun (j,k) -> this.[i,j+a-1,k+c-1])
        member this.Item with get(i:int ,(a:int ,b:num0),(c:num0,d:int )) = ax2(b-a+_1,d-c+_1,fun (j,k) -> this.[i,j+a-1,k+c-1])
        member this.Item with get(i:int ,(a:int ,b:int ),(c:num0,d:int )) = ax2(b-a+_1,d-c+_1,fun (j,k) -> this.[i,j+a-1,k+c-1])
        member this.Item with get(i:int ,(a:num0,b:num0),(c:int ,d:num0)) = ax2(b-a+_1,d-c+_1,fun (j,k) -> this.[i,j+a-1,k+c-1])
        member this.Item with get(i:int ,(a:num0,b:int ),(c:int ,d:num0)) = ax2(b-a+_1,d-c+_1,fun (j,k) -> this.[i,j+a-1,k+c-1])
        member this.Item with get(i:int ,(a:int ,b:num0),(c:int ,d:num0)) = ax2(b-a+_1,d-c+_1,fun (j,k) -> this.[i,j+a-1,k+c-1])
        member this.Item with get(i:int ,(a:int ,b:int ),(c:int ,d:num0)) = ax2(b-a+_1,d-c+_1,fun (j,k) -> this.[i,j+a-1,k+c-1])
        member this.Item with get(i:int ,(a:num0,b:num0),(c:int ,d:int )) = ax2(b-a+_1,d-c+_1,fun (j,k) -> this.[i,j+a-1,k+c-1])
        member this.Item with get(i:int ,(a:num0,b:int ),(c:int ,d:int )) = ax2(b-a+_1,d-c+_1,fun (j,k) -> this.[i,j+a-1,k+c-1])
        member this.Item with get(i:int ,(a:int ,b:num0),(c:int ,d:int )) = ax2(b-a+_1,d-c+_1,fun (j,k) -> this.[i,j+a-1,k+c-1])
        member this.Item with get(i:int ,(a:int ,b:int ),(c:int ,d:int )) = ax2(b-a+_1,d-c+_1,fun (j,k) -> this.[i,j+a-1,k+c-1])
        
        member this.Item with get((a:num0,b:num0),(c:num0,d:num0),(e:num0,f:num0)) = ax3(b-a+_1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i+a-1,j+c-1,k+e-1])
        member this.Item with get((a:num0,b:int ),(c:num0,d:num0),(e:num0,f:num0)) = ax3(b-a+_1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i+a-1,j+c-1,k+e-1])
        member this.Item with get((a:int ,b:num0),(c:num0,d:num0),(e:num0,f:num0)) = ax3(b-a+_1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i+a-1,j+c-1,k+e-1])
        member this.Item with get((a:int ,b:int ),(c:num0,d:num0),(e:num0,f:num0)) = ax3(b-a+_1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i+a-1,j+c-1,k+e-1])
        member this.Item with get((a:num0,b:num0),(c:num0,d:int ),(e:num0,f:num0)) = ax3(b-a+_1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i+a-1,j+c-1,k+e-1])
        member this.Item with get((a:num0,b:int ),(c:num0,d:int ),(e:num0,f:num0)) = ax3(b-a+_1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i+a-1,j+c-1,k+e-1])
        member this.Item with get((a:int ,b:num0),(c:num0,d:int ),(e:num0,f:num0)) = ax3(b-a+_1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i+a-1,j+c-1,k+e-1])
        member this.Item with get((a:int ,b:int ),(c:num0,d:int ),(e:num0,f:num0)) = ax3(b-a+_1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i+a-1,j+c-1,k+e-1])
        member this.Item with get((a:num0,b:num0),(c:int ,d:num0),(e:num0,f:num0)) = ax3(b-a+_1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i+a-1,j+c-1,k+e-1])
        member this.Item with get((a:num0,b:int ),(c:int ,d:num0),(e:num0,f:num0)) = ax3(b-a+_1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i+a-1,j+c-1,k+e-1])
        member this.Item with get((a:int ,b:num0),(c:int ,d:num0),(e:num0,f:num0)) = ax3(b-a+_1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i+a-1,j+c-1,k+e-1])
        member this.Item with get((a:int ,b:int ),(c:int ,d:num0),(e:num0,f:num0)) = ax3(b-a+_1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i+a-1,j+c-1,k+e-1])
        member this.Item with get((a:num0,b:num0),(c:int ,d:int ),(e:num0,f:num0)) = ax3(b-a+_1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i+a-1,j+c-1,k+e-1])
        member this.Item with get((a:num0,b:int ),(c:int ,d:int ),(e:num0,f:num0)) = ax3(b-a+_1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i+a-1,j+c-1,k+e-1])
        member this.Item with get((a:int ,b:num0),(c:int ,d:int ),(e:num0,f:num0)) = ax3(b-a+_1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i+a-1,j+c-1,k+e-1])
        member this.Item with get((a:int ,b:int ),(c:int ,d:int ),(e:num0,f:num0)) = ax3(b-a+_1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i+a-1,j+c-1,k+e-1])
        member this.Item with get((a:num0,b:num0),(c:num0,d:num0),(e:num0,f:int )) = ax3(b-a+_1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i+a-1,j+c-1,k+e-1])
        member this.Item with get((a:num0,b:int ),(c:num0,d:num0),(e:num0,f:int )) = ax3(b-a+_1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i+a-1,j+c-1,k+e-1])
        member this.Item with get((a:int ,b:num0),(c:num0,d:num0),(e:num0,f:int )) = ax3(b-a+_1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i+a-1,j+c-1,k+e-1])
        member this.Item with get((a:int ,b:int ),(c:num0,d:num0),(e:num0,f:int )) = ax3(b-a+_1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i+a-1,j+c-1,k+e-1])
        member this.Item with get((a:num0,b:num0),(c:num0,d:int ),(e:num0,f:int )) = ax3(b-a+_1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i+a-1,j+c-1,k+e-1])
        member this.Item with get((a:num0,b:int ),(c:num0,d:int ),(e:num0,f:int )) = ax3(b-a+_1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i+a-1,j+c-1,k+e-1])
        member this.Item with get((a:int ,b:num0),(c:num0,d:int ),(e:num0,f:int )) = ax3(b-a+_1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i+a-1,j+c-1,k+e-1])
        member this.Item with get((a:int ,b:int ),(c:num0,d:int ),(e:num0,f:int )) = ax3(b-a+_1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i+a-1,j+c-1,k+e-1])
        member this.Item with get((a:num0,b:num0),(c:int ,d:num0),(e:num0,f:int )) = ax3(b-a+_1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i+a-1,j+c-1,k+e-1])
        member this.Item with get((a:num0,b:int ),(c:int ,d:num0),(e:num0,f:int )) = ax3(b-a+_1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i+a-1,j+c-1,k+e-1])
        member this.Item with get((a:int ,b:num0),(c:int ,d:num0),(e:num0,f:int )) = ax3(b-a+_1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i+a-1,j+c-1,k+e-1])
        member this.Item with get((a:int ,b:int ),(c:int ,d:num0),(e:num0,f:int )) = ax3(b-a+_1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i+a-1,j+c-1,k+e-1])
        member this.Item with get((a:num0,b:num0),(c:int ,d:int ),(e:num0,f:int )) = ax3(b-a+_1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i+a-1,j+c-1,k+e-1])
        member this.Item with get((a:num0,b:int ),(c:int ,d:int ),(e:num0,f:int )) = ax3(b-a+_1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i+a-1,j+c-1,k+e-1])
        member this.Item with get((a:int ,b:num0),(c:int ,d:int ),(e:num0,f:int )) = ax3(b-a+_1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i+a-1,j+c-1,k+e-1])
        member this.Item with get((a:int ,b:int ),(c:int ,d:int ),(e:num0,f:int )) = ax3(b-a+_1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i+a-1,j+c-1,k+e-1])
        member this.Item with get((a:num0,b:num0),(c:num0,d:num0),(e:int ,f:num0)) = ax3(b-a+_1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i+a-1,j+c-1,k+e-1])
        member this.Item with get((a:num0,b:int ),(c:num0,d:num0),(e:int ,f:num0)) = ax3(b-a+_1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i+a-1,j+c-1,k+e-1])
        member this.Item with get((a:int ,b:num0),(c:num0,d:num0),(e:int ,f:num0)) = ax3(b-a+_1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i+a-1,j+c-1,k+e-1])
        member this.Item with get((a:int ,b:int ),(c:num0,d:num0),(e:int ,f:num0)) = ax3(b-a+_1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i+a-1,j+c-1,k+e-1])
        member this.Item with get((a:num0,b:num0),(c:num0,d:int ),(e:int ,f:num0)) = ax3(b-a+_1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i+a-1,j+c-1,k+e-1])
        member this.Item with get((a:num0,b:int ),(c:num0,d:int ),(e:int ,f:num0)) = ax3(b-a+_1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i+a-1,j+c-1,k+e-1])
        member this.Item with get((a:int ,b:num0),(c:num0,d:int ),(e:int ,f:num0)) = ax3(b-a+_1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i+a-1,j+c-1,k+e-1])
        member this.Item with get((a:int ,b:int ),(c:num0,d:int ),(e:int ,f:num0)) = ax3(b-a+_1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i+a-1,j+c-1,k+e-1])
        member this.Item with get((a:num0,b:num0),(c:int ,d:num0),(e:int ,f:num0)) = ax3(b-a+_1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i+a-1,j+c-1,k+e-1])
        member this.Item with get((a:num0,b:int ),(c:int ,d:num0),(e:int ,f:num0)) = ax3(b-a+_1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i+a-1,j+c-1,k+e-1])
        member this.Item with get((a:int ,b:num0),(c:int ,d:num0),(e:int ,f:num0)) = ax3(b-a+_1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i+a-1,j+c-1,k+e-1])
        member this.Item with get((a:int ,b:int ),(c:int ,d:num0),(e:int ,f:num0)) = ax3(b-a+_1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i+a-1,j+c-1,k+e-1])
        member this.Item with get((a:num0,b:num0),(c:int ,d:int ),(e:int ,f:num0)) = ax3(b-a+_1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i+a-1,j+c-1,k+e-1])
        member this.Item with get((a:num0,b:int ),(c:int ,d:int ),(e:int ,f:num0)) = ax3(b-a+_1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i+a-1,j+c-1,k+e-1])
        member this.Item with get((a:int ,b:num0),(c:int ,d:int ),(e:int ,f:num0)) = ax3(b-a+_1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i+a-1,j+c-1,k+e-1])
        member this.Item with get((a:int ,b:int ),(c:int ,d:int ),(e:int ,f:num0)) = ax3(b-a+_1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i+a-1,j+c-1,k+e-1])
        member this.Item with get((a:num0,b:num0),(c:num0,d:num0),(e:int ,f:int )) = ax3(b-a+_1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i+a-1,j+c-1,k+e-1])
        member this.Item with get((a:num0,b:int ),(c:num0,d:num0),(e:int ,f:int )) = ax3(b-a+_1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i+a-1,j+c-1,k+e-1])
        member this.Item with get((a:int ,b:num0),(c:num0,d:num0),(e:int ,f:int )) = ax3(b-a+_1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i+a-1,j+c-1,k+e-1])
        member this.Item with get((a:int ,b:int ),(c:num0,d:num0),(e:int ,f:int )) = ax3(b-a+_1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i+a-1,j+c-1,k+e-1])
        member this.Item with get((a:num0,b:num0),(c:num0,d:int ),(e:int ,f:int )) = ax3(b-a+_1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i+a-1,j+c-1,k+e-1])
        member this.Item with get((a:num0,b:int ),(c:num0,d:int ),(e:int ,f:int )) = ax3(b-a+_1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i+a-1,j+c-1,k+e-1])
        member this.Item with get((a:int ,b:num0),(c:num0,d:int ),(e:int ,f:int )) = ax3(b-a+_1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i+a-1,j+c-1,k+e-1])
        member this.Item with get((a:int ,b:int ),(c:num0,d:int ),(e:int ,f:int )) = ax3(b-a+_1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i+a-1,j+c-1,k+e-1])
        member this.Item with get((a:num0,b:num0),(c:int ,d:num0),(e:int ,f:int )) = ax3(b-a+_1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i+a-1,j+c-1,k+e-1])
        member this.Item with get((a:num0,b:int ),(c:int ,d:num0),(e:int ,f:int )) = ax3(b-a+_1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i+a-1,j+c-1,k+e-1])
        member this.Item with get((a:int ,b:num0),(c:int ,d:num0),(e:int ,f:int )) = ax3(b-a+_1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i+a-1,j+c-1,k+e-1])
        member this.Item with get((a:int ,b:int ),(c:int ,d:num0),(e:int ,f:int )) = ax3(b-a+_1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i+a-1,j+c-1,k+e-1])
        member this.Item with get((a:num0,b:num0),(c:int ,d:int ),(e:int ,f:int )) = ax3(b-a+_1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i+a-1,j+c-1,k+e-1])
        member this.Item with get((a:num0,b:int ),(c:int ,d:int ),(e:int ,f:int )) = ax3(b-a+_1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i+a-1,j+c-1,k+e-1])
        member this.Item with get((a:int ,b:num0),(c:int ,d:int ),(e:int ,f:int )) = ax3(b-a+_1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i+a-1,j+c-1,k+e-1])
        member this.Item with get((a:int ,b:int ),(c:int ,d:int ),(e:int ,f:int )) = ax3(b-a+_1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i+a-1,j+c-1,k+e-1])

        member this.Item with get(_:unit,(c:num0,d:num0),(e:num0,f:num0)) = ax3(this.size1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i,j+c-1,k+e-1])
        member this.Item with get(_:unit,(c:num0,d:int ),(e:num0,f:num0)) = ax3(this.size1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i,j+c-1,k+e-1])
        member this.Item with get(_:unit,(c:int ,d:num0),(e:num0,f:num0)) = ax3(this.size1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i,j+c-1,k+e-1])
        member this.Item with get(_:unit,(c:int ,d:int ),(e:num0,f:num0)) = ax3(this.size1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i,j+c-1,k+e-1])
        member this.Item with get(_:unit,(c:num0,d:num0),(e:num0,f:int )) = ax3(this.size1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i,j+c-1,k+e-1])
        member this.Item with get(_:unit,(c:num0,d:int ),(e:num0,f:int )) = ax3(this.size1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i,j+c-1,k+e-1])
        member this.Item with get(_:unit,(c:int ,d:num0),(e:num0,f:int )) = ax3(this.size1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i,j+c-1,k+e-1])
        member this.Item with get(_:unit,(c:int ,d:int ),(e:num0,f:int )) = ax3(this.size1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i,j+c-1,k+e-1])
        member this.Item with get(_:unit,(c:num0,d:num0),(e:int ,f:num0)) = ax3(this.size1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i,j+c-1,k+e-1])
        member this.Item with get(_:unit,(c:num0,d:int ),(e:int ,f:num0)) = ax3(this.size1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i,j+c-1,k+e-1])
        member this.Item with get(_:unit,(c:int ,d:num0),(e:int ,f:num0)) = ax3(this.size1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i,j+c-1,k+e-1])
        member this.Item with get(_:unit,(c:int ,d:int ),(e:int ,f:num0)) = ax3(this.size1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i,j+c-1,k+e-1])
        member this.Item with get(_:unit,(c:num0,d:num0),(e:int ,f:int )) = ax3(this.size1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i,j+c-1,k+e-1])
        member this.Item with get(_:unit,(c:num0,d:int ),(e:int ,f:int )) = ax3(this.size1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i,j+c-1,k+e-1])
        member this.Item with get(_:unit,(c:int ,d:num0),(e:int ,f:int )) = ax3(this.size1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i,j+c-1,k+e-1])
        member this.Item with get(_:unit,(c:int ,d:int ),(e:int ,f:int )) = ax3(this.size1,d-c+_1,f-e+_1,fun (i,j,k) -> this.[i,j+c-1,k+e-1])
        member this.Item with get((a:num0,b:num0),_:unit,(e:num0,f:num0)) = ax3(b-a+_1,this.size2,f-e+_1,fun (i,j,k) -> this.[i+a-1,j,k+e-1])
        member this.Item with get((a:num0,b:int ),_:unit,(e:num0,f:num0)) = ax3(b-a+_1,this.size2,f-e+_1,fun (i,j,k) -> this.[i+a-1,j,k+e-1])
        member this.Item with get((a:int ,b:num0),_:unit,(e:num0,f:num0)) = ax3(b-a+_1,this.size2,f-e+_1,fun (i,j,k) -> this.[i+a-1,j,k+e-1])
        member this.Item with get((a:int ,b:int ),_:unit,(e:num0,f:num0)) = ax3(b-a+_1,this.size2,f-e+_1,fun (i,j,k) -> this.[i+a-1,j,k+e-1])
        member this.Item with get((a:num0,b:num0),_:unit,(e:num0,f:int )) = ax3(b-a+_1,this.size2,f-e+_1,fun (i,j,k) -> this.[i+a-1,j,k+e-1])
        member this.Item with get((a:num0,b:int ),_:unit,(e:num0,f:int )) = ax3(b-a+_1,this.size2,f-e+_1,fun (i,j,k) -> this.[i+a-1,j,k+e-1])
        member this.Item with get((a:int ,b:num0),_:unit,(e:num0,f:int )) = ax3(b-a+_1,this.size2,f-e+_1,fun (i,j,k) -> this.[i+a-1,j,k+e-1])
        member this.Item with get((a:int ,b:int ),_:unit,(e:num0,f:int )) = ax3(b-a+_1,this.size2,f-e+_1,fun (i,j,k) -> this.[i+a-1,j,k+e-1])
        member this.Item with get((a:num0,b:num0),_:unit,(e:int ,f:num0)) = ax3(b-a+_1,this.size2,f-e+_1,fun (i,j,k) -> this.[i+a-1,j,k+e-1])
        member this.Item with get((a:num0,b:int ),_:unit,(e:int ,f:num0)) = ax3(b-a+_1,this.size2,f-e+_1,fun (i,j,k) -> this.[i+a-1,j,k+e-1])
        member this.Item with get((a:int ,b:num0),_:unit,(e:int ,f:num0)) = ax3(b-a+_1,this.size2,f-e+_1,fun (i,j,k) -> this.[i+a-1,j,k+e-1])
        member this.Item with get((a:int ,b:int ),_:unit,(e:int ,f:num0)) = ax3(b-a+_1,this.size2,f-e+_1,fun (i,j,k) -> this.[i+a-1,j,k+e-1])
        member this.Item with get((a:num0,b:num0),_:unit,(e:int ,f:int )) = ax3(b-a+_1,this.size2,f-e+_1,fun (i,j,k) -> this.[i+a-1,j,k+e-1])
        member this.Item with get((a:num0,b:int ),_:unit,(e:int ,f:int )) = ax3(b-a+_1,this.size2,f-e+_1,fun (i,j,k) -> this.[i+a-1,j,k+e-1])
        member this.Item with get((a:int ,b:num0),_:unit,(e:int ,f:int )) = ax3(b-a+_1,this.size2,f-e+_1,fun (i,j,k) -> this.[i+a-1,j,k+e-1])
        member this.Item with get((a:int ,b:int ),_:unit,(e:int ,f:int )) = ax3(b-a+_1,this.size2,f-e+_1,fun (i,j,k) -> this.[i+a-1,j,k+e-1])
        member this.Item with get((a:num0,b:num0),(c:num0,d:num0),_:unit) = ax3(b-a+_1,d-c+_1,this.size3,fun (i,j,k) -> this.[i+a-1,j+c-1,k])
        member this.Item with get((a:num0,b:int ),(c:num0,d:num0),_:unit) = ax3(b-a+_1,d-c+_1,this.size3,fun (i,j,k) -> this.[i+a-1,j+c-1,k])
        member this.Item with get((a:int ,b:num0),(c:num0,d:num0),_:unit) = ax3(b-a+_1,d-c+_1,this.size3,fun (i,j,k) -> this.[i+a-1,j+c-1,k])
        member this.Item with get((a:int ,b:int ),(c:num0,d:num0),_:unit) = ax3(b-a+_1,d-c+_1,this.size3,fun (i,j,k) -> this.[i+a-1,j+c-1,k])
        member this.Item with get((a:num0,b:num0),(c:num0,d:int ),_:unit) = ax3(b-a+_1,d-c+_1,this.size3,fun (i,j,k) -> this.[i+a-1,j+c-1,k])
        member this.Item with get((a:num0,b:int ),(c:num0,d:int ),_:unit) = ax3(b-a+_1,d-c+_1,this.size3,fun (i,j,k) -> this.[i+a-1,j+c-1,k])
        member this.Item with get((a:int ,b:num0),(c:num0,d:int ),_:unit) = ax3(b-a+_1,d-c+_1,this.size3,fun (i,j,k) -> this.[i+a-1,j+c-1,k])
        member this.Item with get((a:int ,b:int ),(c:num0,d:int ),_:unit) = ax3(b-a+_1,d-c+_1,this.size3,fun (i,j,k) -> this.[i+a-1,j+c-1,k])
        member this.Item with get((a:num0,b:num0),(c:int ,d:num0),_:unit) = ax3(b-a+_1,d-c+_1,this.size3,fun (i,j,k) -> this.[i+a-1,j+c-1,k])
        member this.Item with get((a:num0,b:int ),(c:int ,d:num0),_:unit) = ax3(b-a+_1,d-c+_1,this.size3,fun (i,j,k) -> this.[i+a-1,j+c-1,k])
        member this.Item with get((a:int ,b:num0),(c:int ,d:num0),_:unit) = ax3(b-a+_1,d-c+_1,this.size3,fun (i,j,k) -> this.[i+a-1,j+c-1,k])
        member this.Item with get((a:int ,b:int ),(c:int ,d:num0),_:unit) = ax3(b-a+_1,d-c+_1,this.size3,fun (i,j,k) -> this.[i+a-1,j+c-1,k])
        member this.Item with get((a:num0,b:num0),(c:int ,d:int ),_:unit) = ax3(b-a+_1,d-c+_1,this.size3,fun (i,j,k) -> this.[i+a-1,j+c-1,k])
        member this.Item with get((a:num0,b:int ),(c:int ,d:int ),_:unit) = ax3(b-a+_1,d-c+_1,this.size3,fun (i,j,k) -> this.[i+a-1,j+c-1,k])
        member this.Item with get((a:int ,b:num0),(c:int ,d:int ),_:unit) = ax3(b-a+_1,d-c+_1,this.size3,fun (i,j,k) -> this.[i+a-1,j+c-1,k])
        member this.Item with get((a:int ,b:int ),(c:int ,d:int ),_:unit) = ax3(b-a+_1,d-c+_1,this.size3,fun (i,j,k) -> this.[i+a-1,j+c-1,k])

        member this.Item with get((a:num0,b:num0),_:unit,_:unit) = ax3(b-a+_1,this.size2,this.size3,fun (i,j,k) -> this.[i+a-1,j,k])
        member this.Item with get((a:num0,b:int ),_:unit,_:unit) = ax3(b-a+_1,this.size2,this.size3,fun (i,j,k) -> this.[i+a-1,j,k])
        member this.Item with get((a:int ,b:num0),_:unit,_:unit) = ax3(b-a+_1,this.size2,this.size3,fun (i,j,k) -> this.[i+a-1,j,k])
        member this.Item with get((a:int ,b:int ),_:unit,_:unit) = ax3(b-a+_1,this.size2,this.size3,fun (i,j,k) -> this.[i+a-1,j,k])
        member this.Item with get(_:unit,(c:num0,d:num0),_:unit) = ax3(this.size1,d-c+_1,this.size3,fun (i,j,k) -> this.[i,j+c-1,k])
        member this.Item with get(_:unit,(c:num0,d:int ),_:unit) = ax3(this.size1,d-c+_1,this.size3,fun (i,j,k) -> this.[i,j+c-1,k])
        member this.Item with get(_:unit,(c:int ,d:num0),_:unit) = ax3(this.size1,d-c+_1,this.size3,fun (i,j,k) -> this.[i,j+c-1,k])
        member this.Item with get(_:unit,(c:int ,d:int ),_:unit) = ax3(this.size1,d-c+_1,this.size3,fun (i,j,k) -> this.[i,j+c-1,k])
        member this.Item with get(_:unit,_:unit,(e:num0,f:num0)) = ax3(this.size1,this.size2,f-e+_1,fun (i,j,k) -> this.[i,j,k+e-1])
        member this.Item with get(_:unit,_:unit,(e:num0,f:int )) = ax3(this.size1,this.size2,f-e+_1,fun (i,j,k) -> this.[i,j,k+e-1])
        member this.Item with get(_:unit,_:unit,(e:int ,f:num0)) = ax3(this.size1,this.size2,f-e+_1,fun (i,j,k) -> this.[i,j,k+e-1])
        member this.Item with get(_:unit,_:unit,(e:int ,f:int )) = ax3(this.size1,this.size2,f-e+_1,fun (i,j,k) -> this.[i,j,k+e-1])


        static member (<==) (v1:num3,v2:num3) =
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
                br.branch <| fun b ->
                    b.IF (v1.size3 .=/ v2.size3) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '<==' array size 3 mismatch: "+v1.name+","+v2.name)]
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
                        iter.num v1.size3 <| fun k ->
                            v1.[i,j,k] <== v2.[i,j,k]
              |H ->
                let u1 = v1.name
                let u2 = v2.name
                num0.type_subst_warning(v1.etype,v2.etype)
                p.codewrite("<math>" + u1 + "<mo>&larr;</mo>" + u2 + "</math>\n<br/>\n")
              |NL ->
                ()
                
        static member (<==) (v1:num3,v2:num0) =
            let p = p.param
            match p.lang with
              |F|T ->
                let u1 = v1.name
                let (_,u2,c2) = v2.code.str
                p.codewrite(u1 + " = " + u2)
                //一時変数を削除
                c2 |> List.iter (fun c -> match c with |Var(t,n,_) -> p.dispose(t,n) |_ -> ())
              |C89|C99 ->
                iter.num v1.size1 <| fun i ->
                    iter.num v1.size2 <| fun j ->
                        iter.num v1.size3 <| fun k ->
                            v1.[i,j,k] <== v2
              |H ->
                let u1 = v1.name
                let (_,u2,c2) = v2.code.str
                p.codewrite("<math>" + u1 + "<mo>&larr;</mo>" + u2 + "</math>\n<br/>\n")
                //一時変数を削除
                c2 |> List.iter (fun c -> match c with |Var(t,n,_) -> p.dispose(t,n) |_ -> ())
              |NL ->
                ()
        static member (<==) (v1:num3,v2:int) =
            v1 <== (Int_e v2)
        static member (<==) (v1:num3,v2:double) =
            v1 <== (Dbl_e v2)
        ///<summary>値を0で初期化</summary> 
        override this.clear() = 
            this <== 0
        ///<summary>配列サイズ変数をメモリ未割当て状態に初期化</summary>
        override this.sizeinit() = 
            this.size1 <== -1
            this.size2 <== -1
            this.size3 <== -1
        /// <summary>
        /// この変数を関数内変数に変換
        /// </summary>
        /// <param name="cm">コメント</param>
        member __.farg (cm:string) = fun code -> 
            let p = p.param
            p.addarg (typ,size1,name,cm) <| fun (t,v,n) -> code(num3(t,v,n))
        static member (+) (x:num3,y:num3) =
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
                br.branch <| fun b ->
                    b.IF (x.size3 .=/ y.size3) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '+' array size 3 mismatch")]
                p.comment("****************************************************")
            ax3(x.size1,x.size2,x.size3, fun (i,j,k) -> x.[i,j,k]+y.[i,j,k])
        static member (+) (x:num0,y:num3) = ax3(y.size1,y.size2,y.size3, fun (i,j,k) -> x+y.[i,j,k])
        static member (+) (x:num3,y:num0) = ax3(x.size1,x.size2,x.size3, fun (i,j,k) -> x.[i,j,k]+y)
        static member (+) (x:int,y:num3) = (I x) + y
        static member (+) (x:num3,y:int) = x + (I y)
        static member (+) (x:double,y:num3) = (D x) + y
        static member (+) (x:num3,y:double) = x + (D y)
        static member (+) (x:double*double,y:num3) = x + y
        static member (+) (x:num3,y:double*double) = x + y
        
        static member (-) (x:num3,y:num3) =
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
                br.branch <| fun b ->
                    b.IF (x.size3 .=/ y.size3) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '-' array size 3 mismatch")]
                p.comment("****************************************************")
            ax3(x.size1,x.size2,x.size3, fun (i,j,k) -> x.[i,j,k]-y.[i,j,k])
        static member (-) (x:num0,y:num3) = ax3(y.size1,y.size2,y.size3, fun (i,j,k) -> x-y.[i,j,k])
        static member (-) (x:num3,y:num0) = ax3(x.size1,x.size2,x.size3, fun (i,j,k) -> x.[i,j,k]-y)
        static member (-) (x:int,y:num3) = (I x) - y
        static member (-) (x:num3,y:int) = x - (I y)
        static member (-) (x:double,y:num3) = (D x) - y
        static member (-) (x:num3,y:double) = x - (D y)
        static member (-) (x:double*double,y:num3) = x - y
        static member (-) (x:num3,y:double*double) = x - y
        
        static member (*) (x:num3,y:num3) =
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
                br.branch <| fun b ->
                    b.IF (x.size3 .=/ y.size3) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '*' array size 3 mismatch")]
                p.comment("****************************************************")
            ax3(x.size1,x.size2,x.size3, fun (i,j,k) -> x.[i,j,k]*y.[i,j,k])
        static member (*) (x:num0,y:num3) = ax3(y.size1,y.size2,y.size3, fun (i,j,k) -> x*y.[i,j,k])
        static member (*) (x:num3,y:num0) = ax3(x.size1,x.size2,x.size3, fun (i,j,k) -> x.[i,j,k]*y)
        static member (*) (x:int,y:num3) = (I x) * y
        static member (*) (x:num3,y:int) = x * (I y)
        static member (*) (x:double,y:num3) = (D x) * y
        static member (*) (x:num3,y:double) = x * (D y)
        static member (*) (x:double*double,y:num3) = x * y
        static member (*) (x:num3,y:double*double) = x * y
        
        static member (/) (x:num3,y:num3) =
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
                br.branch <| fun b ->
                    b.IF (x.size3 .=/ y.size3) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '/' array size 3 mismatch")]
                p.comment("****************************************************")
            ax3(x.size1,x.size2,x.size3, fun (i,j,k) -> x.[i,j,k]/y.[i,j,k])
        static member (/) (x:num0,y:num3) = ax3(y.size1,y.size2,y.size3, fun (i,j,k) -> x/y.[i,j,k])
        static member (/) (x:num3,y:num0) = ax3(x.size1,x.size2,x.size3, fun (i,j,k) -> x.[i,j,k]/y)
        static member (/) (x:int,y:num3) = (I x) / y
        static member (/) (x:num3,y:int) = x / (I y)
        static member (/) (x:double,y:num3) = (D x) / y
        static member (/) (x:num3,y:double) = x / (D y)
        static member (/) (x:double*double,y:num3) = x / y
        static member (/) (x:num3,y:double*double) = x / y
            
    and ax3(n:num0,m:num0,l:num0,f:(num0*num0*num0)->num0) =
        member _.size1 = n
        member _.size2 = m
        member _.size3 = l
        member _.Item with get(i:num0,j:num0,k:num0) = f(i,j,k)
        member _.Item with get(i:int,j:int,k:int) = f(I i,I j,I k)
        static member (+) (x:ax3,y:ax3) =
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
                br.branch <| fun b ->
                    b.IF (x.size3 .=/ y.size3) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '+' array size 3 mismatch")]
                p.comment("****************************************************")
            ax3(x.size1,x.size2,x.size3,fun (i,j,k) ->x.[i,j,k]+y.[i,j,k])
        static member (+) (x:ax3,y:num3) =
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
                br.branch <| fun b ->
                    b.IF (x.size3 .=/ y.size3) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '+' array size 3 mismatch")]
                p.comment("****************************************************")
            ax3(x.size1,x.size2,x.size3,fun (i,j,k) ->x.[i,j,k]+y.[i,j,k])
        static member (+) (x:num3,y:ax3) =
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
                br.branch <| fun b ->
                    b.IF (x.size3 .=/ y.size3) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '+' array size 3 mismatch")]
                p.comment("****************************************************")
            ax3(x.size1,x.size2,x.size3,fun (i,j,k) ->x.[i,j,k]+y.[i,j,k])
        static member (+) (x:ax3,y:int) =
            ax3(x.size1,x.size2,x.size3,fun (i,j,k) ->x.[i,j,k]+y)
        static member (+) (x:ax3,y:double) =
            ax3(x.size1,x.size2,x.size3,fun (i,j,k) ->x.[i,j,k]+y)
        static member (+) (x:ax3,y:double*double) =
            ax3(x.size1,x.size2,x.size3,fun (i,j,k) ->x.[i,j,k]+y)
        static member (+) (x:int,y:ax3) =
            ax3(y.size1,y.size2,y.size3,fun (i,j,k) ->x+y.[i,j,k])
        static member (+) (x:double,y:ax3) =
            ax3(y.size1,y.size2,y.size3,fun (i,j,k) ->x+y.[i,j,k])
        static member (+) (x:double*double,y:ax3) =
            ax3(y.size1,y.size2,y.size3,fun (i,j,k) ->x+y.[i,j,k])
        static member (-) (x:ax3,y:ax3) =
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
                br.branch <| fun b ->
                    b.IF (x.size3 .=/ y.size3) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '-' array size 3 mismatch")]
                p.comment("****************************************************")
            ax3(x.size1,x.size2,x.size3,fun (i,j,k) ->x.[i,j,k]-y.[i,j,k])
        static member (-) (x:ax3,y:num3) =
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
                br.branch <| fun b ->
                    b.IF (x.size3 .=/ y.size3) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '-' array size 3 mismatch")]
                p.comment("****************************************************")
            ax3(x.size1,x.size2,x.size3,fun (i,j,k) ->x.[i,j,k]-y.[i,j,k])
        static member (-) (x:num3,y:ax3) =
            let p = p.param
            if p.debugmode then
                p.error_code_counter_inc()
                p.comment("***debug array1 access check: "+p.error_code_counter.ToString()+"*****************************")
                br.branch <| fun b ->
                    b.IF (x.size1 .=/ y.size1) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" array size 1 mismatch")]
                br.branch <| fun b ->
                    b.IF (x.size2 .=/ y.size2) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" array size 2 mismatch")]
                br.branch <| fun b ->
                    b.IF (x.size3 .=/ y.size3) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" array size 3 mismatch")]
                p.comment("****************************************************")
            ax3(x.size1,x.size2,x.size3,fun (i,j,k) ->x.[i,j,k]-y.[i,j,k])
        static member (-) (x:ax3,y:int) =
            ax3(x.size1,x.size2,x.size3,fun (i,j,k) ->x.[i,j,k]-y)
        static member (-) (x:ax3,y:double) =
            ax3(x.size1,x.size2,x.size3,fun (i,j,k) ->x.[i,j,k]-y)
        static member (-) (x:ax3,y:double*double) =
            ax3(x.size1,x.size2,x.size3,fun (i,j,k) ->x.[i,j,k]-y)
        static member (-) (x:int,y:ax3) =
            ax3(y.size1,y.size2,y.size3,fun (i,j,k) ->x-y.[i,j,k])
        static member (-) (x:double,y:ax3) =
            ax3(y.size1,y.size2,y.size3,fun (i,j,k) ->x-y.[i,j,k])
        static member (-) (x:double*double,y:ax3) =
            ax3(y.size1,y.size2,y.size3,fun (i,j,k) ->x-y.[i,j,k])
        static member (*) (x:ax3,y:ax3) =
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
                br.branch <| fun b ->
                    b.IF (x.size3 .=/ y.size3) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '*' array size 3 mismatch")]
                p.comment("****************************************************")
            ax3(x.size1,x.size2,x.size3,fun (i,j,k) ->x.[i,j,k]*y.[i,j,k])
        static member (*) (x:ax3,y:num3) =
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
                br.branch <| fun b ->
                    b.IF (x.size3 .=/ y.size3) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '*' array size 3 mismatch")]
                p.comment("****************************************************")
            ax3(x.size1,x.size2,x.size3,fun (i,j,k) ->x.[i,j,k]*y.[i,j,k])
        static member (*) (x:num3,y:ax3) =
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
                br.branch <| fun b ->
                    b.IF (x.size3 .=/ y.size3) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '*' array size 3 mismatch")]
                p.comment("****************************************************")
            ax3(x.size1,x.size2,x.size3,fun (i,j,k) ->x.[i,j,k]*y.[i,j,k])
        static member (*) (x:ax3,y:int) =
            ax3(x.size1,x.size2,x.size3,fun (i,j,k) ->x.[i,j,k]*y)
        static member (*) (x:ax3,y:double) =
            ax3(x.size1,x.size2,x.size3,fun (i,j,k) ->x.[i,j,k]*y)
        static member (*) (x:ax3,y:double*double) =
            ax3(x.size1,x.size2,x.size3,fun (i,j,k) ->x.[i,j,k]*y)
        static member (*) (x:int,y:ax3) =
            ax3(y.size1,y.size2,y.size3,fun (i,j,k) ->x*y.[i,j,k])
        static member (*) (x:double,y:ax3) =
            ax3(y.size1,y.size2,y.size3,fun (i,j,k) ->x*y.[i,j,k])
        static member (*) (x:double*double,y:ax3) =
            ax3(y.size1,y.size2,y.size3,fun (i,j,k) ->x*y.[i,j,k])
        static member (/) (x:ax3,y:ax3) =
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
                br.branch <| fun b ->
                    b.IF (x.size3 .=/ y.size3) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '/' array size 3 mismatch")]
                p.comment("****************************************************")
            ax3(x.size1,x.size2,x.size3,fun (i,j,k) ->x.[i,j,k]/y.[i,j,k])
        static member (/) (x:ax3,y:num3) =
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
                br.branch <| fun b ->
                    b.IF (x.size3 .=/ y.size3) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '/' array size 3 mismatch")]
                p.comment("****************************************************")
            ax3(x.size1,x.size2,x.size3,fun (i,j,k) ->x.[i,j,k]/y.[i,j,k])
        static member (/) (x:num3,y:ax3) =
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
                br.branch <| fun b ->
                    b.IF (x.size3 .=/ y.size3) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '/' array size 3 mismatch")]
                p.comment("****************************************************")
            ax3(x.size1,x.size2,x.size3,fun (i,j,k) ->x.[i,j,k]/y.[i,j,k])
        static member (/) (x:ax3,y:int) =
            ax3(x.size1,x.size2,x.size3,fun (i,j,k) ->x.[i,j,k]/y)
        static member (/) (x:ax3,y:double) =
            ax3(x.size1,x.size2,x.size3,fun (i,j,k) ->x.[i,j,k]/y)
        static member (/) (x:ax3,y:double*double) =
            ax3(x.size1,x.size2,x.size3,fun (i,j,k) ->x.[i,j,k]/y)
        static member (/) (x:int,y:ax3) =
            ax3(y.size1,y.size2,y.size3,fun (i,j,k) ->x/y.[i,j,k])
        static member (/) (x:double,y:ax3) =
            ax3(y.size1,y.size2,y.size3,fun (i,j,k) ->x/y.[i,j,k])
        static member (/) (x:double*double,y:ax3) =
            ax3(y.size1,y.size2,y.size3,fun (i,j,k) ->x/y.[i,j,k])

        static member (<==) (x:num3,y:ax3) =
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
                br.branch <| fun b ->
                    b.IF (x.size3 .=/ y.size3) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '<==' array size 3 mismatch")]
                p.comment("****************************************************")
            iter.num x.size1 <| fun i ->
                iter.num x.size2 <| fun j ->
                    iter.num x.size2 <| fun k ->
                        x.[i,j,k] <== y.[i,j,k]
        static member (<==) (x:ax3,y:num3) =
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
                br.branch <| fun b ->
                    b.IF (x.size3 .=/ y.size3) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '<==' array size 3 mismatch")]
                p.comment("****************************************************")
            iter.num x.size1 <| fun i ->
                iter.num x.size2 <| fun j ->
                    iter.num x.size2 <| fun k ->
                        x.[i,j,k] <== y.[i,j,k]
        static member (<==) (x:ax3,y:ax3) =
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
                br.branch <| fun b ->
                    b.IF (x.size3 .=/ y.size3) <| fun () -> 
                        print.s[Str_e ("ERROR"+p.error_code_counter.ToString()+" operator '<==' array size 3 mismatch")]
                p.comment("****************************************************")
            iter.num x.size1 <| fun i ->
                iter.num x.size2 <| fun j ->
                    iter.num x.size2 <| fun k ->
                        x.[i,j,k] <== y.[i,j,k]
        member this.abs
            with get() =
                ax3(this.size1,this.size2,this.size3,fun (i,j,k) -> this.[i,j,k].abs)
        member this.pow
            with get() =
                ax3(this.size1,this.size2,this.size3,fun (i,j,k) -> this.[i,j,k].pow)
        member this.pha
            with get() =
                ax3(this.size1,this.size2,this.size3,fun (i,j,k) -> this.[i,j,k].pha)

    [<AutoOpen>]
    module asm_ax3 =
        type asm with
            static member pow(x:ax3,y:num0) =
                ax3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.pow(x.[i,j,k],y))
            static member pow(x:ax3,y:int) =
                ax3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.pow(x.[i,j,k],y))
            static member pow(x:ax3,y:double) =
                ax3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.pow(x.[i,j,k],y))
            static member pow(x:ax3,y:double*double) =
                ax3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.pow(x.[i,j,k],y))
            static member sin(x:ax3) =
                ax3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.sin(x.[i,j,k]))
            static member cos(x:ax3) =
                ax3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.cos(x.[i,j,k]))
            static member tan(x:ax3) =
                ax3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.tan(x.[i,j,k]))
            static member asin(x:ax3) =
                ax3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.asin(x.[i,j,k]))
            static member acos(x:ax3) =
                ax3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.acos(x.[i,j,k]))
            static member atan(x:ax3) =
                ax3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.atan(x.[i,j,k]))
            static member atan2(x:ax3,y:ax3) =
                ax3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.atan2(x.[i,j,k],y.[i,j,k]))
            static member exp(x:ax3) =
                ax3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.exp(x.[i,j,k]))
            static member abs(x:ax3) =
                ax3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.abs(x.[i,j,k]))
            static member log(x:ax3) =
                ax3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.log(x.[i,j,k]))
            static member log10(x:ax3) =
                ax3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.log10(x.[i,j,k]))
            static member sqrt(x:ax3) =
                ax3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.sqrt(x.[i,j,k]))
            static member floor(x:ax3) =
                ax3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.floor(x.[i,j,k]))
            static member ceil(x:ax3) =
                ax3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.ceil(x.[i,j,k]))
            static member toint(x:ax3) =
                ax3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.toint(x.[i,j,k]))
            static member todouble(x:ax3) =
                ax3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.todouble(x.[i,j,k]))
            static member conj(x:ax3) =
                ax3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.conj(x.[i,j,k]))
            static member pow(x:num3,y:num0) =
                ax3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.pow(x.[i,j,k],y))
            static member pow(x:num3,y:int) =
                ax3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.pow(x.[i,j,k],y))
            static member pow(x:num3,y:double) =
                ax3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.pow(x.[i,j,k],y))
            static member pow(x:num3,y:double*double) =
                ax3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.pow(x.[i,j,k],y))
            static member sin(x:num3) =
                ax3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.sin(x.[i,j,k]))
            static member cos(x:num3) =
                ax3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.cos(x.[i,j,k]))
            static member tan(x:num3) =
                ax3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.tan(x.[i,j,k]))
            static member asin(x:num3) =
                ax3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.asin(x.[i,j,k]))
            static member acos(x:num3) =
                ax3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.acos(x.[i,j,k]))
            static member atan(x:num3) =
                ax3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.atan(x.[i,j,k]))
            static member atan2(x:num3,y:ax3) =
                ax3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.atan2(x.[i,j,k],y.[i,j,k]))
            static member exp(x:num3) =
                ax3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.exp(x.[i,j,k]))
            static member abs(x:num3) =
                ax3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.abs(x.[i,j,k]))
            static member log(x:num3) =
                ax3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.log(x.[i,j,k]))
            static member log10(x:num3) =
                ax3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.log10(x.[i,j,k]))
            static member sqrt(x:num3) =
                ax3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.sqrt(x.[i,j,k]))
            static member floor(x:num3) =
                ax3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.floor(x.[i,j,k]))
            static member ceil(x:num3) =
                ax3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.ceil(x.[i,j,k]))
            static member toint(x:num3) =
                ax3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.toint(x.[i,j,k]))
            static member todouble(x:num3) =
                ax3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.todouble(x.[i,j,k]))
            static member conj(x:num3) =
                ax3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.conj(x.[i,j,k]))
            
    [<AutoOpen>]
    module num3_ax3 =
        type num3 with
            member this.abs
                with get() =
                    ax3(this.size1,this.size2,this.size3,fun (i,j,k) -> this.[i,j,k].abs)
            member this.pow
                with get() =
                    ax3(this.size1,this.size2,this.size3,fun (i,j,k) -> this.[i,j,k].pow)
            member this.pha
                with get() =
                    ax3(this.size1,this.size2,this.size3,fun (i,j,k) -> this.[i,j,k].pha)
        
    [<AutoOpen>]
    module num3_tbinder =
        ///<summary>数値型の型に従って処理を分岐</summary>
        type tbinder with
            /// <summary>
            /// 指定された式または変数eqが整数型の場合のみcodeを実行
            /// </summary>
            /// <param name="eq"></param>
            static member i(eq:num3) = fun code ->
                match eq.etype with
                  |It _ -> code()
                  |_ -> ()
            /// <summary>
            /// 指定された式または変数eqが実数型の場合のみcodeを実行
            /// </summary>
            /// <param name="eq"></param>
            static member d(eq:num3) = fun code ->
                match eq.etype with
                  |Dt -> code()
                  |_ -> ()
            /// <summary>
            /// 指定された式または変数eqが複素数型の場合のみcodeを実行
            /// </summary>
            /// <param name="eq"></param>
            static member z(eq:num3) = fun code ->
                match eq.etype with
                  |Zt -> code()
                  |_ -> ()
              