(*
Copyright (c) 2022 Jun-ichiro Sugisaka

This software is released under the MIT License.
http://opensource.org/licenses/mit-license.php
*)
namespace Aqualis
    
    open Aqualis_base
    
    module GeneralizedVar =
        
        ///<summary>一時変数の生成と使用</summary>
        type gch () =
            
            ///<summary>指定した型の一時変数を生成</summary>
            static member n e = fun code ->
                match e with 
                |Bt |Nt |St |Structure _ -> printfn "%s: 変数を生成できない型です(gch.n)" <| e.ToString()
                |_ -> ()
                let p = p.param
                let x = 
                    match e with
                    |Zt -> p.z_cache_var.getAutoVar()
                    |Dt -> p.d_cache_var.getAutoVar()
                    |_ -> p.i_cache_var.getAutoVar()
                code <| num0(e,Var x)
                match e with
                |It _ -> p.i_cache_var.setVar(e,A0,x,"")
                |Dt -> p.d_cache_var.setVar(e,A0,x,"")
                |Zt -> p.z_cache_var.setVar(e,A0,x,"")
                |_ -> ()
                
            ///<summary>指定した型の一時変数を生成</summary>
            static member n (v:num0) = fun code ->
                match v.etype with 
                |Bt |Nt |St |Structure _ -> printfn "%s: 変数を生成できない型です(gch.n)" <| v.etype.ToString()
                |_ -> ()
                let p = p.param
                let x = 
                    match v.etype with
                    |Zt -> p.z_cache_var.getAutoVar()
                    |Dt -> p.d_cache_var.getAutoVar()
                    |_ -> p.i_cache_var.getAutoVar()
                code <| num0(v.etype,Var x)
                match v.etype with
                |It _ -> p.i_cache_var.setVar(v.etype,A0,x,"")
                |Dt -> p.d_cache_var.setVar(v.etype,A0,x,"")
                |Zt -> p.z_cache_var.setVar(v.etype,A0,x,"")
                |_ -> ()
                
            ///<summary>指定した型の一時変数を生成</summary>
            static member n (v:num1) = fun code ->
                match v.etype with 
                |Bt |Nt |St |Structure _ -> printfn "%s: 変数を生成できない型です(gch.n)" <| v.etype.ToString()
                |_ -> ()
                let p = p.param
                let x = 
                    match v.etype with
                    |Zt -> p.z_cache_var.getAutoVar()
                    |Dt -> p.d_cache_var.getAutoVar()
                    |_ -> p.i_cache_var.getAutoVar()
                code <| num0(v.etype,Var x)
                match v.etype with
                |It _ -> p.i_cache_var.setVar(v.etype,A0,x,"")
                |Dt -> p.d_cache_var.setVar(v.etype,A0,x,"")
                |Zt -> p.z_cache_var.setVar(v.etype,A0,x,"")
                |_ -> ()
                
            ///<summary>指定した型の一時変数を生成</summary>
            static member n (v:num2) = fun code ->
                match v.etype with 
                |Bt |Nt |St |Structure _ -> printfn "%s: 変数を生成できない型です(gch.n)" <| v.etype.ToString()
                |_ -> ()
                let p = p.param
                let x = 
                    match v.etype with
                    |Zt -> p.z_cache_var.getAutoVar()
                    |Dt -> p.d_cache_var.getAutoVar()
                    |_ -> p.i_cache_var.getAutoVar()
                code <| num0(v.etype,Var x)
                match v.etype with
                |It _ -> p.i_cache_var.setVar(v.etype,A0,x,"")
                |Dt -> p.d_cache_var.setVar(v.etype,A0,x,"")
                |Zt -> p.z_cache_var.setVar(v.etype,A0,x,"")
                |_ -> ()
                
            ///<summary>指定した型の一時変数を生成</summary>
            static member n (v:num3) = fun code ->
                match v.etype with 
                |Bt |Nt |St |Structure _ -> printfn "%s: 変数を生成できない型です(gch.n)" <| v.etype.ToString()
                |_ -> ()
                let p = p.param
                let x = 
                    match v.etype with
                    |Zt -> p.z_cache_var.getAutoVar()
                    |Dt -> p.d_cache_var.getAutoVar()
                    |_ -> p.i_cache_var.getAutoVar()
                code <| num0(v.etype,Var x)
                match v.etype with
                |It _ -> p.i_cache_var.setVar(v.etype,A0,x,"")
                |Dt -> p.d_cache_var.setVar(v.etype,A0,x,"")
                |Zt -> p.z_cache_var.setVar(v.etype,A0,x,"")
                |_ -> ()
                
            ///<summary>num1型1次元配列を生成</summary>
            static member n01 e code = 
                match e with 
                |Bt |Nt |St |Structure _ -> printfn "%s: 変数を生成できない型です(gch.n1)" <| e.ToString()
                |_ -> ()
                let p = p.param
                let x = 
                    match e with
                    |Zt -> p.z1_cache_var.getAutoVar()
                    |Dt -> p.d1_cache_var.getAutoVar()
                    |_ -> p.i1_cache_var.getAutoVar()
                let y = num1(e,Var1(A1(0),x))
                code y
                match e with
                |It _ -> p.i1_cache_var.setVar(e,A1(0),x,"")
                |Dt -> p.d1_cache_var.setVar(e,A1(0),x,"")
                |Zt -> p.z1_cache_var.setVar(e,A1(0),x,"")
                |_ -> ()
                
            ///<summary>num0型1次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
            static member n1 (size1:int0) = fun e code ->
                gch.n01 e <| fun v ->
                    v.allocate(size1)
                    code v
                    v.deallocate()
                    
            ///<summary>num0型1次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
            static member n1 (size1:int) = fun e code ->
                gch.n1 size1.I e code
                
            ///<summary>num0型2次元配列を生成</summary>
            static member n02 e code = 
                match e with 
                |Bt |Nt |St |Structure _ -> printfn "%s: 変数を生成できない型です(gch.n2)" <| e.ToString()
                |_ -> ()
                let p = p.param
                let x = 
                    match e with
                    |Zt -> p.z2_cache_var.getAutoVar()
                    |Dt -> p.d2_cache_var.getAutoVar()
                    |_ -> p.i2_cache_var.getAutoVar()
                let y = num2(e,Var2(A2(0,0),x))
                code y
                match e with
                |It _ -> p.i2_cache_var.setVar(e,A2(0,0),x,"")
                |Dt -> p.d2_cache_var.setVar(e,A2(0,0),x,"")
                |Zt -> p.z2_cache_var.setVar(e,A2(0,0),x,"")
                |_ -> ()
                
            ///<summary>num0型2次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
            static member n2 (size1:int0) = fun (size2:int0) e code -> 
                gch.n02 e <| fun v ->
                    v.allocate(size1,size2)
                    code v
                    v.deallocate()
                    
            ///<summary>num0型2次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
            static member n2 (size1:int) = fun (size2:int) e code -> 
                gch.n2 size1.I size2.I e code
                
            ///<summary>整数型3次元配列を生成</summary>
            static member n03 e code = 
                match e with 
                |Bt |Nt |St |Structure _ -> printfn "%s: 変数を生成できない型です(gch.n3)" <| e.ToString()
                |_ -> ()
                let p = p.param
                let x = 
                    match e with
                    |Zt -> p.z3_cache_var.getAutoVar()
                    |Dt -> p.d3_cache_var.getAutoVar()
                    |_ -> p.i3_cache_var.getAutoVar()
                let y = int3(Var3(A3(0,0,0),x))
                code y
                match e with
                |It _ -> p.i3_cache_var.setVar(It 4,A3(0,0,0),x,"")
                |Dt _ -> p.d3_cache_var.setVar(Dt,A3(0,0,0),x,"")
                |Zt _ -> p.z3_cache_var.setVar(Zt,A3(0,0,0),x,"")
                |_ -> ()
                
            ///<summary>整数型3次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
            static member n3 (size1:int0) = fun (size2:int0) (size3:int0) e code ->
                gch.n03 e <| fun v ->
                    v.allocate(size1,size2,size3)
                    code v
                    v.deallocate()
                    
            ///<summary>整数型3次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
            static member n3 (size1:int) = fun (size2:int) (size3:int) e code ->
                gch.n3 size1.I size2.I size3.I e code
                
    ///<summary>変数宣言</summary>
    type gvar () =
         
        ///<summary>整数型変数</summary>
        ///<param name="name">変数名</param>
        static member n0(e:Etype,name:string) = 
            match e with 
            |Bt |Nt |St |Structure _ -> printfn "%s: 変数を生成できない型です(gvar.n0)" <| e.ToString()
            |_ -> ()
            let name_ = match p.lang with |H -> "<mi mathvariant=\"italic\" class=\""+name+"\">"+name+"</mi>" |_ -> name
            p.param.var.setUniqVarWarning(e,A0,name_,"")
            num0(e,Var(name_))
            
        ///<summary>整数型1次元配列</summary>
        ///<param name="name">変数名</param>
        ///<param name="size1">要素数</param>
        static member n1(e:Etype,name:string,size1:int) = 
            match e with 
            |Bt |Nt |St |Structure _ -> printfn "%s: 変数を生成できない型です(gvar.n1)" <| e.ToString()
            |_ -> ()
            let name_ = match p.lang with |H -> "<mi mathvariant=\"italic\" class=\""+name+"\">"+name+"</mi>" |_ -> name
            num1(e,Var1(A1(size1),name_))
            
        ///<summary>整数型可変長1次元配列</summary>
        ///<param name="name">変数名</param>
        static member n1(e:Etype,name:string) =
            match e with 
            |Bt |Nt |St |Structure _ -> printfn "%s: 変数を生成できない型です(gvar.n1)" <| e.ToString()
            |_ -> ()
            let name_ = match p.lang with |H -> "<mi mathvariant=\"italic\" class=\""+name+"\">"+name+"</mi>" |_ -> name
            num1(e,Var1(A1(0),name_))
            
        ///<summary>整数型2次元配列</summary>
        ///<param name="name">変数名</param>
        ///<param name="size1">要素数1</param>
        ///<param name="size2">要素数2</param>
        static member n2(e:Etype,name:string,size1:int,size2:int) = 
            match e with 
            |Bt |Nt |St |Structure _ -> printfn "%s: 変数を生成できない型です(gvar.n2)" <| e.ToString()
            |_ -> ()
            let name_ = match p.lang with |H -> "<mi mathvariant=\"italic\" class=\""+name+"\">"+name+"</mi>" |_ -> name
            num2(e,Var2(A2(size1,size2),name_))
            
        ///<summary>整数型可変長2次元配列</summary>
        ///<param name="name">変数名</param>
        static member n2(e:Etype,name:string) = 
            match e with 
            |Bt |Nt |St |Structure _ -> printfn "%s: 変数を生成できない型です(gvar.n2)" <| e.ToString()
            |_ -> ()
            let name_ = match p.lang with |H -> "<mi mathvariant=\"italic\" class=\""+name+"\">"+name+"</mi>" |_ -> name
            num2(e,Var2(A2(0,0),name_))
            
        ///<summary>整数型3次元配列</summary>
        ///<param name="name">変数名</param>
        ///<param name="size1">要素数1</param>
        ///<param name="size2">要素数2</param>
        ///<param name="size3">要素数3</param>
        static member n3(e:Etype,name:string,size1:int,size2:int,size3:int) = 
            match e with 
            |Bt |Nt |St |Structure _ -> printfn "%s: 変数を生成できない型です(gvar.n3)" <| e.ToString()
            |_ -> ()
            let name_ = match p.lang with |H -> "<mi mathvariant=\"italic\" class=\""+name+"\">"+name+"</mi>" |_ -> name
            num3(e,Var3(A3(size1,size2,size3),name_))
            
        ///<summary>整数型可変長3次元配列</summary>
        ///<param name="name">変数名</param>
        static member n3(e:Etype,name:string) = 
            match e with 
            |Bt |Nt |St |Structure _ -> printfn "%s: 変数を生成できない型です(gvar.n3)" <| e.ToString()
            |_ -> ()
            let name_ = match p.lang with |H -> "<mi mathvariant=\"italic\" class=\""+name+"\">"+name+"</mi>" |_ -> name
            num3(e,Var3(A3(0,0,0),name_))
            