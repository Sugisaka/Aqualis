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

    ///<summary>変数宣言</summary>
    type var () =
        
        ///<summary>複素数の初期値</summary>
        static member private init_z (re,im) =
            let p = p.param
            match p.lang with
              |F ->
                "("+(p.DtoS re)+","+(p.DtoS im)+")"
              |C89 ->
                "{"+(p.DtoS re)+","+(p.DtoS im)+"}"
              |C99 ->
                if im<0.0 then
                    (p.DtoS re)+"-uj*"+(p.DtoS (abs im))
                else
                    (p.DtoS re)+"+uj*"+(p.DtoS im)
              |T ->
                "("+(p.DtoS re)+(if im>=0.0 then "+" else "")+(p.DtoS im)+"{\\rm j})"
              |H ->
                "("+(p.DtoS re)+(if im>=0.0 then "+" else "")+(p.DtoS im)+"&ImaginaryI;)"
              |NL ->
                ""
                
        ///<summary>整数型配列の初期値</summary>
        static member private init_i1 (v:int list) =
            let p = p.param
            match p.lang with
              |F ->
                "(/"+(List.fold (fun acc i -> acc+(if i<>0 then "," else "")+(p.ItoS v.[i])) "" [0..v.Length-1])+"/)"
              |C89 ->
                "{"+(List.fold (fun acc i -> acc+(if i<>0 then "," else "")+(p.ItoS v.[i])) "" [0..v.Length-1])+"}"
              |C99 ->
                "{"+(List.fold (fun acc i -> acc+(if i<>0 then "," else "")+(p.ItoS v.[i])) "" [0..v.Length-1])+"}"
              |T ->
                "{"+(List.fold (fun acc i -> acc+(if i<>0 then "," else "")+(p.ItoS v.[i])) "" [0..v.Length-1])+"}"
              |H ->
                "{"+(List.fold (fun acc i -> acc+(if i<>0 then "," else "")+(p.ItoS v.[i])) "" [0..v.Length-1])+"}"
              |NL ->
                ""
                
        ///<summary>倍精度浮動小数点型配列の初期値</summary>
        static member private init_d1 (v:double list) =
            let p = p.param
            match p.lang with
              |F ->
                "(/"+(List.fold (fun acc i -> acc+(if i<>0 then "," else "")+(p.DtoS v.[i])) "" [0..v.Length-1])+"/)"
              |C89 ->
                "{"+(List.fold (fun acc i -> acc+(if i<>0 then "," else "")+(p.DtoS v.[i])) "" [0..v.Length-1])+"}"
              |C99 ->
                "{"+(List.fold (fun acc i -> acc+(if i<>0 then "," else "")+(p.DtoS v.[i])) "" [0..v.Length-1])+"}"
              |T ->
                "{"+(List.fold (fun acc i -> acc+(if i<>0 then "," else "")+(p.DtoS v.[i])) "" [0..v.Length-1])+"}"
              |H ->
                "{"+(List.fold (fun acc i -> acc+(if i<>0 then "," else "")+(p.DtoS v.[i])) "" [0..v.Length-1])+"}"
              |NL ->
                ""
                
        ///<summary>複素数型配列の初期値</summary>
        static member private init_z1 (v:(double*double) list) =
            let p = p.param
            match p.lang with
              |F ->
                "(/"+(List.fold (fun acc i -> acc+(if i<>0 then "," else "")+(var.init_z v.[i])) "" [0..v.Length-1])+"/)"
              |C89 ->
                "{"+(List.fold (fun acc i -> acc+(if i<>0 then "," else "")+(var.init_z v.[i])) "" [0..v.Length-1])+"}"
              |C99 ->
                "{"+(List.fold (fun acc i -> acc+(if i<>0 then "," else "")+(var.init_z v.[i])) "" [0..v.Length-1])+"}"
              |T ->
                "{"+(List.fold (fun acc i -> acc+(if i<>0 then "," else "")+(var.init_z v.[i])) "" [0..v.Length-1])+"}"
              |H ->
                "{"+(List.fold (fun acc i -> acc+(if i<>0 then "," else "")+(var.init_z v.[i])) "" [0..v.Length-1])+"}"
              |NL ->
                ""
    
        ///<summary>バイト型変数</summary>
        ///<param name="name">変数名</param>
        static member b0 (name:string) = 
            let name_ = match p.lang with |H -> "<mi mathvariant=\"italic\" class=\""+name+"\">"+name+"</mi>" |_ -> name
            p.param.vreg(It 1,A0,name_,"")
            Var(It 1,name_,[])
            
        ///<summary>バイト型変数</summary>
        static member b0 () = 
            let name_ = p.param.i_name()
            p.param.vreg(It 1,A0,name_,"")
            Var(It 1,name_,[])
            
        ///<summary>整数型変数</summary>
        ///<param name="name">変数名</param>
        static member i0 (name:string) = 
            let name_ = match p.lang with |H -> "<mi mathvariant=\"italic\" class=\""+name+"\">"+name+"</mi>" |_ -> name
            p.param.vreg(It 4,A0,name_,"")
            Var(It 4,name_,[])
            
        ///<summary>整数型変数</summary>
        static member i0 () = 
            let name_ = p.param.i_name()
            p.param.vreg(It 4,A0,name_,"")
            Var(It 4,name_,[])
            
        ///<summary>倍精度浮動小数点型変数</summary>
        ///<param name="name">変数名</param>
        static member d0 (name:string) =
            let name_ = match p.lang with |H -> "<mi mathvariant=\"italic\" class=\""+name+"\">"+name+"</mi>" |_ -> name
            p.param.vreg(Dt,A0,name_,"")
            Var(Dt,name_,[])
            
        ///<summary>倍精度浮動小数点型変数</summary>
        static member d0 () = 
            let name_ = p.param.d_name()
            p.param.vreg(Dt,A0,name_,"")
            Var(Dt,name_,[])
            
        ///<summary>複素数型変数</summary>
        ///<param name="name">変数名</param>
        static member z0 (name:string) = 
            let name_ = match p.lang with |H -> "<mi mathvariant=\"italic\" class=\""+name+"\">"+name+"</mi>" |_ -> name
            p.param.vreg(Zt,A0,name_,"")
            Var(Zt,name_,[])
            
        ///<summary>複素数型変数</summary>
        static member z0 () =
            let name_ = p.param.z_name()
            p.param.vreg(Zt,A0,name_,"")
            Var(Zt,name_,[])
            
        ///<summary>整数型1次元配列</summary>
        ///<param name="size1">要素数</param>
        static member i1(size1:int) =
            let p = p.param
            let name = p.i_name()
            num1(It 4,A1(size1),name,"")
            
        ///<summary>整数型1次元配列</summary>
        ///<param name="name">変数名</param>
        ///<param name="size1">要素数</param>
        static member i1(name:string,size1:int) = 
            let name_ = match p.lang with |H -> "<mi mathvariant=\"italic\" class=\""+name+"\">"+name+"</mi>" |_ -> name
            num1(It 4,A1(size1),name_,"")
            
        ///<summary>整数型可変長1次元配列</summary>
        static member i1() =
            let p = p.param
            let name = p.i_name()
            num1(It 4,A1(0),name,"")
            
        ///<summary>整数型可変長1次元配列</summary>
        ///<param name="name">変数名</param>
        static member i1(name:string) =
            let name_ = match p.lang with |H -> "<mi mathvariant=\"italic\" class=\""+name+"\">"+name+"</mi>" |_ -> name
            num1(It 4,A1(0),name_,"")
            
        ///<summary>倍精度浮動小数点型1次元配列</summary>
        ///<param name="size1">要素数</param>
        static member d1(size1:int) =
            let p = p.param
            let name = p.d_name()
            num1(Dt,A1(size1),name,"")
            
        ///<summary>倍精度浮動小数点型1次元配列</summary>
        ///<param name="name">変数名</param>
        ///<param name="size1">要素数</param>
        static member d1(name:string,size1:int) = 
            let name_ = match p.lang with |H -> "<mi mathvariant=\"italic\" class=\""+name+"\">"+name+"</mi>" |_ -> name
            num1(Dt,A1(size1),name_,"")
            
        ///<summary>倍精度浮動小数点型可変長1次元配列</summary>
        static member d1() = 
            let p = p.param
            let name = p.d_name()
            num1(Dt,A1(0),name,"")
            
        ///<summary>倍精度浮動小数点型可変長1次元配列</summary>
        ///<param name="name">変数名</param>
        static member d1(name:string) = 
            let name_ = match p.lang with |H -> "<mi mathvariant=\"italic\" class=\""+name+"\">"+name+"</mi>" |_ -> name
            num1(Dt,A1(0),name_,"")
            
        ///<summary>複素数型1次元配列</summary>
        ///<param name="size1">要素数</param>
        static member z1(size1:int) = 
            let p = p.param
            let name = p.z_name()
            num1(Zt,A1(size1),name,"")
            
        ///<summary>複素数型1次元配列</summary>
        ///<param name="name">変数名</param>
        ///<param name="size1">要素数</param>
        static member z1(name:string,size1:int) = 
            let name_ = match p.lang with |H -> "<mi mathvariant=\"italic\" class=\""+name+"\">"+name+"</mi>" |_ -> name
            num1(Zt,A1(size1),name_,"")
            
        ///<summary>複素数型可変長1次元配列</summary>
        static member z1() = 
            let p = p.param
            let name = p.z_name()
            num1(Zt,A1(0),name,"")
            
        ///<summary>複素数型可変長1次元配列</summary>
        ///<param name="name">変数名</param>
        static member z1(name:string) = 
            let name_ = match p.lang with |H -> "<mi mathvariant=\"italic\" class=\""+name+"\">"+name+"</mi>" |_ -> name
            num1(Zt,A1(0),name_,"")
            
        ///<summary>整数型2次元配列</summary>
        ///<param name="size1">要素数1</param>
        ///<param name="size2">要素数2</param>
        static member i2(size1:int,size2:int) = 
            let p = p.param
            let name = p.i_name()
            num2(It 4,A2(size1,size2),name,"")
            
        ///<summary>整数型2次元配列</summary>
        ///<param name="name">変数名</param>
        ///<param name="size1">要素数1</param>
        ///<param name="size2">要素数2</param>
        static member i2(name:string,size1:int,size2:int) = 
            let name_ = match p.lang with |H -> "<mi mathvariant=\"italic\" class=\""+name+"\">"+name+"</mi>" |_ -> name
            num2(It 4,A2(size1,size2),name_,"")
            
        ///<summary>整数型可変長2次元配列</summary>
        static member i2() = 
            let p = p.param
            let name = p.i_name()
            num2(It 4,A2(0,0),name,"")
            
        ///<summary>整数型可変長2次元配列</summary>
        ///<param name="name">変数名</param>
        static member i2(name:string) = 
            let name_ = match p.lang with |H -> "<mi mathvariant=\"italic\" class=\""+name+"\">"+name+"</mi>" |_ -> name
            num2(It 4,A2(0,0),name_,"")
            
        ///<summary>倍精度浮動小数点型2次元配列</summary>
        ///<param name="size1">要素数1</param>
        ///<param name="size2">要素数2</param>
        static member d2(size1:int,size2:int) = 
            let p = p.param
            let name = p.d_name()
            num2(Dt,A2(size1,size2),name,"")
            
        ///<summary>倍精度浮動小数点型2次元配列</summary>
        ///<param name="name">変数名</param>
        ///<param name="size1">要素数1</param>
        ///<param name="size2">要素数2</param>
        static member d2(name:string,size1:int,size2:int) = 
            let name_ = match p.lang with |H -> "<mi mathvariant=\"italic\" class=\""+name+"\">"+name+"</mi>" |_ -> name
            num2(Dt,A2(size1,size2),name_,"")
            
        ///<summary>倍精度浮動小数点型可変長2次元配列</summary>
        static member d2() = 
            let p = p.param
            let name = p.d_name()
            num2(Dt,A2(0,0),name,"")
            
        ///<summary>倍精度浮動小数点型可変長2次元配列</summary>
        ///<param name="name">変数名</param>
        static member d2(name:string) = 
            let name_ = match p.lang with |H -> "<mi mathvariant=\"italic\" class=\""+name+"\">"+name+"</mi>" |_ -> name
            num2(Dt,A2(0,0),name_,"")
            
        ///<summary>複素数型2次元配列</summary>
        ///<param name="size1">要素数1</param>
        ///<param name="size2">要素数2</param>
        static member z2(size1:int,size2:int) = 
            let p = p.param
            let name = p.z_name()
            num2(Zt,A2(size1,size2),name,"")
            
        ///<summary>複素数型2次元配列</summary>
        ///<param name="name">変数名</param>
        ///<param name="size1">要素数1</param>
        ///<param name="size2">要素数2</param>
        static member z2(name:string,size1:int,size2:int) = 
            let name_ = match p.lang with |H -> "<mi mathvariant=\"italic\" class=\""+name+"\">"+name+"</mi>" |_ -> name
            num2(Zt,A2(size1,size2),name_,"")
            
        ///<summary>複素数型可変長2次元配列</summary>
        static member z2() = 
            let p = p.param
            let name = p.z_name()
            num2(Zt,A2(0,0),name,"")
            
        ///<summary>複素数型可変長2次元配列</summary>
        ///<param name="name">変数名</param>
        static member z2(name:string) = 
            let name_ = match p.lang with |H -> "<mi mathvariant=\"italic\" class=\""+name+"\">"+name+"</mi>" |_ -> name
            num2(Zt,A2(0,0),name_,"")
            
        ///<summary>整数型3次元配列</summary>
        ///<param name="size1">要素数1</param>
        ///<param name="size2">要素数2</param>
        ///<param name="size3">要素数3</param>
        static member i3(size1:int,size2:int,size3:int) = 
            let p = p.param
            let name = p.i_name()
            num3(It 4,A3(size1,size2,size3),name,"")
            
        ///<summary>整数型3次元配列</summary>
        ///<param name="name">変数名</param>
        ///<param name="size1">要素数1</param>
        ///<param name="size2">要素数2</param>
        ///<param name="size3">要素数3</param>
        static member i3(name:string,size1:int,size2:int,size3:int) = 
            let name_ = match p.lang with |H -> "<mi mathvariant=\"italic\" class=\""+name+"\">"+name+"</mi>" |_ -> name
            num3(It 4,A3(size1,size2,size3),name_,"")
            
        ///<summary>整数型可変長3次元配列</summary>
        static member i3() = 
            let p = p.param
            let name = p.i_name()
            num3(It 4,A3(0,0,0),name,"")
            
        ///<summary>整数型可変長3次元配列</summary>
        ///<param name="name">変数名</param>
        static member i3(name:string) = 
            let name_ = match p.lang with |H -> "<mi mathvariant=\"italic\" class=\""+name+"\">"+name+"</mi>" |_ -> name
            num3(It 4,A3(0,0,0),name_,"")
            
        ///<summary>倍精度浮動小数点型3次元配列</summary>
        ///<param name="size1">要素数1</param>
        ///<param name="size2">要素数2</param>
        ///<param name="size3">要素数3</param>
        static member d3(size1:int,size2:int,size3:int) = 
            let p = p.param
            let name = p.d_name()
            num3(Dt,A3(size1,size2,size3),name,"")
            
        ///<summary>倍精度浮動小数点型3次元配列</summary>
        ///<param name="name">変数名</param>
        ///<param name="size1">要素数1</param>
        ///<param name="size2">要素数2</param>
        ///<param name="size3">要素数3</param>
        static member d3(name:string,size1:int,size2:int,size3:int) = 
            let name_ = match p.lang with |H -> "<mi mathvariant=\"italic\" class=\""+name+"\">"+name+"</mi>" |_ -> name
            num3(Dt,A3(size1,size2,size3),name_,"")
            
        ///<summary>倍精度浮動小数点型可変長3次元配列</summary>
        static member d3() = 
            let p = p.param
            let name = p.d_name()
            num3(Dt,A3(0,0,0),name,"")
            
        ///<summary>倍精度浮動小数点型可変長3次元配列</summary>
        ///<param name="name">変数名</param>
        static member d3(name:string) = 
            let name_ = match p.lang with |H -> "<mi mathvariant=\"italic\" class=\""+name+"\">"+name+"</mi>" |_ -> name
            num3(Dt,A3(0,0,0),name_,"")
            
        ///<summary>複素数型3次元配列</summary>
        ///<param name="size1">要素数1</param>
        ///<param name="size2">要素数2</param>
        ///<param name="size3">要素数3</param>
        static member z3(size1:int,size2:int,size3:int) = 
            let p = p.param
            let name = p.z_name()
            num3(Zt,A3(size1,size2,size3),name,"")
            
        ///<summary>複素数型3次元配列</summary>
        ///<param name="name">変数名</param>
        ///<param name="size1">要素数1</param>
        ///<param name="size2">要素数2</param>
        ///<param name="size3">要素数3</param>
        static member z3(name:string,size1:int,size2:int,size3:int) = 
            let name_ = match p.lang with |H -> "<mi mathvariant=\"italic\" class=\""+name+"\">"+name+"</mi>" |_ -> name
            num3(Zt,A3(size1,size2,size3),name_,"")
            
        ///<summary>複素数型可変長3次元配列</summary>
        static member z3() = 
            let p = p.param
            let name = p.z_name()
            num3(Zt,A3(0,0,0),name,"")
            
        ///<summary>複素数型可変長3次元配列</summary>
        ///<param name="name">変数名</param>
        static member z3(name:string) = 
            let name_ = match p.lang with |H -> "<mi mathvariant=\"italic\" class=\""+name+"\">"+name+"</mi>" |_ -> name
            num3(Zt,A3(0,0,0),name_,"")
            
        ///<summary>整数型定数</summary>
        ///<param name="name">変数名</param>
        ///<param name="v">定数</param>
        static member ip0(name:string,v) = 
            let p = p.param
            p.vreg(It 4,A0,name,(p.ItoS v))
            let name_ = match p.lang with |H -> "<mi mathvariant=\"italic\" class=\""+name+"\">"+name+"</mi>" |_ -> name
            Var(It 4,name_,[])
            
        ///<summary>倍精度浮動小数点型定数</summary>
        ///<param name="name">変数名</param>
        ///<param name="v">定数</param>
        static member dp0(name:string,v) = 
            let p = p.param
            p.vreg(Dt,A0,name,(p.DtoS v))
            let name_ = match p.lang with |H -> "<mi mathvariant=\"italic\" class=\""+name+"\">"+name+"</mi>" |_ -> name
            Var(Dt,name_,[])
            
        ///<summary>複素数型定数</summary>
        ///<param name="name">変数名</param>
        ///<param name="v">定数</param>
        static member zp0(name,(v:double*double)) =
            let (v_re,v_im) = v
            p.param.vreg(Zt,A0,name,var.init_z (v_re,v_im))
            let name_ = match p.lang with |H -> "<mi mathvariant=\"italic\" class=\""+name+"\">"+name+"</mi>" |_ -> name
            Var(Zt,name_,[])
            
        ///<summary>整数型1次元配列定数</summary>
        ///<param name="name">変数名</param>
        ///<param name="v">定数</param>
        static member ip1(name:string,v:int list) =
            let name_ = match p.lang with |H -> "<mi mathvariant=\"italic\" class=\""+name+"\">"+name+"</mi>" |_ -> name
            num1(It 4,A1(v.Length),name,(var.init_i1 v))
            
        ///<summary>倍精度浮動小数点型1次元配列定数</summary>
        ///<param name="name">変数名</param>
        ///<param name="v">定数</param>
        static member dp1(name:string,v:double list) = 
            let name_ = match p.lang with |H -> "<mi mathvariant=\"italic\" class=\""+name+"\">"+name+"</mi>" |_ -> name
            num1(Dt,A1(v.Length),name_,(var.init_d1 v))
            
        ///<summary>複素数型1次元配列定数</summary>
        ///<param name="name">変数名</param>
        ///<param name="v">定数</param>
        static member zp1(name:string,v:(double*double) list) =
            let name_ = match p.lang with |H -> "<mi mathvariant=\"italic\" class=\""+name+"\">"+name+"</mi>" |_ -> name
            num1(Zt,A1(v.Length),name_,(var.init_z1 v))
            
