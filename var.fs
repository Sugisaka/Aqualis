(*
Copyright (c) 2022 Jun-ichiro Sugisaka

This software is released under the MIT License.
http://opensource.org/licenses/mit-license.php
*)
namespace Aqualis
    
    open Aqualis_base

    ///<summary>変数宣言</summary>
    type var () =
        
        ///<summary>複素数の初期値</summary>
        static member private init_z (re,im) =
            match p.lang with
            |F ->
                "("+(p.DtoS re)+","+(p.DtoS im)+")"
            |C ->
                if im<0.0 then
                    (p.DtoS re)+"-uj*"+(p.DtoS (abs im))
                else
                    (p.DtoS re)+"+uj*"+(p.DtoS im)
            |T ->
                "("+(p.DtoS re)+(if im>=0.0 then "+" else "")+(p.DtoS im)+"\\mathrm{j})"
            |H ->
                "("+(p.DtoS re)+(if im>=0.0 then "+" else "")+(p.DtoS im)+"\\mathrm{j})"
            |P ->
                if im<0.0 then
                    (p.DtoS re)+"-"+(p.DtoS (abs im))+"i"
                else
                    (p.DtoS re)+"+"+(p.DtoS im)+"i"
                
        ///<summary>整数型配列の初期値</summary>
        static member private init_i1 (v:int list) =
            match p.lang with
            |F ->
                "(/"+(List.fold (fun acc i -> acc+(if i<>0 then "," else "")+(p.ItoS v.[i])) "" [0..v.Length-1])+"/)"
            |C ->
                "{"+(List.fold (fun acc i -> acc+(if i<>0 then "," else "")+(p.ItoS v.[i])) "" [0..v.Length-1])+"}"
            |T ->
                "{"+(List.fold (fun acc i -> acc+(if i<>0 then "," else "")+(p.ItoS v.[i])) "" [0..v.Length-1])+"}"
            |H ->
                "{"+(List.fold (fun acc i -> acc+(if i<>0 then "," else "")+(p.ItoS v.[i])) "" [0..v.Length-1])+"}"
            |P ->
                "["+(List.fold (fun acc i -> acc+(if i<>0 then "," else "")+(p.ItoS v.[i])) "" [0..v.Length-1])+"]"
                
        ///<summary>倍精度浮動小数点型配列の初期値</summary>
        static member private init_d1 (v:double list) =
            match p.lang with
            |F ->
                "(/"+(List.fold (fun acc i -> acc+(if i<>0 then "," else "")+(p.DtoS v.[i])) "" [0..v.Length-1])+"/)"
            |C ->
                "{"+(List.fold (fun acc i -> acc+(if i<>0 then "," else "")+(p.DtoS v.[i])) "" [0..v.Length-1])+"}"
            |T ->
                "{"+(List.fold (fun acc i -> acc+(if i<>0 then "," else "")+(p.DtoS v.[i])) "" [0..v.Length-1])+"}"
            |H ->
                "{"+(List.fold (fun acc i -> acc+(if i<>0 then "," else "")+(p.DtoS v.[i])) "" [0..v.Length-1])+"}"
            |P ->
                "["+(List.fold (fun acc i -> acc+(if i<>0 then "," else "")+(p.DtoS v.[i])) "" [0..v.Length-1])+"]"
                
        ///<summary>複素数型配列の初期値</summary>
        static member private init_z1 (v:(double*double) list) =
            match p.lang with
            |F ->
                "(/"+(List.fold (fun acc i -> acc+(if i<>0 then "," else "")+(var.init_z v.[i])) "" [0..v.Length-1])+"/)"
            |C ->
                "{"+(List.fold (fun acc i -> acc+(if i<>0 then "," else "")+(var.init_z v.[i])) "" [0..v.Length-1])+"}"
            |T ->
                "{"+(List.fold (fun acc i -> acc+(if i<>0 then "," else "")+(var.init_z v.[i])) "" [0..v.Length-1])+"}"
            |H ->
                "{"+(List.fold (fun acc i -> acc+(if i<>0 then "," else "")+(var.init_z v.[i])) "" [0..v.Length-1])+"}"
            |P ->
                "["+(List.fold (fun acc i -> acc+(if i<>0 then "," else "")+(var.init_z v.[i])) "" [0..v.Length-1])+"]"
    
        ///<summary>バイト型変数</summary>
        ///<param name="name">変数名</param>
        static member b0 (name:string) = 
            p.var.setUniqVarWarning(It 1,A0,name,"")
            Var(It 1,name)
            
        ///<summary>整数型変数</summary>
        ///<param name="name">変数名</param>
        static member i0 (name:string) = 
            p.var.setUniqVarWarning(It 4,A0,name,"")
            Var(It 1,name)
            
        ///<summary>倍精度浮動小数点型変数</summary>
        ///<param name="name">変数名</param>
        static member d0 (name:string) =
            p.var.setUniqVarWarning(Dt,A0,name,"")
            Var(Dt,name)
            
        ///<summary>複素数型変数</summary>
        ///<param name="name">変数名</param>
        static member z0 (name:string) = 
            p.var.setUniqVarWarning(Zt,A0,name,"")
            Var(Zt,name)
            
        ///<summary>整数型1次元配列</summary>
        ///<param name="name">変数名</param>
        ///<param name="size1">要素数</param>
        static member i1(name:string,size1:int) = 
            num1(It 4,A1(size1),name,"")
            
        ///<summary>整数型可変長1次元配列</summary>
        ///<param name="name">変数名</param>
        static member i1(name:string) =
            num1(It 4,A1(0),name,"")
            
        ///<summary>倍精度浮動小数点型1次元配列</summary>
        ///<param name="name">変数名</param>
        ///<param name="size1">要素数</param>
        static member d1(name:string,size1:int) = 
            num1(Dt,A1(size1),name,"")
            
        ///<summary>倍精度浮動小数点型可変長1次元配列</summary>
        ///<param name="name">変数名</param>
        static member d1(name:string) = 
            num1(Dt,A1(0),name,"")
            
        ///<summary>複素数型1次元配列</summary>
        ///<param name="name">変数名</param>
        ///<param name="size1">要素数</param>
        static member z1(name:string,size1:int) = 
            num1(Zt,A1(size1),name,"")
            
        ///<summary>複素数型可変長1次元配列</summary>
        ///<param name="name">変数名</param>
        static member z1(name:string) = 
            num1(Zt,A1(0),name,"")
            
        ///<summary>整数型2次元配列</summary>
        ///<param name="name">変数名</param>
        ///<param name="size1">要素数1</param>
        ///<param name="size2">要素数2</param>
        static member i2(name:string,size1:int,size2:int) = 
            num2(It 4,A2(size1,size2),name,"")
            
        ///<summary>整数型可変長2次元配列</summary>
        ///<param name="name">変数名</param>
        static member i2(name:string) = 
            num2(It 4,A2(0,0),name,"")
            
        ///<summary>倍精度浮動小数点型2次元配列</summary>
        ///<param name="name">変数名</param>
        ///<param name="size1">要素数1</param>
        ///<param name="size2">要素数2</param>
        static member d2(name:string,size1:int,size2:int) = 
            num2(Dt,A2(size1,size2),name,"")
            
        ///<summary>倍精度浮動小数点型可変長2次元配列</summary>
        ///<param name="name">変数名</param>
        static member d2(name:string) = 
            num2(Dt,A2(0,0),name,"")
            
        ///<summary>複素数型2次元配列</summary>
        ///<param name="name">変数名</param>
        ///<param name="size1">要素数1</param>
        ///<param name="size2">要素数2</param>
        static member z2(name:string,size1:int,size2:int) = 
            num2(Zt,A2(size1,size2),name,"")
            
        ///<summary>複素数型可変長2次元配列</summary>
        ///<param name="name">変数名</param>
        static member z2(name:string) = 
            num2(Zt,A2(0,0),name,"")
            
        ///<summary>整数型3次元配列</summary>
        ///<param name="name">変数名</param>
        ///<param name="size1">要素数1</param>
        ///<param name="size2">要素数2</param>
        ///<param name="size3">要素数3</param>
        static member i3(name:string,size1:int,size2:int,size3:int) = 
            num3(It 4,A3(size1,size2,size3),name,"")
            
        ///<summary>整数型可変長3次元配列</summary>
        ///<param name="name">変数名</param>
        static member i3(name:string) = 
            num3(It 4,A3(0,0,0),name,"")
            
        ///<summary>倍精度浮動小数点型3次元配列</summary>
        ///<param name="name">変数名</param>
        ///<param name="size1">要素数1</param>
        ///<param name="size2">要素数2</param>
        ///<param name="size3">要素数3</param>
        static member d3(name:string,size1:int,size2:int,size3:int) = 
            num3(Dt,A3(size1,size2,size3),name,"")
            
        ///<summary>倍精度浮動小数点型可変長3次元配列</summary>
        ///<param name="name">変数名</param>
        static member d3(name:string) = 
            num3(Dt,A3(0,0,0),name,"")
            
        ///<summary>複素数型3次元配列</summary>
        ///<param name="name">変数名</param>
        ///<param name="size1">要素数1</param>
        ///<param name="size2">要素数2</param>
        ///<param name="size3">要素数3</param>
        static member z3(name:string,size1:int,size2:int,size3:int) = 
            num3(Zt,A3(size1,size2,size3),name,"")
            
        ///<summary>複素数型可変長3次元配列</summary>
        ///<param name="name">変数名</param>
        static member z3(name:string) = 
            num3(Zt,A3(0,0,0),name,"")
            
        ///<summary>整数型定数</summary>
        ///<param name="name">変数名</param>
        ///<param name="v">定数</param>
        static member ip0(name:string,v) = 
            p.var.setUniqVarWarning(It 4,A0,name,(p.ItoS v))
            Var(It 4, name)
            
        ///<summary>倍精度浮動小数点型定数</summary>
        ///<param name="name">変数名</param>
        ///<param name="v">定数</param>
        static member dp0(name:string,v) = 
            p.var.setUniqVarWarning(Dt,A0,name,(p.DtoS v))
            Var(Dt, name)
            
        ///<summary>整数型定数</summary>
        ///<param name="name">変数名</param>
        ///<param name="v">定数</param>
        static member internal ip0_noWarning(name:string,v) = 
            p.var.setUniqVar(It 4,A0,name,(p.ItoS v))
            Var(It 4, name)
            
        ///<summary>倍精度浮動小数点型定数</summary>
        ///<param name="name">変数名</param>
        ///<param name="v">定数</param>
        static member internal dp0_noWarning(name:string,v) = 
            p.var.setUniqVar(Dt,A0,name,(p.DtoS v))
            Var(Dt, name)
            
        ///<summary>複素数型定数</summary>
        ///<param name="name">変数名</param>
        ///<param name="v">定数</param>
        static member zp0(name,(v:double*double)) =
            let (v_re,v_im) = v
            p.var.setUniqVarWarning(Zt,A0,name,var.init_z (v_re,v_im))
            Var(Zt, name)
            
        ///<summary>整数型1次元配列定数</summary>
        ///<param name="name">変数名</param>
        ///<param name="v">定数</param>
        static member ip1(name:string,v:int list) =
            p.var.setUniqVarWarning(It 4,A1(v.Length),name,var.init_i1 v)
            num1(It 4,Var1(A1(v.Length),name))
            
        ///<summary>倍精度浮動小数点型1次元配列定数</summary>
        ///<param name="name">変数名</param>
        ///<param name="v">定数</param>
        static member dp1(name:string,v:double list) = 
            p.var.setUniqVarWarning(Zt,A1(v.Length),name,var.init_d1 v)
            num1(Dt,Var1(A1(v.Length),name))
            
        ///<summary>複素数型1次元配列定数</summary>
        ///<param name="name">変数名</param>
        ///<param name="v">定数</param>
        static member zp1(name:string,v:(double*double) list) =
            p.var.setUniqVarWarning(Zt,A0,name,var.init_z1 v)
            num1(Zt,Var1(A1(v.Length),name))
            
        ///<summary>整数型変数</summary>
        ///<param name="name">変数名</param>
        static member n0(e:Etype,name:string) = 
            match e with 
            |Nt |St |Structure _ -> printfn "%s: 変数を生成できない型です(gvar.n0)" <| e.ToString()
            |_ -> ()
            p.var.setUniqVarWarning(e,A0,name,"")
            Var(e,name)
            
        ///<summary>整数型1次元配列</summary>
        ///<param name="name">変数名</param>
        ///<param name="size1">要素数</param>
        static member n1(e:Etype,name:string,size1:int) = 
            match e with 
            |Nt |St |Structure _ -> printfn "%s: 変数を生成できない型です(gvar.n1)" <| e.ToString()
            |_ -> ()
            num1(e,Var1(A1(size1),name))
            
        ///<summary>整数型可変長1次元配列</summary>
        ///<param name="name">変数名</param>
        static member n1(e:Etype,name:string) =
            match e with 
            |Nt |St |Structure _ -> printfn "%s: 変数を生成できない型です(gvar.n1)" <| e.ToString()
            |_ -> ()
            num1(e,Var1(A1(0),name))
            
        ///<summary>整数型2次元配列</summary>
        ///<param name="name">変数名</param>
        ///<param name="size1">要素数1</param>
        ///<param name="size2">要素数2</param>
        static member n2(e:Etype,name:string,size1:int,size2:int) = 
            match e with 
            |Nt |St |Structure _ -> printfn "%s: 変数を生成できない型です(gvar.n2)" <| e.ToString()
            |_ -> ()
            num2(e,Var2(A2(size1,size2),name))
            
        ///<summary>整数型可変長2次元配列</summary>
        ///<param name="name">変数名</param>
        static member n2(e:Etype,name:string) = 
            match e with 
            |Nt |St |Structure _ -> printfn "%s: 変数を生成できない型です(gvar.n2)" <| e.ToString()
            |_ -> ()
            num2(e,Var2(A2(0,0),name))
            
        ///<summary>整数型3次元配列</summary>
        ///<param name="name">変数名</param>
        ///<param name="size1">要素数1</param>
        ///<param name="size2">要素数2</param>
        ///<param name="size3">要素数3</param>
        static member n3(e:Etype,name:string,size1:int,size2:int,size3:int) = 
            match e with 
            |Nt |St |Structure _ -> printfn "%s: 変数を生成できない型です(gvar.n3)" <| e.ToString()
            |_ -> ()
            num3(e,Var3(A3(size1,size2,size3),name))
            
        ///<summary>整数型可変長3次元配列</summary>
        ///<param name="name">変数名</param>
        static member n3(e:Etype,name:string) = 
            match e with 
            |Nt |St |Structure _ -> printfn "%s: 変数を生成できない型です(gvar.n3)" <| e.ToString()
            |_ -> ()
            num3(e,Var3(A3(0,0,0),name))