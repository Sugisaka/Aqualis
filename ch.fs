﻿(*
Copyright (c) 2022 Jun-ichiro Sugisaka

This software is released under the MIT License.
http://opensource.org/licenses/mit-license.php
*)
namespace Aqualis
    
    open Aqualis_base

    ///<summary>一時変数の生成と使用</summary>
    type ch () =
        
        ///<summary>整数型一時変数の生成</summary>
        static member i code = 
            let x = p.i_cache_var.getAutoVar()
            code <| Var(It 4,x)
            p.i_cache_var.setVar(It 4,A0,x,"")
            
        ///<summary>倍精度浮動小数点型一時変数の生成</summary>
        static member d code = 
            let x = p.d_cache_var.getAutoVar()
            code <| Var(Dt, x)
            p.d_cache_var.setVar(Dt,A0,x,"")
            
        ///<summary>複素数型一時変数の生成</summary>
        static member z code = 
            let x = p.z_cache_var.getAutoVar()
            code <| Var(Zt, x)
            p.z_cache_var.setVar(Zt,A0,x,"")
            
        ///<summary>整数型一時変数を生成し、valueを代入してからcodeで使用</summary>
        static member si (value:num0) = fun code ->
            ch.i <| fun v ->
                v <== value
                code v
                
        ///<summary>整数型一時変数を生成し、valueを代入してからcodeで使用</summary>
        static member si (value:int) = ch.si (value.I)
        
        ///<summary>倍精度浮動小数点型一時変数を生成し、valueを代入してからcodeで使用</summary>
        static member sd (value:num0) = fun code ->
            ch.d <| fun v ->
                v <== value
                code v
                
        ///<summary>倍精度浮動小数点型一時変数を生成し、valueを代入してからcodeで使用</summary>
        static member sd (value:double) = ch.sd (value.D)
        
        ///<summary>複素数型一時変数を生成し、valueを代入してからcodeで使用</summary>
        static member sz (value:num0) = fun code ->
            ch.z <| fun v ->
                v <== value
                code v
                
        ///<summary>複素数型一時変数を生成し、valueを代入してからcodeで使用</summary>
        static member sz (re:double,im:double) = ch.sz (re+asm.uj*im)
        
        ///<summary>整数型1次元配列を生成</summary>
        static member i01 code = 
            let x = p.i1_cache_var.getAutoVar()
            let y = num1(It 4,Var1(A1(0),x))
            code y
            p.i1_cache_var.setVar(It 4,A1(0),x,"")
            
        ///<summary>実数型1次元配列を生成</summary>
        static member d01 code = 
            let x = p.d1_cache_var.getAutoVar()
            let y = num1(Dt,Var1(A1(0),x))
            code y
            p.d1_cache_var.setVar(Dt,A1(0),x,"")
            
        ///<summary>複素数型1次元配列を生成</summary>
        static member z01 code = 
            let x = p.z1_cache_var.getAutoVar()
            let y = num1(Zt,Var1(A1(0),x))
            code y
            p.z1_cache_var.setVar(Zt,A1(0),x,"")
            
        ///<summary>整数型1次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
        static member i1 (size1:num0) = fun code ->
            ch.i01 <| fun v ->
                v.allocate(size1)
                code v
                v.deallocate()
                
        ///<summary>整数型1次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
        static member i1 (size1:int) = fun code ->
            ch.i1 (size1.I) code
            
        ///<summary>実数型1次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
        static member d1 (size1:num0) = fun code ->
            ch.d01 <| fun v ->
                v.allocate(size1)
                code v
                v.deallocate()
            
        ///<summary>実数型1次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
        static member d1 (size1:int) = fun code ->
            ch.d1 (size1.I) code
            
        ///<summary>複素数型1次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
        static member z1 (size1:num0) = fun code ->
            ch.z01 <| fun v ->
                v.allocate(size1)
                code v
                v.deallocate()
            
        ///<summary>複素数型1次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
        static member z1 (size1:int) = fun code ->
            ch.z1 (size1.I) code
            
        ///<summary>整数型2次元配列を生成</summary>
        static member i02 code = 
            let x = p.i2_cache_var.getAutoVar()
            let y = num2(It 4,Var2(A2(0,0),x))
            code y
            p.i2_cache_var.setVar(It 4,A2(0,0),x,"")
            
        ///<summary>実数型2次元配列を生成</summary>
        static member d02 code = 
            let x = p.d2_cache_var.getAutoVar()
            let y = num2(Dt,Var2(A2(0,0),x))
            code y
            p.d2_cache_var.setVar(Dt,A2(0,0),x,"")
            
        ///<summary>複素数型2次元配列を生成</summary>
        static member z02 code = 
            let x = p.z2_cache_var.getAutoVar()
            let y = num2(Zt,Var2(A2(0,0),x))
            code y
            p.z2_cache_var.setVar(Zt,A2(0,0),x,"")
            
        ///<summary>整数型2次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
        static member i2 (size1:num0) = fun (size2:num0) code -> 
            ch.i02 <| fun v ->
                v.allocate(size1,size2)
                code v
                v.deallocate()
            
        ///<summary>整数型2次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
        static member i2 (size1:int) = fun (size2:int) code -> 
            ch.i2 size1.I size2.I code
            
        ///<summary>実数型2次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
        static member d2 (size1:num0) = fun (size2:num0) code ->
            ch.d02 <| fun v ->
                v.allocate(size1,size2)
                code v
                v.deallocate()
                
        ///<summary>実数型2次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
        static member d2 (size1:int) = fun (size2:int) code -> 
            ch.d2 size1.I size2.I code
            
        ///<summary>複素数型2次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
        static member z2 (size1:num0) = fun (size2:num0) code ->
            ch.z02 <| fun v ->
                v.allocate(size1,size2)
                code v
                v.deallocate()
            
        ///<summary>複素数型2次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
        static member z2 (size1:int) = fun (size2:int) code -> 
            ch.z2 size1.I size2.I code
            
        ///<summary>整数型3次元配列を生成</summary>
        static member i03 code = 
            let x = p.i3_cache_var.getAutoVar()
            let y = num3(It 4,Var3(A3(0,0,0),x))
            code y
            p.i3_cache_var.setVar(It 4,A3(0,0,0),x,"")
            
        ///<summary>実数型3次元配列を生成</summary>
        static member d03 code = 
            let x = p.d3_cache_var.getAutoVar()
            let y = num3(Dt,Var3(A3(0,0,0),x))
            code y
            p.i3_cache_var.setVar(Dt,A3(0,0,0),x,"")
            
        ///<summary>複素数型3次元配列を生成</summary>
        static member z03 code = 
            let x = p.z3_cache_var.getAutoVar()
            let y = num3(Zt,Var3(A3(0,0,0),x))
            code y
            p.z3_cache_var.setVar(Zt,A3(0,0,0),x,"")
            
        ///<summary>整数型3次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
        static member i3 (size1:num0) = fun (size2:num0) (size3:num0) code ->
            ch.i03 <| fun v ->
                v.allocate(size1,size2,size3)
                code v
                v.deallocate()
            
        ///<summary>整数型3次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
        static member i3 (size1:int) = fun (size2:int) (size3:int) code ->
            ch.i3 size1.I size2.I size3.I code
            
        ///<summary>実数型3次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
        static member d3 (size1:num0) = fun (size2:num0) (size3:num0) code ->
            ch.d03 <| fun v ->
                v.allocate(size1,size2,size3)
                code v
                v.deallocate()
                
        ///<summary>実数型3次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
        static member d3 (size1:int) = fun (size2:int) (size3:int) code ->
            ch.d3 size1.I size2.I size3.I code
            
        ///<summary>複素数型3次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
        static member z3 (size1:num0) = fun (size2:num0) (size3:num0) code ->
            ch.z03 <| fun v ->
                v.allocate(size1,size2,size3)
                code v
                v.deallocate()
                
        ///<summary>複素数型3次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
        static member z3 (size1:int) = fun (size2:int) (size3:int) code ->
            ch.z3 size1.I size2.I size3.I code
            
        static member ii code = 
            ch.i <| fun i1 -> 
                ch.i <| fun i2 -> 
                    code(i1,i2)
                        
        static member id code = 
            ch.i <| fun i1 -> 
                ch.d <| fun d2 -> 
                    code(i1,d2)
                    
        static member iz code = 
            ch.i <| fun i1 -> 
                ch.z <| fun z2 -> 
                    code(i1,z2)
                    
        static member dd code = 
            ch.d <| fun d1 -> 
                ch.d <| fun d2 -> 
                    code(d1,d2)
                    
        static member dz code = 
            ch.d <| fun d1 -> 
                ch.z <| fun z2 -> 
                    code(d1,z2)
                    
        static member zz code = 
            ch.z <| fun z1 -> 
                ch.z <| fun z2 -> 
                    code(z1,z2)
                    
        static member iii code = 
            ch.i <| fun i1 -> 
                ch.i <| fun i2 -> 
                    ch.i <| fun i3 -> 
                        code(i1,i2,i3)
                        
        static member iid code = 
            ch.i <| fun i1 -> 
                ch.i <| fun i2 -> 
                    ch.d <| fun d3 -> 
                        code(i1,i2,d3)
                        
        static member iiz code = 
            ch.i <| fun i1 -> 
                ch.i <| fun i2 -> 
                    ch.z <| fun z3 -> 
                        code(i1,i2,z3)
                        
        static member idd code = 
            ch.i <| fun i1 -> 
                ch.d <| fun d2 -> 
                    ch.d <| fun d3 -> 
                        code(i1,d2,d3)
                        
        static member idz code = 
            ch.i <| fun i1 -> 
                ch.d <| fun d2 -> 
                    ch.z <| fun z3 -> 
                        code(i1,d2,z3)
                        
        static member izz code = 
            ch.i <| fun i1 -> 
                ch.z <| fun z2 -> 
                    ch.z <| fun z3 -> 
                        code(i1,z2,z3)
                        
        static member ddd code = 
            ch.d <| fun d1 -> 
                ch.d <| fun d2 -> 
                    ch.d <| fun d3 -> 
                        code(d1,d2,d3)
                        
        static member ddz code = 
            ch.d <| fun d1 -> 
                ch.d <| fun d2 -> 
                    ch.z <| fun z3 -> 
                        code(d1,d2,z3)
                        
        static member dzz code = 
            ch.d <| fun d1 -> 
                ch.z <| fun z2 -> 
                    ch.z <| fun z3 -> 
                        code(d1,z2,z3)
                        
        static member zzz code = 
            ch.z <| fun z1 -> 
                ch.z <| fun z2 -> 
                    ch.z <| fun z3 -> 
                        code(z1,z2,z3)
                        
        static member iiii code = 
            ch.i <| fun i1 -> 
                ch.i <| fun i2 -> 
                    ch.i <| fun i3 -> 
                        ch.i <| fun i4 -> 
                            code(i1,i2,i3,i4)
                            
        static member iiid code = 
            ch.i <| fun i1 -> 
                ch.i <| fun i2 -> 
                    ch.i <| fun i3 -> 
                        ch.d <| fun d4 -> 
                            code(i1,i2,i3,d4)
                            
        static member iiiz code = 
            ch.i <| fun i1 -> 
                ch.i <| fun i2 -> 
                    ch.i <| fun i3 -> 
                        ch.z <| fun z4 -> 
                            code(i1,i2,i3,z4)
                            
        static member iidd code = 
            ch.i <| fun i1 -> 
                ch.i <| fun i2 -> 
                    ch.d <| fun d3 -> 
                        ch.d <| fun d4 -> 
                            code(i1,i2,d3,d4)
                            
        static member iidz code = 
            ch.i <| fun i1 -> 
                ch.i <| fun i2 -> 
                    ch.d <| fun d3 -> 
                        ch.z <| fun z4 -> 
                            code(i1,i2,d3,z4)
                            
        static member iizz code = 
            ch.i <| fun i1 -> 
                ch.i <| fun i2 -> 
                    ch.z <| fun z3 -> 
                        ch.z <| fun z4 -> 
                            code(i1,i2,z3,z4)
                            
        static member iddd code = 
            ch.i <| fun i1 -> 
                ch.d <| fun d2 -> 
                    ch.d <| fun d3 -> 
                        ch.d <| fun d4 -> 
                            code(i1,d2,d3,d4)
                            
        static member iddz code = 
            ch.i <| fun i1 -> 
                ch.d <| fun d2 -> 
                    ch.d <| fun d3 -> 
                        ch.z <| fun z4 -> 
                            code(i1,d2,d3,z4)
                            
        static member idzz code = 
            ch.i <| fun i1 -> 
                ch.d <| fun d2 -> 
                    ch.z <| fun z3 -> 
                        ch.z <| fun z4 -> 
                            code(i1,d2,z3,z4)
                            
        static member izzz code = 
            ch.i <| fun i1 -> 
                ch.z <| fun z2 -> 
                    ch.z <| fun z3 -> 
                        ch.z <| fun z4 -> 
                            code(i1,z2,z3,z4)
                            
        static member dddd code = 
            ch.d <| fun d1 -> 
                ch.d <| fun d2 -> 
                    ch.d <| fun d3 -> 
                        ch.d <| fun d4 -> 
                            code(d1,d2,d3,d4)
                            
        static member dddz code = 
            ch.d <| fun d1 -> 
                ch.d <| fun d2 -> 
                    ch.d <| fun d3 -> 
                        ch.z <| fun z4 -> 
                            code(d1,d2,d3,z4)
                            
        static member ddzz code = 
            ch.d <| fun d1 -> 
                ch.d <| fun d2 -> 
                    ch.z <| fun z3 -> 
                        ch.z <| fun z4 -> 
                            code(d1,d2,z3,z4)
                            
        static member dzzz code = 
            ch.d <| fun d1 -> 
                ch.z <| fun z2 -> 
                    ch.z <| fun z3 -> 
                        ch.z <| fun z4 -> 
                            code(d1,z2,z3,z4)
                            
        static member zzzz code = 
            ch.z <| fun z1 -> 
                ch.z <| fun z2 -> 
                    ch.z <| fun z3 -> 
                        ch.z <| fun z4 -> 
                            code(z1,z2,z3,z4)
                            
        static member count0 code = 
            ch.i <| fun counter -> 
                counter.clear()
                code(counter)
                
        static member count1 code =
            ch.i <| fun counter ->
                counter <== _1
                code(counter)
                
        ///<summary>ファイルポインタcache変数を生成し、code内の処理を実行</summary>
        static member f code = 
            let name = p.fNumber()
            match p.lang with
              |F -> p.var.setVar(Structure("file"),A0,name,(p.ItoS <| (p.fvar.counter+10)))
              |_ -> p.var.setVar(Structure("file"),A0,name,"")
            code name
            
        ///<summary>文字列cache変数を生成し、code内の処理を実行</summary>
        static member t vt code = 
            let name = p.tName()
            p.var.setVar(Structure("string"),vt,name,"")
            code name
            
        ///<summary>指定した型の一時変数を生成</summary>
        static member n e = fun code ->
            match e with 
            |Nt |St |Structure _ -> printfn "%s: 変数を生成できない型です(ch.n)" <| e.ToString()
            |_ -> ()
            let x = 
                match e with
                |Zt -> p.z_cache_var.getAutoVar()
                |Dt -> p.d_cache_var.getAutoVar()
                |_ -> p.i_cache_var.getAutoVar()
            code <| Var(e, x)
            match e with
            |It _ -> p.i_cache_var.setVar(e,A0,x,"")
            |Dt -> p.d_cache_var.setVar(e,A0,x,"")
            |Zt -> p.z_cache_var.setVar(e,A0,x,"")
            |_ -> ()
            
        ///<summary>指定した型の一時変数を生成</summary>
        static member n (v:num0) = fun code ->
            match v.etype with 
            |Nt |St |Structure _ -> printfn "%s: 変数を生成できない型です(ch.n)" <| v.etype.ToString()
            |_ -> ()
            let x = 
                match v.etype with
                |Zt -> p.z_cache_var.getAutoVar()
                |Dt -> p.d_cache_var.getAutoVar()
                |_ -> p.i_cache_var.getAutoVar()
            code <| Var(v.etype, x)
            match v.etype with
            |It _ -> p.i_cache_var.setVar(v.etype,A0,x,"")
            |Dt -> p.d_cache_var.setVar(v.etype,A0,x,"")
            |Zt -> p.z_cache_var.setVar(v.etype,A0,x,"")
            |_ -> ()
            
        ///<summary>指定した型の一時変数を生成</summary>
        static member n (v:num1) = fun code ->
            match v.etype with 
            |Nt |St |Structure _ -> printfn "%s: 変数を生成できない型です(ch.n)" <| v.etype.ToString()
            |_ -> ()
            let x = 
                match v.etype with
                |Zt -> p.z_cache_var.getAutoVar()
                |Dt -> p.d_cache_var.getAutoVar()
                |_ -> p.i_cache_var.getAutoVar()
            code <| Var(v.etype, x)
            match v.etype with
            |It _ -> p.i_cache_var.setVar(v.etype,A0,x,"")
            |Dt -> p.d_cache_var.setVar(v.etype,A0,x,"")
            |Zt -> p.z_cache_var.setVar(v.etype,A0,x,"")
            |_ -> ()
            
        ///<summary>指定した型の一時変数を生成</summary>
        static member n (v:num2) = fun code ->
            match v.etype with 
            |Nt |St |Structure _ -> printfn "%s: 変数を生成できない型です(ch.n)" <| v.etype.ToString()
            |_ -> ()
            let x = 
                match v.etype with
                |Zt -> p.z_cache_var.getAutoVar()
                |Dt -> p.d_cache_var.getAutoVar()
                |_ -> p.i_cache_var.getAutoVar()
            code <| Var(v.etype, x)
            match v.etype with
            |It _ -> p.i_cache_var.setVar(v.etype,A0,x,"")
            |Dt -> p.d_cache_var.setVar(v.etype,A0,x,"")
            |Zt -> p.z_cache_var.setVar(v.etype,A0,x,"")
            |_ -> ()
            
        ///<summary>指定した型の一時変数を生成</summary>
        static member n (v:num3) = fun code ->
            match v.etype with 
            |Nt |St |Structure _ -> printfn "%s: 変数を生成できない型です(ch.n)" <| v.etype.ToString()
            |_ -> ()
            let x = 
                match v.etype with
                |Zt -> p.z_cache_var.getAutoVar()
                |Dt -> p.d_cache_var.getAutoVar()
                |_ -> p.i_cache_var.getAutoVar()
            code <| Var(v.etype, x)
            match v.etype with
            |It _ -> p.i_cache_var.setVar(v.etype,A0,x,"")
            |Dt -> p.d_cache_var.setVar(v.etype,A0,x,"")
            |Zt -> p.z_cache_var.setVar(v.etype,A0,x,"")
            |_ -> ()
            
        ///<summary>num1型1次元配列を生成</summary>
        static member n01 e code = 
            match e with 
            |Nt |St |Structure _ -> printfn "%s: 変数を生成できない型です(ch.n1)" <| e.ToString()
            |_ -> ()
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
        static member n1 (e,size1:num0) = fun code ->
            ch.n01 e <| fun v ->
                v.allocate(size1)
                code v
                v.deallocate()
                
        ///<summary>num0型1次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
        static member n1 (e,size1:int) = fun code ->
            ch.n1 (e,size1.I) code
            
        ///<summary>num0型2次元配列を生成</summary>
        static member n02 e code = 
            match e with 
            |Nt |St |Structure _ -> printfn "%s: 変数を生成できない型です(ch.n2)" <| e.ToString()
            |_ -> ()
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
        static member n2 (e,size1:num0,size2:num0) = fun code -> 
            ch.n02 e <| fun v ->
                v.allocate(size1,size2)
                code v
                v.deallocate()
                
        ///<summary>num0型2次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
        static member n2 (e,size1:int,size2:int) = fun code -> 
            ch.n2 (e,size1.I,size2.I) code
            
        ///<summary>整数型3次元配列を生成</summary>
        static member n03 e code = 
            match e with 
            |Nt |St |Structure _ -> printfn "%s: 変数を生成できない型です(ch.n3)" <| e.ToString()
            |_ -> ()
            let x = 
                match e with
                |Zt -> p.z3_cache_var.getAutoVar()
                |Dt -> p.d3_cache_var.getAutoVar()
                |_ -> p.i3_cache_var.getAutoVar()
            let y = num3(e,Var3(A3(0,0,0),x))
            code y
            match e with
            |It _ -> p.i3_cache_var.setVar(It 4,A3(0,0,0),x,"")
            |Dt -> p.d3_cache_var.setVar(Dt,A3(0,0,0),x,"")
            |Zt -> p.z3_cache_var.setVar(Zt,A3(0,0,0),x,"")
            |_ -> ()
            
        ///<summary>整数型3次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
        static member n3 (size1:num0) = fun (size2:num0) (size3:num0) e code ->
            ch.n03 e <| fun v ->
                v.allocate(size1,size2,size3)
                code v
                v.deallocate()
                
        ///<summary>整数型3次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
        static member n3 (size1:int) = fun (size2:int) (size3:int) e code ->
            ch.n3 size1.I size2.I size3.I e code
                
        ///<summary>整数型1次元配列を生成し、指定したサイズでメモリ割り当て、GPUに転送→code実行後にメモリ解放</summary>
        static member copyin_i1 (size1:num0) = fun code ->
            match p.lang with
            |F ->
                ch.i1 size1 <| fun i ->
                    p.civar.setVar(It 4, A1(0),i.code+"(1:"+size1.code+")","")
                    code i
                    ignore <| p.civar.getAutoVar()
            |C ->
                ch.i1 size1 <| fun i ->
                    p.civar.setVar(It 4,A1(0),i.code+"[0:"+size1.code+"]","")
                    code i
                    ignore <| p.civar.getAutoVar()
            |_ ->
                ()
                
        ///<summary>整数型1次元配列を生成し、指定したサイズでメモリ割り当て、GPUに転送→code実行後にメモリ解放</summary>
        static member copyin_i1 (size1:int) = fun code ->
            ch.copyin_i1 size1.I code

        ///<summary>実数型1次元配列を生成し、指定したサイズでメモリ割り当て、GPUに転送→code実行後にメモリ解放</summary>
        static member copyin_d1 (size1:num0) = fun code ->
            match p.lang with
            |F ->
                ch.d1 size1 <| fun i ->
                    p.civar.setVar(Dt, A1(0),i.code+"(1:"+size1.code+")","")
                    code i
                    ignore <| p.civar.getAutoVar()
            |C ->
                ch.d1 size1 <| fun i ->
                    p.civar.setVar(Dt,A1(0),i.code+"[0:"+size1.code+"]","")
                    code i
                    ignore <| p.civar.getAutoVar()
            |_ ->
                ()
                
        ///<summary>実数型1次元配列を生成し、指定したサイズでメモリ割り当て、GPUに転送→code実行後にメモリ解放</summary>
        static member copyin_d1 (size1:int) = fun code ->
            ch.copyin_d1 size1.I code

        ///<summary>複素数型1次元配列を生成し、指定したサイズでメモリ割り当て、GPUに転送→code実行後にメモリ解放</summary>
        static member copyin_z1 (size1:num0) = fun code ->
            match p.lang with
            |F ->
                ch.z1 size1 <| fun i ->
                    p.civar.setVar(Zt, A1(0),i.code+"(1:"+size1.code+")","")
                    code i
                    ignore <| p.civar.getAutoVar()
            |C ->
                ch.z1 size1 <| fun i ->
                    p.civar.setVar(Zt,A1(0),i.code+"[0:"+size1.code+"]","")
                    code i
                    ignore <| p.civar.getAutoVar()
            |_ ->
                ()
                
        ///<summary>複素数型1次元配列を生成し、指定したサイズでメモリ割り当て、GPUに転送→code実行後にメモリ解放</summary>
        static member copyin_z1 (size1:int) = fun code ->
            ch.copyin_z1 size1.I code

        ///<summary>整数型2次元配列を生成し、指定したサイズでメモリ割り当て、GPUに転送→code実行後にメモリ解放</summary>
        static member copyin_i2 (size1:num0) = fun (size2:num0) code ->
            match p.lang with
            |F ->
                ch.i2 size1 size2 <| fun i ->
                    p.civar.setVar (It 4, A2(0,0), i.code+"(1:"+size1.code+",1:"+size2.code+")","")
                    code i
                    ignore <| p.civar.getAutoVar()
            |C ->
                ch.i2 size1 size2 <| fun i ->
                    p.civar.setVar (It 4, A2(0,0), i.code+"[0:"+size1.code+"][0:"+size2.code+"]","")
                    code i
                    ignore <| p.civar.getAutoVar()
            |_ ->
                ()

        ///<summary>整数型2次元配列を生成し、指定したサイズでメモリ割り当て、GPUに転送→code実行後にメモリ解放</summary>
        static member copyin_i2 (size1:int) = fun (size2:int) code ->
            ch.copyin_i2 size1.I size2.I code

        ///<summary>実数型2次元配列を生成し、指定したサイズでメモリ割り当て、GPUに転送→code実行後にメモリ解放</summary>
        static member copyin_d2 (size1:num0) = fun (size2:num0) code ->
            match p.lang with
            |F ->
                ch.d2 size1 size2 <| fun i ->
                    p.civar.setVar (It 4, A2(0,0), i.code+"(1:"+size1.code+",1:"+size2.code+")","")
                    code i
                    ignore <| p.civar.getAutoVar()
            |C ->
                ch.d2 size1 size2 <| fun i ->
                    p.civar.setVar (It 4, A2(0,0), i.code+"[0:"+size1.code+"][0:"+size2.code+"]","")
                    code i
                    ignore <| p.civar.getAutoVar()
            |_ ->
                ()

        ///<summary>実数型2次元配列を生成し、指定したサイズでメモリ割り当て、GPUに転送→code実行後にメモリ解放</summary>
        static member copyin_d2 (size1:int) = fun (size2:int) code ->
            ch.copyin_d2 size1.I size2.I code

        ///<summary>複素数型1次元配列を生成し、指定したサイズでメモリ割り当て、GPUに転送→code実行後にメモリ解放</summary>
        static member copyin_z2 (size1:num0) = fun (size2:num0) code ->
            match p.lang with
            |F ->
                ch.z2 size1 size2 <| fun i ->
                    p.civar.setVar (Zt, A2(0,0), i.code+"(1:"+size1.code+",1:"+size2.code+")","")
                    code i
                    ignore <| p.civar.getAutoVar()
            |C ->
                ch.z2 size1 size2 <| fun i ->
                    p.civar.setVar (Zt, A2(0,0), i.code+"[0:"+size1.code+"][0:"+size2.code+"]","")
                    code i
                    ignore <| p.civar.getAutoVar()
            |_ ->
                ()
                
        ///<summary>複素数型1次元配列を生成し、指定したサイズでメモリ割り当て、GPUに転送→code実行後にメモリ解放</summary>
        static member copyin_z2 (size1:int) = fun (size2:int) code ->
            ch.copyin_z2 size1.I size2.I code

        ///<summary>整数型3次元配列を生成し、指定したサイズでメモリ割り当て、GPUに転送→code実行後にメモリ解放</summary>
        static member copyin_i3 (size1:num0) = fun (size2:num0) (size3:num0) code ->
            match p.lang with
            |F ->
                ch.i3 size1 size2 size3 <| fun i ->
                    p.civar.setVar (It 4, A3(0,0,0), i.code+"(1:"+size1.code+",1:"+size2.code+",1:"+size3.code+")","")
                    code i
                    ignore <| p.civar.getAutoVar()
            |C ->
                ch.i3 size1 size2 size3 <| fun i ->
                    p.civar.setVar (It 4, A3(0,0,0), i.code+"[0:"+size1.code+"][0:"+size2.code+"][0:"+size3.code+"]","")
                    code i
                    ignore <| p.civar.getAutoVar()
            |_ ->
                ()
                
        ///<summary>整数型3次元配列を生成し、指定したサイズでメモリ割り当て、GPUに転送→code実行後にメモリ解放</summary>
        static member copyin_i3 (size1:int) = fun (size2:int) (size3:int) code ->
            ch.copyin_i3 size1.I size2.I size3.I code

        ///<summary>実数型3次元配列を生成し、指定したサイズでメモリ割り当て、GPUに転送→code実行後にメモリ解放</summary>
        static member copyin_d3 (size1:num0) = fun (size2:num0) (size3:num0) code ->
            match p.lang with
            |F ->
                ch.d3 size1 size2 size3 <| fun i ->
                    p.civar.setVar (Dt, A3(0,0,0), i.code+"(1:"+size1.code+",1:"+size2.code+",1:"+size3.code+")","")
                    code i
                    ignore <| p.civar.getAutoVar()
            |C ->
                ch.d3 size1 size2 size3 <| fun i ->
                    p.civar.setVar (Dt, A3(0,0,0), i.code+"[0:"+size1.code+"][0:"+size2.code+"][0:"+size3.code+"]","")
                    code i
                    ignore <| p.civar.getAutoVar()
            |_ ->
                ()
                
        ///<summary>実数型3次元配列を生成し、指定したサイズでメモリ割り当て、GPUに転送→code実行後にメモリ解放</summary>
        static member copyin_d3 (size1:int) = fun (size2:int) (size3:int) code ->
            ch.copyin_d3 size1.I size2.I size3.I code

        ///<summary>複素数型3次元配列を生成し、指定したサイズでメモリ割り当て、GPUに転送→code実行後にメモリ解放</summary>
        static member copyin_z3 (size1:num0) = fun (size2:num0) (size3:num0) code ->
            match p.lang with
            |F ->
                ch.z3 size1 size2 size3 <| fun i ->
                    p.civar.setVar (Zt, A3(0,0,0), i.code+"(1:"+size1.code+",1:"+size2.code+",1:"+size3.code+")","")
                    code i
                    ignore <| p.civar.getAutoVar()
            |C ->
                ch.z3 size1 size2 size3 <| fun i ->
                    p.civar.setVar (Zt, A3(0,0,0), i.code+"[0:"+size1.code+"][0:"+size2.code+"][0:"+size3.code+"]","")
                    code i
                    ignore <| p.civar.getAutoVar()
            |_ ->
                ()
                
        ///<summary>実数型3次元配列を生成し、指定したサイズでメモリ割り当て、GPUに転送→code実行後にメモリ解放</summary>
        static member copyin_z3 (size1:int) = fun (size2:int) (size3:int) code ->
            ch.copyin_z3 size1.I size2.I size3.I code

        ///<summary>整数型1次元配列を生成し、指定したサイズでメモリ割り当て、ホストに転送→code実行後にメモリ解放</summary>
        static member copyout_i1 (size1:num0) = fun code ->
            match p.lang with
            |F ->
                ch.i1 size1 <| fun i ->
                    p.covar.setVar(It 4, A1(0),i.code+"(1:"+size1.code+")","")
                    code i
                    ignore <| p.covar.getAutoVar()
            |C ->
                ch.i1 size1 <| fun i ->
                    p.covar.setVar(It 4,A1(0),i.code+"[0:"+size1.code+"]","")
                    code i
                    ignore <| p.covar.getAutoVar()
            |_ ->
                ()

        ///<summary>整数型1次元配列を生成し、指定したサイズでメモリ割り当て、ホストに転送→code実行後にメモリ解放</summary>
        static member copyout_i1 (size1:int) = fun code ->
            ch.copyout_i1 size1.I code

        ///<summary>実数型1次元配列を生成し、指定したサイズでメモリ割り当て、ホストに転送→code実行後にメモリ解放</summary>
        static member copyout_d1 (size1:num0) = fun code ->
            match p.lang with
            |F ->
                ch.d1 size1 <| fun i ->
                    p.covar.setVar(Dt, A1(0),i.code+"(1:"+size1.code+")","")
                    code i
                    ignore <| p.covar.getAutoVar()
            |C ->
                ch.d1 size1 <| fun i ->
                    p.covar.setVar(Dt,A1(0),i.code+"[0:"+size1.code+"]","")
                    code i
                    ignore <| p.covar.getAutoVar()
            |_ ->
                ()

        ///<summary>実数型1次元配列を生成し、指定したサイズでメモリ割り当て、ホストに転送→code実行後にメモリ解放</summary>
        static member copyout_d1 (size1:int) = fun code ->
            ch.copyout_d1 size1.I code

        ///<summary>複素数型1次元配列を生成し、指定したサイズでメモリ割り当て、ホストに転送→code実行後にメモリ解放</summary>
        static member copyout_z1 (size1:num0) = fun code ->
            match p.lang with
            |F ->
                ch.z1 size1 <| fun i ->
                    p.covar.setVar(Zt,A1(0),i.code+"(1:"+size1.code+")","")
                    code i
                    ignore <| p.covar.getAutoVar()
            |C ->
                ch.z1 size1 <| fun i ->
                    p.covar.setVar(Zt,A1(0),i.code+"[0:"+size1.code+"]","")
                    code i
                    ignore <| p.covar.getAutoVar()
            |_ ->
                ()
                
        ///<summary>複素数型1次元配列を生成し、指定したサイズでメモリ割り当て、ホストに転送→code実行後にメモリ解放</summary>
        static member copyout_z1 (size1:int) = fun code ->
            ch.copyout_z1 size1.I code

        ///<summary>整数型2次元配列を生成し、指定したサイズでメモリ割り当て、ホストに転送→code実行後にメモリ解放</summary>
        static member copyout_i2 (size1:num0) = fun (size2:num0) code ->
            match p.lang with
            |F ->
                ch.i2 size1 size2 <| fun i ->
                    p.civar.setVar (It 4, A2(0,0), i.code+"(1:"+size1.code+",1:"+size2.code+")","")
                    code i
                    ignore <| p.civar.getAutoVar()
            |C ->
                ch.i2 size1 size2 <| fun i ->
                    p.civar.setVar (It 4, A2(0,0), i.code+"[0:"+size1.code+"][0:"+size2.code+"]","")
                    code i
                    ignore <| p.civar.getAutoVar()
            |_ ->
                ()

        ///<summary>整数型2次元配列を生成し、指定したサイズでメモリ割り当て、ホストに転送→code実行後にメモリ解放</summary>
        static member copyout_i2 (size1:int) = fun (size2:int) code ->
            ch.copyout_i2 size1.I size2.I code

        ///<summary>実数型2次元配列を生成し、指定したサイズでメモリ割り当て、ホストに転送→code実行後にメモリ解放</summary>
        static member copyout_d2 (size1:num0) = fun (size2:num0) code ->
            match p.lang with
            |F ->
                ch.d2 size1 size2 <| fun i ->
                    p.civar.setVar (Dt, A2(0,0), i.code+"(1:"+size1.code+",1:"+size2.code+")","")
                    code i
                    ignore <| p.civar.getAutoVar()
            |C ->
                ch.d2 size1 size2 <| fun i ->
                    p.civar.setVar (Dt, A2(0,0), i.code+"[0:"+size1.code+"][0:"+size2.code+"]","")
                    code i
                    ignore <| p.civar.getAutoVar()
            |_ ->
                ()

        ///<summary>実数型2次元配列を生成し、指定したサイズでメモリ割り当て、ホストに転送→code実行後にメモリ解放</summary>
        static member copyout_d2 (size1:int) = fun (size2:int) code ->
            ch.copyout_d2 size1.I size2.I code

        ///<summary>複素数型1次元配列を生成し、指定したサイズでメモリ割り当て、ホストに転送→code実行後にメモリ解放</summary>
        static member copyout_z2 (size1:num0) = fun (size2:num0) code ->
            match p.lang with
            |F ->
                ch.z2 size1 size2 <| fun i ->
                    p.civar.setVar (Zt, A2(0,0), i.code+"(1:"+size1.code+",1:"+size2.code+")","")
                    code i
                    ignore <| p.civar.getAutoVar()
            |C ->
                ch.z2 size1 size2 <| fun i ->
                    p.civar.setVar (Zt, A2(0,0), i.code+"[0:"+size1.code+"][0:"+size2.code+"]","")
                    code i
                    ignore <| p.civar.getAutoVar()
            |_ ->
                ()

        ///<summary>複素数型1次元配列を生成し、指定したサイズでメモリ割り当て、ホストに転送→code実行後にメモリ解放</summary>
        static member copyout_z2 (size1:int) = fun (size2:int) code ->
            ch.copyout_z2 size1.I size2.I code

        ///<summary>整数型3次元配列を生成し、指定したサイズでメモリ割り当て、ホストに転送→code実行後にメモリ解放</summary>
        static member copyout_i3 (size1:num0) = fun (size2:num0) (size3:num0) code ->
            match p.lang with
            |F ->
                ch.i3 size1 size2 size3 <| fun i ->
                    p.covar.setVar (It 4, A3(0,0,0), i.code+"(1:"+size1.code+",1:"+size2.code+",1:"+size3.code+")","")
                    code i
                    ignore <| p.covar.getAutoVar()
            |C ->
                ch.i3 size1 size2 size3 <| fun i ->
                    p.covar.setVar (It 4, A3(0,0,0), i.code+"[0:"+size1.code+"][0:"+size2.code+"][0:"+size3.code+"]","")
                    code i
                    ignore <| p.covar.getAutoVar()
            |_ ->
                ()

        ///<summary>整数型3次元配列を生成し、指定したサイズでメモリ割り当て、ホストに転送→code実行後にメモリ解放</summary>
        static member copyout_i3 (size1:int) = fun (size2:int) (size3:int) code ->
            ch.copyout_i3 size1.I size2.I size3.I code

        ///<summary>実数型3次元配列を生成し、指定したサイズでメモリ割り当て、ホストに転送→code実行後にメモリ解放</summary>
        static member copyout_d3 (size1:num0) = fun (size2:num0) (size3:num0) code ->
            match p.lang with
            |F ->
                ch.d3 size1 size2 size3 <| fun i ->
                    p.covar.setVar (Dt, A3(0,0,0), i.code+"(1:"+size1.code+",1:"+size2.code+",1:"+size3.code+")","")
                    code i
                    ignore <| p.covar.getAutoVar()
            |C ->
                ch.d3 size1 size2 size3 <| fun i ->
                    p.covar.setVar (Dt, A3(0,0,0), i.code+"[0:"+size1.code+"][0:"+size2.code+"][0:"+size3.code+"]","")
                    code i
                    ignore <| p.covar.getAutoVar()
            |_ ->
                ()

        ///<summary>実数型3次元配列を生成し、指定したサイズでメモリ割り当て、ホストに転送→code実行後にメモリ解放</summary>
        static member copyout_d3 (size1:int) = fun (size2:int) (size3:int) code ->
            ch.copyout_d3 size1.I size2.I size3.I code

        ///<summary>複素数型3次元配列を生成し、指定したサイズでメモリ割り当て、ホストに転送→code実行後にメモリ解放</summary>
        static member copyout_z3 (size1:num0) = fun (size2:num0) (size3:num0) code ->
            match p.lang with
            |F ->
                ch.z3 size1 size2 size3 <| fun i ->
                    p.covar.setVar (Zt, A3(0,0,0), i.code+"(1:"+size1.code+",1:"+size2.code+",1:"+size3.code+")","")
                    code i
                    ignore <| p.covar.getAutoVar()
            |C ->
                ch.z3 size1 size2 size3 <| fun i ->
                    p.covar.setVar (Zt, A3(0,0,0), i.code+"[0:"+size1.code+"][0:"+size2.code+"][0:"+size3.code+"]","")
                    code i
                    ignore <| p.covar.getAutoVar()
            |_ ->
                ()

        ///<summary>実数型3次元配列を生成し、指定したサイズでメモリ割り当て、ホストに転送→code実行後にメモリ解放</summary>
        static member copyout_z3 (size1:int) = fun (size2:int) (size3:int) code ->
            ch.copyout_z3 size1.I size2.I size3.I code

        ///<summary>整数型一時変数(GPUに転送する変数)の生成</summary>
        static member copyin_i code =
            ch.i <| fun v ->
                p.civar.setVar(It 4,A0,v.code,"")
                code(v)
                ignore <| p.civar.getAutoVar()
                
        ///<summary>倍精度浮動小数点型一時変数(GPUに転送する変数)の生成</summary>
        static member copyin_d code =
            ch.d <| fun v ->
                p.civar.setVar(Dt,A0,v.code,"")
                code(v)
                ignore <| p.civar.getAutoVar()
                
        ///<summary>複素数型一時変数(GPUに転送する変数)の生成</summary>
        static member copyin_z code =
            ch.z <| fun v ->
                p.civar.setVar(Zt,A0,v.code,"")
                code(v)
                ignore <| p.civar.getAutoVar()
                
        ///<summary>整数型一時変数(ホストに転送する変数)の生成</summary>
        static member copyout_i code =
            ch.i <| fun v ->
                p.covar.setVar(It 4,A0,v.code,"")
                code(v)
                ignore <| p.covar.getAutoVar()
                
        ///<summary>倍精度浮動小数点型一時変数(ホストに転送する変数)の生成</summary>
        static member copyout_d code =
            ch.d <| fun v ->
                p.covar.setVar(It 4,A0,v.code,"")
                code(v)
                ignore <| p.covar.getAutoVar()
                
        ///<summary>複素数型一時変数(ホストに転送する変数)の生成</summary>
        static member copyout_z code =
            ch.z <| fun v ->
                p.covar.setVar(It 4,A0,v.code,"")
                code(v)
                ignore <| p.covar.getAutoVar()
                
        ///<summary>整数型一時変数(プライベート変数)の生成</summary>
        static member private_i code =
            ch.i <| fun v ->
                p.pvar.setVar(It 4, A0, v.code,"")
                code(v)

        ///<summary>倍精度浮動小数点型一時変数(プライベート変数)の生成</summary>
        static member private_d code =
            ch.d <| fun v ->
                p.pvar.setVar(Dt, A0, v.code,"")
                code(v)

        ///<summary>複素数型一時変数(プライベート変数)の生成</summary>
        static member private_z code =
            ch.z <| fun v ->
                p.pvar.setVar(Zt, A0, v.code,"")
                code(v)

        static member copyin_ii code = 
            ch.copyin_i <| fun i1 -> 
                ch.copyin_i <| fun i2 -> 
                    code(i1,i2)
                        
        static member copyin_id code = 
            ch.copyin_i <| fun i1 -> 
                ch.copyin_d <| fun d2 -> 
                    code(i1,d2)
                    
        static member copyin_iz code = 
            ch.copyin_i <| fun i1 -> 
                ch.copyin_z <| fun z2 -> 
                    code(i1,z2)
                    
        static member copyin_dd code = 
            ch.copyin_d <| fun d1 -> 
                ch.copyin_d <| fun d2 -> 
                    code(d1,d2)
                    
        static member copyin_dz code = 
            ch.copyin_d <| fun d1 -> 
                ch.copyin_z <| fun z2 -> 
                    code(d1,z2)
                    
        static member copyin_zz code = 
            ch.copyin_z <| fun z1 -> 
                ch.copyin_z <| fun z2 -> 
                    code(z1,z2)
                    
        static member copyin_iii code = 
            ch.copyin_i <| fun i1 -> 
                ch.copyin_i <| fun i2 -> 
                    ch.copyin_i <| fun i3 -> 
                        code(i1,i2,i3)
                        
        static member copyin_iid code = 
            ch.copyin_i <| fun i1 -> 
                ch.copyin_i <| fun i2 -> 
                    ch.copyin_d <| fun d3 -> 
                        code(i1,i2,d3)
                        
        static member copyin_iiz code = 
            ch.copyin_i <| fun i1 -> 
                ch.copyin_i <| fun i2 -> 
                    ch.copyin_z <| fun z3 -> 
                        code(i1,i2,z3)
                        
        static member copyin_idd code = 
            ch.copyin_i <| fun i1 -> 
                ch.copyin_d <| fun d2 -> 
                    ch.copyin_d <| fun d3 -> 
                        code(i1,d2,d3)
                        
        static member copyin_idz code = 
            ch.copyin_i <| fun i1 -> 
                ch.copyin_d <| fun d2 -> 
                    ch.copyin_z <| fun z3 -> 
                        code(i1,d2,z3)
                        
        static member copyin_izz code = 
            ch.copyin_i <| fun i1 -> 
                ch.copyin_z <| fun z2 -> 
                    ch.copyin_z <| fun z3 -> 
                        code(i1,z2,z3)
                        
        static member copyin_ddd code = 
            ch.copyin_d <| fun d1 -> 
                ch.copyin_d <| fun d2 -> 
                    ch.copyin_d <| fun d3 -> 
                        code(d1,d2,d3)
                        
        static member copyin_ddz code = 
            ch.copyin_d <| fun d1 -> 
                ch.copyin_d <| fun d2 -> 
                    ch.copyin_z <| fun z3 -> 
                        code(d1,d2,z3)
                        
        static member copyin_dzz code = 
            ch.copyin_d <| fun d1 -> 
                ch.copyin_z <| fun z2 -> 
                    ch.copyin_z <| fun z3 -> 
                        code(d1,z2,z3)
                        
        static member copyin_zzz code = 
            ch.copyin_z <| fun z1 -> 
                ch.copyin_z <| fun z2 -> 
                    ch.copyin_z <| fun z3 -> 
                        code(z1,z2,z3)
                        
        static member copyin_iiii code = 
            ch.copyin_i <| fun i1 -> 
                ch.copyin_i <| fun i2 -> 
                    ch.copyin_i <| fun i3 -> 
                        ch.copyin_i <| fun i4 -> 
                            code(i1,i2,i3,i4)
                            
        static member copyin_iiid code = 
            ch.copyin_i <| fun i1 -> 
                ch.copyin_i <| fun i2 -> 
                    ch.copyin_i <| fun i3 -> 
                        ch.copyin_d <| fun d4 -> 
                            code(i1,i2,i3,d4)
                            
        static member copyin_iiiz code = 
            ch.copyin_i <| fun i1 -> 
                ch.copyin_i <| fun i2 -> 
                    ch.copyin_i <| fun i3 -> 
                        ch.copyin_z <| fun z4 -> 
                            code(i1,i2,i3,z4)
                            
        static member copyin_iidd code = 
            ch.copyin_i <| fun i1 -> 
                ch.copyin_i <| fun i2 -> 
                    ch.copyin_d <| fun d3 -> 
                        ch.copyin_d <| fun d4 -> 
                            code(i1,i2,d3,d4)
                            
        static member copyin_iidz code = 
            ch.copyin_i <| fun i1 -> 
                ch.copyin_i <| fun i2 -> 
                    ch.copyin_d <| fun d3 -> 
                        ch.copyin_z <| fun z4 -> 
                            code(i1,i2,d3,z4)
                            
        static member copyin_iizz code = 
            ch.copyin_i <| fun i1 -> 
                ch.copyin_i <| fun i2 -> 
                    ch.copyin_z <| fun z3 -> 
                        ch.copyin_z <| fun z4 -> 
                            code(i1,i2,z3,z4)
                            
        static member copyin_iddd code = 
            ch.copyin_i <| fun i1 -> 
                ch.copyin_d <| fun d2 -> 
                    ch.copyin_d <| fun d3 -> 
                        ch.copyin_d <| fun d4 -> 
                            code(i1,d2,d3,d4)
                            
        static member copyin_iddz code = 
            ch.copyin_i <| fun i1 -> 
                ch.copyin_d <| fun d2 -> 
                    ch.copyin_d <| fun d3 -> 
                        ch.copyin_z <| fun z4 -> 
                            code(i1,d2,d3,z4)
                            
        static member copyin_idzz code = 
            ch.copyin_i <| fun i1 -> 
                ch.copyin_d <| fun d2 -> 
                    ch.copyin_z <| fun z3 -> 
                        ch.copyin_z <| fun z4 -> 
                            code(i1,d2,z3,z4)
                            
        static member copyin_izzz code = 
            ch.copyin_i <| fun i1 -> 
                ch.copyin_z <| fun z2 -> 
                    ch.copyin_z <| fun z3 -> 
                        ch.copyin_z <| fun z4 -> 
                            code(i1,z2,z3,z4)
                            
        static member copyin_dddd code = 
            ch.copyin_d <| fun d1 -> 
                ch.copyin_d <| fun d2 -> 
                    ch.copyin_d <| fun d3 -> 
                        ch.copyin_d <| fun d4 -> 
                            code(d1,d2,d3,d4)
                            
        static member copyin_dddz code = 
            ch.copyin_d <| fun d1 -> 
                ch.copyin_d <| fun d2 -> 
                    ch.copyin_d <| fun d3 -> 
                        ch.copyin_z <| fun z4 -> 
                            code(d1,d2,d3,z4)
                            
        static member copyin_ddzz code = 
            ch.copyin_d <| fun d1 -> 
                ch.copyin_d <| fun d2 -> 
                    ch.copyin_z <| fun z3 -> 
                        ch.copyin_z <| fun z4 -> 
                            code(d1,d2,z3,z4)
                            
        static member copyin_dzzz code = 
            ch.copyin_d <| fun d1 -> 
                ch.copyin_z <| fun z2 -> 
                    ch.copyin_z <| fun z3 -> 
                        ch.copyin_z <| fun z4 -> 
                            code(d1,z2,z3,z4)
                            
        static member copyin_zzzz code = 
            ch.copyin_z <| fun z1 -> 
                ch.copyin_z <| fun z2 -> 
                    ch.copyin_z <| fun z3 -> 
                        ch.copyin_z <| fun z4 -> 
                            code(z1,z2,z3,z4)

        static member copyout_ii code = 
            ch.copyout_i <| fun i1 -> 
                ch.copyout_i <| fun i2 -> 
                    code(i1,i2)
                        
        static member copyout_id code = 
            ch.copyout_i <| fun i1 -> 
                ch.copyout_d <| fun d2 -> 
                    code(i1,d2)
                    
        static member copyout_iz code = 
            ch.copyout_i <| fun i1 -> 
                ch.copyout_z <| fun z2 -> 
                    code(i1,z2)
                    
        static member copyout_dd code = 
            ch.copyout_d <| fun d1 -> 
                ch.copyout_d <| fun d2 -> 
                    code(d1,d2)
                    
        static member copyout_dz code = 
            ch.copyout_d <| fun d1 -> 
                ch.copyout_z <| fun z2 -> 
                    code(d1,z2)
                    
        static member copyout_zz code = 
            ch.copyout_z <| fun z1 -> 
                ch.copyout_z <| fun z2 -> 
                    code(z1,z2)
                    
        static member copyout_iii code = 
            ch.copyout_i <| fun i1 -> 
                ch.copyout_i <| fun i2 -> 
                    ch.copyout_i <| fun i3 -> 
                        code(i1,i2,i3)
                        
        static member copyout_iid code = 
            ch.copyout_i <| fun i1 -> 
                ch.copyout_i <| fun i2 -> 
                    ch.copyout_d <| fun d3 -> 
                        code(i1,i2,d3)
                        
        static member copyout_iiz code = 
            ch.copyout_i <| fun i1 -> 
                ch.copyout_i <| fun i2 -> 
                    ch.copyout_z <| fun z3 -> 
                        code(i1,i2,z3)
                        
        static member copyout_idd code = 
            ch.copyout_i <| fun i1 -> 
                ch.copyout_d <| fun d2 -> 
                    ch.copyout_d <| fun d3 -> 
                        code(i1,d2,d3)
                        
        static member copyout_idz code = 
            ch.copyout_i <| fun i1 -> 
                ch.copyout_d <| fun d2 -> 
                    ch.copyout_z <| fun z3 -> 
                        code(i1,d2,z3)
                        
        static member copyout_izz code = 
            ch.copyout_i <| fun i1 -> 
                ch.copyout_z <| fun z2 -> 
                    ch.copyout_z <| fun z3 -> 
                        code(i1,z2,z3)
                        
        static member copyout_ddd code = 
            ch.copyout_d <| fun d1 -> 
                ch.copyout_d <| fun d2 -> 
                    ch.copyout_d <| fun d3 -> 
                        code(d1,d2,d3)
                        
        static member copyout_ddz code = 
            ch.copyout_d <| fun d1 -> 
                ch.copyout_d <| fun d2 -> 
                    ch.copyout_z <| fun z3 -> 
                        code(d1,d2,z3)
                        
        static member copyout_dzz code = 
            ch.copyout_d <| fun d1 -> 
                ch.copyout_z <| fun z2 -> 
                    ch.copyout_z <| fun z3 -> 
                        code(d1,z2,z3)
                        
        static member copyout_zzz code = 
            ch.copyout_z <| fun z1 -> 
                ch.copyout_z <| fun z2 -> 
                    ch.copyout_z <| fun z3 -> 
                        code(z1,z2,z3)
                        
        static member copyout_iiii code = 
            ch.copyout_i <| fun i1 -> 
                ch.copyout_i <| fun i2 -> 
                    ch.copyout_i <| fun i3 -> 
                        ch.copyout_i <| fun i4 -> 
                            code(i1,i2,i3,i4)
                            
        static member copyout_iiid code = 
            ch.copyout_i <| fun i1 -> 
                ch.copyout_i <| fun i2 -> 
                    ch.copyout_i <| fun i3 -> 
                        ch.copyout_d <| fun d4 -> 
                            code(i1,i2,i3,d4)
                            
        static member copyout_iiiz code = 
            ch.copyout_i <| fun i1 -> 
                ch.copyout_i <| fun i2 -> 
                    ch.copyout_i <| fun i3 -> 
                        ch.copyout_z <| fun z4 -> 
                            code(i1,i2,i3,z4)
                            
        static member copyout_iidd code = 
            ch.copyout_i <| fun i1 -> 
                ch.copyout_i <| fun i2 -> 
                    ch.copyout_d <| fun d3 -> 
                        ch.copyout_d <| fun d4 -> 
                            code(i1,i2,d3,d4)
                            
        static member copyout_iidz code = 
            ch.copyout_i <| fun i1 -> 
                ch.copyout_i <| fun i2 -> 
                    ch.copyout_d <| fun d3 -> 
                        ch.copyout_z <| fun z4 -> 
                            code(i1,i2,d3,z4)
                            
        static member copyout_iizz code = 
            ch.copyout_i <| fun i1 -> 
                ch.copyout_i <| fun i2 -> 
                    ch.copyout_z <| fun z3 -> 
                        ch.copyout_z <| fun z4 -> 
                            code(i1,i2,z3,z4)
                            
        static member copyout_iddd code = 
            ch.copyout_i <| fun i1 -> 
                ch.copyout_d <| fun d2 -> 
                    ch.copyout_d <| fun d3 -> 
                        ch.copyout_d <| fun d4 -> 
                            code(i1,d2,d3,d4)
                            
        static member copyout_iddz code = 
            ch.copyout_i <| fun i1 -> 
                ch.copyout_d <| fun d2 -> 
                    ch.copyout_d <| fun d3 -> 
                        ch.copyout_z <| fun z4 -> 
                            code(i1,d2,d3,z4)
                            
        static member copyout_idzz code = 
            ch.copyout_i <| fun i1 -> 
                ch.copyout_d <| fun d2 -> 
                    ch.copyout_z <| fun z3 -> 
                        ch.copyout_z <| fun z4 -> 
                            code(i1,d2,z3,z4)
                            
        static member copyout_izzz code = 
            ch.copyout_i <| fun i1 -> 
                ch.copyout_z <| fun z2 -> 
                    ch.copyout_z <| fun z3 -> 
                        ch.copyout_z <| fun z4 -> 
                            code(i1,z2,z3,z4)
                            
        static member copyout_dddd code = 
            ch.copyout_d <| fun d1 -> 
                ch.copyout_d <| fun d2 -> 
                    ch.copyout_d <| fun d3 -> 
                        ch.copyout_d <| fun d4 -> 
                            code(d1,d2,d3,d4)
                            
        static member copyout_dddz code = 
            ch.copyout_d <| fun d1 -> 
                ch.copyout_d <| fun d2 -> 
                    ch.copyout_d <| fun d3 -> 
                        ch.copyout_z <| fun z4 -> 
                            code(d1,d2,d3,z4)
                            
        static member copyout_ddzz code = 
            ch.copyout_d <| fun d1 -> 
                ch.copyout_d <| fun d2 -> 
                    ch.copyout_z <| fun z3 -> 
                        ch.copyout_z <| fun z4 -> 
                            code(d1,d2,z3,z4)
                            
        static member copyout_dzzz code = 
            ch.copyout_d <| fun d1 -> 
                ch.copyout_z <| fun z2 -> 
                    ch.copyout_z <| fun z3 -> 
                        ch.copyout_z <| fun z4 -> 
                            code(d1,z2,z3,z4)
                            
        static member copyout_zzzz code = 
            ch.copyout_z <| fun z1 -> 
                ch.copyout_z <| fun z2 -> 
                    ch.copyout_z <| fun z3 -> 
                        ch.copyout_z <| fun z4 -> 
                            code(z1,z2,z3,z4)

        static member private_ii code = 
            ch.private_i <| fun i1 -> 
                ch.private_i <| fun i2 -> 
                    code(i1,i2)
                        
        static member private_id code = 
            ch.private_i <| fun i1 -> 
                ch.private_d <| fun d2 -> 
                    code(i1,d2)
                    
        static member private_iz code = 
            ch.private_i <| fun i1 -> 
                ch.private_z <| fun z2 -> 
                    code(i1,z2)
                    
        static member private_dd code = 
            ch.private_d <| fun d1 -> 
                ch.private_d <| fun d2 -> 
                    code(d1,d2)
                    
        static member private_dz code = 
            ch.private_d <| fun d1 -> 
                ch.private_z <| fun z2 -> 
                    code(d1,z2)
                    
        static member private_zz code = 
            ch.private_z <| fun z1 -> 
                ch.private_z <| fun z2 -> 
                    code(z1,z2)
                    
        static member private_iii code = 
            ch.private_i <| fun i1 -> 
                ch.private_i <| fun i2 -> 
                    ch.private_i <| fun i3 -> 
                        code(i1,i2,i3)
                        
        static member private_iid code = 
            ch.private_i <| fun i1 -> 
                ch.private_i <| fun i2 -> 
                    ch.private_d <| fun d3 -> 
                        code(i1,i2,d3)
                        
        static member private_iiz code = 
            ch.private_i <| fun i1 -> 
                ch.private_i <| fun i2 -> 
                    ch.private_z <| fun z3 -> 
                        code(i1,i2,z3)
                        
        static member private_idd code = 
            ch.private_i <| fun i1 -> 
                ch.private_d <| fun d2 -> 
                    ch.private_d <| fun d3 -> 
                        code(i1,d2,d3)
                        
        static member private_idz code = 
            ch.private_i <| fun i1 -> 
                ch.private_d <| fun d2 -> 
                    ch.private_z <| fun z3 -> 
                        code(i1,d2,z3)
                        
        static member private_izz code = 
            ch.private_i <| fun i1 -> 
                ch.private_z <| fun z2 -> 
                    ch.private_z <| fun z3 -> 
                        code(i1,z2,z3)
                        
        static member private_ddd code = 
            ch.private_d <| fun d1 -> 
                ch.private_d <| fun d2 -> 
                    ch.private_d <| fun d3 -> 
                        code(d1,d2,d3)
                        
        static member private_ddz code = 
            ch.private_d <| fun d1 -> 
                ch.private_d <| fun d2 -> 
                    ch.private_z <| fun z3 -> 
                        code(d1,d2,z3)
                        
        static member private_dzz code = 
            ch.private_d <| fun d1 -> 
                ch.private_z <| fun z2 -> 
                    ch.private_z <| fun z3 -> 
                        code(d1,z2,z3)
                        
        static member private_zzz code = 
            ch.private_z <| fun z1 -> 
                ch.private_z <| fun z2 -> 
                    ch.private_z <| fun z3 -> 
                        code(z1,z2,z3)
                        
        static member private_iiii code = 
            ch.private_i <| fun i1 -> 
                ch.private_i <| fun i2 -> 
                    ch.private_i <| fun i3 -> 
                        ch.private_i <| fun i4 -> 
                            code(i1,i2,i3,i4)
                            
        static member private_iiid code = 
            ch.private_i <| fun i1 -> 
                ch.private_i <| fun i2 -> 
                    ch.private_i <| fun i3 -> 
                        ch.private_d <| fun d4 -> 
                            code(i1,i2,i3,d4)
                            
        static member private_iiiz code = 
            ch.private_i <| fun i1 -> 
                ch.private_i <| fun i2 -> 
                    ch.private_i <| fun i3 -> 
                        ch.private_z <| fun z4 -> 
                            code(i1,i2,i3,z4)
                            
        static member private_iidd code = 
            ch.private_i <| fun i1 -> 
                ch.private_i <| fun i2 -> 
                    ch.private_d <| fun d3 -> 
                        ch.private_d <| fun d4 -> 
                            code(i1,i2,d3,d4)
                            
        static member private_iidz code = 
            ch.private_i <| fun i1 -> 
                ch.private_i <| fun i2 -> 
                    ch.private_d <| fun d3 -> 
                        ch.private_z <| fun z4 -> 
                            code(i1,i2,d3,z4)
                            
        static member private_iizz code = 
            ch.private_i <| fun i1 -> 
                ch.private_i <| fun i2 -> 
                    ch.private_z <| fun z3 -> 
                        ch.private_z <| fun z4 -> 
                            code(i1,i2,z3,z4)
                            
        static member private_iddd code = 
            ch.private_i <| fun i1 -> 
                ch.private_d <| fun d2 -> 
                    ch.private_d <| fun d3 -> 
                        ch.private_d <| fun d4 -> 
                            code(i1,d2,d3,d4)
                            
        static member private_iddz code = 
            ch.private_i <| fun i1 -> 
                ch.private_d <| fun d2 -> 
                    ch.private_d <| fun d3 -> 
                        ch.private_z <| fun z4 -> 
                            code(i1,d2,d3,z4)
                            
        static member private_idzz code = 
            ch.private_i <| fun i1 -> 
                ch.private_d <| fun d2 -> 
                    ch.private_z <| fun z3 -> 
                        ch.private_z <| fun z4 -> 
                            code(i1,d2,z3,z4)
                            
        static member private_izzz code = 
            ch.private_i <| fun i1 -> 
                ch.private_z <| fun z2 -> 
                    ch.private_z <| fun z3 -> 
                        ch.private_z <| fun z4 -> 
                            code(i1,z2,z3,z4)
                            
        static member private_dddd code = 
            ch.private_d <| fun d1 -> 
                ch.private_d <| fun d2 -> 
                    ch.private_d <| fun d3 -> 
                        ch.private_d <| fun d4 -> 
                            code(d1,d2,d3,d4)
                            
        static member private_dddz code = 
            ch.private_d <| fun d1 -> 
                ch.private_d <| fun d2 -> 
                    ch.private_d <| fun d3 -> 
                        ch.private_z <| fun z4 -> 
                            code(d1,d2,d3,z4)
                            
        static member private_ddzz code = 
            ch.private_d <| fun d1 -> 
                ch.private_d <| fun d2 -> 
                    ch.private_z <| fun z3 -> 
                        ch.private_z <| fun z4 -> 
                            code(d1,d2,z3,z4)
                            
        static member private_dzzz code = 
            ch.private_d <| fun d1 -> 
                ch.private_z <| fun z2 -> 
                    ch.private_z <| fun z3 -> 
                        ch.private_z <| fun z4 -> 
                            code(d1,z2,z3,z4)
                            
        static member private_zzzz code = 
            ch.private_z <| fun z1 -> 
                ch.private_z <| fun z2 -> 
                    ch.private_z <| fun z3 -> 
                        ch.private_z <| fun z4 -> 
                            code(z1,z2,z3,z4)
                            
    ///<summary>一時変数の生成と使用（処理スキップ）</summary>
    type dummy_ch () =
        
        ///<summary>整数型一時変数の生成</summary>
        static member i code = ()
            
        ///<summary>倍精度浮動小数点型一時変数の生成</summary>
        static member d code = ()
            
        ///<summary>複素数型一時変数の生成</summary>
        static member z code = ()
            
        ///<summary>整数型一時変数を生成し、valueを代入してからcodeで使用</summary>
        static member si (value:num0) = ()
                
        ///<summary>整数型一時変数を生成し、valueを代入してからcodeで使用</summary>
        static member si (value:int) = ()
                
        ///<summary>倍精度浮動小数点型一時変数を生成し、valueを代入してからcodeで使用</summary>
        static member sd (value:num0) = ()
                
        ///<summary>倍精度浮動小数点型一時変数を生成し、valueを代入してからcodeで使用</summary>
        static member sd (value:double) = ()
                
        ///<summary>複素数型一時変数を生成し、valueを代入してからcodeで使用</summary>
        static member sz (value:num0) = ()
                
        ///<summary>複素数型一時変数を生成し、valueを代入してからcodeで使用</summary>
        static member sz (re:double,im:double) = ()
                
        ///<summary>vと同じ型の一時変数を生成</summary>
        static member n (v:num0) = ()
                    
        ///<summary>vと同じ型の一時変数を生成</summary>
        static member n (v:num1) = ()
                    
        ///<summary>vと同じ型の一時変数を生成</summary>
        static member n (v:num2) = ()
                    
        ///<summary>vと同じ型の一時変数を生成</summary>
        static member n (v:num3) = ()
    
        ///<summary>整数型1次元配列を生成</summary>
        static member i01 code = ()
            
        ///<summary>実数型1次元配列を生成</summary>
        static member d01 code = ()
            
        ///<summary>複素数型1次元配列を生成</summary>
        static member z01 code = ()
            
        ///<summary>整数型1次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
        static member i1 (size1:num0) code = ()
            
        ///<summary>実数型1次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
        static member d1 (size1:num0) code = ()
            
        ///<summary>複素数型1次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
        static member z1 (size1:num0) code = ()
            
        ///<summary>整数型2次元配列を生成</summary>
        static member i02 code = ()
            
        ///<summary>実数型2次元配列を生成</summary>
        static member d02 code = ()
            
        ///<summary>複素数型2次元配列を生成</summary>
        static member z02 code = ()
            
        ///<summary>整数型2次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
        static member i2 (size1:num0) (size2:num0) code = ()
            
        ///<summary>実数型2次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
        static member d2 (size1:num0) (size2:num0) code = ()
            
        ///<summary>複素数型2次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
        static member z2 (size1:num0) (size2:num0) code = ()
            
        ///<summary>整数型3次元配列を生成</summary>
        static member i03 code = ()
            
        ///<summary>実数型3次元配列を生成</summary>
        static member d03 code = ()
            
        ///<summary>複素数型3次元配列を生成</summary>
        static member z03 code = ()
            
        ///<summary>整数型3次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
        static member i3 (size1:num0) (size2:num0) (size3:num0) code = ()
            
        ///<summary>実数型3次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
        static member d3 (size1:num0) (size2:num0) (size3:num0) code = ()
            
        ///<summary>複素数型3次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
        static member z3 (size1:num0) (size2:num0) (size3:num0) code = ()
    
        static member ii code = 
            ch.i <| fun i1 -> 
                ch.i <| fun i2 -> 
                    ()
                    
        static member id code = 
            ch.i <| fun i1 -> 
                ch.d <| fun d2 -> 
                    ()
                    
        static member iz code = 
            ch.i <| fun i1 -> 
                ch.z <| fun z2 -> 
                    ()
                    
        static member dd code = 
            ch.d <| fun d1 -> 
                ch.d <| fun d2 -> 
                    ()
                    
        static member dz code = 
            ch.d <| fun d1 -> 
                ch.z <| fun z2 -> 
                    ()
                    
        static member zz code = 
            ch.z <| fun z1 -> 
                ch.z <| fun z2 -> 
                    ()
                    
        static member iii code = 
            ch.i <| fun i1 -> 
                ch.i <| fun i2 -> 
                    ch.i <| fun i3 -> 
                        ()
                        
        static member iid code = 
            ch.i <| fun i1 -> 
                ch.i <| fun i2 -> 
                    ch.d <| fun d3 -> 
                        ()
                        
        static member iiz code = 
            ch.i <| fun i1 -> 
                ch.i <| fun i2 -> 
                    ch.z <| fun z3 -> 
                        ()
                        
        static member idd code = 
            ch.i <| fun i1 -> 
                ch.d <| fun d2 -> 
                    ch.d <| fun d3 -> 
                        ()
                        
        static member idz code = 
            ch.i <| fun i1 -> 
                ch.d <| fun d2 -> 
                    ch.z <| fun z3 -> 
                        ()
                        
        static member izz code = 
            ch.i <| fun i1 -> 
                ch.z <| fun z2 -> 
                    ch.z <| fun z3 -> 
                        ()
                        
        static member ddd code = 
            ch.d <| fun d1 -> 
                ch.d <| fun d2 -> 
                    ch.d <| fun d3 -> 
                        ()
                        
        static member ddz code = 
            ch.d <| fun d1 -> 
                ch.d <| fun d2 -> 
                    ch.z <| fun z3 -> 
                        ()
                        
        static member dzz code = 
            ch.d <| fun d1 -> 
                ch.z <| fun z2 -> 
                    ch.z <| fun z3 -> 
                        ()
                        
        static member zzz code = 
            ch.z <| fun z1 -> 
                ch.z <| fun z2 -> 
                    ch.z <| fun z3 -> 
                        ()
                        
        static member iiii code = 
            ch.i <| fun i1 -> 
                ch.i <| fun i2 -> 
                    ch.i <| fun i3 -> 
                        ch.i <| fun i4 -> 
                            ()
                            
        static member iiid code = 
            ch.i <| fun i1 -> 
                ch.i <| fun i2 -> 
                    ch.i <| fun i3 -> 
                        ch.d <| fun d4 -> 
                            ()
                            
        static member iiiz code = 
            ch.i <| fun i1 -> 
                ch.i <| fun i2 -> 
                    ch.i <| fun i3 -> 
                        ch.z <| fun z4 -> 
                            ()
                            
        static member iidd code = 
            ch.i <| fun i1 -> 
                ch.i <| fun i2 -> 
                    ch.d <| fun d3 -> 
                        ch.d <| fun d4 -> 
                            ()
                            
        static member iidz code = 
            ch.i <| fun i1 -> 
                ch.i <| fun i2 -> 
                    ch.d <| fun d3 -> 
                        ch.z <| fun z4 -> 
                            ()
                            
        static member iizz code = 
            ch.i <| fun i1 -> 
                ch.i <| fun i2 -> 
                    ch.z <| fun z3 -> 
                        ch.z <| fun z4 -> 
                            ()
                            
        static member iddd code = 
            ch.i <| fun i1 -> 
                ch.d <| fun d2 -> 
                    ch.d <| fun d3 -> 
                        ch.d <| fun d4 -> 
                            ()
                            
        static member iddz code = 
            ch.i <| fun i1 -> 
                ch.d <| fun d2 -> 
                    ch.d <| fun d3 -> 
                        ch.z <| fun z4 -> 
                            ()
                            
        static member idzz code = 
            ch.i <| fun i1 -> 
                ch.d <| fun d2 -> 
                    ch.z <| fun z3 -> 
                        ch.z <| fun z4 -> 
                            ()
                            
        static member izzz code = 
            ch.i <| fun i1 -> 
                ch.z <| fun z2 -> 
                    ch.z <| fun z3 -> 
                        ch.z <| fun z4 -> 
                            ()
                            
        static member dddd code = 
            ch.d <| fun d1 -> 
                ch.d <| fun d2 -> 
                    ch.d <| fun d3 -> 
                        ch.d <| fun d4 -> 
                            ()
                            
        static member dddz code = 
            ch.d <| fun d1 -> 
                ch.d <| fun d2 -> 
                    ch.d <| fun d3 -> 
                        ch.z <| fun z4 -> 
                            ()
                            
        static member ddzz code = 
            ch.d <| fun d1 -> 
                ch.d <| fun d2 -> 
                    ch.z <| fun z3 -> 
                        ch.z <| fun z4 -> 
                            ()
                            
        static member dzzz code = 
            ch.d <| fun d1 -> 
                ch.z <| fun z2 -> 
                    ch.z <| fun z3 -> 
                        ch.z <| fun z4 -> 
                            ()
                            
        static member zzzz code = 
            ch.z <| fun z1 -> 
                ch.z <| fun z2 -> 
                    ch.z <| fun z3 -> 
                        ch.z <| fun z4 -> 
                            ()
                            
        static member count0 code = 
            ch.i <| fun counter -> 
                ()
                
        static member count1 code =
            ch.i <| fun counter ->
                ()
                