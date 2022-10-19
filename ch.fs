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

    ///<summary>一時変数の生成と使用</summary>
    type ch () =
        
        ///<summary>整数型一時変数の生成</summary>
        static member i code = 
            let p = p.param
            let x = p.cachevar (It 4) 0
            code <| Var(It 4,x,[])
            p.dispose (It 4,A0,x)
            
        ///<summary>倍精度浮動小数点型一時変数の生成</summary>
        static member d code = 
            let p = p.param
            let x = p.cachevar Dt 0
            code <| Var(Dt,x,[])
            p.dispose (Dt,A0,x)
            
        ///<summary>複素数型一時変数の生成</summary>
        static member z code = 
            let p = p.param
            let x = p.cachevar Zt 0
            code <| Var(Zt,x,[])
            p.dispose (Zt,A0,x)
            
        ///<summary>整数型一時変数を生成し、valueを代入してからcodeで使用</summary>
        static member si (value:num0) =
            let p = p.param
            fun code ->
                let x = p.cachevar (It 4) 0
                let xx = Var(It 4,x,[])
                xx <== value
                code xx
                p.dispose(It 4,A0,x)
                
        ///<summary>整数型一時変数を生成し、valueを代入してからcodeで使用</summary>
        static member si (value:int) =
            let p = p.param
            fun code ->
                let x = p.cachevar (It 4) 0
                let xx = Var(It 4,x,[])
                xx <== value
                code xx
                p.dispose(It 4,A0,x)
                
        ///<summary>倍精度浮動小数点型一時変数を生成し、valueを代入してからcodeで使用</summary>
        static member sd (value:num0) =
            let p = p.param
            fun code -> 
                let x = p.cachevar Dt 0
                let xx = Var(Dt,x,[])
                xx <== value
                code xx
                p.dispose(Dt,A0,x)
                
        ///<summary>倍精度浮動小数点型一時変数を生成し、valueを代入してからcodeで使用</summary>
        static member sd (value:double) =
            let p = p.param
            fun code -> 
                let x = p.cachevar Dt 0
                let xx = Var(Dt,x,[])
                xx <== value
                code xx
                p.dispose(Dt,A0,x)
                
        ///<summary>複素数型一時変数を生成し、valueを代入してからcodeで使用</summary>
        static member sz (value:num0) =
            let p = p.param
            fun code -> 
                let x = p.cachevar Zt 0
                let xx = Var(Zt,x,[])
                xx <== value
                code xx
                p.dispose(Zt,A0,x)
                
        ///<summary>複素数型一時変数を生成し、valueを代入してからcodeで使用</summary>
        static member sz (re:double,im:double) =
            let p = p.param
            fun code -> 
                let x = p.cachevar Zt 0
                let xx = Var(Zt,x,[])
                xx <== (re,im)
                code xx
                p.dispose(Zt,A0,x)
                
        ///<summary>vと同じ型の一時変数を生成</summary>
        static member n (v:Etype) = 
            fun (code:num0->unit) ->
                match v with
                  |Zt ->
                    ch.z code
                  |Dt ->
                    ch.d code
                  |It _ ->
                    ch.i code
                  |_ -> 
                    printfn "ch.n error"

        ///<summary>vと同じ型の一時変数を生成</summary>
        static member n (v:num0) = fun (code:num0->unit) -> ch.n v.etype code
        static member n (v:num1) = fun (code:num0->unit) -> ch.n v.etype code
        static member n (v:num2) = fun (code:num0->unit) -> ch.n v.etype code
        static member n (v:num3) = fun (code:num0->unit) -> ch.n v.etype code
        
        static member f = 
            fun (code:string->unit) ->
                let name = p.param.f_number()
                match p.lang with
                  |F ->
                      ignore <| p.param.vreg(Structure("file"),A0,name,name.Substring(1))
                  |_ ->
                      ignore <| p.param.vreg(Structure("file"),A0,name,"")
                code name
            
        static member t = 
            fun (code:string->unit) ->
                let name = p.param.t_name()
                ignore <| p.param.vreg(Structure("string"),A0,name,"")
                code name
                
        ///<summary>vと同じ型の一時変数を生成</summary>
        static member n1 (v:Etype,size:num0) = 
            fun (code:num1->unit) ->
                match v with
                  |Zt ->
                    ch.z1 size code
                  |Dt ->
                    ch.d1 size code
                  |It _ ->
                    ch.i1 size code
                  |_ -> 
                    printfn "ch.n1 error"
                    
        static member n1 (v:num0,size1:num0) = fun (code:num1->unit) -> ch.n1 (v.etype, size1) code
        static member n1 (v:num0,size1:int) = fun (code:num1->unit) -> ch.n1 (v.etype, I size1) code
        static member n1 (v:num1) = fun (code:num1->unit) -> ch.n1 (v.etype, v.size1) code
        static member n1 (v:num1,size1:num0) = fun (code:num1->unit) -> ch.n1 (v.etype, size1) code
        static member n1 (v:num1,size1:int) = fun (code:num1->unit) -> ch.n1 (v.etype, I size1) code
        static member n1 (v:num2,size1:num0) = fun (code:num1->unit) -> ch.n1 (v.etype, size1) code
        static member n1 (v:num2,size1:int) = fun (code:num1->unit) -> ch.n1 (v.etype, I size1) code
        static member n1 (v:num3,size1:num0) = fun (code:num1->unit) -> ch.n1 (v.etype,size1) code
        static member n1 (v:num3,size1:int) = fun (code:num1->unit) -> ch.n1 (v.etype,I size1) code
        
        ///<summary>vと同じ型の一時変数を生成</summary>
        static member n2 (v:Etype,n1:num0,n2:num0) = 
            fun (code:num2->unit) ->
                match v with
                  |Zt ->
                    ch.z2 n1 n2 code
                  |Dt ->
                    ch.d2 n1 n2 code
                  |It _ ->
                    ch.i2 n1 n2 code
                  |_ -> 
                    printfn "ch.n2 error"
                    
        ///<summary>vと同じ型の一時変数を生成</summary>
        static member n2 (v:num0,size1:num0,size2:num0) = fun (code:num2->unit) -> ch.n2 (v.etype, size1, size2) code
        static member n2 (v:num0,size1:num0,size2:int) = fun (code:num2->unit) -> ch.n2 (v.etype, size1, I size2) code
        static member n2 (v:num0,size1:int,size2:num0) = fun (code:num2->unit) -> ch.n2 (v.etype, I size1, size2) code
        static member n2 (v:num0,size1:int,size2:int) = fun (code:num2->unit) -> ch.n2 (v.etype, I size1, I size2) code
        static member n2 (v:num1,size1:num0,size2:num0) = fun (code:num2->unit) -> ch.n2 (v.etype, size1, size2) code
        static member n2 (v:num1,size1:num0,size2:int) = fun (code:num2->unit) -> ch.n2 (v.etype, size1, I size2) code
        static member n2 (v:num1,size1:int,size2:num0) = fun (code:num2->unit) -> ch.n2 (v.etype, I size1, size2) code
        static member n2 (v:num1,size1:int,size2:int) = fun (code:num2->unit) -> ch.n2 (v.etype, I size1, I size2) code
        static member n2 (v:num2) = fun (code:num2->unit) -> ch.n2 (v.etype, v.size1, v.size2) code
        static member n2 (v:num2,size1:num0,size2:num0) = fun (code:num2->unit) -> ch.n2 (v.etype, size1, size2) code
        static member n2 (v:num2,size1:num0,size2:int) = fun (code:num2->unit) -> ch.n2 (v.etype, size1, I size2) code
        static member n2 (v:num2,size1:int,size2:num0) = fun (code:num2->unit) -> ch.n2 (v.etype, I size1, size2) code
        static member n2 (v:num2,size1:int,size2:int) = fun (code:num2->unit) -> ch.n2 (v.etype, I size1, I size2) code
        
        ///<summary>vと同じ型の一時変数を生成</summary>
        static member n3 (v:Etype,size1:num0,size2:num0,size3:num0) = 
            fun (code:num3->unit) ->
                match v with
                  |Zt ->
                    ch.z3 size1 size2 size3 code
                  |Dt ->
                    ch.d3 size1 size2 size3 code
                  |It _ ->
                    ch.i3 size1 size2 size3 code
                  |_ -> 
                    printfn "ch.n3 error"
                    
        static member n3 (v:num0,size1:num0,size2:num0,size3:num0) = fun (code:num3->unit) -> ch.n3 (v.etype,size1,size2,size3) code
        static member n3 (v:num0,size1:num0,size2:num0,size3:int) = fun (code:num3->unit) -> ch.n3 (v.etype,size1,size2,I size3) code
        static member n3 (v:num0,size1:num0,size2:int,size3:num0) = fun (code:num3->unit) -> ch.n3 (v.etype,size1,I size2,size3) code
        static member n3 (v:num0,size1:num0,size2:int,size3:int) = fun (code:num3->unit) -> ch.n3 (v.etype,size1,I size2,I size3) code
        static member n3 (v:num0,size1:int,size2:num0,size3:num0) = fun (code:num3->unit) -> ch.n3 (v.etype,I size1,size2,size3) code
        static member n3 (v:num0,size1:int,size2:num0,size3:int) = fun (code:num3->unit) -> ch.n3 (v.etype,I size1,size2,I size3) code
        static member n3 (v:num0,size1:int,size2:int,size3:num0) = fun (code:num3->unit) -> ch.n3 (v.etype,I size1,I size2,size3) code
        static member n3 (v:num0,size1:int,size2:int,size3:int) = fun (code:num3->unit) -> ch.n3 (v.etype,I size1,I size2,I size3) code
        static member n3 (v:num1,size1:num0,size2:num0,size3:num0) = fun (code:num3->unit) -> ch.n3 (v.etype,size1,size2,size3) code
        static member n3 (v:num1,size1:num0,size2:num0,size3:int) = fun (code:num3->unit) -> ch.n3 (v.etype,size1,size2,I size3) code
        static member n3 (v:num1,size1:num0,size2:int,size3:num0) = fun (code:num3->unit) -> ch.n3 (v.etype,size1,I size2,size3) code
        static member n3 (v:num1,size1:num0,size2:int,size3:int) = fun (code:num3->unit) -> ch.n3 (v.etype,size1,I size2,I size3) code
        static member n3 (v:num1,size1:int,size2:num0,size3:num0) = fun (code:num3->unit) -> ch.n3 (v.etype,I size1,size2,size3) code
        static member n3 (v:num1,size1:int,size2:num0,size3:int) = fun (code:num3->unit) -> ch.n3 (v.etype,I size1,size2,I size3) code
        static member n3 (v:num1,size1:int,size2:int,size3:num0) = fun (code:num3->unit) -> ch.n3 (v.etype,I size1,I size2,size3) code
        static member n3 (v:num1,size1:int,size2:int,size3:int) = fun (code:num3->unit) -> ch.n3 (v.etype,I size1,I size2,I size3) code
        static member n3 (v:num2,size1:num0,size2:num0,size3:num0) = fun (code:num3->unit) -> ch.n3 (v.etype,size1,size2,size3) code
        static member n3 (v:num2,size1:num0,size2:num0,size3:int) = fun (code:num3->unit) -> ch.n3 (v.etype,size1,size2,I size3) code
        static member n3 (v:num2,size1:num0,size2:int,size3:num0) = fun (code:num3->unit) -> ch.n3 (v.etype,size1,I size2,size3) code
        static member n3 (v:num2,size1:num0,size2:int,size3:int) = fun (code:num3->unit) -> ch.n3 (v.etype,size1,I size2,I size3) code
        static member n3 (v:num2,size1:int,size2:num0,size3:num0) = fun (code:num3->unit) -> ch.n3 (v.etype,I size1,size2,size3) code
        static member n3 (v:num2,size1:int,size2:num0,size3:int) = fun (code:num3->unit) -> ch.n3 (v.etype,I size1,size2,I size3) code
        static member n3 (v:num2,size1:int,size2:int,size3:num0) = fun (code:num3->unit) -> ch.n3 (v.etype,I size1,I size2,size3) code
        static member n3 (v:num2,size1:int,size2:int,size3:int) = fun (code:num3->unit) -> ch.n3 (v.etype,I size1,I size2,I size3) code
        static member n3 (v:num3) = fun (code:num3->unit) -> ch.n3 (v.etype,v.size1,v.size2,v.size3) code
        static member n3 (v:num3,size1:num0,size2:num0,size3:num0) = fun (code:num3->unit) -> ch.n3 (v.etype,size1,size2,size3) code
        static member n3 (v:num3,size1:num0,size2:num0,size3:int) = fun (code:num3->unit) -> ch.n3 (v.etype,size1,size2,I size3) code
        static member n3 (v:num3,size1:num0,size2:int,size3:num0) = fun (code:num3->unit) -> ch.n3 (v.etype,size1,I size2,size3) code
        static member n3 (v:num3,size1:num0,size2:int,size3:int) = fun (code:num3->unit) -> ch.n3 (v.etype,size1,I size2,I size3) code
        static member n3 (v:num3,size1:int,size2:num0,size3:num0) = fun (code:num3->unit) -> ch.n3 (v.etype,I size1,size2,size3) code
        static member n3 (v:num3,size1:int,size2:num0,size3:int) = fun (code:num3->unit) -> ch.n3 (v.etype,I size1,size2,I size3) code
        static member n3 (v:num3,size1:int,size2:int,size3:num0) = fun (code:num3->unit) -> ch.n3 (v.etype,I size1,I size2,size3) code
        static member n3 (v:num3,size1:int,size2:int,size3:int) = fun (code:num3->unit) -> ch.n3 (v.etype,I size1,I size2,I size3) code
        
        ///<summary>整数型1次元配列を生成</summary>
        static member i01 code = 
            let p = p.param
            let x = p.cachevar (It 4) 1
            let y = num1(It 4,A1(0),x)
            code y
            p.dispose(It 4,A1(0),x)
            
        ///<summary>実数型1次元配列を生成</summary>
        static member d01 code = 
            let p = p.param
            let x = p.cachevar Dt 1
            let y = num1(Dt,A1(0),x)
            code y
            p.dispose(Dt,A1(0),x)
            
        ///<summary>複素数型1次元配列を生成</summary>
        static member z01 code = 
            let p = p.param
            let x = p.cachevar Zt 1
            let y = num1(Zt,A1(0),x)
            code y
            p.dispose(Zt,A1(0),x)
            
        ///<summary>整数型1次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
        static member i1 (size1:num0) = fun code ->
            let p = p.param
            let x = p.cachevar (It 4) 1
            let y = num1(It 4,A1(0),x)
            y.allocate(size1)
            code y
            y.deallocate()
            p.dispose(It 4,A1(0),x)
            
        ///<summary>整数型1次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
        static member i1 (size1:int) = fun code ->
            ch.i1 (I size1) code
            
        ///<summary>実数型1次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
        static member d1 (size1:num0) = fun code ->
            let p = p.param
            let x = p.cachevar Dt 1
            let y = num1(Dt,A1(0),x)
            y.allocate(size1)
            code y
            y.deallocate()
            p.dispose(Dt,A1(0),x)
            
        ///<summary>実数型1次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
        static member d1 (size1:int) = fun code ->
            ch.d1 (I size1) code
            
        ///<summary>複素数型1次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
        static member z1 (size1:num0) = fun code ->
            let p = p.param
            let x = p.cachevar Zt 1
            let y = num1(Zt,A1(0),x)
            y.allocate(size1)
            code y
            y.deallocate()
            p.dispose(Zt,A1(0),x)
            
        ///<summary>複素数型1次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
        static member z1 (size1:int) = fun code ->
            ch.z1 (I size1) code
            
        ///<summary>整数型2次元配列を生成</summary>
        static member i02 code = 
            let p = p.param
            let x = p.cachevar (It 4) 2
            let y = num2(It 4,A2(0,0),x)
            code y
            p.dispose(It 4,A2(0,0),x)
            
        ///<summary>実数型2次元配列を生成</summary>
        static member d02 code = 
            let p = p.param
            let x = p.cachevar Dt 2
            let y = num2(Dt,A2(0,0),x)
            code y
            p.dispose(Dt,A2(0,0),x)
            
        ///<summary>複素数型2次元配列を生成</summary>
        static member z02 code = 
            let p = p.param
            let x = p.cachevar Zt 2
            let y = num2(Zt,A2(0,0),x)
            code y
            p.dispose(Zt,A2(0,0),x)
            
        ///<summary>整数型2次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
        static member i2 (size1:num0) = fun (size2:num0) code -> 
            let p = p.param
            let x = p.cachevar (It 4) 2
            let y = num2(It 4,A2(0,0),x)
            y.allocate(size1,size2)
            code y
            y.deallocate()
            p.dispose(It 4,A2(0,0),x)
            
        ///<summary>整数型2次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
        static member i2 (size1:int) = fun (size2:int) code -> 
            ch.i2 (I size1) (I size2) code
            
        ///<summary>実数型2次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
        static member d2 (size1:num0) = fun (size2:num0) code ->
            let p = p.param
            let x = p.cachevar Dt 2
            let y = num2(Dt,A2(0,0),x)
            y.allocate(size1,size2)
            code y
            y.deallocate()
            p.dispose(Dt,A2(0,0),x)
            
        ///<summary>実数型2次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
        static member d2 (size1:int) = fun (size2:int) code -> 
            ch.d2 (I size1) (I size2) code
            
        ///<summary>複素数型2次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
        static member z2 (size1:num0) = fun (size2:num0) code ->
            let p = p.param
            let x = p.cachevar Zt 2
            let y = num2(Zt,A2(0,0),x)
            y.allocate(size1,size2)
            code y
            y.deallocate()
            p.dispose(Zt,A2(0,0),x)
            
        ///<summary>複素数型2次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
        static member z2 (size1:int) = fun (size2:int) code -> 
            ch.z2 (I size1) (I size2) code
            
        ///<summary>整数型3次元配列を生成</summary>
        static member i03 code = 
            let p = p.param
            let x = p.cachevar (It 4) 3
            let y = num3(It 4,A3(0,0,0),x)
            code y
            p.dispose(It 4,A3(0,0,0),x)
            
        ///<summary>実数型3次元配列を生成</summary>
        static member d03 code = 
            let p = p.param
            let x = p.cachevar Dt 3
            let y = num3(Dt,A3(0,0,0),x)
            code y
            p.dispose(Dt,A3(0,0,0),x)
            
        ///<summary>複素数型3次元配列を生成</summary>
        static member z03 code = 
            let p = p.param
            let x = p.cachevar Zt 3
            let y = num3(Zt,A3(0,0,0),x)
            code y
            p.dispose(Zt,A3(0,0,0),x)
            
        ///<summary>整数型3次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
        static member i3 (size1:num0) = fun (size2:num0) (size3:num0) code ->
            let p = p.param
            let x = p.cachevar (It 4) 3
            let y = num3(It 4,A3(0,0,0),x)
            y.allocate(size1,size2,size3)
            code y
            y.deallocate()
            p.dispose(It 4,A3(0,0,0),x)
            
        ///<summary>整数型3次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
        static member i3 (size1:int) = fun (size2:int) (size3:int) code ->
            ch.i3 (I size1) (I size2) (I size3) code
            
        ///<summary>実数型3次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
        static member d3 (size1:num0) = fun (size2:num0) (size3:num0) code ->
            let p = p.param
            let x = p.cachevar Dt 3
            let y = num3(Dt,A3(0,0,0),x)
            y.allocate(size1,size2,size3)
            code y
            y.deallocate()
            p.dispose(Dt,A3(0,0,0),x)
            
        ///<summary>実数型3次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
        static member d3 (size1:int) = fun (size2:int) (size3:int) code ->
            ch.d3 (I size1) (I size2) (I size3) code
            
        ///<summary>複素数型3次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
        static member z3 (size1:num0) = fun (size2:num0) (size3:num0) code ->
            let p = p.param
            let x = p.cachevar Zt 3
            let y = num3(Zt,A3(0,0,0),x)
            y.allocate(size1,size2,size3)
            code y
            y.deallocate()
            p.dispose(Zt,A3(0,0,0),x)
            
        ///<summary>複素数型3次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
        static member z3 (size1:int) = fun (size2:int) (size3:int) code ->
            ch.z3 (I size1) (I size2) (I size3) code
            
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
                counter <== I 1
                code(counter)
                
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
                