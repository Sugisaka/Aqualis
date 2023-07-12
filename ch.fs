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
        static member n (v:ax1) = fun (code:num0->unit) -> ch.n v.etype code
        static member n (v:ax2) = fun (code:num0->unit) -> ch.n v.etype code
        static member n (v:ax3) = fun (code:num0->unit) -> ch.n v.etype code
        
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
        static member n01 (v:Etype) = 
            fun (code:num1->unit) ->
                match v with
                  |Zt ->
                    ch.z01 code
                  |Dt ->
                    ch.d01 code
                  |It _ ->
                    ch.i01 code
                  |_ -> 
                    printfn "ch.n01 error"
                    
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
        static member n1 (v:ax1) = fun (code:num1->unit) -> ch.n1 (v.etype, v.size1) code
        static member n1 (v:ax1,size1:num0) = fun (code:num1->unit) -> ch.n1 (v.etype, size1) code
        static member n1 (v:ax1,size1:int) = fun (code:num1->unit) -> ch.n1 (v.etype, I size1) code
        static member n1 (v:ax2,size1:num0) = fun (code:num1->unit) -> ch.n1 (v.etype, size1) code
        static member n1 (v:ax2,size1:int) = fun (code:num1->unit) -> ch.n1 (v.etype, I size1) code
        static member n1 (v:ax3,size1:num0) = fun (code:num1->unit) -> ch.n1 (v.etype,size1) code
        static member n1 (v:ax3,size1:int) = fun (code:num1->unit) -> ch.n1 (v.etype,I size1) code

        ///<summary>vと同じ型の一時変数を生成</summary>
        static member n02 (v:Etype) = 
            fun (code:num2->unit) ->
                match v with
                  |Zt ->
                    ch.z02 code
                  |Dt ->
                    ch.d02 code
                  |It _ ->
                    ch.i02 code
                  |_ -> 
                    printfn "ch.n02 error"
                    
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
        static member n2 (v:ax1,size1:num0,size2:num0) = fun (code:num2->unit) -> ch.n2 (v.etype, size1, size2) code
        static member n2 (v:ax1,size1:num0,size2:int) = fun (code:num2->unit) -> ch.n2 (v.etype, size1, I size2) code
        static member n2 (v:ax1,size1:int,size2:num0) = fun (code:num2->unit) -> ch.n2 (v.etype, I size1, size2) code
        static member n2 (v:ax1,size1:int,size2:int) = fun (code:num2->unit) -> ch.n2 (v.etype, I size1, I size2) code
        static member n2 (v:ax2) = fun (code:num2->unit) -> ch.n2 (v.etype, v.size1, v.size2) code
        static member n2 (v:ax2,size1:num0,size2:num0) = fun (code:num2->unit) -> ch.n2 (v.etype, size1, size2) code
        static member n2 (v:ax2,size1:num0,size2:int) = fun (code:num2->unit) -> ch.n2 (v.etype, size1, I size2) code
        static member n2 (v:ax2,size1:int,size2:num0) = fun (code:num2->unit) -> ch.n2 (v.etype, I size1, size2) code
        static member n2 (v:ax2,size1:int,size2:int) = fun (code:num2->unit) -> ch.n2 (v.etype, I size1, I size2) code

        ///<summary>vと同じ型の一時変数を生成</summary>
        static member n03 (v:Etype) = 
            fun (code:num3->unit) ->
                match v with
                  |Zt ->
                    ch.z03 code
                  |Dt ->
                    ch.d03 code
                  |It _ ->
                    ch.i03 code
                  |_ -> 
                    printfn "ch.n03 error"
                    
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
                
        ///<summary>整数型1次元配列を生成し、指定したサイズでメモリ割り当て、GPUに転送→code実行後にメモリ解放</summary>
        static member copyin_i1 (size1:num0) = fun code ->
            match p.lang with
              |F ->
                ch.i1 size1 <| fun i ->
                    p.param.civreg (i.name+"(1:"+size1.name+")")
                    code i
                    p.param.rmciv (i.name+"(1:"+size1.name+")")
              |C99 ->
                ch.i1 size1 <| fun i ->
                    p.param.civreg (i.name+"[0:"+size1.name+"]")
                    code i
                    p.param.rmciv (i.name+"[0:"+size1.name+"]")
              |_ ->
                ()

        ///<summary>整数型1次元配列を生成し、指定したサイズでメモリ割り当て、GPUに転送→code実行後にメモリ解放</summary>
        static member copyin_i1 (size1:int) = fun code ->
            ch.copyin_i1 (I size1) code

        ///<summary>実数型1次元配列を生成し、指定したサイズでメモリ割り当て、GPUに転送→code実行後にメモリ解放</summary>
        static member copyin_d1 (size1:num0) = fun code ->
            match p.lang with
              |F ->
                ch.d1 size1 <| fun d ->
                    p.param.civreg (d.name+"(1:"+size1.name+")")
                    code d
                    p.param.rmciv (d.name+"(1:"+size1.name+")")
              |C99 ->
                ch.d1 size1 <| fun d ->
                    p.param.civreg (d.name+"[0:"+size1.name+"]")
                    code d
                    p.param.rmciv (d.name+"[0:"+size1.name+"]")
              |_ ->
                ()

        ///<summary>実数型1次元配列を生成し、指定したサイズでメモリ割り当て、GPUに転送→code実行後にメモリ解放</summary>
        static member copyin_d1 (size1:int) = fun code ->
            ch.copyin_d1 (I size1) code

        ///<summary>複素数型1次元配列を生成し、指定したサイズでメモリ割り当て、GPUに転送→code実行後にメモリ解放</summary>
        static member copyin_z1 (size1:num0) = fun code ->
            match p.lang with
              |F ->
                ch.z1 size1 <| fun z ->
                    p.param.civreg (z.name+"(1:"+size1.name+")")
                    code z
                    p.param.rmciv (z.name+"(1:"+size1.name+")")
              |C99 ->
                ch.z1 size1 <| fun z ->
                    p.param.civreg (z.name+"[0:"+size1.name+"]")
                    code z
                    p.param.rmciv (z.name+"[0:"+size1.name+"]")
              |_ ->
                ()

        ///<summary>複素数型1次元配列を生成し、指定したサイズでメモリ割り当て、GPUに転送→code実行後にメモリ解放</summary>
        static member copyin_z1 (size1:int) = fun code ->
            ch.copyin_z1 (I size1) code

        ///<summary>整数型2次元配列を生成し、指定したサイズでメモリ割り当て、GPUに転送→code実行後にメモリ解放</summary>
        static member copyin_i2 (size1:num0) = fun (size2:num0) code ->
            match p.lang with
              |F ->
                ch.i2 size1 size2 <| fun i ->
                    p.param.civreg (i.name+"(1:"+size1.name+",1:"+size2.name+")")
                    code i
                    p.param.rmciv (i.name+"(1:"+size1.name+",1:"+size2.name+")")
              |C99 ->
                ch.i2 size1 size2 <| fun i ->
                    p.param.civreg (i.name+"[0:"+size1.name+"][0:"+size2.name+"]")
                    code i
                    p.param.rmciv (i.name+"[0:"+size1.name+"][0:"+size2.name+"]")
              |_ ->
                ()

        ///<summary>整数型2次元配列を生成し、指定したサイズでメモリ割り当て、GPUに転送→code実行後にメモリ解放</summary>
        static member copyin_i2 (size1:int) = fun (size2:int) code ->
            ch.copyin_i2 (I size1) (I size2) code

        ///<summary>実数型2次元配列を生成し、指定したサイズでメモリ割り当て、GPUに転送→code実行後にメモリ解放</summary>
        static member copyin_d2 (size1:num0) = fun (size2:num0) code ->
            match p.lang with
              |F ->
                ch.d2 size1 size2 <| fun d ->
                    p.param.civreg (d.name+"(1:"+size1.name+",1:"+size2.name+")")
                    code d
                    p.param.rmciv (d.name+"(1:"+size1.name+",1:"+size2.name+")")
              |C99 ->
                ch.d2 size1 size2 <| fun d ->
                    p.param.civreg (d.name+"[0:"+size1.name+"][0:"+size2.name+"]")
                    code d
                    p.param.rmciv (d.name+"[0:"+size1.name+"][0:"+size2.name+"]")
              |_ ->
                ()

        ///<summary>実数型2次元配列を生成し、指定したサイズでメモリ割り当て、GPUに転送→code実行後にメモリ解放</summary>
        static member copyin_d2 (size1:int) = fun (size2:int) code ->
            ch.copyin_d2 (I size1) (I size2) code

        ///<summary>複素数型1次元配列を生成し、指定したサイズでメモリ割り当て、GPUに転送→code実行後にメモリ解放</summary>
        static member copyin_z2 (size1:num0) = fun (size2:num0) code ->
            match p.lang with
              |F ->
                ch.z2 size1 size2 <| fun z ->
                    p.param.civreg (z.name+"(1:"+size1.name+",1:"+size2.name+")")
                    code z
                    p.param.rmciv (z.name+"(1:"+size1.name+",1:"+size2.name+")")
              |C99 ->
                ch.z2 size1 size2 <| fun z ->
                    p.param.civreg (z.name+"[0:"+size1.name+"][0:"+size2.name+"]")
                    code z
                    p.param.rmciv (z.name+"[0:"+size1.name+"][0:"+size2.name+"]")
              |_ ->
                ()

        ///<summary>複素数型1次元配列を生成し、指定したサイズでメモリ割り当て、GPUに転送→code実行後にメモリ解放</summary>
        static member copyin_z2 (size1:int) = fun (size2:int) code ->
            ch.copyin_z2 (I size1) (I size2) code

        ///<summary>整数型3次元配列を生成し、指定したサイズでメモリ割り当て、GPUに転送→code実行後にメモリ解放</summary>
        static member copyin_i3 (size1:num0) = fun (size2:num0) (size3:num0) code ->
            match p.lang with
              |F ->
                ch.i3 size1 size2 size3 <| fun i ->
                    p.param.civreg (i.name+"(1:"+size1.name+",1:"+size2.name+",1:"+size3.name+")")
                    code i
                    p.param.rmciv (i.name+"(1:"+size1.name+",1:"+size2.name+",1:"+size3.name+")")
              |C99 ->
                ch.i3 size1 size2 size3 <| fun i ->
                    p.param.civreg (i.name+"[0:"+size1.name+"][0:"+size2.name+"][0:"+size3.name+"]")
                    code i
                    p.param.rmciv (i.name+"[0:"+size1.name+"][0:"+size2.name+"][0:"+size3.name+"]")
              |_ ->
                ()

        ///<summary>整数型3次元配列を生成し、指定したサイズでメモリ割り当て、GPUに転送→code実行後にメモリ解放</summary>
        static member copyin_i3 (size1:int) = fun (size2:int) (size3:int) code ->
            ch.copyin_i3 (I size1) (I size2) (I size3) code

        ///<summary>実数型3次元配列を生成し、指定したサイズでメモリ割り当て、GPUに転送→code実行後にメモリ解放</summary>
        static member copyin_d3 (size1:num0) = fun (size2:num0) (size3:num0) code ->
            match p.lang with
              |F ->
                ch.d3 size1 size2 size3 <| fun d ->
                    p.param.civreg (d.name+"(1:"+size1.name+",1:"+size2.name+",1:"+size3.name+")")
                    code d
                    p.param.rmciv (d.name+"(1:"+size1.name+",1:"+size2.name+",1:"+size3.name+")")
              |C99 ->
                ch.d3 size1 size2 size3 <| fun d ->
                    p.param.civreg (d.name+"[0:"+size1.name+"][0:"+size2.name+"][0:"+size3.name+"]")
                    code d
                    p.param.rmciv (d.name+"[0:"+size1.name+"][0:"+size2.name+"][0:"+size3.name+"]")
              |_ ->
                ()

        ///<summary>実数型3次元配列を生成し、指定したサイズでメモリ割り当て、GPUに転送→code実行後にメモリ解放</summary>
        static member copyin_d3 (size1:int) = fun (size2:int) (size3:int) code ->
            ch.copyin_d3 (I size1) (I size2) (I size3) code

        ///<summary>複素数型3次元配列を生成し、指定したサイズでメモリ割り当て、GPUに転送→code実行後にメモリ解放</summary>
        static member copyin_z3 (size1:num0) = fun (size2:num0) (size3:num0) code ->
            match p.lang with
              |F ->
                ch.z3 size1 size2 size3 <| fun z ->
                    p.param.civreg (z.name+"(1:"+size1.name+",1:"+size2.name+",1:"+size3.name+")")
                    code z
                    p.param.rmciv (z.name+"(1:"+size1.name+",1:"+size2.name+",1:"+size3.name+")")
              |C99 ->
                ch.z3 size1 size2 size3 <| fun z ->
                    p.param.civreg (z.name+"[0:"+size1.name+"][0:"+size2.name+"][0:"+size3.name+"]")
                    code z
                    p.param.rmciv (z.name+"[0:"+size1.name+"][0:"+size2.name+"][0:"+size3.name+"]")
              |_ ->
                ()

        ///<summary>実数型3次元配列を生成し、指定したサイズでメモリ割り当て、GPUに転送→code実行後にメモリ解放</summary>
        static member copyin_z3 (size1:int) = fun (size2:int) (size3:int) code ->
            ch.copyin_z3 (I size1) (I size2) (I size3) code

        ///<summary>整数型1次元配列を生成し、指定したサイズでメモリ割り当て、ホストに転送→code実行後にメモリ解放</summary>
        static member copyout_i1 (size1:num0) = fun code ->
            match p.lang with
              |F ->
                ch.i1 size1 <| fun i ->
                    p.param.covreg (i.name+"(1:"+size1.name+")")
                    code i
                    p.param.rmcov (i.name+"(1:"+size1.name+")")
              |C99 ->
                ch.i1 size1 <| fun i ->
                    p.param.covreg (i.name+"[0:"+size1.name+"]")
                    code i
                    p.param.rmcov (i.name+"[0:"+size1.name+"]")
              |_ ->
                ()

        ///<summary>整数型1次元配列を生成し、指定したサイズでメモリ割り当て、ホストに転送→code実行後にメモリ解放</summary>
        static member copyout_i1 (size1:int) = fun code ->
            ch.copyout_i1 (I size1) code

        ///<summary>実数型1次元配列を生成し、指定したサイズでメモリ割り当て、ホストに転送→code実行後にメモリ解放</summary>
        static member copyout_d1 (size1:num0) = fun code ->
            match p.lang with
              |F ->
                ch.d1 size1 <| fun d ->
                    p.param.covreg (d.name+"(1:"+size1.name+")")
                    code d
                    p.param.rmcov (d.name+"(1:"+size1.name+")")
              |C99 ->
                ch.d1 size1 <| fun d ->
                    p.param.covreg (d.name+"[0:"+size1.name+"]")
                    code d
                    p.param.rmcov (d.name+"[0:"+size1.name+"]")
              |_ ->
                ()

        ///<summary>実数型1次元配列を生成し、指定したサイズでメモリ割り当て、ホストに転送→code実行後にメモリ解放</summary>
        static member copyout_d1 (size1:int) = fun code ->
            ch.copyout_d1 (I size1) code

        ///<summary>複素数型1次元配列を生成し、指定したサイズでメモリ割り当て、ホストに転送→code実行後にメモリ解放</summary>
        static member copyout_z1 (size1:num0) = fun code ->
            match p.lang with
              |F ->
                ch.z1 size1 <| fun z ->
                    p.param.covreg (z.name+"(1:"+size1.name+")")
                    code z
                    p.param.rmcov (z.name+"(1:"+size1.name+")")
              |C99 ->
                ch.z1 size1 <| fun z ->
                    p.param.covreg (z.name+"[0:"+size1.name+"]")
                    code z
                    p.param.rmcov (z.name+"[0:"+size1.name+"]")
              |_ ->
                ()

        ///<summary>複素数型1次元配列を生成し、指定したサイズでメモリ割り当て、ホストに転送→code実行後にメモリ解放</summary>
        static member copyout_z1 (size1:int) = fun code ->
            ch.copyout_z1 (I size1) code

        ///<summary>整数型2次元配列を生成し、指定したサイズでメモリ割り当て、ホストに転送→code実行後にメモリ解放</summary>
        static member copyout_i2 (size1:num0) = fun (size2:num0) code ->
            match p.lang with
              |F ->
                ch.i2 size1 size2 <| fun i ->
                    p.param.covreg (i.name+"(1:"+size1.name+",1:"+size2.name+")")
                    code i
                    p.param.rmcov (i.name+"(1:"+size1.name+",1:"+size2.name+")")
              |C99 ->
                ch.i2 size1 size2 <| fun i ->
                    p.param.covreg (i.name+"[0:"+size1.name+"][0:"+size2.name+"]")
                    code i
                    p.param.rmcov (i.name+"[0:"+size1.name+"][0:"+size2.name+"]")
              |_ ->
                ()

        ///<summary>整数型2次元配列を生成し、指定したサイズでメモリ割り当て、ホストに転送→code実行後にメモリ解放</summary>
        static member copyout_i2 (size1:int) = fun (size2:int) code ->
            ch.copyout_i2 (I size1) (I size2) code

        ///<summary>実数型2次元配列を生成し、指定したサイズでメモリ割り当て、ホストに転送→code実行後にメモリ解放</summary>
        static member copyout_d2 (size1:num0) = fun (size2:num0) code ->
            match p.lang with
              |F ->
                ch.d2 size1 size2 <| fun d ->
                    p.param.covreg (d.name+"(1:"+size1.name+",1:"+size2.name+")")
                    code d
                    p.param.rmcov (d.name+"(1:"+size1.name+",1:"+size2.name+")")
              |C99 ->
                ch.d2 size1 size2 <| fun d ->
                    p.param.covreg (d.name+"[0:"+size1.name+"][0:"+size2.name+"]")
                    code d
                    p.param.rmcov (d.name+"[0:"+size1.name+"][0:"+size2.name+"]")
              |_ ->
                ()

        ///<summary>実数型2次元配列を生成し、指定したサイズでメモリ割り当て、ホストに転送→code実行後にメモリ解放</summary>
        static member copyout_d2 (size1:int) = fun (size2:int) code ->
            ch.copyout_d2 (I size1) (I size2) code

        ///<summary>複素数型1次元配列を生成し、指定したサイズでメモリ割り当て、ホストに転送→code実行後にメモリ解放</summary>
        static member copyout_z2 (size1:num0) = fun (size2:num0) code ->
            match p.lang with
              |F ->
                ch.z2 size1 size2 <| fun z ->
                    p.param.covreg (z.name+"(1:"+size1.name+",1:"+size2.name+")")
                    code z
                    p.param.rmcov (z.name+"(1:"+size1.name+",1:"+size2.name+")")
              |C99 ->
                ch.z2 size1 size2 <| fun z ->
                    p.param.covreg (z.name+"[0:"+size1.name+"][0:"+size2.name+"]")
                    code z
                    p.param.rmcov (z.name+"[0:"+size1.name+"][0:"+size2.name+"]")
              |_ ->
                ()

        ///<summary>複素数型1次元配列を生成し、指定したサイズでメモリ割り当て、ホストに転送→code実行後にメモリ解放</summary>
        static member copyout_z2 (size1:int) = fun (size2:int) code ->
            ch.copyout_z2 (I size1) (I size2) code

        ///<summary>整数型3次元配列を生成し、指定したサイズでメモリ割り当て、ホストに転送→code実行後にメモリ解放</summary>
        static member copyout_i3 (size1:num0) = fun (size2:num0) (size3:num0) code ->
            match p.lang with
              |F ->
                ch.i3 size1 size2 size3 <| fun i ->
                    p.param.covreg (i.name+"(1:"+size1.name+",1:"+size2.name+",1:"+size3.name+")")
                    code i
                    p.param.rmcov (i.name+"(1:"+size1.name+",1:"+size2.name+",1:"+size3.name+")")
              |C99 ->
                ch.i3 size1 size2 size3 <| fun i ->
                    p.param.covreg (i.name+"[0:"+size1.name+"][0:"+size2.name+"][0:"+size3.name+"]")
                    code i
                    p.param.rmcov (i.name+"[0:"+size1.name+"][0:"+size2.name+"][0:"+size3.name+"]")
              |_ ->
                ()

        ///<summary>整数型3次元配列を生成し、指定したサイズでメモリ割り当て、ホストに転送→code実行後にメモリ解放</summary>
        static member copyout_i3 (size1:int) = fun (size2:int) (size3:int) code ->
            ch.copyout_i3 (I size1) (I size2) (I size3) code

        ///<summary>実数型3次元配列を生成し、指定したサイズでメモリ割り当て、ホストに転送→code実行後にメモリ解放</summary>
        static member copyout_d3 (size1:num0) = fun (size2:num0) (size3:num0) code ->
            match p.lang with
              |F ->
                ch.d3 size1 size2 size3 <| fun d ->
                    p.param.covreg (d.name+"(1:"+size1.name+",1:"+size2.name+",1:"+size3.name+")")
                    code d
                    p.param.rmcov (d.name+"(1:"+size1.name+",1:"+size2.name+",1:"+size3.name+")")
              |C99 ->
                ch.d3 size1 size2 size3 <| fun d ->
                    p.param.covreg (d.name+"[0:"+size1.name+"][0:"+size2.name+"][0:"+size3.name+"]")
                    code d
                    p.param.rmcov (d.name+"[0:"+size1.name+"][0:"+size2.name+"][0:"+size3.name+"]")
              |_ ->
                ()

        ///<summary>実数型3次元配列を生成し、指定したサイズでメモリ割り当て、ホストに転送→code実行後にメモリ解放</summary>
        static member copyout_d3 (size1:int) = fun (size2:int) (size3:int) code ->
            ch.copyout_d3 (I size1) (I size2) (I size3) code

        ///<summary>複素数型3次元配列を生成し、指定したサイズでメモリ割り当て、ホストに転送→code実行後にメモリ解放</summary>
        static member copyout_z3 (size1:num0) = fun (size2:num0) (size3:num0) code ->
            match p.lang with
              |F ->
                ch.z3 size1 size2 size3 <| fun z ->
                    p.param.covreg (z.name+"(1:"+size1.name+",1:"+size2.name+",1:"+size3.name+")")
                    code z
                    p.param.rmcov (z.name+"(1:"+size1.name+",1:"+size2.name+",1:"+size3.name+")")
              |C99 ->
                ch.z3 size1 size2 size3 <| fun z ->
                    p.param.covreg (z.name+"[0:"+size1.name+"][0:"+size2.name+"][0:"+size3.name+"]")
                    code z
                    p.param.rmcov (z.name+"[0:"+size1.name+"][0:"+size2.name+"][0:"+size3.name+"]")
              |_ ->
                ()

        ///<summary>実数型3次元配列を生成し、指定したサイズでメモリ割り当て、ホストに転送→code実行後にメモリ解放</summary>
        static member copyout_z3 (size1:int) = fun (size2:int) (size3:int) code ->
            ch.copyout_z3 (I size1) (I size2) (I size3) code

        ///<summary>整数型一時変数(GPUに転送する変数)の生成</summary>
        static member copyin_i code =
            ch.i <| fun i1 ->
                p.param.civreg i1.name
                code(i1)
                p.param.rmciv i1.name

        ///<summary>倍精度浮動小数点型一時変数(GPUに転送する変数)の生成</summary>
        static member copyin_d code =
            ch.d <| fun d1 ->
                p.param.civreg d1.name
                code(d1)
                p.param.rmciv d1.name

        ///<summary>複素数型一時変数(GPUに転送する変数)の生成</summary>
        static member copyin_z code =
            ch.z <| fun z1 ->
                p.param.civreg z1.name
                code(z1)
                p.param.rmciv z1.name

        ///<summary>整数型一時変数(ホストに転送する変数)の生成</summary>
        static member copyout_i code =
            ch.i <| fun i1 ->
                p.param.covreg i1.name
                code(i1)
                p.param.rmcov i1.name

        ///<summary>倍精度浮動小数点型一時変数(ホストに転送する変数)の生成</summary>
        static member copyout_d code =
            ch.d <| fun d1 ->
                p.param.covreg d1.name
                code(d1)
                p.param.rmcov d1.name

        ///<summary>複素数型一時変数(ホストに転送する変数)の生成</summary>
        static member copyout_z code =
            ch.z <| fun z1 ->
                p.param.covreg z1.name
                code(z1)
                p.param.covreg z1.name
            
        ///<summary>整数型一時変数(プライベート変数)の生成</summary>
        static member private_i code =
            ch.i <| fun i1 ->
                p.param.pvreg i1.name
                code(i1)

        ///<summary>倍精度浮動小数点型一時変数(プライベート変数)の生成</summary>
        static member private_d code =
            ch.d <| fun d1 ->
                p.param.pvreg d1.name
                code(d1)

        ///<summary>複素数型一時変数(プライベート変数)の生成</summary>
        static member private_z code =
            ch.z <| fun z1 ->
                p.param.pvreg z1.name
                code(z1)

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
                