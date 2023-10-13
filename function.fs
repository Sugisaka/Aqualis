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
    
    ///<summary>関数定義の引数</summary>
    type fn() =
        /// <summary>
        /// 関数定義の引数を追加
        /// </summary>
        /// <param name="typ">変数の型</param>
        /// <param name="vtp">変数の次元</param>
        /// <param name="n">変数名</param>
        static member addarg (typ:Etype,vtp:VarType,n:string) = p.param.addarg(typ,vtp,n)
        
        /// <summary>
        /// 関数定義の引数を追加
        /// </summary>
        /// <param name="sname">構造体名</param>
        /// <param name="vtp">変数の次元</param>
        /// <param name="n">変数名</param>
        static member addarg (sname:string,vtp:VarType,n:string) = p.param.addarg(Structure(sname),vtp,n)
        
    [<AutoOpen>]
    module num_farg =
        type int0 with
            /// <summary>
            /// この変数を関数内変数に変換
            /// </summary>
            member this.farg code =
                fn.addarg (It 4,A0,this.code) <| fun (v,n) -> 
                    code(complex0(Var n))
                    
        type float0 with
            /// <summary>
            /// この変数を関数内変数に変換
            /// </summary>
            member this.farg code =
                fn.addarg (Dt,A0,this.code) <| fun (v,n) -> 
                    code(float0(Var n))
                    
        type complex0 with
            /// <summary>
            /// この変数を関数内変数に変換
            /// </summary>
            member this.farg code =
                fn.addarg (Zt,A0,this.code) <| fun (v,n) -> 
                    code(complex0(Var n))
                    
        type num0 with
            /// <summary>
            /// この変数を関数内変数に変換
            /// </summary>
            member this.farg code =
                fn.addarg (this.etype,A0,this.code) <| fun (v,n) -> 
                    code(num0(this.etype,Var n))
                    
        type int1 with
            /// <summary>
            /// この変数を関数内変数に変換
            /// </summary>
            member this.farg = fun code ->
                match this.expr with
                |Var1(size,name) ->
                    fn.addarg (It 4,size,name) <| fun (v,n) -> code(int1(Var1(v,n)))
                |_ -> 
                    printfn "部分配列を関数の引数にできません"
                    
        type float1 with
            /// <summary>
            /// この変数を関数内変数に変換
            /// </summary>
            member this.farg = fun code ->
                match this.expr with
                |Var1(size,name) ->
                    fn.addarg (Dt,size,name) <| fun (v,n) -> code(float1(Var1(v,n)))
                |_ -> 
                    printfn "部分配列を関数の引数にできません"
                    
        type complex1 with
            /// <summary>
            /// この変数を関数内変数に変換
            /// </summary>
            member this.farg = fun code ->
                match this.expr with
                |Var1(size,name) ->
                    fn.addarg (Zt,size,name) <| fun (v,n) -> code(complex1(Var1(v,n)))
                |_ -> 
                    printfn "部分配列を関数の引数にできません"
                    
        type num1 with
            /// <summary>
            /// この変数を関数内変数に変換
            /// </summary>
            member this.farg = fun code ->
                match this.expr with
                |Var1(size,name) ->
                    fn.addarg (this.etype,size,name) <| fun (v,n) -> code(num1(this.etype,Var1(v,n)))
                |_ -> 
                    printfn "部分配列を関数の引数にできません"
                    
        type int2 with
            /// <summary>
            /// この変数を関数内変数に変換
            /// </summary>
            member this.farg = fun code ->
                match this.expr with
                |Var2(size,name) ->
                    fn.addarg (It 4,size,name) <| fun (v,n) -> code(int2(Var2(v,n)))
                |_ -> 
                    printfn "部分配列を関数の引数にできません"
                    
        type float2 with
            /// <summary>
            /// この変数を関数内変数に変換
            /// </summary>
            member this.farg = fun code ->
                match this.expr with
                |Var2(size,name) ->
                    fn.addarg (Dt,size,name) <| fun (v,n) -> code(float2(Var2(v,n)))
                |_ -> 
                    printfn "部分配列を関数の引数にできません"
                    
        type complex2 with
            /// <summary>
            /// この変数を関数内変数に変換
            /// </summary>
            member this.farg = fun code ->
                match this.expr with
                |Var2(size,name) ->
                    fn.addarg (Zt,size,name) <| fun (v,n) -> code(complex2(Var2(v,n)))
                |_ -> 
                    printfn "部分配列を関数の引数にできません"
                    
        type num2 with
            /// <summary>
            /// この変数を関数内変数に変換
            /// </summary>
            member this.farg = fun code ->
                match this.expr with
                |Var2(size,name) ->
                    fn.addarg (this.etype,size,name) <| fun (v,n) -> code(num2(this.etype,Var2(v,n)))
                |_ -> 
                    printfn "部分配列を関数の引数にできません"
                    
        type int3 with
            /// <summary>
            /// この変数を関数内変数に変換
            /// </summary>
            member this.farg = fun code ->
                match this.expr with
                |Var3(size,name) ->
                    fn.addarg (It 4,size,name) <| fun (v,n) -> code(int3(Var3(v,n)))
                |_ -> 
                    printfn "部分配列を関数の引数にできません"
                    
        type float3 with
            /// <summary>
            /// この変数を関数内変数に変換
            /// </summary>
            member this.farg = fun code ->
                match this.expr with
                |Var3(size,name) ->
                    fn.addarg (Dt,size,name) <| fun (v,n) -> code(float3(Var3(v,n)))
                |_ -> 
                    printfn "部分配列を関数の引数にできません"
                    
        type complex3 with
            /// <summary>
            /// この変数を関数内変数に変換
            /// </summary>
            member this.farg = fun code ->
                match this.expr with
                |Var3(size,name) ->
                    fn.addarg (Zt,size,name) <| fun (v,n) -> code(complex3(Var3(v,n)))
                |_ -> 
                    printfn "部分配列を関数の引数にできません"
                    
        type num3 with
            /// <summary>
            /// この変数を関数内変数に変換
            /// </summary>
            member this.farg = fun code ->
                match this.expr with
                |Var3(size,name) ->
                    fn.addarg (this.etype,size,name) <| fun (v,n) -> code(num3(this.etype,Var3(v,n)))
                |_ -> 
                    printfn "部分配列を関数の引数にできません"
                    