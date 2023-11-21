(*
Copyright (c) 2022 Jun-ichiro Sugisaka

This software is released under the MIT License.
http://opensource.org/licenses/mit-license.php
*)
namespace Aqualis
    
    open Aqualis_base
    
    ///<summary>関数定義の引数</summary>
    type fn() =
        /// <summary>
        /// 関数定義の引数を追加
        /// </summary>
        /// <param name="typ">変数の型</param>
        /// <param name="vtp">変数の次元</param>
        /// <param name="n">変数名</param>
        static member addarg (typ:Etype,vtp:VarType,n:string) = p.addarg(typ,vtp,n)
        
        /// <summary>
        /// 関数定義の引数を追加
        /// </summary>
        /// <param name="sname">構造体名</param>
        /// <param name="vtp">変数の次元</param>
        /// <param name="n">変数名</param>
        static member addarg (sname:string,vtp:VarType,n:string) = p.addarg(Structure(sname),vtp,n)
        
    [<AutoOpen>]
    module num_farg =                    
        type num0 with
            /// <summary>
            /// この変数を関数内変数に変換
            /// </summary>
            member this.farg code =
                fn.addarg (this.etype,A0,this.code) <| fun (v,n) -> 
                    code(num0(Var(this.etype, n)))
                    
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
                    