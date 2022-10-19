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
        /// <param name="comment">変数の説明</param>
        static member addarg (typ:Etype,vtp:VarType,n:string,comment:string) = p.param.addarg(typ,vtp,n,comment)

        /// <summary>
        /// 関数定義の引数を追加
        /// </summary>
        /// <param name="sname">構造体名</param>
        /// <param name="vtp">変数の次元</param>
        /// <param name="n">変数名</param>
        /// <param name="comment">変数の説明</param>
        static member addarg (sname:string,vtp:VarType,n:string,comment:string) = p.param.addarg(sname,vtp,n,comment)
        
