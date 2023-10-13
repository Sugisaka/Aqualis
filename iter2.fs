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

    [<AutoOpen>]
    module iter_array =
        ///<summary>反復処理</summary>
        type iter with
            ///<summary>指定した1次元配列の範囲でループ</summary>
            static member array (ar:base1) = fun code -> 
                iter.range _1 ar.size1 code
                
            ///<summary>指定した2次元配列の範囲でループ</summary>
            static member array (ar:base2) = fun code -> 
                iter.range (_1,_1) (ar.size1,ar.size2) code
                
            ///<summary>指定した3次元配列の範囲でループ</summary>
            static member array (ar:base3) = fun code -> 
                iter.range (_1,_1,_1) (ar.size1,ar.size2,ar.size3) code
                
            ///<summary>連続する2整数を取得しながらループ</summary>
            static member array_overlap2 (ar:base1) = fun code -> 
                iter.range _1 (ar.size1 - _1) <| fun i -> 
                    code(i,i+_1)
                    
            ///<summary>連続する3整数を取得しながらループ</summary>
            static member array_overlap3 (ar:base1) = fun code -> 
                iter.range _2 (ar.size1-_1) <| fun i -> 
                    code(i-_1,i,i+_1)
                    
        ///<summary>反復処理（処理スキップ）</summary>
        type dummy_iter with
            ///<summary>指定した1次元配列の範囲でループ</summary>
            static member array (ar:base1) = fun code -> ()
                
            ///<summary>指定した2次元配列の範囲でループ</summary>
            static member array (ar:base2) = fun code -> ()
                
            ///<summary>指定した3次元配列の範囲でループ</summary>
            static member array (ar:base3) = fun code -> ()
                
            ///<summary>連続する2整数を取得しながらループ</summary>
            static member array_overlap2 (ar:base1) = fun code -> ()
                    
            ///<summary>連続する3整数を取得しながらループ</summary>
            static member array_overlap3 (ar:base1) = fun code -> ()
            