(*
Copyright (c) 2022 Jun-ichiro Sugisaka

This software is released under the MIT License.
http://opensource.org/licenses/mit-license.php
*)
namespace Aqualis
    
    open Aqualis_base

    ///<summary>一時変数の生成と使用</summary>
    type tbinder () =
        static member i (x:num0) = fun code ->
            match x.etype with
            |It _ -> code()
            |_ -> ()
        static member i (x:num1) = fun code ->
            match x.etype with
            |It _ -> code()
            |_ -> ()
        static member i (x:num2) = fun code ->
            match x.etype with
            |It _ -> code()
            |_ -> ()
        static member i (x:num3) = fun code ->
            match x.etype with
            |It _ -> code()
            |_ -> ()
            
        static member d (x:num0) = fun code ->
            match x.etype with
            |Dt -> code()
            |_ -> ()
        static member d (x:num1) = fun code ->
            match x.etype with
            |Dt -> code()
            |_ -> ()
        static member d (x:num2) = fun code ->
            match x.etype with
            |Dt -> code()
            |_ -> ()
        static member d (x:num3) = fun code ->
            match x.etype with
            |Dt -> code()
            |_ -> ()
            
        static member z (x:num0) = fun code ->
            match x.etype with
            |Zt -> code()
            |_ -> ()
        static member z (x:num1) = fun code ->
            match x.etype with
            |Zt -> code()
            |_ -> ()
        static member z (x:num2) = fun code ->
            match x.etype with
            |Zt -> code()
            |_ -> ()
        static member z (x:num3) = fun code ->
            match x.etype with
            |Zt -> code()
            |_ -> ()