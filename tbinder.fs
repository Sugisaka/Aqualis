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

    ///<summary>数値型の型に従って処理を分岐</summary>
    type tbinder () =
        /// <summary>
        /// 指定された式または変数eqが整数型の場合のみcodeを実行
        /// </summary>
        /// <param name="eq"></param>
        static member i(eq:num0) = fun code ->
            match eq.etype with
              |It _ -> code()
              |_ -> ()
        /// <summary>
        /// 指定された式または変数eqが実数型の場合のみcodeを実行
        /// </summary>
        /// <param name="eq"></param>
        static member d(eq:num0) = fun code ->
            match eq.etype with
              |Dt -> code()
              |_ -> ()
        /// <summary>
        /// 指定された式または変数eqが複素数型の場合のみcodeを実行
        /// </summary>
        /// <param name="eq"></param>
        static member z(eq:num0) = fun code ->
            match eq.etype with
              |Zt -> code()
              |_ -> ()
        /// <summary>
        /// 指定された式または変数eqが整数型の場合のみcodeを実行
        /// </summary>
        /// <param name="label">警告文字</param>
        static member i (label:string) = fun (eq:num0) code ->
            match eq.etype with
              |It _ ->
                code()
              |Zt -> 
                printfn "%s" (label+": 整数型が必要ですが、複素数型が適用されています")
                ignore <| Console.Read()
              |Dt -> 
                printfn "%s" (label+": 整数型が必要ですが、実数型が適用されています")
                ignore <| Console.Read()
              |_ -> 
                printfn "%s" (label+": 整数以外の型が適用されています")
                ignore <| Console.Read()
        /// <summary>
        /// 指定された式または変数eqが実数型の場合のみcodeを実行
        /// </summary>
        /// <param name="label">警告文字</param>
        static member d (label:string) = fun (eq:num0) code ->
            match eq.etype with
              |Dt ->
                code()
              |It _ -> 
                printfn "%s" (label+": 実数型が必要ですが、整数型が適用されています")
                ignore <| Console.Read()
              |Zt -> 
                printfn "%s" (label+": 実数型が必要ですが、複素数型が適用されています")
                ignore <| Console.Read()
              |_ -> 
                printfn "%s" (label+": 実数以外の型が適用されています")
                ignore <| Console.Read()
        /// <summary>
        /// 指定された式または変数eqが複素数型の場合のみcodeを実行
        /// </summary>
        /// <param name="label">警告文字</param>
        static member z (label:string) = fun (eq:num0) code ->
            match eq.etype with
              |Zt ->
                code()
              |It _ -> 
                printfn "%s" (label+": 複素数型が必要ですが、整数型が適用されています")
                ignore <| Console.Read()
              |Dt -> 
                printfn "%s" (label+": 複素数型が必要ですが、実数型が適用されています")
                ignore <| Console.Read()
              |_ -> 
                printfn "%s" (label+": 複素数以外の型が適用されています")
                ignore <| Console.Read()
                