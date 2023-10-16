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

    ///<summary>コード生成の設定</summary>
    type AqualisCompiler () =
        
        ///<summary>言語</summary>
        static member lang = p.lang
        
        ///<summary>プロジェクト名</summary>
        static member projname = p.param.projectname
        
        ///<summary>整数を文字列に変換した時の桁数</summary>
        static member int_string_format with get() = p.param.int_string_format

        ///<summary>整数をn桁の文字列で変換するように設定</summary>
        static member set_int_string_format(d) = p.param.set_int_string_format(d)
        
        ///<summary>倍精度浮動小数点をn桁（小数点以下m桁）の文字列で変換するように設定</summary>
        static member double_string_format with get() = p.param.double_string_format
        
        ///<summary>倍精度浮動小数点をn桁（小数点以下m桁）の文字列で変換するように設定</summary>
        static member set_double_string_format(n,d) = p.param.set_double_string_format(n,d)
        
        ///<summary>デバッグモードの切り替え</summary>
        static member set_DebugMode (x:Switch) =
            match x with
            |ON  -> p.param.set_debugmode true
            |OFF -> p.param.set_debugmode false
              
        ///<summary>デバッグモードの切り替え</summary>
        static member set_DisplaySection (x:Switch) =
            match x with
            |ON  -> p.param.set_displaysection true
            |OFF -> p.param.set_displaysection false

        ///<summary>数式の最適化設定</summary>
        static member set_EquationSimplify (x:Switch) =
            match x with
            |ON  -> isEqSimplify <- true
            |OFF -> isEqSimplify <- false
              
        ///<summary>codeをデバッグモードで実行</summary>
        static member debug code =
            AqualisCompiler.set_DebugMode ON
            code()
            AqualisCompiler.set_DebugMode OFF
                
        ///<summary>プログラムの実行を強制終了</summary>
        static member abort() =
            match p.lang with 
            |F  -> p.codewrite("stop") 
            |C-> p.codewrite("return 1;") 
            |T  -> p.codewrite("stop")
            |H  -> p.codewrite("stop")
              
        ///<summary>何かのキーを押すまで実行を一時停止</summary>
        static member stop() =
            match p.lang with
            |F   -> p.codewrite("read *, \n")
            |C -> p.codewrite("getchar();\n")
            |T   -> p.codewrite("stop\n")
            |H   -> p.codewrite("stop\n")
              
        ///<summary>何かのキーを押すまで実行を一時停止</summary>
        ///<param name="t">一時停止前に表示する文字列</param>
        static member stop(t:string) =
            print.t t
            match p.lang with
            |F   -> p.codewrite("read *, \n")
            |C -> p.codewrite("getchar();\n")
            |T   -> p.codewrite("stop\n")
            |H   -> p.codewrite("stop\n")
              
        /// <summary>
        /// コンパイルオプションを追加
        /// </summary>
        /// <param name="t">オプション</param>
        static member option(t:string) =
            p.param.option_("-"+t)