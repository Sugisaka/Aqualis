(*
Copyright (c) 2022 Jun-ichiro Sugisaka

This software is released under the MIT License.
http://opensource.org/licenses/mit-license.php
*)
namespace Aqualis
    
    open Aqualis_base

    ///<summary>コード生成の設定</summary>
    type AqualisCompiler () =
        
        ///<summary>言語</summary>
        static member lang = p.lang
        
        ///<summary>プロジェクト名</summary>
        static member projname = p.projectname
        
        ///<summary>整数を文字列に変換した時の桁数</summary>
        static member int_string_format with get() = p.iFormat

        ///<summary>整数をn桁の文字列で変換するように設定</summary>
        static member set_int_string_format(d) = p.setIFormat(d)
        
        ///<summary>倍精度浮動小数点をn桁（小数点以下m桁）の文字列で変換するように設定</summary>
        static member double_string_format with get() = p.dFormat
        
        ///<summary>倍精度浮動小数点をn桁（小数点以下m桁）の文字列で変換するように設定</summary>
        static member set_double_string_format(n,d) = p.setDFormat(n,d)
        
        ///<summary>デバッグモードの切り替え</summary>
        static member set_DebugMode (x:Switch) =
            match x with
            |ON  -> p.setDebugMode true
            |OFF -> p.setDebugMode false
              
        ///<summary>デバッグモードの切り替え</summary>
        static member set_DisplaySection (x:Switch) =
            match x with
            |ON  -> p.setDisplaySection true
            |OFF -> p.setDisplaySection false

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
            |Fortran  -> p.codewrite("stop") 
            |C99 -> p.codewrite("return 1;") 
            |LaTeX  -> p.codewrite("stop")
            |HTML  -> p.codewrite("stop")
            |Python  -> p.codewrite("sys.exit(1)")
              
        ///<summary>何かのキーを押すまで実行を一時停止</summary>
        static member stop() =
            match p.lang with
            |Fortran   -> p.codewrite("read *, \n")
            |C99 -> p.codewrite("getchar();\n")
            |LaTeX   -> p.codewrite("stop\n")
            |HTML   -> p.codewrite("stop\n")
            |Python  -> p.codewrite("input()")
              
        ///<summary>何かのキーを押すまで実行を一時停止</summary>
        ///<param name="t">一時停止前に表示する文字列</param>
        static member stop(t:string) =
            print.t t
            match p.lang with
            |Fortran   -> p.codewrite("read *, \n")
            |C99 -> p.codewrite("getchar();\n")
            |LaTeX   -> p.codewrite("stop\n")
            |HTML   -> p.codewrite("stop\n")
            |Python  -> p.codewrite("input()")
              
        /// <summary>
        /// インクルードファイル追加（TeXの場合はプリアンブル部挿入コード）
        /// </summary>
        /// <param name="t">オプション</param>
        static member incld(s:string) =
            p.incld s
            
        /// <summary>
        /// コンパイルオプションを追加
        /// </summary>
        /// <param name="t">オプション</param>
        static member option(t:string) =
            p.option("-"+t)