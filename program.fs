(*
Copyright (c) 2022 Jun-ichiro Sugisaka

This software is released under the MIT License.
http://opensource.org/licenses/mit-license.php
*)
namespace Aqualis
    
    open System.IO

    ///<summary>複数のプログラム・関数の管理</summary>
    type program () =
        let mutable param_lst:param list = []
        
        let mutable param_idx = []
    
        ///<summary>作成済みのすべての関数を削除</summary>
        member __.clear() =
            param_lst <- []
            param_idx <- []
            
        ///<summary>メイン関数</summary>
        member __.param_main with get() = param_lst.[0]
        
        ///<summary>インデックスiの関数</summary>
        member __.parami(i:int) = param_lst.[i]
        
        ///<summary>現在生成中の関数</summary>
        member __.param with get() = param_lst.[param_idx.[0]]
        
        ///<summary>現在指定されている言語</summary>
        member __.lang with get() = 
            if param_idx.Length=0 then
                F
            else
                let pp = param_lst.[param_idx.[0]]
                pp.lang
                
        ///<summary>関数を追加</summary>
        member __.param_add(lang_,dir_,proj_) =
            //書き込み中の関数があればストリームを閉じる
            if param_idx.Length>0 then
                param_lst.[param_idx.[0]].cclose()
                param_lst.[param_idx.[0]].vclose()
                param_lst.[param_idx.[0]].hclose()
            let p = new param(lang_,dir_,proj_)
            param_lst <- param_lst@[p]
            param_idx <- (param_lst.Length-1)::param_idx
    
        ///<summary>呼び出し元の関数に戻る</summary>
        member __.param_back() =
            match param_idx with
            |x::y ->
                param_lst.[x].cclose()
                param_lst.[x].vclose()
                param_lst.[x].hclose()
                param_lst.[y.[0]].copen()
                param_lst.[y.[0]].vopen()
                param_lst.[y.[0]].hopen()
                param_idx <- y
            |_ -> ()
              
        ///<summary>設定されたプロジェクト名</summary>
        member __.projectname with get() = param_lst.[param_idx.[0]].projectname
        ///<summary>ソースファイル出力先ディレクトリ</summary>
        member __.dir with get() = param_lst.[param_idx.[0]].dir
        ///<summary>コードをファイルに書き込み</summary>
        member __.codewrite(s) = param_lst.[param_idx.[0]].codewrite(s)
        ///<summary>エラーID</summary>
        member __.errorID with get() = param_lst.[param_idx.[0]].error_code_counter
        ///<summary>エラーIDをインクリメント</summary>
        member __.errorIDinc() = 
            param_lst.[param_idx.[0]].error_code_counter <- param_lst.[param_idx.[0]].error_code_counter + 1
        ///<summary>デバッグモード</summary>
        member __.debugMode with get() = param_lst.[param_idx.[0]].debug_mode
        ///<summary>コメント書き込み</summary>
        member __.comment(s) = param_lst.[param_idx.[0]].comment(s)
        ///<summary>インデントを下げる</summary>
        member __.indentInc() = param_lst.[param_idx.[0]].indent.inc()
        ///<summary>インデントを上げる</summary>
        member __.indentDec() = param_lst.[param_idx.[0]].indent.dec()
        ///<summary>インデント用スペース</summary>
        member __.indentSpace with get() = param_lst.[param_idx.[0]].indent.space
        ///<summary>セクションのヘッダを画面出力</summary>
        member __.displaySection with get() = param_lst.[param_idx.[0]].display_section
        ///<summary>一時変数(整数)</summary>
        member __.i_cache_var with get() = param_lst.[param_idx.[0]].i_cache_var
        ///<summary>一時変数(小数)</summary>
        member __.d_cache_var with get() = param_lst.[param_idx.[0]].d_cache_var
        ///<summary>一時変数(複素数)</summary>
        member __.z_cache_var with get() = param_lst.[param_idx.[0]].z_cache_var
        ///<summary>一時変数(整数1次元配列)</summary>
        member __.i1_cache_var with get() = param_lst.[param_idx.[0]].i1_cache_var
        ///<summary>一時変数(小数1次元配列)</summary>
        member __.d1_cache_var with get() = param_lst.[param_idx.[0]].d1_cache_var
        ///<summary>一時変数(複素数1次元配列)</summary>
        member __.z1_cache_var with get() = param_lst.[param_idx.[0]].z1_cache_var
        ///<summary>一時変数(整数2次元配列)</summary>
        member __.i2_cache_var with get() = param_lst.[param_idx.[0]].i2_cache_var
        ///<summary>一時変数(小数2次元配列)</summary>
        member __.d2_cache_var with get() = param_lst.[param_idx.[0]].d2_cache_var
        ///<summary>一時変数(複素数2次元配列)</summary>
        member __.z2_cache_var with get() = param_lst.[param_idx.[0]].z2_cache_var
        ///<summary>一時変数(整数3次元配列)</summary>
        member __.i3_cache_var with get() = param_lst.[param_idx.[0]].i3_cache_var
        ///<summary>一時変数(小数3次元配列)</summary>
        member __.d3_cache_var with get() = param_lst.[param_idx.[0]].d3_cache_var
        ///<summary>一時変数(複素数3次元配列)</summary>
        member __.z3_cache_var with get() = param_lst.[param_idx.[0]].z3_cache_var
        ///<summary>int型の数値を文字列に変換</summary>
        member __.ItoS(n) = param_lst.[param_idx.[0]].ItoS(n)
        ///<summary>double型の数値を文字列に変換</summary>
        member __.DtoS(n) = param_lst.[param_idx.[0]].DtoS(n)
        ///<summary>整数型を文字列に変換するときの桁数</summary>
        member __.iFormat with get() = param_lst.[param_idx.[0]].int_string_format
        ///<summary>倍精度浮動小数点型を文字列に変換するときの桁数(全体,小数点以下)</summary>
        member __.dFormat with get() = param_lst.[param_idx.[0]].double_string_format
        ///<summary>変数リスト</summary>
        member __.var with get() = param_lst.[param_idx.[0]].var
        ///<summary>mainコードまたは関数内で使用される削除不可能な変数番号（ファイルポインタ）</summary>
        member __.fvar with get() = param_lst.[param_idx.[0]].f_stat_var
        ///<summary>プライベート変数リスト</summary>
        member __.pvar with get() = param_lst.[param_idx.[0]].pvar
        ///<summary>ホストからGPUへ転送する変数リスト</summary>
        member __.civar with get() = param_lst.[param_idx.[0]].civar
        ///<summary>GPUからホストへ転送する変数リスト</summary>
        member __.covar with get() = param_lst.[param_idx.[0]].covar
        ///<summary>ループカウンタ変数を作成し、code内の処理を実行</summary>
        member __.getloopvar with get() = param_lst.[param_idx.[0]].getloopvar
        ///<summary>ループカウンタ変数とループ脱出先gotoラベルを作成し、code内の処理を実行</summary>
        member __.getloopvar_exit with get() = param_lst.[param_idx.[0]].getloopvar_exit
        ///<summary>コードを一時ファイルに書き込み</summary>
        member __.cwrite(s) = param_lst.[param_idx.[0]].cwrite(s)
        ///<summary>構造体・関数宣言の一時ファイルに書き込み</summary>
        member __.hwrite(s) = param_lst.[param_idx.[0]].hwrite(s)
        ///<summary>コンパイル時に必要なライブラリ・オプション</summary>
        member __.option(s) = param_lst.[param_idx.[0]].olist.add(s)
        ///<summary>ライブラリの使用時に必要なextern指定子</summary>
        member __.extn(s) = param_lst.[param_idx.[0]].elist.add(s)
        ///<summary>ライブラリの使用時に必要なヘッダーファイル</summary>
        member __.incld(s) = param_lst.[param_idx.[0]].hlist.add(s)
        ///<summary>ライブラリの使用時に必要なモジュールファイル</summary>
        member __.modl(s) = param_lst.[param_idx.[0]].mlist.add(s)
        ///<summary>コードの一時ファイルを閉じる</summary>
        member __.cclose() = param_lst.[param_idx.[0]].cclose()
        ///<summary>コードの一時ファイルを開く</summary>
        member __.copen() = param_lst.[param_idx.[0]].copen()
        ///<summary>並列処理の一時ファイルを開く</summary>
        member __.popen() = param_lst.[param_idx.[0]].popen()
        ///<summary>並列ループ処理書き込み先一時ファイルストリームを閉じる</summary>
        member __.pclose() = param_lst.[param_idx.[0]].pwriter.Close()
        ///<summary>構造体・関数宣言の一時ファイルを閉じる</summary>
        member __.hclose() = param_lst.[param_idx.[0]].hclose()
        ///<summary>変数宣言の一時ファイルを閉じる</summary>
        member __.vclose() = param_lst.[param_idx.[0]].vclose()
        ///<summary>コンパイル時に必要な他のソースファイル</summary>
        member __.slist with get() = param_lst.[param_idx.[0]].slist
        ///<summary>ライブラリの使用時に必要なモジュールファイル</summary>
        member __.mlist with get() = param_lst.[param_idx.[0]].mlist
        ///<summary>ライブラリの使用時に必要なヘッダーファイル</summary>
        member __.hlist with get() = param_lst.[param_idx.[0]].hlist
        ///<summary>コンパイル時に必要なライブラリ・オプション</summary>
        member __.olist with get() = param_lst.[param_idx.[0]].olist
        ///<summary>ライブラリの使用時に必要なextern指定子</summary>
        member __.elist with get() = param_lst.[param_idx.[0]].elist
        ///<summary>この関数の引数リスト： 関数呼び出しに与えられた変数名,(関数内での変数情報)</summary>
        member __.arglist with get() = param_lst.[param_idx.[0]].arglist
        ///<summary>trueのときOpenMPが使用中</summary>
        member __.isOmpUsed 
            with get()  = param_lst.[param_idx.[0]].isOmpUsed
            and  set(v) = param_lst.[param_idx.[0]].isOmpUsed <- v
        ///<summary>trueのときOpenACCが使用中</summary>
        member __.isOaccUsed
            with get()  = param_lst.[param_idx.[0]].isOaccUsed
            and  set(v) = param_lst.[param_idx.[0]].isOaccUsed <- v
        ///<summary>変数宣言のコード生成</summary>
        member __.declare(typ,vtp,name,param) = param_lst.[param_idx.[0]].declare(typ,vtp,name,param)
        ///<summary>宣言されたすべての変数を一時ファイルに書き込み</summary>
        member __.declareall() = param_lst.[param_idx.[0]].declareall()
        ///<summary>並列処理書き込みモード</summary>
        member __.parmode code = 
            param_lst.[param_idx.[0]].switch_parmode(true)
            code()
            param_lst.[param_idx.[0]].switch_parmode(false)
        ///<summary>trueのとき並列処理を書き込む</summary>
        member __.isparmode with get() = param_lst.[param_idx.[0]].isParMode
        ///<summary>並列処理の一時ファイルの内容</summary>
        member __.readParText() = param_lst.[param_idx.[0]].readpartext()
        ///<summary>並列処理の一時ファイルを削除</summary>
        member __.ParDelete() = param_lst.[param_idx.[0]].pdelete()
        ///<summary>trueの時はデバッグ用のコードを生成する</summary>
        member __.setDebugMode(s) = param_lst.[param_idx.[0]].debug_mode <- s
        ///<summary>セクションのヘッダを画面出力</summary>
        member __.setDisplaySection(s) = param_lst.[param_idx.[0]].display_section <- s
        ///<summary>mainコードまたは関数内で使用される削除不可能な変数番号（ファイルポインタ）</summary>
        member __.fNumber() = param_lst.[param_idx.[0]].f_stat_var.getvar(fun i -> "f"+i.ToString("000"))
        ///<summary>mainコードまたは関数内で使用される削除不可能な変数番号（文字列）</summary>
        member __.tName() = param_lst.[param_idx.[0]].t_stat_var.getvar(fun i -> "t"+i.ToString("000"))
        ///<summary>整数型を文字列に変換するときの桁数</summary>
        member __.setIFormat(n) = param_lst.[param_idx.[0]].int_string_format <- n
        ///<summary>倍精度浮動小数点型を文字列に変換するときの桁数(全体,小数点以下)</summary>
        member __.setDFormat(n,m) = param_lst.[param_idx.[0]].set_double_string_format(n,m)
        /// <summary>
        /// 関数定義の引数を追加
        /// </summary>
        /// <param name="typ">変数の型</param>
        /// <param name="vtp">変数の次元</param>
        /// <param name="n">変数名</param>
        member __.addarg(typ,vtp,n) = param_lst.[param_idx.[0]].addarg(typ,vtp,n)
        member __.codefold(s,cm,writer,sp) = param_lst.[param_idx.[0]].codefold(s,cm,writer,sp)
        member __.paramClear() = param_lst.[param_idx.[0]].clear()
        member __.plist() = param_lst.[param_idx.[0]].plist
        
    module Aqualis_base =
        
        ///<summary>trueのとき数式を最適化</summary>
        let mutable isEqSimplify = true
        
        let p = program()
        