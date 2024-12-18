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
        member _.clear() =
            param_lst <- []
            param_idx <- []
            
        ///<summary>メイン関数</summary>
        member _.param_main with get() = param_lst.[0]
        
        ///<summary>インデックスiの関数</summary>
        member _.parami(i:int) = param_lst.[i]
        
        ///<summary>現在生成中の関数</summary>
        member _.param with get() = param_lst.[param_idx.[0]]
        
        member _.isEmpty with get() = param_lst.Length=0

        ///<summary>現在指定されている言語</summary>
        member this.lang with get() = 
            if param_idx.Length=0 then
                Fortran
            else
                let pp = this.param
                pp.lang
                
        ///<summary>関数を追加</summary>
        member this.param_add(lang_,dir_,proj_) =
            //書き込み中の関数があればストリームを閉じる
            if param_idx.Length>0 then
                this.param.cclose()
                this.param.vclose()
                this.param.hclose()
            let p = new param(lang_,dir_,proj_)
            param_lst <- param_lst@[p]
            param_idx <- (param_lst.Length-1)::param_idx
    
        ///<summary>呼び出し元の関数に戻る</summary>
        member _.param_back() =
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
        member this.projectname with get() = this.param.projectname
        ///<summary>ソースファイル出力先ディレクトリ</summary>
        member this.dir with get() = this.param.dir
        ///<summary>コードをファイルに書き込み</summary>
        member this.codewrite(s) = this.param.codewrite(s)
        ///<summary>エラーID</summary>
        member this.errorID with get() = this.param.error_code_counter
        ///<summary>エラーIDをインクリメント</summary>
        member this.errorIDinc() = 
            this.param.error_code_counter <- this.param.error_code_counter + 1
        ///<summary>デバッグモード</summary>
        member this.debugMode with get() = this.param.debug_mode
        ///<summary>コメント書き込み</summary>
        member this.comment(s) = this.param.comment(s)
        ///<summary>インデントを下げる</summary>
        member this.indentInc() = this.param.indent.inc()
        ///<summary>インデントを上げる</summary>
        member this.indentDec() = this.param.indent.dec()
        ///<summary>インデント用スペース</summary>
        member this.indentSpace with get() = this.param.indent.space
        ///<summary>セクションのヘッダを画面出力</summary>
        member this.displaySection with get() = this.param.display_section
        ///<summary>級数比較用ダミーインデックス</summary>
        member this.dum_cache_var with get() = this.param.dum_cache_var
        ///<summary>一時変数(整数)</summary>
        member this.i_cache_var with get() = this.param.i_cache_var
        ///<summary>一時変数(小数)</summary>
        member this.d_cache_var with get() = this.param.d_cache_var
        ///<summary>一時変数(複素数)</summary>
        member this.z_cache_var with get() = this.param.z_cache_var
        ///<summary>一時変数(整数1次元配列)</summary>
        member this.i1_cache_var with get() = this.param.i1_cache_var
        ///<summary>一時変数(小数1次元配列)</summary>
        member this.d1_cache_var with get() = this.param.d1_cache_var
        ///<summary>一時変数(複素数1次元配列)</summary>
        member this.z1_cache_var with get() = this.param.z1_cache_var
        ///<summary>一時変数(整数2次元配列)</summary>
        member this.i2_cache_var with get() = this.param.i2_cache_var
        ///<summary>一時変数(小数2次元配列)</summary>
        member this.d2_cache_var with get() = this.param.d2_cache_var
        ///<summary>一時変数(複素数2次元配列)</summary>
        member this.z2_cache_var with get() = this.param.z2_cache_var
        ///<summary>一時変数(整数3次元配列)</summary>
        member this.i3_cache_var with get() = this.param.i3_cache_var
        ///<summary>一時変数(小数3次元配列)</summary>
        member this.d3_cache_var with get() = this.param.d3_cache_var
        ///<summary>一時変数(複素数3次元配列)</summary>
        member this.z3_cache_var with get() = this.param.z3_cache_var
        ///<summary>int型の数値を文字列に変換</summary>
        member this.ItoS(n) = 
            if this.isEmpty then
                //Compile関数外で使用された場合
                n.ToString()
            else
                this.param.ItoS(n)
        ///<summary>double型の数値を文字列に変換</summary>
        member this.DtoS(n) =
            if this.isEmpty then
                //Compile関数外で使用された場合
                n.ToString()
            else
                this.param.DtoS(n)
        ///<summary>整数型を文字列に変換するときの桁数</summary>
        member this.iFormat with get() = this.param.int_string_format
        ///<summary>倍精度浮動小数点型を文字列に変換するときの桁数(全体,小数点以下)</summary>
        member this.dFormat with get() = this.param.double_string_format
        ///<summary>変数リスト</summary>
        member this.var with get() = this.param.var
        ///<summary>mainコードまたは関数内で使用される削除不可能な変数番号（ファイルポインタ）</summary>
        member this.fvar with get() = this.param.f_stat_var
        ///<summary>プライベート変数リスト</summary>
        member this.pvar with get() = this.param.pvar
        ///<summary>ホストからGPUへ転送する変数リスト</summary>
        member this.civar with get() = this.param.civar
        ///<summary>GPUからホストへ転送する変数リスト</summary>
        member this.covar with get() = this.param.covar
        ///<summary>ループカウンタ変数を作成し、code内の処理を実行</summary>
        member this.getloopvar with get() = this.param.getloopvar
        ///<summary>ループカウンタ変数とループ脱出先gotoラベルを作成し、code内の処理を実行</summary>
        member this.getloopvar_exit with get() = this.param.getloopvar_exit
        ///<summary>ループ脱出先gotoラベルをひとつ前に戻す</summary>
        member this.exit_false with get() = this.param.exit_false
        ///<summary>ループ脱出先gotoラベルのリセット</summary>
        member this.exit_reset with get() = this.param.exit_reset
        ///<summary>コードを一時ファイルに書き込み</summary>
        member this.cwrite(s) = this.param.cwrite(s)
        ///<summary>構造体・関数宣言の一時ファイルに書き込み</summary>
        member this.hwrite(s) = this.param.hwrite(s)
        ///<summary>コンパイル時に必要なライブラリ・オプション</summary>
        member this.option(s) = this.param.olist.add(s)
        ///<summary>ライブラリの使用時に必要なextern指定子</summary>
        member this.extn(s) = this.param.elist.add(s)
        ///<summary>ライブラリの使用時に必要なヘッダーファイル</summary>
        member this.incld(s) = this.param.hlist.add(s)
        ///<summary>ライブラリの使用時に必要なモジュールファイル</summary>
        member this.modl(s) = this.param.mlist.add(s)
        ///<summary>コードの一時ファイルを閉じる</summary>
        member this.cclose() = this.param.cclose()
        ///<summary>コードの一時ファイルを開く</summary>
        member this.copen() = this.param.copen()
        ///<summary>並列処理の一時ファイルを開く</summary>
        member this.popen() = this.param.popen()
        ///<summary>並列ループ処理書き込み先一時ファイルストリームを閉じる</summary>
        member this.pclose() = this.param.pwriter.Close()
        ///<summary>構造体・関数宣言の一時ファイルを閉じる</summary>
        member this.hclose() = this.param.hclose()
        ///<summary>変数宣言の一時ファイルを閉じる</summary>
        member this.vclose() = this.param.vclose()
        ///<summary>コンパイル時に必要な他のソースファイル</summary>
        member this.slist with get() = this.param.slist
        ///<summary>ライブラリの使用時に必要なモジュールファイル</summary>
        member this.mlist with get() = this.param.mlist
        ///<summary>ライブラリの使用時に必要なヘッダーファイル</summary>
        member this.hlist with get() = this.param.hlist
        ///<summary>コンパイル時に必要なライブラリ・オプション</summary>
        member this.olist with get() = this.param.olist
        ///<summary>ライブラリの使用時に必要なextern指定子</summary>
        member this.elist with get() = this.param.elist
        ///<summary>この関数の引数リスト： 関数呼び出しに与えられた変数名,(関数内での変数情報)</summary>
        member this.arglist with get() = this.param.arglist
        ///<summary>trueのときOpenMPが使用中</summary>
        member this.isOmpUsed 
            with get()  = this.param.isOmpUsed
            and  set(v) = this.param.isOmpUsed <- v
        ///<summary>trueのときOpenACCが使用中</summary>
        member this.isOaccUsed
            with get()  = this.param.isOaccUsed
            and  set(v) = this.param.isOaccUsed <- v
        ///<summary>変数宣言のコード生成</summary>
        member this.declare(typ,vtp,name,param) = this.param.declare(typ,vtp,name,param)
        ///<summary>宣言されたすべての変数を一時ファイルに書き込み</summary>
        member this.declareall() = this.param.declareall()
        ///<summary>並列処理書き込みモード</summary>
        member this.parmode code = 
            this.param.switch_parmode(true)
            code()
            this.param.switch_parmode(false)
        ///<summary>trueのとき並列処理を書き込む</summary>
        member this.isparmode with get() = this.param.isParMode
        ///<summary>並列処理の一時ファイルの内容</summary>
        member this.readParText() = this.param.readpartext()
        ///<summary>並列処理の一時ファイルを削除</summary>
        member this.ParDelete() = this.param.pdelete()
        ///<summary>trueの時はデバッグ用のコードを生成する</summary>
        member this.setDebugMode(s) = this.param.debug_mode <- s
        ///<summary>セクションのヘッダを画面出力</summary>
        member this.setDisplaySection(s) = this.param.display_section <- s
        ///<summary>mainコードまたは関数内で使用される削除不可能な変数番号（ファイルポインタ）</summary>
        member this.fNumber() = this.param.f_stat_var.getvar(fun i -> "f"+i.ToString("000"))
        ///<summary>mainコードまたは関数内で使用される削除不可能な変数番号（文字列）</summary>
        member this.tName() = this.param.t_stat_var.getvar(fun i -> "t"+i.ToString("000"))
        ///<summary>整数型を文字列に変換するときの桁数</summary>
        member this.setIFormat(n) = this.param.int_string_format <- n
        ///<summary>倍精度浮動小数点型を文字列に変換するときの桁数(全体,小数点以下)</summary>
        member this.setDFormat(n,m) = this.param.set_double_string_format(n,m)
        /// <summary>
        /// 関数定義の引数を追加
        /// </summary>
        /// <param name="typ">変数の型</param>
        /// <param name="vtp">変数の次元</param>
        /// <param name="n">変数名</param>
        member this.addarg(typ,vtp,n) = this.param.addarg(typ,vtp,n)
        member this.codefold(s,cm,writer,sp) = this.param.codefold(s,cm,writer,sp)
        member this.paramClear() = this.param.clear()
        member this.plist() = this.param.plist
        
    module Aqualis_base =
        
        ///<summary>trueのとき数式を最適化</summary>
        let mutable isEqSimplify = true
        
        let p = program()
        