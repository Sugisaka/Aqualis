(*
Copyright (c) 2022 Jun-ichiro Sugisaka

This software is released under the MIT License.
http://opensource.org/licenses/mit-license.php
*)
namespace Aqualis
    
    open System
    open System.IO
    open System.Text

    ///<summary>プログラム・関数の設定とコード書き込み</summary>
    type param (lan:Language,outputdir:string,proj:string) =
        ///<summary>trueの時はデバッグ用のコードを生成する</summary>
        let mutable debug_mode = false
        ///<summary>コードのインデント設定</summary>
        let mutable indentsize = 2
        ///<summary>コードの現在のインデント</summary>
        let mutable indentposition = 0
        ///<summary>gotoで使用するラベル番号</summary>
        let mutable goto_label = 10
        ///<summary>整数型を文字列に変換するときの桁数</summary>
        let mutable int_string_format_ = 8
        ///<summary>倍精度浮動小数点型を文字列に変換するときの桁数(全体,小数点以下)</summary>
        let mutable double_string_format_ = (27,17)
        ///<summary>セクションのヘッダを画面出力</summary>
        let mutable display_section = false

        ///<summary>doループのカウンタに現在使用できる変数インデックス</summary>
        let loopvar = new varlist()
        ///<summary>mainコードまたは関数内で使用される削除不可能な変数番号（整数）</summary>
        let i_stat_var = new varlist()
        ///<summary>mainコードまたは関数内で使用される削除不可能な変数番号（小数）</summary>
        let d_stat_var = new varlist()
        ///<summary>mainコードまたは関数内で使用される削除不可能な変数番号（複素数）</summary>
        let z_stat_var = new varlist()
        ///<summary>mainコードまたは関数内で使用される削除不可能な変数番号（文字列）</summary>
        let t_stat_var = new varlist()
        ///<summary>mainコードまたは関数内で使用される削除不可能な変数番号（ファイルポインタ）</summary>
        let f_stat_var = new varlist()
        ///<summary>一つの代入文中の式を分解する際に使用する一時変数番号（整数）。代入処理後にすべてリセットされる</summary>
        let i_temp_var = new varlist()
        ///<summary>一つの代入文中の式を分解する際に使用する一時変数番号（小数）。代入処理後にすべてリセットされる</summary>
        let d_temp_var = new varlist()
        ///<summary>一つの代入文中の式を分解する際に使用する一時変数番号（複素数）。代入処理後にすべてリセットされる</summary>
        let z_temp_var = new varlist()
        ///<summary>複数の代入文で続けて使用できる一時変数（整数）。dispose関数によって手動での削除が必要</summary>
        let i_cache_var = new varlist()
        ///<summary>複数の代入文で続けて使用できる一時変数（小数）。dispose関数によって手動での削除が必要</summary>
        let d_cache_var = new varlist()
        ///<summary>複数の代入文で続けて使用できる一時変数（複素数）。dispose関数によって手動での削除が必要</summary>
        let z_cache_var = new varlist()
        ///<summary>複数の代入文で続けて使用できる一時変数（ファイルポインタ）。dispose関数によって手動での削除が必要</summary>
        let f_cache_var = new varlist()
        ///<summary>複数の代入文で続けて使用できる一時変数（整数1次元配列）。dispose関数によって手動での削除が必要</summary>
        let i1_cache_var = new varlist()
        ///<summary>複数の代入文で続けて使用できる一時変数（小数1次元配列）。dispose関数によって手動での削除が必要</summary>
        let d1_cache_var = new varlist()
        ///<summary>複数の代入文で続けて使用できる一時変数（複素数1次元配列）。dispose関数によって手動での削除が必要</summary>
        let z1_cache_var = new varlist()
        ///<summary>複数の代入文で続けて使用できる一時変数（整数2次元配列）。dispose関数によって手動での削除が必要</summary>
        let i2_cache_var = new varlist()
        ///<summary>複数の代入文で続けて使用できる一時変数（小数2次元配列）。dispose関数によって手動での削除が必要</summary>
        let d2_cache_var = new varlist()
        ///<summary>複数の代入文で続けて使用できる一時変数（複素数2次元配列）。dispose関数によって手動での削除が必要</summary>
        let z2_cache_var = new varlist()
        ///<summary>複数の代入文で続けて使用できる一時変数（整数3次元配列）。dispose関数によって手動での削除が必要</summary>
        let i3_cache_var = new varlist()
        ///<summary>複数の代入文で続けて使用できる一時変数（小数3次元配列）。dispose関数によって手動での削除が必要</summary>
        let d3_cache_var = new varlist()
        ///<summary>複数の代入文で続けて使用できる一時変数（複素数3次元配列）。dispose関数によって手動での削除が必要</summary>
        let z3_cache_var = new varlist()
        ///<summary>ライブラリの使用時に必要なヘッダーファイル</summary>
        let mutable header_ : string list = []
        ///<summary>ライブラリの使用時に必要なモジュールファイル</summary>
        let mutable modl_ : string list = []
        ///<summary>ライブラリの使用時に必要なextern指定子</summary>
        let mutable extn_ : string list = []
        ///<summary>定義された関数のリスト</summary>
        let mutable funlist_: string list = []
        ///<summary>変数リスト</summary>
        let mutable vlist_:(Etype*VarType*string*string) list = []
        ///<summary>プライベート変数リスト</summary>
        let mutable pvlist_:string list = []
        ///<summary>ホストからGPUへ転送する変数リスト</summary>
        let mutable copy_in_vlist_:string list = []
        ///<summary>GPUからホストへ転送する変数リスト</summary>
        let mutable copy_out_vlist_:string list = []
        ///<summary>この関数の引数リスト： 関数呼び出しに与えられた変数名,(関数内での変数情報)</summary>
        let mutable arglist_ : (string*(Etype*VarType*string*string*string)) list = []
        ///<summary>エラーid</summary>
        let mutable error_code_counter_ = 1
        ///<summary>コンパイル時に必要なライブラリ・オプション</summary>
        let mutable olist_ : string list = []
        ///<summary>コンパイル時に必要な他のソースファイル</summary>
        let mutable slist_ : string list = []
        ///<summary>コード書き込み先一時ファイル</summary>
        let cfile = outputdir+"\\"+proj+"_code.bee"
        ///<summary>変数宣言書き込み先一時ファイル</summary>
        let vfile = outputdir+"\\"+proj+"_var.bee"
        ///<summary>構造体・関数宣言書き込み先一時ファイル</summary>
        let hfile = outputdir+"\\"+proj+".bee"
        ///<summary>並列ループ処理書き込み先一時ファイル</summary>
        let pfile = outputdir+"\\"+proj+"_par.bee"
        ///<summary>コード書き込み先一時ファイルストリーム</summary>
        let mutable cwriter:StreamWriter = new StreamWriter(cfile,false)
        ///<summary>変数宣言書き込み先一時ファイルストリーム</summary>
        let mutable vwriter:StreamWriter = new StreamWriter(vfile,false)
        ///<summary>構造体・関数宣言書き込み先一時ファイルストリーム</summary>
        let mutable hwriter:StreamWriter = new StreamWriter(hfile,false)
        ///<summary>並列ループ処理書き込み先一時ファイルストリーム</summary>
        let mutable pwriter:StreamWriter = new StreamWriter(pfile,false)
        ///<summary>trueのとき並列処理を書き込む</summary>
        let mutable par_mode = false
        ///<summary>trueのときOpenMPが使用中</summary>
        let mutable is_omp_used = false
        ///<summary>trueのときOpenACCが使用中</summary>
        let mutable is_oacc_used = false
        
        ///<summary>ソースファイル出力先ディレクトリ</summary>
        member __.dir with get() = outputdir
    
        ///<summary>外部ソースファイル保存先ディレクトリ</summary>
        member __.sourcedir = @"C:\Aqualis\source"
        
        ///<summary>設定されたプログラミング言語</summary>
        member __.lang with get() = lan
    
        ///<summary>設定されたプロジェクト名</summary>
        member __.projectname with get() = proj
    
        ///<summary>定義された関数のリスト</summary>
        member __.funlist with get() = funlist_
    
        ///<summary>呼び出された関数の重複分を除いたリスト</summary>
        member __.funlist_nonoverlap with get() =
            let rec reduce lst1 lst2 =
                match lst1 with
                  |x::y ->
                    match List.tryFind (fun s -> s=x) lst2 with
                      |None ->
                        reduce y (lst2@[x])
                      |_ ->
                        reduce y lst2
                  |[] -> lst2
            reduce funlist_ []
    
        ///<summary>関数の引数リスト</summary>
        member __.arglist with get() = arglist_
        
        ///<summary>コンパイル時に必要な他のソースファイル</summary>
        member __.slist with get() = slist_
        
        ///<summary>コンパイル時に必要なライブラリ・オプション</summary>
        member __.olist with get() = olist_
        
        ///<summary>変数リスト</summary>
        member __.vlist with get() = vlist_
        
        ///<summary>プライベート変数リスト</summary>
        member __.pvlist with get() = pvlist_

        ///<summary>ホストからGPUに転送する変数リスト</summary>
        member __.copy_in_vlist with get() = copy_in_vlist_

        ///<summary>GPUからホストに転送する変数リスト</summary>
        member __.copy_out_vlist with get() = copy_out_vlist_

        ///<summary>並列処理書き込みモード</summary>
        member __.parmode with get() = par_mode

        ///<summary>trueのときOpenMPが使用中</summary>
        member __.isOmpUsed
            with get () = is_omp_used
            and set (value) = is_omp_used <- value
            
        ///<summary>trueのときOpenACCが使用中</summary>
        member __.isOaccUsed
            with get () = is_oacc_used
            and set (value) = is_oacc_used <- value
            
        ///<summary>整数型を文字列に変換するときの桁数</summary>
        member __.int_string_format with get() = int_string_format_
        
        ///<summary>倍精度浮動小数点型を文字列に変換するときの桁数(全体,小数点以下)</summary>
        member __.double_string_format with get() = double_string_format_
            
        ///<summary>デバッグモード</summary>
        member __.debugmode with get() = debug_mode
        
        ///<summary>ディスプレイモード</summary>
        member __.displaysection with get() = display_section

        ///<summary>エラーid</summary>
        member __.error_code_counter with get() = error_code_counter_
        
        ///<summary>ライブラリの使用時に必要なヘッダーファイル</summary>
        member __.header with get() = header_
        
        ///<summary>ライブラリの使用時に必要なモジュールファイル</summary>
        member __.modl with get() = modl_
        
        ///<summary>ライブラリの使用時に必要なextern指定子</summary>
        member __.extn with get() = extn_
        
        ///<summary>Aqualisバージョン番号</summary>
        member __.ver = System.Diagnostics.FileVersionInfo.GetVersionInfo(System.Reflection.Assembly.GetExecutingAssembly().Location)
        
        ///<summary>整数型を文字列に変換するときの桁数を設定</summary>
        member __.set_int_string_format(x) = int_string_format_ <- x
        
        ///<summary>倍精度浮動小数点型を文字列に変換するときの桁数(全体,小数点以下)</summary>
        member __.set_double_string_format(x) = double_string_format_ <- x
        
        ///<summary>デバッグモードを設定</summary>
        member __.set_debugmode(x) = debug_mode <- x
        
        ///<summary>ディスプレイモードを設定</summary>
        member __.set_displaysection(x) = display_section <- x

        ///<summary>変数を追加</summary>
        member __.vlist_add(x) = vlist_ <- vlist_@[x]
        
        ///<summary>変数を追加</summary>
        member __.pvlist_add(x) = pvlist_ <- pvlist_@[x]

        ///<summary>ヘッダファイルのインクルード</summary>
        member __.include_(s) = 
            match List.exists (fun hd -> hd=s) header_ with
              |true -> ()
              |false -> header_<-header_@[s]
              
        ///<summary>モジュールファイルのインクルード</summary>
        member __.module_(s) = 
            match List.exists (fun md -> md=s) modl_ with
              |true -> ()
              |false -> modl_<-modl_@[s]
              
        ///<summary>extern指定子の追加</summary>
        member __.extern_(s) = 
            match List.exists (fun md -> md=s) extn_ with
              |true -> ()
              |false -> extn_<-extn_@[s]
              
        ///<summary>ソースファイルファイルのインクルード</summary>
        member __.source_(s) = 
            match List.exists (fun ss -> ss=s) slist_ with
              |true -> ()
              |false -> slist_<-slist_@[s]
              
        ///<summary>オプション追加</summary>
        member __.option_(s) = 
            match List.exists (fun op -> op=s) olist_ with
              |true -> ()
              |false -> olist_<-olist_@[s]
    
        ///<summary>int型の数値を文字列に変換</summary>
        member __.ItoS (d:int) =
            match lan with
             |H -> "<mn>"+d.ToString()+"</mn>"
             |_ -> d.ToString()
             
        ///<summary>double型の数値を文字列に変換</summary>
        member __.DtoS (d:double) = 
            match lan with
              |F -> d.ToString("0.0#################E0").Replace("E","d") 
              |C89 |C99 -> d.ToString("0.0#################E0")
              |T -> d.ToString()
              |H -> "<mn>"+d.ToString()+"</mn>"
              |NL -> d.ToString("0.0#################E0")
              
        ///<summary>すべての状態をリセット</summary>
        member __.clear() =
            debug_mode <- false
            indentsize <- 2
            indentposition <- 0
            goto_label <- 10
            loopvar.clear()
            i_stat_var.reset()
            d_stat_var.reset()
            z_stat_var.reset()
            i_temp_var.clear()
            d_temp_var.clear()
            z_temp_var.clear()
            i_cache_var.clear()
            d_cache_var.clear()
            z_cache_var.clear()
            f_cache_var.clear()
            i1_cache_var.clear()
            d1_cache_var.clear()
            z1_cache_var.clear()
            i2_cache_var.clear()
            d2_cache_var.clear()
            z2_cache_var.clear()
            i3_cache_var.clear()
            d3_cache_var.clear()
            z3_cache_var.clear()
            vlist_ <- []
            pvlist_ <- []
            header_ <- []
            modl_ <- []
            extn_ <- []
            slist_ <- []
            olist_ <- []
            funlist_ <- []
            arglist_ <- []
            int_string_format_ <- 8
            double_string_format_ <- (27,17)
            error_code_counter_ <- 1
            
        ///<summary>インデント設定に合わせた半角スペース列を生成</summary>
        member __.indent with get() = List.fold (fun acc i -> acc+" ") "" [1..(indentsize*indentposition)]
        
        ///<summary>現在のインデント設定に合わせて半角スペース列を取得</summary>
        member __.indent_s i = (List.fold (fun acc x -> acc+" ") "" [1..indentsize*(indentposition+i)]) 
        
        ///<summary>インデントを下げる</summary>
        member __.indentposition_inc() = indentposition <- indentposition + 1
        
        ///<summary>インデントを上げる</summary>
        member __.indentposition_dec() = indentposition <- indentposition - 1
        
        ///<summary>インデントを指定</summary>
        member __.indentposition_set(x) = indentposition <- x
    
        ///<summary>インデント数を取得</summary>
        member __.indentposition_ with get() = indentposition
        
        ///<summary>gotoラベル番号を増やす</summary>
        member __.goto_label_inc() = goto_label <- goto_label + 1
        
        ///<summary>整数型static変数名を取得</summary>
        member __.i_name() = 
            let name (i:int) = match lan with |H ->"<msub class=\""+"i_"+i.ToString()+"\"><mi>i</mi><mn>"+i.ToString()+"</mn></msub>" |_ -> "i"+i.ToString("000")
            i_stat_var.getvar(name)
            
        ///<summary>小数型static変数名を取得</summary>
        member __.d_name() = 
            let name (i:int) = match lan with |H ->"<msub class=\""+"d_"+i.ToString()+"\"><mi>d</mi><mn>"+i.ToString()+"</mn></msub>" |_ -> "d"+i.ToString("000")
            d_stat_var.getvar(name)
            
        ///<summary>複素数型static変数名を取得</summary>
        member __.z_name() = 
            let name (i:int) = match lan with |H ->"<msub class=\""+"z_"+i.ToString()+"\"><mi>z</mi><mn>"+i.ToString()+"</mn></msub>" |_ -> "z"+i.ToString("000")
            z_stat_var.getvar(name)
            
        ///<summary>文字列型static変数名を取得</summary>
        member __.t_name() = 
            t_stat_var.getvar(fun i -> "t"+i.ToString("000"))
            
        ///<summary>ファイルポインタstatic変数名を取得</summary>
        member __.f_number() = 
            f_stat_var.getvar(fun i -> "f"+i.ToString("000"))
            
        ///<summary>一時変数を生成</summary>
        member __.cachevar (e:Etype) (dim:int) =
            match lan with
              |H ->
                match e,dim with
                  |It _,0 -> 
                    i_cache_var.getvar(fun i -> "<msub class=\""+"i_c"+i.ToString()+"\"><mi>i</mi><mn>"+"c"+i.ToString()+"</mn></msub>")
                  |Dt,0 ->
                    d_cache_var.getvar(fun i -> "<msub class=\""+"d_c"+i.ToString()+"\"><mi>d</mi><mn>"+"c"+i.ToString()+"</mn></msub>")
                  |Zt,0 ->
                    z_cache_var.getvar(fun i -> "<msub class=\""+"z_c"+i.ToString()+"\"><mi>z</mi><mn>"+"c"+i.ToString()+"</mn></msub>")
                  |It _,1 -> 
                    i1_cache_var.getvar(fun i -> "<msub class=\""+"ai_c"+i.ToString()+"\"><mover><mi>i</mi><mo accent=\"true\">.</mo></mover><mn>"+"c"+i.ToString()+"</mn></msub>")
                  |Dt,1 ->
                    d1_cache_var.getvar(fun i -> "<msub class=\""+"ad_c"+i.ToString()+"\"><mover><mi>d</mi><mo accent=\"true\">.</mo></mover><mn>"+"c"+i.ToString()+"</mn></msub>")
                  |Zt,1 ->
                    z1_cache_var.getvar(fun i -> "<msub class=\""+"az_c"+i.ToString()+"\"><mover><mi>z</mi><mo accent=\"true\">.</mo></mover><mn>"+"c"+i.ToString()+"</mn></msub>")
                  |It _,2 -> 
                    i2_cache_var.getvar(fun i -> "<msub class=\""+"aai_c"+i.ToString()+"\"><mover><mi>i</mi><mo accent=\"true\">..</mo></mover><mn>"+"c"+i.ToString()+"</mn></msub>")
                  |Dt,2 ->
                    d2_cache_var.getvar(fun i -> "<msub class=\""+"aad_c"+i.ToString()+"\"><mover><mi>d</mi><mo accent=\"true\">..</mo></mover><mn>"+"c"+i.ToString()+"</mn></msub>")
                  |Zt,2 ->
                    z2_cache_var.getvar(fun i -> "<msub class=\""+"aaz_c"+i.ToString()+"\"><mover><mi>z</mi><mo accent=\"true\">..</mo></mover><mn>"+"c"+i.ToString()+"</mn></msub>")
                  |It _,3 ->
                    i3_cache_var.getvar(fun i -> "<msub class=\""+"aaai_c"+i.ToString()+"\"><mover><mi>i</mi><mo accent=\"true\">...</mo></mover><mn>"+"c"+i.ToString()+"</mn></msub>")
                  |Dt,3 ->
                    d3_cache_var.getvar(fun i -> "<msub class=\""+"aaad_c"+i.ToString()+"\"><mover><mi>d</mi><mo accent=\"true\">...</mo></mover><mn>"+"c"+i.ToString()+"</mn></msub>")
                  |Zt,3 ->
                    z3_cache_var.getvar(fun i -> "<msub class=\""+"aaaz_c"+i.ToString()+"\"><mover><mi>z</mi><mo accent=\"true\">...</mo></mover><mn>"+"c"+i.ToString()+"</mn></msub>")
                  |Structure _,_ ->
                    Console.WriteLine("構造体のcache変数は作成できません")
                    ignore <| Console.Read()
                    ""
                  |_,m when (m<0 || m>3) ->
                    Console.WriteLine("cache変数を生成できない次元の配列です")
                    ignore <| Console.Read()
                    ""
                  |_ ->
                    Console.WriteLine("cache変数を生成できません")
                    ignore <| Console.Read()
                    ""
              |_ ->
                match e,dim with
                  |It _,0 -> 
                    i_cache_var.getvar(fun i -> "i_cache_"+i.ToString("000"))
                  |Dt,0 ->
                    d_cache_var.getvar(fun i -> "d_cache_"+i.ToString("000"))
                  |Zt,0 ->
                    z_cache_var.getvar(fun i -> "z_cache_"+i.ToString("000"))
                  |It _,1 -> 
                    i1_cache_var.getvar(fun i -> "i1_cache_"+i.ToString("000"))
                  |Dt,1 ->
                    d1_cache_var.getvar(fun i -> "d1_cache_"+i.ToString("000"))
                  |Zt,1 ->
                    z1_cache_var.getvar(fun i -> "z1_cache_"+i.ToString("000"))
                  |It _,2 -> 
                    i2_cache_var.getvar(fun i -> "i2_cache_"+i.ToString("000"))
                  |Dt,2 ->
                    d2_cache_var.getvar(fun i -> "d2_cache_"+i.ToString("000"))
                  |Zt,2 ->
                    z2_cache_var.getvar(fun i -> "z2_cache_"+i.ToString("000"))
                  |It _,3 ->
                    i3_cache_var.getvar(fun i -> "i3_cache_"+i.ToString("000"))
                  |Dt,3 ->
                    d3_cache_var.getvar(fun i -> "d3_cache_"+i.ToString("000"))
                  |Zt,3 ->
                    z3_cache_var.getvar(fun i -> "z3_cache_"+i.ToString("000"))
                  |Structure _,_ ->
                    Console.WriteLine("構造体のcache変数は作成できません")
                    ignore <| Console.Read()
                    ""
                  |_,m when (m<0 || m>3) ->
                    Console.WriteLine("cache変数を生成できない次元の配列です")
                    ignore <| Console.Read()
                    ""
                  |_ ->
                    Console.WriteLine("cache変数を生成できません")
                    ignore <| Console.Read()
                    ""
                    
        ///<summary>自動生成した変数を削除</summary>
        member __.dispose (t:Etype,v:VarType,n:string) = 
            match t,v with
              |It _,A0 ->
                i_cache_var.dispose(n)
              |Dt,A0 ->
                d_cache_var.dispose(n)
              |Zt,A0 ->
                z_cache_var.dispose(n)
              |It _,A1(0) ->
                i1_cache_var.dispose(n)
              |Dt,A1(0) ->
                d1_cache_var.dispose(n)
              |Zt,A1(0) ->
                z1_cache_var.dispose(n)
              |It _,A2(0,0) ->
                i2_cache_var.dispose(n)
              |Dt,A2(0,0) ->
                d2_cache_var.dispose(n)
              |Zt,A2(0,0) ->
                z2_cache_var.dispose(n)
              |It _,A3(0,0,0) ->
                i3_cache_var.dispose(n)
              |Dt,A3(0,0,0) ->
                d3_cache_var.dispose(n)
              |Zt,A3(0,0,0) ->
                z3_cache_var.dispose(n)
              |_ -> ()
              
        ///<summary>エラーIDをインクリメント</summary>
        member __.error_code_counter_inc() = error_code_counter_ <- error_code_counter_ + 1
        
        ///<summary>関数の引数を追加</summary>
        member __.arglist_add(x) =
            arglist_ <- arglist_@[x]
        
        ///<summary>関数を追加</summary>
        member __.funlist_add(x) = 
            funlist_ <- x::funlist_
        
        ///<summary>コードを一時ファイルに書き込み</summary>
        member this.cwrite(code:string) =
            if this.isOmpUsed && this.parmode then
                pwriter.Write(code)
            else
                cwriter.Write(code)
            
        ///<summary>コードの一時ファイルを開く</summary>
        member __.copen() = 
            cwriter <- new StreamWriter(cfile,true)
            
        ///<summary>コードの一時ファイルを閉じる</summary>
        member __.cclose() = 
            cwriter.Close()
            
        ///<summary>変数宣言の一時ファイルに書き込み</summary>
        member this.vwrite(code:string) =
            vwriter.Write(this.indent+code)
            
        ///<summary>変数宣言の一時ファイルを開く</summary>
        member __.vopen() = 
            vwriter <- new StreamWriter(vfile,true)
            
        ///<summary>変数宣言の一時ファイルを閉じる</summary>
        member __.vclose() = 
            vwriter.Close()
            
        ///<summary>構造体・関数宣言の一時ファイルに書き込み</summary>
        member __.hwrite(code:string) =
            hwriter.Write(code)
            
        ///<summary>構造体・関数宣言の一時ファイルを開く</summary>
        member __.hopen() = 
            hwriter <- new StreamWriter(hfile,true)
            
        ///<summary>構造体・関数宣言の一時ファイルを閉じる</summary>
        member __.hclose() = 
            hwriter.Close()

        ///<summary>並列処理の一時ファイルに書き込み</summary>
        member __.pwrite(code:string) =
            pwriter.Write(code)

        ///<summary>並列処理の一時ファイルを開く</summary>
        member this.popen() =
            if File.Exists(pfile) then
                this.pclose()
                File.Delete(pfile)
            pwriter <- new StreamWriter(pfile,true)

        ///<summary>並列処理の一時ファイルを閉じる</summary>
        member __.pclose() = 
            pwriter.Close()

        ///<summary>並列処理の一時ファイルの内容</summary>
        member __.readpartext() = File.ReadAllText(pfile)
            
        ///<summary>変数のリストに変数情報を登録</summary>
        member this.vreg(typ,vtp,name,p) =
            let addv (typ,name_,vtp_,p_) =
                if name_<>"" then
                    //パラメータ変数で名前、定数値が一致するものが既にある場合は何もしない
                    if List.exists (fun (_,_,name,p) -> (p_<>"")&&(name_=name)&&(p_=p)) vlist_ then
                        ()
                    //それ以外で変数名が一致する場合は警告
                    elif List.exists (fun (_,_,name,_) -> name_=name) vlist_ then
                        Console.WriteLine("変数「"+name_+"」が複数定義されています")
                        Console.Read() |> ignore
                    //名前が一致するものがない場合はそのまま登録
                    else
                        vlist_<-vlist_@[(typ,vtp_,name_,p_)]
            match lan with 
              |F ->
                addv (typ,name,vtp,p)
                //配列の場合はサイズを保存する変数も登録
                match vtp with
                  |A1(0) -> 
                    addv (It(4),name+"_size",A1(1),"(/ -1 /)")
                  |A2(0,0) ->
                    addv (It(4),name+"_size",A1(2),"(/ -1,-1 /)")
                  |A3(0,0,0) ->
                    addv (It(4),name+"_size",A1(3),"(/ -1,-1,-1 /)")
                  |A1(s1) -> 
                    addv (It(4),name+"_size",A1(1),"(/"+(this.ItoS s1)+"/)")
                  |A2(s1,s2) -> 
                    addv (It(4),name+"_size",A1(2),"(/"+(this.ItoS s1)+","+(this.ItoS s2)+"/)")
                  |A3(s1,s2,s3) -> 
                    addv (It(4),name+"_size",A1(3),"(/"+(this.ItoS s1)+","+(this.ItoS s2)+","+(this.ItoS s3)+"/)")
                  |_ -> ()
              |C89 ->
                addv (typ,name,vtp,p)
                //配列の場合はサイズを保存する変数も登録
                match vtp with
                  |A1(0) -> 
                    addv (It(4),name+"_size",A1(1),"{-1}")
                  |A2(0,0) ->
                    addv (It(4),name+"_size",A1(2),"{-1,-1}")
                  |A3(0,0,0) ->
                    addv (It(4),name+"_size",A1(3),"{-1,-1,-1}")
                  |A1(s1) -> 
                    addv (It(4),name+"_size",A1(1),"{"+(this.ItoS s1)+"}")
                  |A2(s1,s2) -> 
                    addv (It(4),name+"_size",A1(2),"{"+(this.ItoS s1)+","+(this.ItoS s2)+"}")
                  |A3(s1,s2,s3) -> 
                    addv (It(4),name+"_size",A1(3),"{"+(this.ItoS s1)+","+(this.ItoS s2)+","+(this.ItoS s3)+"}")
                  |_ -> ()
              |C99 ->
                addv (typ,name,vtp,p)
                //配列の場合はサイズを保存する変数も登録
                match vtp with
                  |A1(0) -> 
                    addv (It(4),name+"_size",A1(1),"{-1}")
                  |A2(0,0) ->
                    addv (It(4),name+"_size",A1(2),"{-1,-1}")
                  |A3(0,0,0) ->
                    addv (It(4),name+"_size",A1(3),"{-1,-1,-1}")
                  |A1(s1) -> 
                    addv (It(4),name+"_size",A1(1),"{"+(this.ItoS s1)+"}")
                  |A2(s1,s2) -> 
                    addv (It(4),name+"_size",A1(2),"{"+(this.ItoS s1)+","+(this.ItoS s2)+"}")
                  |A3(s1,s2,s3) -> 
                    addv (It(4),name+"_size",A1(3),"{"+(this.ItoS s1)+","+(this.ItoS s2)+","+(this.ItoS s3)+"}")
                  |_ -> ()
              |T ->
                addv (typ,name,vtp,p)
                //配列の場合はサイズを保存する変数も登録
                match vtp with
                  |A1(0) -> 
                    addv (It(4),name+"_size",A1(1),"(/ -1 /)")
                  |A2(0,0) ->
                    addv (It(4),name+"_size",A1(2),"(/ -1,-1 /)")
                  |A3(0,0,0) ->
                    addv (It(4),name+"_size",A1(3),"(/ -1,-1,-1 /)")
                  |A1(s1) -> 
                    addv (It(4),name+"_size",A1(1),"(/"+(this.ItoS s1)+"/)")
                  |A2(s1,s2) -> 
                    addv (It(4),name+"_size",A1(2),"(/"+(this.ItoS s1)+","+(this.ItoS s2)+"/)")
                  |A3(s1,s2,s3) -> 
                    addv (It(4),name+"_size",A1(3),"(/"+(this.ItoS s1)+","+(this.ItoS s2)+","+(this.ItoS s3)+"/)")
                  |_ -> ()
              |H ->
                addv (typ,name,vtp,p)
                ////配列の場合はサイズを保存する変数も登録
                //match vtp with
                //  |A1(0) -> 
                //    addv (It,"<mi mathvariant=\"script\">S</mi><mo>&af;</mo><mo>[</mo>"+name+"<mo>]</mo>",A1(1),"[ -1 ]")
                //  |A2(0,0) ->
                //    addv (It,"<mi mathvariant=\"script\">S</mi><mo>&af;</mo><mo>[</mo>"+name+"<mo>]</mo>",A1(2),"[ -1,-1 ]")
                //  |A3(0,0,0) ->
                //    addv (It,"<mi mathvariant=\"script\">S</mi><mo>&af;</mo><mo>[</mo>"+name+"<mo>]</mo>",A1(3),"[ -1,-1,-1 ]")
                //  |A1(s1) -> 
                //    addv (It,"<mi mathvariant=\"script\">S</mi><mo>&af;</mo><mo>[</mo>"+name+"<mo>]</mo>",A1(1),"["+(this.ItoS s1)+"]")
                //  |A2(s1,s2) -> 
                //    addv (It,"<mi mathvariant=\"script\">S</mi><mo>&af;</mo><mo>[</mo>"+name+"<mo>]</mo>",A1(2),"["+(this.ItoS s1)+","+(this.ItoS s2)+"]")
                //  |A3(s1,s2,s3) -> 
                //    addv (It,"<mi mathvariant=\"script\">S</mi><mo>&af;</mo><mo>[</mo>"+name+"<mo>]</mo>",A1(3),"["+(this.ItoS s1)+","+(this.ItoS s2)+","+(this.ItoS s3)+"]")
                //  |_ -> ()
              |NL ->
                ()
                
        ///<summary>プライベート変数リストに変数名を登録</summary>
        member _.pvreg name =
            let rec findname i =
                if pvlist_.Item(i)<>name then
                    if (i+1)<pvlist_.Length then
                        findname (i+1)
                    else
                        pvlist_<-pvlist_@[name]
            if pvlist_.IsEmpty then
                pvlist_<-pvlist_@[name]
            else
                findname 0

        ///<summary>プライベート変数リストを初期化</summary>
        member _.clearpv() = pvlist_<-[]

        ///<summary>並列処理書き込みモードの切り替え</summary>
        member _.switch_parmode (tf:bool) = par_mode<-tf

        ///<summary>GPUに転送する変数のリストに変数名を追加</summary>
        member _.civreg name =
            let rec findname i =
                if copy_in_vlist_.Item(i)<>name then
                    if (i+1)<copy_in_vlist_.Length then
                        findname (i+1)
                    else
                        copy_in_vlist_<-copy_in_vlist_@[name]
            if copy_in_vlist_.IsEmpty then
                copy_in_vlist_<-copy_in_vlist_@[name]
            else
                findname 0

        ///<summary>GPUに転送する変数リストから変数を削除</summary>
        member _.rmciv (var:string) =
            copy_in_vlist_<-List.filter(fun s -> s<>var) copy_in_vlist_

        ///<summary>ホストに転送する変数のリストに変数名を追加</summary>
        member _.covreg name =
            let rec findname i =
                if copy_out_vlist_.Item(i)<>name then
                    if (i+1)<copy_out_vlist_.Length then
                        findname (i+1)
                    else
                        copy_out_vlist_<-copy_out_vlist_@[name]
            if copy_out_vlist_.IsEmpty then
                copy_out_vlist_<-copy_out_vlist_@[name]
            else
                findname 0

        ///<summary>ホストに転送する変数リストから変数を削除</summary>
        member _.rmcov (var:string) =
            copy_out_vlist_<-List.filter(fun s -> s<>var) copy_out_vlist_
                
        ///<summary>ファイルポインタcache変数を生成し、code内の処理を実行</summary>
        member this.fcache code = 
            let name = this.f_number()
            match lan with
              |F -> this.vreg(Structure("file"),A0,name,(this.ItoS <| (f_stat_var.counter+10)))
              |_ -> this.vreg(Structure("file"),A0,name,"")
            code name
            
        ///<summary>文字列cache変数を生成し、code内の処理を実行</summary>
        member this.tcache vt code = 
            let name = this.t_name()
            this.vreg(Structure("string"),vt,name,"")
            code name
            
        ///<summary>コードsとコメントcmを指定文字数spで改行してwriterに出力</summary>
        member this.codefold (s:string,cm:string,writer:string->unit,sp:int) =
            //文字列に空白文字しか含まれていなければtrue
            let isallspace (code:string) =
                let mutable f=true
                for s in code do if s<>' ' then f<-false else ()
                f
            let slist = s.Split([|'\n'|],StringSplitOptions.RemoveEmptyEntries) //改行文字で分割
            for str in slist do
                if str <> "" then
                    let rec wt (code:string) =
                        if code.Length>sp then 
                            //何文字目で改行するか判断
                            let instring (s:string) = 
                                let m = Array.fold (fun acc (x:char) -> if x='\"' then acc+1 else acc) 0 (s.ToCharArray())
                                m%2=1
                            let sp2 = 
                                let rec count c =
                                    if code.[c-1]=' ' || code.[c-1]=',' || code.[c-1]=')' || code.[c-1]='(' || code.[c-1]='+' || (code.[c-1]='*' && (c-2<0 || code.[c-2]<>'*') && (c>=code.Length || code.[c]<>'*')) || (code.[c-1]='/' && code.[c]<>')' && code.[c]<>'=') then 
                                        c
                                    else 
                                        count (c-1)
                                if instring (code.Substring(0,sp)) then sp else count sp
                            if isallspace(code.Substring(sp2)) then
                                //残りが空白文字だけなのでこの行で終わり
                                writer(code.Substring(0,sp2)+cm+"\n")
                            else
                                //まだコードが続くので継続文字を入れて改行
                                if instring (code.Substring(0,sp2)) then
                                    writer(code.Substring(0,sp2)+"\" &"+"\n")
                                    wt (this.indent_s(1)+"//\""+code.Substring(sp2))
                                else
                                    writer(code.Substring(0,sp2)+" &"+"\n")
                                    wt (this.indent_s(1)+code.Substring(sp2))
                        else
                            writer(code+cm+"\n")
                    wt (this.indent+str)
                    
        ///<summary>コード出力</summary>
        member this.codewrite (ss:string) = 
            match lan with
              |F ->
                //コメントが含まれていれば分割
                let (s,cm) =
                    match ss.IndexOf('!') with
                      | -1 -> (ss,"")
                      | m  -> (ss.Substring(0,m),(ss.Substring(m)).Replace("\n",""))
                match s,cm with
                  |"","" -> ()
                  |"",_ -> this.cwrite(this.indent+cm+"\n")
                  |_ ->
                    this.codefold(s,cm,this.cwrite,100)
              |C89 ->
                if ss<>"" then 
                    let slist = ss.Split([|'\n'|],StringSplitOptions.RemoveEmptyEntries) //改行文字で分割
                    Array.iter (fun code -> this.cwrite(this.indent+code+"\n")) slist
              |C99 ->
                if ss<>"" then 
                    let slist = ss.Split([|'\n'|],StringSplitOptions.RemoveEmptyEntries) //改行文字で分割
                    Array.iter (fun code -> this.cwrite(this.indent+code+"\n")) slist
              |T ->
                if ss<>"" then 
                    let slist = ss.Split([|'\n'|],StringSplitOptions.RemoveEmptyEntries) //改行文字で分割
                    Array.iter (fun code -> this.cwrite((this.indent+code+"\n").Replace("_","\\_"))) slist
              |H ->
                if ss<>"" then 
                    let slist = ss.Split([|'\n'|],StringSplitOptions.RemoveEmptyEntries) //改行文字で分割
                    Array.iter (fun code -> this.cwrite(this.indent+code+"\n")) slist
              |NL ->
                ()
                    
        ///<summary>変数の型名を文字列に変換</summary>
        member __.Stype typ = 
            match lan with
              |F ->
                match typ with 
                  |It 1 -> "integer(1)" 
                  |It _ -> "integer" 
                  |Dt -> "double precision" 
                  |Zt -> "complex(kind(0d0))" 
                  |Structure("string") -> "character(100)" 
                  |Structure("integer(1)") -> "integer(1)" 
                  |Structure("file") -> "integer"
                  |Structure(sname) -> "type("+sname+")"
                  |_ -> ""
              |C89 ->
                match typ with 
                  |It 1 -> "unsigned char" 
                  |It _ -> "int" 
                  |Dt -> "double" 
                  |Zt -> "doublecomplex"
                  |Structure("string") -> "string" 
                  |Structure("char") -> "char" 
                  |Structure("file") -> "FILE*" 
                  |Structure(sname) -> sname 
                  |_ -> ""
              |C99 ->
                match typ with 
                  |It 1 -> "unsigned char" 
                  |It _ -> "int" 
                  |Dt -> "double" 
                  |Zt -> "double complex"
                  |Structure("string") -> "string" 
                  |Structure("char") -> "char" 
                  |Structure("file") -> "FILE*" 
                  |Structure(sname) -> sname 
                  |_ -> ""
              |T ->
                match typ with 
                  |It 1 -> "byte" 
                  |It _ -> "int" 
                  |Dt -> "double" 
                  |Zt -> "complex"
                  |Structure("string") -> "char" 
                  |Structure("char") -> "char" 
                  |Structure(sname) -> sname 
                  |_ -> ""
              |H ->
                match typ with 
                  |It 1 -> "byte" 
                  |It _ -> "int" 
                  |Dt -> "double" 
                  |Zt -> "complex"
                  |Structure("string") -> "char" 
                  |Structure("char") -> "char" 
                  |Structure(sname) -> sname 
                  |_ -> ""
              |NL ->
                match typ with 
                  |It 1 -> "byte" 
                  |It _ -> "int" 
                  |Dt -> "double" 
                  |Zt -> "complex"
                  |Structure("string") -> "char" 
                  |Structure("char") -> "char" 
                  |Structure(sname) -> sname 
                  |_ -> ""
                  
        ///<summary>変数宣言のコード</summary>
        member this.declare (typ:Etype,vtp:VarType,name:string,param:string) =
            match lan with
              |F ->
                match vtp with 
                  |A0                    -> (this.Stype typ)+" :: "+name+(if param<>"" then "="+param else "")
                  |A1(0)                 -> (this.Stype typ)+",allocatable"+" :: "+name+"(:)"+(if param<>"" then "="+param else "")
                  |A2(0,0)               -> (this.Stype typ)+",allocatable"+" :: "+name+"(:,:)"+(if param<>"" then "="+param else "")
                  |A3(0,0,0)             -> (this.Stype typ)+",allocatable"+" :: "+name+"(:,:,:)"+(if param<>"" then "="+param else "")
                  |A1(size1)             -> (this.Stype typ)+" :: "+name+"(1:"+(this.ItoS size1)+")"+(if param<>"" then "="+param else "")
                  |A2(size1,size2)       -> (this.Stype typ)+" :: "+name+"(1:"+(this.ItoS size1)+",1:"+(this.ItoS size2)+")"+(if param<>"" then "="+param else "")
                  |A3(size1,size2,size3) -> (this.Stype typ)+" :: "+name+"(1:"+(this.ItoS size1)+",1:"+(this.ItoS size2)+",1:"+(this.ItoS size3)+")"+(if param<>"" then "="+param else "")
              |C89 ->
                match vtp,(this.Stype typ) with 
                  |A0,"string"              -> "char"+" "+name+"[100]" + ";"
                  |A0,st                    -> st+" "+name+(if param<>"" then "="+param else "") + ";"
                  |A1(0),st                 -> st+" *"+name+(if param<>"" then "="+param else "") + ";"
                  |A2(0,0),st               -> st+" *"+name+(if param<>"" then "="+param else "") + ";"
                  |A3(0,0,0),st             -> st+" *"+name+(if param<>"" then "="+param else "") + ";"
                  |A1(size1),st             -> st+" "+name+"["+(this.ItoS size1)+"]"+(if param<>"" then "="+param else "") + ";"
                  |A2(size1,size2),st       -> st+" "+name+"["+(this.ItoS (size1*size2))+"]"+(if param<>"" then "="+param else "") + ";"
                  |A3(size1,size2,size3),st -> st+" "+name+"["+(this.ItoS (size1*size2*size3))+"]"+(if param<>"" then "="+param else "") + ";"
              |C99 ->
                match vtp,(this.Stype typ) with 
                  |A0,"string"              -> "char"+" "+name+"[100]" + ";"
                  |A0,st                    -> st+" "+name+(if param<>"" then "="+param else "") + ";"
                  |A1(0),st                 -> st+" *"+name+(if param<>"" then "="+param else "") + ";"
                  |A2(0,0),st               -> st+" *"+name+(if param<>"" then "="+param else "") + ";"
                  |A3(0,0,0),st             -> st+" *"+name+(if param<>"" then "="+param else "") + ";"
                  |A1(size1),st             -> st+" "+name+"["+(this.ItoS size1)+"]"+(if param<>"" then "="+param else "") + ";"
                  |A2(size1,size2),st       -> st+" "+name+"["+(this.ItoS (size1*size2))+"]"+(if param<>"" then "="+param else "") + ";"
                  |A3(size1,size2,size3),st -> st+" "+name+"["+(this.ItoS (size1*size2*size3))+"]"+(if param<>"" then "="+param else "") + ";"
              |T ->
                match vtp with 
                  |A0                    -> "\\item "+(this.Stype typ)+" $"+name+"$"+(if param<>"" then "="+param else "")
                  |A1(0)                 -> "\\item "+(this.Stype typ)+" (allocatable)"+" $"+name+"$ (:)"+(if param<>"" then "="+param else "")
                  |A2(0,0)               -> "\\item "+(this.Stype typ)+" (allocatable)"+" $"+name+"$ (:,:)"+(if param<>"" then "="+param else "")
                  |A3(0,0,0)             -> "\\item "+(this.Stype typ)+" (allocatable)"+" $"+name+"$ (:,:,:)"+(if param<>"" then "="+param else "")
                  |A1(size1)             -> "\\item "+(this.Stype typ)+" $"+name+"$ ("+(this.ItoS size1)+")"+(if param<>"" then "="+param else "")
                  |A2(size1,size2)       -> "\\item "+(this.Stype typ)+" $"+name+"$ ("+(this.ItoS size1)+","+(this.ItoS size2)+")"+(if param<>"" then "="+param else "")
                  |A3(size1,size2,size3) -> "\\item "+(this.Stype typ)+" $"+name+"$ ("+(this.ItoS size1)+","+(this.ItoS size2)+","+(this.ItoS size3)+")"+(if param<>"" then "="+param else "")
              |H ->
                match vtp with 
                  |A0                    -> "\t\t\t<li>"+(this.Stype typ)+"<math><mspace width=\"1em\" />"+name+""+(if param<>"" then "="+param else "")+"</math></li>"
                  |A1(0)                 -> "\t\t\t<li>"+(this.Stype typ)+"<math><mspace width=\"1em\" /><mi>(allocatable)</mi>"+" "+name+" <mo>[</mo><mo>:</mo><mo>]</mo>"+(if param<>"" then "="+param else "")+"</math></li>"
                  |A2(0,0)               -> "\t\t\t<li>"+(this.Stype typ)+"<math><mspace width=\"1em\" /><mi>(allocatable)</mi>"+" "+name+" <mo>[</mo><mo>:</mo><mo>,</mo><mo>:</mo><mo>]</mo>"+(if param<>"" then "="+param else "")+"</math></li>"
                  |A3(0,0,0)             -> "\t\t\t<li>"+(this.Stype typ)+"<math><mspace width=\"1em\" /><mi>(allocatable)</mi>"+" "+name+" <mo>[</mo><mo>:</mo><mo>,</mo><mo>:</mo><mo>,</mo><mo>:</mo><mo>]</mo>"+(if param<>"" then "<mo>=</mo>"+param else "")+"</math></li>"
                  |A1(size1)             -> "\t\t\t<li>"+(this.Stype typ)+"<math><mspace width=\"1em\" />"+name+" ("+(this.ItoS size1)+")"+(if param<>"" then "="+param else "")+"</math></li>"
                  |A2(size1,size2)       -> "\t\t\t<li>"+(this.Stype typ)+"<math><mspace width=\"1em\" />"+name+" ("+(this.ItoS size1)+","+(this.ItoS size2)+")"+(if param<>"" then "="+param else "")+"</math></li>"
                  |A3(size1,size2,size3) -> "\t\t\t<li>"+(this.Stype typ)+"<math><mspace width=\"1em\" />"+name+" ("+(this.ItoS size1)+","+(this.ItoS size2)+","+(this.ItoS size3)+")"+(if param<>"" then "="+param else "")+"</math></li>"
              |NL ->
                ""
                  
        ///<summary>宣言されたすべての変数を一時ファイルに書き込み</summary>
        member this.declareall() =
            match lan with
              |F |T ->
                for v in vlist_ do
                    this.codefold(this.declare(v)+"\n","",this.vwrite,100)
                    
                for i in 1..loopvar.maxcounter do this.vwrite(this.declare(It 4,A0,"ic"+i.ToString("000"),"")+"\n")
                
                for i in 1..i_temp_var.maxcounter do this.vwrite(this.declare(It 4,A0,"i_temp_"+i.ToString("000"),"")+"\n")
                for i in 1..d_temp_var.maxcounter do this.vwrite(this.declare(Dt,A0,"d_temp_"+i.ToString("000"),"")+"\n")
                for i in 1..z_temp_var.maxcounter do this.vwrite(this.declare(Zt,A0,"z_temp_"+i.ToString("000"),"")+"\n")
                
                for i in 1..i_cache_var.maxcounter do this.vwrite(this.declare(It 4,A0,"i_cache_"+i.ToString("000"),"")+"\n")
                for i in 1..d_cache_var.maxcounter do this.vwrite(this.declare(Dt,A0,"d_cache_"+i.ToString("000"),"")+"\n")
                for i in 1..z_cache_var.maxcounter do this.vwrite(this.declare(Zt,A0,"z_cache_"+i.ToString("000"),"")+"\n")
                
                for i in 1..i1_cache_var.maxcounter do 
                    this.vwrite(this.declare(It 4,A1(0),"i1_cache_"+i.ToString("000"),"")+"\n")
                    this.vwrite(this.declare(It 4,A1(1),"i1_cache_"+i.ToString("000")+"_size","(/ -1 /)")+"\n")
                    
                for i in 1..d1_cache_var.maxcounter do 
                    this.vwrite(this.declare(Dt,A1(0),"d1_cache_"+i.ToString("000"),"")+"\n")
                    this.vwrite(this.declare(It 4,A1(1),"d1_cache_"+i.ToString("000")+"_size","(/ -1 /)")+"\n")
                    
                for i in 1..z1_cache_var.maxcounter do 
                    this.vwrite(this.declare(Zt,A1(0),"z1_cache_"+i.ToString("000"),"")+"\n")
                    this.vwrite(this.declare(It 4,A1(1),"z1_cache_"+i.ToString("000")+"_size","(/ -1 /)")+"\n")
                    
                for i in 1..i2_cache_var.maxcounter do 
                    this.vwrite(this.declare(It 4,A2(0,0),"i2_cache_"+i.ToString("000"),"")+"\n")
                    this.vwrite(this.declare(It 4,A1(2),"i2_cache_"+i.ToString("000")+"_size","(/ -1,-1 /)")+"\n")
                    
                for i in 1..d2_cache_var.maxcounter do 
                    this.vwrite(this.declare(Dt,A2(0,0),"d2_cache_"+i.ToString("000"),"")+"\n")
                    this.vwrite(this.declare(It 4,A1(2),"d2_cache_"+i.ToString("000")+"_size","(/ -1,-1 /)")+"\n")
                    
                for i in 1..z2_cache_var.maxcounter do 
                    this.vwrite(this.declare(Zt,A2(0,0),"z2_cache_"+i.ToString("000"),"")+"\n")
                    this.vwrite(this.declare(It 4,A1(2),"z2_cache_"+i.ToString("000")+"_size","(/ -1,-1 /)")+"\n")
                    
                for i in 1..i3_cache_var.maxcounter do 
                    this.vwrite(this.declare(It 4,A3(0,0,0),"i3_cache_"+i.ToString("000"),"")+"\n")
                    this.vwrite(this.declare(It 4,A1(3),"i3_cache_"+i.ToString("000")+"_size","(/ -1,-1,-1 /)")+"\n")
                    
                for i in 1..d3_cache_var.maxcounter do 
                    this.vwrite(this.declare(Dt,A3(0,0,0),"d3_cache_"+i.ToString("000"),"")+"\n")
                    this.vwrite(this.declare(It 4,A1(3),"d3_cache_"+i.ToString("000")+"_size","(/ -1,-1,-1 /)")+"\n")
                    
                for i in 1..z3_cache_var.maxcounter do 
                    this.vwrite(this.declare(Zt,A3(0,0,0),"z3_cache_"+i.ToString("000"),"")+"\n")
                    this.vwrite(this.declare(It 4,A1(3),"z3_cache_"+i.ToString("000")+"_size","(/ -1,-1,-1 /)")+"\n")
              |C89 |C99 ->
                
                for v in vlist_ do this.vwrite(this.declare(v)+"\n")
                
                for i in 1..loopvar.maxcounter do this.vwrite(this.declare(It 4,A0,"ic"+i.ToString("000"),"")+"\n")
                
                for i in 1..i_temp_var.maxcounter do this.vwrite(this.declare(It 4,A0,"i_temp_"+i.ToString("000"),"")+"\n")
                for i in 1..d_temp_var.maxcounter do this.vwrite(this.declare(Dt,A0,"d_temp_"+i.ToString("000"),"")+"\n")
                for i in 1..z_temp_var.maxcounter do this.vwrite(this.declare(Zt,A0,"z_temp_"+i.ToString("000"),"")+"\n")
                
                for i in 1..i_cache_var.maxcounter do this.vwrite(this.declare(It 4,A0,"i_cache_"+i.ToString("000"),"")+"\n")
                for i in 1..d_cache_var.maxcounter do this.vwrite(this.declare(Dt,A0,"d_cache_"+i.ToString("000"),"")+"\n")
                for i in 1..z_cache_var.maxcounter do this.vwrite(this.declare(Zt,A0,"z_cache_"+i.ToString("000"),"")+"\n")
                
                for i in 1..i1_cache_var.maxcounter do 
                    this.vwrite(this.declare(It 4,A1(0),"i1_cache_"+i.ToString("000"),"")+"\n")
                    this.vwrite(this.declare(It 4,A1(1),"i1_cache_"+i.ToString("000")+"_size","{-1}")+"\n")
                    
                for i in 1..d1_cache_var.maxcounter do 
                    this.vwrite(this.declare(Dt,A1(0),"d1_cache_"+i.ToString("000"),"")+"\n")
                    this.vwrite(this.declare(It 4,A1(1),"d1_cache_"+i.ToString("000")+"_size","{-1}")+"\n")
                    
                for i in 1..z1_cache_var.maxcounter do 
                    this.vwrite(this.declare(Zt,A1(0),"z1_cache_"+i.ToString("000"),"")+"\n")
                    this.vwrite(this.declare(It 4,A1(1),"z1_cache_"+i.ToString("000")+"_size","{-1}")+"\n")
                    
                for i in 1..i2_cache_var.maxcounter do 
                    this.vwrite(this.declare(It 4,A2(0,0),"i2_cache_"+i.ToString("000"),"")+"\n")
                    this.vwrite(this.declare(It 4,A1(2),"i2_cache_"+i.ToString("000")+"_size","{-1,-1}")+"\n")
                    
                for i in 1..d2_cache_var.maxcounter do 
                    this.vwrite(this.declare(Dt,A2(0,0),"d2_cache_"+i.ToString("000"),"")+"\n")
                    this.vwrite(this.declare(It 4,A1(2),"d2_cache_"+i.ToString("000")+"_size","{-1,-1}")+"\n")
                    
                for i in 1..z2_cache_var.maxcounter do 
                    this.vwrite(this.declare(Zt,A2(0,0),"z2_cache_"+i.ToString("000"),"")+"\n")
                    this.vwrite(this.declare(It 4,A1(2),"z2_cache_"+i.ToString("000")+"_size","{-1,-1}")+"\n")
                    
                for i in 1..i3_cache_var.maxcounter do 
                    this.vwrite(this.declare(It 4,A3(0,0,0),"i3_cache_"+i.ToString("000"),"")+"\n")
                    this.vwrite(this.declare(It 4,A1(3),"i3_cache_"+i.ToString("000")+"_size","{-1,-1,-1}")+"\n")
                    
                for i in 1..d3_cache_var.maxcounter do 
                    this.vwrite(this.declare(Dt,A3(0,0,0),"d3_cache_"+i.ToString("000"),"")+"\n")
                    this.vwrite(this.declare(It 4,A1(3),"d3_cache_"+i.ToString("000")+"_size","{-1,-1,-1}")+"\n")
                    
                for i in 1..z3_cache_var.maxcounter do 
                    this.vwrite(this.declare(Zt,A3(0,0,0),"z3_cache_"+i.ToString("000"),"")+"\n")
                    this.vwrite(this.declare(It 4,A1(3),"z3_cache_"+i.ToString("000")+"_size","{-1,-1,-1}")+"\n")
              |H ->
                
                for v in vlist_ do this.vwrite(this.declare(v)+"\n")
                
                this.vwrite("\t\t\t<li>Loop counter: <math><msub><mi>n</mi><mi>m</mi></msub><mspace width=\"1em\"/><mo>(</mo><mi>m</mi><mo>=</mo><mn>1</mn>"+(if loopvar.maxcounter=1 then "" else "<mo>..</mo><mn>"+loopvar.maxcounter.ToString()+"</mn>")+"<mo>)</mo></math></li>"+"\n")
                
                if i_temp_var.maxcounter>0 then
                    this.vwrite("\t\t\t<li>Temporary variables (integer): <math><msub><msup><mi>i</mi><mo>'</mo></msup><mi>m</mi></msub><mspace width=\"1em\"/><mo>(</mo><mi>m</mi><mo>=</mo><mn>1</mn>"+(if i_temp_var.maxcounter=1 then "" else "<mo>..</mo><mn>"+i_temp_var.maxcounter.ToString()+"</mn>")+"<mo>)</mo></math></li>"+"\n")
                if d_temp_var.maxcounter>0 then
                    this.vwrite("\t\t\t<li>Temporary variables (double): <math><msub><msup><mi>d</mi><mo>'</mo></msup><mi>m</mi></msub><mspace width=\"1em\"/><mo>(</mo><mi>m</mi><mo>=</mo><mn>1</mn>"+(if d_temp_var.maxcounter=1 then "" else "<mo>..</mo><mn>"+d_temp_var.maxcounter.ToString()+"</mn>")+"<mo>)</mo></math></li>"+"\n")
                if z_temp_var.maxcounter>0 then
                    this.vwrite("\t\t\t<li>Temporary variables (complex): <math><msub><msup><mi>z</mi><mo>'</mo></msup><mi>m</mi></msub><mspace width=\"1em\"/><mo>(</mo><mi>m</mi><mo>=</mo><mn>1</mn>"+(if z_temp_var.maxcounter=1 then "" else "<mo>..</mo><mn>"+z_temp_var.maxcounter.ToString()+"</mn>")+"<mo>)</mo></math></li>"+"\n")
                    
                if i_cache_var.maxcounter>0 then
                    this.vwrite("\t\t\t<li>Cache variables (integer): <math><msub><mi>i</mi><mi>cm</mi></msub><mspace width=\"1em\"/><mo>(</mo><mi>m</mi><mo>=</mo><mn>1</mn>"+(if i_cache_var.maxcounter=1 then "" else "<mo>..</mo><mn>"+i_cache_var.maxcounter.ToString()+"</mn>")+"<mo>)</mo></math></li>"+"\n")
                if d_cache_var.maxcounter>0 then
                    this.vwrite("\t\t\t<li>Cache variables (double): <math><msub><mi>d</mi><mi>cm</mi></msub><mspace width=\"1em\"/><mo>(</mo><mi>m</mi><mo>=</mo><mn>1</mn>"+(if d_cache_var.maxcounter=1 then "" else "<mo>..</mo><mn>"+d_cache_var.maxcounter.ToString()+"</mn>")+"<mo>)</mo></math></li>"+"\n")
                if z_cache_var.maxcounter>0 then
                    this.vwrite("\t\t\t<li>Cache variables (complex): <math><msub><mi>z</mi><mi>cm</mi></msub><mspace width=\"1em\"/><mo>(</mo><mi>m</mi><mo>=</mo><mn>1</mn>"+(if z_cache_var.maxcounter=1 then "" else "<mo>..</mo><mn>"+z_cache_var.maxcounter.ToString()+"</mn>")+"<mo>)</mo></math></li>"+"\n")
                    
                if i1_cache_var.maxcounter>0 then
                    this.vwrite("\t\t\t<li>Cache array (integer,1d): <math><msub><mover><mi>i</mi><mo>.</mo></mover><mi>cm</mi></msub><mspace width=\"1em\"/><mo>(</mo><mi>m</mi><mo>=</mo><mn>1</mn>"+(if i1_cache_var.maxcounter=1 then "" else "<mo>..</mo><mn>"+i1_cache_var.maxcounter.ToString()+"</mn>")+"<mo>)</mo></math></li>"+"\n")
                if d1_cache_var.maxcounter>0 then
                    this.vwrite("\t\t\t<li>Cache array (double,1d): <math><msub><mover><mi>d</mi><mo>.</mo></mover><mi>cm</mi></msub><mspace width=\"1em\"/><mo>(</mo><mi>m</mi><mo>=</mo><mn>1</mn>"+(if d1_cache_var.maxcounter=1 then "" else "<mo>..</mo><mn>"+d1_cache_var.maxcounter.ToString()+"</mn>")+"<mo>)</mo></math></li>"+"\n")
                if z1_cache_var.maxcounter>0 then
                    this.vwrite("\t\t\t<li>Cache array (complex,1d): <math><msub><mover><mi>z</mi><mo>.</mo></mover><mi>cm</mi></msub><mspace width=\"1em\"/><mo>(</mo><mi>m</mi><mo>=</mo><mn>1</mn>"+(if z1_cache_var.maxcounter=1 then "" else "<mo>..</mo><mn>"+z1_cache_var.maxcounter.ToString()+"</mn>")+"<mo>)</mo></math></li>"+"\n")

                if i2_cache_var.maxcounter>0 then
                    this.vwrite("\t\t\t<li>Cache array (integer,2d): <math><msub><mover><mi>i</mi><mo>..</mo></mover><mi>cm</mi></msub><mspace width=\"1em\"/><mo>(</mo><mi>m</mi><mo>=</mo><mn>1</mn>"+(if i2_cache_var.maxcounter=1 then "" else "<mo>..</mo><mn>"+i2_cache_var.maxcounter.ToString()+"</mn>")+"<mo>)</mo></math></li>"+"\n")
                if d2_cache_var.maxcounter>0 then
                    this.vwrite("\t\t\t<li>Cache array (double,2d): <math><msub><mover><mi>d</mi><mo>..</mo></mover><mi>cm</mi></msub><mspace width=\"1em\"/><mo>(</mo><mi>m</mi><mo>=</mo><mn>1</mn>"+(if d2_cache_var.maxcounter=1 then "" else "<mo>..</mo><mn>"+d2_cache_var.maxcounter.ToString()+"</mn>")+"<mo>)</mo></math></li>"+"\n")
                if z2_cache_var.maxcounter>0 then
                    this.vwrite("\t\t\t<li>Cache array (complex,2d): <math><msub><mover><mi>z</mi><mo>..</mo></mover><mi>cm</mi></msub><mspace width=\"1em\"/><mo>(</mo><mi>m</mi><mo>=</mo><mn>1</mn>"+(if z2_cache_var.maxcounter=1 then "" else "<mo>..</mo><mn>"+z2_cache_var.maxcounter.ToString()+"</mn>")+"<mo>)</mo></math></li>"+"\n")

                if i3_cache_var.maxcounter>0 then
                    this.vwrite("\t\t\t<li>Cache array (integer,3d): <math><msub><mover><mi>i</mi><mo>...</mo></mover><mi>cm</mi></msub><mspace width=\"1em\"/><mo>(</mo><mi>m</mi><mo>=</mo><mn>1</mn>"+(if i3_cache_var.maxcounter=1 then "" else "<mo>..</mo><mn>"+i3_cache_var.maxcounter.ToString()+"</mn>")+"<mo>)</mo></math></li>"+"\n")
                if d3_cache_var.maxcounter>0 then
                    this.vwrite("\t\t\t<li>Cache array (double,3d): <math><msub><mover><mi>d</mi><mo>...</mo></mover><mi>cm</mi></msub><mspace width=\"1em\"/><mo>(</mo><mi>m</mi><mo>=</mo><mn>1</mn>"+(if d3_cache_var.maxcounter=1 then "" else "<mo>..</mo><mn>"+d3_cache_var.maxcounter.ToString()+"</mn>")+"<mo>)</mo></math></li>"+"\n")
                if z3_cache_var.maxcounter>0 then
                    this.vwrite("\t\t\t<li>Cache array (complex,3d): <math><msub><mover><mi>z</mi><mo>...</mo></mover><mi>cm</mi></msub><mspace width=\"1em\"/><mo>(</mo><mi>m</mi><mo>=</mo><mn>1</mn>"+(if z3_cache_var.maxcounter=1 then "" else "<mo>..</mo><mn>"+z3_cache_var.maxcounter.ToString()+"</mn>")+"<mo>)</mo></math></li>"+"\n")
              |NL ->
                ()
                    
        ///<summary>コメント文</summary>
        member this.comment s = 
            match lan with
              |F ->
                let comment_line str = this.codewrite("!"+str+"\n")
                let rec arrange (str:string) =
                  if str.Length>80 then 
                    comment_line <| str.Substring(0,80)
                    arrange (str.Substring(80,str.Length-80))
                  else
                    comment_line str
                arrange s
              |C89 ->
                let comment_line str = this.codewrite("/* "+str+" */\n")
                let rec arrange (str:string) =
                    if str.Length>80 then 
                        comment_line <| str.Substring(0,80)
                        arrange (str.Substring(80,str.Length-80))
                    else
                        comment_line str
                arrange s
              |C99 ->
                let comment_line str = this.codewrite("/* "+str+" */\n")
                let rec arrange (str:string) =
                    if str.Length>80 then 
                        comment_line <| str.Substring(0,80)
                        arrange (str.Substring(80,str.Length-80))
                    else
                        comment_line str
                arrange s
              |T ->
                let comment_line str = this.codewrite(str+"\n")
                let rec arrange (str:string) =
                    if str.Length>80 then 
                        comment_line <| str.Substring(0,80)
                        arrange (str.Substring(80,str.Length-80))
                    else
                        comment_line str
                arrange s
              |H ->
                let comment_line (str:string) = this.codewrite("<span class=\"comment\">"+(if str.Contains("<mi>") then "<math>"+str+"</math>" else str)+"</span>\n<br/>\n")
                let rec arrange (str:string) =
                    if str.Length>80 then 
                        comment_line <| str.Substring(0,80)
                        arrange (str.Substring(80,str.Length-80))
                    else
                        comment_line str
                arrange s
              |NL ->
                ()
    
        ///<summary>一時変数を取得</summary>
        member __.getvar(t:Etype) =
            match t with
              |It _ ->
                let c = i_temp_var.getvar(fun i -> "i_temp_"+i.ToString("000"))
                (It 4,c,[])
              |Dt ->
                let c = d_temp_var.getvar(fun i -> "d_temp_"+i.ToString("000"))
                (Dt,c,[])
              |Zt ->
                let c = z_temp_var.getvar(fun i -> "z_temp_"+i.ToString("000"))
                (Zt,c,[])
              |_ ->
                (Nt,"",[])
                
        ///<summary>不要になった一時変数を返却</summary>
        member __.dispose(t:Etype,n:string) =
            match t with
              |It _ ->
                i_temp_var.dispose(n)
              |Dt ->
                d_temp_var.dispose(n)
              |Zt ->
                z_temp_var.dispose(n)
              |_ -> ()
              
        /// <summary>
        /// 関数定義の引数を追加
        /// </summary>
        /// <param name="typ">変数の型</param>
        /// <param name="vtp">変数の次元</param>
        /// <param name="n">変数名</param>
        /// <param name="comment">変数の説明</param>
        member this.addarg (typ:Etype,vtp:VarType,n:string,comment:string) =
            fun code ->
                match lan with
                  |F |T ->
                    //関数内ではこの変数名を使用
                    let name = 
                        match typ,vtp with
                          |(It _|Dt|Zt|Structure _),A0 ->
                            "arg"+(arglist_.Length+1).ToString("00")
                          |_ -> 
                            "arg"+(arglist_.Length+1).ToString("00")
                    match vtp with
                      |A0 ->
                        this.arglist_add(n,(typ,vtp,name,"",comment))
                      |A1 _ ->
                        this.arglist_add(n,(typ,vtp,name,"",comment))
                        this.arglist_add(n+"_size",(It 4,A1(1),name+"_size","",""))
                      |A2 _ ->
                        this.arglist_add(n,(typ,vtp,name,"",comment))
                        this.arglist_add(n+"_size",(It 4,A1(2),name+"_size","",""))
                      |A3 _ ->
                        this.arglist_add(n,(typ,vtp,name,"",comment))
                        this.arglist_add(n+"_size",(It 4,A1(3),name+"_size","",""))
                    let argname =
                        match typ,vtp with
                          |(It _|Dt|Zt|Structure _),A0 -> name
                          |_ -> name
                    code(typ,vtp,argname)
                  |C89 |C99 ->
                    //関数内ではこの変数名を使用
                    let name = 
                        match typ,vtp with
                          |(It _|Dt|Zt|Structure _),A0 ->
                            "arg"+(arglist_.Length+1).ToString("00")
                          |_ -> 
                            "arg"+(arglist_.Length+1).ToString("00")
                    match vtp with
                      |A0 ->
                        this.arglist_add(n,(typ,vtp,name,"",comment))
                      |A1 _ ->
                        this.arglist_add(n,(typ,vtp,name,"",comment))
                        this.arglist_add(n+"_size",(It 4,A1(1),name+"_size","",""))
                      |A2 _ ->
                        this.arglist_add(n,(typ,vtp,name,"",comment))
                        this.arglist_add(n+"_size",(It 4,A1(2),name+"_size","",""))
                      |A3 _ ->
                        this.arglist_add(n,(typ,vtp,name,"",comment))
                        this.arglist_add(n+"_size",(It 4,A1(3),name+"_size","",""))
                    let argname =
                        match typ,vtp with
                          |(It _|Dt|Zt|Structure _),A0 -> "(*"+name+")"
                          |_ -> name
                    code(typ,vtp,argname)
                  |H ->
                    //関数内ではこの変数名を使用
                    let name = 
                        match typ,vtp with
                          |(It _|Dt|Zt|Structure _),A0 ->
                            "<mi>arg"+(arglist_.Length+1).ToString("00")+"</mi>"
                          |_ -> 
                            "<mi>arg"+(arglist_.Length+1).ToString("00")+"</mi>"
                    match vtp with
                      |A0 ->
                        this.arglist_add(n,(typ,vtp,name,"",comment))
                      |A1 _ ->
                        this.arglist_add(n,(typ,vtp,name,"",comment))
                        this.arglist_add(n+"_size",(It 4,A1(1),name+"_size","",""))
                      |A2 _ ->
                        this.arglist_add(n,(typ,vtp,name,"",comment))
                        this.arglist_add(n+"_size",(It 4,A1(2),name+"_size","",""))
                      |A3 _ ->
                        this.arglist_add(n,(typ,vtp,name,"",comment))
                        this.arglist_add(n+"_size",(It 4,A1(3),name+"_size","",""))
                    let argname =
                        match typ,vtp with
                          |(It _|Dt|Zt|Structure _),A0 -> name
                          |_ -> name
                    code(typ,vtp,argname)
                  |NL ->
                    ()
                    
        /// <summary>
        /// 関数定義の引数を追加
        /// </summary>
        /// <param name="sname">構造体名</param>
        /// <param name="vtp">変数の次元</param>
        /// <param name="n">変数名</param>
        /// <param name="comment">変数の説明</param>
        member this.addarg (sname:string,vtp:VarType,n:string,comment:string) =
            fun code -> this.addarg (Structure(sname),vtp,n,comment) code
            
        ///<summary>ループカウンタ変数を作成し、code内の処理を実行</summary>
        member __.getloopvar code =
            let counter = 
                match lan with
                  |H ->
                    loopvar.getvar(fun i ->"<msub class=\""+"n_"+i.ToString()+"\"><mi>n</mi><mn>"+i.ToString()+"</mn></msub>")
                  |_ ->
                    loopvar.getvar(fun i ->"ic"+i.ToString("000"))
            code(It 4,counter,[])
            loopvar.dispose(counter)
            
        ///<summary>ループカウンタ変数とループ脱出先gotoラベルを作成し、code内の処理を実行</summary>
        member this.getloopvar_exit code =
            match lan with
              |F ->
                let goto = goto_label.ToString()
                this.goto_label_inc()
                let exit() = this.codewrite("goto "+goto+"\n")
                let counter = loopvar.getvar(fun i ->"ic"+i.ToString("000"))
                code(goto,(It 4,counter,[]),exit)
                loopvar.dispose(counter)
              |C89 ->
                let goto = "_"+goto_label.ToString()
                this.goto_label_inc()
                let exit() = this.codewrite("goto "+goto+";\n")
                let counter = loopvar.getvar(fun i ->"ic"+i.ToString("000"))
                code(goto,(It 4,counter,[]),exit)
                loopvar.dispose(counter)
              |C99 ->
                let goto = "_"+goto_label.ToString()
                this.goto_label_inc()
                let exit() = this.codewrite("goto "+goto+";\n")
                let counter = loopvar.getvar(fun i ->"ic"+i.ToString("000"))
                code(goto,(It 4,counter,[]),exit)
                loopvar.dispose(counter)
              |T ->
                let goto = "_"+goto_label.ToString()
                this.goto_label_inc()
                let exit() = this.codewrite("goto "+goto+"\n")
                let counter = loopvar.getvar(fun i ->"ic"+i.ToString("000"))
                code(goto,(It 4,counter,[]),exit)
                loopvar.dispose(counter)
              |H ->
                let goto = goto_label.ToString()
                this.goto_label_inc()
                let exit() = this.codewrite("<a href=\"#"+goto+"\">goto "+goto+"</a>\n<br/>\n")
                let counter = loopvar.getvar(fun i ->"<msub class=\""+"n_"+i.ToString()+"\"><mi>n</mi><mn>"+i.ToString()+"</mn></msub>")
                code(goto,(It 4,counter,[]),exit)
                loopvar.dispose(counter)
              |NL ->
                ()
                