namespace Aqualis
    
    open System
    
    type program(outputdir,pjname,lang:Language) =
        
        ///<summary>言語設定</summary>
        member val language = lang with get
        
        ///<summary>出力先ディレクトリ</summary>
        member val dir = outputdir with get
        
        ///<summary>プロジェクト名</summary>
        member val projectname = pjname with get
        
        ///<summary>定義された変数リスト</summary>
        member val var = varCollector lang with get
        
        member val varPrivate = varCollector lang with get
        
        member val varCopyIn = varCollector lang with get
        
        member val varCopyOut = varCollector lang with get
        
        ///<summary>整数型変数リスト</summary>
        member val i0 = varGenerator (match lang with |LaTeX|HTML -> (fun n -> "i^{("+n.ToString()+")}") |_ -> fun n -> "i0"+n.ToString "000") with get
        
        ///<summary>倍精度浮動小数点型変数リスト</summary>
        member val d0 = varGenerator (match lang with |LaTeX|HTML -> (fun n -> "d^{("+n.ToString()+")}") |_ -> fun n -> "d0"+n.ToString "000") with get
        
        ///<summary>複素数型変数リスト</summary>
        member val z0 = varGenerator (match lang with |LaTeX|HTML -> (fun n -> "z^{("+n.ToString()+")}") |_ -> fun n -> "z0"+n.ToString "000") with get
        
        ///<summary>文字変数リスト</summary>
        member val c0 = varGenerator (match lang with |LaTeX|HTML -> (fun n -> "c^{("+n.ToString()+")}") |_ -> fun n -> "c0"+n.ToString "000") with get
        
        ///<summary>文字列変数リスト</summary>
        member val t0 = varGenerator (match lang with |LaTeX|HTML -> (fun n -> "t^{("+n.ToString()+")}") |_ -> fun n -> "t0"+n.ToString "000") with get
        
        ///<summary>ファイルポインタリスト</summary>
        member val f0 = varGenerator (match lang with |LaTeX|HTML -> (fun n -> "f^{("+n.ToString()+")}") |_ -> fun n -> "f0"+n.ToString "000") with get
        
        ///<summary>整数型1次元配列リスト</summary>
        member val i1 = varGenerator (match lang with |LaTeX|HTML -> (fun n -> "\\dot{i}^{("+n.ToString()+")}") |_ -> fun n -> "i1"+n.ToString "000") with get
        
        ///<summary>倍精度浮動小数点型1次元配列リスト</summary>
        member val d1 = varGenerator (match lang with |LaTeX|HTML -> (fun n -> "\\dot{d}^{("+n.ToString()+")}") |_ -> fun n -> "d1"+n.ToString "000") with get
        
        ///<summary>複素数型1次元配列リスト</summary>
        member val z1 = varGenerator (match lang with |LaTeX|HTML -> (fun n -> "\\dot{z}^{("+n.ToString()+")}") |_ -> fun n -> "z1"+n.ToString "000") with get
        
        ///<summary>整数型2次元配列リスト</summary>
        member val i2 = varGenerator (match lang with |LaTeX|HTML -> (fun n -> "\\ddot{i}^{("+n.ToString()+")}") |_ -> fun n -> "i2"+n.ToString "000") with get
        
        ///<summary>倍精度浮動小数点型2次元配列リスト</summary>
        member val d2 = varGenerator (match lang with |LaTeX|HTML -> (fun n -> "\\ddot{d}^{("+n.ToString()+")}") |_ -> fun n -> "d2"+n.ToString "000") with get
        
        ///<summary>複素数型2次元配列リスト</summary>
        member val z2 = varGenerator (match lang with |LaTeX|HTML -> (fun n -> "\\ddot{z}^{("+n.ToString()+")}") |_ -> fun n -> "z2"+n.ToString "000") with get
        
        ///<summary>整数型3次元配列リスト</summary>
        member val i3 = varGenerator (match lang with |LaTeX|HTML -> (fun n -> "\\dddot{i}^{("+n.ToString()+")}") |_ -> fun n -> "i3"+n.ToString "000") with get
        
        ///<summary>倍精度浮動小数点型3次元配列リスト</summary>
        member val d3 = varGenerator (match lang with |LaTeX|HTML -> (fun n -> "\\dddot{d}^{("+n.ToString()+")}") |_ -> fun n -> "d3"+n.ToString "000") with get
        
        ///<summary>複素数型3次元配列リスト</summary>
        member val z3 = varGenerator (match lang with |LaTeX|HTML -> (fun n -> "\\dddot{z}^{("+n.ToString()+")}") |_ -> fun n -> "z3"+n.ToString "000") with get
        
        ///<summary>コード書き込み先一時ファイルストリーム</summary>
        member val cwriter = codeWriter((if pjname="" then "" else outputdir+"\\"+pjname+"_code.bee"),2,lang) with get
        
        ///<summary>変数宣言書き込み先一時ファイルストリーム</summary>
        member val vwriter = codeWriter((if pjname="" then "" else outputdir+"\\"+pjname+"_var.bee"),2,lang) with get
        
        ///<summary>構造体・関数宣言書き込み先一時ファイルストリーム</summary>
        member val hwriter = codeWriter((if pjname="" then "" else outputdir+"\\"+pjname+"_str.bee"),2,lang) with get
        
        ///<summary>並列ループ処理書き込み先一時ファイルストリーム</summary>
        member val pwriter = codeWriter((if pjname="" then "" else outputdir+"\\"+pjname+"_par.bee"),2,lang) with get
        
        ///<summary>ライブラリの使用時に必要なヘッダーファイル</summary>
        member val hlist = new UniqueList()
        
        ///<summary>ライブラリの使用時に必要なモジュールファイル</summary>
        member val mlist = new UniqueList()
        
        ///<summary>ライブラリの使用時に必要なextern指定子</summary>
        member val elist = new UniqueList()
        
        ///<summary>定義された関数のリスト</summary>
        member val flist = new UniqueList()
        
        ///<summary>コンパイル時に必要な他のソースファイル</summary>
        member val slist = new UniqueList()
        
        ///<summary>コンパイル時に必要なライブラリ・オプション</summary>
        member val olist = new UniqueList()
        
        member val numFormat = numericFormatController lang with get
        
        member val arg = argumentController lang with get
        
        // ///<summary>セクションのヘッダを画面出力</summary>
        // member this.displaySection with get() = display_section
        // member this.setDisplaySection s = display_section <- s
        
    [<AutoOpen>]
    module aqualisProgram =
        
        let gotoLabel = gotoLabelController()
        let error = errorIDController()
        let debug = debugController()
        let mutable displaySection = false
        let mutable isOmpUsed = false
        let mutable isOaccUsed = false
        ///<summary>trueのとき並列処理を書き込む</summary>
        let mutable isParMode = false
        ///<summary>定義された関数のリスト</summary>
        let mutable funlist: string list = []
        let funlist_nonoverlap() =
            let rec reduce lst1 lst2 =
                match lst1 with
                |x::y ->
                    match List.tryFind (fun s -> s=x) lst2 with
                    |None ->
                        reduce y (lst2@[x])
                    |_ ->
                        reduce y lst2
                |[] -> lst2
            reduce funlist []
        let mutable private programList:list<program> = [program("","",Numeric)]
        
        let mutable pr = programList[0]
        
        let nextProgram(outputdir,projectname,lang:Language) =
            programList <- program(outputdir,projectname,lang)::programList
            pr <- programList[0]
        let backProgram() =
            programList <- programList.Tail
            pr <- programList[0]
            
    ///<summary>コード生成の設定</summary>
    type AqualisCompiler () =
        
        ///<summary>言語</summary>
        static member lang = pr.language
        
        ///<summary>プロジェクト名</summary>
        static member projname = pr.projectname
        
        ///<summary>整数を文字列に変換した時の桁数</summary>
        static member int_string_format with get() = pr.numFormat.iFormat

        ///<summary>整数をn桁の文字列で変換するように設定</summary>
        static member set_int_string_format(d) = pr.numFormat.setIFormat(d)
        
        ///<summary>倍精度浮動小数点をn桁（小数点以下m桁）の文字列で変換するように設定</summary>
        static member double_string_format with get() = pr.numFormat.dFormat
        
        ///<summary>倍精度浮動小数点をn桁（小数点以下m桁）の文字列で変換するように設定</summary>
        static member set_double_string_format(n,d) = pr.numFormat.setDFormat(n,d)
        
        ///<summary>デバッグモードの切り替え</summary>
        static member set_DebugMode (x:Switch) =
            match x with
            |ON  -> debug.setDebugMode true
            |OFF -> debug.setDebugMode false
            
        ///<summary>デバッグモードの切り替え</summary>
        static member set_DisplaySection (x:Switch) =
            match x with
            |ON  -> displaySection <- true
            |OFF -> displaySection <- false
            
        ///<summary>codeをデバッグモードで実行</summary>
        static member debug code =
            AqualisCompiler.set_DebugMode ON
            code()
            AqualisCompiler.set_DebugMode OFF
            
        ///<summary>プログラムの実行を強制終了</summary>
        static member abort() =
            match pr.language with 
            |Fortran -> pr.cwriter.codewrite "stop" 
            |C99     -> pr.cwriter.codewrite "return 1;" 
            |LaTeX   -> pr.cwriter.codewrite "stop"
            |HTML    -> pr.cwriter.codewrite "stop"
            |Python  -> pr.cwriter.codewrite "sys.exit(1)"
            |Numeric -> ()
            
        ///<summary>何かのキーを押すまで実行を一時停止</summary>
        static member stop() =
            match pr.language with
            |Fortran -> pr.cwriter.codewrite "read *, \n"
            |C99     -> pr.cwriter.codewrite "getchar();\n"
            |LaTeX   -> pr.cwriter.codewrite "stop\n"
            |HTML    -> pr.cwriter.codewrite "stop\n"
            |Python  -> pr.cwriter.codewrite "input()"
            |Numeric -> ()
            
        /// <summary>
        /// インクルードファイル追加（TeXの場合はプリアンブル部挿入コード）
        /// </summary>
        /// <param name="t">オプション</param>
        static member incld(s:string) =
            pr.hlist.add s
            
        /// <summary>
        /// コンパイルオプションを追加
        /// </summary>
        /// <param name="t">オプション</param>
        static member option(t:string) =
            pr.olist.add("-"+t)
