namespace Aqualis
    
    open System.IO
    
    type program(outputdir,pjname,lang:Language) =
        
        let cwriter = codeWriter(outputdir+"\\"+pjname+"_code.bee",2,lang)

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
        
        member _.comment(s:string) = cwriter.comment s
        
        member _.codewrite(s:string) = cwriter.codewrite s
        member _.indentInc() = cwriter.indent.inc()
        member _.indentDec() = cwriter.indent.dec()
        member _.appendOpen() = cwriter.appendOpen()
        member _.close() = cwriter.close()
        member _.allCodes with get() = File.ReadAllText(outputdir+"\\"+pjname+"_code.bee")
        member _.delete() = cwriter.delete()
        
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
        ///<summary>現在生成中のプログラミング言語</summary>
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
        let mutable programList:list<program> = []
                    
        let mutable prIndex = 0
        
        // let mutable pr = 
        //     if prIndex >= programList.Length then printfn "%d %d" prIndex programList.Length
        //     programList[prIndex]
        
        let makeProgram(programInfo:list<string*string*Language>) code =
            let programList_temp = programList
            let prIndex_temp = prIndex
            programList <- [for x in programInfo -> program x]
            prIndex <- 0
            let result = code()
            programList <- programList_temp
            prIndex <- prIndex_temp
            result
            
        let codewrite(s:string) = programList[prIndex].codewrite s
        let comment(s:string) = programList[prIndex].comment s
        let language() = programList[prIndex].language
        let projectname() = programList[prIndex].projectname
        let iFormat() = programList[prIndex].numFormat.iFormat

    ///<summary>コード生成の設定</summary>
    type AqualisCompiler () =
        
        ///<summary>言語</summary>
        static member lang() = language()
        
        ///<summary>プロジェクト名</summary>
        static member projname() = projectname()
        
        ///<summary>整数を文字列に変換した時の桁数</summary>
        static member int_string_format with get() = iFormat()

        ///<summary>整数をn桁の文字列で変換するように設定</summary>
        static member set_int_string_format(d) = programList[prIndex].numFormat.setIFormat(d)
        
        ///<summary>倍精度浮動小数点をn桁（小数点以下m桁）の文字列で変換するように設定</summary>
        static member double_string_format with get() = programList[prIndex].numFormat.dFormat
        
        ///<summary>倍精度浮動小数点をn桁（小数点以下m桁）の文字列で変換するように設定</summary>
        static member set_double_string_format(n,d) = programList[prIndex].numFormat.setDFormat(n,d)
        
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
            match language() with 
            |Fortran -> codewrite "stop" 
            |C99     -> codewrite "return 1;" 
            |LaTeX   -> codewrite "stop"
            |HTML    -> codewrite "stop"
            |Python  -> codewrite "sys.exit(1)"
            |JavaScript -> ()
            |PHP -> ()
            |Numeric -> ()
            
        ///<summary>何かのキーを押すまで実行を一時停止</summary>
        static member stop() =
            match language() with
            |Fortran -> codewrite "read *, \n"
            |C99     -> codewrite "getchar();\n"
            |LaTeX   -> codewrite "stop\n"
            |HTML    -> codewrite "stop\n"
            |Python  -> codewrite "input()"
            |JavaScript -> ()
            |PHP -> ()
            |Numeric -> ()
            
        /// <summary>
        /// インクルードファイル追加（TeXの場合はプリアンブル部挿入コード）
        /// </summary>
        /// <param name="t">オプション</param>
        static member incld(s:string) =
            programList[prIndex].hlist.add s
            
        /// <summary>
        /// コンパイルオプションを追加
        /// </summary>
        /// <param name="t">オプション</param>
        static member option(t:string) =
            programList[prIndex].olist.add("-"+t)
