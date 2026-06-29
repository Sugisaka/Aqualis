// 
// Copyright (c) 2026 Jun-ichiro Sugisaka
// 
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
// 
namespace Aqualis
    
    open System.IO
    open System.Threading
    
    type program(outputdir,pjname,lang:Language) =
        
        let cwriter = codeWriter(outputdir+"\\"+pjname,2,lang)
        
        /// 構造体
        let structData = structure()
        
        ///<summary>言語設定</summary>
        member val language = lang with get
        
        ///<summary>出力先ディレクトリ</summary>
        member val dir = outputdir with get
        
        ///<summary>プロジェクト名</summary>
        member val projectName = pjname with get
        
        ///<summary>定義された変数リスト</summary>
        member val var = varCollector lang with get
        
        member val varPrivate = varCollector lang with get
        
        member val varCopyIn = varCollector lang with get
        
        member val varCopyOut = varCollector lang with get
        
        ///<summary>整数型変数リスト</summary>
        member val i0 = varGenerator (
            match lang with
            |LaTeX|HTML|HTMLSequenceDiagram -> fun n -> "i^{("+n.ToString()+")}"
            |PHP -> fun n -> "$i0"+n.ToString "000"
            |_ -> fun n -> "i0"+n.ToString "000"
            ) with get
            
        ///<summary>倍精度浮動小数点型変数リスト</summary>
        member val d0 = varGenerator (
            match lang with
            |LaTeX|HTML|HTMLSequenceDiagram -> fun n -> "d^{("+n.ToString()+")}"
            |PHP -> fun n -> "$d0"+n.ToString "000"
            |_ -> fun n -> "d0"+n.ToString "000"
            ) with get
        
        ///<summary>複素数型変数リスト</summary>
        member val z0 = varGenerator (
            match lang with
            |LaTeX|HTML|HTMLSequenceDiagram -> fun n -> "z^{("+n.ToString()+")}"
            |PHP -> fun n -> "$z0"+n.ToString "000"
            |_ -> fun n -> "z0"+n.ToString "000"
            ) with get
        
        ///<summary>文字変数リスト</summary>
        member val c0 = varGenerator (
            match lang with
            |LaTeX|HTML|HTMLSequenceDiagram -> fun n -> "c^{("+n.ToString()+")}"
            |PHP -> fun n -> "$c0"+n.ToString "000"
            |_ -> fun n -> "c0"+n.ToString "000"
            ) with get
        
        ///<summary>文字列変数リスト</summary>
        member val t0 = varGenerator (
            match lang with
            |LaTeX|HTML|HTMLSequenceDiagram -> fun n -> "t^{("+n.ToString()+")}"
            |PHP -> fun n -> "$t0"+n.ToString "000"
            |_ -> fun n -> "t0"+n.ToString "000"
            ) with get
        
        ///<summary>ファイルポインタリスト</summary>
        member val f0 = varGenerator (
            match lang with
            |LaTeX|HTML|HTMLSequenceDiagram -> fun n -> "f^{("+n.ToString()+")}"
            |PHP -> fun n -> "$f0"+n.ToString "000"
            |_ -> fun n -> "f0"+n.ToString "000"
            ) with get
        
        ///<summary>整数型1次元配列リスト</summary>
        member val i1 = varGenerator (
            match lang with
            |LaTeX|HTML|HTMLSequenceDiagram -> fun n -> "\\dot{i}^{("+n.ToString()+")}"
            |PHP -> fun n -> "$i1"+n.ToString "000"
            |_ -> fun n -> "i1"+n.ToString "000"
            ) with get
        
        ///<summary>倍精度浮動小数点型1次元配列リスト</summary>
        member val d1 = varGenerator (
            match lang with 
            |LaTeX|HTML|HTMLSequenceDiagram -> fun n -> "\\dot{d}^{("+n.ToString()+")}"
            |PHP -> fun n -> "$d1"+n.ToString "000"
            |_ -> fun n -> "d1"+n.ToString "000"
            ) with get
        
        ///<summary>複素数型1次元配列リスト</summary>
        member val z1 = varGenerator (
            match lang with 
            |LaTeX|HTML|HTMLSequenceDiagram -> fun n -> "\\dot{z}^{("+n.ToString()+")}"
            |PHP -> fun n -> "$z1"+n.ToString "000"
            |_ -> fun n -> "z1"+n.ToString "000"
            ) with get
        
        ///<summary>整数型2次元配列リスト</summary>
        member val i2 = varGenerator (
            match lang with 
            |LaTeX|HTML|HTMLSequenceDiagram -> fun n -> "\\ddot{i}^{("+n.ToString()+")}"
            |PHP -> fun n -> "$i2"+n.ToString "000"
            |_ -> fun n -> "i2"+n.ToString "000"
            ) with get
        
        ///<summary>倍精度浮動小数点型2次元配列リスト</summary>
        member val d2 = varGenerator (
            match lang with
            |LaTeX|HTML|HTMLSequenceDiagram -> fun n -> "\\ddot{d}^{("+n.ToString()+")}"
            |PHP -> fun n -> "$d2"+n.ToString "000"
            |_ -> fun n -> "d2"+n.ToString "000"
            ) with get
        
        ///<summary>複素数型2次元配列リスト</summary>
        member val z2 = varGenerator (
            match lang with 
            |LaTeX|HTML|HTMLSequenceDiagram -> fun n -> "\\ddot{z}^{("+n.ToString()+")}"
            |PHP -> fun n -> "$z2"+n.ToString "000"
            |_ -> fun n -> "z2"+n.ToString "000"
            ) with get
        
        ///<summary>整数型3次元配列リスト</summary>
        member val i3 = varGenerator (
            match lang with 
            |LaTeX|HTML|HTMLSequenceDiagram -> fun n -> "\\dddot{i}^{("+n.ToString()+")}"
            |PHP -> fun n -> "$i3"+n.ToString "000"
            |_ -> fun n -> "i3"+n.ToString "000"
            ) with get
        
        ///<summary>倍精度浮動小数点型3次元配列リスト</summary>
        member val d3 = varGenerator (
            match lang with 
            |LaTeX|HTML|HTMLSequenceDiagram -> fun n -> "\\dddot{d}^{("+n.ToString()+")}"
            |PHP -> fun n -> "$d3"+n.ToString "000"
            |_ -> fun n -> "d3"+n.ToString "000"
            ) with get
        
        ///<summary>複素数型3次元配列リスト</summary>
        member val z3 = varGenerator (
            match lang with 
            |LaTeX|HTML|HTMLSequenceDiagram -> fun n -> "\\dddot{z}^{("+n.ToString()+")}"
            |PHP -> fun n -> "$z3"+n.ToString "000"
            |_ -> fun n -> "z3"+n.ToString "000"
            ) with get
        
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
        member _.codewritei(s:string) = cwriter.codewritei s
        member _.codewriten(s:string) = cwriter.codewriten s
        member _.codewritein(s:string) = cwriter.codewritein s
        member _.codewritein(h:string,s:string) = cwriter.codewritein (h,s)
        member _.indentInc() = cwriter.indent.inc()
        member _.indentDec() = cwriter.indent.dec()
        member _.appendOpen() = cwriter.appendOpen()
        member _.close() = cwriter.close()
        member _.allCodes with get() = 
            cwriter.close()
            File.ReadAllText(outputdir+"\\"+pjname)
        member _.delete() = cwriter.delete()
        member _.str with get() = structData

    /// State owned by one code-generation operation.
    ///
    /// The current context is stored in AsyncLocal only to keep the existing DSL
    /// syntax (for example, x <== y) source-compatible. Values created by the DSL
    /// capture this instance and assignments use the captured context.
    type GenerationContext(programs: program list) =
        static let current = AsyncLocal<GenerationContext option>()
        let programs = programs |> List.toArray
        let mutable currentIndex = 0
        let mutable displaySection = false
        let mutable isOmpUsed = false
        let mutable isOaccUsed = false
        let mutable isParMode = false
        let functions = ResizeArray<string>()

        do
            if programs.Length = 0 then
                invalidArg (nameof programs) "At least one program is required."

        member _.Programs = programs

        member _.CurrentIndex = currentIndex

        member _.CurrentProgram = programs[currentIndex]

        member _.DisplaySection
            with get () = displaySection
            and set value = displaySection <- value

        member _.IsOpenMpUsed
            with get () = isOmpUsed
            and set value = isOmpUsed <- value

        member _.IsOpenAccUsed
            with get () = isOaccUsed
            and set value = isOaccUsed <- value

        member _.IsParallelMode
            with get () = isParMode
            and set value = isParMode <- value

        member _.Functions = functions

        member _.DistinctFunctions =
            functions |> Seq.distinct |> Seq.toList

        member _.WithProgram(index: int, code: unit -> 'T) : 'T =
            if index < 0 || index >= programs.Length then
                invalidArg (nameof index) $"Program index {index} is outside the valid range."

            let previousIndex = currentIndex
            try
                currentIndex <- index
                code ()
            finally
                currentIndex <- previousIndex

        member this.Activate(code: unit -> 'T) : 'T =
            let previous = current.Value
            try
                current.Value <- Some this
                code ()
            finally
                current.Value <- previous

        static member TryCurrent = current.Value
        
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
        
        let private withProgramState newProgramList newPrIndex code =
            let oldProgramList = programList
            let oldPrIndex = prIndex
            try
                programList <- newProgramList
                prIndex <- newPrIndex
                code ()
            finally
                programList <- oldProgramList
                prIndex <- oldPrIndex
                
        let makeProgramWithContext
            (programInfo: list<string * string * Language>)
            (code: GenerationContext -> 'T)
            : 'T =
            let programs =
                programInfo
                |> List.map program
            let context = GenerationContext programs

            context.Activate(fun () ->
                withProgramState programs 0 (fun () -> code context))

        /// Backward-compatible entry point. New code should prefer
        /// makeProgramWithContext when it needs direct access to the context.
        let makeProgram (programInfo: list<string * string * Language>) (code: unit -> 'T) : 'T =
            makeProgramWithContext programInfo (fun _ -> code ())
            
        let write(s:string) = programList[prIndex].codewrite s
        let writei(s:string) = programList[prIndex].codewritei s
        let writen(s:string) = programList[prIndex].codewriten s
        let writein(s:string) = programList[prIndex].codewritein s
        let hwritein(h:string,s:string) = programList[prIndex].codewritein (h,s)
        let eqbr() = writein "\\\\"
        let language() = programList[prIndex].language
        
        ///<summary>コメント文を生成</summary>
        let (!) s = programList[prIndex].comment s
        
    ///<summary>コード生成の設定</summary>
    type AqualisCompiler () =
        
        ///<summary>言語</summary>
        static member language with get() = programList[prIndex].language
        
        ///<summary>プロジェクト名</summary>
        static member projectName with get() = programList[prIndex].projectName
        
        ///<summary>整数を文字列に変換した時の桁数</summary>
        static member intFormat with get() = programList[prIndex].numFormat.iFormat

        ///<summary>整数をn桁の文字列で変換するように設定</summary>
        static member intFormatSet d = programList[prIndex].numFormat.setIFormat d
        
        ///<summary>倍精度浮動小数点をn桁（小数点以下m桁）の文字列で変換するように設定</summary>
        static member doubleFormat with get() = programList[prIndex].numFormat.dFormat
        
        ///<summary>倍精度浮動小数点をn桁（小数点以下m桁）の文字列で変換するように設定</summary>
        static member doubleFormatSet(n,d) = programList[prIndex].numFormat.setDFormat(n,d)
        
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
            |Fortran ->
                writein "stop" 
            |C99 ->
                writein "return 1;" 
            |LaTeX ->
                writein "stop"
            |HTML ->
                writein "stop"
            |HTMLSequenceDiagram ->
                writein "stop"
            |Python ->
                writein "sys.exit(1)"
            |JavaScript ->
                ()
            |PHP ->
                ()
            |Numeric ->
                ()
            
        ///<summary>何かのキーを押すまで実行を一時停止</summary>
        static member stop() =
            match language() with
            |Fortran ->
                writein "read *, \n"
            |C99 ->
                writein "getchar();\n"
            |LaTeX ->
                writein "stop\n"
            |HTML ->
                writein "stop\n"
            |HTMLSequenceDiagram ->
                writein "stop\n"
            |Python ->
                writein "input()"
            |JavaScript ->
                ()
            |PHP ->
                ()
            |Numeric ->
                ()
                
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
