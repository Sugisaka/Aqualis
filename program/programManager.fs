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

        let cwriter = new codeWriter(outputdir+"\\"+pjname,2,lang)

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

        interface System.IDisposable with
            member _.Dispose() =
                (cwriter :> System.IDisposable).Dispose()

    type MovieSetting = {
        Character: Switch
        Subtitle: Switch
        Voice: Switch
    }

    [<RequireQualifiedAccess>]
    module MovieSetting =
        let Default = {
            Character = ON
            Subtitle = ON
            Voice = ON
        }

    type internal HtmlGenerationState() =
        member val InputCounter = 0 with get, set
        member val DrawFileName = "" with get, set
        member val BodyFileName = "" with get, set
        member val JavaScriptFileName = "" with get, set
        member val AnimationStartFileName = "" with get, set
        member val AnimationSequenceResetFileName = "" with get, set
        member val AnimationResetFileName = "" with get, set
        member val ContentsDirectory = "" with get, set
        member val ContentsCounter = -1 with get, set
        member val AnimationSequenceCounter = -1 with get, set
        member val AnimationGroupCounter = -1 with get, set
        member val FigureCounter = 0 with get, set
        member val AnimationCounter = 0 with get, set
        member val AnimationButtons = ResizeArray<string * string * int * int>()
        member val AudioFiles = ResizeArray<string>()

    type internal AnimationOptionsState(setting:MovieSetting) =
        member val CharacterEnabled = (setting.Character = ON)
        member val SubtitleEnabled = (setting.Subtitle = ON)
        member val VoiceEnabled = (setting.Voice = ON)

    type internal SequenceDiagramBuilder() =
        member val TerminalLifeLine = 100.0 with get, set
        member val Variables: list<string * int * float> = [] with get, set
        member val Frames: list<float * float * float * float> = [] with get, set
        member val Branches: list<list<string * float>> = [] with get, set

    type internal SequenceDiagramState() =
        member val TopMargin = 40.0 with get, set
        member val LeftMargin = 40.0 with get, set
        member val VariableInterval = 150.0 with get, set
        member val SingleArrowLength = 37.5 with get, set
        member val VariableHeaderWidth = 50.0 with get, set
        member val VariableHeaderHeight = 20.0 with get, set
        member val LineWidth = 2.0 with get, set
        member val ActiveLineWidth = 10.0 with get, set
        member val FrameMargin = 10.0 with get, set
        member val TimeStep = 10.0 with get, set
        member val FrameBorder = 2.0 with get, set
        member val ActiveLineColor = "rgba(0, 191, 255, 0.5)" with get, set
        member val LoopFrameColor = "rgb(255, 0, 0)" with get, set
        member val BranchFrameColor = "rgb(0, 180, 0)" with get, set
        member val SectionFrameColor = "rgb(127,0,255)" with get, set
        member val Builder = SequenceDiagramBuilder()

    type internal WebGenerationState(movieSetting:MovieSetting) =
        member val Html = HtmlGenerationState()
        member val Animation = AnimationOptionsState(movieSetting)
        member val SequenceDiagram = SequenceDiagramState()

    /// State owned by one code-generation operation.
    ///
    /// The current context is stored in AsyncLocal only to keep the existing DSL
    /// syntax (for example, x <== y) source-compatible. Values created by the DSL
    /// capture this instance and assignments use the captured context.
    type private GenerationState =
        {
            Programs: program array
            mutable DisplaySection: bool
            mutable IsOpenMpUsed: bool
            mutable IsOpenAccUsed: bool
            mutable IsParallelMode: bool
            Functions: ResizeArray<string>
            GotoLabels: gotoLabelController
            Errors: errorIDController
            Debug: debugController
            Web: WebGenerationState
        }

    type GenerationContext private (state:GenerationState, currentIndex:int) =
        static let current = AsyncLocal<GenerationContext option>()

        static member private CreateState(programs:program list, movieSetting:MovieSetting) =
            let programArray = programs |> List.toArray
            if programArray.Length = 0 then
                invalidArg (nameof programs) "At least one program is required."
            {
                Programs = programArray
                DisplaySection = false
                IsOpenMpUsed = false
                IsOpenAccUsed = false
                IsParallelMode = false
                Functions = ResizeArray<string>()
                GotoLabels = gotoLabelController()
                Errors = errorIDController()
                Debug = debugController()
                Web = WebGenerationState(movieSetting)
            }

        new(programs:program list) =
            GenerationContext(
                GenerationContext.CreateState(programs, MovieSetting.Default),
                0)

        new(programs:program list, movieSetting:MovieSetting) =
            GenerationContext(
                GenerationContext.CreateState(programs, movieSetting),
                0)

        member _.Programs = state.Programs

        member _.CurrentIndex = currentIndex

        member _.CurrentProgram = state.Programs[currentIndex]

        member _.DisplaySection
            with get () = state.DisplaySection
            and set value = state.DisplaySection <- value

        member _.IsOpenMpUsed
            with get () = state.IsOpenMpUsed
            and set value = state.IsOpenMpUsed <- value

        member _.IsOpenAccUsed
            with get () = state.IsOpenAccUsed
            and set value = state.IsOpenAccUsed <- value

        member _.IsParallelMode
            with get () = state.IsParallelMode
            and set value = state.IsParallelMode <- value

        member this.WithParallelMode(code: unit -> 'T) : 'T =
            let previous = state.IsParallelMode
            try
                state.IsParallelMode <- true
                code()
            finally
                state.IsParallelMode <- previous

        member _.WithDebugMode(enabled:bool, code: unit -> 'T) : 'T =
            let previous = state.Debug.debugMode
            try
                state.Debug.setDebugMode enabled
                code()
            finally
                state.Debug.setDebugMode previous

        member _.Functions = state.Functions

        member _.DistinctFunctions =
            state.Functions |> Seq.distinct |> Seq.toList

        member _.GotoLabels = state.GotoLabels

        member _.Errors = state.Errors

        member _.Debug = state.Debug

        member internal _.Web = state.Web

        member _.ForProgram(index:int) =
            if index < 0 || index >= state.Programs.Length then
                invalidArg (nameof index) $"Program index {index} is outside the valid range."
            GenerationContext(state, index)

        member this.WithProgram(index: int, code: unit -> 'T) : 'T =
            this.ForProgram(index).Activate(code)

        member this.Activate(code: unit -> 'T) : 'T =
            let previous = current.Value
            try
                current.Value <- Some this
                code ()
            finally
                current.Value <- previous

        static member TryCurrent = current.Value

    module internal GenerationScope =
        let requireContext() =
            GenerationContext.TryCurrent
            |> Option.defaultWith (fun () ->
                invalidOp "This operation must run inside a GenerationContext.")

        let currentProgram() =
            (requireContext()).CurrentProgram

        let gotoLabels() =
            (requireContext()).GotoLabels

        let errors() =
            (requireContext()).Errors

        let debug() =
            (requireContext()).Debug

    module internal WebGenerationScope =
        let state() =
            (GenerationScope.requireContext()).Web

        let html() =
            (state()).Html

        let animation() =
            (state()).Animation

        let sequenceDiagram() =
            (state()).SequenceDiagram

    [<AutoOpen>]
    module aqualisProgram =

        ///<summary>現在生成中のプログラミング言語</summary>
        let funlist_nonoverlap() =
            (GenerationScope.requireContext()).DistinctFunctions
        let private disposePrograms (programs:program list) =
            programs
            |> List.iter (fun item ->
                (item :> System.IDisposable).Dispose())

        let makeProgramWithContext
            (programInfo: list<string * string * Language>)
            (code: GenerationContext -> 'T)
            : 'T =
            let programs =
                programInfo
                |> List.map (fun (dir, name, language) ->
                    new program(dir, name, language))
            let context = GenerationContext programs

            try
                context.Activate(fun () -> code context)
            finally
                disposePrograms programs

        let makeProgramWithMovieSetting
            (movieSetting:MovieSetting)
            (programInfo: list<string * string * Language>)
            (code: unit -> 'T)
            : 'T =
            let programs =
                programInfo
                |> List.map (fun (dir, name, language) ->
                    new program(dir, name, language))
            let context = GenerationContext(programs, movieSetting)

            try
                context.Activate(code)
            finally
                disposePrograms programs

        /// Backward-compatible entry point. New code should prefer
        /// makeProgramWithContext when it needs direct access to the context.
        let makeProgram (programInfo: list<string * string * Language>) (code: unit -> 'T) : 'T =
            makeProgramWithContext programInfo (fun _ -> code ())

        let write(s:string) = GenerationScope.currentProgram().codewrite s
        let writei(s:string) = GenerationScope.currentProgram().codewritei s
        let writen(s:string) = GenerationScope.currentProgram().codewriten s
        let writein(s:string) = GenerationScope.currentProgram().codewritein s
        let hwritein(h:string,s:string) = GenerationScope.currentProgram().codewritein (h,s)
        let eqbr() = writein "\\\\"
        let language() = GenerationScope.currentProgram().language

        ///<summary>コメント文を生成</summary>
        let (!) s = GenerationScope.currentProgram().comment s

    ///<summary>コード生成の設定</summary>
    type AqualisCompiler () =

        ///<summary>言語</summary>
        static member language with get() = GenerationScope.currentProgram().language

        ///<summary>プロジェクト名</summary>
        static member projectName with get() = GenerationScope.currentProgram().projectName

        ///<summary>整数を文字列に変換した時の桁数</summary>
        static member intFormat with get() = GenerationScope.currentProgram().numFormat.iFormat

        ///<summary>整数をn桁の文字列で変換するように設定</summary>
        static member intFormatSet d = GenerationScope.currentProgram().numFormat.setIFormat d

        ///<summary>倍精度浮動小数点をn桁（小数点以下m桁）の文字列で変換するように設定</summary>
        static member doubleFormat with get() = GenerationScope.currentProgram().numFormat.dFormat

        ///<summary>倍精度浮動小数点をn桁（小数点以下m桁）の文字列で変換するように設定</summary>
        static member doubleFormatSet(n,d) = GenerationScope.currentProgram().numFormat.setDFormat(n,d)

        ///<summary>デバッグモードの切り替え</summary>
        static member set_DebugMode (x:Switch) =
            match x with
            |ON  -> (GenerationScope.debug()).setDebugMode true
            |OFF -> (GenerationScope.debug()).setDebugMode false

        ///<summary>デバッグモードの切り替え</summary>
        static member set_DisplaySection (x:Switch) =
            match x with
            |ON  -> (GenerationScope.requireContext()).DisplaySection <- true
            |OFF -> (GenerationScope.requireContext()).DisplaySection <- false

        ///<summary>codeをデバッグモードで実行</summary>
        static member debug code =
            let context = GenerationScope.requireContext()
            context.WithDebugMode(true, code)

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
            GenerationScope.currentProgram().hlist.add s

        /// <summary>
        /// コンパイルオプションを追加
        /// </summary>
        /// <param name="t">オプション</param>
        static member option(t:string) =
            GenerationScope.currentProgram().olist.add("-"+t)
