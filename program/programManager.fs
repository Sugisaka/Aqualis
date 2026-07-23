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

        // HTML sequence-diagram state belongs to the concrete output program.
        // Keeping it here makes program switching explicit and removes any need
        // for an ambient web-generation scope.
        let sequenceGate = obj()
        let mutable sequenceTopMargin = 40.0
        let mutable sequenceLeftMargin = 40.0
        let mutable sequenceVariableInterval = 150.0
        let mutable sequenceSingleArrowLength = 37.5
        let mutable sequenceVariableHeaderWidth = 50.0
        let mutable sequenceVariableHeaderHeight = 20.0
        let mutable sequenceLineWidth = 2.0
        let mutable sequenceActiveLineWidth = 10.0
        let mutable sequenceFrameMargin = 10.0
        let mutable sequenceTimeStep = 10.0
        let mutable sequenceFrameBorder = 2.0
        let mutable sequenceActiveLineColor = "rgba(0, 191, 255, 0.5)"
        let mutable sequenceLoopFrameColor = "rgb(255, 0, 0)"
        let mutable sequenceBranchFrameColor = "rgb(0, 180, 0)"
        let mutable sequenceSectionFrameColor = "rgb(127,0,255)"
        let mutable terminalLifeLine = 100.0 
        let mutable sequenceVariables : (string*int*float) list = []
        let mutable sequenceFrames : (float*float*float*float) list = []
        let mutable sequenceBranches : ((string*float) list) list = []

        member internal _.SequenceGate = sequenceGate
        member internal _.SequenceTopMargin with get() = sequenceTopMargin and set v = sequenceTopMargin <- v
        member internal _.SequenceLeftMargin with get() = sequenceLeftMargin and set v = sequenceLeftMargin <- v
        member internal _.SequenceVariableInterval with get() = sequenceVariableInterval and set v = sequenceVariableInterval <- v
        member internal _.SequenceSingleArrowLength with get() = sequenceSingleArrowLength and set v = sequenceSingleArrowLength <- v
        member internal _.SequenceVariableHeaderWidth with get() = sequenceVariableHeaderWidth and set v = sequenceVariableHeaderWidth <- v
        member internal _.SequenceVariableHeaderHeight with get() = sequenceVariableHeaderHeight and set v = sequenceVariableHeaderHeight <- v
        member internal _.SequenceLineWidth with get() = sequenceLineWidth and set v = sequenceLineWidth <- v
        member internal _.SequenceActiveLineWidth with get() = sequenceActiveLineWidth and set v = sequenceActiveLineWidth <- v
        member internal _.SequenceFrameMargin with get() = sequenceFrameMargin and set v = sequenceFrameMargin <- v
        member internal _.SequenceTimeStep with get() = sequenceTimeStep and set v = sequenceTimeStep <- v
        member internal _.SequenceFrameBorder with get() = sequenceFrameBorder and set v = sequenceFrameBorder <- v
        member internal _.SequenceActiveLineColor with get() = sequenceActiveLineColor and set v = sequenceActiveLineColor <- v
        member internal _.SequenceLoopFrameColor with get() = sequenceLoopFrameColor and set v = sequenceLoopFrameColor <- v
        member internal _.SequenceBranchFrameColor with get() = sequenceBranchFrameColor and set v = sequenceBranchFrameColor <- v
        member internal _.SequenceSectionFrameColor with get() = sequenceSectionFrameColor and set v = sequenceSectionFrameColor <- v
        member internal _.TerminalLifeLine with get() = lock sequenceGate (fun () -> terminalLifeLine) and set v = lock sequenceGate (fun () -> terminalLifeLine <- v)
        member internal _.SequenceVariables with get() = lock sequenceGate (fun () -> sequenceVariables) and set v = lock sequenceGate (fun () -> sequenceVariables <- v)
        member internal _.SequenceFrames with get() = lock sequenceGate (fun () -> sequenceFrames) and set v = lock sequenceGate (fun () -> sequenceFrames <- v)
        member internal _.SequenceBranches with get() = lock sequenceGate (fun () -> sequenceBranches) and set v = lock sequenceGate (fun () -> sequenceBranches <- v)

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

    /// <summary>Specifies which presentation features are enabled for movie generation.</summary>
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

    /// <summary>Stores synchronized mutable state used while generating HTML and animations.</summary>
    type internal HtmlGenerationState() =
        let gate = obj()
        let mutable contentsDirectory = ""
        let mutable contentsCounter = -1
        let mutable animationSequenceCounter = -1
        let mutable animationGroupCounter = -1
        let mutable figureCounter = 0
        let mutable animationCounter = 0
        let animationButtons = ResizeArray<string * string * int * int>()
        let audioFiles = ResizeArray<string>()

        /// <summary>Gets or sets the directory that receives generated web content.</summary>
        member _.ContentsDirectory
            with get() = lock gate (fun () -> contentsDirectory)
            and set value = lock gate (fun () -> contentsDirectory <- value)

        /// <summary>Allocates the next unique HTML content number.</summary>
        member _.NextContentsNumber() =
            lock gate (fun () ->
                contentsCounter <- contentsCounter + 1
                contentsCounter)

        /// <summary>Allocates the next unique animation sequence number.</summary>
        member _.NextAnimationSequenceNumber() =
            lock gate (fun () ->
                animationSequenceCounter <- animationSequenceCounter + 1
                animationSequenceCounter)

        /// <summary>Allocates the next unique animation group number.</summary>
        member _.NextAnimationGroupNumber() =
            lock gate (fun () ->
                animationGroupCounter <- animationGroupCounter + 1
                animationGroupCounter)

        /// <summary>Allocates the next unique animated figure number.</summary>
        member _.NextFigureNumber() =
            lock gate (fun () ->
                figureCounter <- figureCounter + 1
                figureCounter)

        /// <summary>Allocates the next unique animation page number.</summary>
        member _.NextAnimationNumber() =
            lock gate (fun () ->
                animationCounter <- animationCounter + 1
                animationCounter)

        /// <summary>Gets the number assigned to the most recently created animation page.</summary>
        member _.AnimationCount =
            lock gate (fun () -> animationCounter)

        /// <summary>Registers the control button data for an animation.</summary>
        member _.AddAnimationButton(button) =
            lock gate (fun () -> animationButtons.Add(button))

        /// <summary>Removes all registered animation control buttons.</summary>
        member _.ClearAnimationButtons() =
            lock gate animationButtons.Clear

        /// <summary>Returns the most recently registered animation control button, if any.</summary>
        member _.TryLastAnimationButton() =
            lock gate (fun () ->
                if animationButtons.Count = 0 then None
                else Some animationButtons[animationButtons.Count - 1])

        /// <summary>Registers an audio file referenced by the generated presentation.</summary>
        member _.AddAudioFile(audioFile:string) =
            lock gate (fun () -> audioFiles.Add(audioFile))

        /// <summary>Gets a snapshot of the registered audio files.</summary>
        member _.AudioFiles =
            lock gate (fun () -> audioFiles |> Seq.toList)

    /// <summary>Contains immutable feature options for animation generation.</summary>
    type internal AnimationOptions = {
        CharacterEnabled: bool
        SubtitleEnabled: bool
        VoiceEnabled: bool
    }

    [<RequireQualifiedAccess>]
    module internal AnimationOptions =
        let create(setting:MovieSetting) = {
            CharacterEnabled = (setting.Character = ON)
            SubtitleEnabled = (setting.Subtitle = ON)
            VoiceEnabled = (setting.Voice = ON)
        }

    /// <summary>Stores mutable data accumulated while building a sequence diagram.</summary>
    type internal SequenceDiagramBuilderState() =
        /// <summary>Gets or sets the current terminal position of the life lines.</summary>
        member val TerminalLifeLine = 100.0 with get, set
        /// <summary>Gets or sets the variables already placed in the diagram.</summary>
        member val Variables: list<string * int * float> = [] with get, set
        /// <summary>Gets or sets the active frame stack.</summary>
        member val Frames: list<float * float * float * float> = [] with get, set
        /// <summary>Gets or sets the active branch stack.</summary>
        member val Branches: list<list<string * float>> = [] with get, set

    /// <summary>Contains an immutable snapshot of sequence diagram rendering settings.</summary>
    type internal SequenceDiagramStyleState = {
        TopMargin: float
        LeftMargin: float
        VariableInterval: float
        SingleArrowLength: float
        VariableHeaderWidth: float
        VariableHeaderHeight: float
        LineWidth: float
        ActiveLineWidth: float
        FrameMargin: float
        TimeStep: float
        FrameBorder: float
        ActiveLineColor: string
        LoopFrameColor: string
        BranchFrameColor: string
        SectionFrameColor: string
    }

    [<RequireQualifiedAccess>]
    module internal SequenceDiagramStyleState =
        let Default = {
            TopMargin = 40.0
            LeftMargin = 40.0
            VariableInterval = 150.0
            SingleArrowLength = 37.5
            VariableHeaderWidth = 50.0
            VariableHeaderHeight = 20.0
            LineWidth = 2.0
            ActiveLineWidth = 10.0
            FrameMargin = 10.0
            TimeStep = 10.0
            FrameBorder = 2.0
            ActiveLineColor = "rgba(0, 191, 255, 0.5)"
            LoopFrameColor = "rgb(255, 0, 0)"
            BranchFrameColor = "rgb(0, 180, 0)"
            SectionFrameColor = "rgb(127,0,255)"
        }

    /// State owned by one code-generation operation.
    ///
    /// DSL values capture an explicit scoped view of this state. No ambient or
    /// thread-local context is used; assignments write through the context carried
    /// by their left-hand value.
    type private GenerationState =
        {
            Gate: obj
            Programs: program array
            ContextIds: System.Guid array
            mutable Active: int
            mutable DisplaySection: int
            mutable IsOpenMpUsed: int
            mutable IsOpenAccUsed: int
            Functions: ResizeArray<string>
            GotoLabels: gotoLabelController
            Errors: errorIDController
            Debug: debugController
            Html: HtmlGenerationState
            AnimationOptions: AnimationOptions
            SequenceDiagramGate: obj
            mutable SequenceDiagramStyle: SequenceDiagramStyleState
            SequenceDiagramBuilder: SequenceDiagramBuilderState
        }

    /// <summary>Provides the scoped state and active output program for one generation operation.</summary>
    type GenerationContext private
        (
            state:GenerationState,
            currentIndex:int,
            debug:debugController,
            parallelMode:bool
        ) =
        /// <summary>Creates the state shared by all scoped views of a generation context.</summary>
        static member private CreateState(programs:program list, movieSetting:MovieSetting) =
            let programArray = programs |> List.toArray
            if programArray.Length = 0 then
                invalidArg (nameof programs) "At least one program is required."
            {
                Gate = obj()
                Programs = programArray
                ContextIds = Array.init programArray.Length (fun _ -> System.Guid.NewGuid())
                Active = 1
                DisplaySection = 0
                IsOpenMpUsed = 0
                IsOpenAccUsed = 0
                Functions = ResizeArray<string>()
                GotoLabels = gotoLabelController()
                Errors = errorIDController()
                Debug = debugController()
                Html = HtmlGenerationState()
                AnimationOptions = AnimationOptions.create movieSetting
                SequenceDiagramGate = obj()
                SequenceDiagramStyle = SequenceDiagramStyleState.Default
                SequenceDiagramBuilder = SequenceDiagramBuilderState()
            }

        /// <summary>Creates a generation context using the default movie settings.</summary>
        new(programs:program list) =
            let state =
                GenerationContext.CreateState(programs, MovieSetting.Default)
            GenerationContext(state, 0, state.Debug, false)

        /// <summary>Creates a generation context using the specified movie settings.</summary>
        new(programs:program list, movieSetting:MovieSetting) =
            let state =
                GenerationContext.CreateState(programs, movieSetting)
            GenerationContext(state, 0, state.Debug, false)

        static member internal ForInternalProgram(program:program) =
            GenerationContext [program]

        member private _.EnsureActive() =
            if System.Threading.Volatile.Read(&state.Active) = 0 then
                invalidOp "This GenerationContext is no longer active. Values created in a Compile callback cannot be used outside that callback."

        member internal _.Deactivate() =
            System.Threading.Interlocked.Exchange(&state.Active, 0) |> ignore

        /// <summary>Gets the output programs owned by this generation operation.</summary>
        member this.Programs =
            this.EnsureActive()
            state.Programs

        /// <summary>Gets the index of the active output program for this scoped context.</summary>
        member this.CurrentIndex =
            this.EnsureActive()
            currentIndex

        /// <summary>Gets the active output program.</summary>
        member this.CurrentProgram =
            this.EnsureActive()
            state.Programs[currentIndex]

        /// <summary>
        /// Gets the stable identity of the output target represented by this context.
        /// Scoped debug/parallel views retain this identity, while another program has
        /// a different identity.
        /// </summary>
        member this.ContextId =
            this.EnsureActive()
            state.ContextIds[currentIndex]

        /// <summary>Gets or sets whether generated code should display section information.</summary>
        member _.DisplaySection
            with get () =
                System.Threading.Volatile.Read(&state.DisplaySection) <> 0
            and set value =
                System.Threading.Interlocked.Exchange(
                    &state.DisplaySection,
                    if value then 1 else 0)
                |> ignore

        /// <summary>Gets or sets whether generated code uses OpenMP.</summary>
        member _.IsOpenMpUsed
            with get () =
                System.Threading.Volatile.Read(&state.IsOpenMpUsed) <> 0
            and set value =
                System.Threading.Interlocked.Exchange(
                    &state.IsOpenMpUsed,
                    if value then 1 else 0)
                |> ignore

        /// <summary>Gets or sets whether generated code uses OpenACC.</summary>
        member _.IsOpenAccUsed
            with get () =
                System.Threading.Volatile.Read(&state.IsOpenAccUsed) <> 0
            and set value =
                System.Threading.Interlocked.Exchange(
                    &state.IsOpenAccUsed,
                    if value then 1 else 0)
                |> ignore

        /// <summary>Gets whether the current scoped context is generating a parallel section.</summary>
        member _.IsParallelMode = parallelMode

        /// <summary>Runs an operation in a child context with parallel mode enabled.</summary>
        member this.WithParallelMode(code: GenerationContext -> 'T) : 'T =
            this.EnsureActive()
            code (GenerationContext(state, currentIndex, debug, true))

        /// <summary>Runs an operation in a child context with the specified debug mode.</summary>
        member this.WithDebugMode(enabled:bool, code: GenerationContext -> 'T) : 'T =
            this.EnsureActive()
            let scopedDebug = debugController()
            scopedDebug.setDebugMode enabled
            code (GenerationContext(
                state,
                currentIndex,
                scopedDebug,
                parallelMode))

        /// <summary>Gets a snapshot of the registered function names.</summary>
        member _.Functions =
            lock state.Gate (fun () ->
                state.Functions |> Seq.toList)

        /// <summary>Registers a generated function name.</summary>
        member _.AddFunction(name:string) =
            lock state.Gate (fun () ->
                state.Functions.Add(name))

        /// <summary>Gets the registered function names without duplicates.</summary>
        member _.DistinctFunctions =
            lock state.Gate (fun () ->
                state.Functions |> Seq.distinct |> Seq.toList)

        /// <summary>Gets the goto-label allocator for this generation operation.</summary>
        member _.GotoLabels = state.GotoLabels

        /// <summary>Gets the error identifier allocator for this generation operation.</summary>
        member _.Errors = state.Errors

        /// <summary>Gets the debug controller for the current scoped context.</summary>
        member _.Debug = debug

        /// <summary>Allocates the next unique HTML content number.</summary>
        member internal _.NextContentsNumber() =
            state.Html.NextContentsNumber()

        /// <summary>Allocates the next unique animation sequence number.</summary>
        member internal _.NextAnimationSequenceNumber() =
            state.Html.NextAnimationSequenceNumber()

        /// <summary>Allocates the next unique animation group number.</summary>
        member internal _.NextAnimationGroupNumber() =
            state.Html.NextAnimationGroupNumber()

        /// <summary>Allocates the next unique animated figure number.</summary>
        member internal _.NextFigureNumber() =
            state.Html.NextFigureNumber()

        /// <summary>Allocates the next unique animation page number.</summary>
        member internal _.NextAnimationNumber() =
            state.Html.NextAnimationNumber()

        /// <summary>Gets the current animation page count.</summary>
        member internal _.AnimationCount =
            state.Html.AnimationCount

        /// <summary>Registers animation control button data.</summary>
        member internal _.AddAnimationButton(button) =
            state.Html.AddAnimationButton(button)

        /// <summary>Clears registered animation control button data.</summary>
        member internal _.ClearAnimationButtons() =
            state.Html.ClearAnimationButtons()

        /// <summary>Returns the most recently registered animation control button, if any.</summary>
        member internal _.TryLastAnimationButton() =
            state.Html.TryLastAnimationButton()

        /// <summary>Registers an audio file used by the generated presentation.</summary>
        member internal _.AddAudioFile(audioFile) =
            state.Html.AddAudioFile(audioFile)

        /// <summary>Gets a snapshot of registered audio files.</summary>
        member internal _.AudioFiles =
            state.Html.AudioFiles

        /// <summary>Gets or sets the directory used for generated web content.</summary>
        member internal _.ContentsDirectory
            with get() = state.Html.ContentsDirectory
            and set value = state.Html.ContentsDirectory <- value

        /// <summary>Gets whether character images are enabled.</summary>
        member internal _.CharacterEnabled =
            state.AnimationOptions.CharacterEnabled

        /// <summary>Gets whether subtitles are enabled.</summary>
        member internal _.SubtitleEnabled =
            state.AnimationOptions.SubtitleEnabled

        /// <summary>Gets whether voice playback is enabled.</summary>
        member internal _.VoiceEnabled =
            state.AnimationOptions.VoiceEnabled

        /// <summary>Gets the current immutable sequence diagram style snapshot.</summary>
        member internal _.SequenceDiagramStyle =
            lock state.SequenceDiagramGate (fun () ->
                state.SequenceDiagramStyle)

        /// <summary>Replaces the sequence diagram style as one synchronized operation.</summary>
        member internal _.SetSequenceDiagramStyle(style) =
            lock state.SequenceDiagramGate (fun () ->
                state.SequenceDiagramStyle <- style)

        /// <summary>Gets or sets the current terminal position of sequence diagram life lines.</summary>
        member internal _.TerminalLifeLine
            with get() =
                lock state.SequenceDiagramGate (fun () ->
                    state.SequenceDiagramBuilder.TerminalLifeLine)
            and set value =
                lock state.SequenceDiagramGate (fun () ->
                    state.SequenceDiagramBuilder.TerminalLifeLine <- value)

        /// <summary>Gets or sets the variables placed in the current sequence diagram.</summary>
        member internal _.SequenceVariables
            with get() =
                lock state.SequenceDiagramGate (fun () ->
                    state.SequenceDiagramBuilder.Variables)
            and set value =
                lock state.SequenceDiagramGate (fun () ->
                    state.SequenceDiagramBuilder.Variables <- value)

        /// <summary>Gets or sets the active sequence diagram frame stack.</summary>
        member internal _.SequenceFrames
            with get() =
                lock state.SequenceDiagramGate (fun () ->
                    state.SequenceDiagramBuilder.Frames)
            and set value =
                lock state.SequenceDiagramGate (fun () ->
                    state.SequenceDiagramBuilder.Frames <- value)

        /// <summary>Gets or sets the active sequence diagram branch stack.</summary>
        member internal _.SequenceBranches
            with get() =
                lock state.SequenceDiagramGate (fun () ->
                    state.SequenceDiagramBuilder.Branches)
            and set value =
                lock state.SequenceDiagramGate (fun () ->
                    state.SequenceDiagramBuilder.Branches <- value)

        /// <summary>Creates a scoped context that targets the output program at the specified index.</summary>
        member this.ForProgram(index:int) =
            this.EnsureActive()
            if index < 0 || index >= state.Programs.Length then
                invalidArg (nameof index) $"Program index {index} is outside the valid range."
            GenerationContext(state, index, debug, parallelMode)

        /// <summary>Runs an operation against the specified output program under the context lock.</summary>
        member this.WithProgram(index: int, code: GenerationContext -> 'T) : 'T =
            lock state.Gate (fun () ->
                code (this.ForProgram(index)))

        /// <summary>
        /// Runs one synchronous DSL-generation transaction under the context lock.
        /// Concurrent transactions on the same context are serialized so that
        /// temporary variables, indentation, and multi-statement blocks cannot
        /// interleave. Do not wait for another transaction on this context from
        /// inside the callback.
        /// </summary>
        member this.GenerateAtomically(code: GenerationContext -> 'T) : 'T =
            this.EnsureActive()
            lock state.Gate (fun () -> code this)

        /// <summary>Runs an operation while holding the generation context lock.</summary>
        member internal this.Synchronize(code: GenerationContext -> 'T) =
            this.GenerateAtomically(code)

    /// <summary>
    /// The explicit environment passed to a Compile callback. Numeric execution has
    /// no code-generation context; every other mode exposes one through this wrapper.
    /// </summary>
    type Aqualis (context:GenerationContext option) =
        member _.GenerationContext = context
        member _.IsNumeric = context.IsNone
        member _.Version = "188.0.0.0"
        member internal _.RequireGenerationContext() =
            context
            |> Option.defaultWith (fun () ->
                invalidOp "This operation is not available during Numeric execution.")

    /// Emits raw source text through the output program selected by this environment.
    type ContextEmit internal (environment:Aqualis) =
        let context = environment.RequireGenerationContext()
        member _.writein(text:string) = context.CurrentProgram.codewritein text
        member _.writei(text:string) = context.CurrentProgram.codewritei text
        member _.write(text:string) = context.CurrentProgram.codewrite text
        member _.comment(text:string) = context.CurrentProgram.comment text
            
    [<AutoOpen>]
    module CompilationEnvironmentEmitExtensions =
        type Aqualis with
            member this.emit = ContextEmit(this)

    /// Shared rules for propagating and validating contexts carried by DSL values.
    module internal GenerationContextMerge =
        let sameTarget (left:GenerationContext) (right:GenerationContext) =
            left.ContextId = right.ContextId

        let merge left right =
            match left, right with
            |None, None -> None
            |Some context, None
            |None, Some context -> Some context
            |Some leftContext, Some rightContext
                when sameTarget leftContext rightContext -> Some leftContext
            |Some _, Some _ ->
                invalidOp "Values from different GenerationContext instances cannot be combined."

        let mergeMany contexts =
            contexts |> Seq.fold merge None

        let requireTarget context =
            context
            |> Option.defaultWith (fun () ->
                invalidOp "The assignment target is not associated with a GenerationContext.")

    [<AutoOpen>]
    module aqualisProgram =

        ///<summary>現在生成中のプログラミング言語</summary>
        let funlist_nonoverlap(context:GenerationContext) =
            context.DistinctFunctions
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
                code context
            finally
                context.Deactivate()
                disposePrograms programs

        let makeProgramWithMovieSetting
            (movieSetting:MovieSetting)
            (programInfo: list<string * string * Language>)
            (code: GenerationContext -> 'T)
            : 'T =
            let programs =
                programInfo
                |> List.map (fun (dir, name, language) ->
                    new program(dir, name, language))
            let context = GenerationContext(programs, movieSetting)

            try
                code context
            finally
                context.Deactivate()
                disposePrograms programs

        let write(context:GenerationContext) (s:string) = context.CurrentProgram.codewrite s
        let writei(context:GenerationContext) (s:string) = context.CurrentProgram.codewritei s
        let writen(context:GenerationContext) (s:string) = context.CurrentProgram.codewriten s
        let writein(context:GenerationContext) (s:string) = context.CurrentProgram.codewritein s
        let hwritein(context:GenerationContext) (h:string,s:string) = context.CurrentProgram.codewritein (h,s)
        let eqbr(context:GenerationContext) = writein context "\\\\"
        let language(context:GenerationContext) = context.CurrentProgram.language

        ///<summary>コメント文を生成</summary>
        let comment(context:GenerationContext) s = context.CurrentProgram.comment s

    ///<summary>コード生成の設定</summary>
    type ContextCompiler internal (environment:Aqualis) =
        let context() = environment.RequireGenerationContext()

        ///<summary>言語</summary>
        member _.language = (context()).CurrentProgram.language

        ///<summary>プロジェクト名</summary>
        member _.projectName = (context()).CurrentProgram.projectName

        ///<summary>整数を文字列に変換した時の桁数</summary>
        member _.intFormat = (context()).CurrentProgram.numFormat.iFormat

        ///<summary>整数をn桁の文字列で変換するように設定</summary>
        member _.intFormatSet d = (context()).CurrentProgram.numFormat.setIFormat d

        ///<summary>倍精度浮動小数点をn桁（小数点以下m桁）の文字列で変換するように設定</summary>
        member _.doubleFormat = (context()).CurrentProgram.numFormat.dFormat

        ///<summary>倍精度浮動小数点をn桁（小数点以下m桁）の文字列で変換するように設定</summary>
        member _.doubleFormatSet(n,d) = (context()).CurrentProgram.numFormat.setDFormat(n,d)

        ///<summary>デバッグモードの切り替え</summary>
        member _.set_DebugMode (x:Switch) =
            match x with
            |ON  -> (context()).Debug.setDebugMode true
            |OFF -> (context()).Debug.setDebugMode false

        ///<summary>デバッグモードの切り替え</summary>
        member _.set_DisplaySection (x:Switch) =
            match x with
            |ON  -> (context()).DisplaySection <- true
            |OFF -> (context()).DisplaySection <- false

        ///<summary>codeをデバッグモードで実行</summary>
        member _.debug code =
            let ctx = context()
            ctx.WithDebugMode(true, fun child -> code (Aqualis(Some child)))

        ///<summary>プログラムの実行を強制終了</summary>
        member _.abort() =
            let ctx = context()
            match ctx.CurrentProgram.language with
            |Fortran ->
                writein ctx "stop"
            |C99 ->
                writein ctx "return 1;"
            |LaTeX ->
                writein ctx "stop"
            |HTML ->
                writein ctx "stop"
            |HTMLSequenceDiagram ->
                writein ctx "stop"
            |Python ->
                writein ctx "sys.exit(1)"
            |JavaScript ->
                ()
            |PHP ->
                ()
            |Numeric ->
                ()

        ///<summary>何かのキーを押すまで実行を一時停止</summary>
        member _.stop() =
            let ctx = context()
            match ctx.CurrentProgram.language with
            |Fortran ->
                writein ctx "read *, \n"
            |C99 ->
                writein ctx "getchar();\n"
            |LaTeX ->
                writein ctx "stop\n"
            |HTML ->
                writein ctx "stop\n"
            |HTMLSequenceDiagram ->
                writein ctx "stop\n"
            |Python ->
                writein ctx "input()"
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
        member _.incld(s:string) =
            (context()).CurrentProgram.hlist.add s

        /// <summary>
        /// コンパイルオプションを追加
        /// </summary>
        /// <param name="t">オプション</param>
        member _.option(t:string) =
            (context()).CurrentProgram.olist.add("-"+t)

    [<AutoOpen>]
    module CompilationEnvironmentCompilerExtensions =
        type Aqualis with
            member this.compiler = ContextCompiler(this)
