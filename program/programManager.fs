//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    open System
    open System.IO
    open System.Threading
   
    /// Generation context that owns output, variables, diagnostics, and target-language state.
    type Aqualis private (outputdir:string option,pjname:string option,lang:Language,isNeutral:bool,publishAtomically:bool,writerDirectory:string option,diagnostics:DiagnosticBag option) =
        let contextId = System.Guid.NewGuid()
        let diagnosticBag =
            diagnostics
            |> Option.orElseWith DiagnosticScope.tryCurrent
            |> Option.defaultWith DiagnosticBag
        let intermediateDirectory =
            lazy
                let path =
                    Path.Combine(
                        Path.GetTempPath(),
                        "aqualis-" + contextId.ToString("N"))
                Directory.CreateDirectory(path) |> ignore
                path
        let cwriter = 
            match (writerDirectory |> Option.orElse outputdir),pjname with
            |Some dir,Some filename ->
                let targetPath = Path.Combine(dir, filename)
                let wr =
                    if publishAtomically then codeWriter.CreateAtomic(targetPath,2,lang)
                    else new codeWriter(targetPath,2,lang)
                Some wr
            |_ -> None

        /// 構造体
        let structData = structure()
        let sequenceGate = obj()
        let mutable active = 1
        let ensureActive() =
            if active = 0 then
                invalidOp "This Aqualis context is no longer active. Values created in a generation callback cannot be used outside that callback."
        let withWriter (action:codeWriter -> unit) =
            ensureActive()
            match cwriter with
            |Some writer -> action writer
            |None -> ()
        let mutable displaySection = false
        let mutable isOpenMpUsed = false
        let mutable isOpenAccUsed = false
        let mutable parallelMode = false
        let mutable terminalLifeLine = 100.0
        let mutable sequenceVariables : (string*int*float) list = []
        let mutable sequenceFrames : (float*float*float*float) list = []
        let mutable sequenceBranches : ((string*float) list) list = []
        let mutable phpFileScopeCounter = 0
        /// Gets or sets the endpoint of the terminal lifeline in sequence diagrams.
        member _.TerminalLifeLine with get() = terminalLifeLine and set(v) = terminalLifeLine <- v
        /// シーケンス図に描画済み変数リスト
        member _.SequenceVariables with get() = sequenceVariables and set(v) = sequenceVariables <- v
        /// Gets or sets sequence-diagram frame bounds.
        member _.SequenceFrames with get() = sequenceFrames and set(v) = sequenceFrames <- v
        /// Gets or sets sequence-diagram branch labels.
        member _.SequenceBranches with get() = sequenceBranches and set(v) = sequenceBranches <- v
        /// フレーム枠座標スタックリスト
        member _.FrameStack with get() = sequenceFrames and set(v) = sequenceFrames <- v
        /// 条件分岐枠スタックリスト
        member _.BranchStack with get() = sequenceBranches and set(v) = sequenceBranches <- v 
        new(outputdir:string option,pjname:string option,lang:Language) =
            new Aqualis(outputdir,pjname,lang,false,false,None,None)
        /// Creates a generation context with a diagnostic bag.
        static member internal CreateWithDiagnostics(outputdir,pjname,lang,diagnostics) =
            new Aqualis(outputdir,pjname,lang,false,false,None,Some diagnostics)
        /// Gets the three-part assembly version.
        static member Version = typeof<Aqualis>.Assembly.GetName().Version.ToString(3)
        /// Creates a neutral context without an output writer.
        static member BlankWriter(lang:Language) = new Aqualis(None,None,lang,true,false,None,None)
        /// Gets the configured output directory, if any.
        member _.Dir with get() = outputdir
        /// Gets the active writer directory or configured output directory.
        member internal _.GeneratedOutputDirectory = writerDirectory |> Option.orElse outputdir
        /// Gets the configured project name, if any.
        member _.ProjectName with get() = pjname
        /// Gets the output file path when directory and project name are known.
        member _.CodeFile with get() = match outputdir,pjname with |Some dir,Some src -> Some(Path.Combine(dir, src)) |_ -> None
        /// Gets the unique identity of this generation context.
        member _.ContextId with get() = contextId
        /// Gets the diagnostic collection owned by this context.
        member _.Diagnostics = diagnosticBag
        /// Adds a generation diagnostic with language and project location.
        member internal _.ReportDiagnostic(code,severity,message,operation,properties) =
            diagnosticBag.Report {
                Code = code
                Severity = severity
                Message = message
                Location = Some(Generation(lang, pjname, operation))
                Properties = properties
            }
        /// Gets or creates a temporary directory owned by this context.
        member internal _.IntermediateDirectory = intermediateDirectory.Value
        /// Gets whether this context can be merged with another target context.
        member internal _.IsNeutral = isNeutral
        /// Gets or sets parallel-generation mode.
        member internal _.ParallelMode with get() = parallelMode and set v = parallelMode <- v
        /// Gets the lock used for sequence counters.
        member internal _.SequenceGate = sequenceGate
        /// Allocates a unique suffix for generated PHP file-operation variables.
        member internal _.NextPhpFileScopeNumber() =
            lock sequenceGate (fun () ->
                phpFileScopeCounter <- phpFileScopeCounter + 1
                phpFileScopeCounter)
        /// Gets whether the context remains active.
        member _.Active = active
        /// Gets or sets whether generated section labels are displayed.
        member _.DisplaySection with get() = displaySection and set v = displaySection <- v
        /// Gets or sets whether generated code requires OpenMP.
        member _.IsOpenMpUsed with get() = isOpenMpUsed and set v = isOpenMpUsed <- v
        /// Gets or sets whether generated code requires OpenACC.
        member _.IsOpenAccUsed with get() = isOpenAccUsed and set v = isOpenAccUsed <- v

        ///<summary>言語設定</summary>
        member val internal language = lang with get

        ///<summary>出力先ディレクトリ</summary>
        member val internal dir = outputdir with get

        ///<summary>プロジェクト名</summary>
        member val internal projectName = pjname with get
      
        ///<summary>ライブラリの使用時に必要なヘッダーファイル</summary>
        member val internal hlist = new UniqueList()

        ///<summary>Python出力で必要なimport</summary>
        member val internal pythonImports = new PythonImportController()

        ///<summary>HTML出力へ明示的に追加する外部資産</summary>
        member val internal htmlAssets = new HtmlAssetController()

        ///<summary>ライブラリの使用時に必要なモジュールファイル</summary>
        member val internal mlist = new UniqueList()

        ///<summary>ライブラリの使用時に必要なextern指定子</summary>
        member val internal elist = new UniqueList()

        ///<summary>定義された関数のリスト</summary>
        member val internal flist = new UniqueList()

        ///<summary>コンパイル時に必要な他のソースファイル</summary>
        member val internal slist = new UniqueList()

        ///<summary>コンパイル時に必要なライブラリ・オプション</summary>
        member val internal olist = new UniqueList()
        /// Gets numeric formatting configuration.
        member val internal numFormat = numericFormatController lang with get
        /// Gets the function-argument controller.
        member val internal arg = argumentController lang with get
        /// Gets the names of functions registered for generation.
        member val internal Functions: ResizeArray<string> = new ResizeArray<string>() with get
        /// Gets generated jump-label state.
        member val internal GotoLabels = gotoLabelController() with get
        /// Gets generated error identifiers.
        member val internal Errors = errorIDController() with get
        /// Gets generated debug-mode state.
        member val internal Debug = debugController() with get
        /// Writes a source comment through the active writer.
        member _.comment(s:string) = withWriter (fun writer -> writer.comment s)
        /// Writes an HTML markup comment through the active writer.
        member internal _.commentHtmlMarkup(s:string) = withWriter (fun writer -> writer.commentHtmlMarkup s)
        /// Writes source text without indentation or a trailing newline.
        member _.codewrite(s:string) = withWriter (fun writer -> writer.codewrite s)
        /// Writes source text without indentation or a trailing newline.
        member _.write(s:string) = withWriter (fun writer -> writer.codewrite s)
        /// Writes source text without indentation and appends a newline.
        member _.writen(s:string) = withWriter (fun writer -> writer.codewriten s)
        /// Writes indented source text and appends a newline.
        member _.writein(s:string) = withWriter (fun writer -> writer.codewritein s)
        /// Writes indented source text without a trailing newline.
        member _.writei(s:string) = withWriter (fun writer -> writer.codewritei s)
        /// Writes indented source text without a trailing newline.
        member _.codewritei(s:string) = withWriter (fun writer -> writer.codewritei s)
        /// Writes source text without indentation and appends a newline.
        member _.codewriten(s:string) = withWriter (fun writer -> writer.codewriten s)
        /// Writes indented source text and appends a newline.
        member _.codewritein(s:string) = withWriter (fun writer -> writer.codewritein s)
        /// Writes indented source text with a line prefix and appends a newline.
        member _.codewritein(h:string,s:string) = withWriter (fun writer -> writer.codewritein (h,s))
        /// Emits a PHP statement, rejecting non-PHP contexts.
        member internal _.writePhpStatement(statement:string) =
            ensureActive()
            if lang <> PHP then
                invalidOp "PHP statements can only be emitted by a PHP generation context."
            withWriter (fun writer -> writer.codewritein("<?php ", statement + " ?>"))
        /// Writes unformatted source text through the active writer.
        member internal _.writeRaw(s:string) = withWriter (fun writer -> writer.cwrite s)
        /// Prepends raw source text to the generated output.
        member internal _.prependRaw(s:string) =
            ensureActive()
            match cwriter with
            |Some writer -> writer.prepend s
            |None -> invalidOp "A writerless Aqualis context cannot register generated prologue code."
        /// Captures code emitted by a callback and returns it with the callback result.
        member internal _.captureCode(action:unit -> 'T) =
            ensureActive()
            match cwriter with
            |Some writer -> writer.capture action
            |None ->
                let result = action()
                "", result
        /// Increases the writer indentation level.
        member _.indentInc() = withWriter (fun writer -> writer.indent.inc())
        /// Decreases the writer indentation level.
        member _.indentDec() = withWriter (fun writer -> writer.indent.dec())
        /// Enables appending to the writer output.
        member _.appendOpen() = withWriter (fun writer -> writer.appendOpen())
        /// Validates generated variable names and closes the output writer.
        member this.close() =
            this.validateGeneratedVariableNames()
            withWriter (fun writer -> writer.close())
        /// Publishes atomically generated output.
        member internal _.publish() =
            ensureActive()
            match cwriter with
            |Some writer -> writer.publish()
            |None -> invalidOp "A writerless Aqualis context cannot publish output."
        /// Closes the writer and reads all generated source text, if available.
        member _.allCodes with get() =
            ensureActive()
            match cwriter with
            |Some wr -> 
                wr.close()
                Some <| File.ReadAllText wr.FilePath
            |None ->
                None
        /// Deletes the generated output file, if one exists.
        member _.delete() = 
            match cwriter with
            |Some wr -> wr.delete()
            |None -> ()
        /// Gets the generated structure registry.
        member _.cstr with get() = structData

        interface System.IDisposable with
            /// Releases resources owned by this generation context.
            member _.Dispose() =
                active <- 0
                match cwriter with
                |Some wr -> (wr :> System.IDisposable).Dispose()
                |None -> ()
                if intermediateDirectory.IsValueCreated && Directory.Exists(intermediateDirectory.Value) then
                    Directory.Delete(intermediateDirectory.Value, true)

        /// Tests whether two contexts have the same identity.
        static member sameTarget (left:Aqualis) (right:Aqualis) =
            left.ContextId = right.ContextId

        /// Combines compatible generation contexts, treating neutral contexts as identity values.
        static member merge (left:Aqualis) (right:Aqualis) =
            match left.language,right.language with
            |_,Numeric |Numeric,_ -> ()
            |a,b when a=b -> ()
            |_ ->
                invalidOp "Values from different language cannot be combined."
            match left.IsNeutral, right.IsNeutral with
            |true, true -> left
            |true, false -> right
            |false, true -> left
            |false, false when Aqualis.sameTarget left right -> left
            |false, false ->
                invalidOp (
                    "Values from different GenerationContext instances cannot be combined. " +
                    $"Left context: {left.ContextId}; right context: {right.ContextId}.")

        /// Folds a collection of compatible contexts into one context.
        static member mergeMany contexts =
            match contexts |> Seq.toList with
            |[] -> Aqualis.BlankWriter Numeric
            |first::rest -> rest |> List.fold Aqualis.merge first
        /// Returns an optional target context or raises when it is absent.
        static member requireTarget context =
            context
            |> Option.defaultWith (fun () ->
                invalidOp "The assignment target is not associated with a GenerationContext.")
        /// Checks that the generation context is still active.
        member private this.EnsureActive() =
            ensureActive()
        /// <summary>Runs an operation in a child context with parallel mode enabled.</summary>
        member this.WithParallelMode(code:Aqualis -> 'T) : 'T =
            this.EnsureActive()
            let previousMode = this.ParallelMode
            this.ParallelMode <- true
            try
                code this
            finally
                this.ParallelMode <- previousMode

        /// Gets the target language.
        member _.Language with get() = lang

        ///<summary>整数型変数リスト</summary>
        member val internal i0 = varGenerator (
            match lang with
            |LaTeX|HTML|HTMLSequenceDiagram -> fun n -> "i^{("+n.ToString()+")}"
            |PHP -> fun n -> "$i0"+n.ToString "000"
            |_ -> fun n -> "i0"+n.ToString "000"
            ) with get

        ///<summary>倍精度浮動小数点型変数リスト</summary>
        member val internal d0 = varGenerator (
            match lang with
            |LaTeX|HTML|HTMLSequenceDiagram -> fun n -> "d^{("+n.ToString()+")}"
            |PHP -> fun n -> "$d0"+n.ToString "000"
            |_ -> fun n -> "d0"+n.ToString "000"
            ) with get

        ///<summary>複素数型変数リスト</summary>
        member val internal z0 = varGenerator (
            match lang with
            |LaTeX|HTML|HTMLSequenceDiagram -> fun n -> "z^{("+n.ToString()+")}"
            |PHP -> fun n -> "$z0"+n.ToString "000"
            |_ -> fun n -> "z0"+n.ToString "000"
            ) with get

        ///<summary>文字変数リスト</summary>
        member val internal c0 = varGenerator (
            match lang with
            |LaTeX|HTML|HTMLSequenceDiagram -> fun n -> "c^{("+n.ToString()+")}"
            |PHP -> fun n -> "$c0"+n.ToString "000"
            |_ -> fun n -> "c0"+n.ToString "000"
            ) with get

        ///<summary>文字列変数リスト</summary>
        member val internal t0 = varGenerator (
            match lang with
            |LaTeX|HTML|HTMLSequenceDiagram -> fun n -> "t^{("+n.ToString()+")}"
            |PHP -> fun n -> "$t0"+n.ToString "000"
            |_ -> fun n -> "t0"+n.ToString "000"
            ) with get

        ///<summary>ファイルポインタリスト</summary>
        member val internal f0 = varGenerator (
            match lang with
            |LaTeX|HTML|HTMLSequenceDiagram -> fun n -> "f^{("+n.ToString()+")}"
            |PHP -> fun n -> "$f0"+n.ToString "000"
            |_ -> fun n -> "f0"+n.ToString "000"
            ) with get

        ///<summary>整数型1次元配列リスト</summary>
        member val internal i1 = varGenerator (
            match lang with
            |LaTeX|HTML|HTMLSequenceDiagram -> fun n -> "\\dot{i}^{("+n.ToString()+")}"
            |PHP -> fun n -> "$i1"+n.ToString "000"
            |_ -> fun n -> "i1"+n.ToString "000"
            ) with get

        ///<summary>倍精度浮動小数点型1次元配列リスト</summary>
        member val internal d1 = varGenerator (
            match lang with
            |LaTeX|HTML|HTMLSequenceDiagram -> fun n -> "\\dot{d}^{("+n.ToString()+")}"
            |PHP -> fun n -> "$d1"+n.ToString "000"
            |_ -> fun n -> "d1"+n.ToString "000"
            ) with get

        ///<summary>複素数型1次元配列リスト</summary>
        member val internal z1 = varGenerator (
            match lang with
            |LaTeX|HTML|HTMLSequenceDiagram -> fun n -> "\\dot{z}^{("+n.ToString()+")}"
            |PHP -> fun n -> "$z1"+n.ToString "000"
            |_ -> fun n -> "z1"+n.ToString "000"
            ) with get

        ///<summary>整数型2次元配列リスト</summary>
        member val internal i2 = varGenerator (
            match lang with
            |LaTeX|HTML|HTMLSequenceDiagram -> fun n -> "\\ddot{i}^{("+n.ToString()+")}"
            |PHP -> fun n -> "$i2"+n.ToString "000"
            |_ -> fun n -> "i2"+n.ToString "000"
            ) with get

        ///<summary>倍精度浮動小数点型2次元配列リスト</summary>
        member val internal d2 = varGenerator (
            match lang with
            |LaTeX|HTML|HTMLSequenceDiagram -> fun n -> "\\ddot{d}^{("+n.ToString()+")}"
            |PHP -> fun n -> "$d2"+n.ToString "000"
            |_ -> fun n -> "d2"+n.ToString "000"
            ) with get

        ///<summary>複素数型2次元配列リスト</summary>
        member val internal z2 = varGenerator (
            match lang with
            |LaTeX|HTML|HTMLSequenceDiagram -> fun n -> "\\ddot{z}^{("+n.ToString()+")}"
            |PHP -> fun n -> "$z2"+n.ToString "000"
            |_ -> fun n -> "z2"+n.ToString "000"
            ) with get

        ///<summary>整数型3次元配列リスト</summary>
        member val internal i3 = varGenerator (
            match lang with
            |LaTeX|HTML|HTMLSequenceDiagram -> fun n -> "\\dddot{i}^{("+n.ToString()+")}"
            |PHP -> fun n -> "$i3"+n.ToString "000"
            |_ -> fun n -> "i3"+n.ToString "000"
            ) with get

        ///<summary>倍精度浮動小数点型3次元配列リスト</summary>
        member val internal d3 = varGenerator (
            match lang with
            |LaTeX|HTML|HTMLSequenceDiagram -> fun n -> "\\dddot{d}^{("+n.ToString()+")}"
            |PHP -> fun n -> "$d3"+n.ToString "000"
            |_ -> fun n -> "d3"+n.ToString "000"
            ) with get

        ///<summary>複素数型3次元配列リスト</summary>
        member val internal z3 = varGenerator (
            match lang with
            |LaTeX|HTML|HTMLSequenceDiagram -> fun n -> "\\dddot{z}^{("+n.ToString()+")}"
            |PHP -> fun n -> "$z3"+n.ToString "000"
            |_ -> fun n -> "z3"+n.ToString "000"
            ) with get

        ///<summary>定義された変数リスト</summary>
        member val cvar = varCollector(lang, diagnosticBag) with get

        /// Validates names of generated variables.
        member private this.validateGeneratedVariableNames() =
            if lang <> Numeric && lang <> LaTeX && lang <> HTML && lang <> HTMLSequenceDiagram then
                let comparer =
                    if lang = Fortran then StringComparer.OrdinalIgnoreCase
                    else StringComparer.Ordinal
                let names = System.Collections.Generic.HashSet<string>(comparer)
                let add name =
                    if not (names.Add name) then
                        invalidArg "name" ("Variable '" + name + "' conflicts with a generated variable name.")
                let addArray name =
                    add name
                    add (name + "_size")
                for _,shape,name,_ in this.cvar.list do
                    match shape with
                    |A0 -> add name
                    |_ -> addArray name
                for generator in [this.i0; this.d0; this.z0; this.c0] do
                    for name in generator.varList do add name
                for generator in [this.i1; this.d1; this.z1; this.i2; this.d2; this.z2; this.i3; this.d3; this.z3] do
                    for name in generator.varList do addArray name

        /// Gets the collector for private variables.
        member val varPrivate = varCollector(lang, diagnosticBag) with get

        /// Gets the collector for copied input variables.
        member val varCopyIn = varCollector(lang, diagnosticBag) with get

        /// Gets the collector for copied output variables.
        member val varCopyOut = varCollector(lang, diagnosticBag) with get
        /// Runs a callback with a context that is disposed afterward.
        static member internal runWithOwnedContext
            (createContext: unit -> Aqualis)
            (code: Aqualis -> 'T)
            : 'T =
            use context = createContext()
            code context

        /// Runs a callback and publishes its atomic output.
        static member internal runWithOwnedAtomicContext
            (createContext: unit -> Aqualis)
            (code: Aqualis -> 'T)
            : 'T =
            use context = createContext()
            let result = code context
            context.publish()
            result

        /// Creates and publishes an atomic program in its project directory.
        static member internal makeAtomicProgramWithContext
            (programInfo: string * string * Language)
            (code: Aqualis -> 'T)
            : 'T =
            let dir,_,_ = programInfo
            Aqualis.makeAtomicProgramInDirectoryWithContext programInfo dir code

        /// Creates and publishes an atomic program in a specified writer directory.
        static member internal makeAtomicProgramInDirectoryWithContext
            (programInfo: string * string * Language)
            (writerDirectory:string)
            (code: Aqualis -> 'T)
            : 'T =
            Aqualis.runWithOwnedAtomicContext
                (fun () ->
                    let dir, name, language = programInfo
                    new Aqualis(
                        Some dir,
                        Some name,
                        language,
                        false,
                        true,
                        Some writerDirectory,
                        None))
                code

        /// Runs an intermediate program and deletes its temporary output.
        static member internal makeIntermediateProgramWithContext
            (programInfo: string * string * Language)
            (code: Aqualis -> 'T)
            : 'T =
            let dir, name, language = programInfo
            use context = new Aqualis(Some dir, Some name, language)
            try
                code context
            finally
                context.delete()

        /// Runs an intermediate program in a specified directory.
        static member internal makeIntermediateProgramInDirectoryWithContext
            (programInfo: string * string * Language)
            (writerDirectory:string)
            (code: Aqualis -> 'T)
            : 'T =
            let dir, name, language = programInfo
            use context =
                new Aqualis(
                    Some dir,
                    Some name,
                    language,
                    false,
                    false,
                    Some writerDirectory,
                    None)
            try
                code context
            finally
                context.delete()

        /// Runs a callback in a new context and atomically publishes its output.
        static member makeProgramWithContext
            (programInfo: string * string * Language)
            (code: Aqualis -> 'T)
            : 'T =
            Aqualis.makeAtomicProgramWithContext programInfo code

        /// Runs a callback with a context that does not own an output writer.
        static member internal runWithWriterlessContext
            (language:Language)
            (code:Aqualis -> 'T)
            : 'T =
            Aqualis.runWithOwnedContext
                (fun () -> new Aqualis(None, None, language))
                code

    /// Adds generation settings and Python import configuration to contexts.
    [<AutoOpen>]
    module SettingExtensions =
        /// Reserved Python keywords for generated identifiers.
        let private pythonKeywords =
            set [
                "False"; "None"; "True"; "and"; "as"; "assert"; "async"
                "await"; "break"; "class"; "continue"; "def"; "del"
                "elif"; "else"; "except"; "finally"; "for"; "from"
                "global"; "if"; "import"; "in"; "is"; "lambda"
                "nonlocal"; "not"; "or"; "pass"; "raise"; "return"
                "try"; "while"; "with"; "yield" ]

        /// Checks whether text is a valid Python identifier.
        let private isPythonIdentifier (value:string) =
            not (String.IsNullOrWhiteSpace value) &&
            (Char.IsLetter(value[0]) || value[0] = '_') &&
            (value |> Seq.skip 1 |> Seq.forall (fun character -> Char.IsLetterOrDigit character || character = '_')) &&
            not (Set.contains value pythonKeywords)

        /// Validates a generated Python module name.
        let private validatePythonModuleName (moduleName:string) =
            if isNull moduleName then nullArg (nameof moduleName)
            if not (
                moduleName.Split('.')
                |> Array.forall isPythonIdentifier) then
                invalidArg (nameof moduleName) "A Python module name must contain valid dot-separated identifiers."
            moduleName

        /// Validates a generated Python symbol name.
        let private validatePythonSymbol (symbol:string) =
            if isNull symbol then nullArg (nameof symbol)
            if not (isPythonIdentifier symbol) then
                invalidArg (nameof symbol) "A Python import symbol must be a valid identifier."
            symbol

        /// Registers explicit imports for generated Python source.
        type PythonImportSettings internal (c:Aqualis) =
            let ensurePython() =
                if c.language <> Python then
                    raise (NotSupportedException("Python imports are only supported by Python generation contexts."))

            /// Registers a validated Python module import.
            member _.ImportModule(moduleName:string) =
                ensurePython()
                c.pythonImports.RequireModule(validatePythonModuleName moduleName)

            /// Registers a validated symbol import from a Python module.
            member _.ImportFrom(moduleName:string, symbol:string) =
                ensurePython()
                c.pythonImports.RequireSymbol(
                    validatePythonModuleName moduleName,
                    validatePythonSymbol symbol)

        /// Configures diagnostics, formatting, and compiler options.
        type AqualisSetting(c:Aqualis) =
            /// Enables or disables debug-mode checks in generated code.
            member _.DebugMode (x:Switch) =
                match x with
                |ON -> c.Debug.setDebugMode true
                |OFF -> c.Debug.setDebugMode false
            /// Sets the format used when converting integers to strings.
            member _.IntToStringFormat x = c.numFormat.setIFormat x
            /// Sets the total and fractional digits used for real-number strings.
            member _.DoubleToStringFormat x = c.numFormat.setDFormat x
            /// Adds a compiler option, prefixing it with a hyphen.
            member _.Option x = c.olist.add ("-"+x)
            
        type Aqualis with
            /// Gets settings bound to this context.
            member this.Setting = AqualisSetting this
            /// Gets explicit Python import settings bound to this context.
            member this.Python = PythonImportSettings this
