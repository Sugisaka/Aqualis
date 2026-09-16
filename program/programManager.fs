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
        member _.TerminalLifeLine with get() = terminalLifeLine and set(v) = terminalLifeLine <- v
        /// シーケンス図に描画済み変数リスト
        member _.SequenceVariables with get() = sequenceVariables and set(v) = sequenceVariables <- v
        member _.SequenceFrames with get() = sequenceFrames and set(v) = sequenceFrames <- v
        member _.SequenceBranches with get() = sequenceBranches and set(v) = sequenceBranches <- v
        /// フレーム枠座標スタックリスト
        member _.FrameStack with get() = sequenceFrames and set(v) = sequenceFrames <- v
        /// 条件分岐枠スタックリスト
        member _.BranchStack with get() = sequenceBranches and set(v) = sequenceBranches <- v 
        new(outputdir:string option,pjname:string option,lang:Language) =
            new Aqualis(outputdir,pjname,lang,false,false,None,None)
        static member internal CreateWithDiagnostics(outputdir,pjname,lang,diagnostics) =
            new Aqualis(outputdir,pjname,lang,false,false,None,Some diagnostics)
        static member Version = typeof<Aqualis>.Assembly.GetName().Version.ToString(3)
        static member BlankWriter(lang:Language) = new Aqualis(None,None,lang,true,false,None,None)
        member _.Dir with get() = outputdir
        member internal _.GeneratedOutputDirectory = writerDirectory |> Option.orElse outputdir
        member _.ProjectName with get() = pjname
        member _.CodeFile with get() = match outputdir,pjname with |Some dir,Some src -> Some(Path.Combine(dir, src)) |_ -> None
        member _.ContextId with get() = contextId
        member _.Diagnostics = diagnosticBag
        member internal _.ReportDiagnostic(code,severity,message,operation,properties) =
            diagnosticBag.Report {
                Code = code
                Severity = severity
                Message = message
                Location = Some(Generation(lang, pjname, operation))
                Properties = properties
            }
        member internal _.IntermediateDirectory = intermediateDirectory.Value
        member internal _.IsNeutral = isNeutral
        member internal _.ParallelMode with get() = parallelMode and set v = parallelMode <- v
        member internal _.SequenceGate = sequenceGate
        /// Allocates a unique suffix for generated PHP file-operation variables.
        member internal _.NextPhpFileScopeNumber() =
            lock sequenceGate (fun () ->
                phpFileScopeCounter <- phpFileScopeCounter + 1
                phpFileScopeCounter)
        member _.Active = active
        member _.DisplaySection with get() = displaySection and set v = displaySection <- v
        member _.IsOpenMpUsed with get() = isOpenMpUsed and set v = isOpenMpUsed <- v
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
        member val internal numFormat = numericFormatController lang with get
        member val internal arg = argumentController lang with get
        member val internal Functions: ResizeArray<string> = new ResizeArray<string>() with get
        member val internal GotoLabels = gotoLabelController() with get
        member val internal Errors = errorIDController() with get
        member val internal Debug = debugController() with get
        member _.comment(s:string) = withWriter (fun writer -> writer.comment s)
        member internal _.commentHtmlMarkup(s:string) = withWriter (fun writer -> writer.commentHtmlMarkup s)
        member _.codewrite(s:string) = withWriter (fun writer -> writer.codewrite s)
        member _.write(s:string) = withWriter (fun writer -> writer.codewrite s)
        member _.writen(s:string) = withWriter (fun writer -> writer.codewriten s)
        member _.writein(s:string) = withWriter (fun writer -> writer.codewritein s)
        member _.writei(s:string) = withWriter (fun writer -> writer.codewritei s)
        member _.codewritei(s:string) = withWriter (fun writer -> writer.codewritei s)
        member _.codewriten(s:string) = withWriter (fun writer -> writer.codewriten s)
        member _.codewritein(s:string) = withWriter (fun writer -> writer.codewritein s)
        member _.codewritein(h:string,s:string) = withWriter (fun writer -> writer.codewritein (h,s))
        member internal _.writePhpStatement(statement:string) =
            ensureActive()
            if lang <> PHP then
                invalidOp "PHP statements can only be emitted by a PHP generation context."
            withWriter (fun writer -> writer.codewritein("<?php ", statement + " ?>"))
        member internal _.writeRaw(s:string) = withWriter (fun writer -> writer.cwrite s)
        member internal _.prependRaw(s:string) =
            ensureActive()
            match cwriter with
            |Some writer -> writer.prepend s
            |None -> invalidOp "A writerless Aqualis context cannot register generated prologue code."
        member internal _.captureCode(action:unit -> 'T) =
            ensureActive()
            match cwriter with
            |Some writer -> writer.capture action
            |None ->
                let result = action()
                "", result
        member _.indentInc() = withWriter (fun writer -> writer.indent.inc())
        member _.indentDec() = withWriter (fun writer -> writer.indent.dec())
        member _.appendOpen() = withWriter (fun writer -> writer.appendOpen())
        member _.close() = withWriter (fun writer -> writer.close())
        member internal _.publish() =
            ensureActive()
            match cwriter with
            |Some writer -> writer.publish()
            |None -> invalidOp "A writerless Aqualis context cannot publish output."
        member _.allCodes with get() =
            ensureActive()
            match cwriter with
            |Some wr -> 
                wr.close()
                Some <| File.ReadAllText wr.FilePath
            |None ->
                None
        member _.delete() = 
            match cwriter with
            |Some wr -> wr.delete()
            |None -> ()
        member _.cstr with get() = structData

        interface System.IDisposable with
            member _.Dispose() =
                active <- 0
                match cwriter with
                |Some wr -> (wr :> System.IDisposable).Dispose()
                |None -> ()
                if intermediateDirectory.IsValueCreated && Directory.Exists(intermediateDirectory.Value) then
                    Directory.Delete(intermediateDirectory.Value, true)

        static member sameTarget (left:Aqualis) (right:Aqualis) =
            left.ContextId = right.ContextId

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

        static member mergeMany contexts =
            match contexts |> Seq.toList with
            |[] -> Aqualis.BlankWriter Numeric
            |first::rest -> rest |> List.fold Aqualis.merge first
        static member requireTarget context =
            context
            |> Option.defaultWith (fun () ->
                invalidOp "The assignment target is not associated with a GenerationContext.")
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

        member val varPrivate = varCollector(lang, diagnosticBag) with get

        member val varCopyIn = varCollector(lang, diagnosticBag) with get

        member val varCopyOut = varCollector(lang, diagnosticBag) with get
        static member internal runWithOwnedContext
            (createContext: unit -> Aqualis)
            (code: Aqualis -> 'T)
            : 'T =
            use context = createContext()
            code context

        static member internal runWithOwnedAtomicContext
            (createContext: unit -> Aqualis)
            (code: Aqualis -> 'T)
            : 'T =
            use context = createContext()
            let result = code context
            context.publish()
            result

        static member internal makeAtomicProgramWithContext
            (programInfo: string * string * Language)
            (code: Aqualis -> 'T)
            : 'T =
            let dir,_,_ = programInfo
            Aqualis.makeAtomicProgramInDirectoryWithContext programInfo dir code

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

        static member makeProgramWithContext
            (programInfo: string * string * Language)
            (code: Aqualis -> 'T)
            : 'T =
            Aqualis.makeAtomicProgramWithContext programInfo code

        static member internal runWithWriterlessContext
            (language:Language)
            (code:Aqualis -> 'T)
            : 'T =
            Aqualis.runWithOwnedContext
                (fun () -> new Aqualis(None, None, language))
                code

    [<AutoOpen>]
    module SettingExtensions =
        let private pythonKeywords =
            set [
                "False"; "None"; "True"; "and"; "as"; "assert"; "async"
                "await"; "break"; "class"; "continue"; "def"; "del"
                "elif"; "else"; "except"; "finally"; "for"; "from"
                "global"; "if"; "import"; "in"; "is"; "lambda"
                "nonlocal"; "not"; "or"; "pass"; "raise"; "return"
                "try"; "while"; "with"; "yield" ]

        let private isPythonIdentifier (value:string) =
            not (String.IsNullOrWhiteSpace value) &&
            (Char.IsLetter(value[0]) || value[0] = '_') &&
            (value |> Seq.skip 1 |> Seq.forall (fun character -> Char.IsLetterOrDigit character || character = '_')) &&
            not (Set.contains value pythonKeywords)

        let private validatePythonModuleName (moduleName:string) =
            if isNull moduleName then nullArg (nameof moduleName)
            if not (
                moduleName.Split('.')
                |> Array.forall isPythonIdentifier) then
                invalidArg (nameof moduleName) "A Python module name must contain valid dot-separated identifiers."
            moduleName

        let private validatePythonSymbol (symbol:string) =
            if isNull symbol then nullArg (nameof symbol)
            if not (isPythonIdentifier symbol) then
                invalidArg (nameof symbol) "A Python import symbol must be a valid identifier."
            symbol

        type PythonImportSettings internal (c:Aqualis) =
            let ensurePython() =
                if c.language <> Python then
                    raise (NotSupportedException("Python imports are only supported by Python generation contexts."))

            member _.ImportModule(moduleName:string) =
                ensurePython()
                c.pythonImports.RequireModule(validatePythonModuleName moduleName)

            member _.ImportFrom(moduleName:string, symbol:string) =
                ensurePython()
                c.pythonImports.RequireSymbol(
                    validatePythonModuleName moduleName,
                    validatePythonSymbol symbol)

        type AqualisSetting(c:Aqualis) =
            ///<summary>デバッグモード設定</summary>
            member _.DebugMode (x:Switch) =
                match x with
                |ON -> c.Debug.setDebugMode true
                |OFF -> c.Debug.setDebugMode false
            ///<summary>整数型を文字列に変換するときの桁数</summary>
            member _.IntToStringFormat x = c.numFormat.setIFormat x
            ///<summary>倍精度浮動小数点型を文字列に変換するときの桁数(全体,小数点以下)</summary>
            member _.DoubleToStringFormat x = c.numFormat.setDFormat x
            ///<summary>コンパイル時のオプションを追加</summary>
            member _.Option x = c.olist.add ("-"+x)
            
        type Aqualis with
            ///<summary>Aqualis設定</summary>
            member this.Setting = AqualisSetting this
            ///<summary>Python生成コードの明示的なimport設定</summary>
            member this.Python = PythonImportSettings this
