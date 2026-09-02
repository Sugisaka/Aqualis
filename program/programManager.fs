//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    open System.IO
    open System.Threading
   
    type Aqualis private (outputdir:string option,pjname:string option,lang:Language,isNeutral:bool) =
        let cwriter = 
            match outputdir,pjname with
            |Some dir,Some filename ->
                let wr = new codeWriter(dir+"\\"+filename,2,lang)
                Some wr
            |_ -> None

        /// 構造体
        let structData = structure()
        let contextId = System.Guid.NewGuid()
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
            new Aqualis(outputdir,pjname,lang,false)
        static member Version = "188.0.0.0"
        static member BlankWriter(lang:Language) = new Aqualis(None,None,lang,true)
        member _.Dir with get() = outputdir
        member _.ProjectName with get() = pjname
        member _.CodeFile with get() = match outputdir,pjname with |Some dir,Some src -> Some(dir+"\\"+src) |_ -> None
        member _.ContextId with get() = contextId
        member internal _.IsNeutral = isNeutral
        member internal _.ParallelMode with get() = parallelMode and set v = parallelMode <- v
        member internal _.SequenceGate = sequenceGate
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
        member _.codewrite(s:string) = withWriter (fun writer -> writer.codewrite s)
        member _.write(s:string) = withWriter (fun writer -> writer.codewrite s)
        member _.writen(s:string) = withWriter (fun writer -> writer.codewriten s)
        member _.writein(s:string) = withWriter (fun writer -> writer.codewritein s)
        member _.writei(s:string) = withWriter (fun writer -> writer.codewritei s)
        member _.codewritei(s:string) = withWriter (fun writer -> writer.codewritei s)
        member _.codewriten(s:string) = withWriter (fun writer -> writer.codewriten s)
        member _.codewritein(s:string) = withWriter (fun writer -> writer.codewritein s)
        member _.codewritein(h:string,s:string) = withWriter (fun writer -> writer.codewritein (h,s))
        member internal _.writeRaw(s:string) = withWriter (fun writer -> writer.cwrite s)
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
        member val cvar = varCollector lang with get

        member val varPrivate = varCollector lang with get

        member val varCopyIn = varCollector lang with get

        member val varCopyOut = varCollector lang with get
        member private this.disposePrograms() = (this :> System.IDisposable).Dispose()
        static member makeProgramWithContext
            (programInfo: string * string * Language)
            (code: Aqualis -> 'T)
            : 'T =
            let context =
                let dir, name, language = programInfo
                new Aqualis(Some dir, Some name, language)
            try
                code context
            finally
                context.disposePrograms()

    [<AutoOpen>]
    module SettingExtensions =
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
