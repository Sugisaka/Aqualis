// 
// Copyright (c) 2026 Jun-ichiro Sugisaka
// 
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
// 
namespace Aqualis
    
    open System
    open System.IO
    
    type varGenerator(h:int->string) =
        let gate = obj()
        let mutable offlineNumList:list<int> = []
        let mutable onlineNumList:list<int> = []
        let mutable offlineStrList:list<string> = []
        let mutable onlineStrList:list<string> = []
        let mutable autoVarCounter = 0
        member _.getVar() =
            let varname,num =
                lock gate (fun () ->
                    match offlineNumList with
                    |a::b ->
                        offlineNumList <- b
                        onlineNumList <- a::onlineNumList
                        h a, a
                    |[] ->
                        autoVarCounter <- autoVarCounter + 1
                        let v = h autoVarCounter
                        onlineNumList <- autoVarCounter::onlineNumList
                        v,autoVarCounter)
            let returnVar() =
                lock gate (fun () ->
                    if List.contains num onlineNumList then
                        onlineNumList <- List.filter (fun x -> x <> num) onlineNumList
                        offlineNumList <- num::offlineNumList)
            varname,returnVar
        member _.getVarAndCounter() =
            let varname,num,counter =
                lock gate (fun () ->
                    match offlineNumList with
                    |a::b ->
                        offlineNumList <- b
                        onlineNumList <- a::onlineNumList
                        h a, a, a
                    |[] ->
                        let v = h autoVarCounter
                        let num = autoVarCounter
                        onlineNumList <- num::onlineNumList
                        autoVarCounter <- autoVarCounter + 1
                        v,num,num)
            let returnVar() =
                lock gate (fun () ->
                    if List.contains num onlineNumList then
                        onlineNumList <- List.filter (fun x -> x <> num) onlineNumList
                        offlineNumList <- num::offlineNumList)
            varname,counter,returnVar
        member this.isVarExist(v:string) =
            lock gate (fun () ->
                List.tryFind (fun x -> x=v) onlineStrList,
                List.tryFind (fun x -> x=v) offlineStrList,
                List.tryFind (fun (x:int) -> h x = v) onlineNumList,
                List.tryFind (fun (x:int) -> h x = v) offlineNumList)
        member this.varName(x:int) = h x
        member this.maxcounter with get() = lock gate (fun () -> autoVarCounter)
        member this.varList with get() = lock gate (fun () -> offlineStrList@([1..autoVarCounter] |> List.map (fun a -> h a)))
        member this.OfflineNumList with get() = lock gate (fun () -> offlineNumList)
        member this.OnlineNumList with get() = lock gate (fun () -> onlineNumList)
        member this.OfflineStrList with get() = lock gate (fun () -> offlineStrList)
        member this.OnlineStrList with get() = lock gate (fun () -> onlineStrList)
        member this.addOfflineStrList(v:string) = lock gate (fun () -> offlineStrList <- v::offlineStrList)
        member this.addOnlineStrList(v:string) = lock gate (fun () -> onlineStrList <- v::onlineStrList)
        member this.removeOfflineNumList(v:int) = lock gate (fun () -> offlineNumList <- List.filter (fun x -> x <> v) offlineNumList)
        member this.removeOnlineNumList(v:int) = lock gate (fun () -> onlineNumList <- List.filter (fun x -> x <> v) onlineNumList)
        member this.removeOfflineStrList(v:string) = lock gate (fun () -> offlineStrList <- List.filter (fun x -> x <> v) offlineStrList)
        member this.removeOnlineStrList(v:string) = lock gate (fun () -> onlineStrList <- List.filter (fun x -> x <> v) onlineStrList)
        
    ///<summary>重複なしリスト</summary>
    type UniqueList() =
        let gate = obj()
        ///<summary>リスト</summary>
        let mutable ulist:list<string> = []
        ///<summary>リストをクリア</summary>
        member _.clear() =
            lock gate (fun () -> ulist <- [])
        ///<summary>リストに項目追加</summary>
        member _.add(s:string) =
            lock gate (fun () ->
                match List.exists (fun t -> t=s) ulist with
                |true -> ()
                |false -> ulist <- ulist@[s])
        ///<summary>未登録の場合だけ項目を追加</summary>
        member _.tryAdd(s:string) =
            lock gate (fun () ->
                match List.exists (fun t -> t=s) ulist with
                |true -> false
                |false ->
                    ulist <- ulist@[s]
                    true)
        ///<summary>リスト</summary>
        member _.list with get() = lock gate (fun () -> ulist)

    /// <summary>Python生成コードが必要とするimportを重複なく収集する。</summary>
    type internal PythonImportController() =
        let gate = obj()
        let mutable modules : Set<string> = Set.empty
        let mutable symbols : Map<string, Set<string>> = Map.empty

        member _.RequireModule(moduleName:string) =
            lock gate (fun () -> modules <- Set.add moduleName modules)

        member _.RequireSymbol(moduleName:string, symbol:string) =
            lock gate (fun () ->
                let current = symbols |> Map.tryFind moduleName |> Option.defaultValue Set.empty
                symbols <- symbols |> Map.add moduleName (Set.add symbol current))

        member _.Snapshot =
            lock gate (fun () -> modules, symbols)

        member this.Merge(source:PythonImportController) =
            let sourceModules,sourceSymbols = source.Snapshot
            lock gate (fun () ->
                modules <- Set.union modules sourceModules
                for moduleName,sourceModuleSymbols in Map.toSeq sourceSymbols do
                    let current = symbols |> Map.tryFind moduleName |> Option.defaultValue Set.empty
                    symbols <- symbols |> Map.add moduleName (Set.union current sourceModuleSymbols))

        member _.Lines =
            lock gate (fun () ->
                let moduleLines =
                    modules
                    |> Set.toList
                    |> List.map (fun moduleName -> "import " + moduleName)
                let symbolLines =
                    symbols
                    |> Map.toList
                    |> List.map (fun (moduleName,moduleSymbols) ->
                        "from " + moduleName + " import " + String.concat ", " (Set.toList moduleSymbols))
                moduleLines @ symbolLines)

    /// <summary>HTML生成コードへ明示的に追加する外部資産を保持する。</summary>
    type internal HtmlAssetController() =
        let gate = obj()
        let mutable mathJaxScript : string option = None
        let mutable fontStylesheet : string option = None

        member _.SetMathJaxScript(url:string) =
            lock gate (fun () -> mathJaxScript <- Some url)

        member _.DisableMathJax() =
            lock gate (fun () -> mathJaxScript <- None)

        member _.SetFontStylesheet(url:string) =
            lock gate (fun () -> fontStylesheet <- Some url)

        member _.UseSystemFonts() =
            lock gate (fun () -> fontStylesheet <- None)

        member _.Snapshot =
            lock gate (fun () -> mathJaxScript, fontStylesheet)

        member this.Merge(source:HtmlAssetController) =
            let sourceMathJaxScript,sourceFontStylesheet = source.Snapshot
            lock gate (fun () ->
                mathJaxScript <- sourceMathJaxScript |> Option.orElse mathJaxScript
                fontStylesheet <- sourceFontStylesheet |> Option.orElse fontStylesheet)
        
    ///<summary>インデントの設定</summary>
    type IndentController(indentsize:int) =
        let gate = obj()
        let mutable indentposition = 0
        member _.inc() = lock gate (fun () -> indentposition <- indentposition + 1)
        member _.dec() = lock gate (fun () -> indentposition <- indentposition - 1)
        member _.clear() = lock gate (fun () -> indentposition <- 0)
        member _.space with get() = lock gate (fun () -> String(' ', indentsize*indentposition))
        
    ///<summary>数値から文字列変換時のフォーマット管理</summary>
    type numericFormatController(lang:Language) =
        let gate = obj()
        let mutable int_string_format = 12
        
        let mutable double_string_format = 27,17
        
        ///<summary>整数型を文字列に変換するときの桁数</summary>
        member this.iFormat with get() = lock gate (fun () -> int_string_format)
        
        ///<summary>整数型を文字列に変換するときの桁数</summary>
        member this.setIFormat n = lock gate (fun () -> int_string_format <- n)
        
        ///<summary>倍精度浮動小数点型を文字列に変換するときの桁数(全体,小数点以下)</summary>
        member this.dFormat with get() = lock gate (fun () -> double_string_format)
        
        ///<summary>倍精度浮動小数点型を文字列に変換するときの桁数(全体,小数点以下)</summary>
        member this.setDFormat(n,m) = lock gate (fun () -> double_string_format <- n,m)

        ///<summary>int型の数値を文字列に変換</summary>
        member fmt.ItoS(n:int) = 
            n.ToString()
        ///<summary>double型の数値を文字列に変換</summary>
        member this.DtoS(d:double) =
            InvariantFormat.codeNumber lang d
            
    ///<summary>デバッグモード管理</summary>
    type debugController() =
        let mutable enabled = 0
        member _.debugMode
            with get() = System.Threading.Volatile.Read(&enabled) <> 0
            and set value =
                System.Threading.Interlocked.Exchange(
                    &enabled,
                    if value then 1 else 0)
                |> ignore
        ///<summary>trueの時はデバッグ用のコードを生成する</summary>
        member this.setDebugMode s = this.debugMode <- s
        
    ///<summary>gotoラベル管理</summary>
    type gotoLabelController() =
        let mutable gotoLabel = 10
        
        member _.nextGotoLabel() =
            System.Threading.Interlocked.Increment(&gotoLabel).ToString()
            
        ///<summary>ループ脱出先gotoラベルをひとつ前に戻す</summary>
        member _.exit_false() =
            System.Threading.Interlocked.Decrement(&gotoLabel) |> ignore
            
        ///<summary>脱出しないループのとき、ループ脱出先gotoラベルを否定</summary>
        member _.exit_reset() =
            System.Threading.Interlocked.Exchange(&gotoLabel, 10) |> ignore
            
    ///<summary>エラーID管理</summary>
    type errorIDController() =
        let mutable errorid = 1
        
        member _.ID
            with get() =
                System.Threading.Volatile.Read(&errorid).ToString()
        member _.inc() =
            System.Threading.Interlocked.Increment(&errorid) |> ignore
        
    type internal AtomicOutputFile = {
        TargetPath : string
        StagingPath : string }

    [<RequireQualifiedAccess>]
    module internal AtomicOutputFile =
        let private publishGate = obj()

        let synchronize action = lock publishGate action

        let create (targetPath:string) =
            if String.IsNullOrWhiteSpace targetPath then
                invalidArg (nameof targetPath) "An atomic output target path is required."

            let fullTargetPath = Path.GetFullPath targetPath
            let targetFileName = Path.GetFileName fullTargetPath
            if String.IsNullOrWhiteSpace targetFileName then
                invalidArg (nameof targetPath) "An atomic output target must identify a file."

            let targetDirectory = Path.GetDirectoryName fullTargetPath
            let stagingFileName =
                "." + targetFileName + ".aqualis-" + Guid.NewGuid().ToString("N") + ".tmp"
            {
                TargetPath = fullTargetPath
                StagingPath = Path.Combine(targetDirectory, stagingFileName)
            }

        let publish output =
            synchronize (fun () ->
                File.Move(output.StagingPath, output.TargetPath, true))

        let discard output =
            if File.Exists output.StagingPath then
                try
                    File.Delete output.StagingPath
                with
                | :? IOException
                | :? UnauthorizedAccessException -> ()

    ///<summary>コード書き込み管理</summary>
    type codeWriter private (filename:string,indentsize:int,lan:Language,publishTarget:string option) =
        
        let gate = obj()
        let mutable cwriter:option<StreamWriter> = if filename = "" then None else Some (new StreamWriter(filename,false))
        let captureWriters = ResizeArray<StringWriter>()
        let prefixes = ResizeArray<string>()
        let mutable published = false

        let disposeWriter() =
            match cwriter with
            |None ->
                ()
            |Some writer ->
                writer.Dispose()
                cwriter <- None

        let requireWriter() =
            match cwriter with
            |Some writer ->
                writer
            |None ->
                raise (
                    ObjectDisposedException(
                        nameof codeWriter,
                        $"The code writer for '{filename}' is closed."))

        let reopen (append:bool) =
            disposeWriter()
            if not (String.IsNullOrEmpty filename) then
                cwriter <- Some(new StreamWriter(filename, append))

        let deleteUnpublishedStagingFile() =
            match publishTarget with
            |Some _ when not published && File.Exists filename ->
                try
                    File.Delete filename
                with
                | :? IOException
                | :? UnauthorizedAccessException -> ()
            |_ -> ()

        let applyPrefixes() =
            if prefixes.Count > 0 then
                (requireWriter()).Flush()
                disposeWriter()
                let output = AtomicOutputFile.create filename
                try
                    let writeStagedFile() =
                        use destination =
                            new FileStream(
                                output.StagingPath,
                                FileMode.CreateNew,
                                FileAccess.Write,
                                FileShare.None)
                        let prefixBytes =
                            prefixes
                            |> String.concat ""
                            |> Text.UTF8Encoding(false).GetBytes
                        destination.Write(prefixBytes, 0, prefixBytes.Length)
                        use source =
                            new FileStream(
                                filename,
                                FileMode.Open,
                                FileAccess.Read,
                                FileShare.Read)
                        source.CopyTo(destination)
                        destination.Flush(true)
                    writeStagedFile()
                    AtomicOutputFile.publish output
                    prefixes.Clear()
                with _ ->
                    AtomicOutputFile.discard output
                    reopen true
                    reraise()

        new(filename:string,indentsize:int,lan:Language) =
            new codeWriter(filename,indentsize,lan,None)

        static member internal CreateAtomic(targetPath:string,indentsize:int,lan:Language) =
            let output = AtomicOutputFile.create targetPath
            new codeWriter(output.StagingPath,indentsize,lan,Some output.TargetPath)
        
        member _.FilePath with get() = filename
        member val indent = IndentController indentsize with get
        
        member _.cwrite(s:string) =
            lock gate (fun () ->
                if captureWriters.Count > 0 then
                    captureWriters[captureWriters.Count - 1].Write s
                else
                    (requireWriter()).Write s)

        /// Registers text that is written before all ordinary generated output.
        member internal _.prepend(s:string) =
            lock gate (fun () ->
                requireWriter() |> ignore
                prefixes.Add s)

        member private this.writeLines(ss:string, transform:string -> string) =
            match lan with
            |Numeric -> ()
            |_ when ss <> "" ->
                ss.Split([|'\n'|], StringSplitOptions.RemoveEmptyEntries)
                |> Array.iter (fun line -> this.cwrite(transform line))
            |_ -> ()

        /// Captures code emitted by an action without closing or replacing the owned file.
        member _.capture(action:unit -> 'T) =
            let captureWriter = new StringWriter()
            lock gate (fun () -> captureWriters.Add captureWriter)
            try
                let result = action()
                captureWriter.ToString(), result
            finally
                lock gate (fun () ->
                    let lastIndex = captureWriters.Count - 1
                    if
                        lastIndex < 0 ||
                        not (Object.ReferenceEquals(captureWriters[lastIndex], captureWriter))
                    then
                        invalidOp "Code captures must be completed in stack order."
                    captureWriters.RemoveAt lastIndex)
                captureWriter.Dispose()
                
        ///<summary>コード出力(インデントなし・改行なし)</summary>
        member this.codewrite (ss:string) = 
            this.writeLines(ss, id)
                
        ///<summary>コード出力(インデントあり・改行なし)</summary>
        member this.codewritei (ss:string) = 
            this.writeLines(ss, fun line -> this.indent.space + line)
                
        ///<summary>コード出力(インデントあり・改行なし・行頭ヘッダ付き)</summary>
        member this.codewritei (h:string,ss:string) = 
            this.writeLines(ss, fun line -> h + this.indent.space + line)
                
        ///<summary>コード出力(インデントなし・改行あり)</summary>
        member this.codewriten (ss:string) = 
            this.writeLines(ss, fun line -> line + "\n")
                
        ///<summary>コード出力(インデントあり・改行あり)</summary>
        member this.codewritein (ss:string) = 
            this.writeLines(ss, fun line -> this.indent.space + line + "\n")

        ///<summary>コード出力(インデントあり・改行あり・行頭ヘッダ付き)</summary>
        member this.codewritein (h:string,ss:string) = 
            this.writeLines(ss, fun line -> h + this.indent.space + line + "\n")

        ///<summary>コメント文</summary>
        member private _.formatComment(line:string) =
            match lan with
            |Fortran -> "!" + line
            |C99|PHP -> "/*" + line + "*/"
            |LaTeX -> "%" + line
            |HTML|HTMLSequenceDiagram ->
                "<span class=\"comment\">" + HtmlTextEncoding.textContent line + "</span><br/>"
            |Python -> "#" + line
            |JavaScript -> "//" + line
            |Numeric -> ""

        member this.comment (ss:string) =
            this.writeLines(
                ss,
                fun line -> this.indent.space + this.formatComment(line) + "\n")

        /// Emits trusted markup used internally to render structured HTML code views.
        member internal this.commentHtmlMarkup(ss:string) =
            match lan with
            |HTML|HTMLSequenceDiagram ->
                if ss<>"" then
                    let slist = ss.Split([|'\n'|],StringSplitOptions.RemoveEmptyEntries)
                    Array.iter (fun code -> this.cwrite(this.indent.space + "<span class=\"comment\">" + code + "</span><br/>\n")) slist
            |_ ->
                invalidOp "Trusted HTML comment markup can only be emitted for an HTML generation target."
                
        ///<summary>ファイルを閉じる</summary>
        member this.close() =
            lock gate (fun () ->
                applyPrefixes()
                disposeWriter())

        ///<summary>一時ファイルに生成したコードを最終パスへ公開する</summary>
        member internal _.publish() =
            lock gate (fun () ->
                match publishTarget with
                |None -> invalidOp "Only an atomic code writer can publish its output."
                |Some targetPath when not published ->
                    applyPrefixes()
                    disposeWriter()
                    AtomicOutputFile.publish {
                        TargetPath = targetPath
                        StagingPath = filename
                    }
                    published <- true
                |Some _ -> ())
            
        ///<summary>ファイルの書き込みを再開</summary>
        member this.appendOpen() =
            lock gate (fun () -> reopen true)
            
        ///<summary>既存のファイルを削除して開き直す</summary>
        member this.deleteOpen() =
            lock gate (fun () -> reopen false)
            
        ///<summary>並列処理の一時ファイルを削除</summary>
        member _.delete() =
            lock gate (fun () ->
                disposeWriter()
                if File.Exists filename then File.Delete filename)
            
        member _.allCode with get() = File.ReadAllText filename

        interface IDisposable with
            member _.Dispose() =
                lock gate (fun () ->
                    disposeWriter()
                    deleteUnpublishedStagingFile())
        
    type argumentController(lang:Language) =
        let gate = obj()
        let mutable arguments:(string*(Etype*VarType*string)) list = []
        ///<summary>この関数の引数リスト： 関数呼び出しに与えられた変数名,(関数内での変数情報)</summary>
        member _.list
            with get() = lock gate (fun () -> arguments)
            and set value = lock gate (fun () -> arguments <- value)
        
        ///<summary>関数の引数を追加</summary>
        member _.add x = lock gate (fun () -> arguments <- arguments@[x])
        
    ///<summary>変数管理</summary>
    type varCollector(lang:Language, diagnostics:DiagnosticBag) =
        let gate = obj()
        ///<summary>型名,変数名,定数</summary>
        let mutable vlist:list<Etype*VarType*string*string> = []
        let sameName (left:string) (right:string) =
            String.Equals(left, right, if lang = Fortran then StringComparison.OrdinalIgnoreCase else StringComparison.Ordinal)
        let isArray = function A0 -> false | A1 _ | A2 _ | A3 _ -> true
        let requireValidShape (shape:VarType) =
            let invalidShape() = invalidArg "atyp" "Fixed array dimensions must be positive and their product must fit in a 32-bit integer."
            let withinLimit dimensions =
                dimensions |> List.forall (fun dimension -> dimension > 0)
                && (dimensions |> List.map bigint |> List.fold (*) 1I) <= bigint Int32.MaxValue
            match shape with
            |A0 | A1 0 | A2(0,0) | A3(0,0,0) -> ()
            |A1 n when withinLimit [n] -> ()
            |A2(n1,n2) when withinLimit [n1;n2] -> ()
            |A3(n1,n2,n3) when withinLimit [n1;n2;n3] -> ()
            |_ -> invalidShape()
        let requireCompatibleName etyp atyp name cst =
            requireValidShape atyp
            if
                vlist
                |> List.exists (fun (_,existingShape,existingName,_) ->
                    (isArray existingShape && sameName (existingName + "_size") name)
                    || (isArray atyp && sameName existingName (name + "_size")))
            then
                invalidArg "name" ("Variable '" + name + "' conflicts with an array size variable.")
            match vlist |> List.tryFind (fun (_,_,existingName,_) -> sameName existingName name) with
            |Some(existingType,existingShape,_,existingInitial)
                when existingType <> etyp || existingShape <> atyp || existingInitial <> cst ->
                invalidArg "name" ("Variable '" + name + "' is already defined with a different type, shape, or initial value.")
            |_ -> ()
        ///<summary>リスト</summary>
        member _.list with get() = lock gate (fun () -> vlist)
        member _.clear() =
            lock gate (fun () -> vlist <- [])
        ///<summary>変数が存在するか検証</summary>
        member _.exists(name_:string) =
            lock gate (fun () ->
                List.exists (fun (etyp,atyp,name,cst) -> name_=name) vlist)
        ///<summary>変数が存在するか検証</summary>
        member _.exists(etyp_,atyp_,name_,cst_) =
            lock gate (fun () ->
                List.exists (fun (etyp,atyp,name,cst) -> etyp_=etyp && atyp_=atyp && name_=name && cst_=cst) vlist)
        ///<summary>重複に関係なく変数を登録</summary>
        member _.setVar(etyp,atyp,name,cst) =
            lock gate (fun () ->
                requireCompatibleName etyp atyp name cst
                vlist <- (etyp,atyp,name,cst)::vlist)
        ///<summary>同名の変数が登録済みの場合は変数を登録しない</summary>
        member this.setUniqVar(etyp,atyp,name,cst) =
            lock gate (fun () ->
                requireCompatibleName etyp atyp name cst
                if not (List.exists (fun (etyp_,atyp_,name_,cst_) -> etyp_=etyp && atyp_=atyp && sameName name_ name && cst_=cst) vlist) then
                    vlist <- (etyp,atyp,name,cst)::vlist) //(etyp,atyp,name,cst)をvlistの先頭部分に追加する。
        ///<summary>同名の変数が登録済みの場合は変数を登録せずに警告を表示</summary>
        member _.trySetUniqVarWarning(etyp,atyp,name,cst) =
            lock gate (fun () ->
                requireCompatibleName etyp atyp name cst
                if List.exists (fun (etyp_,atyp_,name_,cst_) -> etyp_=etyp && atyp_=atyp && sameName name_ name && cst_=cst) vlist then
                    diagnostics.Report {
                        Code = "AQL1002"
                        Severity = Warning
                        Message = "Variable '" + name + "' is already defined; the duplicate definition was ignored."
                        Location = Some(Generation(lang, None, Some "variable declaration"))
                        Properties = Map ["variable", name]
                    }
                    false
                else
                    vlist <- (etyp,atyp,name,cst)::vlist
                    true)
        member this.setUniqVarWarning(etyp,atyp,name,cst) =
            this.trySetUniqVarWarning(etyp,atyp,name,cst) |> ignore
                
        ///<summary>変数の型名を文字列に変換</summary>
        member __.Stype typ = 
            match lang with
            |Fortran ->
                match typ with 
                |It 1 -> "integer(1)" 
                |It _ -> "integer" 
                |Dt -> "double precision" 
                |Zt -> "complex(kind(0d0))" 
                |Structure "string" -> "character(len=:), allocatable"
                |Structure "integer(1)" -> "integer(1)" 
                |Structure "file" -> "integer"
                |Structure sname -> "type(" + sname + ")"
                |_ -> ""
            |C99 ->
                match typ with 
                |It 1 -> "unsigned char" 
                |It _ -> "int" 
                |Dt -> "double" 
                |Zt -> "double complex"
                |Structure "string" -> "char*"
                |Structure "char" -> "char" 
                |Structure "file" -> "FILE*" 
                |Structure sname -> sname 
                |_ -> ""
            |LaTeX ->
                match typ with 
                |It 1 -> "byte" 
                |It _ -> "int" 
                |Dt -> "double" 
                |Zt -> "complex"
                |Structure "string" -> "char" 
                |Structure "char" -> "char" 
                |Structure sname -> sname 
                |_ -> ""
            |HTML ->
                match typ with 
                |It 1 -> "byte" 
                |It _ -> "int" 
                |Dt -> "double" 
                |Zt -> "complex"
                |Structure "string" -> "char" 
                |Structure "char" -> "char" 
                |Structure sname -> sname 
                |_ -> ""
            |Python ->
                match typ with 
                |It 1 -> "int" 
                |It _ -> "int" 
                |Dt -> "float" 
                |Zt -> "complex"
                |Structure "string" -> "str" 
                |Structure "char" -> "str" 
                |Structure "file" -> "io.TextIOWrapper"
                |Structure sname -> sname
                |_ -> ""
            |_ -> ""
            
        ///<summary>変数宣言のコード</summary>
        member this.declare (typ:Etype,vtp:VarType,name:string,param:string,fmt:numericFormatController) =
            match lang with
            |Fortran ->
                match vtp with 
                |A0 when typ = Structure "string" -> "character(len=:), allocatable :: " + name
                |A0                    -> this.Stype typ + " :: " + name + if param<>"" then "=" + param else ""
                |A1 0                  -> this.Stype typ + ",allocatable" + " :: " + name + "(:)" + if param<>"" then " = " + param else ""
                |A2(0,0)               -> this.Stype typ + ",allocatable" + " :: " + name + "(:,:)" + if param<>"" then " = " + param else ""
                |A3(0,0,0)             -> this.Stype typ + ",allocatable" + " :: " + name + "(:,:,:)" + if param<>"" then " = " + param else ""
                |A1 size1              -> this.Stype typ + " :: " + name + "(1:" + fmt.ItoS size1 + ")" + if param<>"" then " = " + param else ""
                |A2(size1,size2)       -> this.Stype typ + " :: " + name + "(1:" + fmt.ItoS size1 + ",1:" + fmt.ItoS size2 + ")" + if param<>"" then " = " + param else ""
                |A3(size1,size2,size3) -> this.Stype typ + " :: " + name + "(1:" + fmt.ItoS size1 + ",1:" + fmt.ItoS size2 + ",1:" + fmt.ItoS size3 + ")" + if param<>"" then " = " + param else ""
            |C99 ->
                match vtp, this.Stype typ with 
                |A0,"char*"               -> "char *" + name + (if param<>"" then " = " + param else " = NULL") + ";"
                |A0,st                    -> st + " " + name + (if param<>"" then " = " + param else "") + ";"
                |A1 0,st                  -> st + " *" + name + (if param<>"" then " = " + param else " = NULL") + ";"
                |A2(0,0),st               -> st + " *" + name + (if param<>"" then " = " + param else " = NULL") + ";"
                |A3(0,0,0),st             -> st + " *" + name + (if param<>"" then " = " + param else " = NULL") + ";"
                |A1 size1,st              -> st + " " + name + "[" + fmt.ItoS size1 + "]" + (if param<>"" then " = " + param else "") + ";"
                |A2(size1,size2),st       -> st + " " + name + "[" + fmt.ItoS (size1*size2) + "]" + (if param<>"" then " = " + param else "") + ";"
                |A3(size1,size2,size3),st -> st + " " + name + "[" + fmt.ItoS (size1*size2*size3) + "]" + (if param<>"" then " = " + param else "") + ";"
            |LaTeX ->
                match vtp with 
                |A0                    -> "\\item " + this.Stype typ + " $" + name + "$" + if param<>"" then "=" + param else ""
                |A1 0                  -> "\\item " + this.Stype typ + " (allocatable)" + " $" + name + "$ (:)" + if param<>"" then "=" + param else ""
                |A2(0,0)               -> "\\item " + this.Stype typ + " (allocatable)" + " $" + name + "$ (:,:)" + if param<>"" then "=" + param else ""
                |A3(0,0,0)             -> "\\item " + this.Stype typ + " (allocatable)" + " $" + name + "$ (:,:,:)" + if param<>"" then "=" + param else ""
                |A1 size1              -> "\\item " + this.Stype typ + " $" + name + "$ (" + fmt.ItoS size1 + ")" + if param<>"" then "=" + param else ""
                |A2(size1,size2)       -> "\\item " + this.Stype typ + " $" + name + "$ (" + fmt.ItoS size1 + "," + fmt.ItoS size2 + ")" + if param<>"" then "=" + param else ""
                |A3(size1,size2,size3) -> "\\item " + this.Stype typ + " $" + name + "$ (" + fmt.ItoS size1 + "," + fmt.ItoS size2 + "," + fmt.ItoS size3 + ")" + if param<>"" then "=" + param else ""
            |HTML ->
                match vtp with 
                |A0                    -> "\t\t\t<li>" + this.Stype typ + ": \\(" + name + "" + (if param<>"" then "=" + param else "") + "\\)</li>"
                |A1 0                  -> "\t\t\t<li>" + this.Stype typ + ": (allocatable)\\(" + " " + name + " [:]" + (if param<>"" then "=" + param else "") + "\\)</li>"
                |A2(0,0)               -> "\t\t\t<li>" + this.Stype typ + ": (allocatable)\\(" + " " + name + " [:,:]" + (if param<>"" then "=" + param else "") + "\\)</li>"
                |A3(0,0,0)             -> "\t\t\t<li>" + this.Stype typ + ": (allocatable)\\(" + " " + name + " [:,:,:]" + (if param<>"" then "=" + param else "") + "\\)</li>"
                |A1 size1              -> "\t\t\t<li>" + this.Stype typ + ": \\(" + name + " (" + fmt.ItoS size1 + ")" + (if param<>"" then "=" + param else "") + "\\)</li>"
                |A2(size1,size2)       -> "\t\t\t<li>" + this.Stype typ + ": \\(" + name + " (" + fmt.ItoS size1 + "," + fmt.ItoS size2 + ")" + (if param<>"" then "=" + param else "") + "\\)</li>"
                |A3(size1,size2,size3) -> "\t\t\t<li>" + this.Stype typ + ": \\(" + name + " (" + fmt.ItoS size1 + "," + fmt.ItoS size2 + "," + fmt.ItoS size3 + ")" + (if param<>"" then "=" + param else "") + "\\)</li>"
            |HTMLSequenceDiagram ->
                match vtp with 
                |A0                    -> "\t\t\t<li>" + this.Stype typ + ": \\(" + name + "" + (if param<>"" then "=" + param else "") + "\\)</li>"
                |A1 0                  -> "\t\t\t<li>" + this.Stype typ + ": (allocatable)\\(" + " " + name + " [:]" + (if param<>"" then "=" + param else "") + "\\)</li>"
                |A2(0,0)               -> "\t\t\t<li>" + this.Stype typ + ": (allocatable)\\(" + " " + name + " [:,:]" + (if param<>"" then "=" + param else "") + "\\)</li>"
                |A3(0,0,0)             -> "\t\t\t<li>" + this.Stype typ + ": (allocatable)\\(" + " " + name + " [:,:,:]" + (if param<>"" then "=" + param else "") + "\\)</li>"
                |A1 size1              -> "\t\t\t<li>" + this.Stype typ + ": \\(" + name + " (" + fmt.ItoS size1 + ")" + (if param<>"" then "=" + param else "") + "\\)</li>"
                |A2(size1,size2)       -> "\t\t\t<li>" + this.Stype typ + ": \\(" + name + " (" + fmt.ItoS size1 + "," + fmt.ItoS size2 + ")" + (if param<>"" then "=" + param else "") + "\\)</li>"
                |A3(size1,size2,size3) -> "\t\t\t<li>" + this.Stype typ + ": \\(" + name + " (" + fmt.ItoS size1 + "," + fmt.ItoS size2 + "," + fmt.ItoS size3 + ")" + (if param<>"" then "=" + param else "") + "\\)</li>"
            |Python ->
                match vtp with 
                |A0 ->
                    match typ with 
                    |Structure "string"|Structure "char"|Structure "file" -> 
                        name + " = " + if param<>"" then param else "0" 
                    |Structure _ ->
                        name + " = " + this.Stype typ + "(" + (if param<>"" then param else "") + ")"
                    |_ ->
                        name + " = " + if param<>"" then param else "0"
                |A1 0 -> 
                    match typ with 
                    |Structure _            -> name + " = numpy.array([], dtype=object)"
                    |It _ |It 1             -> name + " = numpy.array([], dtype=int)"
                    |Dt                     -> name + " = numpy.array([], dtype=float)"
                    |Zt                     -> name + " = numpy.array([], dtype=numpy.complex128)"
                    |_                      -> name + " = numpy.array([])"
                |A2(0,0) -> 
                    match typ with 
                    |Structure _           -> name + " = numpy.array([[]], dtype=object)"
                    |It _ |It 1            -> name + " = numpy.array([[]], dtype=" + this.Stype typ + ")"
                    |Dt                    -> name + " = numpy.array([[]], dtype=float)"
                    |Zt                    -> name + " = numpy.array([[]], dtype=numpy.complex128)"
                    |_                     -> name + " = numpy.array([[]])"
                |A3(0,0,0) -> 
                    match typ with 
                    |Structure _            -> name + " = numpy.array([[[]]], dtype=object)"
                    |It _ |It 1             -> name + " = numpy.array([[[]]], dtype=" + this.Stype typ + ")"
                    |Dt                     -> name + " = numpy.array([[[]]], dtype=float)"
                    |Zt                     -> name + " = numpy.array([[[]]], dtype=numpy.complex128)"
                    |_                      -> name + " = numpy.array([[[]]])"
                |A1 size1 ->
                    match typ with 
                    |Structure _            -> name + " = " + (if param<>"" then "numpy.array(" + param + ")" else "numpy.array([" + this.Stype typ + "() for _ in range(" + fmt.ItoS size1 + ")], dtype=object)") + ""
                    |It _ |It 1             -> name + " = " + (if param<>"" then "numpy.array(" + param + ")" else "numpy.zeros(" + fmt.ItoS size1 + ",dtype=" + this.Stype typ + ")") + ""
                    |Zt                     -> name + " = " + (if param<>"" then "numpy.array(" + param + ")" else "numpy.zeros(" + fmt.ItoS size1 + ", dtype=numpy.complex128)") + ""
                    |_                      -> name + " = " + (if param<>"" then "numpy.array(" + param + ")" else "numpy.zeros(" + fmt.ItoS size1 + ")") + ""
                |A2(size1,size2) -> 
                    match typ with 
                    |Structure _            -> name + " = " + (if param<>"" then "numpy.array(" + param + ").reshape(" + fmt.ItoS size1 + "," + fmt.ItoS size2 + ")" else "numpy.array([[" + this.Stype typ + "() for _ in range(" + fmt.ItoS size2 + ")] for _ in range(" + fmt.ItoS size1 + ")], dtype=object).reshape(" + fmt.ItoS size1 + "," + fmt.ItoS size2 + ")") + ""
                    |It _ |It 1             -> name + " = " + (if param<>"" then "numpy.array(" + param + ").reshape(" + fmt.ItoS size1 + "," + fmt.ItoS size2 + ")" else "numpy.zeros(" + fmt.ItoS size1 + "*" + fmt.ItoS size2 + ",dtype=" + this.Stype typ + ").reshape(" + fmt.ItoS size1 + "," + fmt.ItoS size2 + ")") + ""
                    |Zt                     -> name + " = " + (if param<>"" then "numpy.array(" + param + ").reshape(" + fmt.ItoS size1 + "," + fmt.ItoS size2 + ")" else "numpy.zeros(" + fmt.ItoS size1 + "*" + fmt.ItoS size2 + ", dtype=numpy.complex128).reshape(" + fmt.ItoS size1 + "," + fmt.ItoS size2 + ")") + ""
                    |_                      -> name + " = " + (if param<>"" then "numpy.array(" + param + ").reshape(" + fmt.ItoS size1 + "," + fmt.ItoS size2 + ")" else "numpy.zeros(" + fmt.ItoS size1 + "*" + fmt.ItoS size2 + ").reshape(" + fmt.ItoS size1 + "," + fmt.ItoS size2 + ")") + ""
                |A3(size1,size2,size3) -> 
                    match typ with 
                    |Structure _            -> name + " = " + (if param<>"" then "numpy.array(" + param + ").reshape(" + fmt.ItoS size1 + "," + fmt.ItoS size2 + "," + fmt.ItoS size3 + ")" else "numpy.array([[[" + this.Stype typ + "() for _ in range(" + (fmt.ItoS size3) + ")] for _ in range(" + fmt.ItoS size2 + ")] for _ in range(" + fmt.ItoS size1 + ")], dtype=object).reshape(" + fmt.ItoS size1 + "," + fmt.ItoS size2 + "," + fmt.ItoS size3 + ")") + ""
                    |It _ |It 1             -> name + " = " + (if param<>"" then "numpy.array(" + param + ").reshape(" + fmt.ItoS size1 + "," + fmt.ItoS size2 + "," + fmt.ItoS size3 + ")" else "numpy.zeros(" + fmt.ItoS size1 + "*" + (fmt.ItoS size2) + "*" + (fmt.ItoS size3) + ",dtype=" + this.Stype typ + ").reshape(" + fmt.ItoS size1 + "," + fmt.ItoS size2 + "," + fmt.ItoS size3 + ")") + ""
                    |Zt                     -> name + " = " + (if param<>"" then "numpy.array(" + param + ").reshape(" + fmt.ItoS size1 + "," + fmt.ItoS size2 + "," + fmt.ItoS size3 + ")" else "numpy.zeros(" + fmt.ItoS size1 + "*" + (fmt.ItoS size2) + "*" + (fmt.ItoS size3) + ", dtype=numpy.complex128).reshape(" + fmt.ItoS size1 + "," + fmt.ItoS size2 + "," + fmt.ItoS size3 + ")") + ""
                    |_                      -> name + " = " + (if param<>"" then "numpy.array(" + param + ").reshape(" + fmt.ItoS size1 + "," + fmt.ItoS size2 + "," + fmt.ItoS size3 + ")" else "numpy.zeros(" + fmt.ItoS size1 + "*" + (fmt.ItoS size2) + "*" + (fmt.ItoS size3) + ").reshape(" + fmt.ItoS size1 + "," + fmt.ItoS size2 + "," + fmt.ItoS size3 + ")") + ""
            |JavaScript ->
                let declaration initialValue =
                    "let " + name + " = " + initialValue + ";"
                match vtp with 
                |A0 -> declaration (if param<>"" then param else "0")
                |A1 0
                |A2(0,0)
                |A3(0,0,0) -> declaration "Array()"
                |A1 size1 ->
                    declaration (if param<>"" then param else "Array(" + fmt.ItoS size1 + ")")
                |A2(size1,size2) ->
                    declaration (if param<>"" then param else "Array(" + fmt.ItoS (size1*size2) + ")")
                |A3(size1,size2,size3) ->
                    declaration (if param<>"" then param else "Array(" + fmt.ItoS (size1*size2*size3) + ")")
            |PHP ->
                match vtp with 
                |A0        -> name + if param<>"" then " = " + param else " = 0;"
                |A1 0      -> name + " = [];"
                |A2(0,0)   -> name + " = [];"
                |A3(0,0,0) -> name + " = [];"
                |A1 _      -> name + " = " + if param<>"" then param + ";" else "[];"
                |A2(_,_)   -> name + " = " + if param<>"" then param + ";" else "[];"
                |A3(_,_,_) -> name + " = " + if param<>"" then param + ";" else "[];"
            |Numeric -> ""
