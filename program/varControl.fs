namespace Aqualis
    
    open System
    open System.IO
    
    type varGenerator(h:int->string) =
        let mutable offlineNumList:list<int> = []
        let mutable onlineNumList:list<int> = []
        let mutable offlineStrList:list<string> = []
        let mutable onlineStrList:list<string> = []
        let mutable autoVarCounter = 0
        member _.getVar() =
            let varname,num =
                match offlineNumList with
                |a::b ->
                    offlineNumList <- b
                    onlineNumList <- a::onlineNumList
                    h a, a
                |[] ->
                    autoVarCounter <- autoVarCounter + 1
                    let v = h autoVarCounter
                    onlineNumList <- autoVarCounter::onlineNumList
                    v,autoVarCounter
            let returnVar() =
                onlineNumList <- List.filter (fun x -> x <> num) onlineNumList
                offlineNumList <- num::offlineNumList
            varname,returnVar
        member _.getVarAndCounter() =
            let varname,num =
                match offlineNumList with
                |a::b ->
                    offlineNumList <- b
                    onlineNumList <- a::onlineNumList
                    h a, a
                |[] ->
                    let v = h autoVarCounter
                    onlineNumList <- autoVarCounter::onlineNumList
                    autoVarCounter <- autoVarCounter + 1
                    v,autoVarCounter
            let returnVar() =
                onlineNumList <- List.filter (fun x -> x <> num) onlineNumList
                offlineNumList <- num::offlineNumList
            varname,num,returnVar
        member this.getVar(v:string) =
            match List.tryFind (fun x -> x=v) onlineStrList, List.tryFind (fun (x:int) -> h x = v) onlineNumList with
            |None, None -> 
                onlineStrList <- v::onlineStrList
                let returnVar() =
                    onlineStrList <- List.filter (fun x -> x <> v) onlineStrList
                    offlineStrList <- v::offlineStrList
                v,returnVar
            |_ ->
                let u,f = this.getVar()
                printfn "変数%sは使用中のため、ここで使用できません。代わりに%sを使用します" v u
                u,f
        member this.varName(x:int) = h x
        member this.maxcounter with get() = autoVarCounter
        member this.varList with get() = offlineStrList@([1..autoVarCounter] |> List.map (fun a -> h a))
        
    ///<summary>重複なしリスト</summary>
    type UniqueList() =
        ///<summary>リスト</summary>
        let mutable ulist:list<string> = []
        ///<summary>リストをクリア</summary>
        member _.clear() =
            ulist <- []
        ///<summary>リストに項目追加</summary>
        member _.add(s:string) =
            match List.exists (fun t -> t=s) ulist with
            |true -> ()
            |false -> ulist <- ulist@[s]
        ///<summary>リスト</summary>
        member _.list with get() = ulist
        
    ///<summary>インデントの設定</summary>
    type IndentController(indentsize:int) =
        let mutable indentposition = 0
        member _.inc() = indentposition <- indentposition + 1
        member _.dec() = indentposition <- indentposition - 1
        member _.clear() = indentposition <- 0
        member _.space with get() = String(' ', indentsize*indentposition)
        
    ///<summary>数値から文字列変換時のフォーマット管理</summary>
    type numericFormatController(lang:Language) =
        
        let mutable int_string_format = 8
        
        let mutable double_string_format = 27,12
        
        ///<summary>整数型を文字列に変換するときの桁数</summary>
        member this.iFormat with get() = int_string_format
        
        ///<summary>整数型を文字列に変換するときの桁数</summary>
        member this.setIFormat n = int_string_format <- n
        
        ///<summary>倍精度浮動小数点型を文字列に変換するときの桁数(全体,小数点以下)</summary>
        member this.dFormat with get() = double_string_format
        
        ///<summary>倍精度浮動小数点型を文字列に変換するときの桁数(全体,小数点以下)</summary>
        member this.setDFormat(n,m) = double_string_format <- n,m

        ///<summary>int型の数値を文字列に変換</summary>
        member fmt.ItoS(n:int) = 
            n.ToString()
        ///<summary>double型の数値を文字列に変換</summary>
        member this.DtoS(d:double) =
            match lang with
            |Fortran -> d.ToString("0.0#################E0").Replace("E","d") 
            |C99 -> d.ToString "0.0#################E0"
            |Python -> d.ToString "0.0#################E0"
            |_ -> d.ToString()
            
    ///<summary>デバッグモード管理</summary>
    type debugController() =
        member val debugMode = false with get,set
        ///<summary>trueの時はデバッグ用のコードを生成する</summary>
        member this.setDebugMode s = this.debugMode <- s
        
    ///<summary>gotoラベル管理</summary>
    type gotoLabelController() =
        let mutable gotoLabel = 10
        
        member _.nextGotoLabel() = 
            gotoLabel <- gotoLabel + 1
            gotoLabel.ToString()
            
        ///<summary>ループ脱出先gotoラベルをひとつ前に戻す</summary>
        member _.exit_false() = 
            gotoLabel <- gotoLabel - 1
            
        ///<summary>脱出しないループのとき、ループ脱出先gotoラベルを否定</summary>
        member _.exit_reset() = 
            gotoLabel <- 10
            
    ///<summary>エラーID管理</summary>
    type errorIDController() =
        let mutable errorid = 1
        
        member _.ID with get() = errorid.ToString()
        member _.inc() = errorid <- errorid + 1
        
    ///<summary>コード書き込み管理</summary>
    type codeWriter(filename:string,indentsize:int,lan:Language) =
        
        let mutable cwriter:option<StreamWriter> = if filename = "" then None else Some (new StreamWriter(filename,false))
        
        let mutable isFileOpen = true
        
        member val indent = IndentController indentsize with get
        
        member _.cwrite(s:string) =
            match cwriter with
            |None -> 
                printfn "file %s not opened" filename
            |Some w -> 
                w.Write s
                
        ///<summary>適切な位置で改行してコード出力</summary>
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
                                let m = Array.fold (fun acc (x:char) -> if x='\"' then acc + 1 else acc) 0 (s.ToCharArray())
                                m%2=1
                            let sp2 = 
                                let rec count c =
                                    if code[c-1]=' ' || code[c-1]=',' || code[c-1]=')' || code[c-1]='(' || code[c-1]='+' || code[c-1]='*' && (c-2<0 || code[c-2]<>'*') && (c>=code.Length || code[c]<>'*') || code[c-1]='/' && code[c]<>')' && code[c]<>'=' then 
                                        c
                                    else 
                                        count (c-1)
                                if instring (code.Substring(0,sp)) then sp else count sp
                            if isallspace(code.Substring(sp2)) then
                                //残りが空白文字だけなのでこの行で終わり
                                writer(code.Substring(0,sp2) + cm + "\n")
                            else
                                //まだコードが続くので継続文字を入れて改行
                                if instring (code.Substring(0,sp2)) then
                                    writer(code.Substring(0,sp2) + "\" &" + "\n")
                                    wt (this.indent.space + "//\"" + code.Substring(sp2))
                                else
                                    writer(code.Substring(0,sp2) + " &" + "\n")
                                    wt (this.indent.space + code.Substring(sp2))
                        else
                            writer(code + cm + "\n")
                    wt (this.indent.space + str)
                    
        ///<summary>コード出力(インデントなし・改行なし)</summary>
        member this.codewrite (ss:string) = 
            match lan with
            |Fortran ->
                if ss<>"" then 
                    let slist = ss.Split([|'\n'|],StringSplitOptions.RemoveEmptyEntries) //改行文字で分割
                    Array.iter (fun code -> this.cwrite code) slist
            |C99 ->
                if ss<>"" then 
                    let slist = ss.Split([|'\n'|],StringSplitOptions.RemoveEmptyEntries) //改行文字で分割
                    Array.iter (fun code -> this.cwrite code) slist
            |LaTeX ->
                if ss<>"" then 
                    let slist = ss.Split([|'\n'|],StringSplitOptions.RemoveEmptyEntries) //改行文字で分割
                    Array.iter (fun code -> this.cwrite code) slist
            |HTML ->
                if ss<>"" then 
                    let slist = ss.Split([|'\n'|],StringSplitOptions.RemoveEmptyEntries) //改行文字で分割
                    Array.iter (fun code -> this.cwrite code) slist
            |HTMLSequenceDiagram ->
                if ss<>"" then 
                    let slist = ss.Split([|'\n'|],StringSplitOptions.RemoveEmptyEntries) //改行文字で分割
                    Array.iter (fun code -> this.cwrite code) slist
            |Python ->
                if ss<>"" then 
                    let slist = ss.Split([|'\n'|],StringSplitOptions.RemoveEmptyEntries) //改行文字で分割
                    Array.iter (fun code -> this.cwrite code) slist
            |JavaScript ->
                if ss<>"" then 
                    let slist = ss.Split([|'\n'|],StringSplitOptions.RemoveEmptyEntries) //改行文字で分割
                    Array.iter (fun code -> this.cwrite code) slist
            |PHP ->
                if ss<>"" then 
                    let slist = ss.Split([|'\n'|],StringSplitOptions.RemoveEmptyEntries) //改行文字で分割
                    Array.iter (fun code -> this.cwrite code) slist
            |Numeric ->
                ()
                
        ///<summary>コード出力(インデントあり・改行なし)</summary>
        member this.codewritei (ss:string) = 
            match lan with
            |Fortran ->
                if ss<>"" then 
                    let slist = ss.Split([|'\n'|],StringSplitOptions.RemoveEmptyEntries) //改行文字で分割
                    Array.iter (fun code -> this.cwrite(this.indent.space + code)) slist
            |C99 ->
                if ss<>"" then 
                    let slist = ss.Split([|'\n'|],StringSplitOptions.RemoveEmptyEntries) //改行文字で分割
                    Array.iter (fun code -> this.cwrite(this.indent.space + code)) slist
            |LaTeX ->
                if ss<>"" then 
                    let slist = ss.Split([|'\n'|],StringSplitOptions.RemoveEmptyEntries) //改行文字で分割
                    Array.iter (fun code -> this.cwrite(this.indent.space + code)) slist
            |HTML ->
                if ss<>"" then 
                    let slist = ss.Split([|'\n'|],StringSplitOptions.RemoveEmptyEntries) //改行文字で分割
                    Array.iter (fun code -> this.cwrite(this.indent.space + code)) slist
            |HTMLSequenceDiagram ->
                if ss<>"" then 
                    let slist = ss.Split([|'\n'|],StringSplitOptions.RemoveEmptyEntries) //改行文字で分割
                    Array.iter (fun code -> this.cwrite(this.indent.space + code)) slist
            |Python ->
                if ss<>"" then 
                    let slist = ss.Split([|'\n'|],StringSplitOptions.RemoveEmptyEntries) //改行文字で分割
                    Array.iter (fun code -> this.cwrite(this.indent.space + code)) slist
            |JavaScript ->
                if ss<>"" then 
                    let slist = ss.Split([|'\n'|],StringSplitOptions.RemoveEmptyEntries) //改行文字で分割
                    Array.iter (fun code -> this.cwrite(this.indent.space + code)) slist
            |PHP ->
                if ss<>"" then 
                    let slist = ss.Split([|'\n'|],StringSplitOptions.RemoveEmptyEntries) //改行文字で分割
                    Array.iter (fun code -> this.cwrite(this.indent.space + code)) slist
            |Numeric ->
                ()
                
        ///<summary>コード出力(インデントなし・改行あり)</summary>
        member this.codewriten (ss:string) = 
            match lan with
            |Fortran ->
                if ss<>"" then 
                    let slist = ss.Split([|'\n'|],StringSplitOptions.RemoveEmptyEntries) //改行文字で分割
                    Array.iter (fun code -> this.cwrite(code + "\n")) slist
            |C99 ->
                if ss<>"" then 
                    let slist = ss.Split([|'\n'|],StringSplitOptions.RemoveEmptyEntries) //改行文字で分割
                    Array.iter (fun code -> this.cwrite(code + "\n")) slist
            |LaTeX ->
                if ss<>"" then 
                    let slist = ss.Split([|'\n'|],StringSplitOptions.RemoveEmptyEntries) //改行文字で分割
                    Array.iter (fun code -> this.cwrite(code + "\n")) slist
            |HTML ->
                if ss<>"" then 
                    let slist = ss.Split([|'\n'|],StringSplitOptions.RemoveEmptyEntries) //改行文字で分割
                    Array.iter (fun code -> this.cwrite(code + "\n")) slist
            |HTMLSequenceDiagram ->
                if ss<>"" then 
                    let slist = ss.Split([|'\n'|],StringSplitOptions.RemoveEmptyEntries) //改行文字で分割
                    Array.iter (fun code -> this.cwrite(code + "\n")) slist
            |Python ->
                if ss<>"" then 
                    let slist = ss.Split([|'\n'|],StringSplitOptions.RemoveEmptyEntries) //改行文字で分割
                    Array.iter (fun code -> this.cwrite(code + "\n")) slist
            |JavaScript ->
                if ss<>"" then 
                    let slist = ss.Split([|'\n'|],StringSplitOptions.RemoveEmptyEntries) //改行文字で分割
                    Array.iter (fun code -> this.cwrite(code + "\n")) slist
            |PHP ->
                if ss<>"" then 
                    let slist = ss.Split([|'\n'|],StringSplitOptions.RemoveEmptyEntries) //改行文字で分割
                    Array.iter (fun code -> this.cwrite(code + "\n")) slist
            |Numeric ->
                ()
                
        ///<summary>コード出力(インデントあり・改行あり)</summary>
        member this.codewritein (ss:string) = 
            match lan with
            |Fortran ->
                if ss<>"" then 
                    let slist = ss.Split([|'\n'|],StringSplitOptions.RemoveEmptyEntries) //改行文字で分割
                    Array.iter (fun code -> this.cwrite(this.indent.space + code + "\n")) slist
            |C99 ->
                if ss<>"" then 
                    let slist = ss.Split([|'\n'|],StringSplitOptions.RemoveEmptyEntries) //改行文字で分割
                    Array.iter (fun code -> this.cwrite(this.indent.space + code + "\n")) slist
            |LaTeX ->
                if ss<>"" then 
                    let slist = ss.Split([|'\n'|],StringSplitOptions.RemoveEmptyEntries) //改行文字で分割
                    Array.iter (fun code -> this.cwrite(this.indent.space + code + "\n")) slist
            |HTML ->
                if ss<>"" then 
                    let slist = ss.Split([|'\n'|],StringSplitOptions.RemoveEmptyEntries) //改行文字で分割
                    Array.iter (fun code -> this.cwrite(this.indent.space + code + "\n")) slist
            |HTMLSequenceDiagram ->
                if ss<>"" then 
                    let slist = ss.Split([|'\n'|],StringSplitOptions.RemoveEmptyEntries) //改行文字で分割
                    Array.iter (fun code -> this.cwrite(this.indent.space + code + "\n")) slist
            |Python ->
                if ss<>"" then 
                    let slist = ss.Split([|'\n'|],StringSplitOptions.RemoveEmptyEntries) //改行文字で分割
                    Array.iter (fun code -> this.cwrite(this.indent.space + code + "\n")) slist
            |JavaScript ->
                if ss<>"" then 
                    let slist = ss.Split([|'\n'|],StringSplitOptions.RemoveEmptyEntries) //改行文字で分割
                    Array.iter (fun code -> this.cwrite(this.indent.space + code + "\n")) slist
            |PHP ->
                if ss<>"" then 
                    let slist = ss.Split([|'\n'|],StringSplitOptions.RemoveEmptyEntries) //改行文字で分割
                    Array.iter (fun code -> this.cwrite(this.indent.space + code + "\n")) slist
            |Numeric ->
                ()

        ///<summary>コメント文</summary>
        member this.comment (ss:string) = 
            match lan with
            |Fortran -> 
                if ss<>"" then 
                    let slist = ss.Split([|'\n'|],StringSplitOptions.RemoveEmptyEntries) //改行文字で分割
                    Array.iter (fun code -> this.cwrite(this.indent.space + "!" + code + "\n")) slist
            |C99 ->
                if ss<>"" then 
                    let slist = ss.Split([|'\n'|],StringSplitOptions.RemoveEmptyEntries) //改行文字で分割
                    Array.iter (fun code -> this.cwrite(this.indent.space + "/*" + code + "*/\n")) slist
            |LaTeX ->
                if ss<>"" then 
                    let slist = ss.Split([|'\n'|],StringSplitOptions.RemoveEmptyEntries) //改行文字で分割
                    Array.iter (fun code -> this.cwrite(this.indent.space + "%" + code + "\n")) slist
            |HTML ->
                let comment_line (str:string) = this.codewritein("<span class=\"comment\">" + str + "</span><br/>\n")
                if ss<>"" then 
                    let slist = ss.Split([|'\n'|],StringSplitOptions.RemoveEmptyEntries) //改行文字で分割
                    Array.iter (fun code -> this.cwrite(this.indent.space + "<span class=\"comment\">" + code + "</span><br/>\n")) slist
            |HTMLSequenceDiagram ->
                let comment_line (str:string) = this.codewritein("<span class=\"comment\">" + str + "</span><br/>\n")
                if ss<>"" then 
                    let slist = ss.Split([|'\n'|],StringSplitOptions.RemoveEmptyEntries) //改行文字で分割
                    Array.iter (fun code -> this.cwrite(this.indent.space + "<span class=\"comment\">" + code + "</span><br/>\n")) slist
            |Python ->
                if ss<>"" then 
                    let slist = ss.Split([|'\n'|],StringSplitOptions.RemoveEmptyEntries) //改行文字で分割
                    Array.iter (fun code -> this.cwrite(this.indent.space + "#" + code + "\n")) slist
            |JavaScript ->
                if ss<>"" then 
                    let slist = ss.Split([|'\n'|],StringSplitOptions.RemoveEmptyEntries) //改行文字で分割
                    Array.iter (fun code -> this.cwrite(this.indent.space + "//" + code + "\n")) slist
            |PHP ->
                if ss<>"" then 
                    let slist = ss.Split([|'\n'|],StringSplitOptions.RemoveEmptyEntries) //改行文字で分割
                    Array.iter (fun code -> this.cwrite(this.indent.space + "/*" + code + "*/\n")) slist
            |Numeric ->
                ()
                
        ///<summary>ファイルを閉じる</summary>
        member this.close() =
            match cwriter with
            |None ->
                ()
            |Some w -> 
                w.Close()
            isFileOpen <- false
            
        ///<summary>ファイルの書き込みを再開</summary>
        member this.appendOpen() =
            cwriter <- Some(new StreamWriter(filename,true))
            
        ///<summary>既存のファイルを削除して開き直す</summary>
        member this.deleteOpen() =
            if isFileOpen then
                match cwriter with
                |None -> ()
                |Some w -> w.Close()
            if File.Exists filename then File.Delete filename
            cwriter <- Some(new StreamWriter(filename))
            
        ///<summary>並列処理の一時ファイルを削除</summary>
        member _.delete() = 
            if isFileOpen then
                match cwriter with
                |None -> ()
                |Some w -> w.Close()
            if File.Exists filename then File.Delete filename
            
        member _.allCode with get() = File.ReadAllText filename
        
    type argumentController(lang:Language) =
        
        ///<summary>この関数の引数リスト： 関数呼び出しに与えられた変数名,(関数内での変数情報)</summary>
        member val list : (string*(Etype*VarType*string)) list = [] with get,set
        
        ///<summary>関数の引数を追加</summary>
        member this.add x = this.list <- this.list@[x]
        
    ///<summary>変数管理</summary>
    type varCollector(lang:Language) =
        ///<summary>型名,変数名,定数</summary>
        let mutable vlist:list<Etype*VarType*string*string> = []
        ///<summary>リスト</summary>
        member _.list with get() = vlist
        member _.clear() =
            vlist <- []
        ///<summary>変数が存在するか検証</summary>
        member _.exists(etyp_,atyp_,name_,cst_) =
            List.exists (fun (etyp,atyp,name,cst) -> etyp_=etyp && atyp_=atyp && name_=name && cst_=cst) vlist
        ///<summary>重複に関係なく変数を登録</summary>
        member _.setVar(etyp,atyp,name,cst) =
            vlist <- (etyp,atyp,name,cst)::vlist
        ///<summary>同名の変数が登録済みの場合は変数を登録しない</summary>
        member this.setUniqVar(etyp,atyp,name,cst) =
            if not (this.exists(etyp,atyp,name,cst)) then
                vlist <- (etyp,atyp,name,cst)::vlist //(etyp,atyp,name,cst)をvlistの先頭部分に追加する。
        ///<summary>同名の変数が登録済みの場合は変数を登録せずに警告を表示</summary>
        member this.setUniqVarWarning(etyp,atyp,name,cst) =
            if this.exists(etyp,atyp,name,cst) then
                printfn "%s" ("変数「" + name + "」が複数定義されています")
            else
                vlist <- (etyp,atyp,name,cst)::vlist
                
        ///<summary>変数の型名を文字列に変換</summary>
        member __.Stype typ = 
            match lang with
            |Fortran ->
                match typ with 
                |It 1 -> "integer(1)" 
                |It _ -> "integer" 
                |Dt -> "double precision" 
                |Zt -> "complex(kind(0d0))" 
                |Structure "string" -> "character(100)" 
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
                |Structure "string" -> "string" 
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
                |A0                    -> this.Stype typ + " :: " + name + if param<>"" then "=" + param else ""
                |A1 0                  -> this.Stype typ + ",allocatable" + " :: " + name + "(:)" + if param<>"" then " = " + param else ""
                |A2(0,0)               -> this.Stype typ + ",allocatable" + " :: " + name + "(:,:)" + if param<>"" then " = " + param else ""
                |A3(0,0,0)             -> this.Stype typ + ",allocatable" + " :: " + name + "(:,:,:)" + if param<>"" then " = " + param else ""
                |A1 size1              -> this.Stype typ + " :: " + name + "(1:" + fmt.ItoS size1 + ")" + if param<>"" then " = " + param else ""
                |A2(size1,size2)       -> this.Stype typ + " :: " + name + "(1:" + fmt.ItoS size1 + ",1:" + fmt.ItoS size2 + ")" + if param<>"" then " = " + param else ""
                |A3(size1,size2,size3) -> this.Stype typ + " :: " + name + "(1:" + fmt.ItoS size1 + ",1:" + fmt.ItoS size2 + ",1:" + fmt.ItoS size3 + ")" + if param<>"" then " = " + param else ""
            |C99 ->
                match vtp, this.Stype typ with 
                |A0,"string"              -> "char" + " " + name + "[100]" + ";"
                |A0,st                    -> st + " " + name + (if param<>"" then " = " + param else "") + ";"
                |A1 0,st                  -> st + " *" + name + (if param<>"" then " = " + param else "") + ";"
                |A2(0,0),st               -> st + " *" + name + (if param<>"" then " = " + param else "") + ";"
                |A3(0,0,0),st             -> st + " *" + name + (if param<>"" then " = " + param else "") + ";"
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
                    |_                      -> name + " = numpy.array([])"
                |A2(0,0) -> 
                    match typ with 
                    |Structure _           -> name + " = numpy.array([[]], dtype=object)"
                    |It _ |It 1            -> name + " = numpy.array([[]], dtype=" + this.Stype typ + ")"
                    |Zt                    -> name + " = numpy.array([[]], dtype=numpy.complex128)"
                    |_                     -> name + " = numpy.array([[]])"
                |A3(0,0,0) -> 
                    match typ with 
                    |Structure _            -> name + " = numpy.array([[[]]], dtype=object)"
                    |It _ |It 1             -> name + " = numpy.array([[[]]], dtype=" + this.Stype typ + ")"
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
                match vtp with 
                |A0        -> name + if param<>"" then " = " + param else " = 0;"
                |A1 0      -> name + " = Array();"
                |A2(0,0)   -> name + " = Array();"
                |A3(0,0,0) -> name + " = Array();"
                |A1 _      -> name + " = " + if param<>"" then param else "Array();"
                |A2(_,_)   -> name + " = " + if param<>"" then param else "Array();"
                |A3(_,_,_) -> name + " = " + if param<>"" then param else "Array();"
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
