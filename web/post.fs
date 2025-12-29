namespace Aqualis

type post(id:num0) =
    new(x:string) = post (num0.str(x))
    member _.get with get() = num0.var("_POST["+id.name+"]")
    member this.get_html with get() = Var("htmlspecialchars("+this.get.name+",ENT_QUOTES)")
    ///テキストボックス
    member _.input() = html.taga("input",["type","\"text\""; "name",id.code; "value","\"\""])
    member _.input(a:list<string*string>) = html.taga("input",["type","\"text\""; "name",id.code; "value","\"\""]@a)
    ///パスワード入力テキストボックス
    member _.password() = html.taga("input",["type","\"password\""; "name",id.code;])
    ///テキストボックス
    member _.input(value:string) = html.taga("input",["type","\"text\""; "name",id.code; "value","\""+value+"\""])
    member _.input(value:string,a:list<string*string>) = html.taga("input",["type","\"text\""; "name",id.code; "value","\""+value+"\""]@a)
    member _.textArea() = html.tagb("textarea",["type","\"text\""; "name",id.code;]) <| fun () -> ()
    member _.textArea code = html.tagb("textarea",["type","\"text\""; "name",id.code;]) <| fun () -> code()
    member _.textArea(a:list<string*string>) = html.tagb("textarea",["type","\"text\""; "name",id.code;]@a) <| fun () -> ()
    member _.textArea_contents(a:list<string*string>) = fun code -> html.tagb("textarea",["type","\"text\""; "name",id.code;]@a) code
    member this.textArea_copy() = html.tagb("textarea",["type","\"text\""; "name",id.code;]) <| fun () -> pr.cwriter.codewrite (this.get_html.code)
    member this.textArea_copy(a:list<string*string>) = html.tagb("textarea",["type","\"text\""; "name",id.code;]@a) <| fun () -> pr.cwriter.codewrite (this.get_html.code)
    member _.textArea(value:string) = html.tagb("textarea",["type","\"text\""; "name",id.code;]) <| fun () -> pr.cwriter.codewrite (value)
    member _.input_lock(value:string) = html.taga("input",["type","\"text\""; "name",id.code; "readonly","readonly"; "value","\""+value+"\""])
    member _.input_lock(value:string,a:list<string*string>) = html.taga("input",["type","\"text\""; "name",id.code; "readonly","readonly"; "value","\""+value+"\""]@a)
    ///テキストボックス
    member _.input(value:num0) = html.taga("input",["type","\"text\""; "name",id.code; "value",value.code])
    member _.input(value:num0,a:list<string*string>) = html.taga("input",["type","\"text\""; "name",id.code; "value",value.code]@a)
    member _.input_lock(value:num0) = html.taga("input",["type","\"text\""; "name",id.code; "readonly","readonly"; "value",value.code])
    member _.input_lock(value:num0,a:list<string*string>) = html.taga("input",["type","\"text\""; "name",id.code; "readonly","readonly"; "value",value.code]@a)
    ///パスワード入力テキストボックス
    member _.password(value:string) = html.taga("input",["type","\"password\""; "name",id.code; "value","\""+value+"\""])
    ///パスワード入力テキストボックス
    member _.password(value:num0) = html.taga("input",["type","\"password\""; "name",id.code; "value",value.code])
    ///テキストボックス（送信済みのメッセージを表示）
    member this.input_copy() = html.taga("input",["type","\"text\""; "name",id.code; "value",("\\\""++this.get++"\\\"").code])
    member this.input_copy(a:list<string*string>) = html.taga("input",["type","\"text\""; "name",id.code; "value",("\\\""++this.get++"\\\"").code]@a)
    member this.input_copy_lock() = html.taga("input",["type","\"text\""; "name",id.code; "readonly","readonly"; "value",("\\\""++this.get++"\\\"").code])
    member this.input_copy_lock(a:list<string*string>) = html.taga("input",["type","\"text\""; "name",id.code; "readonly","readonly"; "value",("\\\""++this.get++"\\\"").code]@a)
    ///パスワード入力テキストボックス（送信済みのメッセージを表示）
    member this.password_copy() = html.taga("input",["type","\"password\""; "name",id.code; "value",("\\\""++this.get++"\\\"").code])
    member this.password_copy_lock() = html.taga("input",["type","\"password\""; "name",id.code; "readonly","readonly"; "value",("\\\""++this.get++"\\\"").code])
    member _.submit(value:string) = html.taga("input",["type","\"submit\""; "name",("\\\""++id++"\\\"").code; "value","\""+value+"\""])
    member _.submit(url:string,value:string) = html.taga("input",["type","\"submit\""; "name",("\\\""++id++"\\\"").code; "value","\""+value+"\""; "formaction","\""+url+"\""])
    member _.submit(url:string,value:string,style:string) = html.taga("input",["type","\"submit\""; "name",("\\\""++id++"\\\"").code; "class","\""+style+"\""; "value","\""+value+"\""; "formaction","\""+url+"\""])
    member _.select code = html.tagb ("select",["name",id.code;]) code
    
type postFile(id:num0) =
    new(x:string) = postFile (num0.str(x))
    member _.files with get() = num0.var("_FILES["+id.name+"][\"name\"]")
    member _.err with get() = num0.var("_FILES["+id.name+"][\"error\"]")
    member this.file_upload(dir) =
        let upload = num0.var(id.name+"_file_upload")
        let file = num0.var("_FILES")
        upload <== "./"++file.[id].["name"]
        php.phpcode <| fun () -> pr.cwriter.codewrite("move_uploaded_file($_FILES['file_upload']['tmp_name'], "+upload.name+");")
    member this.file_upload_check(dir) =
        let upload = num0.var(id.name+"_file_upload")
        let file = num0.var("_FILES")
        upload <== "./"++file.[id].["name"]
        br.if1(PHPbool("move_uploaded_file($_FILES['file_upload']['tmp_name'], "+upload.name+")")) <| fun () ->
            php.echo "アップロード完了"
    member this.file_select() =
        html.tagb ("form", ["enctype","multipart/form-data"; "method","post";]) <| fun () ->
            html.taga ("input", ["input name",id.name; "type","file";])
            html.taga ("input", ["type","submit"; "value","アップロード";])
    member this.file_select(action_phpfile:string) =
        html.tagb ("form", ["action",action_phpfile; "enctype","multipart/form-data"; "method","post";]) <| fun () ->
            html.taga ("input", ["input name",id.name; "type","file";])
            html.taga ("input", ["type","submit"; "value","アップロード";])
    member this.files_upload(dir) =
        let file = num0.var("_FILES")
        br.if1(php.isset(file.[id])) <| fun () ->
            iter.foreach(file.[id].["name"]) <| fun i ->
                br.if1(PHPbool("is_uploaded_file("+file.[id].["tmp_name"].[i].name+")")) <| fun () ->
                    php.phpcode <| fun () -> pr.cwriter.codewrite("move_uploaded_file("+file.[id].["tmp_name"].[i].name+", \"./"+dir+"\"."+file.[id].["name"].[i].name+");")
    member this.files_upload_check(dir) =
        let file = num0.var("_FILES")
        br.if1(php.isset(file.[id])) <| fun () ->
            iter.foreach(file.[id].["name"]) <| fun i ->
                br.if1(PHPbool("is_uploaded_file("+file.[id].["tmp_name"].[i].name+")")) <| fun () ->
                    br.if2(PHPbool("move_uploaded_file("+file.[id].["tmp_name"].[i].name+", \"./"+dir+"\"."+file.[id].["name"].[i].name+")"))
                    <| fun () ->
                        php.echo ("アップロード完了: "++file.[id].["name"].[i]++"<br>")
                    <| fun () ->
                        php.echo ("アップロード失敗: "++file.[id].["name"].[i]++"<br>")
    member this.files_select() =
        html.taga ("input", ["multiple name",("\\\""++id++"[]"++"\\\""); "type",num0.str("file");])
        
    member this.files_select(action_phpfile:string) =
        html.taga ("input", ["multiple name",("\\\""++id++"[]"++"\\\""); "type",num0.str("file");])
        
    /// ファイルが指定されているか
    member this.isFileSpecified with get() =
        //ファイルが指定されていないとき、post_newfiles.err[0] = 4になる
        this.err[0] .=/ 4
        
