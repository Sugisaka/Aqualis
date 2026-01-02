namespace Aqualis

type post(id:exprString) =
    new(x:string) = post (Str x)
    new(x:num0) = post (Nvr x.Expr)
    new(x:PHPdata) = post x.expr
    member _.get with get() = PHPdata("$_POST["+id.toString(" . ",StrQuotation)+"]")
    member this.get_html with get() = PHPdata("htmlspecialchars(" + this.get.code + ",ENT_QUOTES)")
    ///テキストボックス
    member _.input() =
        html.taga(
            "input",
            [
                "type", Str "text"
                "name", id
                "value", Str ""
            ]
        )
    member _.input(a:list<string*string>) =
        html.taga(
            "input",
            [
                "type", Str "text"
                "name", id
                "value", Str ""
            ]@(a |> List.map (fun (r,s) -> r,Str s))
        )

    ///パスワード入力テキストボックス
    member _.password() = 
        html.taga(
            "input",
            [
                "type", Str "password"
                "name", id
            ]
        )
    ///テキストボックス
    member _.input(value:string) =
        html.taga(
            "input",
            [
                "type", Str "text"
                "name", id
                "value", Str value
            ]
        )
    member _.input(value:string,a:list<string*string>) =
        html.taga(
            "input",
            [
                "type", Str "text"
                "name", id
                "value", Str value
            ]@(a |> List.map (fun (r,s) -> r,Str s))
        )

    member _.textArea() =
        html.tagb(
            "textarea",
            [
                "type", Str "text"
                "name", id
            ]
        ) <| fun () -> ()
    member _.textArea code =
        html.tagb(
            "textarea",
            [
                "type", Str "text"
                "name", id
            ]
        ) <| fun () -> code()
    member _.textArea(a:list<string*string>) = 
        html.tagb(
            "textarea",
            [
                "type", Str "text"
                "name", id
            ]@(a |> List.map (fun (r,s) -> r,Str s))
        ) <| fun () -> ()
    member _.textArea_contents(a:list<string*string>) = fun code ->
        html.tagb(
            "textarea",
            [
                "type", Str "text"
                "name", id
            ]@(a |> List.map (fun (r,s) -> r,Str s))
        ) code
    member this.textArea_copy() =
        html.tagb(
            "textarea",
            [
                "type", Str "text"
                "name", id
            ]
        ) <| fun () -> codewrite this.get_html.code
    member this.textArea_copy(a:list<string*string>) =
        html.tagb(
            "textarea",
            [
                "type", Str "text"
                "name", id
            ]@(a |> List.map (fun (r,s) -> r,Str s))
        ) <| fun () -> codewrite this.get_html.code
    member _.textArea(value:string) =
        html.tagb(
            "textarea",
            [
                "type", Str "text"
                "name", id
            ]
        ) <| fun () -> codewrite (value)
    member _.input_lock(value:string) =
        html.taga(
            "input",
            [
                "type", Str "text"
                "name", id
                "readonly", Str "readonly"
                "value", Str value
            ]
        )
    member _.input_lock(value:string,a:list<string*string>) =
        html.taga(
            "input",
            [
                "type", Str "text"
                "name", id
                "readonly",Str "readonly"
                "value", Str value
            ]@(a |> List.map (fun (r,s) -> r,Str s))
        )

    ///テキストボックス
    member _.input(value:num0) =
        html.taga(
            "input",
            [
                "type", Str "text"
                "name", id
                "value", Nvr value.Expr
            ]
        )
    member _.input(value:num0,a:list<string*string>) =
        html.taga(
            "input",
            [
                "type", Str "text"
                "name", id
                "value", Nvr value.Expr
            ]@(a |> List.map (fun (r,s) -> r,Str s))
        )

    member _.input_lock(value:num0) =
        html.taga(
            "input",
            [
                "type", Str "text"
                "name", id
                "readonly",Str "readonly"
                "value", Nvr value.Expr
            ]
        )
    member _.input_lock(value:num0,a:list<string*string>) =
        html.taga(
            "input",
            [
                "type", Str "text"
                "name", id
                "readonly",Str "readonly"
                "value", Nvr value.Expr
            ]@(a |> List.map (fun (r,s) -> r,Str s))
        )

    ///パスワード入力テキストボックス
    member _.password(value:string) =
        html.taga(
            "input",
            [
                "type", Str "password"
                "name", id
                "value", Str value
            ]
        )
    ///パスワード入力テキストボックス
    member _.password(value:num0) = 
        html.taga(
            "input",
            [
                "type", Str "password"
                "name", id
                "value", Nvr value.Expr
            ]
        )
    ///テキストボックス（送信済みのメッセージを表示）
    member this.input_copy() = 
        html.taga(
            "input",
            [
                "type", Str "text"
                "name", id
                "value", this.get.expr
            ]
        )
    member this.input_copy(a:list<string*string>) = 
        html.taga(
            "input",
            [
                "type", Str"text"
                "name", id
                "value", this.get.expr
            ]@(a |> List.map (fun (r,s) -> r,Str s))
        )
    member this.input_copy_lock() = 
        html.taga(
            "input",
            [
                "type", Str "text"
                "name", id
                "readonly", Str "readonly"
                "value", this.get.expr
            ]
        )
    member this.input_copy_lock(a:list<string*string>) = 
        html.taga(
            "input",
            [
                "type", Str "text"
                "name", id
                "readonly", Str "readonly"
                "value", this.get.expr
            ]@(a |> List.map (fun (r,s) -> r,Str s))
        )
    ///パスワード入力テキストボックス（送信済みのメッセージを表示）
    member this.password_copy() = 
        html.taga(
            "input",
            [
                "type", Str "password"
                "name",id
                "value", this.get.expr
            ]
        )
    member this.password_copy_lock() =
        html.taga(
            "input",
            [
                "type", Str "password"
                "name", id
                "readonly", Str "readonly"
                "value", this.get.expr
            ]
        )
    member _.submit(value:string) =
        html.taga(
            "input",
            [
                "type", Str "submit"
                "name", id
                "value",Str value
            ]
        )
    member _.submit(url:string,value:string) =
        html.taga(
            "input",
            [
                "type", Str "submit"
                "name", id
                "value", Str value
                "formaction", Str url
            ]
        )
    member _.submit(url:string,value:string,style:string) =
        html.taga("input",
            [
                "type", Str "submit"
                "name", id
                "class", Str style
                "value", Str value
                "formaction", Str url
            ]
        )
    member _.select code = 
        html.tagb (
            "select",
            [
                "name",id
            ]
        ) code
    
type postFile(id:exprString) =
    new(x:string) = postFile (Str x)
    member _.files with get() = PHPdata("_FILES["+id.toString(" . ",StrQuotation)+"][\"name\"]")
    member _.err with get() = PHPdata("_FILES["+id.toString(" . ",StrQuotation)+"][\"error\"]")
    member this.file_upload dir =
        let upload = num0.var(id.toString(" . ",StrQuotation)+"_file_upload")
        let file = PHPdata "_FILES"
        upload <== "./"++file[id].["name"]
        php.phpcode <| fun () -> codewrite("move_uploaded_file($_FILES['file_upload']['tmp_name'], " + upload.Expr.eval (programList[prIndex]) + ");")
    member this.file_upload_check dir =
        let upload = num0.var(id.toString(" . ",StrQuotation)+"_file_upload")
        let file = PHPdata "_FILES"
        upload <== "./"++file.[id].["name"]
        br.if1(bool0(Var(Nt, "move_uploaded_file($_FILES['file_upload']['tmp_name'], " + upload.Expr.eval (programList[prIndex]) + ")", NaN))) <| fun () ->
            php.echo "アップロード完了"
    member this.file_select() =
        html.tagb ("form", ["enctype","multipart/form-data"; "method","post";]) <| fun () ->
            html.taga ("input", ["input name",id.toString(" . ",StrQuotation); "type","file";])
            html.taga ("input", ["type","submit"; "value","アップロード";])
    member this.file_select(action_phpfile:string) =
        html.tagb ("form", ["action",action_phpfile; "enctype","multipart/form-data"; "method","post";]) <| fun () ->
            html.taga ("input", ["input name",id.toString(" . ",StrQuotation); "type","file";])
            html.taga ("input", ["type","submit"; "value","アップロード";])
    member this.files_upload(dir) =
        let file = PHPdata "_FILES"
        br.if1(php.isset(file[id])) <| fun () ->
            file.[id].["name"].foreach <| fun i ->
                br.if1(bool0(Var(Nt, "is_uploaded_file(" + file.[id].["tmp_name"].[i].code + ")",NaN))) <| fun () ->
                    php.phpcode <| fun () -> codewrite("move_uploaded_file(" + file.[id].["tmp_name"].[i].code + ", \"./"+dir+"\"."+file.[id].["name"].[i].code + ");")
    member this.files_upload_check(dir) =
        let file = PHPdata "_FILES"
        br.if1(php.isset(file[id])) <| fun () ->
            file.[id].["name"].foreach <| fun i ->
                br.if1(bool0(Var(Nt,"is_uploaded_file(" + file.[id].["tmp_name"].[i].code + ")",NaN))) <| fun () ->
                    br.if2(bool0(Var(Nt,"move_uploaded_file(" + file.[id].["tmp_name"].[i].code + ", \"./" + dir + "\"." + file.[id].["name"].[i].code + ")",NaN)))
                    <| fun () ->
                        php.echo ("アップロード完了: "++file.[id].["name"].[i]++"<br>")
                    <| fun () ->
                        php.echo ("アップロード失敗: "++file.[id].["name"].[i]++"<br>")
    member this.files_select() =
        html.taga ("input", ["multiple name", "\\\""++id++"[]"++"\\\""; "type",Str "file";])
        
    member this.files_select(action_phpfile:string) =
        html.taga ("input", ["multiple name", "\\\""++id++"[]"++"\\\""; "type",Str "file";])
        
    /// ファイルが指定されているか
    member this.isFileSpecified with get() =
        //ファイルが指定されていないとき、post_newfiles.err[0] = 4になる
        this.err[0].num0 .=/ 4
