// 
// Copyright (c) 2026 Jun-ichiro Sugisaka
// 
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
// 
namespace Aqualis

type post(id:PHPdata) =
    new(x:string) = post (PHPdata [RStr x])
    new(x:num0) = post (PHPdata [RNvr x.Expr])
    member _.get with get() = PHPdata.f("$_POST["+id.toString(" . ",StrQuotation)+"]")
    member this.get_html with get() = PHPdata.f("htmlspecialchars(" + this.get.code + ",ENT_QUOTES)")
    ///テキストボックス
    member _.input() =
        html.taga(
            "input",
            [
                "type", PHPdata "text"
                "name", id
                "value", PHPdata ""
            ]
        )
    member _.input(a:list<string*string>) =
        html.taga(
            "input",
            [
                "type", PHPdata "text"
                "name", id
                "value", PHPdata ""
            ]@(a |> List.map (fun (r,s) -> r,PHPdata s))
        )

    ///パスワード入力テキストボックス
    member _.password() = 
        html.taga(
            "input",
            [
                "type", PHPdata "password"
                "name", id
            ]
        )
    ///テキストボックス
    member _.input(value:string) =
        html.taga(
            "input",
            [
                "type", PHPdata "text"
                "name", id
                "value", PHPdata value
            ]
        )

    member _.input(value:PHPdata,a:list<string*string>) =
        html.taga(
            "input",
            [
                "type", PHPdata "text"
                "name", id
                "value", value
            ]@(a |> List.map (fun (r,s) -> r,PHPdata s))
        )
         
    member _.input(value:string,a:list<string*string>) =
        html.taga(
            "input",
            [
                "type", PHPdata "text"
                "name", id
                "value", PHPdata value
            ]@(a |> List.map (fun (r,s) -> r,PHPdata s))
        )

    member _.textArea() =
        html.tagb(
            "textarea",
            [
                "type", PHPdata "text"
                "name", id
            ]
        ) <| fun () -> ()
    member _.textArea code =
        html.tagb(
            "textarea",
            [
                "type", PHPdata "text"
                "name", id
            ]
        ) <| fun () -> code()
    member _.textArea(a:list<string*string>) = 
        html.tagb0(
            "textarea",
            [
                "type", PHPdata "text"
                "name", id
            ]@(a |> List.map (fun (r,s) -> r,PHPdata s))
        )
    member _.textArea_contents(a:list<string*string>) = fun code ->
        html.tagb(
            "textarea",
            [
                "type", PHPdata "text"
                "name", id
            ]@(a |> List.map (fun (r,s) -> r,PHPdata s))
        ) code
    member this.textArea_copy() =
        html.tagb(
            "textarea",
            [
                "type", PHPdata "text"
                "name", id
            ]
        ) <| fun () -> codewritein this.get_html.phpcode
    member this.textArea_copy(a:list<string*string>) =
        html.tagb(
            "textarea",
            [
                "type", PHPdata "text"
                "name", id
            ]@(a |> List.map (fun (r,s) -> r,PHPdata s))
        ) <| fun () -> codewritein this.get_html.phpcode
    member _.textArea(value:string) =
        html.tagb(
            "textarea",
            [
                "type", PHPdata "text"
                "name", id
            ]
        ) <| fun () -> codewritein value
    member _.input_lock(value:PHPdata) =
        html.taga(
            "input",
            [
                "type", PHPdata "text"
                "name", id
                "readonly", PHPdata "readonly"
                "value", value
            ]
        )
    member _.input_lock(value:string) =
        html.taga(
            "input",
            [
                "type", PHPdata "text"
                "name", id
                "readonly", PHPdata "readonly"
                "value", PHPdata value
            ]
        )
    member _.input_lock(value:string,a:list<string*string>) =
        html.taga(
            "input",
            [
                "type", PHPdata "text"
                "name", id
                "readonly",PHPdata "readonly"
                "value", PHPdata value
            ]@(a |> List.map (fun (r,s) -> r,PHPdata s))
        )

    ///テキストボックス
    member _.input(value:num0) =
        html.taga(
            "input",
            [
                "type", PHPdata "text"
                "name", id
                "value", PHPdata value
            ]
        )
    member _.input(value:num0,a:list<string*string>) =
        html.taga(
            "input",
            [
                "type", PHPdata "text"
                "name", id
                "value", PHPdata value
            ]@(a |> List.map (fun (r,s) -> r,PHPdata s))
        )

    member _.input_lock(value:num0) =
        html.taga(
            "input",
            [
                "type", PHPdata "text"
                "name", id
                "readonly",PHPdata "readonly"
                "value", PHPdata value
            ]
        )
    member _.input_lock(value:num0,a:list<string*string>) =
        html.taga(
            "input",
            [
                "type", PHPdata "text"
                "name", id
                "readonly",PHPdata "readonly"
                "value", PHPdata value
            ]@(a |> List.map (fun (r,s) -> r,PHPdata s))
        )

    ///パスワード入力テキストボックス
    member _.password(value:string) =
        html.taga(
            "input",
            [
                "type", PHPdata "password"
                "name", id
                "value", PHPdata value
            ]
        )
    ///パスワード入力テキストボックス
    member _.password(value:num0) = 
        html.taga(
            "input",
            [
                "type", PHPdata "password"
                "name", id
                "value", PHPdata value
            ]
        )
    ///テキストボックス（送信済みのメッセージを表示）
    member this.input_copy() = 
        html.taga(
            "input",
            [
                "type", PHPdata "text"
                "name", id
                "value", this.get
            ]
        )
    member this.input_copy(a:list<string*string>) = 
        html.taga(
            "input",
            [
                "type", PHPdata "text"
                "name", id
                "value", this.get
            ]@(a |> List.map (fun (r,s) -> r,PHPdata s))
        )
    member this.input_copy_lock() = 
        html.taga(
            "input",
            [
                "type", PHPdata "text"
                "name", id
                "readonly", PHPdata "readonly"
                "value", this.get
            ]
        )
    member this.input_copy_lock(a:list<string*string>) = 
        html.taga(
            "input",
            [
                "type", PHPdata "text"
                "name", id
                "readonly", PHPdata "readonly"
                "value", this.get
            ]@(a |> List.map (fun (r,s) -> r,PHPdata s))
        )
    ///パスワード入力テキストボックス（送信済みのメッセージを表示）
    member this.password_copy() = 
        html.taga(
            "input",
            [
                "type", PHPdata "password"
                "name",id
                "value", this.get
            ]
        )
    member this.password_copy_lock() =
        html.taga(
            "input",
            [
                "type", PHPdata "password"
                "name", id
                "readonly", PHPdata "readonly"
                "value", this.get
            ]
        )
    member _.submit(value:string) =
        html.taga(
            "input",
            [
                "type", PHPdata "submit"
                "name", id
                "value",PHPdata value
            ]
        )
    member _.submit(url:string,value:string) =
        html.taga(
            "input",
            [
                "type", PHPdata "submit"
                "name", id
                "value", PHPdata value
                "formaction", PHPdata url
            ]
        )
    member _.submit(url:string,value:string,style:string) =
        html.taga("input",
            [
                "type", PHPdata "submit"
                "name", id
                "class", PHPdata style
                "value", PHPdata value
                "formaction", PHPdata url
            ]
        )
    member _.select code = 
        html.tagb (
            "select",
            [
                "name",id
            ]
        ) code
    
type postFile(id:PHPdata) =
    new(x:string) = postFile (PHPdata x)
    member _.files with get() = PHPdata.f("$_FILES["+id.toString(" . ",StrQuotation)+"][\"name\"]")
    member _.err with get() = PHPdata.f("$_FILES["+id.toString(" . ",StrQuotation)+"][\"error\"]")
    member this.file_upload dir =
        let upload = PHPdata(id.toString(" . ",StrQuotation)+"_file_upload")
        let file = PHPdata.var "_FILES"
        let aaa = file.[id].["name"]
        upload <== "./"++file.[id].["name"]
        php.phpcode <| fun () -> codewrite("move_uploaded_file($_FILES['file_upload']['tmp_name'], " + upload.code + ");")
    member this.file_upload_check dir =
        let upload = PHPdata(id.toString(" . ",StrQuotation)+"_file_upload")
        let file = PHPdata.var "_FILES"
        upload <== "./"++file.[id].["name"]
        br.if1(bool0(Var(Nt, "move_uploaded_file($_FILES['file_upload']['tmp_name'], " + upload.code + ")", NaN))) <| fun () ->
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
        let file = PHPdata.var "_FILES"
        br.if1(php.isset(file[id])) <| fun () ->
            file.[id].["name"].foreach <| fun i ->
                br.if1(bool0(Var(Nt, "is_uploaded_file(" + file.[id].["tmp_name"].[i].code + ")",NaN))) <| fun () ->
                    php.phpcode <| fun () -> codewrite("move_uploaded_file(" + file.[id].["tmp_name"].[i].code + ", \"./"+dir+"\"."+file.[id].["name"].[i].code + ");")
    member this.files_upload_check(dir) =
        let file = PHPdata.var "_FILES"
        br.if1(php.isset(file[id])) <| fun () ->
            file.[id].["name"].foreach <| fun i ->
                br.if1(bool0(Var(Nt,"is_uploaded_file(" + file.[id].["tmp_name"].[i].code + ")",NaN))) <| fun () ->
                    br.if2(bool0(Var(Nt,"move_uploaded_file(" + file.[id].["tmp_name"].[i].code + ", \"./" + dir + "\"." + file.[id].["name"].[i].code + ")",NaN)))
                    <| fun () ->
                        php.echo ("アップロード完了: "++file.[id].["name"].[i]++"<br>")
                    <| fun () ->
                        php.echo ("アップロード失敗: "++file.[id].["name"].[i]++"<br>")
    member this.files_select() =
        html.taga ("input", ["multiple name", id++"[]"; "type",PHPdata "file";])
        
    member this.files_select(action_phpfile:string) =
        html.taga ("input", ["multiple name", id++"[]"; "type",PHPdata "file";])
        
    /// ファイルが指定されているか
    member this.isFileSpecified with get() =
        //ファイルが指定されていないとき、post_newfiles.err[0] = 4になる
        this.err[0] .=/ 4
