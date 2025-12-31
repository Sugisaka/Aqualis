//#############################################################################
// webページログイン機能
//#############################################################################

let outputdir = __SOURCE_DIRECTORY__
//let outputdir = @"C:\xampp\htdocs\test_sgproc"

#I @"..\..\bin\Debug\net10.0"
#r "Aqualis.dll"

open System
open System.Text
open System.IO
open System.Text.Unicode
open System.Text.Encodings.Web
open System.Text.Json
open Aqualis

[<AutoOpen>]
module memberinfo =
    /// メンバー情報（JSON生成用）
    type Member = {
        ID : string
        PassWord: string}
        
    /// メンバーリスト（JSON生成用）
    type UserList = 
        {List : array<Member>}
        static member makeJSON(outputdir, filename, data) =
            let options = new JsonSerializerOptions(Encoder = JavaScriptEncoder.Create UnicodeRanges.All, WriteIndented = true)
            let memberJson = JsonSerializer.Serialize({List=List.toArray data},options)
            let wr0 = new StreamWriter(outputdir+"\\"+filename)
            wr0.Write memberJson
            wr0.Close()
            
    /// メンバー情報（PHP用）
    type _Member(var:PHPdata) =
        member _.ID = var["ID"]
        member _.PassWord = var["PassWord"]
        
    /// メンバー配列（PHP用）
    type _Members(var:PHPdata) =
        member _.Item with get(i:int) = _Member(var[i])
        member _.Item with get(i:num0) = _Member(var[i])
        
    /// メンバーデータ（PHP用）
    type MemberData(varname:string) =
        let memberData = PHPdata varname
        let memberList = memberData["List"]
        member _.List with get() = _Members memberList
        /// メンバーリストをjsonファイルから読み込み
        member _.ReadJSON(filename:string) =
            memberData <== php.json_decode (php.file_get_contents filename,true)
        member _.Length with get() = php.count memberList
        member _.foreach code =
            memberList.foreach <| fun i -> 
                code i
                
let memberdata = "members.json"

/// <summary>
/// メインページ
/// </summary>
let mainPage() =
    php.phpfile (outputdir, "main.php") <| fun () ->
        html.head "Ediass" <| fun () ->
        php.postCheck()
        html.h1 "ログインページサンプル" <| fun () ->
            /// ログインユーザーID入力用テキストボックス
            let textBoxUserID = TextBox "userid"
            /// ログインパスワード入力用テキストボックス
            let textBoxUserPW = TextBox "userpw"
            /// ログインボタン
            let buttonLogin = Button "login"
            html.form "main.php" <| fun () ->
                br.if2 buttonLogin.isset
                <| fun () ->
                    // ログインボタンを押したとき
                    // メンバーリスト読み込み
                    let memberData = MemberData "mdata"
                    memberData.ReadJSON memberdata
                    /// ログイン検証結果：ユーザーが存在しない→0、パスワードが異なる→1、ログイン成功→2
                    let loginState = var.i0 "loginState"
                    loginState <== 0
                    // ログイン情報検証
                    memberData.foreach <| fun i ->
                        br.if1 (memberData.List[i].ID .= textBoxUserID.text) <| fun () ->
                            loginState <== 1
                            br.if1 (memberData.List[i].PassWord .= textBoxUserPW.text) <| fun () ->
                            loginState <== 2
                    br.branch <| fun b ->
                        // ユーザーが存在しない
                        b.IF (loginState .= 0) <| fun () ->
                            codewrite "ユーザーが存在しません<br>"
                            codewrite "ID:"
                            textBoxUserID.show_copy()
                            codewrite "パスワード:"
                            textBoxUserPW.show_password_copy()
                            buttonLogin.show "ログイン"
                        // ユーザーが存在しない
                        b.IF (loginState .= 1) <| fun () ->
                            codewrite "パスワードが誤りです<br>"
                            codewrite "ID:"
                            textBoxUserID.show_copy()
                            codewrite "パスワード:"
                            textBoxUserPW.show_password_copy()
                            buttonLogin.show "ログイン"
                        // ログイン成功
                        b.EL <| fun () ->
                            codewrite "ID:"
                            textBoxUserID.show_copy_lock()
                            codewrite "パスワード:"
                            textBoxUserPW.show_password_copy_lock()
                            buttonLogin.show_disabled "ログイン"
                            codewrite "<br>"
                            codewrite "ログイン後のコンテンツ"
                <| fun () ->
                    // ログインボタンを押していないとき
                    codewrite "ID:"
                    textBoxUserID.show()
                    codewrite "パスワード:"
                    textBoxUserPW.show_password()
                    buttonLogin.show "ログイン"
                ()

let data = [{ID="user001"; PassWord="abcdef"};{ID="user001"; PassWord="abcdef"};]
UserList.makeJSON(outputdir, memberdata, data)
mainPage()
