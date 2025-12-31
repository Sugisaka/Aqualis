(*
-----------------------------------------------------------------
 docwriter
 php・htmlファイル生成用
-----------------------------------------------------------------
 version 3.0.1
-----------------------------------------------------------------
*)
namespace Aqualis

open System
open System.IO
    
type Button(name:string) =
    let b = post name
    /// ボタンが押されたか判定
    member _.isset with get() = php.isset b.get
    /// <summary>
    /// ボタンの表示
    /// </summary>
    /// <param name="file">ボタン押下時の移動先ファイル</param>
    /// <param name="text">ボタンに表示するテキスト</param>
    member _.show(file:string,text:string) = b.submit(file,text)
    member _.show(text:string) = html.submit(name,text)
    member _.show_disabled(text:string) = html.submit_disabled(name,text)
    
type ButtonVar() =
    /// ボタンが押されたか判定
    member _.isset(id:num0) = php.isset (post id).get
    member _.isset(id:string) = php.isset (post id).get
    /// <summary>
    /// ボタンの表示
    /// </summary>
    /// <param name="id">ボタンID</param>
    /// <param name="file">ボタン押下時の移動先ファイル</param>
    /// <param name="text">ボタンに表示するテキスト</param>
    member _.show(id:num0,file:string,text:string) = (post id).submit(file,text)
    /// <summary>
    /// ボタンの表示
    /// </summary>
    /// <param name="id">ボタンID</param>
    /// <param name="text">ボタンに表示するテキスト</param>
    member _.show(id:num0,text:string) = html.submit(id,text)
    /// <summary>
    /// ボタンの表示
    /// </summary>
    /// <param name="id">ボタンID</param>
    /// <param name="text">ボタンに表示するテキスト</param>
    member _.show(id:string,text:string) = html.submit(id,text)
    /// <summary>
    /// ボタンの表示
    /// </summary>
    /// <param name="id">ボタンID</param>
    /// <param name="text">ボタンに表示するテキスト</param>
    member _.show_disabled(id:num0,text:string) = html.submit_disabled(id,text)
    
type TextBox(name:num0) =
    let t = post name
    new(name:string) = TextBox (num0(Var(Nt,name,NaN)))
    /// テキストが送信されたか判定
    member _.isset with get() = php.isset t.get
    /// 送信されたテキスト
    member _.text with get() = t.get
    /// テキストボックスの表示
    member _.show() = t.input()
    /// テキストボックスの表示(送信テキストを表示)
    member _.show_copy() = t.input_copy()
    /// テキストボックスの表示(スタイル指定)
    member _.show(s:string) = t.input s
    member _.show(atr:list<string*string>) = t.input atr
    /// テキストボックスの表示(表示テキストとスタイル指定)
    member _.show(text:string,atr:list<string*string>) = t.input(text,atr)
    /// テキストボックスの表示(表示テキストとスタイル指定)
    member _.show(text:num0,atr:list<string*string>) = t.input(text,atr)
    member _.show_lock(v:num0) = t.input_lock v
    member _.show_lock(v:string) = t.input_lock v
    /// テキストボックスの表示(送信テキストを表示)
    member _.show_copy(atr:list<string*string>) = t.input_copy atr
    /// テキストボックスの表示(送信テキストを表示、編集不可)
    member _.show_copy_lock() = t.input_copy_lock()
    /// テキストボックスの表示(パスワード入力用)
    member _.show_password() = t.password()
    /// テキストボックスの表示(パスワード入力用、送信テキストを表示)
    member _.show_password_copy() = t.password_copy()
    /// テキストボックスの表示(パスワード入力用、送信テキストを表示、編集不可)
    member _.show_password_copy_lock() = t.password_copy_lock()
    
type TextBoxVar() =
    /// テキストが送信されたか判定
    member _.isset(id:num0) = php.isset (post id).get
    /// テキストが送信されたか判定
    member _.isset(id:string) = php.isset (post id).get
    /// 送信されたテキスト
    member _.text(id:num0) = (post id).get
    /// 送信されたテキスト
    member _.text(id:string) = (post id).get
    /// テキストボックスの表示
    member _.show(id:num0) = (post id).input()
    /// テキストボックスの表示
    member _.show(id:string) = (post id).input()
    /// テキストボックスの表示(送信テキストを表示)
    member _.show_copy(id:num0) = (post id).input_copy()
    /// テキストボックスの表示(送信テキストを表示)
    member _.show_copy(id:string) = (post id).input_copy()
    /// テキストボックスの表示(スタイル指定)
    member _.show(id:num0,s:string) = (post id).input s
    /// テキストボックスの表示(スタイル指定)
    member _.show(id:string,s:string) = (post id).input s
    /// テキストボックスの表示(スタイル指定)
    member _.show(id:num0,s:num0) = (post id).input s
    /// テキストボックスの表示(スタイル指定)
    member _.show(id:string,s:num0) = (post id).input s
    /// テキストボックスの表示(スタイル指定)
    member _.show(id:num0,atr:list<string*string>) = (post id).input atr
    /// テキストボックスの表示(スタイル指定)
    member _.show(id:string,atr:list<string*string>) = (post id).input atr
    /// テキストボックスの表示(表示テキストとスタイル指定)
    member _.show(id:num0,text:string,atr:list<string*string>) = (post id).input(text,atr)
    /// テキストボックスの表示(表示テキストとスタイル指定)
    member _.show(id:string,text:string,atr:list<string*string>) = (post id).input(text,atr)
    /// テキストボックスの表示(表示テキストとスタイル指定)
    member _.show(id:num0,text:num0,atr:list<string*string>) = (post id).input(text,atr)
    /// テキストボックスの表示(表示テキストとスタイル指定)
    member _.show(id:string,text:num0,atr:list<string*string>) = (post id).input(text,atr)
    /// テキストボックスの表示(表示テキストとスタイル指定、編集不可)
    member _.show_lock(id:num0,v:num0) = (post id).input_lock v
    /// テキストボックスの表示(表示テキストとスタイル指定、編集不可)
    member _.show_lock(id:string,v:num0) = (post id).input_lock v
    /// テキストボックスの表示(表示テキストとスタイル指定、編集不可)
    member _.show_lock(id:num0,v:string) = (post id).input_lock v
    /// テキストボックスの表示(表示テキストとスタイル指定、編集不可)
    member _.show_lock(id:string,v:string) = (post id).input_lock v
    /// テキストボックスの表示(送信テキストを表示)
    member _.show_copy(id:num0,atr:list<string*string>) = (post id).input_copy atr
    /// テキストボックスの表示(送信テキストを表示)
    member _.show_copy(id:string,atr:list<string*string>) = (post id).input_copy atr
    /// テキストボックスの表示(送信テキストを表示、編集不可)
    member _.show_copy_lock(id:num0) = (post id).input_copy_lock()
    /// テキストボックスの表示(送信テキストを表示、編集不可)
    member _.show_copy_lock(id:string) = (post id).input_copy_lock()
    /// テキストボックスの表示(パスワード入力用)
    member _.show_password(id:num0) = (post id).password()
    /// テキストボックスの表示(パスワード入力用)
    member _.show_password(id:string) = (post id).password()
    /// テキストボックスの表示(パスワード入力用、送信テキストを表示)
    member _.show_password_copy(id:num0) = (post id).password_copy()
    /// テキストボックスの表示(パスワード入力用、送信テキストを表示)
    member _.show_password_copy(id:string) = (post id).password_copy()
    /// テキストボックスの表示(パスワード入力用、送信テキストを表示、編集不可)
    member _.show_password_copy_lock(id:num0) = (post id).password_copy_lock()
    /// テキストボックスの表示(パスワード入力用、送信テキストを表示、編集不可)
    member _.show_password_copy_lock(id:string) = (post id).password_copy_lock()
    
type TextArea(name:string) =
    let a = post name
    /// 送信されたテキスト
    member _.text with get() = a.get
    member _.text_html with get() = a.get_html
    member _.isset with get() = php.isset a.get
    member _.show() = a.textArea()
    member _.show_contents_ (code:unit->unit) = a.textArea code
    member _.show_contents (atr:list<string*string>) = fun (code:unit->unit) -> a.textArea_contents atr code
    member _.show(atr:list<string*string>) = a.textArea atr
    member _.show_copy() = a.textArea()
    member _.show_copy(atr:list<string*string>) = a.textArea_copy atr
    
type ComboBoxItem = {Tag:string; Text:string}

type ComboBox(name:string,items:list<ComboBoxItem>) =
    let c = post name
    /// 選択されたテキスト
    member _.selectedTag with get() = c.get
    /// コンボボックスを表示（指定された選択項目を選択状態にする）
    member this.show_selectedItem(selectedIndex:int) =    
        c.select <| fun () ->
            for i in items do
                //指定された選択肢を選択中とする
                if items[selectedIndex].Text = i.Text then
                    html.option_selected i.Tag <| fun () -> programList[prIndex].codewrite i.Text
                else
                    html.option i.Tag <| fun () -> programList[prIndex].codewrite i.Text
    /// コンボボックスを表示（送信された選択項目を選択状態にする）
    member this.show_selectedItem() =
        //c.select <| fun () ->
        html.select name <| fun () ->
            for i in items do
                br.if2(this.selectedTag .= num0(Var(Nt,i.Tag,NaN)))
                <| fun () ->
                    html.option_selected i.Tag <| fun () -> programList[prIndex].codewrite i.Text
                <| fun () ->
                    html.option i.Tag <| fun () -> programList[prIndex].codewrite i.Text
    /// コンボボックスを表示（送信された選択項目を選択状態にする）
    member this.show_selectedItem(text:num0) =
        //c.select <| fun () ->
        html.select name <| fun () ->
            for i in items do
                br.if2(text .= num0(Var(Nt,i.Text,NaN)))
                <| fun () ->
                    html.option_selected i.Tag <| fun () -> programList[prIndex].codewrite i.Text
                <| fun () ->
                    html.option i.Tag <| fun () -> programList[prIndex].codewrite i.Text
    /// コンボボックスを表示
    member _.show() =
        //c.select <| fun () ->
        html.select name <| fun () ->
            for i in items do
                html.option i.Tag <| fun () -> programList[prIndex].codewrite i.Text
    member _.foreach code =
        for i in items do code i
        
type ComboBoxVar() =
    /// 選択されたテキスト
    member _.selectedTag(id:num0) = (post id).get
    member _.selectedTag(id:string) = (post id).get
    /// コンボボックスを表示（指定された選択項目を選択状態にする）
    member this.show_selectedItem(id:num0,items:list<ComboBoxItem>,selectedIndex:int) =    
        let c = post id
        c.select <| fun () ->
            for i in items do
                //指定された選択肢を選択中とする
                if items[selectedIndex].Text = i.Text then
                    html.option_selected i.Tag <| fun () -> programList[prIndex].codewrite i.Text
                else
                    html.option i.Tag <| fun () -> programList[prIndex].codewrite i.Text
    /// コンボボックスを表示（送信された選択項目を選択状態にする）
    member this.show_selected(id:num0,items:list<ComboBoxItem>) =
        let c = post id
        //c.select <| fun () ->
        html.select id <| fun () ->
            for i in items do
                br.if2(this.selectedTag id .= num0(Var(Nt,i.Tag,NaN)))
                <| fun () ->
                    html.option_selected i.Tag <| fun () -> programList[prIndex].codewrite i.Text
                <| fun () ->
                    html.option i.Tag <| fun () -> programList[prIndex].codewrite i.Text
    /// コンボボックスを表示（送信された選択項目を選択状態にする）
    member this.show_selectedTag(id:num0,items:list<ComboBoxItem>,tag:num0) =
        //c.select <| fun () ->
        html.select id <| fun () ->
            for i in items do
                br.if2(tag .= num0(Var(Nt,i.Tag,NaN)))
                <| fun () ->
                    html.option_selected i.Tag <| fun () -> programList[prIndex].codewrite i.Text
                <| fun () ->
                    html.option i.Tag <| fun () -> programList[prIndex].codewrite i.Text
    /// コンボボックスを表示（送信された選択項目を選択状態にする）
    member this.show_selectedTag_disabled(id:num0,items:list<ComboBoxItem>,tag:num0) =
        //c.select <| fun () ->
        html.select_disabled id <| fun () ->
            for i in items do
                br.if2(tag .= num0(Var(Nt,i.Tag,NaN)))
                <| fun () ->
                    html.option_selected i.Tag <| fun () -> programList[prIndex].codewrite i.Text
                <| fun () ->
                    html.option i.Tag <| fun () -> programList[prIndex].codewrite i.Text
    /// コンボボックスを表示（送信された選択項目を選択状態にする）
    member this.show_selectedItem(id:num0,items:list<ComboBoxItem>,text:num0) =
        //c.select <| fun () ->
        html.select id <| fun () ->
            for i in items do
                br.if2(text .= num0(Var(Nt,i.Text,NaN)))
                <| fun () ->
                    html.option_selected i.Tag <| fun () -> programList[prIndex].codewrite i.Text
                <| fun () ->
                    html.option i.Tag <| fun () -> programList[prIndex].codewrite i.Text
    /// コンボボックスを表示（送信された選択項目を選択状態にする）
    member this.show_selectedItem_disabled(id:num0,items:list<ComboBoxItem>,text:num0) =
        //c.select <| fun () ->
        html.select_disabled id <| fun () ->
            for i in items do
                br.if2(text .= num0(Var(Nt,i.Text,NaN)))
                <| fun () ->
                    html.option_selected i.Tag <| fun () -> programList[prIndex].codewrite i.Text
                <| fun () ->
                    html.option i.Tag <| fun () -> programList[prIndex].codewrite i.Text

    /// コンボボックスを表示
    member _.show(id:num0,items:list<ComboBoxItem>) =
        //c.select <| fun () ->
        html.select id <| fun () ->
            for i in items do
                html.option i.Tag <| fun () -> programList[prIndex].codewrite i.Text
    member _.foreach (items:list<ComboBoxItem>) code =
        for i in items do code i
        
type CheckBox(name:num0) =
    let cb = post name
    new(name:string) = CheckBox (num0(Var(Nt,name,NaN)))
    member _.isChecked with get() = cb.get .= 1
    member _.status with get() = cb.get
    member _.show() = html.checkbox name
    /// チェックボックス（チェックされたとき1、チェックされていないとき0を送信）
    member _.show_disabled() = html.checkbox_disabled name
    /// チェックボックス（チェックされたとき1、チェックされていないとき0を送信）
    member _.show_checked() = html.checkbox_checked name
    /// チェックボックス（チェックされたとき1、チェックされていないとき0を送信）
    member _.show_checked_disabled() = html.checkbox_checked_disabled name
    
/// IDによって複数のチェックボックスを表す
type CheckBoxVar() =
    member _.isChecked(id:num0) = (post id).get .= 1
    member _.status(id:num0) = (post id).get
    member _.show(id:num0) = html.checkbox id
    /// チェックボックス（チェックされたとき1、チェックされていないとき0を送信）
    member _.show_disabled(id:num0) = html.checkbox_disabled id
    /// チェックボックス（チェックされたとき1、チェックされていないとき0を送信）
    member _.show_checked(id:num0) = html.checkbox_checked id
    /// チェックボックス（チェックされたとき1、チェックされていないとき0を送信）
    member _.show_checked_disabled(id:num0) = html.checkbox_checked_disabled id
