// 
// Copyright (c) 2026 Jun-ichiro Sugisaka
// 
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
// 
namespace Aqualis

open System
open System.IO
    
type Button(name:PHPdata) =
    let b = post name
    new(name:string) = Button (PHPdata name)
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
    member _.isset(id:PHPdata) = php.isset (post id).get
    member _.isset(id:string) = php.isset (post id).get
    /// <summary>
    /// ボタンの表示
    /// </summary>
    /// <param name="id">ボタンID</param>
    /// <param name="file">ボタン押下時の移動先ファイル</param>
    /// <param name="text">ボタンに表示するテキスト</param>
    member _.show(id:PHPdata,file:string,text:string) = (post id).submit(file,text)
    /// <summary>
    /// ボタンの表示
    /// </summary>
    /// <param name="id">ボタンID</param>
    /// <param name="text">ボタンに表示するテキスト</param>
    member _.show(id:PHPdata,text:string) = html.submit(id,text)
    /// <summary>
    /// ボタンの表示
    /// </summary>
    /// <param name="id">ボタンID</param>
    /// <param name="text">ボタンに表示するテキスト</param>
    member _.show(id:num0,text:string) = html.submit(PHPdata id,text)
    /// <summary>
    /// ボタンの表示
    /// </summary>
    /// <param name="id">ボタンID</param>
    /// <param name="text">ボタンに表示するテキスト</param>
    member _.show(id:string,text:string) = html.submit(PHPdata id,text)
    /// <summary>
    /// ボタンの表示
    /// </summary>
    /// <param name="id">ボタンID</param>
    /// <param name="text">ボタンに表示するテキスト</param>
    member _.show_disabled(id:PHPdata,text:string) = html.submit_disabled(id,text)
    
type TextBox(name:PHPdata) =
    let t = post name
    new(name:string) = TextBox (PHPdata name)
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
    member _.show(text:PHPdata,atr:list<string*string>) = t.input(text,atr)
    member _.show_lock(v:PHPdata) = t.input_lock v
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
    member _.isset(id:PHPdata) = php.isset (post id).get
    /// テキストが送信されたか判定
    member _.isset(id:string) = php.isset (post id).get
    /// 送信されたテキスト
    member _.text(id:PHPdata) = (post id).get
    /// 送信されたテキスト
    member _.text(id:string) = (post id).get
    /// テキストボックスの表示
    member _.show(id:PHPdata) = (post id).input()
    /// テキストボックスの表示
    member _.show(id:string) = (post id).input()
    /// テキストボックスの表示(送信テキストを表示)
    member _.show_copy(id:PHPdata) = (post id).input_copy()
    /// テキストボックスの表示(送信テキストを表示)
    member _.show_copy(id:string) = (post id).input_copy()
    /// テキストボックスの表示(スタイル指定)
    member _.show(id:PHPdata,s:string) = (post id).input s
    /// テキストボックスの表示(スタイル指定)
    member _.show(id:string,s:string) = (post id).input s
    /// テキストボックスの表示(スタイル指定)
    member _.show(id:PHPdata,s:PHPdata) = (post id).input s
    /// テキストボックスの表示(スタイル指定)
    member _.show(id:string,s:PHPdata) = (post id).input s
    /// テキストボックスの表示(スタイル指定)
    member _.show(id:PHPdata,atr:list<string*string>) = (post id).input atr
    /// テキストボックスの表示(スタイル指定)
    member _.show(id:string,atr:list<string*string>) = (post id).input atr
    /// テキストボックスの表示(表示テキストとスタイル指定)
    member _.show(id:PHPdata,text:string,atr:list<string*string>) = (post id).input(text,atr)
    /// テキストボックスの表示(表示テキストとスタイル指定)
    member _.show(id:string,text:string,atr:list<string*string>) = (post id).input(text,atr)
    /// テキストボックスの表示(表示テキストとスタイル指定)
    member _.show(id:PHPdata,text:PHPdata,atr:list<string*string>) = (post id).input(text,atr)
    /// テキストボックスの表示(表示テキストとスタイル指定)
    member _.show(id:string,text:PHPdata,atr:list<string*string>) = (post id).input(text,atr)
    /// テキストボックスの表示(表示テキストとスタイル指定、編集不可)
    member _.show_lock(id:PHPdata,v:PHPdata) = (post id).input_lock v
    /// テキストボックスの表示(表示テキストとスタイル指定、編集不可)
    member _.show_lock(id:string,v:PHPdata) = (post id).input_lock v
    /// テキストボックスの表示(表示テキストとスタイル指定、編集不可)
    member _.show_lock(id:PHPdata,v:string) = (post id).input_lock v
    /// テキストボックスの表示(表示テキストとスタイル指定、編集不可)
    member _.show_lock(id:string,v:string) = (post id).input_lock v
    /// テキストボックスの表示(送信テキストを表示)
    member _.show_copy(id:PHPdata,atr:list<string*string>) = (post id).input_copy atr
    /// テキストボックスの表示(送信テキストを表示)
    member _.show_copy(id:string,atr:list<string*string>) = (post id).input_copy atr
    /// テキストボックスの表示(送信テキストを表示、編集不可)
    member _.show_copy_lock(id:PHPdata) = (post id).input_copy_lock()
    /// テキストボックスの表示(送信テキストを表示、編集不可)
    member _.show_copy_lock(id:string) = (post id).input_copy_lock()
    /// テキストボックスの表示(パスワード入力用)
    member _.show_password(id:PHPdata) = (post id).password()
    /// テキストボックスの表示(パスワード入力用)
    member _.show_password(id:string) = (post id).password()
    /// テキストボックスの表示(パスワード入力用、送信テキストを表示)
    member _.show_password_copy(id:PHPdata) = (post id).password_copy()
    /// テキストボックスの表示(パスワード入力用、送信テキストを表示)
    member _.show_password_copy(id:string) = (post id).password_copy()
    /// テキストボックスの表示(パスワード入力用、送信テキストを表示、編集不可)
    member _.show_password_copy_lock(id:PHPdata) = (post id).password_copy_lock()
    /// テキストボックスの表示(パスワード入力用、送信テキストを表示、編集不可)
    member _.show_password_copy_lock(id:string) = (post id).password_copy_lock()
    
type TextArea(name:PHPdata) =
    let a = post name
    /// 送信されたテキスト
    new(name:string) = TextArea (PHPdata name)
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

type ComboBox(name:PHPdata,items:list<ComboBoxItem>) =
    let c = post name
    new(name:string,items) = ComboBox(PHPdata name,items)
    /// 選択されたテキスト
    member _.selectedTag with get() = c.get
    /// コンボボックスを表示（指定された選択項目を選択状態にする）
    member this.show_selectedItem(selectedIndex:int) =    
        c.select <| fun () ->
            for i in items do
                //指定された選択肢を選択中とする
                if items[selectedIndex].Text = i.Text then
                    html.option_selected i.Tag <| fun () -> codewritein i.Text
                else
                    html.option i.Tag <| fun () -> codewritein i.Text
    /// コンボボックスを表示（送信された選択項目を選択状態にする）
    member this.show_selectedItem() =
        //c.select <| fun () ->
        html.select name <| fun () ->
            for i in items do
                br.if2(this.selectedTag .= PHPdata i.Tag)
                <| fun () ->
                    html.option_selected i.Tag <| fun () -> codewritein i.Text
                <| fun () ->
                    html.option i.Tag <| fun () -> codewritein i.Text
    /// コンボボックスを表示（送信された選択項目を選択状態にする）
    member this.show_selectedItem(text:num0) =
        //c.select <| fun () ->
        html.select name <| fun () ->
            for i in items do
                br.if2(text .= num0(Var(Nt,i.Text,NaN)))
                <| fun () ->
                    html.option_selected i.Tag <| fun () -> codewritein i.Text
                <| fun () ->
                    html.option i.Tag <| fun () -> codewritein i.Text
    /// コンボボックスを表示（送信された選択項目を選択状態にする）
    member this.show_selectedItem(text:PHPdata) =
        //c.select <| fun () ->
        html.select name <| fun () ->
            for i in items do
                br.if2(text .= PHPdata i.Text)
                <| fun () ->
                    html.option_selected i.Tag <| fun () -> codewritein i.Text
                <| fun () ->
                    html.option i.Tag <| fun () -> codewritein i.Text
    /// コンボボックスを表示
    member _.show() =
        //c.select <| fun () ->
        html.select name <| fun () ->
            for i in items do
                html.option i.Tag <| fun () -> codewritein i.Text
    member _.foreach code =
        for i in items do code i
        
type ComboBoxVar() =
    /// 選択されたテキスト
    member _.selectedTag(id:PHPdata) = (post id).get
    member _.selectedTag(id:num0) = (post id).get
    member _.selectedTag(id:string) = (post id).get
    /// コンボボックスを表示（指定された選択項目を選択状態にする）
    member this.show_selectedItem(id:PHPdata,items:list<ComboBoxItem>,selectedIndex:int) =    
        let c = post id
        c.select <| fun () ->
            for i in items do
                //指定された選択肢を選択中とする
                if items[selectedIndex].Text = i.Text then
                    html.option_selected i.Tag <| fun () -> codewritein i.Text
                else
                    html.option i.Tag <| fun () -> codewritein i.Text
    /// コンボボックスを表示（送信された選択項目を選択状態にする）
    member this.show_selected(id:PHPdata,items:list<ComboBoxItem>) =
        let c = post id
        //c.select <| fun () ->
        html.select id <| fun () ->
            for i in items do
                br.if2(this.selectedTag id .= PHPdata i.Tag)
                <| fun () ->
                    html.option_selected i.Tag <| fun () -> codewritein i.Text
                <| fun () ->
                    html.option i.Tag <| fun () -> codewritein i.Text
    /// コンボボックスを表示（送信された選択項目を選択状態にする）
    member this.show_selectedTag(id:PHPdata,items:list<ComboBoxItem>,tag:PHPdata) =
        //c.select <| fun () ->
        html.select id <| fun () ->
            for i in items do
                br.if2(tag .= num0(Var(Nt,i.Tag,NaN)))
                <| fun () ->
                    html.option_selected i.Tag <| fun () -> codewritein i.Text
                <| fun () ->
                    html.option i.Tag <| fun () -> codewritein i.Text
    /// コンボボックスを表示（送信された選択項目を選択状態にする）
    member this.show_selectedTag_disabled(id:PHPdata,items:list<ComboBoxItem>,tag:PHPdata) =
        //c.select <| fun () ->
        html.select_disabled id <| fun () ->
            for i in items do
                br.if2(tag .= num0(Var(Nt,i.Tag,NaN)))
                <| fun () ->
                    html.option_selected i.Tag <| fun () -> codewritein i.Text
                <| fun () ->
                    html.option i.Tag <| fun () -> codewritein i.Text
    /// コンボボックスを表示（送信された選択項目を選択状態にする）
    member this.show_selectedItem(id:PHPdata,items:list<ComboBoxItem>,text:num0) =
        //c.select <| fun () ->
        html.select id <| fun () ->
            for i in items do
                br.if2(text .= num0(Var(Nt,i.Text,NaN)))
                <| fun () ->
                    html.option_selected i.Tag <| fun () -> codewritein i.Text
                <| fun () ->
                    html.option i.Tag <| fun () -> codewritein i.Text
    /// コンボボックスを表示（送信された選択項目を選択状態にする）
    member this.show_selectedItem_disabled(id:PHPdata,items:list<ComboBoxItem>,text:num0) =
        //c.select <| fun () ->
        html.select_disabled id <| fun () ->
            for i in items do
                br.if2(text .= num0(Var(Nt,i.Text,NaN)))
                <| fun () ->
                    html.option_selected i.Tag <| fun () -> codewritein i.Text
                <| fun () ->
                    html.option i.Tag <| fun () -> codewritein i.Text

    /// コンボボックスを表示
    member _.show(id:PHPdata,items:list<ComboBoxItem>) =
        //c.select <| fun () ->
        html.select id <| fun () ->
            for i in items do
                html.option i.Tag <| fun () -> codewritein i.Text
    member _.foreach (items:list<ComboBoxItem>) code =
        for i in items do code i
        
type CheckBox(name:PHPdata) =
    let cb = post name
    new(name:string) = CheckBox (PHPdata name)
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
    member _.isChecked(id:PHPdata) = (post id).get .= 1
    member _.status(id:PHPdata) = (post id).get
    member _.show(id:PHPdata) = html.checkbox id
    /// チェックボックス（チェックされたとき1、チェックされていないとき0を送信）
    member _.show_disabled(id:PHPdata) = html.checkbox_disabled id
    /// チェックボックス（チェックされたとき1、チェックされていないとき0を送信）
    member _.show_checked(id:PHPdata) = html.checkbox_checked id
    /// チェックボックス（チェックされたとき1、チェックされていないとき0を送信）
    member _.show_checked_disabled(id:PHPdata) = html.checkbox_checked_disabled id
