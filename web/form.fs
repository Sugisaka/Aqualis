// 
// Copyright (c) 2026 Jun-ichiro Sugisaka
// 
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
// 
namespace Aqualis

open System
open System.IO
    
type Button(context:Aqualis,name:PHPdata) =
    let b = post(context,name)
    new(ctx:Aqualis,name:string) = Button(ctx,PHPdata name)
    /// ボタンが押されたか判定
    member _.isset with get() = context.php.isset b.get
    /// ボタンが押されていないか判定
    member _.isNotset with get() = context.php.isNotset b.get
    /// <summary>
    /// ボタンの表示
    /// </summary>
    /// <param name="file">ボタン押下時の移動先ファイル</param>
    /// <param name="text">ボタンに表示するテキスト</param>
    member _.show(file:string,text:string) = b.submit(file,text)
    member _.show(text:string) = context.html.submit(name,text)
    member _.show_disabled(text:string) = context.html.submit_disabled(name,text)
    
type ButtonVar(context:Aqualis) =
    /// ボタンが押されたか判定
    member _.isset(id:PHPdata) = context.php.isset (post(context,id)).get
    member _.isset(id:string) = context.php.isset (post(context,id)).get
    /// <summary>
    /// ボタンの表示
    /// </summary>
    /// <param name="id">ボタンID</param>
    /// <param name="file">ボタン押下時の移動先ファイル</param>
    /// <param name="text">ボタンに表示するテキスト</param>
    member _.show(id:PHPdata,file:string,text:string) = (post(context,id)).submit(file,text)
    /// <summary>
    /// ボタンの表示
    /// </summary>
    /// <param name="id">ボタンID</param>
    /// <param name="text">ボタンに表示するテキスト</param>
    member _.show(id:PHPdata,text:string) = context.html.submit(id,text)
    /// <summary>
    /// ボタンの表示
    /// </summary>
    /// <param name="id">ボタンID</param>
    /// <param name="text">ボタンに表示するテキスト</param>
    member _.show(id:int0,text:string) = context.html.submit(PHPdata id,text)
    member _.show(id:double0,text:string) = context.html.submit(PHPdata id,text)
    member _.show(id:complex0,text:string) = context.html.submit(PHPdata id,text)
    /// <summary>
    /// ボタンの表示
    /// </summary>
    /// <param name="id">ボタンID</param>
    /// <param name="text">ボタンに表示するテキスト</param>
    member _.show(id:string,text:string) = context.html.submit(PHPdata id,text)
    /// <summary>
    /// ボタンの表示
    /// </summary>
    /// <param name="id">ボタンID</param>
    /// <param name="text">ボタンに表示するテキスト</param>
    member _.show_disabled(id:PHPdata,text:string) = context.html.submit_disabled(id,text)
    
type TextBox(context:Aqualis,name:PHPdata) =
    let t = post(context,name)
    new(ctx:Aqualis,name:string) = TextBox(ctx,PHPdata name)
    /// テキストが送信されたか判定
    member _.isset with get() = context.php.isset t.get
    /// 送信されたテキスト
    member _.text with get() = t.get
    /// テキストボックスの表示
    member _.show() = t.input()
    /// テキストボックスの表示(送信テキストを表示)
    member _.show_copy() = t.input_copy()
    /// 非表示テキストボックス(送信テキストを設定)
    member _.show_copy_hidden() = t.input_copy_hidden()
    /// テキストボックスの表示(スタイル指定)
    member _.show(s:string) = t.input s
    member _.show(atr:list<Atr>) = t.input atr
    /// テキストボックスの表示(表示テキストとスタイル指定)
    member _.show(text:string,atr:list<Atr>) = t.input(text,atr)
    /// テキストボックスの表示(表示テキストとスタイル指定)
    member _.show(text:PHPdata,atr:list<Atr>) = t.input(text,atr)
    /// テキストボックスの表示(表示テキストとスタイル指定)
    member _.show(text:int0,atr:list<Atr>) = t.input(PHPdata text,atr)
    member _.show(text:double0,atr:list<Atr>) = t.input(PHPdata text,atr)
    /// 非表示テキストボックス(表示テキストとスタイル指定)
    member _.show_hidden(text:string,atr:list<Atr>) = t.input_hidden(text,atr)
    /// 非表示テキストボックス(表示テキストとスタイル指定)
    member _.show_hidden(text:PHPdata,atr:list<Atr>) = t.input_hidden(text,atr)
    /// 非表示テキストボックス(表示テキストとスタイル指定)
    member _.show_hidden(text:int0,atr:list<Atr>) = t.input_hidden(PHPdata text,atr)
    member _.show_hidden(text:double0,atr:list<Atr>) = t.input_hidden(PHPdata text,atr)
    member _.show_lock(v:PHPdata) = t.input_lock v
    member _.show_lock(v:int0) = t.input_lock v
    member _.show_lock(v:double0) = t.input_lock v
    member _.show_lock(v:string) = t.input_lock v
    member _.show_lock(v:PHPdata,atr:list<Atr>) = t.input_lock (v,atr)
    member _.show_lock(v:int0,atr:list<Atr>) = t.input_lock (v,atr)
    member _.show_lock(v:double0,atr:list<Atr>) = t.input_lock (v,atr)
    member _.show_lock(v:string,atr:list<Atr>) = t.input_lock (v,atr)
    /// テキストボックスの表示(送信テキストを表示)
    member _.show_copy(atr:list<Atr>) = t.input_copy atr
    /// 非表示テキストボックス(送信テキストを表示)
    member _.show_copy_hidden(atr:list<Atr>) = t.input_copy_hidden atr
    /// テキストボックスの表示(送信テキストを表示、編集不可)
    member _.show_copy_lock() = t.input_copy_lock()
    /// テキストボックスの表示(パスワード入力用)
    member _.show_password() = t.password()
    /// テキストボックスの表示(パスワード入力用、送信テキストを表示)
    member _.show_password_copy() = t.password_copy()
    /// テキストボックスの表示(パスワード入力用、送信テキストを表示、編集不可)
    member _.show_password_copy_lock() = t.password_copy_lock()
    
type TextBoxVar(context:Aqualis) =
    /// テキストが送信されたか判定
    member _.isset(id:PHPdata) = context.php.isset (post(context,id)).get
    /// テキストが送信されたか判定
    member _.isset(id:string) = context.php.isset (post(context,id)).get
    /// 送信されたテキスト
    member _.text(id:PHPdata) = (post(context,id)).get
    /// 送信されたテキスト
    member _.text(id:string) = (post(context,id)).get
    /// テキストボックスの表示
    member _.show(id:PHPdata) = (post(context,id)).input()
    /// テキストボックスの表示
    member _.show(id:string) = (post(context,id)).input()
    /// テキストボックスの表示(送信テキストを表示)
    member _.show_copy(id:PHPdata) = (post(context,id)).input_copy()
    /// テキストボックスの表示(送信テキストを表示)
    member _.show_copy(id:string) = (post(context,id)).input_copy()
    /// テキストボックスの表示(スタイル指定)
    member _.show(id:PHPdata,s:string) = (post(context,id)).input s
    /// テキストボックスの表示(スタイル指定)
    member _.show(id:string,s:string) = (post(context,id)).input s
    /// テキストボックスの表示(スタイル指定)
    member _.show(id:PHPdata,s:PHPdata) = (post(context,id)).input s
    /// テキストボックスの表示(スタイル指定)
    member _.show(id:string,s:PHPdata) = (post(context,id)).input s
    /// テキストボックスの表示(スタイル指定)
    member _.show(id:PHPdata,atr:list<Atr>) = (post(context,id)).input atr
    /// テキストボックスの表示(スタイル指定)
    member _.show(id:string,atr:list<Atr>) = (post(context,id)).input atr
    /// テキストボックスの表示(表示テキストとスタイル指定)
    member _.show(id:PHPdata,text:string,atr:list<Atr>) = (post(context,id)).input(text,atr)
    /// テキストボックスの表示(表示テキストとスタイル指定)
    member _.show(id:string,text:string,atr:list<Atr>) = (post(context,id)).input(text,atr)
    /// テキストボックスの表示(表示テキストとスタイル指定)
    member _.show(id:PHPdata,text:PHPdata,atr:list<Atr>) = (post(context,id)).input(text,atr)
    /// テキストボックスの表示(表示テキストとスタイル指定)
    member _.show(id:string,text:PHPdata,atr:list<Atr>) = (post(context,id)).input(text,atr)
    /// テキストボックスの表示(表示テキストとスタイル指定、編集不可)
    member _.show_lock(id:PHPdata,v:PHPdata) = (post(context,id)).input_lock v
    /// テキストボックスの表示(表示テキストとスタイル指定、編集不可)
    member _.show_lock(id:string,v:PHPdata) = (post(context,id)).input_lock v
    /// テキストボックスの表示(表示テキストとスタイル指定、編集不可)
    member _.show_lock(id:PHPdata,v:string) = (post(context,id)).input_lock v
    /// テキストボックスの表示(表示テキストとスタイル指定、編集不可)
    member _.show_lock(id:string,v:string) = (post(context,id)).input_lock v
    /// テキストボックスの表示(送信テキストを表示)
    member _.show_copy(id:PHPdata,atr:list<Atr>) = (post(context,id)).input_copy atr
    /// テキストボックスの表示(送信テキストを表示)
    member _.show_copy(id:string,atr:list<Atr>) = (post(context,id)).input_copy atr
    /// テキストボックスの表示(送信テキストを表示、編集不可)
    member _.show_copy_lock(id:PHPdata) = (post(context,id)).input_copy_lock()
    /// テキストボックスの表示(送信テキストを表示、編集不可)
    member _.show_copy_lock(id:string) = (post(context,id)).input_copy_lock()
    /// テキストボックスの表示(パスワード入力用)
    member _.show_password(id:PHPdata) = (post(context,id)).password()
    /// テキストボックスの表示(パスワード入力用)
    member _.show_password(id:string) = (post(context,id)).password()
    /// テキストボックスの表示(パスワード入力用、送信テキストを表示)
    member _.show_password_copy(id:PHPdata) = (post(context,id)).password_copy()
    /// テキストボックスの表示(パスワード入力用、送信テキストを表示)
    member _.show_password_copy(id:string) = (post(context,id)).password_copy()
    /// テキストボックスの表示(パスワード入力用、送信テキストを表示、編集不可)
    member _.show_password_copy_lock(id:PHPdata) = (post(context,id)).password_copy_lock()
    /// テキストボックスの表示(パスワード入力用、送信テキストを表示、編集不可)
    member _.show_password_copy_lock(id:string) = (post(context,id)).password_copy_lock()
    
type TextArea(context:Aqualis,name:PHPdata) =
    let a = post(context,name)
    /// 送信されたテキスト
    new(ctx:Aqualis,name:string) = TextArea(ctx,PHPdata name)
    member _.text with get() = a.get
    member _.text_html with get() = a.get_html
    member _.isset with get() = context.php.isset a.get
    member _.show() = a.textArea()
    member _.show_contents_ (code:unit->unit) = a.textArea code
    member _.show_contents (atr:list<Atr>) = fun (code:unit->unit) -> a.textArea_contents atr code
    member _.show(atr:list<Atr>) = a.textArea atr
    member _.show_copy() = a.textArea()
    member _.show_copy(atr:list<Atr>) = a.textArea_copy atr
    
type ComboBoxItem = {Tag:string; Text:string}

type ComboBox(context:Aqualis,name:PHPdata,items:list<ComboBoxItem>) =
    let c = post(context,name)
    new(ctx:Aqualis,name:string,items) = ComboBox(ctx,PHPdata name,items)
    /// 選択されたテキスト
    member _.selectedTag with get() = c.get
    /// コンボボックスを表示（指定された選択項目を選択状態にする）
    member this.show_selectedItem(selectedIndex:int) =    
        c.select <| fun () ->
            for i in items do
                //指定された選択肢を選択中とする
                if items[selectedIndex].Text = i.Text then
                    context.html.option_selected i.Tag <| fun () -> context.writein  i.Text
                else
                    context.html.option i.Tag <| fun () -> context.writein  i.Text
    /// コンボボックスを表示（送信された選択項目を選択状態にする）
    member this.show_selectedItem() =
        //c.select <| fun () ->
        context.html.select name <| fun () ->
            for i in items do
                context.br.if2(this.selectedTag .= PHPdata i.Tag)
                <| fun () ->
                    context.html.option_selected i.Tag <| fun () -> context.writein  i.Text
                <| fun () ->
                    context.html.option i.Tag <| fun () -> context.writein  i.Text
    /// コンボボックスを表示（送信された選択項目を選択状態にする）
    member this.show_selectedItem(text:PHPdata) =
        //c.select <| fun () ->
        context.html.select name <| fun () ->
            for i in items do
                context.br.if2(text .= PHPdata i.Text)
                <| fun () ->
                    context.html.option_selected i.Tag <| fun () -> context.writein  i.Text
                <| fun () ->
                    context.html.option i.Tag <| fun () -> context.writein  i.Text
    /// コンボボックスを表示
    member _.show() =
        //c.select <| fun () ->
        context.html.select name <| fun () ->
            for i in items do
                context.html.option i.Tag <| fun () -> context.writein  i.Text
    member _.foreach code =
        for i in items do code i
        
type ComboBoxVar(context:Aqualis) =
    /// 選択されたテキスト
    member _.selectedTag(id:PHPdata) = (post(context,id)).get
    member _.selectedTag(id:int0) = (post(context,id)).get
    member _.selectedTag(id:string) = (post(context,id)).get
    /// コンボボックスを表示（指定された選択項目を選択状態にする）
    member this.show_selectedItem(id:PHPdata,items:list<ComboBoxItem>,selectedIndex:int) =    
        let c = post(context,id)
        c.select <| fun () ->
            for i in items do
                //指定された選択肢を選択中とする
                if items[selectedIndex].Text = i.Text then
                    context.html.option_selected i.Tag <| fun () -> context.writein  i.Text
                else
                    context.html.option i.Tag <| fun () -> context.writein  i.Text
    /// コンボボックスを表示（送信された選択項目を選択状態にする）
    member this.show_selected(id:PHPdata,items:list<ComboBoxItem>) =
        let c = post(context,id)
        //c.select <| fun () ->
        context.html.select id <| fun () ->
            for i in items do
                context.br.if2(this.selectedTag id .= PHPdata i.Tag)
                <| fun () ->
                    context.html.option_selected i.Tag <| fun () -> context.writein  i.Text
                <| fun () ->
                    context.html.option i.Tag <| fun () -> context.writein  i.Text
    /// コンボボックスを表示（送信された選択項目を選択状態にする）
    member this.show_selectedTag(id:PHPdata,items:list<ComboBoxItem>,tag:PHPdata) =
        //c.select <| fun () ->
        context.html.select id <| fun () ->
            for i in items do
                context.br.if2(tag .= i.Tag)
                <| fun () ->
                    context.html.option_selected i.Tag <| fun () -> context.writein  i.Text
                <| fun () ->
                    context.html.option i.Tag <| fun () -> context.writein  i.Text
    /// コンボボックスを表示（送信された選択項目を選択状態にする）
    member this.show_selectedTag_disabled(id:PHPdata,items:list<ComboBoxItem>,tag:PHPdata) =
        //c.select <| fun () ->
        context.html.select_disabled id <| fun () ->
            for i in items do
                context.br.if2(tag .= i.Tag)
                <| fun () ->
                    context.html.option_selected i.Tag <| fun () -> context.writein  i.Text
                <| fun () ->
                    context.html.option i.Tag <| fun () -> context.writein  i.Text
    /// コンボボックスを表示（送信された選択項目を選択状態にする）
    member this.show_selectedItem(id:PHPdata,items:list<ComboBoxItem>,text:PHPdata) =
        //c.select <| fun () ->
        context.html.select id <| fun () ->
            for i in items do
                context.br.if2(text .= i.Text)
                <| fun () ->
                    context.html.option_selected i.Tag <| fun () -> context.writein  i.Text
                <| fun () ->
                    context.html.option i.Tag <| fun () -> context.writein  i.Text
    /// コンボボックスを表示（送信された選択項目を選択状態にする）
    member this.show_selectedItem_disabled(id:PHPdata,items:list<ComboBoxItem>,text:PHPdata) =
        //c.select <| fun () ->
        context.html.select_disabled id <| fun () ->
            for i in items do
                context.br.if2(text .= i.Text)
                <| fun () ->
                    context.html.option_selected i.Tag <| fun () -> context.writein  i.Text
                <| fun () ->
                    context.html.option i.Tag <| fun () -> context.writein  i.Text

    /// コンボボックスを表示
    member _.show(id:PHPdata,items:list<ComboBoxItem>) =
        //c.select <| fun () ->
        context.html.select id <| fun () ->
            for i in items do
                context.html.option i.Tag <| fun () -> context.writein  i.Text
    member _.foreach (items:list<ComboBoxItem>) code =
        for i in items do code i
        
type CheckBox(context:Aqualis,name:PHPdata) =
    let cb = post(context,name)
    new(ctx:Aqualis,name:string) = CheckBox(ctx,PHPdata name)
    member _.isChecked with get() = cb.get .= 1
    member _.status with get() = cb.get
    member _.show() = context.html.checkbox name
    /// チェックボックス（チェックされたとき1、チェックされていないとき0を送信）
    member _.show_disabled() = context.html.checkbox_disabled name
    /// チェックボックス（チェックされたとき1、チェックされていないとき0を送信）
    member _.show_checked() = context.html.checkbox_checked name
    /// チェックボックス（チェックされたとき1、チェックされていないとき0を送信）
    member _.show_checked_disabled() = context.html.checkbox_checked_disabled name
    
/// IDによって複数のチェックボックスを表す
type CheckBoxVar(context:Aqualis) =
    member _.isChecked(id:PHPdata) = (post(context,id)).get .= 1
    member _.status(id:PHPdata) = (post(context,id)).get
    member _.show(id:PHPdata) = context.html.checkbox id
    /// チェックボックス（チェックされたとき1、チェックされていないとき0を送信）
    member _.show_disabled(id:PHPdata) = context.html.checkbox_disabled id
    /// チェックボックス（チェックされたとき1、チェックされていないとき0を送信）
    member _.show_checked(id:PHPdata) = context.html.checkbox_checked id
    /// チェックボックス（チェックされたとき1、チェックされていないとき0を送信）
    member _.show_checked_disabled(id:PHPdata) = context.html.checkbox_checked_disabled id

[<AutoOpen>]
module CompilationEnvironmentFormExtensions =
    type ContextForm internal (context:Aqualis) =
        member _.textBox(name:string) = TextBox(context,name)
        member _.button(name:string) = Button(context,name)

    type Aqualis with
        ///<summary>HTMLフォーム生成</summary>
        member this.form = ContextForm(this)
