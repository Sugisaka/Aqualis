// 
// Copyright (c) 2026 Jun-ichiro Sugisaka
// 
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
// 
namespace Aqualis

open System
open System.IO
    
type Button(environment:CompilationEnvironment,name:PHPdata) =
    let b = post(environment,name)
    new(environment:CompilationEnvironment,name:string) = Button(environment,PHPdata name)
    /// ボタンが押されたか判定
    member _.isset with get() = environment.php.isset b.get
    /// ボタンが押されていないか判定
    member _.isNotset with get() = environment.php.isNotset b.get
    /// <summary>
    /// ボタンの表示
    /// </summary>
    /// <param name="file">ボタン押下時の移動先ファイル</param>
    /// <param name="text">ボタンに表示するテキスト</param>
    member _.show(file:string,text:string) = b.submit(file,text)
    member _.show(text:string) = environment.webhtml.submit(name,text)
    member _.show_disabled(text:string) = environment.webhtml.submit_disabled(name,text)
    
type ButtonVar(environment:CompilationEnvironment) =
    /// ボタンが押されたか判定
    member _.isset(id:PHPdata) = environment.php.isset (post(environment,id)).get
    member _.isset(id:string) = environment.php.isset (post(environment,id)).get
    /// <summary>
    /// ボタンの表示
    /// </summary>
    /// <param name="id">ボタンID</param>
    /// <param name="file">ボタン押下時の移動先ファイル</param>
    /// <param name="text">ボタンに表示するテキスト</param>
    member _.show(id:PHPdata,file:string,text:string) = (post(environment,id)).submit(file,text)
    /// <summary>
    /// ボタンの表示
    /// </summary>
    /// <param name="id">ボタンID</param>
    /// <param name="text">ボタンに表示するテキスト</param>
    member _.show(id:PHPdata,text:string) = environment.webhtml.submit(id,text)
    /// <summary>
    /// ボタンの表示
    /// </summary>
    /// <param name="id">ボタンID</param>
    /// <param name="text">ボタンに表示するテキスト</param>
    member _.show(id:int0,text:string) = environment.webhtml.submit(PHPdata id,text)
    member _.show(id:double0,text:string) = environment.webhtml.submit(PHPdata id,text)
    member _.show(id:complex0,text:string) = environment.webhtml.submit(PHPdata id,text)
    /// <summary>
    /// ボタンの表示
    /// </summary>
    /// <param name="id">ボタンID</param>
    /// <param name="text">ボタンに表示するテキスト</param>
    member _.show(id:string,text:string) = environment.webhtml.submit(PHPdata id,text)
    /// <summary>
    /// ボタンの表示
    /// </summary>
    /// <param name="id">ボタンID</param>
    /// <param name="text">ボタンに表示するテキスト</param>
    member _.show_disabled(id:PHPdata,text:string) = environment.webhtml.submit_disabled(id,text)
    
type TextBox(environment:CompilationEnvironment,name:PHPdata) =
    let t = post(environment,name)
    new(environment:CompilationEnvironment,name:string) = TextBox(environment,PHPdata name)
    /// テキストが送信されたか判定
    member _.isset with get() = environment.php.isset t.get
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
    
type TextBoxVar(environment:CompilationEnvironment) =
    /// テキストが送信されたか判定
    member _.isset(id:PHPdata) = environment.php.isset (post(environment,id)).get
    /// テキストが送信されたか判定
    member _.isset(id:string) = environment.php.isset (post(environment,id)).get
    /// 送信されたテキスト
    member _.text(id:PHPdata) = (post(environment,id)).get
    /// 送信されたテキスト
    member _.text(id:string) = (post(environment,id)).get
    /// テキストボックスの表示
    member _.show(id:PHPdata) = (post(environment,id)).input()
    /// テキストボックスの表示
    member _.show(id:string) = (post(environment,id)).input()
    /// テキストボックスの表示(送信テキストを表示)
    member _.show_copy(id:PHPdata) = (post(environment,id)).input_copy()
    /// テキストボックスの表示(送信テキストを表示)
    member _.show_copy(id:string) = (post(environment,id)).input_copy()
    /// テキストボックスの表示(スタイル指定)
    member _.show(id:PHPdata,s:string) = (post(environment,id)).input s
    /// テキストボックスの表示(スタイル指定)
    member _.show(id:string,s:string) = (post(environment,id)).input s
    /// テキストボックスの表示(スタイル指定)
    member _.show(id:PHPdata,s:PHPdata) = (post(environment,id)).input s
    /// テキストボックスの表示(スタイル指定)
    member _.show(id:string,s:PHPdata) = (post(environment,id)).input s
    /// テキストボックスの表示(スタイル指定)
    member _.show(id:PHPdata,atr:list<Atr>) = (post(environment,id)).input atr
    /// テキストボックスの表示(スタイル指定)
    member _.show(id:string,atr:list<Atr>) = (post(environment,id)).input atr
    /// テキストボックスの表示(表示テキストとスタイル指定)
    member _.show(id:PHPdata,text:string,atr:list<Atr>) = (post(environment,id)).input(text,atr)
    /// テキストボックスの表示(表示テキストとスタイル指定)
    member _.show(id:string,text:string,atr:list<Atr>) = (post(environment,id)).input(text,atr)
    /// テキストボックスの表示(表示テキストとスタイル指定)
    member _.show(id:PHPdata,text:PHPdata,atr:list<Atr>) = (post(environment,id)).input(text,atr)
    /// テキストボックスの表示(表示テキストとスタイル指定)
    member _.show(id:string,text:PHPdata,atr:list<Atr>) = (post(environment,id)).input(text,atr)
    /// テキストボックスの表示(表示テキストとスタイル指定、編集不可)
    member _.show_lock(id:PHPdata,v:PHPdata) = (post(environment,id)).input_lock v
    /// テキストボックスの表示(表示テキストとスタイル指定、編集不可)
    member _.show_lock(id:string,v:PHPdata) = (post(environment,id)).input_lock v
    /// テキストボックスの表示(表示テキストとスタイル指定、編集不可)
    member _.show_lock(id:PHPdata,v:string) = (post(environment,id)).input_lock v
    /// テキストボックスの表示(表示テキストとスタイル指定、編集不可)
    member _.show_lock(id:string,v:string) = (post(environment,id)).input_lock v
    /// テキストボックスの表示(送信テキストを表示)
    member _.show_copy(id:PHPdata,atr:list<Atr>) = (post(environment,id)).input_copy atr
    /// テキストボックスの表示(送信テキストを表示)
    member _.show_copy(id:string,atr:list<Atr>) = (post(environment,id)).input_copy atr
    /// テキストボックスの表示(送信テキストを表示、編集不可)
    member _.show_copy_lock(id:PHPdata) = (post(environment,id)).input_copy_lock()
    /// テキストボックスの表示(送信テキストを表示、編集不可)
    member _.show_copy_lock(id:string) = (post(environment,id)).input_copy_lock()
    /// テキストボックスの表示(パスワード入力用)
    member _.show_password(id:PHPdata) = (post(environment,id)).password()
    /// テキストボックスの表示(パスワード入力用)
    member _.show_password(id:string) = (post(environment,id)).password()
    /// テキストボックスの表示(パスワード入力用、送信テキストを表示)
    member _.show_password_copy(id:PHPdata) = (post(environment,id)).password_copy()
    /// テキストボックスの表示(パスワード入力用、送信テキストを表示)
    member _.show_password_copy(id:string) = (post(environment,id)).password_copy()
    /// テキストボックスの表示(パスワード入力用、送信テキストを表示、編集不可)
    member _.show_password_copy_lock(id:PHPdata) = (post(environment,id)).password_copy_lock()
    /// テキストボックスの表示(パスワード入力用、送信テキストを表示、編集不可)
    member _.show_password_copy_lock(id:string) = (post(environment,id)).password_copy_lock()
    
type TextArea(environment:CompilationEnvironment,name:PHPdata) =
    let a = post(environment,name)
    /// 送信されたテキスト
    new(environment:CompilationEnvironment,name:string) = TextArea(environment,PHPdata name)
    member _.text with get() = a.get
    member _.text_html with get() = a.get_html
    member _.isset with get() = environment.php.isset a.get
    member _.show() = a.textArea()
    member _.show_contents_ (code:unit->unit) = a.textArea code
    member _.show_contents (atr:list<Atr>) = fun (code:unit->unit) -> a.textArea_contents atr code
    member _.show(atr:list<Atr>) = a.textArea atr
    member _.show_copy() = a.textArea()
    member _.show_copy(atr:list<Atr>) = a.textArea_copy atr
    
type ComboBoxItem = {Tag:string; Text:string}

type ComboBox(environment:CompilationEnvironment,name:PHPdata,items:list<ComboBoxItem>) =
    let c = post(environment,name)
    new(environment:CompilationEnvironment,name:string,items) = ComboBox(environment,PHPdata name,items)
    /// 選択されたテキスト
    member _.selectedTag with get() = c.get
    /// コンボボックスを表示（指定された選択項目を選択状態にする）
    member this.show_selectedItem(selectedIndex:int) =    
        c.select <| fun () ->
            for i in items do
                //指定された選択肢を選択中とする
                if items[selectedIndex].Text = i.Text then
                    environment.html.option_selected i.Tag <| fun () -> writein (environment.RequireGenerationContext()) i.Text
                else
                    environment.html.option i.Tag <| fun () -> writein (environment.RequireGenerationContext()) i.Text
    /// コンボボックスを表示（送信された選択項目を選択状態にする）
    member this.show_selectedItem() =
        //c.select <| fun () ->
        environment.webhtml.select name <| fun () ->
            for i in items do
                environment.br.if2(this.selectedTag .= PHPdata i.Tag)
                <| fun () ->
                    environment.html.option_selected i.Tag <| fun () -> writein (environment.RequireGenerationContext()) i.Text
                <| fun () ->
                    environment.html.option i.Tag <| fun () -> writein (environment.RequireGenerationContext()) i.Text
    /// コンボボックスを表示（送信された選択項目を選択状態にする）
    member this.show_selectedItem(text:PHPdata) =
        //c.select <| fun () ->
        environment.webhtml.select name <| fun () ->
            for i in items do
                environment.br.if2(text .= PHPdata i.Text)
                <| fun () ->
                    environment.html.option_selected i.Tag <| fun () -> writein (environment.RequireGenerationContext()) i.Text
                <| fun () ->
                    environment.html.option i.Tag <| fun () -> writein (environment.RequireGenerationContext()) i.Text
    /// コンボボックスを表示
    member _.show() =
        //c.select <| fun () ->
        environment.webhtml.select name <| fun () ->
            for i in items do
                environment.html.option i.Tag <| fun () -> writein (environment.RequireGenerationContext()) i.Text
    member _.foreach code =
        for i in items do code i
        
type ComboBoxVar(environment:CompilationEnvironment) =
    /// 選択されたテキスト
    member _.selectedTag(id:PHPdata) = (post(environment,id)).get
    member _.selectedTag(id:int0) = (post(environment,id)).get
    member _.selectedTag(id:string) = (post(environment,id)).get
    /// コンボボックスを表示（指定された選択項目を選択状態にする）
    member this.show_selectedItem(id:PHPdata,items:list<ComboBoxItem>,selectedIndex:int) =    
        let c = post(environment,id)
        c.select <| fun () ->
            for i in items do
                //指定された選択肢を選択中とする
                if items[selectedIndex].Text = i.Text then
                    environment.html.option_selected i.Tag <| fun () -> writein (environment.RequireGenerationContext()) i.Text
                else
                    environment.html.option i.Tag <| fun () -> writein (environment.RequireGenerationContext()) i.Text
    /// コンボボックスを表示（送信された選択項目を選択状態にする）
    member this.show_selected(id:PHPdata,items:list<ComboBoxItem>) =
        let c = post(environment,id)
        //c.select <| fun () ->
        environment.webhtml.select id <| fun () ->
            for i in items do
                environment.br.if2(this.selectedTag id .= PHPdata i.Tag)
                <| fun () ->
                    environment.html.option_selected i.Tag <| fun () -> writein (environment.RequireGenerationContext()) i.Text
                <| fun () ->
                    environment.html.option i.Tag <| fun () -> writein (environment.RequireGenerationContext()) i.Text
    /// コンボボックスを表示（送信された選択項目を選択状態にする）
    member this.show_selectedTag(id:PHPdata,items:list<ComboBoxItem>,tag:PHPdata) =
        //c.select <| fun () ->
        environment.webhtml.select id <| fun () ->
            for i in items do
                environment.br.if2(tag .= i.Tag)
                <| fun () ->
                    environment.html.option_selected i.Tag <| fun () -> writein (environment.RequireGenerationContext()) i.Text
                <| fun () ->
                    environment.html.option i.Tag <| fun () -> writein (environment.RequireGenerationContext()) i.Text
    /// コンボボックスを表示（送信された選択項目を選択状態にする）
    member this.show_selectedTag_disabled(id:PHPdata,items:list<ComboBoxItem>,tag:PHPdata) =
        //c.select <| fun () ->
        environment.webhtml.select_disabled id <| fun () ->
            for i in items do
                environment.br.if2(tag .= i.Tag)
                <| fun () ->
                    environment.html.option_selected i.Tag <| fun () -> writein (environment.RequireGenerationContext()) i.Text
                <| fun () ->
                    environment.html.option i.Tag <| fun () -> writein (environment.RequireGenerationContext()) i.Text
    /// コンボボックスを表示（送信された選択項目を選択状態にする）
    member this.show_selectedItem(id:PHPdata,items:list<ComboBoxItem>,text:PHPdata) =
        //c.select <| fun () ->
        environment.webhtml.select id <| fun () ->
            for i in items do
                environment.br.if2(text .= i.Text)
                <| fun () ->
                    environment.html.option_selected i.Tag <| fun () -> writein (environment.RequireGenerationContext()) i.Text
                <| fun () ->
                    environment.html.option i.Tag <| fun () -> writein (environment.RequireGenerationContext()) i.Text
    /// コンボボックスを表示（送信された選択項目を選択状態にする）
    member this.show_selectedItem_disabled(id:PHPdata,items:list<ComboBoxItem>,text:PHPdata) =
        //c.select <| fun () ->
        environment.webhtml.select_disabled id <| fun () ->
            for i in items do
                environment.br.if2(text .= i.Text)
                <| fun () ->
                    environment.html.option_selected i.Tag <| fun () -> writein (environment.RequireGenerationContext()) i.Text
                <| fun () ->
                    environment.html.option i.Tag <| fun () -> writein (environment.RequireGenerationContext()) i.Text

    /// コンボボックスを表示
    member _.show(id:PHPdata,items:list<ComboBoxItem>) =
        //c.select <| fun () ->
        environment.webhtml.select id <| fun () ->
            for i in items do
                environment.html.option i.Tag <| fun () -> writein (environment.RequireGenerationContext()) i.Text
    member _.foreach (items:list<ComboBoxItem>) code =
        for i in items do code i
        
type CheckBox(environment:CompilationEnvironment,name:PHPdata) =
    let cb = post(environment,name)
    new(environment:CompilationEnvironment,name:string) = CheckBox(environment,PHPdata name)
    member _.isChecked with get() = cb.get .= 1
    member _.status with get() = cb.get
    member _.show() = environment.webhtml.checkbox name
    /// チェックボックス（チェックされたとき1、チェックされていないとき0を送信）
    member _.show_disabled() = environment.webhtml.checkbox_disabled name
    /// チェックボックス（チェックされたとき1、チェックされていないとき0を送信）
    member _.show_checked() = environment.webhtml.checkbox_checked name
    /// チェックボックス（チェックされたとき1、チェックされていないとき0を送信）
    member _.show_checked_disabled() = environment.webhtml.checkbox_checked_disabled name
    
/// IDによって複数のチェックボックスを表す
type CheckBoxVar(environment:CompilationEnvironment) =
    member _.isChecked(id:PHPdata) = (post(environment,id)).get .= 1
    member _.status(id:PHPdata) = (post(environment,id)).get
    member _.show(id:PHPdata) = environment.webhtml.checkbox id
    /// チェックボックス（チェックされたとき1、チェックされていないとき0を送信）
    member _.show_disabled(id:PHPdata) = environment.webhtml.checkbox_disabled id
    /// チェックボックス（チェックされたとき1、チェックされていないとき0を送信）
    member _.show_checked(id:PHPdata) = environment.webhtml.checkbox_checked id
    /// チェックボックス（チェックされたとき1、チェックされていないとき0を送信）
    member _.show_checked_disabled(id:PHPdata) = environment.webhtml.checkbox_checked_disabled id

[<AutoOpen>]
module CompilationEnvironmentFormExtensions =
    type ContextForm internal (environment:CompilationEnvironment) =
        member _.textBox(name:string) = TextBox(environment,name)
        member _.button(name:string) = Button(environment,name)

    type CompilationEnvironment with
        member this.form = ContextForm(this)
