//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

open System
open System.IO
open System.Text.Json

/// Adds web-specific HTML elements and attributes.
[<AutoOpen>]
module HtmlWebExtensions =
    /// Builds an HTML attribute from a PHP expression.
    let private phpAttributeCode (name:string) (value:PHPdata) =
        let validName = HtmlEncoding.attributeName name
        validName + "=\"<?php echo htmlspecialchars((string)(" + value.code + "), ENT_QUOTES | ENT_SUBSTITUTE, 'UTF-8'); ?>\""

    type html with
        /// <summary>
        /// 内部要素のないタグ
        /// </summary>
        member this.taga (t:string,lst:list<string*PHPdata>) =
            let tag = HtmlEncoding.elementName t
            this.Context.writei("<"+tag+" ")
            this.Context.indentInc()
            for a,s in lst do
                this.Context.write (phpAttributeCode a s + " ")
            this.Context.indentDec()
            this.Context.writen  " />"
        /// <summary>
        /// 内部要素のあるタグ
        /// </summary>
        member this.tagb0 (t:string,lst:list<string*PHPdata>) = fun code ->
            let tag = HtmlEncoding.elementName t
            if lst.Length=0 then
                this.Context.write ("<"+tag+">")
            else
                this.Context.write ("<"+tag+" ")
                this.Context.indentInc()
                for a,s in lst do
                    this.Context.write (phpAttributeCode a s + " ")
                this.Context.indentDec()
                this.Context.write ">"
            code()
            this.Context.writen ("</"+tag+">")
        /// <summary>
        /// 内部要素のあるタグ
        /// </summary>
        member this.tagb (t:string,lst:list<string*PHPdata>) = fun code ->
            let tag = HtmlEncoding.elementName t
            if lst.Length=0 then
                this.Context.writein ("<"+tag+">")
            else
                this.Context.writei ("<"+tag+" ")
                this.Context.indentInc()
                for a,s in lst do
                    this.Context.writei (phpAttributeCode a s + " ")
                this.Context.indentDec()
                this.Context.writen ">"
            code()
            this.Context.writein ("</"+tag+">")
        /// <summary>
        /// 見出し（h1）要素を生成する
        /// </summary>
        /// <param name="t">見出しに表示する内容</param>
        member this.h1 (t:int0) = fun code ->
            this.tagb "h1" <| fun () -> this.Context.php.echoHtmlText (PHPdata t)
            code()
        /// <summary>
        /// 見出し（h1）要素を生成する
        /// </summary>
        /// <param name="t">見出しに表示する内容</param>
        /// <param name="atr">文字の太さ、色を定義するスタイル情報</param>
        member this.h1 (t:int0,s:Style) = fun code ->
            this.tagb ("h1",[s.atr]) <| fun () -> this.Context.php.echoHtmlText (PHPdata t)
            code()

        /// Writes a level-2 heading from a numeric expression.
        member this.h2 (t:int0) = fun code ->
            this.tagb "h2" <| fun () -> this.Context.php.echoHtmlText (PHPdata t)
            code()
        /// Writes a level-2 heading from a styled numeric expression.
        member this.h2 (t:int0,s:Style) = fun code ->
            this.tagb ("h2",[s.atr]) <| fun () -> this.Context.php.echoHtmlText (PHPdata t)
            code()

        /// Writes a level-3 heading from a numeric expression.
        member this.h3 (t:int0) = fun code ->
            this.tagb "h3" <| fun () -> this.Context.php.echoHtmlText (PHPdata t)
            code()
        /// Writes a level-3 heading from a styled numeric expression.
        member this.h3 (t:int0,s:Style) = fun code ->
            this.tagb ("h3",[s.atr]) <| fun () -> this.Context.php.echoHtmlText (PHPdata t)
            code()

        /// Writes a level-4 heading from a numeric expression.
        member this.h4 (t:int0) = fun code ->
            this.tagb "h4" <| fun () -> this.Context.php.echoHtmlText (PHPdata t)
            code()
        /// Writes a level-4 heading from a styled numeric expression.
        member this.h4 (t:int0,s:Style) = fun code ->
            this.tagb ("h4",[s.atr]) <| fun () -> this.Context.php.echoHtmlText (PHPdata t)
            code()

        /// Writes a level-5 heading from a numeric expression.
        member this.h5 (t:int0) = fun code ->
            this.tagb "h5" <| fun () -> this.Context.php.echoHtmlText (PHPdata t)
            code()
        /// Writes a level-5 heading from a styled numeric expression.
        member this.h5 (t:int0,s:Style) = fun code ->
            this.tagb ("h5",[s.atr]) <| fun () -> this.Context.php.echoHtmlText (PHPdata t)
            code()
        /// <summary>
        /// 見出し（h1）要素を生成する
        /// </summary>
        /// <param name="t">見出しに表示する内容</param>
        member this.h1 (t:double0) = fun code ->
            this.tagb "h1" <| fun () -> this.Context.php.echoHtmlText (PHPdata t)
            code()
        /// <summary>
        /// 見出し（h1）要素を生成する
        /// </summary>
        /// <param name="t">見出しに表示する内容</param>
        /// <param name="atr">文字の太さ、色を定義するスタイル情報</param>
        member this.h1 (t:double0,s:Style) = fun code ->
            this.tagb ("h1",[s.atr]) <| fun () -> this.Context.php.echoHtmlText (PHPdata t)
            code()

        /// Writes a level-2 heading from a numeric expression.
        member this.h2 (t:double0) = fun code ->
            this.tagb "h2" <| fun () -> this.Context.php.echoHtmlText (PHPdata t)
            code()
        /// Writes a level-2 heading from a styled numeric expression.
        member this.h2 (t:double0,s:Style) = fun code ->
            this.tagb ("h2",[s.atr]) <| fun () -> this.Context.php.echoHtmlText (PHPdata t)
            code()

        /// Writes a level-3 heading from a numeric expression.
        member this.h3 (t:double0) = fun code ->
            this.tagb "h3" <| fun () -> this.Context.php.echoHtmlText (PHPdata t)
            code()
        /// Writes a level-3 heading from a styled numeric expression.
        member this.h3 (t:double0,s:Style) = fun code ->
            this.tagb ("h3",[s.atr]) <| fun () -> this.Context.php.echoHtmlText (PHPdata t)
            code()

        /// Writes a level-4 heading from a numeric expression.
        member this.h4 (t:double0) = fun code ->
            this.tagb "h4" <| fun () -> this.Context.php.echoHtmlText (PHPdata t)
            code()
        /// Writes a level-4 heading from a styled numeric expression.
        member this.h4 (t:double0,s:Style) = fun code ->
            this.tagb ("h4",[s.atr]) <| fun () -> this.Context.php.echoHtmlText (PHPdata t)
            code()

        /// Writes a level-5 heading from a numeric expression.
        member this.h5 (t:double0) = fun code ->
            this.tagb "h5" <| fun () -> this.Context.php.echoHtmlText (PHPdata t)
            code()
        /// Writes a level-5 heading from a styled numeric expression.
        member this.h5 (t:double0,s:Style) = fun code ->
            this.tagb ("h5",[s.atr]) <| fun () -> this.Context.php.echoHtmlText (PHPdata t)
            code()
        /// <summary>
        /// フォーム送信用のsubmitボタンを生成する
        /// <para>
        /// nameとvalueの型違いに対応したオーバーロードを提供する
        /// </para>
        /// </summary>
        member this.submit(name:string,value:PHPdata) =
            this.taga("input",["type",PHPdata "submit"; "name",PHPdata name; "value",value])
        /// Writes a submit input with the supplied name and label.
        member this.submit(name:PHPdata,value:string) = this.taga("input",["type",PHPdata "submit"; "name", name; "value",PHPdata value])
        /// <summary>
        /// フォーム送信用のsubmitボタンを生成する
        /// </summary>
        /// <param name="name">name属性に設定する文字列</param>
        /// <param name="value">value属性に設定する文字列</param>
        member this.submit(name:string,value:string) =
            this.taga("input",[Atr("type", "submit"); Atr("name", name); Atr("value", value)])
        /// <summary>
        /// 送信先URLを指定したsubmitボタンを生成する
        /// </summary>
        /// <param name="url">formaction属性に設定するURL</param>
        /// <param name="name">name属性に設定するPHPデータ</param>
        /// <param name="value">value属性に設定する文字列</param>
        member this.submit(url:Url,name:PHPdata,value:string) = this.taga("input",["type",PHPdata "submit"; "name", name; "value",PHPdata value; "formaction",PHPdata (Url.value url)])
        /// <summary>
        /// 無効化されたsubmitボタンを生成する
        /// </summary>
        /// <param name="name">name属性に設定するPHPデータ</param>
        /// <param name="value">value属性に設定するPHPデータ</param>
        member this.submit_disabled(name:PHPdata,value:PHPdata) = this.taga("input",["type",PHPdata "submit"; "name", name; "value",value; "disabled",PHPdata "disabled"])
        /// Writes a disabled submit input with the supplied name and label.
        member this.submit_disabled(name:string,value:PHPdata) = this.taga("input",["type",PHPdata "submit"; "name",PHPdata name; "value",value; "disabled",PHPdata "disabled"])
        /// Writes a disabled submit input with the supplied name and label.
        member this.submit_disabled(name:PHPdata,value:string) = this.taga("input",["type",PHPdata "submit"; "name", name; "value",PHPdata value; "disabled",PHPdata "disabled"])
        /// <summary>
        /// li要素を生成する
        /// </summary>
        /// <param name="a">li要素に設定する属性のリスト</param>
        member this.item (a:list<string*PHPdata>) = fun code -> this.tagb ("li",a) code
        /// <summary>
        /// select要素を生成する
        /// </summary>
        /// <param name="x">name属性に設定するPHPデータ</param>
        member this.select(x:PHPdata) = fun code -> this.tagb ("select",["name",x;]) code
        /// <summary>
        /// 無効化されたselsect要素を生成する
        /// </summary>
        member this.select_disabled(x:PHPdata) = fun code -> this.tagb ("select",["name",x; "disabled",PHPdata "disabled"]) code
        /// <summary>
        /// 任意のHTMLタグの開始タグと終了タグを生成する
        /// </summary>
        /// <param name="t">タグ名</param>
        /// <param name="code">タグ内部の内容を生成する関数</param>
        member this.splitTag t code =
            let tag = HtmlEncoding.elementName t
            let b (lst:list<string*PHPdata>) =
                if lst.Length=0 then
                    this.Context.writein ("<"+tag+">")
                else
                    this.Context.writein ("<"+tag+" ")
                    for a,s in lst do
                        this.Context.writein (phpAttributeCode a s + " ")
                    this.Context.writein ">"
            code b
            this.Context.writein ("</"+tag+">")
        /// <summary>
        /// select要素を生成
        /// </summary>
        member this.Select = this.splitTag "select"
        /// <summary>
        /// tr要素を生成
        /// </summary>
        member this.Tr = this.splitTag "tr"
        /// <summary>
        /// div要素を生成する
        /// </summary>
        /// <param name="a">属性リスト</param>
        member this.div (a:list<string*PHPdata>) = fun code -> this.tagb ("div",a) code
        /// <summary>
        /// CSSdataの内容に応じてHTML要素を生成する
        /// </summary>
        /// <param name="a">生成する要素を指定するCSSデータ</param>
        member this.div (a:CSSdata) = fun code ->
            match a.label with
            |HTMLTag s -> this.tagb s code
            |CSSClass s -> this.tagb ("div",[Atr("class",s)]) code
            |CSSID s -> this.tagb ("div",[Atr("id",s)]) code
            |_ -> ()
        /// <summary>
        /// CSSdataの内容に応じてHTML要素を生成する
        /// </summary>
        /// <param name="a">生成対象を指定するCSSデータ</param>
        /// <param name="atr">追加する属性のリスト</param>
        member this.div (a:CSSdata,atr:list<Atr>) = fun code ->
            match a.label with
            |HTMLTag s -> this.tagb s code
            |CSSClass s -> this.tagb ("div",[Atr("class",s)]@atr) code
            |CSSID s -> this.tagb ("div",[Atr("id",s)]@atr) code
            |_ -> ()
        /// <summary>
        /// CSSdataに基づいてarticle要素を生成する
        /// </summary>
        /// <param name="a">要素に適用するCSSデータ</param>
        member this.article (a:CSSdata) = fun code ->
            match a.label with
            |CSSClass s -> this.tagb ("article",[Atr("class",s)]) code
            |CSSID s -> this.tagb ("article",[Atr("id",s)]) code
            |_ -> ()
        /// <summary>
        /// CSSdataに基づいてaside要素を生成する
        /// </summary>
        member this.aside (a:CSSdata) = fun code ->
            match a.label with
            |CSSClass s -> this.tagb ("aside",[Atr("class",s)]) code
            |CSSID s -> this.tagb ("aside",[Atr("id",s)]) code
            |_ -> ()
        /// <summary>
        /// CSSdataに基づいてpara要素を生成する
        /// </summary>
        member this.para (a:CSSdata) = fun code ->
            match a.label with
            |CSSClass s -> this.tagb ("p",[Atr("class",s)]) code
            |CSSID s -> this.tagb ("p",[Atr("id",s)]) code
            |_ -> ()
        /// <summary>
        /// CSSdataに基づいてsection要素を生成する
        /// </summary>
        member this.section (a:CSSdata) = fun code ->
            match a.label with
            |CSSClass s -> this.tagb ("section",[Atr("class",s)]) code
            |CSSID s -> this.tagb ("section",[Atr("id",s)]) code
            |_ -> ()
        /// <summary>
        /// CSSdataに基づいてspan要素を生成する
        /// </summary>
        member this.span (a:CSSdata) = fun code ->
            match a.label with
            // |CSSClass s -> this.tagb0 ("span",["class",s]) code
            // |CSSID s -> this.tagb0 ("span",["id"
            |CSSClass s -> this.tagb ("span",[Atr("class",s)]) code
            |CSSID s -> this.tagb ("span",[Atr("id",s)]) code
            |_ -> ()

        /// <summary>
        /// チェックボックス（チェックされたとき1、チェックされていないとき0を送信）
        /// </summary>
        member this.checkbox(name:PHPdata) =
            this.taga("input",["type",PHPdata "hidden"; "name", name; "value",PHPdata "0";])
            this.taga("input",["type",PHPdata "checkbox"; "name", name; "value",PHPdata "1";])
        /// <summary>
        /// チェックボックス（チェックされたとき1、チェックされていないとき0を送信）
        /// </summary>
        member this.checkbox_disabled(name:PHPdata) =
            this.taga("input",["type",PHPdata "hidden"; "name", name; "value",PHPdata "0";])
            this.taga("input",["type",PHPdata "checkbox"; "name", name; "value",PHPdata "1"; "disabled",PHPdata "disabled"])
        /// <summary>
        /// チェックボックス（チェックされたとき1、チェックされていないとき0を送信）
        /// </summary>
        member this.checkbox_checked(name:PHPdata) =
            this.taga("input",["type",PHPdata "hidden"; "name", name; "value",PHPdata "0";])
            this.taga("input",["type",PHPdata "checkbox"; "name", name; "value",PHPdata "1"; "checked",PHPdata "checked";])
        /// <summary>
        /// チェックボックス（チェックされたとき1、チェックされていないとき0を送信）
        /// </summary>
        member this.checkbox_checked_disabled(name:PHPdata) =
            this.taga("input",["type",PHPdata "hidden"; "name", name; "value",PHPdata "0";])
            this.taga("input",["type",PHPdata "checkbox"; "name", name; "value",PHPdata "1"; "checked",PHPdata "checked"; "disabled",PHPdata "disabled"])
        /// <summary>
        /// 指定位置に数式テキストを描画する
        /// </summary>
        /// <param name="s">適用するスタイル</param>
        /// <param name="p">表示位置</param>
        /// <param name="text">表示する数式</param>
        member this.Mathtext (s:Style) (p:position) (text:PHPdata) =
            let s1 = Style [{Key = "margin-left"; Value=InvariantFormat.number p.x+"px"}
                            {Key = "margin-top"; Value=InvariantFormat.number p.y+"px"}
                            {Key = "position"; Value = "absolute";}]
            this.tagb ("div", [(s1+s).atr]) <| fun () ->
                this.Context.writein ("\\(" + text.code + "\\)")


        /// <summary>
        /// コードブロックを生成
        /// </summary>
        member this.code (style:list<string*PHPdata>) = fun cd ->
            this.tagb0 ("pre",style) <| fun () ->
                this.tagb0 ("code",[]) <| fun () ->
                    cd()

        /// Writes a code element using the supplied content and style attributes.
        member this.code (style:list<string*PHPdata>, cd:PHPdata) =
            this.tagb0 ("pre",style) <| fun () ->
                this.tagb0 ("code",[]) <| fun () ->
                    this.Context.php.echoHtmlText cd

        /// Writes a code element using the supplied content and style attributes.
        member this.code (style:list<string*string>) = this.code (style |> List.map (fun (a,b) -> a,PHPdata b))

        /// Writes a code element using the supplied content and style attributes.
        member this.code (style:list<string*string>, cd:PHPdata) = this.code (style |> List.map (fun (a,b) -> a,PHPdata b),cd)

        /// Writes a code element using the supplied content and style attributes.
        member this.code (cd:PHPdata) = this.code (([] : (string * PHPdata) list),cd)
        /// <summary>
        /// 罫線指定付きの表を生成
        /// </summary>
        /// <param name="caption">表のタイトル</param>
        /// <param name="borderH">水平罫線の設定</param>
        /// <param name="borderV">垂直罫線の設定</param>
        /// <param name="tlist">表データ</param>
        member this.listTableCells (caption:string) = fun (borderH:list<BorderH>) (borderV:list<BorderV>) (tlist:list<list<TableCell>>) ->
            this.tagb("div",[Atr("class", "fig")]) <| fun () ->
                this.tagb ("span",[Atr("class", "caption")]) <| fun () ->
                    this.Context.html.text caption
                this.tagb("table",[Atr("class", "tab")]) <| fun () ->
                    for j in 0..tlist.Length-1 do
                        this.tagb ("tr",[Atr("class",match borderV[j] with |TrTB -> "trtb" |TrT -> "trt" |TrB -> "trb" |TrN -> "trn")]) <| fun () ->
                            for i in 0..tlist[j].Length-1 do
                                this.tagb ("td",[Atr("class",
                                    match borderH[i] with
                                    |TdL -> "tdl"
                                    |TdC -> "tdc"
                                    |TdR -> "tdr"
                                    |TdJ -> "tdj"
                                    |TdLL -> "tdlL"
                                    |TdCL -> "tdcL"
                                    |TdRL -> "tdrL"
                                    |TdJL -> "tdjL"
                                    |TdLR -> "tdlR"
                                    |TdCR -> "tdcR"
                                    |TdRR -> "tdrR"
                                    |TdJR -> "tdjR"
                                    |TdLLR -> "tdlLR"
                                    |TdCLR -> "tdcLR"
                                    |TdRLR -> "tdrLR"
                                    |TdJLR -> "tdjLR")]) <| fun () ->
                                    match tlist[j][i] with
                                    |TableCell.Text text -> this.Context.html.text text
                                    |TableCell.Php value -> this.Context.php.echoHtmlText value
        /// <summary>
        /// 罫線指定付きの静的テキスト表を生成
        /// </summary>
        member this.listTable (caption:string) = fun (borderH:list<BorderH>) (borderV:list<BorderV>) (tlist:list<list<string>>) ->
            tlist
            |> List.map (List.map TableCell.Text)
            |> this.listTableCells caption borderH borderV
        /// <summary>
        /// num0式を評価し、インラインMathJax文字列を返す
        /// </summary>
        member this.inlineMath(text:int0) =
            AnimationRendering.inlineMath this.Context (text :> INum0)
        /// <summary>
        /// num0式を評価し、インラインMathJax文字列を返す
        /// </summary>
        member this.inlineMath(text:double0) =
            AnimationRendering.inlineMath this.Context (text :> INum0)
        /// <summary>
        /// num0式を評価し、インラインMathJax文字列を返す
        /// </summary>
        member this.inlineMath(text:complex0) =
            AnimationRendering.inlineMath this.Context (text :> INum0)
        /// <summary>
        /// num0式を評価し、MathJax形式で出力する
        /// </summary>
        member this.eq(text:int0) =
            this.Context.writein ("\\("+text.Expr.evalL this.Context + "\\)")
        /// <summary>
        /// num0式を評価し、MathJax形式で出力する
        /// </summary>
        member this.eq(text:double0) =
            this.Context.writein ("\\("+text.Expr.evalL this.Context + "\\)")
        /// <summary>
        /// num0式を評価し、MathJax形式で出力する
        /// </summary>
        member this.eq(text:complex0) =
            this.Context.writein ("\\("+text.Expr.evalL this.Context + "\\)")


        /// <summary>
        /// 指定位置・サイズでテキストブロックを生成
        /// </summary>
        /// <param name="s">適用するスタイル</param>
        /// <param name="p">表示位置</param>
        /// <param name="width, height">ブロックのサイズ</param>
        /// <param name="text">表示する文字列のリスト</param>
        member this.blockText (s:Style) (p:position) (width:float,height:float) (text:list<string>) =
            let padding = 5
            let s1 = Style [size.width (InvariantFormat.number width+"px")
                            size.height (InvariantFormat.number height+"px")
                            {Key = "margin-left"; Value = InvariantFormat.number p.x+"px";}
                            {Key = "margin-top"; Value = InvariantFormat.number p.y+"px";}
                            {Key = "position"; Value = "absolute";}
                            {Key = "overflow-wrap"; Value = "break-word";}]
            this.tagb ("div", [(s1+s).atr]) <| fun () ->
                text |> List.iter (fun s ->
                    this.Context.html.text s
                    this.Context.writein "<br>")
                this.Context.writein ("\r\n")
            {Left = p.x;
            Right = p.x+double width+2.0*double padding;
            Top = p.y;
            Bottom = p.y+double height+2.0*double padding;}

