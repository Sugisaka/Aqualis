// 
// Copyright (c) 2026 Jun-ichiro Sugisaka
// 
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
// 
namespace Aqualis

open System

/// A validated field name for HTML forms and request data.
[<Struct>]
type FieldName =
    private
    | FieldName of string
    member internal this.Value =
        let (FieldName value) = this
        value

[<RequireQualifiedAccess>]
module FieldName =
    /// Creates a field name containing only portable HTML name characters.
    let create (value:string) =
        if isNull value then nullArg (nameof value)
        if String.IsNullOrWhiteSpace value then
            invalidArg (nameof value) "A field name is required."

        let asciiLetter character =
            ('A' <= character && character <= 'Z') || ('a' <= character && character <= 'z')
        let validFirst character = asciiLetter character || character = '_'
        let validRest character =
            validFirst character || ('0' <= character && character <= '9') || character = '-' || character = '.' || character = ':'

        if not (validFirst value[0]) || value |> Seq.skip 1 |> Seq.exists (validRest >> not) then
            invalidArg (nameof value) "A field name must start with a letter or underscore and contain only letters, digits, '_', '-', '.', or ':'."

        FieldName value

    let internal value (fieldName:FieldName) = fieldName.Value

/// A validated PHP variable name without the leading dollar sign.
[<Struct>]
type PhpVariableName =
    private
    | PhpVariableName of string
    member internal this.Value =
        let (PhpVariableName value) = this
        value

[<RequireQualifiedAccess>]
module PhpVariableName =
    /// Creates a portable ASCII PHP variable name.
    let create (value:string) =
        if isNull value then nullArg (nameof value)
        if String.IsNullOrWhiteSpace value then
            invalidArg (nameof value) "A PHP variable name is required."

        let asciiLetter character =
            ('A' <= character && character <= 'Z') || ('a' <= character && character <= 'z')
        let validFirst character = asciiLetter character || character = '_'
        let validRest character = validFirst character || ('0' <= character && character <= '9')

        if not (validFirst value[0]) || value |> Seq.skip 1 |> Seq.exists (validRest >> not) then
            invalidArg (nameof value) "A PHP variable name must start with a letter or underscore and contain only ASCII letters, digits, or underscores."

        PhpVariableName value

    let internal value (name:PhpVariableName) = name.Value

/// A validated single CSS class name.
[<Struct>]
type CssClass =
    private
    | CssClass of string
    member internal this.Value =
        let (CssClass value) = this
        value

[<RequireQualifiedAccess>]
module CssClass =
    /// Creates one CSS class token without whitespace or markup delimiters.
    let create (value:string) =
        if isNull value then nullArg (nameof value)
        if String.IsNullOrWhiteSpace value then
            invalidArg (nameof value) "A CSS class name is required."
        if value |> Seq.exists (fun character -> Char.IsWhiteSpace character || character = '"' || character = '\'' || character = '<' || character = '>') then
            invalidArg (nameof value) "A CSS class name cannot contain whitespace or markup delimiters."
        CssClass value

    let internal value (cssClass:CssClass) = cssClass.Value

/// A validated relative or HTTP(S) URL.
[<Struct>]
type Url =
    private
    | Url of string
    member internal this.Value =
        let (Url value) = this
        value

[<RequireQualifiedAccess>]
module Url =
    let private validateCommon (value:string) =
        if isNull value then nullArg (nameof value)
        if String.IsNullOrWhiteSpace value then
            invalidArg (nameof value) "A URL is required."
        if value.Contains('\r') || value.Contains('\n') then
            invalidArg (nameof value) "A URL cannot contain CR or LF characters."

    /// Creates a relative URL. Scheme-relative and absolute URLs are rejected.
    let relative (value:string) =
        validateCommon value
        if value.Contains('\\') || value.StartsWith("//", StringComparison.Ordinal) || Uri.IsWellFormedUriString(value, UriKind.Absolute) then
            invalidArg (nameof value) "An application-relative URL is required."
        Url value

    /// Creates an absolute HTTPS URL.
    let https (value:string) =
        validateCommon value
        match Uri.TryCreate(value, UriKind.Absolute) with
        | true, uri when uri.Scheme = Uri.UriSchemeHttps && not (String.IsNullOrWhiteSpace uri.Host) -> Url uri.AbsoluteUri
        | _ -> invalidArg (nameof value) "An absolute HTTPS URL is required."

    let internal value (url:Url) = url.Value

/// HTTP status codes supported by the redirect API.
[<RequireQualifiedAccess>]
type RedirectStatus =
    | MovedPermanently
    | Found
    | SeeOther
    | TemporaryRedirect
    | PermanentRedirect

[<RequireQualifiedAccess>]
module internal RedirectStatus =
    let code = function
        | RedirectStatus.MovedPermanently -> 301
        | RedirectStatus.Found -> 302
        | RedirectStatus.SeeOther -> 303
        | RedirectStatus.TemporaryRedirect -> 307
        | RedirectStatus.PermanentRedirect -> 308

/// HTML markup that has been explicitly designated as trusted by the caller.
[<Struct>]
type TrustedHtml =
    private
    | TrustedHtml of string
    member internal this.Value =
        let (TrustedHtml value) = this
        value

/// Explicit escape hatch for content that cannot be represented by the typed HTML API.
[<RequireQualifiedAccess>]
module Unsafe =
    /// Marks a string as trusted HTML. The caller is responsible for ensuring it contains no untrusted data.
    let trustedHtml (value:string) =
        if isNull value then nullArg (nameof value)
        TrustedHtml value

/// 水平方向アライメント
type BorderH = |TdL |TdC |TdR |TdJ |TdLL |TdCL |TdRL |TdJL |TdLR |TdCR |TdRR |TdJR |TdLLR |TdCLR |TdRLR |TdJLR

/// 垂直方向アライメント
type BorderV = |TrTB |TrT |TrB |TrN

/// ファイル読み込みのオプション
type FileFlag =
    ///include_path のファイルを検索
    |FILE_USE_INCLUDE_PATH
    ///配列の各要素の最後の改行を省略
    |FILE_IGNORE_NEW_LINES
    ///空行を読み飛ばす
    |FILE_SKIP_EMPTY_LINES
    ///デフォルトのストリームコンテキストを使用しない
    |FILE_NO_DEFAULT_CONTEXT
    ///オプションのコード生成
    member this.str with get() =
        match this with
        |FILE_USE_INCLUDE_PATH -> "FILE_USE_INCLUDE_PATH"
        |FILE_IGNORE_NEW_LINES -> "FILE_IGNORE_NEW_LINES"
        |FILE_SKIP_EMPTY_LINES -> "FILE_SKIP_EMPTY_LINES"
        |FILE_NO_DEFAULT_CONTEXT -> "FILE_NO_DEFAULT_CONTEXT"
        
///ファイルの入出力モード
type FileOpenMode =
    ///書き込み専用、ファイルがない場合：エラー、ファイルポインタ：先頭
    |Wr
    ///読み・書き込み専用、ファイルがない場合：新規作成、ファイルポインタ：先頭
    |Wrp
    ///読み込み専用、ファイルがない場合：エラー、ファイルポインタ：先頭
    |Rd
    ///読み・書き込み専用、ファイルがない場合：エラー、ファイルポインタ：先頭
    |Rdp
    ///書き込み専用、ファイルがない場合：エラー、ファイルポインタ：先頭
    |Xs
    ///読み・書き込み専用、ファイルがない場合：エラー、ファイルポインタ：先頭
    |Xsp
    ///追記専用、ファイルがない場合：新規作成、ファイルポインタ：末尾
    |Ad
    ///読み・書き込み専用、ファイルがない場合：新規作成、ファイルポインタ：末尾
    |Adp
    ///書き込み専用、ファイルがない場合：新規作成、ファイルポインタ：先頭
    |Nw
    ///読み・書き込み専用、ファイルがない場合：新規作成、ファイルポインタ：先頭
    |Nwp
    member this.str with get() =
        match this with
        |Wr  -> "\"w\""
        |Wrp -> "\"w+\""
        |Rd  -> "\"r\""
        |Rdp -> "\"r+\""
        |Xs  -> "\"x\""
        |Xsp -> "\"x+\""
        |Ad  -> "\"a\""
        |Adp -> "\"a+\""
        |Nw  -> "\"c\""
        |Nwp -> "\"c+\""
