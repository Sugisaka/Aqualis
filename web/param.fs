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
    /// Gets the validated name represented by this value.
    member internal this.Value =
        let (FieldName value) = this
        value

[<RequireQualifiedAccess>]
/// Validation and access helpers for HTML field names.
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
            validFirst character || ('0' <= character && character <= '9') || character = '-' || character = ':'

        if not (validFirst value[0]) || value |> Seq.skip 1 |> Seq.exists (validRest >> not) then
            invalidArg (nameof value) "A field name must start with a letter or underscore and contain only letters, digits, '_', '-', or ':'."

        FieldName value

    /// Gets the validated text from this name wrapper.
    let internal value (fieldName:FieldName) = fieldName.Value

/// A validated PHP variable name without the leading dollar sign.
[<Struct>]
type PhpVariableName =
    private
    | PhpVariableName of string
    /// Gets the validated name represented by this value.
    member internal this.Value =
        let (PhpVariableName value) = this
        value

[<RequireQualifiedAccess>]
/// Validation and access helpers for PHP variable names.
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

    /// Gets the validated text from this name wrapper.
    let internal value (name:PhpVariableName) = name.Value

/// A validated single CSS class name.
[<Struct>]
type CssClass =
    private
    | CssClass of string
    /// Gets the validated name represented by this value.
    member internal this.Value =
        let (CssClass value) = this
        value

[<RequireQualifiedAccess>]
/// Validation and access helpers for CSS class names.
module CssClass =
    /// Creates one CSS class token without whitespace or markup delimiters.
    let create (value:string) =
        if isNull value then nullArg (nameof value)
        if String.IsNullOrWhiteSpace value then
            invalidArg (nameof value) "A CSS class name is required."
        if value |> Seq.exists (fun character -> Char.IsWhiteSpace character || character = '"' || character = '\'' || character = '<' || character = '>') then
            invalidArg (nameof value) "A CSS class name cannot contain whitespace or markup delimiters."
        CssClass value

    /// Gets the validated text from this name wrapper.
    let internal value (cssClass:CssClass) = cssClass.Value

/// HTTP status codes supported by the redirect API.
[<RequireQualifiedAccess>]
type RedirectStatus =
    | MovedPermanently
    | Found
    | SeeOther
    | TemporaryRedirect
    | PermanentRedirect

[<RequireQualifiedAccess>]
/// Maps redirect choices to HTTP status codes.
module internal RedirectStatus =
    /// Maps a redirect status to its HTTP code.
    let code = function
        | RedirectStatus.MovedPermanently -> 301
        | RedirectStatus.Found -> 302
        | RedirectStatus.SeeOther -> 303
        | RedirectStatus.TemporaryRedirect -> 307
        | RedirectStatus.PermanentRedirect -> 308

/// Horizontal alignment and border layout for table cells.
type BorderH = |TdL |TdC |TdR |TdJ |TdLL |TdCL |TdRL |TdJL |TdLR |TdCR |TdRR |TdJR |TdLLR |TdCLR |TdRLR |TdJLR

/// Vertical alignment and border layout for table rows.
type BorderV = |TrTB |TrT |TrB |TrN

/// Flags passed to generated PHP file-reading operations.
type FileFlag =
    /// Searches the configured include path.
    |FILE_USE_INCLUDE_PATH
    /// Omits trailing newlines from returned lines.
    |FILE_IGNORE_NEW_LINES
    /// Skips empty lines.
    |FILE_SKIP_EMPTY_LINES
    /// Disables the default stream context.
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
    /// Gets the PHP file-open mode string for this flag.
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
    /// Gets whether this file mode permits writing.
    member internal this.CanWrite =
        match this with
        | Rd -> false
        | _ -> true
