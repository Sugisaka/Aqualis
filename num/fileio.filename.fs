//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    open System
    open System.Text

    /// Encodes file names and string literals for target languages.
    [<RequireQualifiedAccess>]
    module internal FileNameCode =
        /// Appends an escaped character to a target-language string literal.
        let private appendCommonEscape (builder:StringBuilder) character =
            match character with
            | '\\' -> builder.Append("\\\\") |> ignore; true
            | '"' -> builder.Append("\\\"") |> ignore; true
            | '\n' -> builder.Append("\\n") |> ignore; true
            | '\r' -> builder.Append("\\r") |> ignore; true
            | '\t' -> builder.Append("\\t") |> ignore; true
            | _ -> false

        /// Escapes a string as a C literal.
        let cStringLiteral (value:string) =
            if isNull value then nullArg (nameof value)
            if value.IndexOf '\u0000' >= 0 then
                invalidArg (nameof value) "A generated file name cannot contain NUL."

            let builder = StringBuilder(value.Length + 2)
            builder.Append('"') |> ignore
            for character in value do
                if not (appendCommonEscape builder character) then
                    if Char.IsControl character then
                        builder
                            .Append('\\')
                            .Append(Convert.ToString(int character, 8).PadLeft(3, '0'))
                        |> ignore
                    else
                        builder.Append(character) |> ignore
            builder.Append('"').ToString()

        /// Escapes a string as a Fortran literal.
        let fortranStringLiteral (value:string) =
            if isNull value then nullArg (nameof value)
            if value |> Seq.exists Char.IsControl then
                invalidArg (nameof value) "A generated Fortran file name cannot contain control characters."
            "'" + value.Replace("'", "''") + "'"

        /// Escapes a string as a Python literal.
        let pythonStringLiteral (value:string) =
            if isNull value then nullArg (nameof value)
            if value.IndexOf '\u0000' >= 0 then
                invalidArg (nameof value) "A generated file name cannot contain NUL."

            let builder = StringBuilder(value.Length + 2)
            builder.Append('"') |> ignore
            for character in value do
                if not (appendCommonEscape builder character) then
                    if Char.IsControl character then
                        builder.Append("\\u").Append((int character).ToString("X4")) |> ignore
                    else
                        builder.Append(character) |> ignore
            builder.Append('"').ToString()

        /// Builds the Fortran file-name expression without opening the file.
        let fortran (ctx:Aqualis) (filename:exprString) (intDigit:int option) =
            let integerWidth =
                match intDigit with
                |None -> ctx.numFormat.iFormat
                |Some width when width > 0 -> width
                |Some _ -> invalidArg (nameof intDigit) "The file-name integer width must be positive."
            let f =
                filename.data
                |> List.map (fun s ->
                    match s with
                    |RStr _ ->
                        "A"
                    |RNvr(x,_) when x.etype = It 4 ->
                        "I0." + integerWidth.ToString()
                    |_ ->
                        "")
                |> List.filter (String.IsNullOrEmpty >> not)
                |> fun s -> String.Join(",",s)
            let s =
                filename.data
                |> List.map (fun s ->
                    match s with
                    |RStr t ->
                        fortranStringLiteral t
                    |RNvr(x,_) when x.etype = It 4 ->
                        x.eval ctx
                    |_ ->
                        "")
                |> List.filter (String.IsNullOrEmpty >> not)
                |> fun s -> String.Join(",",s)
            let requiredLength =
                filename.data
                |> List.choose (function
                    |RStr value -> Some("len(" + fortranStringLiteral value + ")")
                    |RNvr(value,_) when value.etype = It 4 -> Some((max integerWidth 11).ToString())
                    |_ -> None)
                |> function
                    |[] -> "0"
                    |terms -> String.Join(" + ", terms)
            f, s, requiredLength

        /// Builds the C99 file-name expression without opening the file.
        let c99 (ctx:Aqualis) (filename:exprString) (intDigit:int option) =
            let integerWidth =
                match intDigit with
                |None -> ctx.numFormat.iFormat
                |Some width when width > 0 -> width
                |Some _ -> invalidArg (nameof intDigit) "The file-name integer width must be positive."
            let f =
                filename.data
                |> List.map (fun s ->
                    match s with
                    |RStr t ->
                        t.Replace("%", "%%")
                    |RNvr(x,_) when x.etype = It 4 ->
                        "%0" + integerWidth.ToString() + "d"
                    |_ ->
                        "")
                |> List.filter (fun s -> s<>"")
                |> fun s -> String.Join("",s)
            let s =
                filename.data
                |> List.map (fun s ->
                    match s with
                    |RStr _ ->
                        ""
                    |RNvr(x,_) when x.etype = It 4 ->
                        x.eval ctx
                    |_ ->
                        "")
                |> List.filter (fun s -> s<>"")
                |> fun s -> String.Join(",",s)
            f, s

        /// Builds the LaTeX file-name expression without opening the file.
        let latex (ctx:Aqualis) (filename:exprString) (intDigit:int option) =
            let s =
                filename.data
                |> List.map (fun s ->
                    match s with
                    |RStr t ->
                        "\""+t+"\""
                    |RNvr(x,_) when x.etype = It 4 ->
                        x.eval ctx
                    |_ ->
                        "")
                |> List.filter (fun s -> s<>"")
                |> fun s -> String.Join("+",s)
            s

        /// Builds the HTML file-name expression without opening the file.
        let html (ctx:Aqualis) (filename:exprString) (intDigit:int option) =
            let s =
                filename.data
                |> List.map (fun s ->
                    match s with
                    |RStr t ->
                        "\""+t+"\""
                    |RNvr(x,_) when x.etype = It 4 ->
                        x.eval ctx
                    |_ ->
                        "")
                |> List.filter (fun s -> s<>"")
                |> fun s -> String.Join("+",s)
            s

        /// Builds the Python file-name expression without opening the file.
        let python (ctx:Aqualis) (filename:exprString) (intDigit:int option) =
            let integerWidth =
                match intDigit with
                |None -> ctx.numFormat.iFormat
                |Some width when width > 0 -> width
                |Some _ -> invalidArg (nameof intDigit) "The file-name integer width must be positive."
            let filenameExpression =
                filename.data
                |> List.choose (fun part ->
                    match part with
                    |RStr t ->
                        Some(pythonStringLiteral t)
                    |RNvr(x,_) when x.etype = It 4 ->
                        Some("format(" + x.eval ctx + ", " + pythonStringLiteral ("0" + integerWidth.ToString() + "d") + ")")
                    |_ ->
                        None)
                |> function
                    |[] -> pythonStringLiteral ""
                    |parts -> String.Join(" + ", parts)
            filenameExpression

