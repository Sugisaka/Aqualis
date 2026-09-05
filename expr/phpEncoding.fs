//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

open System.Text

module internal PhpEncoding =
    /// Renders a .NET string as a PHP double-quoted string literal.
    let stringLiteral (value:string) =
        if isNull value then nullArg (nameof value)

        let result = StringBuilder(value.Length + 2)
        result.Append('"') |> ignore

        for character in value do
            match character with
            | '\\' -> result.Append("\\\\") |> ignore
            | '"' -> result.Append("\\\"") |> ignore
            | '$' -> result.Append("\\$") |> ignore
            | control when int control <= 0x1F || control = '\u007F' ->
                result.Append(sprintf "\\x%02X" (int control)) |> ignore
            | character -> result.Append(character) |> ignore

        result.Append('"').ToString()

    /// Renders a PHP string literal for inclusion in another quoted code string.
    let codeStringLiteral value =
        let literal = stringLiteral value
        literal.Replace("\\", "\\\\").Replace("\"", "\\\"")
