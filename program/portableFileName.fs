//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

open System
open System.IO
open System.Text

/// Validates one portable file-name component before it is used in generated paths.
[<RequireQualifiedAccess>]
module internal PortableFileNameSegment =
    /// Upper limit on the UTF-8 byte length of a file-name component.
    [<Literal>]
    let MaximumUtf8Bytes = 200

    /// Checks whether a segment uses a reserved Windows device name.
    let private isWindowsDeviceName (value:string) =
        let stem = (value.Split('.')[0]).ToUpperInvariant()
        match stem with
        | "CON" | "PRN" | "AUX" | "NUL" -> true
        | _ when stem.Length = 4
                 && (stem.StartsWith("COM", StringComparison.Ordinal)
                     || stem.StartsWith("LPT", StringComparison.Ordinal))
                 && stem[3] >= '1'
                 && stem[3] <= '9' -> true
        | _ -> false

    /// Returns a valid single file-name component, or raises an argument error.
    /// Rejects rooted paths, separators, control and reserved characters,
    /// Windows device names, trailing periods, and values over 200 UTF-8 bytes.
    let validate argumentName subjectName (value:string) =
        let description = "A " + subjectName + " name"

        if String.IsNullOrWhiteSpace value then
            invalidArg argumentName (description + " is required.")
        if not (String.Equals(value, value.Trim(), StringComparison.Ordinal)) then
            invalidArg argumentName (description + " cannot start or end with whitespace.")
        if value = "." || value = ".." || Path.IsPathRooted value then
            invalidArg argumentName (description + " must be one relative file-name segment.")
        if value.IndexOfAny([| '/'; '\\' |]) >= 0 then
            invalidArg argumentName (description + " cannot contain directory separators.")
        if value |> Seq.exists Char.IsControl then
            invalidArg argumentName (description + " cannot contain control characters.")
        if value.IndexOfAny([| '<'; '>'; ':'; '"'; '|'; '?'; '*' |]) >= 0
           || value.IndexOfAny(Path.GetInvalidFileNameChars()) >= 0 then
            invalidArg argumentName (description + " contains characters that are not portable in file names.")
        if value.EndsWith(".", StringComparison.Ordinal) then
            invalidArg argumentName (description + " cannot end with a period.")
        if isWindowsDeviceName value then
            invalidArg argumentName (description + " cannot use a reserved device name.")
        if Encoding.UTF8.GetByteCount(value) > MaximumUtf8Bytes then
            invalidArg argumentName (description + " cannot exceed 200 bytes in UTF-8.")

        value
