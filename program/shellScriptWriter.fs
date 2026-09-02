//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

open System.IO
open System.Text

[<RequireQualifiedAccess>]
module internal ShellCommand =
    let quoteArgument (value:string) =
        if isNull value then
            nullArg (nameof value)
        if value.IndexOf '\u0000' >= 0 then
            invalidArg (nameof value) "A shell argument cannot contain NUL."

        let isSafe character =
            System.Char.IsLetterOrDigit character ||
            "_-./:=+@%,".Contains character

        if value <> "" && value |> Seq.forall isSafe then
            value
        else
            "'" + value.Replace("'", "'\"'\"'") + "'"

    let buildCommand executable arguments =
        executable::arguments
        |> List.filter (System.String.IsNullOrWhiteSpace >> not)
        |> List.map quoteArgument
        |> String.concat " "

    let buildCompileCommand
        compiler
        fixedArguments
        sources
        mainSource
        options
        output =
        buildCommand compiler [
            yield! fixedArguments
            yield! sources
            yield mainSource
            yield! options
            yield "-o"
            yield output
        ]

[<RequireQualifiedAccess>]
module internal ShellScriptWriter =
    let create path =
        let writer = new StreamWriter(path, false, UTF8Encoding(false))
        writer.NewLine <- "\n"
        writer

    let writeCompileAndRun
        (writer:StreamWriter)
        (compileCommand:string)
        (runCommand:string) =
        writer.WriteLine compileCommand
        writer.WriteLine "aqualis_compile_status=$?"
        writer.WriteLine "if [ \"$aqualis_compile_status\" -ne 0 ]; then"
        writer.WriteLine "  printf '%s\\n' 'Aqualis: compilation failed.' >&2"
        writer.WriteLine "  exit \"$aqualis_compile_status\""
        writer.WriteLine "fi"
        writer.WriteLine()
        writer.WriteLine("exec " + runCommand)
