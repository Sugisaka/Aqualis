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
/// Builds safely quoted POSIX shell commands from executable paths and arguments.
module internal ShellCommand =
    /// Quotes a single argument for a POSIX shell, rejecting null and NUL characters.
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

    /// Joins the executable and nonblank arguments into a quoted shell command.
    let buildCommand executable arguments =
        executable::arguments
        |> List.filter (System.String.IsNullOrWhiteSpace >> not)
        |> List.map quoteArgument
        |> String.concat " "

    /// Builds a compiler invocation with sources, options, and an output path.
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
/// Writes UTF-8 shell scripts with Unix line endings.
module internal ShellScriptWriter =
    /// Creates or overwrites a script file without a UTF-8 byte-order mark.
    let create path =
        let writer = new StreamWriter(path, false, UTF8Encoding(false))
        writer.NewLine <- "\n"
        writer

    /// Writes an <c>exec</c> command for the supplied executable and arguments.
    let writeExec
        (writer:StreamWriter)
        executable
        arguments =
        let command = ShellCommand.buildCommand executable arguments
        writer.WriteLine("exec " + command)

    /// Writes a compile command followed by a guarded execution step.
    /// On compilation failure, the generated script reports the error and exits
    /// with the compiler's status.
    let writeCompileAndRun
        (writer:StreamWriter)
        (compileCommand:string)
        runExecutable
        runArguments =
        writer.WriteLine compileCommand
        writer.WriteLine "aqualis_compile_status=$?"
        writer.WriteLine "if [ \"$aqualis_compile_status\" -ne 0 ]; then"
        writer.WriteLine "  printf '%s\\n' 'Aqualis: compilation failed.' >&2"
        writer.WriteLine "  exit \"$aqualis_compile_status\""
        writer.WriteLine "fi"
        writer.WriteLine()
        writeExec writer runExecutable runArguments
