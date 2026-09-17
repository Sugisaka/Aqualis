//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    /// Generates shell scripts for compiled projects.
    module shellscript =

        open System
        open System.IO

        /// Builds execution scripts for a generated project.
        type Shell(context:Aqualis,dir:string,project:string,nproc:int) =
            let language = context.language
            let outputDirectory =
                match context.Dir, context.GeneratedOutputDirectory with
                | Some publicDirectory, Some generatedDirectory
                    when String.Equals(
                        Path.GetFullPath dir,
                        Path.GetFullPath publicDirectory,
                        if OperatingSystem.IsWindows() then StringComparison.OrdinalIgnoreCase
                        else StringComparison.Ordinal) -> generatedDirectory
                | _ -> dir
            let mutable id = 0
            let mutable disposed = false
            let processScriptName =
                match language with
                | Fortran -> Some ("proc_" + project + "_F.sh")
                | C99 -> Some ("proc_" + project + "_C.sh")
                | Python -> Some ("proc_" + project + "_P.sh")
                | _ -> None
            let w =
                if nproc <= 0 then
                    invalidArg (nameof nproc) "The process count must be positive."

                let created = ResizeArray<StreamWriter>()
                try
                    for i in 1..nproc do
                        created.Add(
                            ShellScriptWriter.create(
                                Path.Combine(outputDirectory, "shell_"+project+"_"+i.ToString("00")+".sh")))
                    created.ToArray()
                with _ ->
                    created |> Seq.iter _.Dispose()
                    reraise()

            let disposeWriters() =
                if not disposed then
                    disposed <- true
                    for writer in w do
                        writer.WriteLine()
                        writer.WriteLine("exit \"$aqualis_exit_status\"")
                        writer.Dispose()

            do
                for i in 1..nproc do
                    w.[i-1].Write("#!/bin/bash"+"\n\n")
                    w.[i-1].WriteLine("aqualis_exit_status=0")

            /// Allocates the next job identifier.
            member private this.nextid() =
                id <- id + 1
                if id = nproc then id <- 0

            /// Appends a process launch command to the shell script.
            member private this.addProcess(writeJob:StreamWriter -> string -> unit) =
                match processScriptName with
                | Some scriptName ->
                    writeJob w.[id] scriptName
                    this.nextid()
                | None ->
                    invalidOp "Distributed execution is not supported for this language."

            ///<summary>
            ///<p>ソースファイルのコンパイル・実行するスクリプトファイルを生成</p>
            ///</summary>
            member this.AddProcess() =
                this.addProcess(fun writer scriptName ->
                    ShellCommand.buildCommand "sh" [scriptName]
                    |> writer.WriteLine
                    writer.WriteLine("aqualis_process_status=$?")
                    writer.WriteLine("if [ \"$aqualis_process_status\" -ne 0 ]; then")
                    writer.WriteLine("  aqualis_exit_status=$aqualis_process_status")
                    writer.WriteLine("fi"))

            ///<summary>
            ///<p>ソースファイルのコンパイル・実行するスクリプトファイルを生成</p>
            ///<p>エラーがある場合はメール送信</p>
            ///<p>終了後、実行時間をメール送信</p>
            ///</summary>
            ///<param name="address">メールアドレス</param>
            member this.AddProcess (address:string) =
                if String.IsNullOrWhiteSpace address then
                    invalidArg (nameof address) "The notification address must not be empty."

                this.addProcess(fun writer scriptName ->
                    let mailCommand =
                        ShellCommand.buildCommand
                            "mail"
                            ["-s"; "ProcessNotification"; address]
                    let messageCommand message =
                        ShellCommand.buildCommand "printf" ["%s\\n"; message]
                    let notify message = messageCommand message + " | " + mailCommand
                    let outputLog = ShellCommand.quoteArgument(project + ".log")
                    let timeLog = ShellCommand.quoteArgument(project + "_time.log")
                    let runCommand = ShellCommand.buildCommand "sh" [scriptName]

                    writer.WriteLine(notify ("project " + project + " started"))
                    writer.WriteLine("{ time " + runCommand + "; } > " + outputLog + " 2> " + timeLog)
                    writer.WriteLine("aqualis_process_status=$?")
                    writer.WriteLine("if [ \"$aqualis_process_status\" -eq 0 ]; then")
                    writer.WriteLine("  { " + messageCommand ("project " + project + " finished") + "; cat " + timeLog + "; } | " + mailCommand)
                    writer.WriteLine("else")
                    writer.WriteLine("  aqualis_exit_status=$aqualis_process_status")
                    writer.WriteLine("  { " + messageCommand ("project " + project + " failed") + "; cat " + timeLog + "; } | " + mailCommand)
                    writer.WriteLine("fi")
                    writer.WriteLine())

            /// Closes the script writer.
            member __.Close() =
                disposeWriters()

            interface IDisposable with
                /// Disposes the script writer.
                member _.Dispose() =
                    disposeWriters()

        /// Creates a shell script for a generated project and runs the configuration callback.
        let makeShellScript (context:Aqualis) (dir:string) (project:string) (n:int) code =
            use proc = new Shell(context,dir,project,n)
            code proc
