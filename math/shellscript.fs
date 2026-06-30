// 
// Copyright (c) 2026 Jun-ichiro Sugisaka
// 
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
// 
namespace Aqualis
    
    module shellscript = 
        
        open System
        open System.IO
        open System.Text
        
        type Shell(dir:string,project:string,nproc:int) =
            let mutable id = 0
            let w =
                if nproc <= 0 then
                    invalidArg (nameof nproc) "The process count must be positive."

                let created = ResizeArray<StreamWriter>()
                try
                    for i in 1..nproc do
                        created.Add(
                            new StreamWriter(
                                dir+"\\shell_"+project+"_"+i.ToString("00")+".sh",
                                false,
                                Encoding.Default))
                    created.ToArray()
                with _ ->
                    created |> Seq.iter _.Dispose()
                    reraise()

            let disposeWriters() =
                w |> Array.iter _.Dispose()

            do
                for i in 1..nproc do
                    w.[i-1].Write("#!bin/bash"+"\n\n")
                    
            member private this.nextid() =
                id <- id + 1
                if id = nproc then id <- 0
                
            ///<summary>
            ///<p>ソースファイルのコンパイル・実行するスクリプトファイルを生成</p>
            ///</summary>
            member this.AddProcess() =
                    match AqualisCompiler.language with 
                    |Fortran -> 
                        w.[id].Write("sh proc_" + AqualisCompiler.projectName + "_F.sh\n")
                        this.nextid()
                    |C99 -> 
                        w.[id].Write("sh proc_" + AqualisCompiler.projectName + "_C.sh\n")
                        this.nextid()
                    |Python -> 
                        w.[id].Write("sh proc_" + AqualisCompiler.projectName + "_P.sh\n")
                        this.nextid()
                    |LaTeX -> 
                        ()
                    |HTML -> 
                        ()
                    |HTMLSequenceDiagram -> 
                        ()
                    |JavaScript ->
                        ()
                    |PHP ->
                        ()
                    |Numeric -> 
                        ()
                    
            ///<summary>
            ///<p>ソースファイルのコンパイル・実行するスクリプトファイルを生成</p>
            ///<p>エラーがある場合はメール送信</p>
            ///<p>終了後、実行時間をメール送信</p>
            ///</summary>
            ///<param name="address">メールアドレス</param>
            member __.AddProcess (address:string) =
                match AqualisCompiler.language with
                |Fortran   ->
                    w.[id].Write("echo project "+AqualisCompiler.projectName+" started | mail -s ProcessNotification "+address+"\n")
                    w.[id].Write("(time " + "sh proc_"+AqualisCompiler.projectName+".sh" + ") > "+AqualisCompiler.projectName+".log "+"&"+"> "+AqualisCompiler.projectName+"_time.log\n") 
                    w.[id].Write("(echo project "+AqualisCompiler.projectName+" finished | cat - "+AqualisCompiler.projectName+"_time.log) | mail -s ProcessNotification "+address+"\n")
                |C99 ->
                    w.[id].Write("echo project "+AqualisCompiler.projectName+" started | mail -s ProcessNotification "+address+"\n")
                    w.[id].Write("(time " + "sh proc_"+AqualisCompiler.projectName+"_C.sh" + ") > "+AqualisCompiler.projectName+".log "+"&"+"> "+AqualisCompiler.projectName+"_time.log\n") 
                    w.[id].Write("(echo project "+AqualisCompiler.projectName+" finished | cat - "+AqualisCompiler.projectName+"_time.log) | mail -s ProcessNotification "+address+"\n")
                |Python   ->
                    w.[id].Write("echo project "+AqualisCompiler.projectName+" started | mail -s ProcessNotification "+address+"\n")
                    w.[id].Write("(time " + "sh proc_"+AqualisCompiler.projectName+".sh" + ") > "+AqualisCompiler.projectName+".log "+"&"+"> "+AqualisCompiler.projectName+"_time.log\n") 
                    w.[id].Write("(echo project "+AqualisCompiler.projectName+" finished | cat - "+AqualisCompiler.projectName+"_time.log) | mail -s ProcessNotification "+address+"\n")
                |LaTeX   ->
                    ()
                |HTML   ->
                    ()
                |HTMLSequenceDiagram   ->
                    ()
                |JavaScript ->
                    ()
                |PHP ->
                    ()
                |Numeric -> 
                    ()
                    
            member __.Close() =
                disposeWriters()

            interface IDisposable with
                member _.Dispose() =
                    disposeWriters()
                        
        let makeShellScript (dir:string) (project:string) (n:int) code =
            use proc = new Shell(dir,project,n)
            code proc
