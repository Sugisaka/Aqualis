namespace Aqualis
    
    module shellscript = 
        
        open System.IO
        open System.Text
        
        type Shell(dir:string,project:string,nproc:int) =
            let mutable id = 0
            let mutable w = 
                [|for i in 1..nproc -> new StreamWriter(dir+"\\shell_"+project+"_"+i.ToString("00")+".sh",false,Encoding.Default) |]
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
                    match AqualisCompiler.lang with 
                    |Fortran -> 
                        w.[id].Write("sh proc_"+AqualisCompiler.projname+"_F.sh"+"\n")
                        this.nextid()
                    |C99 -> 
                        w.[id].Write("sh proc_"+AqualisCompiler.projname+"_C.sh"+"\n")
                        this.nextid()
                    |Python -> 
                        w.[id].Write("sh proc_"+AqualisCompiler.projname+"_P.sh"+"\n")
                        this.nextid()
                    |LaTeX -> 
                        ()
                    |HTML -> 
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
                match AqualisCompiler.lang with
                |Fortran   ->
                    w.[id].Write("echo project "+AqualisCompiler.projname+" started | mail -s ProcessNotification "+address+"\n")
                    w.[id].Write("(time " + "sh proc_"+AqualisCompiler.projname+".sh" + ") > "+AqualisCompiler.projname+".log "+"&"+"> "+AqualisCompiler.projname+"_time.log\n") 
                    w.[id].Write("(echo project "+AqualisCompiler.projname+" finished | cat - "+AqualisCompiler.projname+"_time.log) | mail -s ProcessNotification "+address+"\n")
                |C99 ->
                    w.[id].Write("echo project "+AqualisCompiler.projname+" started | mail -s ProcessNotification "+address+"\n")
                    w.[id].Write("(time " + "sh proc_"+AqualisCompiler.projname+"_C.sh" + ") > "+AqualisCompiler.projname+".log "+"&"+"> "+AqualisCompiler.projname+"_time.log\n") 
                    w.[id].Write("(echo project "+AqualisCompiler.projname+" finished | cat - "+AqualisCompiler.projname+"_time.log) | mail -s ProcessNotification "+address+"\n")
                |Python   ->
                    w.[id].Write("echo project "+AqualisCompiler.projname+" started | mail -s ProcessNotification "+address+"\n")
                    w.[id].Write("(time " + "sh proc_"+AqualisCompiler.projname+".sh" + ") > "+AqualisCompiler.projname+".log "+"&"+"> "+AqualisCompiler.projname+"_time.log\n") 
                    w.[id].Write("(echo project "+AqualisCompiler.projname+" finished | cat - "+AqualisCompiler.projname+"_time.log) | mail -s ProcessNotification "+address+"\n")
                |LaTeX   ->
                    ()
                |HTML   ->
                    ()
                |Numeric -> 
                    ()
                    
            member __.Close() =
                for i in 1..nproc do
                    w.[i-1].Close()
                        
        let makeShellScript (dir:string) (project:string) (n:int) code =
            let proc = new Shell(dir,project,n)
            code proc
            proc.Close()
