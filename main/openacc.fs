namespace Aqualis
    
    open System
    
    ///<summary>OpenACC処理</summary>
    type oacc =
        static member temp = 0
        ///<summary>ループを並列化</summary>
        static member parallelize code =
            match pr.language with
            |Fortran ->
                isOaccUsed <- true
                isParMode <- true
                if pr.varCopyIn.list.Length>0 then
                    let instr  = String.Join(",", pr.varCopyIn.list)
                    if pr.varCopyOut.list.Length>0 then
                        let outstr = String.Join(",", pr.varCopyOut.list)
                        pr.cwriter.codewrite("!$acc data copyin("+instr+") copyout("+outstr+")\n")
                        pr.cwriter.codewrite("!$acc kernels"+"\n")
                        code()
                        pr.cwriter.codewrite("!$acc end kernels"+"\n")
                        pr.cwriter.codewrite("!$acc end data"+"\n")
                    else
                        pr.cwriter.codewrite("!$acc data copyin("+instr+")\n")
                        pr.cwriter.codewrite("!$acc kernels"+"\n")
                        code()
                        pr.cwriter.codewrite("!$acc end kernels"+"\n")
                        pr.cwriter.codewrite("!$acc end data"+"\n")
                elif pr.varCopyOut.list.Length>0 then
                    let outstr = String.Join(",", pr.varCopyOut.list)
                    pr.cwriter.codewrite("!$acc data copyout("+outstr+")\n")
                    pr.cwriter.codewrite("!$acc kernels"+"\n")
                    code()
                    pr.cwriter.codewrite("!$acc end kernels"+"\n")
                    pr.cwriter.codewrite("!$acc end data"+"\n")
                else
                    pr.cwriter.codewrite("!$acc kernels"+"\n")
                    code()
                    pr.cwriter.codewrite "!$acc end kernels\n"
                isParMode <- false
            |C99 ->
                isOaccUsed <- true
                isParMode <- true
                if pr.varCopyIn.list.Length>0 then
                    let instr  = String.Join(",", pr.varCopyIn.list)
                    if pr.varCopyOut.list.Length>0 then
                        let outstr = String.Join(",", pr.varCopyOut.list)
                        pr.cwriter.codewrite("#pragma acc data copyin("+instr+") copyout("+outstr+")\n")
                        pr.cwriter.codewrite("{"+"\n")
                        pr.cwriter.codewrite("#pragma acc kernels"+"\n")
                        pr.cwriter.codewrite("{"+"\n")
                        code()
                        pr.cwriter.codewrite("}"+"\n")
                        pr.cwriter.codewrite("}"+"\n")
                    else
                        pr.cwriter.codewrite("#pragma acc data copyin("+instr+")\n")
                        pr.cwriter.codewrite("{"+"\n")
                        pr.cwriter.codewrite("#pragma acc kernels"+"\n")
                        pr.cwriter.codewrite("{"+"\n")
                        code()
                        pr.cwriter.codewrite("}"+"\n")
                        pr.cwriter.codewrite("}"+"\n")
                elif pr.varCopyOut.list.Length>0 then
                    let outstr = String.Join(",", pr.varCopyOut.list)
                    pr.cwriter.codewrite("#pragma acc data copyout("+outstr+")\n")
                    pr.cwriter.codewrite("{"+"\n")
                    pr.cwriter.codewrite("#pragma acc kernels"+"\n")
                    pr.cwriter.codewrite("{"+"\n")
                    code()
                    pr.cwriter.codewrite("}"+"\n")
                    pr.cwriter.codewrite("}"+"\n")
                else
                    pr.cwriter.codewrite("#pragma acc kernels"+"\n")
                    pr.cwriter.codewrite("{"+"\n")
                    code()
                    pr.cwriter.codewrite("}"+"\n")
                isParMode <- false
            |_ ->
                Console.WriteLine "Error : この言語では並列化を実行できません"
                pr.cwriter.codewrite "Error : この言語では並列化を実行できません"
                