namespace Aqualis
    
    open System
    
    ///<summary>OpenACC処理</summary>
    type oacc =
        static member temp = 0
        ///<summary>ループを並列化</summary>
        static member parallelize code =
            match programList[prIndex].language with
            |Fortran ->
                isOaccUsed <- true
                isParMode <- true
                if programList[prIndex].varCopyIn.list.Length>0 then
                    let instr  = String.Join(",", programList[prIndex].varCopyIn.list)
                    if programList[prIndex].varCopyOut.list.Length>0 then
                        let outstr = String.Join(",", programList[prIndex].varCopyOut.list)
                        codewrite("!$acc data copyin("+instr+") copyout("+outstr+")\n")
                        codewrite("!$acc kernels"+"\n")
                        code()
                        codewrite("!$acc end kernels"+"\n")
                        codewrite("!$acc end data"+"\n")
                    else
                        codewrite("!$acc data copyin("+instr+")\n")
                        codewrite("!$acc kernels"+"\n")
                        code()
                        codewrite("!$acc end kernels"+"\n")
                        codewrite("!$acc end data"+"\n")
                elif programList[prIndex].varCopyOut.list.Length>0 then
                    let outstr = String.Join(",", programList[prIndex].varCopyOut.list)
                    codewrite("!$acc data copyout("+outstr+")\n")
                    codewrite("!$acc kernels"+"\n")
                    code()
                    codewrite("!$acc end kernels"+"\n")
                    codewrite("!$acc end data"+"\n")
                else
                    codewrite("!$acc kernels"+"\n")
                    code()
                    codewrite "!$acc end kernels\n"
                isParMode <- false
            |C99 ->
                isOaccUsed <- true
                isParMode <- true
                if programList[prIndex].varCopyIn.list.Length>0 then
                    let instr  = String.Join(",", programList[prIndex].varCopyIn.list)
                    if programList[prIndex].varCopyOut.list.Length>0 then
                        let outstr = String.Join(",", programList[prIndex].varCopyOut.list)
                        codewrite("#pragma acc data copyin("+instr+") copyout("+outstr+")\n")
                        codewrite("{"+"\n")
                        codewrite("#pragma acc kernels"+"\n")
                        codewrite("{"+"\n")
                        code()
                        codewrite("}"+"\n")
                        codewrite("}"+"\n")
                    else
                        codewrite("#pragma acc data copyin("+instr+")\n")
                        codewrite("{"+"\n")
                        codewrite("#pragma acc kernels"+"\n")
                        codewrite("{"+"\n")
                        code()
                        codewrite("}"+"\n")
                        codewrite("}"+"\n")
                elif programList[prIndex].varCopyOut.list.Length>0 then
                    let outstr = String.Join(",", programList[prIndex].varCopyOut.list)
                    codewrite("#pragma acc data copyout("+outstr+")\n")
                    codewrite("{"+"\n")
                    codewrite("#pragma acc kernels"+"\n")
                    codewrite("{"+"\n")
                    code()
                    codewrite("}"+"\n")
                    codewrite("}"+"\n")
                else
                    codewrite("#pragma acc kernels"+"\n")
                    codewrite("{"+"\n")
                    code()
                    codewrite("}"+"\n")
                isParMode <- false
            |_ ->
                Console.WriteLine "Error : この言語では並列化を実行できません"
                codewrite "Error : この言語では並列化を実行できません"
                