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
                        programList[prIndex].codewrite("!$acc data copyin("+instr+") copyout("+outstr+")\n")
                        programList[prIndex].codewrite("!$acc kernels"+"\n")
                        code()
                        programList[prIndex].codewrite("!$acc end kernels"+"\n")
                        programList[prIndex].codewrite("!$acc end data"+"\n")
                    else
                        programList[prIndex].codewrite("!$acc data copyin("+instr+")\n")
                        programList[prIndex].codewrite("!$acc kernels"+"\n")
                        code()
                        programList[prIndex].codewrite("!$acc end kernels"+"\n")
                        programList[prIndex].codewrite("!$acc end data"+"\n")
                elif programList[prIndex].varCopyOut.list.Length>0 then
                    let outstr = String.Join(",", programList[prIndex].varCopyOut.list)
                    programList[prIndex].codewrite("!$acc data copyout("+outstr+")\n")
                    programList[prIndex].codewrite("!$acc kernels"+"\n")
                    code()
                    programList[prIndex].codewrite("!$acc end kernels"+"\n")
                    programList[prIndex].codewrite("!$acc end data"+"\n")
                else
                    programList[prIndex].codewrite("!$acc kernels"+"\n")
                    code()
                    programList[prIndex].codewrite "!$acc end kernels\n"
                isParMode <- false
            |C99 ->
                isOaccUsed <- true
                isParMode <- true
                if programList[prIndex].varCopyIn.list.Length>0 then
                    let instr  = String.Join(",", programList[prIndex].varCopyIn.list)
                    if programList[prIndex].varCopyOut.list.Length>0 then
                        let outstr = String.Join(",", programList[prIndex].varCopyOut.list)
                        programList[prIndex].codewrite("#pragma acc data copyin("+instr+") copyout("+outstr+")\n")
                        programList[prIndex].codewrite("{"+"\n")
                        programList[prIndex].codewrite("#pragma acc kernels"+"\n")
                        programList[prIndex].codewrite("{"+"\n")
                        code()
                        programList[prIndex].codewrite("}"+"\n")
                        programList[prIndex].codewrite("}"+"\n")
                    else
                        programList[prIndex].codewrite("#pragma acc data copyin("+instr+")\n")
                        programList[prIndex].codewrite("{"+"\n")
                        programList[prIndex].codewrite("#pragma acc kernels"+"\n")
                        programList[prIndex].codewrite("{"+"\n")
                        code()
                        programList[prIndex].codewrite("}"+"\n")
                        programList[prIndex].codewrite("}"+"\n")
                elif programList[prIndex].varCopyOut.list.Length>0 then
                    let outstr = String.Join(",", programList[prIndex].varCopyOut.list)
                    programList[prIndex].codewrite("#pragma acc data copyout("+outstr+")\n")
                    programList[prIndex].codewrite("{"+"\n")
                    programList[prIndex].codewrite("#pragma acc kernels"+"\n")
                    programList[prIndex].codewrite("{"+"\n")
                    code()
                    programList[prIndex].codewrite("}"+"\n")
                    programList[prIndex].codewrite("}"+"\n")
                else
                    programList[prIndex].codewrite("#pragma acc kernels"+"\n")
                    programList[prIndex].codewrite("{"+"\n")
                    code()
                    programList[prIndex].codewrite("}"+"\n")
                isParMode <- false
            |_ ->
                Console.WriteLine "Error : この言語では並列化を実行できません"
                programList[prIndex].codewrite "Error : この言語では並列化を実行できません"
                