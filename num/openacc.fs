// 
// Copyright (c) 2026 Jun-ichiro Sugisaka
// 
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
// 
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
                        codewritein("!$acc data copyin("+instr+") copyout("+outstr+")\n")
                        codewritein("!$acc kernels"+"\n")
                        code()
                        codewritein("!$acc end kernels"+"\n")
                        codewritein("!$acc end data"+"\n")
                    else
                        codewritein("!$acc data copyin("+instr+")\n")
                        codewritein("!$acc kernels"+"\n")
                        code()
                        codewritein("!$acc end kernels"+"\n")
                        codewritein("!$acc end data"+"\n")
                elif programList[prIndex].varCopyOut.list.Length>0 then
                    let outstr = String.Join(",", programList[prIndex].varCopyOut.list)
                    codewritein("!$acc data copyout("+outstr+")\n")
                    codewritein("!$acc kernels"+"\n")
                    code()
                    codewritein("!$acc end kernels"+"\n")
                    codewritein("!$acc end data"+"\n")
                else
                    codewritein("!$acc kernels"+"\n")
                    code()
                    codewritein "!$acc end kernels\n"
                isParMode <- false
            |C99 ->
                isOaccUsed <- true
                isParMode <- true
                if programList[prIndex].varCopyIn.list.Length>0 then
                    let instr  = String.Join(",", programList[prIndex].varCopyIn.list)
                    if programList[prIndex].varCopyOut.list.Length>0 then
                        let outstr = String.Join(",", programList[prIndex].varCopyOut.list)
                        codewritein("#pragma acc data copyin("+instr+") copyout("+outstr+")\n")
                        codewritein("{"+"\n")
                        codewritein("#pragma acc kernels"+"\n")
                        codewritein("{"+"\n")
                        code()
                        codewritein("}"+"\n")
                        codewritein("}"+"\n")
                    else
                        codewritein("#pragma acc data copyin("+instr+")\n")
                        codewritein("{"+"\n")
                        codewritein("#pragma acc kernels"+"\n")
                        codewritein("{"+"\n")
                        code()
                        codewritein("}"+"\n")
                        codewritein("}"+"\n")
                elif programList[prIndex].varCopyOut.list.Length>0 then
                    let outstr = String.Join(",", programList[prIndex].varCopyOut.list)
                    codewritein("#pragma acc data copyout("+outstr+")\n")
                    codewritein("{"+"\n")
                    codewritein("#pragma acc kernels"+"\n")
                    codewritein("{"+"\n")
                    code()
                    codewritein("}"+"\n")
                    codewritein("}"+"\n")
                else
                    codewritein("#pragma acc kernels"+"\n")
                    codewritein("{"+"\n")
                    code()
                    codewritein("}"+"\n")
                isParMode <- false
            |_ ->
                Console.WriteLine "Error : この言語では並列化を実行できません"
                codewritein "Error : この言語では並列化を実行できません"
                