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
                        writein("!$acc data copyin("+instr+") copyout("+outstr+")\n")
                        writein("!$acc kernels"+"\n")
                        code()
                        writein("!$acc end kernels"+"\n")
                        writein("!$acc end data"+"\n")
                    else
                        writein("!$acc data copyin("+instr+")\n")
                        writein("!$acc kernels"+"\n")
                        code()
                        writein("!$acc end kernels"+"\n")
                        writein("!$acc end data"+"\n")
                elif programList[prIndex].varCopyOut.list.Length>0 then
                    let outstr = String.Join(",", programList[prIndex].varCopyOut.list)
                    writein("!$acc data copyout("+outstr+")\n")
                    writein("!$acc kernels"+"\n")
                    code()
                    writein("!$acc end kernels"+"\n")
                    writein("!$acc end data"+"\n")
                else
                    writein("!$acc kernels"+"\n")
                    code()
                    writein "!$acc end kernels\n"
                isParMode <- false
            |C99 ->
                isOaccUsed <- true
                isParMode <- true
                if programList[prIndex].varCopyIn.list.Length>0 then
                    let instr  = String.Join(",", programList[prIndex].varCopyIn.list)
                    if programList[prIndex].varCopyOut.list.Length>0 then
                        let outstr = String.Join(",", programList[prIndex].varCopyOut.list)
                        writein("#pragma acc data copyin("+instr+") copyout("+outstr+")\n")
                        writein("{"+"\n")
                        writein("#pragma acc kernels"+"\n")
                        writein("{"+"\n")
                        code()
                        writein("}"+"\n")
                        writein("}"+"\n")
                    else
                        writein("#pragma acc data copyin("+instr+")\n")
                        writein("{"+"\n")
                        writein("#pragma acc kernels"+"\n")
                        writein("{"+"\n")
                        code()
                        writein("}"+"\n")
                        writein("}"+"\n")
                elif programList[prIndex].varCopyOut.list.Length>0 then
                    let outstr = String.Join(",", programList[prIndex].varCopyOut.list)
                    writein("#pragma acc data copyout("+outstr+")\n")
                    writein("{"+"\n")
                    writein("#pragma acc kernels"+"\n")
                    writein("{"+"\n")
                    code()
                    writein("}"+"\n")
                    writein("}"+"\n")
                else
                    writein("#pragma acc kernels"+"\n")
                    writein("{"+"\n")
                    code()
                    writein("}"+"\n")
                isParMode <- false
            |_ ->
                Console.WriteLine "Error : この言語では並列化を実行できません"
                writein "Error : この言語では並列化を実行できません"
                