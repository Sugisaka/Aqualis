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
            match (GenerationScope.currentProgram()).language with
            |Fortran ->
                (GenerationScope.requireContext()).IsOpenAccUsed <- true
                if (GenerationScope.currentProgram()).varCopyIn.list.Length>0 then
                    let instr  = String.Join(",", (GenerationScope.currentProgram()).varCopyIn.list)
                    if (GenerationScope.currentProgram()).varCopyOut.list.Length>0 then
                        let outstr = String.Join(",", (GenerationScope.currentProgram()).varCopyOut.list)
                        writein("!$acc data copyin("+instr+") copyout("+outstr+")\n")
                        writein("!$acc kernels"+"\n")
                        (GenerationScope.requireContext()).WithParallelMode(code)
                        writein("!$acc end kernels"+"\n")
                        writein("!$acc end data"+"\n")
                    else
                        writein("!$acc data copyin("+instr+")\n")
                        writein("!$acc kernels"+"\n")
                        (GenerationScope.requireContext()).WithParallelMode(code)
                        writein("!$acc end kernels"+"\n")
                        writein("!$acc end data"+"\n")
                elif (GenerationScope.currentProgram()).varCopyOut.list.Length>0 then
                    let outstr = String.Join(",", (GenerationScope.currentProgram()).varCopyOut.list)
                    writein("!$acc data copyout("+outstr+")\n")
                    writein("!$acc kernels"+"\n")
                    (GenerationScope.requireContext()).WithParallelMode(code)
                    writein("!$acc end kernels"+"\n")
                    writein("!$acc end data"+"\n")
                else
                    writein("!$acc kernels"+"\n")
                    (GenerationScope.requireContext()).WithParallelMode(code)
                    writein "!$acc end kernels\n"
            |C99 ->
                (GenerationScope.requireContext()).IsOpenAccUsed <- true
                if (GenerationScope.currentProgram()).varCopyIn.list.Length>0 then
                    let instr  = String.Join(",", (GenerationScope.currentProgram()).varCopyIn.list)
                    if (GenerationScope.currentProgram()).varCopyOut.list.Length>0 then
                        let outstr = String.Join(",", (GenerationScope.currentProgram()).varCopyOut.list)
                        writein("#pragma acc data copyin("+instr+") copyout("+outstr+")\n")
                        writein("{"+"\n")
                        writein("#pragma acc kernels"+"\n")
                        writein("{"+"\n")
                        (GenerationScope.requireContext()).WithParallelMode(code)
                        writein("}"+"\n")
                        writein("}"+"\n")
                    else
                        writein("#pragma acc data copyin("+instr+")\n")
                        writein("{"+"\n")
                        writein("#pragma acc kernels"+"\n")
                        writein("{"+"\n")
                        (GenerationScope.requireContext()).WithParallelMode(code)
                        writein("}"+"\n")
                        writein("}"+"\n")
                elif (GenerationScope.currentProgram()).varCopyOut.list.Length>0 then
                    let outstr = String.Join(",", (GenerationScope.currentProgram()).varCopyOut.list)
                    writein("#pragma acc data copyout("+outstr+")\n")
                    writein("{"+"\n")
                    writein("#pragma acc kernels"+"\n")
                    writein("{"+"\n")
                    (GenerationScope.requireContext()).WithParallelMode(code)
                    writein("}"+"\n")
                    writein("}"+"\n")
                else
                    writein("#pragma acc kernels"+"\n")
                    writein("{"+"\n")
                    (GenerationScope.requireContext()).WithParallelMode(code)
                    writein("}"+"\n")
            |_ ->
                Console.WriteLine "Error : この言語では並列化を実行できません"
                writein "Error : この言語では並列化を実行できません"
