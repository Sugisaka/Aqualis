(*
Copyright (c) 2022 Jun-ichiro Sugisaka

This software is released under the MIT License.
http://opensource.org/licenses/mit-license.php
*)
namespace Aqualis
    
    open System
    open System.IO
    open System.Text
    open Aqualis_base
    
    ///<summary>OpenACC処理</summary>
    type oacc () =
        ///<summary>ループを並列化</summary>
        static member parallelize code =
            match p.lang with
              |F ->
                let p = p.param
                p.isOaccUsed <- true
                p.switch_parmode(true)
                if p.copy_in_vlist.Length>0 then
                    let instr  = String.Join(",", p.copy_in_vlist)
                    if p.copy_out_vlist.Length>0 then
                        let outstr = String.Join(",", p.copy_out_vlist)
                        p.codewrite("!$acc data copyin("+instr+") copyout("+outstr+")\n")
                        p.codewrite("!$acc kernels"+"\n")
                        code()
                        p.codewrite("!$acc end kernels"+"\n")
                        p.codewrite("!$acc end data"+"\n")
                    else
                        p.codewrite("!$acc data copyin("+instr+")\n")
                        p.codewrite("!$acc kernels"+"\n")
                        code()
                        p.codewrite("!$acc end kernels"+"\n")
                        p.codewrite("!$acc end data"+"\n")
                elif p.copy_out_vlist.Length>0 then
                    let outstr = String.Join(",", p.copy_out_vlist)
                    p.codewrite("!$acc data copyout("+outstr+")\n")
                    p.codewrite("!$acc kernels"+"\n")
                    code()
                    p.codewrite("!$acc end kernels"+"\n")
                    p.codewrite("!$acc end data"+"\n")
                else
                    p.codewrite("!$acc kernels"+"\n")
                    code()
                    p.codewrite("!$acc end kernels\n")
                p.switch_parmode(false)
              |C99 ->
                let p = p.param
                p.isOaccUsed <- true
                p.switch_parmode(true)
                if p.copy_in_vlist.Length>0 then
                    let instr  = String.Join(",", p.copy_in_vlist)
                    if p.copy_out_vlist.Length>0 then
                        let outstr = String.Join(",", p.copy_out_vlist)
                        p.codewrite("#pragma acc data copyin("+instr+") copyout("+outstr+")\n")
                        p.codewrite("#pragma acc kernels"+"\n")
                        code()
                        p.codewrite("#pragma acc end kernels"+"\n")
                        p.codewrite("#pragma acc end data"+"\n")
                    else
                        p.codewrite("#pragma acc data copyin("+instr+")\n")
                        p.codewrite("#pragma acc kernels"+"\n")
                        code()
                        p.codewrite("#pragma acc end kernels"+"\n")
                        p.codewrite("#pragma acc end data"+"\n")
                elif p.copy_out_vlist.Length>0 then
                    let outstr = String.Join(",", p.copy_out_vlist)
                    p.codewrite("#pragma acc data copyout("+outstr+")\n")
                    p.codewrite("#pragma acc kernels"+"\n")
                    code()
                    p.codewrite("#pragma acc end kernels"+"\n")
                    p.codewrite("#pragma acc end data"+"\n")
                else
                    p.codewrite("#pragma acc kernels"+"\n")
                    code()
                    p.codewrite("#pragma acc end kernels\n")
                p.switch_parmode(false)
              |_ ->
                let p = p.param
                Console.WriteLine("Error : この言語では並列化を実行できません")
                p.codewrite("Error : この言語では並列化を実行できません")
                