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
                p.param.isOaccUsed <- true
                p.param.switch_parmode(true)
                if p.param.civar.list.Length>0 then
                    let instr  = String.Join(",", p.param.civar)
                    if p.param.covar.list.Length>0 then
                        let outstr = String.Join(",", p.param.covar)
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
                elif p.param.covar.list.Length>0 then
                    let outstr = String.Join(",", p.param.covar)
                    p.codewrite("!$acc data copyout("+outstr+")\n")
                    p.codewrite("!$acc kernels"+"\n")
                    code()
                    p.codewrite("!$acc end kernels"+"\n")
                    p.codewrite("!$acc end data"+"\n")
                else
                    p.codewrite("!$acc kernels"+"\n")
                    code()
                    p.codewrite("!$acc end kernels\n")
                p.param.switch_parmode(false)
            |C ->
                p.param.isOaccUsed <- true
                p.param.switch_parmode(true)
                if p.param.civar.list.Length>0 then
                    let instr  = String.Join(",", p.param.civar)
                    if p.param.covar.list.Length>0 then
                        let outstr = String.Join(",", p.param.covar)
                        p.codewrite("#pragma acc data copyin("+instr+") copyout("+outstr+")\n")
                        p.codewrite("{"+"\n")
                        p.codewrite("#pragma acc kernels"+"\n")
                        p.codewrite("{"+"\n")
                        code()
                        p.codewrite("}"+"\n")
                        p.codewrite("}"+"\n")
                    else
                        p.codewrite("#pragma acc data copyin("+instr+")\n")
                        p.codewrite("{"+"\n")
                        p.codewrite("#pragma acc kernels"+"\n")
                        p.codewrite("{"+"\n")
                        code()
                        p.codewrite("}"+"\n")
                        p.codewrite("}"+"\n")
                elif p.param.covar.list.Length>0 then
                    let outstr = String.Join(",", p.param.covar)
                    p.codewrite("#pragma acc data copyout("+outstr+")\n")
                    p.codewrite("{"+"\n")
                    p.codewrite("#pragma acc kernels"+"\n")
                    p.codewrite("{"+"\n")
                    code()
                    p.codewrite("}"+"\n")
                    p.codewrite("}"+"\n")
                else
                    p.codewrite("#pragma acc kernels"+"\n")
                    p.codewrite("{"+"\n")
                    code()
                    p.codewrite("}"+"\n")
                p.param.switch_parmode(false)
            |_ ->
                Console.WriteLine("Error : この言語では並列化を実行できません")
                p.codewrite("Error : この言語では並列化を実行できません")
                