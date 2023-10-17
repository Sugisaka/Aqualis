(*
Copyright (c) 2022 Jun-ichiro Sugisaka

This software is released under the MIT License.
http://opensource.org/licenses/mit-license.php
*)
namespace Aqualis
    
    open System
    open Aqualis_base
    
    ///<summary>OpenACC処理</summary>
    type oacc () =
        ///<summary>ループを並列化</summary>
        static member parallelize code =
            match p.lang with
            |F ->
                p.isOaccUsed <- true
                p.parmode <| fun () ->
                    if p.civar.list.Length>0 then
                        let instr  = String.Join(",", p.civar)
                        if p.covar.list.Length>0 then
                            let outstr = String.Join(",", p.covar)
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
                    elif p.covar.list.Length>0 then
                        let outstr = String.Join(",", p.covar)
                        p.codewrite("!$acc data copyout("+outstr+")\n")
                        p.codewrite("!$acc kernels"+"\n")
                        code()
                        p.codewrite("!$acc end kernels"+"\n")
                        p.codewrite("!$acc end data"+"\n")
                    else
                        p.codewrite("!$acc kernels"+"\n")
                        code()
                        p.codewrite("!$acc end kernels\n")
            |C ->
                p.isOaccUsed <- true
                p.parmode <| fun () ->
                    if p.civar.list.Length>0 then
                        let instr  = String.Join(",", p.civar)
                        if p.covar.list.Length>0 then
                            let outstr = String.Join(",", p.covar)
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
                    elif p.covar.list.Length>0 then
                        let outstr = String.Join(",", p.covar)
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
            |_ ->
                Console.WriteLine("Error : この言語では並列化を実行できません")
                p.codewrite("Error : この言語では並列化を実行できません")
                