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
    
    ///<summary>OpenMP処理</summary>
    type omp () =
        ///<summary>ループを並列化</summary>
        static member parallelize code =
            match p.lang with
            |F ->
                p.param.isOmpUsed <- true
                p.param.module_("omp_lib")
                p.param.switch_parmode(true)
                p.param.cclose()
                p.param.popen()
                code()
                p.param.pclose()
                p.param.switch_parmode(false)
                p.param.copen()
                let rec printplist list n counter (s:string) =
                    match (list,counter) with
                    |[],_ ->
                        if (counter>0) && n=0 then
                            p.codewrite("!$omp parallel do private("+s+")"+"\n")
                        elif (counter>0) then
                            p.codewrite("!$omp private("+s+")"+"\n")
                        else
                            ()
                    |(_,_,pp,_)::lst,7 ->
                        if n=0 then
                            p.codewrite("!$omp parallel do private("+s+") &"+"\n")
                        else
                            p.codewrite("!$omp private("+s+") &"+"\n")
                        printplist lst 1 1 pp
                    |(_,_,p,_)::lst,_ ->
                        printplist lst n (counter+1) (s+(if counter=0 then "" else ",")+p)
                printplist p.param.pvar.list 0 0 ""
                //一時ファイルの内容を書き込み
                p.param.pclose()
                p.param.cwrite(p.param.readpartext())
                p.param.pdelete()
                p.codewrite("!$omp end parallel do\n")
                p.param.pvar.clear()
            |C ->
                p.param.isOmpUsed <- true
                p.param.include_("<omp.h>")
                p.param.switch_parmode(true)
                p.param.cclose()
                p.param.popen()
                code()
                p.param.pclose()
                p.param.switch_parmode(false)
                p.param.copen()
                if p.param.pvar.list.Length>0 then
                    let s = String.Join(",", List.map (fun (_,_,s,_) -> s) p.param.pvar.list)
                    p.codewrite("#pragma omp parallel for private("+s+")\n")
                else
                    p.codewrite("#pragma omp parallel for\n")
                //一時ファイルの内容を書き込み
                p.param.cwrite(p.param.readpartext())
                p.param.pvar.clear()
            |_ ->
                ()

        ///<summary>ループを並列化</summary>
        static member parallelize_th (th:int) = fun code ->
            match p.lang with
            |F ->
                p.param.isOmpUsed <- true
                p.param.module_("omp_lib")
                p.param.switch_parmode(true)
                p.param.cclose()
                p.param.popen()
                code()
                p.param.pclose()
                p.param.switch_parmode(false)
                p.param.copen()
                let rec printplist list n counter (s:string) =
                    match (list,counter) with
                    |[],_ ->
                        if (counter>0) && n=0 then
                            p.codewrite("!$omp parallel do private("+s+") num_threads("+(string th)+")"+"\n")
                        elif (counter>0) then
                            p.codewrite("!$omp private("+s+")"+"\n")
                        else
                            ()
                    |(_,_,pp,_)::lst,7 ->
                        if n=0 then
                            p.codewrite("!$omp parallel do private("+s+") num_threads("+(string th)+") &"+"\n")
                        else
                            p.codewrite("!$omp private("+s+") &"+"\n")
                        printplist lst 1 1 pp
                    |(_,_,p,_)::lst,_ ->
                        printplist lst n (counter+1) (s+(if counter=0 then "" else ",")+p)
                printplist p.param.pvar.list 0 0 ""
                //一時ファイルの内容を書き込み
                p.param.cwrite(p.param.readpartext())
                p.codewrite("!$omp end parallel do\n")
                p.param.pvar.clear()
            |C ->
                p.param.isOmpUsed <- true
                p.param.include_("<omp.h>")
                p.param.switch_parmode(true)
                p.param.cclose()
                p.param.popen()
                code()
                p.param.pclose()
                p.param.switch_parmode(false)
                p.param.copen()
                if p.param.pvar.list.Length>0 then
                    let s = String.Join(",", List.map (fun (_,_,s,_) -> s) p.param.pvar.list)
                    p.codewrite("#pragma omp parallel for private("+s+") num_threads("+(string th)+")\n")
                else
                    p.codewrite("#pragma omp parallel for num_threads("+(string th)+")\n")
                //一時ファイルの内容を書き込み
                p.param.cwrite(p.param.readpartext())
                p.param.pvar.clear()
            |_ ->
                ()

        ///<summary>ループを並列化</summary>
        static member reduction (var:num0) (ope:string) = fun code ->
            match p.lang with
            |F ->
                p.param.isOmpUsed <- true
                p.param.module_("omp_lib")
                p.param.switch_parmode(true)
                p.param.cclose()
                p.param.popen()
                code()
                p.param.pclose()
                p.param.switch_parmode(false)
                p.param.copen()
                let rec printplist list n counter (s:string) =
                    match (list,counter) with
                    |[],_ ->
                        if (counter>0) && n=0 then
                            p.codewrite("!$omp parallel do private("+s+") reduction("+ope+":"+var.code+")"+"\n")
                        elif (counter>0) then
                            p.codewrite("!$omp private("+s+")"+"\n")
                        else
                            ()
                    |(_,_,pp,_)::lst,7 ->
                        if n=0 then
                            p.codewrite("!$omp parallel do private("+s+") reduction("+ope+":"+var.code+") &"+"\n")
                        else
                            p.codewrite("!$omp private("+s+") &"+"\n")
                        printplist lst 1 1 pp
                    |(_,_,p,_)::lst,_ ->
                        printplist lst n (counter+1) (s+(if counter=0 then "" else ",")+p)
                printplist p.param.pvar.list 0 0 ""
                //一時ファイルの内容を書き込み
                p.param.cwrite(p.param.readpartext())
                p.codewrite("!$omp end parallel do\n")
                p.param.pvar.clear()
            |C ->
                p.param.isOmpUsed <- true
                p.param.include_("<omp.h>")
                p.param.switch_parmode(true)
                p.param.cclose()
                p.param.popen()
                code()
                p.param.pclose()
                p.param.switch_parmode(false)
                p.param.copen()
                if p.param.pvar.list.Length>0 then
                    let s = String.Join(",", List.map (fun (_,_,s,_) -> s) p.param.pvar.list)
                    match ope with
                    |"+" |"-" |"*" ->
                        p.codewrite("#pragma omp parallel for private("+s+") reduction("+ope+":"+var.code+")\n")
                    |_ ->
                        ()
                else
                    match ope with
                    |"+" |"-" |"*" ->
                        p.codewrite("#pragma omp parallel for reduction("+ope+":"+var.code+")\n")
                    |_ ->
                        ()
                //一時ファイルの内容を書き込み
                p.param.cwrite(p.param.readpartext())
                p.param.pvar.clear()
            |_ ->
                ()

        ///<summary>ループを並列化</summary>
        static member reduction_th (th:int) (var:num0) (ope:string) = fun code ->
            match p.lang with
            |F ->
                p.param.isOmpUsed <- true
                p.param.module_("omp_lib")
                p.param.switch_parmode(true)
                p.param.cclose()
                p.param.popen()
                code()
                p.param.pclose()
                p.param.switch_parmode(false)
                p.param.copen()
                let rec printplist list n counter (s:string) =
                    match (list,counter) with
                    |[],_ ->
                        if (counter>0) && n=0 then
                            p.codewrite("!$omp parallel do private("+s+") num_threads("+(string th)+") reduction("+ope+":"+var.code+")"+"\n")
                        elif (counter>0) then
                            p.codewrite("!$omp private("+s+")"+"\n")
                        else
                            ()
                    |(_,_,pp,_)::lst,7 ->
                        if n=0 then
                            p.codewrite("!$omp parallel do private("+s+") num_threads("+(string th)+") reduction("+ope+":"+var.code+") &"+"\n")
                        else
                            p.codewrite("!$omp private("+s+") &"+"\n")
                        printplist lst 1 1 pp
                    |(_,_,p,_)::lst,_ ->
                        printplist lst n (counter+1) (s+(if counter=0 then "" else ",")+p)
                printplist p.param.pvar.list 0 0 ""
                //一時ファイルの内容を書き込み
                p.param.cwrite(p.param.readpartext())
                p.codewrite("!$omp end parallel do\n")
                p.param.pvar.clear()
            |C ->
                p.param.isOmpUsed <- true
                p.param.include_("<omp.h>")
                p.param.switch_parmode(true)
                p.param.cclose()
                p.param.popen()
                code()
                p.param.pclose()
                p.param.switch_parmode(false)
                p.param.copen()
                if p.param.pvar.list.Length>0 then
                    let s = String.Join(",", List.map (fun (_,_,s,_) -> s) p.param.pvar.list)
                    match ope with
                    |"+" |"-" |"*" ->
                        p.codewrite("#pragma omp parallel for private("+s+") num_threads("+(string th)+") reduction("+ope+":"+var.code+")\n")
                    |_ ->
                        ()
                else
                    match ope with
                    |"+" |"-" |"*" ->
                        p.codewrite("#pragma omp parallel for num_threads("+(string th)+") reduction("+ope+":"+var.code+")\n")
                    |_ ->
                        ()
                //一時ファイルの内容を書き込み
                p.param.cwrite(p.param.readpartext())
                p.param.pvar.clear()
            |_ ->
                ()

        ///<summary>それぞれ別スレッドで実行</summary>
        static member sections (th:int) = fun code ->
            match p.lang with
            |F ->
                p.param.isOmpUsed <- true
                p.param.module_("omp_lib")
                p.param.switch_parmode(true)
                p.param.cclose()
                p.param.popen()
                p.indentInc()
                code()
                p.indentDec()
                p.param.pclose()
                p.param.switch_parmode(false)
                p.param.copen()
                if p.param.pvar.list.Length>0 then
                    let s = String.Join(",", List.map (fun (_,_,s,_) -> s) p.param.pvar.list)
                    p.codewrite("!$omp parallel private("+s+") num_threads("+(string th)+")\n")
                else
                    p.codewrite("!$omp parallel num_threads("+(string th)+")\n")
                p.indentInc()
                p.codewrite("!$omp sections"+"\n")
                p.indentInc()
                p.param.cwrite(p.param.readpartext())
                p.indentDec()
                p.codewrite("!$omp end sections"+"\n")
                p.indentDec()
                p.codewrite("!$omp end parallel\n")
                p.param.pvar.clear()
            |C ->
                p.param.isOmpUsed <- true
                p.param.include_("<omp.h>")
                p.param.switch_parmode(true)
                p.param.cclose()
                p.param.popen()
                p.indentInc()
                code()
                p.indentDec()
                p.param.pclose()
                p.param.switch_parmode(false)
                p.param.copen()
                if p.param.pvar.list.Length>0 then
                    let s = String.Join(",", List.map (fun (_,_,s,_) -> p.param.pvar.list))
                    p.codewrite("#pragma omp parallel private("+s+") num_threads("+(string th)+")\n")
                else
                    p.codewrite("#pragma omp parallel num_threads("+(string th)+")\n")
                p.codewrite("{"+"\n")
                p.indentInc()
                p.codewrite("#pragma omp sections"+"\n")
                p.codewrite("{"+"\n")
                p.indentInc()
                p.param.cwrite(p.param.readpartext())
                p.indentDec()
                p.codewrite("}"+"\n")
                p.indentDec()
                p.codewrite("}"+"\n")
                p.param.pvar.clear()
            |_ ->
                ()

        ///<summary>別スレッドで実行</summary>
        static member section code =
            match p.lang with
            |F ->
                p.codewrite("!$omp section"+"\n")
                code()
            |C ->
                p.codewrite("#pragma omp section"+"\n")
                p.codewrite("{"+"\n")
                code()
                p.codewrite("}"+"\n")
            |_ ->
                ()

        ///<summary>スレッド番号の取得</summary>
        static member thread_num with get() =
            match p.lang with
            |F |C ->
                num0(It 4,Var "omp_get_thread_num()")
            |_ ->
                Console.WriteLine("Error : この言語ではスレッド番号の取得はできません")
                num0(Nt,NaN)

        ///<summary>最大スレッド数の取得</summary>
        static member max_threads with get() =
            match p.lang with
            |F |C ->
                num0(It 4,Var "omp_get_max_threads()")
            |_ ->
                Console.WriteLine("この言語ではスレッド番号の取得はできません")
                num0(Nt,NaN)