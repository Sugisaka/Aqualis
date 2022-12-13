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
                let p = p.param
                p.isOmpUsed <- true
                p.switch_parmode(true)
                p.cclose()
                p.popen()
                code()
                p.pclose()
                p.switch_parmode(false)
                p.copen()
                let rec printplist list n counter (s:string) =
                    match (list,counter) with
                      |[],_ ->
                        if (counter>0) && n=0 then
                            p.codewrite("!$omp parallel do private("+s+")"+"\n")
                        elif (counter>0) then
                            p.codewrite("!$omp private("+s+")"+"\n")
                        else
                            ()
                      |pp::lst,7 ->
                        if n=0 then
                            p.codewrite("!$omp parallel do private("+s+") &"+"\n")
                        else
                            p.codewrite("!$omp private("+s+") &"+"\n")
                        printplist lst 1 1 pp
                      |p::lst,_ ->
                        printplist lst n (counter+1) (s+(if counter=0 then "" else ",")+p)
                printplist p.pvlist 0 0 ""
                //一時ファイルの内容を書き込み
                p.cwrite(p.readpartext())
                p.codewrite("!$omp end parallel do\n")
                p.clearpv()
              |C99 ->
                let p = p.param
                p.isOmpUsed <- true
                p.switch_parmode(true)
                p.cclose()
                p.popen()
                code()
                p.pclose()
                p.switch_parmode(false)
                p.copen()
                if p.pvlist.Length>0 then
                    let s = String.Join(",", p.pvlist)
                    p.codewrite("#pragma omp parallel for private("+s+")\n")
                else
                    p.codewrite("#pragma omp parallel for\n")
                //一時ファイルの内容を書き込み
                p.cwrite(p.readpartext())
                p.clearpv()
              |_ ->
                ()

        ///<summary>ループを並列化</summary>
        static member parallelize_th (th:int) = fun code ->
            match p.lang with
              |F ->
                let p = p.param
                p.isOmpUsed <- true
                p.switch_parmode(true)
                p.cclose()
                p.popen()
                code()
                p.pclose()
                p.switch_parmode(false)
                p.copen()
                let rec printplist list n counter (s:string) =
                    match (list,counter) with
                      |[],_ ->
                        if (counter>0) && n=0 then
                            p.codewrite("!$omp parallel do private("+s+") num_threads("+(string th)+")"+"\n")
                        elif (counter>0) then
                            p.codewrite("!$omp private("+s+")"+"\n")
                        else
                            ()
                      |pp::lst,7 ->
                        if n=0 then
                            p.codewrite("!$omp parallel do private("+s+") num_threads("+(string th)+") &"+"\n")
                        else
                            p.codewrite("!$omp private("+s+") &"+"\n")
                        printplist lst 1 1 pp
                      |p::lst,_ ->
                        printplist lst n (counter+1) (s+(if counter=0 then "" else ",")+p)
                printplist p.pvlist 0 0 ""
                //一時ファイルの内容を書き込み
                p.cwrite(p.readpartext())
                p.codewrite("!$omp end parallel do\n")
                p.clearpv()
              |C99 ->
                let p = p.param
                p.isOmpUsed <- true
                p.switch_parmode(true)
                p.cclose()
                p.popen()
                code()
                p.pclose()
                p.switch_parmode(false)
                p.copen()
                if p.pvlist.Length>0 then
                    let s = String.Join(",", p.pvlist)
                    p.codewrite("#pragma omp parallel for private("+s+") num_threads("+(string th)+")\n")
                else
                    p.codewrite("#pragma omp parallel for num_threads("+(string th)+")\n")
                //一時ファイルの内容を書き込み
                p.cwrite(p.readpartext())
                p.clearpv()
              |_ ->
                ()

        ///<summary>ループを並列化</summary>
        static member reduction (var:num0) (ope:string) = fun code ->
            match p.lang with
              |F ->
                let p = p.param
                p.isOmpUsed <- true
                p.switch_parmode(true)
                p.cclose()
                p.popen()
                code()
                p.pclose()
                p.switch_parmode(false)
                p.copen()
                let rec printplist list n counter (s:string) =
                    match (list,counter) with
                      |[],_ ->
                        if (counter>0) && n=0 then
                            p.codewrite("!$omp parallel do private("+s+") reduction("+ope+":"+var.name+")"+"\n")
                        elif (counter>0) then
                            p.codewrite("!$omp private("+s+")"+"\n")
                        else
                            ()
                      |pp::lst,7 ->
                        if n=0 then
                            p.codewrite("!$omp parallel do private("+s+") reduction("+ope+":"+var.name+") &"+"\n")
                        else
                            p.codewrite("!$omp private("+s+") &"+"\n")
                        printplist lst 1 1 pp
                      |p::lst,_ ->
                        printplist lst n (counter+1) (s+(if counter=0 then "" else ",")+p)
                printplist p.pvlist 0 0 ""
                //一時ファイルの内容を書き込み
                p.cwrite(p.readpartext())
                p.codewrite("!$omp end parallel do\n")
                p.clearpv()
              |C99 ->
                let p = p.param
                p.isOmpUsed <- true
                p.switch_parmode(true)
                p.cclose()
                p.popen()
                code()
                p.pclose()
                p.switch_parmode(false)
                p.copen()
                if p.pvlist.Length>0 then
                    let s = String.Join(",", p.pvlist)
                    match ope with
                      |"+" |"-" |"*" ->
                        p.codewrite("#pragma omp parallel for private("+s+") reduction("+ope+":"+var.name+")\n")
                      |_ ->
                        ()
                else
                    match ope with
                      |"+" |"-" |"*" ->
                        p.codewrite("#pragma omp parallel for reduction("+ope+":"+var.name+")\n")
                      |_ ->
                        ()
                //一時ファイルの内容を書き込み
                p.cwrite(p.readpartext())
                p.clearpv()
              |_ ->
                ()

        ///<summary>ループを並列化</summary>
        static member reduction_th (th:int) (var:num0) (ope:string) = fun code ->
            match p.lang with
              |F ->
                let p = p.param
                p.isOmpUsed <- true
                p.switch_parmode(true)
                p.cclose()
                p.popen()
                code()
                p.pclose()
                p.switch_parmode(false)
                p.copen()
                let rec printplist list n counter (s:string) =
                    match (list,counter) with
                      |[],_ ->
                        if (counter>0) && n=0 then
                            p.codewrite("!$omp parallel do private("+s+") num_threads("+(string th)+") reduction("+ope+":"+var.name+")"+"\n")
                        elif (counter>0) then
                            p.codewrite("!$omp private("+s+")"+"\n")
                        else
                            ()
                      |pp::lst,7 ->
                        if n=0 then
                            p.codewrite("!$omp parallel do private("+s+") num_threads("+(string th)+") reduction("+ope+":"+var.name+") &"+"\n")
                        else
                            p.codewrite("!$omp private("+s+") &"+"\n")
                        printplist lst 1 1 pp
                      |p::lst,_ ->
                        printplist lst n (counter+1) (s+(if counter=0 then "" else ",")+p)
                printplist p.pvlist 0 0 ""
                //一時ファイルの内容を書き込み
                p.cwrite(p.readpartext())
                p.codewrite("!$omp end parallel do\n")
                p.clearpv()
              |C99 ->
                let p = p.param
                p.isOmpUsed <- true
                p.switch_parmode(true)
                p.cclose()
                p.popen()
                code()
                p.pclose()
                p.switch_parmode(false)
                p.copen()
                if p.pvlist.Length>0 then
                    let s = String.Join(",", p.pvlist)
                    match ope with
                      |"+" |"-" |"*" ->
                        p.codewrite("#pragma omp parallel for private("+s+") num_threads("+(string th)+") reduction("+ope+":"+var.name+")\n")
                      |_ ->
                        ()
                else
                    match ope with
                      |"+" |"-" |"*" ->
                        p.codewrite("#pragma omp parallel for num_threads("+(string th)+") reduction("+ope+":"+var.name+")\n")
                      |_ ->
                        ()
                //一時ファイルの内容を書き込み
                p.cwrite(p.readpartext())
                p.clearpv()
              |_ ->
                ()

        ///<summary>それぞれ別スレッドで実行</summary>
        static member sections (th:int) = fun code ->
            match p.lang with
              |F ->
                let p = p.param
                p.isOmpUsed <- true
                p.switch_parmode(true)
                p.cclose()
                p.popen()
                p.indentposition_inc()
                code()
                p.indentposition_dec()
                p.pclose()
                p.switch_parmode(false)
                p.copen()
                if p.pvlist.Length>0 then
                    let s = String.Join(",", p.pvlist)
                    p.codewrite("!$omp parallel private("+s+") num_threads("+(string th)+")\n")
                else
                    p.codewrite("!$omp parallel num_threads("+(string th)+")\n")
                p.indentposition_inc()
                p.codewrite("!$omp sections"+"\n")
                p.indentposition_inc()
                p.cwrite(p.readpartext())
                p.indentposition_dec()
                p.codewrite("!$omp end sections"+"\n")
                p.indentposition_dec()
                p.codewrite("!$omp end parallel do\n")
                p.clearpv()
              |C99 ->
                let p = p.param
                p.isOmpUsed <- true
                p.switch_parmode(true)
                p.cclose()
                p.popen()
                p.indentposition_inc()
                code()
                p.indentposition_dec()
                p.pclose()
                p.switch_parmode(false)
                p.copen()
                if p.pvlist.Length>0 then
                    let s = String.Join(",", p.pvlist)
                    p.codewrite("#pragma omp parallel private("+s+") num_threads("+(string th)+")\n")
                else
                    p.codewrite("#pragma omp parallel num_threads("+(string th)+")\n")
                p.codewrite("{"+"\n")
                p.indentposition_inc()
                p.codewrite("#pragma omp sections"+"\n")
                p.codewrite("{"+"\n")
                p.indentposition_inc()
                p.cwrite(p.readpartext())
                p.indentposition_dec()
                p.codewrite("}"+"\n")
                p.indentposition_dec()
                p.codewrite("}"+"\n")
                p.clearpv()
              |_ ->
                ()

        ///<summary>別スレッドで実行</summary>
        static member section code =
            match p.lang with
              |F ->
                let p = p.param
                p.codewrite("!$omp section"+"\n")
                code()
              |C99 ->
                let p = p.param
                p.codewrite("#pragma omp section"+"\n")
                p.codewrite("{"+"\n")
                code()
                p.codewrite("}"+"\n")
              |_ ->
                ()

        ///<summary>スレッド番号の取得</summary>
        static member thread_num with get() =
            match p.lang with
              |F |C99 ->
                Var(It 4,"omp_get_thread_num()",[])
              |_ ->
                Console.WriteLine("Error : この言語ではスレッド番号の取得はできません")
                NaN

        ///<summary>最大スレッド数の取得</summary>
        static member max_threads with get() =
            match p.lang with
              |F |C99 ->
                Var(It 4,"omp_get_max_threads()",[])
              |_ ->
                Console.WriteLine("この言語ではスレッド番号の取得はできません")
                NaN
