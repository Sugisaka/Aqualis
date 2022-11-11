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
                p.cwrite("parallel block\n")
                code()
                p.cwrite("end parallel block\n")
                p.pclose()
                p.cclose()
                //並列化するループ処理を一時ファイルに書き込み
                p.writepcode()
                p.pclose()
                p.rewritecfile()
                if p.pvlist.Length>0 then
                    let s = String.Join(",", p.pvlist)
                    p.codewrite("!$omp parallel do private("+s+")\n")
                else
                    p.codewrite("!$omp parallel do\n")
                //一時ファイルの内容を書き込み
                p.cwrite(p.readpartext())
                p.codewrite("!$omp end parallel do\n")
                p.clearpv()
                p.switch_parmode(false)
              |C99 ->
                let p = p.param
                p.isOmpUsed <- true
                p.switch_parmode(true)
                p.cwrite("parallel block\n")
                p.indentposition_inc()
                code()
                p.indentposition_dec()
                p.cwrite("end parallel block\n")
                p.pclose()
                p.cclose()
                //並列化するループ処理を一時ファイルに書き込み
                p.writepcode()
                p.pclose()
                p.rewritecfile()
                if p.pvlist.Length>0 then
                    let s = String.Join(",", p.pvlist)
                    p.codewrite("#pragma omp parallel for private("+s+")\n")
                else
                    p.codewrite("#pragma omp parallel for\n")
                //一時ファイルの内容を書き込み
                p.cwrite(p.readpartext())
                p.clearpv()
                p.switch_parmode(false)
              |_ ->
                let p = p.param
                Console.WriteLine("Error : この言語では並列化を実行できません")
                p.codewrite("Error : この言語では並列化を実行できません")

        ///<summary>ループを並列化</summary>
        static member parallelize_th (th:int) = fun code ->
            match p.lang with
              |F ->
                let p = p.param
                p.isOmpUsed <- true
                p.switch_parmode(true)
                p.cwrite("parallel block\n")
                code()
                p.cwrite("end parallel block\n")
                p.pclose()
                p.cclose()
                //並列化するループ処理を一時ファイルに書き込み
                p.writepcode()
                p.pclose()
                p.rewritecfile()
                if p.pvlist.Length>0 then
                    let s = String.Join(",", p.pvlist)
                    p.codewrite("!$omp parallel do private("+s+") num_threads("+(string th)+")\n")
                else
                    p.codewrite("!$omp parallel do num_threads("+(string th)+")\n")
                //一時ファイルの内容を書き込み
                p.cwrite(p.readpartext())
                p.codewrite("!$omp end parallel do\n")
                p.clearpv()
                p.switch_parmode(false)
              |C99 ->
                let p = p.param
                p.isOmpUsed <- true
                p.switch_parmode(true)
                p.cwrite("parallel block\n")
                code()
                p.cwrite("end parallel block\n")
                p.pclose()
                p.cclose()
                //並列化するループ処理を一時ファイルに書き込み
                p.writepcode()
                p.pclose()
                p.rewritecfile()
                if p.pvlist.Length>0 then
                    let s = String.Join(",", p.pvlist)
                    p.codewrite("#pragma omp parallel for private("+s+") num_threads("+(string th)+")\n")
                else
                    p.codewrite("#pragma omp parallel for num_threads("+(string th)+")\n")
                //一時ファイルの内容を書き込み
                p.cwrite(p.readpartext())
                p.clearpv()
                p.switch_parmode(false)
              |_ ->
                let p = p.param
                Console.WriteLine("Error : この言語では並列化を実行できません")
                p.codewrite("Error : この言語では並列化を実行できません")

        ///<summary>ループを並列化</summary>
        static member reduction (var:num0) (ope:string) = fun code ->
            match p.lang with
              |F ->
                let p = p.param
                p.isOmpUsed <- true
                p.switch_parmode(true)
                p.cwrite("parallel block\n")
                code()
                p.cwrite("end parallel block\n")
                p.pclose()
                p.cclose()
                //並列化するループ処理を一時ファイルに書き込み
                p.writepcode()
                p.pclose()
                p.rewritecfile()
                if p.pvlist.Length>0 then
                    let s = String.Join(",", p.pvlist)
                    match ope with
                      |"+" |"-" |"*" ->
                        p.codewrite("!$omp parallel do private("+s+") reduction("+ope+":"+var.name+")\n")
                      |_ ->
                        ()
                else
                    match ope with
                      |"+" |"-" |"*" ->
                        p.codewrite("!$omp parallel do reduction("+ope+":"+var.name+")\n")
                      |_ ->
                        ()
                p.cwrite(p.readpartext())
                p.codewrite("!$omp end parallel do\n")
                p.clearpv()
                p.switch_parmode(false)
              |C99 ->
                let p = p.param
                p.isOmpUsed <- true
                p.switch_parmode(true)
                p.cwrite("parallel block\n")
                p.indentposition_inc()
                code()
                p.indentposition_dec()
                p.cwrite("end parallel block\n")
                p.pclose()
                p.cclose()
                //並列化するループ処理を一時ファイルに書き込み
                p.writepcode()
                p.pclose()
                p.rewritecfile()
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
                p.cwrite(p.readpartext())
                p.clearpv()
                p.switch_parmode(false)
              |_ ->
                let p = p.param
                Console.WriteLine("Error : この言語では並列化を実行できません")
                p.codewrite("Error : この言語では並列化を実行できません")

        ///<summary>ループを並列化</summary>
        static member reduction_th (th:int) (var:num0) (ope:string) = fun code ->
            match p.lang with
              |F ->
                let p = p.param
                p.isOmpUsed <- true
                p.switch_parmode(true)
                p.cwrite("parallel block\n")
                code()
                p.cwrite("end parallel block\n")
                p.pclose()
                p.cclose()
                //並列化するループ処理を一時ファイルに書き込み
                p.writepcode()
                p.pclose()
                p.rewritecfile()
                if p.pvlist.Length>0 then
                    let s = String.Join(",", p.pvlist)
                    match ope with
                      |"+" |"-" |"*" ->
                        p.codewrite("!$omp parallel do private("+s+") num_threads("+(string th)+") reduction("+ope+":"+var.name+")\n")
                      |_ ->
                        ()
                else
                    match ope with
                      |"+" |"-" |"*" ->
                        p.codewrite("!$omp parallel do num_threads("+(string th)+") reduction("+ope+":"+var.name+")\n")
                      |_ ->
                        ()
                p.cwrite(p.readpartext())
                p.codewrite("!$omp end parallel do\n")
                p.clearpv()
                p.switch_parmode(false)
              |C99 ->
                let p = p.param
                p.isOmpUsed <- true
                p.switch_parmode(true)
                p.cwrite("parallel block\n")
                p.indentposition_inc()
                code()
                p.indentposition_dec()
                p.cwrite("end parallel block\n")
                p.pclose()
                p.cclose()
                //並列化するループ処理を一時ファイルに書き込み
                p.writepcode()
                p.pclose()
                p.rewritecfile()
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
                p.cwrite(p.readpartext())
                p.clearpv()
                p.switch_parmode(false)
              |_ ->
                let p = p.param
                Console.WriteLine("Error : この言語では並列化を実行できません")
                p.codewrite("Error : この言語では並列化を実行できません")

        ///<summary>それぞれ別スレッドで実行</summary>
        static member sections (th:int) = fun code ->
            match p.lang with
              |F ->
                let p = p.param
                p.isOmpUsed <- true
                p.cwrite("parallel block\n")
                p.indentposition_inc()
                code()
                p.indentposition_dec()
                p.cwrite("end parallel block\n")
                p.pclose()
                p.cclose()
                //並列化するループ処理を一時ファイルに書き込み
                p.writepcode()
                p.pclose()
                p.rewritecfile()
                if p.pvlist.Length>0 then
                    let s = String.Join(",", p.pvlist)
                    p.codewrite("!$omp parallel private("+s+") num_threads("+(string th)+")\n")
                else
                    p.codewrite("!$omp parallel num_threads("+(string th)+")\n")
                p.indentposition_inc()
                p.codewrite("!$omp sections"+"\n")
                p.cwrite(p.readpartext())
                p.codewrite("!$omp end sections"+"\n")
                p.indentposition_dec()
                p.codewrite("!$omp end parallel"+"\n")
                p.clearpv()
              |C99 ->
                let p = p.param
                p.isOmpUsed <- true
                p.cwrite("parallel block\n")
                p.indentposition_inc()
                code()
                p.indentposition_dec()
                p.cwrite("end parallel block\n")
                p.pclose()
                p.cclose()
                //並列化するループ処理を一時ファイルに書き込み
                p.writepcode()
                p.pclose()
                p.rewritecfile()
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
