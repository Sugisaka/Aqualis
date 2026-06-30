//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    open System

    ///<summary>OpenMP処理</summary>
    type omp =
        static member temp = 0
        ///<summary>ループを並列化</summary>
        static member parallelize code =
            match (GenerationScope.currentProgram()).language with
            |Fortran ->
                (GenerationScope.requireContext()).IsOpenMpUsed <- true
                (GenerationScope.currentProgram()).mlist.add "omp_lib"
                (GenerationScope.currentProgram()).close()
                (GenerationScope.requireContext()).WithParallelMode(code)
                (GenerationScope.currentProgram()).appendOpen()
                let rec printplist list n counter (s:string) =
                    match list,counter with
                    |[],_ ->
                        if counter>0 && n=0 then
                            writein("!$omp parallel do private("+s+")"+"\n")
                        elif (counter>0) then
                            writein("!$omp private("+s+")"+"\n")
                        else
                            ()
                    |(_,_,pp,_)::lst,7 ->
                        if n=0 then
                            writein("!$omp parallel do private("+s+") &"+"\n")
                        else
                            writein("!$omp private("+s+") &"+"\n")
                        printplist lst 1 1 pp
                    |(_,_,p,_)::lst,_ ->
                        printplist lst n (counter+1) (s+(if counter=0 then "" else ",")+p)
                printplist (GenerationScope.currentProgram()).varPrivate.list 0 0 ""
                writein "!$omp end parallel do\n"
                (GenerationScope.currentProgram()).varPrivate.clear()
            |C99 ->
                (GenerationScope.requireContext()).IsOpenMpUsed <- true
                (GenerationScope.currentProgram()).hlist.add "<omp.h>"
                (GenerationScope.currentProgram()).close()
                (GenerationScope.requireContext()).WithParallelMode(code)
                (GenerationScope.currentProgram()).appendOpen()
                if (GenerationScope.currentProgram()).varPrivate.list.Length > 0 then
                    let s = String.Join(",", List.map (fun (_,_,s,_) -> s) (GenerationScope.currentProgram()).varPrivate.list)
                    writein("#pragma omp parallel for private("+s+")\n")
                else
                    writein "#pragma omp parallel for\n"
                (GenerationScope.currentProgram()).varPrivate.clear()
            |_ ->
                ()

        ///<summary>ループを並列化</summary>
        static member parallelize_th (th:int) = fun code ->
            match (GenerationScope.currentProgram()).language with
            |Fortran ->
                (GenerationScope.requireContext()).IsOpenMpUsed <- true
                (GenerationScope.currentProgram()).mlist.add "omp_lib"
                (GenerationScope.currentProgram()).close()
                (GenerationScope.requireContext()).WithParallelMode(code)
                (GenerationScope.currentProgram()).appendOpen()
                let rec printplist list n counter (s:string) =
                    match list,counter with
                    |[],_ ->
                        if counter>0 && n=0 then
                            writein("!$omp parallel do private("+s+") num_threads("+(string th)+")"+"\n")
                        elif counter>0 then
                            writein("!$omp private("+s+")"+"\n")
                        else
                            ()
                    |(_,_,pp,_)::lst,7 ->
                        if n=0 then
                            writein("!$omp parallel do private("+s+") num_threads("+(string th)+") &"+"\n")
                        else
                            writein("!$omp private("+s+") &"+"\n")
                        printplist lst 1 1 pp
                    |(_,_,p,_)::lst,_ ->
                        printplist lst n (counter+1) (s+(if counter=0 then "" else ",")+p)
                printplist (GenerationScope.currentProgram()).varPrivate.list 0 0 ""
                writein "!$omp end parallel do\n"
                (GenerationScope.currentProgram()).varPrivate.clear()
            |C99 ->
                (GenerationScope.requireContext()).IsOpenMpUsed <- true
                (GenerationScope.currentProgram()).hlist.add "<omp.h>"
                (GenerationScope.currentProgram()).close()
                (GenerationScope.requireContext()).WithParallelMode(code)
                (GenerationScope.currentProgram()).appendOpen()
                if (GenerationScope.currentProgram()).varPrivate.list.Length > 0 then
                    let s = String.Join(",", List.map (fun (_,_,s,_) -> s) (GenerationScope.currentProgram()).varPrivate.list)
                    writein("#pragma omp parallel for private("+s+") num_threads("+(string th)+")\n")
                else
                    writein("#pragma omp parallel for num_threads("+(string th)+")\n")
                (GenerationScope.currentProgram()).varPrivate.clear()
            |_ ->
                ()

        ///<summary>ループを並列化</summary>
        static member reduction (var:num0) (ope:string) = fun code ->
            match (GenerationScope.currentProgram()).language with
            |Fortran ->
                (GenerationScope.requireContext()).IsOpenMpUsed <- true
                (GenerationScope.currentProgram()).mlist.add "omp_lib"
                (GenerationScope.currentProgram()).close()
                (GenerationScope.requireContext()).WithParallelMode(code)
                (GenerationScope.currentProgram()).appendOpen()
                let rec printplist list n counter (s:string) =
                    match list,counter with
                    |[],_ ->
                        if counter>0 && n=0 then
                            writein("!$omp parallel do private("+s+") reduction("+ope+":"+var.code+")"+"\n")
                        elif counter>0 then
                            writein("!$omp private("+s+")"+"\n")
                        else
                            ()
                    |(_,_,pp,_)::lst,7 ->
                        if n=0 then
                            writein("!$omp parallel do private("+s+") reduction("+ope+":"+var.code+") &"+"\n")
                        else
                            writein("!$omp private("+s+") &"+"\n")
                        printplist lst 1 1 pp
                    |(_,_,p,_)::lst,_ ->
                        printplist lst n (counter+1) (s+(if counter=0 then "" else ",")+p)
                printplist (GenerationScope.currentProgram()).varPrivate.list 0 0 ""
                writein "!$omp end parallel do\n"
                (GenerationScope.currentProgram()).varPrivate.clear()
            |C99 ->
                (GenerationScope.requireContext()).IsOpenMpUsed <- true
                (GenerationScope.currentProgram()).hlist.add "<omp.h>"
                (GenerationScope.currentProgram()).close()
                (GenerationScope.requireContext()).WithParallelMode(code)
                (GenerationScope.currentProgram()).appendOpen()
                if (GenerationScope.currentProgram()).varPrivate.list.Length>0 then
                    let s = String.Join(",", List.map (fun (_,_,s,_) -> s) (GenerationScope.currentProgram()).varPrivate.list)
                    match ope with
                    |"+" |"-" |"*" ->
                        writein("#pragma omp parallel for private("+s+") reduction("+ope+":"+var.code+")\n")
                    |_ ->
                        ()
                else
                    match ope with
                    |"+" |"-" |"*" ->
                        writein("#pragma omp parallel for reduction("+ope+":"+var.code+")\n")
                    |_ ->
                        ()
                (GenerationScope.currentProgram()).varPrivate.clear()
            |_ ->
                ()

        ///<summary>ループを並列化</summary>
        static member reduction_th (th:int) (var:num0) (ope:string) = fun code ->
            match (GenerationScope.currentProgram()).language with
            |Fortran ->
                (GenerationScope.requireContext()).IsOpenMpUsed <- true
                (GenerationScope.currentProgram()).mlist.add "omp_lib"
                (GenerationScope.currentProgram()).close()
                (GenerationScope.requireContext()).WithParallelMode(code)
                (GenerationScope.currentProgram()).appendOpen()
                let rec printplist list n counter (s:string) =
                    match list,counter with
                    |[],_ ->
                        if counter>0 && n=0 then
                            writein("!$omp parallel do private("+s+") num_threads("+(string th)+") reduction("+ope+":"+var.code+")"+"\n")
                        elif counter>0 then
                            writein("!$omp private("+s+")"+"\n")
                        else
                            ()
                    |(_,_,pp,_)::lst,7 ->
                        if n=0 then
                            writein("!$omp parallel do private("+s+") num_threads("+(string th)+") reduction("+ope+":"+var.code+") &"+"\n")
                        else
                            writein("!$omp private("+s+") &"+"\n")
                        printplist lst 1 1 pp
                    |(_,_,p,_)::lst,_ ->
                        printplist lst n (counter+1) (s+(if counter=0 then "" else ",")+p)
                printplist (GenerationScope.currentProgram()).varPrivate.list 0 0 ""
                writein "!$omp end parallel do\n"
                (GenerationScope.currentProgram()).varPrivate.clear()
            |C99 ->
                (GenerationScope.requireContext()).IsOpenMpUsed <- true
                (GenerationScope.currentProgram()).hlist.add "<omp.h>"
                (GenerationScope.currentProgram()).close()
                (GenerationScope.requireContext()).WithParallelMode(code)
                (GenerationScope.currentProgram()).appendOpen()
                if (GenerationScope.currentProgram()).varPrivate.list.Length>0 then
                    let s = String.Join(",", List.map (fun (_,_,s,_) -> s) (GenerationScope.currentProgram()).varPrivate.list)
                    match ope with
                    |"+" |"-" |"*" ->
                        writein("#pragma omp parallel for private("+s+") num_threads("+(string th)+") reduction("+ope+":"+var.code+")\n")
                    |_ ->
                        ()
                else
                    match ope with
                    |"+" |"-" |"*" ->
                        writein("#pragma omp parallel for num_threads("+(string th)+") reduction("+ope+":"+var.code+")\n")
                    |_ ->
                        ()
                (GenerationScope.currentProgram()).varPrivate.clear()
            |_ ->
                ()

        ///<summary>それぞれ別スレッドで実行</summary>
        static member sections (th:int) = fun code ->
            match (GenerationScope.currentProgram()).language with
            |Fortran ->
                (GenerationScope.requireContext()).IsOpenMpUsed <- true
                (GenerationScope.currentProgram()).mlist.add "omp_lib"
                (GenerationScope.currentProgram()).close()
                (GenerationScope.requireContext()).WithParallelMode(code)
                (GenerationScope.currentProgram()).appendOpen()
                if (GenerationScope.currentProgram()).varPrivate.list.Length>0 then
                    let s = String.Join(",", List.map (fun (_,_,s,_) -> s) (GenerationScope.currentProgram()).varPrivate.list)
                    writein("!$omp parallel private(" + s + ") num_threads(" + string th + ")\n")
                else
                    writein("!$omp parallel num_threads(" + string th + ")\n")
                (GenerationScope.currentProgram()).indentInc()
                writein("!$omp sections"+"\n")
                (GenerationScope.currentProgram()).indentInc()
                (GenerationScope.currentProgram()).indentDec()
                writein("!$omp end sections"+"\n")
                (GenerationScope.currentProgram()).indentDec()
                writein "!$omp end parallel\n"
                (GenerationScope.currentProgram()).varPrivate.clear()
            |C99 ->
                (GenerationScope.requireContext()).IsOpenMpUsed <- true
                (GenerationScope.currentProgram()).hlist.add "<omp.h>"
                (GenerationScope.currentProgram()).close()
                (GenerationScope.requireContext()).WithParallelMode(code)
                (GenerationScope.currentProgram()).appendOpen()
                if (GenerationScope.currentProgram()).varPrivate.list.Length > 0 then
                    let s = String.Join(",", List.map (fun (_,_,s,_) -> s) (GenerationScope.currentProgram()).varPrivate.list)
                    writein("#pragma omp parallel private(" + s + ") num_threads(" + string th + ")\n")
                else
                    writein("#pragma omp parallel num_threads(" + string th + ")\n")
                writein("{"+"\n")
                (GenerationScope.currentProgram()).indentInc()
                writein("#pragma omp sections"+"\n")
                writein("{"+"\n")
                (GenerationScope.currentProgram()).indentInc()
                (GenerationScope.currentProgram()).indentDec()
                writein("}"+"\n")
                (GenerationScope.currentProgram()).indentDec()
                writein("}"+"\n")
                (GenerationScope.currentProgram()).varPrivate.clear()
            |_ ->
                ()

        ///<summary>別スレッドで実行</summary>
        static member section code =
            match (GenerationScope.currentProgram()).language with
            |Fortran ->
                writein("!$omp section"+"\n")
                (GenerationScope.requireContext()).WithParallelMode(code)
            |C99 ->
                writein("#pragma omp section"+"\n")
                writein("{"+"\n")
                (GenerationScope.requireContext()).WithParallelMode(code)
                writein("}"+"\n")
            |_ ->
                ()

        ///<summary>スレッド番号の取得</summary>
        static member thread_num with get() =
            match (GenerationScope.currentProgram()).language with
            |Fortran |C99 ->
                num0(Var(It 4,"omp_get_thread_num()",NaN))
            |_ ->
                printfn "Error : この言語ではスレッド番号の取得はできません"
                num0 NaN

        ///<summary>最大スレッド数の取得</summary>
        static member max_threads with get() =
            match (GenerationScope.currentProgram()).language with
            |Fortran |C99 ->
                num0(Var(It 4,"omp_get_max_threads()",NaN))
            |_ ->
                printfn "この言語ではスレッド番号の取得はできません"
                num0 NaN
