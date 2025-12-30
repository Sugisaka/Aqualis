namespace Aqualis
    
    open System
    
    ///<summary>OpenMP処理</summary>
    type omp =
        static member temp = 0
        ///<summary>ループを並列化</summary>
        static member parallelize code =
            match pr.language with
            |Fortran ->
                isOmpUsed <- true
                pr.mlist.add "omp_lib"
                isParMode <- true
                pr.close()
                code()
                isParMode <- false
                pr.appendOpen()
                let rec printplist list n counter (s:string) =
                    match list,counter with
                    |[],_ ->
                        if counter>0 && n=0 then
                            pr.codewrite("!$omp parallel do private("+s+")"+"\n")
                        elif (counter>0) then
                            pr.codewrite("!$omp private("+s+")"+"\n")
                        else
                            ()
                    |(_,_,pp,_)::lst,7 ->
                        if n=0 then
                            pr.codewrite("!$omp parallel do private("+s+") &"+"\n")
                        else
                            pr.codewrite("!$omp private("+s+") &"+"\n")
                        printplist lst 1 1 pp
                    |(_,_,p,_)::lst,_ ->
                        printplist lst n (counter+1) (s+(if counter=0 then "" else ",")+p)
                printplist pr.varPrivate.list 0 0 ""
                pr.codewrite "!$omp end parallel do\n"
                pr.varPrivate.clear()
            |C99 ->
                isOmpUsed <- true
                pr.hlist.add "<omp.h>"
                isParMode <- true
                pr.close()
                code()
                isParMode <- false
                pr.appendOpen()
                if pr.varPrivate.list.Length > 0 then
                    let s = String.Join(",", List.map (fun (_,_,s,_) -> s) pr.varPrivate.list)
                    pr.codewrite("#pragma omp parallel for private("+s+")\n")
                else
                    pr.codewrite "#pragma omp parallel for\n"
                pr.varPrivate.clear()
            |_ ->
                ()
                
        ///<summary>ループを並列化</summary>
        static member parallelize_th (th:int) = fun code ->
            match pr.language with
            |Fortran ->
                isOmpUsed <- true
                pr.mlist.add "omp_lib"
                isParMode <- true
                pr.close()
                code()
                isParMode <- false
                pr.appendOpen()
                let rec printplist list n counter (s:string) =
                    match list,counter with
                    |[],_ ->
                        if counter>0 && n=0 then
                            pr.codewrite("!$omp parallel do private("+s+") num_threads("+(string th)+")"+"\n")
                        elif counter>0 then
                            pr.codewrite("!$omp private("+s+")"+"\n")
                        else
                            ()
                    |(_,_,pp,_)::lst,7 ->
                        if n=0 then
                            pr.codewrite("!$omp parallel do private("+s+") num_threads("+(string th)+") &"+"\n")
                        else
                            pr.codewrite("!$omp private("+s+") &"+"\n")
                        printplist lst 1 1 pp
                    |(_,_,p,_)::lst,_ ->
                        printplist lst n (counter+1) (s+(if counter=0 then "" else ",")+p)
                printplist pr.varPrivate.list 0 0 ""
                pr.codewrite "!$omp end parallel do\n"
                pr.varPrivate.clear()
            |C99 ->
                isOmpUsed <- true
                pr.hlist.add "<omp.h>"
                isParMode <- true
                pr.close()
                code()
                isParMode <- false
                pr.appendOpen()
                if pr.varPrivate.list.Length > 0 then
                    let s = String.Join(",", List.map (fun (_,_,s,_) -> s) pr.varPrivate.list)
                    pr.codewrite("#pragma omp parallel for private("+s+") num_threads("+(string th)+")\n")
                else
                    pr.codewrite("#pragma omp parallel for num_threads("+(string th)+")\n")
                pr.varPrivate.clear()
            |_ ->
                ()

        ///<summary>ループを並列化</summary>
        static member reduction (var:num0) (ope:string) = fun code ->
            match pr.language with
            |Fortran ->
                isOmpUsed <- true
                pr.mlist.add "omp_lib"
                isParMode <- true
                pr.close()
                code()
                isParMode <- false
                pr.appendOpen()
                let rec printplist list n counter (s:string) =
                    match list,counter with
                    |[],_ ->
                        if counter>0 && n=0 then
                            pr.codewrite("!$omp parallel do private("+s+") reduction("+ope+":"+var.code+")"+"\n")
                        elif counter>0 then
                            pr.codewrite("!$omp private("+s+")"+"\n")
                        else
                            ()
                    |(_,_,pp,_)::lst,7 ->
                        if n=0 then
                            pr.codewrite("!$omp parallel do private("+s+") reduction("+ope+":"+var.code+") &"+"\n")
                        else
                            pr.codewrite("!$omp private("+s+") &"+"\n")
                        printplist lst 1 1 pp
                    |(_,_,p,_)::lst,_ ->
                        printplist lst n (counter+1) (s+(if counter=0 then "" else ",")+p)
                printplist pr.varPrivate.list 0 0 ""
                pr.codewrite "!$omp end parallel do\n"
                pr.varPrivate.clear()
            |C99 ->
                isOmpUsed <- true
                pr.hlist.add "<omp.h>"
                isParMode <- true
                pr.close()
                code()
                isParMode <- false
                pr.appendOpen()
                if pr.varPrivate.list.Length>0 then
                    let s = String.Join(",", List.map (fun (_,_,s,_) -> s) pr.varPrivate.list)
                    match ope with
                    |"+" |"-" |"*" ->
                        pr.codewrite("#pragma omp parallel for private("+s+") reduction("+ope+":"+var.code+")\n")
                    |_ ->
                        ()
                else
                    match ope with
                    |"+" |"-" |"*" ->
                        pr.codewrite("#pragma omp parallel for reduction("+ope+":"+var.code+")\n")
                    |_ ->
                        ()
                pr.varPrivate.clear()
            |_ ->
                ()

        ///<summary>ループを並列化</summary>
        static member reduction_th (th:int) (var:num0) (ope:string) = fun code ->
            match pr.language with
            |Fortran ->
                isOmpUsed <- true
                pr.mlist.add "omp_lib"
                isParMode <- true
                pr.close()
                code()
                isParMode <- false
                pr.appendOpen()
                let rec printplist list n counter (s:string) =
                    match list,counter with
                    |[],_ ->
                        if counter>0 && n=0 then
                            pr.codewrite("!$omp parallel do private("+s+") num_threads("+(string th)+") reduction("+ope+":"+var.code+")"+"\n")
                        elif counter>0 then
                            pr.codewrite("!$omp private("+s+")"+"\n")
                        else
                            ()
                    |(_,_,pp,_)::lst,7 ->
                        if n=0 then
                            pr.codewrite("!$omp parallel do private("+s+") num_threads("+(string th)+") reduction("+ope+":"+var.code+") &"+"\n")
                        else
                            pr.codewrite("!$omp private("+s+") &"+"\n")
                        printplist lst 1 1 pp
                    |(_,_,p,_)::lst,_ ->
                        printplist lst n (counter+1) (s+(if counter=0 then "" else ",")+p)
                printplist pr.varPrivate.list 0 0 ""
                pr.codewrite "!$omp end parallel do\n"
                pr.varPrivate.clear()
            |C99 ->
                isOmpUsed <- true
                pr.hlist.add "<omp.h>"
                isParMode <- true
                pr.close()
                code()
                isParMode <- false
                pr.appendOpen()
                if pr.varPrivate.list.Length>0 then
                    let s = String.Join(",", List.map (fun (_,_,s,_) -> s) pr.varPrivate.list)
                    match ope with
                    |"+" |"-" |"*" ->
                        pr.codewrite("#pragma omp parallel for private("+s+") num_threads("+(string th)+") reduction("+ope+":"+var.code+")\n")
                    |_ ->
                        ()
                else
                    match ope with
                    |"+" |"-" |"*" ->
                        pr.codewrite("#pragma omp parallel for num_threads("+(string th)+") reduction("+ope+":"+var.code+")\n")
                    |_ ->
                        ()
                pr.varPrivate.clear()
            |_ ->
                ()

        ///<summary>それぞれ別スレッドで実行</summary>
        static member sections (th:int) = fun code ->
            match pr.language with
            |Fortran ->
                isOmpUsed <- true
                pr.mlist.add "omp_lib"
                isParMode <- true
                pr.close()
                code()
                isParMode <- false
                pr.appendOpen()
                if pr.varPrivate.list.Length>0 then
                    let s = String.Join(",", List.map (fun (_,_,s,_) -> s) pr.varPrivate.list)
                    pr.codewrite("!$omp parallel private(" + s + ") num_threads(" + string th + ")\n")
                else
                    pr.codewrite("!$omp parallel num_threads(" + string th + ")\n")
                pr.indentInc()
                pr.codewrite("!$omp sections"+"\n")
                pr.indentInc()
                pr.indentDec()
                pr.codewrite("!$omp end sections"+"\n")
                pr.indentDec()
                pr.codewrite "!$omp end parallel\n"
                pr.varPrivate.clear()
            |C99 ->
                isOmpUsed <- true
                pr.hlist.add "<omp.h>"
                isParMode <- true
                pr.close()
                code()
                isParMode <- false
                pr.appendOpen()
                if pr.varPrivate.list.Length > 0 then
                    let s = String.Join(",", List.map (fun (_,_,s,_) -> s) pr.varPrivate.list)
                    pr.codewrite("#pragma omp parallel private(" + s + ") num_threads(" + string th + ")\n")
                else
                    pr.codewrite("#pragma omp parallel num_threads(" + string th + ")\n")
                pr.codewrite("{"+"\n")
                pr.indentInc()
                pr.codewrite("#pragma omp sections"+"\n")
                pr.codewrite("{"+"\n")
                pr.indentInc()
                pr.indentDec()
                pr.codewrite("}"+"\n")
                pr.indentDec()
                pr.codewrite("}"+"\n")
                pr.varPrivate.clear()
            |_ ->
                ()

        ///<summary>別スレッドで実行</summary>
        static member section code =
            match pr.language with
            |Fortran ->
                pr.codewrite("!$omp section"+"\n")
                code()
            |C99 ->
                pr.codewrite("#pragma omp section"+"\n")
                pr.codewrite("{"+"\n")
                code()
                pr.codewrite("}"+"\n")
            |_ ->
                ()

        ///<summary>スレッド番号の取得</summary>
        static member thread_num with get() =
            match pr.language with
            |Fortran |C99 ->
                num0(Var(It 4,"omp_get_thread_num()",NaN))
            |_ ->
                printfn "Error : この言語ではスレッド番号の取得はできません"
                num0 NaN

        ///<summary>最大スレッド数の取得</summary>
        static member max_threads with get() =
            match pr.language with
            |Fortran |C99 ->
                num0(Var(It 4,"omp_get_max_threads()",NaN))
            |_ ->
                printfn "この言語ではスレッド番号の取得はできません"
                num0 NaN