namespace Aqualis
    
    open System
    
    ///<summary>OpenMP処理</summary>
    type omp =
        static member temp = 0
        ///<summary>ループを並列化</summary>
        static member parallelize code =
            match programList[prIndex].language with
            |Fortran ->
                isOmpUsed <- true
                programList[prIndex].mlist.add "omp_lib"
                isParMode <- true
                programList[prIndex].close()
                code()
                isParMode <- false
                programList[prIndex].appendOpen()
                let rec printplist list n counter (s:string) =
                    match list,counter with
                    |[],_ ->
                        if counter>0 && n=0 then
                            codewrite("!$omp parallel do private("+s+")"+"\n")
                        elif (counter>0) then
                            codewrite("!$omp private("+s+")"+"\n")
                        else
                            ()
                    |(_,_,pp,_)::lst,7 ->
                        if n=0 then
                            codewrite("!$omp parallel do private("+s+") &"+"\n")
                        else
                            codewrite("!$omp private("+s+") &"+"\n")
                        printplist lst 1 1 pp
                    |(_,_,p,_)::lst,_ ->
                        printplist lst n (counter+1) (s+(if counter=0 then "" else ",")+p)
                printplist programList[prIndex].varPrivate.list 0 0 ""
                codewrite "!$omp end parallel do\n"
                programList[prIndex].varPrivate.clear()
            |C99 ->
                isOmpUsed <- true
                programList[prIndex].hlist.add "<omp.h>"
                isParMode <- true
                programList[prIndex].close()
                code()
                isParMode <- false
                programList[prIndex].appendOpen()
                if programList[prIndex].varPrivate.list.Length > 0 then
                    let s = String.Join(",", List.map (fun (_,_,s,_) -> s) programList[prIndex].varPrivate.list)
                    codewrite("#pragma omp parallel for private("+s+")\n")
                else
                    codewrite "#pragma omp parallel for\n"
                programList[prIndex].varPrivate.clear()
            |_ ->
                ()
                
        ///<summary>ループを並列化</summary>
        static member parallelize_th (th:int) = fun code ->
            match programList[prIndex].language with
            |Fortran ->
                isOmpUsed <- true
                programList[prIndex].mlist.add "omp_lib"
                isParMode <- true
                programList[prIndex].close()
                code()
                isParMode <- false
                programList[prIndex].appendOpen()
                let rec printplist list n counter (s:string) =
                    match list,counter with
                    |[],_ ->
                        if counter>0 && n=0 then
                            codewrite("!$omp parallel do private("+s+") num_threads("+(string th)+")"+"\n")
                        elif counter>0 then
                            codewrite("!$omp private("+s+")"+"\n")
                        else
                            ()
                    |(_,_,pp,_)::lst,7 ->
                        if n=0 then
                            codewrite("!$omp parallel do private("+s+") num_threads("+(string th)+") &"+"\n")
                        else
                            codewrite("!$omp private("+s+") &"+"\n")
                        printplist lst 1 1 pp
                    |(_,_,p,_)::lst,_ ->
                        printplist lst n (counter+1) (s+(if counter=0 then "" else ",")+p)
                printplist programList[prIndex].varPrivate.list 0 0 ""
                codewrite "!$omp end parallel do\n"
                programList[prIndex].varPrivate.clear()
            |C99 ->
                isOmpUsed <- true
                programList[prIndex].hlist.add "<omp.h>"
                isParMode <- true
                programList[prIndex].close()
                code()
                isParMode <- false
                programList[prIndex].appendOpen()
                if programList[prIndex].varPrivate.list.Length > 0 then
                    let s = String.Join(",", List.map (fun (_,_,s,_) -> s) programList[prIndex].varPrivate.list)
                    codewrite("#pragma omp parallel for private("+s+") num_threads("+(string th)+")\n")
                else
                    codewrite("#pragma omp parallel for num_threads("+(string th)+")\n")
                programList[prIndex].varPrivate.clear()
            |_ ->
                ()

        ///<summary>ループを並列化</summary>
        static member reduction (var:num0) (ope:string) = fun code ->
            match programList[prIndex].language with
            |Fortran ->
                isOmpUsed <- true
                programList[prIndex].mlist.add "omp_lib"
                isParMode <- true
                programList[prIndex].close()
                code()
                isParMode <- false
                programList[prIndex].appendOpen()
                let rec printplist list n counter (s:string) =
                    match list,counter with
                    |[],_ ->
                        if counter>0 && n=0 then
                            codewrite("!$omp parallel do private("+s+") reduction("+ope+":"+var.code+")"+"\n")
                        elif counter>0 then
                            codewrite("!$omp private("+s+")"+"\n")
                        else
                            ()
                    |(_,_,pp,_)::lst,7 ->
                        if n=0 then
                            codewrite("!$omp parallel do private("+s+") reduction("+ope+":"+var.code+") &"+"\n")
                        else
                            codewrite("!$omp private("+s+") &"+"\n")
                        printplist lst 1 1 pp
                    |(_,_,p,_)::lst,_ ->
                        printplist lst n (counter+1) (s+(if counter=0 then "" else ",")+p)
                printplist programList[prIndex].varPrivate.list 0 0 ""
                codewrite "!$omp end parallel do\n"
                programList[prIndex].varPrivate.clear()
            |C99 ->
                isOmpUsed <- true
                programList[prIndex].hlist.add "<omp.h>"
                isParMode <- true
                programList[prIndex].close()
                code()
                isParMode <- false
                programList[prIndex].appendOpen()
                if programList[prIndex].varPrivate.list.Length>0 then
                    let s = String.Join(",", List.map (fun (_,_,s,_) -> s) programList[prIndex].varPrivate.list)
                    match ope with
                    |"+" |"-" |"*" ->
                        codewrite("#pragma omp parallel for private("+s+") reduction("+ope+":"+var.code+")\n")
                    |_ ->
                        ()
                else
                    match ope with
                    |"+" |"-" |"*" ->
                        codewrite("#pragma omp parallel for reduction("+ope+":"+var.code+")\n")
                    |_ ->
                        ()
                programList[prIndex].varPrivate.clear()
            |_ ->
                ()

        ///<summary>ループを並列化</summary>
        static member reduction_th (th:int) (var:num0) (ope:string) = fun code ->
            match programList[prIndex].language with
            |Fortran ->
                isOmpUsed <- true
                programList[prIndex].mlist.add "omp_lib"
                isParMode <- true
                programList[prIndex].close()
                code()
                isParMode <- false
                programList[prIndex].appendOpen()
                let rec printplist list n counter (s:string) =
                    match list,counter with
                    |[],_ ->
                        if counter>0 && n=0 then
                            codewrite("!$omp parallel do private("+s+") num_threads("+(string th)+") reduction("+ope+":"+var.code+")"+"\n")
                        elif counter>0 then
                            codewrite("!$omp private("+s+")"+"\n")
                        else
                            ()
                    |(_,_,pp,_)::lst,7 ->
                        if n=0 then
                            codewrite("!$omp parallel do private("+s+") num_threads("+(string th)+") reduction("+ope+":"+var.code+") &"+"\n")
                        else
                            codewrite("!$omp private("+s+") &"+"\n")
                        printplist lst 1 1 pp
                    |(_,_,p,_)::lst,_ ->
                        printplist lst n (counter+1) (s+(if counter=0 then "" else ",")+p)
                printplist programList[prIndex].varPrivate.list 0 0 ""
                codewrite "!$omp end parallel do\n"
                programList[prIndex].varPrivate.clear()
            |C99 ->
                isOmpUsed <- true
                programList[prIndex].hlist.add "<omp.h>"
                isParMode <- true
                programList[prIndex].close()
                code()
                isParMode <- false
                programList[prIndex].appendOpen()
                if programList[prIndex].varPrivate.list.Length>0 then
                    let s = String.Join(",", List.map (fun (_,_,s,_) -> s) programList[prIndex].varPrivate.list)
                    match ope with
                    |"+" |"-" |"*" ->
                        codewrite("#pragma omp parallel for private("+s+") num_threads("+(string th)+") reduction("+ope+":"+var.code+")\n")
                    |_ ->
                        ()
                else
                    match ope with
                    |"+" |"-" |"*" ->
                        codewrite("#pragma omp parallel for num_threads("+(string th)+") reduction("+ope+":"+var.code+")\n")
                    |_ ->
                        ()
                programList[prIndex].varPrivate.clear()
            |_ ->
                ()

        ///<summary>それぞれ別スレッドで実行</summary>
        static member sections (th:int) = fun code ->
            match programList[prIndex].language with
            |Fortran ->
                isOmpUsed <- true
                programList[prIndex].mlist.add "omp_lib"
                isParMode <- true
                programList[prIndex].close()
                code()
                isParMode <- false
                programList[prIndex].appendOpen()
                if programList[prIndex].varPrivate.list.Length>0 then
                    let s = String.Join(",", List.map (fun (_,_,s,_) -> s) programList[prIndex].varPrivate.list)
                    codewrite("!$omp parallel private(" + s + ") num_threads(" + string th + ")\n")
                else
                    codewrite("!$omp parallel num_threads(" + string th + ")\n")
                programList[prIndex].indentInc()
                codewrite("!$omp sections"+"\n")
                programList[prIndex].indentInc()
                programList[prIndex].indentDec()
                codewrite("!$omp end sections"+"\n")
                programList[prIndex].indentDec()
                codewrite "!$omp end parallel\n"
                programList[prIndex].varPrivate.clear()
            |C99 ->
                isOmpUsed <- true
                programList[prIndex].hlist.add "<omp.h>"
                isParMode <- true
                programList[prIndex].close()
                code()
                isParMode <- false
                programList[prIndex].appendOpen()
                if programList[prIndex].varPrivate.list.Length > 0 then
                    let s = String.Join(",", List.map (fun (_,_,s,_) -> s) programList[prIndex].varPrivate.list)
                    codewrite("#pragma omp parallel private(" + s + ") num_threads(" + string th + ")\n")
                else
                    codewrite("#pragma omp parallel num_threads(" + string th + ")\n")
                codewrite("{"+"\n")
                programList[prIndex].indentInc()
                codewrite("#pragma omp sections"+"\n")
                codewrite("{"+"\n")
                programList[prIndex].indentInc()
                programList[prIndex].indentDec()
                codewrite("}"+"\n")
                programList[prIndex].indentDec()
                codewrite("}"+"\n")
                programList[prIndex].varPrivate.clear()
            |_ ->
                ()

        ///<summary>別スレッドで実行</summary>
        static member section code =
            match programList[prIndex].language with
            |Fortran ->
                codewrite("!$omp section"+"\n")
                code()
            |C99 ->
                codewrite("#pragma omp section"+"\n")
                codewrite("{"+"\n")
                code()
                codewrite("}"+"\n")
            |_ ->
                ()

        ///<summary>スレッド番号の取得</summary>
        static member thread_num with get() =
            match programList[prIndex].language with
            |Fortran |C99 ->
                num0(Var(It 4,"omp_get_thread_num()",NaN))
            |_ ->
                printfn "Error : この言語ではスレッド番号の取得はできません"
                num0 NaN

        ///<summary>最大スレッド数の取得</summary>
        static member max_threads with get() =
            match programList[prIndex].language with
            |Fortran |C99 ->
                num0(Var(It 4,"omp_get_max_threads()",NaN))
            |_ ->
                printfn "この言語ではスレッド番号の取得はできません"
                num0 NaN