namespace Aqualis
    
    open System
    
    ///<summary>ファイル入出力</summary>
    type io () =
        
        static member private fileAccess (filename:exprString) readmode isbinary code =
            match pr.language with
            |Fortran ->
                ch.f <| fun fp ->
                    let f = 
                        match filename with
                        |Str _ ->
                            "A"
                        |Nvr x when x.etype = It 4 ->
                            "I" + pr.numFormat.iFormat.ToString()
                        |Nvr _ ->
                            printfn "ファイル名に指定できない型の変数が入っています"
                            ""
                        |NSL x -> 
                            x 
                            |> List.map (fun s -> 
                                match s with
                                |Str _ -> 
                                    "A"
                                |Nvr x when x.etype = It 4 -> 
                                    "I" + pr.numFormat.iFormat.ToString()
                                |_ -> 
                                    "")
                            |> fun s -> String.Join(",",s)
                    let s = 
                        match filename with
                        |Str t ->
                            "\""+t+"\""
                        |Nvr x when x.etype = It 4 ->
                            x.eval pr
                        |Nvr _ ->
                            printfn "ファイル名に指定できない型の変数が入っています"
                            ""
                        |NSL x -> 
                            x 
                            |> List.map (fun s -> 
                                match s with 
                                |Str t -> 
                                    "\""+t+"\""
                                |Nvr x when x.etype = It 4 -> 
                                    x.eval pr
                                |_ -> 
                                    "")
                            |> fun s -> String.Join(",",s)
                    ch.t <| A0 <| fun id ->
                        let btname = "byte_tmp"
                        //変数byte_tmpをリストに追加（存在していない場合のみ）
                        pr.var.setUniqVar(Structure "integer(1)",A0,btname,"")
                        pr.cwriter.codewrite("write("+id+",\"("+f+")\") "+s+"\n")
                        ch.i <| fun counter ->
                            let c = counter.Expr.eval pr
                            pr.cwriter.codewrite("do "+c+" = 1, len_trim("+id+")"+"\n")
                            pr.cwriter.codewrite("  if ( "+id+"( "+c+":"+c+" ).EQ.\" \" ) "+id+"( "+c+":"+c+" ) = \"0\""+"\n")
                            pr.cwriter.codewrite("end do"+"\n")
                        if isbinary then
                            pr.cwriter.codewrite("open("+fp+", file=trim("+id+"), access='stream', form='unformatted')"+"\n")
                        else
                            pr.cwriter.codewrite("open("+fp+", file=trim("+id+"))"+"\n")
                        code fp
                        pr.cwriter.codewrite("close("+fp+")"+"\n")
            |C99 ->
                ch.f <| fun fp ->
                    let f = 
                        match filename with
                        |Str t ->
                            t
                        |Nvr x when x.etype = It 4 ->
                            "%" + pr.numFormat.iFormat.ToString "00" + "d"
                        |Nvr _ ->
                            printfn "ファイル名に指定できない型の変数が入っています"
                            ""
                        |NSL x -> 
                            x 
                            |> List.map (fun s -> 
                                match s with
                                |Str t ->
                                    t
                                |Nvr x when x.etype = It 4 ->
                                    "%" + pr.numFormat.iFormat.ToString "00" + "d"
                                |_ ->
                                    "")
                            |> List.filter (fun s -> s<>"")
                            |> fun s -> String.Join("",s)
                    let s = 
                        match filename with
                        |Str t ->
                            ""
                        |Nvr x when x.etype = It 4 ->
                            x.eval pr
                        |Nvr _ ->
                            printfn "ファイル名に指定できない型の変数が入っています"
                            ""
                        |NSL x -> 
                            x 
                            |> List.map (fun s -> 
                                match s with
                                |Str _ ->
                                    ""
                                |Nvr x when x.etype = It 4 ->
                                    x.eval pr
                                |_ ->
                                    "")
                            |> List.filter (fun s -> s<>"")
                            |> fun s -> String.Join(",",s)
                    ch.t <| A0 <| fun id ->
                        let btname = "byte_tmp"
                        //変数byte_tmpをリストに追加（存在していない場合のみ）
                        pr.var.setUniqVar(Structure "char",A0,btname,"")
                        pr.cwriter.codewrite("sprintf("+id+",\""+f+"\""+(if s="" then "" else ",")+s+");\n")
                        if isbinary then
                            pr.cwriter.codewrite(fp+" = "+"fopen("+id+",\""+(if readmode then "rb" else "wb")+"\");"+"\n")
                        else
                            pr.cwriter.codewrite(fp+" = "+"fopen("+id+",\""+(if readmode then "r" else "w")+"\");"+"\n")
                        code fp
                        pr.cwriter.codewrite("fclose("+fp+")"+";\n")
            |LaTeX ->
                ch.f <| fun fp ->
                    let s = 
                        match filename with
                        |Str t ->
                            "\""+t+"\""
                        |Nvr x when x.etype = It 4 ->
                            x.eval pr
                        |Nvr _ ->
                            printfn "ファイル名に指定できない型の変数が入っています"
                            ""
                        |NSL x -> 
                            x 
                            |> List.map (fun s -> 
                                match s with
                                |Str t ->
                                    "\""+t+"\""
                                |Nvr x when x.etype = It 4 ->
                                    x.eval pr
                                |_ ->
                                    "")
                            |> List.filter (fun s -> s<>"")
                            |> fun s -> String.Join("+",s)
                    ch.t <| A0 <| fun id ->
                        let btname = "byte_tmp"
                        if isbinary then
                            pr.cwriter.codewrite(fp+" = "+"open binary file("+id+",\""+(if readmode then "rb" else "wb")+"\");"+"\n")
                        else
                            pr.cwriter.codewrite(fp+" = "+"open text file("+id+",\""+(if readmode then "r" else "w")+"\");"+"\n")
                        code fp
                        pr.cwriter.codewrite("close("+fp+")"+";\n")
            |HTML ->
                ch.f <| fun fp ->
                    let s = 
                        match filename with
                        |Str t ->
                            "\""+t+"\""
                        |Nvr x when x.etype = It 4 ->
                            x.eval pr
                        |Nvr _ ->
                            printfn "ファイル名に指定できない型の変数が入っています"
                            ""
                        |NSL x -> 
                            x 
                            |> List.map (fun s -> 
                                match s with
                                |Str t ->
                                    "\""+t+"\""
                                |Nvr x when x.etype = It 4 ->
                                    x.eval pr
                                |_ ->
                                    "")
                            |> List.filter (fun s -> s<>"")
                            |> fun s -> String.Join("+",s)
                    ch.t <| A0 <| fun id ->
                        let btname = "byte_tmp"
                        if isbinary then
                            pr.cwriter.codewrite(fp+" = "+"open binary file("+id+",\""+(if readmode then "rb" else "wb")+"\");"+"\n")
                        else
                            pr.cwriter.codewrite(fp+" = "+"open text file("+id+",\""+(if readmode then "r" else "w")+"\");"+"\n")
                        code fp
                        pr.cwriter.codewrite("close("+fp+")"+";\n")
            |Python ->
                ch.f <| fun fp ->
                    let f = 
                        match filename with
                        |Str t ->
                            t
                        |Nvr x when x.etype = It 4 ->
                            "%" + pr.numFormat.iFormat.ToString "00" + "d"
                        |Nvr _ ->
                            printfn "ファイル名に指定できない型の変数が入っています"
                            ""
                        |NSL x -> 
                            x 
                            |> List.map (fun s -> 
                                match s with
                                |Str t ->
                                    t
                                |Nvr x when x.etype = It 4 ->
                                    "%" + pr.numFormat.iFormat.ToString "00" + "d"
                                |_ ->
                                    "")
                            |> List.filter (fun s -> s<>"")
                            |> fun s -> String.Join("",s)
                    let s = 
                        match filename with
                        |Str t ->
                            ""
                        |Nvr x when x.etype = It 4 ->
                            x.eval pr
                        |Nvr _ ->
                            printfn "ファイル名に指定できない型の変数が入っています"
                            ""
                        |NSL x -> 
                            x 
                            |> List.map (fun s ->
                                match s with
                                |Str _ ->
                                    ""
                                |Nvr x when x.etype = It 4 ->
                                    x.eval pr
                                |_ ->
                                    "")
                            |> List.filter (fun s -> s<>"")
                            |> fun s -> String.Join(",",s)
                    ch.t <| A0 <| fun id ->
                        let btname = "byte_tmp"
                        //変数byte_tmpをリストに追加（存在していない場合のみ）
                        pr.var.setUniqVar(Structure "char",A0,btname,"")
                        pr.cwriter.codewrite(id+"= \""+f+"\"%("+s+")\n")
                        if isbinary then
                            pr.cwriter.codewrite(fp+" = "+"open("+id+",mode=\""+(if readmode then "rb" else "wb")+"\")"+"\n")
                        else
                            pr.cwriter.codewrite(fp+" = "+"open("+id+",mode=\""+(if readmode then "r" else "w")+"\")"+"\n")
                        code(fp)
                        pr.cwriter.codewrite(fp+".close()"+"\n")
            |_ -> ()
            
        static member private Write1 (fp:string) (lst:num0 list) =
            match pr.language with
            |Fortran ->
                let tab = var.ip0_noWarning("tab",2313)
                let double0string_format_F = 
                    let a,b = pr.numFormat.dFormat
                    "E"+a.ToString()+"."+b.ToString()+"e3"
                let format = 
                    lst
                    |> (fun b ->
                        [for n in 0..(b.Length-1) do
                            match b.[n].etype with
                            |It _ -> 
                                yield "I"+pr.numFormat.iFormat.ToString()
                            |Dt ->
                                yield double0string_format_F
                            |Zt ->
                                yield double0string_format_F
                                yield double0string_format_F 
                            |_ -> ()
                        ])
                    |> (fun b ->
                          [for n in 0..(b.Length-1) do
                              yield b.[n]
                              if n<(b.Length-1) then yield "A1"
                          ])
                    |> fun s -> String.Join(",",s)
                let code =
                    lst
                    |> (fun b ->
                        [for n in 0..(b.Length-1) do
                            match b.[n].etype,b[n].Expr with 
                            |It _,Int v -> yield pr.numFormat.ItoS(v)
                            |Dt  ,Int v -> yield pr.numFormat.DtoS(double v)
                            |_,Dbl v -> yield pr.numFormat.DtoS v
                            |Zt,_ ->
                                yield b[n].re.Expr.eval pr
                                yield b[n].im.Expr.eval pr
                            |(It _|Dt),_ -> yield b.[n].Expr.eval pr
                            |_ -> ()])
                    |> (fun b ->
                          [for n in 0..(b.Length-1) do
                              yield b.[n]
                              if n<(b.Length-1) then yield tab.Expr.eval pr
                          ])
                    |> fun s -> String.Join(",",s)
                pr.cwriter.codewrite("write("+fp+",\"("+format+")\") "+code+"\n")
            |C99 ->
                let int0string_format_C =
                    "%"+pr.numFormat.iFormat.ToString()+"d"
                let double0string_format_C = 
                    let a,b = pr.numFormat.dFormat
                    "%"+a.ToString()+"."+b.ToString()+"e"
                let format = 
                    lst
                    |> (fun b -> 
                        [for n in 0..(b.Length-1) do
                            match b.[n],b.[n].etype with
                            |_,It _ ->
                                yield int0string_format_C
                            |_,Dt ->
                                yield double0string_format_C
                            |_,Zt ->
                                yield double0string_format_C
                                yield double0string_format_C
                            |_ -> ()
                        ])
                    |> (fun b ->
                          [for n in 0..(b.Length-1) do
                              yield b.[n]
                              if n<(b.Length-1) then yield "\\t"
                          ])
                    |> fun s -> String.Join("",s)
                let code =
                    [for b in lst do
                        match b.etype,b.Expr with 
                        |_,Int v -> yield pr.numFormat.ItoS v
                        |_,Dbl v -> yield pr.numFormat.DtoS v
                        |Zt,_ ->
                            yield b.re.Expr.eval pr
                            yield b.im.Expr.eval pr
                        |(It _|Dt),_ -> yield b.Expr.eval pr
                        |_ -> ()]
                    |> (fun s -> String.Join(",",s))
                pr.cwriter.codewrite("fprintf("+fp+",\""+format+"\\n\""+(if code ="" then "" else ",")+code+");\n")
            |LaTeX ->
                let double0string_format_F = 
                    let a,b = pr.numFormat.dFormat
                    "E"+a.ToString()+"."+b.ToString()+"e3"
                let format = 
                    lst
                    |> List.map (fun b -> 
                        match b.etype with
                          |It _ ->"I"+pr.numFormat.iFormat.ToString()
                          |Dt -> double0string_format_F
                          |Zt -> double0string_format_F+","+double0string_format_F 
                          |_ -> "")
                    |> (fun s -> String.Join("",s))
                let code =
                    lst
                    |> List.map (fun b ->
                        match b.etype,b.Expr with 
                          |_,Int v -> pr.numFormat.ItoS v
                          |_,Dbl v -> pr.numFormat.DtoS v
                          |Zt,_ -> b.re.Expr.eval pr+","+b.im.Expr.eval pr
                          |(It _|Dt),_ -> b.Expr.eval pr
                          |_ -> "")
                    |> fun s -> String.Join(",",s)
                pr.cwriter.codewrite("write("+fp+",\"("+format+")\") "+code+"\n")
            |HTML ->
                let double0string_format_F = 
                    let a,b = pr.numFormat.dFormat
                    "E"+a.ToString()+"."+b.ToString()+"e3"
                let format = 
                    lst
                    |> List.map (fun b -> 
                        match b.etype with
                          |It _ ->"I"+pr.numFormat.iFormat.ToString()
                          |Dt -> double0string_format_F
                          |Zt -> double0string_format_F+","+double0string_format_F 
                          |_ -> "")
                    |> fun s -> String.Join("",s)
                let code =
                    lst
                    |> List.map (fun b ->
                        match b.etype,b.Expr with 
                          |_,Int v -> pr.numFormat.ItoS(v)
                          |_,Dbl v -> pr.numFormat.DtoS(v)
                          |Zt,_ -> b.re.Expr.eval pr+","+b.im.Expr.eval pr
                          |(It _ |Dt),_ -> b.Expr.eval pr
                          |_ -> "")
                    |> (fun s -> String.Join(",",s))
                pr.cwriter.codewrite("Write(text): \\("+fp+" \\leftarrow "+code+"\\)<br/>")
            |Python ->
                let int0string_format_C =
                    "%"+pr.numFormat.iFormat.ToString()+"d"
                let double0string_format_C = 
                    let a,b = pr.numFormat.dFormat
                    "%"+a.ToString()+"."+b.ToString()+"e"
                let format = 
                    lst
                    |> (fun b -> 
                        [for n in 0..(b.Length-1) do
                            match b.[n],b.[n].etype with
                            |_,It _ ->
                                yield int0string_format_C
                            |_,Dt ->
                                yield double0string_format_C
                            |_,Zt ->
                                yield double0string_format_C
                                yield double0string_format_C
                            |_ -> ()
                        ])
                    |> (fun b ->
                          [for n in 0..(b.Length-1) do
                              yield b.[n]
                              if n<(b.Length-1) then yield "\\t"
                          ])
                    |> (fun s -> String.Join("",s))
                let code =
                    [for b in lst do
                        match b.etype,b.Expr with 
                        |_,Int v -> yield pr.numFormat.ItoS(v)
                        |_,Dbl v -> yield pr.numFormat.DtoS(v)
                        |Zt,_ ->
                            yield b.re.Expr.eval pr
                            yield b.im.Expr.eval pr
                        |(It _|Dt),_ -> yield b.Expr.eval pr
                        |_ -> ()]
                    |> (fun s -> String.Join(",",s))
                pr.cwriter.codewrite(fp+".write(\""+format+"\\n\" %("+code+"))\n")
            |_ -> ()
            
        static member private Write2 (fp:string) (lst:exprString) =
            let lst = reduceExprString.reduce lst
            match pr.language with
            |Fortran ->
                let tab = var.ip0_noWarning("tab",2313)
                let double0string_format_F = 
                    let a,b = pr.numFormat.dFormat
                    "E"+a.ToString()+"."+b.ToString()+"e3"
                let format = 
                    lst
                    |> (fun b ->
                        [for n in 0..(b.Length-1) do
                            match b[n].etype with
                            |It _ -> 
                                yield "I"+pr.numFormat.iFormat.ToString()
                            |Dt ->
                                yield double0string_format_F
                            |Zt ->
                                yield double0string_format_F
                                yield double0string_format_F 
                            |_ -> ()
                        ])
                    |> fun s -> String.Join(",",s)
                let code =
                    lst
                    |> (fun b ->
                        [for n in 0..(b.Length-1) do
                            match b[n].etype,b[n] with 
                            |It _,RNvr(Int v) -> yield pr.numFormat.ItoS(v)
                            |Dt  ,RNvr(Int v) -> yield pr.numFormat.DtoS(double v)
                            |_   ,RNvr(Dbl v) -> yield pr.numFormat.DtoS v
                            |Zt  ,RNvr v ->
                                yield (Re v).eval pr
                                yield (Im v).eval pr
                            |(It _|Dt),RNvr v -> yield v.eval pr
                            |_ -> ()])
                    |> fun s -> String.Join(",",s)
                pr.cwriter.codewrite("write("+fp+",\"("+format+")\") "+code+"\n")
            |C99 ->
                let int0string_format_C =
                    "%"+pr.numFormat.iFormat.ToString()+"d"
                let double0string_format_C = 
                    let a,b = pr.numFormat.dFormat
                    "%"+a.ToString()+"."+b.ToString()+"e"
                let format = 
                    lst
                    |> (fun b -> 
                        [for n in 0..(b.Length-1) do
                            match b[n],b[n].etype with
                            |_,It _ ->
                                yield int0string_format_C
                            |_,Dt ->
                                yield double0string_format_C
                            |_,Zt ->
                                yield double0string_format_C
                                yield double0string_format_C
                            |_ -> ()
                        ])
                    |> fun s -> String.Join("",s)
                let code =
                    [for b in lst do
                        match b.etype,b with 
                        |It _,RNvr(Int v) ->
                            yield pr.numFormat.ItoS v
                        |Dt ,RNvr(Int v) ->
                            yield pr.numFormat.DtoS (double v)
                        |_ ,RNvr(Dbl v) ->
                            yield pr.numFormat.DtoS v
                        |Zt ,RNvr v ->
                            yield (Re v).eval pr
                            yield (Im v).eval pr
                        |(It _|Dt),RNvr v ->
                            yield v.eval pr
                        |_ -> ()]
                    |> (fun s -> String.Join(",",s))
                pr.cwriter.codewrite("fprintf("+fp+",\""+format+"\\n\""+(if code ="" then "" else ",")+code+");\n")
            |LaTeX ->
                let double0string_format_F = 
                    let a,b = pr.numFormat.dFormat
                    "E"+a.ToString()+"."+b.ToString()+"e3"
                let format = 
                    lst
                    |> List.map (fun b -> 
                        match b.etype with
                          |It _ ->"I"+pr.numFormat.iFormat.ToString()
                          |Dt -> double0string_format_F
                          |Zt -> double0string_format_F+","+double0string_format_F 
                          |_ -> "")
                    |> (fun s -> String.Join("",s))
                let code =
                    lst
                    |> List.map (fun b ->
                        match b.etype,b with 
                          |It _,RNvr(Int v) -> pr.numFormat.ItoS v
                          |Dt,RNvr(Int v) -> pr.numFormat.DtoS (double v)
                          |_,RNvr(Dbl v) -> pr.numFormat.DtoS v
                          |Zt,RNvr v -> (Re v).eval pr+","+(Im v).eval pr
                          |(It _|Dt),RNvr v -> v.eval pr
                          |_ -> "")
                    |> fun s -> String.Join(",",s)
                pr.cwriter.codewrite("write("+fp+",\"("+format+")\") "+code+"\n")
            |HTML ->
                let double0string_format_F = 
                    let a,b = pr.numFormat.dFormat
                    "E"+a.ToString()+"."+b.ToString()+"e3"
                let format = 
                    lst
                    |> List.map (fun b -> 
                        match b.etype with
                          |It _ ->"I"+pr.numFormat.iFormat.ToString()
                          |Dt -> double0string_format_F
                          |Zt -> double0string_format_F+","+double0string_format_F 
                          |_ -> "")
                    |> fun s -> String.Join("",s)
                let code =
                    lst
                    |> List.map (fun b ->
                        match b.etype,b with 
                          |It _,RNvr(Int v) -> pr.numFormat.ItoS v
                          |Dt,RNvr(Int v) -> pr.numFormat.DtoS(double v)
                          |_,RNvr(Dbl v) -> pr.numFormat.DtoS v
                          |Zt,RNvr v -> (Re v).eval pr+","+(Im v).eval pr
                          |(It _ |Dt),RNvr v -> v.eval pr
                          |_ -> "")
                    |> fun s -> String.Join(",",s)
                pr.cwriter.codewrite("Write(text): \\("+fp+" \\leftarrow "+code+"\\)<br/>")
            |Python ->
                let int0string_format_C =
                    "%"+pr.numFormat.iFormat.ToString()+"d"
                let double0string_format_C = 
                    let a,b = pr.numFormat.dFormat
                    "%"+a.ToString()+"."+b.ToString()+"e"
                let format = 
                    lst
                    |> (fun b -> 
                        [for n in 0..(b.Length-1) do
                            match b.[n],b.[n].etype with
                            |_,It _ ->
                                yield int0string_format_C
                            |_,Dt ->
                                yield double0string_format_C
                            |_,Zt ->
                                yield double0string_format_C
                                yield double0string_format_C
                            |_ -> ()
                        ])
                    |> (fun b ->
                          [for n in 0..(b.Length-1) do
                              yield b.[n]
                              if n<(b.Length-1) then yield "\\t"
                          ])
                    |> (fun s -> String.Join("",s))
                let code =
                    [for b in lst do
                        match b.etype,b with 
                        |It _,RNvr(Int v) -> yield pr.numFormat.ItoS v
                        |Dt,RNvr(Int v) -> yield pr.numFormat.DtoS(double v)
                        |_,RNvr(Dbl v) -> yield pr.numFormat.DtoS v
                        |Zt,RNvr v ->
                            yield (Re v).eval pr
                            yield (Im v).eval pr
                        |(It _|Dt),RNvr v -> yield v.eval pr
                        |_ -> ()]
                    |> fun s -> String.Join(",",s)
                pr.cwriter.codewrite(fp+".write(\""+format+"\\n\" %("+code+"))\n")
            |_ -> ()
            
        static member private Write_bin (fp:string) (v:num0) =
            match pr.language with
            |Fortran ->
                match v.etype,v.Expr with 
                |_,Int(v) ->
                    pr.cwriter.codewrite("write("+fp+") "+pr.numFormat.ItoS(v)+"\n")
                |_,Dbl(v) ->
                    pr.cwriter.codewrite("write("+fp+") "+pr.numFormat.DtoS(v)+"\n")
                |Zt,_ ->
                    pr.cwriter.codewrite("write("+fp+") "+v.re.Expr.eval pr+"\n")
                    pr.cwriter.codewrite("write("+fp+") "+v.im.Expr.eval pr+"\n")
                |It _,_ ->
                    pr.cwriter.codewrite("write("+fp+") "+v.Expr.eval pr+"\n")
                |Dt,_ ->
                    pr.cwriter.codewrite("write("+fp+") "+v.Expr.eval pr+"\n")
                |_ -> ()
            |C99 ->
                match v.etype,v.Expr with 
                |_,Int _ ->
                    ch.i <| fun tmp ->
                        tmp <== v
                        pr.cwriter.codewrite("fwrite(&"+tmp.Expr.eval pr+",sizeof("+tmp.Expr.eval pr+"),1,"+fp+");\n")
                |_,Dbl _ ->
                    ch.i <| fun tmp ->
                        tmp <== v
                        pr.cwriter.codewrite("fwrite(&"+tmp.Expr.eval pr+",sizeof("+tmp.Expr.eval pr+"),1,"+fp+");\n")
                |Zt,_ ->
                    ch.dd <| fun (tmp_r,tmp_i) ->
                        tmp_r <== v.re
                        tmp_i <== v.im
                        pr.cwriter.codewrite("fwrite(&"+tmp_r.Expr.eval pr+",sizeof("+tmp_r.Expr.eval pr+"),1,"+fp+");\n")
                        pr.cwriter.codewrite("fwrite(&"+tmp_i.Expr.eval pr+",sizeof("+tmp_i.Expr.eval pr+"),1,"+fp+");\n")
                |It _,_ ->
                    ch.i <| fun tmp ->
                        tmp <== v
                        pr.cwriter.codewrite("fwrite(&"+tmp.Expr.eval pr+",sizeof("+tmp.Expr.eval pr+"),1,"+fp+");\n")
                |Dt,_ ->
                    ch.d <| fun tmp ->
                        tmp <== v
                        pr.cwriter.codewrite("fwrite(&"+tmp.Expr.eval pr+",sizeof("+tmp.Expr.eval pr+"),1,"+fp+");\n")
                |_ ->
                    ()
            |LaTeX ->
                match v.etype,v.Expr with 
                |_,Int v ->
                    pr.cwriter.codewrite("write("+fp+") "+pr.numFormat.ItoS(v)+"\n")
                |_,Dbl v ->
                    pr.cwriter.codewrite("write("+fp+") "+pr.numFormat.DtoS(v)+"\n")
                |Zt,_ ->
                    pr.cwriter.codewrite("write("+fp+") "+v.re.Expr.eval pr+"\n")
                    pr.cwriter.codewrite("write("+fp+") "+v.im.Expr.eval pr+"\n")
                |It _,_ ->
                    pr.cwriter.codewrite("write("+fp+") "+v.Expr.eval pr+"\n")
                |Dt,_ ->
                    pr.cwriter.codewrite("write("+fp+") "+v.Expr.eval pr+"\n")
                |_ -> ()
            |HTML ->
                match v.etype,v.Expr with 
                |_,Int v ->
                    pr.cwriter.codewrite("Write(binary): \\("+fp+" \\leftarrow "+pr.numFormat.ItoS(v)+"\\)<br/>\n")
                |_,Dbl v ->
                    pr.cwriter.codewrite("Write(binary): \\("+fp+" \\leftarrow "+pr.numFormat.DtoS(v)+"\\)<br/>\n")
                |Zt,_ ->
                    pr.cwriter.codewrite("Write(binary): \\("+fp+" \\leftarrow "+v.re.Expr.eval pr+"\\)<br/>\n")
                    pr.cwriter.codewrite("Write(binary): \\("+fp+" \\leftarrow "+v.im.Expr.eval pr+"\\)<br/>\n")
                |It _,_ ->
                    pr.cwriter.codewrite("Write(binary): \\("+fp+" \\leftarrow "+v.Expr.eval pr+"\\)<br/>\n")
                |Dt,_ ->
                    pr.cwriter.codewrite("Write(binary): \\("+fp+" \\leftarrow "+v.Expr.eval pr+"\\)<br/>\n")
                |_ -> ()
            |Python ->
                match v.etype,v.Expr with 
                |_,Int _ ->
                    ch.i <| fun tmp ->
                        tmp <== v
                        pr.cwriter.codewrite(fp+".write(struct.pack('i', "+tmp.Expr.eval pr+"))\n")
                |_,Dbl _ ->
                    ch.i <| fun tmp ->
                        tmp <== v
                        pr.cwriter.codewrite(fp+".write(struct.pack('d', "+tmp.Expr.eval pr+"))\n")
                |Zt,_ ->
                    ch.dd <| fun (tmp_r,tmp_i) ->
                        tmp_r <== v.re
                        tmp_i <== v.im
                        pr.cwriter.codewrite(fp+".write(struct.pack('d', "+tmp_r.Expr.eval pr+"))\n")
                        pr.cwriter.codewrite(fp+".write(struct.pack('d', "+tmp_i.Expr.eval pr+"))\n")
                |It _,_ ->
                    ch.i <| fun tmp ->
                        tmp <== v
                        pr.cwriter.codewrite(fp+".write(struct.pack('i', "+tmp.Expr.eval pr+"))\n")
                |Dt,_ ->
                    ch.d <| fun tmp ->
                        tmp <== v
                        pr.cwriter.codewrite(fp+".write(struct.pack('d', "+tmp.Expr.eval pr+"))\n")
                |_ ->
                    ()
            |_ -> ()
                    
        static member private Read (fp:string) (iostat:num0) (lst:num0 list) = 
            let rec cpxvarlist list (s:num0 list) counter =
                match s with
                |a::b -> 
                    match a.etype with
                    |Zt -> cpxvarlist <| list@[Zt,counter,a] <| b <| counter+1
                    |t   -> cpxvarlist <| list@[t,0,a] <| b <| counter
                |[] -> counter,list
            let Nz,varlist = cpxvarlist [] lst 0
    
            match pr.language with
            |Fortran ->
                ch.dx (2*Nz) <| fun tmp ->
                    let double0string_format_F = 
                        let a,b = pr.numFormat.dFormat
                        "E"+a.ToString()+"."+b.ToString()+"e3"
                    let format = 
                        varlist
                        |> (fun b -> 
                            [for (t,_,_) in b do
                                match t with
                                |It _ ->
                                    yield "I"+pr.numFormat.iFormat.ToString()
                                |Dt ->
                                    yield double0string_format_F
                                |Zt ->
                                    yield double0string_format_F
                                    yield double0string_format_F
                                |_ -> ()
                            ])
                        |> (fun b ->
                              [for n in 0..(b.Length-1) do
                                  yield b.[n]
                                  if n<(b.Length-1) then yield "A1"
                              ])
                        |> fun s -> String.Join(",",s)
                    ch.ix (varlist.Length+Nz-1) <| fun tab ->
                        let code =
                            varlist
                            |> (fun b ->
                                [for t,m,b in b do
                                    match t,b.Expr with 
                                    |Zt,Var _ ->
                                        yield tmp[2*m  ].Expr.eval pr
                                        yield tmp[2*m+1].Expr.eval pr
                                    |_,Var(_,n,_) ->
                                        yield n
                                    |_ -> 
                                        printfn "ファイル読み込みデータの保存先が変数ではありません"
                                        yield ""
                                ])
                            |> (fun b ->
                                  [for n in 0..(b.Length-1) do
                                      yield b[n]
                                      if n<(b.Length-1) then yield tab[n].Expr.eval pr
                                  ])
                            |> fun s -> String.Join(",",s)
                        pr.cwriter.codewrite("read("+fp+",\"("+format+")\",iostat="+iostat.Expr.eval pr+") "+code+"\n")
                        for (t,m,b) in varlist do
                            match t with
                            |Zt ->
                                b <== tmp[2*m]+asm.uj*tmp[2*m+1]
                            |_ ->
                                ()
            |C99 ->
                ch.dx (2*Nz) <| fun tmp ->
                    let format = 
                        varlist
                        |> (fun b -> 
                              [for (t,_,_) in b do
                                match t with
                                |It _ ->
                                    yield "%d"
                                |Dt -> 
                                    yield "%lf"
                                |Zt -> 
                                    yield "%lf"
                                    yield "%lf"
                                |_ -> ()
                              ])
                        |> (fun s -> String.Join("",s))
                    let code =
                      varlist
                      |> (fun b ->
                            [for (t,m,a) in b do
                                match t,a.Expr with 
                                |Zt,Var _ ->
                                    yield "&"+tmp[2*m  ].Expr.eval pr
                                    yield "&"+tmp[2*m+1].Expr.eval pr
                                |_,Var(_,n,_) ->
                                    yield "&"+n
                                |_ ->
                                    printfn "ファイル読み込みデータの保存先が変数ではありません"
                                    yield ""
                            ])
                      |> fun s -> String.Join(",",s)
                    pr.cwriter.codewrite("fscanf("+fp+",\""+format+"\","+code+");\n")
                    for t,m,b in varlist do
                        match t with
                        |Zt ->
                            b <== tmp[2*m]+asm.uj*tmp[2*m+1]
                        |_ ->
                            ()
            |LaTeX ->
                let double0string_format_F = 
                    let a,b = pr.numFormat.dFormat
                    "E"+a.ToString()+"."+b.ToString()+"e3"
                let format = 
                    lst
                    |> List.map (fun b -> 
                        match b.etype with
                          |It _ ->"I"+pr.numFormat.iFormat.ToString()
                          |Dt -> double0string_format_F
                          |_ -> "")
                    |> (fun s -> String.Join(",",s))
                let code =
                    lst
                    |> List.map (fun b ->
                        match b.Expr with 
                        |Var(_,n,_) -> n
                        |_ -> "")
                    |> fun s -> String.Join(",",s)
                pr.cwriter.codewrite("read("+fp+",\"("+format+")\",iostat="+iostat.Expr.eval pr+") "+code+"\n")
            |HTML ->
                let double0string_format_F = 
                    let a,b = pr.numFormat.dFormat
                    "E"+a.ToString()+"."+b.ToString()+"e3"
                let format = 
                    lst
                    |> List.map (fun b -> 
                        match b.etype with
                        |It _ ->"I"+pr.numFormat.iFormat.ToString()
                        |Dt -> double0string_format_F
                        |_ -> "")
                    |> fun s -> String.Join(",",s)
                let code =
                    lst
                    |> List.map (fun b ->
                        match b.Expr with 
                        |Var(_,n,_) -> n
                        |_ -> "")
                    |> fun s -> String.Join("<mo>,</mo>",s)
                pr.cwriter.codewrite("Read(text): \\("+code+" \\leftarrow "+fp+"\\)<br/>\n")
            |Python ->
                ch.dx (2*Nz) <| fun tmp ->
                    let format = 
                        varlist
                        |> (fun b -> 
                              [for (t,_,_) in b do
                                match t with
                                |It _ ->
                                    yield "%d"
                                |Dt -> 
                                    yield "%f"
                                |Zt -> 
                                    yield "%f"
                                    yield "%f"
                                |_ -> ()
                              ])
                        |> (fun s -> String.Join("",s))
                    let code =
                      varlist
                      |> (fun b ->
                            [for t,m,a in b do
                                match t,a.Expr with 
                                |Zt,Var _ ->
                                    yield tmp[2*m  ].Expr.eval pr
                                    yield tmp[2*m+1].Expr.eval pr
                                |_,Var(_,n,_) ->
                                    yield n
                                |_ ->
                                    printfn "ファイル読み込みデータの保存先が変数ではありません"
                                    yield ""
                            ])
                      |> fun s -> String.Join(",",s)
                    //書式指定をしてファイルから値を読み込み。まだ、完成してない
                    pr.cwriter.codewrite("lines = " + fp + ".readline()\n")
                    pr.cwriter.codewrite "word_list = re.split(r\'[\\t\\n]\', lines)\n"
                    let mutable cnt = 0
                    for t,_,a in varlist do
                        //let a_string = string a
                        match t with
                        |It _ ->
                            pr.cwriter.codewrite(a.Expr.eval pr+" = int(word_list["+cnt.ToString()+"])")
                            cnt <- cnt + 1
                        |Dt -> 
                            pr.cwriter.codewrite(a.Expr.eval pr+"= float(word_list["+cnt.ToString()+"])")
                            cnt <- cnt + 1
                        |Zt -> 
                            pr.cwriter.codewrite(a.Expr.eval pr+" = complex(float(word_list["+cnt.ToString()+"]),float(word_list["+(cnt+1).ToString()+"]))")
                            cnt <- cnt + 2
                        |_ -> ()
            |_ -> ()
            
        static member private Read_bin (fp:string) (iostat:num0) (v:num0) = 
            match pr.language with
            |Fortran ->
                match v.etype,v.Expr with 
                |Zt,Var _ ->
                    ch.dd <| fun (re,im) ->
                        pr.cwriter.codewrite("read("+fp+",iostat="+iostat.Expr.eval pr+") "+re.Expr.eval pr+"\n")
                        pr.cwriter.codewrite("read("+fp+",iostat="+iostat.Expr.eval pr+") "+im.Expr.eval pr+"\n")
                        v <== re+asm.uj*im
                |_,Var(_,n,_) ->
                    pr.cwriter.codewrite("read("+fp+",iostat="+iostat.Expr.eval pr+") "+n+"\n")
                |_ -> 
                    Console.WriteLine("ファイル読み込みデータの保存先が変数ではありません")
            |C99 ->
                match v.etype,v.Expr with 
                |Zt,Var _ ->
                    ch.dd <| fun (re,im) ->
                        pr.cwriter.codewrite("fread(&"+re.Expr.eval pr+",sizeof("+re.Expr.eval pr+"),1,"+fp+");"+"\n")
                        pr.cwriter.codewrite("fread(&"+im.Expr.eval pr+",sizeof("+im.Expr.eval pr+"),1,"+fp+");"+"\n")
                        v <== re+asm.uj*im
                |_,Var(_,n,_) ->
                    pr.cwriter.codewrite("fread(&"+n+",sizeof("+n+"),1,"+fp+");"+"\n")
                |_ -> 
                    printfn "ファイル読み込みデータの保存先が変数ではありません"
            |LaTeX ->
                match v.etype,v.Expr with 
                |Zt,Var _ ->
                    ch.dd <| fun (re,im) ->
                        pr.cwriter.codewrite("read("+fp+",iostat="+iostat.Expr.eval pr+") "+re.Expr.eval pr+"\n")
                        pr.cwriter.codewrite("read("+fp+",iostat="+iostat.Expr.eval pr+") "+im.Expr.eval pr+"\n")
                        v <== re+asm.uj*im
                |_,Var(_,n,_) ->
                    pr.cwriter.codewrite("read("+fp+",iostat="+iostat.Expr.eval pr+") "+n+"\n")
                |_ -> 
                    printfn "ファイル読み込みデータの保存先が変数ではありません"
            |HTML ->
                match v.Expr with 
                |Var(_,n,_) ->
                    pr.cwriter.codewrite("Read(binary): \\("+n+" \\leftarrow "+fp+"\\)<br/>\n")
                |_ -> 
                    printfn "ファイル読み込みデータの保存先が変数ではありません"
            |Python ->
                match v.etype,v.Expr with 
                |Zt,Var _ ->
                    ch.dd <| fun (re,im) ->
                        pr.cwriter.codewrite(re.Expr.eval pr+" = struct.unpack('d', "+fp+".read(8))[0]"+"\n")
                        pr.cwriter.codewrite(im.Expr.eval pr+" = struct.unpack('d', "+fp+".read(8))[0]"+"\n")
                        v <== re+asm.uj*im
                |It _,Var(_,n,_) ->
                    pr.cwriter.codewrite(n+" = struct.unpack('i', "+fp+".read(4))[0]"+"\n")
                |Dt,Var(_,n,_) ->
                    pr.cwriter.codewrite(n+" = struct.unpack('d', "+fp+".read(8))[0]"+"\n")
                |_ -> 
                    printfn "ファイル読み込みデータの保存先が変数ではありません"
            |_ -> ()
                    
        static member private Read_byte (fp:string) (iostat:num0) (e:num0) = 
            pr.cwriter.codewrite("read("+fp+", iostat="+iostat.Expr.eval pr+") byte_tmp\n")
            let ee =
                match e.etype,e.Expr with 
                |It _,Var(_,n,_) -> n 
                |_ -> "byte値を整数型以外の変数に格納できません"
            pr.cwriter.codewrite(ee + "=" + "byte_tmp\n")
            
        ///<summary>ファイル出力（タブ区切りデータ）</summary>
        static member fileOutput (filename:exprString) = fun code ->
            io.fileAccess filename false false <| fun fp ->
                code(io.Write1 fp)
                
        ///<summary>ファイル出力（コード出力）</summary>
        static member codeOutput (filename:exprString) = fun code ->
            io.fileAccess filename false false <| fun fp ->
                code(io.Write2 fp)
                
        ///<summary>ファイル出力（タブ区切りデータ）</summary>
        static member fileOutput (filename:string) = fun code -> io.fileOutput (Str filename) code
        
        ///<summary>ファイル出力（コード出力）</summary>
        static member codeOutput (filename:string) = fun code -> io.codeOutput (Str filename) code
        
        ///<summary>バイナリファイル出力</summary>
        static member binfileOutput (filename:exprString) = fun code ->
            io.fileAccess filename false true <| fun fp ->
                code(io.Write_bin fp)

        ///<summary>バイナリファイル出力</summary>
        static member binfileOutput (filename:string) = fun code -> io.binfileOutput (Str filename) code

        ///<summary>ファイル読み込み</summary>
        static member fileInput (filename:exprString) = fun code ->
            ch.i <| fun iostat ->
                io.fileAccess filename true false <| fun fp ->
                    code(io.Read fp iostat)
                    
        ///<summary>ファイル読み込み</summary>
        static member fileInput (filename:string) = fun code -> io.fileInput (Str filename) code

        ///<summary>バイナリファイルの読み込み</summary>
        static member binfileInput (filename:exprString) = fun code ->
            ch.i <| fun iostat ->
                io.fileAccess filename true true <| fun fp ->
                    code(io.Read_bin fp iostat)
                    
        ///<summary>バイナリファイルの読み込み</summary>
        static member binfileInput (filename:string) = fun code -> io.binfileInput (Str filename) code
                
        ///<summary>テキストファイルの行数をカウント</summary>
        static member file_LineCount (counter:num0) (filename:exprString) varlist =
            ch.i <| fun iostat ->
                io.fileAccess filename true false <| fun fp ->
                    iter.loop <| fun (ext,i) ->
                        io.Read fp iostat varlist
                        br.branch <| fun b ->
                            b.IF (iostat .< 0) <| fun () ->
                                counter <== i-1
                                ext()
                                
        ///<summary>ファイルの読み込み</summary>
        static member file_Read (filename:exprString) varlist code =
            ch.i <| fun iostat ->
                io.fileAccess filename true false <| fun fp ->
                    iter.loop <| fun (ext,i) ->
                        io.Read fp iostat varlist
                        br.branch <| fun b ->
                            b.IF (iostat .< 0) <| fun () ->
                                ext()
                            b.EL <| fun () ->
                                code(i)
                                
        ///<summary>配列をファイルに保存</summary>
        static member save_text (f:num3,filename:exprString) =
            io.fileOutput filename <| fun w ->
                f.foreach <| fun (i,j,k) ->
                    w [i;j;k;f.[i,j,k]]

        ///<summary>配列をファイルに保存</summary>
        static member save_text (f:num2,filename:exprString) =
            io.fileOutput filename <| fun w -> 
                f.foreach <| fun (i,j) ->
                    w [i;j;f.[i,j]]

        ///<summary>配列をファイルに保存</summary>
        static member save_text (f:num1,filename:exprString) =
            io.fileOutput filename <| fun w -> 
                f.foreach <| fun i -> 
                    w [i;f.[i]]

        ///<summary>数値をファイルに保存</summary>
        static member save_text (f:num0,filename:exprString) =
            io.fileOutput filename <| fun w -> 
                w [f]
                
        ///<summary>配列をファイルに保存</summary>
        static member save_text (f:num3,filename:string) = io.save_text(f,Str filename)
        static member save_text (f:num2,filename:string) = io.save_text(f,Str filename)
        static member save_text (f:num1,filename:string) = io.save_text(f,Str filename)
        static member save_text (f:num0,filename:string) = io.save_text(f,Str filename)
        
        ///<summary>数値をファイルに保存</summary>
        static member save (f:num0,filename:exprString) =
            io.binfileOutput filename <| fun w ->
                //データフォーマット
                w _1
                //データ型
                match f.etype with
                |Etype.It(4) -> w <| I 1004
                |Etype.Dt    -> w <| I 2000
                |Etype.Zt    -> w <| I 3000
                |_           -> w <| I 0
                //データ次元
                w _0
                //データサイズ
                w _1
                //データ本体
                match f.etype with
                |Zt ->
                    w f.re
                    w f.im
                |_ ->
                    w f
                    
        ///<summary>1次元データをファイルに保存</summary>
        static member save (f:num1,filename:exprString) =
                io.binfileOutput filename <| fun w ->
                    //データフォーマット
                    w _1
                    //データ型
                    match f.etype with
                    |Etype.It(4) -> w <| I 1004
                    |Etype.Dt    -> w <| I 2000
                    |Etype.Zt    -> w <| I 3000
                    |_           -> w <| I 0
                    //データ次元
                    w _1
                    //データサイズ
                    w f.size1
                    //データ本体
                    iter.num f.size1 <| fun i ->
                        match f[0].etype with
                        |Zt ->
                            w f[i].re
                            w f[i].im
                        |_ ->
                            w f[i]
                            
        ///<summary>2次元データをファイルに保存</summary>
        static member save (f:num2,filename:exprString) =
            io.binfileOutput filename <| fun w ->
                //データフォーマット
                w _1
                //データ型
                match f.etype with
                |Etype.It(4) -> w <| I 1004
                |Etype.Dt    -> w <| I 2000
                |Etype.Zt    -> w <| I 3000
                |_           -> w <| I 0
                //データ次元
                w _2
                //データサイズ
                w f.size1
                w f.size2
                //データ本体
                iter.num f.size2 <| fun j ->
                    iter.num f.size1 <| fun i ->
                        match f[0,0].etype with
                        |Zt ->
                            w f[i,j].re
                            w f[i,j].im
                        |_ ->
                            w f[i,j]
                            
        ///<summary>3次元データをファイルに保存</summary>
        static member save (f:num3,filename:exprString) =
            io.binfileOutput filename <| fun w ->
                //データフォーマット
                w _1
                //データ型
                match f.etype with
                |Etype.It 4 -> w <| I 1004
                |Etype.Dt   -> w <| I 2000
                |Etype.Zt   -> w <| I 3000
                |_          -> w <| I 0
                //データ次元
                w _3
                //データサイズ
                w f.size1
                w f.size2
                w f.size3
                //データ本体
                iter.num f.size3 <| fun k ->
                    iter.num f.size2 <| fun j ->
                        iter.num f.size1 <| fun i ->
                            match f[_0,_0,_0].etype with
                            |Zt ->
                                w f[i,j,k].re
                                w f[i,j,k].im
                            |_ ->
                                w f[i,j,k]
                                
        static member save (f:num3,filename:string) = io.save(f,Str filename)
        static member save (f:num2,filename:string) = io.save(f,Str filename)
        static member save (f:num1,filename:string) = io.save(f,Str filename)
        static member save (f:num0,filename:string) = io.save(f,Str filename)
        
        ///<summary>数値をファイルから読み込み</summary>
        static member load (f:num0,filename:exprString) =
            let reader (r:num0->unit) (nt:int,t:Etype) =
                ch.i <| fun n ->
                    //データ型
                    r n
                    br.if2 (n.=nt)
                        <| fun () ->
                            //データ次元
                            r n
                            br.if2 (n.=0)
                                <| fun () ->
                                    ch.i <| fun n1 ->
                                        //データサイズ
                                        r n1
                                        //データ本体
                                        match t with
                                        |Zt ->
                                            ch.dd <| fun (re,im) ->
                                                r re
                                                r im
                                                f <== re + asm.uj*im
                                        |_ ->
                                            r f
                                <| fun () ->
                                    print.t "Invalid data dimension"
                        <| fun () ->
                            print.t "invalid data type"
            io.binfileInput filename <| fun r ->
            ch.i <| fun n ->
                //データフォーマット
                r n
                br.branch <| fun b ->
                    b.IF (n.=1) <| fun () ->
                        match f.etype with
                        |It 4 ->
                            reader r (1004,f.etype)
                        |Dt    -> 
                            reader r (2000,f.etype)
                        |Zt    -> 
                            reader r (3000,f.etype)
                        |_ -> 
                            print.t "invalid data type"
                                
        ///<summary>1次元データをファイルから読み込み</summary>
        static member load (f:num1,filename:exprString) =
            let reader (r:num0->unit) (nt:int,t:Etype) =
                ch.i <| fun n ->
                    //データ型
                    r n
                    br.if2 (n.=nt)
                        <| fun () ->
                            //データ次元
                            r n
                            br.if2 (n.=1)
                                <| fun () ->
                                    ch.i <| fun n1 ->
                                        //データサイズ
                                        r n1
                                        f.allocate n1
                                        //データ本体
                                        match t with
                                        |It _ ->
                                            iter.num f.size1 <| fun i ->
                                                ch.i <| fun u ->
                                                    r u
                                                    f[i] <== u
                                        |Dt ->
                                            iter.num f.size1 <| fun i ->
                                                ch.d <| fun u ->
                                                    r u
                                                    f[i] <== u
                                        |Zt ->
                                            iter.num f.size1 <| fun i ->
                                                ch.dd <| fun (re,im) ->
                                                    r re
                                                    r im
                                                    f[i] <== re + asm.uj*im
                                        |_ ->
                                            ()
                                <| fun () ->
                                    print.t "Invalid data dimension"
                        <| fun () ->
                            print.t ": invalid data type"
            io.binfileInput filename <| fun r ->
            ch.i <| fun n ->
                //データフォーマット
                r n
                br.branch <| fun b ->
                    b.IF (n.=1) <| fun () ->
                        match f[0].etype with
                        |Etype.It(4) ->
                            reader r (1004,f[0].etype)
                        |Etype.Dt    -> 
                            reader r (2000,f[0].etype)
                        |Etype.Zt    -> 
                            reader r (3000,f[0].etype)
                        |_ -> 
                            print.t "invalid data type"
                            
        ///<summary>2次元データをファイルから読み込み</summary>
        static member load (f:num2,filename:exprString) =
            let reader (r:num0->unit) (nt:int,t:Etype) =
                ch.i <| fun n ->
                    //データ型
                    r n
                    br.if2 (n.=nt)
                        <| fun () ->
                            //データ次元
                            r n
                            br.if2 (n.=2)
                                <| fun () ->
                                    ch.ii <| fun (n1,n2) ->
                                        //データサイズ
                                        r n1
                                        r n2
                                        f.allocate(n1,n2)
                                        //データ本体
                                        match t with
                                        |It _ ->
                                            iter.num f.size2 <| fun j ->
                                                iter.num f.size1 <| fun i ->
                                                    ch.i <| fun u ->
                                                        r u
                                                        f[i,j] <== u
                                        |Dt ->
                                            iter.num f.size2 <| fun j ->
                                                iter.num f.size1 <| fun i ->
                                                    ch.d <| fun u ->
                                                        r u
                                                        f[i,j] <== u
                                        |Zt ->
                                            iter.num f.size2 <| fun j ->
                                                iter.num f.size1 <| fun i ->
                                                    ch.dd <| fun (re,im) ->
                                                        r re
                                                        r im
                                                        f[i,j] <== re + asm.uj*im
                                        |_ ->
                                            ()
                                <| fun () ->
                                    print.t "Invalid data dimension"
                        <| fun () ->
                            print.t ": invalid data type"
                            print.cc n (I nt)
            io.binfileInput filename <| fun r ->
            ch.i <| fun n ->
                //データフォーマット
                r n
                br.branch <| fun b ->
                    b.IF (n.=1) <| fun () ->
                        match f[0,0].etype with
                        |It 4 ->
                            reader r (1004,f[0,0].etype)
                        |Dt   -> 
                            reader r (2000,f[0,0].etype)
                        |Zt   -> 
                            reader r (3000,f[0,0].etype)
                        |_ -> 
                            print.t "invalid data type"
                            
        ///<summary>3次元データをファイルから読み込み</summary>
        static member load (f:num3,filename:exprString) =
            let reader (r:num0->unit) (nt:int,t:Etype) =
                ch.i <| fun n ->
                    //データ型
                    r n
                    br.if2 (n.=nt)
                        <| fun () ->
                            //データ次元
                            r n
                            br.if2 (n.=3)
                                <| fun () ->
                                    ch.iii <| fun (n1,n2,n3) ->
                                        //データサイズ
                                        r n1
                                        r n2
                                        r n3
                                        f.allocate(n1,n2,n3)
                                        //データ本体
                                        match t with
                                        |It _ ->
                                            iter.num f.size3 <| fun k ->
                                                iter.num f.size2 <| fun j ->
                                                    iter.num f.size1 <| fun i ->
                                                        ch.i <| fun u ->
                                                            r u
                                                            f[i,j,k] <== u
                                        |Dt ->
                                            iter.num f.size3 <| fun k ->
                                                iter.num f.size2 <| fun j ->
                                                    iter.num f.size1 <| fun i ->
                                                        ch.d <| fun u ->
                                                            r u
                                                            f[i,j,k] <== u
                                        |Zt ->
                                            iter.num f.size3 <| fun k ->
                                                iter.num f.size2 <| fun j ->
                                                    iter.num f.size1 <| fun i ->
                                                        ch.dd <| fun (re,im) ->
                                                            r re
                                                            r im
                                                            f[i,j,k] <== re + asm.uj*im
                                        |_ ->
                                            ()
                                <| fun () ->
                                    print.t "Invalid data dimension"
                        <| fun () ->
                            print.t "invalid data type"
            io.binfileInput filename <| fun r ->
            ch.i <| fun n ->
                //データフォーマット
                r n
                br.branch <| fun b ->
                    b.IF (n.=1) <| fun () ->
                        match f[_0,_0,_0].etype with
                        |Etype.It(4) ->
                            reader r (1004,f[_0,_0,_0].etype)
                        |Etype.Dt    -> 
                            reader r (2000,f[_0,_0,_0].etype)
                        |Etype.Zt    -> 
                            reader r (3000,f[_0,_0,_0].etype)
                        |_ -> 
                            print.t "invalid data type"
                            
        static member load (f:num3,filename:string) = io.load(f,Str filename)
        static member load (f:num2,filename:string) = io.load(f,Str filename)
        static member load (f:num1,filename:string) = io.load(f,Str filename)
        static member load (f:num0,filename:string) = io.load(f,Str filename)
        
    ///<summary>ファイル入出力（処理スキップ）</summary>
    type dummy_io () =
        
        static member fileOutput (filename:num0 list) code = ()
        
        static member fileInput (filename:num0 list) code = ()
        
        static member binfileInput (filename:num0 list) code = ()
        
        static member file_LineCount (counter:num0) (filename:num0 list) varlist = ()
        
        static member file_Read (filename:num0 list) varlist code = ()
        
        ///<summary>配列をファイルに保存</summary>
        static member array (f:num2) = fun filename -> ()
        
        ///<summary>配列をファイルに保存</summary>
        static member array (f:num1) = fun filename -> ()
        