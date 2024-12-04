(*
Copyright (c) 2022 Jun-ichiro Sugisaka

This software is released under the MIT License.
http://opensource.org/licenses/mit-license.php
*)
namespace Aqualis
    
    open System
    open Aqualis_base

    ///<summary>ファイル入出力</summary>
    type io () =
        
        static member private fileAccess (filename:num0 list) readmode isbinary code =
            match p.lang with
            |F ->
                ch.f <| fun fp ->
                    let f = 
                        filename
                        |> List.map (fun s -> match s.etype with |St -> "A" |It _ -> "I"+p.iFormat.ToString() |_ -> "")
                        |> (fun s -> String.Join(",",s))
                    let s = 
                        filename
                        |> List.map (fun s -> s.code)
                        |> (fun s -> String.Join(",",s))
                    ch.t <| A0 <| fun id ->
                        let btname = "byte_tmp"
                        //変数byte_tmpをリストに追加（存在していない場合のみ）
                        p.var.setUniqVar(Structure("integer(1)"),A0,btname,"")
                        p.codewrite("write("+id+",\"("+f+")\") "+s+"\n")
                        p.getloopvar <| fun counter ->
                            p.codewrite("do "+counter+" = 1, len_trim("+id+")"+"\n")
                            p.codewrite("  if ( "+id+"( "+counter+":"+counter+" ).EQ.\" \" ) "+id+"( "+counter+":"+counter+" ) = \"0\""+"\n")
                            p.codewrite("end do"+"\n")
                        if isbinary then
                            p.codewrite("open("+fp+", file=trim("+id+"), access='stream', form='unformatted')"+"\n")
                        else
                            p.codewrite("open("+fp+", file=trim("+id+"))"+"\n")
                        code(fp)
                        p.codewrite("close("+fp+")"+"\n")
            |C ->
                ch.f <| fun fp ->
                    let f = 
                        filename
                        |> List.map (fun s -> match s,s.etype with |Str_c(v),_ -> v |_,It _ -> "%"+p.iFormat.ToString("00")+"d" |_ -> "")
                        |> (fun s -> String.Join("",s))
                    let s = 
                        [for s in filename do
                          match s.etype with 
                            |St -> ()
                            |_ -> yield s.code ]
                        |> (fun s -> String.Join(",",s))
                    ch.t <| A0 <| fun id ->
                        let btname = "byte_tmp"
                        //変数byte_tmpをリストに追加（存在していない場合のみ）
                        p.var.setUniqVar(Structure("char"),A0,btname,"")
                        p.codewrite("sprintf("+id+",\""+f+"\""+(if s="" then "" else ",")+s+");\n")
                        if isbinary then
                            p.codewrite(fp+" = "+"fopen("+id+",\""+(if readmode then "rb" else "wb")+"\");"+"\n")
                        else
                            p.codewrite(fp+" = "+"fopen("+id+",\""+(if readmode then "r" else "w")+"\");"+"\n")
                        code(fp)
                        p.codewrite("fclose("+fp+")"+";\n")
            |T ->
                ch.f <| fun fp ->
                    let f = 
                        filename
                        |> List.map (fun s -> match s.etype with |St -> "%s" |It _ -> "%"+p.iFormat.ToString("00")+"d" |_ -> "")
                        |> (fun s -> String.Join("",s))
                    let s = 
                        filename
                        |> List.map (fun s -> s.code)
                        |> (fun s -> String.Join(",",s))
                    ch.t <| A0 <| fun id ->
                        let btname = "byte_tmp"
                        //変数byte_tmpをリストに追加（存在していない場合のみ）
                        p.var.setUniqVar(Structure("char"),A0,btname,"")
                        p.codewrite("sprintf("+id+",\""+f+"\","+s+");\n")
                        if isbinary then
                            p.codewrite(fp+" = "+"fopen("+id+",\""+(if readmode then "rb" else "wb")+"\");"+"\n")
                        else
                            p.codewrite(fp+" = "+"fopen("+id+",\""+(if readmode then "r" else "w")+"\");"+"\n")
                        code(fp)
                        p.codewrite("fclose $"+fp+" "+"$\n")
            |H ->
                ch.f <| fun fp ->
                    let f = 
                        filename
                        |> List.map (fun s -> match s.etype with |St -> "%s" |It _ -> "%"+p.iFormat.ToString("00")+"d" |_ -> "")
                        |> (fun s -> String.Join("",s))
                    let s = 
                        filename
                        |> List.map (fun s -> s.code)
                        |> (fun s -> String.Join(",",s))
                    ch.t <| A0 <| fun id ->
                        let btname = "byte_tmp"
                        //変数byte_tmpをリストに追加（存在していない場合のみ）
                        p.var.setUniqVar(Structure("char"),A0,btname,"")
                        p.codewrite("sprintf("+id+",\""+f+"\","+s+");\n")
                        if isbinary then
                            p.codewrite("\\("+fp+"\\)"+" = "+"fopen("+id+",\""+(if readmode then "rb" else "wb")+"\");"+"\n")
                        else
                            p.codewrite("\\("+fp+"\\)"+" = "+"fopen("+id+",\""+(if readmode then "r" else "w")+"\");"+"\n")
                        code(fp)
                        p.codewrite("fclose \\("+fp+" "+"\\)\n")
            |P ->
                ch.f <| fun fp ->
                    let f = 
                        filename
                        |> List.map (fun s -> match s,s.etype with |Str_c(v),_ -> v |_,It _ -> "%"+p.iFormat.ToString("00")+"d" |_ -> "")
                        |> (fun s -> String.Join("",s))
                    let s = 
                        [for s in filename do
                          match s.etype with 
                            |St -> ()
                            |_ -> yield s.code ]
                        |> (fun s -> String.Join(",",s))
                    ch.t <| A0 <| fun id ->
                        let btname = "byte_tmp"
                        //変数byte_tmpをリストに追加（存在していない場合のみ）
                        p.var.setUniqVar(Structure("char"),A0,btname,"")
                        p.codewrite(id+"= \""+f+"\"%("+s+")\n")
                        if isbinary then
                            p.codewrite(fp+" = "+"open("+id+",mode=\""+(if readmode then "rb" else "wb")+"\")"+"\n")
                        else
                            p.codewrite(fp+" = "+"open("+id+",mode=\""+(if readmode then "r" else "w")+"\")"+"\n")
                        code(fp)
                        p.codewrite(fp+".close()"+"\n")
        static member private Write (fp:string) (lst:num0 list) =
            match p.lang with
            |F ->
                let tab = var.ip0_noWarning("tab",2313)
                let double0string_format_F = 
                    let (a,b)=p.dFormat
                    "E"+a.ToString()+"."+b.ToString()+"e3"
                let format = 
                    lst
                    |> (fun b ->
                        [for n in 0..(b.Length-1) do
                            match b.[n].etype with
                            |It _ -> 
                                yield "I"+p.iFormat.ToString()
                            |Dt ->
                                yield double0string_format_F
                            |Zt ->
                                yield double0string_format_F
                                yield double0string_format_F 
                            |St -> 
                                yield "A"
                            |_ -> ()
                        ])
                    |> (fun b ->
                          [for n in 0..(b.Length-1) do
                              yield b.[n]
                              if n<(b.Length-1) then yield "A1"
                          ])
                    |> (fun s -> String.Join(",",s))
                let code =
                    lst
                    |> (fun b ->
                        [for n in 0..(b.Length-1) do
                            match b.[n].etype,b.[n] with 
                            |It _,Int_c(v) -> yield p.ItoS(v)
                            |Dt  ,Int_c(v) -> yield p.DtoS(double v)
                            |_,Dbl_c(v) -> yield p.DtoS(v)
                            |_,Str_c(v) -> yield "\""+v.Replace("\"","\"\"")+"\""
                            |Zt,_ ->
                                yield (b.[n].re.code)
                                yield (b.[n].im.code)
                            |(It _|Dt),_ -> yield b.[n].code
                            |_ -> ()])
                    |> (fun b ->
                          [for n in 0..(b.Length-1) do
                              yield b.[n]
                              if n<(b.Length-1) then yield tab.code
                          ])
                    |> (fun s -> String.Join(",",s))
                p.codewrite("write("+fp+",\"("+format+")\") "+code+"\n")
            |C ->
                let int0string_format_C =
                    "%"+p.iFormat.ToString()+"d"
                let double0string_format_C = 
                    let (a,b)=p.dFormat
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
                            |Str_c(v),_ ->
                                yield v.Replace("\"","\\\"")
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
                        |_,Int_c(v) -> yield p.ItoS(v)
                        |_,Dbl_c(v) -> yield p.DtoS(v)
                        |Zt,_ ->
                            yield b.re.code
                            yield b.im.code
                        |(It _|Dt),_ -> yield b.code
                        |_ -> ()]
                    |> (fun s -> String.Join(",",s))
                p.codewrite("fprintf("+fp+",\""+format+"\\n\""+(if code ="" then "" else ",")+code+");\n")
            |T ->
                let double0string_format_F = 
                    let (a,b)=p.dFormat
                    "E"+a.ToString()+"."+b.ToString()+"e3"
                let format = 
                    lst
                    |> List.map (fun b -> 
                        match b.etype with
                          |It _ ->"I"+p.iFormat.ToString()
                          |Dt -> double0string_format_F
                          |Zt -> double0string_format_F+","+double0string_format_F 
                          |St -> "A"
                          |_ -> "")
                    |> (fun s -> String.Join("",s))
                let code =
                    lst
                    |> List.map (fun b ->
                        match b.etype,b with 
                          |_,Int_c(v) -> p.ItoS(v)
                          |_,Dbl_c(v) -> p.DtoS(v)
                          |_,Str_c(v) -> v
                          |Zt,Var _ -> b.re.code+","+b.im.code
                          |_,Var(_,n) -> n
                          |_,Formula(_,n) -> n 
                          |_ -> "")
                    |> (fun s -> String.Join(",",s))
                p.codewrite("write("+fp+",\"("+format+")\") "+code+"\n")
            |H ->
                let double0string_format_F = 
                    let (a,b)=p.dFormat
                    "E"+a.ToString()+"."+b.ToString()+"e3"
                let format = 
                    lst
                    |> List.map (fun b -> 
                        match b.etype with
                          |It _ ->"I"+p.iFormat.ToString()
                          |Dt -> double0string_format_F
                          |Zt -> double0string_format_F+","+double0string_format_F 
                          |St -> "A"
                          |_ -> "")
                    |> (fun s -> String.Join("",s))
                let code =
                    lst
                    |> List.map (fun b ->
                        match b.etype,b with 
                          |_,Int_c(v) -> p.ItoS(v)
                          |_,Dbl_c(v) -> p.DtoS(v)
                          |_,Str_c(v) -> "\""+v+"\""
                          |Zt,Var _ -> b.re.code+","+b.im.code
                          |_,Var(_,n) -> n
                          |_,Formula(_,n) -> n 
                          |_ -> "")
                    |> (fun s -> String.Join(",",s))
                p.codewrite("Write(text): \\("+fp+" \\leftarrow "+code+"\\)<br/>")
            |P ->
                let int0string_format_C =
                    "%"+p.iFormat.ToString()+"d"
                let double0string_format_C = 
                    let (a,b)=p.dFormat
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
                            |Str_c(v),_ ->
                                yield v.Replace("\"","\\\"")
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
                        |_,Int_c(v) -> yield p.ItoS(v)
                        |_,Dbl_c(v) -> yield p.DtoS(v)
                        |Zt,_ ->
                            yield b.re.code
                            yield b.im.code
                        |(It _|Dt),_ -> yield b.code
                        |_ -> ()]
                    |> (fun s -> String.Join(",",s))
                p.codewrite(fp+".write(\""+format+"\\n\" %("+code+"))\n")
                
        static member private Write_bin (fp:string) (v:num0) =
            match p.lang with
            |F ->
                match v.etype,v with 
                |_,Int_c(v) ->
                    p.codewrite("write("+fp+") "+p.ItoS(v)+"\n")
                |_,Dbl_c(v) ->
                    p.codewrite("write("+fp+") "+p.DtoS(v)+"\n")
                |_,Str_c(v) ->
                    p.codewrite("write("+fp+") "+v+"\n")
                |Zt,_ ->
                    p.codewrite("write("+fp+") "+v.re.code+"\n")
                    p.codewrite("write("+fp+") "+v.im.code+"\n")
                |It _,_ ->
                    p.codewrite("write("+fp+") "+v.code+"\n")
                |Dt,_ ->
                    p.codewrite("write("+fp+") "+v.code+"\n")
                |_ -> ()
            |C ->
                match v.etype,v with 
                |_,Int_c _ ->
                    ch.i <| fun tmp ->
                        tmp <== v
                        p.codewrite("fwrite(&"+tmp.code+",sizeof("+tmp.code+"),1,"+fp+");\n")
                |_,Dbl_c _ ->
                    ch.i <| fun tmp ->
                        tmp <== v
                        p.codewrite("fwrite(&"+tmp.code+",sizeof("+tmp.code+"),1,"+fp+");\n")
                |Zt,_ ->
                    ch.dd <| fun (tmp_r,tmp_i) ->
                        tmp_r <== v.re
                        tmp_i <== v.im
                        p.codewrite("fwrite(&"+tmp_r.code+",sizeof("+tmp_r.code+"),1,"+fp+");\n")
                        p.codewrite("fwrite(&"+tmp_i.code+",sizeof("+tmp_i.code+"),1,"+fp+");\n")
                |It _,_ ->
                    ch.i <| fun tmp ->
                        tmp <== v
                        p.codewrite("fwrite(&"+tmp.code+",sizeof("+tmp.code+"),1,"+fp+");\n")
                |Dt,_ ->
                    ch.d <| fun tmp ->
                        tmp <== v
                        p.codewrite("fwrite(&"+tmp.code+",sizeof("+tmp.code+"),1,"+fp+");\n")
                |_ ->
                    ()
            |T ->
                match v.etype,v with 
                |_,Int_c(v) ->
                    p.codewrite("write("+fp+") "+p.ItoS(v)+"\n")
                |_,Dbl_c(v) ->
                    p.codewrite("write("+fp+") "+p.DtoS(v)+"\n")
                |_,Str_c(v) ->
                    p.codewrite("write("+fp+") "+v+"\n")
                |Zt,_ ->
                    p.codewrite("write("+fp+") "+v.re.code+"\n")
                    p.codewrite("write("+fp+") "+v.im.code+"\n")
                |It _,_ ->
                    p.codewrite("write("+fp+") "+v.code+"\n")
                |Dt,_ ->
                    p.codewrite("write("+fp+") "+v.code+"\n")
                |_ -> ()
            |H ->
                match v.etype,v with 
                |_,Int_c(v) ->
                    p.codewrite("Write(binary): \\("+fp+" \\leftarrow "+p.ItoS(v)+"\\)<br/>\n")
                |_,Dbl_c(v) ->
                    p.codewrite("Write(binary): \\("+fp+" \\leftarrow "+p.DtoS(v)+"\\)<br/>\n")
                |_,Str_c(v) ->
                    p.codewrite("Write(binary): \\("+fp+" \\leftarrow "+v+"\\)<br/>\n")
                |Zt,_ ->
                    p.codewrite("Write(binary): \\("+fp+" \\leftarrow "+v.re.code+"\\)<br/>\n")
                    p.codewrite("Write(binary): \\("+fp+" \\leftarrow "+v.im.code+"\\)<br/>\n")
                |It _,_ ->
                    p.codewrite("Write(binary): \\("+fp+" \\leftarrow "+v.code+"\\)<br/>\n")
                |Dt,_ ->
                    p.codewrite("Write(binary): \\("+fp+" \\leftarrow "+v.code+"\\)<br/>\n")
                |_ -> ()
            |P ->
                match v.etype,v with 
                |_,Int_c _ ->
                    ch.i <| fun tmp ->
                        tmp <== v
                        p.codewrite(fp+".write(struct.pack('i', "+tmp.code+"))\n")
                |_,Dbl_c _ ->
                    ch.i <| fun tmp ->
                        tmp <== v
                        p.codewrite(fp+".write(struct.pack('d', "+tmp.code+"))\n")
                |Zt,_ ->
                    ch.dd <| fun (tmp_r,tmp_i) ->
                        tmp_r <== v.re
                        tmp_i <== v.im
                        p.codewrite(fp+".write(struct.pack('d', "+tmp_r.code+"))\n")
                        p.codewrite(fp+".write(struct.pack('d', "+tmp_i.code+"))\n")
                |It _,_ ->
                    ch.i <| fun tmp ->
                        tmp <== v
                        p.codewrite(fp+".write(struct.pack('i', "+tmp.code+"))\n")
                |Dt,_ ->
                    ch.d <| fun tmp ->
                        tmp <== v
                        p.codewrite(fp+".write(struct.pack('d', "+tmp.code+"))\n")
                |_ ->
                    ()
                
        static member private Read (fp:string) (iostat:num0) (lst:num0 list) = 
            let rec cpxvarlist list (s:num0 list) counter =
                match s with
                |a::b -> 
                    match a.etype with
                    |Zt -> cpxvarlist <| list@[Zt,counter,a] <| b <| counter+1
                    |t   -> cpxvarlist <| list@[t,0,a] <| b <| counter
                |[] -> counter,list
            let Nz,varlist = cpxvarlist [] lst 0
    
            match p.lang with
            |F ->
                ch.d01 <| fun tmp ->
                    if Nz>0 then tmp.allocate(2*Nz)
                    let double0string_format_F = 
                        let (a,b)=p.dFormat
                        "E"+a.ToString()+"."+b.ToString()+"e3"
                    let format = 
                        varlist
                        |> (fun b -> 
                            [for (t,_,_) in b do
                                match t with
                                |It _ ->
                                    yield "I"+p.iFormat.ToString()
                                |Dt ->
                                    yield double0string_format_F
                                |Zt ->
                                    yield double0string_format_F
                                    yield double0string_format_F
                                |St ->
                                    yield "A"
                                |_ -> ()
                            ])
                        |> (fun b ->
                              [for n in 0..(b.Length-1) do
                                  yield b.[n]
                                  if n<(b.Length-1) then yield "A1"
                              ])
                        |> (fun s -> String.Join(",",s))
                    ch.i1 (varlist.Length-1) <| fun tab ->
                        let code =
                            varlist
                            |> (fun b ->
                                [for (t,m,b) in b do
                                    match t,b with 
                                    |Zt,Var _ ->
                                        yield tmp.[2*m+1].code
                                        yield tmp.[2*m+2].code
                                    |_,Var(_,n) ->
                                        yield n
                                    |_ -> 
                                        Console.WriteLine("ファイル読み込みデータの保存先が変数ではありません")
                                        yield ""
                                ])
                            |> (fun b ->
                                  [for n in 0..(b.Length-1) do
                                      yield b.[n]
                                      if n<(b.Length-1) then yield tab.[n+1].code
                                  ])
                            |> (fun s -> String.Join(",",s))
                        p.codewrite("read("+fp+",\"("+format+")\",iostat="+iostat.code+") "+code+"\n")
                        for (t,m,b) in varlist do
                            match t with
                            |Zt ->
                                b <== tmp.[2*m+1]+asm.uj*tmp.[2*m+2]
                            |_ ->
                                ()
                    if Nz>0 then tmp.deallocate()
            |C ->
                ch.d1 (I <| 2*Nz) <| fun tmp ->
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
                                |St ->
                                    yield "%s"
                                |_ -> ()
                              ])
                        |> (fun s -> String.Join("",s))
                    let code =
                      varlist
                      |> (fun b ->
                            [for (t,m,a) in b do
                                match t,a with 
                                |Zt,Var _ ->
                                    yield "&"+tmp.[2*m+1].code
                                    yield "&"+tmp.[2*m+2].code
                                |_,Var(_,n) ->
                                    yield "&"+n
                                |_ ->
                                    Console.WriteLine("ファイル読み込みデータの保存先が変数ではありません")
                                    yield ""
                            ])
                      |> (fun s -> String.Join(",",s))
                    p.codewrite("fscanf("+fp+",\""+format+"\","+code+");\n")
                    for (t,m,b) in varlist do
                        match t with
                        |Zt ->
                            b <== tmp.[2*m+1]+asm.uj*tmp.[2*m+2]
                        |_ ->
                            ()
            |T ->
                let double0string_format_F = 
                    let (a,b)=p.dFormat
                    "E"+a.ToString()+"."+b.ToString()+"e3"
                let format = 
                    lst
                    |> List.map (fun b -> 
                        match b.etype with
                          |It _ ->"I"+p.iFormat.ToString()
                          |Dt -> double0string_format_F
                          |St -> "A"
                          |_ -> "")
                    |> (fun s -> String.Join(",",s))
                let code =
                    lst
                    |> List.map (fun b ->
                        match b with 
                        |Var(_,n) -> n
                        |Formula(_,n) -> n 
                        |_ -> "")
                    |> (fun s -> String.Join(",",s))
                p.codewrite("read("+fp+",\"("+format+")\",iostat="+iostat.code+") "+code+"\n")
            |H ->
                let double0string_format_F = 
                    let (a,b)=p.dFormat
                    "E"+a.ToString()+"."+b.ToString()+"e3"
                let format = 
                    lst
                    |> List.map (fun b -> 
                        match b.etype with
                        |It _ ->"I"+p.iFormat.ToString()
                        |Dt -> double0string_format_F
                        |St -> "A"
                        |_ -> "")
                    |> (fun s -> String.Join(",",s))
                let code =
                    lst
                    |> List.map (fun b ->
                        match b with 
                        |Var(_,n) -> n
                        |Formula(_,n) -> n 
                        |_ -> "")
                    |> (fun s -> String.Join("<mo>,</mo>",s))
                p.codewrite("Read(text): \\("+code+" \\leftarrow "+fp+"\\)<br/>\n")
            |P ->
                ch.d1 (I <| 2*Nz) <| fun tmp ->
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
                                |St ->
                                    yield "%s"
                                |_ -> ()
                              ])
                        |> (fun s -> String.Join("",s))
                    let code =
                      varlist
                      |> (fun b ->
                            [for (t,m,a) in b do
                                match t,a with 
                                |Zt,Var _ ->
                                    yield tmp.[2*m+1].code
                                    yield tmp.[2*m+2].code
                                |_,Var(_,n) ->
                                    yield n
                                |_ ->
                                    Console.WriteLine("ファイル読み込みデータの保存先が変数ではありません")
                                    yield ""
                            ])
                      |> (fun s -> String.Join(",",s))
                    //書式指定をしてファイルから値を読み込み。まだ、完成してない
                    p.codewrite("lines = "+fp+".readline()\n")
                    p.codewrite("word_list = re.split(r\'[\\t\\n]\', lines)\n")

                    let mutable cnt = 0
                    for (t,_,a) in varlist do
                        let cnt_string = string cnt
                        //let a_string = string a
                        match t with
                        |It _ ->
                            p.codewrite(a.code+" = int(word_list["+cnt_string+"])")
                        |Dt -> 
                            p.codewrite(a.code+"= float(word_list["+cnt_string+"])")
                        |Zt -> 
                            p.codewrite(a.code+" = complex(float(word_list["+cnt_string+"]),float(word_list["+cnt_string+"+1]))")
                            cnt <- cnt+1
                        |_ -> ()
                        cnt <- cnt+1
                
        static member private Read_bin (fp:string) (iostat:num0) (v:num0) = 
            match p.lang with
            |F ->
                match v.etype,v with 
                |Zt,Var _ ->
                    ch.dd <| fun (re,im) ->
                        p.codewrite("read("+fp+",iostat="+iostat.code+") "+re.code+"\n")
                        p.codewrite("read("+fp+",iostat="+iostat.code+") "+im.code+"\n")
                        v <== re+asm.uj*im
                |_,Var(_,n) ->
                    p.codewrite("read("+fp+",iostat="+iostat.code+") "+n+"\n")
                |_ -> 
                    Console.WriteLine("ファイル読み込みデータの保存先が変数ではありません")
            |C->
                match v.etype,v with 
                |Zt,Var _ ->
                    ch.dd <| fun (re,im) ->
                        p.codewrite("fread(&"+re.code+",sizeof("+re.code+"),1,"+fp+");"+"\n")
                        p.codewrite("fread(&"+im.code+",sizeof("+im.code+"),1,"+fp+");"+"\n")
                        v <== re+asm.uj*im
                |_,Var(_,n) ->
                    p.codewrite("fread(&"+n+",sizeof("+n+"),1,"+fp+");"+"\n")
                |_ -> 
                    Console.WriteLine("ファイル読み込みデータの保存先が変数ではありません")
            |T ->
                match v.etype,v with 
                |Zt,Var _ ->
                    ch.dd <| fun (re,im) ->
                        p.codewrite("read("+fp+",iostat="+iostat.code+") "+re.code+"\n")
                        p.codewrite("read("+fp+",iostat="+iostat.code+") "+im.code+"\n")
                        v <== re+asm.uj*im
                |_,Var(_,n) ->
                    p.codewrite("read("+fp+",iostat="+iostat.code+") "+n+"\n")
                |_ -> 
                    Console.WriteLine("ファイル読み込みデータの保存先が変数ではありません")
            |H ->
                match v with 
                |Var(_,n) ->
                    p.codewrite("Read(binary): \\("+n+" \\leftarrow "+fp+"\\)<br/>\n")
                |_ -> 
                    Console.WriteLine("ファイル読み込みデータの保存先が変数ではありません")
            |P->
                match v.etype,v with 
                |Zt,Var _ ->
                    ch.dd <| fun (re,im) ->
                        p.codewrite(re.code+" = struct.unpack('d', "+fp+".read(8))[0]"+"\n")
                        p.codewrite(im.code+" = struct.unpack('d', "+fp+".read(8))[0]"+"\n")
                        v <== re+asm.uj*im
                |It _,Var(_,n) ->
                    p.codewrite(n+" = struct.unpack('i', "+fp+".read(4))[0]"+"\n")
                |Dt,Var(_,n) ->
                    p.codewrite(n+" = struct.unpack('d', "+fp+".read(8))[0]"+"\n")
                |_ -> 
                    Console.WriteLine("ファイル読み込みデータの保存先が変数ではありません")
                    
        static member private Read_byte (fp:string) (iostat:num0) (e:num0) = 
            p.codewrite("read("+fp+", iostat="+iostat.code+") byte_tmp\n")
            let ee =
                match e.etype,e with 
                |It _,Var(_,n) -> n 
                |_ -> "byte値を整数型以外の変数に格納できません"
            p.codewrite(ee + "=" + "byte_tmp\n")
            
        ///<summary>ファイル出力</summary>
        static member fileOutput (filename:num0 list) code =
            io.fileAccess filename false false <| fun fp ->
                code(io.Write fp)
                
        ///<summary>ファイル出力</summary>
        static member binfileOutput (filename:num0 list) code =
            io.fileAccess filename false true <| fun fp ->
                code(io.Write_bin fp)

        ///<summary>ファイル読み込み</summary>
        static member fileInput (filename:num0 list) code =
            ch.i <| fun iostat ->
                io.fileAccess filename true false <| fun fp ->
                    code(io.Read fp iostat)
                
        ///<summary>バイナリファイルの読み込み</summary>
        static member binfileInput (filename:num0 list) code =
            ch.i <| fun iostat ->
                io.fileAccess filename true true <| fun fp ->
                    code(io.Read_bin fp iostat)
                    
        ///<summary>テキストファイルの行数をカウント</summary>
        static member file_LineCount (counter:num0) (filename:num0 list) varlist =
            ch.i <| fun iostat ->
                io.fileAccess filename true false <| fun fp ->
                    iter.loop <| fun (ext,i) ->
                        io.Read fp iostat varlist
                        br.branch <| fun b ->
                            b.IF (iostat .< 0) <| fun () ->
                                counter <== i-1
                                ext()
                                
        ///<summary>ファイルの読み込み</summary>
        static member file_Read (filename:num0 list) varlist code =
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
        static member save_text (f:num3) =
            fun filename ->
                io.fileOutput filename <| fun w -> 
                    iter.array f <| fun (i,j,k) ->
                        w [i;j;k;f.[i,j,k]]
                        
        ///<summary>配列をファイルに保存</summary>
        static member save_text (f:num2) =
            fun filename ->
                io.fileOutput filename <| fun w -> 
                    iter.array f <| fun (i,j) ->
                        w [i;j;f.[i,j]]
                        
        ///<summary>配列をファイルに保存</summary>
        static member save_text (f:num1) =
            fun filename ->
                io.fileOutput filename <| fun w -> 
                    iter.array f <| fun i -> 
                        w [i;f.[i]]

        ///<summary>数値をファイルに保存</summary>
        static member save_text (f:num0) =
            fun filename ->
                io.fileOutput filename <| fun w -> 
                    w [f]
                    
        ///<summary>数値をファイルに保存</summary>
        static member save (f:num0) =
            fun filename ->
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
        static member save (f:num1) =
            fun filename ->
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
                        match f[1].etype with
                        |Zt ->
                            w f[i].re
                            w f[i].im
                        |_ ->
                            w f[i]
                            
        ///<summary>2次元データをファイルに保存</summary>
        static member save (f:num2) =
            fun filename ->
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
                            match f[1,1].etype with
                            |Zt ->
                                w f[i,j].re
                                w f[i,j].im
                            |_ ->
                                w f[i,j]
                            
        ///<summary>3次元データをファイルに保存</summary>
        static member save (f:num3) =
            fun filename ->
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
                    w _3
                    //データサイズ
                    w f.size1
                    w f.size2
                    w f.size3
                    //データ本体
                    iter.num f.size3 <| fun k ->
                        iter.num f.size2 <| fun j ->
                            iter.num f.size1 <| fun i ->
                                match f[1,1,1].etype with
                                |Zt ->
                                    w f[i,j,k].re
                                    w f[i,j,k].im
                                |_ ->
                                    w f[i,j,k]
                                    
        ///<summary>数値をファイルから読み込み</summary>
        static member load (f:num0) =
            fun filename ->
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
                                print.s <| filename@[!.": invalid data type"]
                io.binfileInput filename <| fun r ->
                ch.i <| fun n ->
                    //データフォーマット
                    r n
                    br.branch <| fun b ->
                        b.IF (n.=1) <| fun () ->
                            match f.etype with
                            |Etype.It(4) ->
                                reader r (1004,f.etype)
                            |Etype.Dt    -> 
                                reader r (2000,f.etype)
                            |Etype.Zt    -> 
                                reader r (3000,f.etype)
                            |_ -> 
                                print.t "invalid data type"
                                  
        ///<summary>1次元データをファイルから読み込み</summary>
        static member load (f:num1) =
            fun filename ->
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
                                print.s <| filename@[!.": invalid data type"]
                io.binfileInput filename <| fun r ->
                ch.i <| fun n ->
                    //データフォーマット
                    r n
                    br.branch <| fun b ->
                        b.IF (n.=1) <| fun () ->
                            match f[1].etype with
                            |Etype.It(4) ->
                                reader r (1004,f[1].etype)
                            |Etype.Dt    -> 
                                reader r (2000,f[1].etype)
                            |Etype.Zt    -> 
                                reader r (3000,f[1].etype)
                            |_ -> 
                                print.t "invalid data type"
                                  
        ///<summary>2次元データをファイルから読み込み</summary>
        static member load (f:num2) =
            fun filename ->
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
                                print.s <| filename@[!.": invalid data type"]
                                print.cc n (I nt)
                io.binfileInput filename <| fun r ->
                ch.i <| fun n ->
                    //データフォーマット
                    r n
                    br.branch <| fun b ->
                        b.IF (n.=1) <| fun () ->
                            match f[1,1].etype with
                            |Etype.It(4) ->
                                reader r (1004,f[1,1].etype)
                            |Etype.Dt    -> 
                                reader r (2000,f[1,1].etype)
                            |Etype.Zt    -> 
                                reader r (3000,f[1,1].etype)
                            |_ -> 
                                print.t "invalid data type"
                                  
        ///<summary>3次元データをファイルから読み込み</summary>
        static member load (f:num3) =
            fun filename ->
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
                                print.s <| filename@[!.": invalid data type"]
                io.binfileInput filename <| fun r ->
                ch.i <| fun n ->
                    //データフォーマット
                    r n
                    br.branch <| fun b ->
                        b.IF (n.=1) <| fun () ->
                            match f[1,1,1].etype with
                            |Etype.It(4) ->
                                reader r (1004,f[1,1,1].etype)
                            |Etype.Dt    -> 
                                reader r (2000,f[1,1,1].etype)
                            |Etype.Zt    -> 
                                reader r (3000,f[1,1,1].etype)
                            |_ -> 
                                  print.t "invalid data type"
                                  
    ///<summary>区切り文字・スペースなしでファイル出力</summary>
    type io2 () =
        
        static member private cat (con:string) (lst:string list) = [0..lst.Length-1] |> List.fold (fun acc i -> acc + (if i=0 then "" else con) + lst.[i]) ""
            
        static member private fileAccess (filename:num0 list) readmode isbinary code =
            match p.lang with
            |F ->
                ch.f <| fun fp ->
                    let f = 
                        filename
                        |> List.map (fun s -> match s.etype with |St -> "A" |It _ -> "I"+p.iFormat.ToString() |_ -> "")
                        |> io2.cat ","
                    let s = 
                        filename
                        |> List.map (fun s -> s.code)
                        |> io2.cat ","
                    ch.t <| A0 <| fun id ->
                        let btname = "byte_tmp"
                        //変数byte_tmpをリストに追加（存在していない場合のみ）
                        p.var.setUniqVar(Structure("integer(1)"),A0,btname,"")
                        p.codewrite("write("+id+",\"("+f+")\") "+s+"\n")
                        p.getloopvar <| fun counter ->
                            p.codewrite("do "+counter+" = 1, len_trim("+id+")"+"\n")
                            p.codewrite("  if ( "+id+"( "+counter+":"+counter+" ).EQ.\" \" ) "+id+"( "+counter+":"+counter+" ) = \"0\""+"\n")
                            p.codewrite("end do"+"\n")
                        if isbinary then
                            p.codewrite("open("+fp+", file=trim("+id+"), access='stream', form='unformatted')"+"\n")
                        else
                            p.codewrite("open("+fp+", file=trim("+id+"))"+"\n")
                        code(fp)
                        p.codewrite("close("+fp+")"+"\n")
            |C ->
                ch.f <| fun fp ->
                    let f = 
                        filename
                        |> List.map (fun s -> match s,s.etype with |Str_c(v),_ -> v |_,It _ -> "%"+p.iFormat.ToString("00")+"d" |_ -> "")
                        |> io2.cat ""
                    let s = 
                        [for s in filename do
                            match s.etype with
                            |St -> ()
                            |_ -> yield s.code]
                        |> io2.cat ","
                    ch.t <| A0 <| fun id ->
                        let btname = "byte_tmp"
                        //変数byte_tmpをリストに追加（存在していない場合のみ）
                        p.var.setUniqVar(Structure("char"),A0,btname,"")
                        p.codewrite("sprintf("+id+",\""+f+"\""+(if s="" then "" else ",")+s+");\n")
                        if isbinary then
                            p.codewrite(fp+" = "+"fopen("+id+",\""+(if readmode then "rb" else "wb")+"\");"+"\n")
                        else
                            p.codewrite(fp+" = "+"fopen("+id+",\""+(if readmode then "r" else "w")+"\");"+"\n")
                        code(fp)
                        p.codewrite("fclose("+fp+")"+";\n")
            |T ->
                ch.f <| fun fp ->
                    let f = 
                        filename
                        |> List.map (fun s -> match s.etype with |St -> "%s" |It _ -> "%"+p.iFormat.ToString("00")+"d" |_ -> "")
                        |> io2.cat ","
                    let s = 
                        filename
                        |> List.map (fun s -> s.code)
                        |> io2.cat ","
                    ch.t <| A0 <| fun id ->
                        let btname = "byte_tmp"
                        //変数byte_tmpをリストに追加（存在していない場合のみ）
                        p.var.setUniqVar(Structure("char"),A0,btname,"")
                        p.codewrite("sprintf("+id+",\""+f+"\","+s+");\n")
                        if isbinary then
                            p.codewrite(fp+" = "+"fopen("+id+",\""+(if readmode then "rb" else "wb")+"\");"+"\n")
                        else
                            p.codewrite(fp+" = "+"fopen("+id+",\""+(if readmode then "r" else "w")+"\");"+"\n")
                        code(fp)
                        p.codewrite("fclose $"+fp+" "+"$\n")
            |H ->
                ch.f <| fun fp ->
                    let f = 
                        filename
                        |> List.map (fun s -> match s.etype with |St -> "%s" |It _ -> "%"+p.iFormat.ToString("00")+"d" |_ -> "")
                        |> (fun s -> String.Join("",s))
                    let s = 
                        filename
                        |> List.map (fun s -> s.code)
                        |> (fun s -> String.Join(",",s))
                    ch.t <| A0 <| fun id ->
                        let btname = "byte_tmp"
                        //変数byte_tmpをリストに追加（存在していない場合のみ）
                        p.var.setUniqVar(Structure("char"),A0,btname,"")
                        p.codewrite("sprintf("+id+",\""+f+"\","+s+");\n")
                        if isbinary then
                            p.codewrite("\\("+fp+"\\)"+" = "+"fopen("+id+",\""+(if readmode then "rb" else "wb")+"\");"+"\n")
                        else
                            p.codewrite("\\("+fp+"\\)"+" = "+"fopen("+id+",\""+(if readmode then "r" else "w")+"\");"+"\n")
                        code(fp)
                        p.codewrite("fclose \\("+fp+" "+"\\)\n")
            |P ->
                ch.f <| fun fp ->
                    let f = 
                        filename
                        |> List.map (fun s -> match s,s.etype with |Str_c(v),_ -> v |_,It _ -> "%"+p.iFormat.ToString("00")+"d" |_ -> "")
                        |> io2.cat ""
                    let s = 
                        [for s in filename do
                            match s.etype with
                            |St -> ()
                            |_ -> yield s.code]
                        |> io2.cat ","
                    ch.t <| A0 <| fun id ->
                        let btname = "byte_tmp"
                        //変数byte_tmpをリストに追加（存在していない場合のみ）
                        p.var.setUniqVar(Structure("char"),A0,btname,"")
                        p.codewrite(id+"= \""+f+"\"%("+s+")\n")
                        if isbinary then
                            p.codewrite(fp+" = "+"open("+id+",mode=\""+(if readmode then "rb" else "wb")+"\")"+"\n")
                        else
                            p.codewrite(fp+" = "+"open("+id+",mode=\""+(if readmode then "r" else "w")+"\")"+"\n")
                        code(fp)
                        p.codewrite(fp+".close()"+"\n")
        static member private Write (fp:string) (lst:num0 list) =
            match p.lang with
            |F ->
                let format = 
                    lst
                    |> (fun b ->
                        [for n in 0..(b.Length-1) do
                            match b.[n].etype with
                            |It _ -> 
                                yield "I0"
                            |Dt ->
                                yield "F0.3"
                            |Zt ->
                                yield "F0.3"
                                yield "F0.3"
                            |St -> 
                                yield "A"
                            |_ -> ()
                        ])
                    |> io2.cat ","
                let code =
                    lst
                    |> (fun b ->
                        [for n in 0..(b.Length-1) do
                            match b.[n].etype,b.[n] with 
                            |It _,Int_c(v) -> yield p.ItoS(v)
                            |Dt  ,Int_c(v) -> yield p.DtoS(double v)
                            |_,Dbl_c(v) -> yield p.DtoS(v)
                            |_,Str_c(v) -> yield "\""+v.Replace("\"","\"\"")+"\""
                            |Zt,Var _ ->
                                yield (b.[n].re.code)
                                yield (b.[n].im.code)
                            |(It _|Dt),_ -> yield b[n].code
                            |_ -> () ])
                    |> io2.cat ","
                p.codewrite("write("+fp+",\"("+format+")\") "+code+"\n")
            |C ->
                let int0string_format_C = "%d"
                let double0string_format_C = "%.3f"
                let format = 
                    lst
                    |> (fun b -> 
                        [for n in 0..(b.Length-1) do
                            match b.[n],b.[n].etype with
                            |_,It _ ->
                                yield "%d"
                            |_,Dt ->
                                yield "%.3f"
                            |_,Zt ->
                                yield "%.3f"
                                yield "%.3f"
                            |Str_c(v),_ ->
                                yield v.Replace("\"","\\\"")
                            |_ -> ()
                        ])
                    |> io2.cat ""
                let code =
                    [for b in lst do
                        match b.etype,b with 
                        |_,Int_c(v) -> yield p.ItoS(v)
                        |_,Dbl_c(v) -> yield p.DtoS(v)
                        |Zt,Var _ ->
                            yield b.re.code
                            yield b.im.code
                        |(It _|Dt),_ -> yield b.code
                        |_ -> ()]
                    |> io2.cat ","
                p.codewrite("fprintf("+fp+",\""+format+"\\n\""+(if code ="" then "" else ",")+code+");\n")
            |T ->
                let code =
                    lst
                    |> List.map (fun b ->
                        match b.etype,b with 
                        |_,Int_c(v) -> p.ItoS(v)
                        |_,Dbl_c(v) -> p.DtoS(v)
                        |_,Str_c(v) -> "\""+v+"\""
                        |Zt,Var _ -> b.re.code+","+b.im.code
                        |_,Var(_,n) -> n
                        |_,Formula(_,n) -> n 
                        |_ -> "")
                    |> io2.cat ","
                p.codewrite("write("+fp+") "+code+"\n")
            |H ->
                let code =
                    lst
                    |> List.map (fun b ->
                        match b.etype,b with 
                        |_,Int_c(v) -> p.ItoS(v)
                        |_,Dbl_c(v) -> p.DtoS(v)
                        |_,Str_c(v) -> "\""+v+"\""
                        |Zt,Var _ -> b.re.code+","+b.im.code
                        |_,Var(_,n) -> n
                        |_,Formula(_,n) -> n 
                        |_ -> "")
                    |> io2.cat ","
                p.codewrite("Write(text): \\("+fp+" \\leftarrow "+code+"\\)<br/>\n")
            |P ->
                let int0string_format_C = "%d"
                let double0string_format_C = "%.3f"
                let format = 
                    lst
                    |> (fun b -> 
                        [for n in 0..(b.Length-1) do
                            match b.[n],b.[n].etype with
                            |_,It _ ->
                                yield "%d"
                            |_,Dt ->
                                yield "%.3f"
                            |_,Zt ->
                                yield "%.3f"
                                yield "%.3f"
                            |Str_c(v),_ ->
                                yield v.Replace("\"","\\\"")
                            |_ -> ()
                        ])
                    |> io2.cat ""
                let code =
                    [for b in lst do
                        match b.etype,b with 
                        |_,Int_c(v) -> yield p.ItoS(v)
                        |_,Dbl_c(v) -> yield p.DtoS(v)
                        |Zt,Var _ ->
                            yield b.re.code
                            yield b.im.code
                        |(It _|Dt),_ -> yield b.code
                        |_ -> ()]
                    |> io2.cat ","
                p.codewrite(fp+".write(\""+format+"\" , % ("+(if code ="" then "" else ",")+code+"))\n")
                
        ///<summary>ファイル出力</summary>
        static member fileOutput (filename:num0 list) code =
            io2.fileAccess filename false false <| fun fp ->
                code(io2.Write fp)
                
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
        