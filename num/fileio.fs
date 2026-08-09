//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    open System

    module private FileIoReadTarget =
        let require (ctx:Aqualis) target =
            match target with
            |RNvr(_,c) when c.CodeFile=None ->
                invalidOp "A file-read target is not associated with a GenerationContext."
            |RNvr(Var _ as value,targetContext) ->
                Aqualis.merge ctx targetContext |> ignore
                value,targetContext
            |RNvr _ ->
                invalidOp "A file-read target must be a variable."
            |RStr _ ->
                invalidOp "A file-read target must be a variable."

    type TextReader internal (ctx:Aqualis,fp:string,iostat:int0) =
        // let context() = ctx.RequireGenerationContext()
        // let program() = (context()).CurrentProgram
        let writein text = ctx.codewritein(text + "\n")
        member _.tt (lst:exprString) =
            let rec cpxvarlist list (s:list<reduceExprString>) counter =
                match s with
                |a::b ->
                    match a.etype with
                    |Zt -> cpxvarlist <| list@[Zt,counter,a] <| b <| counter+1
                    |t   -> cpxvarlist <| list@[t,0,a] <| b <| counter
                |[] -> counter,list
            let Nz,varlist = cpxvarlist [] lst.data 0
            lst.data
            |> List.iter (FileIoReadTarget.require ctx >> ignore)

            match ctx.language with
            |Fortran ->
                ctx.ch.dx (2*Nz) <| fun tmp ->
                    let double0string_format_F =
                        let a,b = ctx.numFormat.dFormat
                        "E"+a.ToString()+"."+b.ToString()+"e3"
                    let format =
                        varlist
                        |> (fun b ->
                            [for (t,_,_) in b do
                                match t with
                                |It _ ->
                                    yield "I"+ctx.numFormat.iFormat.ToString()
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
                    ctx.ch.ix (varlist.Length+Nz-1) <| fun tab ->
                        let code =
                            varlist
                            |> (fun b ->
                                [for t,m,b in b do
                                    match t,b with
                                    |Zt,RNvr(Var _,_) ->
                                        yield tmp[2*m  ].Expr.eval ctx
                                        yield tmp[2*m+1].Expr.eval ctx
                                    |_,RNvr(Var(_,n,_),_) ->
                                        yield n
                                    |_ ->
                                        printfn "ファイル読み込みデータの保存先が変数ではありません"
                                        yield ""
                                ])
                            |> (fun b ->
                                  [for n in 0..(b.Length-1) do
                                      yield b[n]
                                      if n<(b.Length-1) then yield tab[n].Expr.eval ctx
                                  ])
                            |> fun s -> String.Join(",",s)
                        writein("read("+fp+",\"("+format+")\",iostat="+iostat.Expr.eval ctx+") "+code+"\n")
                        for t,m,b in varlist do
                            match t,b with
                            |Zt,target ->
                                let value,targetContext =
                                    FileIoReadTarget.require ctx target
                                complex0(value,context=targetContext)
                                    <== tmp[2*m]+asm.uj*tmp[2*m+1]
                            |_ ->
                                ()
            |C99 ->
                ctx.ch.dx (2*Nz) <| fun tmp ->
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
                            [for t,m,a in b do
                                match t,a with
                                |Zt,RNvr(Var _,_) ->
                                    yield "&"+tmp[2*m  ].Expr.eval ctx
                                    yield "&"+tmp[2*m+1].Expr.eval ctx
                                |_,RNvr(Var(_,n,_),_) ->
                                    yield "&"+n
                                |_ ->
                                    printfn "ファイル読み込みデータの保存先が変数ではありません"
                                    yield ""
                            ])
                      |> fun s -> String.Join(",",s)
                    writein("fscanf("+fp+",\""+format+"\","+code+");\n")
                    for t,m,b in varlist do
                        match t,b with
                        |Zt,target ->
                            let value,targetContext =
                                FileIoReadTarget.require ctx target
                            complex0(value,context=targetContext)
                                <== tmp[2*m]+asm.uj*tmp[2*m+1]
                        |_ ->
                            ()
            |LaTeX ->
                let double0string_format_F =
                    let a,b = ctx.numFormat.dFormat
                    "E"+a.ToString()+"."+b.ToString()+"e3"
                let format =
                    lst.data
                    |> List.map (fun b ->
                        match b.etype with
                          |It _ ->"I"+ctx.numFormat.iFormat.ToString()
                          |Dt -> double0string_format_F
                          |_ -> "")
                    |> fun s -> String.Join(",",s)
                let code =
                    lst.data
                    |> List.map (fun b ->
                        match b with
                        |RNvr(Var(_,n,_),_) -> n
                        |_ -> "")
                    |> fun s -> String.Join(",",s)
                writein("read("+fp+",\"("+format+")\",iostat="+iostat.Expr.eval ctx+") "+code+"\n")
            |HTML ->
                let double0string_format_F =
                    let a,b = ctx.numFormat.dFormat
                    "E"+a.ToString()+"."+b.ToString()+"e3"
                let format =
                    lst.data
                    |> List.map (fun b ->
                        match b.etype with
                        |It _ ->"I"+ctx.numFormat.iFormat.ToString()
                        |Dt -> double0string_format_F
                        |_ -> "")
                    |> fun s -> String.Join(",",s)
                let code =
                    lst.data
                    |> List.map (fun b ->
                        match b with
                        |RNvr(Var(_,n,_),_) -> n
                        |_ -> "")
                    |> fun s -> String.Join("<mo>,</mo>",s)
                writein("Read(text): \\("+code+" \\leftarrow "+fp+"\\)<br/>\n")
            |Python ->
                ctx.ch.dx (2*Nz) <| fun tmp ->
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
                                match t,a with
                                |Zt,RNvr(Var _,_) ->
                                    yield tmp[2*m  ].Expr.eval ctx
                                    yield tmp[2*m+1].Expr.eval ctx
                                |_,RNvr(Var(_,n,_),_) ->
                                    yield n
                                |_ ->
                                    printfn "ファイル読み込みデータの保存先が変数ではありません"
                                    yield ""
                            ])
                      |> fun s -> String.Join(",",s)
                    //書式指定をしてファイルから値を読み込み。まだ、完成してない
                    writein("lines = " + fp + ".readline()\n")
                    writein "word_list = re.split(r\'[\\t\\n]\', lines)\n"
                    let mutable cnt = 0
                    for t,_,a in varlist do
                        //let a_string = string a
                        match t,a with
                        |It _,RNvr(v,_) ->
                            writein(v.eval ctx+" = int(word_list["+cnt.ToString()+"])")
                            cnt <- cnt + 1
                        |Dt,RNvr(v,_) ->
                            writein(v.eval ctx+"= float(word_list["+cnt.ToString()+"])")
                            cnt <- cnt + 1
                        |Zt,RNvr(v,_) ->
                            writein(v.eval ctx+" = complex(float(word_list["+cnt.ToString()+"]),float(word_list["+(cnt+1).ToString()+"]))")
                            cnt <- cnt + 2
                        |_ -> ()
            |_ -> ()

        member private _.ReadByte target =
            let e,_ = FileIoReadTarget.require ctx target
            writein("read("+fp+", iostat="+iostat.Expr.eval ctx+") byte_tmp\n")
            let ee =
                match e.etype,e with
                |It _,Var(_,n,_) -> n
                |_ -> "byte値を整数型以外の変数に格納できません"
            writein(ee + "=" + "byte_tmp\n")

        member this.t (x:int0) = this.tt (iv x)
        member this.t (x:double0) = this.tt (dv x)
        member this.t (x:complex0) = this.tt (zv x)
        member this.b (x:int0) = this.ReadByte(RNvr(x.Expr,x.Context))

    type BinReader internal (ctx:Aqualis,fp:string,iostat:int0) =
        // let context() = ctx.RequireGenerationContext()
        // let program() = (context()).CurrentProgram
        let writein text = ctx.codewritein(text + "\n")
        member private _.ReadBin target =
            let v,targetContext =
                FileIoReadTarget.require ctx target
            match ctx.language with
            |Fortran ->
                match v.etype,v with
                |Zt,Var _ ->
                    ctx.ch.dd <| fun (re,im) ->
                        writein("read("+fp+",iostat="+iostat.Expr.eval ctx+") "+re.Expr.eval ctx+"\n")
                        writein("read("+fp+",iostat="+iostat.Expr.eval ctx+") "+im.Expr.eval ctx+"\n")
                        complex0(v,context=targetContext) <== re+asm.uj*im
                |_,Var(_,n,_) ->
                    writein("read("+fp+",iostat="+iostat.Expr.eval ctx+") "+n+"\n")
                |_ ->
                    Console.WriteLine "ファイル読み込みデータの保存先が変数ではありません"
            |C99 ->
                match v.etype,v with
                |Zt,Var _ ->
                    ctx.ch.dd <| fun (re,im) ->
                        writein("fread(&"+re.Expr.eval ctx+",sizeof("+re.Expr.eval ctx+"),1,"+fp+");"+"\n")
                        writein("fread(&"+im.Expr.eval ctx+",sizeof("+im.Expr.eval ctx+"),1,"+fp+");"+"\n")
                        complex0(v,context=targetContext) <== re+asm.uj*im
                |_,Var(_,n,_) ->
                    writein("fread(&"+n+",sizeof("+n+"),1,"+fp+");"+"\n")
                |_ ->
                    printfn "ファイル読み込みデータの保存先が変数ではありません"
            |LaTeX ->
                match v.etype,v with
                |Zt,Var _ ->
                    ctx.ch.dd <| fun (re,im) ->
                        writein("read("+fp+",iostat="+iostat.Expr.eval ctx+") "+re.Expr.eval ctx+"\n")
                        writein("read("+fp+",iostat="+iostat.Expr.eval ctx+") "+im.Expr.eval ctx+"\n")
                        complex0(v,context=targetContext) <== re+asm.uj*im
                |_,Var(_,n,_) ->
                    writein("read("+fp+",iostat="+iostat.Expr.eval ctx+") "+n+"\n")
                |_ ->
                    printfn "ファイル読み込みデータの保存先が変数ではありません"
            |HTML ->
                match v with
                |Var(_,n,_) ->
                    writein("Read(binary): \\("+n+" \\leftarrow "+fp+"\\)<br/>\n")
                |_ ->
                    printfn "ファイル読み込みデータの保存先が変数ではありません"
            |Python ->
                match v.etype,v with
                |Zt,Var _ ->
                    ctx.ch.dd <| fun (re,im) ->
                        writein(re.Expr.eval ctx+" = struct.unpack('d', "+fp+".read(8))[0]"+"\n")
                        writein(im.Expr.eval ctx+" = struct.unpack('d', "+fp+".read(8))[0]"+"\n")
                        complex0(v,context=targetContext) <== re+asm.uj*im
                |It _,Var(_,n,_) ->
                    writein(n+" = struct.unpack('i', "+fp+".read(4))[0]"+"\n")
                |Dt,Var(_,n,_) ->
                    writein(n+" = struct.unpack('d', "+fp+".read(8))[0]"+"\n")
                |_ ->
                    printfn "ファイル読み込みデータの保存先が変数ではありません"
            |_ -> ()
        member this.b (x:int0) = this.ReadBin(RNvr(x.Expr,x.Context))
        member this.b (x:double0) = this.ReadBin(RNvr(x.Expr,x.Context))
        member this.b (x:complex0) = this.ReadBin(RNvr(x.Expr,x.Context))

    type TextWriter internal (ctx:Aqualis,fp:string) =
        // let context() = ctx.RequireGenerationContext()
        // let program() = (context()).CurrentProgram
        let writein text = ctx.codewritein(text + "\n")
        member _.tt (lst:exprString) =
            match ctx.language with
            |Fortran ->
                let tab = ctx.var.i0NoWarning("tab",2313)
                let double0string_format_F =
                    let a,b = ctx.numFormat.dFormat
                    "E"+a.ToString()+"."+b.ToString()+"e3"
                let format =
                    lst
                    |> (fun b ->
                        [for n in b.data do
                            match n,n.etype with
                            |RStr _,_ ->
                                yield "A1"
                            |RNvr _, It 4 ->
                                yield "I"+ctx.numFormat.iFormat.ToString()
                            |RNvr _, Dt ->
                                yield double0string_format_F
                            |RNvr _, Zt ->
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
                        [for n in b.data do
                            match n.etype,n with
                            |It _,RNvr(Int v,_) -> yield ctx.numFormat.ItoS(v)
                            |Dt  ,RNvr(Int v,_) -> yield ctx.numFormat.DtoS(double v)
                            |_   ,RNvr(Dbl v,_) -> yield ctx.numFormat.DtoS v
                            |Zt  ,RNvr(v,_) ->
                                let z = complex0 v
                                yield z.re.Expr.eval ctx
                                yield z.im.Expr.eval ctx
                            |It _,RNvr(v,_) -> yield v.eval ctx
                            |Dt  ,RNvr(v,_) -> yield v.eval ctx
                            |_ -> ()])
                    |> (fun b ->
                          [for n in 0..(b.Length-1) do
                              yield b.[n]
                              if n<(b.Length-1) then yield tab.Expr.eval ctx
                          ])
                    |> fun s -> String.Join(",",s)
                writein("write("+fp+",\"("+format+")\") "+code+"\n")
            |C99 ->
                let int0string_format_C =
                    "%"+ctx.numFormat.iFormat.ToString()+"d"
                let double0string_format_C =
                    let a,b = ctx.numFormat.dFormat
                    "%"+a.ToString()+"."+b.ToString()+"e"
                let format =
                    lst
                    |> (fun b ->
                        [for n in b.data do
                            match n,n.etype with
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
                    [for b in lst.data do
                        match b.etype,b with
                        |_,RNvr(Int v,_) -> yield ctx.numFormat.ItoS v
                        |_,RNvr(Dbl v,_) -> yield ctx.numFormat.DtoS v
                        |Zt,RNvr(v,_) ->
                            let z = complex0 v
                            yield z.re.Expr.eval ctx
                            yield z.im.Expr.eval ctx
                        |(It _|Dt),RNvr(v,_) -> yield v.eval ctx
                        |_ -> ()]
                    |> fun s -> String.Join(",",s)
                writein("fprintf("+fp+",\""+format+"\\n\""+(if code ="" then "" else ",")+code+");\n")
            |LaTeX ->
                let code =
                    lst.data
                    |> List.map (fun b ->
                        match b.etype,b with
                          |_,RNvr(Int v,_) -> ctx.numFormat.ItoS v
                          |_,RNvr(Dbl v,_) -> ctx.numFormat.DtoS v
                          |Zt,RNvr(v,_) ->
                              let z = complex0 v
                              z.re.Expr.eval ctx+","+z.im.Expr.eval ctx
                          |(It _|Dt),RNvr(v,_) -> v.eval ctx
                          |_ -> "")
                    |> fun s -> String.Join(",",s)
                writein("Write(text): \\("+fp+" \\leftarrow "+code+"\\)\n")
            |HTML ->
                let code =
                    lst.data
                    |> List.map (fun b ->
                        match b.etype,b with
                          |_,RNvr(Int v,_) -> ctx.numFormat.ItoS v
                          |_,RNvr(Dbl v,_) -> ctx.numFormat.DtoS v
                          |Zt,RNvr(v,_) ->
                              let z = complex0 v
                              z.re.Expr.eval ctx+","+z.im.Expr.eval ctx
                          |(It _ |Dt),RNvr(v,_) -> v.eval ctx
                          |_ -> "")
                    |> fun s -> String.Join(",",s)
                writein("Write(text): \\("+fp+" \\leftarrow "+code+"\\)<br/>")
            |Python ->
                let int0string_format_C =
                    "%"+ctx.numFormat.iFormat.ToString()+"d"
                let double0string_format_C =
                    let a,b = ctx.numFormat.dFormat
                    "%"+a.ToString()+"."+b.ToString()+"e"
                let format =
                    lst.data
                    |> (fun b ->
                        [for n in b do
                            match n,n.etype with
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
                    [for b in lst.data do
                        match b.etype,b with
                        |_,RNvr(Int v,_) -> yield ctx.numFormat.ItoS v
                        |_,RNvr(Dbl v,_) -> yield ctx.numFormat.DtoS v
                        |Zt,RNvr(v,_) ->
                            let z = complex0 v
                            yield z.re.Expr.eval ctx
                            yield z.im.Expr.eval ctx
                        |(It _|Dt),RNvr(v,_) -> yield v.eval ctx
                        |_ -> ()]
                    |> fun s -> String.Join(",",s)
                writein(fp+".write(\""+format+"\\n\" %("+code+"))\n")
            |_ -> ()

        member this.t (x:string) = this.tt (st x)
        member this.t (x:int0) = this.tt (iv x)
        member this.t (x:double0) = this.tt (dv x)
        member this.t (x:complex0) = this.tt (zv x)
        member this.t (x:int) = this.tt (iv (I x))
        member this.t (x:double) = this.tt (dv (D x))

        member _.cc (lst:exprString) =
            match ctx.language with
            |Fortran ->
                let tab = ctx.var.i0NoWarning("tab",2313)
                let int0string_format_F = "I0"
                let double0string_format_F = "G0"
                let format =
                    lst.data
                    |> (fun b ->
                        [for n in 0..(b.Length-1) do
                            match b[n],b[n].etype with
                            |_,It _ ->
                                yield int0string_format_F
                            |_,Dt ->
                                yield double0string_format_F
                            |_,Zt ->
                                yield double0string_format_F
                                yield double0string_format_F
                            |RStr _,_ ->
                                yield "A"
                            |_ -> ()
                        ])
                    |> fun s -> String.Join(",",s)
                let code =
                    lst.data
                    |> (fun b ->
                        [for n in 0..(b.Length-1) do
                            match b[n],b[n].etype with
                            |RNvr(Int v,_), It _ -> yield ctx.numFormat.ItoS(v)
                            |RNvr(Int v,_), Dt   -> yield ctx.numFormat.DtoS(double v)
                            |RNvr(Dbl v,_), _    -> yield ctx.numFormat.DtoS v
                            |RNvr(v,_), Zt   ->
                                yield (Re v).eval ctx
                                yield (Im v).eval ctx
                            |RNvr(v,_),(It _|Dt) -> yield v.eval ctx
                            |RStr v,_ -> yield "\"" + v.Replace("\"","\"\"") + "\""
                            |_ -> ()])
                    |> fun s -> String.Join(",",s)
                writein("write("+fp+",\"("+format+")\") "+code+"\n")
            |C99 ->
                let int0string_format_C = "%d"
                let double0string_format_C = "%.17g"
                let format =
                    lst.data
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
                            |RStr v,_ ->
                                yield v.Replace("\"","\\\"")
                            |_ -> ()
                        ])
                    |> fun s -> String.Join("",s)
                let code =
                    [for b in lst.data do
                        match b.etype,b with
                        |It _,RNvr(Int v,_) ->
                            yield ctx.numFormat.ItoS v
                        |Dt ,RNvr(Int v,_) ->
                            yield ctx.numFormat.DtoS (double v)
                        |_ ,RNvr(Dbl v,_) ->
                            yield ctx.numFormat.DtoS v
                        |Zt ,RNvr(v,_) ->
                            yield (Re v).eval ctx
                            yield (Im v).eval ctx
                        |(It _|Dt),RNvr(v,_) ->
                            yield v.eval ctx
                        |_ -> ()]
                    |> fun s -> String.Join(",",s)
                writein("fprintf("+fp+",\""+format+"\\n\""+(if code ="" then "" else ",")+code+");\n")
            |LaTeX ->
                let code =
                    lst.data
                    |> List.map (fun b ->
                        match b,b.etype with
                          |RNvr(Int v,_),It _ -> ctx.numFormat.ItoS v
                          |RNvr(Int v,_),Dt -> ctx.numFormat.DtoS (double v)
                          |RNvr(Dbl v,_),_ -> ctx.numFormat.DtoS v
                          |RNvr(v,_),Zt -> (Re v).eval ctx+","+(Im v).eval ctx
                          |RNvr(v,_),(It _|Dt) -> v.eval ctx
                          |RStr v,_ -> "\"" + v.Replace("\"","\\\"") + "\""
                          |_ -> "")
                    |> fun s -> String.Join(",",s)
                writein("Write(text): \\("+fp+" \\leftarrow "+code+"\\)\n")
            |HTML ->
                let code =
                    lst.data
                    |> List.map (fun b ->
                        match b,b.etype with
                          |RNvr(Int v,_),It _ -> ctx.numFormat.ItoS v
                          |RNvr(Int v,_),Dt -> ctx.numFormat.DtoS(double v)
                          |RNvr(Dbl v,_),_ -> ctx.numFormat.DtoS v
                          |RNvr(v,_),Zt -> (Re v).eval ctx+","+(Im v).eval ctx
                          |RNvr(v,_),(It _ |Dt) -> v.eval ctx
                          |RStr v,_ -> "\"" + v.Replace("\"","\\\"") + "\""
                          |_ -> "")
                    |> fun s -> String.Join(",",s)
                writein("Write(text): \\("+fp+" \\leftarrow "+code+"\\)<br/>")
            |Python ->
                let int0string_format_P = "%d"
                let double0string_format_P = "%.17g"
                let format =
                    lst.data
                    |> (fun b ->
                        [for n in 0..(b.Length-1) do
                            match b.[n],b.[n].etype with
                            |_,It _ ->
                                yield int0string_format_P
                            |_,Dt ->
                                yield double0string_format_P
                            |_,Zt ->
                                yield double0string_format_P
                                yield double0string_format_P
                            |RStr v,_ ->
                                yield v.Replace("\"","\\\"")
                            |_ -> ()
                        ])
                    |> (fun b ->
                          [for n in 0..(b.Length-1) do
                              yield b.[n]
                              if n<(b.Length-1) then yield "\\t"
                          ])
                    |> fun s -> String.Join("",s)
                let code =
                    [for b in lst.data do
                        match b.etype,b with
                        |It _,RNvr(Int v,_) -> yield ctx.numFormat.ItoS v
                        |Dt,RNvr(Int v,_) -> yield ctx.numFormat.DtoS(double v)
                        |_,RNvr(Dbl v,_) -> yield ctx.numFormat.DtoS v
                        |Zt,RNvr(v,_) ->
                            yield (Re v).eval ctx
                            yield (Im v).eval ctx
                        |(It _|Dt),RNvr(v,_) -> yield v.eval ctx
                        |_ -> ()]
                    |> fun s -> String.Join(",",s)
                writein(fp+".write(\""+format+"\\n\" %("+code+"))\n")
            |_ -> ()

    type BinWriter internal (ctx:Aqualis,fp:string) =
        // let context() = ctx.RequireGenerationContext()
        // let program() = (context()).CurrentProgram
        let writein text = ctx.codewritein(text + "\n")
        member private _.WriteBin (v:expr) =
            match ctx.language with
            |Fortran ->
                match v.etype,v with
                |_,Int v ->
                    writein("write("+fp+") "+ctx.numFormat.ItoS(v)+"\n")
                |_,Dbl v ->
                    writein("write("+fp+") "+ctx.numFormat.DtoS(v)+"\n")
                |Zt,_ ->
                    let z = complex0 v
                    writein("write("+fp+") "+z.re.Expr.eval ctx+"\n")
                    writein("write("+fp+") "+z.im.Expr.eval ctx+"\n")
                |It _,_ ->
                    writein("write("+fp+") "+v.eval ctx+"\n")
                |Dt,_ ->
                    writein("write("+fp+") "+v.eval ctx+"\n")
                |_ -> ()
            |C99 ->
                match v.etype,v with
                |_,Int _ ->
                    ctx.ch.i <| fun tmp ->
                        tmp <== int0 v
                        writein("fwrite(&"+tmp.Expr.eval ctx+",sizeof("+tmp.Expr.eval ctx+"),1,"+fp+");\n")
                |_,Dbl _ ->
                    ctx.ch.d <| fun tmp ->
                        tmp <== double0 v
                        writein("fwrite(&"+tmp.Expr.eval ctx+",sizeof("+tmp.Expr.eval ctx+"),1,"+fp+");\n")
                |Zt,_ ->
                    ctx.ch.dd <| fun (tmp_r,tmp_i) ->
                        let z = complex0 v
                        tmp_r <== z.re
                        tmp_i <== z.im
                        writein("fwrite(&"+tmp_r.Expr.eval ctx+",sizeof("+tmp_r.Expr.eval ctx+"),1,"+fp+");\n")
                        writein("fwrite(&"+tmp_i.Expr.eval ctx+",sizeof("+tmp_i.Expr.eval ctx+"),1,"+fp+");\n")
                |It _,_ ->
                    ctx.ch.i <| fun tmp ->
                        tmp <== int0 v
                        writein("fwrite(&"+tmp.Expr.eval ctx+",sizeof("+tmp.Expr.eval ctx+"),1,"+fp+");\n")
                |Dt,_ ->
                    ctx.ch.d <| fun tmp ->
                        tmp <== double0 v
                        writein("fwrite(&"+tmp.Expr.eval ctx+",sizeof("+tmp.Expr.eval ctx+"),1,"+fp+");\n")
                |_ ->
                    ()
            |LaTeX ->
                match v.etype,v with
                |_,Int v ->
                    writein("write("+fp+") "+ctx.numFormat.ItoS(v)+"\n")
                |_,Dbl v ->
                    writein("write("+fp+") "+ctx.numFormat.DtoS(v)+"\n")
                |Zt,_ ->
                    let z = complex0 v
                    writein("write("+fp+") "+z.re.Expr.eval ctx+"\n")
                    writein("write("+fp+") "+z.im.Expr.eval ctx+"\n")
                |It _,_ ->
                    writein("write("+fp+") "+v.eval ctx+"\n")
                |Dt,_ ->
                    writein("write("+fp+") "+v.eval ctx+"\n")
                |_ -> ()
            |HTML ->
                match v.etype,v with
                |_,Int v ->
                    writein("Write(binary): \\("+fp+" \\leftarrow "+ctx.numFormat.ItoS(v)+"\\)<br/>\n")
                |_,Dbl v ->
                    writein("Write(binary): \\("+fp+" \\leftarrow "+ctx.numFormat.DtoS(v)+"\\)<br/>\n")
                |Zt,_ ->
                    let z = complex0 v
                    writein("Write(binary): \\("+fp+" \\leftarrow "+z.re.Expr.eval ctx+"\\)<br/>\n")
                    writein("Write(binary): \\("+fp+" \\leftarrow "+z.im.Expr.eval ctx+"\\)<br/>\n")
                |It _,_ ->
                    writein("Write(binary): \\("+fp+" \\leftarrow "+v.eval ctx+"\\)<br/>\n")
                |Dt,_ ->
                    writein("Write(binary): \\("+fp+" \\leftarrow "+v.eval ctx+"\\)<br/>\n")
                |_ -> ()
            |Python ->
                match v.etype,v with
                |_,Int _ ->
                    ctx.ch.i <| fun tmp ->
                        tmp <== int0 v
                        writein(fp+".write(struct.pack('i', "+tmp.Expr.eval ctx+"))\n")
                |_,Dbl _ ->
                    ctx.ch.d <| fun tmp ->
                        tmp <== double0 v
                        writein(fp+".write(struct.pack('d', "+tmp.Expr.eval ctx+"))\n")
                |Zt,_ ->
                    ctx.ch.dd <| fun (tmp_r,tmp_i) ->
                        let z = complex0 v
                        tmp_r <== z.re
                        tmp_i <== z.im
                        writein(fp+".write(struct.pack('d', "+tmp_r.Expr.eval ctx+"))\n")
                        writein(fp+".write(struct.pack('d', "+tmp_i.Expr.eval ctx+"))\n")
                |It _,_ ->
                    ctx.ch.i <| fun tmp ->
                        tmp <== int0 v
                        writein(fp+".write(struct.pack('i', "+tmp.Expr.eval ctx+"))\n")
                |Dt,_ ->
                    ctx.ch.d <| fun tmp ->
                        tmp <== double0 v
                        writein(fp+".write(struct.pack('d', "+tmp.Expr.eval ctx+"))\n")
                |_ ->
                    ()
            |_ -> ()
        member this.b (v:int) = this.WriteBin ((I v).Expr)
        member this.b (v:int0) = this.WriteBin v.Expr
        member this.b (v:double) = this.WriteBin ((D v).Expr)
        member this.b (v:double0) = this.WriteBin v.Expr
        member this.b (v:complex0) = this.WriteBin v.Expr

    ///<summary>ファイル入出力</summary>
    type ContextIo internal (ctx:Aqualis) =
        // let context() = ctx.RequireGenerationContext()
        // let program() = (context()).CurrentProgram
        let writein text = ctx.codewritein(text + "\n")

        member private this.fileAccess (filename:exprString,intDigit:option<int>) readmode isbinary code =
            match ctx.language with
            |Fortran ->
                ctx.ch.f <| fun fp ->
                    let f =
                        filename.data
                        |> List.map (fun s ->
                            match s with
                            |RStr _ ->
                                "A"
                            |RNvr(x,_) when x.etype = It 4 ->
                                "I" + match intDigit with |None -> ctx.numFormat.iFormat.ToString() |Some n -> n.ToString()
                            |_ ->
                                "")
                        |> fun s -> String.Join(",",s)
                    let s =
                        filename.data
                        |> List.map (fun s ->
                            match s with
                            |RStr t ->
                                "\""+t+"\""
                            |RNvr(x,_) when x.etype = It 4 ->
                                x.eval ctx
                            |_ ->
                                "")
                        |> fun s -> String.Join(",",s)
                    ctx.ch.t <| A0 <| fun id ->
                        let btname = "byte_tmp"
                        //変数byte_tmpをリストに追加（存在していない場合のみ）
                        ctx.cvar.setUniqVar(Structure "integer(1)",A0,btname,"")
                        writein("write("+id+",\"("+f+")\") "+s+"\n")
                        ctx.ch.i <| fun counter ->
                            let c = counter.Expr.eval ctx
                            writein("do "+c+" = 1, len_trim("+id+")"+"\n")
                            writein("  if ( "+id+"( "+c+":"+c+" ).EQ.\" \" ) "+id+"( "+c+":"+c+" ) = \"0\""+"\n")
                            writein("end do"+"\n")
                        if isbinary then
                            writein("open("+fp+", file=trim("+id+"), access='stream', form='unformatted')"+"\n")
                        else
                            writein("open("+fp+", file=trim("+id+"))"+"\n")
                        code fp
                        writein("close("+fp+")"+"\n")
            |C99 ->
                ctx.ch.f <| fun fp ->
                    let f =
                        filename.data
                        |> List.map (fun s ->
                            match s with
                            |RStr t ->
                                t
                            |RNvr(x,_) when x.etype = It 4 ->
                                "%0" + (match intDigit with |None -> ctx.numFormat.iFormat.ToString() |Some n -> n.ToString()) + "d"
                            |_ ->
                                "")
                        |> List.filter (fun s -> s<>"")
                        |> fun s -> String.Join("",s)
                    let s =
                        filename.data
                        |> List.map (fun s ->
                            match s with
                            |RStr _ ->
                                ""
                            |RNvr(x,_) when x.etype = It 4 ->
                                x.eval ctx
                            |_ ->
                                "")
                        |> List.filter (fun s -> s<>"")
                        |> fun s -> String.Join(",",s)
                    ctx.ch.t <| A0 <| fun id ->
                        let btname = "byte_tmp"
                        //変数byte_tmpをリストに追加（存在していない場合のみ）
                        ctx.cvar.setUniqVar(Structure "char",A0,btname,"")
                        writein("sprintf("+id+",\""+f+"\""+(if s="" then "" else ",")+s+");\n")
                        if isbinary then
                            writein(fp+" = "+"fopen("+id+",\""+(if readmode then "rb" else "wb")+"\");"+"\n")
                        else
                            writein(fp+" = "+"fopen("+id+",\""+(if readmode then "r" else "w")+"\");"+"\n")
                        code fp
                        writein("fclose("+fp+")"+";\n")
            |LaTeX ->
                ctx.ch.f <| fun fp ->
                    let s =
                        filename.data
                        |> List.map (fun s ->
                            match s with
                            |RStr t ->
                                "\""+t+"\""
                            |RNvr(x,_) when x.etype = It 4 ->
                                x.eval ctx
                            |_ ->
                                "")
                        |> List.filter (fun s -> s<>"")
                        |> fun s -> String.Join("+",s)
                    ctx.ch.t <| A0 <| fun id ->
                        let btname = "byte_tmp"
                        if isbinary then
                            writein(fp+" = "+"open binary file("+id+",\""+(if readmode then "rb" else "wb")+"\");"+"\n")
                        else
                            writein(fp+" = "+"open text file("+id+",\""+(if readmode then "r" else "w")+"\");"+"\n")
                        code fp
                        writein("close("+fp+")"+";\n")
            |HTML ->
                ctx.ch.f <| fun fp ->
                    let s =
                        filename.data
                        |> List.map (fun s ->
                            match s with
                            |RStr t ->
                                "\""+t+"\""
                            |RNvr(x,_) when x.etype = It 4 ->
                                x.eval ctx
                            |_ ->
                                "")
                        |> List.filter (fun s -> s<>"")
                        |> fun s -> String.Join("+",s)
                    ctx.ch.t <| A0 <| fun id ->
                        let btname = "byte_tmp"
                        if isbinary then
                            writein(fp+" = "+"open binary file("+id+",\""+(if readmode then "rb" else "wb")+"\");"+"\n")
                        else
                            writein(fp+" = "+"open text file("+id+",\""+(if readmode then "r" else "w")+"\");"+"\n")
                        code fp
                        writein("close("+fp+")"+";\n")
            |Python ->
                ctx.ch.f <| fun fp ->
                    let f =
                        filename.data
                        |> List.map (fun s ->
                            match s with
                            |RStr t ->
                                t
                            |RNvr(x,_) when x.etype = It 4 ->
                                "%0" + (match intDigit with |None -> ctx.numFormat.iFormat.ToString() |Some n -> n.ToString()) + "d"
                            |_ ->
                                "")
                        |> List.filter (fun s -> s<>"")
                        |> fun s -> String.Join("",s)
                    let s =
                        filename.data
                        |> List.map (fun s ->
                            match s with
                            |RStr _ ->
                                ""
                            |RNvr(x,_) when x.etype = It 4 ->
                                x.eval ctx
                            |_ ->
                                "")
                        |> List.filter (fun s -> s<>"")
                        |> fun s -> String.Join(",",s)
                    ctx.ch.t <| A0 <| fun id ->
                        let btname = "byte_tmp"
                        //変数byte_tmpをリストに追加（存在していない場合のみ）
                        ctx.cvar.setUniqVar(Structure "char",A0,btname,"")
                        writein(id+"= \""+f+"\"%("+s+")\n")
                        if isbinary then
                            writein(fp+" = "+"open("+id+",mode=\""+(if readmode then "rb" else "wb")+"\")"+"\n")
                        else
                            writein(fp+" = "+"open("+id+",mode=\""+(if readmode then "r" else "w")+"\")"+"\n")
                        code(fp)
                        writein(fp+".close()"+"\n")
            |_ -> ()

        member private this.Write1 (fp:string) (lst:exprString) =
            match ctx.language with
            |Fortran ->
                let tab:int0 = ctx.var.i0NoWarning("tab",2313)
                let double0string_format_F =
                    let a,b = ctx.numFormat.dFormat
                    "E"+a.ToString()+"."+b.ToString()+"e3"
                let format =
                    lst
                    |> (fun b ->
                        [for n in b.data do
                            match n,n.etype with
                            |RStr _,_ ->
                                yield "A1"
                            |RNvr _, It 4 ->
                                yield "I"+ctx.numFormat.iFormat.ToString()
                            |RNvr _, Dt ->
                                yield double0string_format_F
                            |RNvr _, Zt ->
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
                        [for n in b.data do
                            match n.etype,n with
                            |It _,RNvr(Int v,_) -> yield ctx.numFormat.ItoS(v)
                            |Dt  ,RNvr(Int v,_) -> yield ctx.numFormat.DtoS(double v)
                            |_   ,RNvr(Dbl v,_) -> yield ctx.numFormat.DtoS v
                            |Zt  ,RNvr(v,_) ->
                                let z = complex0 v
                                yield z.re.Expr.eval ctx
                                yield z.im.Expr.eval ctx
                            |It _,RNvr(v,_) -> yield v.eval ctx
                            |Dt  ,RNvr(v,_) -> yield v.eval ctx
                            |_ -> ()])
                    |> (fun b ->
                          [for n in 0..(b.Length-1) do
                              yield b.[n]
                              if n<(b.Length-1) then yield tab.Expr.eval ctx
                          ])
                    |> fun s -> String.Join(",",s)
                writein("write("+fp+",\"("+format+")\") "+code+"\n")
            |C99 ->
                let int0string_format_C =
                    "%"+ctx.numFormat.iFormat.ToString()+"d"
                let double0string_format_C =
                    let a,b = ctx.numFormat.dFormat
                    "%"+a.ToString()+"."+b.ToString()+"e"
                let format =
                    lst
                    |> (fun b ->
                        [for n in b.data do
                            match n,n.etype with
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
                    [for b in lst.data do
                        match b.etype,b with
                        |_,RNvr(Int v,_) -> yield ctx.numFormat.ItoS v
                        |_,RNvr(Dbl v,_) -> yield ctx.numFormat.DtoS v
                        |Zt,RNvr(v,_) ->
                            let z = complex0 v
                            yield z.re.Expr.eval ctx
                            yield z.im.Expr.eval ctx
                        |(It _|Dt),RNvr(v,_) -> yield v.eval ctx
                        |_ -> ()]
                    |> fun s -> String.Join(",",s)
                writein("fprintf("+fp+",\""+format+"\\n\""+(if code ="" then "" else ",")+code+");\n")
            |LaTeX ->
                let code =
                    lst.data
                    |> List.map (fun b ->
                        match b.etype,b with
                          |_,RNvr(Int v,_) -> ctx.numFormat.ItoS v
                          |_,RNvr(Dbl v,_) -> ctx.numFormat.DtoS v
                          |Zt,RNvr(v,_) ->
                              let z = complex0 v
                              z.re.Expr.eval ctx+","+z.im.Expr.eval ctx
                          |(It _|Dt),RNvr(v,_) -> v.eval ctx
                          |_ -> "")
                    |> fun s -> String.Join(",",s)
                writein("Write(text): \\("+fp+" \\leftarrow "+code+"\\)\n")
            |HTML ->
                let code =
                    lst.data
                    |> List.map (fun b ->
                        match b.etype,b with
                          |_,RNvr(Int v,_) -> ctx.numFormat.ItoS v
                          |_,RNvr(Dbl v,_) -> ctx.numFormat.DtoS v
                          |Zt,RNvr(v,_) ->
                              let z = complex0 v
                              z.re.Expr.eval ctx+","+z.im.Expr.eval ctx
                          |(It _ |Dt),RNvr(v,_) -> v.eval ctx
                          |_ -> "")
                    |> fun s -> String.Join(",",s)
                writein("Write(text): \\("+fp+" \\leftarrow "+code+"\\)<br/>")
            |Python ->
                let int0string_format_C =
                    "%"+ctx.numFormat.iFormat.ToString()+"d"
                let double0string_format_C =
                    let a,b = ctx.numFormat.dFormat
                    "%"+a.ToString()+"."+b.ToString()+"e"
                let format =
                    lst.data
                    |> (fun b ->
                        [for n in b do
                            match n,n.etype with
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
                    [for b in lst.data do
                        match b.etype,b with
                        |_,RNvr(Int v,_) -> yield ctx.numFormat.ItoS v
                        |_,RNvr(Dbl v,_) -> yield ctx.numFormat.DtoS v
                        |Zt,RNvr(v,_) ->
                            let z = complex0 v
                            yield z.re.Expr.eval ctx
                            yield z.im.Expr.eval ctx
                        |(It _|Dt),RNvr(v,_) -> yield v.eval ctx
                        |_ -> ()]
                    |> fun s -> String.Join(",",s)
                writein(fp+".write(\""+format+"\\n\" %("+code+"))\n")
            |_ -> ()

        member private this.Write2 (fp:string) (lst:exprString) =
            match ctx.language with
            |Fortran ->
                let tab = ctx.var.i0NoWarning("tab",2313)
                let int0string_format_F = "I0"
                let double0string_format_F = "G0"
                let format =
                    lst.data
                    |> (fun b ->
                        [for n in 0..(b.Length-1) do
                            match b[n],b[n].etype with
                            |_,It _ ->
                                yield int0string_format_F
                            |_,Dt ->
                                yield double0string_format_F
                            |_,Zt ->
                                yield double0string_format_F
                                yield double0string_format_F
                            |RStr _,_ ->
                                yield "A"
                            |_ -> ()
                        ])
                    |> fun s -> String.Join(",",s)
                let code =
                    lst.data
                    |> (fun b ->
                        [for n in 0..(b.Length-1) do
                            match b[n],b[n].etype with
                            |RNvr(Int v,_), It _ -> yield ctx.numFormat.ItoS(v)
                            |RNvr(Int v,_), Dt   -> yield ctx.numFormat.DtoS(double v)
                            |RNvr(Dbl v,_), _    -> yield ctx.numFormat.DtoS v
                            |RNvr(v,_), Zt   ->
                                yield (Re v).eval ctx
                                yield (Im v).eval ctx
                            |RNvr(v,_),(It _|Dt) -> yield v.eval ctx
                            |RStr v,_ -> yield "\"" + v.Replace("\"","\"\"") + "\""
                            |_ -> ()])
                    |> fun s -> String.Join(",",s)
                writein("write("+fp+",\"("+format+")\") "+code+"\n")
            |C99 ->
                let int0string_format_C = "%d"
                let double0string_format_C = "%.17g"
                let format =
                    lst.data
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
                            |RStr v,_ ->
                                yield v.Replace("\"","\\\"")
                            |_ -> ()
                        ])
                    |> fun s -> String.Join("",s)
                let code =
                    [for b in lst.data do
                        match b.etype,b with
                        |It _,RNvr(Int v,_) ->
                            yield ctx.numFormat.ItoS v
                        |Dt ,RNvr(Int v,_) ->
                            yield ctx.numFormat.DtoS (double v)
                        |_ ,RNvr(Dbl v,_) ->
                            yield ctx.numFormat.DtoS v
                        |Zt ,RNvr(v,_) ->
                            yield (Re v).eval ctx
                            yield (Im v).eval ctx
                        |(It _|Dt),RNvr(v,_) ->
                            yield v.eval ctx
                        |_ -> ()]
                    |> fun s -> String.Join(",",s)
                writein("fprintf("+fp+",\""+format+"\\n\""+(if code ="" then "" else ",")+code+");\n")
            |LaTeX ->
                let code =
                    lst.data
                    |> List.map (fun b ->
                        match b,b.etype with
                          |RNvr(Int v,_),It _ -> ctx.numFormat.ItoS v
                          |RNvr(Int v,_),Dt -> ctx.numFormat.DtoS (double v)
                          |RNvr(Dbl v,_),_ -> ctx.numFormat.DtoS v
                          |RNvr(v,_),Zt -> (Re v).eval ctx+","+(Im v).eval ctx
                          |RNvr(v,_),(It _|Dt) -> v.eval ctx
                          |RStr v,_ -> "\"" + v.Replace("\"","\\\"") + "\""
                          |_ -> "")
                    |> fun s -> String.Join(",",s)
                writein("Write(text): \\("+fp+" \\leftarrow "+code+"\\)\n")
            |HTML ->
                let code =
                    lst.data
                    |> List.map (fun b ->
                        match b,b.etype with
                          |RNvr(Int v,_),It _ -> ctx.numFormat.ItoS v
                          |RNvr(Int v,_),Dt -> ctx.numFormat.DtoS(double v)
                          |RNvr(Dbl v,_),_ -> ctx.numFormat.DtoS v
                          |RNvr(v,_),Zt -> (Re v).eval ctx+","+(Im v).eval ctx
                          |RNvr(v,_),(It _ |Dt) -> v.eval ctx
                          |RStr v,_ -> "\"" + v.Replace("\"","\\\"") + "\""
                          |_ -> "")
                    |> fun s -> String.Join(",",s)
                writein("Write(text): \\("+fp+" \\leftarrow "+code+"\\)<br/>")
            |Python ->
                let int0string_format_P = "%d"
                let double0string_format_P = "%.17g"
                let format =
                    lst.data
                    |> (fun b ->
                        [for n in 0..(b.Length-1) do
                            match b.[n],b.[n].etype with
                            |_,It _ ->
                                yield int0string_format_P
                            |_,Dt ->
                                yield double0string_format_P
                            |_,Zt ->
                                yield double0string_format_P
                                yield double0string_format_P
                            |RStr v,_ ->
                                yield v.Replace("\"","\\\"")
                            |_ -> ()
                        ])
                    |> (fun b ->
                          [for n in 0..(b.Length-1) do
                              yield b.[n]
                              if n<(b.Length-1) then yield "\\t"
                          ])
                    |> fun s -> String.Join("",s)
                let code =
                    [for b in lst.data do
                        match b.etype,b with
                        |It _,RNvr(Int v,_) -> yield ctx.numFormat.ItoS v
                        |Dt,RNvr(Int v,_) -> yield ctx.numFormat.DtoS(double v)
                        |_,RNvr(Dbl v,_) -> yield ctx.numFormat.DtoS v
                        |Zt,RNvr(v,_) ->
                            yield (Re v).eval ctx
                            yield (Im v).eval ctx
                        |(It _|Dt),RNvr(v,_) -> yield v.eval ctx
                        |_ -> ()]
                    |> fun s -> String.Join(",",s)
                writein(fp+".write(\""+format+"\\n\" %("+code+"))\n")
            |_ -> ()

        member private this.Write_bin (fp:string) (v:expr) =
            match ctx.language with
            |Fortran ->
                match v.etype,v with
                |_,Int v ->
                    writein("write("+fp+") "+ctx.numFormat.ItoS(v)+"\n")
                |_,Dbl v ->
                    writein("write("+fp+") "+ctx.numFormat.DtoS(v)+"\n")
                |Zt,_ ->
                    let z = complex0 v
                    writein("write("+fp+") "+z.re.Expr.eval ctx+"\n")
                    writein("write("+fp+") "+z.im.Expr.eval ctx+"\n")
                |It _,_ ->
                    writein("write("+fp+") "+v.eval ctx+"\n")
                |Dt,_ ->
                    writein("write("+fp+") "+v.eval ctx+"\n")
                |_ -> ()
            |C99 ->
                match v.etype,v with
                |_,Int _ ->
                    ctx.ch.i <| fun tmp ->
                        tmp <== int0 v
                        writein("fwrite(&"+tmp.Expr.eval ctx+",sizeof("+tmp.Expr.eval ctx+"),1,"+fp+");\n")
                |_,Dbl _ ->
                    ctx.ch.d <| fun tmp ->
                        tmp <== double0 v
                        writein("fwrite(&"+tmp.Expr.eval ctx+",sizeof("+tmp.Expr.eval ctx+"),1,"+fp+");\n")
                |Zt,_ ->
                    ctx.ch.dd <| fun (tmp_r,tmp_i) ->
                        let z = complex0 v
                        tmp_r <== z.re
                        tmp_i <== z.im
                        writein("fwrite(&"+tmp_r.Expr.eval ctx+",sizeof("+tmp_r.Expr.eval ctx+"),1,"+fp+");\n")
                        writein("fwrite(&"+tmp_i.Expr.eval ctx+",sizeof("+tmp_i.Expr.eval ctx+"),1,"+fp+");\n")
                |It _,_ ->
                    ctx.ch.i <| fun tmp ->
                        tmp <== int0 v
                        writein("fwrite(&"+tmp.Expr.eval ctx+",sizeof("+tmp.Expr.eval ctx+"),1,"+fp+");\n")
                |Dt,_ ->
                    ctx.ch.d <| fun tmp ->
                        tmp <== double0 v
                        writein("fwrite(&"+tmp.Expr.eval ctx+",sizeof("+tmp.Expr.eval ctx+"),1,"+fp+");\n")
                |_ ->
                    ()
            |LaTeX ->
                match v.etype,v with
                |_,Int v ->
                    writein("write("+fp+") "+ctx.numFormat.ItoS(v)+"\n")
                |_,Dbl v ->
                    writein("write("+fp+") "+ctx.numFormat.DtoS(v)+"\n")
                |Zt,_ ->
                    let z = complex0 v
                    writein("write("+fp+") "+z.re.Expr.eval ctx+"\n")
                    writein("write("+fp+") "+z.im.Expr.eval ctx+"\n")
                |It _,_ ->
                    writein("write("+fp+") "+v.eval ctx+"\n")
                |Dt,_ ->
                    writein("write("+fp+") "+v.eval ctx+"\n")
                |_ -> ()
            |HTML ->
                match v.etype,v with
                |_,Int v ->
                    writein("Write(binary): \\("+fp+" \\leftarrow "+ctx.numFormat.ItoS(v)+"\\)<br/>\n")
                |_,Dbl v ->
                    writein("Write(binary): \\("+fp+" \\leftarrow "+ctx.numFormat.DtoS(v)+"\\)<br/>\n")
                |Zt,_ ->
                    let z = complex0 v
                    writein("Write(binary): \\("+fp+" \\leftarrow "+z.re.Expr.eval ctx+"\\)<br/>\n")
                    writein("Write(binary): \\("+fp+" \\leftarrow "+z.im.Expr.eval ctx+"\\)<br/>\n")
                |It _,_ ->
                    writein("Write(binary): \\("+fp+" \\leftarrow "+v.eval ctx+"\\)<br/>\n")
                |Dt,_ ->
                    writein("Write(binary): \\("+fp+" \\leftarrow "+v.eval ctx+"\\)<br/>\n")
                |_ -> ()
            |Python ->
                match v.etype,v with
                |_,Int _ ->
                    ctx.ch.i <| fun tmp ->
                        tmp <== int0 v
                        writein(fp+".write(struct.pack('i', "+tmp.Expr.eval ctx+"))\n")
                |_,Dbl _ ->
                    ctx.ch.d <| fun tmp ->
                        tmp <== double0 v
                        writein(fp+".write(struct.pack('d', "+tmp.Expr.eval ctx+"))\n")
                |Zt,_ ->
                    ctx.ch.dd <| fun (tmp_r,tmp_i) ->
                        let z = complex0 v
                        tmp_r <== z.re
                        tmp_i <== z.im
                        writein(fp+".write(struct.pack('d', "+tmp_r.Expr.eval ctx+"))\n")
                        writein(fp+".write(struct.pack('d', "+tmp_i.Expr.eval ctx+"))\n")
                |It _,_ ->
                    ctx.ch.i <| fun tmp ->
                        tmp <== int0 v
                        writein(fp+".write(struct.pack('i', "+tmp.Expr.eval ctx+"))\n")
                |Dt,_ ->
                    ctx.ch.d <| fun tmp ->
                        tmp <== double0 v
                        writein(fp+".write(struct.pack('d', "+tmp.Expr.eval ctx+"))\n")
                |_ ->
                    ()
            |_ -> ()

        member private this.Read (fp:string) (iostat:int0) (lst:exprString) =
            let rec cpxvarlist list (s:list<reduceExprString>) counter =
                match s with
                |a::b ->
                    match a.etype with
                    |Zt -> cpxvarlist <| list@[Zt,counter,a] <| b <| counter+1
                    |t   -> cpxvarlist <| list@[t,0,a] <| b <| counter
                |[] -> counter,list
            let Nz,varlist = cpxvarlist [] lst.data 0
            lst.data
            |> List.iter (FileIoReadTarget.require ctx >> ignore)

            match ctx.language with
            |Fortran ->
                ctx.ch.dx (2*Nz) <| fun tmp ->
                    let double0string_format_F =
                        let a,b = ctx.numFormat.dFormat
                        "E"+a.ToString()+"."+b.ToString()+"e3"
                    let format =
                        varlist
                        |> (fun b ->
                            [for (t,_,_) in b do
                                match t with
                                |It _ ->
                                    yield "I"+ctx.numFormat.iFormat.ToString()
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
                    ctx.ch.ix (varlist.Length+Nz-1) <| fun tab ->
                        let code =
                            varlist
                            |> (fun b ->
                                [for t,m,b in b do
                                    match t,b with
                                    |Zt,RNvr(Var _,_) ->
                                        yield tmp[2*m  ].Expr.eval ctx
                                        yield tmp[2*m+1].Expr.eval ctx
                                    |_,RNvr(Var(_,n,_),_) ->
                                        yield n
                                    |_ ->
                                        printfn "ファイル読み込みデータの保存先が変数ではありません"
                                        yield ""
                                ])
                            |> (fun b ->
                                  [for n in 0..(b.Length-1) do
                                      yield b[n]
                                      if n<(b.Length-1) then yield tab[n].Expr.eval ctx
                                  ])
                            |> fun s -> String.Join(",",s)
                        writein("read("+fp+",\"("+format+")\",iostat="+iostat.Expr.eval ctx+") "+code+"\n")
                        for t,m,b in varlist do
                            match t,b with
                            |Zt,target ->
                                let value,targetContext =
                                    FileIoReadTarget.require ctx target
                                complex0(value,context=targetContext)
                                    <== tmp[2*m]+asm.uj*tmp[2*m+1]
                            |_ ->
                                ()
            |C99 ->
                ctx.ch.dx (2*Nz) <| fun tmp ->
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
                            [for t,m,a in b do
                                match t,a with
                                |Zt,RNvr(Var _,_) ->
                                    yield "&"+tmp[2*m  ].Expr.eval ctx
                                    yield "&"+tmp[2*m+1].Expr.eval ctx
                                |_,RNvr(Var(_,n,_),_) ->
                                    yield "&"+n
                                |_ ->
                                    printfn "ファイル読み込みデータの保存先が変数ではありません"
                                    yield ""
                            ])
                      |> fun s -> String.Join(",",s)
                    writein("fscanf("+fp+",\""+format+"\","+code+");\n")
                    for t,m,b in varlist do
                        match t,b with
                        |Zt,target ->
                            let value,targetContext =
                                FileIoReadTarget.require ctx target
                            complex0(value,context=targetContext)
                                <== tmp[2*m]+asm.uj*tmp[2*m+1]
                        |_ ->
                            ()
            |LaTeX ->
                let double0string_format_F =
                    let a,b = ctx.numFormat.dFormat
                    "E"+a.ToString()+"."+b.ToString()+"e3"
                let format =
                    lst.data
                    |> List.map (fun b ->
                        match b.etype with
                          |It _ ->"I"+ctx.numFormat.iFormat.ToString()
                          |Dt -> double0string_format_F
                          |_ -> "")
                    |> fun s -> String.Join(",",s)
                let code =
                    lst.data
                    |> List.map (fun b ->
                        match b with
                        |RNvr(Var(_,n,_),_) -> n
                        |_ -> "")
                    |> fun s -> String.Join(",",s)
                writein("read("+fp+",\"("+format+")\",iostat="+iostat.Expr.eval ctx+") "+code+"\n")
            |HTML ->
                let double0string_format_F =
                    let a,b = ctx.numFormat.dFormat
                    "E"+a.ToString()+"."+b.ToString()+"e3"
                let format =
                    lst.data
                    |> List.map (fun b ->
                        match b.etype with
                        |It _ ->"I"+ctx.numFormat.iFormat.ToString()
                        |Dt -> double0string_format_F
                        |_ -> "")
                    |> fun s -> String.Join(",",s)
                let code =
                    lst.data
                    |> List.map (fun b ->
                        match b with
                        |RNvr(Var(_,n,_),_) -> n
                        |_ -> "")
                    |> fun s -> String.Join("<mo>,</mo>",s)
                writein("Read(text): \\("+code+" \\leftarrow "+fp+"\\)<br/>\n")
            |Python ->
                ctx.ch.dx (2*Nz) <| fun tmp ->
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
                                match t,a with
                                |Zt,RNvr(Var _,_) ->
                                    yield tmp[2*m  ].Expr.eval ctx
                                    yield tmp[2*m+1].Expr.eval ctx
                                |_,RNvr(Var(_,n,_),_) ->
                                    yield n
                                |_ ->
                                    printfn "ファイル読み込みデータの保存先が変数ではありません"
                                    yield ""
                            ])
                      |> fun s -> String.Join(",",s)
                    //書式指定をしてファイルから値を読み込み。まだ、完成してない
                    writein("lines = " + fp + ".readline()\n")
                    writein "word_list = re.split(r\'[\\t\\n]\', lines)\n"
                    let mutable cnt = 0
                    for t,_,a in varlist do
                        //let a_string = string a
                        match t,a with
                        |It _,RNvr(v,_) ->
                            writein(v.eval ctx+" = int(word_list["+cnt.ToString()+"])")
                            cnt <- cnt + 1
                        |Dt,RNvr(v,_) ->
                            writein(v.eval ctx+"= float(word_list["+cnt.ToString()+"])")
                            cnt <- cnt + 1
                        |Zt,RNvr(v,_) ->
                            writein(v.eval ctx+" = complex(float(word_list["+cnt.ToString()+"]),float(word_list["+(cnt+1).ToString()+"]))")
                            cnt <- cnt + 2
                        |_ -> ()
            |_ -> ()

        member private this.Read_bin (fp:string) (iostat:int0) (v:expr) =
            match ctx.language with
            |Fortran ->
                match v.etype,v with
                |Zt,Var _ ->
                    ctx.ch.dd <| fun (re,im) ->
                        writein("read("+fp+",iostat="+iostat.Expr.eval ctx+") "+re.Expr.eval ctx+"\n")
                        writein("read("+fp+",iostat="+iostat.Expr.eval ctx+") "+im.Expr.eval ctx+"\n")
                        complex0 v <== re+asm.uj*im
                |_,Var(_,n,_) ->
                    writein("read("+fp+",iostat="+iostat.Expr.eval ctx+") "+n+"\n")
                |_ ->
                    Console.WriteLine "ファイル読み込みデータの保存先が変数ではありません"
            |C99 ->
                match v.etype,v with
                |Zt,Var _ ->
                    ctx.ch.dd <| fun (re,im) ->
                        writein("fread(&"+re.Expr.eval ctx+",sizeof("+re.Expr.eval ctx+"),1,"+fp+");"+"\n")
                        writein("fread(&"+im.Expr.eval ctx+",sizeof("+im.Expr.eval ctx+"),1,"+fp+");"+"\n")
                        complex0 v <== re+asm.uj*im
                |_,Var(_,n,_) ->
                    writein("fread(&"+n+",sizeof("+n+"),1,"+fp+");"+"\n")
                |_ ->
                    printfn "ファイル読み込みデータの保存先が変数ではありません"
            |LaTeX ->
                match v.etype,v with
                |Zt,Var _ ->
                    ctx.ch.dd <| fun (re,im) ->
                        writein("read("+fp+",iostat="+iostat.Expr.eval ctx+") "+re.Expr.eval ctx+"\n")
                        writein("read("+fp+",iostat="+iostat.Expr.eval ctx+") "+im.Expr.eval ctx+"\n")
                        complex0 v <== re+asm.uj*im
                |_,Var(_,n,_) ->
                    writein("read("+fp+",iostat="+iostat.Expr.eval ctx+") "+n+"\n")
                |_ ->
                    printfn "ファイル読み込みデータの保存先が変数ではありません"
            |HTML ->
                match v with
                |Var(_,n,_) ->
                    writein("Read(binary): \\("+n+" \\leftarrow "+fp+"\\)<br/>\n")
                |_ ->
                    printfn "ファイル読み込みデータの保存先が変数ではありません"
            |Python ->
                match v.etype,v with
                |Zt,Var _ ->
                    ctx.ch.dd <| fun (re,im) ->
                        writein(re.Expr.eval ctx+" = struct.unpack('d', "+fp+".read(8))[0]"+"\n")
                        writein(im.Expr.eval ctx+" = struct.unpack('d', "+fp+".read(8))[0]"+"\n")
                        complex0 v <== re+asm.uj*im
                |It _,Var(_,n,_) ->
                    writein(n+" = struct.unpack('i', "+fp+".read(4))[0]"+"\n")
                |Dt,Var(_,n,_) ->
                    writein(n+" = struct.unpack('d', "+fp+".read(8))[0]"+"\n")
                |_ ->
                    printfn "ファイル読み込みデータの保存先が変数ではありません"
            |_ -> ()

        member private this.Read_byte (fp:string) (iostat:int0) (e:expr) =
            writein("read("+fp+", iostat="+iostat.Expr.eval ctx+") byte_tmp\n")
            let ee =
                match e.etype,e with
                |It _,Var(_,n,_) -> n
                |_ -> "byte値を整数型以外の変数に格納できません"
            writein(ee + "=" + "byte_tmp\n")

        ///<summary>ファイル出力（タブ区切りデータ）</summary>
        member this.fileOutput (filename:exprString) = fun code ->
            this.fileAccess (filename,None) false false <| fun fp ->
                let writer = TextWriter(ctx,fp)
                code writer
        ///<summary>ファイル出力（タブ区切りデータ）</summary>
        member this.fileOutput (filename:exprString,intDigit:int) = fun code ->
            this.fileAccess (filename,Some intDigit) false false <| fun fp ->
                let writer = TextWriter(ctx,fp)
                code writer

        ///<summary>ファイル出力（タブ区切りデータ）</summary>
        member this.fileOutput (filename:string) = fun code -> this.fileOutput (st filename) code

        ///<summary>バイナリファイル出力</summary>
        member this.binfileOutput (filename:exprString) = fun code ->
            this.fileAccess (filename,None) false true <| fun fp ->
                let writer = BinWriter(ctx,fp)
                code writer
        ///<summary>バイナリファイル出力</summary>
        member this.binfileOutput (filename:exprString,intDigit:int) = fun code ->
            this.fileAccess (filename,Some intDigit) false true <| fun fp ->
                let writer = BinWriter(ctx,fp)
                code writer

        ///<summary>バイナリファイル出力</summary>
        member this.binfileOutput (filename:string) = fun code -> this.binfileOutput (st filename) code

        ///<summary>ファイル読み込み</summary>
        member this.fileInput (filename:exprString) = fun code ->
            ctx.ch.i <| fun iostat ->
                this.fileAccess (filename,None) true false <| fun fp ->
                    let reader = TextReader(ctx,fp,iostat)
                    code reader

        ///<summary>ファイル読み込み</summary>
        member this.fileInput (filename:exprString,intDigit:int) = fun code ->
            ctx.ch.i <| fun iostat ->
                this.fileAccess (filename,Some intDigit) true false <| fun fp ->
                    let reader = TextReader(ctx,fp,iostat)
                    code reader

        ///<summary>ファイル読み込み</summary>
        member this.fileInput (filename:string) = fun code ->
            this.fileInput (st filename) code

        ///<summary>バイナリファイルの読み込み</summary>
        member this.binfileInput (filename:exprString) = fun code ->
            ctx.ch.i <| fun iostat ->
                this.fileAccess (filename,None) true true <| fun fp ->
                    let reader = BinReader(ctx,fp,iostat)
                    code reader

        ///<summary>バイナリファイルの読み込み</summary>
        member this.binfileInput (filename:exprString,intDigit:int) = fun code ->
            ctx.ch.i <| fun iostat ->
                this.fileAccess (filename,Some intDigit) true true <| fun fp ->
                    let reader = BinReader(ctx,fp,iostat)
                    code reader

        ///<summary>バイナリファイルの読み込み</summary>
        member this.binfileInput (filename:string) = fun code -> this.binfileInput (st filename) code

        ///<summary>ファイルの読み込み</summary>
        member this.file_Read (filename:exprString) = fun varlist code ->
            ctx.ch.i <| fun iostat ->
                this.fileAccess (filename,None) true false <| fun fp ->
                    ctx.iter.loop <| fun (ext,i) ->
                        this.Read fp iostat varlist
                        ctx.br.branch <| fun b ->
                            b.IF (iostat .< 0) <| fun () ->
                                ext()
                            b.EL <| fun () ->
                                code(i)

        ///<summary>ファイルの読み込み</summary>
        member this.file_Read (filename:exprString,intDigit:int) = fun varlist code ->
            ctx.ch.i <| fun iostat ->
                this.fileAccess (filename,Some intDigit) true false <| fun fp ->
                    ctx.iter.loop <| fun (ext,i) ->
                        this.Read fp iostat varlist
                        ctx.br.branch <| fun b ->
                            b.IF (iostat .< 0) <| fun () ->
                                ext()
                            b.EL <| fun () ->
                                code(i)

        ///<summary>配列をファイルに保存</summary>
        member this.save_text (f:int3,filename:exprString) =
            this.fileOutput filename <| fun w ->
                f.foreach <| fun (i,j,k) ->
                    w.tt <| i++j++k++f[i,j,k]

        ///<summary>配列をファイルに保存</summary>
        member this.save_text (f:int2,filename:exprString) =
            this.fileOutput filename <| fun w ->
                f.foreach <| fun (i,j) ->
                    w.tt <| i++j++f[i,j]

        ///<summary>配列をファイルに保存</summary>
        member this.save_text (f:int1,filename:exprString) =
            this.fileOutput filename <| fun w ->
                f.foreach <| fun i ->
                    w.tt <| i++f[i]

        ///<summary>数値をファイルに保存</summary>
        member this.save_text (f:int0,filename:exprString) =
            this.fileOutput filename <| fun w ->
                w.t f

        ///<summary>配列をファイルに保存</summary>
        member this.save_text (f:double3,filename:exprString) =
            this.fileOutput filename <| fun w ->
                f.foreach <| fun (i,j,k) ->
                    w.tt <| i++j++k++f[i,j,k]

        ///<summary>配列をファイルに保存</summary>
        member this.save_text (f:double2,filename:exprString) =
            this.fileOutput filename <| fun w ->
                f.foreach <| fun (i,j) ->
                    w.tt <| i++j++f[i,j]

        ///<summary>配列をファイルに保存</summary>
        member this.save_text (f:double1,filename:exprString) =
            this.fileOutput filename <| fun w ->
                f.foreach <| fun i ->
                    w.tt <| i++f[i]

        ///<summary>数値をファイルに保存</summary>
        member this.save_text (f:double0,filename:exprString) =
            this.fileOutput filename <| fun w ->
                w.t f

        ///<summary>配列をファイルに保存</summary>
        member this.save_text (f:complex3,filename:exprString) =
            this.fileOutput filename <| fun w ->
                f.foreach <| fun (i,j,k) ->
                    w.tt <| i++j++k++f[i,j,k]

        ///<summary>配列をファイルに保存</summary>
        member this.save_text (f:complex2,filename:exprString) =
            this.fileOutput filename <| fun w ->
                f.foreach <| fun (i,j) ->
                    w.tt <| i++j++f[i,j]

        ///<summary>配列をファイルに保存</summary>
        member this.save_text (f:complex1,filename:exprString) =
            this.fileOutput filename <| fun w ->
                f.foreach <| fun i ->
                    w.tt <| i++f[i]

        ///<summary>数値をファイルに保存</summary>
        member this.save_text (f:complex0,filename:exprString) =
            this.fileOutput filename <| fun w ->
                w.t f

        ///<summary>配列をファイルに保存</summary>
        member this.save_text (f:int3,filename:string) = this.save_text(f,st filename)
        member this.save_text (f:int2,filename:string) = this.save_text(f,st filename)
        member this.save_text (f:int1,filename:string) = this.save_text(f,st filename)
        member this.save_text (f:int0,filename:string) = this.save_text(f,st filename)
        ///<summary>配列をファイルに保存</summary>
        member this.save_text (f:double3,filename:string) = this.save_text(f,st filename)
        member this.save_text (f:double2,filename:string) = this.save_text(f,st filename)
        member this.save_text (f:double1,filename:string) = this.save_text(f,st filename)
        member this.save_text (f:double0,filename:string) = this.save_text(f,st filename)
        ///<summary>配列をファイルに保存</summary>
        member this.save_text (f:complex3,filename:string) = this.save_text(f,st filename)
        member this.save_text (f:complex2,filename:string) = this.save_text(f,st filename)
        member this.save_text (f:complex1,filename:string) = this.save_text(f,st filename)
        member this.save_text (f:complex0,filename:string) = this.save_text(f,st filename)

        ///<summary>数値をファイルに保存</summary>
        member private this.save (f:expr,filename:exprString) =
            this.binfileOutput filename <| fun w ->
                //データフォーマット
                w.b _1
                //データ型
                match f.etype with
                |Etype.It 4  -> w.b 1004
                |Etype.Dt    -> w.b 2000
                |Etype.Zt    -> w.b 3000
                |_           -> w.b 0
                //データ次元
                w.b _0
                //データサイズ
                w.b _1
                //データ本体
                match f.etype with
                |Zt ->
                    w.b (complex0 f).re
                    w.b (complex0 f).im
                |Dt ->
                    w.b (double0 f)
                |It 4 ->
                    w.b (int0 f)
                |_ -> ()
        member private this.save (f:int0,filename:exprString) = this.save (f.Expr,filename)
        member private this.save (f:double0,filename:exprString) = this.save (f.Expr,filename)
        member private this.save (f:complex0,filename:exprString) = this.save (f.Expr,filename)

        ///<summary>1次元データをファイルに保存</summary>
        member this.save (f:int1,filename:exprString) =
                this.binfileOutput filename <| fun w ->
                    //データフォーマット
                    w.b _1
                    //データ型
                    match f.etype with
                    |Etype.It 4 -> w.b 1004
                    |Etype.Dt   -> w.b 2000
                    |Etype.Zt   -> w.b 3000
                    |_          -> w.b 0
                    //データ次元
                    w.b _1
                    //データサイズ
                    w.b f.size1
                    //データ本体
                    ctx.iter.num f.size1 <| fun i ->
                        w.b f[i]
        ///<summary>1次元データをファイルに保存</summary>
        member this.save (f:double1,filename:exprString) =
                this.binfileOutput filename <| fun w ->
                    //データフォーマット
                    w.b _1
                    //データ型
                    match f.etype with
                    |Etype.It 4 -> w.b 1004
                    |Etype.Dt   -> w.b 2000
                    |Etype.Zt   -> w.b 3000
                    |_          -> w.b 0
                    //データ次元
                    w.b _1
                    //データサイズ
                    w.b f.size1
                    //データ本体
                    ctx.iter.num f.size1 <| fun i ->
                        w.b f[i]
        ///<summary>1次元データをファイルに保存</summary>
        member this.save (f:complex1,filename:exprString) =
                this.binfileOutput filename <| fun w ->
                    //データフォーマット
                    w.b _1
                    //データ型
                    match f.etype with
                    |Etype.It 4  -> w.b 1004
                    |Etype.Dt    -> w.b 2000
                    |Etype.Zt    -> w.b 3000
                    |_           -> w.b 0
                    //データ次元
                    w.b _1
                    //データサイズ
                    w.b f.size1
                    //データ本体
                    ctx.iter.num f.size1 <| fun i ->
                        w.b f[i].re
                        w.b f[i].im

        ///<summary>2次元データをファイルに保存</summary>
        member this.save (f:int2,filename:exprString) =
            this.binfileOutput filename <| fun w ->
                //データフォーマット
                w.b _1
                //データ型
                match f.etype with
                |Etype.It 4 -> w.b 1004
                |Etype.Dt   -> w.b 2000
                |Etype.Zt   -> w.b 3000
                |_          -> w.b 0
                //データ次元
                w.b _2
                //データサイズ
                w.b f.size1
                w.b f.size2
                //データ本体
                ctx.iter.num f.size2 <| fun j ->
                    ctx.iter.num f.size1 <| fun i ->
                        w.b f[i,j]


        ///<summary>2次元データをファイルに保存</summary>
        member this.save (f:double2,filename:exprString) =
            this.binfileOutput filename <| fun w ->
                //データフォーマット
                w.b _1
                //データ型
                match f.etype with
                |Etype.It 4 -> w.b 1004
                |Etype.Dt   -> w.b 2000
                |Etype.Zt   -> w.b 3000
                |_          -> w.b 0
                //データ次元
                w.b _2
                //データサイズ
                w.b f.size1
                w.b f.size2
                //データ本体
                ctx.iter.num f.size2 <| fun j ->
                    ctx.iter.num f.size1 <| fun i ->
                        w.b f[i,j]


        ///<summary>2次元データをファイルに保存</summary>
        member this.save (f:complex2,filename:exprString) =
            this.binfileOutput filename <| fun w ->
                //データフォーマット
                w.b _1
                //データ型
                match f.etype with
                |Etype.It 4 -> w.b 1004
                |Etype.Dt   -> w.b 2000
                |Etype.Zt   -> w.b 3000
                |_          -> w.b 0
                //データ次元
                w.b _2
                //データサイズ
                w.b f.size1
                w.b f.size2
                //データ本体
                ctx.iter.num f.size2 <| fun j ->
                    ctx.iter.num f.size1 <| fun i ->
                        w.b f[i,j].re
                        w.b f[i,j].im

        ///<summary>3次元データをファイルに保存</summary>
        member this.save (f:int3,filename:exprString) =
            this.binfileOutput filename <| fun w ->
                //データフォーマット
                w.b _1
                //データ型
                match f.etype with
                |Etype.It 4 -> w.b 1004
                |Etype.Dt   -> w.b 2000
                |Etype.Zt   -> w.b 3000
                |_          -> w.b 0
                //データ次元
                w.b _3
                //データサイズ
                w.b f.size1
                w.b f.size2
                w.b f.size3
                //データ本体
                ctx.iter.num f.size3 <| fun k ->
                    ctx.iter.num f.size2 <| fun j ->
                        ctx.iter.num f.size1 <| fun i ->
                            w.b f[i,j,k]

        ///<summary>3次元データをファイルに保存</summary>
        member this.save (f:double3,filename:exprString) =
            this.binfileOutput filename <| fun w ->
                //データフォーマット
                w.b _1
                //データ型
                match f.etype with
                |Etype.It 4 -> w.b 1004
                |Etype.Dt   -> w.b 2000
                |Etype.Zt   -> w.b 3000
                |_          -> w.b 0
                //データ次元
                w.b _3
                //データサイズ
                w.b f.size1
                w.b f.size2
                w.b f.size3
                //データ本体
                ctx.iter.num f.size3 <| fun k ->
                    ctx.iter.num f.size2 <| fun j ->
                        ctx.iter.num f.size1 <| fun i ->
                            w.b f[i,j,k]

        ///<summary>3次元データをファイルに保存</summary>
        member this.save (f:complex3,filename:exprString) =
            this.binfileOutput filename <| fun w ->
                //データフォーマット
                w.b _1
                //データ型
                match f.etype with
                |Etype.It 4 -> w.b 1004
                |Etype.Dt   -> w.b 2000
                |Etype.Zt   -> w.b 3000
                |_          -> w.b 0
                //データ次元
                w.b _3
                //データサイズ
                w.b f.size1
                w.b f.size2
                w.b f.size3
                //データ本体
                ctx.iter.num f.size3 <| fun k ->
                    ctx.iter.num f.size2 <| fun j ->
                        ctx.iter.num f.size1 <| fun i ->
                            w.b f[i,j,k].re
                            w.b f[i,j,k].im
        member this.save (f:int3,filename:string) = this.save(f,st filename)
        member this.save (f:int2,filename:string) = this.save(f,st filename)
        member this.save (f:int1,filename:string) = this.save(f,st filename)
        member this.save (f:int0,filename:string) = this.save(f,st filename)
        member this.save (f:double3,filename:string) = this.save(f,st filename)
        member this.save (f:double2,filename:string) = this.save(f,st filename)
        member this.save (f:double1,filename:string) = this.save(f,st filename)
        member this.save (f:double0,filename:string) = this.save(f,st filename)
        member this.save (f:complex3,filename:string) = this.save(f,st filename)
        member this.save (f:complex2,filename:string) = this.save(f,st filename)
        member this.save (f:complex1,filename:string) = this.save(f,st filename)
        member this.save (f:complex0,filename:string) = this.save(f,st filename)

        ///<summary>数値をファイルから読み込み</summary>
        member this.load (f:int0,filename:exprString) =
            let reader (r:BinReader) (nt:int,t:Etype) =
                ctx.ch.i <| fun n ->
                    //データ型
                    r.b n
                    ctx.br.if2 (n.=nt)
                        <| fun () ->
                            //データ次元
                            r.b n
                            ctx.br.if2 (n.=0)
                                <| fun () ->
                                    ctx.ch.i <| fun n1 ->
                                        //データサイズ
                                        r.b n1
                                        //データ本体
                                        r.b f
                                <| fun () ->
                                    ctx.print.s "invalid data dimension"
                        <| fun () ->
                            ctx.print.s "invalid data type"
            this.binfileInput filename <| fun r ->
            ctx.ch.i <| fun n ->
                //データフォーマット
                r.b n
                ctx.br.branch <| fun b ->
                    b.IF (n.=1) <| fun () ->
                        match f.etype with
                        |It 4 ->
                            reader r (1004,f.etype)
                        |Dt    ->
                            reader r (2000,f.etype)
                        |Zt    ->
                            reader r (3000,f.etype)
                        |_ ->
                            ctx.print.s "invalid data type"

        ///<summary>数値をファイルから読み込み</summary>
        member this.load (f:double0,filename:exprString) =
            let reader (r:BinReader) (nt:int,t:Etype) =
                ctx.ch.i <| fun n ->
                    //データ型
                    r.b n
                    ctx.br.if2 (n.=nt)
                        <| fun () ->
                            //データ次元
                            r.b n
                            ctx.br.if2 (n.=0)
                                <| fun () ->
                                    ctx.ch.i <| fun n1 ->
                                        //データサイズ
                                        r.b n1
                                        //データ本体
                                        r.b f
                                <| fun () ->
                                    ctx.print.s "invalid data dimension"
                        <| fun () ->
                            ctx.print.s "invalid data type"
            this.binfileInput filename <| fun r ->
            ctx.ch.i <| fun n ->
                //データフォーマット
                r.b n
                ctx.br.branch <| fun b ->
                    b.IF (n.=1) <| fun () ->
                        match f.etype with
                        |It 4 ->
                            reader r (1004,f.etype)
                        |Dt    ->
                            reader r (2000,f.etype)
                        |Zt    ->
                            reader r (3000,f.etype)
                        |_ ->
                            ctx.print.s "invalid data type"

        ///<summary>数値をファイルから読み込み</summary>
        member this.load (f:complex0,filename:exprString) =
            let reader (r:BinReader) (nt:int,t:Etype) =
                ctx.ch.i <| fun n ->
                    //データ型
                    r.b n
                    ctx.br.if2 (n.=nt)
                        <| fun () ->
                            //データ次元
                            r.b n
                            ctx.br.if2 (n.=0)
                                <| fun () ->
                                    ctx.ch.i <| fun n1 ->
                                        //データサイズ
                                        r.b n1
                                        //データ本体
                                        ctx.ch.dd <| fun (re,im) ->
                                            r.b re
                                            r.b im
                                            f <== re + asm.uj*im
                                <| fun () ->
                                    ctx.print.s "invalid data dimension"
                        <| fun () ->
                            ctx.print.s "invalid data type"
            this.binfileInput filename <| fun r ->
            ctx.ch.i <| fun n ->
                //データフォーマット
                r.b n
                ctx.br.branch <| fun b ->
                    b.IF (n.=1) <| fun () ->
                        match f.etype with
                        |It 4 ->
                            reader r (1004,f.etype)
                        |Dt    ->
                            reader r (2000,f.etype)
                        |Zt    ->
                            reader r (3000,f.etype)
                        |_ ->
                            ctx.print.s "invalid data type"

        ///<summary>1次元データをファイルから読み込み</summary>
        member this.load (f:int1,filename:exprString) =
            let reader (r:BinReader) (nt:int,t:Etype) =
                ctx.ch.i <| fun n ->
                    //データ型
                    r.b n
                    ctx.br.if2 (n.=nt)
                        <| fun () ->
                            //データ次元
                            r.b n
                            ctx.br.if2 (n.=1)
                                <| fun () ->
                                    ctx.ch.i <| fun n1 ->
                                        //データサイズ
                                        r.b n1
                                        f.allocate n1
                                        //データ本体
                                        ctx.iter.num f.size1 <| fun i ->
                                            ctx.ch.i <| fun u ->
                                                r.b u
                                                f[i] <== u
                                <| fun () ->
                                    ctx.print.s "invalid data dimension"
                        <| fun () ->
                            ctx.print.s ": invalid data type"
            this.binfileInput filename <| fun r ->
            ctx.ch.i <| fun n ->
                //データフォーマット
                r.b n
                ctx.br.branch <| fun b ->
                    b.IF (n.=1) <| fun () ->
                        match f[0].etype with
                        |Etype.It 4  ->
                            reader r (1004,f[0].etype)
                        |Etype.Dt    ->
                            reader r (2000,f[0].etype)
                        |Etype.Zt    ->
                            reader r (3000,f[0].etype)
                        |_ ->
                            ctx.print.s "invalid data type"

        ///<summary>1次元データをファイルから読み込み</summary>
        member this.load (f:double1,filename:exprString) =
            let reader (r:BinReader) (nt:int,t:Etype) =
                ctx.ch.i <| fun n ->
                    //データ型
                    r.b n
                    ctx.br.if2 (n.=nt)
                        <| fun () ->
                            //データ次元
                            r.b n
                            ctx.br.if2 (n.=1)
                                <| fun () ->
                                    ctx.ch.i <| fun n1 ->
                                        //データサイズ
                                        r.b n1
                                        f.allocate n1
                                        //データ本体
                                        ctx.iter.num f.size1 <| fun i ->
                                            ctx.ch.d <| fun u ->
                                                r.b u
                                                f[i] <== u
                                <| fun () ->
                                    ctx.print.s "invalid data dimension"
                        <| fun () ->
                            ctx.print.s ": invalid data type"
            this.binfileInput filename <| fun r ->
            ctx.ch.i <| fun n ->
                //データフォーマット
                r.b n
                ctx.br.branch <| fun b ->
                    b.IF (n.=1) <| fun () ->
                        match f[0].etype with
                        |Etype.It 4  ->
                            reader r (1004,f[0].etype)
                        |Etype.Dt    ->
                            reader r (2000,f[0].etype)
                        |Etype.Zt    ->
                            reader r (3000,f[0].etype)
                        |_ ->
                            ctx.print.s "invalid data type"

        ///<summary>1次元データをファイルから読み込み</summary>
        member this.load (f:complex1,filename:exprString) =
            let reader (r:BinReader) (nt:int,t:Etype) =
                ctx.ch.i <| fun n ->
                    //データ型
                    r.b n
                    ctx.br.if2 (n.=nt)
                        <| fun () ->
                            //データ次元
                            r.b n
                            ctx.br.if2 (n.=1)
                                <| fun () ->
                                    ctx.ch.i <| fun n1 ->
                                        //データサイズ
                                        r.b n1
                                        f.allocate n1
                                        //データ本体
                                        match t with
                                        |It _ ->
                                            ctx.iter.num f.size1 <| fun i ->
                                                ctx.ch.i <| fun u ->
                                                    r.b u
                                                    f[i] <== u
                                        |Dt ->
                                            ctx.iter.num f.size1 <| fun i ->
                                                ctx.ch.d <| fun u ->
                                                    r.b u
                                                    f[i] <== u
                                        |Zt ->
                                            ctx.iter.num f.size1 <| fun i ->
                                                ctx.ch.dd <| fun (re,im) ->
                                                    r.b re
                                                    r.b im
                                                    f[i] <== re + asm.uj*im
                                        |_ ->
                                            ()
                                <| fun () ->
                                    ctx.print.s "invalid data dimension"
                        <| fun () ->
                            ctx.print.s ": invalid data type"
            this.binfileInput filename <| fun r ->
            ctx.ch.i <| fun n ->
                //データフォーマット
                r.b n
                ctx.br.branch <| fun b ->
                    b.IF (n.=1) <| fun () ->
                        match f[0].etype with
                        |Etype.It 4  ->
                            reader r (1004,f[0].etype)
                        |Etype.Dt    ->
                            reader r (2000,f[0].etype)
                        |Etype.Zt    ->
                            reader r (3000,f[0].etype)
                        |_ ->
                            ctx.print.s "invalid data type"

        ///<summary>2次元データをファイルから読み込み</summary>
        member this.load (f:int2,filename:exprString) =
            let reader (r:BinReader) (nt:int,t:Etype) =
                ctx.ch.i <| fun n ->
                    //データ型
                    r.b n
                    ctx.br.if2 (n.=nt)
                        <| fun () ->
                            //データ次元
                            r.b n
                            ctx.br.if2 (n.=2)
                                <| fun () ->
                                    ctx.ch.ii <| fun (n1,n2) ->
                                        //データサイズ
                                        r.b n1
                                        r.b n2
                                        f.allocate(n1,n2)
                                        //データ本体
                                        ctx.iter.num f.size2 <| fun j ->
                                            ctx.iter.num f.size1 <| fun i ->
                                                ctx.ch.i <| fun u ->
                                                    r.b u
                                                    f[i,j] <== u
                                <| fun () ->
                                    ctx.print.s "invalid data dimension"
                        <| fun () ->
                            ctx.print.s ": invalid data type"
                            ctx.print.tt <| n++(I nt)
            this.binfileInput filename <| fun r ->
            ctx.ch.i <| fun n ->
                //データフォーマット
                r.b n
                ctx.br.branch <| fun b ->
                    b.IF (n.=1) <| fun () ->
                        match f[0,0].etype with
                        |It 4 ->
                            reader r (1004,f[0,0].etype)
                        |Dt   ->
                            reader r (2000,f[0,0].etype)
                        |Zt   ->
                            reader r (3000,f[0,0].etype)
                        |_ ->
                            ctx.print.s "invalid data type"

        ///<summary>2次元データをファイルから読み込み</summary>
        member this.load (f:double2,filename:exprString) =
            let reader (r:BinReader) (nt:int,t:Etype) =
                ctx.ch.i <| fun n ->
                    //データ型
                    r.b n
                    ctx.br.if2 (n.=nt)
                        <| fun () ->
                            //データ次元
                            r.b n
                            ctx.br.if2 (n.=2)
                                <| fun () ->
                                    ctx.ch.ii <| fun (n1,n2) ->
                                        //データサイズ
                                        r.b n1
                                        r.b n2
                                        f.allocate(n1,n2)
                                        //データ本体
                                        ctx.iter.num f.size2 <| fun j ->
                                            ctx.iter.num f.size1 <| fun i ->
                                                ctx.ch.d <| fun u ->
                                                    r.b u
                                                    f[i,j] <== u
                                <| fun () ->
                                    ctx.print.s "invalid data dimension"
                        <| fun () ->
                            ctx.print.s ": invalid data type"
                            ctx.print.tt <| n++(I nt)
            this.binfileInput filename <| fun r ->
            ctx.ch.i <| fun n ->
                //データフォーマット
                r.b n
                ctx.br.branch <| fun b ->
                    b.IF (n.=1) <| fun () ->
                        match f[0,0].etype with
                        |It 4 ->
                            reader r (1004,f[0,0].etype)
                        |Dt   ->
                            reader r (2000,f[0,0].etype)
                        |Zt   ->
                            reader r (3000,f[0,0].etype)
                        |_ ->
                            ctx.print.s "invalid data type"

        ///<summary>2次元データをファイルから読み込み</summary>
        member this.load (f:complex2,filename:exprString) =
            let reader (r:BinReader) (nt:int,t:Etype) =
                ctx.ch.i <| fun n ->
                    //データ型
                    r.b n
                    ctx.br.if2 (n.=nt)
                        <| fun () ->
                            //データ次元
                            r.b n
                            ctx.br.if2 (n.=2)
                                <| fun () ->
                                    ctx.ch.ii <| fun (n1,n2) ->
                                        //データサイズ
                                        r.b n1
                                        r.b n2
                                        f.allocate(n1,n2)
                                        //データ本体
                                        match t with
                                        |It _ ->
                                            ctx.iter.num f.size2 <| fun j ->
                                                ctx.iter.num f.size1 <| fun i ->
                                                    ctx.ch.i <| fun u ->
                                                        r.b u
                                                        f[i,j] <== u
                                        |Dt ->
                                            ctx.iter.num f.size2 <| fun j ->
                                                ctx.iter.num f.size1 <| fun i ->
                                                    ctx.ch.d <| fun u ->
                                                        r.b u
                                                        f[i,j] <== u
                                        |Zt ->
                                            ctx.iter.num f.size2 <| fun j ->
                                                ctx.iter.num f.size1 <| fun i ->
                                                    ctx.ch.dd <| fun (re,im) ->
                                                        r.b re
                                                        r.b im
                                                        f[i,j] <== re + asm.uj*im
                                        |_ ->
                                            ()
                                <| fun () ->
                                    ctx.print.s "invalid data dimension"
                        <| fun () ->
                            ctx.print.s ": invalid data type"
                            ctx.print.tt <| n++(I nt)
            this.binfileInput filename <| fun r ->
            ctx.ch.i <| fun n ->
                //データフォーマット
                r.b n
                ctx.br.branch <| fun b ->
                    b.IF (n.=1) <| fun () ->
                        match f[0,0].etype with
                        |It 4 ->
                            reader r (1004,f[0,0].etype)
                        |Dt   ->
                            reader r (2000,f[0,0].etype)
                        |Zt   ->
                            reader r (3000,f[0,0].etype)
                        |_ ->
                            ctx.print.s "invalid data type"

        ///<summary>3次元データをファイルから読み込み</summary>
        member this.load (f:int3,filename:exprString) =
            let reader (r:BinReader) (nt:int,t:Etype) =
                ctx.ch.i <| fun n ->
                    //データ型
                    r.b n
                    ctx.br.if2 (n.=nt)
                        <| fun () ->
                            //データ次元
                            r.b n
                            ctx.br.if2 (n.=3)
                                <| fun () ->
                                    ctx.ch.iii <| fun (n1,n2,n3) ->
                                        //データサイズ
                                        r.b n1
                                        r.b n2
                                        r.b n3
                                        f.allocate(n1,n2,n3)
                                        //データ本体
                                        ctx.iter.num f.size3 <| fun k ->
                                            ctx.iter.num f.size2 <| fun j ->
                                                ctx.iter.num f.size1 <| fun i ->
                                                    ctx.ch.i <| fun u ->
                                                        r.b u
                                                        f[i,j,k] <== u
                                <| fun () ->
                                    ctx.print.s "invalid data dimension"
                        <| fun () ->
                            ctx.print.s "invalid data type"
            this.binfileInput filename <| fun r ->
            ctx.ch.i <| fun n ->
                //データフォーマット
                r.b n
                ctx.br.branch <| fun b ->
                    b.IF (n.=1) <| fun () ->
                        match f[_0,_0,_0].etype with
                        |Etype.It 4  ->
                            reader r (1004,f[_0,_0,_0].etype)
                        |Etype.Dt    ->
                            reader r (2000,f[_0,_0,_0].etype)
                        |Etype.Zt    ->
                            reader r (3000,f[_0,_0,_0].etype)
                        |_ ->
                            ctx.print.s "invalid data type"

        member this.load (f:int3,filename:string) = this.load(f,st filename)
        member this.load (f:int2,filename:string) = this.load(f,st filename)
        member this.load (f:int1,filename:string) = this.load(f,st filename)
        member this.load (f:int0,filename:string) = this.load(f,st filename)

        ///<summary>3次元データをファイルから読み込み</summary>
        member this.load (f:double3,filename:exprString) =
            let reader (r:BinReader) (nt:int,t:Etype) =
                ctx.ch.i <| fun n ->
                    //データ型
                    r.b n
                    ctx.br.if2 (n.=nt)
                        <| fun () ->
                            //データ次元
                            r.b n
                            ctx.br.if2 (n.=3)
                                <| fun () ->
                                    ctx.ch.iii <| fun (n1,n2,n3) ->
                                        //データサイズ
                                        r.b n1
                                        r.b n2
                                        r.b n3
                                        f.allocate(n1,n2,n3)
                                        //データ本体
                                        ctx.iter.num f.size3 <| fun k ->
                                            ctx.iter.num f.size2 <| fun j ->
                                                ctx.iter.num f.size1 <| fun i ->
                                                    ctx.ch.d <| fun u ->
                                                        r.b u
                                                        f[i,j,k] <== u
                                <| fun () ->
                                    ctx.print.s "invalid data dimension"
                        <| fun () ->
                            ctx.print.s "invalid data type"
            this.binfileInput filename <| fun r ->
            ctx.ch.i <| fun n ->
                //データフォーマット
                r.b n
                ctx.br.branch <| fun b ->
                    b.IF (n.=1) <| fun () ->
                        match f[_0,_0,_0].etype with
                        |Etype.It 4  ->
                            reader r (1004,f[_0,_0,_0].etype)
                        |Etype.Dt    ->
                            reader r (2000,f[_0,_0,_0].etype)
                        |Etype.Zt    ->
                            reader r (3000,f[_0,_0,_0].etype)
                        |_ ->
                            ctx.print.s "invalid data type"

        member this.load (f:double3,filename:string) = this.load(f,st filename)
        member this.load (f:double2,filename:string) = this.load(f,st filename)
        member this.load (f:double1,filename:string) = this.load(f,st filename)
        member this.load (f:double0,filename:string) = this.load(f,st filename)

        ///<summary>3次元データをファイルから読み込み</summary>
        member this.load (f:complex3,filename:exprString) =
            let reader (r:BinReader) (nt:int,t:Etype) =
                ctx.ch.i <| fun n ->
                    //データ型
                    r.b n
                    ctx.br.if2 (n.=nt)
                        <| fun () ->
                            //データ次元
                            r.b n
                            ctx.br.if2 (n.=3)
                                <| fun () ->
                                    ctx.ch.iii <| fun (n1,n2,n3) ->
                                        //データサイズ
                                        r.b n1
                                        r.b n2
                                        r.b n3
                                        f.allocate(n1,n2,n3)
                                        //データ本体
                                        ctx.iter.num f.size3 <| fun k ->
                                            ctx.iter.num f.size2 <| fun j ->
                                                ctx.iter.num f.size1 <| fun i ->
                                                    ctx.ch.dd <| fun (re,im) ->
                                                        r.b re
                                                        r.b im
                                                        f[i,j,k] <== re + asm.uj*im
                                <| fun () ->
                                    ctx.print.s "invalid data dimension"
                        <| fun () ->
                            ctx.print.s "invalid data type"
            this.binfileInput filename <| fun r ->
            ctx.ch.i <| fun n ->
                //データフォーマット
                r.b n
                ctx.br.branch <| fun b ->
                    b.IF (n.=1) <| fun () ->
                        match f[_0,_0,_0].etype with
                        |Etype.It 4  ->
                            reader r (1004,f[_0,_0,_0].etype)
                        |Etype.Dt    ->
                            reader r (2000,f[_0,_0,_0].etype)
                        |Etype.Zt    ->
                            reader r (3000,f[_0,_0,_0].etype)
                        |_ ->
                            ctx.print.s "invalid data type"

        member this.load (f:complex3,filename:string) = this.load(f,st filename)
        member this.load (f:complex2,filename:string) = this.load(f,st filename)
        member this.load (f:complex1,filename:string) = this.load(f,st filename)
        member this.load (f:complex0,filename:string) = this.load(f,st filename)

    ///<summary>ファイル入出力（処理スキップ）</summary>
    [<AutoOpen>]
    module CompilationEnvironmentIoExtensions =
        type Aqualis with
            member this.io = ContextIo(this)
