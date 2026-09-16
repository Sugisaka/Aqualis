//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    open System

    module private FileIoReadTarget =
        let reject() : 'T =
            invalidOp "A file-read target must be a variable."

        let require (ctx:Aqualis) target =
            match target with
            |RNvr(_,c) when c.CodeFile=None ->
                invalidOp "A file-read target is not associated with a GenerationContext."
            |RNvr(Var _ as value,targetContext) ->
                Aqualis.merge ctx targetContext |> ignore
                value,targetContext
            |RNvr _ ->
                reject()
            |RStr _ ->
                reject()

    type TextReader internal (ctx:Aqualis,fp:string,iostat:int0) =
        // let context() = ctx.RequireGenerationContext()
        // let program() = (context()).CurrentProgram
        let writein text = ctx.codewritein(text + "\n")
        member _.tt (lst:exprString) =
            if List.isEmpty lst.data then
                invalidArg (nameof lst) "A file-read record must contain at least one target."
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
                                        yield FileIoReadTarget.reject()
                                ])
                            |> (fun b ->
                                  [for n in 0..(b.Length-1) do
                                      yield b[n]
                                      if n<(b.Length-1) then yield tab[n].Expr.eval ctx
                                  ])
                            |> fun s -> String.Join(",",s)
                        writein("read("+fp+",\"("+format+")\",iostat="+iostat.Expr.eval ctx+") "+code+"\n")
                        writein("if (" + iostat.code + " /= 0) error stop 'Aqualis: invalid text input record.'\n")
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
                                    yield FileIoReadTarget.reject()
                            ])
                      |> fun s -> String.Join(",",s)
                    writein("if (fscanf("+fp+",\""+format+"\","+code+") != " +
                            string (varlist.Length + Nz) +
                            ") { fprintf(stderr, \"Aqualis: invalid text input record.\\n\"); exit(EXIT_FAILURE); }\n")
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
                writein("word_list = " + fp + ".readline().split()")
                writein("if len(word_list) != " + string (varlist.Length + Nz) +
                        ": raise ValueError('Aqualis: invalid text input record.')")
                let mutable cnt = 0
                for t,_,a in varlist do
                    match t,a with
                    |It _,RNvr(v,_) ->
                        writein(v.eval ctx+" = int(word_list["+cnt.ToString()+"])")
                        cnt <- cnt + 1
                    |Dt,RNvr(v,_) ->
                        writein(v.eval ctx+" = float(word_list["+cnt.ToString()+"])")
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
        let readFortran target =
            writein("read(" + fp + ",iostat=" + iostat.code + ") " + target)
            writein("if (" + iostat.code + " /= 0) error stop 'Aqualis: invalid binary input record.'")
        let readC target =
            writein("if (fread(&" + target + ",sizeof(" + target + "),1," + fp +
                    ") != 1) { fprintf(stderr, \"Aqualis: invalid binary input record.\\n\"); exit(EXIT_FAILURE); }")
        member private _.ReadBin target =
            let v,targetContext =
                FileIoReadTarget.require ctx target
            match ctx.language with
            |Fortran ->
                match v.etype,v with
                |Zt,Var _ ->
                    ctx.ch.dd <| fun (re,im) ->
                        readFortran (re.Expr.eval ctx)
                        readFortran (im.Expr.eval ctx)
                        complex0(v,context=targetContext) <== re+asm.uj*im
                |_,Var(_,n,_) ->
                    readFortran n
                |_ ->
                    FileIoReadTarget.reject()
            |C99 ->
                match v.etype,v with
                |Zt,Var _ ->
                    ctx.ch.dd <| fun (re,im) ->
                        readC (re.Expr.eval ctx)
                        readC (im.Expr.eval ctx)
                        complex0(v,context=targetContext) <== re+asm.uj*im
                |_,Var(_,n,_) ->
                    readC n
                |_ ->
                    FileIoReadTarget.reject()
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
                    FileIoReadTarget.reject()
            |HTML ->
                match v with
                |Var(_,n,_) ->
                    writein("Read(binary): \\("+n+" \\leftarrow "+fp+"\\)<br/>\n")
                |_ ->
                    FileIoReadTarget.reject()
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
                    FileIoReadTarget.reject()
            |_ -> ()
        member this.b (x:int0) = this.ReadBin(RNvr(x.Expr,x.Context))
        member this.b (x:double0) = this.ReadBin(RNvr(x.Expr,x.Context))
        member this.b (x:complex0) = this.ReadBin(RNvr(x.Expr,x.Context))
