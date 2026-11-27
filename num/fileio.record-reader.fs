//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    open System
    open System.Text

    /// Emits formatted record reads used by file_Read.
    [<RequireQualifiedAccess>]
    module internal FileIoRecordReader =
        let private fortran (ctx:Aqualis) (fp:string) (iostat:int0) (Nz:int) (varlist:(Etype * int * reduceExprString) list) =
            let writein text = ctx.codewritein(text + "\n")
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
                    if Nz > 0 then
                        ctx.br.if1 (iostat .= 0) <| fun () ->
                            for t,m,b in varlist do
                                match t,b with
                                |Zt,target ->
                                    let value,targetContext =
                                        FileIoReadTarget.require ctx target
                                    complex0(value,context=targetContext)
                                        <== tmp[2*m]+asm.uj*tmp[2*m+1]
                                |_ -> ()

        let private c99 (ctx:Aqualis) (fp:string) (iostat:int0) (Nz:int) (varlist:(Etype * int * reduceExprString) list) =
            let writein text = ctx.codewritein(text + "\n")
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
                let expectedCount = varlist.Length + Nz
                writein(iostat.code + " = fscanf("+fp+",\""+format+"\","+code+");\n")
                writein("if (" + iostat.code + " != EOF && " + iostat.code + " != " +
                        string expectedCount + ") { fprintf(stderr, \"Aqualis: invalid text input record.\\n\"); exit(EXIT_FAILURE); }\n")
                ctx.br.if1 (iostat .= expectedCount) <| fun () ->
                    for t,m,b in varlist do
                        match t,b with
                        |Zt,target ->
                            let value,targetContext =
                                FileIoReadTarget.require ctx target
                            complex0(value,context=targetContext)
                                <== tmp[2*m]+asm.uj*tmp[2*m+1]
                        |_ -> ()

        let private latex (ctx:Aqualis) (fp:string) (iostat:int0) (lst:exprString) =
            let writein text = ctx.codewritein(text + "\n")
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

        let private html (ctx:Aqualis) (fp:string) (iostat:int0) (lst:exprString) =
            let writein text = ctx.codewritein(text + "\n")
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

        let private python (ctx:Aqualis) (fp:string) (iostat:int0) (Nz:int) (varlist:(Etype * int * reduceExprString) list) =
            let writein text = ctx.codewritein(text + "\n")
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
                                yield FileIoReadTarget.reject()
                        ])
                  |> fun s -> String.Join(",",s)
                writein("lines = " + fp + ".readline()\n")
                writein("if lines == b'':\n")
                ctx.indentInc()
                writein(iostat.code + " = -1\n")
                ctx.indentDec()
                writein("else:\n")
                ctx.indentInc()
                writein("word_list = lines.split()\n")
                writein("if len(word_list) != " + string (varlist.Length + Nz) +
                        ": raise ValueError('Aqualis: invalid text input record.')\n")
                iostat <== 0
                let mutable cnt = 0
                for t,_,a in varlist do
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
                ctx.indentDec()

        let read (ctx:Aqualis) (fp:string) (iostat:int0) (lst:exprString) =
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
            |Fortran -> fortran ctx fp iostat Nz varlist
            |C99 -> c99 ctx fp iostat Nz varlist
            |LaTeX -> latex ctx fp iostat lst
            |HTML -> html ctx fp iostat lst
            |Python -> python ctx fp iostat Nz varlist
            |_ -> ()
