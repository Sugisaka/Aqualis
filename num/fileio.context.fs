//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    open System
    open System.Text

    [<RequireQualifiedAccess>]
    module private FileNameCode =
        let private appendCommonEscape (builder:StringBuilder) character =
            match character with
            | '\\' -> builder.Append("\\\\") |> ignore; true
            | '"' -> builder.Append("\\\"") |> ignore; true
            | '\n' -> builder.Append("\\n") |> ignore; true
            | '\r' -> builder.Append("\\r") |> ignore; true
            | '\t' -> builder.Append("\\t") |> ignore; true
            | _ -> false

        let cStringLiteral (value:string) =
            if isNull value then nullArg (nameof value)
            if value.IndexOf '\u0000' >= 0 then
                invalidArg (nameof value) "A generated file name cannot contain NUL."

            let builder = StringBuilder(value.Length + 2)
            builder.Append('"') |> ignore
            for character in value do
                if not (appendCommonEscape builder character) then
                    if Char.IsControl character then
                        builder
                            .Append('\\')
                            .Append(Convert.ToString(int character, 8).PadLeft(3, '0'))
                        |> ignore
                    else
                        builder.Append(character) |> ignore
            builder.Append('"').ToString()

        let fortranStringLiteral (value:string) =
            if isNull value then nullArg (nameof value)
            if value |> Seq.exists Char.IsControl then
                invalidArg (nameof value) "A generated Fortran file name cannot contain control characters."
            "'" + value.Replace("'", "''") + "'"

        let pythonStringLiteral (value:string) =
            if isNull value then nullArg (nameof value)
            if value.IndexOf '\u0000' >= 0 then
                invalidArg (nameof value) "A generated file name cannot contain NUL."

            let builder = StringBuilder(value.Length + 2)
            builder.Append('"') |> ignore
            for character in value do
                if not (appendCommonEscape builder character) then
                    if Char.IsControl character then
                        builder.Append("\\u").Append((int character).ToString("X4")) |> ignore
                    else
                        builder.Append(character) |> ignore
            builder.Append('"').ToString()

    type ContextIo internal (ctx:Aqualis) =
        // let context() = ctx.RequireGenerationContext()
        // let program() = (context()).CurrentProgram
        let writein text = ctx.codewritein(text + "\n")

        member internal _.GenerationContext = ctx

        member private this.fileAccess (filename:exprString,intDigit:option<int>) readmode isbinary code =
            match ctx.language with
            |Fortran ->
                ctx.ch.f <| fun fp ->
                    let integerWidth =
                        match intDigit with
                        |None -> ctx.numFormat.iFormat
                        |Some width when width > 0 -> width
                        |Some _ -> invalidArg (nameof intDigit) "The file-name integer width must be positive."
                    let f =
                        filename.data
                        |> List.map (fun s ->
                            match s with
                            |RStr _ ->
                                "A"
                            |RNvr(x,_) when x.etype = It 4 ->
                                "I0." + integerWidth.ToString()
                            |_ ->
                                "")
                        |> List.filter (String.IsNullOrEmpty >> not)
                        |> fun s -> String.Join(",",s)
                    let s =
                        filename.data
                        |> List.map (fun s ->
                            match s with
                            |RStr t ->
                                FileNameCode.fortranStringLiteral t
                            |RNvr(x,_) when x.etype = It 4 ->
                                x.eval ctx
                            |_ ->
                                "")
                        |> List.filter (String.IsNullOrEmpty >> not)
                        |> fun s -> String.Join(",",s)
                    let requiredLength =
                        filename.data
                        |> List.choose (function
                            |RStr value -> Some("len(" + FileNameCode.fortranStringLiteral value + ")")
                            |RNvr(value,_) when value.etype = It 4 -> Some((max integerWidth 11).ToString())
                            |_ -> None)
                        |> function
                            |[] -> "0"
                            |terms -> String.Join(" + ", terms)
                    ctx.ch.t <| A0 <| fun id ->
                        let btname = "byte_tmp"
                        //変数byte_tmpをリストに追加（存在していない場合のみ）
                        ctx.cvar.setUniqVar(Structure "integer(1)",A0,btname,"")
                        writein("allocate(character(len=" + requiredLength + ") :: " + id + ")\n")
                        writein("write("+id+",\"("+f+")\") "+s+"\n")
                        if isbinary then
                            writein("open("+fp+", file=trim("+id+"), access='stream', form='unformatted')"+"\n")
                        else
                            writein("open("+fp+", file=trim("+id+"))"+"\n")
                        code fp
                        writein("close("+fp+")"+"\n")
                        writein("deallocate(" + id + ")\n")
            |C99 ->
                ctx.ch.f <| fun fp ->
                    let integerWidth =
                        match intDigit with
                        |None -> ctx.numFormat.iFormat
                        |Some width when width > 0 -> width
                        |Some _ -> invalidArg (nameof intDigit) "The file-name integer width must be positive."
                    let f =
                        filename.data
                        |> List.map (fun s ->
                            match s with
                            |RStr t ->
                                t.Replace("%", "%%")
                            |RNvr(x,_) when x.etype = It 4 ->
                                "%0" + integerWidth.ToString() + "d"
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
                        let lengthName = id + "_length"
                        let arguments = if s="" then "" else "," + s
                        let formatLiteral = FileNameCode.cStringLiteral f
                        ctx.cvar.setUniqVar(It 4,A0,lengthName,"")
                        writein(lengthName + " = snprintf(NULL,0," + formatLiteral + arguments + ");\n")
                        writein("if (" + lengthName + " < 0) { fprintf(stderr, \"Aqualis: failed to format a file name.\\n\"); exit(EXIT_FAILURE); }\n")
                        writein(id + " = (char *)malloc((size_t)" + lengthName + " + 1U);\n")
                        writein("if (" + id + " == NULL) { fprintf(stderr, \"Aqualis: failed to allocate a file name.\\n\"); exit(EXIT_FAILURE); }\n")
                        writein("if (snprintf(" + id + ",(size_t)" + lengthName + " + 1U," + formatLiteral + arguments + ") != " + lengthName + ") { free(" + id + "); " + id + " = NULL; fprintf(stderr, \"Aqualis: failed to format a file name.\\n\"); exit(EXIT_FAILURE); }\n")
                        if isbinary then
                            writein(fp+" = "+"fopen("+id+",\""+(if readmode then "rb" else "wb")+"\");"+"\n")
                        else
                            writein(fp+" = "+"fopen("+id+",\""+(if readmode then "r" else "w")+"\");"+"\n")
                        writein("if (" + fp + " == NULL) { fprintf(stderr, \"Aqualis: failed to open file %s.\\n\", " + id + "); free(" + id + "); " + id + " = NULL; exit(EXIT_FAILURE); }\n")
                        code fp
                        writein("if (fclose(" + fp + ") != 0) { fprintf(stderr, \"Aqualis: failed to close file %s.\\n\", " + id + "); free(" + id + "); " + id + " = NULL; exit(EXIT_FAILURE); }\n")
                        writein("free(" + id + ");\n")
                        writein(id + " = NULL;\n")
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
                    let integerWidth =
                        match intDigit with
                        |None -> ctx.numFormat.iFormat
                        |Some width when width > 0 -> width
                        |Some _ -> invalidArg (nameof intDigit) "The file-name integer width must be positive."
                    let filenameExpression =
                        filename.data
                        |> List.choose (fun part ->
                            match part with
                            |RStr t ->
                                Some(FileNameCode.pythonStringLiteral t)
                            |RNvr(x,_) when x.etype = It 4 ->
                                Some("format(" + x.eval ctx + ", " + FileNameCode.pythonStringLiteral ("0" + integerWidth.ToString() + "d") + ")")
                            |_ ->
                                None)
                        |> function
                            |[] -> FileNameCode.pythonStringLiteral ""
                            |parts -> String.Join(" + ", parts)
                    ctx.ch.t <| A0 <| fun id ->
                        writein(id + " = " + filenameExpression + "\n")
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
                                    yield FileIoReadTarget.reject()
                            ])
                      |> fun s -> String.Join(",",s)
                    writein("lines = " + fp + ".readline()\n")
                    writein("if lines == '':\n")
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
                    FileIoReadTarget.reject()
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
                    FileIoReadTarget.reject()
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
                        complex0 v <== re+asm.uj*im
                |It _,Var(_,n,_) ->
                    writein(n+" = struct.unpack('i', "+fp+".read(4))[0]"+"\n")
                |Dt,Var(_,n,_) ->
                    writein(n+" = struct.unpack('d', "+fp+".read(8))[0]"+"\n")
                |_ ->
                    FileIoReadTarget.reject()
            |_ -> ()

        member private this.Read_byte (fp:string) (iostat:int0) (e:expr) =
            writein("read("+fp+", iostat="+iostat.Expr.eval ctx+") byte_tmp\n")
            let ee =
                match e.etype,e with
                |It _,Var(_,n,_) -> n
                |_ -> FileIoReadTarget.reject()
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
                        if ctx.language = Fortran then
                            writein("if (" + iostat.code + " > 0) error stop 'Aqualis: invalid text input record.'\n")
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
                        if ctx.language = Fortran then
                            writein("if (" + iostat.code + " > 0) error stop 'Aqualis: invalid text input record.'\n")
                        ctx.br.branch <| fun b ->
                            b.IF (iostat .< 0) <| fun () ->
                                ext()
                            b.EL <| fun () ->
                                code(i)
