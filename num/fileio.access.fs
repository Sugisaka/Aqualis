//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    open System
    open System.Text

    /// Emits language-specific file opening and scoped resource cleanup.
    [<RequireQualifiedAccess>]
    module internal FileIoAccess =
        let private fortran (ctx:Aqualis) (filename:exprString) (intDigit:int option) readmode isbinary (code:string -> string -> unit) =
            let writein text = ctx.codewritein(text + "\n")
            ctx.ch.f <| fun fp ->
                let f, s, requiredLength = FileNameCode.fortran ctx filename intDigit
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
                    ctx.WithRuntimeCleanup(
                        (fun () -> writein("close("+fp+")\n"); writein("deallocate("+id+")\n")),
                        (fun () -> code fp id))

        let private c99 (ctx:Aqualis) (filename:exprString) (intDigit:int option) readmode isbinary (code:string -> string -> unit) =
            let writein text = ctx.codewritein(text + "\n")
            ctx.ch.f <| fun fp ->
                let f, s = FileNameCode.c99 ctx filename intDigit
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
                        writein(fp+" = "+"fopen("+id+",\""+(if readmode then "rb" else "w")+"\");"+"\n")
                    writein("if (" + fp + " == NULL) { fprintf(stderr, \"Aqualis: failed to open file %s.\\n\", " + id + "); free(" + id + "); " + id + " = NULL; exit(EXIT_FAILURE); }\n")
                    ctx.WithRuntimeCleanup(
                        (fun () ->
                            writein("if (fclose(" + fp + ") != 0) { fprintf(stderr, \"Aqualis: failed to close file %s.\\n\", " + id + "); free(" + id + "); " + id + " = NULL; exit(EXIT_FAILURE); }\n")
                            writein("free(" + id + ");\n")
                            writein(id + " = NULL;\n")),
                        (fun () -> code fp id))

        let private latex (ctx:Aqualis) (filename:exprString) (intDigit:int option) readmode isbinary (code:string -> string -> unit) =
            let writein text = ctx.codewritein(text + "\n")
            ctx.ch.f <| fun fp ->
                let s = FileNameCode.latex ctx filename intDigit
                ctx.ch.t <| A0 <| fun id ->
                    let btname = "byte_tmp"
                    if isbinary then
                        writein(fp+" = "+"open binary file("+id+",\""+(if readmode then "rb" else "wb")+"\");"+"\n")
                    else
                        writein(fp+" = "+"open text file("+id+",\""+(if readmode then "r" else "w")+"\");"+"\n")
                    code fp id
                    writein("close("+fp+")"+";\n")

        let private html (ctx:Aqualis) (filename:exprString) (intDigit:int option) readmode isbinary (code:string -> string -> unit) =
            let writein text = ctx.codewritein(text + "\n")
            ctx.ch.f <| fun fp ->
                let s = FileNameCode.html ctx filename intDigit
                ctx.ch.t <| A0 <| fun id ->
                    let btname = "byte_tmp"
                    if isbinary then
                        writein(fp+" = "+"open binary file("+id+",\""+(if readmode then "rb" else "wb")+"\");"+"\n")
                    else
                        writein(fp+" = "+"open text file("+id+",\""+(if readmode then "r" else "w")+"\");"+"\n")
                    code fp id
                    writein("close("+fp+")"+";\n")

        let private python (ctx:Aqualis) (filename:exprString) (intDigit:int option) readmode isbinary (code:string -> string -> unit) =
            let writein text = ctx.codewritein(text + "\n")
            ctx.ch.f <| fun fp ->
                let filenameExpression = FileNameCode.python ctx filename intDigit
                ctx.ch.t <| A0 <| fun id ->
                    writein(id + " = " + filenameExpression + "\n")
                    if isbinary then
                        writein(fp+" = "+"open("+id+",mode=\""+(if readmode then "rb" else "wb")+"\")"+"\n")
                    else
                        writein(fp+" = "+"open("+id+",mode=\""+(if readmode then "rb" else "w")+"\")"+"\n")
                    ctx.WithRuntimeCleanup((fun () -> writein(fp+".close()\n")),(fun () -> code fp id))

        let withFile (ctx:Aqualis) (filename:exprString,intDigit:int option) readmode isbinary (code:string -> string -> unit) =
            match ctx.language with
            |Fortran -> fortran ctx filename intDigit readmode isbinary code
            |C99 -> c99 ctx filename intDigit readmode isbinary code
            |LaTeX -> latex ctx filename intDigit readmode isbinary code
            |HTML -> html ctx filename intDigit readmode isbinary code
            |Python -> python ctx filename intDigit readmode isbinary code
            |JavaScript |PHP ->
                raise (NotSupportedException("File I/O is not supported for " + string ctx.language + "."))
            |_ -> ()

