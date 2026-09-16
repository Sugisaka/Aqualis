//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    open System

    type TextWriter internal (ctx:Aqualis,fp:string) =
        // let context() = ctx.RequireGenerationContext()
        // let program() = (context()).CurrentProgram
        let writein text = ctx.codewritein(text + "\n")
        member _.tt (lst:exprString) =
            match ctx.language with
            |Fortran ->
                let double0string_format_F =
                    let a,b = ctx.numFormat.dFormat
                    "E"+a.ToString()+"."+b.ToString()+"e3"
                let format =
                    lst
                    |> (fun b ->
                        [for n in b.data do
                            match n,n.etype with
                            |RStr _,_ ->
                                yield "A"
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
                            |_,RStr value -> yield OutputTextLiteral.fortran value
                            |_ -> ()])
                    |> (fun b ->
                          [for n in 0..(b.Length-1) do
                              yield b.[n]
                              if n<(b.Length-1) then yield "achar(9)"
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
                            |RStr value,_ ->
                                yield value.Replace("%", "%%")
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
                              if n<(b.Length-1) then yield "\t"
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
                writein("fprintf(" + fp + "," + OutputTextLiteral.c (format + "\n") +
                        (if code = "" then "" else "," + code) + ");\n")
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
                let hasNumeric = lst.data |> List.exists (function RNvr _ -> true | _ -> false)
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
                            |RStr value,_ ->
                                yield (if hasNumeric then value.Replace("%", "%%") else value)
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
                              if n<(b.Length-1) then yield "\t"
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
                let literal = OutputTextLiteral.python (format + "\n")
                writein(fp + ".write(" +
                        (if code = "" then literal else literal + " %(" + code + ")") +
                        ")\n")
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
                                yield v.Replace("%", "%%")
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
                writein("fprintf(" + fp + "," + OutputTextLiteral.c (format + "\n") +
                        (if code = "" then "" else "," + code) + ");\n")
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
                let hasNumeric = lst.data |> List.exists (function RNvr _ -> true | _ -> false)
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
                                yield (if hasNumeric then v.Replace("%", "%%") else v)
                            |_ -> ()
                        ])
                    |> (fun b ->
                          [for n in 0..(b.Length-1) do
                              yield b.[n]
                              if n<(b.Length-1) then yield "\t"
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
                let literal = OutputTextLiteral.python (format + "\n")
                writein(fp + ".write(" +
                        (if code = "" then literal else literal + " %(" + code + ")") +
                        ")\n")
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
