//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    open System

    ///<summary>画面表示</summary>
    type internal PrintEmitter () =
        ///<summary>変数リストを画面表示</summary>
        static member internal ttWith (program:program) (lst:exprString) =
            match program.language with
            |Fortran ->
                let clist =
                    [for q in lst.data do
                        match q with
                        |RStr x ->
                            yield "\""+x+"\""
                        |RNvr (x,_) when x.etype = Zt ->
                            yield (Re x).eval (program)
                            yield (Im x).eval (program)
                        |RNvr (x,_) ->
                            yield x.eval (program) ]
                program.codewritein("print *, " + String.concat "," clist + "\n")
            |C99 ->
                let int0string_format_C =
                    "%"+program.numFormat.iFormat.ToString()+"d"
                let double0string_format_C =
                    let a,b = program.numFormat.dFormat
                    "%"+a.ToString()+"."+b.ToString()+"e"
                let format =
                    lst.data
                    |> List.map( fun (q:reduceExprString) ->
                        match q with
                        |RStr x -> x
                        |RNvr (x,_) when x.etype = It 4 -> int0string_format_C
                        |RNvr (x,_) when x.etype = Dt -> double0string_format_C
                        |RNvr (x,_) when x.etype = Zt -> double0string_format_C + double0string_format_C
                        |_ -> "")
                    |> List.filter (fun s -> s <> "")
                    |> fun s -> String.Join("",s)
                let code =
                    lst.data
                    |> List.map (fun (q:reduceExprString) ->
                        match q with
                        |RStr _ -> ""
                        |RNvr (x,_) when x.etype = Zt -> (Re x).eval (program) + "," + (Im x).eval (program)
                        |RNvr (x,_) -> x.eval (program))
                    |> List.filter (fun s -> s <> "")
                    |> fun s -> String.Join(",",s)
                program.codewritein("printf(\""+format+"\\n\","+code+");\n")
            |LaTeX ->
                let code =
                    lst.data
                    |> List.map (fun (q:reduceExprString) ->
                        match q with
                        |RStr x -> x
                        |RNvr (x,_) -> x.eval (program))
                    |> List.filter (fun s -> s <> "")
                    |> fun s -> String.Join(",",s)
                program.codewritein("print, " + code + "\n")
            |HTML ->
                let code =
                    lst.data
                    |> List.map (fun (q:reduceExprString) ->
                        match q with
                        |RStr x -> x
                        |RNvr (x,_) -> x.eval (program))
                    |> List.filter (fun s -> s <> "")
                    |> fun s -> String.Join(",",s)
                program.codewritein("Print \\("+code+"\\)\n")
                program.codewritein "<br/>\n"
            |HTMLSequenceDiagram ->
                let code =
                    lst.data
                    |> List.map (fun (q:reduceExprString) ->
                        match q with
                        |RStr x -> x
                        |RNvr (x,_) -> x.eval (program))
                    |> List.filter (fun s -> s <> "")
                    |> fun s -> String.Join(",",s)
                program.codewritein("Print \\("+code+"\\)\n")
                program.codewritein "<br/>\n"
            |Python ->
                let int0string_format_C =
                    "%"+program.numFormat.iFormat.ToString()+"d"
                let double0string_format_C =
                    let a,b = program.numFormat.dFormat
                    "%"+a.ToString()+"."+b.ToString()+"e"
                let format =
                    lst.data
                    |> List.map (fun (q:reduceExprString) ->
                        match q with
                        |RStr x -> x
                        |RNvr (x,_) when x.etype = It 4 -> int0string_format_C
                        |RNvr (x,_) when x.etype = Dt  -> double0string_format_C
                        |RNvr (x,_) when x.etype = Zt  -> double0string_format_C + double0string_format_C
                        |_ -> "")
                    |> List.filter (fun s -> s <> "")
                    |> fun s -> String.Join("",s)
                let code =
                    lst.data
                    |> List.map (fun (q:reduceExprString) ->
                        match q with
                        |RNvr (x,_) when x.etype = Zt -> (Re x).eval (program) + "," + (Im x).eval (program)
                        |RNvr (x,_) -> x.eval (program)
                        |_ -> "")
                    |> List.filter (fun s -> s <> "")
                    |> fun s -> String.Join(",",s)
                program.codewritein("print(\"" + format + "\" %(" + code + "))\n")
            |JavaScript ->
                let int0string_format_C =
                    "%"+program.numFormat.iFormat.ToString()+"d"
                let double0string_format_C =
                    let a,b = program.numFormat.dFormat
                    "%"+a.ToString()+"."+b.ToString()+"e"
                let format =
                    lst.data
                    |> List.map (fun (q:reduceExprString) ->
                        match q with
                        |RStr x -> x
                        |RNvr (x,_) when x.etype = It 4 -> int0string_format_C
                        |RNvr (x,_) when x.etype = Dt  -> double0string_format_C
                        |RNvr (x,_) when x.etype = Zt  -> double0string_format_C + double0string_format_C
                        |_ -> "")
                    |> List.filter (fun s -> s <> "")
                    |> fun s -> String.Join("",s)
                let code =
                    lst.data
                    |> List.map (fun (q:reduceExprString) ->
                        match q with
                        |RNvr (x,_) when x.etype = Zt -> (Re x).eval (program) + "," + (Im x).eval (program)
                        |RNvr (x,_) -> x.eval (program)
                        |_ -> "")
                    |> List.filter (fun s -> s <> "")
                    |> fun s -> String.Join(",",s)
                program.codewritein("print(" + code + ");\n")
            |PHP ->
                let int0string_format_C =
                    "%"+program.numFormat.iFormat.ToString()+"d"
                let double0string_format_C =
                    let a,b = program.numFormat.dFormat
                    "%"+a.ToString()+"."+b.ToString()+"e"
                let format =
                    lst.data
                    |> List.map (fun (q:reduceExprString) ->
                        match q with
                        |RStr x -> x
                        |RNvr (x,_) when x.etype = It 4 -> int0string_format_C
                        |RNvr (x,_) when x.etype = Dt  -> double0string_format_C
                        |RNvr (x,_) when x.etype = Zt  -> double0string_format_C + double0string_format_C
                        |_ -> "")
                    |> List.filter (fun s -> s <> "")
                    |> fun s -> String.Join("",s)
                let code =
                    lst.data
                    |> List.map (fun (q:reduceExprString) ->
                        match q with
                        |RNvr (x,_) when x.etype = Zt -> (Re x).eval (program) + "," + (Im x).eval (program)
                        |RNvr (x,_) -> x.eval (program)
                        |_ -> "")
                    |> List.filter (fun s -> s <> "")
                    |> fun s -> String.Join(",",s)
                program.codewritein("print(" + code + ");\n")
            |Numeric ->
                for v in lst.data do
                    match v with
                    |RNvr (Int x,_) -> printf "%d " x
                    |RNvr (Dbl x,_) -> printf "%e " x
                    |RNvr (Cpx (re,im),_) -> printf "%e %e " re im
                    |_ -> ()
        ///<summary>文字列を画面表示</summary>
        static member internal sWith (program:program) (str:string) =
            match program.language with
            |Fortran ->
                program.codewritein("print *, "+"\""+str+"\""+"\n")
            |C99 ->
                program.codewritein("printf(\""+str+"\""+");\n")
            |LaTeX ->
                program.codewritein("print, \""+str+"\"\n")
            |HTML ->
                program.codewritein("Print \\("+str+"\\)\n")
                program.codewritein "<br/>\n"
            |HTMLSequenceDiagram ->
                program.codewritein("Print \\("+str+"\\)\n")
                program.codewritein "<br/>\n"
            |Python ->
                program.codewritein("print(\""+str+"\")\n")
            |JavaScript ->
                program.codewritein("print(\""+str+"\")\n")
            |PHP ->
                program.codewritein("print(\""+str+"\")\n")
            |Numeric ->
                printfn "%s" str

        ///<summary>1個の項目を画面表示</summary>
    type ContextPrint internal (environment:Aqualis) =
        member internal _.Environment = environment

        member _.s(str:string) =
            match environment.GenerationContext with
            |Some context -> PrintEmitter.sWith context.CurrentProgram str
            |None -> printfn "%s" str

        member _.tt(value:exprString) =
            match environment.GenerationContext with
            |Some context ->
                GenerationContextMerge.merge (Some context) value.Context |> ignore
                PrintEmitter.ttWith context.CurrentProgram value
            |None ->
                for item in value.data do
                    match item with
                    |RNvr (Int x,_) -> printf "%d " x
                    |RNvr (Dbl x,_) -> printf "%e " x
                    |RNvr (Cpx(re,im),_) -> printf "%e %e " re im
                    |RStr text -> printf "%s" text
                    |_ -> ()

        member this.t(value:int0) = this.tt(iv value)
        member this.t(value:double0) = this.tt(dv value)
        member this.t(value:complex0) = this.tt(zv value)

    [<AutoOpen>]
    module CompilationEnvironmentPrintExtensions =
        type Aqualis with
            member this.print = ContextPrint(this)
