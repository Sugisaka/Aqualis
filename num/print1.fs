//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    open System
    open System.Text

    /// Escapes output text literals for target languages.
    [<RequireQualifiedAccess>]
    module internal OutputTextLiteral =
        /// Quotes and escapes text for a generated literal.
        let private quoted (cStyle:bool) (value:string) =
            if isNull value then nullArg (nameof value)
            let builder = StringBuilder(value.Length + 2)
            builder.Append('"') |> ignore
            for character in value do
                match character with
                | '\\' -> builder.Append("\\\\") |> ignore
                | '"' -> builder.Append("\\\"") |> ignore
                | '\n' -> builder.Append("\\n") |> ignore
                | '\r' -> builder.Append("\\r") |> ignore
                | '\t' -> builder.Append("\\t") |> ignore
                | c when Char.IsControl c && cStyle && int c <= 255 ->
                    builder.Append('\\').Append(Convert.ToString(int c, 8).PadLeft(3, '0')) |> ignore
                | c when Char.IsControl c ->
                    builder.Append("\\u").Append((int c).ToString("X4")) |> ignore
                | c -> builder.Append(c) |> ignore
            builder.Append('"').ToString()

        /// Escapes a string as a quoted C literal.
        let c value = quoted true value
        /// Escapes a string as a quoted Python literal.
        let python value = quoted false value
        /// Escapes a string as a quoted JavaScript literal.
        let javaScript value = quoted false value

        /// Escapes a string as a quoted Fortran literal.
        let fortran (value:string) =
            if isNull value then nullArg (nameof value)
            let parts = ResizeArray<string>()
            let text = StringBuilder()
            let flush () =
                if text.Length > 0 then
                    parts.Add("'" + text.ToString().Replace("'", "''") + "'")
                    text.Clear() |> ignore
            for character in value do
                if Char.IsControl character then
                    flush()
                    parts.Add("achar(" + string (int character) + ")")
                else
                    text.Append(character) |> ignore
            flush()
            if parts.Count = 0 then "''" else String.Join("//", parts)

    ///<summary>画面表示</summary>
    type internal PrintEmitter () =
        ///<summary>変数リストを画面表示</summary>
        static member internal ttWith (program:Aqualis) (lst:exprString) =
            match program.language with
            |Fortran ->
                let clist =
                    [for q in lst.data do
                        match q with
                        |RStr x ->
                            yield OutputTextLiteral.fortran x
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
                        |RStr x -> x.Replace("%", "%%")
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
                program.codewritein("printf(" + OutputTextLiteral.c (format + "\n") +
                                    (if code = "" then "" else "," + code) + ");\n")
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
                        |RStr x -> HtmlEncoding.textContent x
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
                        |RStr x -> HtmlEncoding.textContent x
                        |RNvr (x,_) -> x.eval (program))
                    |> List.filter (fun s -> s <> "")
                    |> fun s -> String.Join(",",s)
                program.codewritein("Print \\("+code+"\\)\n")
                program.codewritein "<br/>\n"
            |Python ->
                let hasNumeric = lst.data |> List.exists (function RNvr _ -> true | _ -> false)
                let int0string_format_C =
                    "%"+program.numFormat.iFormat.ToString()+"d"
                let double0string_format_C =
                    let a,b = program.numFormat.dFormat
                    "%"+a.ToString()+"."+b.ToString()+"e"
                let format =
                    lst.data
                    |> List.map (fun (q:reduceExprString) ->
                        match q with
                        |RStr x -> if hasNumeric then x.Replace("%", "%%") else x
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
                let literal = OutputTextLiteral.python format
                program.codewritein("print(" +
                                    (if code = "" then literal else literal + " %(" + code + ")") +
                                    ")\n")
            |JavaScript ->
                let parts =
                    lst.data
                    |> List.collect (function
                        |RStr value -> [OutputTextLiteral.javaScript value]
                        |RNvr (value,_) when value.etype = Zt ->
                            ["String(" + (Re value).eval program + ")";
                             "String(" + (Im value).eval program + ")"]
                        |RNvr (value,_) -> ["String(" + value.eval program + ")"])
                program.codewritein("print(" +
                                    (if List.isEmpty parts then "\"\"" else String.concat " + " parts) +
                                    ");\n")
            |PHP ->
                let parts =
                    lst.data
                    |> List.collect (function
                        |RStr value -> [PhpEncoding.stringLiteral value]
                        |RNvr (value,_) when value.etype = Zt ->
                            ["(" + (Re value).eval program + ")";
                             "(" + (Im value).eval program + ")"]
                        |RNvr (value,_) -> [value.eval program])
                program.writePhpStatement("print(" +
                                          (if List.isEmpty parts then "\"\"" else String.concat " . " parts) +
                                          ");")
            |Numeric ->
                for v in lst.data do
                    match v with
                    |RNvr (Int x,_) -> printf "%d " x
                    |RNvr (Dbl x,_) -> printf "%e " x
                    |RNvr (Cpx (re,im),_) -> printf "%e %e " re im
                    |_ -> ()
        ///<summary>文字列を画面表示</summary>
        static member internal sWith (program:Aqualis) (str:string) =
            match program.language with
            |Fortran ->
                program.codewritein("print *, " + OutputTextLiteral.fortran str + "\n")
            |C99 ->
                program.codewritein("fputs(" + OutputTextLiteral.c str + ", stdout);\n")
            |LaTeX ->
                program.codewritein("print, \""+str+"\"\n")
            |HTML ->
                program.codewritein(HtmlEncoding.textContent ("Print \\(" + str + "\\)") + "\n")
                program.codewritein "<br/>\n"
            |HTMLSequenceDiagram ->
                program.codewritein(HtmlEncoding.textContent ("Print \\(" + str + "\\)") + "\n")
                program.codewritein "<br/>\n"
            |Python ->
                program.codewritein("print(" + OutputTextLiteral.python str + ")\n")
            |JavaScript ->
                program.codewritein("print(" + OutputTextLiteral.javaScript str + ")\n")
            |PHP ->
                program.writePhpStatement("print(" + PhpEncoding.stringLiteral str + ");")
            |Numeric ->
                printfn "%s" str

        ///<summary>1個の項目を画面表示</summary>
    type ContextPrint internal (c:Aqualis) =
        
        /// Gets the owning generation context.
        member _.Environment with get() = c
        
        /// Writes literal text to the generated output.
        member _.s(str:string) =
            match c.CodeFile with
            |Some _ -> PrintEmitter.sWith c str
            |None -> printfn "%s" str

        /// Writes an expression string to the generated output.
        member _.tt(value:exprString) =
            match c.CodeFile with
            |Some _ ->
                Aqualis.merge c value.Context |> ignore
                PrintEmitter.ttWith c value
            |None ->
                for item in value.data do
                    match item with
                    |RNvr (Int x,_) -> printf "%d " x
                    |RNvr (Dbl x,_) -> printf "%e " x
                    |RNvr (Cpx(re,im),_) -> printf "%e %e " re im
                    |RStr text -> printf "%s" text
                    |_ -> ()

        /// Writes a numeric expression to the generated output.
        member this.t(value:int0) = this.tt(iv value)
        /// Writes a numeric expression to the generated output.
        member this.t(value:double0) = this.tt(dv value)
        /// Writes a numeric expression to the generated output.
        member this.t(value:complex0) = this.tt(zv value)

    /// Adds formatted output to Aqualis.
    [<AutoOpen>]
    module CompilationEnvironmentPrintExtensions =
        type Aqualis with
            ///<summary>画面表示</summary>
            member this.print = ContextPrint this
