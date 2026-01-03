namespace Aqualis
    
    open System
    
    ///<summary>画面表示</summary>
    type print () =
        ///<summary>変数リストを画面表示</summary>
        static member s (lst:exprString list)  = 
            match programList[prIndex].language with
            |Fortran ->
                let clist = 
                    [for q in lst do
                        match q with
                        |Str x ->
                            yield "\""+x+"\""
                        |Nvr x when x.etype = Zt ->
                            yield (Re x).eval (programList[prIndex])
                            yield (Im x).eval (programList[prIndex])
                        |Nvr x ->
                            yield x.eval (programList[prIndex])
                        |_ -> () ]
                codewritein("print *, " + String.concat "," clist + "\n")
            |C99 ->
                let int0string_format_C =
                    "%"+programList[prIndex].numFormat.iFormat.ToString()+"d"
                let double0string_format_C = 
                    let a,b = programList[prIndex].numFormat.dFormat
                    "%"+a.ToString()+"."+b.ToString()+"e"
                let format = 
                    lst 
                    |> List.map( fun (q:exprString) ->
                        match q with
                        |Str x -> x
                        |Nvr x when x.etype = It 4 -> int0string_format_C
                        |Nvr x when x.etype = Dt -> double0string_format_C
                        |Nvr x when x.etype = Zt -> double0string_format_C + double0string_format_C
                        |_ -> "")
                    |> List.filter (fun s -> s <> "")
                    |> fun s -> String.Join("",s)
                let code =
                    lst
                    |> List.map (fun q ->
                        match q with
                        |Str _ -> ""
                        |Nvr x when x.etype = Zt -> (Re x).eval (programList[prIndex]) + "," + (Im x).eval (programList[prIndex])
                        |Nvr x -> x.eval (programList[prIndex])
                        |_ -> "")
                    |> List.filter (fun s -> s <> "")
                    |> fun s -> String.Join(",",s)
                codewritein("printf(\""+format+"\\n\","+code+");\n")
            |LaTeX ->
                let code = 
                    lst
                    |> List.map (fun q ->
                        match q with
                        |Str x -> x
                        |Nvr x -> x.eval (programList[prIndex])
                        |_ -> "")
                    |> List.filter (fun s -> s <> "")
                    |> fun s -> String.Join(",",s)
                codewritein("print, " + code + "\n")
            |HTML ->
                let code = 
                    lst
                    |> List.map (fun q ->
                        match q with
                        |Str x -> x
                        |Nvr x -> x.eval (programList[prIndex])
                        |_ -> "")
                    |> List.filter (fun s -> s <> "")
                    |> fun s -> String.Join(",",s)
                codewritein("Print \\("+code+"\\)\n")
                codewritein "<br/>\n"
            |HTMLSequenceDiagram ->
                let code = 
                    lst
                    |> List.map (fun q ->
                        match q with
                        |Str x -> x
                        |Nvr x -> x.eval (programList[prIndex])
                        |_ -> "")
                    |> List.filter (fun s -> s <> "")
                    |> fun s -> String.Join(",",s)
                codewritein("Print \\("+code+"\\)\n")
                codewritein "<br/>\n"
            |Python ->
                let int0string_format_C =
                    "%"+programList[prIndex].numFormat.iFormat.ToString()+"d"
                let double0string_format_C = 
                    let a,b = programList[prIndex].numFormat.dFormat
                    "%"+a.ToString()+"."+b.ToString()+"e"
                let format = 
                    lst
                    |> List.map (fun q ->
                        match q with
                        |Str x -> x
                        |Nvr x when x.etype = It 4 -> int0string_format_C
                        |Nvr x when x.etype = Dt  -> double0string_format_C
                        |Nvr x when x.etype = Zt  -> double0string_format_C + double0string_format_C
                        |_ -> "")
                    |> List.filter (fun s -> s <> "")
                    |> fun s -> String.Join("",s)
                let code = 
                    lst
                    |> List.map (fun q ->
                        match q with
                        |Nvr x when x.etype = Zt -> (Re x).eval (programList[prIndex]) + "," + (Im x).eval (programList[prIndex])
                        |Nvr x -> x.eval (programList[prIndex])
                        |_ -> "")
                    |> List.filter (fun s -> s <> "")
                    |> fun s -> String.Join(",",s)
                codewritein("print(\"" + format + "\" %(" + code + "))\n")
            |JavaScript ->
                let int0string_format_C =
                    "%"+programList[prIndex].numFormat.iFormat.ToString()+"d"
                let double0string_format_C = 
                    let a,b = programList[prIndex].numFormat.dFormat
                    "%"+a.ToString()+"."+b.ToString()+"e"
                let format = 
                    lst
                    |> List.map (fun q ->
                        match q with
                        |Str x -> x
                        |Nvr x when x.etype = It 4 -> int0string_format_C
                        |Nvr x when x.etype = Dt  -> double0string_format_C
                        |Nvr x when x.etype = Zt  -> double0string_format_C + double0string_format_C
                        |_ -> "")
                    |> List.filter (fun s -> s <> "")
                    |> fun s -> String.Join("",s)
                let code = 
                    lst
                    |> List.map (fun q ->
                        match q with
                        |Nvr x when x.etype = Zt -> (Re x).eval (programList[prIndex]) + "," + (Im x).eval (programList[prIndex])
                        |Nvr x -> x.eval (programList[prIndex])
                        |_ -> "")
                    |> List.filter (fun s -> s <> "")
                    |> fun s -> String.Join(",",s)
                codewritein("print(" + code + ");\n")
            |PHP ->
                let int0string_format_C =
                    "%"+programList[prIndex].numFormat.iFormat.ToString()+"d"
                let double0string_format_C = 
                    let a,b = programList[prIndex].numFormat.dFormat
                    "%"+a.ToString()+"."+b.ToString()+"e"
                let format = 
                    lst
                    |> List.map (fun q ->
                        match q with
                        |Str x -> x
                        |Nvr x when x.etype = It 4 -> int0string_format_C
                        |Nvr x when x.etype = Dt  -> double0string_format_C
                        |Nvr x when x.etype = Zt  -> double0string_format_C + double0string_format_C
                        |_ -> "")
                    |> List.filter (fun s -> s <> "")
                    |> fun s -> String.Join("",s)
                let code = 
                    lst
                    |> List.map (fun q ->
                        match q with
                        |Nvr x when x.etype = Zt -> (Re x).eval (programList[prIndex]) + "," + (Im x).eval (programList[prIndex])
                        |Nvr x -> x.eval (programList[prIndex])
                        |_ -> "")
                    |> List.filter (fun s -> s <> "")
                    |> fun s -> String.Join(",",s)
                codewritein("print(" + code + ");\n")
            |Numeric ->
                for v in lst do
                    match v with
                    |Nvr (Int x) -> printf "%d " x
                    |Nvr (Dbl x) -> printf "%e " x
                    |Nvr (Cpx (re,im)) -> printf "%e %e " re im
                    |_ -> ()
        ///<summary>文字列を画面表示</summary>
        static member t (str:string) =
            match programList[prIndex].language with
            |Fortran ->
                codewritein("print *, "+"\""+str+"\""+"\n")
            |C99 ->
                codewritein("printf(\""+str+"\""+");\n")
            |LaTeX ->
                codewritein("print, \""+str+"\"\n")
            |HTML ->
                codewritein("Print \\("+str+"\\)\n")
                codewritein "<br/>\n"
            |HTMLSequenceDiagram ->
                codewritein("Print \\("+str+"\\)\n")
                codewritein "<br/>\n"
            |Python ->
                codewritein("print(\""+str+"\")\n")
            |JavaScript ->
                codewritein("print(\""+str+"\")\n")
            |PHP ->
                codewritein("print(\""+str+"\")\n")
            |Numeric ->
                printfn "%s" str
        static member w (ss:exprString) = print.s (match ss with |Str _ |Nvr _ -> [ss] |NSL lst -> lst)
        static member n (ss:list<num0>) = print.s (ss |> List.map (fun x -> Nvr x.Expr))
        ///<summary>1個の項目を画面表示</summary>
        static member c (ss:num0) = print.n [ss]
        ///<summary>2個の項目を画面表示</summary>
        static member cc (s1:num0) (s2:num0) = print.n [s1;s2]
        ///<summary>3個の項目を画面表示</summary>
        static member ccc (s1:num0) (s2:num0) (s3:num0) = print.n [s1;s2;s3]
        ///<summary>4個の項目を画面表示</summary>
        static member cccc (s1:num0) (s2:num0) (s3:num0) (s4:num0) = print.n [s1;s2;s3;s4]
