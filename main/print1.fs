namespace Aqualis
    
    open System
    
    ///<summary>画面表示</summary>
    type print () =
        ///<summary>変数リストを画面表示</summary>
        static member s (lst:exprString list)  = 
            match pr.language with
            |Fortran ->
                let clist = 
                    [for q in lst do
                        match q with
                        |Str x ->
                            yield "\""+x+"\""
                        |Nvr x when x.etype = Zt ->
                            yield (Re x).eval pr
                            yield (Im x).eval pr
                        |Nvr x ->
                            yield x.eval pr
                        |_ -> () ]
                pr.cwriter.codewrite("print *, " + String.concat "," clist + "\n")
            |C99 ->
                let int0string_format_C =
                    "%"+pr.numFormat.iFormat.ToString()+"d"
                let double0string_format_C = 
                    let a,b = pr.numFormat.dFormat
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
                        |Nvr x when x.etype = Zt -> (Re x).eval pr + "," + (Im x).eval pr
                        |Nvr x -> x.eval pr
                        |_ -> "")
                    |> List.filter (fun s -> s <> "")
                    |> fun s -> String.Join(",",s)
                pr.cwriter.codewrite("printf(\""+format+"\\n\","+code+");\n")
            |LaTeX ->
                let code = 
                    lst
                    |> List.map (fun q ->
                        match q with
                        |Str x -> x
                        |Nvr x -> x.eval pr
                        |_ -> "")
                    |> List.filter (fun s -> s <> "")
                    |> fun s -> String.Join(",",s)
                pr.cwriter.codewrite("print, " + code + "\n")
            |HTML ->
                let code = 
                    lst
                    |> List.map (fun q ->
                        match q with
                        |Str x -> x
                        |Nvr x -> x.eval pr
                        |_ -> "")
                    |> List.filter (fun s -> s <> "")
                    |> fun s -> String.Join(",",s)
                pr.cwriter.codewrite("Print \\("+code+"\\)\n")
                pr.cwriter.codewrite "<br/>\n"
            |Python ->
                let int0string_format_C =
                    "%"+pr.numFormat.iFormat.ToString()+"d"
                let double0string_format_C = 
                    let a,b = pr.numFormat.dFormat
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
                        |Nvr x when x.etype = Zt -> (Re x).eval pr + "," + (Im x).eval pr
                        |Nvr x -> x.eval pr
                        |_ -> "")
                    |> List.filter (fun s -> s <> "")
                    |> fun s -> String.Join(",",s)
                pr.cwriter.codewrite("print(\"" + format + "\" %(" + code + "))\n")
            |Numeric ->
                for v in lst do
                    match v with
                    |Nvr (Int x) -> printf "%d " x
                    |Nvr (Dbl x) -> printf "%e " x
                    |Nvr (Cpx (re,im)) -> printf "%e %e " re im
                    |_ -> ()
        ///<summary>文字列を画面表示</summary>
        static member t (str:string) =
            match pr.language with
            |Fortran ->
                pr.cwriter.codewrite("print *, "+"\""+str+"\""+"\n")
            |C99 ->
                pr.cwriter.codewrite("printf(\""+str+"\""+");\n")
            |LaTeX ->
                pr.cwriter.codewrite("print, \""+str+"\"\n")
            |HTML ->
                pr.cwriter.codewrite("Print \\("+str+"\\)\n")
                pr.cwriter.codewrite "<br/>\n"
            |Python ->
                pr.cwriter.codewrite("print(\""+str+"\")\n")
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
