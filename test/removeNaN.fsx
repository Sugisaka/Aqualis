open System.IO

for a,c,k in [("f90","!","!"); ("c","/*","//")] do
    let rd = new StreamReader(__SOURCE_DIRECTORY__ + @"\test1."+a)
    let wr = new StreamWriter(__SOURCE_DIRECTORY__ + @"\test2."+a)
    let rec edit (lst:list<string>) =
        match rd.ReadLine() with
        |null ->
            for s in lst do wr.WriteLine(s)
            rd.Close()
            wr.Close()
        |t when t.StartsWith(c) ->
            match List.tryFind (fun (s:string) -> s.Contains("NaN") || s.Contains("∞")) lst with
            |None ->
                for s in lst do wr.WriteLine(s)
            |Some _ ->
                for s in lst do wr.WriteLine(k+s)
            edit [t]
        |t ->
            edit (lst@[t])
    edit []