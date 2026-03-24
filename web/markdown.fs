namespace Aqualis

open System.IO
open System.Text.RegularExpressions

type MarkDownContents =
    /// 箇条書き（インデント）
    |UL of int
    /// 番号付き箇条書き（インデント）
    |OL of int
    /// 表組み
    |Table
    /// 数式
    |Math
    /// コードブロック（言語）
    |CodeBlock of string
    
/// マークダウン解読要素（解読中に収集するデータ型を'aに指定）
type MarkDown<'a> =
    {
        Link : option<string> -> string -> option<string> -> string
        Italic : string -> string
        Strong : string -> string
        InlineMath : string -> string
        InlineCode : string -> string
        Section1 : string -> list<'a> -> list<'a>
        Section2 : string -> list<'a> -> list<'a>
        Section3 : string -> list<'a> -> list<'a>
        Section4 : string -> list<'a> -> list<'a>
        Section5 : string -> list<'a> -> list<'a>
        Image : string -> list<'a> -> list<'a>
        OpenUL : list<'a> -> list<'a>
        ItemUL : string -> list<'a> -> list<'a>
        CloseUL : list<'a> -> list<'a>
        OpenOL : list<'a> -> list<'a>
        ItemOL : string -> list<'a> -> list<'a>
        CloseOL : list<'a> -> list<'a>
        OpenCodeBlock : string -> list<'a> -> list<'a>
        InsideCodeBlock : string -> list<'a> -> list<'a>
        CloseCodeBlock : list<'a> -> list<'a>
        OpenMath : list<'a> -> list<'a>
        InsideMath : string -> list<'a> -> list<'a>
        CloseMath : list<'a> -> list<'a>
        OpenTable : list<'a> -> list<'a>
        TableHeader : list<string> -> list<'a> -> list<'a>
        TableData : list<string> -> list<'a> -> list<'a>
        CloseTable : list<'a> -> list<'a>
        Paragraph : string -> list<'a> -> list<'a>
        Direct : string -> list<'a> -> list<'a>
    }

[<AutoOpen>]
module markDown =
    let rec readmd (md:MarkDown<'a>) (rd:StreamReader) (stack:list<MarkDownContents>) (data:list<'a>) =
        /// 閉じていないタグをすべて閉じる
        let closeAllStack data stack =
            List.fold (fun d (x:MarkDownContents) -> 
                match x with
                |UL _ -> md.CloseUL d
                |OL _ -> md.CloseOL d
                |Math -> md.CloseMath d
                |Table -> md.CloseTable d
                |CodeBlock _ -> md.CloseCodeBlock d ) data stack
                
        match rd.ReadLine() with
        |null -> 
            rd.Close()
            closeAllStack data stack
        |code ->
            let convertedCode =
                match stack with
                |CodeBlock _ :: _ ->
                    code
                |_ ->
                    code
                    |> fun s -> s.Replace("\t", "  ")
                    |> fun s -> s.Replace("\\]", "]")
                    |> fun s -> s.Replace("\\[", "[")
                    |> fun s -> 
                        // 強調
                        let pattern = @"\*\*([^*]+)\*\*"
                        let m = Regex.Match(s, pattern)
                        if m.Success then
                            Regex.Replace(s, pattern, MatchEvaluator (fun m -> md.Strong m.Groups[1].Value))
                        else
                            s
                    |> fun s -> 
                        // 斜体
                        let pattern = @"\*([^*]+)\*"
                        let m = Regex.Match(s, pattern)
                        if m.Success then
                            Regex.Replace(s, pattern, MatchEvaluator (fun m -> md.Italic m.Groups[1].Value))
                        else
                            s 
                    |> fun s -> 
                        // インライン数式
                        let pattern = @"\$(.+?)\$"
                        let m = Regex.Match(s, pattern)
                        if m.Success then
                            Regex.Replace(s, pattern, MatchEvaluator (fun m -> md.InlineMath m.Groups[1].Value))
                        else
                            s
                    |> fun s -> 
                        // インラインコード
                        if not <| Regex.IsMatch(s, @"^```\s*([A-Za-z0-9_+-]+)\s*$") && not <| Regex.IsMatch(s, @"^```\s*$") then 
                            let pattern = @"`([^`]*)`"
                            let m = Regex.Match(s, pattern)
                            if m.Success then
                                Regex.Replace(s, pattern, MatchEvaluator (fun m -> md.InlineCode m.Groups[1].Value))
                            else
                                s
                        else
                            s
                    |> fun s -> 
                        // リンク
                        let pattern = @"(?<!!)\[(.*?)\]\((.*?)\)"
                        let m = Regex.Match(s, pattern)
                        let p = Regex.Match(s, @"!\[([^\]]*)\]\(([^)]+)\)")
                        Regex.Replace(
                            s,
                            pattern,
                            MatchEvaluator(fun m ->
                                let lab = m.Groups.[1].Value
                                let url = m.Groups.[2].Value
                                md.Link (Some lab) url None
                            )
                        )
                    |> fun s -> 
                        // リンク（表示テキスト指定）
                        let pattern = @"(?<!!)\[\[([^#\|]+)\|([^\]]+)\]\]"
                        let m = Regex.Match(s, pattern)
                        Regex.Replace(
                            s,
                            pattern,
                            MatchEvaluator(fun m ->
                                let url = m.Groups.[1].Value
                                let lab = m.Groups.[2].Value
                                md.Link (Some lab) url None
                            )
                        )
                    |> fun s -> 
                        // リンク（セクション、表示テキスト指定）
                        let pattern = @"(?<!!)\[\[([^#\|]+)#([^|\]]+)\|([^\]]+)\]\]"
                        let m = Regex.Match(s, pattern)
                        Regex.Replace(
                            s,
                            pattern,
                            MatchEvaluator(fun m ->
                                let url = m.Groups.[1].Value
                                let sec = m.Groups.[2].Value
                                let lab = m.Groups.[3].Value
                                md.Link (Some lab) url (Some sec)
                            )
                        )
                    |> fun s -> 
                        // リンク（セクション）
                        let pattern = @"(?<!!)\[\[([^#\|]+)#([^\]]+)\]\]"
                        let m = Regex.Match(s, pattern)
                        Regex.Replace(
                            s,
                            pattern,
                            MatchEvaluator(fun m ->
                                let url = m.Groups.[1].Value
                                let sec = m.Groups.[2].Value
                                md.Link None url (Some sec)
                            )
                        )
                    |> fun s -> 
                        // リンク
                        let pattern = @"(?<!!)\[\[([^\]]+)\]\]"
                        let m = Regex.Match(s, pattern)
                        let p = Regex.Match(s, @"!\[\[([^\]]+)\]\]")
                        Regex.Replace(
                            s,
                            pattern,
                            MatchEvaluator(fun m ->
                                let url = m.Groups.[1].Value
                                let lab = m.Groups.[1].Value
                                md.Link (Some lab) url None
                            )
                        )
            let mh1 = Regex.Match(convertedCode, @"^#(?!#)\s+(.+)$")
            let mh2 = Regex.Match(convertedCode, @"^##(?!#)\s+(.+)$")
            let mh3 = Regex.Match(convertedCode, @"^###(?!#)\s+(.+)$")
            let mh4 = Regex.Match(convertedCode, @"^####(?!#)\s+(.+)$")
            let mh5 = Regex.Match(convertedCode, @"^#####(?!#)\s+(.+)$")
            let mi1 = Regex.Match(convertedCode, @"!\[\[([^\]]+)\]\]")
            let mi2 = Regex.Match(convertedCode, @"!\[([^\]]*)\]\(([^)]+)\)")
            let mul = Regex.Match(convertedCode, @"^(\s*)-\s+(.*)$")
            let mol = Regex.Match(convertedCode, @"^( *)(\d+)\.\s+(.*)$")
            let mcb = Regex.Match(convertedCode, @"^```\s*([A-Za-z0-9_+-]+)\s*$")
            let tableData = 
                if convertedCode.StartsWith "|" && convertedCode.EndsWith "|" then
                    Regex.Matches(convertedCode, @"\|([^|]*)")
                    |> Seq.cast<Match>
                    |> Seq.map (fun m -> m.Groups.[1].Value.Trim())
                    |> Seq.toList
                else
                    []
            let isTableSeparator (s:list<string>) =
                (match s with | [] -> [] | _  -> s |> List.rev |> List.tail |> List.rev)
                |> List.fold (fun acc cell -> 
                    acc && Regex.IsMatch(cell, @"^:?-{3,}:?$")) true
            match convertedCode,stack with
            |s,_ when Regex.IsMatch(s, @"^```\s*$") ->
                readmd md rd stack.Tail (md.CloseCodeBlock data)
            |s,CodeBlock _::_ ->
                readmd md rd stack (md.InsideCodeBlock s data)
            |"",_ ->
                //閉じていないタグをすべて閉じる
                let data2 = List.fold (fun d (x:MarkDownContents) -> 
                    match x with
                    |UL _ -> md.CloseUL d
                    |OL _ -> md.CloseOL d
                    |Math -> md.CloseMath d
                    |Table -> md.CloseTable d
                    |CodeBlock _ -> md.CloseCodeBlock d ) data stack
                readmd md rd [] data2
            |_ when mh1.Success ->
                readmd md rd [] (md.Section1 mh1.Groups.[1].Value (closeAllStack data stack))
            |_ when mh2.Success ->
                readmd md rd [] (md.Section2 mh2.Groups.[1].Value (closeAllStack data stack))
            |_ when mh3.Success ->
                readmd md rd [] (md.Section3 mh3.Groups.[1].Value (closeAllStack data stack))
            |_ when mh4.Success ->
                readmd md rd [] (md.Section4 mh4.Groups.[1].Value (closeAllStack data stack))
            |_ when mh5.Success ->
                readmd md rd [] (md.Section5 mh5.Groups.[1].Value (closeAllStack data stack))
            |_ when mi1.Success ->
                readmd md rd stack (md.Image mi1.Groups.[1].Value data)
            |_ when mi2.Success ->
                readmd md rd stack (md.Image mi2.Groups.[2].Value data)
            |_,Table::_ when isTableSeparator tableData ->
                readmd md rd stack data
            |_,Table::_ ->
                readmd md rd stack (md.TableData tableData data)
            |_,_ when tableData.Length>0 ->
                readmd md rd (Table::stack) (md.TableHeader tableData (md.OpenTable data))
            |_ when mcb.Success ->
                let lang = mcb.Groups.[1].Value
                readmd md rd (CodeBlock lang::stack) (md.OpenCodeBlock lang data)
            |s,Math ::_ when Regex.IsMatch(s, @"^\$\$\s*$") ->
                readmd md rd stack.Tail (md.CloseMath data)
            |s,Math ::_ ->
                readmd md rd stack (md.InsideMath s data)
            |s,_ when Regex.IsMatch(s, @"^\$\$\s*$") ->
                readmd md rd (Math::stack) (md.OpenMath data)
            |_ when mul.Success ->
                let i = mul.Groups.[1].Value.Length/2
                let text = mul.Groups.[2].Value
                match stack with
                |UL j::_ when i=j -> 
                    //前行が同じインデントの箇条書き
                    readmd md rd stack (md.ItemUL text data)
                |UL j::st when i>j -> 
                    //前行より深いインデントの箇条書き
                    readmd md rd (UL i::stack) (md.ItemUL text (md.OpenUL data))
                |UL j::st when i<j -> 
                    //前行より浅いインデントの箇条書き
                    readmd md rd st (md.ItemUL text (md.CloseUL data))
                |OL j::st when i=j -> 
                    //前行が同じインデントの箇条書き
                    readmd md rd (UL i::st) (md.ItemUL text (md.OpenUL (md.CloseOL data)))
                |OL j::_ when i>j -> 
                    //前行より深いインデントの箇条書き
                    readmd md rd (UL i::stack) (md.ItemUL text (md.OpenUL data))
                |OL j::st when i<j -> 
                    //前行より浅いインデントの箇条書き
                    readmd md rd st (md.ItemUL text (md.CloseOL data))
                |_ ->
                    //前行が箇条書きでない
                    readmd md rd (UL i::stack) (md.ItemUL text (md.OpenUL data))
            |_ when mol.Success ->
                let i = mol.Groups.[1].Value.Length/2
                let text = mol.Groups.[3].Value
                match stack with
                |OL j::_ when i=j -> 
                    //前行が同じインデントの箇条書き
                    readmd md rd stack (md.ItemOL text data)
                |OL j::st when i>j -> 
                    //前行より深いインデントの箇条書き
                    readmd md rd (OL i::stack) (md.ItemOL text (md.OpenOL data))
                |OL j::st when i<j -> 
                    //前行より浅いインデントの箇条書き
                    readmd md rd st (md.ItemOL text (md.CloseOL data))
                |UL j::st when i=j -> 
                    //前行が同じインデントの箇条書き
                    readmd md rd (OL i::st) (md.ItemOL text (md.OpenOL (md.CloseUL data)))
                |UL j::_ when i>j -> 
                    //前行より深いインデントの箇条書き
                    readmd md rd (OL i::stack) (md.ItemOL text (md.OpenOL data))
                |UL j::st when i<j -> 
                    //前行より浅いインデントの箇条書き
                    readmd md rd st (md.ItemOL text (md.CloseUL data))
                |_ ->
                    //前行が箇条書きでない
                    readmd md rd (OL i::stack) (md.ItemOL text (md.OpenOL data))
            |s,_ ->
                match stack with
                |UL _::st ->
                    readmd md rd st (md.Direct s (md.CloseUL data))
                |OL _::st ->
                    readmd md rd st (md.Direct s (md.CloseOL data))
                |Math ::st ->
                    readmd md rd st (md.Direct s (md.CloseMath data))
                |CodeBlock _ ::st ->
                    readmd md rd st (md.Direct s (md.CloseCodeBlock data))
                |_ ->
                    readmd md rd stack (md.Paragraph s data)
                    
    let readMarkDown (MarkDownfilename:string) (md:MarkDown<'a>) = 
        let rd = new StreamReader(MarkDownfilename)
        readmd md rd [] []
        
    let convertHTML (MarkDownfilename:string) (HTMLfilename:string) =
        let wr = new StreamWriter(HTMLfilename)
        let md = {
            Link = fun label url section ->
                "<a href=" +
                url +
                (match section with 
                |None -> ""
                |Some s -> "#" + s) +
                ">" +
                (match label with
                |None -> 
                    url + 
                    match section with 
                    |None -> ""
                    |Some s -> ":" + s
                |Some s -> s) +
                "</a>"
            Italic = fun s ->
                "<i>"+s+"</i>"
            Strong = fun s ->
                "<strong>"+s+"</strong>"
            InlineMath = fun s ->
                "\\("+s+"\\)"
            InlineCode = fun s ->
                "<code>"+s+"</code>"
            Section1 = fun s data -> 
                wr.WriteLine("<h1>"+s+"</h1>")
                data
            Section2 = fun s data -> 
                wr.WriteLine("<h2>"+s+"</h2>")
                data
            Section3 = fun s data -> 
                wr.WriteLine("<h3>"+s+"</h3>")
                data
            Section4 = fun s data -> 
                wr.WriteLine("<h4>"+s+"</h4>")
                data
            Section5 = fun s data -> 
                wr.WriteLine("<h5>"+s+"</h5>")
                data
            Image = fun s data -> 
                wr.WriteLine("<div><img src=\"img/"+s+"\"/></div>")
                data
            OpenUL = fun data -> 
                wr.WriteLine "<ul>"
                data
            ItemUL = fun s data -> 
                wr.WriteLine ("<li>"+s+"</li>")
                data
            CloseUL = fun data -> 
                wr.WriteLine "</ul>"
                data
            OpenOL = fun data -> 
                wr.WriteLine "<ol>"
                data
            ItemOL = fun s data -> 
                wr.WriteLine ("<li>"+s+"</li>")
                data
            CloseOL = fun data -> 
                wr.WriteLine "</ol>"
                data
            OpenMath = fun data ->
                wr.WriteLine "\\["
                data
            InsideMath = fun s data ->
                wr.WriteLine s
                data
            CloseMath = fun data ->
                wr.WriteLine "\\]"
                data
            OpenCodeBlock = fun lang data -> 
                wr.WriteLine "<pre class = \"codeboxB\" ><code>"
                data
            InsideCodeBlock = fun s data -> 
                wr.WriteLine s
                data
            CloseCodeBlock = fun data ->
                wr.WriteLine "</code></pre>"
                data
            OpenTable = fun data ->
                wr.WriteLine "<table>"
                data
            TableHeader = fun s data ->
                wr.Write "<tr>"
                s |> List.iter (fun s -> wr.Write ("<th>"+s+"</th>"))
                wr.WriteLine "</tr>"
                data
            TableData = fun s data ->
                wr.Write "<tr>"
                s |> List.iter (fun s -> wr.Write ("<td>"+s+"</td>"))
                wr.WriteLine "</tr>"
                data
            CloseTable = fun data ->
                wr.WriteLine "</table>"
                data
            Paragraph = fun s data -> 
                wr.WriteLine ("<p>"+s+"</p>")
                data
            Direct = fun s data -> 
                wr.WriteLine s
                data
            }
        ignore <| readMarkDown MarkDownfilename md
        wr.Close()
