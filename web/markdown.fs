namespace Aqualis

open System.IO
open System.Text.RegularExpressions

module private MarkdownRegex =
    let private options =
        RegexOptions.Compiled ||| RegexOptions.CultureInvariant

    let private create pattern =
        Regex(pattern, options, System.TimeSpan.FromMilliseconds 500.0)

    // Inline constructs are ordered from the most structurally specific to the
    // least specific. Regex.Replace walks the input once and selects the first
    // alternative that matches at each position.
    let inlineSyntax =
        create (
            @"(?<code>`(?<codeText>[^`]*)`)" +
            @"|(?<strong>\*\*(?<strongText>[^*]+)\*\*)" +
            @"|(?<italic>\*(?<italicText>[^*]+)\*)" +
            @"|(?<math>\$(?<mathText>.+?)\$)" +
            @"|(?<markdownLink>(?<!!)\[(?<markdownLabel>.*?)\]\((?<markdownUrl>.*?)\))" +
            @"|(?<wikiSectionLabel>(?<!!)\[\[(?<wikiSectionLabelUrl>[^#\|]+)#(?<wikiSectionLabelSection>[^|\]]+)\|(?<wikiSectionLabelText>[^\]]+)\]\])" +
            @"|(?<wikiLabel>(?<!!)\[\[(?<wikiLabelUrl>[^#\|]+)\|(?<wikiLabelText>[^\]]+)\]\])" +
            @"|(?<wikiSection>(?<!!)\[\[(?<wikiSectionUrl>[^#\|]+)#(?<wikiSectionName>[^\]]+)\]\])" +
            @"|(?<wiki>(?<!!)\[\[(?<wikiUrl>[^\]]+)\]\])"
        )
    let codeBlockStart = create @"^```\s*([A-Za-z0-9_+-]+)\s*$"
    let codeBlockEnd = create @"^```\s*$"
    let markdownImage = create @"!\[([^\]]*)\]\(([^)]+)\)"
    let wikiImage = create @"!\[\[([^\]]+)\]\]"
    let heading = create @"^(?<marks>#{1,5})\s+(?<text>.+)$"
    let unorderedList = create @"^(\s*)-\s+(.*)$"
    let orderedList = create @"^( *)(\d+)\.\s+(.*)$"
    let tableCell = create @"\|([^|]*)"
    let tableSeparator = create @"^:?-{3,}:?$"
    let mathBlockDelimiter = create @"^\$\$\s*$"

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
    let private closeAllStack (md:MarkDown<'a>) data stack =
        List.fold (fun d (x:MarkDownContents) ->
            match x with
            |UL _ -> md.CloseUL d
            |OL _ -> md.CloseOL d
            |Math -> md.CloseMath d
            |Table -> md.CloseTable d
            |CodeBlock _ -> md.CloseCodeBlock d) data stack

    let private renderInlineMatch (md:MarkDown<'a>) (m:Match) =
        if m.Groups["code"].Success then
            md.InlineCode m.Groups["codeText"].Value
        elif m.Groups["strong"].Success then
            md.Strong m.Groups["strongText"].Value
        elif m.Groups["italic"].Success then
            md.Italic m.Groups["italicText"].Value
        elif m.Groups["math"].Success then
            md.InlineMath m.Groups["mathText"].Value
        elif m.Groups["markdownLink"].Success then
            md.Link (Some m.Groups["markdownLabel"].Value) m.Groups["markdownUrl"].Value None
        elif m.Groups["wikiSectionLabel"].Success then
            md.Link
                (Some m.Groups["wikiSectionLabelText"].Value)
                m.Groups["wikiSectionLabelUrl"].Value
                (Some m.Groups["wikiSectionLabelSection"].Value)
        elif m.Groups["wikiLabel"].Success then
            md.Link (Some m.Groups["wikiLabelText"].Value) m.Groups["wikiLabelUrl"].Value None
        elif m.Groups["wikiSection"].Success then
            md.Link None m.Groups["wikiSectionUrl"].Value (Some m.Groups["wikiSectionName"].Value)
        elif m.Groups["wiki"].Success then
            let url = m.Groups["wikiUrl"].Value
            md.Link (Some url) url None
        else
            m.Value

    let private isTableSeparator (cells:string list) =
        let rec allButLastAreSeparators (remaining:string list) =
            match remaining with
            |[]
            |[_] -> true
            |cell::rest ->
                MarkdownRegex.tableSeparator.IsMatch(cell) &&
                allButLastAreSeparators rest
        allButLastAreSeparators cells

    let rec readmd (md:MarkDown<'a>) (rd:StreamReader) (stack:list<MarkDownContents>) (data:list<'a>) =
        /// 閉じていないタグをすべて閉じる
        match rd.ReadLine() with
        |null -> 
            rd.Close()
            closeAllStack md data stack
        |code ->
            let normalizedCode =
                match stack with
                |CodeBlock _ :: _ ->
                    code
                |_ ->
                    code
                        .Replace("\t", "  ")
                        .Replace("\\]", "]")
                        .Replace("\\[", "[")
                        // 強調
            let codeBlockStartMatch =
                MarkdownRegex.codeBlockStart.Match(normalizedCode)
            let isCodeBlockEnd =
                MarkdownRegex.codeBlockEnd.IsMatch(normalizedCode)
            let convertedCode =
                match stack with
                |CodeBlock _::_ -> normalizedCode
                |_ when codeBlockStartMatch.Success || isCodeBlockEnd -> normalizedCode
                |_ ->
                    MarkdownRegex.inlineSyntax.Replace(
                        normalizedCode,
                        MatchEvaluator (renderInlineMatch md))
            let headingMatch = MarkdownRegex.heading.Match(convertedCode)
            let mi1 = MarkdownRegex.wikiImage.Match(convertedCode)
            let mi2 = MarkdownRegex.markdownImage.Match(convertedCode)
            let mul = MarkdownRegex.unorderedList.Match(convertedCode)
            let mol = MarkdownRegex.orderedList.Match(convertedCode)
            let tableData = 
                if convertedCode.StartsWith "|" && convertedCode.EndsWith "|" then
                    MarkdownRegex.tableCell.Matches(convertedCode)
                    |> Seq.cast<Match>
                    |> Seq.map (fun m -> m.Groups.[1].Value.Trim())
                    |> Seq.toList
                else
                    []
            match convertedCode,stack with
            |_,CodeBlock _::rest when isCodeBlockEnd ->
                readmd md rd rest (md.CloseCodeBlock data)
            |s,CodeBlock _::_ ->
                readmd md rd stack (md.InsideCodeBlock s data)
            |"",_ ->
                //閉じていないタグをすべて閉じる
                readmd md rd [] (closeAllStack md data stack)
            |_ when headingMatch.Success ->
                let level = headingMatch.Groups["marks"].Value.Length
                let text = headingMatch.Groups["text"].Value
                let closedData = closeAllStack md data stack
                let sectionData =
                    match level with
                    |1 -> md.Section1 text closedData
                    |2 -> md.Section2 text closedData
                    |3 -> md.Section3 text closedData
                    |4 -> md.Section4 text closedData
                    |5 -> md.Section5 text closedData
                    |_ -> closedData
                readmd md rd [] sectionData
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
            |_ when codeBlockStartMatch.Success ->
                let lang = codeBlockStartMatch.Groups.[1].Value
                readmd md rd (CodeBlock lang::stack) (md.OpenCodeBlock lang data)
            |s,Math ::_ when MarkdownRegex.mathBlockDelimiter.IsMatch(s) ->
                readmd md rd stack.Tail (md.CloseMath data)
            |s,Math ::_ ->
                readmd md rd stack (md.InsideMath s data)
            |s,_ when MarkdownRegex.mathBlockDelimiter.IsMatch(s) ->
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
