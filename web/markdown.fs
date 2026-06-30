namespace Aqualis

open System.IO
open System
open System.Net
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

type InlineNode =
    |Text of string
    |Strong of string
    |Italic of string
    |InlineMath of string
    |InlineCode of string
    |Link of label:string option * url:string * section:string option
    
/// マークダウン解読要素（解読中に収集するデータ型を'aに指定）
type MarkDown<'a> =
    {
        Section1 : InlineNode list -> list<'a> -> list<'a>
        Section2 : InlineNode list -> list<'a> -> list<'a>
        Section3 : InlineNode list -> list<'a> -> list<'a>
        Section4 : InlineNode list -> list<'a> -> list<'a>
        Section5 : InlineNode list -> list<'a> -> list<'a>
        Image : string -> string -> list<'a> -> list<'a>
        OpenUL : list<'a> -> list<'a>
        ItemUL : InlineNode list -> list<'a> -> list<'a>
        CloseUL : list<'a> -> list<'a>
        OpenOL : list<'a> -> list<'a>
        ItemOL : InlineNode list -> list<'a> -> list<'a>
        CloseOL : list<'a> -> list<'a>
        OpenCodeBlock : string -> list<'a> -> list<'a>
        InsideCodeBlock : string -> list<'a> -> list<'a>
        CloseCodeBlock : list<'a> -> list<'a>
        OpenMath : list<'a> -> list<'a>
        InsideMath : string -> list<'a> -> list<'a>
        CloseMath : list<'a> -> list<'a>
        OpenTable : list<'a> -> list<'a>
        TableHeader : list<InlineNode list> -> list<'a> -> list<'a>
        TableData : list<InlineNode list> -> list<'a> -> list<'a>
        CloseTable : list<'a> -> list<'a>
        Paragraph : InlineNode list -> list<'a> -> list<'a>
        Direct : InlineNode list -> list<'a> -> list<'a>
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

    let private inlineNode (m:Match) =
        if m.Groups["code"].Success then
            InlineCode m.Groups["codeText"].Value
        elif m.Groups["strong"].Success then
            Strong m.Groups["strongText"].Value
        elif m.Groups["italic"].Success then
            Italic m.Groups["italicText"].Value
        elif m.Groups["math"].Success then
            InlineMath m.Groups["mathText"].Value
        elif m.Groups["markdownLink"].Success then
            Link (
                Some m.Groups["markdownLabel"].Value,
                m.Groups["markdownUrl"].Value,
                None)
        elif m.Groups["wikiSectionLabel"].Success then
            Link (
                Some m.Groups["wikiSectionLabelText"].Value,
                m.Groups["wikiSectionLabelUrl"].Value,
                Some m.Groups["wikiSectionLabelSection"].Value)
        elif m.Groups["wikiLabel"].Success then
            Link (
                Some m.Groups["wikiLabelText"].Value,
                m.Groups["wikiLabelUrl"].Value,
                None)
        elif m.Groups["wikiSection"].Success then
            Link (
                None,
                m.Groups["wikiSectionUrl"].Value,
                Some m.Groups["wikiSectionName"].Value)
        elif m.Groups["wiki"].Success then
            let url = m.Groups["wikiUrl"].Value
            Link (Some url, url, None)
        else
            Text m.Value

    let private parseInline (value:string) =
        let nodes = ResizeArray<InlineNode>()
        let mutable position = 0

        for item in MarkdownRegex.inlineSyntax.Matches(value) do
            if item.Index > position then
                nodes.Add(Text(value.Substring(position, item.Index - position)))
            nodes.Add(inlineNode item)
            position <- item.Index + item.Length

        if position < value.Length then
            nodes.Add(Text(value.Substring(position)))

        nodes |> Seq.toList

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
            let headingMatch = MarkdownRegex.heading.Match(normalizedCode)
            let mi1 = MarkdownRegex.wikiImage.Match(normalizedCode)
            let mi2 = MarkdownRegex.markdownImage.Match(normalizedCode)
            let mul = MarkdownRegex.unorderedList.Match(normalizedCode)
            let mol = MarkdownRegex.orderedList.Match(normalizedCode)
            let tableData = 
                if normalizedCode.StartsWith "|" && normalizedCode.EndsWith "|" then
                    MarkdownRegex.tableCell.Matches(normalizedCode)
                    |> Seq.cast<Match>
                    |> Seq.map (fun m -> m.Groups.[1].Value.Trim())
                    |> Seq.toList
                else
                    []
            match normalizedCode,stack with
            |_,CodeBlock _::rest when isCodeBlockEnd ->
                readmd md rd rest (md.CloseCodeBlock data)
            |s,CodeBlock _::_ ->
                readmd md rd stack (md.InsideCodeBlock s data)
            |"",_ ->
                //閉じていないタグをすべて閉じる
                readmd md rd [] (closeAllStack md data stack)
            |_ when headingMatch.Success ->
                let level = headingMatch.Groups["marks"].Value.Length
                let text = headingMatch.Groups["text"].Value |> parseInline
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
                let source = mi1.Groups.[1].Value
                readmd md rd stack (md.Image source source data)
            |_ when mi2.Success ->
                readmd md rd stack (
                    md.Image mi2.Groups.[1].Value mi2.Groups.[2].Value data)
            |_,Table::_ when isTableSeparator tableData ->
                readmd md rd stack data
            |_,Table::_ ->
                readmd md rd stack (
                    md.TableData (tableData |> List.map parseInline) data)
            |_,_ when tableData.Length>0 ->
                readmd md rd (Table::stack) (
                    md.TableHeader
                        (tableData |> List.map parseInline)
                        (md.OpenTable data))
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
                let text = mul.Groups.[2].Value |> parseInline
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
                let text = mol.Groups.[3].Value |> parseInline
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
                    readmd md rd st (md.Direct (parseInline s) (md.CloseUL data))
                |OL _::st ->
                    readmd md rd st (md.Direct (parseInline s) (md.CloseOL data))
                |Math ::st ->
                    readmd md rd st (md.Direct (parseInline s) (md.CloseMath data))
                |CodeBlock _ ::st ->
                    readmd md rd st (md.Direct (parseInline s) (md.CloseCodeBlock data))
                |_ ->
                    readmd md rd stack (md.Paragraph (parseInline s) data)
                    
    let readMarkDown (MarkDownfilename:string) (md:MarkDown<'a>) =
        use rd = new StreamReader(MarkDownfilename)
        readmd md rd [] []

    let private encodeHtml (value:string) =
        WebUtility.HtmlEncode(value)

    let private sanitizeUrl (value:string) =
        let value = value.Trim()
        match Uri.TryCreate(value, UriKind.RelativeOrAbsolute) with
        |true, uri when
            not uri.IsAbsoluteUri ||
            uri.Scheme.Equals(Uri.UriSchemeHttp, StringComparison.OrdinalIgnoreCase) ||
            uri.Scheme.Equals(Uri.UriSchemeHttps, StringComparison.OrdinalIgnoreCase) ->
            value
        |_ ->
            "#"

    let private renderInline nodes =
        nodes
        |> List.map (function
            |Text value -> encodeHtml value
            |Strong value -> "<strong>" + encodeHtml value + "</strong>"
            |Italic value -> "<i>" + encodeHtml value + "</i>"
            |InlineMath value -> "\\(" + encodeHtml value + "\\)"
            |InlineCode value -> "<code>" + encodeHtml value + "</code>"
            |Link(label, url, section) ->
                let href =
                    sanitizeUrl url +
                    (section
                     |> Option.map (fun value -> "#" + value)
                     |> Option.defaultValue "")
                let text =
                    label
                    |> Option.defaultValue (
                        url +
                        (section
                         |> Option.map (fun value -> ":" + value)
                         |> Option.defaultValue ""))
                "<a href=\"" + encodeHtml href + "\">" +
                encodeHtml text +
                "</a>")
        |> String.concat ""

    let convertHTML (MarkDownfilename:string) (HTMLfilename:string) =
        use wr = new StreamWriter(HTMLfilename)
        let md = {
            Section1 = fun value data ->
                wr.WriteLine("<h1>"+renderInline value+"</h1>")
                data
            Section2 = fun value data ->
                wr.WriteLine("<h2>"+renderInline value+"</h2>")
                data
            Section3 = fun value data ->
                wr.WriteLine("<h3>"+renderInline value+"</h3>")
                data
            Section4 = fun value data ->
                wr.WriteLine("<h4>"+renderInline value+"</h4>")
                data
            Section5 = fun value data ->
                wr.WriteLine("<h5>"+renderInline value+"</h5>")
                data
            Image = fun alt source data ->
                let source = "img/" + sanitizeUrl source
                wr.WriteLine(
                    "<div><img src=\"" + encodeHtml source +
                    "\" alt=\"" + encodeHtml alt + "\"/></div>")
                data
            OpenUL = fun data ->
                wr.WriteLine "<ul>"
                data
            ItemUL = fun value data ->
                wr.WriteLine ("<li>"+renderInline value+"</li>")
                data
            CloseUL = fun data ->
                wr.WriteLine "</ul>"
                data
            OpenOL = fun data ->
                wr.WriteLine "<ol>"
                data
            ItemOL = fun value data ->
                wr.WriteLine ("<li>"+renderInline value+"</li>")
                data
            CloseOL = fun data ->
                wr.WriteLine "</ol>"
                data
            OpenMath = fun data ->
                wr.WriteLine "\\["
                data
            InsideMath = fun value data ->
                wr.WriteLine(encodeHtml value)
                data
            CloseMath = fun data ->
                wr.WriteLine "\\]"
                data
            OpenCodeBlock = fun _ data ->
                wr.WriteLine "<pre class=\"codeboxB\"><code>"
                data
            InsideCodeBlock = fun value data ->
                wr.WriteLine(encodeHtml value)
                data
            CloseCodeBlock = fun data ->
                wr.WriteLine "</code></pre>"
                data
            OpenTable = fun data ->
                wr.WriteLine "<table>"
                data
            TableHeader = fun cells data ->
                wr.Write "<tr>"
                cells
                |> List.iter (fun value ->
                    wr.Write("<th>"+renderInline value+"</th>"))
                wr.WriteLine "</tr>"
                data
            TableData = fun cells data ->
                wr.Write "<tr>"
                cells
                |> List.iter (fun value ->
                    wr.Write("<td>"+renderInline value+"</td>"))
                wr.WriteLine "</tr>"
                data
            CloseTable = fun data ->
                wr.WriteLine "</table>"
                data
            Paragraph = fun value data ->
                wr.WriteLine("<p>"+renderInline value+"</p>")
                data
            Direct = fun value data ->
                wr.WriteLine(renderInline value)
                data
            }
        ignore <| readMarkDown MarkDownfilename md
