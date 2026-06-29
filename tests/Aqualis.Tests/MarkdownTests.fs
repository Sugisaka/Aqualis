namespace Aqualis.Tests

open System.IO
open Xunit
open Aqualis

module MarkdownTests =
    [<Fact>]
    let ``shared regular expressions preserve markdown conversion`` () =
        use output = new TemporaryDirectory()
        let source = Path.Combine(output.Path, "source.md")
        let destination = Path.Combine(output.Path, "result.html")

        File.WriteAllText(
            source,
            String.concat "\n" [
                "# Heading"
                "## Heading 2"
                "### Heading 3"
                "#### Heading 4"
                "##### Heading 5"
                "**strong** *italic* $x+1$ `code`"
                "`*literal* $literal$`"
                "[label](page.html)"
                "[[page|wiki label]]"
                "[[page#section|section label]]"
                "![](figure.png)"
                "- item"
                "| A | B |"
                "| --- | --- |"
                "| 1 | 2 |"
                ""
                "```fsharp"
                "let x = 1"
                "```"
            ])

        convertHTML source destination
        let html = File.ReadAllText(destination)

        Assert.Contains("<h1>Heading</h1>", html)
        Assert.Contains("<h2>Heading 2</h2>", html)
        Assert.Contains("<h3>Heading 3</h3>", html)
        Assert.Contains("<h4>Heading 4</h4>", html)
        Assert.Contains("<h5>Heading 5</h5>", html)
        Assert.Contains("<strong>strong</strong>", html)
        Assert.Contains("<i>italic</i>", html)
        Assert.Contains(@"\(x+1\)", html)
        Assert.Contains("<code>code</code>", html)
        Assert.Contains("<code>*literal* $literal$</code>", html)
        Assert.Contains("<a href=page.html>label</a>", html)
        Assert.Contains("<a href=page>wiki label</a>", html)
        Assert.Contains("<a href=page#section>section label</a>", html)
        Assert.Contains("<img src=\"img/figure.png\"/>", html)
        Assert.Contains("<li>item</li>", html)
        Assert.Contains("<table>", html)
        Assert.Contains("let x = 1", html)
