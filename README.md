# Aqualis
Algorithm and equation analyzer for lightwave simulation.

This F# library generates C, Fortran, Python, LaTeX, HTML, JavaScript, and PHP source code for numerical simulation and related applications.

## Installation

Aqualis targets .NET 10. Install the .NET 10 SDK to run F# scripts or build an application that uses Aqualis. Add version `188.0.0` from NuGet to an F# project with:

```sh
dotnet add package Aqualis --version 188.0.0
```

For an F# script, reference the package directly instead of copying the DLL:

```fsharp
#r "nuget: Aqualis, 188.0.0"
```

## How to use

Save the following as `hello.fsx`, then run `dotnet fsi hello.fsx`. It creates a `generated` directory next to the script and writes C, Fortran, and Python source files there.

```fsharp
#r "nuget: Aqualis, 188.0.0"

open System.IO
open Aqualis

let outputdir = Path.Combine(__SOURCE_DIRECTORY__, "generated")
Directory.CreateDirectory(outputdir) |> ignore

Compile [C99; Fortran; Python] outputdir "hello" "1.0.0" <| fun ctx ->
    ctx.print.s "Hello World!"
```

The version passed to `Compile` identifies your generated project; it is separate from the Aqualis package version. Compiling or running generated code requires the corresponding language tools.

See also the [Japanese manual](https://github.com/Sugisaka/Aqualis/blob/master/docs/doc-jp.md) or [English manual](https://github.com/Sugisaka/Aqualis/blob/master/docs/doc-en.md) for detailed usage and prerequisites.

For PHP file uploads, configure private upload storage outside the public web root. Generated PHP rejects public or overly permissive storage directories before saving files.

## Optional HTML assets

Generated HTML does not reference external CDNs by default. Applications that need MathJax or a web-font stylesheet must provide and reference those assets explicitly. A relative URL can point to files deployed alongside the generated output; an HTTPS URL is an explicit CDN opt-in.

```fsharp
Compile [HTML] outputdir projectname version <| fun context ->
    context.HtmlAssets.UseMathJax(Url.relative "assets/mathjax/tex-chtml.js")
    context.HtmlAssets.UseFontStylesheet(Url.relative "assets/fonts.css")
    // Generate the document body here.
```

Aqualis does not include or download MathJax, fonts, or other third-party web assets.

## Structured diagnostics

Use `CompileWithDiagnostics` when warnings need to be inspected programmatically without console output.

```fsharp
let result =
    CompileWithDiagnostics [C99] outputdir projectname version <| fun context ->
        // Generate the program here.
        ()

for diagnostic in result.Diagnostics do
    printfn "%s: %s" diagnostic.Code diagnostic.Message
```

`Compile` remains available for compatibility and renders collected diagnostics to standard error. Error diagnostics abort the output transaction. `CompileWithDiagnosticPolicy` can additionally treat warnings as errors or limit the number of collected diagnostics. Context-free operations can be inspected with `Diagnostics.Capture`.

## Generated output ownership

Successful `Compile` runs create a per-project `.aqualis-generated-<project>.json` manifest in the output directory. On later runs for the same project, files listed in the previous manifest but not generated again are removed transactionally. Unrelated files are left alone; if a previously generated file was edited after the previous run, compilation stops rather than deleting or replacing that edit. Existing outputs from before a manifest is first created are not automatically claimed or removed. Do not run multiple generator processes against the same output directory concurrently.

## License
[MIT License](https://github.com/Sugisaka/Aqualis/blob/master/LICENSE.txt)

Copyright (c) 2023 Jun-ichiro Sugisaka
