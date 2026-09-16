# Aqualis
Algorithm and equation analyzer for lightwave simulation

This library generates C/FORTRAN/Python/LaTeX/HTML source codes for numerical simulation. You can create simple readable and high-performance programs under multiparadigm programming (F#).

## Installation
1. Install Visual Studio 2026 or the Build Tools for Visual Studio 2026. During installation, be sure to select "F# desktop language support."
2. Copy `Aqualis.dll` to `C:\Aqualis\lib\(version number)` (`(version number)` refers to the Aqualis version number such as `188_0_0_0`, for example).

## How to use

Run the script file `sample1.fsx` to generate C, Fortran, and Python source files in `C:\home\work`.

See also the [Japanese manual](docs/doc-jp.md) or [English manual](docs/doc-en.md).

For PHP file uploads, follow the [private upload storage deployment requirements](docs/php-upload-storage-jp.md). Generated PHP rejects public or overly permissive storage directories before saving files.

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

Successful `Compile` runs create a per-project `.aqualis-generated-<project>.json` manifest in the output directory. On later runs for the same project, files listed in the previous manifest but not generated again are removed transactionally. Unrelated files are left alone; if an obsolete generated file was edited after the previous run, compilation stops rather than deleting that edit. Existing outputs from before a manifest is first created are not automatically claimed or removed. Do not run multiple generator processes against the same output directory concurrently.

## License
[MIT License](LICENSE.txt)

Copyright (c) 2023 Jun-ichiro Sugisaka
