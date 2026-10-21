# Aqualis
Algorithm and equation analyzer for lightwave simulation.

This F# library generates C, Fortran, Python, LaTeX, HTML, JavaScript, and PHP source code for numerical simulation and related applications.

## Installation

Aqualis targets .NET 10. Install the .NET 10 SDK to run F# scripts or build an application that uses Aqualis. Add version `188.1.1` from NuGet to an F# project with:

```sh
dotnet add package Aqualis --version 188.1.1
```

For an F# script, reference the package directly instead of copying the DLL:

```fsharp
#r "nuget: Aqualis, 188.1.1"
```

## How to use

Save the following as `hello.fsx`, then run `dotnet fsi hello.fsx`. It creates a `generated` directory next to the script and writes C, Fortran, and Python source files there.

```fsharp
#r "nuget: Aqualis, 188.1.1"

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

## Integer powers

`int0.powr` represents an integer result. Constant integer powers and generated
C and Python integer powers truncate negative-exponent results toward zero, reject zero
raised to a non-positive power, and report results outside the 32-bit range.
Use `asm.pow` for real-valued powers, including fractional results.

## Optimization convergence

Higher-level optimizers stop when the gradient's Euclidean norm is at most
`1e-8` by default. Existing method signatures remain available. Use a
`WithConvergence` variant to set absolute tolerances and inspect the termination
code in the generated program:

```fsharp
let termination = context.var.i0 "termination"
let options =
    { OptimizationConvergence.Default with
        GradientTolerance = 1e-10
        StepTolerance = Some 1e-12
        ObjectiveTolerance = None }
context.optimization.findmin_NewtonWithConvergence
    (100, 40, 64) (options, termination) position (D 1.0)
    objective gradient hessian None
```

The three counts are the maximum optimizer iterations, line-search iterations,
and bracket expansions. Tolerances must be finite and nonnegative; `None`
disables the optional step and objective tests. Step tolerance applies to the
Euclidean norm of the change in position; objective tolerance applies to the
absolute change in objective value. These two tests indicate stagnation and do
not guarantee a small gradient or a local minimum. Gradient tolerance is checked
before each step and again after the final permitted step.

`OptimizationTermination` defines the generated integer codes: `IterationLimit`
(0), `GradientTolerance` (1), `StepTolerance` (2), `ObjectiveTolerance` (3), and
`DirectionBreakdown` (4). Zero optimizer iterations retain the initial point and
report `IterationLimit`. The existing step callback still receives the initial
point and each completed step. Objective/gradient callbacks should compute their
outputs without unrelated side effects, since convergence checks evaluate them.

Newton methods solve `H p = -g` directly instead of forming an inverse matrix.
A singular Hessian fails through the existing solver's diagnostic; an initial
point satisfying the gradient tolerance stops before the Hessian is solved.

## Generated output ownership

Successful `Compile` runs create a per-project `.aqualis-generated-<project>.json` manifest in the output directory. On later runs for the same project, files listed in the previous manifest but not generated again are removed transactionally. Unrelated files are left alone; if a previously generated file was edited after the previous run, compilation stops rather than deleting or replacing that edit. Existing outputs from before a manifest is first created are not automatically claimed or removed. Do not run multiple generator processes against the same output directory concurrently.

## License
[MIT License](https://github.com/Sugisaka/Aqualis/blob/master/LICENSE.txt)

Copyright (c) 2023 Jun-ichiro Sugisaka
