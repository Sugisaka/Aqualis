namespace Aqualis.Tests

open System
open System.IO
open System.Reflection
open Xunit
open Aqualis

module CompilationGenerationTests =
    [<Fact>]
    let ``all compilation targets preserve output files callback order and public API`` () =
        let targets = [Fortran; C99; LaTeX; HTML; HTMLSequenceDiagram; Python; JavaScript; PHP; Numeric]
        let generated =
            targets
            |> List.map (fun language ->
                use output = new TemporaryDirectory()
                let mutable calls = 0
                let result =
                    CompileWithDiagnostics [language] output.Path "sample-project" "1.0<&" (fun context ->
                        calls <- calls + 1
                        if language <> Numeric then
                            context.comment "generation fixture"
                            let value = context.var.i0 "value"
                            value <== 42
                            context.print.s "hello <&"
                            if List.contains language [Fortran; C99; Python; JavaScript] then
                                context.func "noop" ignore)
                Assert.Equal(1, calls)
                Assert.Empty(result.Diagnostics)
                let returnedFiles =
                    result.OutputFiles
                    |> List.map (fun path -> Path.GetRelativePath(output.Path, path).Replace('\\', '/'))
                    |> List.sort
                let files =
                    Directory.GetFiles(output.Path, "*", SearchOption.AllDirectories)
                    |> Array.sort
                    |> Array.map (fun path ->
                        let relative = Path.GetRelativePath(output.Path, path).Replace('\\', '/')
                        // Ownership hashes are covered by existing transaction tests.
                        if relative.StartsWith(".aqualis-generated-", StringComparison.Ordinal) then ""
                        else
                            relative + "\n" + File.ReadAllText(path).Replace("\r\n", "\n").Replace(Aqualis.Version, "<AqualisVersion>"))
                    |> String.concat "\n"
                string language + "\n" + String.concat "\n" returnedFiles + "\n" + files)
            |> String.concat "\n--- target ---\n"
        let api =
            typeof<Aqualis>.Assembly.GetType("Aqualis.Aqualis_main").GetMethods(BindingFlags.Public ||| BindingFlags.Static ||| BindingFlags.DeclaredOnly)
            |> Array.map string
            |> Array.sort
            |> String.concat "\n"
        let actual = generated + "\n--- API ---\n" + api
        let golden = TestHelpers.goldenPath "compilation" "all-targets.txt"
        let expected = File.ReadAllText(golden).Replace("\r\n", "\n")
        // The original multiline Fortran literal emits an extra leading blank
        // line in CRLF checkouts. Preserve that existing checkout-dependent output.
        let compilerSource = Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "main", "compile.fs")
        let expected =
            if File.ReadAllText(compilerSource).Contains("\r\n") then expected
            else expected.Replace("contains\n\ndouble precision function", "contains\ndouble precision function")
        Assert.Equal(expected, actual)

    [<Fact>]
    let ``multiple targets invoke callbacks in the requested order`` () =
        use output = new TemporaryDirectory()
        let targets = [PHP; C99; Numeric; Python; Fortran; JavaScript; HTML; LaTeX]
        let visited = ResizeArray<Language>()
        let result =
            CompileWithDiagnostics targets output.Path "ordered" "1.0" (fun context ->
                visited.Add context.language)
        Assert.Equal<Language list>(targets, List.ofSeq visited)
        Assert.Empty(result.Diagnostics)
        for path in result.OutputFiles do
            Assert.True(File.Exists path)
