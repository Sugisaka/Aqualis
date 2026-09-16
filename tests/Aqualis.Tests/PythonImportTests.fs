namespace Aqualis.Tests

open System
open System.IO
open System.Text.RegularExpressions
open Xunit
open Aqualis

module PythonImportTests =
    let private generate project code =
        use output = new TemporaryDirectory()
        Compile [Python] output.Path project "1.0" code
        File.ReadAllText(Path.Combine(output.Path, project + ".py"))

    let private occurrences needle source =
        Regex.Matches(source, Regex.Escape needle).Count

    [<Fact>]
    let ``Simple Python programs do not import SciPy`` () =
        let source =
            generate "python-core-only" (fun context ->
                let value = context.var.i0 "value"
                value <== 42
                context.print.t value)

        Assert.DoesNotContain("from scipy.linalg", source)
        Assert.DoesNotContain("from scipy.special", source)

    [<Fact>]
    let ``Explicit Python imports are deduplicated and deterministically ordered`` () =
        let source =
            generate "explicit-python-imports" (fun context ->
                context.Python.ImportModule "custom_module"
                context.Python.ImportFrom("scipy.linalg", "svd")
                context.Python.ImportFrom("scipy.linalg", "solve")
                context.Python.ImportFrom("scipy.linalg", "svd"))

        Assert.Equal(1, occurrences "import custom_module" source)
        Assert.Equal(1, occurrences "from scipy.linalg import solve, svd" source)

    [<Fact>]
    let ``Bessel generation imports only the required SciPy special symbol`` () =
        let source =
            generate "python-bessel" (fun context ->
                let argument = context.var.d0 "argument"
                asm.besselj0 argument ignore)

        Assert.Contains("from scipy.special import jv", source)
        Assert.DoesNotContain("from scipy.special import jv, yn", source)
        Assert.DoesNotContain("from scipy.linalg", source)

    [<Fact>]
    let ``Python function dependencies are inherited by the parent program`` () =
        let source =
            generate "python-function-import" (fun context ->
                context.func "uses_scipy" <| fun childContext ->
                    childContext.Python.ImportFrom("scipy.linalg", "solve"))

        Assert.Equal(1, occurrences "from scipy.linalg import solve" source)

    [<Fact>]
    let ``Python imports reject invalid identifiers`` () =
        use output = new TemporaryDirectory()
        use context = new Aqualis(Some output.Path, Some "invalid.py", Python)

        Assert.Throws<ArgumentException>(fun () ->
            context.Python.ImportModule "scipy.linalg; injected = True")
        |> ignore
        Assert.Throws<ArgumentException>(fun () ->
            context.Python.ImportFrom("scipy.linalg", "solve as injected"))
        |> ignore
        Assert.Throws<ArgumentException>(fun () ->
            context.Python.ImportModule "scipy.class")
        |> ignore

    [<Fact>]
    let ``Python import API is unsupported in other languages`` () =
        use output = new TemporaryDirectory()

        Assert.Throws<NotSupportedException>(fun () ->
            Compile [C99] output.Path "invalid-python-import" "1.0" <| fun context ->
                context.Python.ImportFrom("scipy.linalg", "solve"))
        |> ignore

        Assert.False(File.Exists(Path.Combine(output.Path, "invalid-python-import.c")))
