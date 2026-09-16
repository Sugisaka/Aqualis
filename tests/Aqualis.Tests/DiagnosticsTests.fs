namespace Aqualis.Tests

open System
open System.IO
open Xunit
open Aqualis

module DiagnosticsTests =
    let private diagnostic code severity = {
        Code = code
        Severity = severity
        Message = code
        Location = None
        Properties = Map.empty
    }

    [<Fact>]
    let ``context-free expression diagnostics can be captured`` () =
        let value,diagnostics =
            Diagnostics.Capture(fun () -> expr.simpDiv(Int 1, Int 0))

        match value with
        | NaN -> ()
        | other -> failwith $"Expected NaN, but received {other}."
        let item = Assert.Single diagnostics
        Assert.Equal("AQL4001", item.Code)
        Assert.Equal(Error, item.Severity)
        Assert.Equal(Some(Expression "division"), item.Location)

    [<Fact>]
    let ``diagnostic bag is bounded and reports suppression`` () =
        let bag = DiagnosticBag(maxDiagnostics = 2)

        bag.Report(diagnostic "TEST0001" Warning)
        bag.Report(diagnostic "TEST0002" Warning)
        bag.Report(diagnostic "TEST0003" Warning)
        bag.Report(diagnostic "TEST0004" Warning)

        let diagnostics = bag.Snapshot()
        Assert.Equal(3, diagnostics.Length)
        Assert.Equal("AQL0000", diagnostics[2].Code)

    [<Fact>]
    let ``diagnostic bag accepts concurrent reports`` () =
        let bag = DiagnosticBag(maxDiagnostics = 600)

        System.Threading.Tasks.Parallel.For(
            0,
            500,
            fun (index:int) -> bag.Report(diagnostic ("TEST" + index.ToString("0000")) Warning))
        |> ignore

        Assert.Equal(500, bag.Count)
        Assert.False(bag.HasErrors)

    [<Fact>]
    let ``CompileWithDiagnostics returns warnings and commits output`` () =
        use output = new TemporaryDirectory()
        let projectName = "diagnostic-warning"

        let result =
            CompileWithDiagnostics [C99] output.Path projectName "1" <| fun context ->
                context.cvar.setUniqVarWarning(It 4, A0, "duplicate", "0")
                context.cvar.setUniqVarWarning(It 4, A0, "duplicate", "0")

        let item = Assert.Single result.Diagnostics
        Assert.Equal("AQL1002", item.Code)
        Assert.Equal(Warning, item.Severity)
        Assert.True(File.Exists(Path.Combine(output.Path, projectName + ".c")))
        Assert.Contains(Path.Combine(output.Path, projectName + ".c"), result.OutputFiles)

    [<Fact>]
    let ``variable declarations reject conflicting types shapes and initial values`` () =
        use output = new TemporaryDirectory()
        use context = new Aqualis(Some output.Path, Some "conflicts.c", C99)

        context.var.i0 "sameType" |> ignore
        Assert.Throws<ArgumentException>(fun () -> context.var.d0 "sameType" |> ignore) |> ignore

        context.var.i1("sameShape", 2) |> ignore
        Assert.Throws<ArgumentException>(fun () -> context.var.i2("sameShape", 2, 2) |> ignore) |> ignore

        context.var.ip1("sameInitial", [1]) |> ignore
        Assert.Throws<ArgumentException>(fun () -> context.var.ip1("sameInitial", [2]) |> ignore) |> ignore

    [<Fact>]
    let ``initialized arrays reject empty values before declaration`` () =
        use output = new TemporaryDirectory()
        use context = new Aqualis(Some output.Path, Some "empty.c", C99)

        Assert.Throws<ArgumentException>(fun () -> context.var.ip1("integers", []) |> ignore) |> ignore
        Assert.Throws<ArgumentException>(fun () -> context.var.dp1("reals", []) |> ignore) |> ignore
        Assert.Throws<ArgumentException>(fun () -> context.var.zp1("complexes", []) |> ignore) |> ignore
        Assert.Empty(context.cvar.list)

    [<Fact>]
    let ``diagnostic policy can treat warnings as transaction failures`` () =
        use output = new TemporaryDirectory()
        let projectName = "warning-as-error"
        let targetPath = Path.Combine(output.Path, projectName + ".c")
        File.WriteAllText(targetPath, "previous output")
        let policy = {
            DiagnosticPolicy.defaults with
                TreatWarningsAsErrors = true
        }

        let error =
            Assert.Throws<AqualisCompilationException>(fun () ->
                CompileWithDiagnosticPolicy policy [C99] output.Path projectName "1" <| fun context ->
                    context.cvar.setUniqVarWarning(It 4, A0, "duplicate", "0")
                    context.cvar.setUniqVarWarning(It 4, A0, "duplicate", "0")
                |> ignore)

        Assert.Contains(error.Diagnostics, fun item -> item.Code = "AQL1002")
        Assert.Equal("previous output", File.ReadAllText targetPath)

    [<Fact>]
    let ``error diagnostics abort Compile transaction and preserve previous output`` () =
        use output = new TemporaryDirectory()
        let projectName = "diagnostic-error"
        let targetPath = Path.Combine(output.Path, projectName + ".php")
        File.WriteAllText(targetPath, "previous output")

        let error =
            Assert.Throws<AqualisCompilationException>(fun () ->
                CompileWithDiagnostics [PHP] output.Path projectName "1" <| fun _ ->
                    PHPdata("not a scalar").int0 |> ignore
                |> ignore)

        Assert.Contains(error.Diagnostics, fun item -> item.Code = "AQL2001")
        Assert.Equal("previous output", File.ReadAllText targetPath)

    [<Fact>]
    let ``graph data diagnostics include input location`` () =
        use output = new TemporaryDirectory()
        let inputPath = Path.Combine(output.Path, "invalid-data.txt")
        File.WriteAllText(inputPath, "1 invalid")

        let result =
            graph1d.readdataWithDiagnostics
                inputPath
                ((fun get -> get 1), (fun get -> get 2))

        let item = Assert.Single result.Diagnostics
        Assert.Equal("AQL3002", item.Code)
        Assert.Equal(Warning, item.Severity)
        Assert.Equal(Some(InputFile(inputPath, Some 1, Some 2)), item.Location)
