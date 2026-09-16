namespace Aqualis.Tests

open System
open System.IO
open Xunit
open Aqualis

module LapackSolveGenerationTests =
    [<Fact>]
    let ``unsupported languages reject every linear solve overload`` () =
        for language in [JavaScript; PHP; HTMLSequenceDiagram; Numeric] do
            use context = new Aqualis(None, None, language)
            let realMatrix = context.var.d2("realMatrix", 2, 2)
            let realVector = context.var.d1("realVector", 2)
            let realRightHandSides = context.var.d2("realRightHandSides", 2, 1)
            let complexMatrix = context.var.z2("complexMatrix", 2, 2)
            let complexVector = context.var.z1("complexVector", 2)
            let complexRightHandSides = context.var.z2("complexRightHandSides", 2, 1)

            for operation in
                [ fun () -> context.la.solve_simuleq(realMatrix, realVector)
                  fun () -> context.la.solve_simuleq(complexMatrix, complexVector)
                  fun () -> context.la.solve_simuleqs(realMatrix, realRightHandSides)
                  fun () -> context.la.solve_simuleqs(complexMatrix, complexRightHandSides) ] do
                let error = Assert.Throws<NotSupportedException>(Action operation)
                Assert.Equal(
                    $"{language} code generation does not support LAPACK linear solve.",
                    error.Message)

    [<Fact>]
    let ``complex linear solve emits balanced LaTeX math delimiters`` () =
        use output = new TemporaryDirectory()
        Compile [LaTeX] output.Path "complex_solve_latex" "1.0" <| fun context ->
            let matrix = context.var.z2("matrix", 2, 2)
            let rhs = context.var.z1("rhs", 2)
            context.la.solve_simuleq(matrix, rhs)

        let source = File.ReadAllText(Path.Combine(output.Path, "complex_solve_latex.tex"))
        let equation = source.Split('\n') |> Array.find (fun line -> line.Contains("\\leftarrow"))
        Assert.Equal(2, equation |> Seq.filter ((=) '$') |> Seq.length)
