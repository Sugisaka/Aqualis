namespace Aqualis.Tests

open System.IO
open System.Text.RegularExpressions
open Xunit
open Aqualis

module LapackDeterminantGenerationTests =
    let private generatePython project code =
        use output = new TemporaryDirectory()
        Compile [Python] output.Path project "1.0" code
        File.ReadAllText(Path.Combine(output.Path, project + ".py"))

    let private assertStableLogDeterminant (source:string) =
        let assignment =
            Regex.Match(
                source,
                @"(?m)^(?<result>d\d+) = numpy\.linalg\.slogdet\(matrix\)\[1\] / numpy\.log\(10\.0\)$")

        Assert.True(assignment.Success, "The generated Python must use numpy.linalg.slogdet.")
        Assert.DoesNotContain("np.", source)
        Assert.DoesNotContain("P ,L ,U = lu(", source)
        Assert.DoesNotContain("det_U", source)
        Assert.DoesNotContain("sign =", source)

        let generatedResult = assignment.Groups["result"].Value
        let remainingSource = source.Substring(assignment.Index + assignment.Length)
        Assert.DoesNotContain(generatedResult + " = 0", remainingSource)
        Assert.Contains("result = " + generatedResult, remainingSource)

    [<Fact>]
    let ``Python real determinant keeps the slogdet result`` () =
        let source =
            generatePython "real_determinant" (fun context ->
                let matrix = context.var.d2("matrix", 2, 2)
                let result = context.var.d0 "result"
                context.la.determinant matrix <| fun value -> result <== value)

        assertStableLogDeterminant source

    [<Fact>]
    let ``Python complex determinant keeps the slogdet result`` () =
        let source =
            generatePython "complex_determinant" (fun context ->
                let matrix = context.var.z2("matrix", 2, 2)
                let result = context.var.d0 "result"
                context.la.determinant matrix <| fun value -> result <== value)

        assertStableLogDeterminant source

    [<Fact>]
    let ``C real determinant declares dgetrf with a real matrix pointer`` () =
        use output = new TemporaryDirectory()
        Compile [C99] output.Path "real_determinant" "1.0" (fun context ->
            let matrix = context.var.d2("matrix", 2, 2)
            context.la.determinant matrix ignore)
        let source =
            File.ReadAllText(Path.Combine(output.Path, "real_determinant.c"))

        Assert.Contains(
            "extern void dgetrf_(int *, int *, double *, int *, int *, int *);",
            source)
        Assert.DoesNotContain(
            "dgetrf_(int *, int *, double complex *,",
            source)
