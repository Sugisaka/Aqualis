namespace Aqualis.Tests

open System.IO
open Xunit
open Aqualis

module LapackRankGenerationTests =
    let private generate language extension project code =
        use output = new TemporaryDirectory()
        Compile [language] output.Path project "1.0" code
        File.ReadAllText(Path.Combine(output.Path, project + extension))

    let private callsTo routine (source:string) =
        source.Split('\n')
        |> Array.map _.Trim()
        |> Array.filter _.StartsWith(routine + "(")

    [<Fact>]
    let ``C real rank generation uses the dgesdd pointer ABI`` () =
        let source =
            generate C99 ".c" "real_rank" (fun context ->
                let rank = context.var.i0 "rank"
                let matrix = context.var.d2("matrix", 2, 3)
                context.la.rank(rank, matrix, double0(Dbl 1.0e-10)))

        Assert.Contains(
            "extern void dgesdd_(char *, int *, int *, double *, int *, double *, double *, int *, double *, int *, double *, int *, int *, int *);",
            source)
        Assert.Contains("matrix_size[0]", source)
        Assert.Contains("matrix_size[1]", source)
        Assert.DoesNotContain("rwork", source)
        let calls = callsTo "dgesdd_" source
        Assert.Equal(2, calls.Length)
        calls |> Array.iter (fun call ->
            Assert.DoesNotContain("*", call)
            Assert.Equal(13, call |> Seq.filter ((=) ',') |> Seq.length))

    [<Fact>]
    let ``C complex rank generation uses the zgesdd pointer ABI`` () =
        let source =
            generate C99 ".c" "complex_rank" (fun context ->
                let rank = context.var.d0 "rank"
                let matrix = context.var.z2("matrix", 3, 2)
                context.la.rank(rank, matrix, double0(Dbl 1.0e-10)))

        Assert.Contains(
            "extern void zgesdd_(char *, int *, int *, double complex *, int *, double *, double complex *, int *, double complex *, int *, double complex *, int *, double *, int *, int *);",
            source)
        Assert.Contains("matrix_size[0]", source)
        Assert.Contains("matrix_size[1]", source)
        let calls = callsTo "zgesdd_" source
        Assert.Equal(2, calls.Length)
        calls |> Array.iter (fun call ->
            Assert.DoesNotContain("*", call)
            Assert.Equal(14, call |> Seq.filter ((=) ',') |> Seq.length))

    [<Fact>]
    let ``Python rank generation uses the supplied condition`` () =
        let source =
            generate Python ".py" "python_rank" (fun context ->
                let rank = context.var.i0 "rank"
                let condition = context.var.d0 "condition"
                let matrix = context.var.d2("matrix", 2, 3)
                context.la.rank(rank, matrix, condition))

        Assert.Contains("numpy.sum(", source)
        Assert.Contains(" > condition)", source)
        Assert.DoesNotContain("threshold = 1e-10", source)
