namespace Aqualis.Tests

open System
open System.IO
open Xunit
open Aqualis

module OpenMpGenerationTests =
    let private generate language extension project code =
        use output = new TemporaryDirectory()
        Compile [language] output.Path project "1.0" code
        File.ReadAllText(Path.Combine(output.Path, project + extension))

    [<Fact>]
    let ``C parallel loop is emitted after its OpenMP directive`` () =
        let source =
            generate C99 ".c" "openmp-loop-order" (fun context ->
                context.omp.parallelize <| fun parallelContext ->
                    parallelContext.iter.num 2 <| fun _ ->
                        parallelContext.writein "puts(\"parallel\");")

        let directiveIndex = source.IndexOf("#pragma omp parallel for", StringComparison.Ordinal)
        let loopIndex = source.IndexOf("for(", StringComparison.Ordinal)

        Assert.True(directiveIndex >= 0)
        Assert.True(loopIndex > directiveIndex)
        Assert.Contains("private(", source)
        Assert.Contains("puts(\"parallel\");", source)

    [<Fact>]
    let ``Fortran private clause uses OpenMP continuation lines`` () =
        let source =
            generate Fortran ".f90" "openmp-private-wrap" (fun context ->
                context.omp.parallelize <| fun parallelContext ->
                    for index in 1..12 do
                        parallelContext.varPrivate.setVar(
                            It 4,
                            A0,
                            sprintf "long_private_variable_%02d" index,
                            "")
                    parallelContext.writein "do i = 1, 2"
                    parallelContext.writein "end do")

        let directiveIndex = source.IndexOf("!$omp parallel do private(", StringComparison.Ordinal)
        let continuationIndex = source.IndexOf("!$omp& ", StringComparison.Ordinal)
        let loopIndex = source.IndexOf("do i = 1, 2", StringComparison.Ordinal)
        let endIndex = source.IndexOf("!$omp end parallel do", StringComparison.Ordinal)

        Assert.True(directiveIndex >= 0)
        Assert.True(continuationIndex > directiveIndex)
        Assert.Contains(", &", source)
        Assert.True(loopIndex > continuationIndex)
        Assert.True(endIndex > loopIndex)

    [<Fact>]
    let ``OpenMP callback failure leaves the parent writer usable`` () =
        use output = new TemporaryDirectory()

        Aqualis.makeProgramWithContext
            (output.Path, "openmp-exception.c", C99)
            (fun context ->
                Assert.Throws<InvalidOperationException>(fun () ->
                    context.omp.parallelize (fun _ -> invalidOp "expected"))
                |> ignore

                Assert.False(context.ParallelMode)
                Assert.Empty(context.varPrivate.list)
                Assert.False(context.IsOpenMpUsed)
                Assert.Empty(context.hlist.list)
                context.writein "/* still open */"
                context.close())

        let source = File.ReadAllText(Path.Combine(output.Path, "openmp-exception.c"))
        Assert.Contains("/* still open */", source)
        Assert.DoesNotContain("#pragma omp parallel for", source)

    [<Fact>]
    let ``C sections keep every section inside the OpenMP region`` () =
        let source =
            generate C99 ".c" "openmp-sections" (fun context ->
                context.omp.sections 2 <| fun parallelContext ->
                    parallelContext.omp.section (fun sectionContext ->
                        sectionContext.writein "puts(\"first\");")
                    parallelContext.omp.section (fun sectionContext ->
                        sectionContext.writein "puts(\"second\");"))

        let parallelIndex = source.IndexOf("#pragma omp parallel", StringComparison.Ordinal)
        let sectionsIndex = source.IndexOf("#pragma omp sections", StringComparison.Ordinal)
        let firstIndex = source.IndexOf("puts(\"first\");", StringComparison.Ordinal)
        let secondIndex = source.IndexOf("puts(\"second\");", StringComparison.Ordinal)

        Assert.True(parallelIndex >= 0)
        Assert.True(sectionsIndex > parallelIndex)
        Assert.True(firstIndex > sectionsIndex)
        Assert.True(secondIndex > firstIndex)

    [<Fact>]
    let ``C reduction directive is emitted before its loop body`` () =
        let source =
            generate C99 ".c" "openmp-reduction" (fun context ->
                let total = context.var.d0 "total"
                context.omp.reduction(total, "+") <| fun parallelContext ->
                    parallelContext.writein "for (int i = 0; i < 2; ++i) { total += i; }")

        let directiveIndex = source.IndexOf("#pragma omp parallel for", StringComparison.Ordinal)
        let reductionIndex = source.IndexOf("reduction(+:total)", StringComparison.Ordinal)
        let loopIndex = source.IndexOf("for (int i = 0;", StringComparison.Ordinal)

        Assert.True(directiveIndex >= 0)
        Assert.True(reductionIndex > directiveIndex)
        Assert.True(loopIndex > reductionIndex)

    [<Fact>]
    let ``OpenMP rejects invalid thread counts before running the callback`` () =
        use context = new Aqualis(None, None, C99)
        let mutable callbackRan = false

        Assert.Throws<ArgumentException>(fun () ->
            context.omp.parallelize_th 0 (fun _ -> callbackRan <- true))
        |> ignore

        Assert.False(callbackRan)
        Assert.False(context.IsOpenMpUsed)
