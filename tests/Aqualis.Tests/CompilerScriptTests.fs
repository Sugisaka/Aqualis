namespace Aqualis.Tests

open System.IO
open Xunit
open Aqualis

module CompilerScriptTests =
    let private generateCScript outputDirectory projectName configure =
        Compile
            [C99]
            outputDirectory
            projectName
            ("test", "1.0")
            (fun () ->
                let context = GenerationContext.TryCurrent.Value
                context.CurrentProgram.slist.add "extra source.c"
                configure context)

        File.ReadAllText(
            Path.Combine(
                outputDirectory,
                "proc_" + projectName + "_C.sh"))

    let private generateFortranScript outputDirectory projectName configure =
        Compile
            [Fortran]
            outputDirectory
            projectName
            ("test", "1.0")
            (fun () ->
                let context = GenerationContext.TryCurrent.Value
                context.CurrentProgram.slist.add "extra source.f90"
                context.CurrentProgram.slist.add "quote'source.f90"
                context.CurrentProgram.slist.add "$generated`source.f90"
                context.CurrentProgram.olist.add "linker option"
                configure context)

        File.ReadAllText(
            Path.Combine(
                outputDirectory,
                "proc_" + projectName + "_F.sh"))

    [<Fact>]
    let ``C compile scripts separate and quote every argument`` () =
        use output = new TemporaryDirectory()

        let normal =
            generateCScript output.Path "normal" ignore
        let openMp =
            generateCScript output.Path "openmp" (fun context ->
                context.IsOpenMpUsed <- true)
        let openAcc =
            generateCScript output.Path "openacc" (fun context ->
                context.IsOpenAccUsed <- true)

        Assert.Contains(
            "gcc 'extra source.c' normal.c -lm -o normal.exe",
            normal)
        Assert.Contains(
            "gcc -fopenmp 'extra source.c' openmp.c -lm -o openmp.exe",
            openMp)
        Assert.Contains(
            "pgcc -acc -Minfo=accel 'extra source.c' openacc.c -lm -o openacc.exe",
            openAcc)

        Assert.DoesNotContain("gccextra", normal)
        Assert.DoesNotContain("-Minfo=accelextra", openAcc)

    [<Fact>]
    let ``Fortran compile scripts quote each argument and use direct compiler paths`` () =
        use output = new TemporaryDirectory()

        let normal =
            generateFortranScript output.Path "normal" ignore
        let openMp =
            generateFortranScript output.Path "openmp" (fun context ->
                context.IsOpenMpUsed <- true)
        let openAcc =
            generateFortranScript output.Path "openacc" (fun context ->
                context.IsOpenAccUsed <- true)

        let quotedSources =
            "'extra source.f90' 'quote'\"'\"'source.f90' " +
            "'$generated`source.f90'"

        Assert.Contains(
            "/usr/bin/gfortran -ffree-line-length-none " +
            quotedSources +
            " normal.f90 'linker option' -o normal.exe",
            normal)
        Assert.Contains(
            "/usr/bin/gfortran -fopenmp " +
            quotedSources +
            " openmp.f90 'linker option' -o openmp.exe",
            openMp)
        Assert.Contains(
            "/usr/bin/pgfortran -acc -Minfo=accel " +
            quotedSources +
            " openacc.f90 'linker option' -o openacc.exe",
            openAcc)

        for script in [normal; openMp; openAcc] do
            Assert.StartsWith("#!/bin/bash", script)
            Assert.DoesNotContain("$FC", script)

        Assert.Contains("./normal.exe", normal)
        Assert.Contains("./openmp.exe", openMp)
        Assert.Contains("./openacc.exe", openAcc)
