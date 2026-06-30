namespace Aqualis.Tests

open System.IO
open Xunit
open Aqualis

module CompilerScriptTests =
    let private generateScript outputDirectory projectName configure =
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

    [<Fact>]
    let ``C compile scripts separate and quote every argument`` () =
        use output = new TemporaryDirectory()

        let normal =
            generateScript output.Path "normal" ignore
        let openMp =
            generateScript output.Path "openmp" (fun context ->
                context.IsOpenMpUsed <- true)
        let openAcc =
            generateScript output.Path "openacc" (fun context ->
                context.IsOpenAccUsed <- true)

        Assert.Contains(
            "gcc \"extra source.c\" normal.c -lm -o normal.exe",
            normal)
        Assert.Contains(
            "gcc -fopenmp \"extra source.c\" openmp.c -lm -o openmp.exe",
            openMp)
        Assert.Contains(
            "pgcc -acc -Minfo=accel \"extra source.c\" openacc.c -lm -o openacc.exe",
            openAcc)

        Assert.DoesNotContain("gccextra", normal)
        Assert.DoesNotContain("-Minfo=accelextra", openAcc)
