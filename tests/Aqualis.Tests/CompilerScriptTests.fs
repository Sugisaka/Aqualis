namespace Aqualis.Tests

open System.IO
open Xunit
open Aqualis

type private FunctionArgumentStructure(
    structureName,
    name,
    environment:Aqualis) =
    inherit structureValue<FunctionArgumentStructure>(
        structureName,
        name,
        environment)

    static member StructureName = "FunctionArgumentStructure"

    new(name,environment:Aqualis) =
        environment.str.reg(
            FunctionArgumentStructure.StructureName,
            name)
        FunctionArgumentStructure(
            FunctionArgumentStructure.StructureName,
            name,
            environment)

    override _.Rewrap(name,targetEnvironment) =
        FunctionArgumentStructure(
            structureName,
            name,
            targetEnvironment)

    member _.Value =
        environment.str.d0(
            structureName,
            name,
            "value")

module CompilerScriptTests =
    let private assertLfOnlyWithoutBom path =
        let bytes = File.ReadAllBytes path
        let hasUtf8Bom =
            bytes.Length >= 3 &&
            bytes.[0] = 0xEFuy &&
            bytes.[1] = 0xBBuy &&
            bytes.[2] = 0xBFuy

        Assert.True(bytes |> Array.contains 0x0Auy, $"LF was not found in {path}.")
        Assert.False(bytes |> Array.contains 0x0Duy, $"CR was found in {path}.")
        Assert.False(hasUtf8Bom, $"UTF-8 BOM was found in {path}.")

    let private generateCScript outputDirectory projectName configure =
        Compile
            [C99]
            outputDirectory
            projectName
            "1.0"
            (fun environment ->
                environment.slist.add "extra source.c"
                configure environment)

        File.ReadAllText(
            Path.Combine(
                outputDirectory,
                "proc_" + projectName + "_C.sh"))

    let private generateFortranScript outputDirectory projectName configure =
        Compile
            [Fortran]
            outputDirectory
            projectName
            "1.0"
            (fun environment ->
                environment.slist.add "extra source.f90"
                environment.slist.add "quote'source.f90"
                environment.slist.add "$generated`source.f90"
                environment.olist.add "linker option"
                configure environment)

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

    [<Fact>]
    let ``generated shell scripts use LF and UTF-8 without BOM`` () =
        use output = new TemporaryDirectory()

        generateCScript output.Path "c-script" ignore |> ignore
        generateFortranScript output.Path "fortran-script" ignore |> ignore

        Compile
            [Python]
            output.Path
            "python-script"
            "1.0"
            ignore

        Compile
            [C99]
            output.Path
            "distributed"
            "1.0"
            (fun environment ->
                use scripts = new shellscript.Shell(environment, output.Path, "distributed", 2)
                scripts.AddProcess()
                scripts.AddProcess())

        [ "proc_c-script_C.sh"
          "proc_fortran-script_F.sh"
          "proc_python-script_P.sh"
          "proc_distributed_C.sh"
          "shell_distributed_01.sh"
          "shell_distributed_02.sh" ]
        |> List.iter (fun fileName ->
            Path.Combine(output.Path, fileName)
            |> assertLfOnlyWithoutBom)

    [<Fact>]
    let ``function arguments are rebound to the function context`` () =
        use output = new TemporaryDirectory()
        use source =
            new Aqualis(Some output.Path, Some "source.c", C99)
        use target =
            new Aqualis(Some output.Path, Some "target.c", C99)
        let result = double0(Var(Dt, "result", NaN), source)
        let value = double0(Var(Dt, "value", NaN), source)
        let values = int1(It 4, Var1(A1 2, "values"), source)
        let structureValue =
            FunctionArgumentStructure("structureValue", source)

        result.farg target <| fun reboundResult ->
        value.farg target <| fun reboundValue ->
        values.farg target <| fun reboundValues ->
        structureValue.farg target <| fun reboundStructure ->
            Assert.Same(target, reboundResult.Context)
            Assert.Same(target, reboundValue.Context)
            Assert.Same(target, reboundValues.Context)
            Assert.Same(target, reboundStructure.Context)
            Assert.Same(target, reboundStructure.Value.Context)
            Assert.Contains("arg", reboundResult.code)
