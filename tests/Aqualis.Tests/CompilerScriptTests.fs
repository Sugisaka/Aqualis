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
        ?context=environment.GenerationContext)

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
                let context = environment.GenerationContext.Value
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
            "1.0"
            (fun environment ->
                let context = environment.GenerationContext.Value
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

        Compile
            [Fortran; C99; Python]
            output.Path
            "function-context"
            "1.0"
            (fun environment ->
                environment.ch.dd <| fun (result,value) ->
                environment.ch.i1 2 <| fun values ->
                    let structureValue =
                        FunctionArgumentStructure(
                            "structureValue",
                            environment)
                    structureValue.Value <== 3.0

                    environment.func "context_function" <| fun functionEnvironment ->
                        result.farg functionEnvironment <| fun result ->
                        value.farg functionEnvironment <| fun value ->
                        values.farg functionEnvironment <| fun values ->
                        structureValue.farg functionEnvironment <| fun structureValue ->
                            result <==
                                value +
                                values[0] +
                                structureValue.Value
                            functionEnvironment.print.t result)

        let fortran =
            File.ReadAllText(
                Path.Combine(output.Path, "function-context.f90"))
        let c =
            File.ReadAllText(
                Path.Combine(output.Path, "function-context.c"))
        let python =
            File.ReadAllText(
                Path.Combine(output.Path, "function-context.py"))

        for generated in [fortran; c; python] do
            Assert.Contains("context_function", generated)
            Assert.Contains("arg01", generated)
