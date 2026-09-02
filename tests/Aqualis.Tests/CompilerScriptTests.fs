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
    let private assertCompileFailureStopsExecution executable (script:string) =
        let lines =
            script.Split('\n', System.StringSplitOptions.RemoveEmptyEntries)
            |> Array.map _.TrimEnd()
        let statusIndex =
            lines |> Array.findIndex ((=) "aqualis_compile_status=$?")
        let guardIndex =
            lines
            |> Array.findIndex ((=) "if [ \"$aqualis_compile_status\" -ne 0 ]; then")
        let exitIndex =
            lines
            |> Array.findIndex ((=) "  exit \"$aqualis_compile_status\"")
        let endGuardIndex =
            lines |> Array.findIndex ((=) "fi")
        let executionLine = "exec " + executable
        let executionIndexes =
            lines
            |> Array.indexed
            |> Array.choose (fun (index,line) ->
                if line = executionLine then Some index else None)

        Assert.True(statusIndex > 0)
        Assert.Contains(" -o ", lines[statusIndex - 1])
        Assert.Equal(statusIndex + 1, guardIndex)
        Assert.True(exitIndex > guardIndex)
        Assert.True(endGuardIndex > exitIndex)
        Assert.Single(executionIndexes) |> ignore
        Assert.True(executionIndexes[0] > endGuardIndex)

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

        for project,script in ["normal",normal; "openmp",openMp; "openacc",openAcc] do
            assertCompileFailureStopsExecution ("./" + project + ".exe") script

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

        for project,script in ["normal",normal; "openmp",openMp; "openacc",openAcc] do
            assertCompileFailureStopsExecution ("./" + project + ".exe") script

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
    let ``mail notified processes are distributed round robin`` () =
        use output = new TemporaryDirectory()

        Compile
            [C99]
            output.Path
            "mail-distributed"
            "1.0"
            (fun environment ->
                use scripts =
                    new shellscript.Shell(
                        environment,
                        output.Path,
                        "mail-distributed",
                        2)
                for _ in 1..5 do
                    scripts.AddProcess("notify@example.com"))

        let first =
            File.ReadAllText(
                Path.Combine(output.Path, "shell_mail-distributed_01.sh"))
        let second =
            File.ReadAllText(
                Path.Combine(output.Path, "shell_mail-distributed_02.sh"))
        let countRuns (script:string) =
            script.Split("{ time sh proc_mail-distributed_C.sh; }").Length - 1

        Assert.Equal(3, countRuns first)
        Assert.Equal(2, countRuns second)
        for script in [first; second] do
            Assert.StartsWith("#!/bin/bash", script)
            Assert.Contains("aqualis_exit_status=0", script)
            Assert.Contains("aqualis_process_status=$?", script)
            Assert.Contains("aqualis_exit_status=$aqualis_process_status", script)
            Assert.EndsWith("exit \"$aqualis_exit_status\"\n", script)
            Assert.Contains("project mail-distributed finished", script)
            Assert.Contains("project mail-distributed failed", script)
            Assert.Contains("> mail-distributed.log 2> mail-distributed_time.log", script)
            Assert.DoesNotContain("&>", script)

    [<Fact>]
    let ``mail notified processes use the language specific script`` () =
        use output = new TemporaryDirectory()

        for language, project, suffix in
            [C99, "mail-c", "_C.sh"
             Fortran, "mail-fortran", "_F.sh"
             Python, "mail-python", "_P.sh"] do
            Compile
                [language]
                output.Path
                project
                "1.0"
                (fun environment ->
                    use scripts =
                        new shellscript.Shell(environment, output.Path, project, 1)
                    scripts.AddProcess("notify@example.com"))

            let script =
                File.ReadAllText(
                    Path.Combine(output.Path, "shell_" + project + "_01.sh"))
            Assert.Contains("sh proc_" + project + suffix, script)

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

    [<Fact>]
    let ``function generation keeps the parent context open and inherits dependencies`` () =
        use output = new TemporaryDirectory()

        Compile
            [C99]
            output.Path
            "function-context"
            "1.0"
            (fun context ->
                context.ch.D "result" <| fun result ->
                context.ch.D "value" <| fun value ->
                    context.func "copy_value" <| fun childContext ->
                        childContext.hlist.add "<time.h>"
                        childContext.olist.add "-lchild"
                        childContext.IsOpenMpUsed <- true
                        result.farg childContext <| fun target ->
                        value.farg childContext <| fun source ->
                            target <== source

                    Assert.Equal(1, context.Active)
                    context.writein "/* parent remains open */")

        let generated =
            File.ReadAllText(
                Path.Combine(output.Path, "function-context.c"))
        let compileScript =
            File.ReadAllText(
                Path.Combine(output.Path, "proc_function-context_C.sh"))

        Assert.Contains("#include <time.h>", generated)
        Assert.Contains("void copy_value(", generated)
        Assert.Contains("(*arg01) = (*arg02);", generated)
        Assert.Contains("copy_value(&result, &value);", generated)
        Assert.Contains("/* parent remains open */", generated)
        Assert.Contains("gcc -fopenmp", compileScript)
        Assert.Contains("-lchild", compileScript)
