namespace Aqualis.Tests

open System.IO
open Xunit
open Aqualis

module GoldenFileTests =
    let private generateAssignment language extension =
        use output = new TemporaryDirectory()
        let filename = "assignment." + extension

        makeProgramWithContext
            [output.Path, filename, language]
            (fun context ->
                let value = var.i0 "value"
                value <== 42
                context.CurrentProgram.close())

        File.ReadAllText(Path.Combine(output.Path, filename))
        |> TestHelpers.normalizeGeneratedCode

    let private assertGolden language filename actual =
        let expected =
            TestHelpers.goldenPath language filename
            |> File.ReadAllText
            |> TestHelpers.normalizeGeneratedCode

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``C99 scalar assignment matches the golden file`` () =
        generateAssignment C99 "c"
        |> assertGolden "c" "scalar-assignment.txt"

    [<Fact>]
    let ``Fortran scalar assignment matches the golden file`` () =
        generateAssignment Fortran "f90"
        |> assertGolden "fortran" "scalar-assignment.txt"

    [<Fact>]
    let ``Python scalar assignment matches the golden file`` () =
        generateAssignment Python "py"
        |> assertGolden "python" "scalar-assignment.txt"
