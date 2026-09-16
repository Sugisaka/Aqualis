namespace Aqualis.Tests

open System
open System.IO
open Xunit
open Aqualis

module UnsupportedOperationTests =
    let private assertNotSupported expectedMessage action =
        let error = Assert.Throws<NotSupportedException>(Action action)
        Assert.Equal(expectedMessage, error.Message)

    [<Fact>]
    let ``executable backends reject equation display consistently`` () =
        let cases =
            [ Fortran, "Fortran"
              C99, "C99"
              Python, "Python"
              JavaScript, "JavaScript"
              PHP, "PHP" ]

        for language,targetName in cases do
            use context = new Aqualis(None, None, language)
            assertNotSupported
                $"{targetName} code generation does not support equation display."
                (fun () -> expr.equiv (Int 1) (Int 2) context)
            assertNotSupported
                $"{targetName} code generation does not support aligned equation display."
                (fun () -> expr.equivAlign (Int 1) (Int 2) context)

    [<Fact>]
    let ``raw multidimensional indexing fails instead of emitting NaN`` () =
        let cases = [ C99, "C99"; PHP, "PHP" ]

        for language,targetName in cases do
            use context = new Aqualis(None, None, language)
            assertNotSupported
                $"{targetName} code generation does not support two-dimensional array indexing."
                (fun () -> Idx2(Dt, "matrix", Int 0, Int 1).eval context |> ignore)
            assertNotSupported
                $"{targetName} code generation does not support three-dimensional array indexing."
                (fun () -> Idx3(Dt, "tensor", Int 0, Int 1, Int 2).eval context |> ignore)

    [<Fact>]
    let ``unsupported symbolic differentiation stops immediately`` () =
        use context = Aqualis.BlankWriter Numeric
        let x = Var(Dt, "x", NaN)
        let y = Var(Dt, "y", NaN)

        assertNotSupported
            "Symbolic differentiation does not support atan2."
            (fun () -> expr.diff (Atan2(x, y)) x context |> ignore)
        assertNotSupported
            "Symbolic differentiation does not support the absolute value of a complex expression."
            (fun () -> expr.diff (Abs(Zt, Var(Zt, "z", NaN))) x context |> ignore)

    [<Fact>]
    let ``unsupported numeric execution does not silently skip work`` () =
        assertNotSupported
            "Numeric evaluation does not support a loop whose bounds are not integers ('1.5' to '2')."
            (fun () -> expr.rangeN (Dbl 1.5) (Int 2) ignore)

        assertNotSupported
            "Numeric evaluation does not support the expression 'x'."
            (fun () -> Var(Dt, "x", NaN).eval() |> ignore)

    [<Fact>]
    let ``partial arrays cannot be silently omitted from function arguments`` () =
        use context = new Aqualis(None, None, C99)
        let partial = int1(It 4, Arx1(I 2, fun _ -> Int 1))

        assertNotSupported
            "Function arguments do not support partial one-dimensional arrays."
            (fun () -> partial.farg context ignore)

    [<Fact>]
    let ``unsupported generation preserves existing source files`` () =
        use output = new TemporaryDirectory()
        let cases =
            [ Fortran, "unsupported-fortran", ".f90"
              C99, "unsupported-c99", ".c"
              Python, "unsupported-python", ".py"
              JavaScript, "unsupported-javascript", ".js"
              PHP, "unsupported-php", ".php" ]

        for language,projectName,extension in cases do
            let targetPath = Path.Combine(output.Path, projectName + extension)
            let previousContents = "previous " + projectName
            File.WriteAllText(targetPath, previousContents)

            Assert.Throws<NotSupportedException>(fun () ->
                Compile [language] output.Path projectName "1" <| fun context ->
                    context.writein "partial generated source"
                    expr.equiv (Int 1) (Int 2) context)
            |> ignore

            Assert.Equal(previousContents, File.ReadAllText(targetPath))
            Assert.False(File.Exists(Path.Combine(output.Path, projectName)))
            Assert.Empty(
                Directory.GetFiles(
                    output.Path,
                    "." + projectName + extension + ".aqualis-*.tmp"))

            let freshProjectName = projectName + "-fresh"
            let freshTargetPath = Path.Combine(output.Path, freshProjectName + extension)
            Assert.Throws<NotSupportedException>(fun () ->
                Compile [language] output.Path freshProjectName "1" <| fun context ->
                    context.writein "partial generated source"
                    expr.equiv (Int 1) (Int 2) context)
            |> ignore

            Assert.False(File.Exists(freshTargetPath))
            Assert.False(File.Exists(Path.Combine(output.Path, freshProjectName)))
            Assert.Empty(
                Directory.GetFiles(
                    output.Path,
                    "." + freshProjectName + extension + ".aqualis-*.tmp"))
