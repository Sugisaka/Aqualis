namespace Aqualis.Tests

open System
open System.Globalization
open System.IO
open Xunit
open Aqualis

module NumericFormattingTests =
    let private withCulture (cultureName:string) code =
        let previousCulture = CultureInfo.CurrentCulture
        let previousUiCulture = CultureInfo.CurrentUICulture
        try
            CultureInfo.CurrentCulture <- CultureInfo cultureName
            CultureInfo.CurrentUICulture <- CultureInfo cultureName
            code()
        finally
            CultureInfo.CurrentCulture <- previousCulture
            CultureInfo.CurrentUICulture <- previousUiCulture

    [<Theory>]
    [<InlineData("ja-JP")>]
    [<InlineData("fr-FR")>]
    [<InlineData("de-DE")>]
    [<InlineData("ar-EG")>]
    let ``finite numeric literals are culture independent`` cultureName =
        withCulture cultureName <| fun () ->
            Assert.Equal("1.5E0", numericFormatController(C99).DtoS 1.5)
            Assert.Equal("1.5d0", numericFormatController(Fortran).DtoS 1.5)
            Assert.Equal("1.5E0", numericFormatController(Python).DtoS 1.5)
            Assert.Equal("1.5", InvariantFormat.number 1.5)

    [<Fact>]
    let ``complex expression strings preserve real and imaginary parts`` () =
        let value = Cpx(1.25, -2.5)
        let expression = Add(Zt, Cpx(1.0, 2.0), Cpx(3.0, 4.0))

        Assert.Equal("Cpx(1.25, -2.5) ", value.ToString())
        Assert.Contains("Cpx(1, 2)", expression.ToString())
        Assert.Contains("Cpx(3, 4)", expression.ToString())
        Assert.DoesNotContain("Cpx(1.25, 1.25)", value.ToString())

    [<Fact>]
    let ``integer divided by a complex literal simplifies to the quotient`` () =
        let simplified = (Int 2 / Cpx(1.0, 1.0)).simp

        match simplified with
        |Cpx(real, imaginary) ->
            Assert.Equal(1.0, real, 12)
            Assert.Equal(-1.0, imaginary, 12)
        |other ->
            Assert.Fail($"Expected a complex literal, but got {other}.")

    [<Fact>]
    let ``Python modulo renders a scalar remainder with grouped operands`` () =
        use target = Aqualis.BlankWriter Python
        let left = Add(It 4, Var(It 4, "left", NaN), Var(It 4, "right", NaN))
        let right = Sub(It 4, Var(It 4, "modulus", NaN), Var(It 4, "offset", NaN))

        let simple = Mod(It 4, Var(It 4, "value", NaN), Int 2).evalPy target
        let compound = Mod(It 4, left, right).evalPy target

        Assert.Equal("(value) % (2)", simple)
        Assert.Equal("(left+right) % (modulus-offset)", compound)
        Assert.DoesNotContain("divmod", simple)
        Assert.DoesNotContain("divmod", compound)

    [<Fact>]
    let ``Python FFT uses scalar modulo for its even-size branch`` () =
        use output = new TemporaryDirectory()

        Compile [Python] output.Path "python-fft-modulo" "1.0" <| fun context ->
            context.ch.z1 4 <| fun input ->
                context.ch.z1 4 <| fun transformed ->
                    context.fft1.fft("plan", input, transformed)

        let generated =
            File.ReadAllText(Path.Combine(output.Path, "python-fft-modulo.py"))

        Assert.Contains(") % (2) == 0", generated)
        Assert.DoesNotContain("divmod", generated)

    [<Fact>]
    let ``Python inverse FFT disables pyFFTW inverse normalization`` () =
        use output = new TemporaryDirectory()

        Compile [Python] output.Path "python-fft-scaling" "1.0" <| fun context ->
            context.ch.z1 4 <| fun input ->
                context.ch.z1 4 <| fun transformed ->
                    context.fft1.fft("forwardPlan1", input, transformed)
                    context.fft1.ifft("inversePlan1", transformed, input)
            context.ch.z2 (2, 3) <| fun input ->
                context.ch.z2 (2, 3) <| fun transformed ->
                    context.fft2.fft("forwardPlan2", input, transformed)
                    context.fft2.ifft("inversePlan2", transformed, input)

        let generated =
            File.ReadAllText(Path.Combine(output.Path, "python-fft-scaling.py"))

        Assert.Contains("forwardPlan1()", generated)
        Assert.Contains("inversePlan1(normalise_idft=False)", generated)
        Assert.Contains("forwardPlan2()", generated)
        Assert.Contains("inversePlan2(normalise_idft=False)", generated)
        Assert.DoesNotContain("forwardPlan1(normalise_idft=False)", generated)
        Assert.DoesNotContain("forwardPlan2(normalise_idft=False)", generated)
        Assert.Equal(
            2,
            System.Text.RegularExpressions.Regex.Matches(generated, "#normalize").Count)

    [<Fact>]
    let ``complex literals render without recursively simplifying their expansion`` () =
        use output = new TemporaryDirectory()
        let value = Cpx(1.0, 2.0)
        let cases : (Language * (expr -> Aqualis -> string)) list =
            [
                C99, fun expression target -> expression.evalC target
                Fortran, fun expression target -> expression.evalF target
                PHP, fun expression target -> expression.evalPh target
                Python, fun expression target -> expression.evalPy target
            ]

        for language, render in cases do
            let target =
                new Aqualis(
                    Some output.Path,
                    Some ("complex-" + language.ToString()),
                    language)

            try
                let format = numericFormatController language
                let expected =
                    format.DtoS 1.0 + "+uj*" + format.DtoS 2.0

                Assert.Equal(expected, render value target)
            finally
                target.close()
                target.delete()

    [<Fact>]
    let ``JavaScript conversions use valid JavaScript syntax`` () =
        use output = new TemporaryDirectory()
        use target =
            new Aqualis(Some output.Path, Some "javascript-conversions", JavaScript)

        let floatingValue = Var(Dt, "value", NaN)
        let integerValue = Var(It 4, "count", NaN)

        Assert.Equal(
            "Math.trunc(value)",
            ToInt floatingValue |> fun expression -> expression.evalJ target)
        Assert.Equal(
            "Number(count)",
            ToDbl integerValue |> fun expression -> expression.evalJ target)

    [<Fact>]
    let ``JavaScript rejects unsupported expressions`` () =
        use output = new TemporaryDirectory()
        use target =
            new Aqualis(Some output.Path, Some "javascript-unsupported", JavaScript)

        let complexValue = Var(Zt, "complexValue", NaN)
        let otherComplexValue = Var(Zt, "otherComplexValue", NaN)
        let unsupported =
            [
                "complex-number literals", Cpx(1.0, 2.0)
                "complex-number values", complexValue
                "complex-number arithmetic",
                    Add(Zt, complexValue, otherComplexValue)
                "complex-number functions", Exp(Zt, complexValue)
                "the absolute-value operation for complex numbers",
                    Abs(Dt, complexValue)
                "complex-number comparisons",
                    Eq(complexValue, otherComplexValue)
                "the real-part operation (Re)", Re complexValue
                "the imaginary-part operation (Im)", Im complexValue
                "the complex-conjugate operation (Conj)", Conj complexValue
                "complex-number array values",
                    Idx1(Zt, "complexValues", Int 0)
                "two-dimensional array indexing",
                    Idx2(Dt, "matrix", Int 0, Int 1)
                "three-dimensional array indexing",
                    Idx3(Dt, "tensor", Int 0, Int 1, Int 2)
            ]

        for operation,expression in unsupported do
            let error =
                Assert.Throws<NotSupportedException>(fun () ->
                    expression.evalJ target |> ignore)

            Assert.Equal(
                $"JavaScript code generation does not support {operation}.",
                error.Message)

        let unsupportedDisplayOperations : (string * (unit -> unit)) list =
            [ "equation display", fun () ->
                  expr.equivJ (Int 1) (Int 2) target
              "aligned equation display", fun () ->
                  expr.equivAlignJ (Int 1) (Int 2) target ]

        for operation,render in unsupportedDisplayOperations do
            let error =
                Assert.Throws<NotSupportedException>(Action render)
            Assert.Equal(
                $"JavaScript code generation does not support {operation}.",
                error.Message)

    [<Fact>]
    let ``JavaScript loop exits use break instead of goto`` () =
        use output = new TemporaryDirectory()
        let path = Path.Combine(output.Path, "javascript-loop.js")

        Aqualis.makeProgramWithContext
            (output.Path, "javascript-loop.js", JavaScript)
            (fun context ->
                expr.loopJ context <| fun (exit,_) -> exit()
                expr.range_exitJ context None (Int 0) (Int 2) <| fun (exit,_) ->
                    exit()
                context.close())

        let generated = File.ReadAllText path
        Assert.Contains("break;", generated)
        Assert.DoesNotContain("goto", generated)

    [<Fact>]
    let ``non-finite numeric literals have explicit language representations`` () =
        Assert.Equal("NAN", numericFormatController(C99).DtoS Double.NaN)
        Assert.Equal("INFINITY", numericFormatController(C99).DtoS Double.PositiveInfinity)
        Assert.Equal("-INFINITY", numericFormatController(C99).DtoS Double.NegativeInfinity)

        Assert.Equal("float('nan')", numericFormatController(Python).DtoS Double.NaN)
        Assert.Equal("float('inf')", numericFormatController(Python).DtoS Double.PositiveInfinity)
        Assert.Equal("-float('inf')", numericFormatController(Python).DtoS Double.NegativeInfinity)

        Assert.Equal(
            "ieee_value(0.0d0, ieee_quiet_nan)",
            numericFormatController(Fortran).DtoS Double.NaN)
        Assert.Equal(
            "ieee_value(0.0d0, ieee_positive_inf)",
            numericFormatController(Fortran).DtoS Double.PositiveInfinity)
        Assert.Equal(
            "ieee_value(0.0d0, ieee_negative_inf)",
            numericFormatController(Fortran).DtoS Double.NegativeInfinity)

    [<Fact>]
    let ``machine-readable artifact formatting rejects non-finite values`` () =
        Assert.Throws<ArgumentException>(fun () -> InvariantFormat.number Double.NaN |> ignore)
        |> ignore
        Assert.Throws<ArgumentException>(fun () -> InvariantFormat.number Double.PositiveInfinity |> ignore)
        |> ignore
        Assert.Throws<ArgumentException>(fun () -> InvariantFormat.number Double.NegativeInfinity |> ignore)
        |> ignore

    [<Fact>]
    let ``generated source keeps decimal points under a comma-decimal culture`` () =
        use output = new TemporaryDirectory()

        withCulture "fr-FR" <| fun () ->
            Compile
                [Fortran; C99; Python]
                output.Path
                "culture"
                "1.0"
                (fun environment ->
                    let value = environment.var.d0 "value"
                    value <== 1.5)

        let fortran = File.ReadAllText(Path.Combine(output.Path, "culture.f90"))
        let c = File.ReadAllText(Path.Combine(output.Path, "culture.c"))
        let python = File.ReadAllText(Path.Combine(output.Path, "culture.py"))

        Assert.Contains("value = 1.5d0", fortran)
        Assert.Contains("value = 1.5E0", c)
        Assert.Contains("value = 1.5E0", python)
        Assert.DoesNotContain("1,5", fortran)
        Assert.DoesNotContain("1,5", c)
        Assert.DoesNotContain("1,5", python)

    [<Fact>]
    let ``Fortran output imports IEEE arithmetic when using explicit non-finite values`` () =
        use output = new TemporaryDirectory()

        Compile
            [Fortran]
            output.Path
            "nonfinite"
            "1.0"
            (fun environment ->
                let value = environment.var.d0 "value"
                value <== Double.NaN)

        let generated =
            File.ReadAllText(Path.Combine(output.Path, "nonfinite.f90"))

        Assert.Contains("use, intrinsic :: ieee_arithmetic", generated)
        Assert.Contains("ieee_value(0.0d0, ieee_quiet_nan)", generated)
