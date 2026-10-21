namespace Aqualis.Tests

open System
open System.IO
open Xunit
open Aqualis

module LapackBackendGenerationTests =
    [<Fact>]
    let ``FFT plan failures are checked before execution in every native transform`` () =
        for language in [C99; Fortran] do
            for dimensions in [1; 2] do
                for inverse in [false; true] do
                    use output = new TemporaryDirectory()
                    Compile [language] output.Path "fft_plan_guard" "1.0" (fun context ->
                        if dimensions = 1 then
                            let input = context.var.z1("input", 3)
                            let result = context.var.z1("result", 3)
                            if inverse then context.fft1.ifft("plan",input,result)
                            else context.fft1.fft("plan",input,result)
                        else
                            let input = context.var.z2("input", 2, 3)
                            let result = context.var.z2("result", 2, 3)
                            if inverse then context.fft2.ifft("plan",input,result)
                            else context.fft2.fft("plan",input,result))
                    let extension = if language = C99 then ".c" else ".f90"
                    let source = File.ReadAllText(Path.Combine(output.Path,"fft_plan_guard"+extension))
                    let planning = source.IndexOf("plan_dft_",StringComparison.Ordinal)
                    let guard = source.IndexOf("FFT plan creation failed.",StringComparison.Ordinal)
                    let execute = source.IndexOf("fftw_execute(",StringComparison.Ordinal)
                    Assert.True(planning >= 0 && guard > planning && execute > guard)
                    if language = C99 then
                        Assert.Contains("plan == NULL",source)
                        Assert.Contains("exit(EXIT_FAILURE)",source)
                    else
                        Assert.Contains("integer(8) :: plan",source)
                        Assert.DoesNotContain("plan(8)",source)
                        Assert.Contains("plan == 0",source)
                        Assert.Contains("error stop",source)

    [<Fact>]
    let ``LAPACK dependent operations reject unsupported languages before generation`` () =
        for language in [JavaScript; PHP; HTMLSequenceDiagram; Numeric] do
            use context = new Aqualis(None, None, language)
            let realMatrix = context.var.d2("realMatrix", 2, 2)
            let realOutput = context.var.d2("realOutput", 2, 2)
            let realOther = context.var.d2("realOther", 2, 2)
            let realVector = context.var.d1("realVector", 2)
            let realSingular = context.var.d1("realSingular", 2)
            let realRank = context.var.i0 "realRank"
            let complexMatrix = context.var.z2("complexMatrix", 2, 2)
            let complexOutput = context.var.z2("complexOutput", 2, 2)
            let complexOther = context.var.z2("complexOther", 2, 2)
            let complexVector = context.var.z1("complexVector", 2)
            let alpha = context.var.z1("alpha", 2)
            let beta = context.var.z1("beta", 2)
            let complexSingular = context.var.d1("complexSingular", 2)
            let complexRank = context.var.d0 "complexRank"
            let threshold = double0(Dbl 1e-10)

            let cases : (string * (unit -> unit)) list =
                [ "LAPACK matrix inversion", fun () -> context.la.inverse_matrix(realOutput, realMatrix)
                  "LAPACK matrix inversion", fun () -> context.la.inverse_matrix(complexOutput, complexMatrix)
                  "LAPACK determinant", fun () -> context.la.determinant realMatrix ignore
                  "LAPACK determinant", fun () -> context.la.determinant complexMatrix ignore
                  "LAPACK rank", fun () -> context.la.rank(realRank, realMatrix, threshold)
                  "LAPACK rank", fun () -> context.la.rank(complexRank, complexMatrix, threshold)
                  "LAPACK SVD", fun () -> context.la.svd realMatrix (realOutput, realSingular, realOther)
                  "LAPACK SVD", fun () -> context.la.svd complexMatrix (complexOutput, complexSingular, complexOther)
                  "LAPACK eigenvalue calculation", fun () -> context.la.eigen_matrix (alpha, complexOutput) complexMatrix
                  "LAPACK eigenvalue calculation", fun () -> context.la.eigen_matrix2 (alpha, beta, complexOutput) complexMatrix complexOther
                  "LAPACK pseudoinverse", fun () -> context.la.inverse_matrix2(realOutput, realMatrix, threshold)
                  "LAPACK pseudoinverse", fun () -> context.la.inverse_matrix2(complexOutput, complexMatrix, threshold)
                  "LAPACK homogeneous solve", fun () -> context.la.solve_homogeneq(realMatrix, realVector)
                  "LAPACK homogeneous solve", fun () -> context.la.solve_homogeneq(complexMatrix, complexVector)
                  "LAPACK Tikhonov solve", fun () -> context.la.solve_simuleq_t(realMatrix, realVector) ignore
                  "LAPACK Tikhonov solve", fun () -> context.la.solve_simuleq_t(complexMatrix, complexVector) ignore
                  "LAPACK Tikhonov solve", fun () -> context.la.solve_simuleq_tt(realMatrix, realVector, 1.0) ignore
                  "LAPACK Tikhonov solve", fun () -> context.la.solve_simuleq_tt(complexMatrix, complexVector, 1.0) ignore
                  "LAPACK Tikhonov solve", fun () -> context.la.solve_simuleq_tt2(complexMatrix, complexOutput, threshold) ignore ]

            for operationName, action in cases do
                let error = Assert.Throws<NotSupportedException>(Action action)
                Assert.Equal(
                    $"{language} code generation does not support {operationName}.",
                    error.Message)
