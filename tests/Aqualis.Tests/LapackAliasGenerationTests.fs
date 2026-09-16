namespace Aqualis.Tests

open System
open Xunit
open Aqualis

module LapackAliasGenerationTests =
    [<Fact>]
    let ``solve rejects a shared coefficient matrix and right-hand side`` () =
        for language in [C99; Fortran; Python] do
            use output = new TemporaryDirectory()
            Compile [language] output.Path "solve_shared_matrix" "1.0" <| fun context ->
                let realMatrix = context.var.d2 "realMatrix"
                realMatrix.allocate(2, 2)
                let realError =
                    Assert.Throws<ArgumentException>(fun () ->
                        context.la.solve_simuleqs(realMatrix, realMatrix))
                Assert.Contains("coefficient matrix and right-hand side must be different", realError.Message)

                let complexMatrix = context.var.z2 "complexMatrix"
                complexMatrix.allocate(2, 2)
                let complexError =
                    Assert.Throws<ArgumentException>(fun () ->
                        context.la.solve_simuleqs(complexMatrix, complexMatrix))
                Assert.Contains("coefficient matrix and right-hand side must be different", complexError.Message)

    [<Fact>]
    let ``eigen rejects an input matrix reused for eigenvectors`` () =
        for language in [C99; Fortran; Python] do
            use output = new TemporaryDirectory()
            Compile [language] output.Path "eigen_shared_matrix" "1.0" <| fun context ->
                let matrix = context.var.z2 "matrix"
                let values = context.var.z1 "values"
                matrix.allocate(2, 2)
                values.allocate 2
                let error =
                    Assert.Throws<ArgumentException>(fun () ->
                        context.la.eigen_matrix (values, matrix) matrix)
                Assert.Contains("eigenvector output must be different from input matrices", error.Message)

    [<Fact>]
    let ``generalized eigen rejects shared input and output arrays`` () =
        for language in [C99; Fortran; Python] do
            use output = new TemporaryDirectory()
            Compile [language] output.Path "generalized_eigen_shared_arrays" "1.0" <| fun context ->
                let matrixA = context.var.z2 "matrixA"
                let matrixB = context.var.z2 "matrixB"
                let vectors = context.var.z2 "vectors"
                let alpha = context.var.z1 "alpha"
                let beta = context.var.z1 "beta"
                matrixA.allocate(2, 2)
                matrixB.allocate(2, 2)
                vectors.allocate(2, 2)
                alpha.allocate 2
                beta.allocate 2

                for input in [matrixA; matrixB] do
                    let error =
                        Assert.Throws<ArgumentException>(fun () ->
                            context.la.eigen_matrix2 (alpha, beta, input) matrixA matrixB)
                    Assert.Contains("eigenvector output must be different from input matrices", error.Message)

                let valuesError =
                    Assert.Throws<ArgumentException>(fun () ->
                        context.la.eigen_matrix2 (alpha, alpha, vectors) matrixA matrixB)
                Assert.Contains("eigenvalue outputs must be different vectors", valuesError.Message)
