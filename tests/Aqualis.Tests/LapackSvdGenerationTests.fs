namespace Aqualis.Tests

open System
open Xunit
open Aqualis

module LapackSvdGenerationTests =
    [<Fact>]
    let ``SVD rejects shared U and VT output matrices`` () =
        for language in [C99; Fortran; Python] do
            use output = new TemporaryDirectory()
            Compile [language] output.Path "svd_shared_outputs" "1.0" <| fun context ->
                let realInput = context.var.d2 "realInput"
                let realOutput = context.var.d2 "realOutput"
                let realSingular = context.var.d1 "realSingular"
                realInput.allocate(2, 2)
                realOutput.allocate(2, 2)
                realSingular.allocate 2
                let realError =
                    Assert.Throws<ArgumentException>(fun () ->
                        context.la.svd realInput (realOutput, realSingular, realOutput))
                Assert.Contains("U and VT must be different matrices", realError.Message)

                let complexInput = context.var.z2 "complexInput"
                let complexOutput = context.var.z2 "complexOutput"
                let complexSingular = context.var.d1 "complexSingular"
                complexInput.allocate(2, 2)
                complexOutput.allocate(2, 2)
                complexSingular.allocate 2
                let complexError =
                    Assert.Throws<ArgumentException>(fun () ->
                        context.la.svd complexInput (complexOutput, complexSingular, complexOutput))
                Assert.Contains("U and VT must be different matrices", complexError.Message)
