namespace Aqualis.Tests

open System
open System.IO
open Xunit
open Aqualis

module IterativeSolverTests =
    [<Fact>]
    let ``BiCGSTAB rejects invalid tolerance and iteration limit`` () =
        use context = Aqualis.BlankWriter C99
        let rhs = context.var.z1("rhs", 1)
        let solution = context.var.z1("solution", 1)
        let multiply (result:complex1, input:complex1) = result[0] <== input[0]

        for tolerance in [0.0; -1.0; Double.NaN; Double.PositiveInfinity] do
            let error =
                Assert.Throws<ArgumentException>(fun () ->
                    simuleq.BiCGSTAB context rhs solution tolerance 10 multiply None)
            Assert.Equal("tol", error.ParamName)

        let error =
            Assert.Throws<ArgumentException>(fun () ->
                simuleq.BiCGSTAB context rhs solution 1e-8 0 multiply None)
        Assert.Equal("max_iteration", error.ParamName)

    [<Fact>]
    let ``associated Legendre polynomial emits an invalid order guard`` () =
        use output = new TemporaryDirectory()
        Compile [C99] output.Path "invalid-legendre" "1" <| fun context ->
            let result = context.var.d0 "result"
            context.math.aplgndr result (I 0, I 1, D 0.5)

        let generated = File.ReadAllText(Path.Combine(output.Path, "invalid-legendre.c"))
        Assert.Contains("Aqualis: Associated Legendre polynomial requires", generated)
