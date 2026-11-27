namespace Aqualis.Tests

open System.IO
open System.Reflection
open Xunit
open Aqualis

module ArraySizeGenerationTests =
    let private languages = [Fortran; C99; Python; JavaScript; PHP; LaTeX; HTML; HTMLSequenceDiagram; Numeric]

    [<Fact>]
    let ``named array sizes preserve expressions rendering contexts and public API`` () =
        let generated =
            languages
            |> List.map (fun language ->
                use context = Aqualis.BlankWriter language
                let sizes =
                    [for dynamic in [false; true] do
                        let a = base1(Dt, Var1((if dynamic then A1 0 else A1 4), "vector"), context)
                        let b = base2(Dt, Var2((if dynamic then A2(0,0) else A2(4,5)), "matrix"), context)
                        let c = base3(Dt, Var3((if dynamic then A3(0,0,0) else A3(4,5,6)), "tensor"), context)
                        yield! [a.size1; b.size1; b.size2; c.size1; c.size2; c.size3]]
                let rendered =
                    sizes |> List.map (fun size ->
                        Assert.Same(context, size.Context)
                        sprintf "%A | %s" size.Expr (size.Expr.eval context))
                string language + "\n" + String.concat "\n" rendered)
            |> String.concat "\n--- language ---\n"
        let api =
            [typeof<base1>; typeof<base2>; typeof<base3>]
            |> List.collect (fun t ->
                t.GetMembers(BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.DeclaredOnly)
                |> Array.map (fun memberInfo -> t.Name + ": " + string memberInfo)
                |> Array.toList)
            |> List.sort |> String.concat "\n"
        let actual = generated + "\n--- API ---\n" + api
        let golden = TestHelpers.goldenPath "arrays" "sizes.txt"
        Assert.Equal(File.ReadAllText(golden).Replace("\r\n", "\n"), actual)

    [<Fact>]
    let ``computed arrays return their original size values and contexts`` () =
        for language in languages do
            use context = Aqualis.BlankWriter language
            use extentContext = Aqualis.BlankWriter language
            let first = int0(Int 4, extentContext)
            let second = int0(Var(It 4,"extent",NaN), extentContext)
            let third = int0(Int 6, extentContext)
            let a = base1(Dt, Arx1(first, fun _ -> Dbl 0.0), context)
            let b = base2(Dt, Arx2(first, second, fun _ -> Dbl 0.0), context)
            let c = base3(Dt, Arx3(first, second, third, fun _ -> Dbl 0.0), context)
            for expected,actual in [first,a.size1; first,b.size1; second,b.size2; first,c.size1; second,c.size2; third,c.size3] do
                Assert.Same(expected, actual)
                Assert.Same(extentContext, actual.Context)
