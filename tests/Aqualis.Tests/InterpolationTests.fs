namespace Aqualis.Tests

open System
open System.IO
open Xunit
open Aqualis

module InterpolationTests =
    let private generateC code =
        use output = new TemporaryDirectory()
        Compile [C99] output.Path "interpolation" "1.0" code
        File.ReadAllText(Path.Combine(output.Path, "interpolation.c"))

    [<Fact>]
    let ``linear interpolation uses the last valid real array index`` () =
        let source =
            generateC (fun context ->
                let result = context.var.d0 "result"
                let interpolation =
                    interpolate.LinearInterpolate1d(
                        context,
                        "sample",
                        [0.0; 1.0; 2.0],
                        [0.0; 10.0; 20.0])
                interpolation.y (double0(Dbl 2.0)) (fun value ->
                    result <== value))

        Assert.Contains("sample_x[sample_x_size[0]-1]", source)
        Assert.Contains("sample_y[sample_x_size[0]-1]", source)
        Assert.Contains("sample_x[0]", source)
        Assert.DoesNotContain("sample_x[sample_x_size[0]]", source)
        Assert.DoesNotContain("sample_y[sample_x_size[0]]", source)

    [<Fact>]
    let ``linear interpolation uses the last valid complex array index`` () =
        let source =
            generateC (fun context ->
                let result = context.var.z0 "result"
                let interpolation =
                    interpolate.LinearInterpolate1z(
                        context,
                        "sample",
                        [0.0; 1.0; 2.0],
                        [(0.0, 0.0); (10.0, 1.0); (20.0, 2.0)])
                interpolation.y (double0(Dbl 2.0)) (fun value ->
                    result <== value))

        Assert.Contains("sample_x[sample_x_size[0]-1]", source)
        Assert.Contains("sample_y[sample_x_size[0]-1]", source)
        Assert.DoesNotContain("sample_x[sample_x_size[0]]", source)
        Assert.DoesNotContain("sample_y[sample_x_size[0]]", source)

    [<Fact>]
    let ``linear interpolation validates input data`` () =
        use output = new TemporaryDirectory()
        use context =
            new Aqualis(Some output.Path, Some "validation.c", C99)

        Assert.Throws<ArgumentException>(fun () ->
            interpolate.LinearInterpolate1d(
                context,
                "too_short",
                [0.0],
                [1.0])
            |> ignore)
        |> ignore

        Assert.Throws<ArgumentException>(fun () ->
            interpolate.LinearInterpolate1d(
                context,
                "different_lengths",
                [0.0; 1.0],
                [1.0])
            |> ignore)
        |> ignore

        Assert.Throws<ArgumentException>(fun () ->
            interpolate.LinearInterpolate1z(
                context,
                "not_increasing",
                [0.0; 0.0],
                [(1.0, 0.0); (2.0, 0.0)])
            |> ignore)
        |> ignore

        Assert.Throws<ArgumentException>(fun () ->
            interpolate.LinearInterpolate1d(
                context,
                "not_finite",
                [0.0; Double.NaN],
                [1.0; 2.0])
            |> ignore)
        |> ignore
