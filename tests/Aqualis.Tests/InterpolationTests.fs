namespace Aqualis.Tests

open System
open System.IO
open System.Text.RegularExpressions
open Xunit
open Aqualis

module InterpolationTests =
    let private generateC code =
        use output = new TemporaryDirectory()
        Compile [C99] output.Path "interpolation" "1.0" code
        File.ReadAllText(Path.Combine(output.Path, "interpolation.c"))

    let private assertSplineUsesLeftEndpointOrigin (source:string) =
        Assert.Matches(
            Regex(@"query\s*-\s*x\[i0\d+\]"),
            source)
        Assert.DoesNotMatch(
            Regex(@"query\s*-\s*x\[i0\d+\s*\+\s*1\]"),
            source)
        Assert.Matches(
            Regex(@"query\s*==\s*x\[x_size\[0\]\s*-\s*1\]"),
            source)
        Assert.Matches(
            Regex(@"query\s*-\s*x\[x_size\[0\]\s*-\s*2\]"),
            source)

    [<Fact>]
    let ``real spline value and derivative use the interval left endpoint`` () =
        let source =
            generateC (fun context ->
                let interpolation = context.interpolate.splineDouble()
                let query = context.var.d0 "query"
                let value = context.var.d0 "value"
                let derivative = context.var.d0 "derivative"

                interpolation.p value query
                interpolation.dp derivative query)

        assertSplineUsesLeftEndpointOrigin source

    [<Fact>]
    let ``complex spline value and derivative use the interval left endpoint`` () =
        let source =
            generateC (fun context ->
                let interpolation = context.interpolate.splineComplex(true)
                let query = context.var.d0 "query"
                let value = context.var.z0 "value"
                let derivative = context.var.z0 "derivative"

                interpolation.p value query
                interpolation.dp derivative query)

        assertSplineUsesLeftEndpointOrigin source

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
