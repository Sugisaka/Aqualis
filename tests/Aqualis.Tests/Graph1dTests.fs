namespace Aqualis.Tests

open System
open System.IO
open System.Text.RegularExpressions
open Xunit
open Aqualis

module Graph1dTests =
    let private assertPoint (expectedX:double) (expectedY:double) (actualX:double,actualY:double) =
        Assert.Equal(expectedX, actualX, 10)
        Assert.Equal(expectedY, actualY, 10)

    let private writeDataFile () =
        let output = new TemporaryDirectory()
        let source = Path.Combine(output.Path, "columns.dat")
        File.WriteAllLines(source, [| "1 10"; "2 20" |])
        output, source

    let private renderDataGraph (xScale:Scale) (yScale:Scale) rows =
        use output = new TemporaryDirectory()
        let dataFileName = "plot-data.dat"
        let graphFileName = "plot.svg"
        File.WriteAllLines(Path.Combine(output.Path, dataFileName), rows)

        graph1d.makeGraph output.Path graphFileName (graph1d.A4PTwoColSingle 1) <| fun addGraph ->
            addGraph
                (1, 1)
                None
                {
                    Xaxis = {Scale=xScale; Range=Auto; NumFormat=None}
                    Yaxis = {Scale=yScale; Range=Auto; NumFormat=None}
                    Xlabel = "x"
                    Ylabel = "y"
                }
                [
                    Datafile {
                        Style = Points {
                            Shape = Circle
                            Size = 1.0
                            StrokeStyle = fun _ -> color.stroke.none
                            FillStyle = fun _ -> color.fill.black
                        }
                        FileName = dataFileName
                        Legend = None
                        Xcolumn = fun data -> data 1
                        Ycolumn = fun data -> data 2
                    }
                ]

        File.ReadAllText(Path.Combine(output.Path, graphFileName))

    [<Fact>]
    let ``column zero returns NaN without an out of range exception`` () =
        let output, source = writeDataFile ()
        use _ = output

        let x, y =
            graph1d.readdata source ((fun data -> data 0), (fun data -> data 2))

        Assert.All(x, fun value -> Assert.True(Double.IsNaN value))
        Assert.Equal(10.0, y[0])
        Assert.Equal(20.0, y[1])

    [<Fact>]
    let ``first and last one-based columns remain valid`` () =
        let output, source = writeDataFile ()
        use _ = output

        let first, last =
            graph1d.readdata source ((fun data -> data 1), (fun data -> data 2))

        Assert.Equal(1.0, first[0])
        Assert.Equal(2.0, first[1])
        Assert.Equal(10.0, last[0])
        Assert.Equal(20.0, last[1])

    [<Fact>]
    let ``columns outside the one-based range return NaN`` () =
        let output, source = writeDataFile ()
        use _ = output

        let below, above =
            graph1d.readdata source ((fun data -> data -1), (fun data -> data 3))

        Assert.All(below, fun value -> Assert.True(Double.IsNaN value))
        Assert.All(above, fun value -> Assert.True(Double.IsNaN value))

    [<Theory>]
    [<InlineData("0 0")>]
    [<InlineData("10 5")>]
    [<InlineData("-10 -5")>]
    let ``constant linear data produces finite plot coordinates`` row =
        let svg = renderDataGraph Linear Linear [|row; row|]
        let normalized = svg.ToLowerInvariant()

        Assert.DoesNotContain("nan", normalized)
        Assert.DoesNotContain("infinity", normalized)

    [<Fact>]
    let ``constant power of ten on logarithmic axis produces a finite range`` () =
        let svg = renderDataGraph Scale.Log10 Linear [|"10 1"; "10 2"|]
        let normalized = svg.ToLowerInvariant()

        Assert.DoesNotContain("nan", normalized)
        Assert.DoesNotContain("infinity", normalized)

    [<Fact>]
    let ``function segments use the lower boundary when leaving and reentering the range`` () =
        let segments =
            graph1d.clipFunctionSegments
                (0.0,1.0)
                [|(0.0,0.5); (1.0,-1.0); (2.0,0.5)|]

        Assert.Equal(2, segments.Length)
        Assert.Equal(2, segments.[0].Length)
        Assert.Equal(2, segments.[1].Length)
        assertPoint 0.0 0.5 segments.[0].[0]
        assertPoint (1.0/3.0) 0.0 segments.[0].[1]
        assertPoint (5.0/3.0) 0.0 segments.[1].[0]
        assertPoint 2.0 0.5 segments.[1].[1]

    [<Fact>]
    let ``function segment crossing the complete range is clipped at both boundaries`` () =
        let segments =
            graph1d.clipFunctionSegments
                (0.0,1.0)
                [|(0.0,-1.0); (1.0,2.0)|]

        Assert.Single(segments) |> ignore
        Assert.Equal(2, segments.[0].Length)
        assertPoint (1.0/3.0) 0.0 segments.[0].[0]
        assertPoint (2.0/3.0) 1.0 segments.[0].[1]

    [<Fact>]
    let ``nonfinite function samples split visible segments`` () =
        let segments =
            graph1d.clipFunctionSegments
                (0.0,1.0)
                [|
                    (0.0,0.25)
                    (1.0,0.5)
                    (2.0,Double.NaN)
                    (3.0,0.5)
                    (4.0,0.75)
                |]

        Assert.Equal(2, segments.Length)
        Assert.Equal(2, segments.[0].Length)
        Assert.Equal(2, segments.[1].Length)
        assertPoint 0.0 0.25 segments.[0].[0]
        assertPoint 1.0 0.5 segments.[0].[1]
        assertPoint 3.0 0.5 segments.[1].[0]
        assertPoint 4.0 0.75 segments.[1].[1]

    [<Fact>]
    let ``data polyline does not connect across an outside excursion`` () =
        let segments =
            graph1d.clipPolylineSegments
                (0.0,1.0)
                (0.0,1.0)
                [|(0.25,0.5); (2.0,0.5); (0.75,0.5)|]

        Assert.Equal(2, segments.Length)
        assertPoint 0.25 0.5 segments.[0].[0]
        assertPoint 1.0 0.5 segments.[0].[1]
        assertPoint 1.0 0.5 segments.[1].[0]
        assertPoint 0.75 0.5 segments.[1].[1]

    [<Fact>]
    let ``data segment with both endpoints outside is clipped through the rectangle`` () =
        let segments =
            graph1d.clipPolylineSegments
                (0.0,1.0)
                (0.0,1.0)
                [|(-1.0,0.5); (2.0,0.5)|]

        Assert.Single(segments) |> ignore
        Assert.Equal(2, segments.[0].Length)
        assertPoint 0.0 0.5 segments.[0].[0]
        assertPoint 1.0 0.5 segments.[0].[1]

    [<Fact>]
    let ``data segment clips correctly in reverse direction`` () =
        let segments =
            graph1d.clipPolylineSegments
                (0.0,1.0)
                (0.0,1.0)
                [|(2.0,0.25); (-1.0,0.75)|]

        Assert.Single(segments) |> ignore
        assertPoint 1.0 (5.0/12.0) segments.[0].[0]
        assertPoint 0.0 (7.0/12.0) segments.[0].[1]

    [<Fact>]
    let ``outside and nonfinite data segments are omitted`` () =
        let outside =
            graph1d.clipPolylineSegments
                (0.0,1.0)
                (0.0,1.0)
                [|(-2.0,2.0); (-1.0,3.0)|]
        let split =
            graph1d.clipPolylineSegments
                (0.0,1.0)
                (0.0,1.0)
                [|
                    (0.0,0.25)
                    (0.5,0.5)
                    (Double.NaN,0.5)
                    (0.5,0.5)
                    (1.0,0.75)
                |]

        Assert.Empty(outside)
        Assert.Equal(2, split.Length)
        assertPoint 0.0 0.25 split.[0].[0]
        assertPoint 0.5 0.5 split.[0].[1]
        assertPoint 0.5 0.5 split.[1].[0]
        assertPoint 1.0 0.75 split.[1].[1]

    [<Fact>]
    let ``data file rendering emits separate paths around an outside excursion`` () =
        use output = new TemporaryDirectory()
        let dataFileName = "clipped-lines.dat"
        let graphFileName = "clipped-lines.svg"
        File.WriteAllLines(
            Path.Combine(output.Path, dataFileName),
            [|"0.25 0.5"; "2.0 0.5"; "0.75 0.5"|])

        graph1d.makeGraph output.Path graphFileName (graph1d.A4PTwoColSingle 1) <| fun addGraph ->
            addGraph
                (1, 1)
                None
                {
                    Xaxis = {Scale=Linear; Range=MinMax(0.0,1.0); NumFormat=None}
                    Yaxis = {Scale=Linear; Range=MinMax(0.0,1.0); NumFormat=None}
                    Xlabel = "x"
                    Ylabel = "y"
                }
                [
                    Datafile {
                        Style = Lines {Style=color.stroke.magenta 0.5}
                        FileName = dataFileName
                        Legend = None
                        Xcolumn = fun data -> data 1
                        Ycolumn = fun data -> data 2
                    }
                ]

        let svg = File.ReadAllText(Path.Combine(output.Path, graphFileName))
        Assert.Equal(2, Regex.Matches(svg, "stroke:rgb\\(255,0,255\\)").Count)
        Assert.DoesNotContain("nan", svg.ToLowerInvariant())
        Assert.DoesNotContain("infinity", svg.ToLowerInvariant())

    [<Fact>]
    let ``function plot rejects fewer than two samples`` () =
        use output = new TemporaryDirectory()

        let thrown =
            Assert.Throws<ArgumentException>(fun () ->
                graph1d.makeGraph output.Path "invalid-sampling.svg" (graph1d.A4PTwoColSingle 1) <| fun addGraph ->
                    addGraph
                        (1, 1)
                        None
                        {
                            Xaxis = {Scale=Linear; Range=MinMax(0.0,1.0); NumFormat=None}
                            Yaxis = {Scale=Linear; Range=MinMax(0.0,1.0); NumFormat=None}
                            Xlabel = "x"
                            Ylabel = "y"
                        }
                        [
                            Function {
                                Style = Lines {Style=color.stroke.black 0.5}
                                Legend = None
                                Sampling = 1
                                Function = id
                            }
                        ])

        Assert.Equal("Sampling", thrown.ParamName)
