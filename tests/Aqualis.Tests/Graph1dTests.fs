namespace Aqualis.Tests

open System
open System.IO
open Xunit
open Aqualis

module Graph1dTests =
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
