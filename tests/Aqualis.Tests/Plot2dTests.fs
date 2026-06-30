namespace Aqualis.Tests

open System.IO
open Xunit
open Aqualis

module Plot2dTests =
    [<Fact>]
    let ``constant x values report an error without dividing by zero`` () =
        use output = new TemporaryDirectory()
        let source = Path.Combine(output.Path, "constant-x.dat")
        File.WriteAllLines(
            source,
            [|
                "1 1 10"
                "1 2 20"
                "1 3 30"
            |])

        let plot = plot2d()
        let thrown =
            Record.Exception(fun () ->
                plot.FileRead(source, 1, 2, 3, -1))

        Assert.Null(thrown)
        Assert.Contains("xの値が一定です", plot.Error)
        Assert.Equal(0, plot.Nx)

    [<Fact>]
    let ``constant y values report an error without dividing by zero`` () =
        use output = new TemporaryDirectory()
        let source = Path.Combine(output.Path, "constant-y.dat")
        File.WriteAllLines(
            source,
            [|
                "1 1 10"
                "2 1 20"
                "3 1 30"
            |])

        let plot = plot2d()
        let thrown =
            Record.Exception(fun () ->
                plot.FileRead(source, 1, 2, 3, -1))

        Assert.Null(thrown)
        Assert.Contains("yの値が一定です", plot.Error)
        Assert.Equal(0, plot.Ny)

    [<Fact>]
    let ``real grid data is parsed with comments and one split per row`` () =
        use output = new TemporaryDirectory()
        let source = Path.Combine(output.Path, "real-grid.dat")
        File.WriteAllLines(
            source,
            [|
                "# x y value"
                "0 0 1"
                "1 0 2"
                "# a comment between data rows"
                "0 1 3"
                "1 1 4"
            |])

        let plot = plot2d()
        plot.FileRead(source, 1, 2, 3, -1)

        Assert.Equal("", plot.Error)
        Assert.Equal(2, plot.Nx)
        Assert.Equal(2, plot.Ny)

    [<Fact>]
    let ``complex grid data uses the shared row parser`` () =
        use output = new TemporaryDirectory()
        let source = Path.Combine(output.Path, "complex-grid.csv")
        File.WriteAllLines(
            source,
            [|
                "0,0,1,2"
                "1,0,3,4"
                "0,1,5,6"
                "1,1,7,8"
            |])

        let plot = plot2d()
        plot.FileRead(source, 1, 2, 3, 4)

        Assert.Equal("", plot.Error)
        Assert.Equal(2, plot.Nx)
        Assert.Equal(2, plot.Ny)

    [<Fact>]
    let ``invalid numeric data is reported without throwing`` () =
        use output = new TemporaryDirectory()
        let source = Path.Combine(output.Path, "invalid-grid.dat")
        File.WriteAllLines(
            source,
            [|
                "0 0 1"
                "invalid 1 2"
            |])

        let plot = plot2d()
        let thrown =
            Record.Exception(fun () ->
                plot.FileRead(source, 1, 2, 3, -1))

        Assert.Null(thrown)
        Assert.Contains("数値に変換できません", plot.Error)
