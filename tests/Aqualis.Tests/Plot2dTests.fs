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
