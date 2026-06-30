namespace Aqualis.Tests

open System
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

    [<Fact>]
    let ``supported binary value types are validated and loaded`` () =
        use output = new TemporaryDirectory()

        let writeHeader (writer:BinaryWriter) (valueType:int) =
            writer.Write(1)
            writer.Write(valueType)
            writer.Write(2)
            writer.Write(2)
            writer.Write(2)

        let integerPath = Path.Combine(output.Path, "integer.bin")
        use integerStream = File.Create(integerPath)
        use integerWriter = new BinaryWriter(integerStream)
        writeHeader integerWriter 1004
        [|1; 2; 3; 4|] |> Array.iter integerWriter.Write
        integerWriter.Close()

        let realPath = Path.Combine(output.Path, "real.bin")
        use realStream = File.Create(realPath)
        use realWriter = new BinaryWriter(realStream)
        writeHeader realWriter 2000
        [|1.0; 2.0; 3.0; 4.0|] |> Array.iter realWriter.Write
        realWriter.Close()

        let complexPath = Path.Combine(output.Path, "complex.bin")
        use complexStream = File.Create(complexPath)
        use complexWriter = new BinaryWriter(complexStream)
        writeHeader complexWriter 3000
        [|1.0; 2.0; 3.0; 4.0; 5.0; 6.0; 7.0; 8.0|]
        |> Array.iter complexWriter.Write
        complexWriter.Close()

        for path in [integerPath; realPath; complexPath] do
            let plot = plot2d()
            plot.FileRead(path)
            Assert.Equal("", plot.Error)
            Assert.Equal(2, plot.Nx)
            Assert.Equal(2, plot.Ny)

    [<Fact>]
    let ``invalid binary dimensions and sizes are rejected before allocation`` () =
        use output = new TemporaryDirectory()
        let path = Path.Combine(output.Path, "invalid-size.bin")
        use stream = File.Create(path)
        use writer = new BinaryWriter(stream)
        writer.Write(1)
        writer.Write(2000)
        writer.Write(2)
        writer.Write(Int32.MaxValue)
        writer.Write(2)
        writer.Close()

        let plot = plot2d()
        let thrown = Record.Exception(fun () -> plot.FileRead(path))

        Assert.Null(thrown)
        Assert.Contains("配列サイズが有効範囲外です", plot.Error)
        Assert.Equal(0, plot.Nx)
        Assert.Equal(0, plot.Ny)

    [<Fact>]
    let ``truncated binary data leaves the plot unloaded`` () =
        use output = new TemporaryDirectory()
        let path = Path.Combine(output.Path, "truncated.bin")
        use stream = File.Create(path)
        use writer = new BinaryWriter(stream)
        writer.Write(1)
        writer.Write(3000)
        writer.Write(2)
        writer.Write(2)
        writer.Write(2)
        writer.Write(1.0)
        writer.Close()

        let plot = plot2d()
        let thrown = Record.Exception(fun () -> plot.FileRead(path))

        Assert.Null(thrown)
        Assert.Contains("データが不足しています", plot.Error)
        Assert.Equal(0, plot.Nx)
        Assert.Equal(0, plot.Ny)
