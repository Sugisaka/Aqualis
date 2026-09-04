namespace Aqualis.Tests

open System
open System.IO
open Xunit
open Aqualis

module Plot2dTests =
    let private createLoadedPlot (output:TemporaryDirectory) =
        let source = Path.Combine(output.Path, "color-bar-source.dat")
        File.WriteAllLines(
            source,
            [|
                "0 0 1"
                "1 0 2"
                "0 1 3"
                "1 1 4"
            |])
        let plot = plot2d()
        plot.FileRead(source, 1, 2, 3, -1)
        Assert.Equal("", plot.Error)
        plot

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
    let ``unordered complete grid data is accepted`` () =
        use output = new TemporaryDirectory()
        let source = Path.Combine(output.Path, "unordered-grid.dat")
        File.WriteAllLines(
            source,
            [|
                "1 1 4"
                "0 0 1"
                "1 0 2"
                "0 1 3"
            |])

        let plot = plot2d()
        plot.FileRead(source, 1, 2, 3, -1)

        Assert.Equal("", plot.Error)
        Assert.Equal(2, plot.Nx)
        Assert.Equal(2, plot.Ny)

    [<Fact>]
    let ``missing real grid point is rejected and partial data is discarded`` () =
        use output = new TemporaryDirectory()
        let source = Path.Combine(output.Path, "missing-grid.dat")
        File.WriteAllLines(
            source,
            [|
                "0 0 1"
                "1 0 2"
                "0 1 3"
            |])

        let plot = plot2d()
        plot.FileRead(source, 1, 2, 3, -1)

        Assert.Contains("格子点が不足しています", plot.Error)
        Assert.Equal(0, plot.Nx)
        Assert.Equal(0, plot.Ny)

    [<Fact>]
    let ``missing complex grid point is rejected`` () =
        use output = new TemporaryDirectory()
        let source = Path.Combine(output.Path, "missing-complex-grid.csv")
        File.WriteAllLines(
            source,
            [|
                "0,0,1,2"
                "1,0,3,4"
                "0,1,5,6"
            |])

        let plot = plot2d()
        plot.FileRead(source, 1, 2, 3, 4)

        Assert.Contains("格子点が不足しています", plot.Error)
        Assert.Equal(0, plot.Nx)
        Assert.Equal(0, plot.Ny)

    [<Fact>]
    let ``duplicate point in an otherwise complete grid is rejected`` () =
        use output = new TemporaryDirectory()
        let source = Path.Combine(output.Path, "duplicate-grid.dat")
        File.WriteAllLines(
            source,
            [|
                "0 0 1"
                "1 0 2"
                "0 1 3"
                "1 1 4"
                "1 1 5"
            |])

        let plot = plot2d()
        plot.FileRead(source, 1, 2, 3, -1)

        Assert.Contains("格子点が重複しています", plot.Error)
        Assert.DoesNotContain("格子点が不足しています", plot.Error)
        Assert.Equal(0, plot.Nx)
        Assert.Equal(0, plot.Ny)

    [<Fact>]
    let ``duplicate and missing grid points cannot cancel each other`` () =
        use output = new TemporaryDirectory()
        let source = Path.Combine(output.Path, "duplicate-and-missing-grid.dat")
        File.WriteAllLines(
            source,
            [|
                "0 0 1"
                "1 0 2"
                "0 1 3"
                "0 1 4"
            |])

        let plot = plot2d()
        plot.FileRead(source, 1, 2, 3, -1)

        Assert.Contains("格子点が重複しています", plot.Error)
        Assert.Contains("格子点が不足しています", plot.Error)
        Assert.Contains("最初の行=4", plot.Error)
        Assert.Equal(0, plot.Nx)
        Assert.Equal(0, plot.Ny)

    [<Fact>]
    let ``coordinates outside the inferred regular grid are rejected`` () =
        use output = new TemporaryDirectory()
        let source = Path.Combine(output.Path, "off-grid.dat")
        File.WriteAllLines(
            source,
            [|
                "0 0 1"
                "1 0 2"
                "2.1 0 3"
                "0 1 4"
                "1 1 5"
                "2.1 1 6"
            |])

        let plot = plot2d()
        plot.FileRead(source, 1, 2, 3, -1)

        Assert.Contains("等間隔格子上にない座標があります", plot.Error)
        Assert.Equal(0, plot.Nx)
        Assert.Equal(0, plot.Ny)

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

    [<Fact>]
    let ``color bar rejects dimensions below two before creating a file`` () =
        use output = new TemporaryDirectory()
        let plot = createLoadedPlot output

        for width,height in [(-1, 2); (0, 2); (1, 2); (2, -1); (2, 0); (2, 1)] do
            let target = Path.Combine(output.Path, $"invalid-{width}-{height}.bmp")
            let thrown =
                Assert.Throws<ArgumentException>(fun () ->
                    plot.writeColorBar(
                        target,
                        width,
                        height,
                        colorMap.Gray,
                        PlotColorRange.Auto,
                        plot2d.getRe))

            Assert.Equal((if width < 2 then "width" else "height"), thrown.ParamName)
            Assert.False(File.Exists target)

    [<Fact>]
    let ``minimum color bar has matching BMP dimensions and file size`` () =
        use output = new TemporaryDirectory()
        let plot = createLoadedPlot output
        let target = Path.Combine(output.Path, "minimum-color-bar.bmp")

        plot.writeColorBar(target, 2, 2, colorMap.Gray, PlotColorRange.Auto, plot2d.getRe)

        use stream = File.OpenRead target
        use reader = new BinaryReader(stream)
        Assert.Equal(byte 'B', reader.ReadByte())
        Assert.Equal(byte 'M', reader.ReadByte())
        let headerFileSize = reader.ReadInt32()
        stream.Position <- 18L
        Assert.Equal(2, reader.ReadInt32())
        Assert.Equal(2, reader.ReadInt32())
        Assert.Equal(70L, stream.Length)
        Assert.Equal(int64 headerFileSize, stream.Length)

    [<Fact>]
    let ``complex minimum color bar produces finite pixel bytes`` () =
        use output = new TemporaryDirectory()
        let plot = createLoadedPlot output
        let target = Path.Combine(output.Path, "minimum-complex-color-bar.bmp")

        plot.writeColorBar(target, 2, 2, colorMap.ComplexVivid, PlotColorRange.Auto, plot2d.getAbs)

        let bytes = File.ReadAllBytes target
        Assert.Equal(70, bytes.Length)
        Assert.True(bytes[54..] |> Array.exists ((<>) 0uy))

    [<Fact>]
    let ``oversized color bar is rejected before creating a file`` () =
        use output = new TemporaryDirectory()
        let plot = createLoadedPlot output
        let target = Path.Combine(output.Path, "oversized-color-bar.bmp")

        let thrown =
            Assert.Throws<ArgumentException>(fun () ->
                plot.writeColorBar(
                    target,
                    Int32.MaxValue,
                    2,
                    colorMap.Gray,
                    PlotColorRange.Auto,
                    plot2d.getRe))

        Assert.Equal("dimensions", thrown.ParamName)
        Assert.False(File.Exists target)

    [<Fact>]
    let ``bitmap rejects nonpositive enlargement before creating a file`` () =
        use output = new TemporaryDirectory()
        let plot = createLoadedPlot output

        for enlarge in [0; -1] do
            let target = Path.Combine(output.Path, $"invalid-enlarge-{enlarge}.bmp")
            let thrown =
                Assert.Throws<ArgumentException>(fun () ->
                    plot.writeBMP24(
                        target,
                        colorMap.Gray,
                        PlotColorRange.Auto,
                        plot2d.getRe,
                        None,
                        enlarge))

            Assert.Equal("enlarge", thrown.ParamName)
            Assert.False(File.Exists target)

    [<Theory>]
    [<InlineData(20000)>]
    [<InlineData(Int32.MaxValue)>]
    let ``oversized bitmap is rejected before creating a file`` enlarge =
        use output = new TemporaryDirectory()
        let plot = createLoadedPlot output
        let target = Path.Combine(output.Path, $"oversized-{enlarge}.bmp")

        let thrown =
            Assert.Throws<ArgumentException>(fun () ->
                plot.writeBMP24(
                    target,
                    colorMap.Gray,
                    PlotColorRange.Auto,
                    plot2d.getRe,
                    None,
                    enlarge))

        Assert.Equal("enlarge", thrown.ParamName)
        Assert.False(File.Exists target)

    [<Fact>]
    let ``invalid bitmap request does not truncate an existing file`` () =
        use output = new TemporaryDirectory()
        let plot = createLoadedPlot output
        let target = Path.Combine(output.Path, "existing.bmp")
        let original = [|1uy; 2uy; 3uy; 4uy|]
        File.WriteAllBytes(target, original)

        Assert.Throws<ArgumentException>(fun () ->
            plot.writeBMP24(
                target,
                colorMap.Gray,
                PlotColorRange.Auto,
                plot2d.getRe,
                None,
                Int32.MaxValue))
        |> ignore

        Assert.Equal<byte>(original, File.ReadAllBytes target)

    [<Fact>]
    let ``enlarged bitmap header dimensions and sizes match the file`` () =
        use output = new TemporaryDirectory()
        let plot = createLoadedPlot output
        let target = Path.Combine(output.Path, "enlarged.bmp")

        plot.writeBMP24(
            target,
            colorMap.Gray,
            PlotColorRange.Auto,
            plot2d.getRe,
            None,
            2)

        use stream = File.OpenRead target
        use reader = new BinaryReader(stream)
        Assert.Equal(byte 'B', reader.ReadByte())
        Assert.Equal(byte 'M', reader.ReadByte())
        let headerFileSize = reader.ReadInt32()
        stream.Position <- 18L
        Assert.Equal(4, reader.ReadInt32())
        Assert.Equal(4, reader.ReadInt32())
        stream.Position <- 34L
        let headerPixelBytes = reader.ReadInt32()
        Assert.Equal(48, headerPixelBytes)
        Assert.Equal(102L, stream.Length)
        Assert.Equal(int64 headerFileSize, stream.Length)
