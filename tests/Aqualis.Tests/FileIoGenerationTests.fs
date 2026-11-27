namespace Aqualis.Tests

open System
open System.IO
open System.Reflection
open Xunit
open Aqualis

module FileIoGenerationTests =
    [<Fact>]
    let ``file operations preserve generated output and public API`` () =
        let generated =
            [Fortran; C99; Python; LaTeX; HTML]
            |> List.map (fun language ->
                use output = new TemporaryDirectory()
                use context = new Aqualis(Some output.Path, Some "io.txt", language)
                let io = context.io
                let value = context.var.i0 "value"
                let real = context.var.d0 "realValue"
                let complex = context.var.z0 "complexValue"
                let name = st "quote'\"%日本_" ++ value ++ ".dat"
                io.fileOutput name (fun writer -> writer.tt (st "row=" ++ value ++ real); writer.cc (iv value))
                io.fileOutput(name, 5) (fun writer -> writer.t value)
                io.fileOutput "plain.txt" (fun _ -> io.fileOutput "nested.txt" (fun writer -> writer.t "nested"))
                io.binfileOutput name (fun writer -> writer.b value; writer.b real; writer.b complex)
                io.binfileOutput(name, 3) (fun writer -> writer.b 42)
                io.binfileOutput "plain.bin" ignore
                io.fileInput name (fun reader -> reader.tt (iv value ++ real))
                io.fileInput(name, 4) (fun reader -> reader.t value)
                io.fileInput "plain.txt" (fun reader -> reader.t real)
                if List.contains language [Fortran; C99; Python] then
                    io.fileInput "complex.txt" (fun reader -> reader.t complex)
                    io.fileInput "byte.txt" (fun reader -> reader.b value)
                io.binfileInput name (fun reader -> reader.b value; reader.b real; reader.b complex)
                io.binfileInput(name, 6) (fun reader -> reader.b value)
                io.binfileInput "plain.bin" ignore
                io.file_Read name (iv value ++ real) (fun _ -> context.comment "record")
                io.file_Read(name, 2) (iv value) (fun _ -> context.comment "record with width")
                context.close()
                string language + "\n" + File.ReadAllText(Path.Combine(output.Path, "io.txt")).Replace("\r\n", "\n"))
            |> String.concat "\n--- language ---\n"
        let api =
            typeof<ContextIo>.GetMembers(BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.DeclaredOnly)
            |> Array.map string |> Array.sort |> String.concat "\n"
        let actual = generated + "\n--- API ---\n" + api
        let golden = TestHelpers.goldenPath "fileio" "operations.txt"
        Assert.Equal(File.ReadAllText(golden).Replace("\r\n", "\n"), actual)

    [<Theory>]
    [<InlineData("C99")>]
    [<InlineData("Fortran")>]
    [<InlineData("Python")>]
    let ``file name integer widths reject nonpositive values`` target =
        let language = match target with "C99" -> C99 | "Fortran" -> Fortran | _ -> Python
        use output = new TemporaryDirectory()
        use context = new Aqualis(Some output.Path, Some "invalid.txt", language)
        for width in [0; -1] do
            let error = Assert.Throws<ArgumentException>(fun () -> context.io.fileOutput(st "test", width) ignore)
            Assert.Equal("intDigit", error.ParamName)
