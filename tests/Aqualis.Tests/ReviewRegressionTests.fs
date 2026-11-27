namespace Aqualis.Tests

open System
open System.IO
open Xunit
open Aqualis

module ReviewRegressionTests =
    [<Fact>]
    let ``unmanaged output collisions abort the whole transaction`` () =
        for fileName in ["collision.py"; "proc_collision_P.sh"] do
            use output = new TemporaryDirectory()
            let path = Path.Combine(output.Path, fileName)
            File.WriteAllText(path, "user content")
            Assert.Throws<IOException>(fun () -> Compile [C99; Python] output.Path "collision" "1" ignore) |> ignore
            Assert.Equal("user content", File.ReadAllText path)
            Assert.Single(Directory.GetFiles output.Path) |> ignore
            Assert.Empty(Directory.GetDirectories output.Path)

    [<Fact>]
    let ``explicit replacement claims unmanaged files but protects subsequent edits`` () =
        use output = new TemporaryDirectory()
        let path = Path.Combine(output.Path, "replace.py")
        File.WriteAllText(path, "old content")
        let policy = { CompilationOutputPolicy.defaults with OverwriteUnmanagedFiles = true }
        CompileWithOutputPolicy policy [Python] output.Path "replace" "1" ignore |> ignore
        Assert.Contains("import numpy", File.ReadAllText path)
        File.WriteAllText(path, "user edit")
        Assert.Throws<IOException>(fun () ->
            CompileWithOutputPolicy policy [Python] output.Path "replace" "2" ignore |> ignore) |> ignore
        Assert.Equal("user edit", File.ReadAllText path)

    [<Fact>]
    let ``new target cannot overwrite unmanaged file beside managed outputs`` () =
        use output = new TemporaryDirectory()
        Compile [C99] output.Path "extend" "1" ignore
        let cPath = Path.Combine(output.Path, "extend.c")
        let previous = File.ReadAllText cPath
        let pyPath = Path.Combine(output.Path, "extend.py")
        File.WriteAllText(pyPath, "user content")
        Assert.Throws<IOException>(fun () -> Compile [C99; Python] output.Path "extend" "2" ignore) |> ignore
        Assert.Equal(previous, File.ReadAllText cPath)
        Assert.Equal("user content", File.ReadAllText pyPath)

    [<Fact>]
    let ``FFT rejects slices and computed inputs and outputs before publication`` () =
        for language in [C99; Fortran; Python] do
            for inverse in [false; true] do
                for outputSlice in [false; true] do
                    use output = new TemporaryDirectory()
                    let error = Assert.Throws<NotSupportedException>(fun () ->
                        Compile [language] output.Path "fft_slice" "1" (fun c ->
                            let matrix = c.var.z2("matrix", 3, 3)
                            let vector = c.var.z1("vector", 3)
                            let input, result = if outputSlice then vector, matrix[(),0] else matrix[(),0], vector
                            if inverse then c.fft1.ifft("plan", input, result)
                            else c.fft1.fft("plan", input, result)))
                    Assert.Contains("FFT array slices", error.Message)
                    Assert.Empty(Directory.GetFileSystemEntries output.Path)
                for outputExpression in [false; true] do
                    use output = new TemporaryDirectory()
                    Assert.Throws<NotSupportedException>(fun () ->
                        Compile [language] output.Path "fft_expression" "1" (fun c ->
                            let matrix = c.var.z2("matrix", 3, 3)
                            let expression = asm.conj matrix
                            let input, result = if outputExpression then matrix, expression else expression, matrix
                            if inverse then c.fft2.ifft("plan", input, result)
                            else c.fft2.fft("plan", input, result))) |> ignore
                    Assert.Empty(Directory.GetFileSystemEntries output.Path)

    [<Fact>]
    let ``invalid variable identifiers fail without publishing files`` () =
        for language,name in [C99,"for"; Python,"class"; JavaScript,"return"; Fortran,"1value";
                              PHP,"this"; Python,"numpy"; C99,"i0001"; Python,"arg01";
                              C99,"x;exit(0)"; Python,""] do
            use output = new TemporaryDirectory()
            Assert.Throws<ArgumentException>(fun () ->
                Compile [language] output.Path "invalid_name" "1" (fun c -> c.var.d0 name |> ignore)) |> ignore
            Assert.Empty(Directory.GetFileSystemEntries output.Path)

    [<Fact>]
    let ``keyword rules respect target language namespaces`` () =
        for language,name in [Fortran,"if"; PHP,"for"; C99,"Class"; Python,"value_1"] do
            use output = new TemporaryDirectory()
            Compile [language] output.Path "valid_name" "1" (fun c ->
                let value = c.var.d0 name
                value <== 1.0)

    [<Fact>]
    let ``Python linear algebra writes into function array arguments`` () =
        use output = new TemporaryDirectory()
        Compile [Python] output.Path "array_results" "1" (fun c ->
            let matrix = c.var.d2("matrix", 2, 2)
            let rhs = c.var.d1("rhs", 2)
            c.func "solve_system" (fun child ->
                matrix.farg child (fun a -> rhs.farg child (fun b -> child.la.solve_simuleq(a,b))))
            let inverse = c.var.d2("inverse", 2, 2)
            c.la.inverse_matrix(inverse,matrix)
            let u = c.var.d2("u", 2, 2)
            let s = c.var.d1("s", 2)
            let vt = c.var.d2("vt", 2, 2)
            c.la.svd matrix (u,s,vt)
            let complexMatrix = c.var.z2("complexMatrix", 2, 2)
            let eigenvalues = c.var.z1("eigenvalues", 2)
            let denominators = c.var.z1("denominators", 2)
            let eigenvectors = c.var.z2("eigenvectors", 2, 2)
            c.la.eigen_matrix (eigenvalues,eigenvectors) complexMatrix
            c.la.eigen_matrix2 (eigenvalues,denominators,eigenvectors) complexMatrix complexMatrix)
        let source = File.ReadAllText(Path.Combine(output.Path, "array_results.py"))
        Assert.Contains("arg03[...] = solve(arg01, arg03)", source)
        Assert.Contains("inverse[...] = numpy.linalg.inv(matrix)", source)
        Assert.Contains("u[...],s[...],vt[...] = svd(matrix)", source)
        Assert.Contains("eigenvalues[...],eigenvectors[...] = eig(complexMatrix)", source)
        Assert.Contains("(eigenvalues[...], denominators[...]), eigenvectors[...] = eig(", source)
