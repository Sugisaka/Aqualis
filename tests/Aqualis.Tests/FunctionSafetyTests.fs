namespace Aqualis.Tests

open System
open System.IO
open Xunit
open Aqualis

module FunctionSafetyTests =
    [<Fact>]
    let ``function and variable collisions fail in either definition order`` () =
        for language in [C99; Fortran; Python] do
            for functionFirst in [false; true] do
                for arraySize in [false; true] do
                    use output = new TemporaryDirectory()
                    let error = Assert.Throws<ArgumentException>(fun () ->
                        Compile [language] output.Path "name_collision" "1" (fun c ->
                            let functionName = if arraySize then "values_size" else "values"
                            let defineFunction () = c.func functionName ignore
                            if functionFirst then defineFunction()
                            if arraySize then c.var.d1("values", 2) |> ignore
                            else c.var.d0 "values" |> ignore
                            if not functionFirst then defineFunction()))
                    Assert.Contains("generated function name", error.Message)
                    Assert.Empty(Directory.GetFileSystemEntries output.Path)

    [<Fact>]
    let ``Fortran function name checks ignore case and include local scope`` () =
        for kind in [0..3] do
            use output = new TemporaryDirectory()
            Assert.Throws<ArgumentException>(fun () ->
                Compile [Fortran] output.Path "fortran_names" "1" (fun c ->
                    match kind with
                    | 0 ->
                        c.var.d0 "Evaluate" |> ignore
                        c.func "evaluate" ignore
                    | 1 ->
                        c.func "Evaluate" ignore
                        c.func "evaluate" ignore
                    | 2 -> c.func "evaluate" (fun f -> f.var.d0 "Evaluate" |> ignore)
                    | _ ->
                        let value = c.var.d0 "value"
                        c.func "arg01" (fun f -> value.farg f ignore))) |> ignore
            Assert.Empty(Directory.GetFileSystemEntries output.Path)

    [<Fact>]
    let ``function names cannot shadow generated temporaries`` () =
        for language in [C99; Fortran; Python] do
            use output = new TemporaryDirectory()
            Assert.Throws<ArgumentException>(fun () ->
                Compile [language] output.Path "temporary_name_collision" "1" (fun c ->
                    c.ch.d (fun temporary -> temporary <== 1.0)
                    c.func "d0001" ignore)) |> ignore
            Assert.Empty(Directory.GetFileSystemEntries output.Path)

    [<Fact>]
    let ``case sensitive and distinct local names remain valid`` () =
        for language in [C99; Python] do
            use output = new TemporaryDirectory()
            Compile [language] output.Path "distinct_names" "1" (fun c ->
                c.var.d0 "Evaluate" |> ignore
                c.func "evaluate" (fun f -> f.var.d0 "evaluate" |> ignore)
                c.func "other_function" (fun f -> f.var.d0 "Evaluate" |> ignore))

    [<Fact>]
    let ``scalar function arguments reject constants and computed expressions`` () =
        for language in [C99; Fortran; Python; HTML; LaTeX] do
            for kind in [0..5] do
                use output = new TemporaryDirectory()
                let mutable argumentBodyCalled = false
                let error = Assert.Throws<NotSupportedException>(fun () ->
                    Compile [language] output.Path "invalid_scalar_argument" "1" (fun c ->
                        let i = c.var.i0 "integer_value"
                        let d = c.var.d0 "real_value"
                        let z = c.var.z0 "complex_value"
                        c.func "update_value" (fun f ->
                            let called _ = argumentBodyCalled <- true
                            match kind with
                            | 0 -> (I 1).farg f called
                            | 1 -> (D 1.0).farg f called
                            | 2 -> (complex0(Cpx(1.0, 2.0))).farg f called
                            | 3 -> (i + 1).farg f called
                            | 4 -> (d + 1.0).farg f called
                            | _ -> (z + 1.0).farg f called)))
                Assert.Contains("pass a variable or array element", error.Message)
                Assert.False(argumentBodyCalled)
                Assert.Empty(Directory.GetFileSystemEntries output.Path)

    [<Fact>]
    let ``rejected scalar arguments do not consume formal argument slots`` () =
        use output = new TemporaryDirectory()
        Compile [Python] output.Path "scalar_recovery" "1" (fun c ->
            let value = c.var.d0 "value"
            c.func "update_value" (fun f ->
                Assert.Throws<NotSupportedException>(fun () -> (value + 1.0).farg f ignore) |> ignore
                value.farg f (fun v -> v <== 2.0)))
        let source = File.ReadAllText(Path.Combine(output.Path, "scalar_recovery.py"))
        Assert.Contains("def update_value(arg01):", source)
        Assert.Contains("value = update_value(value)", source)

    [<Fact>]
    let ``scalar variables and array elements remain writable function arguments`` () =
        for language in [C99; Fortran; Python; HTML; LaTeX] do
            use output = new TemporaryDirectory()
            Compile [language] output.Path "writable_scalar_arguments" "1" (fun c ->
                let i = c.var.i0 "integer_value"
                let d = c.var.d0 "real_value"
                let z = c.var.z0 "complex_value"
                let a = c.var.i1("vector", 2)
                let b = c.var.d2("matrix", 2, 3)
                let e = c.var.z3("volume", 2, 3, 4)
                c.func "update_values" (fun f ->
                    i.farg f (fun v -> v <== 1)
                    d.farg f (fun v -> v <== 2.0)
                    z.farg f (fun v -> v <== 3.0)
                    a[0].farg f (fun v -> v <== 4)
                    b[0,1].farg f (fun v -> v <== 5.0)
                    e[0,1,2].farg f (fun v -> v <== 6.0)))
            if language = Python then
                let source = File.ReadAllText(Path.Combine(output.Path, "writable_scalar_arguments.py"))
                Assert.Contains("integer_value, real_value, complex_value, vector[0], matrix[0,1], volume[0,1,2] = update_values(", source)

    [<Fact>]
    let ``nested functions fail before executing their body or publishing files`` () =
        for language in [C99; Fortran; Python; HTML; LaTeX] do
            for hasArgument in [false; true] do
                use output = new TemporaryDirectory()
                let mutable nestedBodyCalled = false
                let error = Assert.Throws<NotSupportedException>(fun () ->
                    Compile [language] output.Path "nested_functions" "1" (fun c ->
                        let value = c.var.d0 "value"
                        c.func "outer_fn" (fun f ->
                            let nested () =
                                f.func "inner_fn" (fun _ -> nestedBodyCalled <- true)
                            if hasArgument then value.farg f (fun _ -> nested())
                            else nested())))
                Assert.Contains("nested function definitions", error.Message)
                Assert.False(nestedBodyCalled)
                Assert.Empty(Directory.GetFileSystemEntries output.Path)

    [<Fact>]
    let ``rejecting a nested function leaves ordinary sibling functions usable`` () =
        for language, extension in [C99, ".c"; Fortran, ".f90"; Python, ".py"; HTML, ".html"; LaTeX, ".tex"] do
            use output = new TemporaryDirectory()
            Compile [language] output.Path "sibling_functions" "1" (fun c ->
                c.func "first_fn" (fun f ->
                    Assert.Throws<NotSupportedException>(fun () ->
                        f.func "second_fn" (fun _ -> failwith "Nested body must not run")) |> ignore
                    f.print.s "first")
                c.func "second_fn" (fun f -> f.print.s "second"))
            let source = File.ReadAllText(Path.Combine(output.Path, "sibling_functions" + extension))
            Assert.Contains("first_fn", source)
            Assert.Contains("second_fn", source)

    [<Theory>]
    [<InlineData(1)>]
    [<InlineData(2)>]
    [<InlineData(3)>]
    let ``Python whole array copies preserve destination storage`` rank =
        use output = new TemporaryDirectory()
        Compile [Python] output.Path "array_copy" "1" (fun c ->
            match rank with
            | 1 ->
                let src = c.var.d1("src", 2)
                let dst = c.var.d1("dst", 2)
                dst <== src
                c.func "copy_array" (fun f -> src.farg f (fun s -> dst.farg f (fun d -> d <== s; d <== d)))
            | 2 ->
                let src = c.var.z2("src", 2, 3)
                let dst = c.var.z2("dst", 2, 3)
                dst <== src
                c.func "copy_array" (fun f -> src.farg f (fun s -> dst.farg f (fun d -> d <== s; d <== d)))
            | _ ->
                let src = c.var.i3("src", 2, 3, 4)
                let dst = c.var.i3("dst", 2, 3, 4)
                dst <== src
                c.func "copy_array" (fun f -> src.farg f (fun s -> dst.farg f (fun d -> d <== s; d <== d))))
        let source = File.ReadAllText(Path.Combine(output.Path, "array_copy.py"))
        Assert.Contains("dst[...] = copy.deepcopy(src)", source)
        Assert.Contains("arg03[...] = copy.deepcopy(arg01)", source)
        Assert.Contains("arg03[...] = copy.deepcopy(arg03)", source)
        Assert.DoesNotContain("arg03 = copy.deepcopy(", source)
        Assert.Contains("Array size (first dimension) mismatch", source)
        if rank >= 2 then Assert.Contains("Array size (second dimension) mismatch", source)
        if rank >= 3 then Assert.Contains("Array size (third dimension) mismatch", source)

    [<Fact>]
    let ``borrowed C and Python arrays reject storage changes in every rank`` () =
        for language in [C99; Python] do
            for rank in [1;2;3] do
                for release in [false;true] do
                    use output = new TemporaryDirectory()
                    let error = Assert.Throws<NotSupportedException>(fun () ->
                        Compile [language] output.Path "borrowed" "1" (fun c ->
                            match rank with
                            | 1 ->
                                let a = c.var.d1 "values"
                                c.func "mutate" (fun f -> a.farg f (fun b -> if release then b.deallocate() else b.allocate 2))
                            | 2 ->
                                let a = c.var.z2 "values"
                                c.func "mutate" (fun f -> a.farg f (fun b -> if release then b.deallocate() else b.allocate(2,2)))
                            | _ ->
                                let a = c.var.i3 "values"
                                c.func "mutate" (fun f -> a.farg f (fun b -> if release then b.deallocate() else b.allocate(2,2,2)))))
                    Assert.Contains("manage storage in the caller", error.Message)
                    Assert.Empty(Directory.GetFileSystemEntries output.Path)

    [<Fact>]
    let ``borrowed arrays allow element updates and local temporary storage`` () =
        for language in [C99; Python; Fortran] do
            use output = new TemporaryDirectory()
            Compile [language] output.Path "borrowed_update" "1" (fun c ->
                let a = c.var.d1 "values"
                a.allocate 2
                c.func "update_values" (fun f ->
                    a.farg f (fun b ->
                        f.ch.d1 2 (fun temporary ->
                            temporary[0] <== 7.0
                            b[0] <== temporary[0])))
                a.deallocate())

    [<Fact>]
    let ``Fortran retains allocatable array argument support`` () =
        use output = new TemporaryDirectory()
        Compile [Fortran] output.Path "fortran_allocate" "1" (fun c ->
            let a = c.var.d1 "values"
            c.func "make_values" (fun f -> a.farg f (fun b -> b.allocate 2)))
        let source = File.ReadAllText(Path.Combine(output.Path,"fortran_allocate.f90"))
        Assert.Contains("allocatable :: arg01(:)", source)
        Assert.Contains("allocate(arg01", source)

    [<Fact>]
    let ``Python functions cannot replace imported modules or runtime helpers`` () =
        for name in ["numpy";"math";"cmath";"copy";"re";"struct";"sys";"random_seed";"solve";"eig";"svd";"jv";"yn";"_aqualis_int_div";"_aqualis_int_power"] do
            use output = new TemporaryDirectory()
            let mutable bodyCalled = false
            let error = Assert.Throws<ArgumentException>(fun () ->
                Compile [Python] output.Path "reserved_function" "1" (fun context ->
                    context.func name (fun _ -> bodyCalled <- true)))
            Assert.Equal("functionName", error.ParamName)
            Assert.Contains("reserved", error.Message)
            Assert.False(bodyCalled)
            Assert.Empty(Directory.GetFileSystemEntries output.Path)
