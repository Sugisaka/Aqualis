namespace Aqualis.Tests

open System
open System.IO
open Xunit
open Aqualis

module FunctionSafetyTests =
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
