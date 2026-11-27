namespace Aqualis.Tests

open System
open System.IO
open Xunit
open Aqualis

module FunctionSafetyTests =
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
