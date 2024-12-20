//#############################################################################
// project title
let projectname = "test5"
let version = "1.0.0"
//#############################################################################
 
let outputdir = __SOURCE_DIRECTORY__

#I @"..\bin\Debug\net8.0"
#r "Aqualis.dll"
 
open Aqualis

Compile [Fortran;C99;Python] outputdir projectname ("aaa","aaa") <| fun () ->
    let x = var.d0("a")
    let y = var.d0("a")
    x <== asm.pi
    y <== 1
    ch.n (It 4) <| fun n ->
    ch.n Dt <| fun x ->
        n <== 1
        x <== 1.0
        x <== n
        n <== x
        print.c n
        print.c x
        