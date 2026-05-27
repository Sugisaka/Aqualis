//#############################################################################
// project title
let projectname = "sample"
// Aqualis version, this sample program version
let version = "186.0.4.0", "1.0.0"
// Directory for source file output
let outputdir = @"C:\home\work"
//#############################################################################

#I "C:\\Aqualis\\lib\\186_0_4_0"
#r "Aqualis.dll"

open Aqualis

Compile [C99;Fortran;Python] outputdir projectname version <| fun () ->
    // print text
    print.t "Hello World!"
    // provide interger variables
    ch.iii <| fun (x,y,z) ->
        // substitute 1 to x
        x <== 1
        // substitute 2 to y
        y <== 2
        // substitute x+y to z
        z <== x + y
        // print z
        print.c z
