//#############################################################################
// project title
let projectname = "sample01"
// sample program version
let version = "1.0.0"
// Directory for source file output
let outputdir = @"C:\home\work"
//#############################################################################

#r "nuget: Aqualis, 188.0.0"

open Aqualis

Compile [C99;Fortran;Python] outputdir projectname version <| fun ctx ->
    // print text
    ctx.print.s "Hello World!"
    // provide interger variables
    ctx.ch.i <| fun x ->
    ctx.ch.i <| fun y ->
    ctx.ch.i <| fun z ->
        // substitute 1 to x
        x <== 1
        // substitute 2 to y
        y <== 2
        // substitute x+y to z
        z <== x + y
        // print z
        ctx.print.t z
