//#############################################################################
// project title
let projectname = "sample09C"
// sample program version
let version = "1.0.0"
// Directory for source file output
let outputdir = @"C:\home\work"
//#############################################################################

#I @"..\bin\Debug\net10.0"
#r "Aqualis.dll"

open Aqualis

Compile [HTMLSequenceDiagram] outputdir "test8c" version <| fun ctx ->
    
    //条件分岐
    ctx.ch.I "x" <| fun x ->
        x <== 5
        ctx.br.if2 (x .> 3) <| fun () ->
            x <== 0
        <| fun () ->
            x <== 1
