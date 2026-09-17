//#############################################################################
// project title
let projectname = "sample09E"
// sample program version
let version = "1.0.0"
// Directory for source file output
let outputdir = @"C:\home\work"
//#############################################################################

#I @"..\bin\Debug\net10.0"
#r "Aqualis.dll"

open Aqualis

Compile [HTMLSequenceDiagram] outputdir "test8e" version <| fun ctx ->
    
    //結果(間違った代入)
    ctx.ch.I "x" <| fun x ->
    ctx.ch.I "x_1" <| fun x1 ->
        x <== 0
        x1 <== 0
        ctx.iter.range (1,10) <| fun i ->
            x <== x1 + i