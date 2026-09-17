//#############################################################################
// project title
let projectname = "sample09B"
// sample program version
let version = "1.0.0"
// Directory for source file output
let outputdir = @"C:\home\work"
//#############################################################################

#r "nuget: Aqualis, 188.0.0"

open Aqualis

Compile [HTMLSequenceDiagram] outputdir "test8b" version <| fun ctx ->

    //反復処理
    ctx.ch.I "x" <| fun x ->
        x <== 0
        ctx.iter.range (0,3) <| fun i ->
            x <== x + 1
            x <== 2 * x
