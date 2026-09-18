//#############################################################################
// project title
let projectname = "sample09A"
// sample program version
let version = "1.0.0"
// Directory for source file output
let outputdir = @"C:\home\work"
//#############################################################################

#r "nuget: Aqualis, 188.0.2"

open Aqualis

Compile [HTMLSequenceDiagram] outputdir "test8a" version <| fun ctx ->
    //変数の定義と代入
    ctx.ch.I "x" <| fun x ->
    ctx.ch.I "y" <| fun y ->
        x <== 0
        y <== x + 1

