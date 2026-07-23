//#############################################################################
// シーケンス図テスト
let projectname = "test8"
let version = "1.0.0"
//#############################################################################

let outputdir = __SOURCE_DIRECTORY__

#I @"..\..\bin\Debug\net10.0"
#r "Aqualis.dll"

open Aqualis

Compile [HTMLSequenceDiagram] outputdir "test8b" version <| fun ctx ->

    //反復処理
    ctx.ch.I "x" <| fun x ->
        x <== 0
        ctx.iter.range (0,3) <| fun i ->
            x <== x + 1
            x <== 2 * x
