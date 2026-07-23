//#############################################################################
// シーケンス図テスト
let projectname = "test8"
let version = "1.0.0"
//#############################################################################

let outputdir = __SOURCE_DIRECTORY__

#I @"..\..\bin\Debug\net10.0"
#r "Aqualis.dll"

open Aqualis

Compile [HTMLSequenceDiagram] outputdir "test8a" version <| fun ctx ->
    //変数の定義と代入
    ctx.ch.I "x" <| fun x ->
    ctx.ch.I "y" <| fun y ->
        x <== 0
        y <== x + 1

