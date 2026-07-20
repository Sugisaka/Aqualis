//#############################################################################
// シーケンス図テスト
let projectname = "test8"
let version = "1.0.0"
//#############################################################################

let outputdir = __SOURCE_DIRECTORY__

#I @"..\..\bin\Debug\net10.0"
#r "Aqualis.dll"

open Aqualis

Compile [HTMLSequenceDiagram] outputdir 
    "test8d" (version,"aaa") <| fun () ->
    
    //結果(正しい代入)
    ch.I "x" <| fun x ->
    ch.I "x_1" <| fun x1 ->
        x <== 0
        x1 <== 0
        iter.range (1,10) <| fun i ->
            x <== x + i
