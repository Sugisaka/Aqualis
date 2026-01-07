//#############################################################################
// シーケンス図テスト
let projectname = "test8"
let version = "1.0.0"
//#############################################################################

let outputdir = __SOURCE_DIRECTORY__

#I @"..\..\bin\Debug\net10.0"
#r "Aqualis.dll"

open Aqualis

Compile [HTMLSequenceDiagram] outputdir "test8" (version,"aaa") <| fun () ->
    ch.ii <| fun (x,x1) ->
        x <== 0
        x1 <== 0
        iter.range (1,10) <| fun i ->
            x <== x1 + i
        br.if2 (x1 .= 0)
        <| fun i ->
            x <== 1
        <| fun i ->
            x <== 2
