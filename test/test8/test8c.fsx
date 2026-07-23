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
    "test8c" (version,"aaa") <| fun ctx ->
    
    //条件分岐
    ctx.ch.I "x" <| fun x ->
        x <== 5
        ctx.br.if2 (x .> 3) <| fun () ->
            x <== 0
        <| fun () ->
            x <== 1
