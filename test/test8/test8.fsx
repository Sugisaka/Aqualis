//#############################################################################
// project title
let projectname = "test8"
let version = "1.0.0"
//#############################################################################

let outputdir = __SOURCE_DIRECTORY__

#I @"..\..\bin\Debug\net10.0"
#r "Aqualis.dll"

open Aqualis

module cvdraw =
    let mutable cv = canvas()
    let canvasDraw filename code =
        freeCanvas outputdir filename <| fun c ->
            cv <- c
            code()
            
open cvdraw

canvasDraw "p24" <| fun () ->
    ch.ii <| fun (x,x1) ->
        x <== 0
        x1 <== 0
        iter.range (1,10) <| fun i ->
            x <== x1 + i
