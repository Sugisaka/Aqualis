//#############################################################################
// project title
let projectname = "test5"
let version = "1.0.0"
//#############################################################################
 
let outputdir = __SOURCE_DIRECTORY__

#I @"C:\home\LightwaveLaboratory\Aqualis\bin\Debug\net6.0"
#r "Aqualis.dll"
 
open Aqualis

Compile [F;C;] outputdir projectname ("aaa","aaa") <| fun () ->
    ch.n (It 4) <| fun n ->
    ch.n Dt <| fun x ->
        n <== 1
        x <== 1.0
        x <== n
        n <== x
        print.c n
        print.c x
        