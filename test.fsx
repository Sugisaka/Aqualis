//#############################################################################
// プログラムの説明文
let projectname = "test"
let version = "0.0.0"
//#############################################################################

let outputdir = @"C:\cygwin64\home\work"

#I ".\\bin\\Debug\\net6.0"
#r "Aqualis.dll"

let fullversion = ("180.0.0.0",version)

open Aqualis

Compile [F] outputdir projectname fullversion <| fun () ->
    ch.private_i <| fun a ->
        omp.sections 3 <| fun () ->
            omp.section <| fun () ->
                a <== 1
                print.c a
            omp.section <| fun () ->
                a <== 2
                print.c a
            omp.section <| fun () ->
                a <== 3
                print.c a