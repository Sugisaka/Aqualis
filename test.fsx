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

Compile [C99] outputdir projectname fullversion <| fun () ->
    omp.sections 3 <| fun () ->
        omp.section <| fun () ->
            print.c omp.thread_num
        omp.section <| fun () ->
            print.c omp.thread_num
        omp.section <| fun () ->
            print.c omp.thread_num

    omp.sections 3 <| fun () ->
        omp.section <| fun () ->
            print.c omp.thread_num
        omp.section <| fun () ->
            print.c omp.thread_num
        omp.section <| fun () ->
            print.c omp.thread_num