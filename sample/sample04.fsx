//#############################################################################
// project title
let projectname = "sample04"
// sample program version
let version = "1.0.0"
// Directory for source file output
let outputdir = @"C:\home\work"
//#############################################################################

#r "nuget: Aqualis, 188.0.3"

open Aqualis

Compile [Fortran;C99;Python] outputdir projectname version <| fun ctx ->
    let f(y:double0,x:double0,n:int0,n1:int1) =
        ctx.func "func1" <| fun c ->
            y.farg c <| fun y ->
            x.farg c <| fun x ->
            n.farg c <| fun n ->
            n1.farg c <| fun n1 ->
                y <== x + n + n1[0]
                c.print.t y
    ctx.ch.idd <| fun (n,x,z) ->
    ctx.ch.i1 4 <| fun n1 ->
        n <== 1
        x <== 2
        z <== 0
        n1[0] <== 3
        f(z,x,n,n1)
        ctx.print.t z
