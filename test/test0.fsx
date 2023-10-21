//#############################################################################
// project title
let projectname = "test0"
let version = "1.0.0"
//#############################################################################
 
let outputdir = __SOURCE_DIRECTORY__

#I @"C:\home\LightwaveLaboratory\Aqualis\bin\Debug\net6.0"
#r "Aqualis.dll"
 
open Aqualis

Compile [F;C;T;H;] outputdir projectname ("aaa","bbb") <| fun () ->
    let x = var.d0("x")
    let y = var.d0("y")
    x <== asm.pi
    y <== 1
    ch.d <| fun a ->
    ch.d1 10 <| fun b ->
    ch.z2 10 20 <| fun c ->
        a <== asm.sin(x)
        iter.num b.size1 <| fun i ->
            b[i] <== b[i]/b.size1
        iter.num c.size1 <| fun i ->
        iter.num c.size2 <| fun j ->
            c[i,j] <== c[i,j]/(c.size1*c.size2)
            c[i,j] <== (c[i,j]+x*a/y)/(c.size1*c.size2)