//#############################################################################
// project title
let projectname = "test7"
let version = "1.0.0"
//#############################################################################
 
let outputdir = __SOURCE_DIRECTORY__

#I @"C:\home\LightwaveLaboratory\Aqualis\bin\Debug\net8.0"
#r "Aqualis.dll"
 
open Aqualis
open Aqualis_base

Compile [F;C;] outputdir projectname ("aaa","aaa") <| fun () ->
    let dd = 1E-7
    ch.dddd <| fun (x,y1,y2,dy) ->
    ch.d <| fun c1 ->
        x <== -10
        codestr.section "001" <| fun () ->
            let f (x:num0) = x+2+c1
            !"代数微分"
            dy <== asm.diff (f x) x
            !"数値微分"
            y1 <== f x
            y2 <== f (x+dd)
            print.cc dy ((y2-y1)/dd)
        codestr.section "002" <| fun () ->
            let f (x:num0) = 5*x+2
            !"代数微分"
            dy <== asm.diff (f x) x
            !"数値微分"
            y1 <== f x
            y2 <== f (x+dd)
            print.cc dy ((y2-y1)/dd)
        codestr.section "003" <| fun () ->
            let f (x:num0) = 5*x*x+2
            !"代数微分"
            dy <== asm.diff (f x) x
            !"数値微分"
            y1 <== f x
            y2 <== f (x+dd)
            print.cc dy ((y2-y1)/dd)
        codestr.section "004" <| fun () ->
            let f(x:num0) = (x+1)/(2*x+7)
            !"代数微分"
            dy <== asm.diff (f x) x
            !"数値微分"
            y1 <== f x
            y2 <== f (x+dd)
            print.cc dy ((y2-y1)/dd)
        codestr.section "005" <| fun () ->
            let f(x:num0) = x*asm.cos(x*x+2*x+1)
            !"代数微分"
            dy <== asm.diff (f x) x
            !"数値微分"
            y1 <== f x
            y2 <== f (x+dd)
            print.cc dy ((y2-y1)/dd)
        codestr.section "006" <| fun () ->
            let f(x:num0) = asm.sum 1 4 <| fun n -> n*x
            !"代数微分"
            dy <== asm.diff (f x) x
            !"数値微分"
            y1 <== f x
            y2 <== f (x+dd)
            print.cc dy ((y2-y1)/dd)
        codestr.section "007" <| fun () ->
            let f(x:num0) = (asm.sum 1 4 <| fun n -> n*x)+(asm.sum 1 4 <| fun n -> n*x)
            !"代数微分"
            dy <== asm.diff (f x) x
            !"数値微分"
            y1 <== f x
            y2 <== f (x+dd)
            print.cc dy ((y2-y1)/dd)
        codestr.section "008" <| fun () ->
            let f(x:num0) = (asm.sum 1 4 <| fun n -> n*x)+(asm.sum 1 5 <| fun n -> n*x)
            !"代数微分"
            dy <== asm.diff (f x) x
            !"数値微分"
            y1 <== f x
            y2 <== f (x+dd)
            print.cc dy ((y2-y1)/dd)
        codestr.section "009" <| fun () ->
            let f(x:num0) = (asm.sum 1 4 <| fun n -> n*x*x)+(asm.sum 1 10 <| fun n -> n*x)
            !"代数微分"
            dy <== asm.diff (f x) x
            !"数値微分"
            y1 <== f x
            y2 <== f (x+dd)
            print.cc dy ((y2-y1)/dd)
        codestr.section "010" <| fun () ->
            let f(x:num0) = asm.exp(asm.sum 1 4 <| fun n -> n*x/100)
            !"代数微分"
            dy <== asm.diff (f x) x
            !"数値微分"
            y1 <== f x
            y2 <== f (x+dd)
            print.cc dy ((y2-y1)/dd)
        codestr.section "011" <| fun () ->
            let f(x:num0) = asm.abs(asm.sum 1 4 <| fun n -> n*x)
            !"代数微分"
            dy <== asm.diff (f x) x
            !"数値微分"
            y1 <== f x
            y2 <== f (x+dd)
            print.cc dy ((y2-y1)/dd)
        codestr.section "012" <| fun () ->
            let f(x:num0) = asm.pow(asm.abs(asm.sum 1 4 <| fun n -> n*x),2)
            !"代数微分"
            dy <== asm.diff (f x) x
            !"数値微分"
            y1 <== f x
            y2 <== f (x+dd)
            print.cc dy ((y2-y1)/dd)
    ch.dddz <| fun (y1,y2r,y2i,dy) -> ch.z <| fun z ->
        z <== -1.8+asm.uj*3.5
        codestr.section "013" <| fun () ->
            let f(x:num0) = asm.pow(asm.abs(asm.sum 1 4 <| fun n -> n*x),2)
            !"代数微分"
            dy <== asm.diff (f z) z
            !"数値微分"
            y1 <== f z
            y2r <== f (z+dd)
            y2i <== f (z+dd*asm.uj)
            print.cc dy ((y2r-y1)/dd+asm.uj*(y2i-y1)/dd)
    ch.d1 10 <| fun ar ->
        codestr.section "014" <| fun () ->
            ar.foreach <| fun i -> ar[i] <== i
            let f(a:num1) = asm.sum 1 10 <| fun n -> n*a[n]
            ar.foreach <| fun i ->
                !"代数微分"
                dy <== asm.diff (f ar) ar[i]
                !"数値微分"
                y1 <== f ar
                ar[i] <== ar[i] + dd
                y2 <== f ar
                ar[i] <== ar[i] - dd
                print.ccc i dy ((y2-y1)/dd)