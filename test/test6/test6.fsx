//#############################################################################
// project title
let projectname = "test6"
let version = "1.0.0"
//#############################################################################
 
let outputdir = __SOURCE_DIRECTORY__

#I @"..\..\bin\Debug\net10.0"
#r "Aqualis.dll"

open Aqualis

Compile [Fortran;C99;Python] outputdir projectname ("aaa","aaa") <| fun () ->
    let dd = 1E-5
    ch.dddd <| fun (x,y1,y2,dy) ->
    ch.d <| fun c1 ->
        x <== -10
        dummy_codestr.section "001" <| fun () ->
            let f (x:num0) = x+2+c1
            !"代数微分"
            dy <== asm.diff (f x) x
            !"数値微分"
            y1 <== f x
            y2 <== f (x+dd)
            print.cc dy ((y2-y1)/dd)
        dummy_codestr.section "002" <| fun () ->
            let f (x:num0) = 5*x+2
            !"代数微分"
            dy <== asm.diff (f x) x
            !"数値微分"
            y1 <== f x
            y2 <== f (x+dd)
            print.cc dy ((y2-y1)/dd)
        dummy_codestr.section "003" <| fun () ->
            let f (x:num0) = 5*x*x+2
            !"代数微分"
            dy <== asm.diff (f x) x
            !"数値微分"
            y1 <== f x
            y2 <== f (x+dd)
            print.cc dy ((y2-y1)/dd)
        dummy_codestr.section "004" <| fun () ->
            let f(x:num0) = (x+1)/(2*x+7)
            !"代数微分"
            dy <== asm.diff (f x) x
            !"数値微分"
            y1 <== f x
            y2 <== f (x+dd)
            print.cc dy ((y2-y1)/dd)
        dummy_codestr.section "005" <| fun () ->
            let f(x:num0) = x*asm.cos(x*x+2*x+1)
            !"代数微分"
            dy <== asm.diff (f x) x
            !"数値微分"
            y1 <== f x
            y2 <== f (x+dd)
            print.cc dy ((y2-y1)/dd)
        dummy_codestr.section "006" <| fun () ->
            let f(x:num0) = asm.dSum (1,4) <| fun n -> n*x
            !"代数微分"
            dy <== asm.diff (f x) x
            !"数値微分"
            y1 <== f x
            y2 <== f (x+dd)
            print.cc dy ((y2-y1)/dd)
        dummy_codestr.section "007" <| fun () ->
            let f(x:num0) = (asm.dSum (1,4) <| fun n -> n*x)+(asm.dSum (1,4) <| fun n -> n*x)
            !"代数微分"
            dy <== asm.diff (f x) x
            !"数値微分"
            y1 <== f x
            y2 <== f (x+dd)
            print.cc dy ((y2-y1)/dd)
        dummy_codestr.section "008" <| fun () ->
            let f(x:num0) = (asm.dSum (1,4) <| fun n -> n*x*x)+(asm.dSum (1,10) <| fun n -> n*x)
            !"代数微分"
            dy <== asm.diff (f x) x
            !"数値微分"
            y1 <== f x
            y2 <== f (x+dd)
            print.cc dy ((y2-y1)/dd)
        dummy_codestr.section "009" <| fun () ->
            let f(x:num0) = asm.exp(asm.dSum (1,4) <| fun n -> n*x/100)
            !"代数微分"
            dy <== asm.diff (f x) x
            !"数値微分"
            y1 <== f x
            y2 <== f (x+dd)
            print.cc dy ((y2-y1)/dd)
        dummy_codestr.section "010" <| fun () ->
            let f(x:num0) = asm.pow(asm.abs(asm.dSum (1,4) <| fun n -> n*x),2)
            !"代数微分"
            dy <== asm.diff (f x) x
            !"数値微分"
            y1 <== f x
            y2 <== f (x+dd)
            print.cc dy ((y2-y1)/dd)
        dummy_codestr.section "011" <| fun () ->
            ch.dddz <| fun (y1,y2r,y2i,dy) -> ch.z <| fun z ->
                z <== -1.8+asm.uj*3.5
                let f(x:num0) = asm.pow(asm.abs(asm.zSum (1,4) <| fun n -> n*x),2)
                !"代数微分"
                dy <== asm.diff (f z) z
                !"数値微分"
                y1 <== f z
                y2r <== f (z+dd)
                y2i <== f (z+dd*asm.uj)
                print.ccc y1 y2r y2i
                print.cc dy ((y2r-y1)/dd+asm.uj*(y2i-y1)/dd)
        dummy_codestr.section "012" <| fun () ->
            ch.d1 10 <| fun ar ->
                ar.foreach <| fun i -> ar[i] <== i
                let f(a:num1) = (asm.dSum (0,9) <| fun n -> n*n*a[n])/asm.sqrt(asm.dSum (0,9) <| fun n -> n*a[n])
                ar.foreach <| fun i ->
                    !"代数微分"
                    dy <== asm.diff (f ar) ar[i]
                    !"数値微分"
                    y1 <== f ar
                    ar[i] <== ar[i] + dd
                    y2 <== f ar
                    ar[i] <== ar[i] - dd
                    print.ccc i dy ((y2-y1)/dd)
        codestr.section "013" <| fun () ->
            ch.d1 10 <| fun ar ->
                ar.foreach <| fun i -> ar[i] <== i
                let f(a:num1) = 
                    asm.dLet (asm.dSum (0,9) <| fun n -> n*n) (fun x -> (asm.dSum (0,9) <| fun n -> n*n*a[n])/x)
                ar.foreach <| fun i ->
                    !"代数微分"
                    dy <== asm.diff (f ar) ar[i]
                    !"数値微分"
                    y1 <== f ar
                    ar[i] <== ar[i] + dd
                    y2 <== f ar
                    ar[i] <== ar[i] - dd
                    print.ccc i dy ((y2-y1)/dd)