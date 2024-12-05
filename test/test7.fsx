//#############################################################################
// project title
let projectname = "test7"
let version = "1.0.0"
//#############################################################################
 
let outputdir = @"C:\home\kitahara\Aqualis\test\result"

#I @"C:\home\kitahara\Aqualis\bin\Debug\net8.0"
#r "Aqualis.dll"
 
open Aqualis

Compile [F;C;P] outputdir projectname ("aaa","aaa") <| fun () ->
    let dd = 1E-7
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
            let f(x:num0) = asm.sum 1 4 <| fun n -> n*x
            !"代数微分"
            dy <== asm.diff (f x) x
            !"数値微分"
            y1 <== f x
            y2 <== f (x+dd)
            print.cc dy ((y2-y1)/dd)
        dummy_codestr.section "007" <| fun () ->
            let f(x:num0) = (asm.sum 1 4 <| fun n -> n*x)+(asm.sum 1 4 <| fun n -> n*x)
            !"代数微分"
            dy <== asm.diff (f x) x
            !"数値微分"
            y1 <== f x
            y2 <== f (x+dd)
            print.cc dy ((y2-y1)/dd)
        dummy_codestr.section "008" <| fun () ->
            let f(x:num0) = (asm.sum 1 4 <| fun n -> n*x)+(asm.sum 1 5 <| fun n -> n*x)
            !"代数微分"
            dy <== asm.diff (f x) x
            !"数値微分"
            y1 <== f x
            y2 <== f (x+dd)
            print.cc dy ((y2-y1)/dd)
        dummy_codestr.section "009" <| fun () ->
            let f(x:num0) = (asm.sum 1 4 <| fun n -> n*x*x)+(asm.sum 1 10 <| fun n -> n*x)
            !"代数微分"
            dy <== asm.diff (f x) x
            !"数値微分"
            y1 <== f x
            y2 <== f (x+dd)
            print.cc dy ((y2-y1)/dd)
        dummy_codestr.section "010" <| fun () ->
            let f(x:num0) = asm.exp(asm.sum 1 4 <| fun n -> n*x/100)
            !"代数微分"
            dy <== asm.diff (f x) x
            !"数値微分"
            y1 <== f x
            y2 <== f (x+dd)
            print.cc dy ((y2-y1)/dd)
        dummy_codestr.section "011" <| fun () ->
            let f(x:num0) = asm.abs(asm.sum 1 4 <| fun n -> n*x)
            !"代数微分"
            dy <== asm.diff (f x) x
            !"数値微分"
            y1 <== f x
            y2 <== f (x+dd)
            print.cc dy ((y2-y1)/dd)
        dummy_codestr.section "012" <| fun () ->
            let f(x:num0) = asm.pow(asm.abs(asm.sum 1 4 <| fun n -> n*x),2)
            !"代数微分"
            dy <== asm.diff (f x) x
            !"数値微分"
            y1 <== f x
            y2 <== f (x+dd)
            print.cc dy ((y2-y1)/dd)
        dummy_codestr.section "013" <| fun () ->
            ch.dddz <| fun (y1,y2r,y2i,dy) -> ch.z <| fun z ->
                z <== -1.8+asm.uj*3.5
                let f(x:num0) = asm.pow(asm.abs(asm.sum 1 4 <| fun n -> n*x),2)
                !"代数微分"
                dy <== asm.diff (f z) z
                !"数値微分"
                y1 <== f z
                y2r <== f (z+dd)
                y2i <== f (z+dd*asm.uj)
                print.cc dy ((y2r-y1)/dd+asm.uj*(y2i-y1)/dd)
        codestr.section "014a" <| fun () ->
            ch.d1 10 <| fun ar ->
                ar.foreach <| fun i -> ar[i] <== i
                let f(a:num1) = (asm.sum 1 10 <| fun n -> n*n*a[n])/asm.sqrt(asm.sum 1 10 <| fun n -> n*a[n])
                (f ar).eval()
                ar.foreach <| fun i ->
                    !"代数微分"
                    dy <== asm.diff (f ar) ar[i]
                    !"数値微分"
                    y1 <== f ar
                    ar[i] <== ar[i] + dd
                    (f ar).eval()
                    y2 <== f ar
                    ar[i] <== ar[i] - dd
                    (f ar).eval()
                    print.ccc i dy ((y2-y1)/dd)
        codestr.section "014b" <| fun () ->
            ch.d1 10 <| fun ar ->
                let q1 = var.d0("tmp1")
                let q2 = var.d0("tmp2")
                ar.foreach <| fun i -> ar[i] <== i
                let f(a:num1) = asm.xlet(q1,asm.sum 1 10 <| fun n -> n*n*a[n])/asm.sqrt(asm.xlet(q2,asm.sum 1 10 <| fun n -> n*a[n]))
                (f ar).eval()
                ar.foreach <| fun i ->
                    !"代数微分"
                    dy <== asm.diff (f ar) ar[i]
                    !"数値微分"
                    y1 <== f ar
                    ar[i] <== ar[i] + dd
                    (f ar).eval()
                    y2 <== f ar
                    ar[i] <== ar[i] - dd
                    (f ar).eval()
                    print.ccc i dy ((y2-y1)/dd)