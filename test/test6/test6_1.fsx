//#############################################################################
// 微分演算テスト
let projectname = "test6_1"
let version = "1.0.0"
//#############################################################################
 
let outputdir = __SOURCE_DIRECTORY__

#I @"..\..\bin\Debug\net10.0"
#r "Aqualis.dll"

open Aqualis

Compile [Fortran;C99;Python] outputdir projectname "aaa" <| fun ctx ->
    let dd = 1E-5
    ctx.ch.dddd <| fun (x,y1,y2,dy) ->
    ctx.ch.d <| fun c1 ->
        x <== -10
        dummy_group.section "001" <| fun () ->
            let f (x:double0) = x+2+c1
            ctx.comment "代数微分"
            dy <== asm.diff (f x, x)
            ctx.comment "数値微分"
            y1 <== f x
            y2 <== f (x+dd)
            ctx.print.tt <| dy ++ (y2-y1)/dd
        dummy_group.section "002" <| fun () ->
            let f (x:double0) = 5*x+2
            ctx.comment "代数微分"
            dy <== asm.diff (f x, x)
            ctx.comment "数値微分"
            y1 <== f x
            y2 <== f (x+dd)
            ctx.print.tt <| dy ++ (y2-y1)/dd
        dummy_group.section "003" <| fun () ->
            let f (x:double0) = 5*x*x+2
            ctx.comment "代数微分"
            dy <== asm.diff (f x, x)
            ctx.comment "数値微分"
            y1 <== f x
            y2 <== f (x+dd)
            ctx.print.tt <| dy++((y2-y1)/dd)
        dummy_group.section "004" <| fun () ->
            let f(x:double0) = (x+1)/(2*x+7)
            ctx.comment "代数微分"
            dy <== asm.diff (f x, x)
            ctx.comment "数値微分"
            y1 <== f x
            y2 <== f (x+dd)
            ctx.print.tt <| dy ++ (y2-y1)/dd
        dummy_group.section "005" <| fun () ->
            let f(x:double0) = x*asm.cos(x*x+2*x+1)
            ctx.comment "代数微分"
            dy <== asm.diff (f x, x)
            ctx.comment "数値微分"
            y1 <== f x
            y2 <== f (x+dd)
            ctx.print.tt <| dy ++ (y2-y1)/dd
        dummy_group.section "006" <| fun () ->
            let f(x:double0) = asm.dSum (1,4) <| fun n -> n*x
            ctx.comment "代数微分"
            dy <== asm.diff (f x, x)
            ctx.comment "数値微分"
            y1 <== f x
            y2 <== f (x+dd)
            ctx.print.tt <| dy ++ (y2-y1)/dd
        dummy_group.section "007" <| fun () ->
            let f(x:double0) = (asm.dSum (1,4) <| fun n -> n*x)+(asm.dSum (1,4) <| fun n -> n*x)
            ctx.comment "代数微分"
            dy <== asm.diff (f x, x)
            ctx.comment "数値微分"
            y1 <== f x
            y2 <== f (x+dd)
            ctx.print.tt <| dy ++ (y2-y1)/dd
        dummy_group.section "008" <| fun () ->
            let f(x:double0) = (asm.dSum (1,4) <| fun n -> n*x*x)+(asm.dSum (1,10) <| fun n -> n*x)
            ctx.comment "代数微分"
            dy <== asm.diff (f x, x)
            ctx.comment "数値微分"
            y1 <== f x
            y2 <== f (x+dd)
            ctx.print.tt <| dy ++ (y2-y1)/dd
        dummy_group.section "009" <| fun () ->
            let f(x:double0) = asm.exp(asm.dSum (1,4) <| fun n -> n*x/100)
            ctx.comment "代数微分"
            dy <== asm.diff (f x, x)
            ctx.comment "数値微分"
            y1 <== f x
            y2 <== f (x+dd)
            ctx.print.tt <| dy ++ (y2-y1)/dd
        dummy_group.section "010" <| fun () ->
            let f(x:double0) = asm.pow(asm.abs(asm.dSum (1,4) <| fun n -> n*x),2)
            ctx.comment "代数微分"
            dy <== asm.diff (f x, x)
            ctx.comment "数値微分"
            y1 <== f x
            y2 <== f (x+dd)
            ctx.print.tt <| dy++((y2-y1)/dd)
        dummy_group.section "011" <| fun () ->
            ctx.ch.dddz <| fun (y1,y2r,y2i,dy) -> ctx.ch.z <| fun z ->
                z <== -1.8+asm.uj*3.5
                let f(x:complex0) = asm.pow(asm.abs(asm.zSum (1,4) <| fun n -> n*x),2)
                ctx.comment "代数微分"
                dy <== asm.diff (f z, z)
                ctx.comment "数値微分"
                y1 <== f z
                y2r <== f (z+dd)
                y2i <== f (z+dd*asm.uj)
                ctx.print.tt <| y1++y2r++y2i
                ctx.print.tt <| dy ++ ((y2r-y1)/dd+asm.uj*(y2i-y1)/dd)
        group.section "012" <| fun () ->
            ctx.ch.d1 10 <| fun ar ->
                ar.foreach <| fun i -> ar[i] <== i
                let f(a:double1) = (asm.dSum (0,9) <| fun n -> n*n*a[n])/asm.sqrt(asm.dSum (0,9) <| fun n -> n*a[n])
                ar.foreach <| fun i ->
                    ctx.comment "代数微分"
                    dy <== asm.diff (f ar, ar[i])
                    ctx.comment "数値微分"
                    y1 <== f ar
                    ar[i] <== ar[i] + dd
                    y2 <== f ar
                    ar[i] <== ar[i] - dd
                    ctx.print.tt <| i ++ dy ++ (y2-y1)/dd
        dummy_group.section "013" <| fun () ->
            ctx.ch.d1 10 <| fun ar ->
                ar.foreach <| fun i -> ar[i] <== i
                let f(a:double1) = 
                    asm.dLet (asm.dSum (0,9) <| fun n -> asm.todouble(n*n)) (fun x -> (asm.dSum (0,9) <| fun n -> n*n*a[n])/x)
                ar.foreach <| fun i ->
                    ctx.comment "代数微分"
                    dy <== asm.diff (f ar, ar[i])
                    ctx.comment "数値微分"
                    y1 <== f ar
                    ar[i] <== ar[i] + dd
                    y2 <== f ar
                    ar[i] <== ar[i] - dd
                    ctx.print.tt <| i ++ dy ++ (y2-y1)/dd
