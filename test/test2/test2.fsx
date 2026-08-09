//#############################################################################
// 数式フォーマットテスト
let projectname = "test2"
let version = "1.0.0"
//#############################################################################
 
let outputdir = __SOURCE_DIRECTORY__

#I @"..\..\bin\Debug\net10.0"
#r "Aqualis.dll"

open Aqualis
 
Compile [Fortran;C99;Python;LaTeX;HTML] outputdir projectname "aaa" <| fun ctx ->
    ctx.Debug.setDebugMode false
    ctx.group.h2 "testA" <| fun () ->
        ctx.ch.i <| fun m ->
        ctx.ch.i <| fun n ->
        ctx.ch.d <| fun x ->
        ctx.ch.d <| fun y ->
            ctx.comment "001"
            n <== 1
            m <== 2
            x <== 3.0
            y <== 4.0
            ctx.comment "002"
            m <== n+1
            ctx.comment "003"
            m <== m+n
            ctx.comment "004"
            m <== (m+n)*(m+n)
            ctx.comment "005"
            x <== (m+n)/(m+n)
            ctx.comment "006"
            y <== x/y/y
            ctx.comment "007"
            m <== m-n
            ctx.comment "008"
            x <== m/n
            ctx.comment "009"
            m <== m./n
            ctx.comment "010"
            ctx.print.t n
            ctx.print.t n
            ctx.print.t (n+1)
            ctx.comment "011"
            n <== -1
            ctx.comment "012"
            let t = -n+1
            m <== -n+1
            ctx.comment "013"
            m <== -m+n
            ctx.comment "014"
            m <== -(m+n)*(m+n)
            ctx.comment "015"
            x <== -(m+n)/(m+n)
            ctx.comment "016"
            y <== -x/y/y
            ctx.comment "017"
            m <== -m-n
            ctx.comment "018"
            x <== -m/n
            ctx.comment "019"
            m <== -m./n
            ctx.comment "020"
            x <== (4*m)/2
            ctx.comment "021"
            m <== (4*m)./2
            ctx.comment "022"
            x <== 0.745
            y <== asm.sin(-(x+y)/x)
            y <== asm.cos x
            y <== asm.tan x
            y <== asm.exp x
            y <== asm.log x
            y <== asm.log10 x
            y <== asm.sqrt x
            y <== asm.asin x
            y <== asm.acos x
            y <== asm.atan x
            y <== asm.atan2(x,y)
            y <== asm.floor x
            y <== asm.ceil x
    ctx.group.h2 "testB" <| fun () ->
       ctx.ch.d1 10 <| fun x1 ->
       ctx.ch.d1 10 <| fun y1 ->
       ctx.ch.d1 10 <| fun z1 ->
            ctx.comment "021"
            z1 <== x1 + y1
            ctx.comment "022"
            z1[(1,3)] <== x1[(4,6)] + y1[(4,6)]
            ctx.comment "023"
            z1 <== x1 * (x1 + y1)
       ctx.ch.d2 (10, 5) <| fun x2 ->
       ctx.ch.d2 (10, 5) <| fun y2 ->
       ctx.ch.d2 (10, 5) <| fun z2 ->
            ctx.comment "024"
            z2 <== x2 + y2
            ctx.comment "025"
            z2[(_1,_3),3] <== x2[(_4,_6),3] + y2[(_4,_6),3]
            ctx.comment "026"
            z2 <== x2 * (x2 + y2)
       ctx.comment "027"
       ctx.ch.ii <| fun (n,m) ->
            n <== 1
       ctx.comment "028"
       ctx.ch.iidd <| fun (n,m,x,y) ->
            n <== 1
            m <== 1
            m <== 4*n*2
            ctx.comment "aaa"
            x <== 4*n/2
            ctx.comment "bbb"
            m <== 4*n./2
            ctx.comment "ccc"
            //m <== 4*n/2
    ctx.group.h2 "testC" <| fun () ->
       ctx.ch.id <| fun (p,q) ->
            ctx.comment "001"
            p <== p+p
            ctx.comment "002"
            p <== p-p
            ctx.comment "003"
            q <== p/p
            ctx.comment "004"
            p <== p./p
            ctx.comment "005"
            q <== (p+q)+(p+q)
            ctx.comment "006"
            q <== (p+p)+(p+p)
            ctx.comment "007"
            q <== (p+q)-(p+q)
            ctx.comment "008"
            q <== (p+p)-(p+p)
            ctx.comment "009"
            q <== (p+q)/(p+q)
            ctx.comment "010"
            q <== (p+p)/(p+p)
            ctx.comment "011"
            q <== (p+p)./(p+p)
    ctx.group.h2 "testD" <| fun () ->
       ctx.ch.dd <| fun (x,y) ->
            ctx.comment "001"
            ctx.print.s "001"
            x <== 1
            y <== asm.sin x
            ctx.print.t y
            y <== asm.cos x
            ctx.print.t y
            y <== asm.tan x
            ctx.print.t y
            y <== asm.exp x
            ctx.print.t y
            y <== asm.log x
            ctx.print.t y
            y <== asm.log10 x
            ctx.print.t y
            y <== asm.sqrt x
            ctx.print.t y
            y <== asm.asin x
            ctx.print.t y
            y <== asm.acos x
            ctx.print.t y
            y <== asm.atan x
            ctx.print.t y
            y <== asm.atan2(x,y)
            ctx.print.t y
       ctx.ch.ddd <| fun (x,x2,y) ->
            ctx.comment "002"
            ctx.print.s "002"
            x <== 0.28
            y <== asm.sin x
            ctx.print.t y
            y <== asm.cos x
            ctx.print.t y
            y <== asm.tan x
            ctx.print.t y
            y <== asm.exp x
            ctx.print.t y
            y <== asm.log x
            ctx.print.t y
            y <== asm.log10 x
            ctx.print.t y
            y <== asm.sqrt x
            ctx.print.t y
            y <== asm.asin x
            ctx.print.t y
            y <== asm.acos x
            ctx.print.t y
            y <== asm.atan x
            ctx.print.t y
            y <== asm.atan2(x,x2)
            ctx.print.t y
            y <== asm.floor x
            ctx.print.t y
            y <== asm.ceil x
            ctx.print.t y
       ctx.ch.zz <| fun (x,y) ->
            ctx.comment "003"
            ctx.print.s "003"
            x <== 1.0-asm.uj*2.0
            y <== asm.sin x
            ctx.print.t y
            y <== asm.cos x
            ctx.print.t y
            y <== asm.tan x
            ctx.print.t y
            y <== asm.exp x
            ctx.print.t y
            y <== asm.log x
            ctx.print.t y
            y <== asm.sqrt x
            ctx.print.t y
            y <== asm.asin x
            ctx.print.t y
            y <== asm.acos x
            ctx.print.t y
            y <== asm.atan x
            ctx.print.t y