//#############################################################################
// 数式フォーマットテスト
let projectname = "test2"
let version = "1.0.0"
//#############################################################################
 
let outputdir = __SOURCE_DIRECTORY__

#I @"..\..\bin\Debug\net10.0"
#r "Aqualis.dll"

open Aqualis
 
Compile [Fortran;C99;Python;LaTeX;HTML] outputdir projectname ("aaa","aaa") <| fun () ->
    AqualisCompiler.set_DebugMode OFF
    codestr.h2 "testA" <| fun () ->
        ch.i <| fun m ->
        ch.i <| fun n ->
        ch.d <| fun x ->
        ch.d <| fun y ->
            !"001"
            n <== 1
            m <== 2
            x <== 3.0
            y <== 4.0
            !"002"
            m <== n+1
            !"003"
            m <== m+n
            !"004"
            m <== (m+n)*(m+n)
            !"005"
            x <== (m+n)/(m+n)
            !"006"
            y <== x/y/y
            !"007"
            m <== m-n
            !"008"
            x <== m/n
            !"009"
            m <== m./n
            !"010"
            print.c n
            print.c n
            print.c (n+1)
            !"011"
            n <== -1
            !"012"
            let t = -n+1
            m <== -n+1
            !"013"
            m <== -m+n
            !"014"
            m <== -(m+n)*(m+n)
            !"015"
            x <== -(m+n)/(m+n)
            !"016"
            y <== -x/y/y
            !"017"
            m <== -m-n
            !"018"
            x <== -m/n
            !"019"
            m <== -m./n
            !"020"
            x <== (4*m)/2
            !"021"
            m <== (4*m)./2
            !"022"
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
    codestr.h2 "testB" <| fun () ->
        ch.d1 10 <| fun x1 ->
        ch.d1 10 <| fun y1 ->
        ch.d1 10 <| fun z1 ->
            !"021"
            z1 <== x1 + y1
            !"022"
            z1[(1,3)] <== x1[(4,6)] + y1[(4,6)]
            !"023"
            z1 <== x1 * (x1 + y1)
        ch.d2 10 5 <| fun x2 ->
        ch.d2 10 5 <| fun y2 ->
        ch.d2 10 5 <| fun z2 ->
            !"024"
            z2 <== x2 + y2
            !"025"
            z2[(_1,_3),3] <== x2[(_4,_6),3] + y2[(_4,_6),3]
            !"026"
            z2 <== x2 * (x2 + y2)
        !"027"
        ch.ii <| fun (n,m) ->
            n <== 1
        !"028"
        ch.iidd <| fun (n,m,x,y) ->
            n <== 1
            m <== 1
            m <== 4*n*2
            !"aaa"
            x <== 4*n/2
            !"bbb"
            m <== 4*n./2
            !"ccc"
            //m <== 4*n/2
    codestr.h2 "testC" <| fun () ->
        ch.id <| fun (p,q) ->
            !"001"
            p <== p+p
            !"002"
            p <== p-p
            !"003"
            q <== p/p
            !"004"
            p <== p./p
            !"005"
            q <== (p+q)+(p+q)
            !"006"
            q <== (p+p)+(p+p)
            !"007"
            q <== (p+q)-(p+q)
            !"008"
            q <== (p+p)-(p+p)
            !"009"
            q <== (p+q)/(p+q)
            !"010"
            q <== (p+p)/(p+p)
            !"011"
            q <== (p+p)./(p+p)
    codestr.h2 "testD" <| fun () ->
        ch.dd <| fun (x,y) ->
            !"001"
            print.t "001"
            x <== 1
            y <== asm.sin x
            print.c y
            y <== asm.cos x
            print.c y
            y <== asm.tan x
            print.c y
            y <== asm.exp x
            print.c y
            y <== asm.log x
            print.c y
            y <== asm.log10 x
            print.c y
            y <== asm.sqrt x
            print.c y
            y <== asm.asin x
            print.c y
            y <== asm.acos x
            print.c y
            y <== asm.atan x
            print.c y
            y <== asm.atan2(x,y)
            print.c y
        ch.ddd <| fun (x,x2,y) ->
            !"002"
            print.t "002"
            x <== 0.28
            y <== asm.sin x
            print.c y
            y <== asm.cos x
            print.c y
            y <== asm.tan x
            print.c y
            y <== asm.exp x
            print.c y
            y <== asm.log x
            print.c y
            y <== asm.log10 x
            print.c y
            y <== asm.sqrt x
            print.c y
            y <== asm.asin x
            print.c y
            y <== asm.acos x
            print.c y
            y <== asm.atan x
            print.c y
            y <== asm.atan2(x,x2)
            print.c y
            y <== asm.floor x
            print.c y
            y <== asm.ceil x
            print.c y
        ch.zz <| fun (x,y) ->
            !"003"
            print.t "003"
            x <== 1.0-asm.uj*2.0
            y <== asm.sin x
            print.c y
            y <== asm.cos x
            print.c y
            y <== asm.tan x
            print.c y
            y <== asm.exp x
            print.c y
            y <== asm.log x
            print.c y
            y <== asm.sqrt x
            print.c y
            y <== asm.asin x
            print.c y
            y <== asm.acos x
            print.c y
            y <== asm.atan x
            print.c y