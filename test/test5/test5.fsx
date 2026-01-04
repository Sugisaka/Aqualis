//#############################################################################
// ファイル入出力テスト
let projectname = "test5"
let version = "1.0.0"
//#############################################################################
 
let outputdir = __SOURCE_DIRECTORY__

#I @"..\..\bin\Debug\net10.0"
#r "Aqualis.dll"

open Aqualis
 
Compile [Fortran;C99;Python] outputdir (projectname+"tw") ("aaa","aaa") <| fun () ->
    ch.iidz <| fun (n,m,x,z) ->
        n <== 1
        m <== 2
        x <== 1.234
        z <== 5.6+asm.uj*7.8
        io.fileOutput ("testA"++n++"_"++m++".dat") <| fun wr ->
            wr [n; n+1; n+2;]
            wr [n+3; n+4; n+5]
            wr [x;z]
            wr [asm.sin x;asm.exp z;]
            wr [x+asm.cos x;]
            wr [z.abs;z.pow]
            
Compile [Fortran;C99;Python] outputdir (projectname+"tr") ("aaa","aaa") <| fun () ->
    ch.iidz <| fun (n,m,x,z) ->
        n <== 1
        m <== 2
        x <== 1.234
        z <== 5.6+asm.uj*7.8
        io.fileInput ("testA"++n++"_"++m++".dat") <| fun rd ->
            ch.iiii <| fun (n1,n2,n3,n4) ->
            ch.ddz <| fun (x1,x2,z1) ->
            rd [n1; n2; n3;]
            print.cc n1 n
            print.cc n2 (n+1)
            print.cc n3 (n+2)
            rd [n1; n2; n3;]
            print.cc n1 (n+3)
            print.cc n2 (n+4)
            print.cc n3 (n+5)
            rd [x1;z1]
            print.cc x x1
            print.cc z z1
            rd [x1;z1]
            print.cc <| asm.sin(x) <| x1
            print.cc <| asm.exp(z) <| z1
            rd [x1]
            print.cc <| x+asm.cos(x) <| x1
            rd [x1;x2]
            print.cc z.abs x1
            print.cc z.pow x2

Compile [Fortran;C99;Python] outputdir (projectname+"bw") ("aaa","aaa") <| fun () ->
    ch.iidz <| fun (n,m,x,z) ->
        n <== 1
        m <== 2
        x <== 1.234
        z <== 5.6+asm.uj*7.8
        io.binfileOutput ("testA"++n++"_"++m++".bin") <| fun wr ->
            wr n
            wr (n+1)
            wr (n+2)
            wr (n+3)
            wr (n+4)
            wr (n+5)
            wr x
            wr z
            wr <| asm.sin x
            wr <| asm.exp z
            wr <| x+asm.cos x
            wr z.abs
            wr z.pow
            
Compile [Fortran;C99;Python] outputdir (projectname+"br") ("aaa","aaa") <| fun () ->
    ch.iidz <| fun (n,m,x,z) ->
        n <== 1
        m <== 2
        x <== 1.234
        z <== 5.6+asm.uj*7.8
        io.binfileInput ("testA"++n++"_"++m++".bin") <| fun rd ->
            ch.iiii <| fun (n1,n2,n3,n4) ->
            ch.ddz <| fun (x1,x2,z1) ->
            rd n1
            rd n2
            rd n3
            print.cc n1 n
            print.cc n2 (n+1)
            print.cc n3 (n+2)
            rd n1
            rd n2
            rd n3
            print.cc n1 (n+3)
            print.cc n2 (n+4)
            print.cc n3 (n+5)
            rd x1
            rd z1
            print.cc x x1
            print.cc z z1
            rd x1
            rd z1
            print.cc <| asm.sin(x) <| x1
            print.cc <| asm.exp(z) <| z1
            rd x1
            print.cc <| x+asm.cos(x) <| x1
            rd x1
            rd x2
            print.cc z.abs x1
            print.cc z.pow x2
