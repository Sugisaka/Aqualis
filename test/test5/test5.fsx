//#############################################################################
// ファイル入出力テスト
let projectname = "test5"
let version = "1.0.0"
//#############################################################################
 
let outputdir = __SOURCE_DIRECTORY__

#I @"..\..\bin\Debug\net10.0"
#r "Aqualis.dll"

open Aqualis
 
Compile [Fortran;C99;Python] outputdir (projectname+"tw") ("aaa","aaa") <| fun ctx ->
    ctx.ch.iidz <| fun (n,m,x,z) ->
        n <== 1
        m <== 2
        x <== 1.234
        z <== 5.6+asm.uj*7.8
        ctx.io.fileOutput ("testA"++n++"_"++m++".dat") <| fun wr ->
            wr.tt <| n++(n+1)++(n+2)
            wr.tt <| (n+3)++(n+4)++(n+5)
            wr.tt <| x++z
            wr.tt <| asm.sin x++asm.exp z
            wr.tt <| x++asm.cos x
            wr.tt <| z.abs++z.pow
            
Compile [Fortran;C99;Python] outputdir (projectname+"tr") ("aaa","aaa") <| fun ctx ->
    ctx.ch.iidz <| fun (n,m,x,z) ->
        n <== 1
        m <== 2
        x <== 1.234
        z <== 5.6+asm.uj*7.8
        ctx.io.fileInput ("testA"++n++"_"++m++".dat") <| fun rd ->
            ctx.ch.iiii <| fun (n1,n2,n3,n4) ->
            ctx.ch.ddz <| fun (x1,x2,z1) ->
            rd.tt <| n1++n2++n3
            ctx.print.tt <| n1++n
            ctx.print.tt <| n2++(n+1)
            ctx.print.tt <| n3++(n+2)
            rd.tt <| n1++n2++n3
            ctx.print.tt <| n1++(n+3)
            ctx.print.tt <| n2++(n+4)
            ctx.print.tt <| n3++(n+5)
            rd.tt <| x1++z1
            ctx.print.tt <| x++x1
            ctx.print.tt <| z++z1
            rd.tt <| x1++z1
            ctx.print.tt <| asm.sin(x)++x1
            ctx.print.tt <| asm.exp(z)++z1
            rd.t x1
            ctx.print.tt <| x+asm.cos(x)++x1
            rd.tt <| x1++x2
            ctx.print.tt <| z.abs++x1
            ctx.print.tt <| z.pow++x2
            
Compile [Fortran;C99;Python] outputdir (projectname+"bw") ("aaa","aaa") <| fun ctx ->
    ctx.ch.iidz <| fun (n,m,x,z) ->
        n <== 1
        m <== 2
        x <== 1.234
        z <== 5.6+asm.uj*7.8
        ctx.io.binfileOutput ("testA"++n++"_"++m++".bin") <| fun wr ->
            wr.b n
            wr.b (n+1)
            wr.b (n+2)
            wr.b (n+3)
            wr.b (n+4)
            wr.b (n+5)
            wr.b x
            wr.b z
            wr.b (asm.sin x)
            wr.b (asm.exp z)
            wr.b (x+asm.cos x)
            wr.b z.abs
            wr.b z.pow
            
Compile [Fortran;C99;Python] outputdir (projectname+"br") ("aaa","aaa") <| fun ctx ->
    ctx.ch.iidz <| fun (n,m,x,z) ->
        n <== 1
        m <== 2
        x <== 1.234
        z <== 5.6+asm.uj*7.8
        ctx.io.binfileInput ("testA"++n++"_"++m++".bin") <| fun rd ->
            ctx.ch.iiii <| fun (n1,n2,n3,n4) ->
            ctx.ch.ddz <| fun (x1,x2,z1) ->
            rd.b n1
            rd.b n2
            rd.b n3
            ctx.print.tt <| n1++n
            ctx.print.tt <| n2++(n+1)
            ctx.print.tt <| n3++(n+2)
            rd.b n1
            rd.b n2
            rd.b n3
            ctx.print.tt <| n1++(n+3)
            ctx.print.tt <| n2++(n+4)
            ctx.print.tt <| n3++(n+5)
            rd.b x1
            rd.b z1
            ctx.print.tt <| x++x1
            ctx.print.tt <| z++z1
            rd.b x1
            rd.b z1
            ctx.print.tt <| asm.sin x ++ x1
            ctx.print.tt <| asm.exp z ++ z1
            rd.b x1
            ctx.print.tt <| x+asm.cos x ++ x1
            rd.b x1
            rd.b x2
            ctx.print.tt <| z.abs++x1
            ctx.print.tt <| z.pow++x2
