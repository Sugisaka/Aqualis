//#############################################################################
// 基本演算テスト
let projectname = "test0"
let version = "1.0.0"
//#############################################################################
 
let outputdir = __SOURCE_DIRECTORY__

#I @"..\..\bin\Debug\net10.0"
#r "Aqualis.dll"
 
open Aqualis

let step = 2

group.section (step,1) <| fun () ->
    
    let res1 =
        asm.dLet 2 <| fun x ->
            asm.dLet 3 <| fun y ->
                asm.dSum (1, 4) <| fun z -> (x+y)*z
                
    let aqualis  = new AqualisBuilder<double0>()
    let res2 = aqualis{
        let! x = asm.dLet 2
        let! y = asm.dLet 3
        let  s = asm.dSum (1, 4) <| fun z -> (x+y)*z
        return s}
        
    printfn "--- Direct expression ----------------------"
    printfn "%s" <| res1.Expr.eval().ToString()
    printfn "--- Monad ----------------------------------"
    printfn "%s" <| (res2 id).Expr.eval().ToString()
    printfn "--------------------------------------------"
    
group.section (step,2) <| fun () ->
    Compile [Fortran;C99;Python;HTML;LaTeX;] outputdir projectname "bbb" <| fun c ->
        let x = c.var.i0 "x"
        let y = c.var.d0 "y"
        let z = c.var.z0 "z"
        x <== 1
        y <== asm.pi
        z <== asm.uj
        c.ch.i <| fun z ->
            c.ch.I "i" <| fun x ->
                c.ch.I "i" <| fun y -> // Variable i is already in use; using i0002 instead.
                    c.iter.num 10 <| fun i ->
                        x <== z + x + y 
                    c.iter.num 10 <| fun i ->
                        x <== z + x + y
                    c.iter.num 10 <| fun i ->
                        x <== z + x + y                        
        c.ch.i <| fun x ->
            c.ch.i <| fun y ->
                c.ch.i <| fun z ->
                    x <== 1
                    c.br.if1 (Or [x .< y .< z; z .< 1]) <| fun () ->
                        x <== 0
                    y <== 2
                    z <== 3
                    c.print.t x
                    c.print.s "aaa"
                    c.print.tt <| x++y++z
                    c.print.tt <| x++"aaa"++y++"bbb"++z
                    
        c.io.fileOutput "test.dat" <| fun wr ->
            c.ch.z <| fun z ->
                z <== 1+asm.uj*2
                wr.t z

        c.io.fileInput "test.dat" <| fun rd ->
            c.ch.z <| fun z ->
                rd.t z
                c.print.t z
        c.ch.i1 10 <| fun x ->
            x[0] <== 0
            
group.section (step,3) <| fun () ->
    Compile [Fortran;C99;Python;LaTeX;HTML] outputdir projectname "bbb" <| fun ctx ->
        let x = ctx.var.d0 "x"
        let y = ctx.var.d0 "y"
        x <== asm.pi
        ctx.emit.comment "test"
        y <== 1
        ctx.ch.d <| fun a ->
        ctx.ch.d1 10 <| fun b ->
        ctx.ch.z2 (10, 20) <| fun c ->
            a <== asm.sin x
            ctx.iter.num b.size1 <| fun i ->
                b[i] <== b[i]/b.size1
            ctx.iter.num c.size1 <| fun i ->
            ctx.iter.num c.size2 <| fun j ->
                c[i,j] <== c[i,j]/(c.size1*c.size2)
                c[i,j] <== (c[i,j]+x*a/y)/(c.size1*c.size2)
