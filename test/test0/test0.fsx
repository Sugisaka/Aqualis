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

group.section 1 step <| fun () ->
    
    let res1 =
        asm.dLet 2 <| fun x ->
            asm.dLet 3 <| fun y ->
                asm.dSum (1, 4) <| fun z -> (x+y)*z
                
    let aqualis  = new AqualisBuilder<num0>()
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
    
group.section 2 step <| fun () ->
    Compile [Fortran;C99;Python;HTML;LaTeX;] outputdir projectname ("aaa","bbb") <| fun () ->
        let x = var.i0 "x"
        let y = var.d0 "y"
        let z = var.z0 "z"
        x <== 1
        y <== asm.pi
        z <== asm.uj
        ch.i <| fun z ->
            ch.I "i" <| fun x ->
                ch.I "i" <| fun y ->
                    iter.num 10 <| fun i ->
                        x <== z + x + y 
                    iter.num 10 <| fun i ->
                        x <== z + x + y
                    iter.num 10 <| fun i ->
                        x <== z + x + y                        
        ch.i <| fun x ->
            ch.i <| fun y ->
                ch.i <| fun z ->
                    x <== 1
                    br.if1 (Or [x .< y .< z; z .< 1]) <| fun () ->
                        x <== 0
                    y <== 2
                    z <== 3
                    print.c x
                    print.ccc x y z
                    print.t <| "aaa"
                    print.n [x; y; z]
                    print.w <| x++y++z
                    print.w <| x++"aaa"++y++"bbb"++z
                    
        io.fileOutput "test.dat" <| fun wr ->
            ch.z <| fun z ->
                z <== 1+asm.uj*2
                wr [z]

        io.fileInput "test.dat" <| fun rd ->
            ch.z <| fun z ->
                rd [z]
                print.c z
        ch.i1 10 <| fun x ->
            x[0] <== 0
            
group.section 3 step <| fun () ->
    Compile [Fortran;C99;Python;LaTeX;HTML] outputdir projectname ("aaa","bbb") <| fun () ->
        let x = var.d0 "x"
        let y = var.d0 "y"
        x <== asm.pi
        !"test"
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
