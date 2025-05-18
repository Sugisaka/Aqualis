//#############################################################################
// sample
let projectname = "test8"
let version = "0.0.0"
//#############################################################################

let outputdir = __SOURCE_DIRECTORY__ + @"\result"

#I @"..\bin\Debug\net8.0"
#r "Aqualis.dll"

open Aqualis

// 実数特異値分解
Compile [Fortran;C99;Python] outputdir (projectname+"_svd1") ("aaa","aaa") <| fun () ->
    ch.d2 2 4 <| fun mat ->
    ch.i <| fun ns ->
        br.if2  (mat.size1.<mat.size2) 
        <| fun () -> 
            ns <== mat.size1 
        <| fun () -> 
            ns <== mat.size2
        mat[0,0] <== 1
        mat[0,1] <== 2
        mat[0,2] <== 3
        mat[0,3] <== 4
        mat[1,0] <== 5
        mat[1,1] <== 4
        mat[1,2] <== 3
        mat[1,3] <== 2
        ch.d1 ns <| fun s -> 
        ch.d2 mat.size1 mat.size2 <| fun s' -> 
        ch.d2 mat.size1 mat.size1 <| fun u -> 
        ch.d2 mat.size2 mat.size2 <| fun vt -> 
            La.svd mat (u,s,vt)
            s'.clear()
            s.foreach <| fun i ->
                s'[i,i] <== s[i]
            La.matmul (u,s') <| fun us ->
                La.matmul (us,vt) <| fun usv ->
                    // 特異値分解：U * S * Vt = A
                    print.c usv
                    
// 実数一般化逆行列
Compile [Fortran;C99;Python] outputdir (projectname+"_inv1") ("aaa","aaa") <| fun () ->
    ch.d2 2 4 <| fun A ->
    ch.d2 2 4 <| fun A' ->
    ch.d2 4 2 <| fun B ->
        A[0,0] <== 1
        A[0,1] <== 2
        A[0,2] <== 3
        A[0,3] <== 4
        A[1,0] <== 5
        A[1,1] <== 4
        A[1,2] <== 3
        A[1,3] <== 2
        A' <== A
        La.inverse_matrix2 B A (D 1E-5)
        La.matmul(B,A') <| fun C ->
            La.matmul(A',C) <| fun D ->
                // 一般化逆行列の性質：A * B * A = A
                print.c D
                
// 複素特異値分解
Compile [Fortran;C99;Python] outputdir (projectname+"_svd2") ("aaa","aaa") <| fun () ->
    ch.z2 2 4 <| fun mat ->
    ch.i <| fun ns ->
        br.if2  (mat.size1.<mat.size2) 
        <| fun () -> 
            ns <== mat.size1 
        <| fun () -> 
            ns <== mat.size2
        mat[0,0] <== 1+asm.uj*5
        mat[0,1] <== 2+asm.uj*4
        mat[0,2] <== 3+asm.uj*3
        mat[0,3] <== 4+asm.uj*2
        mat[1,0] <== 5+asm.uj*1
        mat[1,1] <== 4+asm.uj*0
        mat[1,2] <== 3+asm.uj*1
        mat[1,3] <== 2+asm.uj*2
        ch.d1 ns <| fun s -> 
        ch.z2 mat.size1 mat.size2 <| fun s' -> 
        ch.z2 mat.size1 mat.size1 <| fun u -> 
        ch.z2 mat.size2 mat.size2 <| fun vt -> 
            La.svd mat (u,s,vt)
            s'.clear()
            s.foreach <| fun i ->
                s'[i,i] <== s[i]
            La.matmul (u,s') <| fun us ->
                La.matmul (us,vt) <| fun usv ->
                    // 特異値分解：U * S * Vt = A
                    print.c usv
                    
// 複素一般化逆行列
Compile [Fortran;C99;Python] outputdir (projectname+"_inv2") ("aaa","aaa") <| fun () ->
    ch.z2 2 4 <| fun A ->
    ch.z2 2 4 <| fun A' ->
    ch.z2 4 2 <| fun B ->
        A[0,0] <== 1+asm.uj*5
        A[0,1] <== 2+asm.uj*4
        A[0,2] <== 3+asm.uj*3
        A[0,3] <== 4+asm.uj*2
        A[1,0] <== 5+asm.uj*1
        A[1,1] <== 4+asm.uj*0
        A[1,2] <== 3+asm.uj*1
        A[1,3] <== 2+asm.uj*2
        A' <== A
        La.inverse_matrix2 B A (D 1E-5)
        La.matmul(B,A') <| fun C ->
            La.matmul(A',C) <| fun D ->
                // 一般化逆行列の性質：A * B * A = A
                print.c D
