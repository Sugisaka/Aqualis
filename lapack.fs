(*
Copyright (c) 2022 Jun-ichiro Sugisaka

This software is released under the MIT License.
http://opensource.org/licenses/mit-license.php
*)
namespace Aqualis

    open Aqualis_base
    
    type La() =
        
        /// <summary>
        /// 行列×ベクトルの計算
        /// </summary>
        /// <param name="x">a×b</param>
        /// <param name="a">a</param>
        /// <param name="b">b</param>
        static member matmul (x:num1,a:num2,b:num1) =
            x.clear()
            iter.num a.size1 <| fun i ->
                iter.num a.size2 <| fun j ->
                    x.[i] <== x.[i] + a.[i,j] * b.[j]                      
                    
        /// <summary>
        /// 行列×ベクトルの計算
        /// </summary>
        /// <param name="a">行列</param>
        /// <param name="b">ベクトル</param>
        static member matmul (a:num2,b:num1) = fun code ->
            ch.n1 (a.etype,a.size1) <| fun x ->
                La.matmul (x,a,b)
                code x
            
        /// <summary>
        /// 行列×行列の計算
        /// </summary>
        /// <param name="u">計算結果</param>
        static member matmul (u:num2,a:num2,b:num2) =
            u.clear()
            iter.num a.size1 <| fun i ->
                iter.num b.size2 <| fun j ->
                    iter.num a.size2 <| fun k ->
                        u.[i,j] <== u.[i,j] + a.[i,k] * b.[k,j]

        /// <summary>
        /// 行列a×行列bの計算
        /// </summary>
        /// <param name="a">a</param>
        /// <param name="b">b</param>
        /// <param name="code">a×bに対する処理</param>
        static member matmul (a:num2,b:num2) = fun code ->
            ch.n2 (a.etype,a.size1,b.size2) <| fun u ->
                La.matmul (u,a,b)
                code u
            
        /// <summary>
        /// ベクトルの内積計算
        /// </summary>
        /// <param name="x">a・b</param>
        /// <param name="a">a</param>
        /// <param name="b">b</param>
        static member dot (x:num0,a:num1,b:num1) =
            tbinder.d a <| fun () ->
                x.clear()
                iter.num a.size1 <| fun j ->
                    x <== x + a.[j] * b.[j]
            tbinder.z a <| fun () ->
                x.clear()
                iter.num a.size1 <| fun j ->
                    x <== x + asm.conj(a.[j]) * b.[j]
            
        /// <summary>
        /// ベクトルの内積計算
        /// </summary>
        /// <param name="a"></param>
        /// <param name="b"></param>
        /// <param name="code">a・bに対する処理</param>
        static member dot (a:num1,b:num1) = fun code ->
            ch.n a <| fun x ->
                La.dot (x,a,b)
                code x
            
        /// <summary>
        /// ベクトルのノルム(L2ノルム)計算
        /// </summary>
        /// <param name="a">a</param>
        /// <param name="code">ノルムaに対する処理</param>
        static member norm (a:num1) = fun code ->
            tbinder.d a <| fun () ->
                La.dot (a,a) <| fun b -> code(asm.sqrt(b))
            tbinder.z a <| fun () ->
                La.dot (a,a) <| fun b -> code(asm.sqrt(b.re))
            
        /// <summary>
        /// ベクトルの規格化
        /// </summary>
        /// <param name="a"></param>
        static member normalize (a:num1) =
            La.norm a <| fun c ->
                a <== a/c
                
        ///<summary>連立方程式の求解</summary>
        ///<param name="matrix">係数行列</param>
        ///<param name="y">定数項ベクトル→解ベクトル</param>
        static member solve_simuleq (matrix:num2,y:num1) =
            p.option("-llapack")
            p.option("-lblas")
            codestr.section "連立方程式の求解" <| fun () ->
                ch.iii <| fun (N,b,info) ->
                    N <== matrix.size1
                    b <== 1
                    ch.i1 N <| fun ipiv ->
                    tbinder.z matrix <| fun () ->
                        match p.lang with
                        |F -> 
                            p.codewrite("call zgesv("+N.code+","+b.code+","+matrix.code+","+N.code+","+ipiv.code+","+y.code+","+N.code+","+info.code+")"+"\n")
                        |C -> 
                            p.extn("void zgesv_(int *n, int *nrhs, double complex *a, int *lda, int *ipiv, double complex *b, int *ldb, int *info)")
                            p.codewrite("zgesv_(&"+N.code+","+"&"+b.code+","+matrix.code+",&"+N.code+","+ipiv.code+","+y.code+",&"+N.code+",&"+info.code+")"+";\n")
                        |T -> 
                            p.codewrite("$"+y.code+" \\leftarrow "+matrix.code+"^{-1}"+y.code+"$$\\\\\n")
                        |H -> 
                            p.codewrite("\\("+y.code+" \\leftarrow "+matrix.code+"^{-1}"+y.code+"\\)<br/>\n")
                    tbinder.d matrix <| fun () ->
                        match p.lang with
                        |F -> 
                            p.codewrite("call dgesv("+N.code+","+b.code+","+matrix.code+","+N.code+","+ipiv.code+","+y.code+","+N.code+","+info.code+")"+"\n")
                        |C -> 
                            p.extn("void dgesv_(int *n, int *nrhs, double *a, int *lda, int *ipiv, double *b, int *ldb, int *info)")
                            p.codewrite("dgesv_(&"+N.code+","+"&"+b.code+","+matrix.code+",&"+N.code+","+ipiv.code+","+y.code+",&"+N.code+",&"+info.code+")"+";\n")
                        |T -> 
                            p.codewrite("$"+y.code+" \\leftarrow "+matrix.code+"^{-1}"+y.code+"$\\\\\n")
                        |H -> 
                            p.codewrite("\\("+y.code+" \\leftarrow "+matrix.code+"^{-1}"+y.code+"\\)<br/>\n")
                            
        ///<summary>連立方程式の求解</summary>
        ///<param name="matrix">係数行列</param>
        ///<param name="y">定数項ベクトルを列方向に並べた配列→解ベクトルを列方向に並べた配列</param>
        static member solve_simuleqs (matrix:num2,y:num2) =
            p.option("-llapack")
            p.option("-lblas")
            codestr.section "連立方程式の求解" <| fun () ->
                ch.iii <| fun (N,b,info) ->
                    N <== matrix.size1
                    b <== y.size2
                    ch.i1 N <| fun ipiv ->
                        tbinder.z matrix <| fun () ->
                            match p.lang with
                            |F -> 
                                p.codewrite("call zgesv("+N.code+","+b.code+","+matrix.code+","+N.code+","+ipiv.code+","+y.code+","+N.code+","+info.code+")"+"\n")
                            |C -> 
                                p.extn("void zgesv_(int *n, int *nrhs, double complex *a, int *lda, int *ipiv, double complex *b, int *ldb, int *info)")
                                p.codewrite("zgesv_(&"+N.code+","+"&"+b.code+","+matrix.code+",&"+N.code+","+ipiv.code+","+y.code+",&"+N.code+",&"+info.code+")"+";\n")
                            |T -> 
                                p.codewrite("$"+y.code+" \\leftarrow "+matrix.code+"^{-1}"+y.code+"$\\\\\n")
                            |H -> 
                                p.codewrite("\\("+y.code+" \\leftarrow "+matrix.code+"^{-1}"+y.code+"\\)<br/>\n")
                        tbinder.d matrix <| fun () ->
                            match p.lang with
                            |F -> 
                                p.codewrite("call dgesv("+N.code+","+b.code+","+matrix.code+","+N.code+","+ipiv.code+","+y.code+","+N.code+","+info.code+")"+"\n")
                            |C -> 
                                p.extn("void dgesv_(int *n, int *nrhs, double *a, int *lda, int *ipiv, double *b, int *ldb, int *info)")
                                p.codewrite("dgesv_(&"+N.code+","+"&"+b.code+","+matrix.code+",&"+N.code+","+ipiv.code+","+y.code+",&"+N.code+",&"+info.code+")"+";\n")
                            |T -> 
                                p.codewrite("$"+y.code+" \\leftarrow "+matrix.code+"^{-1}"+y.code+"$\\\\\n")
                            |H -> 
                                p.codewrite("\\("+y.code+" \\leftarrow "+matrix.code+"^{-1}"+y.code+"\\)<br/>\n")
                                
        ///<summary>逆行列の計算</summary>
        ///<param name="mat1">元の行列</param>
        ///<param name="mat2">mat1の逆行列</param>
        static member inverse_matrix (mat2:num2) (mat1:num2) =
            p.option("-llapack")
            p.option("-lblas")
            codestr.section "逆行列の計算" <| fun () ->
                mat2.clear()
                iter.num mat1.size1 <| fun i -> mat2.[i,i] <== 1.0
                ch.ii <| fun (npre,info) ->
                    npre<==mat1.size1
                    ch.i1 npre <| fun ipiv ->
                        ipiv.clear()
                        tbinder.z mat2 <| fun () ->
                            match p.lang with
                            |F -> 
                                p.codewrite("call zgesv("+npre.code+", "+npre.code+","+mat1.code+", "+npre.code+", "+ipiv.code+","+mat2.code+", "+npre.code+", "+info.code+")")
                            |C -> 
                                p.extn("void zgesv_(int *n, int *nrhs, double complex *a, int *lda, int *ipiv, double complex *b, int *ldb, int *info)")
                                p.codewrite("zgesv_(&"+npre.code+","+"&"+npre.code+", "+mat1.code+", &"+npre.code+", "+ipiv.code+", *"+mat2.code+", &"+npre.code+", &"+info.code+");")
                            |T -> 
                                p.codewrite("$"+mat2.code+" \\leftarrow "+mat1.code+"^{-1}"+"$"+"\\\\\n")
                            |H -> 
                                p.codewrite("\\("+mat2.code+" \\leftarrow "+mat1.code+"^{-1}"+"\\)"+"<br/>\n")
                        tbinder.d mat2 <| fun () ->
                            match p.lang with
                            |F -> 
                                p.codewrite("call dgesv("+npre.code+", "+npre.code+","+mat1.code+", "+npre.code+", "+ipiv.code+","+mat2.code+", "+npre.code+", "+info.code+")")
                            |C -> 
                                p.extn("void dgesv_(int *n, int *nrhs, double *a, int *lda, int *ipiv, double *b, int *ldb, int *info)")
                                p.codewrite("dgesv_(&"+npre.code+","+"&"+npre.code+", "+mat1.code+", &"+npre.code+", "+ipiv.code+", *"+mat2.code+", &"+npre.code+", &"+info.code+");")
                            |T -> 
                                p.codewrite("$"+mat2.code+" \\leftarrow "+mat1.code+"^{-1}"+"$"+"\\\\\n")
                            |H -> 
                                p.codewrite("\\("+mat2.code+" \\leftarrow "+mat1.code+"^{-1}"+"\\)"+"<br/>\n")
                        br.if1 (info .=/ 0) <| fun () -> print.s[!.("InvMatrix Info: ");info]
                        
        ///<summary>行列の階数</summary>
        ///<param name="rank">行列matの階数</param>
        ///<param name="mat">行列</param>
        ///<param name="cond">0とみなす上限値</param>
        static member rank (rank:num0) (mat:num2) (cond:num0) =
            p.option("-llapack")
            p.option("-lblas")
            codestr.section "行列の階数" <| fun () ->
                ch.iii <| fun (npre,info,lwork) ->
                    npre<==mat.size1
                    ch.i1 npre <| fun ipiv -> ch.d1 npre <| fun s -> ch.z2 npre npre <| fun u -> ch.z2 npre npre <| fun vt -> ch.d1 (5*npre) <| fun rwork -> ch.i1 (8*npre) <| fun iwork -> 
                        ipiv.clear()
                        lwork <== 2*npre+npre
                        tbinder.z mat <| fun () ->
                            ch.z1 lwork <| fun work ->
                            //特異値分解を利用
                            match p.lang with
                            |F -> 
                                p.codewrite("call zgesdd('N', " + 
                                    npre.code + "," + " " + 
                                    npre.code + ","  + 
                                    mat.code+", "  + 
                                    npre.code + ", "  + 
                                    s.code + ","   + 
                                    u.code + ", "  + 
                                    npre.code + ","   + 
                                    vt.code + ", "  + 
                                    npre.code + ", "  + 
                                    work.code + ", "  + 
                                    lwork.code + ","   + 
                                    rwork.code + ","   + 
                                    iwork.code + ", "  + 
                                    info.code + ")")
                            |C -> 
                                p.extn("void zgesdd_(char jobz, int m, int n, double complex *a, int lda, double *s, double complex *u, int ldu, double complex *vt, int ldvt, double complex *work, int lwork, double *rwork, int *iwork, int info)")
                                p.codewrite("zgesdd_('N', &" +  
                                    npre.code + "," + "&" + 
                                    npre.code + ", " + 
                                    mat.code+", &" + 
                                    npre.code + ", *" + 
                                    s.code + ", *" + 
                                    u.code + ", &" + 
                                    npre.code + ", *" + 
                                    vt.code + ", &" + 
                                    npre.code + ", &" + 
                                    work.code + ", &" + 
                                    lwork.code + ", *" + 
                                    rwork.code + ", *" + 
                                    iwork.code + ", &" + 
                                    info.code + ");")
                            |T -> 
                                p.codewrite("\\("+rank.code+" \\leftarrow "+"\\mathrm{rank}\\left["+mat.code+"\\right]"+"$\\\\\n")
                            |H -> 
                                p.codewrite("\\("+rank.code+" \\leftarrow "+"\\mathrm{rank}\\left["+mat.code+"\\right]"+"\\)<br/>\n")
                        tbinder.d mat <| fun () ->
                            ch.d1 lwork <| fun work ->
                            //特異値分解を利用
                            match p.lang with
                            |F -> 
                                p.codewrite("call dgesdd('N', " + 
                                    npre.code + "," + " " + 
                                    npre.code + ","  + 
                                    mat.code+", "  + 
                                    npre.code + ", "  + 
                                    s.code + ","   + 
                                    u.code + ", "  + 
                                    npre.code + ","   + 
                                    vt.code + ", "  + 
                                    npre.code + ", "  + 
                                    work.code + ", "  + 
                                    lwork.code + ","   + 
                                    rwork.code + ","   + 
                                    iwork.code + ", "  + 
                                    info.code + ")")
                            |C -> 
                                p.extn("void dgesdd_(char jobz, int m, int n, double *a, int lda, double *s, double *u, int ldu, double *vt, int ldvt, double *work, int lwork, int *iwork, int info)")
                                p.codewrite("dgesdd_('N', &" +  
                                    npre.code + "," + "&" + 
                                    npre.code + ", " + 
                                    mat.code+", &" + 
                                    npre.code + ", *" + 
                                    s.code + ", *" + 
                                    u.code + ", &" + 
                                    npre.code + ", *" + 
                                    vt.code + ", &" + 
                                    npre.code + ", &" + 
                                    work.code + ", &" + 
                                    lwork.code + ", *" + 
                                    rwork.code + ", *" + 
                                    iwork.code + ", &" + 
                                    info.code + ");")
                            |T -> 
                                p.codewrite("\\("+rank.code+" \\leftarrow "+"\\mathrm{rank}\\left["+mat.code+"\\right]"+"$\\\\\n")
                            |H -> 
                                p.codewrite("\\("+rank.code+" \\leftarrow "+"\\mathrm{rank}\\left["+mat.code+"\\right]"+"\\)<br/>\n")
                        br.if1 (info .=/ 0) <| fun () -> print.s[!.("rank Info: ");info]
                        rank.clear()
                        iter.array s <| fun i -> 
                            br.if1 (s.[i] .> cond) <| fun () -> rank.inc()
                            
        ///<summary>疑似逆行列の計算</summary>
        ///<param name="mat2">matの疑似逆行列</param>
        ///<param name="mat">行列</param>
        ///<param name="cond">0とみなす上限値</param>
        static member inverse_matrix2 (mat2:num2) (mat:num2) (cond:num0) =
            codestr.section "疑似逆行列" <| fun () ->
                ch.iii <| fun (npre,info,lwork) ->
                    npre<==mat.size1
                    ch.i1 npre <| fun ipiv -> ch.d1 npre <| fun s -> ch.z2 npre npre <| fun u -> ch.z2 npre npre <| fun u2 -> ch.z2 npre npre <| fun vt -> ch.d1 (npre*(5*npre+7)) <| fun rwork -> ch.i1 (8*npre) <| fun iwork -> 
                        ipiv.clear()
                        lwork <== npre*npre+2*npre+npre
                        La.svd mat (u,s,vt)
                        //特異値分解した行列をもとに、疑似逆行列は (v^*)×(s^-1)×(u^*)
                        iter.array s <| fun i -> 
                            iter.array s <| fun j -> 
                                br.if2 (s.[i] .> cond)
                                    (fun () ->
                                    u2.[i,j]<==(asm.conj(u.[j,i])/s.[i]))
                                    (fun () ->
                                    //condより小さい特異値は無視
                                    u2.[i,j].clear())
                        mat2.clear()
                        iter.array s <| fun i -> 
                            iter.array s <| fun j ->
                                iter.array s <| fun p ->
                                    mat2.[i,j] <== mat2.[i,j] + asm.conj(vt.[p,i])*u2.[p,j]
                                    
        /// <summary>
        /// Ax=λxの固有値λと固有ベクトルxを計算
        /// </summary>
        /// <param name="eigenvalues">固有値</param>
        /// <param name="eigenvectors">固有ベクトル</param>
        /// <param name="mat1">複素非対称行列</param>
        static member eigen_matrix (eigenvalues:num1,eigenvectors:num2) (mat1:num2) =
            p.option("-llapack")
            p.option("-lblas")
            tbinder.z mat1 <| fun () ->
                codestr.section "非対称複素行列の固有値" <| fun () ->
                    eigenvectors.clear()
                    ch.iii <| fun (npre,ldvldummy,info) ->
                            npre<==mat1.size1
                            ch.z2 _1 _1 <| fun dummy ->
                                ch.i <| fun lwork ->
                                    lwork <== 2*npre
                                    ch.z1 lwork <| fun work ->
                                        ch.d1 (2*npre) <| fun rwork ->
                                            eigenvalues.clear()
                                            ldvldummy <== 1
                                            match p.lang with
                                            |F -> 
                                                p.codewrite("call zgeev('No left vectors', 'Vectors (right)', "    + 
                                                    npre.code + ", "   + 
                                                    mat1.code + ", "  + 
                                                    npre.code + ", "  + 
                                                    eigenvalues.code + ","   + 
                                                    dummy.code + ",  " + 
                                                    ldvldummy.code + ", "  + 
                                                    eigenvectors.code + ", "  + 
                                                    npre.code + ", "  + 
                                                    work.code + ", "  + 
                                                    lwork.code + ", "  + 
                                                    rwork.code + ", "  + 
                                                    info.code + ")")
                                            |C -> 
                                                p.extn("void zgeev_(char *, char *, int *, double complex *, int *, double complex *, double complex *, int *, double complex *, int *, double complex *, int *, double *, int *)")
                                                p.codewrite("zgeev_('N', 'V', &" + 
                                                    npre.code + ", "  + 
                                                    mat1.code + ", &" + 
                                                    npre.code + ", " + 
                                                    eigenvalues.code + ", " + 
                                                    dummy.code + ", &" + 
                                                    ldvldummy.code + ", " + 
                                                    eigenvectors.code + ", &" + 
                                                    npre.code + ", " + 
                                                    work.code + ", &" + 
                                                    lwork.code + ", " + 
                                                    rwork.code + ", &" + 
                                                    info.code + ");")
                                            |T -> 
                                                p.codewrite("Solve: $"+mat1.code+eigenvectors.code+" = "+eigenvalues.code+eigenvectors.code+"$"+"<br/>\n")
                                            |H -> 
                                                p.codewrite("Solve: \\("+mat1.code+eigenvectors.code+" = "+eigenvalues.code+eigenvectors.code+"\\)"+"<br/>\n")
                                            br.if1 (info .=/ 0) <| fun () -> print.s[!.("Eigenvalue Info: ");info]
            tbinder.d mat1 <| fun () ->
                codestr.section "非対称実行列の固有値" <| fun () ->
                    eigenvectors.clear()
                    ch.d1 eigenvectors.size1 <| fun eigenvalues_re ->
                    ch.d1 eigenvectors.size1 <| fun eigenvalues_im ->
                    ch.iii <| fun (npre,ldvldummy,info) ->
                            npre<==mat1.size1
                            ch.d2 _1 _1 <| fun dummy ->
                                ch.i <| fun lwork ->
                                    lwork <== 4*npre
                                    ch.d1 lwork <| fun work ->
                                            eigenvalues.clear()
                                            ldvldummy <== 1
                                            match p.lang with
                                            |F -> 
                                                p.codewrite("call dgeev('No left vectors', 'Vectors (right)', "    + 
                                                    npre.code + ", "   + 
                                                    mat1.code + ", "  + 
                                                    npre.code + ", "  + 
                                                    eigenvalues_re.code + ", "  + 
                                                    eigenvalues_im.code + ","   + 
                                                    dummy.code + ",  " + 
                                                    ldvldummy.code + ", "  + 
                                                    eigenvectors.code + ", "  + 
                                                    npre.code + ", "  + 
                                                    work.code + ", "  + 
                                                    lwork.code + ", " + 
                                                    info.code + ")")
                                            |C -> 
                                                p.extn("void dgeev_(char *, char *, int *, double *, int *, double *, double *, int *, double *, int *, double *, int *, double *, int *)")
                                                p.codewrite("dgeev_('N', 'V', &" + 
                                                    npre.code + ", "  + 
                                                    mat1.code + ", &" + 
                                                    npre.code + ", " + 
                                                    eigenvalues_re.code + ", " + 
                                                    eigenvalues_im.code + ", " + 
                                                    dummy.code + ", &" + 
                                                    ldvldummy.code + ", " + 
                                                    eigenvectors.code + ", &" + 
                                                    npre.code + ", " + 
                                                    work.code + ", &" + 
                                                    lwork.code + ", &" + 
                                                    info.code + ");")
                                            |T -> 
                                                p.codewrite("Solve: $"+mat1.code+eigenvectors.code+" = "+eigenvalues.code+eigenvectors.code+"$"+"<br/>\n")
                                            |H -> 
                                                p.codewrite("Solve: \\("+mat1.code+eigenvectors.code+" = "+eigenvalues.code+eigenvectors.code+"\\)"+"<br/>\n")
                                            br.if1 (info .=/ 0) <| fun () -> print.s[!.("Eigenvalue Info: ");info]
                            eigenvalues.foreach <| fun i -> eigenvalues[i] <== eigenvalues_re[i] + asm.uj * eigenvalues_im[i]
                            
        /// <summary>
        /// Ax=λBxの固有値λと固有ベクトルxを計算
        /// </summary>
        /// <param name="eigenvalues1">λ1(λ=λ1/λ2)</param>
        /// <param name="eigenvalues2">λ2(λ=λ1/λ2)</param>
        /// <param name="eigenvectors">固有ベクトルx</param>
        /// <param name="mat1">行列A</param>
        /// <param name="mat2">行列B</param>
        static member eigen_matrix2 (eigenvalues1:num1,eigenvalues2:num1,eigenvectors:num2) (mat1:num2) (mat2:num2) =
            tbinder.z mat1 <| fun () ->
                codestr.section "非対称複素行列の一般化固有値" <| fun () ->
                    p.option("-llapack")
                    p.option("-lblas")
                    eigenvectors.clear()
                    ch.iii <| fun (npre,ldvldummy,info) ->
                        npre<==mat1.size1
                        ch.z2 _1 _1 <| fun dummy ->
                            ch.i <| fun lwork ->
                                lwork <== npre + 64 * npre
                                ch.z1 lwork <| fun work ->
                                    ch.d1 (8*npre) <| fun rwork ->
                                        eigenvalues1.clear()
                                        eigenvalues2.clear()
                                        ldvldummy <== 1
                                        match p.lang with
                                        |F -> 
                                            p.codewrite("call zggev("+
                                                "'N'" + ", " +
                                                "'V'" + ", " + 
                                                npre.code + ", " + 
                                                mat1.code + ", " + 
                                                npre.code + ", " + 
                                                mat2.code + ", " + 
                                                npre.code + ", " + 
                                                eigenvalues1.code + ", " + 
                                                eigenvalues2.code + "," + 
                                                dummy.code + ",  " + 
                                                ldvldummy.code + ", " +
                                                eigenvectors.code + ", " + 
                                                npre.code + ", "  + 
                                                work.code + ", "  + 
                                                lwork.code + ", "  + 
                                                rwork.code + ", "  + 
                                                info.code + ")")
                                        |C -> 
                                            p.extn("void zggev_(char *, char *, int *, double complex *, int *, double complex *, int *, double complex *, double complex *, double complex *, int *, double complex *, int *, double complex *, int *, double *, int *)")
                                            p.codewrite("zggev_(" +
                                                "'N'" + ", " +
                                                "'V'" + ", " +
                                                "&" + npre.code + ", " +
                                                mat1.code + ", " +
                                                "&" + npre.code + ", " +
                                                mat2.code + ", " +
                                                "&" + npre.code + ", " +
                                                eigenvalues1.code + ", " +
                                                eigenvalues2.code + ", " +
                                                dummy.code + ", "+
                                                "&" + ldvldummy.code + ", " +
                                                eigenvectors.code + ", "+
                                                "&" + npre.code + ", " +
                                                work.code + ", " +
                                                "&" + lwork.code + ", " +
                                                rwork.code + ", " +
                                                "&" + info.code + ");")
                                        |T -> 
                                            p.codewrite("Solve: $"+mat1.code+eigenvectors.code+" = "+"\\frac{"+eigenvalues1.code+"}{"+eigenvalues2.code+"}"+mat2.code+eigenvectors.code+"$\\\\\n")
                                        |H -> 
                                            p.codewrite("Solve: \\("+mat1.code+eigenvectors.code+" = "+"\\frac{"+eigenvalues1.code+"}{"+eigenvalues2.code+"}"+mat2.code+eigenvectors.code+"\\)<br/>\n")
                                        br.if1 (info .=/ 0) <| fun () -> print.s[!.("Eigenvalue Info: ");info]
            tbinder.d mat1 <| fun () ->
                codestr.section "非対称複素行列の一般化固有値" <| fun () ->
                    p.option("-llapack")
                    p.option("-lblas")
                    eigenvectors.clear()
                    ch.d1 eigenvalues1.size1 <| fun eigenvalues1re ->
                    ch.d1 eigenvalues1.size1 <| fun eigenvalues1im ->
                    ch.iii <| fun (npre,ldvldummy,info) ->
                        npre<==mat1.size1
                        ch.d2 _1 _1 <| fun dummy ->
                            ch.i <| fun lwork ->
                                lwork <== npre + 64 * npre
                                ch.d1 lwork <| fun work ->
                                    eigenvalues1re.clear()
                                    eigenvalues1im.clear()
                                    eigenvalues2.clear()
                                    ldvldummy <== 1
                                    match p.lang with
                                    |F -> 
                                        p.codewrite("call dggev("+
                                            "'N'" + ", " +
                                            "'V'" + ", " + 
                                            npre.code + ", " + 
                                            mat1.code + ", " + 
                                            npre.code + ", " + 
                                            mat2.code + ", " + 
                                            npre.code + ", " + 
                                            eigenvalues1re.code + ", " + 
                                            eigenvalues1im.code + ", " + 
                                            eigenvalues2.code + "," + 
                                            dummy.code + ",  " + 
                                            ldvldummy.code + ", " +
                                            eigenvectors.code + ", " + 
                                            npre.code + ", "  + 
                                            work.code + ", "  + 
                                            lwork.code + ", "  + 
                                            info.code + ")")
                                    |C -> 
                                        p.extn("void dggev_(char *, char *, int *, double complex *, int *, double complex *, int *, double complex *, double complex *, double complex *, int *, double complex *, int *, double complex *, int *, double *, int *)")
                                        p.codewrite("dggev_(" +
                                            "'N'" + ", " +
                                            "'V'" + ", " +
                                            "&" + npre.code + ", " +
                                            mat1.code + ", " +
                                            "&" + npre.code + ", " +
                                            mat2.code + ", " +
                                            "&" + npre.code + ", " +
                                            eigenvalues1re.code + ", " +
                                            eigenvalues1im.code + ", " +
                                            eigenvalues2.code + ", " +
                                            dummy.code + ", "+
                                            "&" + ldvldummy.code + ", " +
                                            eigenvectors.code + ", "+
                                            "&" + npre.code + ", " +
                                            work.code + ", " +
                                            "&" + lwork.code + ", " +
                                            "&" + info.code + ");")
                                    |T -> 
                                        p.codewrite("Solve: $"+mat1.code+eigenvectors.code+" = "+"\\frac{"+eigenvalues1.code+"}{"+eigenvalues2.code+"}"+mat2.code+eigenvectors.code+"$\\\\\n")
                                    |H -> 
                                        p.codewrite("Solve: \\("+mat1.code+eigenvectors.code+" = "+"\\frac{"+eigenvalues1.code+"}{"+eigenvalues2.code+"}"+mat2.code+eigenvectors.code+"\\)<br/>\n")
                                    br.if1 (info .=/ 0) <| fun () -> print.s[!.("Eigenvalue Info: ");info]
                        iter.num eigenvalues1.size1 <| fun i ->
                            eigenvalues1.[i] <== eigenvalues1re.[i] + asm.uj + eigenvalues1im.[i]
                                        
        /// <summary>
        /// 連立方程式の求解(Tikhonovの正則化法)
        /// </summary>
        /// <param name="fu_mat">係数行列</param>
        /// <param name="fu_cst">定数項ベクトル</param>
        /// <param name="code">解に対して行う処理</param>
        static member solve_simuleq_t(fu_mat:num2,fu_cst:num1) code =
            codestr.h2 "連立方程式の求解(Tikhonovの正則化法)" <| fun () ->
                ch.d2 fu_mat.size2 fu_mat.size2 <| fun FF ->
                ch.d1 fu_mat.size2 <| fun bb ->
                    let lambda = 1E-6 //正則化パラメータ
                    FF.clear()
                    //FF = fu_mat^T * fu_mat
                    ch.d <| fun tmp ->
                        iter.num FF.size1 <| fun i ->
                            iter.num FF.size2 <| fun j ->
                                tmp.clear()
                                iter.num fu_mat.size1 <| fun k ->
                                    tmp <== tmp + fu_mat.[k,i]*fu_mat.[k,j]
                                FF.[i,j] <== tmp
                                
                    //FF = fu_mat^T * fu_mat + λ^2 * I
                    iter.num FF.size1 <| fun i ->
                            FF.[i,i] <== FF.[i,i] + lambda * lambda
                    //bb = fu_mat^T * fu_cst
                    ch.d <| fun tmp ->
                        bb.clear()
                        iter.num bb.size1 <| fun i ->
                            tmp.clear()
                            iter.num fu_cst.size1 <| fun k ->
                                tmp <== tmp + fu_mat.[k,i]*fu_cst.[k]
                            bb.[i] <== tmp
                    La.solve_simuleq(FF,bb)
                    code(bb)
                    
        /// <summary>
        /// 連立方程式の求解(Tikhonovの正則化法)
        /// </summary>
        /// <param name="fu_mat">係数行列</param>
        /// <param name="fu_cst">定数項ベクトル</param>
        /// <param name="lambda">正則化パラメータ</param>
        /// <param name="code">解に対して行う処理</param>
        static member solve_simuleq_tt(fu_mat:num2,fu_cst:num1,lambda:double) code =
            codestr.h2 "連立方程式の求解(Tikhonovの正則化法)" <| fun () ->
                match fu_mat.etype with
                |Zt ->
                    ch.z2 fu_mat.size2 fu_mat.size2 <| fun FF ->
                    ch.z1 fu_mat.size2 <| fun bb ->
                        //let lambda = 1E-6 //正則化パラメータ
                        FF.clear()
                        //FF = fu_mat^T * fu_mat
                        ch.z <| fun tmp ->
                            iter.num FF.size1 <| fun i ->
                                iter.num FF.size2 <| fun j ->
                                    tmp.clear()
                                    iter.num fu_mat.size1 <| fun k ->
                                        tmp <== tmp + asm.conj(fu_mat.[k,i])*fu_mat.[k,j]
                                    FF.[i,j] <== tmp
                        //FF = fu_mat^T * fu_mat + λ^2 * I
                        iter.num FF.size1 <| fun i ->
                                FF.[i,i] <== FF.[i,i] + lambda * lambda
                        //bb = fu_mat^T * fu_cst
                        ch.z <| fun tmp ->
                            bb.clear()
                            iter.num bb.size1 <| fun i ->
                                tmp.clear()
                                iter.num fu_cst.size1 <| fun k ->
                                    tmp <== tmp + asm.conj(fu_mat.[k,i])*fu_cst.[k]
                                bb.[i] <== tmp
                        La.solve_simuleq(FF,bb)
                        code(bb)
                |_ ->
                    ch.d2 fu_mat.size2 fu_mat.size2 <| fun FF ->
                    ch.d1 fu_mat.size2 <| fun bb ->
                        //let lambda = 1E-6 //正則化パラメータ
                        FF.clear()
                        //FF = fu_mat^T * fu_mat
                        ch.d <| fun tmp ->
                            iter.num FF.size1 <| fun i ->
                                iter.num FF.size2 <| fun j ->
                                    tmp.clear()
                                    iter.num fu_mat.size1 <| fun k ->
                                        tmp <== tmp + fu_mat.[k,i]*fu_mat.[k,j]
                                    FF.[i,j] <== tmp
                        //FF = fu_mat^T * fu_mat + λ^2 * I
                        iter.num FF.size1 <| fun i ->
                                FF.[i,i] <== FF.[i,i] + lambda * lambda
                        //bb = fu_mat^T * fu_cst
                        ch.d <| fun tmp ->
                            bb.clear()
                            iter.num bb.size1 <| fun i ->
                                tmp.clear()
                                iter.num fu_cst.size1 <| fun k ->
                                    tmp <== tmp + fu_mat.[k,i]*fu_cst.[k]
                                bb.[i] <== tmp
                        La.solve_simuleq(FF,bb)
                        code(bb)
                        
        /// <summary>
        /// 連立方程式の求解(Tikhonovの正則化法)
        /// </summary>
        /// <param name="fu_mat">係数行列</param>
        /// <param name="fu_cst">定数項ベクトル(列サイズ=1)</param>
        /// <param name="lambda">正則化パラメータ</param>
        /// <param name="code">解に対して行う処理</param>
        static member solve_simuleq_tt2(fu_mat:num2,fu_cst:num2,lambda:num0) code =
            codestr.h2 "連立方程式の求解(Tikhonovの正則化法)" <| fun () ->
                ch.z2 fu_mat.size2 fu_mat.size2 <| fun FF ->
                ch.z1 fu_mat.size2 <| fun bb ->
                    FF.clear()
                    //FF = fu_mat^T * fu_mat
                    ch.z <| fun tmp ->
                        iter.num FF.size1 <| fun i ->
                            iter.num FF.size2 <| fun j ->
                                tmp.clear()
                                iter.num fu_mat.size1 <| fun k ->
                                    tmp <== tmp + asm.conj(fu_mat.[k,i])*fu_mat.[k,j]
                                FF.[i,j] <== tmp
                    //FF = fu_mat^T * fu_mat + λ^2 * I
                    iter.num FF.size1 <| fun i ->
                            FF.[i,i] <== FF.[i,i] + lambda * lambda
                    //bb = fu_mat^T * fu_cst
                    ch.z <| fun tmp ->
                        bb.clear()
                        iter.num bb.size1 <| fun i ->
                            tmp.clear()
                            iter.num fu_cst.size1 <| fun k ->
                                tmp <== tmp + asm.conj(fu_mat.[k,i])*fu_cst.[k,1]
                            bb.[i] <== tmp
                    La.solve_simuleq(FF,bb)
                    code(bb)
                    
        /// <summary>
        /// 行列式の常用対数を計算
        /// </summary>
        /// <param name="matrix">行列</param>
        /// <param name="code">行列式の値を用いて実行するコード</param>
        static member determinant (matrix:num2) code =
            p.option("-llapack")
            p.option("-lblas")
            tbinder.z matrix <| fun () ->
                codestr.section "行列式の常用対数を計算" <| fun () ->
                    ch.iid <| fun (N,info,d) ->
                        N <== matrix.size1
                        ch.i1 N <| fun ipiv ->
                            match p.lang with
                            |F -> 
                                p.codewrite("call zgetrf("+N.code+","+N.code+","+matrix.code+","+N.code+","+ipiv.code+","+info.code+")"+"\n")
                            |C -> 
                                p.extn("void zgetrf_(int *, int *, double complex *, int *, int *, int *)")
                                p.codewrite("zgetrf_(&"+N.code+","+"&"+N.code+","+matrix.code+",&"+N.code+","+ipiv.code+",&"+info.code+")"+";\n")
                            |T -> 
                                p.codewrite("$"+d.code+" = "+"\\left|"+matrix.code+"\\right|"+"$"+"\\\\\n")
                            |H -> 
                                p.codewrite("\\("+d.code+" = "+"\\left|"+matrix.code+"\\right|"+"\\)"+"<br/>\n")
                        d.clear()
                        iter.num N <| fun i ->
                            d <== d + asm.log10(asm.abs(matrix.[i,i]))
                        code(d)
            tbinder.d matrix <| fun () ->
                codestr.section "行列式の常用対数を計算" <| fun () ->
                    ch.iid <| fun (N,info,d) ->
                        N <== matrix.size1
                        ch.i1 N <| fun ipiv ->
                            match p.lang with
                            |F -> 
                                p.codewrite("call dgetrf("+N.code+","+N.code+","+matrix.code+","+N.code+","+ipiv.code+","+info.code+")"+"\n")
                            |C -> 
                                p.extn("void dgetrf_(int *, int *, double complex *, int *, int *, int *)")
                                p.codewrite("dgetrf_(&"+N.code+","+"&"+N.code+","+matrix.code+",&"+N.code+","+ipiv.code+",&"+info.code+")"+";\n")
                            |T -> 
                                p.codewrite("$"+d.code+" = "+"\\left|"+matrix.code+"\\right|"+"$"+"\\\\\n")
                            |H -> 
                                p.codewrite("\\("+d.code+" = "+"\\left|"+matrix.code+"\\right|"+"\\)"+"<br/>\n")
                        d.clear()
                        iter.num N <| fun i ->
                            d <== d + asm.log10(asm.abs(matrix.[i,i]))
                        code(d)
                        
        /// <summary>
        /// mat = u * s * v に特異値分解
        /// </summary>
        /// <param name="mat1">複素行列</param>
        /// <param name="u">複素行列u</param>
        /// <param name="s">正方行列sの対角成分</param>
        /// <param name="vt">複素行列vの転置</param>
        static member svd (mat1:num2) (u:num2,s:num1,vt:num2) =
            p.option("-llapack")
            p.option("-lblas")
            match p.lang with
            |T ->
                p.codewrite("$"+mat1.code+" = "+u.code+s.code+vt.code+"^{\\mathrm{T}}"+"$\\\\\n")
            |H ->
                p.codewrite("\\("+mat1.code+" = "+u.code+s.code+vt.code+"^{\\mathrm{T}}"+"\\)<br/>\n")
            |_ ->
                tbinder.z mat1 <| fun () ->
                    codestr.section "非対称複素行列の特異値分解" <| fun () ->
                        s.clear()
                        u.clear()
                        vt.clear()
                        ch.iiii <| fun (m,n,lda,info) ->
                            m <== mat1.size1
                            n <== mat1.size2
                            ch.ii <| fun (ldu,ldvt) ->
                            ch.i <| fun lwork ->
                                ch.z01 <| fun work ->
                                ch.z2 1 1 <| fun dummy ->
                                ch.d1 (5*m) <| fun rwork ->
                                    lda <== m
                                    ldu <== u.size1
                                    ldvt <== vt.size2
                                    match p.lang with
                                    |F -> 
                                        lwork <== -1
                                        p.codewrite("call zgesvd("+
                                        "'A', "+
                                        "'A', " + 
                                        m.code + ", " + 
                                        n.code + ", " +
                                        mat1.code + ", " +
                                        lda.code + ", " +
                                        s.code+ ", " + 
                                        u.code + ", "  + 
                                        ldu.code + ", " + 
                                        vt.code + ", "  + 
                                        ldvt.code + ", " + 
                                        dummy.code + ", " + 
                                        lwork.code + ", " + 
                                        rwork.code + ", " + 
                                        info.code + ")")
                                        let nb = 64
                                        br.if2 (m+3*n+nb*(m+n) .> asm.toint(asm.floor(dummy[1,1].re+0.5)))
                                            <| fun () ->
                                                lwork <== m+3*n+nb*(m+n)
                                            <| fun () ->
                                                lwork <== asm.toint(asm.floor(dummy[1,1].re+0.5))
                                        work.allocate(lwork)
                                        p.codewrite("call zgesvd("+
                                        "'A', "+
                                        "'A', " + 
                                        m.code + ", " + 
                                        n.code + ", " +
                                        mat1.code + ", " +
                                        lda.code + ", " +
                                        s.code+ ", " + 
                                        u.code + ", "  + 
                                        ldu.code + ", " + 
                                        vt.code + ", "  + 
                                        ldvt.code + ", " + 
                                        work.code + ", " + 
                                        lwork.code + ", " + 
                                        rwork.code + ", " + 
                                        info.code + ")")
                                        work.deallocate()
                                    |C -> 
                                        p.extn("void zgesvd_(char *, char *, int *, int *, double complex *, int *, double *, double complex *, int *, double complex *, int *, double complex *, int *, double *, int *)")
                                        p.codewrite("zgesvd_(" + 
                                            "'A'" + ", " + 
                                            "'A'" + ", " + 
                                            "&" + m.code + ", " + 
                                            "&" + n.code + ", " +
                                            mat1.code + ", " +
                                            "&" + lda.code + ", " +
                                            s.code+ ", " + 
                                            u.code + ", "  + 
                                            "&" + ldu.code + ", " + 
                                            vt.code + ", "  + 
                                            "&" + ldvt.code + ", " + 
                                            dummy.code + ", " + 
                                            "&" + lwork.code + ", " + 
                                            rwork.code + ", " + 
                                            "&" + info.code + ");")
                                        let nb = 64
                                        br.if2 (m+3*n+nb*(m+n) .> asm.toint(asm.floor(dummy[1,1].re+0.5)))
                                            <| fun () ->
                                                lwork <== m+3*n+nb*(m+n)
                                            <| fun () ->
                                                lwork <== asm.toint(asm.floor(dummy[1,1].re+0.5))
                                        work.allocate(lwork)
                                        p.codewrite("zgesvd_(" + 
                                            "'A'" + ", " + 
                                            "'A'" + ", " + 
                                            "&" + m.code + ", " + 
                                            "&" + n.code + ", " +
                                            mat1.code + ", " +
                                            "&" + lda.code + ", " +
                                            s.code+ ", " + 
                                            u.code + ", "  + 
                                            "&" + ldu.code + ", " + 
                                            vt.code + ", "  + 
                                            "&" + ldvt.code + ", " + 
                                            work.code + ", " + 
                                            "&" + lwork.code + ", " + 
                                            rwork.code + ", " + 
                                            "&" + info.code + ");")
                                        work.deallocate()
                                    |_ -> 
                                        ()
                tbinder.d mat1 <| fun () ->
                    codestr.section "非対称実行列の特異値分解" <| fun () ->
                        s.clear()
                        u.clear()
                        vt.clear()
                        ch.iiii <| fun (m,n,lda,info) ->
                            m <== mat1.size1
                            n <== mat1.size2
                            ch.ii <| fun (ldu,ldvt) ->
                            ch.i <| fun lwork ->
                                ch.d1 lwork <| fun work ->
                                ch.d2 1 1 <| fun dummy ->
                                ch.d1 (5*m) <| fun rwork ->
                                    lda <== m
                                    ldu <== u.size1
                                    ldvt <== vt.size2
                                    match p.lang with
                                    |F -> 
                                        lwork <== -1
                                        p.codewrite("call dgesvd("+
                                        "'A', "+
                                        "'A', " + 
                                        m.code + ", " + 
                                        n.code + ", " +
                                        mat1.code + ", " +
                                        lda.code + ", " +
                                        s.code+ ", " + 
                                        u.code + ", "  + 
                                        ldu.code + ", " + 
                                        vt.code + ", "  + 
                                        ldvt.code + ", " + 
                                        dummy.code + ", " + 
                                        lwork.code + ", " + 
                                        rwork.code + ", " + 
                                        info.code + ")")
                                        let nb = 64
                                        br.if2 (m+4*n+nb*(m+n) .> asm.toint(asm.floor(dummy[1,1].re+0.5)))
                                            <| fun () ->
                                                lwork <== m+4*n+nb*(m+n)
                                            <| fun () ->
                                                lwork <== asm.toint(asm.floor(dummy[1,1].re+0.5))
                                        work.allocate(lwork)
                                        p.codewrite("call dgesvd("+
                                        "'A', "+
                                        "'A', " + 
                                        m.code + ", " + 
                                        n.code + ", " +
                                        mat1.code + ", " +
                                        lda.code + ", " +
                                        s.code+ ", " + 
                                        u.code + ", "  + 
                                        ldu.code + ", " + 
                                        vt.code + ", "  + 
                                        ldvt.code + ", " + 
                                        work.code + ", " + 
                                        lwork.code + ", " + 
                                        rwork.code + ", " + 
                                        info.code + ")")
                                        work.deallocate()
                                    |C -> 
                                        p.extn("void dgesvd_(char *, char *, int *, int *, double complex *, int *, double *, double complex *, int *, double complex *, int *, double complex *, int *, double *, int *)")
                                        lwork <== -1
                                        p.codewrite("dgesvd_(" + 
                                            "'A'" + ", " + 
                                            "'A'" + ", " + 
                                            "&" + m.code + ", " + 
                                            "&" + n.code + ", " +
                                            mat1.code + ", " +
                                            "&" + lda.code + ", " +
                                            s.code+ ", " + 
                                            u.code + ", "  + 
                                            "&" + ldu.code + ", " + 
                                            vt.code + ", "  + 
                                            "&" + ldvt.code + ", " + 
                                            work.code + ", " + 
                                            "&" + lwork.code + ", " + 
                                            rwork.code + ", " + 
                                            "&" + info.code + ");")
                                        let nb = 64
                                        br.if2 (m+4*n+nb*(m+n) .> asm.toint(asm.floor(dummy[1,1].re+0.5)))
                                            <| fun () ->
                                                lwork <== m+4*n+nb*(m+n)
                                            <| fun () ->
                                                lwork <== asm.toint(asm.floor(dummy[1,1].re+0.5))
                                        work.allocate(lwork)
                                        p.codewrite("dgesvd_(" + 
                                            "'A'" + ", " + 
                                            "'A'" + ", " + 
                                            "&" + m.code + ", " + 
                                            "&" + n.code + ", " +
                                            mat1.code + ", " +
                                            "&" + lda.code + ", " +
                                            s.code+ ", " + 
                                            u.code + ", "  + 
                                            "&" + ldu.code + ", " + 
                                            vt.code + ", "  + 
                                            "&" + ldvt.code + ", " + 
                                            work.code + ", " + 
                                            "&" + lwork.code + ", " + 
                                            rwork.code + ", " + 
                                            "&" + info.code + ");")
                                        work.deallocate()
                                    |_ -> 
                                        ()
                                        
        /// <summary>
        /// 連立同次方程式を求解
        /// </summary>
        /// <param name="mat">複素係数行列</param>
        /// <param name="f">連立方程式の解</param>
        static member solve_homogeneq (mat:num2) (f:num1) =
                ch.d1 mat.size1 <| fun s ->
                ch.z2 mat.size1 mat.size2 <| fun u ->
                ch.z2 mat.size1 mat.size2 <| fun vt ->
                    La.svd mat (u,s,vt)
                    !"0に近いほど正確な解"
                    print.tc "solve_homogeneq" s.[mat.size1]
                    iter.range _1 mat.size1 <| fun i ->
                        f.[i] <== asm.conj(vt.[mat.size1,i])