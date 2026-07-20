//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    type La() =

        /// <summary>
        /// 行列×ベクトルの計算
        /// </summary>
        /// <param name="x">a×b</param>
        /// <param name="a">a</param>
        /// <param name="b">b</param>
        static member matmul (x:double1,a:double2,b:double1) =
            x.clear()
            iter.num a.size1 <| fun i ->
                iter.num a.size2 <| fun j ->
                    x[i] <== x[i] + a[i,j] * b[j]
        /// <summary>
        /// 行列×ベクトルの計算
        /// </summary>
        /// <param name="x">a×b</param>
        /// <param name="a">a</param>
        /// <param name="b">b</param>
        static member matmul (x:complex1,a:complex2,b:double1) =
            x.clear()
            iter.num a.size1 <| fun i ->
                iter.num a.size2 <| fun j ->
                    x[i] <== x[i] + a[i,j] * b[j]
        /// <summary>
        /// 行列×ベクトルの計算
        /// </summary>
        /// <param name="x">a×b</param>
        /// <param name="a">a</param>
        /// <param name="b">b</param>
        static member matmul (x:complex1,a:double2,b:complex1) =
            x.clear()
            iter.num a.size1 <| fun i ->
                iter.num a.size2 <| fun j ->
                    x[i] <== x[i] + a[i,j] * b[j]
        /// <summary>
        /// 行列×ベクトルの計算
        /// </summary>
        /// <param name="x">a×b</param>
        /// <param name="a">a</param>
        /// <param name="b">b</param>
        static member matmul (x:complex1,a:complex2,b:complex1) =
            x.clear()
            iter.num a.size1 <| fun i ->
                iter.num a.size2 <| fun j ->
                    x[i] <== x[i] + a[i,j] * b[j]

        /// <summary>
        /// 行列×ベクトルの計算
        /// </summary>
        /// <param name="a">行列</param>
        /// <param name="b">ベクトル</param>
        static member matmul (a:double2,b:double1) = fun code ->
            ch.d1 a.size1 <| fun x ->
                La.matmul (x,a,b)
                code x
        /// <summary>
        /// 行列×ベクトルの計算
        /// </summary>
        /// <param name="a">行列</param>
        /// <param name="b">ベクトル</param>
        static member matmul (a:complex2,b:double1) = fun code ->
            ch.z1 a.size1 <| fun x ->
                La.matmul (x,a,b)
                code x
        /// <summary>
        /// 行列×ベクトルの計算
        /// </summary>
        /// <param name="a">行列</param>
        /// <param name="b">ベクトル</param>
        static member matmul (a:double2,b:complex1) = fun code ->
            ch.z1 a.size1 <| fun x ->
                La.matmul (x,a,b)
                code x
        /// <summary>
        /// 行列×ベクトルの計算
        /// </summary>
        /// <param name="a">行列</param>
        /// <param name="b">ベクトル</param>
        static member matmul (a:complex2,b:complex1) = fun code ->
            ch.z1 a.size1 <| fun x ->
                La.matmul (x,a,b)
                code x

        /// <summary>
        /// 行列×行列の計算
        /// </summary>
        /// <param name="u">計算結果</param>
        static member matmul (u:double2,a:double2,b:double2) =
            u.clear()
            iter.num a.size1 <| fun i ->
                iter.num b.size2 <| fun j ->
                    iter.num a.size2 <| fun k ->
                        u[i,j] <== u[i,j] + a[i,k] * b[k,j]
        /// <summary>
        /// 行列×行列の計算
        /// </summary>
        /// <param name="u">計算結果</param>
        static member matmul (u:complex2,a:complex2,b:double2) =
            u.clear()
            iter.num a.size1 <| fun i ->
                iter.num b.size2 <| fun j ->
                    iter.num a.size2 <| fun k ->
                        u[i,j] <== u[i,j] + a[i,k] * b[k,j]
        /// <summary>
        /// 行列×行列の計算
        /// </summary>
        /// <param name="u">計算結果</param>
        static member matmul (u:complex2,a:double2,b:complex2) =
            u.clear()
            iter.num a.size1 <| fun i ->
                iter.num b.size2 <| fun j ->
                    iter.num a.size2 <| fun k ->
                        u[i,j] <== u[i,j] + a[i,k] * b[k,j]
        /// <summary>
        /// 行列×行列の計算
        /// </summary>
        /// <param name="u">計算結果</param>
        static member matmul (u:complex2,a:complex2,b:complex2) =
            u.clear()
            iter.num a.size1 <| fun i ->
                iter.num b.size2 <| fun j ->
                    iter.num a.size2 <| fun k ->
                        u[i,j] <== u[i,j] + a[i,k] * b[k,j]

        /// <summary>
        /// 行列a×行列bの計算
        /// </summary>
        /// <param name="a">a</param>
        /// <param name="b">b</param>
        /// <param name="code">a×bに対する処理</param>
        static member matmul (a:double2,b:double2) = fun code ->
            ch.d2 a.size1 b.size2 <| fun u ->
                La.matmul (u,a,b)
                code u
        /// <summary>
        /// 行列a×行列bの計算
        /// </summary>
        /// <param name="a">a</param>
        /// <param name="b">b</param>
        /// <param name="code">a×bに対する処理</param>
        static member matmul (a:double2,b:complex2) = fun code ->
            ch.z2 a.size1 b.size2 <| fun u ->
                La.matmul (u,a,b)
                code u
        /// <summary>
        /// 行列a×行列bの計算
        /// </summary>
        /// <param name="a">a</param>
        /// <param name="b">b</param>
        /// <param name="code">a×bに対する処理</param>
        static member matmul (a:complex2,b:double2) = fun code ->
            ch.z2 a.size1 b.size2 <| fun u ->
                La.matmul (u,a,b)
                code u
        /// <summary>
        /// 行列a×行列bの計算
        /// </summary>
        /// <param name="a">a</param>
        /// <param name="b">b</param>
        /// <param name="code">a×bに対する処理</param>
        static member matmul (a:complex2,b:complex2) = fun code ->
            ch.z2 a.size1 b.size2 <| fun u ->
                La.matmul (u,a,b)
                code u

        /// <summary>
        /// ベクトルの内積計算
        /// </summary>
        /// <param name="x">a・b</param>
        /// <param name="a">a</param>
        /// <param name="b">b</param>
        static member dot (x:double0,a:double1,b:double1) =
            x.clear()
            iter.num a.size1 <| fun j ->
                x <== x + a[j] * b[j]
        /// <summary>
        /// ベクトルの内積計算
        /// </summary>
        /// <param name="x">a・b</param>
        /// <param name="a">a</param>
        /// <param name="b">b</param>
        static member dot (x:complex0,a:complex1,b:double1) =
            x.clear()
            iter.num a.size1 <| fun j ->
                x <== x + asm.conj(a[j]) * b[j]
        /// <summary>
        /// ベクトルの内積計算
        /// </summary>
        /// <param name="x">a・b</param>
        /// <param name="a">a</param>
        /// <param name="b">b</param>
        static member dot (x:complex0,a:double1,b:complex1) =
            x.clear()
            iter.num a.size1 <| fun j ->
                x <== x + a[j] * b[j]
        /// <summary>
        /// ベクトルの内積計算
        /// </summary>
        /// <param name="x">a・b</param>
        /// <param name="a">a</param>
        /// <param name="b">b</param>
        static member dot (x:complex0,a:complex1,b:complex1) =
            x.clear()
            iter.num a.size1 <| fun j ->
                x <== x + asm.conj(a[j]) * b[j]

        /// <summary>
        /// ベクトルの内積計算
        /// </summary>
        /// <param name="a"></param>
        /// <param name="b"></param>
        /// <param name="code">a・bに対する処理</param>
        static member dot (a:double1,b:double1) = fun code ->
            ch.d <| fun x ->
                La.dot (x,a,b)
                code x
        /// <summary>
        /// ベクトルの内積計算
        /// </summary>
        /// <param name="a"></param>
        /// <param name="b"></param>
        /// <param name="code">a・bに対する処理</param>
        static member dot (a:complex1,b:double1) = fun code ->
            ch.z <| fun x ->
                La.dot (x,a,b)
                code x
        /// <summary>
        /// ベクトルの内積計算
        /// </summary>
        /// <param name="a"></param>
        /// <param name="b"></param>
        /// <param name="code">a・bに対する処理</param>
        static member dot (a:double1,b:complex1) = fun code ->
            ch.z <| fun x ->
                La.dot (x,a,b)
                code x
        /// <summary>
        /// ベクトルの内積計算
        /// </summary>
        /// <param name="a"></param>
        /// <param name="b"></param>
        /// <param name="code">a・bに対する処理</param>
        static member dot (a:complex1,b:complex1) = fun code ->
            ch.z <| fun x ->
                La.dot (x,a,b)
                code x

        /// <summary>
        /// ベクトルのノルム(L2ノルム)計算
        /// </summary>
        /// <param name="a">a</param>
        /// <param name="code">ノルムaに対する処理</param>
        static member norm (a:double1) = fun code ->
            La.dot (a,a) <| fun b -> code(asm.sqrt b)
            
        /// <summary>
        /// ベクトルのノルム(L2ノルム)計算
        /// </summary>
        /// <param name="a">a</param>
        /// <param name="code">ノルムaに対する処理</param>
        static member norm (a:complex1) = fun code ->
            La.dot (a,a) <| fun b -> code(asm.sqrt b.re)

        /// <summary>
        /// ベクトルの規格化
        /// </summary>
        /// <param name="a"></param>
        static member normalize (a:double1) =
            La.norm a <| fun c ->
                a <== a/c
        /// <summary>
        /// ベクトルの規格化
        /// </summary>
        /// <param name="a"></param>
        static member normalize (a:complex1) =
            La.norm a <| fun c ->
                a <== a/c

        ///<summary>連立方程式の求解</summary>
        ///<param name="matrix">係数行列</param>
        ///<param name="y">定数項ベクトル→解ベクトル</param>
        static member solve_simuleq (matrix:complex2,y:complex1) =
            (GenerationScope.currentProgram()).olist.add "-llapack"
            (GenerationScope.currentProgram()).olist.add "-lblas"
            group.section "連立方程式の求解" <| fun () ->
                match (GenerationScope.currentProgram()).language with
                |Fortran ->
                    ch.iii <| fun (N,b,info) ->
                        N <== matrix.size1
                        b <== 1
                        ch.i1 N <| fun ipiv ->
                            writein("call zgesv("+N.code+","+b.code+","+matrix.code+","+N.code+","+ipiv.code+","+y.code+","+N.code+","+info.code+")"+"\n")
                |C99 ->
                    ch.iii <| fun (N,b,info) ->
                        N <== matrix.size1
                        b <== 1
                        ch.i1 N <| fun ipiv ->
                            (GenerationScope.currentProgram()).elist.add "void zgesv_(int *n, int *nrhs, double complex *a, int *lda, int *ipiv, double complex *b, int *ldb, int *info)"
                            writein("zgesv_(&"+N.code+","+"&"+b.code+","+matrix.code+",&"+N.code+","+ipiv.code+","+y.code+",&"+N.code+",&"+info.code+")"+";\n")
                |LaTeX ->
                    writein("$"+y.code+" \\leftarrow "+matrix.code+"^{-1}"+y.code+"$$\\\\\n")
                |HTML ->
                    writein("\\("+y.code+" \\leftarrow "+matrix.code+"^{-1}"+y.code+"\\)<br/>\n")
                |Python ->
                    writein(y.code+" = solve("+matrix.code+", "+y.code+")"+"\n")
                |_ -> ()
                
        ///<summary>連立方程式の求解</summary>
        ///<param name="matrix">係数行列</param>
        ///<param name="y">定数項ベクトル→解ベクトル</param>
        static member solve_simuleq (matrix:double2,y:double1) =
            (GenerationScope.currentProgram()).olist.add "-llapack"
            (GenerationScope.currentProgram()).olist.add "-lblas"
            group.section "連立方程式の求解" <| fun () ->
                match (GenerationScope.currentProgram()).language with
                |Fortran ->
                    ch.iii <| fun (N,b,info) ->
                        N <== matrix.size1
                        b <== 1
                        ch.i1 N <| fun ipiv ->
                            writein("call dgesv("+N.code+","+b.code+","+matrix.code+","+N.code+","+ipiv.code+","+y.code+","+N.code+","+info.code+")"+"\n")
                |C99 ->
                    ch.iii <| fun (N,b,info) ->
                        N <== matrix.size1
                        b <== 1
                        ch.i1 N <| fun ipiv ->
                            (GenerationScope.currentProgram()).elist.add "void dgesv_(int *n, int *nrhs, double *a, int *lda, int *ipiv, double *b, int *ldb, int *info)"
                            writein("dgesv_(&"+N.code+","+"&"+b.code+","+matrix.code+",&"+N.code+","+ipiv.code+","+y.code+",&"+N.code+",&"+info.code+")"+";\n")
                |LaTeX ->
                    writein("$"+y.code+" \\leftarrow "+matrix.code+"^{-1}"+y.code+"$\\\\\n")
                |HTML ->
                    writein("\\("+y.code+" \\leftarrow "+matrix.code+"^{-1}"+y.code+"\\)<br/>\n")
                |Python ->
                    writein(y.code+" = solve("+matrix.code+", "+y.code+")"+"\n")
                |_ -> ()
                
        ///<summary>連立方程式の求解</summary>
        ///<param name="matrix">係数行列</param>
        ///<param name="y">定数項ベクトルを列方向に並べた配列→解ベクトルを列方向に並べた配列</param>
        static member solve_simuleqs (matrix:complex2,y:complex2) =
            (GenerationScope.currentProgram()).olist.add "-llapack"
            (GenerationScope.currentProgram()).olist.add "-lblas"
            group.section "連立方程式の求解" <| fun () ->
                match (GenerationScope.currentProgram()).language with
                |Fortran ->
                    ch.iii <| fun (N,b,info) ->
                        N <== matrix.size1
                        b <== y.size2
                        ch.i1 N <| fun ipiv ->
                            writein("call zgesv("+N.code+","+b.code+","+matrix.code+","+N.code+","+ipiv.code+","+y.code+","+N.code+","+info.code+")"+"\n")
                |C99 ->
                    ch.iii <| fun (N,b,info) ->
                        N <== matrix.size1
                        b <== y.size2
                        ch.i1 N <| fun ipiv ->
                            (GenerationScope.currentProgram()).elist.add "void zgesv_(int *n, int *nrhs, double complex *a, int *lda, int *ipiv, double complex *b, int *ldb, int *info)"
                            writein("zgesv_(&"+N.code+","+"&"+b.code+","+matrix.code+",&"+N.code+","+ipiv.code+","+y.code+",&"+N.code+",&"+info.code+")"+";\n")
                |LaTeX ->
                    writein("$"+y.code+" \\leftarrow "+matrix.code+"^{-1}"+y.code+"$\\\\\n")
                |HTML ->
                    writein("\\("+y.code+" \\leftarrow "+matrix.code+"^{-1}"+y.code+"\\)<br/>\n")
                |Python ->
                    writein(y.code+" = solve("+matrix.code+", "+y.code+")"+"\n")
                |_ -> ()
                
        ///<summary>連立方程式の求解</summary>
        ///<param name="matrix">係数行列</param>
        ///<param name="y">定数項ベクトルを列方向に並べた配列→解ベクトルを列方向に並べた配列</param>
        static member solve_simuleqs (matrix:double2,y:double2) =
            (GenerationScope.currentProgram()).olist.add "-llapack"
            (GenerationScope.currentProgram()).olist.add "-lblas"
            group.section "連立方程式の求解" <| fun () ->
                match (GenerationScope.currentProgram()).language with
                |Fortran ->
                    ch.iii <| fun (N,b,info) ->
                        N <== matrix.size1
                        b <== y.size2
                        ch.i1 N <| fun ipiv ->
                            writein("call dgesv("+N.code+","+b.code+","+matrix.code+","+N.code+","+ipiv.code+","+y.code+","+N.code+","+info.code+")"+"\n")
                |C99 ->
                    ch.iii <| fun (N,b,info) ->
                        N <== matrix.size1
                        b <== y.size2
                        ch.i1 N <| fun ipiv ->
                            (GenerationScope.currentProgram()).elist.add "void dgesv_(int *n, int *nrhs, double *a, int *lda, int *ipiv, double *b, int *ldb, int *info)"
                            writein("dgesv_(&"+N.code+","+"&"+b.code+","+matrix.code+",&"+N.code+","+ipiv.code+","+y.code+",&"+N.code+",&"+info.code+")"+";\n")
                |LaTeX ->
                    writein("$"+y.code+" \\leftarrow "+matrix.code+"^{-1}"+y.code+"$\\\\\n")
                |HTML ->
                    writein("\\("+y.code+" \\leftarrow "+matrix.code+"^{-1}"+y.code+"\\)<br/>\n")
                |Python ->
                    writein(y.code+" = solve("+matrix.code+", "+y.code+")"+"\n")
                |_ -> ()
                
        ///<summary>逆行列の計算</summary>
        ///<param name="mat1">元の行列</param>
        ///<param name="mat2">mat1の逆行列</param>
        static member inverse_matrix (mat2:double2,mat1:double2) =
            (GenerationScope.currentProgram()).olist.add "-llapack"
            (GenerationScope.currentProgram()).olist.add "-lblas"
            group.section "逆行列の計算" <| fun () ->
                mat2.clear()
                iter.num mat1.size1 <| fun i -> mat2[i,i] <== 1.0
                match (GenerationScope.currentProgram()).language with
                |Fortran ->
                    ch.ii <| fun (npre,info) ->
                        npre<==mat1.size1
                        ch.i1 npre <| fun ipiv ->
                            ipiv.clear()
                            writein("call zgesv("+npre.code+", "+npre.code+","+mat1.code+", "+npre.code+", "+ipiv.code+","+mat2.code+", "+npre.code+", "+info.code+")")
                |C99 ->
                    ch.ii <| fun (npre,info) ->
                        npre<==mat1.size1
                        ch.i1 npre <| fun ipiv ->
                            ipiv.clear()
                            (GenerationScope.currentProgram()).elist.add "void zgesv_(int *n, int *nrhs, double complex *a, int *lda, int *ipiv, double complex *b, int *ldb, int *info)"
                            writein("zgesv_(&"+npre.code+","+"&"+npre.code+", "+mat1.code+", &"+npre.code+", "+ipiv.code+", *"+mat2.code+", &"+npre.code+", &"+info.code+");")
                |LaTeX ->
                    writein("$"+mat2.code+" \\leftarrow "+mat1.code+"^{-1}"+"$"+"\\\\\n")
                |HTML ->
                    writein("\\("+mat2.code+" \\leftarrow "+mat1.code+"^{-1}"+"\\)"+"<br/>\n")
                |Python ->
                    writein(mat2.code+" = numpy.linalg.inv("+mat1.code+")"+"\n")
                |_ -> ()
                    
        ///<summary>逆行列の計算</summary>
        ///<param name="mat1">元の行列</param>
        ///<param name="mat2">mat1の逆行列</param>
        static member inverse_matrix (mat2:complex2,mat1:complex2) =
            (GenerationScope.currentProgram()).olist.add "-llapack"
            (GenerationScope.currentProgram()).olist.add "-lblas"
            group.section "逆行列の計算" <| fun () ->
                mat2.clear()
                iter.num mat1.size1 <| fun i -> mat2[i,i] <== 1.0
                match (GenerationScope.currentProgram()).language with
                |Fortran ->
                    ch.ii <| fun (npre,info) ->
                        npre<==mat1.size1
                        ch.i1 npre <| fun ipiv ->
                            ipiv.clear()
                            writein("call dgesv("+npre.code+", "+npre.code+","+mat1.code+", "+npre.code+", "+ipiv.code+","+mat2.code+", "+npre.code+", "+info.code+")")
                            br.if1 (info .=/ 0) <| fun () -> print.tt <| "InvMatrix Info: "++info
                |C99 ->
                    ch.ii <| fun (npre,info) ->
                        npre<==mat1.size1
                        ch.i1 npre <| fun ipiv ->
                            ipiv.clear()
                            (GenerationScope.currentProgram()).elist.add "void dgesv_(int *n, int *nrhs, double *a, int *lda, int *ipiv, double *b, int *ldb, int *info)"
                            writein("dgesv_(&"+npre.code+","+"&"+npre.code+", "+mat1.code+", &"+npre.code+", "+ipiv.code+", *"+mat2.code+", &"+npre.code+", &"+info.code+");")
                            br.if1 (info .=/ 0) <| fun () -> print.tt <| "InvMatrix Info: "++info
                |LaTeX ->
                    writein("$"+mat2.code+" \\leftarrow "+mat1.code+"^{-1}"+"$"+"\\\\\n")
                |HTML ->
                    writein("\\("+mat2.code+" \\leftarrow "+mat1.code+"^{-1}"+"\\)"+"<br/>\n")
                |Python ->
                    writein(mat2.code+" = numpy.linalg.inv("+mat1.code+")"+"\n")
                |_ -> ()

        ///<summary>行列の階数</summary>
        ///<param name="rank">行列matの階数</param>
        ///<param name="mat">行列</param>
        ///<param name="cond">0とみなす上限値</param>
        static member rank (rank:double0,mat:complex2,cond:double0) =
            (GenerationScope.currentProgram()).olist.add "-llapack"
            (GenerationScope.currentProgram()).olist.add "-lblas"
            group.section "行列の階数" <| fun () ->
                ch.d1 mat.size1 <| fun s -> ch.z2 mat.size1 mat.size1 <| fun u -> ch.z2 mat.size1 mat.size1 <| fun vt ->
                    //特異値分解を利用
                    match (GenerationScope.currentProgram()).language with
                    |Fortran ->
                        ch.iii <| fun (npre,info,lwork) ->
                            npre<==mat.size1
                            ch.i1 mat.size1 <| fun ipiv -> ch.d1 (5*mat.size1) <| fun rwork -> ch.i1 (8*mat.size1) <| fun iwork ->
                            ch.z1 lwork <| fun work ->
                                ipiv.clear()
                                lwork <== 2*npre+npre
                                writein("call zgesdd('N', " +
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
                                br.if1 (info .=/ 0) <| fun () -> print.tt <| "rank Info: "++info
                    |C99 ->
                        ch.iii <| fun (npre,info,lwork) ->
                            npre<==mat.size1
                            ch.i1 mat.size1 <| fun ipiv -> ch.d1 (5*mat.size1) <| fun rwork -> ch.i1 (8*mat.size1) <| fun iwork ->
                            ch.z1 lwork <| fun work ->
                            ch.c <| fun jobz ->
                                match jobz with
                                |Var(_,name,_) ->
                                    ipiv.clear()
                                    lwork <== 2*npre+npre
                                    (GenerationScope.currentProgram()).elist.add "void zgesdd_(char jobz, int m, int n, double complex *a, int lda, double *s, double complex *u, int ldu, double complex *vt, int ldvt, double complex *work, int lwork, double *rwork, int *iwork, int info)"
                                    writein(name + " = 'N';")
                                    writein("zgesdd_(" +
                                        "&" + name + ", " +
                                        "&" + npre.code + "," +
                                        "&" + npre.code + ", " +
                                        mat.code+", " +
                                        "&" + npre.code + ", " +
                                        "*" + s.code + ", " +
                                        "*" + u.code + ", " +
                                        "&" + npre.code + ", " +
                                        "*" + vt.code + ", " +
                                        "&" + npre.code + ", " +
                                        "&" + work.code + ", " +
                                        "&" + lwork.code + ", " +
                                        "*" + rwork.code + ", " +
                                        "*" + iwork.code + ", " +
                                        "&" + info.code + ");")
                                    br.if1 (info .=/ 0) <| fun () -> print.tt <| "rank Info: "++info
                                |_ -> ()
                    |LaTeX ->
                        writein("\\("+rank.code+" \\leftarrow "+"\\mathrm{rank}\\left["+mat.code+"\\right]"+"$\\\\\n")
                    |HTML ->
                        writein("\\("+rank.code+" \\leftarrow "+"\\mathrm{rank}\\left["+mat.code+"\\right]"+"\\)<br/>\n")
                    |Python ->
                        //左特異ベクトルu.code、特異値s.code、右特異ベクトルvt.codeを求める
                        writein(u.code+","+s.code+","+vt.code+" = svd("+mat.code+")"+"\n")
                        writein "threshold = 1e-10  # ゼロの閾値\n"
                        //行列の階級rank.codeを求める
                        writein(rank.code+" = numpy.sum("+s.code+" > threshold)"+"\n")
                    |_ -> ()

        ///<summary>行列の階数</summary>
        ///<param name="rank">行列matの階数</param>
        ///<param name="mat">行列</param>
        ///<param name="cond">0とみなす上限値</param>
        static member rank (rank:int0,mat:double2,cond:double0) =
            (GenerationScope.currentProgram()).olist.add "-llapack"
            (GenerationScope.currentProgram()).olist.add "-lblas"
            group.section "行列の階数" <| fun () ->
                ch.d1 mat.size1 <| fun s -> ch.z2 mat.size1 mat.size1 <| fun u -> ch.z2 mat.size1 mat.size1 <| fun vt ->
                    //特異値分解を利用
                    match (GenerationScope.currentProgram()).language with
                    |Fortran ->
                        ch.iii <| fun (npre,info,lwork) ->
                            npre<==mat.size1
                            ch.i1 mat.size1 <| fun ipiv -> ch.d1 (5*mat.size1) <| fun rwork -> ch.i1 (8*mat.size1) <| fun iwork ->
                            ch.d1 lwork <| fun work ->
                                ipiv.clear()
                                lwork <== 2*npre+npre
                                writein("call dgesdd('N', " +
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
                                br.if1 (info .=/ 0) <| fun () -> print.tt <| "rank Info: "++info
                    |C99 ->
                        ch.iii <| fun (npre,info,lwork) ->
                            npre<==mat.size1
                            ch.i1 mat.size1 <| fun ipiv -> ch.d1 (5*mat.size1) <| fun rwork -> ch.i1 (8*mat.size1) <| fun iwork ->
                            ch.d1 lwork <| fun work ->
                            ch.c <| fun jobz ->
                                match jobz with
                                |Var(_,name,_) ->
                                    ipiv.clear()
                                    lwork <== 2*npre+npre
                                    (GenerationScope.currentProgram()).elist.add "void dgesdd_(char jobz, int m, int n, double *a, int lda, double *s, double *u, int ldu, double *vt, int ldvt, double *work, int lwork, int *iwork, int info)"
                                    writein(name + " = 'N';")
                                    writein("dgesdd_(" +
                                        "&" + name + ", " +
                                        "&" + npre.code + "," +
                                        "&" + npre.code + ", " +
                                        mat.code+", " +
                                        "&" + npre.code + ", " +
                                        "*" + s.code + ", " +
                                        "*" + u.code + ", "+
                                        "&" + npre.code + ", " +
                                        "*" + vt.code + ", " +
                                        "&" + npre.code + ", " +
                                        "&" + work.code + ", " +
                                        "&" + lwork.code + ", " +
                                        "*" + rwork.code + ", " +
                                        "*" + iwork.code + ", " +
                                        "&" + info.code + ");")
                                    br.if1 (info .=/ 0) <| fun () -> print.tt <| "rank Info: "++info
                                |_ -> ()
                    |LaTeX ->
                        writein("\\("+rank.code+" \\leftarrow "+"\\mathrm{rank}\\left["+mat.code+"\\right]"+"$\\\\\n")
                    |HTML ->
                        writein("\\("+rank.code+" \\leftarrow "+"\\mathrm{rank}\\left["+mat.code+"\\right]"+"\\)<br/>\n")
                    |Python ->
                        //左特異ベクトルu.code、特異値s.code、右特異ベクトルvt.codeを求める
                        writein(u.code+","+s.code+","+vt.code+" = svd("+mat.code+")"+"\n")
                        writein "threshold = 1e-10  # ゼロの閾値\n"
                        //行列の階級rank.codeを求める
                        writein(rank.code+" = numpy.sum("+s.code+" > threshold)"+"\n")
                    |_ -> ()
                    rank.clear()
                    s.foreach <| fun i ->
                        br.if1 (s[i] .> cond) <| fun () -> rank.inc()

        ///<summary>疑似逆行列の計算</summary>
        ///<param name="mat2">matの疑似逆行列</param>
        ///<param name="mat">行列</param>
        ///<param name="cond">特異値を0とみなす上限値</param>
        static member inverse_matrix2 (mat2:complex2,mat:complex2,cond:double0) =
            group.section "疑似逆行列" <| fun () ->
                ch.i <| fun ns ->
                    br.if2  (mat.size1.<mat.size2)
                    <| fun () ->
                        ns <== mat.size1
                    <| fun () ->
                        ns <== mat.size2
                    ch.d1 ns <| fun s ->
                    ch.z2 mat.size1 mat.size1 <| fun u ->
                    ch.z2 mat.size2 mat.size2 <| fun vt ->
                    ch.z2 mat.size2 mat.size1 <| fun u2 ->
                        La.svd mat (u,s,vt)
                        //特異値分解した行列をもとに、疑似逆行列は (v^*)×(s^-1)×(u^*)
                        u2.clear()
                        iter.num ns <| fun i ->
                            iter.num u.size1 <| fun j ->
                                //condより小さい特異値は無視
                                br.if1 (s[i]/s[0] .> cond) <| fun () ->
                                    u2[i,j] <== asm.conj(u[j,i]) / s[i]
                        mat2.clear()
                        iter.num vt.size2 <| fun i ->
                            iter.num u2.size2  <| fun j ->
                                iter.num u2.size1 <| fun p ->
                                    mat2[i,j] <== mat2[i,j] + asm.conj(vt[p,i])*u2[p,j]
                                    
        ///<summary>疑似逆行列の計算</summary>
        ///<param name="mat2">matの疑似逆行列</param>
        ///<param name="mat">行列</param>
        ///<param name="cond">特異値を0とみなす上限値</param>
        static member inverse_matrix2 (mat2:double2,mat:double2,cond:double0) =
            group.section "疑似逆行列" <| fun () ->
                ch.i <| fun ns ->
                    br.if2  (mat.size1.<mat.size2)
                    <| fun () ->
                        ns <== mat.size1
                    <| fun () ->
                        ns <== mat.size2
                    ch.d1 ns <| fun s ->
                    ch.d2 mat.size1 mat.size1 <| fun u ->
                    ch.d2 mat.size2 mat.size2 <| fun vt ->
                    ch.d2 mat.size2 mat.size1 <| fun u2 ->
                        La.svd mat (u,s,vt)
                        //特異値分解した行列をもとに、疑似逆行列は (v^*)×(s^-1)×(u^*)
                        u2.clear()
                        iter.num ns <| fun i ->
                            iter.num u.size1 <| fun j ->
                                br.if1 (s[i]/s[0] .> cond) <| fun () ->
                                    u2[i,j] <== u[j,i] / s[i]
                        mat2.clear()
                        iter.num vt.size2 <| fun i ->
                            iter.num u2.size2  <| fun j ->
                                iter.num u2.size1 <| fun p ->
                                    mat2[i,j] <== mat2[i,j] + vt[p,i] * u2[p,j]

        /// <summary>
        /// Ax=λxの固有値λと固有ベクトルxを計算
        /// </summary>
        /// <param name="eigenvalues">固有値</param>
        /// <param name="eigenvectors">固有ベクトル</param>
        /// <param name="mat1">複素非対称行列</param>
        static member eigen_matrix (eigenvalues:complex1,eigenvectors:complex2) (mat1:complex2) =
            (GenerationScope.currentProgram()).olist.add "-llapack"
            (GenerationScope.currentProgram()).olist.add "-lblas"
            group.section "非対称複素行列の固有値" <| fun () ->
                eigenvectors.clear()
                match (GenerationScope.currentProgram()).language with
                |Fortran ->
                    ch.iii <| fun (npre,ldvldummy,info) ->
                            npre<==mat1.size1
                            ch.z2 _1 _1 <| fun dummy ->
                                ch.i <| fun lwork ->
                                    lwork <== 2*npre
                                    ch.z1 lwork <| fun work ->
                                        ch.d1 (2*npre) <| fun rwork ->
                                            eigenvalues.clear()
                                            ldvldummy <== 1
                                            writein("call zgeev('No left vectors', 'Vectors (right)', "    +
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
                                            br.if1 (info .=/ 0) <| fun () -> print.tt <| "Eigenvalue Info: "++info
                |C99 ->
                    ch.iii <| fun (npre,ldvldummy,info) ->
                            npre<==mat1.size1
                            ch.z2 _1 _1 <| fun dummy ->
                                ch.i <| fun lwork ->
                                    lwork <== 2*npre
                                    ch.z1 lwork <| fun work ->
                                        ch.d1 (2*npre) <| fun rwork ->
                                        ch.c <| fun jobvl ->
                                        ch.c <| fun jobvr ->
                                            match jobvl,jobvr with
                                            |Var(_,jobvl,_),Var(_,jobvr,_) ->
                                                eigenvalues.clear()
                                                ldvldummy <== 1
                                                (GenerationScope.currentProgram()).elist.add "void zgeev_(char *, char *, int *, double complex *, int *, double complex *, double complex *, int *, double complex *, int *, double complex *, int *, double *, int *)"
                                                writein(jobvl + " = 'N';")
                                                writein(jobvr + " = 'V';")
                                                writein("zgeev_(" +
                                                    "&" + jobvl + ", " +
                                                    "&" + jobvr + ", " +
                                                    "&" + npre.code + ", "  +
                                                    mat1.code + ", " +
                                                    "&" + npre.code + ", " +
                                                    eigenvalues.code + ", " +
                                                    dummy.code + ", " +
                                                    "&" + ldvldummy.code + ", " +
                                                    eigenvectors.code + ", " +
                                                    "&" + npre.code + ", " +
                                                    work.code + ", " +
                                                    "&" + lwork.code + ", " +
                                                    rwork.code + ", " +
                                                    "&" + info.code + ");")
                                                br.if1 (info .=/ 0) <| fun () -> print.tt <| "Eigenvalue Info: "++info
                                            |_ -> ()
                |LaTeX ->
                    writein("Solve: $"+mat1.code+eigenvectors.code+" = "+eigenvalues.code+eigenvectors.code+"$"+"<br/>\n")
                |HTML ->
                    writein("Solve: \\("+mat1.code+eigenvectors.code+" = "+eigenvalues.code+eigenvectors.code+"\\)"+"<br/>\n")
                |Python ->
                    writein(eigenvalues.code+","+eigenvectors.code+" = eig("+mat1.code+")"+"\n")
                |_ -> ()
                
        /// <summary>
        /// Ax=λBxの固有値λと固有ベクトルxを計算
        /// </summary>
        /// <param name="eigenvalues1">λ1(λ=λ1/λ2)</param>
        /// <param name="eigenvalues2">λ2(λ=λ1/λ2)</param>
        /// <param name="eigenvectors">固有ベクトルx</param>
        /// <param name="mat1">行列A</param>
        /// <param name="mat2">行列B</param>
        static member eigen_matrix2 (eigenvalues1:complex1,eigenvalues2:complex1,eigenvectors:complex2) (mat1:complex2) (mat2:complex2) =
                group.section "非対称複素行列の一般化固有値" <| fun () ->
                    (GenerationScope.currentProgram()).olist.add "-llapack"
                    (GenerationScope.currentProgram()).olist.add "-lblas"
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
                                        match (GenerationScope.currentProgram()).language with
                                        |Fortran ->
                                            writein("call zggev("+
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
                                        |C99 ->
                                            ch.c <| fun jobvl ->
                                            ch.c <| fun jobvr ->
                                                match jobvl,jobvr with
                                                |Var(_,jobvl,_),Var(_,jobvr,_) ->
                                                    (GenerationScope.currentProgram()).elist.add "void zggev_(char *, char *, int *, double complex *, int *, double complex *, int *, double complex *, double complex *, double complex *, int *, double complex *, int *, double complex *, int *, double *, int *)"
                                                    writein(jobvl + " = 'N';")
                                                    writein(jobvr + " = 'V';")
                                                    writein("zggev_(" +
                                                        "&" + jobvl + ", " +
                                                        "&" + jobvr + ", " +
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
                                                |_ -> ()
                                        |LaTeX ->
                                            writein("Solve: $"+mat1.code+eigenvectors.code+" = "+"\\frac{"+eigenvalues1.code+"}{"+eigenvalues2.code+"}"+mat2.code+eigenvectors.code+"$\\\\\n")
                                        |HTML ->
                                            writein("Solve: \\("+mat1.code+eigenvectors.code+" = "+"\\frac{"+eigenvalues1.code+"}{"+eigenvalues2.code+"}"+mat2.code+eigenvectors.code+"\\)<br/>\n")
                                        //Pythonのscipy.linalg.eigは、一般化固有値問題を単独の出力で処理することが可能
                                        //Pythonでは、一般化固有値の計算が単一の出力で提供されるため、ユーザーは結果を手軽に利用できる。これにより、計算過程や出力の管理がシンプルになる。
                                        //Fortranでは、二つの固有値配列を出力することで、行列 AとB の関係性を明示的に示している。この設計は、行列間の相互作用をより詳細に理解するためのもの
                                        //このコードでは、周囲と合わせるため、行列を入れ替えてeigenvalues2.codeを出している。
                                        //ちなみに一般化固有ベクトルは二つも出す必要はないので、二行目で出しているeigenvectors.code_dasokuはおまけだと思っていい。理由は以下。
                                        //一般化固有値問題 Ax=λBx の形式では、行列 B に対して左固有ベクトルが計算されることはない。したがって、一般化固有ベクトルは一意に定まることが多い。
                                        |Python ->
                                            writein(eigenvalues1.code+","+eigenvectors.code+" = eig("+mat1.code+","+mat2.code+")"+"\n")
                                            writein(eigenvalues2.code+", "+eigenvectors.code+"_dasoku = eig("+mat2.code+","+mat1.code+")"+"\n")
                                        |_ -> ()
                                        br.if1 (info .=/ 0) <| fun () -> print.tt <| "Eigenvalue Info: "++info

        /// <summary>
        /// 連立方程式の求解(Tikhonovの正則化法)
        /// </summary>
        /// <param name="fu_mat">係数行列</param>
        /// <param name="fu_cst">定数項ベクトル</param>
        /// <param name="code">解に対して行う処理</param>
        static member solve_simuleq_t(fu_mat:double2,fu_cst:double1) = fun code ->
            group.h2 "連立方程式の求解(Tikhonovの正則化法)" <| fun () ->
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
                                    tmp <== tmp + fu_mat[k,i]*fu_mat[k,j]
                                FF[i,j] <== tmp

                    //FF = fu_mat^T * fu_mat + λ^2 * I
                    iter.num FF.size1 <| fun i ->
                            FF[i,i] <== FF[i,i] + lambda * lambda
                    //bb = fu_mat^T * fu_cst
                    ch.d <| fun tmp ->
                        bb.clear()
                        iter.num bb.size1 <| fun i ->
                            tmp.clear()
                            iter.num fu_cst.size1 <| fun k ->
                                tmp <== tmp + fu_mat[k,i]*fu_cst[k]
                            bb[i] <== tmp
                    La.solve_simuleq(FF,bb)
                    code bb

        /// <summary>
        /// 連立方程式の求解(Tikhonovの正則化法)
        /// </summary>
        /// <param name="fu_mat">係数行列</param>
        /// <param name="fu_cst">定数項ベクトル</param>
        /// <param name="code">解に対して行う処理</param>
        static member solve_simuleq_t(fu_mat:complex2,fu_cst:complex1) = fun code ->
            group.h2 "連立方程式の求解(Tikhonovの正則化法)" <| fun () ->
                ch.z2 fu_mat.size2 fu_mat.size2 <| fun FF ->
                ch.z1 fu_mat.size2 <| fun bb ->
                    let lambda = 1E-6 //正則化パラメータ
                    FF.clear()
                    //FF = fu_mat^T * fu_mat
                    ch.z <| fun tmp ->
                        iter.num FF.size1 <| fun i ->
                            iter.num FF.size2 <| fun j ->
                                tmp.clear()
                                iter.num fu_mat.size1 <| fun k ->
                                    tmp <== tmp + fu_mat[k,i]*fu_mat[k,j]
                                FF[i,j] <== tmp

                    //FF = fu_mat^T * fu_mat + λ^2 * I
                    iter.num FF.size1 <| fun i ->
                            FF[i,i] <== FF[i,i] + lambda * lambda
                    //bb = fu_mat^T * fu_cst
                    ch.z <| fun tmp ->
                        bb.clear()
                        iter.num bb.size1 <| fun i ->
                            tmp.clear()
                            iter.num fu_cst.size1 <| fun k ->
                                tmp <== tmp + fu_mat[k,i]*fu_cst[k]
                            bb[i] <== tmp
                    La.solve_simuleq(FF,bb)
                    code bb

        /// <summary>
        /// 連立方程式の求解(Tikhonovの正則化法)
        /// </summary>
        /// <param name="fu_mat">係数行列</param>
        /// <param name="fu_cst">定数項ベクトル</param>
        /// <param name="lambda">正則化パラメータ</param>
        /// <param name="code">解に対して行う処理</param>
        static member solve_simuleq_tt(fu_mat:complex2,fu_cst:complex1,lambda:double) = fun code ->
            group.h2 "連立方程式の求解(Tikhonovの正則化法)" <| fun () ->
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
                                    tmp <== tmp + asm.conj(fu_mat[k,i])*fu_mat[k,j]
                                FF[i,j] <== tmp
                    //FF = fu_mat^T * fu_mat + λ^2 * I
                    iter.num FF.size1 <| fun i ->
                            FF[i,i] <== FF[i,i] + lambda * lambda
                    //bb = fu_mat^T * fu_cst
                    ch.z <| fun tmp ->
                        bb.clear()
                        iter.num bb.size1 <| fun i ->
                            tmp.clear()
                            iter.num fu_cst.size1 <| fun k ->
                                tmp <== tmp + asm.conj(fu_mat[k,i])*fu_cst[k]
                            bb[i] <== tmp
                    La.solve_simuleq(FF,bb)
                    code(bb)

        /// <summary>
        /// 連立方程式の求解(Tikhonovの正則化法)
        /// </summary>
        /// <param name="fu_mat">係数行列</param>
        /// <param name="fu_cst">定数項ベクトル</param>
        /// <param name="lambda">正則化パラメータ</param>
        /// <param name="code">解に対して行う処理</param>
        static member solve_simuleq_tt(fu_mat:double2,fu_cst:double1,lambda:double) = fun code ->
            group.h2 "連立方程式の求解(Tikhonovの正則化法)" <| fun () ->
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
                                    tmp <== tmp + fu_mat[k,i]*fu_mat[k,j]
                                FF[i,j] <== tmp
                    //FF = fu_mat^T * fu_mat + λ^2 * I
                    iter.num FF.size1 <| fun i ->
                            FF[i,i] <== FF[i,i] + lambda * lambda
                    //bb = fu_mat^T * fu_cst
                    ch.d <| fun tmp ->
                        bb.clear()
                        iter.num bb.size1 <| fun i ->
                            tmp.clear()
                            iter.num fu_cst.size1 <| fun k ->
                                tmp <== tmp + fu_mat[k,i]*fu_cst[k]
                            bb[i] <== tmp
                    La.solve_simuleq(FF,bb)
                    code bb

        /// <summary>
        /// 連立方程式の求解(Tikhonovの正則化法)
        /// </summary>
        /// <param name="fu_mat">係数行列</param>
        /// <param name="fu_cst">定数項ベクトル(列サイズ=1)</param>
        /// <param name="lambda">正則化パラメータ</param>
        /// <param name="code">解に対して行う処理</param>
        static member solve_simuleq_tt2(fu_mat:complex2,fu_cst:complex2,lambda:double0) code =
            group.h2 "連立方程式の求解(Tikhonovの正則化法)" <| fun () ->
                ch.z2 fu_mat.size2 fu_mat.size2 <| fun FF ->
                ch.z1 fu_mat.size2 <| fun bb ->
                    FF.clear()
                    //FF = fu_mat^T * fu_mat
                    ch.z <| fun tmp ->
                        iter.num FF.size1 <| fun i ->
                            iter.num FF.size2 <| fun j ->
                                tmp.clear()
                                iter.num fu_mat.size1 <| fun k ->
                                    tmp <== tmp + asm.conj(fu_mat[k,i])*fu_mat[k,j]
                                FF[i,j] <== tmp
                    //FF = fu_mat^T * fu_mat + λ^2 * I
                    iter.num FF.size1 <| fun i ->
                            FF[i,i] <== FF[i,i] + lambda * lambda
                    //bb = fu_mat^T * fu_cst
                    ch.z <| fun tmp ->
                        bb.clear()
                        iter.num bb.size1 <| fun i ->
                            tmp.clear()
                            iter.num fu_cst.size1 <| fun k ->
                                tmp <== tmp + asm.conj(fu_mat[k,i])*fu_cst[k,1]
                            bb[i] <== tmp
                    La.solve_simuleq(FF,bb)
                    code bb

        /// <summary>
        /// 行列式の常用対数を計算
        /// </summary>
        /// <param name="matrix">行列</param>
        /// <param name="code">行列式の値を用いて実行するコード</param>
        static member determinant (matrix:complex2) = fun code ->
            (GenerationScope.currentProgram()).olist.add "-llapack"
            (GenerationScope.currentProgram()).olist.add "-lblas"
            group.section "行列式の常用対数を計算" <| fun () ->
                ch.d <| fun d ->
                    match (GenerationScope.currentProgram()).language with
                    |Fortran ->
                        ch.iid <| fun (N,info,d) ->
                            N <== matrix.size1
                            ch.i1 N <| fun ipiv ->
                                writein("call zgetrf("+N.code+","+N.code+","+matrix.code+","+N.code+","+ipiv.code+","+info.code+")"+"\n")
                    |C99 ->
                        ch.iid <| fun (N,info,d) ->
                            N <== matrix.size1
                            ch.i1 N <| fun ipiv ->
                                (GenerationScope.currentProgram()).elist.add "void zgetrf_(int *, int *, double complex *, int *, int *, int *)"
                                writein("zgetrf_(&"+N.code+","+"&"+N.code+","+matrix.code+",&"+N.code+","+ipiv.code+",&"+info.code+")"+";\n")
                    |LaTeX ->
                        writein("$"+d.code+" = "+"\\left|"+matrix.code+"\\right|"+"$"+"\\\\\n")
                    |HTML ->
                        writein("\\("+d.code+" = "+"\\left|"+matrix.code+"\\right|"+"\\)"+"<br/>\n")
                    |Python ->
                        //LU分解
                        writein("P ,L ,U = lu("+matrix.code+")"+"\n")
                        //上三角行列 U の対角成分の積を計算
                        writein("det_U = numpy.prod(numpy.diag(U))"+"\n")
                        //行列式を計算
                        //pの行列式は、ピボット行列の行交換の回数で符号が決まる
                        writein("sign = (-1) ** numpy.sum(numpy.arange("+matrix.code+".shape[0]) != numpy.argsort(numpy.argsort(P[:, 0])))"+"\n")
                        writein("det_"+matrix.code+" = sign * det_U"+"\n")
                        //行列式の常用対数を計算
                        writein(d.code+" = numpy.log10(det_"+matrix.code+")"+"\n")
                    |_ -> ()
                    d.clear()
                    iter.num matrix.size1 <| fun i ->
                        d <== d + asm.log10(asm.abs(matrix[i,i]))
                    code d

        /// <summary>
        /// 行列式の常用対数を計算
        /// </summary>
        /// <param name="matrix">行列</param>
        /// <param name="code">行列式の値を用いて実行するコード</param>
        static member determinant (matrix:double2) = fun code ->
            (GenerationScope.currentProgram()).olist.add "-llapack"
            (GenerationScope.currentProgram()).olist.add "-lblas"
            group.section "行列式の常用対数を計算" <| fun () ->
                ch.d <| fun d ->
                    match (GenerationScope.currentProgram()).language with
                    |Fortran ->
                        ch.iid <| fun (N,info,d) ->
                            N <== matrix.size1
                            ch.i1 N <| fun ipiv ->
                                writein("call dgetrf("+N.code+","+N.code+","+matrix.code+","+N.code+","+ipiv.code+","+info.code+")"+"\n")
                    |C99 ->
                        ch.iid <| fun (N,info,d) ->
                            N <== matrix.size1
                            ch.i1 N <| fun ipiv ->
                                (GenerationScope.currentProgram()).elist.add "void dgetrf_(int *, int *, double complex *, int *, int *, int *)"
                                writein("dgetrf_(&"+N.code+","+"&"+N.code+","+matrix.code+",&"+N.code+","+ipiv.code+",&"+info.code+")"+";\n")
                    |LaTeX ->
                        writein("$"+d.code+" = "+"\\left|"+matrix.code+"\\right|"+"$"+"\\\\\n")
                    |HTML ->
                        writein("\\("+d.code+" = "+"\\left|"+matrix.code+"\\right|"+"\\)"+"<br/>\n")
                    |Python ->
                        //LU分解
                        writein("P ,L ,U = lu("+matrix.code+")"+"\n")
                        //上三角行列 U の対角成分の積を計算
                        writein("det_U = numpy.prod(numpy.diag(U))"+"\n")
                        //行列式を計算
                        //pの行列式は、ピボット行列の行交換の回数で符号が決まる
                        writein("sign = (-1) ** numpy.sum(numpy.arange("+matrix.code+".shape[0]) != numpy.argsort(numpy.argsort(P[:, 0])))"+"\n")
                        writein("det_"+matrix.code+" = sign * det_U"+"\n")
                        //行列式の常用対数を計算
                        writein(d.code+" = np.log10(np.abs(det_"+matrix.code+"))"+"\n")
                    |_ -> ()
                    d.clear()
                    iter.num matrix.size1 <| fun i ->
                        d <== d + asm.log10(asm.abs(matrix[i,i]))
                    code d

        /// <summary>
        /// mat = u * s * v に特異値分解
        /// </summary>
        /// <param name="mat1">複素行列</param>
        /// <param name="u">複素行列u</param>
        /// <param name="s">正方行列sの対角成分</param>
        /// <param name="vt">複素行列vの転置</param>
        static member svd (mat1:complex2) = fun (u:complex2,s:double1,vt:complex2) ->
            (GenerationScope.currentProgram()).olist.add "-llapack"
            (GenerationScope.currentProgram()).olist.add "-lblas"
            match (GenerationScope.currentProgram()).language with
            |LaTeX ->
                writein("$"+mat1.code+" = "+u.code+s.code+vt.code+"^{\\mathrm{T}}"+"$\\\\\n")
            |HTML ->
                writein("\\("+mat1.code+" = "+u.code+s.code+vt.code+"^{\\mathrm{T}}"+"\\)<br/>\n")
            |Python ->
                //左特異ベクトルu.code、特異値s.code、右特異ベクトルvt.codeを求める
                writein(u.code+","+s.code+","+vt.code+" = svd("+mat1.code+")"+"\n")
            |_ ->
                group.section "非対称複素行列の特異値分解" <| fun () ->
                    s.clear()
                    u.clear()
                    vt.clear()
                    match (GenerationScope.currentProgram()).language with
                    |Fortran ->
                        ch.iiii <| fun (m,n,lda,info) ->
                        ch.i <| fun ns ->
                            m <== mat1.size1
                            n <== mat1.size2
                            br.if2 (m.<n) (fun () -> ns <== m) (fun () -> ns <== n)
                            ch.ii <| fun (ldu,ldvt) ->
                            ch.i <| fun lwork ->
                                ch.z01 <| fun work ->
                                ch.d1 (5*ns) <| fun rwork ->
                                    lda <== m
                                    ldu <== u.size1
                                    ldvt <== vt.size2
                                    lwork <== -1
                                    work.allocate 1
                                    writein("call zgesvd("+
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
                                    lwork <== asm.toint work[0].re
                                    work.deallocate()
                                    work.allocate lwork
                                    writein("call zgesvd("+
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
                    |C99 ->
                        ch.iiii <| fun (m,n,lda,info) ->
                        ch.i <| fun ns ->
                            m <== mat1.size1
                            n <== mat1.size2
                            br.if2 (m.<n) (fun () -> ns <== m) (fun () -> ns <== n)
                            ch.ii <| fun (ldu,ldvt) ->
                            ch.i <| fun lwork ->
                                ch.z01 <| fun work ->
                                ch.z <| fun wkopt ->
                                ch.d1 (5*ns) <| fun rwork ->
                                ch.c <| fun jobu ->
                                ch.c <| fun jobv ->
                                    match jobu,jobv with
                                    |Var(_,jobu,_),Var(_,jobv,_) ->
                                        lda <== m
                                        ldu <== u.size1
                                        ldvt <== vt.size2
                                        lwork <== -1
                                        (GenerationScope.currentProgram()).elist.add "void zgesvd_(char *, char *, int *, int *, double complex *, int *, double *, double complex *, int *, double complex *, int *, double complex *, int *, double *, int *)"
                                        writein(jobu + " = 'A';")
                                        writein(jobv + " = 'A';")
                                        writein("zgesvd_(" +
                                            "&" + jobu + ", " +
                                            "&" + jobv + ", " +
                                            "&" + m.code + ", " +
                                            "&" + n.code + ", " +
                                            mat1.code + ", " +
                                            "&" + lda.code + ", " +
                                            s.code+ ", " +
                                            u.code + ", "  +
                                            "&" + ldu.code + ", " +
                                            vt.code + ", "  +
                                            "&" + ldvt.code + ", " +
                                            "&" + wkopt.code + ", " +
                                            "&" + lwork.code + ", " +
                                            rwork.code + ", " +
                                            "&" + info.code + ");")
                                        lwork <== asm.toint wkopt.re
                                        work.allocate lwork
                                        writein("zgesvd_(" +
                                            "&" + jobu + ", " +
                                            "&" + jobv + ", " +
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
                                    |_ -> ()
                    |Python ->
                        //左特異ベクトルu.code、特異値s.code、右特異ベクトルvt.codeを求める
                        writein(u.code+","+s.code+","+vt.code+" = svd("+mat1.code+")"+"\n")
                    |_ ->
                        ()

        /// <summary>
        /// mat = u * s * v に特異値分解
        /// </summary>
        /// <param name="mat1">複素行列</param>
        /// <param name="u">複素行列u</param>
        /// <param name="s">正方行列sの対角成分</param>
        /// <param name="vt">複素行列vの転置</param>
        static member svd (mat1:double2) = fun (u:double2,s:double1,vt:double2) ->
            (GenerationScope.currentProgram()).olist.add "-llapack"
            (GenerationScope.currentProgram()).olist.add "-lblas"
            match (GenerationScope.currentProgram()).language with
            |LaTeX ->
                writein("$"+mat1.code+" = "+u.code+s.code+vt.code+"^{\\mathrm{T}}"+"$\\\\\n")
            |HTML ->
                writein("\\("+mat1.code+" = "+u.code+s.code+vt.code+"^{\\mathrm{T}}"+"\\)<br/>\n")
            |Python ->
                //左特異ベクトルu.code、特異値s.code、右特異ベクトルvt.codeを求める
                writein(u.code+","+s.code+","+vt.code+" = svd("+mat1.code+")"+"\n")
            |_ ->
                group.section "非対称実行列の特異値分解" <| fun () ->
                    s.clear()
                    u.clear()
                    vt.clear()
                    match (GenerationScope.currentProgram()).language with
                    |Fortran ->
                        ch.iiii <| fun (m,n,lda,info) ->
                            m <== mat1.size1
                            n <== mat1.size2
                            ch.ii <| fun (ldu,ldvt) ->
                            ch.i <| fun lwork ->
                                ch.d01 <| fun work ->
                                    lda <== m
                                    ldu <== u.size1
                                    ldvt <== vt.size2
                                    lwork <== -1
                                    work.allocate 1
                                    writein("call dgesvd("+
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
                                        info.code + ")")
                                    lwork <== asm.toint work[0]
                                    work.deallocate()
                                    work.allocate lwork
                                    writein("call dgesvd("+
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
                                        info.code + ")")
                                    work.deallocate()
                    |C99 ->
                        ch.iiii <| fun (m,n,lda,info) ->
                            m <== mat1.size1
                            n <== mat1.size2
                            ch.ii <| fun (ldu,ldvt) ->
                            ch.i <| fun lwork ->
                                ch.d01 <| fun work ->
                                ch.c <| fun jobu ->
                                ch.c <| fun jobv ->
                                    match jobu,jobv with
                                    |Var(_,jobu,_),Var(_,jobv,_) ->
                                        lda <== m
                                        ldu <== u.size1
                                        ldvt <== vt.size2
                                        (GenerationScope.currentProgram()).elist.add "void dgesvd_(char *, char *, int *, int *, double *, int *, double *, double *, int *, double *, int *, double *, int *, int *)"
                                        writein(jobu + " = 'A';")
                                        writein(jobv + " = 'A';")
                                        lwork <== -1
                                        work.allocate 1
                                        writein("dgesvd_(" +
                                            "&" + jobu + ", " +
                                            "&" + jobv + ", " +
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
                                            "&" + info.code + ");")
                                        lwork <== asm.toint work[0]
                                        work.deallocate()
                                        work.allocate lwork
                                        writein("dgesvd_(" +
                                            "&" + jobu + ", " +
                                            "&" + jobv + ", " +
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
                                            "&" + info.code + ");")
                                        work.deallocate()
                                    |_ -> ()
                    |Python ->
                        //左特異ベクトルu.code、特異値s.code、右特異ベクトルvt.codeを求める
                        writein(u.code+","+s.code+","+vt.code+" = svd("+mat1.code+")"+"\n")
                    |_ ->
                        ()

        /// <summary>
        /// 連立同次方程式を求解
        /// </summary>
        /// <param name="mat">複素係数行列</param>
        /// <param name="f">連立方程式の解</param>
        static member solve_homogeneq (mat:double2,f:double1) =
                ch.d1 mat.size1 <| fun s ->
                ch.d2 mat.size1 mat.size2 <| fun u ->
                ch.d2 mat.size1 mat.size2 <| fun vt ->
                    La.svd mat (u,s,vt)
                    !"0に近いほど正確な解"
                    print.tt <| "solve_homogeneq"++s[mat.size1]
                    iter.num mat.size1 <| fun i ->
                        f[i] <== vt[mat.size1,i]

        /// <summary>
        /// 連立同次方程式を求解
        /// </summary>
        /// <param name="mat">複素係数行列</param>
        /// <param name="f">連立方程式の解</param>
        static member solve_homogeneq (mat:complex2,f:complex1) =
                ch.d1 mat.size1 <| fun s ->
                ch.z2 mat.size1 mat.size2 <| fun u ->
                ch.z2 mat.size1 mat.size2 <| fun vt ->
                    La.svd mat (u,s,vt)
                    !"0に近いほど正確な解"
                    print.tt <| "solve_homogeneq"++s[mat.size1]
                    iter.num mat.size1 <| fun i ->
                        f[i] <== asm.conj(vt[mat.size1,i])
