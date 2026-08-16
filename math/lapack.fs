//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    type ContextLa internal (context:Aqualis) =

        /// <summary>
        /// 行列×ベクトルの計算
        /// </summary>
        /// <param name="x">a×b</param>
        /// <param name="a">a</param>
        /// <param name="b">b</param>
        member this.matmul (x:double1,a:double2,b:double1) =
            x.clear()
            context.iter.num a.size1 <| fun i ->
                context.iter.num a.size2 <| fun j ->
                    x[i] <== x[i] + a[i,j] * b[j]
        /// <summary>
        /// 行列×ベクトルの計算
        /// </summary>
        /// <param name="x">a×b</param>
        /// <param name="a">a</param>
        /// <param name="b">b</param>
        member this.matmul (x:complex1,a:complex2,b:double1) =
            x.clear()
            context.iter.num a.size1 <| fun i ->
                context.iter.num a.size2 <| fun j ->
                    x[i] <== x[i] + a[i,j] * b[j]
        /// <summary>
        /// 行列×ベクトルの計算
        /// </summary>
        /// <param name="x">a×b</param>
        /// <param name="a">a</param>
        /// <param name="b">b</param>
        member this.matmul (x:complex1,a:double2,b:complex1) =
            x.clear()
            context.iter.num a.size1 <| fun i ->
                context.iter.num a.size2 <| fun j ->
                    x[i] <== x[i] + a[i,j] * b[j]
        /// <summary>
        /// 行列×ベクトルの計算
        /// </summary>
        /// <param name="x">a×b</param>
        /// <param name="a">a</param>
        /// <param name="b">b</param>
        member this.matmul (x:complex1,a:complex2,b:complex1) =
            x.clear()
            context.iter.num a.size1 <| fun i ->
                context.iter.num a.size2 <| fun j ->
                    x[i] <== x[i] + a[i,j] * b[j]

        /// <summary>
        /// 行列×ベクトルの計算
        /// </summary>
        /// <param name="a">行列</param>
        /// <param name="b">ベクトル</param>
        member this.matmul (a:double2,b:double1) = fun code ->
            context.ch.d1 a.size1 <| fun x ->
                this.matmul (x,a,b)
                code x
        /// <summary>
        /// 行列×ベクトルの計算
        /// </summary>
        /// <param name="a">行列</param>
        /// <param name="b">ベクトル</param>
        member this.matmul (a:complex2,b:double1) = fun code ->
            context.ch.z1 a.size1 <| fun x ->
                this.matmul (x,a,b)
                code x
        /// <summary>
        /// 行列×ベクトルの計算
        /// </summary>
        /// <param name="a">行列</param>
        /// <param name="b">ベクトル</param>
        member this.matmul (a:double2,b:complex1) = fun code ->
            context.ch.z1 a.size1 <| fun x ->
                this.matmul (x,a,b)
                code x
        /// <summary>
        /// 行列×ベクトルの計算
        /// </summary>
        /// <param name="a">行列</param>
        /// <param name="b">ベクトル</param>
        member this.matmul (a:complex2,b:complex1) = fun code ->
            context.ch.z1 a.size1 <| fun x ->
                this.matmul (x,a,b)
                code x

        /// <summary>
        /// 行列×行列の計算
        /// </summary>
        /// <param name="u">計算結果</param>
        member this.matmul (u:double2,a:double2,b:double2) =
            u.clear()
            context.iter.num a.size1 <| fun i ->
                context.iter.num b.size2 <| fun j ->
                    context.iter.num a.size2 <| fun k ->
                        u[i,j] <== u[i,j] + a[i,k] * b[k,j]
        /// <summary>
        /// 行列×行列の計算
        /// </summary>
        /// <param name="u">計算結果</param>
        member this.matmul (u:complex2,a:complex2,b:double2) =
            u.clear()
            context.iter.num a.size1 <| fun i ->
                context.iter.num b.size2 <| fun j ->
                    context.iter.num a.size2 <| fun k ->
                        u[i,j] <== u[i,j] + a[i,k] * b[k,j]
        /// <summary>
        /// 行列×行列の計算
        /// </summary>
        /// <param name="u">計算結果</param>
        member this.matmul (u:complex2,a:double2,b:complex2) =
            u.clear()
            context.iter.num a.size1 <| fun i ->
                context.iter.num b.size2 <| fun j ->
                    context.iter.num a.size2 <| fun k ->
                        u[i,j] <== u[i,j] + a[i,k] * b[k,j]
        /// <summary>
        /// 行列×行列の計算
        /// </summary>
        /// <param name="u">計算結果</param>
        member this.matmul (u:complex2,a:complex2,b:complex2) =
            u.clear()
            context.iter.num a.size1 <| fun i ->
                context.iter.num b.size2 <| fun j ->
                    context.iter.num a.size2 <| fun k ->
                        u[i,j] <== u[i,j] + a[i,k] * b[k,j]

        /// <summary>
        /// 行列a×行列bの計算
        /// </summary>
        /// <param name="a">a</param>
        /// <param name="b">b</param>
        /// <param name="code">a×bに対する処理</param>
        member this.matmul (a:double2,b:double2) = fun code ->
            context.ch.d2 (a.size1, b.size2) <| fun u ->
                this.matmul (u,a,b)
                code u
        /// <summary>
        /// 行列a×行列bの計算
        /// </summary>
        /// <param name="a">a</param>
        /// <param name="b">b</param>
        /// <param name="code">a×bに対する処理</param>
        member this.matmul (a:double2,b:complex2) = fun code ->
            context.ch.z2 (a.size1, b.size2) <| fun u ->
                this.matmul (u,a,b)
                code u
        /// <summary>
        /// 行列a×行列bの計算
        /// </summary>
        /// <param name="a">a</param>
        /// <param name="b">b</param>
        /// <param name="code">a×bに対する処理</param>
        member this.matmul (a:complex2,b:double2) = fun code ->
            context.ch.z2 (a.size1, b.size2) <| fun u ->
                this.matmul (u,a,b)
                code u
        /// <summary>
        /// 行列a×行列bの計算
        /// </summary>
        /// <param name="a">a</param>
        /// <param name="b">b</param>
        /// <param name="code">a×bに対する処理</param>
        member this.matmul (a:complex2,b:complex2) = fun code ->
            context.ch.z2 (a.size1, b.size2) <| fun u ->
                this.matmul (u,a,b)
                code u

        /// <summary>
        /// ベクトルの内積計算
        /// </summary>
        /// <param name="x">a・b</param>
        /// <param name="a">a</param>
        /// <param name="b">b</param>
        member this.dot (x:double0,a:double1,b:double1) =
            x.clear()
            context.iter.num a.size1 <| fun j ->
                x <== x + a[j] * b[j]
        /// <summary>
        /// ベクトルの内積計算
        /// </summary>
        /// <param name="x">a・b</param>
        /// <param name="a">a</param>
        /// <param name="b">b</param>
        member this.dot (x:complex0,a:complex1,b:double1) =
            x.clear()
            context.iter.num a.size1 <| fun j ->
                x <== x + asm.conj(a[j]) * b[j]
        /// <summary>
        /// ベクトルの内積計算
        /// </summary>
        /// <param name="x">a・b</param>
        /// <param name="a">a</param>
        /// <param name="b">b</param>
        member this.dot (x:complex0,a:double1,b:complex1) =
            x.clear()
            context.iter.num a.size1 <| fun j ->
                x <== x + a[j] * b[j]
        /// <summary>
        /// ベクトルの内積計算
        /// </summary>
        /// <param name="x">a・b</param>
        /// <param name="a">a</param>
        /// <param name="b">b</param>
        member this.dot (x:complex0,a:complex1,b:complex1) =
            x.clear()
            context.iter.num a.size1 <| fun j ->
                x <== x + asm.conj(a[j]) * b[j]

        /// <summary>
        /// ベクトルの内積計算
        /// </summary>
        /// <param name="a"></param>
        /// <param name="b"></param>
        /// <param name="code">a・bに対する処理</param>
        member this.dot (a:double1,b:double1) = fun code ->
            context.ch.d <| fun x ->
                this.dot (x,a,b)
                code x
        /// <summary>
        /// ベクトルの内積計算
        /// </summary>
        /// <param name="a"></param>
        /// <param name="b"></param>
        /// <param name="code">a・bに対する処理</param>
        member this.dot (a:complex1,b:double1) = fun code ->
            context.ch.z <| fun x ->
                this.dot (x,a,b)
                code x
        /// <summary>
        /// ベクトルの内積計算
        /// </summary>
        /// <param name="a"></param>
        /// <param name="b"></param>
        /// <param name="code">a・bに対する処理</param>
        member this.dot (a:double1,b:complex1) = fun code ->
            context.ch.z <| fun x ->
                this.dot (x,a,b)
                code x
        /// <summary>
        /// ベクトルの内積計算
        /// </summary>
        /// <param name="a"></param>
        /// <param name="b"></param>
        /// <param name="code">a・bに対する処理</param>
        member this.dot (a:complex1,b:complex1) = fun code ->
            context.ch.z <| fun x ->
                this.dot (x,a,b)
                code x

        /// <summary>
        /// ベクトルのノルム(L2ノルム)計算
        /// </summary>
        /// <param name="a">a</param>
        /// <param name="code">ノルムaに対する処理</param>
        member this.norm (a:double1) = fun code ->
            this.dot (a,a) <| fun b -> code(asm.sqrt b)
            
        /// <summary>
        /// ベクトルのノルム(L2ノルム)計算
        /// </summary>
        /// <param name="a">a</param>
        /// <param name="code">ノルムaに対する処理</param>
        member this.norm (a:complex1) = fun code ->
            this.dot (a,a) <| fun b -> code(asm.sqrt b.re)

        /// <summary>
        /// ベクトルの規格化
        /// </summary>
        /// <param name="a"></param>
        member this.normalize (a:double1) =
            this.norm a <| fun c ->
                a <== a/c
        /// <summary>
        /// ベクトルの規格化
        /// </summary>
        /// <param name="a"></param>
        member this.normalize (a:complex1) =
            this.norm a <| fun c ->
                a <== a/c

        ///<summary>連立方程式の求解</summary>
        ///<param name="matrix">係数行列</param>
        ///<param name="y">定数項ベクトル→解ベクトル</param>
        member this.solve_simuleq (matrix:complex2,y:complex1) =
            context.olist.add "-llapack"
            context.olist.add "-lblas"
            context.group.section "連立方程式の求解" <| fun () ->
                match context.language with
                |Fortran ->
                    context.ch.iii <| fun (N,b,info) ->
                        N <== matrix.size1
                        b <== 1
                        context.ch.i1 N <| fun ipiv ->
                            context.codewritein("call zgesv("+N.code+","+b.code+","+matrix.code+","+N.code+","+ipiv.code+","+y.code+","+N.code+","+info.code+")"+"\n")
                |C99 ->
                    context.ch.iii <| fun (N,b,info) ->
                        N <== matrix.size1
                        b <== 1
                        context.ch.i1 N <| fun ipiv ->
                            context.elist.add "void zgesv_(int *n, int *nrhs, double complex *a, int *lda, int *ipiv, double complex *b, int *ldb, int *info)"
                            context.codewritein("zgesv_(&"+N.code+","+"&"+b.code+","+matrix.code+",&"+N.code+","+ipiv.code+","+y.code+",&"+N.code+",&"+info.code+")"+";\n")
                |LaTeX ->
                    context.codewritein("$"+y.code+" \\leftarrow "+matrix.code+"^{-1}"+y.code+"$$\\\\\n")
                |HTML ->
                    context.codewritein("\\("+y.code+" \\leftarrow "+matrix.code+"^{-1}"+y.code+"\\)<br/>\n")
                |Python ->
                    context.codewritein(y.code+" = solve("+matrix.code+", "+y.code+")"+"\n")
                |_ -> ()
                
        ///<summary>連立方程式の求解</summary>
        ///<param name="matrix">係数行列</param>
        ///<param name="y">定数項ベクトル→解ベクトル</param>
        member this.solve_simuleq (matrix:double2,y:double1) =
            context.olist.add "-llapack"
            context.olist.add "-lblas"
            context.group.section "連立方程式の求解" <| fun () ->
                match context.language with
                |Fortran ->
                    context.ch.iii <| fun (N,b,info) ->
                        N <== matrix.size1
                        b <== 1
                        context.ch.i1 N <| fun ipiv ->
                            context.codewritein("call dgesv("+N.code+","+b.code+","+matrix.code+","+N.code+","+ipiv.code+","+y.code+","+N.code+","+info.code+")"+"\n")
                |C99 ->
                    context.ch.iii <| fun (N,b,info) ->
                        N <== matrix.size1
                        b <== 1
                        context.ch.i1 N <| fun ipiv ->
                            context.elist.add "void dgesv_(int *n, int *nrhs, double *a, int *lda, int *ipiv, double *b, int *ldb, int *info)"
                            context.codewritein("dgesv_(&"+N.code+","+"&"+b.code+","+matrix.code+",&"+N.code+","+ipiv.code+","+y.code+",&"+N.code+",&"+info.code+")"+";\n")
                |LaTeX ->
                    context.codewritein("$"+y.code+" \\leftarrow "+matrix.code+"^{-1}"+y.code+"$\\\\\n")
                |HTML ->
                    context.codewritein("\\("+y.code+" \\leftarrow "+matrix.code+"^{-1}"+y.code+"\\)<br/>\n")
                |Python ->
                    context.codewritein(y.code+" = solve("+matrix.code+", "+y.code+")"+"\n")
                |_ -> ()
                
        ///<summary>連立方程式の求解</summary>
        ///<param name="matrix">係数行列</param>
        ///<param name="y">定数項ベクトルを列方向に並べた配列→解ベクトルを列方向に並べた配列</param>
        member this.solve_simuleqs (matrix:complex2,y:complex2) =
            context.olist.add "-llapack"
            context.olist.add "-lblas"
            context.group.section "連立方程式の求解" <| fun () ->
                match context.language with
                |Fortran ->
                    context.ch.iii <| fun (N,b,info) ->
                        N <== matrix.size1
                        b <== y.size2
                        context.ch.i1 N <| fun ipiv ->
                            context.codewritein("call zgesv("+N.code+","+b.code+","+matrix.code+","+N.code+","+ipiv.code+","+y.code+","+N.code+","+info.code+")"+"\n")
                |C99 ->
                    context.ch.iii <| fun (N,b,info) ->
                        N <== matrix.size1
                        b <== y.size2
                        context.ch.i1 N <| fun ipiv ->
                            context.elist.add "void zgesv_(int *n, int *nrhs, double complex *a, int *lda, int *ipiv, double complex *b, int *ldb, int *info)"
                            context.codewritein("zgesv_(&"+N.code+","+"&"+b.code+","+matrix.code+",&"+N.code+","+ipiv.code+","+y.code+",&"+N.code+",&"+info.code+")"+";\n")
                |LaTeX ->
                    context.codewritein("$"+y.code+" \\leftarrow "+matrix.code+"^{-1}"+y.code+"$\\\\\n")
                |HTML ->
                    context.codewritein("\\("+y.code+" \\leftarrow "+matrix.code+"^{-1}"+y.code+"\\)<br/>\n")
                |Python ->
                    context.codewritein(y.code+" = solve("+matrix.code+", "+y.code+")"+"\n")
                |_ -> ()
                
        ///<summary>連立方程式の求解</summary>
        ///<param name="matrix">係数行列</param>
        ///<param name="y">定数項ベクトルを列方向に並べた配列→解ベクトルを列方向に並べた配列</param>
        member this.solve_simuleqs (matrix:double2,y:double2) =
            context.olist.add "-llapack"
            context.olist.add "-lblas"
            context.group.section "連立方程式の求解" <| fun () ->
                match context.language with
                |Fortran ->
                    context.ch.iii <| fun (N,b,info) ->
                        N <== matrix.size1
                        b <== y.size2
                        context.ch.i1 N <| fun ipiv ->
                            context.codewritein("call dgesv("+N.code+","+b.code+","+matrix.code+","+N.code+","+ipiv.code+","+y.code+","+N.code+","+info.code+")"+"\n")
                |C99 ->
                    context.ch.iii <| fun (N,b,info) ->
                        N <== matrix.size1
                        b <== y.size2
                        context.ch.i1 N <| fun ipiv ->
                            context.elist.add "void dgesv_(int *n, int *nrhs, double *a, int *lda, int *ipiv, double *b, int *ldb, int *info)"
                            context.codewritein("dgesv_(&"+N.code+","+"&"+b.code+","+matrix.code+",&"+N.code+","+ipiv.code+","+y.code+",&"+N.code+",&"+info.code+")"+";\n")
                |LaTeX ->
                    context.codewritein("$"+y.code+" \\leftarrow "+matrix.code+"^{-1}"+y.code+"$\\\\\n")
                |HTML ->
                    context.codewritein("\\("+y.code+" \\leftarrow "+matrix.code+"^{-1}"+y.code+"\\)<br/>\n")
                |Python ->
                    context.codewritein(y.code+" = solve("+matrix.code+", "+y.code+")"+"\n")
                |_ -> ()
                
        ///<summary>逆行列の計算</summary>
        ///<param name="mat1">元の行列</param>
        ///<param name="mat2">mat1の逆行列</param>
        member this.inverse_matrix (mat2:double2,mat1:double2) =
            context.olist.add "-llapack"
            context.olist.add "-lblas"
            context.group.section "逆行列の計算" <| fun () ->
                mat2.clear()
                context.iter.num mat1.size1 <| fun i -> mat2[i,i] <== 1.0
                match context.language with
                |Fortran ->
                    context.ch.ii <| fun (npre,info) ->
                        npre<==mat1.size1
                        context.ch.i1 npre <| fun ipiv ->
                            ipiv.clear()
                            context.codewritein("call zgesv("+npre.code+", "+npre.code+","+mat1.code+", "+npre.code+", "+ipiv.code+","+mat2.code+", "+npre.code+", "+info.code+")")
                |C99 ->
                    context.ch.ii <| fun (npre,info) ->
                        npre<==mat1.size1
                        context.ch.i1 npre <| fun ipiv ->
                            ipiv.clear()
                            context.elist.add "void zgesv_(int *n, int *nrhs, double complex *a, int *lda, int *ipiv, double complex *b, int *ldb, int *info)"
                            context.codewritein("zgesv_(&"+npre.code+","+"&"+npre.code+", "+mat1.code+", &"+npre.code+", "+ipiv.code+", *"+mat2.code+", &"+npre.code+", &"+info.code+");")
                |LaTeX ->
                    context.codewritein("$"+mat2.code+" \\leftarrow "+mat1.code+"^{-1}"+"$"+"\\\\\n")
                |HTML ->
                    context.codewritein("\\("+mat2.code+" \\leftarrow "+mat1.code+"^{-1}"+"\\)"+"<br/>\n")
                |Python ->
                    context.codewritein(mat2.code+" = numpy.linalg.inv("+mat1.code+")"+"\n")
                |_ -> ()
                    
        ///<summary>逆行列の計算</summary>
        ///<param name="mat1">元の行列</param>
        ///<param name="mat2">mat1の逆行列</param>
        member this.inverse_matrix (mat2:complex2,mat1:complex2) =
            context.olist.add "-llapack"
            context.olist.add "-lblas"
            context.group.section "逆行列の計算" <| fun () ->
                mat2.clear()
                context.iter.num mat1.size1 <| fun i -> mat2[i,i] <== 1.0
                match context.language with
                |Fortran ->
                    context.ch.ii <| fun (npre,info) ->
                        npre<==mat1.size1
                        context.ch.i1 npre <| fun ipiv ->
                            ipiv.clear()
                            context.codewritein("call dgesv("+npre.code+", "+npre.code+","+mat1.code+", "+npre.code+", "+ipiv.code+","+mat2.code+", "+npre.code+", "+info.code+")")
                            context.br.if1 (info .=/ 0) <| fun () -> context.print.tt <| "InvMatrix Info: "++info
                |C99 ->
                    context.ch.ii <| fun (npre,info) ->
                        npre<==mat1.size1
                        context.ch.i1 npre <| fun ipiv ->
                            ipiv.clear()
                            context.elist.add "void dgesv_(int *n, int *nrhs, double *a, int *lda, int *ipiv, double *b, int *ldb, int *info)"
                            context.codewritein("dgesv_(&"+npre.code+","+"&"+npre.code+", "+mat1.code+", &"+npre.code+", "+ipiv.code+", *"+mat2.code+", &"+npre.code+", &"+info.code+");")
                            context.br.if1 (info .=/ 0) <| fun () -> context.print.tt <| "InvMatrix Info: "++info
                |LaTeX ->
                    context.codewritein("$"+mat2.code+" \\leftarrow "+mat1.code+"^{-1}"+"$"+"\\\\\n")
                |HTML ->
                    context.codewritein("\\("+mat2.code+" \\leftarrow "+mat1.code+"^{-1}"+"\\)"+"<br/>\n")
                |Python ->
                    context.codewritein(mat2.code+" = numpy.linalg.inv("+mat1.code+")"+"\n")
                |_ -> ()

        ///<summary>行列の階数</summary>
        ///<param name="rank">行列matの階数</param>
        ///<param name="mat">行列</param>
        ///<param name="cond">0とみなす上限値</param>
        member this.rank (rank:double0,mat:complex2,cond:double0) =
            context.olist.add "-llapack"
            context.olist.add "-lblas"
            context.group.section "行列の階数" <| fun () ->
                context.ch.d1 mat.size1 <| fun s -> context.ch.z2 (mat.size1, mat.size1) <| fun u -> context.ch.z2 (mat.size1, mat.size1) <| fun vt ->
                    //特異値分解を利用
                    match context.language with
                    |Fortran ->
                        context.ch.iii <| fun (npre,info,lwork) ->
                            npre<==mat.size1
                            context.ch.i1 mat.size1 <| fun ipiv -> context.ch.d1 (5*mat.size1) <| fun rwork -> context.ch.i1 (8*mat.size1) <| fun iwork ->
                            context.ch.z1 lwork <| fun work ->
                                ipiv.clear()
                                lwork <== 2*npre+npre
                                context.codewritein("call zgesdd('N', " +
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
                                context.br.if1 (info .=/ 0) <| fun () -> context.print.tt <| "rank Info: "++info
                    |C99 ->
                        context.ch.iii <| fun (npre,info,lwork) ->
                            npre<==mat.size1
                            context.ch.i1 mat.size1 <| fun ipiv -> context.ch.d1 (5*mat.size1) <| fun rwork -> context.ch.i1 (8*mat.size1) <| fun iwork ->
                            context.ch.z1 lwork <| fun work ->
                            context.ch.c <| fun jobz ->
                                match jobz with
                                |Var(_,name,_) ->
                                    ipiv.clear()
                                    lwork <== 2*npre+npre
                                    context.elist.add "void zgesdd_(char jobz, int m, int n, double complex *a, int lda, double *s, double complex *u, int ldu, double complex *vt, int ldvt, double complex *work, int lwork, double *rwork, int *iwork, int info)"
                                    context.codewritein(name + " = 'N';")
                                    context.codewritein("zgesdd_(" +
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
                                    context.br.if1 (info .=/ 0) <| fun () -> context.print.tt <| "rank Info: "++info
                                |_ -> ()
                    |LaTeX ->
                        context.codewritein("\\("+rank.code+" \\leftarrow "+"\\mathrm{rank}\\left["+mat.code+"\\right]"+"$\\\\\n")
                    |HTML ->
                        context.codewritein("\\("+rank.code+" \\leftarrow "+"\\mathrm{rank}\\left["+mat.code+"\\right]"+"\\)<br/>\n")
                    |Python ->
                        //左特異ベクトルu.code、特異値s.code、右特異ベクトルvt.codeを求める
                        context.codewritein(u.code+","+s.code+","+vt.code+" = svd("+mat.code+")"+"\n")
                        context.codewritein "threshold = 1e-10  # ゼロの閾値\n"
                        //行列の階級rank.codeを求める
                        context.codewritein(rank.code+" = numpy.sum("+s.code+" > threshold)"+"\n")
                    |_ -> ()

        ///<summary>行列の階数</summary>
        ///<param name="rank">行列matの階数</param>
        ///<param name="mat">行列</param>
        ///<param name="cond">0とみなす上限値</param>
        member this.rank (rank:int0,mat:double2,cond:double0) =
            context.olist.add "-llapack"
            context.olist.add "-lblas"
            context.group.section "行列の階数" <| fun () ->
                context.ch.d1 mat.size1 <| fun s -> context.ch.z2 (mat.size1, mat.size1) <| fun u -> context.ch.z2 (mat.size1, mat.size1) <| fun vt ->
                    //特異値分解を利用
                    match context.language with
                    |Fortran ->
                        context.ch.iii <| fun (npre,info,lwork) ->
                            npre<==mat.size1
                            context.ch.i1 mat.size1 <| fun ipiv -> context.ch.d1 (5*mat.size1) <| fun rwork -> context.ch.i1 (8*mat.size1) <| fun iwork ->
                            context.ch.d1 lwork <| fun work ->
                                ipiv.clear()
                                lwork <== 2*npre+npre
                                context.codewritein("call dgesdd('N', " +
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
                                context.br.if1 (info .=/ 0) <| fun () -> context.print.tt <| "rank Info: "++info
                    |C99 ->
                        context.ch.iii <| fun (npre,info,lwork) ->
                            npre<==mat.size1
                            context.ch.i1 mat.size1 <| fun ipiv -> context.ch.d1 (5*mat.size1) <| fun rwork -> context.ch.i1 (8*mat.size1) <| fun iwork ->
                            context.ch.d1 lwork <| fun work ->
                            context.ch.c <| fun jobz ->
                                match jobz with
                                |Var(_,name,_) ->
                                    ipiv.clear()
                                    lwork <== 2*npre+npre
                                    context.elist.add "void dgesdd_(char jobz, int m, int n, double *a, int lda, double *s, double *u, int ldu, double *vt, int ldvt, double *work, int lwork, int *iwork, int info)"
                                    context.codewritein(name + " = 'N';")
                                    context.codewritein("dgesdd_(" +
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
                                    context.br.if1 (info .=/ 0) <| fun () -> context.print.tt <| "rank Info: "++info
                                |_ -> ()
                    |LaTeX ->
                        context.codewritein("\\("+rank.code+" \\leftarrow "+"\\mathrm{rank}\\left["+mat.code+"\\right]"+"$\\\\\n")
                    |HTML ->
                        context.codewritein("\\("+rank.code+" \\leftarrow "+"\\mathrm{rank}\\left["+mat.code+"\\right]"+"\\)<br/>\n")
                    |Python ->
                        //左特異ベクトルu.code、特異値s.code、右特異ベクトルvt.codeを求める
                        context.codewritein(u.code+","+s.code+","+vt.code+" = svd("+mat.code+")"+"\n")
                        context.codewritein "threshold = 1e-10  # ゼロの閾値\n"
                        //行列の階級rank.codeを求める
                        context.codewritein(rank.code+" = numpy.sum("+s.code+" > threshold)"+"\n")
                    |_ -> ()
                    rank.clear()
                    s.foreach <| fun i ->
                        context.br.if1 (s[i] .> cond) <| fun () -> rank.inc()

        ///<summary>疑似逆行列の計算</summary>
        ///<param name="mat2">matの疑似逆行列</param>
        ///<param name="mat">行列</param>
        ///<param name="cond">特異値を0とみなす上限値</param>
        member this.inverse_matrix2 (mat2:complex2,mat:complex2,cond:double0) =
            context.group.section "疑似逆行列" <| fun () ->
                context.ch.i <| fun ns ->
                    context.br.if2  (mat.size1.<mat.size2)
                    <| fun () ->
                        ns <== mat.size1
                    <| fun () ->
                        ns <== mat.size2
                    context.ch.d1 ns <| fun s ->
                    context.ch.z2 (mat.size1, mat.size1) <| fun u ->
                    context.ch.z2 (mat.size2, mat.size2) <| fun vt ->
                    context.ch.z2 (mat.size2, mat.size1) <| fun u2 ->
                        this.svd mat (u,s,vt)
                        //特異値分解した行列をもとに、疑似逆行列は (v^*)×(s^-1)×(u^*)
                        u2.clear()
                        context.iter.num ns <| fun i ->
                            context.iter.num u.size1 <| fun j ->
                                //condより小さい特異値は無視
                                context.br.if1 (s[i]/s[0] .> cond) <| fun () ->
                                    u2[i,j] <== asm.conj(u[j,i]) / s[i]
                        mat2.clear()
                        context.iter.num vt.size2 <| fun i ->
                            context.iter.num u2.size2  <| fun j ->
                                context.iter.num u2.size1 <| fun p ->
                                    mat2[i,j] <== mat2[i,j] + asm.conj(vt[p,i])*u2[p,j]
                                    
        ///<summary>疑似逆行列の計算</summary>
        ///<param name="mat2">matの疑似逆行列</param>
        ///<param name="mat">行列</param>
        ///<param name="cond">特異値を0とみなす上限値</param>
        member this.inverse_matrix2 (mat2:double2,mat:double2,cond:double0) =
            context.group.section "疑似逆行列" <| fun () ->
                context.ch.i <| fun ns ->
                    context.br.if2  (mat.size1.<mat.size2)
                    <| fun () ->
                        ns <== mat.size1
                    <| fun () ->
                        ns <== mat.size2
                    context.ch.d1 ns <| fun s ->
                    context.ch.d2 (mat.size1, mat.size1) <| fun u ->
                    context.ch.d2 (mat.size2, mat.size2) <| fun vt ->
                    context.ch.d2 (mat.size2, mat.size1) <| fun u2 ->
                        this.svd mat (u,s,vt)
                        //特異値分解した行列をもとに、疑似逆行列は (v^*)×(s^-1)×(u^*)
                        u2.clear()
                        context.iter.num ns <| fun i ->
                            context.iter.num u.size1 <| fun j ->
                                context.br.if1 (s[i]/s[0] .> cond) <| fun () ->
                                    u2[i,j] <== u[j,i] / s[i]
                        mat2.clear()
                        context.iter.num vt.size2 <| fun i ->
                            context.iter.num u2.size2  <| fun j ->
                                context.iter.num u2.size1 <| fun p ->
                                    mat2[i,j] <== mat2[i,j] + vt[p,i] * u2[p,j]

        /// <summary>
        /// Ax=λxの固有値λと固有ベクトルxを計算
        /// </summary>
        /// <param name="eigenvalues">固有値</param>
        /// <param name="eigenvectors">固有ベクトル</param>
        /// <param name="mat1">複素非対称行列</param>
        member this.eigen_matrix (eigenvalues:complex1,eigenvectors:complex2) (mat1:complex2) =
            context.olist.add "-llapack"
            context.olist.add "-lblas"
            context.group.section "非対称複素行列の固有値" <| fun () ->
                eigenvectors.clear()
                match context.language with
                |Fortran ->
                    context.ch.iii <| fun (npre,ldvldummy,info) ->
                            npre<==mat1.size1
                            context.ch.z2 (_1, _1) <| fun dummy ->
                                context.ch.i <| fun lwork ->
                                    lwork <== 2*npre
                                    context.ch.z1 lwork <| fun work ->
                                        context.ch.d1 (2*npre) <| fun rwork ->
                                            eigenvalues.clear()
                                            ldvldummy <== 1
                                            context.codewritein("call zgeev('No left vectors', 'Vectors (right)', "    +
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
                                            context.br.if1 (info .=/ 0) <| fun () -> context.print.tt <| "Eigenvalue Info: "++info
                |C99 ->
                    context.ch.iii <| fun (npre,ldvldummy,info) ->
                            npre<==mat1.size1
                            context.ch.z2 (_1, _1) <| fun dummy ->
                                context.ch.i <| fun lwork ->
                                    lwork <== 2*npre
                                    context.ch.z1 lwork <| fun work ->
                                        context.ch.d1 (2*npre) <| fun rwork ->
                                        context.ch.c <| fun jobvl ->
                                        context.ch.c <| fun jobvr ->
                                            match jobvl,jobvr with
                                            |Var(_,jobvl,_),Var(_,jobvr,_) ->
                                                eigenvalues.clear()
                                                ldvldummy <== 1
                                                context.elist.add "void zgeev_(char *, char *, int *, double complex *, int *, double complex *, double complex *, int *, double complex *, int *, double complex *, int *, double *, int *)"
                                                context.codewritein(jobvl + " = 'N';")
                                                context.codewritein(jobvr + " = 'V';")
                                                context.codewritein("zgeev_(" +
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
                                                context.br.if1 (info .=/ 0) <| fun () -> context.print.tt <| "Eigenvalue Info: "++info
                                            |_ -> ()
                |LaTeX ->
                    context.codewritein("Solve: $"+mat1.code+eigenvectors.code+" = "+eigenvalues.code+eigenvectors.code+"$"+"<br/>\n")
                |HTML ->
                    context.codewritein("Solve: \\("+mat1.code+eigenvectors.code+" = "+eigenvalues.code+eigenvectors.code+"\\)"+"<br/>\n")
                |Python ->
                    context.codewritein(eigenvalues.code+","+eigenvectors.code+" = eig("+mat1.code+")"+"\n")
                |_ -> ()
                
        /// <summary>
        /// Ax=λBxの固有値λと固有ベクトルxを計算
        /// </summary>
        /// <param name="eigenvalues1">λ1(λ=λ1/λ2)</param>
        /// <param name="eigenvalues2">λ2(λ=λ1/λ2)</param>
        /// <param name="eigenvectors">固有ベクトルx</param>
        /// <param name="mat1">行列A</param>
        /// <param name="mat2">行列B</param>
        member this.eigen_matrix2 (eigenvalues1:complex1,eigenvalues2:complex1,eigenvectors:complex2) (mat1:complex2) (mat2:complex2) =
                context.group.section "非対称複素行列の一般化固有値" <| fun () ->
                    context.olist.add "-llapack"
                    context.olist.add "-lblas"
                    eigenvectors.clear()
                    context.ch.iii <| fun (npre,ldvldummy,info) ->
                        npre<==mat1.size1
                        context.ch.z2 (_1, _1) <| fun dummy ->
                            context.ch.i <| fun lwork ->
                                lwork <== npre + 64 * npre
                                context.ch.z1 lwork <| fun work ->
                                    context.ch.d1 (8*npre) <| fun rwork ->
                                        eigenvalues1.clear()
                                        eigenvalues2.clear()
                                        ldvldummy <== 1
                                        match context.language with
                                        |Fortran ->
                                            context.codewritein("call zggev("+
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
                                            context.ch.c <| fun jobvl ->
                                            context.ch.c <| fun jobvr ->
                                                match jobvl,jobvr with
                                                |Var(_,jobvl,_),Var(_,jobvr,_) ->
                                                    context.elist.add "void zggev_(char *, char *, int *, double complex *, int *, double complex *, int *, double complex *, double complex *, double complex *, int *, double complex *, int *, double complex *, int *, double *, int *)"
                                                    context.codewritein(jobvl + " = 'N';")
                                                    context.codewritein(jobvr + " = 'V';")
                                                    context.codewritein("zggev_(" +
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
                                            context.codewritein("Solve: $"+mat1.code+eigenvectors.code+" = "+"\\frac{"+eigenvalues1.code+"}{"+eigenvalues2.code+"}"+mat2.code+eigenvectors.code+"$\\\\\n")
                                        |HTML ->
                                            context.codewritein("Solve: \\("+mat1.code+eigenvectors.code+" = "+"\\frac{"+eigenvalues1.code+"}{"+eigenvalues2.code+"}"+mat2.code+eigenvectors.code+"\\)<br/>\n")
                                        //Pythonのscipy.linalg.eigは、一般化固有値問題を単独の出力で処理することが可能
                                        //Pythonでは、一般化固有値の計算が単一の出力で提供されるため、ユーザーは結果を手軽に利用できる。これにより、計算過程や出力の管理がシンプルになる。
                                        //Fortranでは、二つの固有値配列を出力することで、行列 AとB の関係性を明示的に示している。この設計は、行列間の相互作用をより詳細に理解するためのもの
                                        //このコードでは、周囲と合わせるため、行列を入れ替えてeigenvalues2.codeを出している。
                                        //ちなみに一般化固有ベクトルは二つも出す必要はないので、二行目で出しているeigenvectors.code_dasokuはおまけだと思っていい。理由は以下。
                                        //一般化固有値問題 Ax=λBx の形式では、行列 B に対して左固有ベクトルが計算されることはない。したがって、一般化固有ベクトルは一意に定まることが多い。
                                        |Python ->
                                            context.codewritein(eigenvalues1.code+","+eigenvectors.code+" = eig("+mat1.code+","+mat2.code+")"+"\n")
                                            context.codewritein(eigenvalues2.code+", "+eigenvectors.code+"_dasoku = eig("+mat2.code+","+mat1.code+")"+"\n")
                                        |_ -> ()
                                        context.br.if1 (info .=/ 0) <| fun () -> context.print.tt <| "Eigenvalue Info: "++info

        /// <summary>
        /// 連立方程式の求解(Tikhonovの正則化法)
        /// </summary>
        /// <param name="fu_mat">係数行列</param>
        /// <param name="fu_cst">定数項ベクトル</param>
        /// <param name="code">解に対して行う処理</param>
        member this.solve_simuleq_t(fu_mat:double2,fu_cst:double1) = fun code ->
            context.group.h2 "連立方程式の求解(Tikhonovの正則化法)" <| fun () ->
                context.ch.d2 (fu_mat.size2, fu_mat.size2) <| fun FF ->
                context.ch.d1 fu_mat.size2 <| fun bb ->
                    let lambda = 1E-6 //正則化パラメータ
                    FF.clear()
                    //FF = fu_mat^T * fu_mat
                    context.ch.d <| fun tmp ->
                        context.iter.num FF.size1 <| fun i ->
                            context.iter.num FF.size2 <| fun j ->
                                tmp.clear()
                                context.iter.num fu_mat.size1 <| fun k ->
                                    tmp <== tmp + fu_mat[k,i]*fu_mat[k,j]
                                FF[i,j] <== tmp

                    //FF = fu_mat^T * fu_mat + λ^2 * I
                    context.iter.num FF.size1 <| fun i ->
                            FF[i,i] <== FF[i,i] + lambda * lambda
                    //bb = fu_mat^T * fu_cst
                    context.ch.d <| fun tmp ->
                        bb.clear()
                        context.iter.num bb.size1 <| fun i ->
                            tmp.clear()
                            context.iter.num fu_cst.size1 <| fun k ->
                                tmp <== tmp + fu_mat[k,i]*fu_cst[k]
                            bb[i] <== tmp
                    this.solve_simuleq(FF,bb)
                    code bb

        /// <summary>
        /// 連立方程式の求解(Tikhonovの正則化法)
        /// </summary>
        /// <param name="fu_mat">係数行列</param>
        /// <param name="fu_cst">定数項ベクトル</param>
        /// <param name="code">解に対して行う処理</param>
        member this.solve_simuleq_t(fu_mat:complex2,fu_cst:complex1) = fun code ->
            context.group.h2 "連立方程式の求解(Tikhonovの正則化法)" <| fun () ->
                context.ch.z2 (fu_mat.size2, fu_mat.size2) <| fun FF ->
                context.ch.z1 fu_mat.size2 <| fun bb ->
                    let lambda = 1E-6 //正則化パラメータ
                    FF.clear()
                    //FF = fu_mat^T * fu_mat
                    context.ch.z <| fun tmp ->
                        context.iter.num FF.size1 <| fun i ->
                            context.iter.num FF.size2 <| fun j ->
                                tmp.clear()
                                context.iter.num fu_mat.size1 <| fun k ->
                                    tmp <== tmp + fu_mat[k,i]*fu_mat[k,j]
                                FF[i,j] <== tmp

                    //FF = fu_mat^T * fu_mat + λ^2 * I
                    context.iter.num FF.size1 <| fun i ->
                            FF[i,i] <== FF[i,i] + lambda * lambda
                    //bb = fu_mat^T * fu_cst
                    context.ch.z <| fun tmp ->
                        bb.clear()
                        context.iter.num bb.size1 <| fun i ->
                            tmp.clear()
                            context.iter.num fu_cst.size1 <| fun k ->
                                tmp <== tmp + fu_mat[k,i]*fu_cst[k]
                            bb[i] <== tmp
                    this.solve_simuleq(FF,bb)
                    code bb

        /// <summary>
        /// 連立方程式の求解(Tikhonovの正則化法)
        /// </summary>
        /// <param name="fu_mat">係数行列</param>
        /// <param name="fu_cst">定数項ベクトル</param>
        /// <param name="lambda">正則化パラメータ</param>
        /// <param name="code">解に対して行う処理</param>
        member this.solve_simuleq_tt(fu_mat:complex2,fu_cst:complex1,lambda:double) = fun code ->
            context.group.h2 "連立方程式の求解(Tikhonovの正則化法)" <| fun () ->
                context.ch.z2 (fu_mat.size2, fu_mat.size2) <| fun FF ->
                context.ch.z1 fu_mat.size2 <| fun bb ->
                    //let lambda = 1E-6 //正則化パラメータ
                    FF.clear()
                    //FF = fu_mat^T * fu_mat
                    context.ch.z <| fun tmp ->
                        context.iter.num FF.size1 <| fun i ->
                            context.iter.num FF.size2 <| fun j ->
                                tmp.clear()
                                context.iter.num fu_mat.size1 <| fun k ->
                                    tmp <== tmp + asm.conj(fu_mat[k,i])*fu_mat[k,j]
                                FF[i,j] <== tmp
                    //FF = fu_mat^T * fu_mat + λ^2 * I
                    context.iter.num FF.size1 <| fun i ->
                            FF[i,i] <== FF[i,i] + lambda * lambda
                    //bb = fu_mat^T * fu_cst
                    context.ch.z <| fun tmp ->
                        bb.clear()
                        context.iter.num bb.size1 <| fun i ->
                            tmp.clear()
                            context.iter.num fu_cst.size1 <| fun k ->
                                tmp <== tmp + asm.conj(fu_mat[k,i])*fu_cst[k]
                            bb[i] <== tmp
                    this.solve_simuleq(FF,bb)
                    code(bb)

        /// <summary>
        /// 連立方程式の求解(Tikhonovの正則化法)
        /// </summary>
        /// <param name="fu_mat">係数行列</param>
        /// <param name="fu_cst">定数項ベクトル</param>
        /// <param name="lambda">正則化パラメータ</param>
        /// <param name="code">解に対して行う処理</param>
        member this.solve_simuleq_tt(fu_mat:double2,fu_cst:double1,lambda:double) = fun code ->
            context.group.h2 "連立方程式の求解(Tikhonovの正則化法)" <| fun () ->
                context.ch.d2 (fu_mat.size2, fu_mat.size2) <| fun FF ->
                context.ch.d1 fu_mat.size2 <| fun bb ->
                    //let lambda = 1E-6 //正則化パラメータ
                    FF.clear()
                    //FF = fu_mat^T * fu_mat
                    context.ch.d <| fun tmp ->
                        context.iter.num FF.size1 <| fun i ->
                            context.iter.num FF.size2 <| fun j ->
                                tmp.clear()
                                context.iter.num fu_mat.size1 <| fun k ->
                                    tmp <== tmp + fu_mat[k,i]*fu_mat[k,j]
                                FF[i,j] <== tmp
                    //FF = fu_mat^T * fu_mat + λ^2 * I
                    context.iter.num FF.size1 <| fun i ->
                            FF[i,i] <== FF[i,i] + lambda * lambda
                    //bb = fu_mat^T * fu_cst
                    context.ch.d <| fun tmp ->
                        bb.clear()
                        context.iter.num bb.size1 <| fun i ->
                            tmp.clear()
                            context.iter.num fu_cst.size1 <| fun k ->
                                tmp <== tmp + fu_mat[k,i]*fu_cst[k]
                            bb[i] <== tmp
                    this.solve_simuleq(FF,bb)
                    code bb

        /// <summary>
        /// 連立方程式の求解(Tikhonovの正則化法)
        /// </summary>
        /// <param name="fu_mat">係数行列</param>
        /// <param name="fu_cst">定数項ベクトル(列サイズ=1)</param>
        /// <param name="lambda">正則化パラメータ</param>
        /// <param name="code">解に対して行う処理</param>
        member this.solve_simuleq_tt2(fu_mat:complex2,fu_cst:complex2,lambda:double0) code =
            context.group.h2 "連立方程式の求解(Tikhonovの正則化法)" <| fun () ->
                context.ch.z2 (fu_mat.size2, fu_mat.size2) <| fun FF ->
                context.ch.z1 fu_mat.size2 <| fun bb ->
                    FF.clear()
                    //FF = fu_mat^T * fu_mat
                    context.ch.z <| fun tmp ->
                        context.iter.num FF.size1 <| fun i ->
                            context.iter.num FF.size2 <| fun j ->
                                tmp.clear()
                                context.iter.num fu_mat.size1 <| fun k ->
                                    tmp <== tmp + asm.conj(fu_mat[k,i])*fu_mat[k,j]
                                FF[i,j] <== tmp
                    //FF = fu_mat^T * fu_mat + λ^2 * I
                    context.iter.num FF.size1 <| fun i ->
                            FF[i,i] <== FF[i,i] + lambda * lambda
                    //bb = fu_mat^T * fu_cst
                    context.ch.z <| fun tmp ->
                        bb.clear()
                        context.iter.num bb.size1 <| fun i ->
                            tmp.clear()
                            context.iter.num fu_cst.size1 <| fun k ->
                                tmp <== tmp + asm.conj(fu_mat[k,i])*fu_cst[k,1]
                            bb[i] <== tmp
                    this.solve_simuleq(FF,bb)
                    code bb

        /// <summary>
        /// 行列式の常用対数を計算
        /// </summary>
        /// <param name="matrix">行列</param>
        /// <param name="code">行列式の値を用いて実行するコード</param>
        member this.determinant (matrix:complex2) = fun code ->
            context.olist.add "-llapack"
            context.olist.add "-lblas"
            context.group.section "行列式の常用対数を計算" <| fun () ->
                context.ch.d <| fun d ->
                    match context.language with
                    |Fortran ->
                        context.ch.iid <| fun (N,info,d) ->
                            N <== matrix.size1
                            context.ch.i1 N <| fun ipiv ->
                                context.codewritein("call zgetrf("+N.code+","+N.code+","+matrix.code+","+N.code+","+ipiv.code+","+info.code+")"+"\n")
                    |C99 ->
                        context.ch.iid <| fun (N,info,d) ->
                            N <== matrix.size1
                            context.ch.i1 N <| fun ipiv ->
                                context.elist.add "void zgetrf_(int *, int *, double complex *, int *, int *, int *)"
                                context.codewritein("zgetrf_(&"+N.code+","+"&"+N.code+","+matrix.code+",&"+N.code+","+ipiv.code+",&"+info.code+")"+";\n")
                    |LaTeX ->
                        context.codewritein("$"+d.code+" = "+"\\left|"+matrix.code+"\\right|"+"$"+"\\\\\n")
                    |HTML ->
                        context.codewritein("\\("+d.code+" = "+"\\left|"+matrix.code+"\\right|"+"\\)"+"<br/>\n")
                    |Python ->
                        //LU分解
                        context.codewritein("P ,L ,U = lu("+matrix.code+")"+"\n")
                        //上三角行列 U の対角成分の積を計算
                        context.codewritein("det_U = numpy.prod(numpy.diag(U))"+"\n")
                        //行列式を計算
                        //pの行列式は、ピボット行列の行交換の回数で符号が決まる
                        context.codewritein("sign = (-1) ** numpy.sum(numpy.arange("+matrix.code+".shape[0]) != numpy.argsort(numpy.argsort(P[:, 0])))"+"\n")
                        context.codewritein("det_"+matrix.code+" = sign * det_U"+"\n")
                        //行列式の常用対数を計算
                        context.codewritein(d.code+" = numpy.log10(det_"+matrix.code+")"+"\n")
                    |_ -> ()
                    d.clear()
                    context.iter.num matrix.size1 <| fun i ->
                        d <== d + asm.log10(asm.abs(matrix[i,i]))
                    code d

        /// <summary>
        /// 行列式の常用対数を計算
        /// </summary>
        /// <param name="matrix">行列</param>
        /// <param name="code">行列式の値を用いて実行するコード</param>
        member this.determinant (matrix:double2) = fun code ->
            context.olist.add "-llapack"
            context.olist.add "-lblas"
            context.group.section "行列式の常用対数を計算" <| fun () ->
                context.ch.d <| fun d ->
                    match context.language with
                    |Fortran ->
                        context.ch.iid <| fun (N,info,d) ->
                            N <== matrix.size1
                            context.ch.i1 N <| fun ipiv ->
                                context.codewritein("call dgetrf("+N.code+","+N.code+","+matrix.code+","+N.code+","+ipiv.code+","+info.code+")"+"\n")
                    |C99 ->
                        context.ch.iid <| fun (N,info,d) ->
                            N <== matrix.size1
                            context.ch.i1 N <| fun ipiv ->
                                context.elist.add "void dgetrf_(int *, int *, double complex *, int *, int *, int *)"
                                context.codewritein("dgetrf_(&"+N.code+","+"&"+N.code+","+matrix.code+",&"+N.code+","+ipiv.code+",&"+info.code+")"+";\n")
                    |LaTeX ->
                        context.codewritein("$"+d.code+" = "+"\\left|"+matrix.code+"\\right|"+"$"+"\\\\\n")
                    |HTML ->
                        context.codewritein("\\("+d.code+" = "+"\\left|"+matrix.code+"\\right|"+"\\)"+"<br/>\n")
                    |Python ->
                        //LU分解
                        context.codewritein("P ,L ,U = lu("+matrix.code+")"+"\n")
                        //上三角行列 U の対角成分の積を計算
                        context.codewritein("det_U = numpy.prod(numpy.diag(U))"+"\n")
                        //行列式を計算
                        //pの行列式は、ピボット行列の行交換の回数で符号が決まる
                        context.codewritein("sign = (-1) ** numpy.sum(numpy.arange("+matrix.code+".shape[0]) != numpy.argsort(numpy.argsort(P[:, 0])))"+"\n")
                        context.codewritein("det_"+matrix.code+" = sign * det_U"+"\n")
                        //行列式の常用対数を計算
                        context.codewritein(d.code+" = np.log10(np.abs(det_"+matrix.code+"))"+"\n")
                    |_ -> ()
                    d.clear()
                    context.iter.num matrix.size1 <| fun i ->
                        d <== d + asm.log10(asm.abs(matrix[i,i]))
                    code d

        /// <summary>
        /// mat = u * s * v に特異値分解
        /// </summary>
        /// <param name="mat1">複素行列</param>
        /// <param name="u">複素行列u</param>
        /// <param name="s">正方行列sの対角成分</param>
        /// <param name="vt">複素行列vの転置</param>
        member this.svd (mat1:complex2) = fun (u:complex2,s:double1,vt:complex2) ->
            context.olist.add "-llapack"
            context.olist.add "-lblas"
            match context.language with
            |LaTeX ->
                context.codewritein("$"+mat1.code+" = "+u.code+s.code+vt.code+"^{\\mathrm{T}}"+"$\\\\\n")
            |HTML ->
                context.codewritein("\\("+mat1.code+" = "+u.code+s.code+vt.code+"^{\\mathrm{T}}"+"\\)<br/>\n")
            |Python ->
                //左特異ベクトルu.code、特異値s.code、右特異ベクトルvt.codeを求める
                context.codewritein(u.code+","+s.code+","+vt.code+" = svd("+mat1.code+")"+"\n")
            |_ ->
                context.group.section "非対称複素行列の特異値分解" <| fun () ->
                    s.clear()
                    u.clear()
                    vt.clear()
                    match context.language with
                    |Fortran ->
                        context.ch.iiii <| fun (m,n,lda,info) ->
                        context.ch.i <| fun ns ->
                            m <== mat1.size1
                            n <== mat1.size2
                            context.br.if2 (m.<n) (fun () -> ns <== m) (fun () -> ns <== n)
                            context.ch.ii <| fun (ldu,ldvt) ->
                            context.ch.i <| fun lwork ->
                                context.ch.z01 <| fun work ->
                                context.ch.d1 (5*ns) <| fun rwork ->
                                    lda <== m
                                    ldu <== u.size1
                                    ldvt <== vt.size2
                                    lwork <== -1
                                    work.allocate 1
                                    context.codewritein("call zgesvd("+
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
                                    context.codewritein("call zgesvd("+
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
                        context.ch.iiii <| fun (m,n,lda,info) ->
                        context.ch.i <| fun ns ->
                            m <== mat1.size1
                            n <== mat1.size2
                            context.br.if2 (m.<n) (fun () -> ns <== m) (fun () -> ns <== n)
                            context.ch.ii <| fun (ldu,ldvt) ->
                            context.ch.i <| fun lwork ->
                                context.ch.z01 <| fun work ->
                                context.ch.z <| fun wkopt ->
                                context.ch.d1 (5*ns) <| fun rwork ->
                                context.ch.c <| fun jobu ->
                                context.ch.c <| fun jobv ->
                                    match jobu,jobv with
                                    |Var(_,jobu,_),Var(_,jobv,_) ->
                                        lda <== m
                                        ldu <== u.size1
                                        ldvt <== vt.size2
                                        lwork <== -1
                                        context.elist.add "void zgesvd_(char *, char *, int *, int *, double complex *, int *, double *, double complex *, int *, double complex *, int *, double complex *, int *, double *, int *)"
                                        context.codewritein(jobu + " = 'A';")
                                        context.codewritein(jobv + " = 'A';")
                                        context.codewritein("zgesvd_(" +
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
                                        context.codewritein("zgesvd_(" +
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
                        context.codewritein(u.code+","+s.code+","+vt.code+" = svd("+mat1.code+")"+"\n")
                    |_ ->
                        ()

        /// <summary>
        /// mat = u * s * v に特異値分解
        /// </summary>
        /// <param name="mat1">複素行列</param>
        /// <param name="u">複素行列u</param>
        /// <param name="s">正方行列sの対角成分</param>
        /// <param name="vt">複素行列vの転置</param>
        member this.svd (mat1:double2) = fun (u:double2,s:double1,vt:double2) ->
            context.olist.add "-llapack"
            context.olist.add "-lblas"
            match context.language with
            |LaTeX ->
                context.codewritein("$"+mat1.code+" = "+u.code+s.code+vt.code+"^{\\mathrm{T}}"+"$\\\\\n")
            |HTML ->
                context.codewritein("\\("+mat1.code+" = "+u.code+s.code+vt.code+"^{\\mathrm{T}}"+"\\)<br/>\n")
            |Python ->
                //左特異ベクトルu.code、特異値s.code、右特異ベクトルvt.codeを求める
                context.codewritein(u.code+","+s.code+","+vt.code+" = svd("+mat1.code+")"+"\n")
            |_ ->
                context.group.section "非対称実行列の特異値分解" <| fun () ->
                    s.clear()
                    u.clear()
                    vt.clear()
                    match context.language with
                    |Fortran ->
                        context.ch.iiii <| fun (m,n,lda,info) ->
                            m <== mat1.size1
                            n <== mat1.size2
                            context.ch.ii <| fun (ldu,ldvt) ->
                            context.ch.i <| fun lwork ->
                                context.ch.d01 <| fun work ->
                                    lda <== m
                                    ldu <== u.size1
                                    ldvt <== vt.size2
                                    lwork <== -1
                                    work.allocate 1
                                    context.codewritein("call dgesvd("+
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
                                    context.codewritein("call dgesvd("+
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
                        context.ch.iiii <| fun (m,n,lda,info) ->
                            m <== mat1.size1
                            n <== mat1.size2
                            context.ch.ii <| fun (ldu,ldvt) ->
                            context.ch.i <| fun lwork ->
                                context.ch.d01 <| fun work ->
                                context.ch.c <| fun jobu ->
                                context.ch.c <| fun jobv ->
                                    match jobu,jobv with
                                    |Var(_,jobu,_),Var(_,jobv,_) ->
                                        lda <== m
                                        ldu <== u.size1
                                        ldvt <== vt.size2
                                        context.elist.add "void dgesvd_(char *, char *, int *, int *, double *, int *, double *, double *, int *, double *, int *, double *, int *, int *)"
                                        context.codewritein(jobu + " = 'A';")
                                        context.codewritein(jobv + " = 'A';")
                                        lwork <== -1
                                        work.allocate 1
                                        context.codewritein("dgesvd_(" +
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
                                        context.codewritein("dgesvd_(" +
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
                        context.codewritein(u.code+","+s.code+","+vt.code+" = svd("+mat1.code+")"+"\n")
                    |_ ->
                        ()

        /// <summary>
        /// 連立同次方程式を求解
        /// </summary>
        /// <param name="mat">複素係数行列</param>
        /// <param name="f">連立方程式の解</param>
        member this.solve_homogeneq (mat:double2,f:double1) =
                context.ch.d1 mat.size1 <| fun s ->
                context.ch.d2 (mat.size1, mat.size2) <| fun u ->
                context.ch.d2 (mat.size1, mat.size2) <| fun vt ->
                    this.svd mat (u,s,vt)
                    context.group.comment "0に近いほど正確な解"
                    context.print.tt <| "solve_homogeneq"++s[mat.size1]
                    context.iter.num mat.size1 <| fun i ->
                        f[i] <== vt[mat.size1,i]

        /// <summary>
        /// 連立同次方程式を求解
        /// </summary>
        /// <param name="mat">複素係数行列</param>
        /// <param name="f">連立方程式の解</param>
        member this.solve_homogeneq (mat:complex2,f:complex1) =
                context.ch.d1 mat.size1 <| fun s ->
                context.ch.z2 (mat.size1, mat.size2) <| fun u ->
                context.ch.z2 (mat.size1, mat.size2) <| fun vt ->
                    this.svd mat (u,s,vt)
                    context.group.comment "0に近いほど正確な解"
                    context.print.tt <| "solve_homogeneq"++s[mat.size1]
                    context.iter.num mat.size1 <| fun i ->
                        f[i] <== asm.conj(vt[mat.size1,i])

    [<AutoOpen>]
    module CompilationEnvironmentLaExtensions =
        type Aqualis with
            ///<summary>線形代数ライブラリ</summary>
            member this.la = ContextLa(this)
