(*
Copyright (c) 2022 Jun-ichiro Sugisaka

This software is released under the MIT License.
http://opensource.org/licenses/mit-license.php
*)
namespace Aqualis

    open Aqualis_base
    
    module lapack = 

        type La() =
            /// <summary>
            /// 行列×ベクトルの計算
            /// </summary>
            /// <param name="x">計算結果</param>
            static member matmul (x:num1) = fun (a:num2,b:num1) ->
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
                ch.n1 (a,a.size1) <| fun x ->
                    La.matmul x (a,b)
                    code x
                    
            /// <summary>
            /// 行列×行列の計算
            /// </summary>
            /// <param name="u">計算結果</param>
            static member matmul (u:num2) = fun (a:num2,b:num2) ->
                    u.clear()
                    iter.num a.size1 <| fun i ->
                        iter.num b.size2 <| fun j ->
                            iter.num a.size2 <| fun k ->
                                u.[i,j] <== u.[i,j] + a.[i,k] * b.[k,j]
                                
            /// <summary>
            /// 行列a×行列bの計算
            /// </summary>
            /// <param name="a"></param>
            /// <param name="b"></param>
            static member matmul (a:num2,b:num2) = fun code ->
                ch.n2 (a,a.size1,b.size2) <| fun u ->
                    La.matmul u (a,b)
                    code u
                    
            /// <summary>
            /// ベクトルの内積計算
            /// </summary>
            /// <param name="a"></param>
            /// <param name="b"></param>
            static member dot (x:num0) = fun (a:num1,b:num1) ->
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
            static member dot (a:num1,b:num1) = fun code ->
                ch.n a <| fun x ->
                    La.dot x (a,b)
                    code x
                    
            /// <summary>
            /// ベクトルのノルム計算
            /// </summary>
            /// <param name="a"></param>
            /// <param name="code"></param>
            static member norm (a:num1) code =
                La.dot (a,a) code
                
            /// <summary>
            /// ベクトルの規格化
            /// </summary>
            /// <param name="a"></param>
            static member normalize (a:num1) =
                La.dot (a,a) <| fun c ->
                    a <== a/asm.sqrt(c)
                    
        ///<summary>連立方程式の求解</summary>
        ///<param name="matrix">係数行列</param>
        ///<param name="y">定数項ベクトル→解ベクトル</param>
        let solve_simuleq (matrix:num2,y:num1) =
            p.param.option_("-llapack")
            p.param.option_("-lblas")
            codestr.section "連立方程式の求解" <| fun () ->
                ch.iii <| fun (N,b,info) ->
                    N <== matrix.size1
                    b <== 1
                    ch.i1 N <| fun ipiv ->
                    tbinder.z matrix <| fun () ->
                        match p.lang with
                          |F -> 
                            p.param.codewrite("call zgesv("+N.name+","+b.name+","+matrix.name+","+N.name+","+ipiv.name+","+y.name+","+N.name+","+info.name+")"+"\n")
                          |C89 -> 
                            p.param.extern_("void zgesv_(int *n, int *nrhs, doublecomplex *a, int *lda, int *ipiv, doublecomplex *b, int *ldb, int *info)")
                            p.param.codewrite("zgesv_(&"+N.name+","+"&"+b.name+","+matrix.name+",&"+N.name+","+ipiv.name+","+y.name+",&"+N.name+",&"+info.name+")"+";\n")
                          |C99 -> 
                            p.param.extern_("void zgesv_(int *n, int *nrhs, double complex *a, int *lda, int *ipiv, double complex *b, int *ldb, int *info)")
                            p.param.codewrite("zgesv_(&"+N.name+","+"&"+b.name+","+matrix.name+",&"+N.name+","+ipiv.name+","+y.name+",&"+N.name+",&"+info.name+")"+";\n")
                          |T -> 
                            p.param.codewrite("zgesv("+N.name+","+b.name+","+matrix.name+","+N.name+","+ipiv.name+","+y.name+","+N.name+","+info.name+")"+"\n")
                          |H -> 
                            p.param.codewrite("<math>"+y.name+"<mo>&larr;</mo>"+"<msup>"+matrix.name+"<mn>-1</mn></msup>"+y.name+"</math>"+"\n<br/>\n")
                          |NL -> 
                            ()
                    tbinder.d matrix <| fun () ->
                        match p.lang with
                          |F -> 
                            p.param.codewrite("call dgesv("+N.name+","+b.name+","+matrix.name+","+N.name+","+ipiv.name+","+y.name+","+N.name+","+info.name+")"+"\n")
                          |C89 -> 
                            p.param.extern_("void dgesv_(int *n, int *nrhs, double *a, int *lda, int *ipiv, double *b, int *ldb, int *info)")
                            p.param.codewrite("dgesv_(&"+N.name+","+"&"+b.name+","+matrix.name+",&"+N.name+","+ipiv.name+","+y.name+",&"+N.name+",&"+info.name+")"+";\n")
                          |C99 -> 
                            p.param.extern_("void dgesv_(int *n, int *nrhs, double *a, int *lda, int *ipiv, double *b, int *ldb, int *info)")
                            p.param.codewrite("dgesv_(&"+N.name+","+"&"+b.name+","+matrix.name+",&"+N.name+","+ipiv.name+","+y.name+",&"+N.name+",&"+info.name+")"+";\n")
                          |T -> 
                            p.param.codewrite("dgesv("+N.name+","+b.name+","+matrix.name+","+N.name+","+ipiv.name+","+y.name+","+N.name+","+info.name+")"+"\n")
                          |H -> 
                            p.param.codewrite("<math>"+y.name+"<mo>&larr;</mo>"+"<msup>"+matrix.name+"<mn>-1</mn></msup>"+y.name+"</math>"+"\n<br/>\n")
                          |NL -> 
                            ()
                            
        ///<summary>連立方程式の求解</summary>
        ///<param name="matrix">係数行列</param>
        ///<param name="y">定数項ベクトルを列方向に並べた配列→解ベクトルを列方向に並べた配列</param>
        let solve_simuleqs (matrix:num2,y:num2) =
            p.param.option_("-llapack")
            p.param.option_("-lblas")
            codestr.section "連立方程式の求解" <| fun () ->
                ch.iii <| fun (N,b,info) ->
                    N <== matrix.size1
                    b <== y.size2
                    ch.i1 N <| fun ipiv ->
                        tbinder.z matrix <| fun () ->
                            match p.lang with
                              |F -> 
                                p.param.codewrite("call zgesv("+N.name+","+b.name+","+matrix.name+","+N.name+","+ipiv.name+","+y.name+","+N.name+","+info.name+")"+"\n")
                              |C89 -> 
                                p.param.extern_("void zgesv_(int *n, int *nrhs, doublecomplex *a, int *lda, int *ipiv, doublecomplex *b, int *ldb, int *info)")
                                p.param.codewrite("zgesv_(&"+N.name+","+"&"+b.name+","+matrix.name+",&"+N.name+","+ipiv.name+","+y.name+",&"+N.name+",&"+info.name+")"+";\n")
                              |C99 -> 
                                p.param.extern_("void zgesv_(int *n, int *nrhs, double complex *a, int *lda, int *ipiv, double complex *b, int *ldb, int *info)")
                                p.param.codewrite("zgesv_(&"+N.name+","+"&"+b.name+","+matrix.name+",&"+N.name+","+ipiv.name+","+y.name+",&"+N.name+",&"+info.name+")"+";\n")
                              |T -> 
                                p.param.codewrite("zgesv("+N.name+","+b.name+","+matrix.name+","+N.name+","+ipiv.name+","+y.name+","+N.name+","+info.name+")"+"\n")
                              |H -> 
                                p.param.codewrite("<math>"+y.name+"<mo>&larr;</mo>"+"<msup>"+matrix.name+"<mn>-1</mn></msup>"+y.name+"</math>"+"\n<br/>\n")
                              |NL -> 
                                ()
                        tbinder.d matrix <| fun () ->
                            match p.lang with
                              |F -> 
                                p.param.codewrite("call dgesv("+N.name+","+b.name+","+matrix.name+","+N.name+","+ipiv.name+","+y.name+","+N.name+","+info.name+")"+"\n")
                              |C89 -> 
                                p.param.extern_("void dgesv_(int *n, int *nrhs, double *a, int *lda, int *ipiv, double *b, int *ldb, int *info)")
                                p.param.codewrite("dgesv_(&"+N.name+","+"&"+b.name+","+matrix.name+",&"+N.name+","+ipiv.name+","+y.name+",&"+N.name+",&"+info.name+")"+";\n")
                              |C99 -> 
                                p.param.extern_("void dgesv_(int *n, int *nrhs, double *a, int *lda, int *ipiv, double *b, int *ldb, int *info)")
                                p.param.codewrite("dgesv_(&"+N.name+","+"&"+b.name+","+matrix.name+",&"+N.name+","+ipiv.name+","+y.name+",&"+N.name+",&"+info.name+")"+";\n")
                              |T -> 
                                p.param.codewrite("dgesv("+N.name+","+b.name+","+matrix.name+","+N.name+","+ipiv.name+","+y.name+","+N.name+","+info.name+")"+"\n")
                              |H -> 
                                p.param.codewrite("<math>"+y.name+"<mo>&larr;</mo>"+"<msup>"+matrix.name+"<mn>-1</mn></msup>"+y.name+"</math>"+"\n<br/>\n")
                              |NL -> 
                                ()
                                
        ///<summary>逆行列の計算</summary>
        ///<param name="mat1">元の行列</param>
        ///<param name="mat2">mat1の逆行列</param>
        let inverse_matrix (mat2:num2) (mat1:num2) =
            p.param.option_("-llapack")
            p.param.option_("-lblas")
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
                                p.param.codewrite("call zgesv("+npre.name+", "+npre.name+","+mat1.name+", "+npre.name+", "+ipiv.name+","+mat2.name+", "+npre.name+", "+info.name+")")
                              |C89 -> 
                                p.param.extern_("void zgesv_(int *n, int *nrhs, doublecomplex *a, int *lda, int *ipiv, doublecomplex *b, int *ldb, int *info)")
                                p.param.codewrite("zgesv_(&"+npre.name+","+"&"+npre.name+", "+mat1.name+", &"+npre.name+", "+ipiv.name+", *"+mat2.name+", &"+npre.name+", &"+info.name+");")
                              |C99 -> 
                                p.param.extern_("void zgesv_(int *n, int *nrhs, double complex *a, int *lda, int *ipiv, double complex *b, int *ldb, int *info)")
                                p.param.codewrite("zgesv_(&"+npre.name+","+"&"+npre.name+", "+mat1.name+", &"+npre.name+", "+ipiv.name+", *"+mat2.name+", &"+npre.name+", &"+info.name+");")
                              |T -> 
                                p.param.codewrite("call zgesv("+npre.name+", "+npre.name+","+mat1.name+", "+npre.name+", "+ipiv.name+","+mat2.name+", "+npre.name+", "+info.name+")")
                              |H -> 
                                p.param.codewrite("<math>"+mat2.name+"<mo>&larr;</mo>"+"<msup>"+mat1.name+"<mn>-1</mn></msup>"+"</math>"+"\n<br/>\n")
                              |NL -> 
                                ()
                        tbinder.d mat2 <| fun () ->
                            match p.lang with
                              |F -> 
                                p.param.codewrite("call dgesv("+npre.name+", "+npre.name+","+mat1.name+", "+npre.name+", "+ipiv.name+","+mat2.name+", "+npre.name+", "+info.name+")")
                              |C89 -> 
                                p.param.extern_("void dgesv_(int *n, int *nrhs, double *a, int *lda, int *ipiv, double *b, int *ldb, int *info)")
                                p.param.codewrite("dgesv_(&"+npre.name+","+"&"+npre.name+", "+mat1.name+", &"+npre.name+", "+ipiv.name+", *"+mat2.name+", &"+npre.name+", &"+info.name+");")
                              |C99 -> 
                                p.param.extern_("void dgesv_(int *n, int *nrhs, double *a, int *lda, int *ipiv, double *b, int *ldb, int *info)")
                                p.param.codewrite("dgesv_(&"+npre.name+","+"&"+npre.name+", "+mat1.name+", &"+npre.name+", "+ipiv.name+", *"+mat2.name+", &"+npre.name+", &"+info.name+");")
                              |T -> 
                                p.param.codewrite("call dgesv("+npre.name+", "+npre.name+","+mat1.name+", "+npre.name+", "+ipiv.name+","+mat2.name+", "+npre.name+", "+info.name+")")
                              |H -> 
                                p.param.codewrite("<math>"+mat2.name+"<mo>&larr;</mo>"+"<msup>"+mat1.name+"<mn>-1</mn></msup>"+"</math>"+"\n<br/>\n")
                              |NL -> 
                                ()
                        br.if1 (info .=/ 0) <| fun () -> print.s[!.("InvMatrix Info: ");info]
                        
        ///<summary>行列の階数</summary>
        let rank (rank:num0) (mat:num2) (cond:num0) =
            p.param.option_("-llapack")
            p.param.option_("-lblas")
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
                                p.param.codewrite("call zgesdd('N', " + npre.name + "," + " " + npre.name + ","  + mat.name+", "  + npre.name + ", "  + s.name + ","   + u.name + ", "  + npre.name + ","   + vt.name + ", "  + npre.name + ", "  + work.name + ", "  + lwork.name + ","   + rwork.name + ","   + iwork.name + ", "  + info.name + ")")
                              |C89 -> 
                                ch.t <| fun x ->
                                    p.param.extern_("void zgesdd_(char jobz, int m, int n, doublecomplex *a, int lda, double *s, doublecomplex *u, int ldu, doublecomplex *vt, int ldvt, doublecomplex *work, int lwork, double *rwork, int *iwork, int info)")
                                    p.param.codewrite(x+" = 'N';")
                                    p.param.codewrite("zgesdd_(&"+x+", &" +  npre.name + "," + "&" + npre.name + ", " + mat.name+", &" + npre.name + ", *" + s.name + ", *" + u.name + ", &" + npre.name + ", *" + vt.name + ", &" + npre.name + ", &" + work.name + ", &" + lwork.name + ", *" + rwork.name + ", *" + iwork.name + ", &" + info.name + ");")
                              |C99 -> 
                                ch.t <| fun x ->
                                    p.param.extern_("void zgesdd_(char jobz, int m, int n, double complex *a, int lda, double *s, double complex *u, int ldu, double complex *vt, int ldvt, double complex *work, int lwork, double *rwork, int *iwork, int info)")
                                    p.param.codewrite(x+" = 'N';")
                                    p.param.codewrite("zgesdd_(&"+x+", &" +  npre.name + "," + "&" + npre.name + ", " + mat.name+", &" + npre.name + ", *" + s.name + ", *" + u.name + ", &" + npre.name + ", *" + vt.name + ", &" + npre.name + ", &" + work.name + ", &" + lwork.name + ", *" + rwork.name + ", *" + iwork.name + ", &" + info.name + ");")
                              |T -> 
                                p.param.codewrite("call zgesdd('N', " + npre.name + "," + " " + npre.name + ","  + mat.name+", "  + npre.name + ", "  + s.name + ","   + u.name + ", "  + npre.name + ","   + vt.name + ", "  + npre.name + ", "  + work.name + ", "  + lwork.name + ","   + rwork.name + ","   + iwork.name + ", "  + info.name + ")")
                              |H -> 
                                p.param.codewrite("<math>"+rank.name+"<mo>&larr;</mo>"+"<mi>rank</mi><mo>[</mo>"+mat.name+"<mo>]</mo></math>"+"\n<br/>\n")
                              |NL -> 
                                ()
                        tbinder.d mat <| fun () ->
                            ch.d1 lwork <| fun work ->
                            //特異値分解を利用
                            match p.lang with
                              |F -> 
                                p.param.codewrite("call dgesdd('N', " + npre.name + "," + " " + npre.name + ","  + mat.name+", "  + npre.name + ", "  + s.name + ","   + u.name + ", "  + npre.name + ","   + vt.name + ", "  + npre.name + ", "  + work.name + ", "  + lwork.name + ","   + rwork.name + ","   + iwork.name + ", "  + info.name + ")")
                              |C89 -> 
                                ch.t <| fun x ->
                                    p.param.extern_("void dgesdd_(char jobz, int m, int n, double *a, int lda, double *s, double *u, int ldu, double *vt, int ldvt, double *work, int lwork, int *iwork, int info)")
                                    p.param.codewrite(x+" = 'N';")
                                    p.param.codewrite("dgesdd_(&"+x+", &" +  npre.name + "," + "&" + npre.name + ", " + mat.name+", &" + npre.name + ", *" + s.name + ", *" + u.name + ", &" + npre.name + ", *" + vt.name + ", &" + npre.name + ", &" + work.name + ", &" + lwork.name + ", *" + rwork.name + ", *" + iwork.name + ", &" + info.name + ");")
                              |C99 -> 
                                ch.t <| fun x ->
                                    p.param.extern_("void dgesdd_(char jobz, int m, int n, double *a, int lda, double *s, double *u, int ldu, double *vt, int ldvt, double *work, int lwork, int *iwork, int info)")
                                    p.param.codewrite(x+" = 'N';")
                                    p.param.codewrite("dgesdd_(&"+x+", &" +  npre.name + "," + "&" + npre.name + ", " + mat.name+", &" + npre.name + ", *" + s.name + ", *" + u.name + ", &" + npre.name + ", *" + vt.name + ", &" + npre.name + ", &" + work.name + ", &" + lwork.name + ", *" + rwork.name + ", *" + iwork.name + ", &" + info.name + ");")
                              |T -> 
                                p.param.codewrite("call dgesdd('N', " + npre.name + "," + " " + npre.name + ","  + mat.name+", "  + npre.name + ", "  + s.name + ","   + u.name + ", "  + npre.name + ","   + vt.name + ", "  + npre.name + ", "  + work.name + ", "  + lwork.name + ","   + rwork.name + ","   + iwork.name + ", "  + info.name + ")")
                              |H -> 
                                p.param.codewrite("<math>"+rank.name+"<mo>&larr;</mo>"+"<mi>rank</mi><mo>[</mo>"+mat.name+"<mo>]</mo></math>"+"\n<br/>\n")
                              |NL -> 
                                ()
                        br.if1 (info .=/ 0) <| fun () -> print.s[!.("rank Info: ");info]
                        rank.clear()
                        iter.array s <| fun i -> 
                            br.if1 (s.[i].>cond) <| fun () -> rank.increment()
        
        let inverse_matrix2 (mat2:num2) (mat:num2) (cond:num0) =
            p.param.option_("-llapack")
            p.param.option_("-lblas")
            codestr.section "疑似逆行列" <| fun () ->
                ch.iii <| fun (npre,info,lwork) ->
                    npre<==mat.size1
                    ch.i1 npre <| fun ipiv -> ch.d1 npre <| fun s -> ch.z2 npre npre <| fun u -> ch.z2 npre npre <| fun u2 -> ch.z2 npre npre <| fun vt -> ch.d1 (npre*(5*npre+7)) <| fun rwork -> ch.i1 (8*npre) <| fun iwork -> 
                        ipiv.clear()
                        lwork <== npre*npre+2*npre+npre
                        tbinder.z mat <| fun () ->
                            ch.z1 lwork <| fun work ->
                            match p.lang with
                              |F -> 
                                p.param.codewrite("call zgesdd('A', " + npre.name + "," + " " + npre.name + ","  + mat.name+", "  + npre.name + ", "  + s.name + ","   + u.name + ", "  + npre.name + ","   + vt.name + ", "  + npre.name + ", "  + work.name + ", "  + lwork.name + ","   + rwork.name + ","   + iwork.name + ", "  + info.name + ")")
                              |C89 -> 
                                ch.t <| fun x ->
                                    p.param.extern_("void zgesdd_(char jobz, int m, int n, doublecomplex *a, int lda, double *s, doublecomplex *u, int ldu, doublecomplex *vt, int ldvt, doublecomplex *work, int lwork, double *rwork, int *iwork, int info)")
                                    p.param.codewrite(x+" = 'A';")
                                    p.param.codewrite("zgesdd_(&"+x+", &" +  npre.name + "," + "&" + npre.name + ", " + mat.name+", &" + npre.name + ", *" + s.name + ", *" + u.name + ", &" + npre.name + ", *" + vt.name + ", &" + npre.name + ", &" + work.name + ", &" + lwork.name + ", *" + rwork.name + ", *" + iwork.name + ", &" + info.name + ");")
                              |C99 -> 
                                ch.t <| fun x ->
                                    p.param.extern_("void zgesdd_(char jobz, int m, int n, double complex *a, int lda, double *s, double complex *u, int ldu, double complex *vt, int ldvt, double complex *work, int lwork, double *rwork, int *iwork, int info)")
                                    p.param.codewrite(x+" = 'A';")
                                    p.param.codewrite("zgesdd_(&"+x+", &" +  npre.name + "," + "&" + npre.name + ", " + mat.name+", &" + npre.name + ", *" + s.name + ", *" + u.name + ", &" + npre.name + ", *" + vt.name + ", &" + npre.name + ", &" + work.name + ", &" + lwork.name + ", *" + rwork.name + ", *" + iwork.name + ", &" + info.name + ");")
                              |T -> 
                                p.param.codewrite("call zgesdd('A', " + npre.name + "," + " " + npre.name + ","  + mat.name+", "  + npre.name + ", "  + s.name + ","   + u.name + ", "  + npre.name + ","   + vt.name + ", "  + npre.name + ", "  + work.name + ", "  + lwork.name + ","   + rwork.name + ","   + iwork.name + ", "  + info.name + ")")
                              |H -> 
                                p.param.codewrite("<math>"+mat2.name+"<mo>&larr;</mo>"+"<msup>"+mat.name+"<mn>-1</mn></msup>"+"</math>"+"\n<br/>\n")
                              |NL -> 
                                ()
                        tbinder.d mat <| fun () ->
                            ch.d1 lwork <| fun work ->
                            match p.lang with
                              |F -> 
                                p.param.codewrite("call dgesdd('A', " + npre.name + "," + " " + npre.name + ","  + mat.name+", "  + npre.name + ", "  + s.name + ","   + u.name + ", "  + npre.name + ","   + vt.name + ", "  + npre.name + ", "  + work.name + ", "  + lwork.name + ","   + rwork.name + ","   + iwork.name + ", "  + info.name + ")")
                              |C89 -> 
                                ch.t <| fun x ->
                                    p.param.extern_("void dgesdd_(char jobz, int m, int n, double *a, int lda, double *s, double *u, int ldu, double *vt, int ldvt, double *work, int lwork, int *iwork, int info)")
                                    p.param.codewrite(x+" = 'A';")
                                    p.param.codewrite("dgesdd_(&"+x+", &" +  npre.name + "," + "&" + npre.name + ", " + mat.name+", &" + npre.name + ", *" + s.name + ", *" + u.name + ", &" + npre.name + ", *" + vt.name + ", &" + npre.name + ", &" + work.name + ", &" + lwork.name + ", *" + rwork.name + ", *" + iwork.name + ", &" + info.name + ");")
                              |C99 -> 
                                ch.t <| fun x ->
                                    p.param.extern_("void dgesdd_(char jobz, int m, int n, double *a, int lda, double *s, double *u, int ldu, double *vt, int ldvt, double *work, int lwork, int *iwork, int info)")
                                    p.param.codewrite(x+" = 'A';")
                                    p.param.codewrite("dgesdd_(&"+x+", &" +  npre.name + "," + "&" + npre.name + ", " + mat.name+", &" + npre.name + ", *" + s.name + ", *" + u.name + ", &" + npre.name + ", *" + vt.name + ", &" + npre.name + ", &" + work.name + ", &" + lwork.name + ", *" + rwork.name + ", *" + iwork.name + ", &" + info.name + ");")
                              |T -> 
                                p.param.codewrite("call dgesdd('A', " + npre.name + "," + " " + npre.name + ","  + mat.name+", "  + npre.name + ", "  + s.name + ","   + u.name + ", "  + npre.name + ","   + vt.name + ", "  + npre.name + ", "  + work.name + ", "  + lwork.name + ","   + rwork.name + ","   + iwork.name + ", "  + info.name + ")")
                              |H -> 
                                p.param.codewrite("<math>"+mat2.name+"<mo>&larr;</mo>"+"<msup>"+mat.name+"<mn>-1</mn></msup>"+"</math>"+"\n<br/>\n")
                              |NL -> 
                                ()
                        br.if1 (info .=/ 0) <| fun () -> print.s[!.("rank Info: ");info]
                        //特異値分解した行列をもとに、疑似逆行列は (v^*)×(s^-1)×(u^*)
                        iter.array s <| fun i -> 
                            iter.array s <| fun j -> 
                                br.if2 (s.[i].>cond)
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
        let eigen_matrix (eigenvalues:num1,eigenvectors:num2) (mat1:num2) =
            p.param.option_("-llapack")
            p.param.option_("-lblas")
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
                                                p.param.codewrite("call zgeev('No left vectors', 'Vectors (right)', "    + npre.name + ", "   + mat1.name + ", "  + npre.name + ", "  + eigenvalues.name + ","   + dummy.name + ",  " + ldvldummy.name + ", "  + eigenvectors.name + ", "  + npre.name + ", "  + work.name + ", "  + lwork.name + ", "  + rwork.name + ", "  + info.name + ")")
                                              |C89 -> 
                                                p.param.extern_("void zgeev_(char *, char *, int *, doublecomplex *, int *, doublecomplex *, doublecomplex *, int *, doublecomplex *, int *, doublecomplex *, int *, double *, int *)")
                                                ch.t <| fun x ->
                                                ch.t <| fun y ->
                                                    p.param.codewrite(x+" = 'N';")
                                                    p.param.codewrite(y+" = 'V';")
                                                    p.param.codewrite("zgeev_(&"+x+", &"+y+", &" + npre.name + ", "  + mat1.name + ", &" + npre.name + ", " + eigenvalues.name + ", " + dummy.name + ", &" + ldvldummy.name + ", " + eigenvectors.name + ", &" + npre.name + ", " + work.name + ", &" + lwork.name + ", " + rwork.name + ", &" + info.name + ");")
                                              |C99 -> 
                                                p.param.extern_("void zgeev_(char *, char *, int *, double complex *, int *, double complex *, double complex *, int *, double complex *, int *, double complex *, int *, double *, int *)")
                                                ch.t <| fun x ->
                                                ch.t <| fun y ->
                                                    p.param.codewrite(x+" = 'N';")
                                                    p.param.codewrite(y+" = 'V';")
                                                    p.param.codewrite("zgeev_(&"+x+", &"+y+", &" + npre.name + ", "  + mat1.name + ", &" + npre.name + ", " + eigenvalues.name + ", " + dummy.name + ", &" + ldvldummy.name + ", " + eigenvectors.name + ", &" + npre.name + ", " + work.name + ", &" + lwork.name + ", " + rwork.name + ", &" + info.name + ");")
                                              |T -> 
                                                p.param.codewrite("call zgeev('No left vectors', 'Vectors (right)', "    + npre.name + ", "   + mat1.name + ", "  + npre.name + ", "  + eigenvalues.name + ","   + dummy.name + ",  " + ldvldummy.name + ", "  + eigenvectors.name + ", "  + npre.name + ", "  + work.name + ", "  + lwork.name + ", "  + rwork.name + ", "  + info.name + ")")
                                              |H -> 
                                                p.param.codewrite("<math><mi>solve</mi><mspace width=\"0.5em\" />"+mat1.name+eigenvectors.name+"<mo>=</mo>"+eigenvalues.name+eigenvectors.name+"</math>"+"\n<br/>\n")
                                              |NL -> 
                                                ()
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
                                                p.param.codewrite("call dgeev('No left vectors', 'Vectors (right)', "    + npre.name + ", "   + mat1.name + ", "  + npre.name + ", "  + eigenvalues_re.name + ", "  + eigenvalues_im.name + ","   + dummy.name + ",  " + ldvldummy.name + ", "  + eigenvectors.name + ", "  + npre.name + ", "  + work.name + ", "  + lwork.name + ", " + info.name + ")")
                                              |C89 -> 
                                                p.param.extern_("void dgeev_(char *, char *, int *, double *, int *, double *, double *, int *, double *, int *, double *, int *, double *, int *)")
                                                ch.t <| fun x ->
                                                ch.t <| fun y ->
                                                    p.param.codewrite(x+" = 'N';")
                                                    p.param.codewrite(y+" = 'V';")
                                                    p.param.codewrite("dgeev_(&"+x+", &"+y+", &" + npre.name + ", "  + mat1.name + ", &" + npre.name + ", " + eigenvalues_re.name + ", " + eigenvalues_im.name + ", " + dummy.name + ", &" + ldvldummy.name + ", " + eigenvectors.name + ", &" + npre.name + ", " + work.name + ", &" + lwork.name + ", &" + info.name + ");")
                                              |C99 -> 
                                                p.param.extern_("void dgeev_(char *, char *, int *, double *, int *, double *, double *, int *, double *, int *, double *, int *, double *, int *)")
                                                ch.t <| fun x ->
                                                ch.t <| fun y ->
                                                    p.param.codewrite(x+" = 'N';")
                                                    p.param.codewrite(y+" = 'V';")
                                                    p.param.codewrite("dgeev_(&"+x+", &"+y+", &" + npre.name + ", "  + mat1.name + ", &" + npre.name + ", " + eigenvalues_re.name + ", " + eigenvalues_im.name + ", " + dummy.name + ", &" + ldvldummy.name + ", " + eigenvectors.name + ", &" + npre.name + ", " + work.name + ", &" + lwork.name + ", &" + info.name + ");")
                                              |T -> 
                                                p.param.codewrite("call dgeev('No left vectors', 'Vectors (right)', "    + npre.name + ", "   + mat1.name + ", "  + npre.name + ", "  + eigenvalues.name + ","   + dummy.name + ",  " + ldvldummy.name + ", "  + eigenvectors.name + ", "  + npre.name + ", "  + work.name + ", "  + lwork.name + ", "  + info.name + ")")
                                              |H -> 
                                                p.param.codewrite("<math><mi>solve</mi><mspace width=\"0.5em\" />"+mat1.name+eigenvectors.name+"<mo>=</mo>"+eigenvalues.name+eigenvectors.name+"</math>"+"\n<br/>\n")
                                              |NL -> 
                                                ()
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
        let eigen_matrix2 (eigenvalues1:num1,eigenvalues2:num1,eigenvectors:num2) (mat1:num2) (mat2:num2) =
            tbinder.z mat1 <| fun () ->
                codestr.section "非対称複素行列の一般化固有値" <| fun () ->
                    p.param.option_("-llapack")
                    p.param.option_("-lblas")
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
                                            p.param.codewrite("call zggev("+
                                                "'N'" + ", " +
                                                "'V'" + ", " + 
                                                npre.name + ", " + 
                                                mat1.name + ", " + 
                                                npre.name + ", " + 
                                                mat2.name + ", " + 
                                                npre.name + ", " + 
                                                eigenvalues1.name + ", " + 
                                                eigenvalues2.name + "," + 
                                                dummy.name + ",  " + 
                                                ldvldummy.name + ", " +
                                                eigenvectors.name + ", " + 
                                                npre.name + ", "  + 
                                                work.name + ", "  + 
                                                lwork.name + ", "  + 
                                                rwork.name + ", "  + 
                                                info.name + ")")
                                          |C89 -> 
                                            p.param.extern_("void zggev_(char *, char *, int *, doublecomplex *, int *, doublecomplex *, int *, doublecomplex *, doublecomplex *, doublecomplex *, int *, doublecomplex *, int *, doublecomplex *, int *, double *, int *)")
                                            ch.t <| fun x ->
                                            ch.t <| fun y ->
                                                p.param.codewrite(x+" = 'N';")
                                                p.param.codewrite(y+" = 'V';")
                                                p.param.codewrite("zggev_(" +
                                                    "&" + x + ", " +
                                                    "&" + y + ", " +
                                                    "&" + npre.name + ", " +
                                                    mat1.name + ", " +
                                                    "&" + npre.name + ", " +
                                                    mat2.name + ", " +
                                                    "&" + npre.name + ", " +
                                                    eigenvalues1.name + ", " +
                                                    eigenvalues2.name + ", " +
                                                    dummy.name + ", "+
                                                    "&" + ldvldummy.name + ", " +
                                                    eigenvectors.name + ", "+
                                                    "&" + npre.name + ", " +
                                                    work.name + ", " +
                                                    "&" + lwork.name + ", " +
                                                    rwork.name + ", " +
                                                    "&" + info.name + ");")
                                          |C99 -> 
                                            p.param.extern_("void zggev_(char *, char *, int *, double complex *, int *, double complex *, int *, double complex *, double complex *, double complex *, int *, double complex *, int *, double complex *, int *, double *, int *)")
                                            ch.t <| fun x ->
                                            ch.t <| fun y ->
                                                p.param.codewrite(x+" = 'N';")
                                                p.param.codewrite(y+" = 'V';")
                                                p.param.codewrite("zggev_(" +
                                                    "&" + x + ", " +
                                                    "&" + y + ", " +
                                                    "&" + npre.name + ", " +
                                                    mat1.name + ", " +
                                                    "&" + npre.name + ", " +
                                                    mat2.name + ", " +
                                                    "&" + npre.name + ", " +
                                                    eigenvalues1.name + ", " +
                                                    eigenvalues2.name + ", " +
                                                    dummy.name + ", "+
                                                    "&" + ldvldummy.name + ", " +
                                                    eigenvectors.name + ", "+
                                                    "&" + npre.name + ", " +
                                                    work.name + ", " +
                                                    "&" + lwork.name + ", " +
                                                    rwork.name + ", " +
                                                    "&" + info.name + ");")
                                          |T -> 
                                            p.param.codewrite("call zggev("+
                                                "'N'" + ", " +
                                                "'V'" + ", " + 
                                                npre.name + ", " + 
                                                mat1.name + ", " + 
                                                npre.name + ", " + 
                                                mat2.name + ", " + 
                                                npre.name + ", " + 
                                                eigenvalues1.name + ", " + 
                                                eigenvalues2.name + "," + 
                                                dummy.name + ",  " + 
                                                ldvldummy.name + ", " +
                                                eigenvectors.name + ", " + 
                                                npre.name + ", "  + 
                                                work.name + ", "  + 
                                                lwork.name + ", "  + 
                                                rwork.name + ", "  + 
                                                info.name + ")")
                                          |H -> 
                                            p.param.codewrite("<math><mi>solve</mi><mspace width=\"0.5em\" />"+mat1.name+"<mi>&chi;</mi>"+"<mo>=</mo>"+"<mi>&lambda;</mi>"+mat2.name+"<mi>&chi;</mi></math>"+"\n<br/>\n")
                                            p.param.codewrite("<math><mi>&lambda;</mi><mo>=</mo>"+"<mfrac><mrow>"+eigenvalues1.name+"</mrow><mrow>"+eigenvalues2.name+"</mrow></mfrac></math>"+"\n<br/>\n")
                                          |NL -> 
                                            ()
                                        br.if1 (info .=/ 0) <| fun () -> print.s[!.("Eigenvalue Info: ");info]
            tbinder.d mat1 <| fun () ->
                codestr.section "非対称複素行列の一般化固有値" <| fun () ->
                    p.param.option_("-llapack")
                    p.param.option_("-lblas")
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
                                            p.param.codewrite("call dggev("+
                                                "'N'" + ", " +
                                                "'V'" + ", " + 
                                                npre.name + ", " + 
                                                mat1.name + ", " + 
                                                npre.name + ", " + 
                                                mat2.name + ", " + 
                                                npre.name + ", " + 
                                                eigenvalues1re.name + ", " + 
                                                eigenvalues1im.name + ", " + 
                                                eigenvalues2.name + "," + 
                                                dummy.name + ",  " + 
                                                ldvldummy.name + ", " +
                                                eigenvectors.name + ", " + 
                                                npre.name + ", "  + 
                                                work.name + ", "  + 
                                                lwork.name + ", "  + 
                                                info.name + ")")
                                          |C89 -> 
                                            p.param.extern_("void dggev_(char *, char *, int *, doublecomplex *, int *, doublecomplex *, int *, doublecomplex *, doublecomplex *, doublecomplex *, int *, doublecomplex *, int *, doublecomplex *, int *, double *, int *)")
                                            ch.t <| fun x ->
                                            ch.t <| fun y ->
                                                p.param.codewrite(x+" = 'N';")
                                                p.param.codewrite(y+" = 'V';")
                                                p.param.codewrite("dggev_(" +
                                                    "&" + x + ", " +
                                                    "&" + y + ", " +
                                                    "&" + npre.name + ", " +
                                                    mat1.name + ", " +
                                                    "&" + npre.name + ", " +
                                                    mat2.name + ", " +
                                                    "&" + npre.name + ", " +
                                                    eigenvalues1re.name + ", " +
                                                    eigenvalues1im.name + ", " +
                                                    eigenvalues2.name + ", " +
                                                    dummy.name + ", "+
                                                    "&" + ldvldummy.name + ", " +
                                                    eigenvectors.name + ", "+
                                                    "&" + npre.name + ", " +
                                                    work.name + ", " +
                                                    "&" + lwork.name + ", " +
                                                    "&" + info.name + ");")
                                          |C99 -> 
                                            p.param.extern_("void dggev_(char *, char *, int *, double complex *, int *, double complex *, int *, double complex *, double complex *, double complex *, int *, double complex *, int *, double complex *, int *, double *, int *)")
                                            ch.t <| fun x ->
                                            ch.t <| fun y ->
                                                p.param.codewrite(x+" = 'N';")
                                                p.param.codewrite(y+" = 'V';")
                                                p.param.codewrite("dggev_(" +
                                                    "&" + x + ", " +
                                                    "&" + y + ", " +
                                                    "&" + npre.name + ", " +
                                                    mat1.name + ", " +
                                                    "&" + npre.name + ", " +
                                                    mat2.name + ", " +
                                                    "&" + npre.name + ", " +
                                                    eigenvalues1re.name + ", " +
                                                    eigenvalues1im.name + ", " +
                                                    eigenvalues2.name + ", " +
                                                    dummy.name + ", "+
                                                    "&" + ldvldummy.name + ", " +
                                                    eigenvectors.name + ", "+
                                                    "&" + npre.name + ", " +
                                                    work.name + ", " +
                                                    "&" + lwork.name + ", " +
                                                    "&" + info.name + ");")
                                          |T -> 
                                            p.param.codewrite("call dggev("+
                                                "'N'" + ", " +
                                                "'V'" + ", " + 
                                                npre.name + ", " + 
                                                mat1.name + ", " + 
                                                npre.name + ", " + 
                                                mat2.name + ", " + 
                                                npre.name + ", " + 
                                                eigenvalues1re.name + ", " + 
                                                eigenvalues1im.name + ", " + 
                                                eigenvalues2.name + "," + 
                                                dummy.name + ",  " + 
                                                ldvldummy.name + ", " +
                                                eigenvectors.name + ", " + 
                                                npre.name + ", "  + 
                                                work.name + ", "  + 
                                                lwork.name + ", "  + 
                                                info.name + ")")
                                          |H -> 
                                            p.param.codewrite("<math><mi>solve</mi><mspace width=\"0.5em\" />"+mat1.name+"<mi>&chi;</mi>"+"<mo>=</mo>"+"<mi>&lambda;</mi>"+mat2.name+"<mi>&chi;</mi></math>"+"\n<br/>\n")
                                            p.param.codewrite("<math><mi>&lambda;</mi><mo>=</mo>"+"<mfrac><mrow>"+eigenvalues1re.name+"<mo>+</mo></mi>j<mi>"+eigenvalues1im.name+"</mrow><mrow>"+eigenvalues2.name+"</mrow></mfrac></math>"+"\n<br/>\n")
                                          |NL -> 
                                            ()
                                        br.if1 (info .=/ 0) <| fun () -> print.s[!.("Eigenvalue Info: ");info]
                        iter.num eigenvalues1.size1 <| fun i ->
                            eigenvalues1.[i] <== eigenvalues1re.[i] + asm.uj + eigenvalues1im.[i]
                                        
        /// <summary>
        /// 連立方程式の求解(Tikhonovの正則化法)
        /// </summary>
        /// <param name="fu_mat">係数行列</param>
        /// <param name="fu_cst">定数項ベクトル</param>
        /// <param name="code">解に対して行う処理</param>
        let solve_simuleq_t(fu_mat:num2,fu_cst:num1) code =
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
                    solve_simuleq(FF,bb)
                    code(bb)

        /// <summary>
        /// 連立方程式の求解(Tikhonovの正則化法)
        /// </summary>
        /// <param name="fu_mat">係数行列</param>
        /// <param name="fu_cst">定数項ベクトル</param>
        /// <param name="lambda">正則化パラメータ</param>
        /// <param name="code">解に対して行う処理</param>
        let solve_simuleq_tt(fu_mat:num2,fu_cst:num1,lambda:double) code =
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
                        solve_simuleq(FF,bb)
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
                        solve_simuleq(FF,bb)
                        code(bb)
                    
        /// <summary>
        /// 連立方程式の求解(Tikhonovの正則化法)
        /// </summary>
        /// <param name="fu_mat">係数行列</param>
        /// <param name="fu_cst">定数項ベクトル(列サイズ=1)</param>
        /// <param name="lambda">正則化パラメータ</param>
        /// <param name="code">解に対して行う処理</param>
        let solve_simuleq_tt2(fu_mat:num2,fu_cst:num2,lambda:num0) code =
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
                    solve_simuleq(FF,bb)
                    code(bb)

        /// <summary>
        /// 行列式の常用対数を計算
        /// </summary>
        /// <param name="matrix">行列</param>
        /// <param name="code">行列式の値を用いて実行するコード</param>
        let determinant (matrix:num2) code =
            p.param.option_("-llapack")
            p.param.option_("-lblas")
            tbinder.z matrix <| fun () ->
                codestr.section "行列式の常用対数を計算" <| fun () ->
                    ch.iid <| fun (N,info,d) ->
                        N <== matrix.size1
                        ch.i1 N <| fun ipiv ->
                            match p.lang with
                              |F -> 
                                p.param.codewrite("call zgetrf("+N.name+","+N.name+","+matrix.name+","+N.name+","+ipiv.name+","+info.name+")"+"\n")
                              |C89 -> 
                                p.param.extern_("void zgetrf_(int *, int *, doublecomplex *, int *, int *, int *)")
                                p.param.codewrite("zgetrf_(&"+N.name+","+"&"+N.name+","+matrix.name+",&"+N.name+","+ipiv.name+",&"+info.name+")"+";\n")
                              |C99 -> 
                                p.param.extern_("void zgetrf_(int *, int *, double complex *, int *, int *, int *)")
                                p.param.codewrite("zgetrf_(&"+N.name+","+"&"+N.name+","+matrix.name+",&"+N.name+","+ipiv.name+",&"+info.name+")"+";\n")
                              |T -> 
                                p.param.codewrite("zgetrf("+N.name+","+N.name+","+matrix.name+","+N.name+","+ipiv.name+","+info.name+")"+"\n")
                              |H -> 
                                p.param.codewrite("<math>"+d.name+"<mo>&larr;</mo>"+"<mo>|</mo>"+matrix.name+"<mo>|</mo>"+"</math>"+"\n<br/>\n")
                              |NL -> 
                                ()
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
                                p.param.codewrite("call dgetrf("+N.name+","+N.name+","+matrix.name+","+N.name+","+ipiv.name+","+info.name+")"+"\n")
                              |C89 -> 
                                p.param.extern_("void dgetrf_(int *, int *, doublecomplex *, int *, int *, int *)")
                                p.param.codewrite("dgetrf_(&"+N.name+","+"&"+N.name+","+matrix.name+",&"+N.name+","+ipiv.name+",&"+info.name+")"+";\n")
                              |C99 -> 
                                p.param.extern_("void dgetrf_(int *, int *, double complex *, int *, int *, int *)")
                                p.param.codewrite("dgetrf_(&"+N.name+","+"&"+N.name+","+matrix.name+",&"+N.name+","+ipiv.name+",&"+info.name+")"+";\n")
                              |T -> 
                                p.param.codewrite("dgetrf("+N.name+","+N.name+","+matrix.name+","+N.name+","+ipiv.name+","+info.name+")"+"\n")
                              |H -> 
                                p.param.codewrite("<math>"+d.name+"<mo>&larr;</mo>"+"<mo>|</mo>"+matrix.name+"<mo>|</mo>"+"</math>"+"\n<br/>\n")
                              |NL -> 
                                ()
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
        let svd (mat1:num2) (u:num2,s:num1,vt:num2) =
            p.param.option_("-llapack")
            p.param.option_("-lblas")
            match p.lang with
              |H ->
                p.param.codewrite("<math>"+u.name+"<mo>&middot;</mo>"+s.name+"<mo>&middot;</mo>"+"<msup>"+vt.name+"<mi>T</mi></msup><mo>&larr;</mo>"+mat1.name+"</math>\n<br/>\n")
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
                                lwork <== 5*n+n
                                ch.z1 lwork <| fun work ->
                                ch.d1 (5*m) <| fun rwork ->
                                    lda <== m
                                    ldu <== u.size1
                                    ldvt <== vt.size2
                                    match p.lang with
                                      |F -> 
                                        p.param.codewrite("call zgesvd("+
                                          "'A', "+
                                          "'A', " + 
                                          m.name + ", " + 
                                          n.name + ", " +
                                          mat1.name + ", " +
                                          lda.name + ", " +
                                          s.name+ ", " + 
                                          u.name + ", "  + 
                                          ldu.name + ", " + 
                                          vt.name + ", "  + 
                                          ldvt.name + ", " + 
                                          work.name + ", " + 
                                          lwork.name + ", " + 
                                          rwork.name + ", " + 
                                          info.name + ")")
                                      |C89 -> 
                                        p.param.extern_("void zgesvd_(char *, char *, int *, int *, double complex *, int *, double *, double complex *, int *, double complex *, int *, double complex *, int *, double *, int *)")
                                        ch.t <| fun x ->
                                        ch.t <| fun y ->
                                            p.param.codewrite(x+" = 'A';")
                                            p.param.codewrite(y+" = 'A';")
                                            p.param.codewrite("zgesvd_(" + 
                                              "&" + x + ", " + 
                                              "&" + y + ", " + 
                                              "&" + m.name + ", " + 
                                              "&" + n.name + ", " +
                                              mat1.name + ", " +
                                              "&" + lda.name + ", " +
                                              s.name+ ", " + 
                                              u.name + ", "  + 
                                              "&" + ldu.name + ", " + 
                                              vt.name + ", "  + 
                                              "&" + ldvt.name + ", " + 
                                              work.name + ", " + 
                                              "&" + lwork.name + ", " + 
                                              rwork.name + ", " + 
                                              "&" + info.name + ");")
                                      |C99 -> 
                                        p.param.extern_("void zgesvd_(char *, char *, int *, int *, double complex *, int *, double *, double complex *, int *, double complex *, int *, double complex *, int *, double *, int *)")
                                        ch.t <| fun x ->
                                        ch.t <| fun y ->
                                            p.param.codewrite(x+" = 'A';")
                                            p.param.codewrite(y+" = 'A';")
                                            p.param.codewrite("zgesvd_(" + 
                                              "&" + x + ", " + 
                                              "&" + y + ", " + 
                                              "&" + m.name + ", " + 
                                              "&" + n.name + ", " +
                                              mat1.name + ", " +
                                              "&" + lda.name + ", " +
                                              s.name+ ", " + 
                                              u.name + ", "  + 
                                              "&" + ldu.name + ", " + 
                                              vt.name + ", "  + 
                                              "&" + ldvt.name + ", " + 
                                              work.name + ", " + 
                                              "&" + lwork.name + ", " + 
                                              rwork.name + ", " + 
                                              "&" + info.name + ");")
                                      |T -> 
                                        p.param.codewrite("zgesvd("+
                                          "'A', "+
                                          "'A', " + 
                                          m.name + ", " + 
                                          n.name + ", " +
                                          mat1.name + ", " +
                                          lda.name + ", " +
                                          s.name+ ", " + 
                                          u.name + ", "  + 
                                          ldu.name + ", " + 
                                          vt.name + ", "  + 
                                          ldvt.name + ", " + 
                                          work.name + ", " + 
                                          lwork.name + ", " + 
                                          rwork.name + ", " + 
                                          info.name + ")")
                                      |H -> 
                                        ()
                                      |NL -> 
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
                                lwork <== 5*n+n
                                ch.d1 lwork <| fun work ->
                                ch.d1 (5*m) <| fun rwork ->
                                    lda <== m
                                    ldu <== u.size1
                                    ldvt <== vt.size2
                                    match p.lang with
                                      |F -> 
                                        p.param.codewrite("call dgesvd("+
                                          "'A', "+
                                          "'A', " + 
                                          m.name + ", " + 
                                          n.name + ", " +
                                          mat1.name + ", " +
                                          lda.name + ", " +
                                          s.name+ ", " + 
                                          u.name + ", "  + 
                                          ldu.name + ", " + 
                                          vt.name + ", "  + 
                                          ldvt.name + ", " + 
                                          work.name + ", " + 
                                          lwork.name + ", " + 
                                          rwork.name + ", " + 
                                          info.name + ")")
                                      |C89 -> 
                                        p.param.extern_("void dgesvd_(char *, char *, int *, int *, double complex *, int *, double *, double complex *, int *, double complex *, int *, double complex *, int *, double *, int *)")
                                        ch.t <| fun x ->
                                        ch.t <| fun y ->
                                            p.param.codewrite(x+" = 'A';")
                                            p.param.codewrite(y+" = 'A';")
                                            p.param.codewrite("dgesvd_(" + 
                                              "&" + x + ", " + 
                                              "&" + y + ", " + 
                                              "&" + m.name + ", " + 
                                              "&" + n.name + ", " +
                                              mat1.name + ", " +
                                              "&" + lda.name + ", " +
                                              s.name+ ", " + 
                                              u.name + ", "  + 
                                              "&" + ldu.name + ", " + 
                                              vt.name + ", "  + 
                                              "&" + ldvt.name + ", " + 
                                              work.name + ", " + 
                                              "&" + lwork.name + ", " + 
                                              rwork.name + ", " + 
                                              "&" + info.name + ");")
                                      |C99 -> 
                                        p.param.extern_("void dgesvd_(char *, char *, int *, int *, double complex *, int *, double *, double complex *, int *, double complex *, int *, double complex *, int *, double *, int *)")
                                        ch.t <| fun x ->
                                        ch.t <| fun y ->
                                            p.param.codewrite(x+" = 'A';")
                                            p.param.codewrite(y+" = 'A';")
                                            p.param.codewrite("dgesvd_(" + 
                                              "&" + x + ", " + 
                                              "&" + y + ", " + 
                                              "&" + m.name + ", " + 
                                              "&" + n.name + ", " +
                                              mat1.name + ", " +
                                              "&" + lda.name + ", " +
                                              s.name+ ", " + 
                                              u.name + ", "  + 
                                              "&" + ldu.name + ", " + 
                                              vt.name + ", "  + 
                                              "&" + ldvt.name + ", " + 
                                              work.name + ", " + 
                                              "&" + lwork.name + ", " + 
                                              rwork.name + ", " + 
                                              "&" + info.name + ");")
                                      |T -> 
                                        p.param.codewrite("dgesvd("+
                                          "'A', "+
                                          "'A', " + 
                                          m.name + ", " + 
                                          n.name + ", " +
                                          mat1.name + ", " +
                                          lda.name + ", " +
                                          s.name+ ", " + 
                                          u.name + ", "  + 
                                          ldu.name + ", " + 
                                          vt.name + ", "  + 
                                          ldvt.name + ", " + 
                                          work.name + ", " + 
                                          lwork.name + ", " + 
                                          rwork.name + ", " + 
                                          info.name + ")")
                                      |H -> 
                                        ()
                                      |NL -> 
                                        ()
                                      
        /// <summary>
        /// 連立同次方程式を求解
        /// </summary>
        /// <param name="mat">複素係数行列</param>
        /// <param name="f">連立方程式の解</param>
        let solve_homogeneq (mat:num2) (f:num1) =
            ch.d1 mat.size1 <| fun s ->
            ch.z2 mat.size1 mat.size2 <| fun u ->
            ch.z2 mat.size1 mat.size2 <| fun vt ->
                svd mat (u,s,vt)
                !"0に近いほど正確な解"
                print.tag_c "solve_homogeneq" s.[mat.size1]
                iter.range _1 mat.size1 <| fun i ->
                    f.[i] <== asm.conj(vt.[mat.size1,i])