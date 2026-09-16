//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    [<AutoOpen>]
    module ContextLaEigenExtensions =
        type ContextLa with
            /// <summary>
            /// Ax=λxの固有値λと固有ベクトルxを計算
            /// </summary>
            /// <param name="eigenvalues">固有値</param>
            /// <param name="eigenvectors">固有ベクトル</param>
            /// <param name="mat1">複素非対称行列</param>
            member this.eigen_matrix (eigenvalues:complex1,eigenvectors:complex2) (mat1:complex2) =
                this.GenerationContext.olist.add "-llapack"
                this.GenerationContext.olist.add "-lblas"
                this.GenerationContext.group.section "非対称複素行列の固有値" <| fun () ->
                    eigenvectors.clear()
                    match this.GenerationContext.language with
                    |Fortran ->
                        this.GenerationContext.ch.iii <| fun (npre,ldvldummy,info) ->
                                npre<==mat1.size1
                                this.GenerationContext.ch.z2 (_1, _1) <| fun dummy ->
                                    this.GenerationContext.ch.i <| fun lwork ->
                                        lwork <== 2*npre
                                        this.GenerationContext.ch.z1 lwork <| fun work ->
                                            this.GenerationContext.ch.d1 (2*npre) <| fun rwork ->
                                                eigenvalues.clear()
                                                ldvldummy <== 1
                                                this.GenerationContext.codewritein("call zgeev('No left vectors', 'Vectors (right)', "    +
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
                                                this.GenerationContext.br.if1 (info .=/ 0) <| fun () -> this.GenerationContext.print.tt <| "Eigenvalue Info: "++info
                    |C99 ->
                        this.GenerationContext.ch.iii <| fun (npre,ldvldummy,info) ->
                                npre<==mat1.size1
                                this.GenerationContext.ch.z2 (_1, _1) <| fun dummy ->
                                    this.GenerationContext.ch.i <| fun lwork ->
                                        lwork <== 2*npre
                                        this.GenerationContext.ch.z1 lwork <| fun work ->
                                            this.GenerationContext.ch.d1 (2*npre) <| fun rwork ->
                                            this.GenerationContext.ch.c <| fun jobvl ->
                                            this.GenerationContext.ch.c <| fun jobvr ->
                                                match jobvl,jobvr with
                                                |Var(_,jobvl,_),Var(_,jobvr,_) ->
                                                    eigenvalues.clear()
                                                    ldvldummy <== 1
                                                    this.GenerationContext.elist.add "void zgeev_(char *, char *, int *, double complex *, int *, double complex *, double complex *, int *, double complex *, int *, double complex *, int *, double *, int *)"
                                                    this.GenerationContext.codewritein(jobvl + " = 'N';")
                                                    this.GenerationContext.codewritein(jobvr + " = 'V';")
                                                    this.GenerationContext.codewritein("zgeev_(" +
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
                                                    this.GenerationContext.br.if1 (info .=/ 0) <| fun () -> this.GenerationContext.print.tt <| "Eigenvalue Info: "++info
                                                |_ -> ()
                    |LaTeX ->
                        this.GenerationContext.codewritein("Solve: $"+mat1.code+eigenvectors.code+" = "+eigenvalues.code+eigenvectors.code+"$"+"<br/>\n")
                    |HTML ->
                        this.GenerationContext.codewritein("Solve: \\("+mat1.code+eigenvectors.code+" = "+eigenvalues.code+eigenvectors.code+"\\)"+"<br/>\n")
                    |Python ->
                        this.RequirePythonLinalg "eig"
                        this.GenerationContext.codewritein(eigenvalues.code+","+eigenvectors.code+" = eig("+mat1.code+")"+"\n")
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
                    this.GenerationContext.group.section "非対称複素行列の一般化固有値" <| fun () ->
                        this.GenerationContext.olist.add "-llapack"
                        this.GenerationContext.olist.add "-lblas"
                        eigenvectors.clear()
                        this.GenerationContext.ch.iii <| fun (npre,ldvldummy,info) ->
                            npre<==mat1.size1
                            this.GenerationContext.ch.z2 (_1, _1) <| fun dummy ->
                                this.GenerationContext.ch.i <| fun lwork ->
                                    lwork <== npre + 64 * npre
                                    this.GenerationContext.ch.z1 lwork <| fun work ->
                                        this.GenerationContext.ch.d1 (8*npre) <| fun rwork ->
                                            eigenvalues1.clear()
                                            eigenvalues2.clear()
                                            ldvldummy <== 1
                                            match this.GenerationContext.language with
                                            |Fortran ->
                                                this.GenerationContext.codewritein("call zggev("+
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
                                                this.GenerationContext.ch.c <| fun jobvl ->
                                                this.GenerationContext.ch.c <| fun jobvr ->
                                                    match jobvl,jobvr with
                                                    |Var(_,jobvl,_),Var(_,jobvr,_) ->
                                                        this.GenerationContext.elist.add "void zggev_(char *, char *, int *, double complex *, int *, double complex *, int *, double complex *, double complex *, double complex *, int *, double complex *, int *, double complex *, int *, double *, int *)"
                                                        this.GenerationContext.codewritein(jobvl + " = 'N';")
                                                        this.GenerationContext.codewritein(jobvr + " = 'V';")
                                                        this.GenerationContext.codewritein("zggev_(" +
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
                                                this.GenerationContext.codewritein("Solve: $"+mat1.code+eigenvectors.code+" = "+"\\frac{"+eigenvalues1.code+"}{"+eigenvalues2.code+"}"+mat2.code+eigenvectors.code+"$\\\\\n")
                                            |HTML ->
                                                this.GenerationContext.codewritein("Solve: \\("+mat1.code+eigenvectors.code+" = "+"\\frac{"+eigenvalues1.code+"}{"+eigenvalues2.code+"}"+mat2.code+eigenvectors.code+"\\)<br/>\n")
                                            //Pythonのscipy.linalg.eigは、一般化固有値問題を単独の出力で処理することが可能
                                            //Pythonでは、一般化固有値の計算が単一の出力で提供されるため、ユーザーは結果を手軽に利用できる。これにより、計算過程や出力の管理がシンプルになる。
                                            //Fortranでは、二つの固有値配列を出力することで、行列 AとB の関係性を明示的に示している。この設計は、行列間の相互作用をより詳細に理解するためのもの
                                            //このコードでは、周囲と合わせるため、行列を入れ替えてeigenvalues2.codeを出している。
                                            //ちなみに一般化固有ベクトルは二つも出す必要はないので、二行目で出しているeigenvectors.code_dasokuはおまけだと思っていい。理由は以下。
                                            //一般化固有値問題 Ax=λBx の形式では、行列 B に対して左固有ベクトルが計算されることはない。したがって、一般化固有ベクトルは一意に定まることが多い。
                                            |Python ->
                                                this.RequirePythonLinalg "eig"
                                                this.GenerationContext.codewritein(eigenvalues1.code+","+eigenvectors.code+" = eig("+mat1.code+","+mat2.code+")"+"\n")
                                                this.GenerationContext.codewritein(eigenvalues2.code+", "+eigenvectors.code+"_dasoku = eig("+mat2.code+","+mat1.code+")"+"\n")
                                            |_ -> ()
                                            this.GenerationContext.br.if1 (info .=/ 0) <| fun () -> this.GenerationContext.print.tt <| "Eigenvalue Info: "++info
