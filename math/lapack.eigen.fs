//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    [<AutoOpen>]
    /// Adds standard and generalized eigenvalue calculations to the linear-algebra context.
    module ContextLaEigenExtensions =
        /// Validates matrix and output shapes for eigenvalue operations.
        let private requireEigenShapes (context:Aqualis) (matrix:complex2)
                                       (eigenvalues:complex1) (eigenvectors:complex2) =
            match context.language with
            | C99 | Fortran | Python ->
                LapackValidation.require context (matrix.size1 .<= 0) "LAPACK eigen matrix order must be positive."
                LapackValidation.require context (matrix.size1 .=/ matrix.size2) "LAPACK eigen matrix must be square."
                LapackValidation.require context (eigenvalues.size1 .=/ matrix.size1) "LAPACK eigenvalue count must match matrix order."
                LapackValidation.require context (eigenvectors.size1 .=/ matrix.size1) "LAPACK eigenvector shape must match matrix order."
                LapackValidation.require context (eigenvectors.size2 .=/ matrix.size1) "LAPACK eigenvector shape must match matrix order."
            | _ -> ()

        /// LAPACK-backed linear algebra operations for an Aqualis context.
        type ContextLa with
            /// <summary>Computes eigenvalues and right eigenvectors of a general complex matrix.</summary>
            /// <param name="eigenvalues">Output eigenvalue vector.</param>
            /// <param name="eigenvectors">Output matrix of right eigenvectors.</param>
            /// <param name="mat1">Square complex input matrix; LAPACK paths may overwrite it.</param>
            member this.eigen_matrix (eigenvalues:complex1,eigenvectors:complex2) (mat1:complex2) =
                LapackValidation.requireBackend this.GenerationContext "LAPACK eigenvalue calculation"
                if eigenvectors.code = mat1.code then
                    invalidArg (nameof eigenvectors) "LAPACK eigenvector output must be different from input matrices."
                this.GenerationContext.olist.add "-llapack"
                this.GenerationContext.olist.add "-lblas"
                this.GenerationContext.group.section "非対称複素行列の固有値" <| fun () ->
                    requireEigenShapes this.GenerationContext mat1 eigenvalues eigenvectors
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
                                                LapackValidation.checkInfo this.GenerationContext info "eigenvalue"
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
                                                    LapackValidation.checkInfo this.GenerationContext info "eigenvalue"
                                                |_ -> ()
                    |LaTeX ->
                        this.GenerationContext.codewritein("Solve: $"+mat1.code+eigenvectors.code+" = "+eigenvalues.code+eigenvectors.code+"$"+"<br/>\n")
                    |HTML ->
                        this.GenerationContext.codewritein("Solve: \\("+mat1.code+eigenvectors.code+" = "+eigenvalues.code+eigenvectors.code+"\\)"+"<br/>\n")
                    |Python ->
                        this.RequirePythonLinalg "eig"
                        this.GenerationContext.codewritein(eigenvalues.code+","+eigenvectors.code+" = eig("+mat1.code+")"+"\n")
                    |_ -> ()
                    
            /// <summary>Computes generalized right eigenvectors satisfying A*x = lambda*B*x.</summary>
            /// <param name="eigenvalues1">Numerators of the generalized eigenvalues.</param>
            /// <param name="eigenvalues2">Denominators of the generalized eigenvalues.</param>
            /// <param name="eigenvectors">Output matrix of right eigenvectors.</param>
            /// <param name="mat1">First square complex input matrix.</param>
            /// <param name="mat2">Second square complex input matrix.</param>
            member this.eigen_matrix2 (eigenvalues1:complex1,eigenvalues2:complex1,eigenvectors:complex2) (mat1:complex2) (mat2:complex2) =
                    LapackValidation.requireBackend this.GenerationContext "LAPACK eigenvalue calculation"
                    if eigenvalues1.code = eigenvalues2.code then
                        invalidArg (nameof eigenvalues2) "LAPACK generalized eigenvalue outputs must be different vectors."
                    if eigenvectors.code = mat1.code || eigenvectors.code = mat2.code then
                        invalidArg (nameof eigenvectors) "LAPACK eigenvector output must be different from input matrices."
                    this.GenerationContext.group.section "非対称複素行列の一般化固有値" <| fun () ->
                        this.GenerationContext.olist.add "-llapack"
                        this.GenerationContext.olist.add "-lblas"
                        requireEigenShapes this.GenerationContext mat1 eigenvalues1 eigenvectors
                        match this.GenerationContext.language with
                        | C99 | Fortran | Python ->
                            LapackValidation.require this.GenerationContext (eigenvalues2.size1 .=/ mat1.size1) "LAPACK second eigenvalue count must match matrix order."
                        | _ -> ()
                        LapackValidation.require this.GenerationContext (mat2.size1 .=/ mat1.size1) "LAPACK eigen matrix orders must match."
                        LapackValidation.require this.GenerationContext (mat2.size2 .=/ mat1.size1) "LAPACK eigen matrix orders must match."
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
                                                LapackValidation.checkInfo this.GenerationContext info "generalized eigenvalue"
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
                                                        LapackValidation.checkInfo this.GenerationContext info "generalized eigenvalue"
                                                    |_ -> ()
                                            |LaTeX ->
                                                this.GenerationContext.codewritein("Solve: $"+mat1.code+eigenvectors.code+" = "+"\\frac{"+eigenvalues1.code+"}{"+eigenvalues2.code+"}"+mat2.code+eigenvectors.code+"$\\\\\n")
                                            |HTML ->
                                                this.GenerationContext.codewritein("Solve: \\("+mat1.code+eigenvectors.code+" = "+"\\frac{"+eigenvalues1.code+"}{"+eigenvalues2.code+"}"+mat2.code+eigenvectors.code+"\\)<br/>\n")
                                            |Python ->
                                                this.RequirePythonLinalg "eig"
                                                this.GenerationContext.codewritein("("+eigenvalues1.code+", "+eigenvalues2.code+"), "+eigenvectors.code+" = eig("+mat1.code+", "+mat2.code+", homogeneous_eigvals=True)\n")
                                            |_ -> ()
