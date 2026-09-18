//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    /// Linear-system solvers on the LAPACK context.
    [<AutoOpen>]
    module ContextLaSolveExtensions =
        /// Requires a target backend for linear-system solving.
        let private requireSolveBackend (context:Aqualis) =
            LapackValidation.requireBackend context "LAPACK linear solve"

        /// Checks the LAPACK status of a linear solve.
        let private checkSolveInfo (context:Aqualis) (info:int0) =
            match context.language with
            |Fortran ->
                context.codewritein("if (" + info.code + " /= 0) error stop 'Aqualis: LAPACK solve failed.'\n")
            |C99 ->
                context.codewritein("if (" + info.code +
                                    " != 0) { fprintf(stderr, \"Aqualis: LAPACK solve failed (INFO=%d).\\n\", " +
                                    info.code + "); exit(EXIT_FAILURE); }\n")
            |_ -> ()
        /// Validates a shape precondition for linear solving.
        let private requireSolveShape (context:Aqualis) (condition:bool0) message =
            match context.language with
            | C99 ->
                context.br.if1 condition (fun () ->
                    context.codewritein("fprintf(stderr, " + OutputTextLiteral.c ("Aqualis: " + message + "\n") + "); exit(EXIT_FAILURE);\n"))
            | Fortran ->
                context.br.if1 condition (fun () ->
                    context.codewritein("error stop " + OutputTextLiteral.fortran ("Aqualis: " + message) + "\n"))
            | Python ->
                context.br.if1 condition (fun () ->
                    context.codewritein("raise ValueError(" + OutputTextLiteral.python ("Aqualis: " + message) + ")\n"))
            | _ -> ()

        /// Requires a square coefficient matrix.
        let private requireSquareMatrix context (matrixSize1:int0) (matrixSize2:int0) =
            requireSolveShape context (matrixSize1 .<= 0) "LAPACK matrix order must be positive."
            requireSolveShape context (matrixSize1 .=/ matrixSize2) "LAPACK matrix must be square."
        /// LAPACK-backed linear algebra operations for an Aqualis context.
        type ContextLa with
            ///<summary>連立方程式の求解</summary>
            ///<param name="matrix">係数行列</param>
            ///<param name="y">定数項ベクトル→解ベクトル</param>
            member this.solve_simuleq (matrix:complex2,y:complex1) =
                requireSolveBackend this.GenerationContext
                this.GenerationContext.olist.add "-llapack"
                this.GenerationContext.olist.add "-lblas"
                this.GenerationContext.group.section "連立方程式の求解" <| fun () ->
                    requireSquareMatrix this.GenerationContext matrix.size1 matrix.size2
                    requireSolveShape this.GenerationContext (y.size1 .=/ matrix.size1) "LAPACK right-hand side length must match matrix order."
                    match this.GenerationContext.language with
                    |Fortran ->
                        this.GenerationContext.ch.iii <| fun (N,b,info) ->
                            N <== matrix.size1
                            b <== 1
                            this.GenerationContext.ch.i1 N <| fun ipiv ->
                                this.GenerationContext.codewritein("call zgesv("+N.code+","+b.code+","+matrix.code+","+N.code+","+ipiv.code+","+y.code+","+N.code+","+info.code+")"+"\n")
                                checkSolveInfo this.GenerationContext info
                    |C99 ->
                        this.GenerationContext.ch.iii <| fun (N,b,info) ->
                            N <== matrix.size1
                            b <== 1
                            this.GenerationContext.ch.i1 N <| fun ipiv ->
                                this.GenerationContext.elist.add "void zgesv_(int *n, int *nrhs, double complex *a, int *lda, int *ipiv, double complex *b, int *ldb, int *info)"
                                this.GenerationContext.codewritein("zgesv_(&"+N.code+","+"&"+b.code+","+matrix.code+",&"+N.code+","+ipiv.code+","+y.code+",&"+N.code+",&"+info.code+")"+";\n")
                                checkSolveInfo this.GenerationContext info
                    |LaTeX ->
                        this.GenerationContext.codewritein("$"+y.code+" \\leftarrow "+matrix.code+"^{-1}"+y.code+"$\\\\\n")
                    |HTML ->
                        this.GenerationContext.codewritein("\\("+y.code+" \\leftarrow "+matrix.code+"^{-1}"+y.code+"\\)<br/>\n")
                    |Python ->
                        this.RequirePythonLinalg "solve"
                        this.GenerationContext.codewritein(y.code+" = solve("+matrix.code+", "+y.code+")"+"\n")
                    |_ -> ()
                    
            ///<summary>連立方程式の求解</summary>
            ///<param name="matrix">係数行列</param>
            ///<param name="y">定数項ベクトル→解ベクトル</param>
            member this.solve_simuleq (matrix:double2,y:double1) =
                requireSolveBackend this.GenerationContext
                this.GenerationContext.olist.add "-llapack"
                this.GenerationContext.olist.add "-lblas"
                this.GenerationContext.group.section "連立方程式の求解" <| fun () ->
                    requireSquareMatrix this.GenerationContext matrix.size1 matrix.size2
                    requireSolveShape this.GenerationContext (y.size1 .=/ matrix.size1) "LAPACK right-hand side length must match matrix order."
                    match this.GenerationContext.language with
                    |Fortran ->
                        this.GenerationContext.ch.iii <| fun (N,b,info) ->
                            N <== matrix.size1
                            b <== 1
                            this.GenerationContext.ch.i1 N <| fun ipiv ->
                                this.GenerationContext.codewritein("call dgesv("+N.code+","+b.code+","+matrix.code+","+N.code+","+ipiv.code+","+y.code+","+N.code+","+info.code+")"+"\n")
                                checkSolveInfo this.GenerationContext info
                    |C99 ->
                        this.GenerationContext.ch.iii <| fun (N,b,info) ->
                            N <== matrix.size1
                            b <== 1
                            this.GenerationContext.ch.i1 N <| fun ipiv ->
                                this.GenerationContext.elist.add "void dgesv_(int *n, int *nrhs, double *a, int *lda, int *ipiv, double *b, int *ldb, int *info)"
                                this.GenerationContext.codewritein("dgesv_(&"+N.code+","+"&"+b.code+","+matrix.code+",&"+N.code+","+ipiv.code+","+y.code+",&"+N.code+",&"+info.code+")"+";\n")
                                checkSolveInfo this.GenerationContext info
                    |LaTeX ->
                        this.GenerationContext.codewritein("$"+y.code+" \\leftarrow "+matrix.code+"^{-1}"+y.code+"$\\\\\n")
                    |HTML ->
                        this.GenerationContext.codewritein("\\("+y.code+" \\leftarrow "+matrix.code+"^{-1}"+y.code+"\\)<br/>\n")
                    |Python ->
                        this.RequirePythonLinalg "solve"
                        this.GenerationContext.codewritein(y.code+" = solve("+matrix.code+", "+y.code+")"+"\n")
                    |_ -> ()
                    
            ///<summary>連立方程式の求解</summary>
            ///<param name="matrix">係数行列</param>
            ///<param name="y">定数項ベクトルを列方向に並べた配列→解ベクトルを列方向に並べた配列</param>
            member this.solve_simuleqs (matrix:complex2,y:complex2) =
                requireSolveBackend this.GenerationContext
                if matrix.code = y.code then
                    invalidArg (nameof y) "LAPACK coefficient matrix and right-hand side must be different matrices."
                this.GenerationContext.olist.add "-llapack"
                this.GenerationContext.olist.add "-lblas"
                this.GenerationContext.group.section "連立方程式の求解" <| fun () ->
                    requireSquareMatrix this.GenerationContext matrix.size1 matrix.size2
                    requireSolveShape this.GenerationContext (y.size1 .=/ matrix.size1) "LAPACK right-hand side rows must match matrix order."
                    requireSolveShape this.GenerationContext (y.size2 .<= 0) "LAPACK right-hand side must have at least one column."
                    match this.GenerationContext.language with
                    |Fortran ->
                        this.GenerationContext.ch.iii <| fun (N,b,info) ->
                            N <== matrix.size1
                            b <== y.size2
                            this.GenerationContext.ch.i1 N <| fun ipiv ->
                                this.GenerationContext.codewritein("call zgesv("+N.code+","+b.code+","+matrix.code+","+N.code+","+ipiv.code+","+y.code+","+N.code+","+info.code+")"+"\n")
                                checkSolveInfo this.GenerationContext info
                    |C99 ->
                        this.GenerationContext.ch.iii <| fun (N,b,info) ->
                            N <== matrix.size1
                            b <== y.size2
                            this.GenerationContext.ch.i1 N <| fun ipiv ->
                                this.GenerationContext.elist.add "void zgesv_(int *n, int *nrhs, double complex *a, int *lda, int *ipiv, double complex *b, int *ldb, int *info)"
                                this.GenerationContext.codewritein("zgesv_(&"+N.code+","+"&"+b.code+","+matrix.code+",&"+N.code+","+ipiv.code+","+y.code+",&"+N.code+",&"+info.code+")"+";\n")
                                checkSolveInfo this.GenerationContext info
                    |LaTeX ->
                        this.GenerationContext.codewritein("$"+y.code+" \\leftarrow "+matrix.code+"^{-1}"+y.code+"$\\\\\n")
                    |HTML ->
                        this.GenerationContext.codewritein("\\("+y.code+" \\leftarrow "+matrix.code+"^{-1}"+y.code+"\\)<br/>\n")
                    |Python ->
                        this.RequirePythonLinalg "solve"
                        this.GenerationContext.codewritein(y.code+" = solve("+matrix.code+", "+y.code+")"+"\n")
                    |_ -> ()
                    
            ///<summary>連立方程式の求解</summary>
            ///<param name="matrix">係数行列</param>
            ///<param name="y">定数項ベクトルを列方向に並べた配列→解ベクトルを列方向に並べた配列</param>
            member this.solve_simuleqs (matrix:double2,y:double2) =
                requireSolveBackend this.GenerationContext
                if matrix.code = y.code then
                    invalidArg (nameof y) "LAPACK coefficient matrix and right-hand side must be different matrices."
                this.GenerationContext.olist.add "-llapack"
                this.GenerationContext.olist.add "-lblas"
                this.GenerationContext.group.section "連立方程式の求解" <| fun () ->
                    requireSquareMatrix this.GenerationContext matrix.size1 matrix.size2
                    requireSolveShape this.GenerationContext (y.size1 .=/ matrix.size1) "LAPACK right-hand side rows must match matrix order."
                    requireSolveShape this.GenerationContext (y.size2 .<= 0) "LAPACK right-hand side must have at least one column."
                    match this.GenerationContext.language with
                    |Fortran ->
                        this.GenerationContext.ch.iii <| fun (N,b,info) ->
                            N <== matrix.size1
                            b <== y.size2
                            this.GenerationContext.ch.i1 N <| fun ipiv ->
                                this.GenerationContext.codewritein("call dgesv("+N.code+","+b.code+","+matrix.code+","+N.code+","+ipiv.code+","+y.code+","+N.code+","+info.code+")"+"\n")
                                checkSolveInfo this.GenerationContext info
                    |C99 ->
                        this.GenerationContext.ch.iii <| fun (N,b,info) ->
                            N <== matrix.size1
                            b <== y.size2
                            this.GenerationContext.ch.i1 N <| fun ipiv ->
                                this.GenerationContext.elist.add "void dgesv_(int *n, int *nrhs, double *a, int *lda, int *ipiv, double *b, int *ldb, int *info)"
                                this.GenerationContext.codewritein("dgesv_(&"+N.code+","+"&"+b.code+","+matrix.code+",&"+N.code+","+ipiv.code+","+y.code+",&"+N.code+",&"+info.code+")"+";\n")
                                checkSolveInfo this.GenerationContext info
                    |LaTeX ->
                        this.GenerationContext.codewritein("$"+y.code+" \\leftarrow "+matrix.code+"^{-1}"+y.code+"$\\\\\n")
                    |HTML ->
                        this.GenerationContext.codewritein("\\("+y.code+" \\leftarrow "+matrix.code+"^{-1}"+y.code+"\\)<br/>\n")
                    |Python ->
                        this.RequirePythonLinalg "solve"
                        this.GenerationContext.codewritein(y.code+" = solve("+matrix.code+", "+y.code+")"+"\n")
                    |_ -> ()
                    
            ///<summary>逆行列の計算</summary>
            ///<param name="mat1">元の行列</param>
            ///<param name="mat2">mat1の逆行列</param>
            member this.inverse_matrix (mat2:double2,mat1:double2) =
                LapackValidation.requireBackend this.GenerationContext "LAPACK matrix inversion"
                this.GenerationContext.olist.add "-llapack"
                this.GenerationContext.olist.add "-lblas"
                this.GenerationContext.group.section "逆行列の計算" <| fun () ->
                    requireSquareMatrix this.GenerationContext mat1.size1 mat1.size2
                    requireSolveShape this.GenerationContext (mat2.size1 .=/ mat1.size1) "LAPACK inverse output shape must match matrix order."
                    requireSolveShape this.GenerationContext (mat2.size2 .=/ mat1.size1) "LAPACK inverse output shape must match matrix order."
                    let calculate (source:double2) =
                        mat2.clear()
                        this.GenerationContext.iter.num mat1.size1 <| fun i -> mat2[i,i] <== 1.0
                        match this.GenerationContext.language with
                        |Fortran ->
                            this.GenerationContext.ch.ii <| fun (npre,info) ->
                                npre<==mat1.size1
                                this.GenerationContext.ch.i1 npre <| fun ipiv ->
                                    ipiv.clear()
                                    this.GenerationContext.codewritein("call dgesv("+npre.code+", "+npre.code+","+source.code+", "+npre.code+", "+ipiv.code+","+mat2.code+", "+npre.code+", "+info.code+")\n")
                                    checkSolveInfo this.GenerationContext info
                        |C99 ->
                            this.GenerationContext.ch.ii <| fun (npre,info) ->
                                npre<==mat1.size1
                                this.GenerationContext.ch.i1 npre <| fun ipiv ->
                                    ipiv.clear()
                                    this.GenerationContext.elist.add "void dgesv_(int *n, int *nrhs, double *a, int *lda, int *ipiv, double *b, int *ldb, int *info)"
                                    this.GenerationContext.codewritein("dgesv_(&"+npre.code+","+"&"+npre.code+", "+source.code+", &"+npre.code+", "+ipiv.code+", "+mat2.code+", &"+npre.code+", &"+info.code+");\n")
                                    checkSolveInfo this.GenerationContext info
                        |LaTeX ->
                            this.GenerationContext.codewritein("$"+mat2.code+" \\leftarrow "+source.code+"^{-1}"+"$"+"\\\\\n")
                        |HTML ->
                            this.GenerationContext.codewritein("\\("+mat2.code+" \\leftarrow "+source.code+"^{-1}"+"\\)"+"<br/>\n")
                        |Python ->
                            this.GenerationContext.codewritein(mat2.code+" = numpy.linalg.inv("+source.code+")"+"\n")
                        |_ -> ()
                        
                    if mat2.code = mat1.code then
                        this.GenerationContext.ch.d2 (mat1.size1, mat1.size2) <| fun source ->
                            source <== mat1
                            calculate source
                    else calculate mat1
            ///<summary>逆行列の計算</summary>
            ///<param name="mat1">元の行列</param>
            ///<param name="mat2">mat1の逆行列</param>
            member this.inverse_matrix (mat2:complex2,mat1:complex2) =
                LapackValidation.requireBackend this.GenerationContext "LAPACK matrix inversion"
                this.GenerationContext.olist.add "-llapack"
                this.GenerationContext.olist.add "-lblas"
                this.GenerationContext.group.section "逆行列の計算" <| fun () ->
                    requireSquareMatrix this.GenerationContext mat1.size1 mat1.size2
                    requireSolveShape this.GenerationContext (mat2.size1 .=/ mat1.size1) "LAPACK inverse output shape must match matrix order."
                    requireSolveShape this.GenerationContext (mat2.size2 .=/ mat1.size1) "LAPACK inverse output shape must match matrix order."
                    let calculate (source:complex2) =
                        mat2.clear()
                        this.GenerationContext.iter.num mat1.size1 <| fun i -> mat2[i,i] <== 1.0
                        match this.GenerationContext.language with
                        |Fortran ->
                            this.GenerationContext.ch.ii <| fun (npre,info) ->
                                npre<==mat1.size1
                                this.GenerationContext.ch.i1 npre <| fun ipiv ->
                                    ipiv.clear()
                                    this.GenerationContext.codewritein("call zgesv("+npre.code+", "+npre.code+","+source.code+", "+npre.code+", "+ipiv.code+","+mat2.code+", "+npre.code+", "+info.code+")\n")
                                    checkSolveInfo this.GenerationContext info
                        |C99 ->
                            this.GenerationContext.ch.ii <| fun (npre,info) ->
                                npre<==mat1.size1
                                this.GenerationContext.ch.i1 npre <| fun ipiv ->
                                    ipiv.clear()
                                    this.GenerationContext.elist.add "void zgesv_(int *n, int *nrhs, double complex *a, int *lda, int *ipiv, double complex *b, int *ldb, int *info)"
                                    this.GenerationContext.codewritein("zgesv_(&"+npre.code+","+"&"+npre.code+", "+source.code+", &"+npre.code+", "+ipiv.code+", "+mat2.code+", &"+npre.code+", &"+info.code+");\n")
                                    checkSolveInfo this.GenerationContext info
                        |LaTeX ->
                            this.GenerationContext.codewritein("$"+mat2.code+" \\leftarrow "+source.code+"^{-1}"+"$"+"\\\\\n")
                        |HTML ->
                            this.GenerationContext.codewritein("\\("+mat2.code+" \\leftarrow "+source.code+"^{-1}"+"\\)"+"<br/>\n")
                        |Python ->
                            this.GenerationContext.codewritein(mat2.code+" = numpy.linalg.inv("+source.code+")"+"\n")
                        |_ -> ()
                    if mat2.code = mat1.code then
                        this.GenerationContext.ch.z2 (mat1.size1, mat1.size2) <| fun source ->
                            source <== mat1
                            calculate source
                    else calculate mat1
