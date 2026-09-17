//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    [<AutoOpen>]
    /// Adds base-10 log-absolute-determinant calculations to the linear-algebra context.
    module ContextLaDeterminantExtensions =
        /// Checks the LAPACK status of a determinant operation.
        let private checkDeterminantInfo (context:Aqualis) (info:int0) =
            // GETRF reports a positive INFO for a zero pivot: the determinant is zero.
            // A negative INFO is an invalid LAPACK argument and remains an error.
            match context.Language with
            | C99 ->
                context.codewritein("if (" + info.code +
                    " < 0) { fprintf(stderr, \"Aqualis: LAPACK determinant failed (INFO=%d).\\n\", " +
                    info.code + "); exit(EXIT_FAILURE); }\n")
            | Fortran ->
                context.codewritein("if (" + info.code +
                    " < 0) error stop 'Aqualis: LAPACK determinant failed.'\n")
            | _ -> ()

        /// LAPACK-backed linear algebra operations for an Aqualis context.
        type ContextLa with
            /// <summary>
            /// Computes the base-10 logarithm of the absolute determinant of a
            /// complex square matrix and passes it to a callback. A singular
            /// matrix yields negative infinity. The Fortran and C99 LAPACK paths
            /// overwrite the input during factorization.
            /// </summary>
            /// <param name="matrix">Square input matrix; overwritten by the Fortran and C99 paths.</param>
            /// <param name="code">Callback receiving the logarithm.</param>
            member this.determinant (matrix:complex2) = fun code ->
                LapackValidation.requireBackend this.GenerationContext "LAPACK determinant"
                this.GenerationContext.olist.add "-llapack"
                this.GenerationContext.olist.add "-lblas"
                this.GenerationContext.group.section "行列式の常用対数を計算" <| fun () ->
                    LapackValidation.require this.GenerationContext (matrix.size1 .<= 0) "LAPACK determinant matrix order must be positive."
                    LapackValidation.require this.GenerationContext (matrix.size1 .=/ matrix.size2) "LAPACK determinant matrix must be square."
                    this.GenerationContext.ch.d <| fun d ->
                        let calculateFromFactorizedDiagonal () =
                            d.clear()
                            this.GenerationContext.iter.num matrix.size1 <| fun i ->
                                d <== d + asm.log10(asm.abs(matrix[i,i]))
                            code d
                        let finishFactorization info =
                            checkDeterminantInfo this.GenerationContext info
                            this.GenerationContext.br.if2 (info .> 0)
                                (fun () ->
                                    d <== System.Double.NegativeInfinity
                                    code d)
                                calculateFromFactorizedDiagonal
                        match this.GenerationContext.language with
                        |Fortran ->
                            this.GenerationContext.ch.ii <| fun (N,info) ->
                                N <== matrix.size1
                                this.GenerationContext.ch.i1 N <| fun ipiv ->
                                    this.GenerationContext.codewritein("call zgetrf("+N.code+","+N.code+","+matrix.code+","+N.code+","+ipiv.code+","+info.code+")"+"\n")
                                    finishFactorization info
                        |C99 ->
                            this.GenerationContext.ch.ii <| fun (N,info) ->
                                N <== matrix.size1
                                this.GenerationContext.ch.i1 N <| fun ipiv ->
                                    this.GenerationContext.elist.add "void zgetrf_(int *, int *, double complex *, int *, int *, int *)"
                                    this.GenerationContext.codewritein("zgetrf_(&"+N.code+","+"&"+N.code+","+matrix.code+",&"+N.code+","+ipiv.code+",&"+info.code+")"+";\n")
                                    finishFactorization info
                        |LaTeX ->
                            this.GenerationContext.codewritein("$"+d.code+" = "+"\\left|"+matrix.code+"\\right|"+"$"+"\\\\\n")
                            calculateFromFactorizedDiagonal()
                        |HTML ->
                            this.GenerationContext.codewritein("\\("+d.code+" = "+"\\left|"+matrix.code+"\\right|"+"\\)"+"<br/>\n")
                            calculateFromFactorizedDiagonal()
                        |Python ->
                            this.GenerationContext.codewritein(
                                d.code + " = numpy.linalg.slogdet(" + matrix.code +
                                ")[1] / numpy.log(10.0)\n")
                            code d
                        |_ -> calculateFromFactorizedDiagonal()
    
            /// <summary>
            /// Computes the base-10 logarithm of the absolute determinant of a
            /// real square matrix and passes it to a callback. A singular matrix
            /// yields negative infinity. The Fortran and C99 LAPACK paths
            /// overwrite the input during factorization.
            /// </summary>
            /// <param name="matrix">Square input matrix; overwritten by the Fortran and C99 paths.</param>
            /// <param name="code">Callback receiving the logarithm.</param>
            member this.determinant (matrix:double2) = fun code ->
                LapackValidation.requireBackend this.GenerationContext "LAPACK determinant"
                this.GenerationContext.olist.add "-llapack"
                this.GenerationContext.olist.add "-lblas"
                this.GenerationContext.group.section "行列式の常用対数を計算" <| fun () ->
                    LapackValidation.require this.GenerationContext (matrix.size1 .<= 0) "LAPACK determinant matrix order must be positive."
                    LapackValidation.require this.GenerationContext (matrix.size1 .=/ matrix.size2) "LAPACK determinant matrix must be square."
                    this.GenerationContext.ch.d <| fun d ->
                        let calculateFromFactorizedDiagonal () =
                            d.clear()
                            this.GenerationContext.iter.num matrix.size1 <| fun i ->
                                d <== d + asm.log10(asm.abs(matrix[i,i]))
                            code d
                        let finishFactorization info =
                            checkDeterminantInfo this.GenerationContext info
                            this.GenerationContext.br.if2 (info .> 0)
                                (fun () ->
                                    d <== System.Double.NegativeInfinity
                                    code d)
                                calculateFromFactorizedDiagonal
                        match this.GenerationContext.language with
                        |Fortran ->
                            this.GenerationContext.ch.ii <| fun (N,info) ->
                                N <== matrix.size1
                                this.GenerationContext.ch.i1 N <| fun ipiv ->
                                    this.GenerationContext.codewritein("call dgetrf("+N.code+","+N.code+","+matrix.code+","+N.code+","+ipiv.code+","+info.code+")"+"\n")
                                    finishFactorization info
                        |C99 ->
                            this.GenerationContext.ch.ii <| fun (N,info) ->
                                N <== matrix.size1
                                this.GenerationContext.ch.i1 N <| fun ipiv ->
                                    this.GenerationContext.elist.add "void dgetrf_(int *, int *, double *, int *, int *, int *)"
                                    this.GenerationContext.codewritein("dgetrf_(&"+N.code+","+"&"+N.code+","+matrix.code+",&"+N.code+","+ipiv.code+",&"+info.code+")"+";\n")
                                    finishFactorization info
                        |LaTeX ->
                            this.GenerationContext.codewritein("$"+d.code+" = "+"\\left|"+matrix.code+"\\right|"+"$"+"\\\\\n")
                            calculateFromFactorizedDiagonal()
                        |HTML ->
                            this.GenerationContext.codewritein("\\("+d.code+" = "+"\\left|"+matrix.code+"\\right|"+"\\)"+"<br/>\n")
                            calculateFromFactorizedDiagonal()
                        |Python ->
                            this.GenerationContext.codewritein(
                                d.code + " = numpy.linalg.slogdet(" + matrix.code +
                                ")[1] / numpy.log(10.0)\n")
                            code d
                        |_ -> calculateFromFactorizedDiagonal()
