//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    [<AutoOpen>]
    module ContextLaSvdExtensions =
        let private requireSvdShapes (context:Aqualis) (rows:int0) (columns:int0)
                                     (uRows:int0) (uColumns:int0) (singularCount:int0)
                                     (vtRows:int0) (vtColumns:int0) =
            match context.language with
            | C99 | Fortran ->
                LapackValidation.require context (rows .<= 0) "LAPACK SVD matrix rows must be positive."
                LapackValidation.require context (columns .<= 0) "LAPACK SVD matrix columns must be positive."
                LapackValidation.require context (uRows .=/ rows) "LAPACK SVD U shape is invalid."
                LapackValidation.require context (uColumns .=/ rows) "LAPACK SVD U shape is invalid."
                LapackValidation.require context (vtRows .=/ columns) "LAPACK SVD VT shape is invalid."
                LapackValidation.require context (vtColumns .=/ columns) "LAPACK SVD VT shape is invalid."
                context.ch.i <| fun expected ->
                    context.br.if2 (rows .< columns) (fun () -> expected <== rows) (fun () -> expected <== columns)
                    LapackValidation.require context (singularCount .=/ expected) "LAPACK SVD singular-value count is invalid."
            | _ -> ()

        type ContextLa with
            /// <summary>
            /// mat = u * s * v に特異値分解
            /// </summary>
            /// <param name="mat1">複素行列</param>
            /// <param name="u">複素行列u</param>
            /// <param name="s">正方行列sの対角成分</param>
            /// <param name="vt">複素行列vの転置</param>
            member this.svd (mat1:complex2) = fun (u:complex2,s:double1,vt:complex2) ->
                this.GenerationContext.olist.add "-llapack"
                this.GenerationContext.olist.add "-lblas"
                requireSvdShapes this.GenerationContext mat1.size1 mat1.size2 u.size1 u.size2 s.size1 vt.size1 vt.size2
                match this.GenerationContext.language with
                |LaTeX ->
                    this.GenerationContext.codewritein("$"+mat1.code+" = "+u.code+s.code+vt.code+"^{\\mathrm{T}}"+"$\\\\\n")
                |HTML ->
                    this.GenerationContext.codewritein("\\("+mat1.code+" = "+u.code+s.code+vt.code+"^{\\mathrm{T}}"+"\\)<br/>\n")
                |Python ->
                    this.RequirePythonLinalg "svd"
                    //左特異ベクトルu.code、特異値s.code、右特異ベクトルvt.codeを求める
                    this.GenerationContext.codewritein(u.code+","+s.code+","+vt.code+" = svd("+mat1.code+")"+"\n")
                |_ ->
                    this.GenerationContext.group.section "非対称複素行列の特異値分解" <| fun () ->
                        s.clear()
                        u.clear()
                        vt.clear()
                        match this.GenerationContext.language with
                        |Fortran ->
                            this.GenerationContext.ch.iiii <| fun (m,n,lda,info) ->
                            this.GenerationContext.ch.i <| fun ns ->
                                m <== mat1.size1
                                n <== mat1.size2
                                this.GenerationContext.br.if2 (m.<n) (fun () -> ns <== m) (fun () -> ns <== n)
                                this.GenerationContext.ch.ii <| fun (ldu,ldvt) ->
                                this.GenerationContext.ch.i <| fun lwork ->
                                    this.GenerationContext.ch.z01 <| fun work ->
                                    this.GenerationContext.ch.d1 (5*ns) <| fun rwork ->
                                        lda <== m
                                        ldu <== u.size1
                                        ldvt <== vt.size2
                                        lwork <== -1
                                        work.allocate 1
                                        this.GenerationContext.codewritein("call zgesvd("+
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
                                        LapackValidation.checkInfo this.GenerationContext info "SVD workspace query"
                                        lwork <== asm.toint work[0].re
                                        work.deallocate()
                                        work.allocate lwork
                                        this.GenerationContext.codewritein("call zgesvd("+
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
                                        LapackValidation.checkInfo this.GenerationContext info "SVD"
                                        work.deallocate()
                        |C99 ->
                            this.GenerationContext.ch.iiii <| fun (m,n,lda,info) ->
                            this.GenerationContext.ch.i <| fun ns ->
                                m <== mat1.size1
                                n <== mat1.size2
                                this.GenerationContext.br.if2 (m.<n) (fun () -> ns <== m) (fun () -> ns <== n)
                                this.GenerationContext.ch.ii <| fun (ldu,ldvt) ->
                                this.GenerationContext.ch.i <| fun lwork ->
                                    this.GenerationContext.ch.z01 <| fun work ->
                                    this.GenerationContext.ch.z <| fun wkopt ->
                                    this.GenerationContext.ch.d1 (5*ns) <| fun rwork ->
                                    this.GenerationContext.ch.c <| fun jobu ->
                                    this.GenerationContext.ch.c <| fun jobv ->
                                        match jobu,jobv with
                                        |Var(_,jobu,_),Var(_,jobv,_) ->
                                            lda <== m
                                            ldu <== u.size1
                                            ldvt <== vt.size2
                                            lwork <== -1
                                            this.GenerationContext.elist.add "void zgesvd_(char *, char *, int *, int *, double complex *, int *, double *, double complex *, int *, double complex *, int *, double complex *, int *, double *, int *)"
                                            this.GenerationContext.codewritein(jobu + " = 'A';")
                                            this.GenerationContext.codewritein(jobv + " = 'A';")
                                            this.GenerationContext.codewritein("zgesvd_(" +
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
                                            LapackValidation.checkInfo this.GenerationContext info "SVD workspace query"
                                            lwork <== asm.toint wkopt.re
                                            work.allocate lwork
                                            this.GenerationContext.codewritein("zgesvd_(" +
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
                                            LapackValidation.checkInfo this.GenerationContext info "SVD"
                                            work.deallocate()
                                        |_ -> ()
                        |Python ->
                            this.RequirePythonLinalg "svd"
                            //左特異ベクトルu.code、特異値s.code、右特異ベクトルvt.codeを求める
                            this.GenerationContext.codewritein(u.code+","+s.code+","+vt.code+" = svd("+mat1.code+")"+"\n")
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
                this.GenerationContext.olist.add "-llapack"
                this.GenerationContext.olist.add "-lblas"
                requireSvdShapes this.GenerationContext mat1.size1 mat1.size2 u.size1 u.size2 s.size1 vt.size1 vt.size2
                match this.GenerationContext.language with
                |LaTeX ->
                    this.GenerationContext.codewritein("$"+mat1.code+" = "+u.code+s.code+vt.code+"^{\\mathrm{T}}"+"$\\\\\n")
                |HTML ->
                    this.GenerationContext.codewritein("\\("+mat1.code+" = "+u.code+s.code+vt.code+"^{\\mathrm{T}}"+"\\)<br/>\n")
                |Python ->
                    this.RequirePythonLinalg "svd"
                    //左特異ベクトルu.code、特異値s.code、右特異ベクトルvt.codeを求める
                    this.GenerationContext.codewritein(u.code+","+s.code+","+vt.code+" = svd("+mat1.code+")"+"\n")
                |_ ->
                    this.GenerationContext.group.section "非対称実行列の特異値分解" <| fun () ->
                        s.clear()
                        u.clear()
                        vt.clear()
                        match this.GenerationContext.language with
                        |Fortran ->
                            this.GenerationContext.ch.iiii <| fun (m,n,lda,info) ->
                                m <== mat1.size1
                                n <== mat1.size2
                                this.GenerationContext.ch.ii <| fun (ldu,ldvt) ->
                                this.GenerationContext.ch.i <| fun lwork ->
                                    this.GenerationContext.ch.d01 <| fun work ->
                                        lda <== m
                                        ldu <== u.size1
                                        ldvt <== vt.size2
                                        lwork <== -1
                                        work.allocate 1
                                        this.GenerationContext.codewritein("call dgesvd("+
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
                                        LapackValidation.checkInfo this.GenerationContext info "SVD workspace query"
                                        lwork <== asm.toint work[0]
                                        work.deallocate()
                                        work.allocate lwork
                                        this.GenerationContext.codewritein("call dgesvd("+
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
                                        LapackValidation.checkInfo this.GenerationContext info "SVD"
                                        work.deallocate()
                        |C99 ->
                            this.GenerationContext.ch.iiii <| fun (m,n,lda,info) ->
                                m <== mat1.size1
                                n <== mat1.size2
                                this.GenerationContext.ch.ii <| fun (ldu,ldvt) ->
                                this.GenerationContext.ch.i <| fun lwork ->
                                    this.GenerationContext.ch.d01 <| fun work ->
                                    this.GenerationContext.ch.c <| fun jobu ->
                                    this.GenerationContext.ch.c <| fun jobv ->
                                        match jobu,jobv with
                                        |Var(_,jobu,_),Var(_,jobv,_) ->
                                            lda <== m
                                            ldu <== u.size1
                                            ldvt <== vt.size2
                                            this.GenerationContext.elist.add "void dgesvd_(char *, char *, int *, int *, double *, int *, double *, double *, int *, double *, int *, double *, int *, int *)"
                                            this.GenerationContext.codewritein(jobu + " = 'A';")
                                            this.GenerationContext.codewritein(jobv + " = 'A';")
                                            lwork <== -1
                                            work.allocate 1
                                            this.GenerationContext.codewritein("dgesvd_(" +
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
                                            LapackValidation.checkInfo this.GenerationContext info "SVD workspace query"
                                            lwork <== asm.toint work[0]
                                            work.deallocate()
                                            work.allocate lwork
                                            this.GenerationContext.codewritein("dgesvd_(" +
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
                                            LapackValidation.checkInfo this.GenerationContext info "SVD"
                                            work.deallocate()
                                        |_ -> ()
                        |Python ->
                            this.RequirePythonLinalg "svd"
                            //左特異ベクトルu.code、特異値s.code、右特異ベクトルvt.codeを求める
                            this.GenerationContext.codewritein(u.code+","+s.code+","+vt.code+" = svd("+mat1.code+")"+"\n")
                        |_ ->
                            ()
