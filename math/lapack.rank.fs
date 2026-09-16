//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    [<AutoOpen>]
    module ContextLaRankExtensions =
        type ContextLa with
            ///<summary>行列の階数</summary>
            ///<param name="rank">行列matの階数</param>
            ///<param name="mat">行列</param>
            ///<param name="cond">0とみなす上限値</param>
            member this.rank (rank:double0,mat:complex2,cond:double0) =
                this.GenerationContext.olist.add "-llapack"
                this.GenerationContext.olist.add "-lblas"
                this.GenerationContext.group.section "行列の階数" <| fun () ->
                    LapackValidation.require this.GenerationContext (mat.size1 .<= 0) "LAPACK rank matrix rows must be positive."
                    LapackValidation.require this.GenerationContext (mat.size2 .<= 0) "LAPACK rank matrix columns must be positive."
                    this.GenerationContext.ch.iii <| fun (m,n,ns) ->
                        m <== mat.size1
                        n <== mat.size2
                        this.GenerationContext.br.if2 (m.<n) (fun () -> ns <== m) (fun () -> ns <== n)
                        this.GenerationContext.ch.d1 ns <| fun s ->
                        rank.clear()
                        let countRank () =
                            s.foreach <| fun i ->
                                this.GenerationContext.br.if1 (s[i] .> cond) <| fun () -> rank <== rank + 1
                        match this.GenerationContext.language with
                        |Fortran ->
                            this.GenerationContext.ch.iii <| fun (lda,info,lwork) ->
                            this.GenerationContext.ch.ii <| fun (ldu,ldvt) ->
                            this.GenerationContext.ch.z1 1 <| fun u ->
                            this.GenerationContext.ch.z1 1 <| fun vt ->
                            this.GenerationContext.ch.z01 <| fun work ->
                            this.GenerationContext.ch.d1 (5*ns) <| fun rwork ->
                            this.GenerationContext.ch.i1 (8*ns) <| fun iwork ->
                                this.GenerationContext.br.if2 (m .< 1) (fun () -> lda <== 1) (fun () -> lda <== m)
                                ldu <== 1
                                ldvt <== 1
                                lwork <== -1
                                work.allocate 1
                                let callZgesdd () =
                                    this.GenerationContext.codewritein("call zgesdd('N', " +
                                        m.code + ", " + n.code + ", " + mat.code + ", " +
                                        lda.code + ", " + s.code + ", " + u.code + ", " +
                                        ldu.code + ", " + vt.code + ", " + ldvt.code + ", " +
                                        work.code + ", " + lwork.code + ", " + rwork.code + ", " +
                                        iwork.code + ", " + info.code + ")")
                                callZgesdd()
                                LapackValidation.checkInfo this.GenerationContext info "rank workspace query"
                                lwork <== asm.toint work[0].re
                                work.deallocate()
                                work.allocate lwork
                                callZgesdd()
                                work.deallocate()
                                LapackValidation.checkInfo this.GenerationContext info "rank"
                                countRank()
                        |C99 ->
                            this.GenerationContext.ch.iii <| fun (lda,info,lwork) ->
                            this.GenerationContext.ch.ii <| fun (ldu,ldvt) ->
                            this.GenerationContext.ch.z1 1 <| fun u ->
                            this.GenerationContext.ch.z1 1 <| fun vt ->
                            this.GenerationContext.ch.z01 <| fun work ->
                            this.GenerationContext.ch.d1 (5*ns) <| fun rwork ->
                            this.GenerationContext.ch.i1 (8*ns) <| fun iwork ->
                            this.GenerationContext.ch.c <| fun jobz ->
                                match jobz with
                                |Var(_,name,_) ->
                                    this.GenerationContext.br.if2 (m .< 1) (fun () -> lda <== 1) (fun () -> lda <== m)
                                    ldu <== 1
                                    ldvt <== 1
                                    lwork <== -1
                                    this.GenerationContext.elist.add "void zgesdd_(char *, int *, int *, double complex *, int *, double *, double complex *, int *, double complex *, int *, double complex *, int *, double *, int *, int *)"
                                    this.GenerationContext.codewritein(name + " = 'N';")
                                    work.allocate 1
                                    let callZgesdd () =
                                        this.GenerationContext.codewritein("zgesdd_(" +
                                            "&" + name + ", &" + m.code + ", &" + n.code + ", " +
                                            mat.code + ", &" + lda.code + ", " + s.code + ", " +
                                            u.code + ", &" + ldu.code + ", " + vt.code + ", &" +
                                            ldvt.code + ", " + work.code + ", &" + lwork.code + ", " +
                                            rwork.code + ", " + iwork.code + ", &" + info.code + ");")
                                    callZgesdd()
                                    LapackValidation.checkInfo this.GenerationContext info "rank workspace query"
                                    lwork <== asm.toint work[0].re
                                    work.deallocate()
                                    work.allocate lwork
                                    callZgesdd()
                                    work.deallocate()
                                    LapackValidation.checkInfo this.GenerationContext info "rank"
                                    countRank()
                                |_ -> ()
                        |LaTeX ->
                            this.GenerationContext.codewritein("$"+rank.code+" \\leftarrow "+"\\mathrm{rank}\\left["+mat.code+"\\right]"+"$\\\\\n")
                        |HTML ->
                            this.GenerationContext.codewritein("\\("+rank.code+" \\leftarrow "+"\\mathrm{rank}\\left["+mat.code+"\\right]"+"\\)<br/>\n")
                        |Python ->
                            this.RequirePythonLinalg "svd"
                            this.GenerationContext.codewritein("_,"+s.code+",_ = svd("+mat.code+")"+"\n")
                            this.GenerationContext.codewritein(rank.code+" = numpy.sum("+s.code+" > "+cond.Expr.eval this.GenerationContext+")"+"\n")
                        |_ -> ()
    
            ///<summary>行列の階数</summary>
            ///<param name="rank">行列matの階数</param>
            ///<param name="mat">行列</param>
            ///<param name="cond">0とみなす上限値</param>
            member this.rank (rank:int0,mat:double2,cond:double0) =
                this.GenerationContext.olist.add "-llapack"
                this.GenerationContext.olist.add "-lblas"
                this.GenerationContext.group.section "行列の階数" <| fun () ->
                    LapackValidation.require this.GenerationContext (mat.size1 .<= 0) "LAPACK rank matrix rows must be positive."
                    LapackValidation.require this.GenerationContext (mat.size2 .<= 0) "LAPACK rank matrix columns must be positive."
                    this.GenerationContext.ch.iii <| fun (m,n,ns) ->
                        m <== mat.size1
                        n <== mat.size2
                        this.GenerationContext.br.if2 (m.<n) (fun () -> ns <== m) (fun () -> ns <== n)
                        this.GenerationContext.ch.d1 ns <| fun s ->
                        rank.clear()
                        let countRank () =
                            s.foreach <| fun i ->
                                this.GenerationContext.br.if1 (s[i] .> cond) <| fun () -> rank.inc()
                        match this.GenerationContext.language with
                        |Fortran ->
                            this.GenerationContext.ch.iii <| fun (lda,info,lwork) ->
                            this.GenerationContext.ch.ii <| fun (ldu,ldvt) ->
                            this.GenerationContext.ch.d1 1 <| fun u ->
                            this.GenerationContext.ch.d1 1 <| fun vt ->
                            this.GenerationContext.ch.d01 <| fun work ->
                            this.GenerationContext.ch.i1 (8*ns) <| fun iwork ->
                                this.GenerationContext.br.if2 (m .< 1) (fun () -> lda <== 1) (fun () -> lda <== m)
                                ldu <== 1
                                ldvt <== 1
                                lwork <== -1
                                work.allocate 1
                                let callDgesdd () =
                                    this.GenerationContext.codewritein("call dgesdd('N', " +
                                        m.code + ", " + n.code + ", " + mat.code + ", " +
                                        lda.code + ", " + s.code + ", " + u.code + ", " +
                                        ldu.code + ", " + vt.code + ", " + ldvt.code + ", " +
                                        work.code + ", " + lwork.code + ", " + iwork.code + ", " +
                                        info.code + ")")
                                callDgesdd()
                                LapackValidation.checkInfo this.GenerationContext info "rank workspace query"
                                lwork <== asm.toint work[0]
                                work.deallocate()
                                work.allocate lwork
                                callDgesdd()
                                work.deallocate()
                                LapackValidation.checkInfo this.GenerationContext info "rank"
                                countRank()
                        |C99 ->
                            this.GenerationContext.ch.iii <| fun (lda,info,lwork) ->
                            this.GenerationContext.ch.ii <| fun (ldu,ldvt) ->
                            this.GenerationContext.ch.d1 1 <| fun u ->
                            this.GenerationContext.ch.d1 1 <| fun vt ->
                            this.GenerationContext.ch.d01 <| fun work ->
                            this.GenerationContext.ch.i1 (8*ns) <| fun iwork ->
                            this.GenerationContext.ch.c <| fun jobz ->
                                match jobz with
                                |Var(_,name,_) ->
                                    this.GenerationContext.br.if2 (m .< 1) (fun () -> lda <== 1) (fun () -> lda <== m)
                                    ldu <== 1
                                    ldvt <== 1
                                    lwork <== -1
                                    this.GenerationContext.elist.add "void dgesdd_(char *, int *, int *, double *, int *, double *, double *, int *, double *, int *, double *, int *, int *, int *)"
                                    this.GenerationContext.codewritein(name + " = 'N';")
                                    work.allocate 1
                                    let callDgesdd () =
                                        this.GenerationContext.codewritein("dgesdd_(" +
                                            "&" + name + ", &" + m.code + ", &" + n.code + ", " +
                                            mat.code + ", &" + lda.code + ", " + s.code + ", " +
                                            u.code + ", &" + ldu.code + ", " + vt.code + ", &" +
                                            ldvt.code + ", " + work.code + ", &" + lwork.code + ", " +
                                            iwork.code + ", &" + info.code + ");")
                                    callDgesdd()
                                    LapackValidation.checkInfo this.GenerationContext info "rank workspace query"
                                    lwork <== asm.toint work[0]
                                    work.deallocate()
                                    work.allocate lwork
                                    callDgesdd()
                                    work.deallocate()
                                    LapackValidation.checkInfo this.GenerationContext info "rank"
                                    countRank()
                                |_ -> ()
                        |LaTeX ->
                            this.GenerationContext.codewritein("$"+rank.code+" \\leftarrow "+"\\mathrm{rank}\\left["+mat.code+"\\right]"+"$\\\\\n")
                        |HTML ->
                            this.GenerationContext.codewritein("\\("+rank.code+" \\leftarrow "+"\\mathrm{rank}\\left["+mat.code+"\\right]"+"\\)<br/>\n")
                        |Python ->
                            this.RequirePythonLinalg "svd"
                            this.GenerationContext.codewritein("_,"+s.code+",_ = svd("+mat.code+")"+"\n")
                            this.GenerationContext.codewritein(rank.code+" = numpy.sum("+s.code+" > "+cond.Expr.eval this.GenerationContext+")"+"\n")
                        |_ -> ()
    
            ///<summary>疑似逆行列の計算</summary>
            ///<param name="mat2">matの疑似逆行列</param>
            ///<param name="mat">行列</param>
            ///<param name="cond">特異値を0とみなす上限値</param>
            member this.inverse_matrix2 (mat2:complex2,mat:complex2,cond:double0) =
                this.GenerationContext.group.section "疑似逆行列" <| fun () ->
                    this.GenerationContext.ch.i <| fun ns ->
                        this.GenerationContext.br.if2  (mat.size1.<mat.size2)
                        <| fun () ->
                            ns <== mat.size1
                        <| fun () ->
                            ns <== mat.size2
                        this.GenerationContext.ch.d1 ns <| fun s ->
                        this.GenerationContext.ch.z2 (mat.size1, mat.size1) <| fun u ->
                        this.GenerationContext.ch.z2 (mat.size2, mat.size2) <| fun vt ->
                        this.GenerationContext.ch.z2 (mat.size2, mat.size1) <| fun u2 ->
                            this.svd mat (u,s,vt)
                            //特異値分解した行列をもとに、疑似逆行列は (v^*)×(s^-1)×(u^*)
                            u2.clear()
                            this.GenerationContext.iter.num ns <| fun i ->
                                this.GenerationContext.iter.num u.size1 <| fun j ->
                                    //condより小さい特異値は無視
                                    this.GenerationContext.br.if1 (s[i]/s[0] .> cond) <| fun () ->
                                        u2[i,j] <== asm.conj(u[j,i]) / s[i]
                            mat2.clear()
                            this.GenerationContext.iter.num vt.size2 <| fun i ->
                                this.GenerationContext.iter.num u2.size2  <| fun j ->
                                    this.GenerationContext.iter.num u2.size1 <| fun p ->
                                        mat2[i,j] <== mat2[i,j] + asm.conj(vt[p,i])*u2[p,j]
                                        
            ///<summary>疑似逆行列の計算</summary>
            ///<param name="mat2">matの疑似逆行列</param>
            ///<param name="mat">行列</param>
            ///<param name="cond">特異値を0とみなす上限値</param>
            member this.inverse_matrix2 (mat2:double2,mat:double2,cond:double0) =
                this.GenerationContext.group.section "疑似逆行列" <| fun () ->
                    this.GenerationContext.ch.i <| fun ns ->
                        this.GenerationContext.br.if2  (mat.size1.<mat.size2)
                        <| fun () ->
                            ns <== mat.size1
                        <| fun () ->
                            ns <== mat.size2
                        this.GenerationContext.ch.d1 ns <| fun s ->
                        this.GenerationContext.ch.d2 (mat.size1, mat.size1) <| fun u ->
                        this.GenerationContext.ch.d2 (mat.size2, mat.size2) <| fun vt ->
                        this.GenerationContext.ch.d2 (mat.size2, mat.size1) <| fun u2 ->
                            this.svd mat (u,s,vt)
                            //特異値分解した行列をもとに、疑似逆行列は (v^*)×(s^-1)×(u^*)
                            u2.clear()
                            this.GenerationContext.iter.num ns <| fun i ->
                                this.GenerationContext.iter.num u.size1 <| fun j ->
                                    this.GenerationContext.br.if1 (s[i]/s[0] .> cond) <| fun () ->
                                        u2[i,j] <== u[j,i] / s[i]
                            mat2.clear()
                            this.GenerationContext.iter.num vt.size2 <| fun i ->
                                this.GenerationContext.iter.num u2.size2  <| fun j ->
                                    this.GenerationContext.iter.num u2.size1 <| fun p ->
                                        mat2[i,j] <== mat2[i,j] + vt[p,i] * u2[p,j]
