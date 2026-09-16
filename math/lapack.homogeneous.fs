//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    [<AutoOpen>]
    module ContextLaHomogeneousExtensions =
        let private requireHomogeneousShapes (context:Aqualis) (rows:int0) (columns:int0) (resultLength:int0) =
            LapackValidation.require context (rows .<= 0) "LAPACK homogeneous matrix rows must be positive."
            LapackValidation.require context (columns .<= 0) "LAPACK homogeneous matrix columns must be positive."
            LapackValidation.require context (resultLength .=/ columns) "LAPACK homogeneous solution length must match matrix columns."

        type ContextLa with
            /// <summary>
            /// 連立同次方程式を求解
            /// </summary>
            /// <param name="mat">複素係数行列</param>
            /// <param name="f">連立方程式の解</param>
            member this.solve_homogeneq (mat:double2,f:double1) =
                LapackValidation.requireBackend this.GenerationContext "LAPACK homogeneous solve"
                requireHomogeneousShapes this.GenerationContext mat.size1 mat.size2 f.size1
                this.GenerationContext.ch.i <| fun singularCount ->
                    this.GenerationContext.br.if2 (mat.size1 .< mat.size2)
                        (fun () -> singularCount <== mat.size1)
                        (fun () -> singularCount <== mat.size2)
                    this.GenerationContext.ch.d1 singularCount <| fun s ->
                    this.GenerationContext.ch.d2 (mat.size1, mat.size1) <| fun u ->
                    this.GenerationContext.ch.d2 (mat.size2, mat.size2) <| fun vt ->
                        this.svd mat (u,s,vt)
                        this.GenerationContext.group.comment "0に近いほど正確な解"
                        this.GenerationContext.print.tt <| "solve_homogeneq"++s[singularCount-1]
                        this.GenerationContext.iter.num mat.size2 <| fun i ->
                            f[i] <== vt[mat.size2-1,i]
    
            /// <summary>
            /// 連立同次方程式を求解
            /// </summary>
            /// <param name="mat">複素係数行列</param>
            /// <param name="f">連立方程式の解</param>
            member this.solve_homogeneq (mat:complex2,f:complex1) =
                LapackValidation.requireBackend this.GenerationContext "LAPACK homogeneous solve"
                requireHomogeneousShapes this.GenerationContext mat.size1 mat.size2 f.size1
                this.GenerationContext.ch.i <| fun singularCount ->
                    this.GenerationContext.br.if2 (mat.size1 .< mat.size2)
                        (fun () -> singularCount <== mat.size1)
                        (fun () -> singularCount <== mat.size2)
                    this.GenerationContext.ch.d1 singularCount <| fun s ->
                    this.GenerationContext.ch.z2 (mat.size1, mat.size1) <| fun u ->
                    this.GenerationContext.ch.z2 (mat.size2, mat.size2) <| fun vt ->
                        this.svd mat (u,s,vt)
                        this.GenerationContext.group.comment "0に近いほど正確な解"
                        this.GenerationContext.print.tt <| "solve_homogeneq"++s[singularCount-1]
                        this.GenerationContext.iter.num mat.size2 <| fun i ->
                            f[i] <== asm.conj(vt[mat.size2-1,i])
