//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    [<AutoOpen>]
    module ContextLaHomogeneousExtensions =
        type ContextLa with
            /// <summary>
            /// 連立同次方程式を求解
            /// </summary>
            /// <param name="mat">複素係数行列</param>
            /// <param name="f">連立方程式の解</param>
            member this.solve_homogeneq (mat:double2,f:double1) =
                    this.GenerationContext.ch.d1 mat.size1 <| fun s ->
                    this.GenerationContext.ch.d2 (mat.size1, mat.size2) <| fun u ->
                    this.GenerationContext.ch.d2 (mat.size1, mat.size2) <| fun vt ->
                        this.svd mat (u,s,vt)
                        this.GenerationContext.group.comment "0に近いほど正確な解"
                        this.GenerationContext.print.tt <| "solve_homogeneq"++s[mat.size1-1]
                        this.GenerationContext.iter.num mat.size1 <| fun i ->
                            f[i] <== vt[mat.size1,i]
    
            /// <summary>
            /// 連立同次方程式を求解
            /// </summary>
            /// <param name="mat">複素係数行列</param>
            /// <param name="f">連立方程式の解</param>
            member this.solve_homogeneq (mat:complex2,f:complex1) =
                    this.GenerationContext.ch.d1 mat.size1 <| fun s ->
                    this.GenerationContext.ch.z2 (mat.size1, mat.size2) <| fun u ->
                    this.GenerationContext.ch.z2 (mat.size1, mat.size2) <| fun vt ->
                        this.svd mat (u,s,vt)
                        this.GenerationContext.group.comment "0に近いほど正確な解"
                        this.GenerationContext.print.tt <| "solve_homogeneq"++s[mat.size1-1]
                        this.GenerationContext.iter.num mat.size1 <| fun i ->
                            f[i] <== asm.conj(vt[mat.size1,i])
