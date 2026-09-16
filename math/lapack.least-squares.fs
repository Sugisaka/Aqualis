//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    [<AutoOpen>]
    module ContextLaLeastSquaresExtensions =
        let private requireTikhonovVectorShapes (context:Aqualis) (rows:int0) (columns:int0) (rhsLength:int0) =
            LapackValidation.require context (rows .<= 0) "LAPACK Tikhonov matrix rows must be positive."
            LapackValidation.require context (columns .<= 0) "LAPACK Tikhonov matrix columns must be positive."
            LapackValidation.require context (rhsLength .=/ rows) "LAPACK Tikhonov right-hand side length must match matrix rows."

        let private requireTikhonovColumnShapes (context:Aqualis) (rows:int0) (columns:int0)
                                                (rhsRows:int0) (rhsColumns:int0) =
            requireTikhonovVectorShapes context rows columns rhsRows
            LapackValidation.require context (rhsColumns .=/ 1) "LAPACK Tikhonov right-hand side must have one column."

        let private requireFiniteTikhonovValue (context:Aqualis) (value:double0) =
            let expression = value.Expr.eval context
            let condition =
                match context.Language with
                | C99 -> Some("!isfinite(" + expression + ")")
                | Fortran -> Some(".not. ieee_is_finite(" + expression + ")")
                | Python -> Some("not numpy.isfinite(" + expression + ")")
                | _ -> None
            condition |> Option.iter (fun expression ->
                LapackValidation.require context
                    (bool0(Var(Nt, expression, NaN), context))
                    "LAPACK Tikhonov calculation overflowed or produced a non-finite value.")

        let private checkRealTikhonovIntermediates (context:Aqualis) (matrix:double2) (vector:double1) =
            match context.Language with
            | C99 | Fortran | Python ->
                matrix.foreach <| fun (i,j) -> requireFiniteTikhonovValue context matrix[i,j]
                vector.foreach <| fun i -> requireFiniteTikhonovValue context vector[i]
            | _ -> ()

        let private checkComplexTikhonovIntermediates (context:Aqualis) (matrix:complex2) (vector:complex1) =
            match context.Language with
            | C99 | Fortran | Python ->
                matrix.foreach <| fun (i,j) ->
                    requireFiniteTikhonovValue context matrix[i,j].re
                    requireFiniteTikhonovValue context matrix[i,j].im
                vector.foreach <| fun i ->
                    requireFiniteTikhonovValue context vector[i].re
                    requireFiniteTikhonovValue context vector[i].im
            | _ -> ()

        let private checkRealTikhonovSolution (context:Aqualis) (vector:double1) =
            match context.Language with
            | C99 | Fortran | Python -> vector.foreach <| fun i -> requireFiniteTikhonovValue context vector[i]
            | _ -> ()

        let private checkComplexTikhonovSolution (context:Aqualis) (vector:complex1) =
            match context.Language with
            | C99 | Fortran | Python ->
                vector.foreach <| fun i ->
                    requireFiniteTikhonovValue context vector[i].re
                    requireFiniteTikhonovValue context vector[i].im
            | _ -> ()

        type ContextLa with
            /// <summary>
            /// 連立方程式の求解(Tikhonovの正則化法)
            /// </summary>
            /// <param name="fu_mat">係数行列</param>
            /// <param name="fu_cst">定数項ベクトル</param>
            /// <param name="code">解に対して行う処理</param>
            member this.solve_simuleq_t(fu_mat:double2,fu_cst:double1) = fun code ->
                this.GenerationContext.group.h2 "連立方程式の求解(Tikhonovの正則化法)" <| fun () ->
                    requireTikhonovVectorShapes this.GenerationContext fu_mat.size1 fu_mat.size2 fu_cst.size1
                    this.GenerationContext.ch.d2 (fu_mat.size2, fu_mat.size2) <| fun FF ->
                    this.GenerationContext.ch.d1 fu_mat.size2 <| fun bb ->
                        let lambda = 1E-6 //正則化パラメータ
                        FF.clear()
                        //FF = fu_mat^T * fu_mat
                        this.GenerationContext.ch.d <| fun tmp ->
                            this.GenerationContext.iter.num FF.size1 <| fun i ->
                                this.GenerationContext.iter.num FF.size2 <| fun j ->
                                    tmp.clear()
                                    this.GenerationContext.iter.num fu_mat.size1 <| fun k ->
                                        tmp <== tmp + fu_mat[k,i]*fu_mat[k,j]
                                    FF[i,j] <== tmp
    
                        //FF = fu_mat^T * fu_mat + λ^2 * I
                        this.GenerationContext.iter.num FF.size1 <| fun i ->
                                FF[i,i] <== FF[i,i] + lambda * lambda
                        //bb = fu_mat^T * fu_cst
                        this.GenerationContext.ch.d <| fun tmp ->
                            bb.clear()
                            this.GenerationContext.iter.num bb.size1 <| fun i ->
                                tmp.clear()
                                this.GenerationContext.iter.num fu_cst.size1 <| fun k ->
                                    tmp <== tmp + fu_mat[k,i]*fu_cst[k]
                                bb[i] <== tmp
                        checkRealTikhonovIntermediates this.GenerationContext FF bb
                        this.solve_simuleq(FF,bb)
                        checkRealTikhonovSolution this.GenerationContext bb
                        code bb
    
            /// <summary>
            /// 連立方程式の求解(Tikhonovの正則化法)
            /// </summary>
            /// <param name="fu_mat">係数行列</param>
            /// <param name="fu_cst">定数項ベクトル</param>
            /// <param name="code">解に対して行う処理</param>
            member this.solve_simuleq_t(fu_mat:complex2,fu_cst:complex1) = fun code ->
                this.GenerationContext.group.h2 "連立方程式の求解(Tikhonovの正則化法)" <| fun () ->
                    requireTikhonovVectorShapes this.GenerationContext fu_mat.size1 fu_mat.size2 fu_cst.size1
                    this.GenerationContext.ch.z2 (fu_mat.size2, fu_mat.size2) <| fun FF ->
                    this.GenerationContext.ch.z1 fu_mat.size2 <| fun bb ->
                        let lambda = 1E-6 //正則化パラメータ
                        FF.clear()
                        //FF = fu_mat^H * fu_mat
                        this.GenerationContext.ch.z <| fun tmp ->
                            this.GenerationContext.iter.num FF.size1 <| fun i ->
                                this.GenerationContext.iter.num FF.size2 <| fun j ->
                                    tmp.clear()
                                    this.GenerationContext.iter.num fu_mat.size1 <| fun k ->
                                        tmp <== tmp + asm.conj(fu_mat[k,i])*fu_mat[k,j]
                                    FF[i,j] <== tmp
    
                        //FF = fu_mat^H * fu_mat + λ^2 * I
                        this.GenerationContext.iter.num FF.size1 <| fun i ->
                                FF[i,i] <== FF[i,i] + lambda * lambda
                        //bb = fu_mat^H * fu_cst
                        this.GenerationContext.ch.z <| fun tmp ->
                            bb.clear()
                            this.GenerationContext.iter.num bb.size1 <| fun i ->
                                tmp.clear()
                                this.GenerationContext.iter.num fu_cst.size1 <| fun k ->
                                    tmp <== tmp + asm.conj(fu_mat[k,i])*fu_cst[k]
                                bb[i] <== tmp
                        checkComplexTikhonovIntermediates this.GenerationContext FF bb
                        this.solve_simuleq(FF,bb)
                        checkComplexTikhonovSolution this.GenerationContext bb
                        code bb
    
            /// <summary>
            /// 連立方程式の求解(Tikhonovの正則化法)
            /// </summary>
            /// <param name="fu_mat">係数行列</param>
            /// <param name="fu_cst">定数項ベクトル</param>
            /// <param name="lambda">正則化パラメータ</param>
            /// <param name="code">解に対して行う処理</param>
            member this.solve_simuleq_tt(fu_mat:complex2,fu_cst:complex1,lambda:double) = fun code ->
                this.GenerationContext.group.h2 "連立方程式の求解(Tikhonovの正則化法)" <| fun () ->
                    requireTikhonovVectorShapes this.GenerationContext fu_mat.size1 fu_mat.size2 fu_cst.size1
                    this.GenerationContext.ch.z2 (fu_mat.size2, fu_mat.size2) <| fun FF ->
                    this.GenerationContext.ch.z1 fu_mat.size2 <| fun bb ->
                        //let lambda = 1E-6 //正則化パラメータ
                        FF.clear()
                        //FF = fu_mat^T * fu_mat
                        this.GenerationContext.ch.z <| fun tmp ->
                            this.GenerationContext.iter.num FF.size1 <| fun i ->
                                this.GenerationContext.iter.num FF.size2 <| fun j ->
                                    tmp.clear()
                                    this.GenerationContext.iter.num fu_mat.size1 <| fun k ->
                                        tmp <== tmp + asm.conj(fu_mat[k,i])*fu_mat[k,j]
                                    FF[i,j] <== tmp
                        //FF = fu_mat^T * fu_mat + λ^2 * I
                        this.GenerationContext.iter.num FF.size1 <| fun i ->
                                FF[i,i] <== FF[i,i] + lambda * lambda
                        //bb = fu_mat^T * fu_cst
                        this.GenerationContext.ch.z <| fun tmp ->
                            bb.clear()
                            this.GenerationContext.iter.num bb.size1 <| fun i ->
                                tmp.clear()
                                this.GenerationContext.iter.num fu_cst.size1 <| fun k ->
                                    tmp <== tmp + asm.conj(fu_mat[k,i])*fu_cst[k]
                                bb[i] <== tmp
                        checkComplexTikhonovIntermediates this.GenerationContext FF bb
                        this.solve_simuleq(FF,bb)
                        checkComplexTikhonovSolution this.GenerationContext bb
                        code(bb)
    
            /// <summary>
            /// 連立方程式の求解(Tikhonovの正則化法)
            /// </summary>
            /// <param name="fu_mat">係数行列</param>
            /// <param name="fu_cst">定数項ベクトル</param>
            /// <param name="lambda">正則化パラメータ</param>
            /// <param name="code">解に対して行う処理</param>
            member this.solve_simuleq_tt(fu_mat:double2,fu_cst:double1,lambda:double) = fun code ->
                this.GenerationContext.group.h2 "連立方程式の求解(Tikhonovの正則化法)" <| fun () ->
                    requireTikhonovVectorShapes this.GenerationContext fu_mat.size1 fu_mat.size2 fu_cst.size1
                    this.GenerationContext.ch.d2 (fu_mat.size2, fu_mat.size2) <| fun FF ->
                    this.GenerationContext.ch.d1 fu_mat.size2 <| fun bb ->
                        //let lambda = 1E-6 //正則化パラメータ
                        FF.clear()
                        //FF = fu_mat^T * fu_mat
                        this.GenerationContext.ch.d <| fun tmp ->
                            this.GenerationContext.iter.num FF.size1 <| fun i ->
                                this.GenerationContext.iter.num FF.size2 <| fun j ->
                                    tmp.clear()
                                    this.GenerationContext.iter.num fu_mat.size1 <| fun k ->
                                        tmp <== tmp + fu_mat[k,i]*fu_mat[k,j]
                                    FF[i,j] <== tmp
                        //FF = fu_mat^T * fu_mat + λ^2 * I
                        this.GenerationContext.iter.num FF.size1 <| fun i ->
                                FF[i,i] <== FF[i,i] + lambda * lambda
                        //bb = fu_mat^T * fu_cst
                        this.GenerationContext.ch.d <| fun tmp ->
                            bb.clear()
                            this.GenerationContext.iter.num bb.size1 <| fun i ->
                                tmp.clear()
                                this.GenerationContext.iter.num fu_cst.size1 <| fun k ->
                                    tmp <== tmp + fu_mat[k,i]*fu_cst[k]
                                bb[i] <== tmp
                        checkRealTikhonovIntermediates this.GenerationContext FF bb
                        this.solve_simuleq(FF,bb)
                        checkRealTikhonovSolution this.GenerationContext bb
                        code bb
    
            /// <summary>
            /// 連立方程式の求解(Tikhonovの正則化法)
            /// </summary>
            /// <param name="fu_mat">係数行列</param>
            /// <param name="fu_cst">定数項ベクトル(列サイズ=1)</param>
            /// <param name="lambda">正則化パラメータ</param>
            /// <param name="code">解に対して行う処理</param>
            member this.solve_simuleq_tt2(fu_mat:complex2,fu_cst:complex2,lambda:double0) code =
                this.GenerationContext.group.h2 "連立方程式の求解(Tikhonovの正則化法)" <| fun () ->
                    requireTikhonovColumnShapes this.GenerationContext fu_mat.size1 fu_mat.size2 fu_cst.size1 fu_cst.size2
                    this.GenerationContext.ch.z2 (fu_mat.size2, fu_mat.size2) <| fun FF ->
                    this.GenerationContext.ch.z1 fu_mat.size2 <| fun bb ->
                        FF.clear()
                        //FF = fu_mat^T * fu_mat
                        this.GenerationContext.ch.z <| fun tmp ->
                            this.GenerationContext.iter.num FF.size1 <| fun i ->
                                this.GenerationContext.iter.num FF.size2 <| fun j ->
                                    tmp.clear()
                                    this.GenerationContext.iter.num fu_mat.size1 <| fun k ->
                                        tmp <== tmp + asm.conj(fu_mat[k,i])*fu_mat[k,j]
                                    FF[i,j] <== tmp
                        //FF = fu_mat^T * fu_mat + λ^2 * I
                        this.GenerationContext.iter.num FF.size1 <| fun i ->
                                FF[i,i] <== FF[i,i] + lambda * lambda
                        //bb = fu_mat^T * fu_cst
                        this.GenerationContext.ch.z <| fun tmp ->
                            bb.clear()
                            this.GenerationContext.iter.num bb.size1 <| fun i ->
                                tmp.clear()
                                this.GenerationContext.iter.num fu_cst.size1 <| fun k ->
                                    tmp <== tmp + asm.conj(fu_mat[k,i])*fu_cst[k,0]
                                bb[i] <== tmp
                        checkComplexTikhonovIntermediates this.GenerationContext FF bb
                        this.solve_simuleq(FF,bb)
                        checkComplexTikhonovSolution this.GenerationContext bb
                        code bb
