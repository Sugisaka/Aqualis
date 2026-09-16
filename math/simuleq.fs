//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    module simuleq =

        /// <summary>前処理付きBiCGSTAB法による連立方程式の求解</summary>
        /// <param name="b">定数項</param>
        /// <param name="x">暫定解→近似解</param>
        /// <param name="tol">収束判定値</param>
        /// <param name="max_iteration">最大反復回数</param>
        /// <param name="integralequation_matmul">行列－ベクトル積実行関数</param>
        /// <param name="prec">前処理行列</param>
        let BiCGSTAB (context:Aqualis) (b:complex1) (x:complex1) (tol:double) (max_iteration:int) integralequation_matmul1 (prec:(complex1->complex1->unit)option) =
            if not (System.Double.IsFinite tol) || tol <= 0.0 then
                invalidArg (nameof tol) "BiCGSTAB tolerance must be finite and positive."
            if max_iteration <= 0 then
                invalidArg (nameof max_iteration) "BiCGSTAB maximum iteration count must be positive."
            NumericArrayValidation.require context (b.size1 .<= 0) "BiCGSTAB vector length must be positive."
            NumericArrayValidation.require context (x.size1 .=/ b.size1) "BiCGSTAB solution length must match the right-hand side."
            context.group.Section "Bi-CGSTAB法" <| fun () ->
                //ベクトルのノルム
                let norm(norm_:double0,b:complex1) =
                  context.la.norm b <| fun stableNorm -> norm_ <== stableNorm
                //ベクトルの内積
                let dot_product2(dot_product2_:complex0,a:complex1,b:complex1) =
                  dot_product2_ <== 0
                  context.iter.num a.size1 <| fun i ->
                      dot_product2_ <== dot_product2_ + asm.conj(a[i]) * b[i]

                context.ch.z1 b.size1 <| fun r -> context.ch.z1 b.size1 <| fun t -> context.ch.z1 b.size1 <| fun p -> context.ch.z1 b.size1 <| fun v -> context.ch.z1 b.size1 <| fun s -> context.ch.z1 b.size1 <| fun p_hat -> context.ch.z1 b.size1 <| fun s_hat -> context.ch.z1 b.size1 <| fun r_tld ->
                    context.ch.d <| fun bnrm2 ->
                        norm(bnrm2,b)
                        NumericArrayValidation.requireFinite context bnrm2 "BiCGSTAB right-hand side norm must be finite."
                        context.br.if1 (bnrm2 .= 0.0) <| fun () -> bnrm2 <== 1.0
                        integralequation_matmul1(t,x)
                        context.iter.num r.size1 <| fun i ->
                            r[i] <== (b[i] - t[i]) / bnrm2
                        context.ch.d <| fun err ->
                            context.ch.d <| fun norm_ ->
                                norm(norm_,r)
                                err <== norm_
                                NumericArrayValidation.requireFinite context err "BiCGSTAB residual norm must be finite."
                                context.print.tt <| _0++err
                            context.br.if1 (err .> tol) <| fun () ->
                                context.ch.z <| fun omega ->
                                    omega <== 1
                                    context.iter.num r.size1 <| fun i -> r_tld.[i] <== r.[i]
                                    context.ch.zzzz <| fun (rho,rho_1,alpha,beta) ->
                                        context.ch.i <| fun converged ->
                                            converged <== 0
                                            //反復処理
                                            context.iter.num_exit (I max_iteration) <| fun (exit,i) ->
                                                dot_product2(rho,r_tld,r)
                                                NumericArrayValidation.require context (asm.abs rho .= 0.0) "BiCGSTAB broke down: residual inner product is zero."
                                                context.br.if2 (i .> 0)
                                                  (fun () ->
                                                    beta <== rho/rho_1*( alpha/omega )
                                                    context.iter.num r.size1 <| fun j -> p.[j] <== r.[j] + beta*( p.[j] - omega*v.[j]))
                                                  (fun () ->
                                                    context.iter.num r.size1 <| fun j -> p.[j] <== r.[j])
                                                //前処理
                                                match prec with
                                                    |Some(pr)  -> pr p_hat p
                                                    |None -> context.iter.num r.size1 <| fun j -> p_hat.[j] <== p.[j]
                                                //インピーダンス行列×電磁流ベクトル
                                                integralequation_matmul1(v,p_hat)
                                                context.ch.z <| fun z ->
                                                    dot_product2(z,r_tld,v)
                                                    NumericArrayValidation.require context (asm.abs z .= 0.0) "BiCGSTAB broke down: matrix inner product is zero."
                                                    alpha <== rho / z
                                                context.iter.num r.size1 <| fun j ->
                                                    s.[j] <== r.[j] - alpha*v.[j]
                                                context.ch.d <| fun norm_ ->
                                                    norm(norm_,s)
                                                    NumericArrayValidation.requireFinite context norm_ "BiCGSTAB residual norm must be finite."
                                                    context.br.if1 (norm_ .<= tol) <| fun () ->
                                                        context.iter.num r.size1 <| fun j -> x.[j] <== x.[j] + bnrm2*(alpha*p_hat.[j])
                                                        converged <== 1
                                                        context.print.s "converged"
                                                        exit()
                                                //前処理
                                                match prec with
                                                    |Some(pr)  -> pr s_hat s
                                                    |None -> context.iter.num r.size1 <| fun j -> s_hat.[j] <== s.[j]
                                                //インピーダンス行列×電磁流ベクトル
                                                integralequation_matmul1(t,s_hat)
                                                context.ch.zz <| fun (z1,z2) ->
                                                    dot_product2(z1,t,s)
                                                    dot_product2(z2,t,t)
                                                    NumericArrayValidation.require context (asm.abs z2 .= 0.0) "BiCGSTAB broke down: correction norm is zero."
                                                    omega <== z1/z2
                                                context.iter.num r.size1 <| fun j ->
                                                    x.[j] <== x.[j] + bnrm2*(alpha*p_hat.[j] + omega*s_hat.[j])
                                                context.iter.num r.size1 <| fun j ->
                                                    r.[j] <== s.[j] - omega * t.[j]
                                                context.ch.d <| fun norm_ ->
                                                    norm(norm_,r)
                                                    err <== norm_
                                                    NumericArrayValidation.requireFinite context err "BiCGSTAB residual norm must be finite."
                                                context.print.tt <| i++err
                                                //収束判定
                                                context.br.if1 (err .<= tol) <| fun () ->
                                                    converged <== 1
                                                    context.print.s "converged"
                                                    exit()
                                                NumericArrayValidation.require context (asm.abs omega .= 0.0) "BiCGSTAB broke down: correction factor is zero."
                                                rho_1 <== rho
                                            NumericArrayValidation.require context (converged .= 0) "BiCGSTAB failed to converge within the maximum iteration count."
