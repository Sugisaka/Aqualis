//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    /// Least-squares operations on the LAPACK context.
    [<AutoOpen>]
    module ContextLaLeastSquaresExtensions =
        /// Validates vector dimensions for Tikhonov least squares.
        let private requireTikhonovVectorShapes (context:Aqualis) (rows:int0) (columns:int0) (rhsLength:int0) =
            LapackValidation.require context (rows .<= 0) "LAPACK Tikhonov matrix rows must be positive."
            LapackValidation.require context (columns .<= 0) "LAPACK Tikhonov matrix columns must be positive."
            LapackValidation.require context (rhsLength .=/ rows) "LAPACK Tikhonov right-hand side length must match matrix rows."

        /// Validates column dimensions for Tikhonov least squares.
        let private requireTikhonovColumnShapes (context:Aqualis) (rows:int0) (columns:int0)
                                                (rhsRows:int0) (rhsColumns:int0) =
            requireTikhonovVectorShapes context rows columns rhsRows
            LapackValidation.require context (rhsColumns .=/ 1) "LAPACK Tikhonov right-hand side must have one column."

        /// Requires a finite Tikhonov regularization value.
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

        /// Solves Tikhonov least squares without forming the normal equations.
        let private solve_real (la:ContextLa) (context:Aqualis) (matrix:double2) (rhs:double1) (regularization:double0) code =
            requireTikhonovVectorShapes context matrix.size1 matrix.size2 rhs.size1
            context.ch.d <| fun lambda ->
                lambda <== regularization
                requireFiniteTikhonovValue context lambda
                LapackValidation.require context (lambda .< 0.0) "LAPACK Tikhonov regularization must be nonnegative."
                matrix.foreach <| fun (i,j) -> requireFiniteTikhonovValue context matrix[i,j]
                rhs.foreach <| fun i -> requireFiniteTikhonovValue context rhs[i]
                context.ch.i <| fun count ->
                    context.br.if2 (matrix.size1 .< matrix.size2)
                        (fun () -> count <== matrix.size1)
                        (fun () -> count <== matrix.size2)
                    context.ch.d2 (matrix.size1,matrix.size2) <| fun source ->
                    context.ch.d2 (matrix.size1,matrix.size1) <| fun u ->
                    context.ch.d2 (matrix.size2,matrix.size2) <| fun vt ->
                    context.ch.d1 count <| fun singular ->
                    context.ch.d1 matrix.size2 <| fun solution ->
                        // Native SVD overwrites its input; preserve the caller's matrix.
                        source <== matrix
                        la.svd source (u,singular,vt)
                        solution.clear()
                        context.iter.num count <| fun k ->
                            requireFiniteTikhonovValue context singular[k]
                            context.ch.ddd <| fun (scale,weight,cutoff) ->
                                weight <== 0.0
                                // A zero regularizer selects the numerical pseudoinverse.
                                cutoff <== 2.220446049250313e-16 * (matrix.size1 + matrix.size2) * singular[0]
                                context.br.if1 (And [singular[k] .> 0.0; Or [lambda .> 0.0; singular[k] .> cutoff]]) <| fun () ->
                                    context.br.if2 (singular[k] .> lambda)
                                        (fun () -> scale <== singular[k])
                                        (fun () -> scale <== lambda)
                                    weight <== ((singular[k]/scale) / ((singular[k]/scale)*(singular[k]/scale) + (lambda/scale)*(lambda/scale))) / scale
                                context.ch.d <| fun projected ->
                                    projected.clear()
                                    context.iter.num matrix.size1 <| fun i ->
                                        projected <== projected + u[i,k] * rhs[i]
                                    projected <== projected * weight
                                    context.iter.num matrix.size2 <| fun j ->
                                        solution[j] <== solution[j] + vt[k,j] * projected
                        solution.foreach <| fun i -> requireFiniteTikhonovValue context solution[i]
                        code solution

        /// Solves Tikhonov least squares without forming the normal equations.
        let private solve_complex (la:ContextLa) (context:Aqualis) (matrix:complex2) (rhs:complex1) (regularization:double0) code =
            requireTikhonovVectorShapes context matrix.size1 matrix.size2 rhs.size1
            context.ch.d <| fun lambda ->
                lambda <== regularization
                requireFiniteTikhonovValue context lambda
                LapackValidation.require context (lambda .< 0.0) "LAPACK Tikhonov regularization must be nonnegative."
                matrix.foreach <| fun (i,j) -> requireFiniteTikhonovValue context matrix[i,j].re; requireFiniteTikhonovValue context matrix[i,j].im
                rhs.foreach <| fun i -> requireFiniteTikhonovValue context rhs[i].re; requireFiniteTikhonovValue context rhs[i].im
                context.ch.i <| fun count ->
                    context.br.if2 (matrix.size1 .< matrix.size2)
                        (fun () -> count <== matrix.size1)
                        (fun () -> count <== matrix.size2)
                    context.ch.z2 (matrix.size1,matrix.size2) <| fun source ->
                    context.ch.z2 (matrix.size1,matrix.size1) <| fun u ->
                    context.ch.z2 (matrix.size2,matrix.size2) <| fun vt ->
                    context.ch.d1 count <| fun singular ->
                    context.ch.z1 matrix.size2 <| fun solution ->
                        // Native SVD overwrites its input; preserve the caller's matrix.
                        source <== matrix
                        la.svd source (u,singular,vt)
                        solution.clear()
                        context.iter.num count <| fun k ->
                            requireFiniteTikhonovValue context singular[k]
                            context.ch.ddd <| fun (scale,weight,cutoff) ->
                                weight <== 0.0
                                // A zero regularizer selects the numerical pseudoinverse.
                                cutoff <== 2.220446049250313e-16 * (matrix.size1 + matrix.size2) * singular[0]
                                context.br.if1 (And [singular[k] .> 0.0; Or [lambda .> 0.0; singular[k] .> cutoff]]) <| fun () ->
                                    context.br.if2 (singular[k] .> lambda)
                                        (fun () -> scale <== singular[k])
                                        (fun () -> scale <== lambda)
                                    weight <== ((singular[k]/scale) / ((singular[k]/scale)*(singular[k]/scale) + (lambda/scale)*(lambda/scale))) / scale
                                context.ch.z <| fun projected ->
                                    projected.clear()
                                    context.iter.num matrix.size1 <| fun i ->
                                        projected <== projected + asm.conj(u[i,k]) * rhs[i]
                                    projected <== projected * weight
                                    context.iter.num matrix.size2 <| fun j ->
                                        solution[j] <== solution[j] + asm.conj(vt[k,j]) * projected
                        solution.foreach <| fun i -> requireFiniteTikhonovValue context solution[i].re; requireFiniteTikhonovValue context solution[i].im
                        code solution

        /// LAPACK-backed least squares with an absolute regularization parameter.
        type ContextLa with
            /// Solves real Tikhonov least squares with a runtime regularization value.
            member this.solve_simuleq_tt(matrix:double2,rhs:double1,lambda:double0) = fun code ->
                LapackValidation.requireBackend this.GenerationContext "LAPACK Tikhonov solve"
                solve_real this this.GenerationContext matrix rhs lambda code
            /// Solves complex Tikhonov least squares with a runtime regularization value.
            member this.solve_simuleq_tt(matrix:complex2,rhs:complex1,lambda:double0) = fun code ->
                LapackValidation.requireBackend this.GenerationContext "LAPACK Tikhonov solve"
                solve_complex this this.GenerationContext matrix rhs lambda code
            /// Solves real Tikhonov least squares with a constant regularization value.
            member this.solve_simuleq_tt(matrix:double2,rhs:double1,lambda:double) = fun code ->
                this.solve_simuleq_tt(matrix,rhs,D lambda) code
            /// Solves complex Tikhonov least squares with a constant regularization value.
            member this.solve_simuleq_tt(matrix:complex2,rhs:complex1,lambda:double) = fun code ->
                this.solve_simuleq_tt(matrix,rhs,D lambda) code
            /// Solves real Tikhonov least squares with the compatibility default 1e-6.
            member this.solve_simuleq_t(matrix:double2,rhs:double1) = fun code ->
                this.solve_simuleq_tt(matrix,rhs,1e-6) code
            /// Solves complex Tikhonov least squares with the compatibility default 1e-6.
            member this.solve_simuleq_t(matrix:complex2,rhs:complex1) = fun code ->
                this.solve_simuleq_tt(matrix,rhs,1e-6) code
            /// Solves complex Tikhonov least squares for a one-column right-hand side.
            member this.solve_simuleq_tt2(matrix:complex2,rhs:complex2,lambda:double0) = fun code ->
                let context = this.GenerationContext
                LapackValidation.requireBackend context "LAPACK Tikhonov solve"
                requireTikhonovColumnShapes context matrix.size1 matrix.size2 rhs.size1 rhs.size2
                context.ch.z1 rhs.size1 <| fun vector ->
                    context.iter.num rhs.size1 <| fun i -> vector[i] <== rhs[i,0]
                    solve_complex this context matrix vector lambda code
