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

        /// Decomposes finite doubles before multiplication can lose their exponent range.
        let private binaryParts (context:Aqualis) (value:double0) code =
            context.ch.d <| fun fraction ->
            context.ch.i <| fun exponent ->
                let expression = value.Expr.eval context
                match context.Language with
                |C99 -> context.codewritein(fraction.code+" = frexp("+expression+", &"+exponent.code+");")
                |Fortran ->
                    context.codewritein(fraction.code+" = fraction("+expression+")")
                    context.codewritein(exponent.code+" = exponent("+expression+")")
                |Python -> context.codewritein(fraction.code+", "+exponent.code+" = math.frexp("+expression+")")
                |_ -> fraction <== value; exponent <== 0
                code (fraction,exponent)

        /// Restores a binary exponent only when the scalar result is needed.
        let private binaryScale (context:Aqualis) (result:double0) (mantissa:double0) (power:int0) =
            let m = mantissa.Expr.eval context
            let e = power.Expr.eval context
            match context.Language with
            |C99 -> context.codewritein(result.code+" = scalbn("+m+", "+e+");")
            |Fortran -> context.codewritein(result.code+" = scale("+m+", "+e+")")
            |Python -> context.codewritein(result.code+" = numpy.ldexp("+m+", "+e+")")
            |_ -> result <== mantissa

        /// Scales each dot product by its own nonzero products, rather than by
        /// unrelated RHS entries. Keeps the exponent separate through filtering.
        let private scaledDot (context:Aqualis) (count:int0) terms code =
            context.ch.i <| fun largest ->
            context.ch.dddd <| fun (sum,correction,term,next) ->
                largest <== -8192
                let products action =
                    context.iter.num count <| fun i ->
                        for (left:double0),(right:double0) in terms i do
                            context.br.if1 (And [left .=/ 0.0;right .=/ 0.0]) <| fun () ->
                                binaryParts context left <| fun (lm,le) ->
                                binaryParts context right <| fun (rm,re) -> action (lm*rm) (le+re)
                products (fun _ exponent ->
                    context.br.if1 (exponent .> largest) <| fun () -> largest <== exponent)
                sum.clear()
                correction.clear()
                products (fun mantissa exponent ->
                    binaryScale context term mantissa (exponent-largest)
                    next <== sum+term
                    context.br.if2 (asm.abs sum .>= asm.abs term)
                        (fun () -> correction <== correction+((sum-next)+term))
                        (fun () -> correction <== correction+((term-next)+sum))
                    sum <== next)
                sum <== sum+correction
                code (sum,largest)

        /// Avoids overflowing SVD coordinates or underflowing intermediate ratios.
        let private filteredProjection (context:Aqualis) (result:double0)
                                       (projected:double0) (projectionExponent:int0)
                                       (singular:double0) (vector:double0)
                                       (scale:double0) (denominator:double0) =
            let rec split values parts =
                match values with
                |(value:double0,divide)::rest ->
                    binaryParts context value <| fun (fraction,exponent) ->
                        split rest ((fraction,exponent,divide)::parts)
                |[] ->
                    let parts = List.rev parts
                    let mantissa = parts |> List.fold (fun value (fraction,_,divide) -> if divide then value/fraction else value*fraction) (D 1.0)
                    let power = parts |> List.fold (fun value (_,exponent,divide) -> if divide then value-exponent else value+exponent) projectionExponent
                    binaryScale context result mantissa power
            split [projected,false;singular,false;vector,false;scale,true;scale,true;denominator,true] []

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
                    context.ch.d2 (matrix.size1,count) <| fun u ->
                    context.ch.d2 (count,matrix.size2) <| fun vt ->
                    context.ch.d1 count <| fun singular ->
                    context.ch.d1 matrix.size2 <| fun solution ->
                        // Native SVD overwrites its input; preserve the caller's matrix.
                        source <== matrix
                        la.svdReduced source (u,singular,vt)
                        solution.clear()
                        context.iter.num count <| fun k ->
                            requireFiniteTikhonovValue context singular[k]
                            context.ch.ddd <| fun (scale,denominator,cutoff) ->
                                cutoff <== 2.220446049250313e-16 * (matrix.size1 + matrix.size2) * singular[0]
                                context.br.if1 (And [singular[k] .> 0.0; Or [lambda .> 0.0; singular[k] .> cutoff]]) <| fun () ->
                                    context.br.if2 (singular[k] .> lambda)
                                        (fun () -> scale <== singular[k])
                                        (fun () -> scale <== lambda)
                                    denominator <== (singular[k]/scale)*(singular[k]/scale) + (lambda/scale)*(lambda/scale)
                                    scaledDot context matrix.size1 (fun i -> [u[i,k],rhs[i]]) <| fun (projected,power) ->
                                        context.iter.num matrix.size2 <| fun j ->
                                            context.ch.d <| fun term ->
                                                filteredProjection context term projected power singular[k] vt[k,j] scale denominator
                                                solution[j] <== solution[j]+term
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
                    context.ch.z2 (matrix.size1,count) <| fun u ->
                    context.ch.z2 (count,matrix.size2) <| fun vt ->
                    context.ch.d1 count <| fun singular ->
                    context.ch.z1 matrix.size2 <| fun solution ->
                        // Native SVD overwrites its input; preserve the caller's matrix.
                        source <== matrix
                        la.svdReduced source (u,singular,vt)
                        solution.clear()
                        context.iter.num count <| fun k ->
                            requireFiniteTikhonovValue context singular[k]
                            context.ch.ddd <| fun (scale,denominator,cutoff) ->
                                cutoff <== 2.220446049250313e-16 * (matrix.size1 + matrix.size2) * singular[0]
                                context.br.if1 (And [singular[k] .> 0.0; Or [lambda .> 0.0; singular[k] .> cutoff]]) <| fun () ->
                                    context.br.if2 (singular[k] .> lambda)
                                        (fun () -> scale <== singular[k])
                                        (fun () -> scale <== lambda)
                                    denominator <== (singular[k]/scale)*(singular[k]/scale) + (lambda/scale)*(lambda/scale)
                                    scaledDot context matrix.size1 (fun i -> [u[i,k].re,rhs[i].re;u[i,k].im,rhs[i].im]) <| fun (real,rePower) ->
                                    scaledDot context matrix.size1 (fun i -> [u[i,k].re,rhs[i].im;-u[i,k].im,rhs[i].re]) <| fun (imag,imPower) ->
                                        context.iter.num matrix.size2 <| fun j ->
                                            context.ch.dddd <| fun (rr,ii,ir,ri) ->
                                                filteredProjection context rr real rePower singular[k] vt[k,j].re scale denominator
                                                filteredProjection context ii imag imPower singular[k] vt[k,j].im scale denominator
                                                filteredProjection context ir imag imPower singular[k] vt[k,j].re scale denominator
                                                filteredProjection context ri real rePower singular[k] vt[k,j].im scale denominator
                                                solution[j] <== solution[j]+(rr+ii)+asm.uj*(ir-ri)
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
