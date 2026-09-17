//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    open System

    [<RequireQualifiedAccess>]
    /// Validates LAPACK inputs and reports backend errors.
    module internal LapackValidation =
        /// Requires a target language with the requested LAPACK operation.
        let requireBackend (context:Aqualis) operation =
            match context.Language with
            | C99 | Fortran | Python | LaTeX | HTML -> ()
            | language -> UnsupportedOperation.codeGeneration (string language) operation

        /// Reports a failed LAPACK precondition.
        let require (context:Aqualis) (condition:bool0) message =
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
            | JavaScript ->
                context.br.if1 condition (fun () ->
                    context.codewritein("throw new Error(" + OutputTextLiteral.javaScript ("Aqualis: " + message) + ");\n"))
            | PHP ->
                context.br.if1 condition (fun () ->
                    context.codewritein("<?php throw new Exception(" + OutputTextLiteral.c ("Aqualis: " + message) + "); ?>\n"))
            | _ -> ()

        /// Checks the LAPACK status value and reports a failed operation.
        let checkInfo (context:Aqualis) (info:int0) operation =
            match context.language with
            | C99 ->
                context.codewritein("if (" + info.code + " != 0) { fprintf(stderr, \"Aqualis: LAPACK " + operation + " failed (INFO=%d).\\n\", " + info.code + "); exit(EXIT_FAILURE); }\n")
            | Fortran ->
                context.codewritein("if (" + info.code + " /= 0) error stop 'Aqualis: LAPACK " + operation + " failed.'\n")
            | _ -> ()

    /// LAPACK-backed linear algebra operations for an Aqualis context.
    type ContextLa internal (context:Aqualis) =
        let requirePythonLinalg symbol =
            context.pythonImports.RequireSymbol("scipy.linalg", symbol)

        let requireMatvec rows columns vectorLength outputLength =
            LapackValidation.require context (columns .=/ vectorLength) "LAPACK matrix-vector inner dimensions must match."
            LapackValidation.require context (outputLength .=/ rows) "LAPACK matrix-vector output length must match matrix rows."

        let requireMatmul rows inner rightRows columns outputRows outputColumns =
            LapackValidation.require context (inner .=/ rightRows) "LAPACK matrix multiplication inner dimensions must match."
            LapackValidation.require context (outputRows .=/ rows) "LAPACK matrix multiplication output shape must match result."
            LapackValidation.require context (outputColumns .=/ columns) "LAPACK matrix multiplication output shape must match result."

        let withScaledRealNorm (a:double1) code =
            context.ch.dd <| fun (scale,sumSquares) ->
                scale.clear()
                a.foreach <| fun i ->
                    let magnitude = asm.abs a[i]
                    context.br.if1 (Or [magnitude .> scale; magnitude .=/ magnitude]) <| fun () -> scale <== magnitude
                sumSquares.clear()
                context.br.if1 (scale .= D Double.PositiveInfinity) <| fun () -> sumSquares <== 1.0
                context.br.if1 (And [scale .> 0.0; scale .< D Double.PositiveInfinity]) <| fun () ->
                    a.foreach <| fun i ->
                        let ratio = a[i] / scale
                        sumSquares <== sumSquares + ratio * ratio
                code scale sumSquares

        let withScaledComplexNorm (a:complex1) code =
            context.ch.dd <| fun (scale,sumSquares) ->
                scale.clear()
                a.foreach <| fun i ->
                    let realMagnitude = asm.abs a[i].re
                    let imaginaryMagnitude = asm.abs a[i].im
                    context.br.if1 (Or [realMagnitude .> scale; realMagnitude .=/ realMagnitude]) <| fun () -> scale <== realMagnitude
                    context.br.if1 (Or [imaginaryMagnitude .> scale; imaginaryMagnitude .=/ imaginaryMagnitude]) <| fun () -> scale <== imaginaryMagnitude
                sumSquares.clear()
                context.br.if1 (scale .= D Double.PositiveInfinity) <| fun () -> sumSquares <== 1.0
                context.br.if1 (And [scale .> 0.0; scale .< D Double.PositiveInfinity]) <| fun () ->
                    a.foreach <| fun i ->
                        let realRatio = a[i].re / scale
                        let imaginaryRatio = a[i].im / scale
                        sumSquares <== sumSquares + realRatio * realRatio + imaginaryRatio * imaginaryRatio
                code scale sumSquares

        /// Gets the owning generation context.
        member internal _.GenerationContext = context
        /// Requires a supported Python linear algebra backend.
        member internal _.RequirePythonLinalg symbol = requirePythonLinalg symbol

        /// <summary>
        /// 行列×ベクトルの計算
        /// </summary>
        /// <param name="x">a×b</param>
        /// <param name="a">a</param>
        /// <param name="b">b</param>
        member this.matmul (x:double1,a:double2,b:double1) =
            requireMatvec a.size1 a.size2 b.size1 x.size1
            let calculate (target:double1) =
                target.clear()
                context.iter.num a.size1 <| fun i ->
                    context.iter.num a.size2 <| fun j ->
                        target[i] <== target[i] + a[i,j] * b[j]
            if x.code = b.code then
                context.ch.d1 x.size1 <| fun temporary ->
                    calculate temporary
                    context.iter.num x.size1 <| fun i -> x[i] <== temporary[i]
            else calculate x
        /// <summary>
        /// 行列×ベクトルの計算
        /// </summary>
        /// <param name="x">a×b</param>
        /// <param name="a">a</param>
        /// <param name="b">b</param>
        member this.matmul (x:complex1,a:complex2,b:double1) =
            requireMatvec a.size1 a.size2 b.size1 x.size1
            x.clear()
            context.iter.num a.size1 <| fun i ->
                context.iter.num a.size2 <| fun j ->
                    x[i] <== x[i] + a[i,j] * b[j]
        /// <summary>
        /// 行列×ベクトルの計算
        /// </summary>
        /// <param name="x">a×b</param>
        /// <param name="a">a</param>
        /// <param name="b">b</param>
        member this.matmul (x:complex1,a:double2,b:complex1) =
            requireMatvec a.size1 a.size2 b.size1 x.size1
            let calculate (target:complex1) =
                target.clear()
                context.iter.num a.size1 <| fun i ->
                    context.iter.num a.size2 <| fun j ->
                        target[i] <== target[i] + a[i,j] * b[j]
            if x.code = b.code then
                context.ch.z1 x.size1 <| fun temporary ->
                    calculate temporary
                    context.iter.num x.size1 <| fun i -> x[i] <== temporary[i]
            else calculate x
        /// <summary>
        /// 行列×ベクトルの計算
        /// </summary>
        /// <param name="x">a×b</param>
        /// <param name="a">a</param>
        /// <param name="b">b</param>
        member this.matmul (x:complex1,a:complex2,b:complex1) =
            requireMatvec a.size1 a.size2 b.size1 x.size1
            let calculate (target:complex1) =
                target.clear()
                context.iter.num a.size1 <| fun i ->
                    context.iter.num a.size2 <| fun j ->
                        target[i] <== target[i] + a[i,j] * b[j]
            if x.code = b.code then
                context.ch.z1 x.size1 <| fun temporary ->
                    calculate temporary
                    context.iter.num x.size1 <| fun i -> x[i] <== temporary[i]
            else calculate x

        /// <summary>
        /// 行列×ベクトルの計算
        /// </summary>
        /// <param name="a">行列</param>
        /// <param name="b">ベクトル</param>
        member this.matmul (a:double2,b:double1) = fun code ->
            context.ch.d1 a.size1 <| fun x ->
                this.matmul (x,a,b)
                code x
        /// <summary>
        /// 行列×ベクトルの計算
        /// </summary>
        /// <param name="a">行列</param>
        /// <param name="b">ベクトル</param>
        member this.matmul (a:complex2,b:double1) = fun code ->
            context.ch.z1 a.size1 <| fun x ->
                this.matmul (x,a,b)
                code x
        /// <summary>
        /// 行列×ベクトルの計算
        /// </summary>
        /// <param name="a">行列</param>
        /// <param name="b">ベクトル</param>
        member this.matmul (a:double2,b:complex1) = fun code ->
            context.ch.z1 a.size1 <| fun x ->
                this.matmul (x,a,b)
                code x
        /// <summary>
        /// 行列×ベクトルの計算
        /// </summary>
        /// <param name="a">行列</param>
        /// <param name="b">ベクトル</param>
        member this.matmul (a:complex2,b:complex1) = fun code ->
            context.ch.z1 a.size1 <| fun x ->
                this.matmul (x,a,b)
                code x

        /// <summary>
        /// 行列×行列の計算
        /// </summary>
        /// <param name="u">計算結果</param>
        member this.matmul (u:double2,a:double2,b:double2) =
            requireMatmul a.size1 a.size2 b.size1 b.size2 u.size1 u.size2
            let calculate (target:double2) =
                target.clear()
                context.iter.num a.size1 <| fun i ->
                    context.iter.num b.size2 <| fun j ->
                        context.iter.num a.size2 <| fun k ->
                            target[i,j] <== target[i,j] + a[i,k] * b[k,j]
            if u.code = a.code || u.code = b.code then
                context.ch.d2 (u.size1,u.size2) <| fun temporary ->
                    calculate temporary
                    context.iter.num u.size1 <| fun i ->
                        context.iter.num u.size2 <| fun j -> u[i,j] <== temporary[i,j]
            else calculate u

        /// <summary>Calculates u = transpose(a) * b.</summary>
        member internal _.matmulTransposeLeft (u:double2,a:double2,b:double2) =
            requireMatmul a.size2 a.size1 b.size1 b.size2 u.size1 u.size2
            let calculate (target:double2) =
                target.clear()
                context.iter.num a.size2 <| fun i ->
                    context.iter.num b.size2 <| fun j ->
                        context.iter.num a.size1 <| fun k ->
                            target[i,j] <== target[i,j] + a[k,i] * b[k,j]
            if u.code = a.code || u.code = b.code then
                context.ch.d2 (u.size1,u.size2) <| fun temporary ->
                    calculate temporary
                    context.iter.num u.size1 <| fun i ->
                        context.iter.num u.size2 <| fun j -> u[i,j] <== temporary[i,j]
            else calculate u
        /// <summary>
        /// 行列×行列の計算
        /// </summary>
        /// <param name="u">計算結果</param>
        member this.matmul (u:complex2,a:complex2,b:double2) =
            requireMatmul a.size1 a.size2 b.size1 b.size2 u.size1 u.size2
            let calculate (target:complex2) =
                target.clear()
                context.iter.num a.size1 <| fun i ->
                    context.iter.num b.size2 <| fun j ->
                        context.iter.num a.size2 <| fun k ->
                            target[i,j] <== target[i,j] + a[i,k] * b[k,j]
            if u.code = a.code then
                context.ch.z2 (u.size1,u.size2) <| fun temporary ->
                    calculate temporary
                    context.iter.num u.size1 <| fun i ->
                        context.iter.num u.size2 <| fun j -> u[i,j] <== temporary[i,j]
            else calculate u
        /// <summary>
        /// 行列×行列の計算
        /// </summary>
        /// <param name="u">計算結果</param>
        member this.matmul (u:complex2,a:double2,b:complex2) =
            requireMatmul a.size1 a.size2 b.size1 b.size2 u.size1 u.size2
            let calculate (target:complex2) =
                target.clear()
                context.iter.num a.size1 <| fun i ->
                    context.iter.num b.size2 <| fun j ->
                        context.iter.num a.size2 <| fun k ->
                            target[i,j] <== target[i,j] + a[i,k] * b[k,j]
            if u.code = b.code then
                context.ch.z2 (u.size1,u.size2) <| fun temporary ->
                    calculate temporary
                    context.iter.num u.size1 <| fun i ->
                        context.iter.num u.size2 <| fun j -> u[i,j] <== temporary[i,j]
            else calculate u
        /// <summary>
        /// 行列×行列の計算
        /// </summary>
        /// <param name="u">計算結果</param>
        member this.matmul (u:complex2,a:complex2,b:complex2) =
            requireMatmul a.size1 a.size2 b.size1 b.size2 u.size1 u.size2
            let calculate (target:complex2) =
                target.clear()
                context.iter.num a.size1 <| fun i ->
                    context.iter.num b.size2 <| fun j ->
                        context.iter.num a.size2 <| fun k ->
                            target[i,j] <== target[i,j] + a[i,k] * b[k,j]
            if u.code = a.code || u.code = b.code then
                context.ch.z2 (u.size1,u.size2) <| fun temporary ->
                    calculate temporary
                    context.iter.num u.size1 <| fun i ->
                        context.iter.num u.size2 <| fun j -> u[i,j] <== temporary[i,j]
            else calculate u

        /// <summary>
        /// 行列a×行列bの計算
        /// </summary>
        /// <param name="a">a</param>
        /// <param name="b">b</param>
        /// <param name="code">a×bに対する処理</param>
        member this.matmul (a:double2,b:double2) = fun code ->
            context.ch.d2 (a.size1, b.size2) <| fun u ->
                this.matmul (u,a,b)
                code u
        /// <summary>
        /// 行列a×行列bの計算
        /// </summary>
        /// <param name="a">a</param>
        /// <param name="b">b</param>
        /// <param name="code">a×bに対する処理</param>
        member this.matmul (a:double2,b:complex2) = fun code ->
            context.ch.z2 (a.size1, b.size2) <| fun u ->
                this.matmul (u,a,b)
                code u
        /// <summary>
        /// 行列a×行列bの計算
        /// </summary>
        /// <param name="a">a</param>
        /// <param name="b">b</param>
        /// <param name="code">a×bに対する処理</param>
        member this.matmul (a:complex2,b:double2) = fun code ->
            context.ch.z2 (a.size1, b.size2) <| fun u ->
                this.matmul (u,a,b)
                code u
        /// <summary>
        /// 行列a×行列bの計算
        /// </summary>
        /// <param name="a">a</param>
        /// <param name="b">b</param>
        /// <param name="code">a×bに対する処理</param>
        member this.matmul (a:complex2,b:complex2) = fun code ->
            context.ch.z2 (a.size1, b.size2) <| fun u ->
                this.matmul (u,a,b)
                code u

        /// <summary>
        /// ベクトルの内積計算
        /// </summary>
        /// <param name="x">a・b</param>
        /// <param name="a">a</param>
        /// <param name="b">b</param>
        member this.dot (x:double0,a:double1,b:double1) =
            LapackValidation.require context (a.size1 .=/ b.size1) "LAPACK dot product vector lengths must match."
            context.ch.d <| fun result ->
                result.clear()
                context.iter.num a.size1 <| fun j ->
                    result <== result + a[j] * b[j]
                x <== result
        /// <summary>
        /// ベクトルの内積計算
        /// </summary>
        /// <param name="x">a・b</param>
        /// <param name="a">a</param>
        /// <param name="b">b</param>
        member this.dot (x:complex0,a:complex1,b:double1) =
            LapackValidation.require context (a.size1 .=/ b.size1) "LAPACK dot product vector lengths must match."
            context.ch.z <| fun result ->
                result.clear()
                context.iter.num a.size1 <| fun j ->
                    result <== result + asm.conj(a[j]) * b[j]
                x <== result
        /// <summary>
        /// ベクトルの内積計算
        /// </summary>
        /// <param name="x">a・b</param>
        /// <param name="a">a</param>
        /// <param name="b">b</param>
        member this.dot (x:complex0,a:double1,b:complex1) =
            LapackValidation.require context (a.size1 .=/ b.size1) "LAPACK dot product vector lengths must match."
            context.ch.z <| fun result ->
                result.clear()
                context.iter.num a.size1 <| fun j ->
                    result <== result + a[j] * b[j]
                x <== result
        /// <summary>
        /// ベクトルの内積計算
        /// </summary>
        /// <param name="x">a・b</param>
        /// <param name="a">a</param>
        /// <param name="b">b</param>
        member this.dot (x:complex0,a:complex1,b:complex1) =
            LapackValidation.require context (a.size1 .=/ b.size1) "LAPACK dot product vector lengths must match."
            context.ch.z <| fun result ->
                result.clear()
                context.iter.num a.size1 <| fun j ->
                    result <== result + asm.conj(a[j]) * b[j]
                x <== result

        /// <summary>
        /// ベクトルの内積計算
        /// </summary>
        /// <param name="a"></param>
        /// <param name="b"></param>
        /// <param name="code">a・bに対する処理</param>
        member this.dot (a:double1,b:double1) = fun code ->
            context.ch.d <| fun x ->
                this.dot (x,a,b)
                code x
        /// <summary>
        /// ベクトルの内積計算
        /// </summary>
        /// <param name="a"></param>
        /// <param name="b"></param>
        /// <param name="code">a・bに対する処理</param>
        member this.dot (a:complex1,b:double1) = fun code ->
            context.ch.z <| fun x ->
                this.dot (x,a,b)
                code x
        /// <summary>
        /// ベクトルの内積計算
        /// </summary>
        /// <param name="a"></param>
        /// <param name="b"></param>
        /// <param name="code">a・bに対する処理</param>
        member this.dot (a:double1,b:complex1) = fun code ->
            context.ch.z <| fun x ->
                this.dot (x,a,b)
                code x
        /// <summary>
        /// ベクトルの内積計算
        /// </summary>
        /// <param name="a"></param>
        /// <param name="b"></param>
        /// <param name="code">a・bに対する処理</param>
        member this.dot (a:complex1,b:complex1) = fun code ->
            context.ch.z <| fun x ->
                this.dot (x,a,b)
                code x

        /// <summary>
        /// ベクトルのノルム(L2ノルム)計算
        /// </summary>
        /// <param name="a">a</param>
        /// <param name="code">ノルムaに対する処理</param>
        member this.norm (a:double1) = fun code ->
            withScaledRealNorm a <| fun scale sumSquares ->
                code(scale * asm.sqrt sumSquares)
            
        /// <summary>
        /// ベクトルのノルム(L2ノルム)計算
        /// </summary>
        /// <param name="a">a</param>
        /// <param name="code">ノルムaに対する処理</param>
        member this.norm (a:complex1) = fun code ->
            withScaledComplexNorm a <| fun scale sumSquares ->
                code(scale * asm.sqrt sumSquares)

        /// <summary>
        /// ベクトルの規格化
        /// </summary>
        /// <param name="a"></param>
        member this.normalize (a:double1) =
            withScaledRealNorm a <| fun scale sumSquares ->
                LapackValidation.require context (scale .<= 0.0) "LAPACK normalization requires a nonzero vector."
                a.foreach <| fun i -> a[i] <== (a[i] / scale) / asm.sqrt sumSquares
        /// <summary>
        /// ベクトルの規格化
        /// </summary>
        /// <param name="a"></param>
        member this.normalize (a:complex1) =
            withScaledComplexNorm a <| fun scale sumSquares ->
                LapackValidation.require context (scale .<= 0.0) "LAPACK normalization requires a nonzero vector."
                a.foreach <| fun i -> a[i] <== (a[i] / scale) / asm.sqrt sumSquares
