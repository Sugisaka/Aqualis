//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    open System

    type ContextLa internal (context:Aqualis) =
        let requirePythonLinalg symbol =
            context.pythonImports.RequireSymbol("scipy.linalg", symbol)

        member internal _.GenerationContext = context
        member internal _.RequirePythonLinalg symbol = requirePythonLinalg symbol

        /// <summary>
        /// 行列×ベクトルの計算
        /// </summary>
        /// <param name="x">a×b</param>
        /// <param name="a">a</param>
        /// <param name="b">b</param>
        member this.matmul (x:double1,a:double2,b:double1) =
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
        member this.matmul (x:complex1,a:complex2,b:double1) =
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
        member this.matmul (x:complex1,a:complex2,b:complex1) =
            x.clear()
            context.iter.num a.size1 <| fun i ->
                context.iter.num a.size2 <| fun j ->
                    x[i] <== x[i] + a[i,j] * b[j]

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
            u.clear()
            context.iter.num a.size1 <| fun i ->
                context.iter.num b.size2 <| fun j ->
                    context.iter.num a.size2 <| fun k ->
                        u[i,j] <== u[i,j] + a[i,k] * b[k,j]

        /// <summary>Calculates u = transpose(a) * b.</summary>
        member internal _.matmulTransposeLeft (u:double2,a:double2,b:double2) =
            u.clear()
            context.iter.num a.size2 <| fun i ->
                context.iter.num b.size2 <| fun j ->
                    context.iter.num a.size1 <| fun k ->
                        u[i,j] <== u[i,j] + a[k,i] * b[k,j]
        /// <summary>
        /// 行列×行列の計算
        /// </summary>
        /// <param name="u">計算結果</param>
        member this.matmul (u:complex2,a:complex2,b:double2) =
            u.clear()
            context.iter.num a.size1 <| fun i ->
                context.iter.num b.size2 <| fun j ->
                    context.iter.num a.size2 <| fun k ->
                        u[i,j] <== u[i,j] + a[i,k] * b[k,j]
        /// <summary>
        /// 行列×行列の計算
        /// </summary>
        /// <param name="u">計算結果</param>
        member this.matmul (u:complex2,a:double2,b:complex2) =
            u.clear()
            context.iter.num a.size1 <| fun i ->
                context.iter.num b.size2 <| fun j ->
                    context.iter.num a.size2 <| fun k ->
                        u[i,j] <== u[i,j] + a[i,k] * b[k,j]
        /// <summary>
        /// 行列×行列の計算
        /// </summary>
        /// <param name="u">計算結果</param>
        member this.matmul (u:complex2,a:complex2,b:complex2) =
            u.clear()
            context.iter.num a.size1 <| fun i ->
                context.iter.num b.size2 <| fun j ->
                    context.iter.num a.size2 <| fun k ->
                        u[i,j] <== u[i,j] + a[i,k] * b[k,j]

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
            x.clear()
            context.iter.num a.size1 <| fun j ->
                x <== x + a[j] * b[j]
        /// <summary>
        /// ベクトルの内積計算
        /// </summary>
        /// <param name="x">a・b</param>
        /// <param name="a">a</param>
        /// <param name="b">b</param>
        member this.dot (x:complex0,a:complex1,b:double1) =
            x.clear()
            context.iter.num a.size1 <| fun j ->
                x <== x + asm.conj(a[j]) * b[j]
        /// <summary>
        /// ベクトルの内積計算
        /// </summary>
        /// <param name="x">a・b</param>
        /// <param name="a">a</param>
        /// <param name="b">b</param>
        member this.dot (x:complex0,a:double1,b:complex1) =
            x.clear()
            context.iter.num a.size1 <| fun j ->
                x <== x + a[j] * b[j]
        /// <summary>
        /// ベクトルの内積計算
        /// </summary>
        /// <param name="x">a・b</param>
        /// <param name="a">a</param>
        /// <param name="b">b</param>
        member this.dot (x:complex0,a:complex1,b:complex1) =
            x.clear()
            context.iter.num a.size1 <| fun j ->
                x <== x + asm.conj(a[j]) * b[j]

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
            this.dot (a,a) <| fun b -> code(asm.sqrt b)
            
        /// <summary>
        /// ベクトルのノルム(L2ノルム)計算
        /// </summary>
        /// <param name="a">a</param>
        /// <param name="code">ノルムaに対する処理</param>
        member this.norm (a:complex1) = fun code ->
            this.dot (a,a) <| fun b -> code(asm.sqrt b.re)

        /// <summary>
        /// ベクトルの規格化
        /// </summary>
        /// <param name="a"></param>
        member this.normalize (a:double1) =
            this.norm a <| fun c ->
                a <== a/c
        /// <summary>
        /// ベクトルの規格化
        /// </summary>
        /// <param name="a"></param>
        member this.normalize (a:complex1) =
            this.norm a <| fun c ->
                a <== a/c

