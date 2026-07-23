//#############################################################################
// 微分演算テスト
let projectname = "test6_2"
let version = "1.0.0"
//#############################################################################
 
let outputdir = __SOURCE_DIRECTORY__

#I @"..\..\bin\Debug\net10.0"
#r "Aqualis.dll"

open Aqualis

/// <summary>
/// fとgの相関計算
/// 結果はfに上書き
/// </summary>
/// <param name="f">2次元データ</param>
/// <param name="g">2次元データ</param>
let correlation(f:complex2,g:complex2,ctx:CompilationEnvironment) =
    // fのフーリエ変換
    ctx.fft2.fft("ftplan1", f, f)
    // gのフーリエ変換
    ctx.fft2.fft("ftplan2", g, g)
    // F×G* → f
    f.foreach <| fun (i,j) ->
        f[i,j] <== f[i,j]*g[i,j].conj
    // fの逆フーリエ変換
    ctx.fft2.ifft("ftplan3", f, f)
    
Compile [Fortran;C99;Python] outputdir projectname "aaa" <| fun ctx ->
    let N = 101
    ctx.ch.z2 (N, N) <| fun f ->
    ctx.ch.z2 (N, N) <| fun g ->
        // fの生成
        f.clear()
        f[50,50] <== 1
        ctx.io.save_text(f,"f.dat")
        
        // gの生成
        g.clear()
        g[50,50] <== 1
        ctx.io.save_text(g,"g.dat")
        
        // fとgの相関
        correlation(f,g,ctx)
        
        ctx.io.save_text(f,"fg.dat")
