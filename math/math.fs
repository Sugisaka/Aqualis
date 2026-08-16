//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    type ContextMath internal (context:Aqualis) =
        ///<summary>整数次第2種球ハンケル関数を計算</summary>
        ///<param name="hn">球ハンケル関数の値</param>
        ///<param name="n">ハンケル関数の次数</param>
        ///<param name="x">ハンケル関数の引数</param>
        member _.hn (hn:complex0) (n:int0,x:double0) =
            // hn(x) = j * (-1)^n * x^n * [x^(-1) * (d/dx)]^n * [e^(-jx) * (x^(-1))]
            //c1(m): e^(-jx)(x^(-m))の係数
            context.ch.z1 (2*n+1) <| fun c1 ->
            context.ch.z1 (2*n+1) <| fun c2 ->
                c1.clear()
                c2.clear()
                //初期値：微分前なのでe^(-jx)(x^(-1))の項のみ
                c1[1-1] <== 1.0
                //(d/dx)[e^(-jx)(x^(-m))] = (-j)e^(-jx)(x^(-m)) + (-m)e^(-jx)(x^(-(m+1)))
                context.iter.num n <| fun _ ->
                    //(d/dx)
                    context.iter.num (2*n) <| fun m ->
                        c2[m  ] <== c2[m  ] - asm.uj*c1[m] //e^(-jx)を微分、x^(-m)はそのまま
                        c2[m+1] <== c2[m+1] - (m+1)*c1[m]      //e^(-jx)はそのままでx^(-m)を微分
                    //x^(-1)
                    context.iter.num (2*n) <| fun m ->
                        c2[2*n+1-m+1] <== c2[2*n+1-m]
                    c2[1-1] <== 0
                    //c1 ← c2
                    c1 <== c2
                    c2.clear()
                //c(m): e^(-jx)(x^(n-m))の係数
                hn.clear()
                context.iter.range (_0, 2*n) <| fun m ->
                    hn <== hn + asm.pow(x,n-m-1)*c1[m+1-1]
                hn <== asm.uj*asm.pow(-1,n)*asm.exp(-asm.uj*x)*hn

        ///<summary>整数次第2種球ハンケル関数を計算</summary>
        ///<param name="hn">hn(i) = 引数x(i)における球ハンケル関数の値</param>
        ///<param name="N">引数の数(i=1,2,…N)</param>
        ///<param name="n">ハンケル関数の次数</param>
        ///<param name="x">x(i):ハンケル関数の引数</param>
        member _.hnarray (hn:int0->complex0) (N:int0) (n:int0,x:int0->double0) =
            // hn(x) = j * (-1)^n * x^n * [x^(-1) * (d/dx)]^n * [e^(-jx) * (x^(-1))]
            //c1(m): e^(-jx)(x^(-m))の係数
            context.ch.z1 (2*n+1) <| fun c1 ->
            context.ch.z1 (2*n+1) <| fun c2 ->
                c1.clear()
                c2.clear()
                //初期値：微分前なのでe^(-jx)(x^(-1))の項のみ
                c1[1-1] <== 1.0
                //(d/dx)[e^(-jx)(x^(-m))] = (-j)e^(-jx)(x^(-m)) + (-m)e^(-jx)(x^(-(m+1)))
                context.iter.num n <| fun _ ->
                    //(d/dx)
                    context.iter.num (2*n) <| fun m ->
                        c2[m  ] <== c2[m  ] - asm.uj*c1[m] //e^(-jx)を微分、x^(-m)はそのまま
                        c2[m+1] <== c2[m+1] - m*c1[m]      //e^(-jx)はそのままでx^(-m)を微分
                    //x^(-1)
                    context.iter.num (2*n) <| fun m ->
                        c2[2*n+1-m+1] <== c2[2*n+1-m]
                    c2[1-1] <== 0
                    //c1 ← c2
                    c1 <== c2
                    c2.clear()
                //c(m): e^(-jx)(x^(n-m))の係数
                context.iter.num N <| fun i ->
                    hn(i+1).clear()
                    context.iter.range (_0, 2*n) <| fun m ->
                        hn(i+1) <== hn(i+1) + asm.pow(x(i+1),n-m-1)*c1[m+1]
                    hn(i+1) <== asm.uj*asm.pow(-1,n)*asm.exp(-asm.uj*x(i+1))*hn(i+1)

        ///<summary>整数次第2種球ハンケル関数を計算</summary>
        ///<param name="n">ハンケル関数の次数</param>
        ///<param name="code">code(f)：f(hn,x)でhn(x)を計算</param>
        member _.hnfunc (n:int0) code =
            // hn(x) = j * (-1)^n * x^n * [x^(-1) * (d/dx)]^n * [e^(-jx) * (x^(-1))]
            //c1(m): e^(-jx)(x^(-m))の係数
            context.ch.z1 (2*n+1) <| fun c1 ->
            context.ch.z1 (2*n+1) <| fun c2 ->
                c1.clear()
                c2.clear()
                //初期値：微分前なのでe^(-jx)(x^(-1))の項のみ
                c1[1-1] <== 1.0
                //(d/dx)[e^(-jx)(x^(-m))] = (-j)e^(-jx)(x^(-m)) + (-m)e^(-jx)(x^(-(m+1)))
                context.iter.num n <| fun _ ->
                    //(d/dx)
                    context.iter.num (2*n) <| fun m ->
                        c2[m  ] <== c2[m  ] - asm.uj*c1[m] //e^(-jx)を微分、x^(-m)はそのまま
                        c2[m+1] <== c2[m+1] - (m+1)*c1[m]      //e^(-jx)はそのままでx^(-m)を微分
                    //x^(-1)
                    context.iter.num (2*n) <| fun m ->
                        c2[2*n+1-m+1] <== c2[2*n+1-m]
                    c2[1-1] <== 0
                    //c1 ← c2
                    c1 <== c2
                    c2.clear()
                //c(m): e^(-jx)(x^(n-m))の係数
                let f (hn:complex0,x:double0) =
                    hn.clear()
                    context.iter.range (_0, 2*n) <| fun m ->
                        hn <== hn + asm.pow(x,n-m-1)*c1[m+1-1]
                    hn <== asm.uj*asm.pow(-1,n)*asm.exp(-asm.uj*x)*hn
                code f

        //Legendre多項式 P_l
        member _.plgndr (pl:double0) (l:int0,x:double0) =
            context.group.Section "Legendre多項式を計算" <| fun () ->
                context.ch.d <| fun pll ->
                context.ch.d <| fun pmm ->
                context.ch.d <| fun pmmp1 ->
                    pmm<==1.0
                    context.br.branch <| fun b ->
                        b.IF (l .= 0) <| fun () ->
                            pl <== pmm
                        b.EL <| fun () ->
                            pmmp1<==x*pmm
                            context.br.branch <| fun b ->
                               b.IF (l .= 1) <| fun () ->
                                   pl<==pmmp1
                               b.EL <| fun () ->
                                   context.iter.range (_2, l) <| fun ll ->
                                       pll<==(x*(2*ll-1)*pmmp1-(ll-1)*pmm)/ll
                                       pmm<==pmmp1
                                       pmmp1<==pll
                                   pl<==pll

        //<summary>Legendre多項式を0次からn次まで計算</summary>
        //<param name="pl">pl[i] = i-1次のLegendre多項式を保存</param>
        //<param name="(l,x)">計算するLegendre多項式の最高次数と引数</param>
        member _.plgndrarray (pl:double1) (l:int0,x:double0) =
            context.group.Section ("0次から"+l.code+"次までのLegendre多項式を計算") <| fun () ->
                context.ch.d <| fun pmm ->
                context.ch.d <| fun pmmp1 ->
                    pmm<==1.0
                    pl[1-1]<==pmm
                    context.br.branch <| fun b ->
                        b.IF (l .= 0) <| fun () ->
                            pl[l+1-1] <== pmm
                        b.EL <| fun () ->
                            pmmp1<==x*pmm
                            context.br.branch <| fun b ->
                                b.IF (l .= 1) <| fun () ->
                                    pl[l+1-1]<==pmmp1
                                b.EL <| fun () ->
                                    pl[2-1]<==pmmp1
                                    context.iter.range (_2, l) <| fun ll ->
                                        pl[ll+1-1]<==(x*(2*ll-1)*pmmp1-(ll-1)*pmm)/ll
                                        pmm<==pmmp1
                                        pmmp1<==pl[ll+1-1]

        //<summary>Legendre陪多項式を計算</summary>
        //<param name="pl">P_l^m</param>
        //<param name="l">整数</param>
        //<param name="m">整数</param>
        //<param name="x">実数</param>
        member _.aplgndr (pl:double0) (l:int0, m:int0, x:double0) =
            context.ch.dd <| fun (fact,pll) ->
            context.ch.ddd <| fun (pmm,pmmp1,somx2) ->
                context.br.if1 (Or [m .< 0; m .> l; asm.abs(x) .> 1.0]) <| fun () ->
                    context.print.s "Bad arguments in routine plgndr"
                pmm <== 1.0
                context.br.if1 (m .> 0) <| fun () ->
                    somx2 <== asm.sqrt((1.0-x)*(1.0+x))
                    fact <== 1.0
                    context.iter.num m <| fun i ->
                        pmm <== -pmm*fact*somx2
                        fact <== fact + 2.0
                context.br.if2 (l .= m)
                    <| fun () ->
                        pl <== pmm;
                    <| fun () ->
                        pmmp1 <== x*(2*m+1)*pmm
                        context.br.if2 (l .= (m+1))
                            <| fun () ->
                                pl <== pmmp1;
                            <| fun () ->
                                context.iter.range (m+2, l) <| fun ll ->
                                    pll <== (x*(2*ll-1)*pmmp1-(ll+m-1)*pmm)/(ll-m)
                                    pmm <== pmmp1
                                    pmmp1 <== pll
                                pl <== pll

    [<AutoOpen>]
    module CompilationEnvironmentMathExtensions =
        type Aqualis with
            ///<summary>特殊関数</summary>
            member this.math = ContextMath(this)
