(*
Copyright (c) 2022 Jun-ichiro Sugisaka

This software is released under the MIT License.
http://opensource.org/licenses/mit-license.php
*)
namespace Aqualis
    
    module math = 
        ///<summary>整数次第2種球ハンケル関数を計算</summary>
        ///<param name="hn">球ハンケル関数の値</param>
        ///<param name="n">ハンケル関数の次数</param>
        ///<param name="x">ハンケル関数の引数</param>
        let hn (hn:num0) (n:num0,x:num0) =
            // hn(x) = j * (-1)^n * x^n * [x^(-1) * (d/dx)]^n * [e^(-jx) * (x^(-1))]
            //c1(m): e^(-jx)(x^(-m))の係数
            ch.z1 (2*n+1) <| fun c1 ->
            ch.z1 (2*n+1) <| fun c2 ->
                c1.clear()
                c2.clear()
                //初期値：微分前なのでe^(-jx)(x^(-1))の項のみ
                c1[1-1] <== 1.0
                //(d/dx)[e^(-jx)(x^(-m))] = (-j)e^(-jx)(x^(-m)) + (-m)e^(-jx)(x^(-(m+1)))
                iter.num n <| fun _ ->
                    //(d/dx)
                    iter.num (2*n) <| fun m ->
                        c2[m  ] <== c2[m  ] - asm.uj*c1[m] //e^(-jx)を微分、x^(-m)はそのまま
                        c2[m+1] <== c2[m+1] - (m+1)*c1[m]      //e^(-jx)はそのままでx^(-m)を微分
                    //x^(-1)
                    iter.num (2*n) <| fun m ->
                        c2[2*n+1-m+1] <== c2[2*n+1-m]
                    c2[1-1] <== 0
                    //c1 ← c2
                    c1 <== c2
                    c2.clear()
                //c(m): e^(-jx)(x^(n-m))の係数
                hn.clear()
                iter.range _0 (2*n) <| fun m ->
                    hn <== hn + asm.pow(x,n-m-1)*c1[m+1-1]
                hn <== asm.uj*asm.pow(-1,n)*asm.exp(-asm.uj*x)*hn
                
        ///<summary>整数次第2種球ハンケル関数を計算</summary>
        ///<param name="hn">hn(i) = 引数x(i)における球ハンケル関数の値</param>
        ///<param name="N">引数の数(i=1,2,…N)</param>
        ///<param name="n">ハンケル関数の次数</param>
        ///<param name="x">x(i):ハンケル関数の引数</param>
        let hnarray (hn:num0->num0) (N:num0) (n:num0,x:num0->num0) =
            // hn(x) = j * (-1)^n * x^n * [x^(-1) * (d/dx)]^n * [e^(-jx) * (x^(-1))]
            //c1(m): e^(-jx)(x^(-m))の係数
            ch.z1 (2*n+1) <| fun c1 ->
            ch.z1 (2*n+1) <| fun c2 ->
                c1.clear()
                c2.clear()
                //初期値：微分前なのでe^(-jx)(x^(-1))の項のみ
                c1[1-1] <== 1.0
                //(d/dx)[e^(-jx)(x^(-m))] = (-j)e^(-jx)(x^(-m)) + (-m)e^(-jx)(x^(-(m+1)))
                iter.num n <| fun _ ->
                    //(d/dx)
                    iter.num (2*n) <| fun m ->
                        c2[m  ] <== c2[m  ] - asm.uj*c1[m] //e^(-jx)を微分、x^(-m)はそのまま
                        c2[m+1] <== c2[m+1] - m*c1[m]      //e^(-jx)はそのままでx^(-m)を微分
                    //x^(-1)
                    iter.num (2*n) <| fun m ->
                        c2[2*n+1-m+1] <== c2[2*n+1-m]
                    c2[1-1] <== 0
                    //c1 ← c2
                    c1 <== c2
                    c2.clear()
                //c(m): e^(-jx)(x^(n-m))の係数
                iter.num N <| fun i ->
                    hn(i+1).clear()
                    iter.range _0 (2*n) <| fun m ->
                        hn(i+1) <== hn(i+1) + asm.pow(x(i+1),n-m-1)*c1[m+1]
                    hn(i+1) <== asm.uj*asm.pow(-1,n)*asm.exp(-asm.uj*x(i+1))*hn(i+1)
    
        ///<summary>整数次第2種球ハンケル関数を計算</summary>
        ///<param name="n">ハンケル関数の次数</param>
        ///<param name="code">code(f)：f(hn,x)でhn(x)を計算</param>
        let hnfunc (n:num0) code =
            // hn(x) = j * (-1)^n * x^n * [x^(-1) * (d/dx)]^n * [e^(-jx) * (x^(-1))]
            //c1(m): e^(-jx)(x^(-m))の係数
            ch.z1 (2*n+1) <| fun c1 ->
            ch.z1 (2*n+1) <| fun c2 ->
                c1.clear()
                c2.clear()
                //初期値：微分前なのでe^(-jx)(x^(-1))の項のみ
                c1[1-1] <== 1.0
                //(d/dx)[e^(-jx)(x^(-m))] = (-j)e^(-jx)(x^(-m)) + (-m)e^(-jx)(x^(-(m+1)))
                iter.num n <| fun _ ->
                    //(d/dx)
                    iter.num (2*n) <| fun m ->
                        c2[m  ] <== c2[m  ] - asm.uj*c1[m] //e^(-jx)を微分、x^(-m)はそのまま
                        c2[m+1] <== c2[m+1] - (m+1)*c1[m]      //e^(-jx)はそのままでx^(-m)を微分
                    //x^(-1)
                    iter.num (2*n) <| fun m ->
                        c2[2*n+1-m+1] <== c2[2*n+1-m]
                    c2[1-1] <== 0
                    //c1 ← c2
                    c1 <== c2
                    c2.clear()
                //c(m): e^(-jx)(x^(n-m))の係数
                let f (hn:num0,x:num0) =
                    hn.clear()
                    iter.range _0 (2*n) <| fun m ->
                        hn <== hn + asm.pow(x,n-m-1)*c1[m+1-1]
                    hn <== asm.uj*asm.pow(-1,n)*asm.exp(-asm.uj*x)*hn
                code f
                
        //Legendre多項式 P_l
        let plgndr (pl:num0) (l:num0,x:num0) =
            codestr.section "Legendre多項式を計算" <| fun () ->
                ch.i <| fun ll ->
                ch.d <| fun fact ->
                ch.d <| fun pll ->
                ch.d <| fun pmm ->
                ch.d <| fun pmmp1 ->
                ch.d <| fun somx2 ->
                    pmm<==1.0
                    br.branch <| fun b ->
                        b.IF (l .= 0) <| fun () -> 
                            pl <== pmm
                        b.EL <| fun () -> 
                            pmmp1<==x*pmm
                            br.branch <| fun b ->
                               b.IF (l .= 1) <| fun () ->
                                   pl<==pmmp1
                               b.EL <| fun () ->
                                   iter.range _2 l <| fun ll ->
                                       pll<==(x*(2*ll-1)*pmmp1-(ll-1)*pmm)/ll
                                       pmm<==pmmp1
                                       pmmp1<==pll
                                   pl<==pll
                                   
        //<summary>Legendre多項式を0次からn次まで計算</summary>
        //<param name="pl">pl[i] = i-1次のLegendre多項式を保存</param>
        //<param name="(l,x)">計算するLegendre多項式の最高次数と引数</param>
        let plgndrarray (pl:num1) (l:num0,x:num0) =
            codestr.section ("0次から"+l.code+"次までのLegendre多項式を計算") <| fun () ->
                ch.d <| fun fact ->
                ch.d <| fun pmm ->
                ch.d <| fun pmmp1 ->
                ch.d <| fun somx2 ->
                    pmm<==1.0
                    pl[1-1]<==pmm
                    br.branch <| fun b ->
                        b.IF (l .= 0) <| fun () -> 
                            pl[l+1-1] <== pmm
                        b.EL <| fun () -> 
                            pmmp1<==x*pmm
                            br.branch <| fun b ->
                                b.IF (l .= 1) <| fun () ->
                                    pl[l+1-1]<==pmmp1
                                b.EL <| fun () ->
                                    pl[2-1]<==pmmp1
                                    iter.range _2 l <| fun ll ->
                                        pl[ll+1-1]<==(x*(2*ll-1)*pmmp1-(ll-1)*pmm)/ll
                                        pmm<==pmmp1
                                        pmmp1<==pl[ll+1-1]
                                        
        //<summary>Legendre陪多項式を計算</summary>
        //<param name="pl">P_l^m</param>
        //<param name="l">整数</param>
        //<param name="m">整数</param>
        //<param name="x">実数</param>
        let aplgndr (pl:num0) (l:num0, m:num0, x:num0) =
            ch.dd <| fun (fact,pll) ->
            ch.ddd <| fun (pmm,pmmp1,somx2) ->
                br.if1 (Or [m .< 0; m .> l; asm.abs(x) .> 1.0]) <| fun () ->
                    print.t "Bad arguments in routine plgndr"
                pmm <== 1.0
                br.if1 (m .> 0) <| fun () ->
                    somx2 <== asm.sqrt((1.0-x)*(1.0+x))
                    fact <== 1.0
                    iter.num m <| fun i ->
                        pmm <== -pmm*fact*somx2
                        fact <== fact + 2.0
                br.if2 (l .= m)
                    <| fun () ->
                        pl <== pmm;
                    <| fun () ->
                        pmmp1 <== x*(2*m+1)*pmm
                        br.if2 (l .= (m+1))
                            <| fun () ->
                                pl <== pmmp1;
                            <| fun () ->
                                iter.range (m+2) l <| fun ll ->
                                    pll <== (x*(2*ll-1)*pmmp1-(ll+m-1)*pmm)/(ll-m)
                                    pmm <== pmmp1
                                    pmmp1 <== pll
                                pl <== pll
