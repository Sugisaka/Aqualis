(*
Copyright (c) 2022 Jun-ichiro Sugisaka

This software is released under the MIT License.
http://opensource.org/licenses/mit-license.php
*)
namespace Aqualis
    
    ///<summary>データ補間</summary>
    module interpolate = 
        
        open Aqualis.lapack
        open num0calc

        ///<summary>倍精度浮動小数点型の１次元線形補間データ</summary>
        type LinearInterpolate1d(id:string,data_x,data_y) =
            let X = var.dp1(id+"_x",data_x)
            let Y = var.dp1(id+"_y",data_y)
            ///<summary>元データを補間し、任意のxに対する値yを求めてcodeを実行</summary>
            member this.y (x:float0) code =
                ch.i <| fun flag ->
                    flag<==0
                    iter.range _1 (X.size1-1) <| fun i ->
                        br.if1 (X[i] <=. x <. X[i+1]) <| fun () ->
                            flag<==1
                            ch.d <| fun z ->
                                z <== Y[i] + (Y[i+1]-Y[i])*(x-X[i])/(X[i+1]-X[i])
                                code(z)
                    br.if1 (x =. X[X.size1]) <| fun () ->
                        flag<==1
                        code(Y[X.size1])
                    br.if1(flag =. 0) <| fun () ->
                        print.c x
                        print.c "is out of range:"
                        print.c X[1]
                        print.c X[X.size1]
                        print.br
                        
        ///<summary>倍精度浮動小数点型の１次元線形補間データ</summary>
        type LinearInterpolate1z(id:string,data_x,data_y) =
            let X = var.dp1(id+"_x",data_x)
            let Y = var.zp1(id+"_y",data_y)
            ///<summary>元データを補間し、任意のxに対する値yを求めてcodeを実行</summary>
            member this.y (x:float0) code =
                ch.i <| fun flag ->
                    flag<==0
                    iter.range _1 (X.size1-1) <| fun i ->
                        br.if1 (X[i] <=. x <. X[i+1]) <| fun () ->
                            flag<==1
                            ch.z <| fun z ->
                                z <== Y[i] + (Y[i+1]-Y[i])*(x-X[i])/(X[i+1]-X[i])
                                code(z)
                    br.if1 (x =. X[X.size1]) <| fun () ->
                        flag<==1
                        code(Y[X.size1])
                    br.if1(flag =. 0) <| fun () ->
                        print.c x
                        print.c "is out of range:"
                        print.c X[1]
                        print.c X[X.size1]
                        print.br
                    
        type splineInterpolate(iscpx:bool) =
            
            let f = 
                if iscpx then 
                    let u = var.z2("f")
                    u.num2
                else
                    let u = var.d2("f")
                    u.num2
            let g =
                if iscpx then
                    let u = var.z1("g")
                    u.num1
                else
                    let u = var.d1("g")
                    u.num1
            let x = var.d1("x")
            let y =
                if iscpx then
                    let u = var.z1("y")
                    u.num1
                else
                    let u = var.d1("y")
                    u.num1
                    
            let a(n:int0) = 3*n-2
            
            let b(n:int0) = 3*n-1
            
            let c(n:int0) = 3*n+0
        
            /// <summary>
            /// 補間前データ[x]
            /// </summary>
            member __.X = x
            
            /// <summary>
            /// 補間前データ[y]
            /// </summary>
            member __.Y = y
            
            /// <summary>
            /// 補間を実行
            /// </summary>
            member __.set() =
                ch.i <| fun N ->
                    N <== x.size1
                    f.allocate((if iscpx then Zt else Dt), 3*N-3, 3*N-3)
                    g.allocate((if iscpx then Zt else Dt), 3*N-3)
                    f.clear()
                    g.clear()
                    !"f''(x1)=0"
                    f[1,a(_1)] <== 0.0.D.num0
                    f[1,b(_1)] <== (2).I.num0
                    g[1] <== (0).I.num0
                    iter.range _2 (N-1) <| fun n ->
                        ch.d <| fun dx ->
                            dx <== x[n]-x[n-1]
                            !"f(xn)=yN"
                            f[3*n-4,a(n-1)] <== asm.pow(dx,3)
                            f[3*n-4,b(n-1)] <== asm.pow(dx,2)
                            f[3*n-4,c(n-1)] <== dx
                            g[3*n-4] <== y[n] - y[n-1]
                            !"f'(x[n-1])=f'(x[n])"
                            f[3*n-3,a(n-1)] <== 3*asm.pow(dx,2)
                            f[3*n-3,b(n-1)] <== 2*dx
                            f[3*n-3,c(n-1)] <== (1).I.num0
                            f[3*n-3,a(n)] <== (0.0).D.num0
                            f[3*n-3,b(n)] <== (0.0).D.num0
                            f[3*n-3,c(n)] <== (-1).I.num0
                            g[3*n-3] <== (0.0).D.num0
                            !"f''(x[n-1])=f''(x[n])"
                            f[3*n-2,a(n-1)] <== 6*dx
                            f[3*n-2,b(n-1)] <== (2).I.num0
                            f[3*n-2,a(n)] <== (0.0).D.num0
                            f[3*n-2,b(n)] <== (-2).I.num0
                            g[3*n-2] <== (0.0).D.num0
                    ch.d <| fun dx ->
                        dx <== x[N]-x[N-1]
                        !"f(xN)=yN"
                        f[3*N-4,a(N-1)] <== asm.pow(dx,3)
                        f[3*N-4,b(N-1)] <== asm.pow(dx,2)
                        f[3*N-4,c(N-1)] <== dx
                        g[3*N-4] <== y[N] - y[N-1]
                        !"f''(xN)=0"
                        f[3*N-3,a(N-1)] <== 6*dx
                        f[3*N-3,b(N-1)] <== (2).I.num0
                        g[3*N-3] <== (0.0).D.num0
                        
                    //io.array f [!."f.dat"]
                    //io.array g [!."g.dat"]
                    if iscpx then
                        let f = complex2(f.expr)
                        let g = complex1(g.expr)
                        La.solve_simuleq(f,g)
                    else
                        let f = float2(f.expr)
                        let g = float1(g.expr)
                        La.solve_simuleq(f,g)
                
                    
            /// <summary>
            /// 補間データをファイルに保存
            /// </summary>
            /// <param name="filename"></param>
            member __.save(filename:string) =
                io.fileOutput (filename+"_x.dat",[]) <| fun wr ->
                    wr.Write x.size1
                    wr.br
                    iter.num x.size1 <| fun i ->
                        wr.Write x[i]
                        wr.br
                io.fileOutput (filename+"_y.dat",[]) <| fun wr ->
                    wr.Write y.size1
                    wr.br
                    iter.num y.size1 <| fun i ->
                        if iscpx then
                            let y = complex1(y.expr)
                            wr.Write y[i]
                        else
                            let y = float1(y.expr)
                            wr.Write y[i]
                        wr.br
                io.fileOutput (filename+"_g.dat",[]) <| fun wr ->
                    wr.Write g.size1
                    iter.num g.size1 <| fun i ->
                        if iscpx then
                            let g = complex1(g.expr)
                            wr.Write g[i]
                        else
                            let g = float1(g.expr)
                            wr.Write g[i]
                        wr.br
                        
            /// <summary>
            /// 保存した補間データを読み込み
            /// </summary>
            /// <param name="filename"></param>
            member __.load(filename:string) =
                io.fileInput (filename+"_x.dat",[]) <| fun wr ->
                    ch.id <| fun (n,t) ->
                        wr.Read n
                        wr.br
                        x.allocate(n)
                        iter.num x.size1 <| fun i ->
                            wr.Read t
                            wr.br
                            x[i] <== t
                io.fileInput (filename+"_y.dat",[]) <| fun wr ->
                    ch.idd <| fun (n,s,t) ->
                        wr.Read n
                        wr.br
                        y.allocate((if iscpx then Zt else Dt), n)
                        iter.num y.size1 <| fun i ->
                            wr.Read s
                            wr.Read t
                            wr.br
                            y[i] <== s+asm.uj*t
                io.fileInput (filename+"_g.dat",[]) <| fun wr ->
                    ch.idd <| fun (n,s,t) ->
                        wr.Read n
                        wr.br
                        g.allocate((if iscpx then Zt else Dt), n)
                        iter.num g.size1 <| fun i ->
                            wr.Read s
                            wr.Read t
                            wr.br
                            g[i] <== s+asm.uj*t
                                
            /// <summary>
            /// 補間後の関数
            /// </summary>
            /// <param name="yy"></param>
            /// <param name="xx"></param>
            member __.p (yy:float0) = fun (xx:float0) ->
                let g = float1(g.expr)
                let y = float1(y.expr)
                yy.clear()
                iter.num_exit <| x.size1-1 <| fun (ex,i) ->
                    br.if1 (x[i] <=. xx <. x[i+1]) <| fun () ->
                        yy <== g[a(i)]*asm.pow(xx-x[i],3) + g[b(i)]*asm.pow(xx-x[i],2) + g[c(i)]*(xx-x[i]) + y[i]
                        ex()    
            /// <summary>
            /// 補間後の関数
            /// </summary>
            /// <param name="yy"></param>
            /// <param name="xx"></param>
            member __.p (yy:complex0) = fun (xx:float0) ->
                let g = complex1(g.expr)
                let y = complex1(y.expr)
                yy.clear()
                iter.num_exit <| x.size1-1 <| fun (ex,i) ->
                    br.if1 (x[i] <=. xx <. x[i+1]) <| fun () ->
                        yy <== g[a(i)]*asm.pow(xx-x[i],3) + g[b(i)]*asm.pow(xx-x[i],2) + g[c(i)]*(xx-x[i]) + y[i]
                        ex()
                        
            /// <summary>
            /// 補間後の関数の微分
            /// </summary>
            /// <param name="yy"></param>
            /// <param name="xx"></param>
            member __.dp (yy:float0) = fun (xx:float0) ->
                let g = float1(g.expr)
                yy.clear()
                iter.num_exit <| x.size1-1 <| fun (ex,i) ->
                    br.if1 (x[i] <=. xx <. x[i+1]) <| fun () ->
                        yy <== 3*g[a(i)]*asm.pow(xx-x[i],2) + 2*g[b(i)]*(xx-x[i]) + g[c(i)]
                        ex()
            /// <summary>
            /// 補間後の関数の微分
            /// </summary>
            /// <param name="yy"></param>
            /// <param name="xx"></param>
            member __.dp (yy:complex0) = fun (xx:float0) ->
                let g = complex1(g.expr)
                yy.clear()
                iter.num_exit <| x.size1-1 <| fun (ex,i) ->
                    br.if1 (x[i] <=. xx <. x[i+1]) <| fun () ->
                        yy <== 3*g[a(i)]*asm.pow(xx-x[i],2) + 2*g[b(i)]*(xx-x[i]) + g[c(i)]
                        ex()
                        