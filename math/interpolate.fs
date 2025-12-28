namespace Aqualis
    
    ///<summary>データ補間</summary>
    module interpolate = 
        
        ///<summary>倍精度浮動小数点型の１次元線形補間データ</summary>
        type LinearInterpolate1d(id:string,data_x,data_y) =
            let X = var.dp1(id+"_x",data_x)
            let Y = var.dp1(id+"_y",data_y)
            ///<summary>元データを補間し、任意のxに対する値yを求めてcodeを実行</summary>
            member this.y (x:num0) code =
                ch.i <| fun flag ->
                    flag<==0
                    iter.range _0 (X.size1-2) <| fun i ->
                        br.if1 (X.[i] .<= x .< X.[i+1]) <| fun () ->
                            flag<==1
                            ch.d <| fun z ->
                                z <== Y.[i] + (Y.[i+1]-Y.[i])*(x-X.[i])/(X.[i+1]-X.[i])
                                code(z)
                    br.if1 (x.=X.[X.size1]) <| fun () ->
                        flag<==1
                        code(Y.[X.size1])
                    br.if1(flag.=0) <| fun () -> print.w <| x++"is out of range:"++X.[1]++X.[X.size1]
                    
        ///<summary>倍精度浮動小数点型の１次元線形補間データ</summary>
        type LinearInterpolate1z(id:string,data_x,data_y) =
            let X = var.dp1(id+"_x",data_x)
            let Y = var.zp1(id+"_y",data_y)
            ///<summary>元データを補間し、任意のxに対する値yを求めてcodeを実行</summary>
            member this.y (x:num0) code =
                ch.i <| fun flag ->
                    flag<==0
                    iter.range _0 (X.size1-2) <| fun i ->
                        br.if1 (X.[i].<=x.<X.[i+1]) <| fun () ->
                            flag<==1
                            ch.z <| fun z ->
                                z <== Y.[i] + (Y.[i+1]-Y.[i])*(x-X.[i])/(X.[i+1]-X.[i])
                                code(z)
                    br.if1 (x.=X.[X.size1]) <| fun () ->
                        flag<==1
                        code(Y.[X.size1])
                    br.if1(flag.=0) <| fun () -> print.w <| x++"is out of range:"++X.[1]++X.[X.size1]
                    
        type splineInterpolate(iscpx:bool) =
            
            let f = if iscpx then var.z2("f") else var.d2("f")
            let g = if iscpx then var.z1("g") else var.d1("g")
            let x = var.d1("x")
            let y = if iscpx then var.z1("y") else var.d1("y")
            
            let a(n:num0) = 3*n-2
            
            let b(n:num0) = 3*n-1
            
            let c(n:num0) = 3*n+0
        
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
                    f.allocate(3*N-3,3*N-3)
                    g.allocate(3*N-3)
                    f.clear()
                    g.clear()
                    !"f''(x1)=0"
                    f.[1-1,a(_1)-1] <== 0.0
                    f.[1-1,b(_1)-1] <== 2
                    g.[1-1] <== 0
                    iter.range _1 (N-2) <| fun n ->
                        ch.d <| fun dx ->
                            dx <== x.[n+1-1]-x.[n+1-1-1]
                            !"f(xn)=yN"
                            f.[3*(n+1)-4-1,a((n+1)-1)-1] <== asm.pow(dx,3)
                            f.[3*(n+1)-4-1,b((n+1)-1)-1] <== asm.pow(dx,2)
                            f.[3*(n+1)-4-1,c((n+1)-1)-1] <== dx
                            g.[3*(n+1)-4-1] <== y.[(n+1)-1] - y.[(n+1)-1-1]
                            !"f'(x[n-1])=f'(x[n])"
                            f.[3*(n+1)-3-1,a((n+1)-1)-1] <== 3*asm.pow(dx,2)
                            f.[3*(n+1)-3-1,b((n+1)-1)-1] <== 2*dx
                            f.[3*(n+1)-3-1,c((n+1)-1)-1] <== 1
                            f.[3*(n+1)-3-1,a((n+1))-1] <== 0.0
                            f.[3*(n+1)-3-1,b((n+1))-1] <== 0.0
                            f.[3*(n+1)-3-1,c((n+1))-1] <== -1
                            g.[3*(n+1)-3-1] <== 0.0
                            !"f''(x[n-1])=f''(x[n])"
                            f.[3*(n+1)-2-1,a((n+1)-1)-1] <== 6*dx
                            f.[3*(n+1)-2-1,b((n+1)-1)-1] <== 2
                            f.[3*(n+1)-2-1,a((n+1))-1] <== 0.0
                            f.[3*(n+1)-2-1,b((n+1))-1] <== -2
                            g.[3*(n+1)-2-1] <== 0.0
                    ch.d <| fun dx ->
                        dx <== x.[N-1]-x.[N-1-1]
                        !"f(xN)=yN"
                        f.[3*N-4-1,a(N-1)-1] <== asm.pow(dx,3)
                        f.[3*N-4-1,b(N-1)-1] <== asm.pow(dx,2)
                        f.[3*N-4-1,c(N-1)-1] <== dx
                        g.[3*N-4-1] <== y.[N-1] - y.[N-1-1]
                        !"f''(xN)=0"
                        f.[3*N-3-1,a(N-1)-1] <== 6*dx
                        f.[3*N-3-1,b(N-1)-1] <== 2
                        g.[3*N-3-1] <== 0
                        
                    //io.array f [!."f.dat"]
                    //io.array g [!."g.dat"]
                    La.solve_simuleq(f,g)
                    
            /// <summary>
            /// 補間データをファイルに保存
            /// </summary>
            /// <param name="filename"></param>
            member __.save(filename:string) =
                io.fileOutput (filename+"_x.dat") <| fun wr ->
                    wr [x.size1]
                    iter.num x.size1 <| fun i ->
                        wr [x.[i]]
                io.fileOutput (filename+"_y.dat") <| fun wr ->
                    wr [y.size1]
                    iter.num y.size1 <| fun i ->
                        wr [y.[i]]
                io.fileOutput (filename+"_g.dat") <| fun wr ->
                    wr [g.size1]
                    iter.num g.size1 <| fun i ->
                        wr [g.[i]]
                        
            /// <summary>
            /// 保存した補間データを読み込み
            /// </summary>
            /// <param name="filename"></param>
            member __.load(filename:string) =
                if iscpx then
                    io.fileInput (filename+"_x.dat") <| fun wr ->
                        ch.id <| fun (n,t) ->
                            wr [n]
                            x.allocate n
                            iter.num x.size1 <| fun i ->
                                wr [t]
                                x.[i] <== t
                    io.fileInput (filename+"_y.dat") <| fun wr ->
                        ch.idd <| fun (n,s,t) ->
                            wr [n]
                            y.allocate n
                            iter.num y.size1 <| fun i ->
                                wr [s;t]
                                y.[i] <== s+asm.uj*t
                    io.fileInput (filename+"_g.dat") <| fun wr ->
                        ch.idd <| fun (n,s,t) ->
                            wr [n]
                            g.allocate n
                            iter.num g.size1 <| fun i ->
                                wr [s;t]
                                g.[i] <== s+asm.uj*t
                else
                    io.fileInput (filename+"_x.dat") <| fun wr ->
                        ch.id <| fun (n,t) ->
                            wr [n]
                            x.allocate n
                            iter.num x.size1 <| fun i ->
                                wr [t]
                                x.[i] <== t
                    io.fileInput (filename+"_y.dat") <| fun wr ->
                        ch.id <| fun (n,t) ->
                            wr [n]
                            y.allocate n
                            iter.num y.size1 <| fun i ->
                                wr [t]
                                y.[i] <== t
                    io.fileInput (filename+"_g.dat") <| fun wr ->
                        ch.id <| fun (n,t) ->
                            wr [n]
                            g.allocate n
                            iter.num g.size1 <| fun i ->
                                wr [t]
                                g.[i] <== t
                                
            /// <summary>
            /// 補間後の関数
            /// </summary>
            /// <param name="yy"></param>
            /// <param name="xx"></param>
            member __.p (yy:num0) (xx:num0) =
                yy.clear()
                iter.num_exit <| x.size1-1 <| fun (ex,i) ->
                    br.if1 (x.[i].<=xx.<x.[i+1]) <| fun () ->
                        yy <== g.[a(i+1)-1]*asm.pow(xx-x.[i],3) + g.[b(i+1)-1]*asm.pow(xx-x.[i],2) + g.[c(i+1)-1]*(xx-x.[i+1]) + y.[i]
                        ex()
                        
            /// <summary>
            /// 補間後の関数の微分
            /// </summary>
            /// <param name="yy"></param>
            /// <param name="xx"></param>
            member __.dp (yy:num0) (xx:num0) =
                yy.clear()
                iter.num_exit <| x.size1-1 <| fun (ex,i) ->
                    br.if1 (x.[i].<=xx.<x.[i+1]) <| fun () ->
                        yy <== 3*g.[a(i+1)-1]*asm.pow(xx-x.[i+1],2) + 2*g.[b(i+1)-1]*(xx-x.[i+1]) + g.[c(i+1)-1]
                        ex()
                        