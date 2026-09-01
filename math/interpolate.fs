//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    ///<summary>データ補間</summary>
    module interpolate =

        let private validateLinearData (dataX:double list) dataYCount =
            if dataX.Length < 2 then
                invalidArg "data_x" "Linear interpolation requires at least two data points."
            if dataX.Length <> dataYCount then
                invalidArg "data_y" "The x and y data must contain the same number of points."
            if dataX |> List.exists (System.Double.IsFinite >> not) then
                invalidArg "data_x" "The x data must contain only finite values."
            if dataX |> List.pairwise |> List.exists (fun (left, right) -> right <= left) then
                invalidArg "data_x" "The x data must be strictly increasing."

        ///<summary>倍精度浮動小数点型の１次元線形補間データ</summary>
        type LinearInterpolate1d(context:Aqualis,id:string,data_x:double list,data_y:double list) =
            do validateLinearData data_x data_y.Length
            let X = context.var.dp1(id+"_x",data_x)
            let Y = context.var.dp1(id+"_y",data_y)
            ///<summary>元データを補間し、任意のxに対する値yを求めてcodeを実行</summary>
            member _.y (x:double0) code =
                let lastIndex = X.size1 - 1
                context.ch.i <| fun flag ->
                    flag<==0
                    context.iter.range (_0, lastIndex-1) <| fun i ->
                        context.br.if1 (X.[i] .<= x .< X.[i+1]) <| fun () ->
                            flag<==1
                            context.ch.d <| fun z ->
                                z <== Y.[i] + (Y.[i+1]-Y.[i])*(x-X.[i])/(X.[i+1]-X.[i])
                                code(z)
                    context.br.if1 (x.=X.[lastIndex]) <| fun () ->
                        flag<==1
                        code(Y.[lastIndex])
                    context.br.if1(flag.=0) <| fun () -> context.print.tt <| x++"is out of range:"++X.[0]++X.[lastIndex]

        ///<summary>倍精度浮動小数点型の１次元線形補間データ</summary>
        type LinearInterpolate1z(context:Aqualis,id:string,data_x:double list,data_y:(double*double) list) =
            do validateLinearData data_x data_y.Length
            let X = context.var.dp1(id+"_x",data_x)
            let Y = context.var.zp1(id+"_y",data_y)
            ///<summary>元データを補間し、任意のxに対する値yを求めてcodeを実行</summary>
            member this.y (x:double0) code =
                let lastIndex = X.size1 - 1
                context.ch.i <| fun flag ->
                    flag<==0
                    context.iter.range (_0, lastIndex-1) <| fun i ->
                        context.br.if1 (X.[i].<=x.<X.[i+1]) <| fun () ->
                            flag<==1
                            context.ch.z <| fun z ->
                                z <== Y.[i] + (Y.[i+1]-Y.[i])*(x-X.[i])/(X.[i+1]-X.[i])
                                code(z)
                    context.br.if1 (x.=X.[lastIndex]) <| fun () ->
                        flag<==1
                        code(Y.[lastIndex])
                    context.br.if1(flag.=0) <| fun () -> context.print.tt <| x++"is out of range:"++X.[0]++X.[lastIndex]

        type splineInterpolateDouble(context:Aqualis) =

            let f = context.var.d2 "f"
            let g = context.var.d1 "g"
            let x = context.var.d1 "x"
            let y = context.var.d1 "y"

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
                context.ch.i <| fun N ->
                    N <== x.size1
                    f.allocate(3*N-3,3*N-3)
                    g.allocate(3*N-3)
                    f.clear()
                    g.clear()
                    context.group.comment "interpolation constraint"
                    f.[1-1,a _1-1] <== 0.0
                    f.[1-1,b _1-1] <== 2
                    g.[1-1] <== 0
                    context.iter.range (_1, N-2) <| fun n ->
                        context.ch.d <| fun dx ->
                            dx <== x.[n+1-1]-x.[n+1-1-1]
                            context.group.comment "interpolation constraint"
                            f.[3*(n+1)-4-1,a(n+1-1)-1] <== asm.pow(dx,3)
                            f.[3*(n+1)-4-1,b(n+1-1)-1] <== asm.pow(dx,2)
                            f.[3*(n+1)-4-1,c(n+1-1)-1] <== dx
                            g.[3*(n+1)-4-1] <== y.[(n+1)-1] - y.[(n+1)-1-1]
                            context.group.comment "interpolation constraint"
                            f.[3*(n+1)-3-1,a(n+1-1)-1] <== 3*asm.pow(dx,2)
                            f.[3*(n+1)-3-1,b(n+1-1)-1] <== 2*dx
                            f.[3*(n+1)-3-1,c(n+1-1)-1] <== 1
                            f.[3*(n+1)-3-1,a(n+1)-1] <== 0.0
                            f.[3*(n+1)-3-1,b(n+1)-1] <== 0.0
                            f.[3*(n+1)-3-1,c(n+1)-1] <== -1
                            g.[3*(n+1)-3-1] <== 0.0
                            context.group.comment "interpolation constraint"
                            f.[3*(n+1)-2-1,a(n+1-1)-1] <== 6*dx
                            f.[3*(n+1)-2-1,b(n+1-1)-1] <== 2
                            f.[3*(n+1)-2-1,a(n+1)-1] <== 0.0
                            f.[3*(n+1)-2-1,b(n+1)-1] <== -2
                            g.[3*(n+1)-2-1] <== 0.0
                    context.ch.d <| fun dx ->
                        dx <== x.[N-1]-x.[N-1-1]
                        context.group.comment "interpolation constraint"
                        f.[3*N-4-1,a(N-1)-1] <== asm.pow(dx,3)
                        f.[3*N-4-1,b(N-1)-1] <== asm.pow(dx,2)
                        f.[3*N-4-1,c(N-1)-1] <== dx
                        g.[3*N-4-1] <== y.[N-1] - y.[N-1-1]
                        context.group.comment "interpolation constraint"
                        f.[3*N-3-1,a(N-1)-1] <== 6*dx
                        f.[3*N-3-1,b(N-1)-1] <== 2
                        g.[3*N-3-1] <== 0
                    context.la.solve_simuleq(f,g)

            /// <summary>
            /// 補間データをファイルに保存
            /// </summary>
            /// <param name="filename"></param>
            member __.save(filename:string) =
                context.io.fileOutput (filename+"_x.dat") <| fun wr ->
                    wr.t x.size1
                    context.iter.num x.size1 <| fun i ->
                        wr.t x.[i]
                context.io.fileOutput (filename+"_y.dat") <| fun wr ->
                    wr.t y.size1
                    context.iter.num y.size1 <| fun i ->
                        wr.t y.[i]
                context.io.fileOutput (filename+"_g.dat") <| fun wr ->
                    wr.t g.size1
                    context.iter.num g.size1 <| fun i ->
                        wr.t g.[i]

            /// <summary>
            /// 保存した補間データを読み込み
            /// </summary>
            /// <param name="filename"></param>
            member __.load(filename:string) =
                context.io.fileInput (filename+"_x.dat") <| fun wr ->
                    context.ch.id <| fun (n,t) ->
                        wr.t n
                        x.allocate n
                        context.iter.num x.size1 <| fun i ->
                            wr.t t
                            x.[i] <== t
                context.io.fileInput (filename+"_y.dat") <| fun wr ->
                    context.ch.id <| fun (n,t) ->
                        wr.t n
                        y.allocate n
                        context.iter.num y.size1 <| fun i ->
                            wr.t t
                            y.[i] <== t
                context.io.fileInput (filename+"_g.dat") <| fun wr ->
                    context.ch.id <| fun (n,t) ->
                        wr.t n
                        g.allocate n
                        context.iter.num g.size1 <| fun i ->
                            wr.t t
                            g.[i] <== t

            /// <summary>
            /// 補間後の関数
            /// </summary>
            /// <param name="yy"></param>
            /// <param name="xx"></param>
            member __.p (yy:double0) (xx:double0) =
                yy.clear()
                context.iter.num_exit (x.size1-1) <| fun (ex,i) ->
                    context.br.if1 (x.[i].<=xx.<x.[i+1]) <| fun () ->
                        yy <== g.[a(i+1)-1]*asm.pow(xx-x.[i],3) + g.[b(i+1)-1]*asm.pow(xx-x.[i],2) + g.[c(i+1)-1]*(xx-x.[i+1]) + y.[i]
                        ex()

            /// <summary>
            /// 補間後の関数の微分
            /// </summary>
            /// <param name="yy"></param>
            /// <param name="xx"></param>
            member __.dp (yy:double0) (xx:double0) =
                yy.clear()
                context.iter.num_exit (x.size1-1) <| fun (ex,i) ->
                    context.br.if1 (x.[i].<=xx.<x.[i+1]) <| fun () ->
                        yy <== 3*g.[a(i+1)-1]*asm.pow(xx-x.[i+1],2) + 2*g.[b(i+1)-1]*(xx-x.[i+1]) + g.[c(i+1)-1]
                        ex()

        type splineInterpolateComplex(context:Aqualis,iscpx:bool) =

            let f = context.var.z2 "f"
            let g = context.var.z1 "g"
            let x = context.var.d1 "x"
            let y = context.var.z1 "y"

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
                context.ch.i <| fun N ->
                    N <== x.size1
                    f.allocate(3*N-3,3*N-3)
                    g.allocate(3*N-3)
                    f.clear()
                    g.clear()
                    context.group.comment "interpolation constraint"
                    f.[1-1,a _1-1] <== 0.0
                    f.[1-1,b _1-1] <== 2
                    g.[1-1] <== 0
                    context.iter.range (_1, N-2) <| fun n ->
                        context.ch.d <| fun dx ->
                            dx <== x.[n+1-1]-x.[n+1-1-1]
                            context.group.comment "interpolation constraint"
                            f.[3*(n+1)-4-1,a(n+1-1)-1] <== asm.pow(dx,3)
                            f.[3*(n+1)-4-1,b(n+1-1)-1] <== asm.pow(dx,2)
                            f.[3*(n+1)-4-1,c(n+1-1)-1] <== dx
                            g.[3*(n+1)-4-1] <== y.[(n+1)-1] - y.[(n+1)-1-1]
                            context.group.comment "interpolation constraint"
                            f.[3*(n+1)-3-1,a(n+1-1)-1] <== 3*asm.pow(dx,2)
                            f.[3*(n+1)-3-1,b(n+1-1)-1] <== 2*dx
                            f.[3*(n+1)-3-1,c(n+1-1)-1] <== 1
                            f.[3*(n+1)-3-1,a(n+1)-1] <== 0.0
                            f.[3*(n+1)-3-1,b(n+1)-1] <== 0.0
                            f.[3*(n+1)-3-1,c(n+1)-1] <== -1
                            g.[3*(n+1)-3-1] <== 0.0
                            context.group.comment "interpolation constraint"
                            f.[3*(n+1)-2-1,a(n+1-1)-1] <== 6*dx
                            f.[3*(n+1)-2-1,b(n+1-1)-1] <== 2
                            f.[3*(n+1)-2-1,a(n+1)-1] <== 0.0
                            f.[3*(n+1)-2-1,b(n+1)-1] <== -2
                            g.[3*(n+1)-2-1] <== 0.0
                    context.ch.d <| fun dx ->
                        dx <== x.[N-1]-x.[N-1-1]
                        context.group.comment "interpolation constraint"
                        f.[3*N-4-1,a(N-1)-1] <== asm.pow(dx,3)
                        f.[3*N-4-1,b(N-1)-1] <== asm.pow(dx,2)
                        f.[3*N-4-1,c(N-1)-1] <== dx
                        g.[3*N-4-1] <== y.[N-1] - y.[N-1-1]
                        context.group.comment "interpolation constraint"
                        f.[3*N-3-1,a(N-1)-1] <== 6*dx
                        f.[3*N-3-1,b(N-1)-1] <== 2
                        g.[3*N-3-1] <== 0
                    context.la.solve_simuleq(f,g)

            /// <summary>
            /// 補間データをファイルに保存
            /// </summary>
            /// <param name="filename"></param>
            member __.save(filename:string) =
                context.io.fileOutput (filename+"_x.dat") <| fun wr ->
                    wr.t x.size1
                    context.iter.num x.size1 <| fun i ->
                        wr.t x.[i]
                context.io.fileOutput (filename+"_y.dat") <| fun wr ->
                    wr.t y.size1
                    context.iter.num y.size1 <| fun i ->
                        wr.t y.[i]
                context.io.fileOutput (filename+"_g.dat") <| fun wr ->
                    wr.t g.size1
                    context.iter.num g.size1 <| fun i ->
                        wr.t g.[i]

            /// <summary>
            /// 保存した補間データを読み込み
            /// </summary>
            /// <param name="filename"></param>
            member __.load(filename:string) =
                if iscpx then
                    context.io.fileInput (filename+"_x.dat") <| fun rd ->
                        context.ch.id <| fun (n,t) ->
                            rd.t n
                            x.allocate n
                            context.iter.num x.size1 <| fun i ->
                                rd.t t
                                x.[i] <== t
                    context.io.fileInput (filename+"_y.dat") <| fun rd ->
                        context.ch.idd <| fun (n,s,t) ->
                            rd.t n
                            y.allocate n
                            context.iter.num y.size1 <| fun i ->
                                rd.tt <| s++t
                                y.[i] <== s+asm.uj*t
                    context.io.fileInput (filename+"_g.dat") <| fun rd ->
                        context.ch.idd <| fun (n,s,t) ->
                            rd.t n
                            g.allocate n
                            context.iter.num g.size1 <| fun i ->
                                rd.tt <| s++t
                                g.[i] <== s+asm.uj*t
                else
                    context.io.fileInput (filename+"_x.dat") <| fun rd ->
                        context.ch.id <| fun (n,t) ->
                            rd.t n
                            x.allocate n
                            context.iter.num x.size1 <| fun i ->
                                rd.t t
                                x.[i] <== t
                    context.io.fileInput (filename+"_y.dat") <| fun rd ->
                        context.ch.id <| fun (n,t) ->
                            rd.t n
                            y.allocate n
                            context.iter.num y.size1 <| fun i ->
                                rd.t t
                                y.[i] <== t
                    context.io.fileInput (filename+"_g.dat") <| fun rd ->
                        context.ch.id <| fun (n,t) ->
                            rd.t n
                            g.allocate n
                            context.iter.num g.size1 <| fun i ->
                                rd.t t
                                g.[i] <== t

            /// <summary>
            /// 補間後の関数
            /// </summary>
            /// <param name="yy"></param>
            /// <param name="xx"></param>
            member __.p (yy:complex0) (xx:double0) =
                yy.clear()
                context.iter.num_exit (x.size1-1) <| fun (ex,i) ->
                    context.br.if1 (x.[i].<=xx.<x.[i+1]) <| fun () ->
                        yy <== g.[a(i+1)-1]*asm.pow(xx-x.[i],3) + g.[b(i+1)-1]*asm.pow(xx-x.[i],2) + g.[c(i+1)-1]*(xx-x.[i+1]) + y.[i]
                        ex()

            /// <summary>
            /// 補間後の関数の微分
            /// </summary>
            /// <param name="yy"></param>
            /// <param name="xx"></param>
            member __.dp (yy:complex0) (xx:double0) =
                yy.clear()
                context.iter.num_exit (x.size1-1) <| fun (ex,i) ->
                    context.br.if1 (x.[i].<=xx.<x.[i+1]) <| fun () ->
                        yy <== 3*g.[a(i+1)-1]*asm.pow(xx-x.[i+1],2) + 2*g.[b(i+1)-1]*(xx-x.[i+1]) + g.[c(i+1)-1]
                        ex()

    type ContextInterpolate internal (context:Aqualis) =
        member _.linearDouble(id,dataX,dataY) = interpolate.LinearInterpolate1d(context,id,dataX,dataY)
        member _.linearComplex(id,dataX,dataY) = interpolate.LinearInterpolate1z(context,id,dataX,dataY)
        member _.splineDouble() = interpolate.splineInterpolateDouble(context)
        member _.splineComplex(isComplex) = interpolate.splineInterpolateComplex(context,isComplex)

    [<AutoOpen>]
    module CompilationEnvironmentInterpolateExtensions =
        type Aqualis with
            ///<summary>データ補間</summary>
            member this.interpolate = ContextInterpolate(this)
