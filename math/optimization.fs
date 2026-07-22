//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

type ContextOptimization internal (environment:CompilationEnvironment) =

    /// <summary>
    /// 1次元極小値検索
    /// </summary>
    /// <param name="m">直線探索の反復数：探索間隔dd*2^(-m)になるまで計算</param>
    /// <param name="x0_">初期解</param>
    /// <param name="df">探索方向</param>
    /// <param name="dd">探索幅</param>
    /// <param name="f">目的関数</param>
    /// <param name="xx">fが極小となるベクトル</param>
    member this.findmin (m:int) (x0_:double1,df:double1) (dd:double0) (f:double0->double1->unit) (xx:double1) =
        let r = 0.5*(1.0+sqrt(5.0))
        environment.ch.d1 x0_.size1 <| fun xa ->
        environment.ch.d1 x0_.size1 <| fun x1 ->
        environment.ch.d1 x0_.size1 <| fun x2 ->
        environment.ch.d1 x0_.size1 <| fun xb ->
        environment.ch.dddd <| fun (fa,f1,f2,fb) ->
        environment.ch.d <| fun fa_ ->
        environment.ch.d <| fun norm_df ->
        environment.ch.i <| fun counter ->
            norm_df.clear()
            environment.iter.num df.size1 <| fun i ->
                norm_df <== norm_df + df.[i]*df.[i]
            norm_df <== asm.sqrt(norm_df)
            xa.foreach <| fun i -> xa.[i] <== x0_.[i]
            f fa xa
            xb.foreach <| fun i -> xb.[i] <== x0_.[i] + dd * df.[i]/norm_df
            f fb xb
            x1.foreach <| fun i -> x1.[i] <== xa.[i] + (xb.[i]-xa.[i])/(1.0+r)
            f f1 x1
            x2.foreach <| fun i -> x2.[i] <== xa.[i] + (xb.[i]-xa.[i])/r
            f f2 x2
            counter.clear()
            fa_ <== fa
            environment.iter.whiledo (counter.<m) <| fun _ ->
                environment.br.branch <| fun b ->
                    b.IF (fa .> f1 .> f2 .> fb) <| fun () ->
                        counter.dec()
                        //xa: そのまま
                        x1 <== x2
                        f1 <== f2
                        x2 <== xb
                        f2 <== fb
                        //xb: 新規計算
                        xb.foreach <| fun i -> xb.[i] <== xa.[i] + (x1.[i]-xa.[i])*(1.0+r)
                        f fb xb
                    b.IF (And [f1.>f2; fa.>f2;]) <| fun () ->
                        counter.inc()
                        xa <== x1
                        fa <== f1
                        x1 <== x2
                        f1 <== f2
                        //xb: そのまま
                        //x2: 新規計算
                        x2.foreach <| fun i -> x2.[i] <== xa.[i] + (xb.[i]-xa.[i])/r
                        f f2 x2
                    b.IF (Or [And [f1.>f2; fa.<f2;]; f1.<=f2;]) <| fun () ->
                        counter.inc()
                        //xa: そのまま
                        xb <== x2
                        fb <== f2
                        x2 <== x1
                        f2 <== f1
                        //x1: 新規計算
                        x1.foreach <| fun i -> x1.[i] <== xa.[i] + (xb.[i]-xa.[i])/(1.0+r)
                        f f1 x1
                    b.EL <| fun () ->
                        environment.print.s "error: findmin"
            xx.foreach <| fun i -> xx.[i] <== 0.5*(xa.[i]+xb.[i])

    /// <summary>
    /// 最急降下法
    /// </summary>
    /// <param name="n">反復数</param>
    /// <param name="m">直線探索の反復数：探索間隔dd0*2^(-m)になるまで計算</param>
    /// <param name="x0">初期解</param>
    /// <param name="dd0">探索間隔</param>
    /// <param name="f">目的関数</param>
    /// <param name="df">目的関数の勾配</param>
    /// <param name="stepProc">各ステップの暫定解に対して行う処理</param>
    member this.findmin_GradientDescent (n:int,m:int) (x0:double1) (dd0:double0) (f:double0->double1->unit) (df:double1->double1->unit) (stepProc:((int0*double1)->unit)option) =
        match stepProc with
          |Some(pu) ->
                pu (_0,x0)
          |None ->
            ()
        environment.ch.d <| fun dd0_ ->
            environment.ch.d1 x0.size1 <| fun y ->
                environment.iter.num_exit (I n) <| fun (ext,i) ->
                    dd0_ <== dd0
                    environment.ch.d1 x0.size1 <| fun (df0:double1) ->
                        df df0 x0
                        //勾配を計算
                        y.foreach <| fun i ->
                            y.[i] <== -df0.[i]
                        environment.la.norm y <| fun nr ->
                            environment.br.if1 (nr.=0.0) <| fun () ->
                                ext()
                        //勾配方向に最小値を探す
                        this.findmin m (x0,y) dd0_ f x0
                    match stepProc with
                      |Some(pu) ->
                            pu(i,x0)
                      |None ->
                        ()

    /// <summary>
    /// ヘッセ行列を用いた共役勾配法
    /// </summary>
    /// <param name="n">反復数</param>
    /// <param name="m">直線探索の反復数：探索間隔dd0*2^(-m)になるまで計算</param>
    /// <param name="x0">初期解</param>
    /// <param name="dd0">探索間隔</param>
    /// <param name="f">目的関数</param>
    /// <param name="df">目的関数の勾配</param>
    /// <param name="fH">目的関数のヘッセ行列</param>
    member this.findmin_ConjugateGradient1 (n:int,m:int) (x0:double1) (dd0:double0) (f:double0->double1->unit) (df:double1->double1->unit) (fH:double2->double1->unit) (stepProc:((int0*double1)->unit)option) =
        match stepProc with
          |Some(pu) ->
                pu (_0,x0)
          |None ->
            ()
        environment.ch.d <| fun a ->
        environment.ch.d <| fun dd0_ ->
            environment.ch.d1 x0.size1 <| fun b ->
            environment.ch.d1 x0.size1 <| fun y ->
                b.clear()
                environment.iter.num_exit (I n) <| fun (ext,i) ->
                    dd0_ <== dd0
                    environment.ch.d1 x0.size1 <| fun df0 ->
                        df df0 x0
                        environment.br.branch <| fun r ->
                            r.IF (i.=0) <| fun () ->
                                a <== 0
                            r.EL <| fun () ->
                                environment.ch.d2 x0.size1 x0.size1 <| fun h ->
                                    fH h x0
                                    environment.la.matmul (h,df0) <| fun p1 ->
                                        environment.la.matmul (h,b) <| fun p2 ->
                                            environment.la.dot (b,p1) <| fun c1 ->
                                                environment.la.dot (b,p2) <| fun c2 ->
                                                    environment.br.if1 (c2.=0.0) <| fun () -> ext()
                                                    a <== c1/c2
                        //勾配を計算
                        y.foreach <| fun i ->
                            y.[i] <== -df0.[i] + a * b.[i]
                        environment.la.norm y <| fun nr ->
                            environment.br.if1 (nr.=0.0) <| fun () -> ext()
                        //勾配方向に最小値を探す
                        this.findmin m (x0,y) dd0_ f x0
                        match stepProc with
                          |Some pu ->
                                pu(i,x0)
                          |None ->
                            ()
                        y.foreach <| fun i ->
                            b.[i] <== y.[i]

    /// <summary>
    /// ヘッセ行列を用いない共役勾配法
    /// </summary>
    /// <param name="n">反復数</param>
    /// <param name="m">直線探索の反復数：探索間隔dd0*2^(-m)になるまで計算</param>
    /// <param name="x0">初期解</param>
    /// <param name="dd0">探索間隔</param>
    /// <param name="f">目的関数</param>
    /// <param name="df">目的関数の勾配</param>
    member this.findmin_ConjugateGradient2 (n:int,m:int) (x0:double1) (dd0:double0) (f:double0->double1->unit) (df:double1->double1->unit) (stepProc:((int0*double1)->unit)option) =
        match stepProc with
          |Some pu ->
                pu (_0,x0)
          |None ->
            ()
        environment.ch.d <| fun a ->
        environment.ch.d <| fun dd0_ ->
            environment.ch.d1 x0.size1 <| fun b ->
            environment.ch.d1 x0.size1 <| fun y ->
            environment.ch.d1 x0.size1 <| fun df1 ->
            environment.ch.d1 x0.size1 <| fun p1 ->
                b.clear()
                environment.iter.num_exit (I n) <| fun (ext,i) ->
                    dd0_ <== dd0
                    environment.ch.d1 x0.size1 <| fun (df0:double1) ->
                        df df0 x0
                        environment.br.branch <| fun r ->
                            r.IF (i.=0) <| fun () ->
                                a <== 0
                            r.EL <| fun () ->
                                p1.foreach <| fun j ->
                                    p1.[j] <== df0.[j] - df1.[j]
                                environment.la.dot (df0,p1) <| fun c1 ->
                                    environment.la.dot (b,p1) <| fun c2 ->
                                        environment.br.if1 (c2.=0.0) <| fun () ->
                                            ext()
                                        a <== c1/c2
                        //勾配を計算
                        y.foreach <| fun i ->
                            y.[i] <== -df0.[i] + a * b.[i]
                        environment.la.norm y <| fun nr ->
                            environment.br.if1 (nr.=0.0) <| fun () ->
                                ext()
                        //勾配方向に最小値を探す
                        environment.ch.d1 x0.size1 <| fun x ->
                            this.findmin m (x0,y) dd0_ f x
                            x0 <== x
                            match stepProc with
                              |Some(pu) ->
                                    pu(i,x0)
                              |None ->
                                ()
                            y.foreach <| fun i ->
                                b.[i] <== y.[i]
                        df1.foreach <| fun i ->
                            df1.[i] <== df0.[i]

    /// <summary>
    /// ニュートン法
    /// </summary>
    /// <param name="n"></param>
    /// <param name="m"></param>
    /// <param name="x0"></param>
    /// <param name="dd0"></param>
    /// <param name="f"></param>
    /// <param name="df"></param>
    /// <param name="fH"></param>
    member this.findmin_Newton (n:int,m:int) (x0:double1) (dd0:double0) (f:double0->double1->unit) (df:double1->double1->unit) (fH:double2->double1->unit) (stepProc:((int0*double1)->unit)option) =
        match stepProc with
          |Some pu ->
                pu (_0,x0)
          |None ->
            ()
        environment.ch.d <| fun dd0_ ->
            environment.iter.num_exit (I n) <| fun (ext,i) ->
                dd0_ <== dd0
                environment.ch.d2 x0.size1 x0.size1 <| fun ih ->
                    environment.ch.d2 x0.size1 x0.size1 <| fun h ->
                        fH h x0
                        environment.la.inverse_matrix (ih, h)
                    environment.ch.d1 x0.size1 <| fun df0 ->
                        df df0 x0
                        environment.la.matmul (ih,df0) <| fun a ->
                            environment.la.norm a <| fun nr ->
                                environment.br.if1 (nr.=0.0) <| fun () -> ext()
                            a.foreach <| fun j ->
                                a.[j] <== -a.[j]
                            this.findmin m (x0,a) dd0_ f x0
                            match stepProc with
                              |Some pu ->
                                    pu(i,x0)
                              |None ->
                                ()

    /// <summary>
    /// 準ニュートン法
    /// </summary>
    /// <param name="n"></param>
    /// <param name="m"></param>
    /// <param name="x0"></param>
    /// <param name="dd0"></param>
    /// <param name="f"></param>
    /// <param name="df"></param>
    member this.findmin_quasiNewton (n:int,m:int) (x0:double1) (dd0:double0) (f:double0->double1->unit) (df:double1->double1->unit) (stepProc:((int0*double1)->unit)option) =
        match stepProc with
          |Some pu ->
                pu (_0,x0)
          |None ->
            ()
        environment.ch.d <| fun dd0_ ->
        environment.ch.d1 x0.size1 <| fun df1 ->
        environment.ch.d1 x0.size1 <| fun y ->
        environment.ch.d1 x0.size1 <| fun s ->
        environment.ch.d2 x0.size1 x0.size1 <| fun B ->
            B.clear()
            environment.iter.num x0.size1 <| fun i ->
                B.[i,i] <== 1.0
            environment.iter.num_exit (I n) <| fun (ext,i) ->
                dd0_ <== dd0
                environment.ch.d1 x0.size1 <| fun df0 ->
                    df df0 x0
                    environment.br.if1 (i.>0) <| fun () ->
                        y.foreach <| fun j -> y.[j] <== df0.[j] - df1.[j]
                        environment.ch.d <| fun p ->
                            p.clear()
                            y.foreach <| fun j -> p <== p + y.[j] * s.[j]
                            environment.ch.d2 x0.size1 x0.size1 <| fun t ->
                                t.clear()
                                environment.iter.num x0.size1 <| fun j -> t.[j,j] <== 1.0
                                t.foreach <| fun (j1,j2) -> t.[j1,j2] <== t.[j1,j2] - y.[j1] * s.[j2] / p
                                environment.ch.d2 x0.size1 x0.size1 <| fun u ->
                                    environment.la.matmul (u,t,B)
                                    environment.la.matmul (B,u,t)
                                    t.foreach <| fun (j1,j2) ->
                                        B.[j1,j2] <== B.[j1,j2] + s.[j1] * s.[j2] / p
                    environment.la.matmul (B,df0) <| fun a ->
                        environment.la.norm a <| fun nr -> environment.br.if1 (nr.=0.0) <| fun () -> ext()
                        a.foreach <| fun i -> a.[i] <== -a.[i]
                        environment.ch.d1 x0.size1 <| fun xx ->
                            this.findmin m (x0,a) dd0_ f xx
                            s.foreach <| fun j -> s.[j] <== xx.[j] - x0.[j]
                            df1 <== df0
                            x0.foreach <| fun i -> x0.[i] <== xx.[i]
                            match stepProc with
                              |Some pu ->
                                    pu(i,x0)
                              |None ->
                                ()

[<AutoOpen>]
module CompilationEnvironmentOptimizationExtensions =
    type CompilationEnvironment with
        member this.optimization = ContextOptimization(this)
