//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

module private OptimizationDefaults =
    [<Literal>]
    let MaxBracketExpansions = 64

type ContextOptimization internal (context:Aqualis) =

    /// <summary>
    /// 1次元極小値検索
    /// </summary>
    /// <param name="m">直線探索の反復数：探索間隔dd*2^(-m)になるまで計算</param>
    /// <param name="x0_">初期解</param>
    /// <param name="df">探索方向</param>
    /// <param name="dd">探索幅</param>
    /// <param name="f">目的関数</param>
    /// <param name="xx">fが極小となるベクトル</param>
    member private this.findminCore (m:int) (maxBracketExpansions:int) (x0_:double1,df:double1) (dd:double0) (f:double0->double1->unit) (xx:double1) =
        if m < 0 then
            invalidArg (nameof m) "The line-search iteration count cannot be negative."
        if maxBracketExpansions < 0 then
            invalidArg (nameof maxBracketExpansions) "The bracket-expansion limit cannot be negative."
        let r = 0.5*(1.0+sqrt(5.0))
        context.ch.d1 x0_.size1 <| fun xa ->
        context.ch.d1 x0_.size1 <| fun x1 ->
        context.ch.d1 x0_.size1 <| fun x2 ->
        context.ch.d1 x0_.size1 <| fun xb ->
        context.ch.dddd <| fun (fa,f1,f2,fb) ->
        context.ch.d <| fun fa_ ->
        context.ch.d <| fun norm_df ->
        context.ch.iii <| fun (counter,expansionCounter,expansionLimitReached) ->
            norm_df.clear()
            context.iter.num df.size1 <| fun i ->
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
            expansionCounter.clear()
            expansionLimitReached.clear()
            fa_ <== fa
            context.iter.whiledo (And [counter.<m; expansionLimitReached.=0]) <| fun _ ->
                context.br.branch <| fun b ->
                    b.IF (fa .> f1 .> f2 .> fb) <| fun () ->
                        context.br.if2 (expansionCounter.<maxBracketExpansions)
                        <| fun () ->
                            expansionCounter.inc()
                            //xa: そのまま
                            x1 <== x2
                            f1 <== f2
                            x2 <== xb
                            f2 <== fb
                            //xb: 新規計算
                            xb.foreach <| fun i -> xb.[i] <== xa.[i] + (x1.[i]-xa.[i])*(1.0+r)
                            f fb xb
                        <| fun () ->
                            expansionLimitReached <== 1
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
                        context.print.s "error: findmin"
            context.br.if2 (expansionLimitReached.=0)
            <| fun () ->
                xx.foreach <| fun i -> xx.[i] <== 0.5*(xa.[i]+xb.[i])
            <| fun () ->
                xx <== xb
                context.print.s "Aqualis: line-search bracket expansion limit reached."

    /// 探索区間の拡張回数に既定の上限を用いて、直線上の極小値を探索します。
    member this.findmin (m:int) (x0_:double1,df:double1) (dd:double0) (f:double0->double1->unit) (xx:double1) =
        this.findminCore m OptimizationDefaults.MaxBracketExpansions (x0_,df) dd f xx

    /// 探索区間の拡張回数に上限を指定して、直線上の極小値を探索します。
    member this.findminWithBracketLimit (m:int,maxBracketExpansions:int) (x0_:double1,df:double1) (dd:double0) (f:double0->double1->unit) (xx:double1) =
        this.findminCore m maxBracketExpansions (x0_,df) dd f xx

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
    member private this.findminGradientDescentCore (n:int,m:int,maxBracketExpansions:int) (x0:double1) (dd0:double0) (f:double0->double1->unit) (df:double1->double1->unit) (stepProc:((int0*double1)->unit)option) =
        match stepProc with
          |Some(pu) ->
                pu (_0,x0)
          |None ->
            ()
        context.ch.d <| fun dd0_ ->
            context.ch.d1 x0.size1 <| fun y ->
                context.iter.num_exit (I n) <| fun (ext,i) ->
                    dd0_ <== dd0
                    context.ch.d1 x0.size1 <| fun (df0:double1) ->
                        df df0 x0
                        //勾配を計算
                        y.foreach <| fun i ->
                            y.[i] <== -df0.[i]
                        context.la.norm y <| fun nr ->
                            context.br.if1 (nr.=0.0) <| fun () ->
                                ext()
                        //勾配方向に最小値を探す
                        this.findminWithBracketLimit (m,maxBracketExpansions) (x0,y) dd0_ f x0
                    match stepProc with
                      |Some(pu) ->
                            pu(i,x0)
                      |None ->
                        ()

    /// 既定の探索区間拡張上限を用いて、最急降下法で極小値を探索します。
    member this.findmin_GradientDescent (n:int,m:int) (x0:double1) (dd0:double0) (f:double0->double1->unit) (df:double1->double1->unit) (stepProc:((int0*double1)->unit)option) =
        this.findminGradientDescentCore (n,m,OptimizationDefaults.MaxBracketExpansions) x0 dd0 f df stepProc

    /// 探索区間の拡張回数に上限を指定して、最急降下法で極小値を探索します。
    member this.findmin_GradientDescentWithBracketLimit (n:int,m:int,maxBracketExpansions:int) (x0:double1) (dd0:double0) (f:double0->double1->unit) (df:double1->double1->unit) (stepProc:((int0*double1)->unit)option) =
        this.findminGradientDescentCore (n,m,maxBracketExpansions) x0 dd0 f df stepProc

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
    member private this.findminConjugateGradient1Core (n:int,m:int,maxBracketExpansions:int) (x0:double1) (dd0:double0) (f:double0->double1->unit) (df:double1->double1->unit) (fH:double2->double1->unit) (stepProc:((int0*double1)->unit)option) =
        match stepProc with
          |Some(pu) ->
                pu (_0,x0)
          |None ->
            ()
        context.ch.d <| fun a ->
        context.ch.d <| fun dd0_ ->
            context.ch.d1 x0.size1 <| fun b ->
            context.ch.d1 x0.size1 <| fun y ->
                b.clear()
                context.iter.num_exit (I n) <| fun (ext,i) ->
                    dd0_ <== dd0
                    context.ch.d1 x0.size1 <| fun df0 ->
                        df df0 x0
                        context.br.branch <| fun r ->
                            r.IF (i.=0) <| fun () ->
                                a <== 0
                            r.EL <| fun () ->
                                context.ch.d2 (x0.size1, x0.size1) <| fun h ->
                                    fH h x0
                                    context.la.matmul (h,df0) <| fun p1 ->
                                        context.la.matmul (h,b) <| fun p2 ->
                                            context.la.dot (b,p1) <| fun c1 ->
                                                context.la.dot (b,p2) <| fun c2 ->
                                                    context.br.if1 (c2.=0.0) <| fun () -> ext()
                                                    a <== c1/c2
                        //勾配を計算
                        y.foreach <| fun i ->
                            y.[i] <== -df0.[i] + a * b.[i]
                        context.la.norm y <| fun nr ->
                            context.br.if1 (nr.=0.0) <| fun () -> ext()
                        //勾配方向に最小値を探す
                        this.findminWithBracketLimit (m,maxBracketExpansions) (x0,y) dd0_ f x0
                        match stepProc with
                          |Some pu ->
                                pu(i,x0)
                          |None ->
                            ()
                        y.foreach <| fun i ->
                            b.[i] <== y.[i]

    /// 既定の探索区間拡張上限を用いて、共役勾配法で極小値を探索します。
    member this.findmin_ConjugateGradient1 (n:int,m:int) (x0:double1) (dd0:double0) (f:double0->double1->unit) (df:double1->double1->unit) (fH:double2->double1->unit) (stepProc:((int0*double1)->unit)option) =
        this.findminConjugateGradient1Core (n,m,OptimizationDefaults.MaxBracketExpansions) x0 dd0 f df fH stepProc

    /// 探索区間の拡張回数に上限を指定して、共役勾配法で極小値を探索します。
    member this.findmin_ConjugateGradient1WithBracketLimit (n:int,m:int,maxBracketExpansions:int) (x0:double1) (dd0:double0) (f:double0->double1->unit) (df:double1->double1->unit) (fH:double2->double1->unit) (stepProc:((int0*double1)->unit)option) =
        this.findminConjugateGradient1Core (n,m,maxBracketExpansions) x0 dd0 f df fH stepProc

    /// <summary>
    /// ヘッセ行列を用いない共役勾配法
    /// </summary>
    /// <param name="n">反復数</param>
    /// <param name="m">直線探索の反復数：探索間隔dd0*2^(-m)になるまで計算</param>
    /// <param name="x0">初期解</param>
    /// <param name="dd0">探索間隔</param>
    /// <param name="f">目的関数</param>
    /// <param name="df">目的関数の勾配</param>
    member private this.findminConjugateGradient2Core (n:int,m:int,maxBracketExpansions:int) (x0:double1) (dd0:double0) (f:double0->double1->unit) (df:double1->double1->unit) (stepProc:((int0*double1)->unit)option) =
        match stepProc with
          |Some pu ->
                pu (_0,x0)
          |None ->
            ()
        context.ch.d <| fun a ->
        context.ch.d <| fun dd0_ ->
            context.ch.d1 x0.size1 <| fun b ->
            context.ch.d1 x0.size1 <| fun y ->
            context.ch.d1 x0.size1 <| fun df1 ->
            context.ch.d1 x0.size1 <| fun p1 ->
                b.clear()
                context.iter.num_exit (I n) <| fun (ext,i) ->
                    dd0_ <== dd0
                    context.ch.d1 x0.size1 <| fun (df0:double1) ->
                        df df0 x0
                        context.br.branch <| fun r ->
                            r.IF (i.=0) <| fun () ->
                                a <== 0
                            r.EL <| fun () ->
                                p1.foreach <| fun j ->
                                    p1.[j] <== df0.[j] - df1.[j]
                                context.la.dot (df0,p1) <| fun c1 ->
                                    context.la.dot (b,p1) <| fun c2 ->
                                        context.br.if1 (c2.=0.0) <| fun () ->
                                            ext()
                                        a <== c1/c2
                        //勾配を計算
                        y.foreach <| fun i ->
                            y.[i] <== -df0.[i] + a * b.[i]
                        context.la.norm y <| fun nr ->
                            context.br.if1 (nr.=0.0) <| fun () ->
                                ext()
                        //勾配方向に最小値を探す
                        context.ch.d1 x0.size1 <| fun x ->
                            this.findminWithBracketLimit (m,maxBracketExpansions) (x0,y) dd0_ f x
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

    /// 既定の探索区間拡張上限を用いて、共役勾配法で極小値を探索します。
    member this.findmin_ConjugateGradient2 (n:int,m:int) (x0:double1) (dd0:double0) (f:double0->double1->unit) (df:double1->double1->unit) (stepProc:((int0*double1)->unit)option) =
        this.findminConjugateGradient2Core (n,m,OptimizationDefaults.MaxBracketExpansions) x0 dd0 f df stepProc

    /// 探索区間の拡張回数に上限を指定して、共役勾配法で極小値を探索します。
    member this.findmin_ConjugateGradient2WithBracketLimit (n:int,m:int,maxBracketExpansions:int) (x0:double1) (dd0:double0) (f:double0->double1->unit) (df:double1->double1->unit) (stepProc:((int0*double1)->unit)option) =
        this.findminConjugateGradient2Core (n,m,maxBracketExpansions) x0 dd0 f df stepProc

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
    member private this.findminNewtonCore (n:int,m:int,maxBracketExpansions:int) (x0:double1) (dd0:double0) (f:double0->double1->unit) (df:double1->double1->unit) (fH:double2->double1->unit) (stepProc:((int0*double1)->unit)option) =
        match stepProc with
          |Some pu ->
                pu (_0,x0)
          |None ->
            ()
        context.ch.d <| fun dd0_ ->
            context.iter.num_exit (I n) <| fun (ext,i) ->
                dd0_ <== dd0
                context.ch.d2 (x0.size1, x0.size1) <| fun ih ->
                    context.ch.d2 (x0.size1, x0.size1) <| fun h ->
                        fH h x0
                        context.la.inverse_matrix (ih, h)
                    context.ch.d1 x0.size1 <| fun df0 ->
                        df df0 x0
                        context.la.matmul (ih,df0) <| fun a ->
                            context.la.norm a <| fun nr ->
                                context.br.if1 (nr.=0.0) <| fun () -> ext()
                            a.foreach <| fun j ->
                                a.[j] <== -a.[j]
                            this.findminWithBracketLimit (m,maxBracketExpansions) (x0,a) dd0_ f x0
                            match stepProc with
                              |Some pu ->
                                    pu(i,x0)
                              |None ->
                                ()

    /// 既定の探索区間拡張上限を用いて、ニュートン法で極小値を探索します。
    member this.findmin_Newton (n:int,m:int) (x0:double1) (dd0:double0) (f:double0->double1->unit) (df:double1->double1->unit) (fH:double2->double1->unit) (stepProc:((int0*double1)->unit)option) =
        this.findminNewtonCore (n,m,OptimizationDefaults.MaxBracketExpansions) x0 dd0 f df fH stepProc

    /// 探索区間の拡張回数に上限を指定して、ニュートン法で極小値を探索します。
    member this.findmin_NewtonWithBracketLimit (n:int,m:int,maxBracketExpansions:int) (x0:double1) (dd0:double0) (f:double0->double1->unit) (df:double1->double1->unit) (fH:double2->double1->unit) (stepProc:((int0*double1)->unit)option) =
        this.findminNewtonCore (n,m,maxBracketExpansions) x0 dd0 f df fH stepProc

    /// <summary>
    /// 準ニュートン法
    /// </summary>
    /// <param name="n"></param>
    /// <param name="m"></param>
    /// <param name="x0"></param>
    /// <param name="dd0"></param>
    /// <param name="f"></param>
    /// <param name="df"></param>
    member private this.findminQuasiNewtonCore (n:int,m:int,maxBracketExpansions:int) (x0:double1) (dd0:double0) (f:double0->double1->unit) (df:double1->double1->unit) (stepProc:((int0*double1)->unit)option) =
        match stepProc with
          |Some pu ->
                pu (_0,x0)
          |None ->
            ()
        context.ch.d <| fun dd0_ ->
        context.ch.d1 x0.size1 <| fun df1 ->
        context.ch.d1 x0.size1 <| fun y ->
        context.ch.d1 x0.size1 <| fun s ->
        context.ch.d2 (x0.size1, x0.size1) <| fun B ->
            B.clear()
            context.iter.num x0.size1 <| fun i ->
                B.[i,i] <== 1.0
            context.iter.num_exit (I n) <| fun (ext,i) ->
                dd0_ <== dd0
                context.ch.d1 x0.size1 <| fun df0 ->
                    df df0 x0
                    context.br.if1 (i.>0) <| fun () ->
                        y.foreach <| fun j -> y.[j] <== df0.[j] - df1.[j]
                        context.ch.d <| fun p ->
                            p.clear()
                            y.foreach <| fun j -> p <== p + y.[j] * s.[j]
                            context.ch.d2 (x0.size1, x0.size1) <| fun t ->
                                t.clear()
                                context.iter.num x0.size1 <| fun j -> t.[j,j] <== 1.0
                                t.foreach <| fun (j1,j2) -> t.[j1,j2] <== t.[j1,j2] - y.[j1] * s.[j2] / p
                                context.ch.d2 (x0.size1, x0.size1) <| fun u ->
                                    context.la.matmul (u,t,B)
                                    context.la.matmul (B,u,t)
                                    t.foreach <| fun (j1,j2) ->
                                        B.[j1,j2] <== B.[j1,j2] + s.[j1] * s.[j2] / p
                    context.la.matmul (B,df0) <| fun a ->
                        context.la.norm a <| fun nr -> context.br.if1 (nr.=0.0) <| fun () -> ext()
                        a.foreach <| fun i -> a.[i] <== -a.[i]
                        context.ch.d1 x0.size1 <| fun xx ->
                            this.findminWithBracketLimit (m,maxBracketExpansions) (x0,a) dd0_ f xx
                            s.foreach <| fun j -> s.[j] <== xx.[j] - x0.[j]
                            df1 <== df0
                            x0.foreach <| fun i -> x0.[i] <== xx.[i]
                            match stepProc with
                              |Some pu ->
                                    pu(i,x0)
                              |None ->
                                ()

    /// 既定の探索区間拡張上限を用いて、準ニュートン法で極小値を探索します。
    member this.findmin_quasiNewton (n:int,m:int) (x0:double1) (dd0:double0) (f:double0->double1->unit) (df:double1->double1->unit) (stepProc:((int0*double1)->unit)option) =
        this.findminQuasiNewtonCore (n,m,OptimizationDefaults.MaxBracketExpansions) x0 dd0 f df stepProc

    /// 探索区間の拡張回数に上限を指定して、準ニュートン法で極小値を探索します。
    member this.findmin_quasiNewtonWithBracketLimit (n:int,m:int,maxBracketExpansions:int) (x0:double1) (dd0:double0) (f:double0->double1->unit) (df:double1->double1->unit) (stepProc:((int0*double1)->unit)option) =
        this.findminQuasiNewtonCore (n,m,maxBracketExpansions) x0 dd0 f df stepProc

[<AutoOpen>]
module CompilationEnvironmentOptimizationExtensions =
    type Aqualis with
        ///<summary>非線形最適化</summary>
        member this.optimization = ContextOptimization(this)
