//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

/// Absolute convergence tolerances. Optional tests are disabled by None.
type OptimizationConvergence =
    { GradientTolerance: float
      StepTolerance: float option
      ObjectiveTolerance: float option }
    /// Default stopping rule: gradient norm at most 1e-8.
    static member Default =
        { GradientTolerance = 1e-8; StepTolerance = None; ObjectiveTolerance = None }

/// Values written to the generated termination variable.
type OptimizationTermination =
    | IterationLimit = 0
    | GradientTolerance = 1
    | StepTolerance = 2
    | ObjectiveTolerance = 3
    | DirectionBreakdown = 4

/// Constants used by optimization routines.
module private OptimizationDefaults =
    /// Maximum number of bracketing expansion steps.
    [<Literal>]
    let MaxBracketExpansions = 64

/// Optimization operations for an Aqualis context.
type ContextOptimization internal (context:Aqualis) =

    /// Shares convergence checks across all higher-level optimizers.
    member private _.iterations (n:int) (options:OptimizationConvergence,termination:int0 option)
        (x:double1) (f:double0->double1->unit) (df:double1->double1->unit) code =
        if n < 0 then invalidArg "n" "The iteration count cannot be negative."
        let validate name value =
            if not (System.Double.IsFinite value) || value < 0.0 then
                invalidArg name "Convergence tolerances must be finite and nonnegative."
        validate "GradientTolerance" options.GradientTolerance
        options.StepTolerance |> Option.iter (validate "StepTolerance")
        options.ObjectiveTolerance |> Option.iter (validate "ObjectiveTolerance")
        let report reason = termination |> Option.iter (fun result -> result <== int reason)
        report OptimizationTermination.IterationLimit
        LapackValidation.require context (x.size1 .<= 0) "Optimization vector length must be positive."
        x.foreach <| fun j ->
            NumericArrayValidation.requireFinite context x[j] "Optimization initial point must be finite."
        let withStepVector code =
            if options.StepTolerance.IsSome then
                context.ch.d1 x.size1 (fun previous -> code (Some previous))
            else code None
        context.ch.d1 x.size1 <| fun gradient ->
        withStepVector <| fun previous ->
        context.ch.dd <| fun (oldValue,newValue) ->
            context.iter.num_exit (I n) <| fun (exit,i) ->
                let stop reason = report reason; exit()
                df gradient x
                gradient.foreach <| fun j ->
                    NumericArrayValidation.requireFinite context gradient[j] "Optimization gradient must be finite."
                context.la.norm gradient <| fun norm ->
                    context.br.if1 (norm .<= options.GradientTolerance) <| fun () ->
                        stop OptimizationTermination.GradientTolerance
                previous |> Option.iter (fun value -> value <== x)
                if options.ObjectiveTolerance.IsSome then
                    f oldValue x
                    NumericArrayValidation.requireFinite context oldValue "Optimization objective must be finite."
                code ((fun () -> stop OptimizationTermination.DirectionBreakdown),i,gradient)
                // Check the final point too, so convergence on the last step is reported.
                context.br.if1 (i .= n-1) <| fun () ->
                    df gradient x
                    gradient.foreach <| fun j ->
                        NumericArrayValidation.requireFinite context gradient[j] "Optimization gradient must be finite."
                    context.la.norm gradient <| fun norm ->
                        context.br.if1 (norm .<= options.GradientTolerance) <| fun () ->
                            stop OptimizationTermination.GradientTolerance
                match options.StepTolerance,previous with
                | Some tolerance,Some delta ->
                    delta.foreach <| fun j -> delta[j] <== x[j]-delta[j]
                    context.la.norm delta <| fun norm ->
                        context.br.if1 (norm .<= tolerance) <| fun () ->
                            stop OptimizationTermination.StepTolerance
                | _ -> ()
                match options.ObjectiveTolerance with
                | Some tolerance ->
                    f newValue x
                    NumericArrayValidation.requireFinite context newValue "Optimization objective must be finite."
                    context.br.if1 (asm.abs(newValue-oldValue) .<= tolerance) <| fun () ->
                        stop OptimizationTermination.ObjectiveTolerance
                | None -> ()

    /// BFGS公式により逆ヘッセ行列の近似を更新します。
    member internal _.updateInverseHessianBfgs (B:double2,y:double1,s:double1,p:double0) =
        context.br.if1 (p.>0.0) <| fun () ->
            context.ch.d2 (B.size1, B.size2) <| fun t ->
                t.clear()
                context.iter.num B.size1 <| fun j -> t.[j,j] <== 1.0
                t.foreach <| fun (j1,j2) -> t.[j1,j2] <== t.[j1,j2] - y.[j1] * s.[j2] / p
                context.ch.d2 (B.size1, B.size2) <| fun u ->
                    context.la.matmul (u,B,t)
                    context.la.matmulTransposeLeft(B,t,u)
                    t.foreach <| fun (j1,j2) ->
                        B.[j1,j2] <== B.[j1,j2] + s.[j1] * s.[j2] / p

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
        let evaluate (value:double0) (point:double1) =
            point.foreach <| fun i -> NumericArrayValidation.requireFinite context point[i] "Line-search point must be finite."
            f value point
            NumericArrayValidation.requireFinite context value "Line-search objective value must be finite."
        LapackValidation.require context (x0_.size1 .<= 0) "Line-search vector length must be positive."
        LapackValidation.require context (df.size1 .=/ x0_.size1) "Line-search direction length must match the initial point."
        LapackValidation.require context (xx.size1 .=/ x0_.size1) "Line-search output length must match the initial point."
        x0_.foreach <| fun i -> NumericArrayValidation.requireFinite context x0_[i] "Line-search initial point must be finite."
        df.foreach <| fun i -> NumericArrayValidation.requireFinite context df[i] "Line-search direction must be finite."
        NumericArrayValidation.requireFinite context dd "Line-search step width must be finite."
        let r = 0.5*(1.0+sqrt(5.0))
        context.ch.d1 x0_.size1 <| fun xa ->
        context.ch.d1 x0_.size1 <| fun x1 ->
        context.ch.d1 x0_.size1 <| fun x2 ->
        context.ch.d1 x0_.size1 <| fun xb ->
        context.ch.dddd <| fun (fa,f1,f2,fb) ->
        context.ch.d <| fun fa_ ->
        context.ch.d <| fun norm_df ->
        context.ch.d1 df.size1 <| fun unitDirection ->
        context.ch.iii <| fun (counter,expansionCounter,expansionLimitReached) ->
            context.la.norm df <| fun stableNorm -> norm_df <== stableNorm
            NumericArrayValidation.requireFinite context norm_df "Line-search direction norm must be finite."
            context.br.if2 (norm_df.>0.0)
            <| fun () ->
                unitDirection <== df
                context.la.normalize unitDirection
                xa.foreach <| fun i -> xa.[i] <== x0_.[i]
                evaluate fa xa
                xb.foreach <| fun i -> xb.[i] <== x0_.[i] + dd * unitDirection.[i]
                evaluate fb xb
                x1.foreach <| fun i -> x1.[i] <== xa.[i] + (xb.[i]-xa.[i])/(1.0+r)
                evaluate f1 x1
                x2.foreach <| fun i -> x2.[i] <== xa.[i] + (xb.[i]-xa.[i])/r
                evaluate f2 x2
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
                                evaluate fb xb
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
                            evaluate f2 x2
                        b.IF (Or [And [f1.>f2; fa.<f2;]; f1.<=f2;]) <| fun () ->
                            counter.inc()
                            //xa: そのまま
                            xb <== x2
                            fb <== f2
                            x2 <== x1
                            f2 <== f1
                            //x1: 新規計算
                            x1.foreach <| fun i -> x1.[i] <== xa.[i] + (xb.[i]-xa.[i])/(1.0+r)
                            evaluate f1 x1
                        b.EL <| fun () ->
                            NumericArrayValidation.fail context "Line search could not advance."
                context.br.if2 (expansionLimitReached.=0)
                <| fun () ->
                    xx.foreach <| fun i ->
                        context.ch.d <| fun span ->
                            context.br.if2 (Or [asm.abs xa.[i] .> 1e300; asm.abs xb.[i] .> 1e300])
                                (fun () ->
                                    context.ch.dd <| fun (halfLeft,halfRight) ->
                                        halfLeft <== 0.5*xa.[i]
                                        halfRight <== 0.5*xb.[i]
                                        xx.[i] <== halfLeft+halfRight)
                                (fun () ->
                                    span <== xb.[i]-xa.[i]
                                    xx.[i] <== xa.[i]+0.5*span)
                <| fun () ->
                    xx <== xb
                    context.print.s "Aqualis: line-search bracket expansion limit reached."
                xx.foreach <| fun i -> NumericArrayValidation.requireFinite context xx[i] "Line-search result must be finite."
            <| fun () ->
                xx <== x0_
                context.print.s "Aqualis: line-search direction is zero; initial point retained."

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
    member private this.findminGradientDescentCore (n:int,m:int,maxBracketExpansions:int) (convergence:OptimizationConvergence,termination:int0 option) (x0:double1) (dd0:double0) (f:double0->double1->unit) (df:double1->double1->unit) (stepProc:((int0*double1)->unit)option) =
        match stepProc with
          |Some(pu) ->
                pu (_0,x0)
          |None ->
            ()
        context.ch.d <| fun dd0_ ->
            context.ch.d1 x0.size1 <| fun y ->
                this.iterations n (convergence,termination) x0 f df <| fun (ext,i,gradient) ->
                    dd0_ <== dd0
                    context.ch.d1 x0.size1 <| fun (df0:double1) ->
                        df0 <== gradient
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
        this.findminGradientDescentCore (n,m,OptimizationDefaults.MaxBracketExpansions) (OptimizationConvergence.Default,None) x0 dd0 f df stepProc

    /// 探索区間の拡張回数に上限を指定して、最急降下法で極小値を探索します。
    member this.findmin_GradientDescentWithBracketLimit (n:int,m:int,maxBracketExpansions:int) (x0:double1) (dd0:double0) (f:double0->double1->unit) (df:double1->double1->unit) (stepProc:((int0*double1)->unit)option) =
        this.findminGradientDescentCore (n,m,maxBracketExpansions) (OptimizationConvergence.Default,None) x0 dd0 f df stepProc

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
    member private this.findminConjugateGradient1Core (n:int,m:int,maxBracketExpansions:int) (convergence:OptimizationConvergence,termination:int0 option) (x0:double1) (dd0:double0) (f:double0->double1->unit) (df:double1->double1->unit) (fH:double2->double1->unit) (stepProc:((int0*double1)->unit)option) =
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
                this.iterations n (convergence,termination) x0 f df <| fun (ext,i,gradient) ->
                    dd0_ <== dd0
                    context.ch.d1 x0.size1 <| fun df0 ->
                        df0 <== gradient
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
        this.findminConjugateGradient1Core (n,m,OptimizationDefaults.MaxBracketExpansions) (OptimizationConvergence.Default,None) x0 dd0 f df fH stepProc

    /// 探索区間の拡張回数に上限を指定して、共役勾配法で極小値を探索します。
    member this.findmin_ConjugateGradient1WithBracketLimit (n:int,m:int,maxBracketExpansions:int) (x0:double1) (dd0:double0) (f:double0->double1->unit) (df:double1->double1->unit) (fH:double2->double1->unit) (stepProc:((int0*double1)->unit)option) =
        this.findminConjugateGradient1Core (n,m,maxBracketExpansions) (OptimizationConvergence.Default,None) x0 dd0 f df fH stepProc

    /// <summary>
    /// ヘッセ行列を用いない共役勾配法
    /// </summary>
    /// <param name="n">反復数</param>
    /// <param name="m">直線探索の反復数：探索間隔dd0*2^(-m)になるまで計算</param>
    /// <param name="x0">初期解</param>
    /// <param name="dd0">探索間隔</param>
    /// <param name="f">目的関数</param>
    /// <param name="df">目的関数の勾配</param>
    member private this.findminConjugateGradient2Core (n:int,m:int,maxBracketExpansions:int) (convergence:OptimizationConvergence,termination:int0 option) (x0:double1) (dd0:double0) (f:double0->double1->unit) (df:double1->double1->unit) (stepProc:((int0*double1)->unit)option) =
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
                this.iterations n (convergence,termination) x0 f df <| fun (ext,i,gradient) ->
                    dd0_ <== dd0
                    context.ch.d1 x0.size1 <| fun (df0:double1) ->
                        df0 <== gradient
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
        this.findminConjugateGradient2Core (n,m,OptimizationDefaults.MaxBracketExpansions) (OptimizationConvergence.Default,None) x0 dd0 f df stepProc

    /// 探索区間の拡張回数に上限を指定して、共役勾配法で極小値を探索します。
    member this.findmin_ConjugateGradient2WithBracketLimit (n:int,m:int,maxBracketExpansions:int) (x0:double1) (dd0:double0) (f:double0->double1->unit) (df:double1->double1->unit) (stepProc:((int0*double1)->unit)option) =
        this.findminConjugateGradient2Core (n,m,maxBracketExpansions) (OptimizationConvergence.Default,None) x0 dd0 f df stepProc

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
    member private this.findminNewtonCore (n:int,m:int,maxBracketExpansions:int) (convergence:OptimizationConvergence,termination:int0 option) (x0:double1) (dd0:double0) (f:double0->double1->unit) (df:double1->double1->unit) (fH:double2->double1->unit) (stepProc:((int0*double1)->unit)option) =
        match stepProc with
          |Some pu ->
                pu (_0,x0)
          |None ->
            ()
        context.ch.d <| fun dd0_ ->
            this.iterations n (convergence,termination) x0 f df <| fun (ext,i,gradient) ->
                dd0_ <== dd0
                context.ch.d2 (x0.size1, x0.size1) <| fun h ->
                    fH h x0
                    context.ch.d1 x0.size1 <| fun a ->
                        a <== gradient
                        a.foreach <| fun j -> a[j] <== -a[j]
                        context.la.solve_simuleq(h,a)
                        context.la.norm a <| fun nr ->
                            context.br.if1 (nr.=0.0) <| fun () -> ext()
                        this.findminWithBracketLimit (m,maxBracketExpansions) (x0,a) dd0_ f x0
                        match stepProc with
                          |Some pu ->
                                pu(i,x0)
                          |None ->
                            ()

    /// 既定の探索区間拡張上限を用いて、ニュートン法で極小値を探索します。
    member this.findmin_Newton (n:int,m:int) (x0:double1) (dd0:double0) (f:double0->double1->unit) (df:double1->double1->unit) (fH:double2->double1->unit) (stepProc:((int0*double1)->unit)option) =
        this.findminNewtonCore (n,m,OptimizationDefaults.MaxBracketExpansions) (OptimizationConvergence.Default,None) x0 dd0 f df fH stepProc

    /// 探索区間の拡張回数に上限を指定して、ニュートン法で極小値を探索します。
    member this.findmin_NewtonWithBracketLimit (n:int,m:int,maxBracketExpansions:int) (x0:double1) (dd0:double0) (f:double0->double1->unit) (df:double1->double1->unit) (fH:double2->double1->unit) (stepProc:((int0*double1)->unit)option) =
        this.findminNewtonCore (n,m,maxBracketExpansions) (OptimizationConvergence.Default,None) x0 dd0 f df fH stepProc

    /// <summary>
    /// 準ニュートン法
    /// </summary>
    /// <param name="n"></param>
    /// <param name="m"></param>
    /// <param name="x0"></param>
    /// <param name="dd0"></param>
    /// <param name="f"></param>
    /// <param name="df"></param>
    member private this.findminQuasiNewtonCore (n:int,m:int,maxBracketExpansions:int) (convergence:OptimizationConvergence,termination:int0 option) (x0:double1) (dd0:double0) (f:double0->double1->unit) (df:double1->double1->unit) (stepProc:((int0*double1)->unit)option) =
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
            this.iterations n (convergence,termination) x0 f df <| fun (ext,i,gradient) ->
                dd0_ <== dd0
                context.ch.d1 x0.size1 <| fun df0 ->
                    df0 <== gradient
                    context.br.if1 (i.>0) <| fun () ->
                        y.foreach <| fun j -> y.[j] <== df0.[j] - df1.[j]
                        context.ch.d <| fun p ->
                            p.clear()
                            y.foreach <| fun j -> p <== p + y.[j] * s.[j]
                            this.updateInverseHessianBfgs(B,y,s,p)
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
        this.findminQuasiNewtonCore (n,m,OptimizationDefaults.MaxBracketExpansions) (OptimizationConvergence.Default,None) x0 dd0 f df stepProc

    /// 探索区間の拡張回数に上限を指定して、準ニュートン法で極小値を探索します。
    member this.findmin_quasiNewtonWithBracketLimit (n:int,m:int,maxBracketExpansions:int) (x0:double1) (dd0:double0) (f:double0->double1->unit) (df:double1->double1->unit) (stepProc:((int0*double1)->unit)option) =
        this.findminQuasiNewtonCore (n,m,maxBracketExpansions) (OptimizationConvergence.Default,None) x0 dd0 f df stepProc

    /// Minimizes with absolute convergence tolerances and a generated termination code.
    member this.findmin_GradientDescentWithConvergence (n:int,m:int,maxBracketExpansions:int) (options:OptimizationConvergence,termination:int0) (x0:double1) (dd0:double0) (f:double0->double1->unit) (df:double1->double1->unit) (stepProc:((int0*double1)->unit)option) =
        this.findminGradientDescentCore (n,m,maxBracketExpansions) (options,Some termination) x0 dd0 f df stepProc

    /// Minimizes with absolute convergence tolerances and a generated termination code.
    member this.findmin_ConjugateGradient1WithConvergence (n:int,m:int,maxBracketExpansions:int) (options:OptimizationConvergence,termination:int0) (x0:double1) (dd0:double0) (f:double0->double1->unit) (df:double1->double1->unit) (fH:double2->double1->unit) (stepProc:((int0*double1)->unit)option) =
        this.findminConjugateGradient1Core (n,m,maxBracketExpansions) (options,Some termination) x0 dd0 f df fH stepProc

    /// Minimizes with absolute convergence tolerances and a generated termination code.
    member this.findmin_ConjugateGradient2WithConvergence (n:int,m:int,maxBracketExpansions:int) (options:OptimizationConvergence,termination:int0) (x0:double1) (dd0:double0) (f:double0->double1->unit) (df:double1->double1->unit) (stepProc:((int0*double1)->unit)option) =
        this.findminConjugateGradient2Core (n,m,maxBracketExpansions) (options,Some termination) x0 dd0 f df stepProc

    /// Solves H p = -g directly, with convergence tolerances and a termination code.
    member this.findmin_NewtonWithConvergence (n:int,m:int,maxBracketExpansions:int) (options:OptimizationConvergence,termination:int0) (x0:double1) (dd0:double0) (f:double0->double1->unit) (df:double1->double1->unit) (fH:double2->double1->unit) (stepProc:((int0*double1)->unit)option) =
        this.findminNewtonCore (n,m,maxBracketExpansions) (options,Some termination) x0 dd0 f df fH stepProc

    /// Minimizes with absolute convergence tolerances and a generated termination code.
    member this.findmin_quasiNewtonWithConvergence (n:int,m:int,maxBracketExpansions:int) (options:OptimizationConvergence,termination:int0) (x0:double1) (dd0:double0) (f:double0->double1->unit) (df:double1->double1->unit) (stepProc:((int0*double1)->unit)option) =
        this.findminQuasiNewtonCore (n,m,maxBracketExpansions) (options,Some termination) x0 dd0 f df stepProc

/// Adds optimization operations to Aqualis.
[<AutoOpen>]
module CompilationEnvironmentOptimizationExtensions =
    type Aqualis with
        ///<summary>非線形最適化</summary>
        member this.optimization = ContextOptimization(this)
