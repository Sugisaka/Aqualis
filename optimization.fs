(*
Copyright (c) 2022 Jun-ichiro Sugisaka

This software is released under the MIT License.
http://opensource.org/licenses/mit-license.php
*)
namespace Aqualis

open Aqualis.lapack

type optimization() =
    
    /// <summary>
    /// 1次元極小値検索
    /// </summary>
    /// <param name="m">直線探索の反復数：探索間隔dd*2^(-m)になるまで計算</param>
    /// <param name="x0_">初期解</param>
    /// <param name="df">探索方向</param>
    /// <param name="dd">探索幅</param>
    /// <param name="f">目的関数</param>
    /// <param name="xx">fが極小となるベクトル</param>
    static member findmin (m:int) (x0_:num1,df:num1) (dd:num0) (f:num0->num1->unit) (xx:num1) =
        let r = 0.5*(1.0+sqrt(5.0))
        ch.d1 x0_.size1 <| fun xa ->
        ch.d1 x0_.size1 <| fun x1 ->
        ch.d1 x0_.size1 <| fun x2 ->
        ch.d1 x0_.size1 <| fun xb ->
        ch.dddd <| fun (fa,f1,f2,fb) ->
        ch.d <| fun fa_ ->
        ch.d <| fun norm_df ->
        ch.i <| fun counter ->
            norm_df.clear()
            iter.num df.size1 <| fun i ->
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
            iter.whiledo (counter.<m) <| fun _ ->
                br.branch <| fun b ->
                    b.IF (fa.>f1.>f2.>fb) <| fun () ->
                        counter.decrement()
                        //xa: そのまま
                        x1 <== x2
                        f1 <== f2
                        x2 <== xb
                        f2 <== fb
                        //xb: 新規計算
                        xb.foreach <| fun i -> xb.[i] <== xa.[i] + (x1.[i]-xa.[i])*(1.0+r)
                        f fb xb
                        //print.s [!."A"; counter; (xa.[1]-x0_.[1])/df.[1]; (xb.[1]-x0_.[1])/df.[1]; !."|"; fa; f1; f2; fb]
                    b.IF (And[f1.>f2; fa.>f2;]) <| fun () ->
                        counter.increment()
                        xa <== x1
                        fa <== f1
                        x1 <== x2
                        f1 <== f2
                        //xb: そのまま
                        //x2: 新規計算
                        x2.foreach <| fun i -> x2.[i] <== xa.[i] + (xb.[i]-xa.[i])/r
                        f f2 x2
                        //print.s [!."B"; counter; (xa.[1]-x0_.[1])/df.[1]; (xb.[1]-x0_.[1])/df.[1]; !."|"; fa; f1; f2; fb]
                    b.IF (Or[And[f1.>f2; fa.<f2;]; f1.<=f2;]) <| fun () ->
                        counter.increment()
                        //xa: そのまま
                        xb <== x2
                        fb <== f2
                        x2 <== x1
                        f2 <== f1
                        //x1: 新規計算
                        x1.foreach <| fun i -> x1.[i] <== xa.[i] + (xb.[i]-xa.[i])/(1.0+r)
                        f f1 x1
                        //print.s [!."C"; counter; (xa.[1]-x0_.[1])/df.[1]; (xb.[1]-x0_.[1])/df.[1]; !."|"; fa; f1; f2; fb]
                    b.EL <| fun () ->
                        print.t "error: findmin"
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
    static member findmin_GradientDescent (n:int,m:int) (x0:num1) (dd0:num0) (f:num0->num1->unit) (df:num1->num1->unit) (stepProc:((num0*num1)->unit)option) =
        match stepProc with
          |Some(pu) ->
                pu (_0,x0)
          |None ->
            ()
        ch.d <| fun dd0_ ->
            ch.d1 x0.size1 <| fun y ->
                iter.num_exit (I n) <| fun (ext,i) ->
                    dd0_ <== dd0
                    ch.d1 x0.size1 <| fun (df0:num1) ->
                        df df0 x0
                        //勾配を計算
                        y.foreach <| fun i ->
                            y.[i] <== -df0.[i]
                        La.norm y <| fun nr ->
                            br.if1 (nr.=0.0) <| fun () -> 
                                ext()
                        //勾配方向に最小値を探す
                        optimization.findmin m (x0,y) dd0_ f x0
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
    static member findmin_ConjugateGradient1 (n:int,m:int) (x0:num1) (dd0:num0) (f:num0->num1->unit) (df:num1->num1->unit) (fH:num2->num1->unit) (stepProc:((num0*num1)->unit)option) =
        match stepProc with
          |Some(pu) ->
                pu (_0,x0)
          |None ->
            ()
        ch.d <| fun a ->
        ch.d <| fun dd0_ ->
            ch.d1 x0.size1 <| fun b ->
            ch.d1 x0.size1 <| fun y ->
                b.clear()
                iter.num_exit (I n) <| fun (ext,i) ->
                    dd0_ <== dd0
                    ch.d1 x0.size1 <| fun df0 -> 
                        df df0 x0
                        br.branch <| fun r ->
                            r.IF (i.=1) <| fun () ->
                                a <== 0
                            r.EL <| fun () ->
                                ch.d2 x0.size1 x0.size1 <| fun h ->
                                    fH h x0
                                    La.matmul (h,df0) <| fun p1 ->
                                        La.matmul (h,b) <| fun p2 ->
                                            La.dot (b,p1) <| fun c1 ->
                                                La.dot (b,p2) <| fun c2 ->
                                                    br.if1 (c2.=0.0) <| fun () -> ext()
                                                    a <== c1/c2
                        //勾配を計算
                        y.foreach <| fun i ->
                            y.[i] <== -df0.[i] + a * b.[i]
                        La.norm y <| fun nr ->
                            br.if1 (nr.=0.0) <| fun () -> ext()
                        //勾配方向に最小値を探す
                        optimization.findmin m (x0,y) dd0_ f x0
                        match stepProc with
                          |Some(pu) ->
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
    static member findmin_ConjugateGradient2 (n:int,m:int) (x0:num1) (dd0:num0) (f:num0->num1->unit) (df:num1->num1->unit) (stepProc:((num0*num1)->unit)option) =
        match stepProc with
          |Some(pu) ->
                pu (_0,x0)
          |None ->
            ()
        ch.d <| fun a ->
        ch.d <| fun dd0_ ->
            ch.d1 x0.size1 <| fun b ->
            ch.d1 x0.size1 <| fun y ->
            ch.d1 x0.size1 <| fun df1 ->
            ch.d1 x0.size1 <| fun p1 ->
                b.clear()
                iter.num_exit (I n) <| fun (ext,i) ->
                    dd0_ <== dd0
                    ch.d1 x0.size1 <| fun (df0:num1) ->
                        df df0 x0
                        br.branch <| fun r ->
                            r.IF (i.=1) <| fun () ->
                                a <== 0
                            r.EL <| fun () ->
                                p1.foreach <| fun j ->
                                    p1.[j] <== df0.[j] - df1.[j]
                                La.dot (df0,p1) <| fun c1 ->
                                    La.dot (b,p1) <| fun c2 ->
                                        br.if1 (c2.=0.0) <| fun () ->
                                            ext()
                                        a <== c1/c2
                        //勾配を計算
                        y.foreach <| fun i ->
                            y.[i] <== -df0.[i] + a * b.[i]
                        La.norm y <| fun nr ->
                            br.if1 (nr.=0.0) <| fun () -> 
                                ext()
                        //勾配方向に最小値を探す
                        ch.d1 x0.size1 <| fun x ->
                            optimization.findmin m (x0,y) dd0_ f x
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
    static member findmin_Newton (n:int,m:int) (x0:num1) (dd0:num0) (f:num0->num1->unit) (df:num1->num1->unit) (fH:num2->num1->unit) (stepProc:((num0*num1)->unit)option) =
        match stepProc with
          |Some(pu) ->
                pu (_0,x0)
          |None ->
            ()
        ch.d <| fun dd0_ ->
            iter.num_exit (I n) <| fun (ext,i) ->
                dd0_ <== dd0
                ch.d2 x0.size1 x0.size1 <| fun ih ->
                    ch.d2 x0.size1 x0.size1 <| fun h ->
                        fH h x0
                        inverse_matrix ih h
                    ch.d1 x0.size1 <| fun df0 ->
                        df df0 x0
                        La.matmul (ih,df0) <| fun a ->
                            La.norm a <| fun nr ->
                                br.if1 (nr.=0.0) <| fun () -> ext()
                            a.foreach <| fun j ->
                                a.[j] <== -a.[j]
                            optimization.findmin m (x0,a) dd0_ f x0
                            match stepProc with
                              |Some(pu) ->
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
    static member findmin_quasiNewton (n:int,m:int) (x0:num1) (dd0:num0) (f:num0->num1->unit) (df:num1->num1->unit) (stepProc:((num0*num1)->unit)option) =
        match stepProc with
          |Some(pu) ->
                pu (_0,x0)
          |None ->
            ()
        ch.d <| fun dd0_ ->
        ch.d1 x0.size1 <| fun df1 ->
        ch.d1 x0.size1 <| fun y ->
        ch.d1 x0.size1 <| fun s ->
        ch.d2 x0.size1 x0.size1 <| fun B ->
            B.clear()
            iter.num x0.size1 <| fun i ->
                B.[i,i] <== 1.0
            iter.num_exit (I n) <| fun (ext,i) ->
                dd0_ <== dd0
                ch.d1 x0.size1 <| fun df0 ->
                    df df0 x0
                    br.if1 (i.>1) <| fun () ->
                        y.foreach <| fun j -> y.[j] <== df0.[j] - df1.[j]
                        ch.d <| fun p ->
                            p.clear()
                            y.foreach <| fun j -> p <== p + y.[j] * s.[j]
                            ch.d2 x0.size1 x0.size1 <| fun t ->
                                t.clear()
                                iter.num x0.size1 <| fun j -> t.[j,j] <== 1.0
                                t.foreach <| fun (j1,j2) -> t.[j1,j2] <== t.[j1,j2] - y.[j1] * s.[j2] / p
                                ch.d2 x0.size1 x0.size1 <| fun u ->
                                    La.matmul (u,t,B)
                                    La.matmul (B,u,t)
                                    t.foreach <| fun (j1,j2) -> 
                                        B.[j1,j2] <== B.[j1,j2] + s.[j1] * s.[j2] / p
                    La.matmul (B,df0) <| fun a ->
                        La.norm a <| fun nr -> br.if1 (nr.=0.0) <| fun () -> ext()
                        a.foreach <| fun i -> a.[i] <== -a.[i]
                        ch.d1 x0.size1 <| fun xx ->
                            optimization.findmin m (x0,a) dd0_ f xx
                            s.foreach <| fun j -> s.[j] <== xx.[j] - x0.[j]
                            df1 <== df0
                            x0.foreach <| fun i -> x0.[i] <== xx.[i]
                            match stepProc with
                              |Some(pu) ->
                                    pu(i,x0)
                              |None ->
                                ()
                                