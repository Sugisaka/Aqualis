// 
// Copyright (c) 2025 Jun-ichiro Sugisaka
// 
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
namespace gengraphics

open System

/// グラフの描画領域、サイズ、マージンを設定
type Layout = 
    {
        /// 描画領域サイズ[x]
        sizeX: float;
        /// 描画領域サイズ[y]
        sizeY: float;
        /// 描画領域左側余白
        marginXl: float;
        /// 描画領域右側余白
        marginXr: float;
        /// 描画領域下側余白
        marginYb: float;
        /// 描画領域上側余白
        marginYt: float;
        /// グラフ間余白[x]
        dX: float;
        /// グラフ間余白[y]
        dY: float;
        /// 描画対象の象限以外のサイズ[x]
        mX: float;
        /// 描画対象の象限以外のサイズ[y]
        mY: float;
    }

/// 描画対象の象限を指定
type Quadrant =
    /// 第1,2,3,4象限(-1≦x≦1,-1≦y≦1)
    | Center
    /// 第2,3象限(-1≦x≦0,-1≦y≦1)
    | Left
    /// 第1,4象限(0≦x≦1,-1≦y≦1)
    | Right
    /// 第1,2象限(-1≦x≦1,-1≦y≦0)
    | Lower
    /// 第3,4象限(-1≦x≦1,0≦y≦1)
    | Upper
    /// 第2象限(0≦x≦1,0≦y≦1)
    | LeftUpper
    /// 第3象限(-1≦x≦0,-1≦y≦0)
    | LeftLower
    /// 第1象限(0≦x≦1,0≦y≦1)
    | RightUpper
    /// 第4象限(0≦x≦1,-1≦y≦0)
    | RightLower
    
    
/// 図形を描画
type drawGraph(sv:svgfilemaker, q:Quadrant, x1,y1,x2,y2, marginX,marginY) =
    
    let (cx,cy,X1_,Y1_,X2_,Y2_) = 
        match q with
        | Center ->
            let cx = 0.5*(x1+x2)
            let cy = 0.5*(y1+y2)
            let X1 = -1.0
            let Y1 = -1.0
            let X2 =  1.0
            let Y2 =  1.0
            (cx,cy,X1,Y1,X2,Y2)
        | Left ->
            let cx = x2-marginX
            let cy = 0.5*(y1+y2)
            let X1 = -1.0
            let Y1 = -1.0
            let X2 = -(x2-cx)/(x1-cx)
            let Y2 = 1.0
            (cx,cy,X1,Y1,X2,Y2)
        | Right ->
            let cx = x1+marginX
            let cy = 0.5*(y1+y2)
            let X1 = (x1-cx)/(x2-cx)
            let Y1 = -1.0
            let X2 = 1.0
            let Y2 = 1.0
            (cx,cy,X1,Y1,X2,Y2)
        | Lower ->
            let cx = 0.5*(x1+x2)
            let cy = y2-marginY
            let X1 = -1.0
            let Y1 = -1.0
            let X2 = 1.0
            let Y2 = -(y2-cy)/(y1-cy)
            (cx,cy,X1,Y1,X2,Y2)
        | Upper ->
            let cx = 0.5*(x1+x2)
            let cy = y1+marginY
            let X1 = -1.0
            let Y1 = (y1-cy)/(y2-cy)
            let X2 = 1.0
            let Y2 = 1.0
            (cx,cy,X1,Y1,X2,Y2)
        | LeftUpper ->
            let cx = x2-marginX
            let cy = y2-marginY
            let X1 = -1.0
            let Y1 = (y1-cy)/(y2-cy)
            let X2 = -(x2-cx)/(x1-cx)
            let Y2 = 1.0
            (cx,cy,X1,Y1,X2,Y2)
        | LeftLower ->
            let cx = x2-marginX
            let cy = y1+marginY
            let X1 = -1.0
            let Y1 = -1.0
            let X2 = -(x2-cx)/(x1-cx)
            let Y2 = -(y2-cy)/(y1-cy)
            (cx,cy,X1,Y1,X2,Y2)
        | RightUpper ->
            let cx = x1+marginX
            let cy = y1+marginY
            let X1 = (x1-cx)/(x2-cx)
            let Y1 = (y1-cy)/(y2-cy)
            let X2 = 1.0
            let Y2 = 1.0
            (cx,cy,X1,Y1,X2,Y2)
        | RightLower ->
            let cx = x1+marginX
            let cy = y2-marginY
            let X1 = (x1-cx)/(x2-cx)
            let Y1 = -1.0
            let X2 = 1.0
            let Y2 = -(y2-cy)/(y1-cy)
            (cx,cy,X1,Y1,X2,Y2)
            
    /// グラフ内ローカル座標系から描画領域グローバル座標系へ変換
    let transX x = 
        x1+(x2-x1)*(x-X1_)/(X2_-X1_)
        
    /// グラフ内ローカル座標系から描画領域グローバル座標系へ変換
    let transY y = 
        y1+(y2-y1)*(y-Y1_)/(Y2_-Y1_)
        
    /// グラフ内ローカル座標系から描画領域グローバル座標系へ変換
    let trans (x,y) = transX x, transY y
    
    /// 描画範囲(グラフ内ローカル座標系)
    member this.X1 with get() = X1_
    /// 描画範囲(グラフ内ローカル座標系)
    member this.Y1 with get() = Y1_
    /// 描画範囲(グラフ内ローカル座標系)
    member this.X2 with get() = X2_
    /// 描画範囲(グラフ内ローカル座標系)
    member this.Y2 with get() = Y2_
    
    member this.svg with get() = sv
    
    /// 矢印を描画[(x1,x1):始点,(x2,y2):終点]
    member this.arrow ((x1:double,y1:double),(x2:double,y2:double),col:color.stroke) =
        let (x1_,y1_) = trans (x1,y1)
        let (x2_,y2_) = trans (x2,y2)
        let t0 = atan2 (y2-y1) (x2-x1)
        let arrowsize = 5.0
        let (x2__,y2__) = x2_-0.9*arrowsize*cos(t0),y2_-0.9*arrowsize*sin(t0)
        sv.line((x1_,y1_),(x2__,y2__),col)
        let t1 = t0+Math.PI-15.0*Math.PI/180.0
        let Xa = x2_+arrowsize*cos(t1)
        let Ya = y2_+arrowsize*sin(t1)
        let t2 = t0+Math.PI+15.0*Math.PI/180.0
        let Xb = x2_+arrowsize*cos(t2)
        let Yb = y2_+arrowsize*sin(t2)
        sv.polygon(([x2_;Xa;Xb],[y2_;Ya;Yb]),col.tofill,color.stroke.none)
        
    /// 点を描画
    member this.point (x:double,y:double) (size:double,fcol:color.fill,scol:color.stroke) =
        if X1_<=x && x<=X2_ && Y1_<=y && y<=Y2_ then
            let (x,y) = trans (x,y)
            sv.circle((x,y),size,fcol,scol)
            
    /// 線分を描画[(x1,x1):始点,(x2,y2):終点]
    member this.line (x1:double,y1:double) (x2:double,y2:double) (scol:color.stroke) =
        let (x1,y1) = trans (x1,y1)
        let (x2,y2) = trans (x2,y2)
        sv.line((x1,y1),(x2,y2),scol)
        
    /// 折れ線を描画[plist:頂点リスト]
    member this.polygon (plist:(double*double)list) (scol:color.stroke) =
        let xlist = plist |> List.map (fun (x,y) -> transX x)
        let ylist = plist |> List.map (fun (x,y) -> transY y)
        sv.polygon((xlist,ylist),color.fill.none,scol)
        
    /// テキストを描画
    member this.text (x:double,y:double) (txt:string) (size:double) (col:color.fill) =
        let (x,y) = trans (x,y)
        sv.text((x,y),txt,size,col,color.stroke.none)
        
    /// 数字テキストを描画
    member this.numtext (x:double,y:double) (txt:string) (size:double) (col:color.fill) =
        let (x,y) = trans (x,y)
        let w = 
            txt.ToCharArray() 
            |> Array.fold (fun acc c -> match c with |'.' -> acc+0.882 |'-' -> acc+1.990 |_ -> acc+1.764) 0.0
            |> (fun x -> x*size/10.0/0.353)
        let h = 2.434*size/10.0/0.353
        sv.text((x-0.5*w-0.88*size/10.0/0.353,y-0.5*h),txt.Replace("-","\u2212"),size,col,color.stroke.none)
        
    /// 横軸の目盛りを描画
    member this.xtic (x:double) =
        let (x,y) = trans (x,0.0)
        sv.line((x,y),(x,y+0.75),color.stroke.black(0.5))
        
    /// 縦軸の目盛りを描画
    member this.ytic (y:double) =
        let (x,y) = trans (0.0,y)
        sv.line((x,y),(x+0.75,y),color.stroke.black(0.5))
        
    /// 横軸の目盛りと数字を描画
    member this.xntic (x:double,size:double) =
        this.xtic x
        let (x1,y1) = trans (x,0.0)
        let w = 
            x.ToString().ToCharArray() 
            |> Array.fold (fun acc c -> match c with |'.' -> acc+0.882 |'-' -> acc+1.990 |_ -> acc+1.764) 0.0
            |> (fun x -> x*size/10.0/0.353)
        let h = 2.434*size/10.0/0.353
        sv.text((x1-0.5*w-0.88*size/10.0/0.353,y1-h-0.5*1.764*size/10.0/0.353),x.ToString().Replace("-","\u2212"),size,color.fill.black,color.stroke.none)
        
    /// 縦軸の目盛りと数字を描画
    member this.yntic (y:double,size:double) =
        this.ytic y
        let (x1,y1) = trans (0.0,y)
        let w = 
            y.ToString().ToCharArray() 
            |> Array.fold (fun acc c -> match c with |'.' -> acc+0.882 |'-' -> acc+1.990 |_ -> acc+1.764) 0.0
            |> (fun x -> x*size/10.0/0.353)
        let h = 2.434*size/10.0/0.353
        sv.text((x1-w-0.5*1.764*size/10.0/0.353-0.88*size/10.0/0.353,y1-0.5*h),y.ToString().Replace("-","\u2212"),size,color.fill.black,color.stroke.none)
        
    /// 横軸の目盛りと数字を描画(目盛り位置と数字が異なる場合)
    member this.xntic (x:double,num:double,size:double) =
        this.xtic x
        let (x1,y1) = trans (x,0.0)
        let w = 
            num.ToString().ToCharArray() 
            |> Array.fold (fun acc c -> match c with |'.' -> acc+0.882 |'-' -> acc+1.990 |_ -> acc+1.764) 0.0
            |> (fun x -> x*size/10.0/0.353)
        let h = 2.434*size/10.0/0.353
        sv.text((x1-0.5*w-0.88*size/10.0/0.353,y1-h-0.5*1.764*size/10.0/0.353),num.ToString().Replace("-","\u2212"),size,color.fill.black,color.stroke.none)
        
    /// 縦軸の目盛りと数字を描画(目盛り位置と数字が異なる場合)
    member this.yntic (y:double,num:double,size:double) =
        this.ytic y
        let (x1,y1) = trans (0.0,y)
        let w = 
            num.ToString().ToCharArray() 
            |> Array.fold (fun acc c -> match c with |'.' -> acc+0.882 |'-' -> acc+1.990 |_ -> acc+1.764) 0.0
            |> (fun x -> x*size/10.0/0.353)
        let h = 2.434*size/10.0/0.353
        sv.text((x1-w-0.5*1.764*size/10.0/0.353-0.88*size/10.0/0.353,y1-0.5*h),num.ToString().Replace("-","\u2212"),size,color.fill.black,color.stroke.none)
        
    /// 横軸の目盛りと数字を描画
    member this.xntic (x:double,size:double,format:string) =
        this.xtic x
        let (x1,y1) = trans (x,0.0)
        let w = 
            x.ToString(format).ToCharArray() 
            |> Array.fold (fun acc c -> match c with |'.' -> acc+0.882 |'-' -> acc+1.990 |_ -> acc+1.764) 0.0
            |> (fun x -> x*size/10.0/0.353)
        let h = 2.434*size/10.0/0.353
        sv.text((x1-0.5*w-0.88*size/10.0/0.353,y1-h-0.5*1.764*size/10.0/0.353),x.ToString(format).Replace("-","\u2212"),size,color.fill.black,color.stroke.none)
        
    /// 縦軸の目盛りと数字を描画
    member this.yntic (y:double,size:double,format:string) =
        this.ytic y
        let (x1,y1) = trans (0.0,y)
        let w = 
            y.ToString(format).ToCharArray() 
            |> Array.fold (fun acc c -> match c with |'.' -> acc+0.882 |'-' -> acc+1.990 |_ -> acc+1.764) 0.0
            |> (fun x -> x*size/10.0/0.353)
        let h = 2.434*size/10.0/0.353
        sv.text((x1-w-0.5*1.764*size/10.0/0.353-0.88*size/10.0/0.353,y1-0.5*h),y.ToString(format).Replace("-","\u2212"),size,color.fill.black,color.stroke.none)
        
    /// 横軸の目盛りと数字を描画(目盛り位置と数字が異なる場合)
    member this.xntic (x:double,num:double,size:double,format:string) =
        this.xtic x
        let (x1,y1) = trans (x,0.0)
        let w = 
            x.ToString(format).ToCharArray() 
            |> Array.fold (fun acc c -> match c with |'.' -> acc+0.882 |'-' -> acc+1.990 |_ -> acc+1.764) 0.0
            |> (fun x -> x*size/10.0/0.353)
        let h = 2.434*size/10.0/0.353
        sv.text((x1-0.5*w-0.88*size/10.0/0.353,y1-h-0.5*1.764*size/10.0/0.353),x.ToString(format).Replace("-","\u2212"),size,color.fill.black,color.stroke.none)
        
    /// 縦軸の目盛りと数字を描画(目盛り位置と数字が異なる場合)
    member this.yntic (y:double,num:double,size:double,format:string) =
        this.ytic y
        let (x1,y1) = trans (0.0,y)
        let w = 
            num.ToString(format).ToCharArray() 
            |> Array.fold (fun acc c -> match c with |'.' -> acc+0.882 |'-' -> acc+1.990 |_ -> acc+1.764) 0.0
            |> (fun x -> x*size/10.0/0.353)
        let h = 2.434*size/10.0/0.353
        sv.text((x1-w-0.5*1.764*size/10.0/0.353-0.88*size/10.0/0.353,y1-0.5*h),num.ToString(format).Replace("-","\u2212"),size,color.fill.black,color.stroke.none)
        
    /// 関数を描画[y=f(x):関数]
    member this.func (N:int) (scol:color.stroke) (f:double->double) =
        let xlist,ylist = 
            let xx = 
                [0..N]
                |> List.map (fun i -> this.X1+(this.X2-this.X1)*(double i)/(double N))
            let yy = 
                xx
                |> List.map (fun x -> f x)
            xx |> List.map (fun x -> transX x), yy |> List.map (fun y -> transY y)
        sv.polygon((xlist,ylist),color.fill.none,scol)
        
/// 指定した象限にグラフ描画
type range2D(sv:svgfilemaker, x1, x2, y1, y2, mX, mY) =
    
    /// 指定した象限にグラフ描画
    member this.plot (q:Quadrant) code =
        let d = drawGraph(sv,q,x1,y1,x2,y2, mX, mY)
        d.arrow ((d.X1,0.0),(d.X2,0.0),color.stroke.black(0.5))
        d.arrow ((0.0,d.Y1),(0.0,d.Y2),color.stroke.black(0.5))
        code d
        
module graph =
    
    /// 横150mm
    let layoutA4(height:double) = {sizeX=425.155; sizeY=height*2.8343625; marginXl=10.0; marginXr=10.0; marginYb=10.0; marginYt=10.0; dX=10.0; dY=10.0; mX=10.0; mY=10.0}
    
    /// 横80mm
    let layoutA4_twocol(height:double) = {sizeX=226.749; sizeY=height*2.8343625; marginXl=10.0; marginXr=10.0; marginYb=10.0; marginYt=10.0; dX=10.0; dY=10.0; mX=10.0; mY=10.0}
    
    /// 指定したレイアウトでグラフ作成
    let make (L:Layout) (outputdir:string) filename (nX:int,nY:int) code =
        gengraphics.svgfile.make outputdir filename (L.sizeX,L.sizeY) 1.0 <| fun sv ->
            //グラフの描画領域
            let sx = (L.sizeX-double(nX-1)*L.dX-L.marginXl-L.marginXr)/(double nX)
            let sy = (L.sizeY-double(nY-1)*L.dY-L.marginYb-L.marginYt)/(double nY)
            let region =
                Array2D.init nX nY <| fun i j ->
                    let x1 = -0.5*L.sizeX+L.marginXl+double(i)*(sx+L.dX)
                    let x2 = -0.5*L.sizeX+L.marginXl+double(i)*(sx+L.dX)+sx
                    let y1 = -0.5*L.sizeY+L.marginYb+double(j)*(sy+L.dY)
                    let y2 = -0.5*L.sizeY+L.marginYb+double(j)*(sy+L.dY)+sy
                    range2D(sv,x1,x2,y1,y2,L.mX,L.mY)
            code region
