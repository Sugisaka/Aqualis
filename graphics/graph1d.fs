// 
// Copyright (c) 2025 Jun-ichiro Sugisaka
// 
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
namespace gengraphics

open System
open System.IO

///<summary>プロット範囲の指定</summary>
type PlotRange = 
    ///<summary>自動</summary>
    |Auto
    ///<summary>範囲の両端(最小,最大)を指定</summary>
    |MinMax of double*double
///<summary>座標軸の指定</summary>
type Scale =
    ///<summary>線形軸</summary>
    |Linear
    ///<summary>対数軸</summary>
    |Log10
///<summary>座標軸の設定</summary>
type Axis = 
    {
        ///<summary>線形軸または対数軸</summary>
        Scale: Scale;
        ///<summary>プロット範囲</summary>
        Range: PlotRange
        ///<summary>数値のフォーマット</summary>
        NumFormat: string option
    }
///<summary>プロット点の形状</summary>
type PlotShape =
    ///<summary>円</summary>
    |Circle
    ///<summary>正方形</summary>
    |Square 
///<summary>線プロット設定</summary>
type LineStyle =
    {
        ///<summary>線プロットのスタイル</summary>
        Style: color.stroke
    }
///<summary>点プロット設定</summary>
type PointStyle = 
    {
        ///<summary>線プロットのスタイル</summary>
        Shape: PlotShape;
        ///<summary>プロット点のサイズ</summary>
        Size: float;
        ///<summary>プロット点(x,y)に対するプロット点の輪郭スタイル</summary>
        StrokeStyle: (double*double)->color.stroke;
        ///<summary>プロット点(x,y)に対するプロット点の塗りスタイル</summary>
        FillStyle: (double*double)->color.fill;
    }
///<summary>線と点プロット設定</summary>
type LPStyle =
    {
        ///<summary>線のスタイル</summary>
        Lines: LineStyle;
        ///<summary>点のスタイル</summary>
        Points: PointStyle
    }
///<summary>プロットスタイル設定</summary>
type PlotStyle =
    ///<summary>線のみでプロット</summary>
    |Lines of LineStyle
    ///<summary>点のみでプロット</summary>
    |Points of PointStyle
    ///<summary>線と点でプロット</summary>
    |LinesPoints of LPStyle
    ///<summary>点の塗り設定</summary>
    member this.pointFill with get() =
        match this with
        |Lines _ -> fun (x,y) -> color.fill.none
        |Points s -> s.FillStyle
        |LinesPoints s -> s.Points.FillStyle
    ///<summary>点の輪郭設定</summary>
    member this.pointStroke with get() =
        match this with
        |Lines _ -> fun (x,y) -> color.stroke.none
        |Points s -> s.StrokeStyle
        |LinesPoints s -> s.Points.StrokeStyle
    ///<summary>線の輪郭設定</summary>
    member this.lineStroke with get() =
        match this with
        |Lines s -> s.Style
        |Points s -> color.stroke.none
        |LinesPoints s -> s.Lines.Style
///<summary>関数プロット</summary>
type FunctionPlot =
    {
        ///<summary>プロットスタイル</summary>
        Style:PlotStyle;
        ///<summary>凡例</summary>
        Legend: string option;
        ///<summary>サンプリング点数</summary>
        Sampling: int;
        ///<summary>プロット関数</summary>
        Function: double->double
    }
///<summary>データプロット</summary>
type DataFilePlot = 
    {
        ///<summary>プロットスタイル</summary>
        Style:PlotStyle;
        ///<summary>ファイル名</summary>
        FileName: string;
        ///<summary>凡例</summary>
        Legend: string option;
        ///<summary>横軸データ列</summary>
        Xcolumn: (int->double) -> double;
        ///<summary>縦軸データ列</summary>
        Ycolumn: (int->double) -> double;
    }
///<summary>プロットタイプ指定</summary>
type Plot = 
    ///<summary>関数プロット</summary>
    |Function of FunctionPlot
    ///<summary>データプロット</summary>
    |Datafile of DataFilePlot
///<summary>グラフの設定</summary>
type GraphStyle =
    {
        ///<summary>横軸の設定</summary>
        Xaxis: Axis
        ///<summary>縦軸の設定</summary>
        Yaxis: Axis
        ///<summary>横軸ラベル</summary>
        Xlabel: string
        ///<summary>縦軸ラベル</summary>
        Ylabel: string
    }
///<summary>グラフ配置の設定</summary>
type GraphSetting =
    {
        ///<summary>キャンバスサイズ[mm]</summary>
        CanvasSize: float*float
        ///<summary>グラフ配置間隔[mm]</summary>
        Interval: float*float
        ///<summary>グラフ左下の座標[mm]</summary>
        Origin: float*float
        ///<summary>グラフサイズ[mm]</summary>
        GraphSize: float*float
        SubCaptionShiftY: float
        /// サブキャプションとグラフフレーム間のy方向間隔
        SubCaptionTextY: float
        // グラフ枠線のスタイル
        FrameStyle: color.stroke
        // 凡例のグラフ右上角からの距離(x)
        LegendPositionX: float
        // 凡例のグラフ右上角からの距離(y)
        LegendPositionY: float
        // 凡例の縦方向間隔
        LegendGapY: float
        // 凡例の記号とテキストの間隔(x)
        LegendmarkX: float
        // 凡例の記号とテキストの間隔(y)
        LegendmarkY: float
        // 凡例の記号(線)の長さ
        LegendLineLength: float
    }
type Greek =
    static member alpha with get() = "α"
    static member beta with get() = "β"
    static member gamma with get() = "γ"
    static member delta with get() = "δ"
    static member epsilon with get() = "ε"
    static member zeta with get() = "ζ"
    static member eta with get() = "η"
    static member theta with get() = "θ"
    static member iota with get() = "ι"
    static member kappa with get() = "κ"
    static member lambda with get() = "λ"
    static member mu with get() = "μ"
    static member nu with get() = "ν"
    static member xi with get() = "ξ"
    static member omicron with get() = "ο"
    static member pi with get() = "π"
    static member rho with get() = "ρ"
    static member finalSigma with get() = "ς"
    static member sigma with get() = "σ"
    static member tau with get() = "τ"
    static member upsilon with get() = "υ"
    static member phi with get() = "φ"
    static member chi with get() = "χ"
    static member psi with get() = "ψ"
    static member omega with get() = "ω"
    static member Alpha with get() = "Α"
    static member Beta with get() = "Β"
    static member Gamma with get() = "Γ"
    static member Delta with get() = "Δ"
    static member Epsilon with get() = "Ε"
    static member Zeta with get() = "Ζ"
    static member Eta with get() = "Η"
    static member Theta with get() = "Θ"
    static member Iota with get() = "Ι"
    static member Kappa with get() = "Κ"
    static member Lambda with get() = "Λ"
    static member Mu with get() = "Μ"
    static member Nu with get() = "Ν"
    static member Xi with get() = "Ξ"
    static member Omicron with get() = "Ο"
    static member Pi with get() = "Π"
    static member Rho with get() = "Ρ"
    static member Sigma with get() = "Σ"
    static member Tau with get() = "Τ"
    static member Upsilon with get() = "Υ"
    static member Phi with get() = "Φ"
    static member Chi with get() = "Χ"
    static member Psi with get() = "Ψ"
    static member Omega with get() = "Ω"
type TextStyle =
    static member Italic t = "<tspan font-style=\"italic\">"+t+"</tspan>"
    static member Bold t = "<tspan font-weight=\"bold\">"+t+"</tspan>"
    static member Sub t = "<tspan font-size=\"7\" baseline-shift=\"sub\">"+t+"</tspan>"
    static member Sup t = "<tspan font-size=\"7\" baseline-shift=\"super\">"+t+"</tspan>"

type graph1d =
    ///<summary>A4縦2段組ドキュメント内の図：横2枚、縦ny枚配置</summary>
    static member A4PTwoColDouble (ny:int) = {
        CanvasSize=(80.0, (float ny)*43.0);
        Interval=(40.0, 43.0);
        Origin=(-40.0+8.5, -0.5*(float ny)*43.0+7.5);
        GraphSize=(29.64, 0.473*70.0);
        SubCaptionShiftY = 4.0;
        SubCaptionTextY = 10.0;
        FrameStyle = color.stroke.black(0.5);
        LegendPositionX = 15.0;
        LegendPositionY = 3.0;
        LegendGapY = 4.0;
        LegendmarkX = 3.0;
        LegendmarkY = 1.0;
        LegendLineLength = 4.0;
        }
    ///<summary>A4縦2段組ドキュメント内の図：横1枚、縦ny枚配置</summary>
    static member A4PTwoColSingle (ny:int) = {
        CanvasSize=(80.0, (float ny)*43.0);
        Interval=(80.0, 43.0);
        Origin=(-40.0+8.5, -0.5*(float ny)*43.0+7.5);
        GraphSize=(70.00, 0.473*70.0);
        SubCaptionShiftY = 4.0;
        SubCaptionTextY = 10.0;
        FrameStyle = color.stroke.black(0.5);
        LegendPositionX = 15.0;
        LegendPositionY = 3.0;
        LegendGapY = 4.0;
        LegendmarkX = 3.0;
        LegendmarkY = 1.0;
        LegendLineLength = 4.0;
        }
    ///<summary>グラフ生成(ダミー)</summary>
    static member dummy_makeGraph (outputdir:string) (filename:string) (setting:GraphSetting) code = ()
    ///<summary>データファイルの読み込み</summary>
    static member readdata (filename:string) (colx:(int->double)->double,coly:(int->double)->double) =
        //データファイルの行数
        let nline =
            let mutable counter:int = 0
            let r = new StreamReader(filename)
            let mutable t = ""
            t <- r.ReadLine()
            while not(t=null) do
                counter <- counter + 1
                t <- r.ReadLine()
            r.Close()
            counter
        let x = Array.zeroCreate<double> nline
        let y = Array.zeroCreate<double> nline
        let r = new StreamReader(filename)
        let mutable sep = [|' '|]
        for i in 0..(nline-1) do
            let t = r.ReadLine()
            if i=0 then 
                if t.Contains(",") then sep <- [|','|]
                elif t.Contains("\t") then sep <- [|'\t'|]
            let k = t.Split(sep,StringSplitOptions.RemoveEmptyEntries)
            let pd i = 
                if i<0 || i>k.Length then
                    printfn "column index over: %d %d" i k.Length
                    nan
                else
                    let (r,v) = Double.TryParse(k[i-1])
                    if not r then 
                        printfn "Not a number: %s" k[i-1]
                        nan
                    else
                        v
            x[i] <- colx pd
            y[i] <- coly pd
        r.Close()
        x,y
        
    ///<summary>グラフ生成</summary>
    /// <param name="outputdir">出力先ディレクトリ</param>
    /// <param name="filename">出力ファイル名</param>
    /// <param name="setting">グラフプロット設定</param>
    /// <param name="code">グラフのプロット</param>
    static member makeGraph (outputdir:string) (filename:string) (setting:GraphSetting) code =
        let (cLx,cLy) = setting.CanvasSize
        let (dx,dy) = setting.Interval
        let (cx0,cy0) = setting.Origin
        let (gLx,gLy) = setting.GraphSize
        /// mmからptに変換
        let mmtopt(x:double) =
            /// 1mmのポイント値
            let scale = 2.83466798951172844
            scale*x
        printfn "-----------------------------------------"
        printfn "Plot %s" (outputdir+"\\"+filename)
        //SVGファイル生成
        svgfile.make outputdir filename (mmtopt cLx,mmtopt cLy) 1.0 <| fun sv ->
            let addGraph (ix:int,iy:int) (subcaption:option<string>) (gstyle:GraphStyle) (data:list<Plot>) =
                printfn "Subplot: (%d,%d)" ix iy
                let (gLx0,gLy0) = match subcaption with |None -> (gLx,gLy) |Some _ -> (gLx,gLy-setting.SubCaptionShiftY)
                /// データファイルが存在するかチェック
                let rec filecheck (lst:list<Plot>) =
                    match lst with
                    |(Datafile f) :: lst0 ->
                        if File.Exists(outputdir+"\\"+f.FileName) then
                            printfn "source: %s" (outputdir+"\\"+f.FileName)
                            filecheck lst0
                        else
                            printfn "Error: %s not found" (outputdir+"\\"+f.FileName)
                            false
                    |a::lst0 ->
                        filecheck lst0
                    |[] -> true
                if filecheck data then
                    /// グラフの配置座標
                    let (cx,cy) =
                        match subcaption with
                        |None -> 
                            cx0+(double (ix-1))*dx, cy0+(double (iy-1))*dy
                        |Some _ -> 
                            cx0+(double (ix-1))*dx, cy0+(double (iy-1))*dy+setting.SubCaptionShiftY
                    /// 目盛り
                    let setTicInterval(range:double) =
                        let c = 
                            if log10(range)<0.0 then
                                -ceil(-log10(range))
                            else
                                floor(log10(range))
                        let n = int(range*10.0**(-c))
                        if n=1 then
                            0.2*10.0**c
                        elif n<4 then
                            0.5*10.0**c
                        elif n<8 then
                            1.0*10.0**c
                        else
                            2.0*10.0**c
                    /// プロット範囲→目盛り
                    let ticList(axis:Axis,x1:double,x2:double) =
                        match axis.Scale with
                        |Linear ->
                            let dt = setTicInterval(x2-x1)
                            let tmin = if x1<0.0 then -dt*floor(-x1/dt) else dt*ceil(x1/dt)
                            let tmax = if x2<0.0 then -dt*ceil(-x2/dt) else dt*floor(x2/dt)
                            let ntic = 1 + int (floor((tmax-tmin)/dt+0.5))
                            [ for i in 1..ntic -> tmin+dt*double (i-1) ]
                        |Log10 ->
                            let tmin = floor(log10(x1))
                            let tmax = ceil(log10(x2))
                            [ for i in tmin..tmax -> 10.0**(double i) ]
                    /// プロット範囲
                    let (xr1,xr2),(yr1,yr2) =
                        let inrange x = match gstyle.Xaxis.Range with |Auto -> true |MinMax(x1,x2) -> x1<=x && x<=x2
                        /// データ範囲検出
                        let searchRange (data:Plot) =
                            match data with
                            |Function _ -> None,None
                            |Datafile s ->
                                if File.Exists(outputdir+"\\"+s.FileName) then
                                    let (xdata,ydata) = graph1d.readdata (outputdir+"\\"+s.FileName) (s.Xcolumn,s.Ycolumn)
                                    let xr = 
                                        List.fold (fun (acc:option<double*double>) (i:int) -> 
                                            match acc with
                                            |None ->
                                                Some(xdata[i],xdata[i])
                                            |Some(x1,x2) ->
                                                Some((if xdata[i]<x1 then xdata[i] else x1), (if x2<xdata[i] then xdata[i] else x2))
                                        ) None [0..(xdata.Length-1)]
                                    let yr = 
                                        let inrange x = match gstyle.Xaxis.Range with |Auto -> true |MinMax(x1,x2) -> x1<=x && x<=x2
                                        List.fold (fun (acc:option<double*double>) (i:int) ->
                                            match acc with
                                            |None when inrange xdata[i] ->
                                                Some(ydata[i],ydata[i])
                                            |None ->
                                                None
                                            |Some(y1,y2) when inrange xdata[i] ->
                                                Some((if ydata[i]<y1 then ydata[i] else y1), (if y2<ydata[i] then ydata[i] else y2))
                                            |Some(y1,y2) ->
                                                Some(y1,y2)
                                        ) None [0..(ydata.Length-1)]
                                    xr,yr
                                else
                                    None,None
                        /// データリストからプロット範囲検出
                        let mergeRange (datalist:Plot list) =
                            let rec merge lst xrange yrange =
                                match lst with
                                |d::s ->
                                    let xr,yr = searchRange d
                                    merge s
                                    <| match xrange,xr with 
                                        |None,None -> None
                                        |None,Some _ -> xr
                                        |Some _, None -> xrange
                                        |Some(x1,x2),Some(_X1,_X2) -> Some((if _X1<x1 then _X1 else x1), (if x2<_X2 then _X2 else x2)) 
                                    <| match yrange,yr with 
                                        |None,None -> None
                                        |None,Some _ -> yr
                                        |Some _, None -> yrange
                                        |Some(y1,y2),Some(_Y1,_Y2) -> Some((if _Y1<y1 then _Y1 else y1), (if y2<_Y2 then _Y2 else y2)) 
                                |[] ->
                                    xrange,yrange
                            merge datalist None None
                        /// データ範囲から目盛間隔に合わせてグラフ範囲決定
                        let autoRange(axis:Axis,x1:double,x2:double) =
                            match axis.Scale with
                            |Linear ->
                                let dt = setTicInterval(x2-x1)
                                let r1 = if x1<0.0 then -dt*ceil(-x1/dt) else dt*floor(x1/dt)
                                let r2 = if x2<0.0 then -dt*floor(-x2/dt) else dt*ceil(x2/dt)
                                (r1,r2)
                            |Log10 ->
                                if x1<0.0 then printfn "負の値は対数軸にプロットできません"
                                (10.0**floor(log10(x1)),10.0**ceil(log10(x2)))
                        let dataxr,datayr = mergeRange data
                        // データの範囲表示(x)
                        match dataxr with
                        |None ->
                            printfn "Data range (x): none"
                        |Some(_X1,_X2) ->
                            printfn "Data range (x): %e %e" _X1 _X2
                        // データの範囲表示(y)
                        match datayr with
                        |None ->
                            printfn "Data range (y): none"
                        |Some(_Y1,_Y2) ->
                            printfn "Data range (y): %e %e" _Y1 _Y2
                        // 目盛り間隔に合わせて範囲を更新(x)
                        let xr1,xr2 =
                            match gstyle.Xaxis.Range, dataxr with
                            |Auto,None ->
                                autoRange (gstyle.Xaxis,0.0,1.0)
                            |Auto,Some(_X1,_X2) ->
                                autoRange (gstyle.Xaxis,_X1,_X2)
                            |MinMax(x1,x2),_ ->
                                autoRange (gstyle.Xaxis,x1,x2)
                        // 目盛り間隔に合わせて範囲を更新(y)
                        let yr1,yr2 =
                            match gstyle.Yaxis.Range, datayr with
                            |Auto,None ->
                                autoRange (gstyle.Yaxis,0.0,1.0)
                            |Auto,Some(_Y1,_Y2) ->
                                autoRange (gstyle.Yaxis,_Y1,_Y2)
                            |MinMax(y1,y2),_ ->
                                autoRange (gstyle.Yaxis,y1,y2)
                        printfn "Plot range (x): %e %e" xr1 xr2    
                        printfn "Plot range (y): %e %e" yr1 yr2    
                        (xr1,xr2),(yr1,yr2)
                    // データ座標→描画座標[mm]
                    let fx x = 
                        match gstyle.Xaxis.Scale with
                        |Linear ->
                            cx + gLx0*(x-xr1)/(xr2-xr1)
                        |Log10 ->
                            cx + gLx0*(log10(x)-log10(xr1))/(log10(xr2)-log10(xr1))
                    let fy y = 
                        match gstyle.Yaxis.Scale with
                        |Linear ->
                            cy + gLy0*(y-yr1)/(yr2-yr1)
                        |Log10 ->
                            cy + gLy0*(log10(y)-log10(yr1))/(log10(yr2)-log10(yr1))
                    let mline (x1,y1) (x2,y2) =
                        sv.line((mmtopt x1, mmtopt y1), (mmtopt x2, mmtopt y2), setting.FrameStyle)
                    let mtextL (x,y) str =
                        sv.text((mmtopt x, mmtopt y), str.ToString(), 8.0, color.fill.black, color.stroke.none)
                    let mtextC (x,y) str =
                        sv.text((mmtopt x, mmtopt y), str.ToString(), 8.0, TimesNewRoman, TextAnchor.Center, None, color.fill.black, color.stroke.none)
                    let mtextV (x,y) str =
                        sv.text((mmtopt x, mmtopt y), str.ToString(), 8.0, TimesNewRoman, TextAnchor.Center, Some(-90.0), color.fill.black, color.stroke.none)
                    let mtextR (x,y) str =
                        sv.text((mmtopt x, mmtopt y), str.ToString(), 8.0, TimesNewRoman, TextAnchor.Right, None, color.fill.black, color.stroke.none)
                    // フレーム
                    sv.rectangle((mmtopt <| cx+0.5*gLx0, mmtopt <| cy+0.5*gLy0), (mmtopt <| gLx0, mmtopt <| gLy0), color.fill.none, setting.FrameStyle)
                    // サブキャプション
                    match subcaption with |None -> () |Some t -> mtextC (cx+0.5*gLx, fy yr1-setting.SubCaptionTextY) <| t
                    /// x軸目盛り
                    let xtic = ticList (gstyle.Xaxis,xr1,xr2)
                    for x in xtic do 
                        mline (fx x, fy yr1) (fx x, (fy yr1)+1.0)
                        match gstyle.Xaxis.Scale with
                        |Linear ->
                            mtextC (fx x, fy yr1-3.0) <| x.ToString(match gstyle.Xaxis.NumFormat with |None -> "0.0" |Some s -> s)
                        |Log10 ->
                            mtextC (fx x, fy yr1-3.5) <| "10<tspan font-size=\"7\" baseline-shift=\"super\">"+(log10(x)).ToString("F0")+"</tspan>"
                    /// y軸目盛り
                    let ytic = ticList (gstyle.Yaxis,yr1,yr2)
                    for y in ytic do 
                        mline (fx xr1, fy y) ((fx xr1)+1.0, fy y)
                        match gstyle.Yaxis.Scale with
                        |Linear ->
                            mtextR (fx xr1, fy y-1.0) <| y.ToString(match gstyle.Yaxis.NumFormat with |None -> "0.0" |Some s -> s)
                        |Log10 ->
                            mtextR (fx xr1, fy y-1.0) <| "10<tspan font-size=\"7\" baseline-shift=\"super\">"+(log10(y)).ToString("F0")+"</tspan>"
                    // x軸ラベル
                    mtextC (cx+0.5*gLx0, fy yr1-6.0) <| gstyle.Xlabel
                    // y軸ラベル
                    mtextV (fx xr1-6.0, cy+0.5*gLy0) <| gstyle.Ylabel
                    // データプロット
                    let dataplot d (xlegend,ylegend) =
                        match d with
                        |Function s ->
                            let rec xylist lst xy0 i =
                                match xy0 with
                                |_ when i=s.Sampling+1 ->
                                    lst
                                |None ->
                                    let x = (xr1+(xr2-xr1)*(double(i-1))/double (s.Sampling-1))
                                    let y = s.Function x
                                    xylist (lst@[x,y]) (Some(x,y)) (i+1)
                                |Some(x0,y0) ->
                                    let x = (xr1+(xr2-xr1)*(double(i-1))/double (s.Sampling-1))
                                    let y = s.Function x
                                    if y>yr2 then
                                        let xx = (yr2-y0)*(x-x0)/(y-y0)+x0
                                        lst@[xx,yr2]
                                    elif y<yr1 then
                                        let xx = (yr1-y0)*(x-x0)/(y-y0)+x0
                                        lst@[xx,yr2]
                                    else
                                        xylist (lst@[x,y]) (Some(x,y)) (i+1)
                            let datxy = xylist [] None 1
                            sv.polygon(List.map (fun (x,y) -> (mmtopt <| fx x,mmtopt <| fy y)) datxy, color.fill.none, s.Style.lineStroke)
                        |Datafile s ->
                            let (xdata,ydata) = graph1d.readdata (outputdir+"\\"+s.FileName) (s.Xcolumn,s.Ycolumn)
                            /// (x,y)がプロット範囲内にあるか判定
                            let inside (x,y) = (xr1<=x && x<=xr2 && yr1<=y && y<=yr2)
                            // 線のプロット
                            (fun code -> match s.Style with |Lines l -> code(l) |LinesPoints lp -> code(lp.Lines) |_ -> ()) <| fun l ->
                                /// プロット線(x1,y1)→(x2,y2)の間にあるグラフ境界線都の交点を計算
                                let middle (x1,y1) (x2,y2) =
                                    if x1<=xr1 && xr1<=x2 then
                                        xr1,y1+(y2-y1)/(x2-x1)*(xr1-x1)
                                    elif x1<=xr2 && xr2<=x2 then
                                        xr2,y1+(y2-y1)/(x2-x1)*(xr2-x1)
                                    elif y1<=yr1 && yr1<=y2 then
                                        x1+(x2-x1)/(y2-y1)*(yr1-y1),yr1
                                    else
                                        x1+(x2-x1)/(y2-y1)*(yr2-y1),yr2
                                let rec plot (i:int) (lst:list<double*double>) (xy0:(double*double) option) =
                                    match i with
                                    // スキャン終了,未プロットデータなし
                                    |_ when i=xdata.Length && lst.Length=0 ->
                                        ()
                                    // スキャン終了,未プロットデータあり
                                    |_ when i=xdata.Length ->
                                        // 10000点ずつ分けてプロット（ノード数が多すぎる折れ線はIllustratorで表示エラー）
                                        let rec polplot (lstA:list<double*double>) lstB =
                                            match lstB with
                                            |[] ->
                                                sv.polygon(lstA, color.fill.none, l.Style)
                                            |(x,y)::_ when lstA.Length=9999 ->
                                                sv.polygon(lstA@[mmtopt <| fx x,mmtopt <| fy y], color.fill.none, l.Style)
                                                polplot [] lstB
                                            |(x,y)::lst0 ->
                                                polplot (lstA@[mmtopt <| fx x,mmtopt <| fy y]) lst0
                                        polplot [] lst
                                    |_ ->
                                        let x = xdata[i]
                                        let y = ydata[i]
                                        match xy0 with
                                            // 前のプロット点は範囲外で現在のプロット点は範囲内
                                            |Some(x0,y0) when not(inside(x0,y0)) && inside(x,y)  ->
                                                let x1,y1 = middle (x0,y0) (x,y)
                                                plot (i+1) (lst@[(x1,y1);(x,y)]) (Some(x,y))
                                            // 前のプロット点は範囲内で現在のプロット点は範囲外
                                            |Some(x0,y0) when inside(x0,y0) && not(inside(x,y))  ->
                                                let x1,y1 = middle (x0,y0) (x,y)
                                                plot (i+1) (lst@[(x1,y1)]) (Some(x,y))
                                            // 現在のプロット点は範囲内
                                            |_ when inside(x,y) ->
                                                plot (i+1) (lst@[(x,y)]) (Some(x,y))
                                            // 現在のプロット点は範囲外
                                            |_ ->
                                                plot (i+1) lst (Some(x,y))
                                plot 0 [] None
                            // 点のプロット
                            (fun code -> match s.Style with |Points p -> code(p) |LinesPoints lp -> code(lp.Points) |_ -> ()) <| fun p ->
                                sv.group <| fun () ->
                                    for i in 0..xdata.Length-1 do
                                        let x = xdata[i]
                                        let y = ydata[i]
                                        if inside (x,y) then
                                            match p.Shape with
                                            |Circle ->
                                                sv.circle((mmtopt <| fx x,mmtopt <| fy y), p.Size, p.FillStyle (x,y), p.StrokeStyle (x,y))
                                            |Square ->
                                                sv.rectangle((mmtopt <| fx x,mmtopt <| fy y), (p.Size, p.Size), p.FillStyle (x,y), p.StrokeStyle (x,y))
                        //凡例
                        (fun code -> 
                            match d with
                            |Function s ->
                                match s.Legend with
                                |None ->
                                    (xlegend,ylegend)
                                |Some u ->
                                    code(s.Style,u)
                                    (xlegend,ylegend-setting.LegendGapY)
                            |Datafile s ->
                                match s.Legend with
                                |None ->
                                    (xlegend,ylegend)
                                |Some u ->
                                    code(s.Style,u)
                                    (xlegend,ylegend-setting.LegendGapY))
                        <| fun (s,u) ->
                            sv.group <| fun () ->
                                (fun code -> match s with |Lines l -> code(l) |LinesPoints lp -> code(lp.Lines) |_ -> ()) <| fun l ->
                                    sv.line((mmtopt <| xlegend-setting.LegendmarkX-0.5*setting.LegendLineLength, mmtopt <| ylegend+setting.LegendmarkY),(mmtopt <| xlegend-setting.LegendmarkX+0.5*setting.LegendLineLength, mmtopt <| ylegend+setting.LegendmarkY),l.Style)
                                (fun code -> match s with |Points p -> code(p) |LinesPoints lp -> code(lp.Points) |_ -> ()) <| fun p ->
                                    match p.Shape with
                                    |Circle ->
                                        sv.circle((mmtopt <| xlegend-setting.LegendmarkX, mmtopt <| ylegend+setting.LegendmarkY), p.Size, p.FillStyle (0.0,0.0), p.StrokeStyle (0.0,0.0))
                                    |Square ->
                                        sv.rectangle((mmtopt <| xlegend-setting.LegendmarkX, mmtopt <| ylegend+setting.LegendmarkY), (p.Size, p.Size), p.FillStyle (0.0,0.0), p.StrokeStyle (0.0,0.0))
                                mtextL (xlegend,ylegend) u
                    let rec plotall lst (xl,yl) =
                        match lst with
                        |[] -> ()
                        |d::lst0 ->
                            plotall lst0 <| dataplot d (xl,yl)
                    plotall data (fx xr2-setting.LegendPositionX,fy yr2-setting.LegendPositionY)
            code addGraph