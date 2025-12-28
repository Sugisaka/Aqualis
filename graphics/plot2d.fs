// 
// Copyright (c) 2025 Jun-ichiro Sugisaka
// 
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
namespace gengraphics
    
    open System
    open System.IO
    
    ///<summary>プロット範囲の指定</summary>
    type PlotColorRange = 
        ///<summary>自動</summary>
        |Auto
        ///<summary>絶対値の最大値を自動指定</summary>
        |AbsAuto
        ///<summary>範囲の両端(最小,最大)を指定</summary>
        |MinMax of double*double
        ///<summary>絶対値の最大値を指定</summary>
        |AbsMax of double
        
    /// データ値とカラーの対応関係
    type Gradation =
        /// (データ値,最小値,最大値)→(赤,緑,青)
        |Rx of ((double*double*double)->(double*double*double))
        /// (データ値実部,データ値虚部,最小値,最大値)→(赤,緑,青)
        |Cx of ((double*double*double*double)->(double*double*double))
        
    module colorMap  =
        /// 黒→白
        let Gray = Rx(
            fun (x,min,max) -> 
                if x<min then
                    0.0,0.0,0.0
                elif x>max then
                    1.0,1.0,1.0
                else
                    (x-min)/(max-min),(x-min)/(max-min),(x-min)/(max-min))
        /// 黒→白(指定範囲未満は青、超過は赤)
        let Overflow = Rx(
            fun (x,min,max) -> 
                if x<min then
                    0.0,0.0,1.0
                elif x>max then
                    1.0,0.0,0.0
                else
                    (x-min)/(max-min),(x-min)/(max-min),(x-min)/(max-min))
        /// 黒→赤→黄→白
        let RedPower = Rx(
            fun (x,min,max) -> 
                let c=(x-min)/(max-min)
                if c<=0.0 then
                    0.0,0.0,0.0 
                elif c<=1.0/3.0 then 
                    3.0*c,0.0,0.0 
                elif c<=2.0/3.0 then 
                    1.0,3.0*(c-1.0/3.0),0.0 
                elif c<=1.0 then 
                    1.0,1.0,3.0*(c-2.0/3.0) 
                else 
                    1.0,1.0,1.0)
        /// 青→白(0)→赤
        let Wave = Rx(
            fun (x,min,max) -> 
                if x>max then
                    1.0,0.0,0.0
                elif x<min then
                    0.0,0.0,1.0
                elif x>0.0 then 
                    1.0,1.0-x/max,1.0-x/max
                else 
                    1.0-x/min,1.0-x/min,1.0)
        /// 複素数プロット用（絶対値最大値：カラー）
        let ComplexVivid = Cx(
            fun (re,im,min,max) -> 
                let p = 
                    let u = atan2 im re
                    (if u < 0.0 then u + 2.0*Math.PI else u)/(2.0*Math.PI)
                let a0 = sqrt(re*re+im*im)
                let a = if a0 < min then 0.0 elif a0 > max then 1.0 else (a0-min)/(max-min)
                let d = 1.0/6.0
                let r,g,b =
                    if p <= 1.0*d then
                        1.0,p/d,0.0
                    elif p <= 2.0*d then
                        1.0-(p-d)/d,1.0,0.0
                    elif p <= 3.0*d then
                        0.0,1.0,(p-2.0*d)/d
                    elif p <= 4.0*d then
                        0.0,1.0-(p-3.0*d)/d,1.0
                    elif p <= 5.0*d then
                        (p-4.0*d)/d,0.0,1.0
                    elif p <= 6.0*d then
                        1.0,0.0,1.0-(p-5.0*d)/d
                    else
                        0.0,0.0,0.0
                a*r,a*g,a*b)
        /// 複素数プロット用（絶対値最大値：白）
        let ComplexLight = Cx(
            fun (re,im,min,max) -> 
                let p = 
                    let u = atan2 im re
                    (if u < 0.0 then u + 2.0*Math.PI else u)/(2.0*Math.PI)
                let a0 = sqrt(re*re+im*im)
                let a = if a0 < min then 0.0 elif a0 > max then 1.0 else (a0-min)/(max-min)
                let d = 1.0/6.0
                let r,g,b =
                    if p <= 1.0*d then
                        1.0,p/d,0.0
                    elif p <= 2.0*d then
                        1.0-(p-d)/d,1.0,0.0
                    elif p <= 3.0*d then
                        0.0,1.0,(p-2.0*d)/d
                    elif p <= 4.0*d then
                        0.0,1.0-(p-3.0*d)/d,1.0
                    elif p <= 5.0*d then
                        (p-4.0*d)/d,0.0,1.0
                    elif p <= 6.0*d then
                        1.0,0.0,1.0-(p-5.0*d)/d
                    else
                        0.0,0.0,0.0
                if a<0.5 then
                    r*a/0.5, g*a/0.5, b*a/0.5
                else
                    r+(1.0-r)*(a-0.5)/0.5, g+(1.0-g)*(a-0.5)/0.5, b+(1.0-b)*(a-0.5)/0.5)
                    
    type plot2d() =
        
        let mutable data : array<double> = [||]
        let mutable isCPXdata : bool = false
        let mutable isDataLoaded : bool = false
        let mutable nx : int = 0
        let mutable ny : int = 0
        let mutable min_ : double = 0.0
        let mutable max_ : double = 0.0
        let mutable error : string = ""
        
        /// データのx方向サンプリング点数
        member _.Nx with get() = nx
        
        /// データのy方向サンプリング点数
        member _.Ny with get() = ny
        
        /// 最小値
        member _.Min with get() = min_
        
        /// 最大値
        member _.Max with get() = max_
        
        /// エラーメッセージ
        member _.Error with get() = error
        
        /// <summary>
        /// データ内の最小値と最大値を計算
        /// </summary>
        /// <param name="f">複素数→プロット値の関数</param>
        member private _.MinMax(f:(double*double)->double) =
            if isCPXdata then
                let mutable min = f(data[0],data[1])
                let mutable max = f(data[0],data[1])
                for i = 0 to nx-1 do
                    for j = 0 to ny-1 do
                        let zre = data[2*(nx*j+i)]
                        let zim = data[2*(nx*j+i)+1]
                        let v = f(zre,zim)
                        if max < v then max <- v
                        if min > v then min <- v
                min,max
            else
                let mutable min = f(data[0],0.0)
                let mutable max = f(data[0],0.0)
                for i = 0 to nx-1 do
                    for j = 0 to ny-1 do
                        let zre = data[nx*j+i]
                        let zim = 0.0
                        if max < f(zre,zim) then max <- f(zre,zim)
                        if min > f(zre,zim) then min <- f(zre,zim)
                min,max
                
        /// <summary>
        /// ファイルからデータ読み込み(テキストデータ)
        /// </summary>
        /// <param name="filename">入力ファイル名</param>
        /// <param name="ix">x座標のデータ列</param>
        /// <param name="iy">y座標のデータ列</param>
        /// <param name="izre">実部のデータ列</param>
        /// <param name="izim">虚部のデータ列</param>
        /// <param name="eval">複素数→プロット値</param>
        member public this.FileRead(filename:string,ix:int,iy:int,izre:int,izim:int,eval:(double*double)->double) =
            isDataLoaded <- false
            error <- "";
            if not <| File.Exists filename then
                error <- "ファイル「"+filename+"」は存在しません"
            if error = "" then
                // izim=-1の場合は実数データ
                isCPXdata <- not (izim <= 0)
                // 区切り文字の判定
                let sep =
                    let reader:StreamReader = new StreamReader(filename)
                    let rec read n =
                        let tmp:string=reader.ReadLine()
                        match tmp with
                        | _ when tmp.Contains "#" -> 
                            read (n+1)
                        | _ when tmp.Contains "\t" -> 
                            reader.Close()
                            [| '\t' |]
                        | _ when tmp.Contains ","    -> 
                            reader.Close()
                            [| ',' |]
                        | _ when tmp.Contains " "    -> 
                            reader.Close()
                            [| ' ' |]
                        | _ -> 
                            reader.Close()
                            [| ' ' |]
                    read 0
                    
                // データサイズの決定
                let _, _, dx, dy, xmin, xmax, ymin, ymax = 
                    let reader = new StreamReader(filename)
                    // 1行読み込んで離散間隔dx,dy、データ範囲xmin,xmax,ymin,ymaxを更新
                    let rec read n x0 y0 dx dy xmin xmax ymin ymax =
                        let tmp=reader.ReadLine()
                        // ファイル末尾
                        if tmp=null then
                            reader.Close()
                            x0, y0, dx, dy, xmin, xmax, ymin, ymax
                        // コメント行の場合：そのまま次の行を読み込む
                        elif tmp.Contains("#") then
                            read n x0 y0 dx dy xmin xmax ymin ymax
                        // ファイル末尾でなく、コメント行でもない場合
                        else
                            // データのi列目を抽出
                            let extract(i:int) =
                                try
                                    Double.Parse(tmp.Split(sep, StringSplitOptions.RemoveEmptyEntries).[i - 1])
                                with
                                | :? System.FormatException ->
                                    error <- (if error = "" then "" else error+"\r\n") + "データ" + (i.ToString()) + "列目「" + tmp.Split(sep, StringSplitOptions.RemoveEmptyEntries).[i - 1] + "」を数値に変換できません"
                                    0.0
                                | :? System.IndexOutOfRangeException ->
                                    error <- (if error = "" then "" else error+"\r\n") + "データ" + (i.ToString()) + "列目は存在しません"
                                    0.0
                            let x = extract(ix)
                            let y = extract(iy)
                            // エラー検出時：ファイル読み込み中断
                            if error<>"" then
                                reader.Close()
                                x0, y0, -1.0, -1.0, xmin, xmax, ymin, ymax
                            // 最初のデータ読み込み。各値に初期値を設定
                            elif n=0 then
                                read (n+1) x y 0.0 0.0 x x y y
                            // 2回目のデータ読み込み。離散間隔に初期値を設定
                            elif n=1 then
                                read (n+1) x0 y0 (abs(x-x0)) (abs(y-y0)) x x y y
                            // 3回目以降のデータ読み込み。データ範囲と離散間隔を更新
                            else
                                let xmin1 = if xmin > x then x else xmin
                                let xmax1 = if xmax < x then x else xmax
                                let ymin1 = if ymin > y then y else ymin
                                let ymax1 = if ymax < y then y else ymax
                                let dx1 = if dx=0.0 || (dx > abs(x-x0) && abs(x-x0)<>0.0) then abs(x-x0) else dx
                                let dy1 = if dy=0.0 || (dy > abs(y-y0) && abs(y-y0)<>0.0) then abs(y-y0) else dy
                                read (n+1) x0 y0 dx1 dy1 xmin1 xmax1 ymin1 ymax1
                                
                    // read関数実行→sizeの返却値 *)
                    read 0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0
                    
                if dx = 0.0 then error <- (if error = "" then "" else error+"\r\n") + "xの値が一定です。1次元データの可能性があります。"
                if dy = 0.0 then error <- (if error = "" then "" else error+"\r\n") + "yの値が一定です。1次元データの可能性があります。"
                
                // 離散間隔とデータ範囲から離散点数を決定
                nx <- int (Math.Floor((xmax - xmin) / dx + 0.5) + 1.0)
                ny <- int (Math.Floor((ymax - ymin) / dy + 0.5) + 1.0)
                
                if error="" then
                    // 配列初期化
                    try
                        // 複素数データ
                        if isCPXdata then
                            data <- Array.zeroCreate(2*nx*ny)
                        // 実数データ
                        else
                            data <- Array.zeroCreate(nx*ny)
                    with
                    | :? System.OutOfMemoryException | :? System.ArgumentException ->
                        error <- (if error = "" then "" else error+"\r\n") + "配列サイズ("+nx.ToString()+","+ny.ToString()+")を確保できません"
                    // ここまでエラーなしの場合：データ読み込み
                    if error = "" then
                        let reader = new StreamReader(filename)
                        let rec readD n =
                            match reader.ReadLine() with
                            |null ->
                                isDataLoaded <- true
                                reader.Close()
                            |tmp when tmp.Contains "#" ->
                                // コメント行なら次の行の読み込みに進む
                                readD (n+1)
                            |tmp ->
                                // ファイル末尾でなくコメント文でない場合
                                // x座標
                                let x = Double.Parse(tmp.Split(sep, StringSplitOptions.RemoveEmptyEntries).[ix - 1])
                                // y座標
                                let y = Double.Parse(tmp.Split(sep, StringSplitOptions.RemoveEmptyEntries).[iy - 1])
                                // x座標の配列インデックス
                                let ix = int (Math.Floor((x - xmin) / dx + 0.5))
                                // y座標の配列インデックス
                                let iy = int (Math.Floor((y - ymin) / dy + 0.5))
                                let k = tmp.Split(sep, StringSplitOptions.RemoveEmptyEntries)
                                try
                                    let re = Double.Parse(k.[izre - 1])
                                    data[nx*iy+ix] <- re
                                with
                                | :? System.FormatException ->
                                    error <- (if error = "" then "" else error+"\r\n") + "データ" + (izre.ToString()) + "列目「" + k.[izre - 1] + "」を数値に変換できません"
                                    reader.Close()
                                | :? System.IndexOutOfRangeException ->
                                    error <- (if error = "" then "" else error+"\r\n") + "データ" + (izre.ToString()) + "列目は存在しません"
                                    reader.Close()
                                if error = "" then
                                    readD (n+1)
                                else
                                    ()
                        let rec readZ n =
                            match reader.ReadLine() with
                            // ファイル末尾の場合
                            |null ->
                                isDataLoaded <- true
                                reader.Close()
                            // コメント行なら次の行の読み込みに進む
                            |tmp when tmp.Contains("#") ->
                                readZ (n+1)
                            // ファイル末尾でなくコメント文でない場合
                            |tmp ->
                                // x座標
                                let x = Double.Parse(tmp.Split(sep, StringSplitOptions.RemoveEmptyEntries).[ix - 1])
                                // y座標
                                let y = Double.Parse(tmp.Split(sep, StringSplitOptions.RemoveEmptyEntries).[iy - 1])
                                // x座標の配列インデックス
                                let ix = int (Math.Floor((x - xmin) / dx + 0.5))
                                // y座標の配列インデックス
                                let iy = int (Math.Floor((y - ymin) / dy + 0.5))
                                let k = tmp.Split(sep, StringSplitOptions.RemoveEmptyEntries)
                                try
                                    let re = Double.Parse(k.[izre - 1])
                                    data[2*(nx*iy+ix)] <- re
                                with
                                | :? System.FormatException ->
                                    let k = tmp.Split(sep, StringSplitOptions.RemoveEmptyEntries)
                                    error <- (if error = "" then "" else error+"\r\n") + "データ" + izre.ToString() + "列目「" + k.[izre - 1] + "」を数値に変換できません"
                                    reader.Close()
                                | :? System.IndexOutOfRangeException ->
                                    error <- (if error = "" then "" else error+"\r\n") + "データ" + izre.ToString() + "列目は存在しません"
                                    reader.Close()
                                if error = "" then
                                    let h = tmp.Split(sep, StringSplitOptions.RemoveEmptyEntries)
                                    try
                                        let im = Double.Parse(h.[izim - 1])
                                        data[2*(nx*iy+ix)+1] <- im
                                    with
                                    | :? System.FormatException ->
                                        error <- (if error = "" then "" else error+"\r\n") + "データ" + (izim.ToString()) + "列目「" + tmp.Split(sep, StringSplitOptions.RemoveEmptyEntries).[izim - 1] + "」を数値に変換できません"
                                        reader.Close()
                                    | :? System.IndexOutOfRangeException ->
                                        error <- (if error = "" then "" else error+"\r\n") + "データ" + izim.ToString() + "列目は存在しません"
                                        reader.Close()
                                if error = "" then
                                    readZ (n+1)
                                else
                                    ()
                        if isCPXdata then
                            readZ 0
                        else
                            readD 0
                        
        /// <summary>
        /// ファイルからデータ読み込み(バイナリデータ)
        /// </summary>
        /// <param name="filename">入力ファイル名</param>
        /// <param name="eval">複素数→プロット値</param>
        member public this.FileRead(filename:string, searchMinMax:(double*double)->double) =
            isDataLoaded <- false
            error <- "";
            if not <| File.Exists filename then
                error <- "ファイル「"+filename+"」は存在しません"
            if error = "" then
                let fs = new FileStream(filename, FileMode.Open, FileAccess.Read)
                let rd = new BinaryReader(fs)
                let format = rd.ReadInt32()
                if format = 1 then
                    let ntype = rd.ReadInt32()
                    let dim = rd.ReadInt32()
                    if dim > 0 then
                        let size1 = rd.ReadInt32()
                        nx <- size1
                    if dim > 1 then
                        let size2 = rd.ReadInt32()
                        ny <- size2
                    if dim > 2 then
                        ignore <| rd.ReadInt32()
                    if ntype = 3000 then
                        data <- Array.zeroCreate(2*nx*ny)
                    else
                        data <- Array.zeroCreate(nx*ny)
                    match ntype with
                    |1004 ->
                        isCPXdata <- false
                        for i = 0 to data.GetUpperBound 0 do
                            data[i] <- double (rd.ReadInt32())
                        isDataLoaded <- true
                    |2000|3000 ->
                        isCPXdata <- true
                        for i = 0 to data.GetUpperBound 0 do
                            data[i] <- rd.ReadDouble()
                        isDataLoaded <- true
                    |_ ->
                        ()
                else
                    error <- (if error = "" then "" else error+"\r\n") + "unknown file format"
                rd.Close()
                
        /// <summary>
        /// 24ビットビットマップファイルを作成
        /// </summary>
        /// <param name="filename">出力ファイル名</param>
        /// <param name="gradation">数値－カラー対応関数</param>
        /// <param name="autoscale">プロット範囲指定（自動又は手動）</param>
        /// <param name="eval">複素数→プロット値</param>
        /// <param name="phaseshift">位相シフト量</param>
        /// <param name="enlarge">データ値をenlarge×enlargeの正方形画素で表現</param>
        member public this.writeBMP24(filename:string, gradation:Gradation, autoscale:PlotColorRange, eval:(double*double)->double, phaseshift:option<double>, enlarge:int) =
            if not isDataLoaded then
                if error = "" then
                    error <- "data is not loaded"
            else
                let Nx = enlarge*nx
                let Ny = enlarge*ny
                let zabs(re,im) = sqrt(re*re+im*im)
                let zpha(re,im) = atan2 im re
                let pshift(re,im,ps) =
                    let a = zabs(re,im)
                    let p = zpha(re,im)+ps
                    a*cos p,a*sin p
                let rest = if (3*Nx)%4=0 then 0 else 4-(3*Nx)%4
                //---データの規格化------------------------------------------------------
                let min,max =
                    match autoscale with
                    |Auto ->
                        this.MinMax eval
                    |AbsAuto ->
                        let min,max = this.MinMax eval
                        if abs min < abs max then -abs max, abs max else -abs min, abs min
                    |MinMax (min,max) ->
                        min,max
                    |AbsMax max ->
                        -abs max, abs max
                min_ <- min
                max_ <- max
                //---ビットマップファイル生成---------------------------------------------
                let f_strm:FileStream = new FileStream(filename, FileMode.Create)
                let bw:BinaryWriter = new BinaryWriter(f_strm)
                //---BMPFILEHEADER構造体--------------------------------------------------
                let bfSize:int32 = 54 + (3 * Nx + rest) * Ny //ファイル全体のバイト数
                let bfReserved1:int16 = 0s          //常に0
                let bfReserved2:int16 = 0s          //常に0
                let bfOffBits:int32 = 54            //ファイルの最初から画像データまでのデータサイズ
                //---BITMAPINFOHEADER-----------------------------------------------------
                let biSize:int32 = 40               //情報ヘッダサイズ(40byte)
                let biWidth:int32 = Nx              //画像の幅  [pixel]
                let biHeight:int32 = Ny             //画像の高さ[pixel]
                let biPlanes:int16 = 1s             //常に1
                let biBitCount:int16 = 24s          //色ビット数[bit]
                let biCompression:int32 = 0         //圧縮形式
                let biSizeimage:int32 = 0           //ビットマップデータのサイズ(0でもよい)
                let biXPelsPerMeter:int32 = 0       //水平解像度(0でもよい)
                let biYPelsPerMeter:int32 = 0       //垂直解像度(0でもよい)
                let biClrUsed:int32 = 0             //ビットマップが実際に使用するカラーテーブルのエントリ数
                let biClrImportant:int32 = 0        //ビットマップの表示に必要な色数
                //---バイナリデータ書き込み-----------------------------------------------
                bw.Write 'B' //BM
                bw.Write 'M' //BM
                bw.Write bfSize        
                bw.Write bfReserved1 
                bw.Write bfReserved2
                bw.Write bfOffBits
                bw.Write biSize
                bw.Write biWidth
                bw.Write biHeight
                bw.Write biPlanes
                bw.Write biBitCount
                bw.Write biCompression
                bw.Write biSizeimage
                bw.Write biXPelsPerMeter
                bw.Write biYPelsPerMeter
                bw.Write biClrUsed
                bw.Write biClrImportant
                //---画像データ本体--------------------------------------------------------                    
                for  j = 0 to Ny-1 do
                    for i = 0 to Nx-1 do
                        let re,im =
                            match isCPXdata,phaseshift with
                            |true,None  -> data[2*(nx*(j/enlarge)+i/enlarge)], data[2*(nx*(j/enlarge)+i/enlarge)+1]
                            |true,Some ps  -> pshift(data[2*(nx*(j/enlarge)+i/enlarge)], data[2*(nx*(j/enlarge)+i/enlarge)+1], ps)
                            |false,_ -> data[nx*(j/enlarge)+i/enlarge], 0.0
                        let r,g,b =
                            match gradation with
                            |Rx f -> f (eval (re,im), min, max)
                            |Cx f -> f (re,im,min,max)
                        bw.Write(byte <| floor (255.0*b+0.5))    //青
                        bw.Write(byte <| floor (255.0*g+0.5))    //緑
                        bw.Write(byte <| floor (255.0*r+0.5))    //赤
                    for _ = 0 to rest-1 do
                        bw.Write(byte 0)
                bw.Close()
                
        /// <summary>
        /// カラーバー出力
        /// </summary>
        /// <param name="filename">出力ファイル名</param>
        /// <param name="width">幅</param>
        /// <param name="height">高さ</param>
        /// <param name="gradation">数値－カラー対応関数</param>
        /// <param name="autoscale">プロット範囲指定（自動又は手動）</param>
        /// <param name="eval">複素数→プロット値</param>
        member public this.writeColorBar(filename:string, width:int, height:int, gradation:Gradation, autoscale:PlotColorRange, eval:(double*double)->double) =
            if not isDataLoaded then
                if error = "" then
                    error <- "data is not loaded"
            else
                let nx = width
                let ny = height
                let rest = if (3*nx)%4=0 then 0 else 4-(3*nx)%4
                //---データの規格化------------------------------------------------------
                let min,max =
                    match autoscale with
                    |Auto ->
                        this.MinMax eval
                    |AbsAuto ->
                        let min,max = this.MinMax eval
                        if abs min < abs max then -abs max, abs max else -abs min, abs min
                    |MinMax (min,max) ->
                        min,max
                    |AbsMax max ->
                        -abs max, abs max
                        
                //---ビットマップファイル生成---------------------------------------------
                let f_strm:FileStream = new FileStream(filename, FileMode.Create)
                let bw:BinaryWriter = new BinaryWriter(f_strm)
                //---BMPFILEHEADER構造体--------------------------------------------------
                let bfSize:int32 = 54 + (3 * nx + rest) * ny //ファイル全体のバイト数
                let bfReserved1:int16 = 0s          //常に0
                let bfReserved2:int16 = 0s          //常に0
                let bfOffBits:int32 = 54            //ファイルの最初から画像データまでのデータサイズ
                //---BITMAPINFOHEADER-----------------------------------------------------
                let biSize:int32 = 40               //情報ヘッダサイズ(40byte)
                let biWidth:int32 = nx              //画像の幅  [pixel]
                let biHeight:int32 = ny             //画像の高さ[pixel]
                let biPlanes:int16 = 1s             //常に1
                let biBitCount:int16 = 24s          //色ビット数[bit]
                let biCompression:int32 = 0         //圧縮形式
                let biSizeimage:int32 = 0           //ビットマップデータのサイズ(0でもよい)
                let biXPelsPerMeter:int32 = 0       //水平解像度(0でもよい)
                let biYPelsPerMeter:int32 = 0       //垂直解像度(0でもよい)
                let biClrUsed:int32 = 0             //ビットマップが実際に使用するカラーテーブルのエントリ数
                let biClrImportant:int32 = 0        //ビットマップの表示に必要な色数
                //---バイナリデータ書き込み-----------------------------------------------
                bw.Write 'B' //BM
                bw.Write 'M' //BM
                bw.Write bfSize        
                bw.Write bfReserved1 
                bw.Write bfReserved2
                bw.Write bfOffBits
                bw.Write biSize
                bw.Write biWidth
                bw.Write biHeight
                bw.Write biPlanes
                bw.Write biBitCount
                bw.Write biCompression
                bw.Write biSizeimage
                bw.Write biXPelsPerMeter
                bw.Write biYPelsPerMeter
                bw.Write biClrUsed
                bw.Write biClrImportant
                //---画像データ本体--------------------------------------------------------                    
                for  j = 0 to ny-1 do
                    for i = 0 to nx-1 do
                        let r,g,b =
                            match gradation with
                            |Rx f -> f (eval (min+(max-min)*double j/double (ny-1), 0.0), min, max)
                            |Cx f -> f ((min+(max-min)*double j/double (ny-1))*cos(2.0*Math.PI*double i/double (nx-1)),(min+(max-min)*double j/double (ny-1))*sin(2.0*Math.PI*double i/double (nx-1)),min,max)
                        bw.Write(byte <| floor (255.0*b+0.5)) //青
                        bw.Write(byte <| floor (255.0*g+0.5)) //緑
                        bw.Write(byte <| floor (255.0*r+0.5)) //赤
                    for _ = 0 to rest-1 do
                        bw.Write(byte 0)
                bw.Close()
                
        /// 複素数→実部
        static member getRe (re:double,_:double) = re
        
        /// 複素数→虚部
        static member getIm (_:double,im:double) = im
        
        /// 複素数→偏角
        static member getPha (re:double,im:double) = 
            let p = atan2 im re
            if p<0.0 then p+2.0*Math.PI else p
            
        /// 複素数→絶対値
        static member getAbs (re:double,im:double) = sqrt (re*re+im*im)
        
        /// 複素数→強度
        static member getPow (re:double,im:double) = re*re+im*im
        
        /// <summary>
        /// データファイルをプロット(複素データ)
        /// </summary>
        /// <param name="inputFilename">入力ファイル名</param>
        /// <param name="outputFilename">出力ファイル名</param>
        /// <param name="ix">x座標のデータ列</param>
        /// <param name="iy">y座標のデータ列</param>
        /// <param name="izre">実部のデータ列</param>
        /// <param name="izim">虚部のデータ列</param>
        /// <param name="autoscale">プロット範囲指定（自動又は手動）</param>
        /// <param name="phaseshift">位相シフト量</param>
        /// <param name="enlarge">データ値をenlarge×enlargeの正方形画素で表現</param>
        /// <param name="gradation">数値－カラー対応関数</param>
        /// <param name="eval">複素数→プロット値</param>
        static member public Plot(inputFilename:string,outputFilename:string,ix:int,iy:int,izre:int,izim:int,autoscale,phaseshift,enlarge,gradation,eval:(double*double)->double) =
            let x = plot2d()
            x.FileRead(inputFilename,ix,iy,izre,izim,eval)
            x.writeBMP24(outputFilename, gradation, autoscale, eval, phaseshift,enlarge)
            if x.Error <> "" then printfn "%s" x.Error
            x
            
        /// <summary>
        /// データファイルをプロット(実数データ)
        /// </summary>
        /// <param name="inputFilename">入力ファイル名</param>
        /// <param name="outputFilename">出力ファイル名</param>
        /// <param name="ix">x座標のデータ列</param>
        /// <param name="iy">y座標のデータ列</param>
        /// <param name="izre">実部のデータ列</param>
        /// <param name="autoscale">プロット範囲指定（自動又は手動）</param>
        /// <param name="phaseshift">位相シフト量</param>
        /// <param name="enlarge">データ値をenlarge×enlargeの正方形画素で表現</param>
        /// <param name="gradation">数値－カラー対応関数</param>
        /// <param name="eval">複素数→プロット値</param>
        static member public Plot(inputFilename:string,outputFilename:string,ix:int,iy:int,izre:int,autoscale,phaseshift,enlarge,gradation,eval:(double*double)->double) =
            let x = plot2d()
            x.FileRead(inputFilename,ix,iy,izre,0,eval)
            x.writeBMP24(outputFilename, gradation, autoscale, eval, phaseshift,enlarge)
            if x.Error <> "" then printfn "%s" x.Error
            x
            
        /// <summary>
        /// データファイルをプロット(バイナリデータ)
        /// </summary>
        /// <param name="inputFilename">入力ファイル名</param>
        /// <param name="outputFilename">出力ファイル名</param>
        /// <param name="autoscale">プロット範囲指定（自動又は手動）</param>
        /// <param name="phaseshift">位相シフト量</param>
        /// <param name="enlarge">データ値をenlarge×enlargeの正方形画素で表現</param>
        /// <param name="gradation">数値－カラー対応関数</param>
        /// <param name="eval">複素数→プロット値</param>
        static member public Plot(inputFilename:string,outputFilename:string,autoscale,phaseshift,enlarge,gradation,eval:(double*double)->double) =
            let x = plot2d()
            x.FileRead(inputFilename,eval)
            x.writeBMP24(outputFilename, gradation, autoscale, eval, phaseshift,enlarge)
            if x.Error <> "" then printfn "%s" x.Error
            x
