// 
// Copyright (c) 2026 Jun-ichiro Sugisaka
// 
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
// 
namespace Aqualis
    
    open System
    open System.Globalization
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

    type private PlotBinaryValueType =
        |Int32Value
        |Float64Value
        |Complex128Value
        
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

        let appendError message =
            error <-
                if String.IsNullOrEmpty error then
                    message
                else
                    error + Environment.NewLine + message
        
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
                        let value = f(zre,zim)
                        if max < value then max <- value
                        if min > value then min <- value
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
        member public this.FileRead(filename:string,ix:int,iy:int,izre:int,izim:int) =
            isDataLoaded <- false
            data <- [||]
            nx <- 0
            ny <- 0
            error <- ""

            let errors = ResizeArray<string>()
            let addError message =
                if not (errors.Contains message) then
                    errors.Add message
            let finishErrors() =
                error <- String.concat Environment.NewLine errors
            let isDataLine (line:string) =
                not (String.IsNullOrWhiteSpace line) &&
                not (line.Contains "#")
            let chooseSeparator (line:string) =
                if line.Contains "\t" then [|'\t'|]
                elif line.Contains "," then [|','|]
                else [|' '|]
            let tryParseColumn column (columns:string array) =
                if column <= 0 || column > columns.Length then
                    addError $"データ{column}列目は存在しません"
                    None
                else
                    let text = columns[column - 1]
                    match Double.TryParse(
                        text,
                        NumberStyles.Float,
                        CultureInfo.InvariantCulture) with
                    |true, value ->
                        Some value
                    |false, _ ->
                        addError $"データ{column}列目「{text}」を数値に変換できません"
                        None

            if not (File.Exists filename) then
                addError $"ファイル「{filename}」は存在しません"
            else
                let firstDataLine =
                    File.ReadLines(filename)
                    |> Seq.tryFind isDataLine

                match firstDataLine with
                |None ->
                    addError "データ行が存在しません"
                |Some firstLine ->
                    let separator = chooseSeparator firstLine
                    let mutable firstPoint: (double * double) option = None
                    let mutable pointCount = 0
                    let mutable dx = 0.0
                    let mutable dy = 0.0
                    let mutable xmin = 0.0
                    let mutable xmax = 0.0
                    let mutable ymin = 0.0
                    let mutable ymax = 0.0

                    // First pass: validate coordinates and determine the grid.
                    for line in File.ReadLines(filename) do
                        if isDataLine line && errors.Count = 0 then
                            let columns =
                                line.Split(
                                    separator,
                                    StringSplitOptions.RemoveEmptyEntries)
                            match
                                tryParseColumn ix columns,
                                tryParseColumn iy columns
                            with
                            |Some x, Some y ->
                                match firstPoint with
                                |None ->
                                    firstPoint <- Some(x, y)
                                    xmin <- x
                                    xmax <- x
                                    ymin <- y
                                    ymax <- y
                                |Some(firstX, firstY) ->
                                    xmin <- min xmin x
                                    xmax <- max xmax x
                                    ymin <- min ymin y
                                    ymax <- max ymax y

                                    let xDistance = abs (x - firstX)
                                    let yDistance = abs (y - firstY)
                                    if xDistance > 0.0 && (dx = 0.0 || xDistance < dx) then
                                        dx <- xDistance
                                    if yDistance > 0.0 && (dy = 0.0 || yDistance < dy) then
                                        dy <- yDistance
                                pointCount <- pointCount + 1
                            |_ ->
                                ()

                    if pointCount = 0 && errors.Count = 0 then
                        addError "データ行が存在しません"
                    if dx <= 0.0 || Double.IsNaN dx || Double.IsInfinity dx then
                        addError "xの値が一定です。1次元データの可能性があります。"
                    if dy <= 0.0 || Double.IsNaN dy || Double.IsInfinity dy then
                        addError "yの値が一定です。1次元データの可能性があります。"

                    if errors.Count = 0 then
                        let calculatedNx =
                            Math.Floor((xmax - xmin) / dx + 0.5) + 1.0
                        let calculatedNy =
                            Math.Floor((ymax - ymin) / dy + 0.5) + 1.0

                        if
                            Double.IsNaN calculatedNx ||
                            Double.IsInfinity calculatedNx ||
                            calculatedNx < 1.0 ||
                            calculatedNx > float Int32.MaxValue ||
                            Double.IsNaN calculatedNy ||
                            Double.IsInfinity calculatedNy ||
                            calculatedNy < 1.0 ||
                            calculatedNy > float Int32.MaxValue
                        then
                            addError "配列サイズが有効範囲外です。"
                        else
                            nx <- int calculatedNx
                            ny <- int calculatedNy
                            isCPXdata <- izim > 0

                            let elementCount =
                                int64 nx * int64 ny *
                                (if isCPXdata then 2L else 1L)
                            if elementCount > int64 Int32.MaxValue then
                                addError $"配列サイズ({nx},{ny})を確保できません"
                            else
                                try
                                    data <- Array.zeroCreate(int elementCount)
                                with
                                | :? OutOfMemoryException
                                | :? ArgumentException
                                | :? OverflowException ->
                                    addError $"配列サイズ({nx},{ny})を確保できません"

                    if errors.Count = 0 then
                        // Second pass: parse each row once and store real/complex data.
                        for line in File.ReadLines(filename) do
                            if isDataLine line && errors.Count = 0 then
                                let columns =
                                    line.Split(
                                        separator,
                                        StringSplitOptions.RemoveEmptyEntries)
                                let imaginary =
                                    if isCPXdata then
                                        tryParseColumn izim columns
                                    else
                                        Some 0.0
                                match
                                    tryParseColumn ix columns,
                                    tryParseColumn iy columns,
                                    tryParseColumn izre columns,
                                    imaginary
                                with
                                |Some x, Some y, Some real, Some imag ->
                                    let xIndex =
                                        int (Math.Floor((x - xmin) / dx + 0.5))
                                    let yIndex =
                                        int (Math.Floor((y - ymin) / dy + 0.5))
                                    if
                                        xIndex < 0 || xIndex >= nx ||
                                        yIndex < 0 || yIndex >= ny
                                    then
                                        addError "データ座標が配列範囲外です"
                                    else
                                        let index = nx * yIndex + xIndex
                                        if isCPXdata then
                                            data[2 * index] <- real
                                            data[2 * index + 1] <- imag
                                        else
                                            data[index] <- real
                                |_ ->
                                    ()

                        isDataLoaded <- errors.Count = 0

            finishErrors()
                        
        /// <summary>
        /// ファイルからデータ読み込み(バイナリデータ)
        /// </summary>
        /// <param name="filename">入力ファイル名</param>
        /// <param name="eval">複素数→プロット値</param>
        member public this.FileRead(filename:string) =
            isDataLoaded <- false
            isCPXdata <- false
            data <- [||]
            nx <- 0
            ny <- 0
            error <- ""

            if not (File.Exists filename) then
                appendError $"ファイル「{filename}」は存在しません"
            else
                try
                    use stream =
                        new FileStream(
                            filename,
                            FileMode.Open,
                            FileAccess.Read,
                            FileShare.Read)
                    use reader = new BinaryReader(stream)

                    if stream.Length < 12L then
                        appendError "バイナリヘッダーが途中で終了しています。"
                    else
                        let format = reader.ReadInt32()
                        let rawValueType = reader.ReadInt32()
                        let dimension = reader.ReadInt32()

                        let valueType =
                            match rawValueType with
                            |1004 -> Some Int32Value
                            |2000 -> Some Float64Value
                            |3000 -> Some Complex128Value
                            |_ ->
                                appendError $"未対応のデータ型です: {rawValueType}"
                                None

                        if format <> 1 then
                            appendError $"未対応のファイル形式です: {format}"
                        if dimension <> 2 then
                            appendError $"plot2dでは2次元データのみ読み込めます: dim={dimension}"

                        let mutable width = 0
                        let mutable height = 0
                        if error = "" then
                            if stream.Length - stream.Position < 8L then
                                appendError "配列サイズを格納したヘッダーが途中で終了しています。"
                            else
                                width <- reader.ReadInt32()
                                height <- reader.ReadInt32()
                                if width <= 0 || height <= 0 then
                                    appendError $"配列サイズが不正です: ({width},{height})"

                        match valueType with
                        |Some valueType when error = "" ->
                            let scalarMultiplier =
                                match valueType with
                                |Complex128Value -> 2L
                                |_ -> 1L
                            let elementCount =
                                int64 width * int64 height * scalarMultiplier

                            if
                                elementCount <= 0L ||
                                elementCount > int64 Array.MaxLength
                            then
                                appendError $"配列サイズが有効範囲外です: ({width},{height})"
                            else
                                let bytesPerElement =
                                    match valueType with
                                    |Int32Value -> 4L
                                    |Float64Value
                                    |Complex128Value -> 8L
                                let expectedBytes =
                                    elementCount * bytesPerElement
                                let remainingBytes =
                                    stream.Length - stream.Position

                                if remainingBytes < expectedBytes then
                                    appendError (
                                        $"データが不足しています。必要: {expectedBytes} byte、" +
                                        $"実際: {remainingBytes} byte")
                                else
                                    let loadedData =
                                        Array.zeroCreate<double> (int elementCount)

                                    match valueType with
                                    |Int32Value ->
                                        for index = 0 to loadedData.Length - 1 do
                                            loadedData[index] <-
                                                double (reader.ReadInt32())
                                    |Float64Value
                                    |Complex128Value ->
                                        for index = 0 to loadedData.Length - 1 do
                                            loadedData[index] <- reader.ReadDouble()

                                    data <- loadedData
                                    nx <- width
                                    ny <- height
                                    isCPXdata <- valueType = Complex128Value
                                    isDataLoaded <- true
                        |_ ->
                            ()
                with
                | :? EndOfStreamException ->
                    appendError "バイナリファイルが途中で終了しています。"
                | :? UnauthorizedAccessException as exceptionInfo ->
                    appendError $"ファイルへアクセスできません: {exceptionInfo.Message}"
                | :? IOException as exceptionInfo ->
                    appendError $"ファイルを読み込めません: {exceptionInfo.Message}"
                | :? OutOfMemoryException ->
                    appendError "データ配列を確保できません。"
                
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
                use f_strm = new FileStream(filename, FileMode.Create)
                use bw = new BinaryWriter(f_strm)
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
                use f_strm = new FileStream(filename, FileMode.Create)
                use bw = new BinaryWriter(f_strm)
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
            x.FileRead(inputFilename,ix,iy,izre,izim)
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
            x.FileRead(inputFilename,ix,iy,izre,0)
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
            x.FileRead inputFilename
            x.writeBMP24(outputFilename, gradation, autoscale, eval, phaseshift,enlarge)
            if x.Error <> "" then printfn "%s" x.Error
            x
