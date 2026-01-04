//#############################################################################
// グラフプロットテスト
let projectname = "test10C"
let version = "1.0.0"
//#############################################################################

let outputdir = __SOURCE_DIRECTORY__

#I @"..\..\bin\Debug\net10.0"
#r "Aqualis.dll"

open Aqualis

let step = 2

//データファイルの作成
group.section step 1 <| fun () ->
    Compile [Fortran] outputdir projectname (version,"aaa") <| fun () ->
        io.fileOutput "data1.dat" <| fun wr ->
            let N = 21
            iter.num N <| fun i ->
                ch.ddd <| fun (x,y1,y2) ->
                    x <== i
                    y1 <== 2*i+1
                    y2 <== 0.5*i*i
                    wr [x; y1; y2]
        io.fileOutput "data2.dat" <| fun wr ->
            let N = 21
            iter.num N <| fun i ->
                ch.ddd <| fun (x,y1,y2) ->
                    x <== i
                    y1 <== -2*(i-10)+1
                    y2 <== 0.5*(i-10)*(i-10)
                    wr [x; y1; y2]
                    
group.section step 2 <| fun () ->
    // outputdir：読み込むデータファイルと生成するsvgファイルのディレクトリ
    // "plot.svg"：グラフのファイル名
    // (graph1d.A4PTwoColSingle 1)：A4サイズ２段組のドキュメントに挿入する図面。グラフは横方向に1個、縦に1個配置
    graph1d.makeGraph outputdir "plot10C.svg" (graph1d.A4PTwoColSingle 1) <| fun addGraph ->
        // (1,1)：グラフを図面内の左から一つ目、下から一つ目の位置に追加
        // None：サブキャプション（グラフ下部に挿入するテキスト）なし
        addGraph (1,1) None
            {
                // 横軸の設定
                // Scale：Linearは線形軸、Log10は対数軸
                // Range：プロット範囲。Autoは自動、MinMax(0,10)とすると1から10の範囲に設定
                // NumFormat：横軸の目盛り数字のフォーマット。F0は小数点以下0桁
                Xaxis = {Scale=Linear; Range=Auto; NumFormat=Some "F0"}
                // 縦軸の設定
                // Scale：Linearは線形軸、Log10は対数軸
                // Range：プロット範囲。Autoは自動、MinMax(0,10)とすると1から10の範囲に設定
                // NumFormat：横軸の目盛り数字のフォーマット。Noneはデフォルト（小数点以下1桁）
                Yaxis = {Scale=Linear; Range=Auto; NumFormat=None}
                // 横軸ラベル
                Xlabel = TextStyle.Italic "x"
                // 縦軸ラベル。TextStyle.Italicは斜体、TextStyle.Subは下付き文字、TextStyle.Supは上付き文字
                Ylabel = TextStyle.Italic "f" + TextStyle.Sub "1"
            }
            [
                // グラフにプロットデータを追加
                Datafile{
                    // 線と点でプロット
                    Style = LinesPoints{
                        // 線のスタイル。黒の破線、線幅：0.5、線の長さ：2.0、破線の隙間：1.0
                        Lines = {Style=color.stroke.dashblack(0.5,[2.0;1.0])}
                        // 点のスタイル。形状：円形、サイズ：1.0、円の枠線：なし、円の塗り色：黒
                        Points = {Shape=Circle; Size=1.0; StrokeStyle = (fun (x,y) -> color.stroke.none); FillStyle = fun (x,y) -> color.fill.black}}
                    // データファイル名
                    FileName = "data1.dat"
                    // 凡例：なし
                    Legend = None
                    // 横軸のデータ
                    // データファイルの1列目を横軸にプロットする場合： Xcolumn = fun data -> data(1)
                    // データファイルの2列目に5を足した値を横軸にプロットする場合： Xcolumn = fun data -> data(2)+5.0
                    // データファイルの3列目を100倍した値を横軸にプロットする場合： Xcolumn = fun data -> 100.0*data(3)
                    Xcolumn = fun data -> data 1
                    // 縦軸のデータ
                    // データファイルの1列目を縦軸にプロットする場合： Ycolumn = fun data -> data(1)
                    // データファイルの2列目に5を足した値を縦軸にプロットする場合： Ycolumn = fun data -> data(2)+5.0
                    // データファイルの2列目の2乗と3列目の2乗の和の平方根を縦軸にプロットする場合： Ycolumn = fun data -> sqrt(data(2)*data(2)+data(3)*data(3))
                    Ycolumn = fun data -> data 2}
                // グラフにプロットデータを追加
                Datafile{
                    // 線と点でプロット
                    Style = LinesPoints{
                        // 線のスタイル。黒の破線、線幅：0.5、線の長さ：2.0、破線の隙間：1.0
                        Lines = {Style=color.stroke.dashRGB(0,100,255,0.5,[2.0;1.0])}
                        // 点のスタイル。形状：円形、サイズ：1.0、円の枠線：なし、円の塗り色：黒
                        Points = {Shape=Circle; Size=1.0; StrokeStyle = (fun (x,y) -> color.stroke.none); FillStyle = fun (x,y) -> color.fill.RGB(0,100,255)}}
                    // データファイル名
                    FileName = "data2.dat"
                    // 凡例：なし
                    Legend = None
                    // 横軸のデータ
                    // データファイルの1列目を横軸にプロットする場合： Xcolumn = fun data -> data(1)
                    // データファイルの2列目に5を足した値を横軸にプロットする場合： Xcolumn = fun data -> data(2)+5.0
                    // データファイルの3列目を100倍した値を横軸にプロットする場合： Xcolumn = fun data -> 100.0*data(3)
                    Xcolumn = fun data -> data 1
                    // 縦軸のデータ
                    // データファイルの1列目を縦軸にプロットする場合： Ycolumn = fun data -> data(1)
                    // データファイルの2列目に5を足した値を縦軸にプロットする場合： Ycolumn = fun data -> data(2)+5.0
                    // データファイルの2列目の2乗と3列目の2乗の和の平方根を縦軸にプロットする場合： Ycolumn = fun data -> sqrt(data(2)*data(2)+data(3)*data(3))
                    Ycolumn = fun data -> data 3}
            ]
