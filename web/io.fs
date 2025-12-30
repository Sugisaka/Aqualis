namespace Aqualis

open System
open System.IO
            
[<AutoOpen>]
module htmlio =
    /// 変数カウンタ
    let mutable icounter = 0
    /// draw関数のコード出力先ファイル名
    let mutable filename_draw = ""
    /// HTML本体のコード出力先ファイル名
    let mutable filename_body = ""
    let mutable filename_js = ""
    /// コンテンツディレクトリ
    let mutable contentsDir = ""
    /// 出力先ディレクトリ
    let mutable outputDir = ""
    // /// draw関数内のコード
    // let mutable wrDraw = OptionWriter()
    // /// body要素内のコード
    // let mutable wrBody = OptionWriter()
    // let mutable wrJS = OptionWriter()
    
    /// HTMLファイルへの書き込み
    let switchMain code =
        prIndex <- 0
        code()
    /// HTMLファイルbodyタグ内への書き込み
    let switchBody code =
        prIndex <- 1
        code()
    /// HTMLファイルJavaScriptコードの書き込み
    let switchJS code =
        prIndex <- 2
        code()
    /// HTMLファイルdraw関数への書き込み
    let switchDraw code =
        prIndex <- 3
        code()
        
    let mutable audioList:string list = []
    /// 図面カウンタ
    let mutable figcounter = 0
    /// アニメーションカウンタ
    let mutable anicounter = 0

    // /// ファイル書き込み
    // let write (t:string) = wrBody.Write t
    // let fileOpen (t:string) = wrBody.FileSet t
    // let fileClose (t:string) = wrBody.Close()
    
    // /// ファイルを閉じて書き込んだコードを取得
    // let read() =
    //     let drawcode = wrDraw.Read()
    //     let bodycode = wrBody.Read()
    //     let jscode = wrJS.Read()
    //     drawcode,bodycode,jscode
