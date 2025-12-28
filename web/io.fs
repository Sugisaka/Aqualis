namespace docWriter

open System
open System.IO

type OptionWriter() =
    let mutable filename = ""
    let mutable wr :option<StreamWriter> = None
    member _.FileSet (file:string) =
        filename <- file
        match wr with
        |None -> 
            ()
        |Some w -> 
            w.Close()
        wr <- Some(new StreamWriter(filename))
    /// ファイル書き込み
    member _.Write (t:string) =
        match wr with
        |None -> ()
        |Some w -> w.WriteLine t
    /// ファイルを閉じる
    member _.Close () =
        match wr with
        |None -> ()
        |Some w -> w.Close()
    /// ファイルを閉じてコードを取得
    member _.Read() =
        match wr with
        |Some w when File.Exists filename ->
            w.Close()
            let code = File.ReadAllText filename
            File.Delete filename
            wr <- None
            code
        |_ ->
            ""
            
[<AutoOpen>]
module io =
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
    /// draw関数内のコード
    let mutable wrDraw = OptionWriter()
    /// body要素内のコード
    let mutable wrBody = OptionWriter()
    let mutable wrJS = OptionWriter()
    /// 音声ファイル配列のコード
    let mutable audioList:string list = []
    /// 図面カウンタ
    let mutable figcounter = 0
    /// アニメーションカウンタ
    let mutable anicounter = 0

    /// ファイル書き込み
    let write (t:string) = wrBody.Write t
    let fileOpen (t:string) = wrBody.FileSet t
    let fileClose (t:string) = wrBody.Close()
    
    /// ファイルを閉じて書き込んだコードを取得
    let read() =
        let drawcode = wrDraw.Read()
        let bodycode = wrBody.Read()
        let jscode = wrJS.Read()
        drawcode,bodycode,jscode
