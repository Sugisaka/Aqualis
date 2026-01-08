// 
// Copyright (c) 2026 Jun-ichiro Sugisaka
// 
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
// 
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
    let mutable filename_JSAnimationStart = ""
    let mutable filename_JSAnimationSeqReset = ""
    let mutable filename_JSAnimationReset = ""
    /// コンテンツディレクトリ
    let mutable contentsDir = ""
    let mutable private contentsCounter = -1
    let mutable private animtionSeqCounter = -1
    let mutable private animationGroupCounter = -1
    // /// draw関数内のコード
    // let mutable wrDraw = OptionWriter()
    // /// body要素内のコード
    // let mutable wrBody = OptionWriter()
    // let mutable wrJS = OptionWriter()
    
    /// HTMLファイルへの書き込み
    let switchMain code =
        let prIndex_temp = prIndex
        prIndex <- 0
        let p = code()
        prIndex <- prIndex_temp
        p
    /// HTMLファイルbodyタグ内への書き込み
    let switchBody code =
        let prIndex_temp = prIndex
        prIndex <- 1
        let p = code()
        prIndex <- prIndex_temp
        p
    /// HTMLファイルJavaScriptコードの書き込み
    let switchJS code =
        let prIndex_temp = prIndex
        prIndex <- 2
        let p = code()
        prIndex <- prIndex_temp
        p
    /// HTMLファイルdraw関数への書き込み
    let switchDraw code =
        let prIndex_temp = prIndex
        prIndex <- 3
        let p = code()
        prIndex <- prIndex_temp
        p
    /// アニメーション開始コードの書き込み
    let switchJSAnimationStart code =
        let prIndex_temp = prIndex
        prIndex <- 4
        let p = code()
        prIndex <- prIndex_temp
        p
    /// アニメーション連結コードの書き込み
    let switchJSAnimationSeqReset code =
        let prIndex_temp = prIndex
        prIndex <- 5
        let p = code()
        prIndex <- prIndex_temp
        p
    /// アニメーション初期化コードの書き込み
    let switchJSAnimationReset code =
        let prIndex_temp = prIndex
        prIndex <- 6
        let p = code()
        prIndex <- prIndex_temp
        p
    /// 自動開始アニメーション実行コードの書き込み
    let switchAutoAnimation code =
        let prIndex_temp = prIndex
        prIndex <- 7
        let p = code()
        prIndex <- prIndex_temp
        p
        
    let mutable animationButtonList:list<string*string*int*int> = []
    let mutable audioList:string list = []
    /// 図面カウンタ
    let mutable figcounter = 0
    /// アニメーションカウンタ
    let mutable anicounter = 0
    
    let nextContentsID() =
        contentsCounter <- contentsCounter + 1
        "contentsID"+contentsCounter.ToString()
        
    let nextAnimationSeqID() =
        animtionSeqCounter <- animtionSeqCounter + 1
        "animationSeqID"+animtionSeqCounter.ToString(),"animationSeqResetID"+animtionSeqCounter.ToString()
        
    let nextAnimationGroup() =
        animationGroupCounter <- animationGroupCounter + 1
        animationGroupCounter.ToString()
        
    let animationButtonReset() =
        animationButtonList <- []
        
    let addAnimationButton(fnameStart,fnameReset,buttonX,buttonY) =
       animationButtonList <- animationButtonList@[fnameStart,fnameReset,buttonX,buttonY]
       
    let addAutoAnimation(fnameStart,fnameReset) =
        switchAutoAnimation <| fun () ->
            writein("animationStartMap['"+fnameStart+"']();")
            
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
