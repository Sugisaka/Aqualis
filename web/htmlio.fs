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
    /// draw関数のコード出力先ファイル名
    /// HTML本体のコード出力先ファイル名
    /// コンテンツディレクトリ
    // /// draw関数内のコード
    // let mutable wrDraw = OptionWriter()
    // /// body要素内のコード
    // let mutable wrBody = OptionWriter()
    // let mutable wrJS = OptionWriter()

    let private withProgramIndex index code =
        let context = GenerationScope.requireContext()
        context.WithProgram(index, code)

    /// HTMLファイルへの書き込み
    let switchMain code = withProgramIndex 0 code
    /// HTMLファイルbodyタグ内への書き込み
    let switchBody code = withProgramIndex 1 code
    /// HTMLファイルdraw関数への書き込み
    let switchJSMain code = withProgramIndex 2 code
    /// HTMLファイルJavaScriptコードの書き込み
    let switchAnimationSeq code = withProgramIndex 3 code
    /// アニメーション開始コードの書き込み
    let switchJSAnimationStart code = withProgramIndex 4 code
    /// アニメーション連結コードの書き込み
    let switchJSAnimationSeqReset code = withProgramIndex 5 code
    /// アニメーション初期化コードの書き込み
    let switchJSAnimationReset code = withProgramIndex 6 code
    /// 自動開始アニメーション実行コードの書き込み
    let switchAutoAnimation code = withProgramIndex 7 code

    /// 図面カウンタ
    /// アニメーションカウンタ

    let nextContentsID() =
        let state = WebGenerationScope.html()
        state.ContentsCounter <- state.ContentsCounter + 1
        "contentsID"+state.ContentsCounter.ToString()

    let nextAnimationSeqID() =
        let state = WebGenerationScope.html()
        state.AnimationSequenceCounter <- state.AnimationSequenceCounter + 1
        "animationSeqID"+state.AnimationSequenceCounter.ToString(),
        "animationSeqResetID"+state.AnimationSequenceCounter.ToString()

    let nextAnimationGroup() =
        let state = WebGenerationScope.html()
        state.AnimationGroupCounter <- state.AnimationGroupCounter + 1
        state.AnimationGroupCounter.ToString()

    let animationButtonReset() =
        (WebGenerationScope.html()).AnimationButtons.Clear()

    let addAnimationButton(fnameStart,fnameReset,buttonX,buttonY) =
       (WebGenerationScope.html()).AnimationButtons.Add(
           fnameStart, fnameReset, buttonX, buttonY)

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
