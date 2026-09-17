//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

open System
open System.IO
open System.Text.Json

[<AutoOpen>]
/// Adds presentation elements to the HTML generation context.
module HtmlGenerationExtensions2 =
    type html with
        /// <summary>
        /// Copies an image through an explicit asset context and writes an img element.
        /// This overload is available to ordinary HTML and PHP generation contexts.
        /// </summary>
        member this.image (assets:WebAssetContext, attributes:list<Atr>, filename:string) =
            if attributes |> List.exists (fun attribute -> String.Equals(attribute.name, "src", StringComparison.OrdinalIgnoreCase)) then
                invalidArg (nameof attributes) "The image source is managed by the asset context; do not supply a src attribute."
            let sourceUrl = assets.Import filename
            this.taga ("img", attributes @ [Atr("src", sourceUrl)])

        /// <summary>Copies an image through an explicit asset context and writes a styled img element.</summary>
        member this.image (assets:WebAssetContext, style:Style, filename:string) =
            this.image (assets, [style.atr], filename)

        /// <summary>Copies an image through an explicit asset context and writes an img element.</summary>
        member this.image (assets:WebAssetContext, filename:string) =
            this.image (assets, [], filename)

    /// Owns the contexts and assets for an HTML presentation.
    type HtmlGenerationContext with
        
        /// <summary>
        /// 指定位置に画像を表示する
        /// </summary>
        /// <param name="s">適用するスタイル</param>
        /// <param name="p">表示位置</param>
        /// <param name="filename">表示する画像のファイル名</param>
        member this.image (s:Style,p:position) = fun (filename:string) ->
            let st = Style [{Key="position"; Value="absolute"}; {Key="margin-left"; Value=InvariantFormat.number p.x+"px"}; {Key="margin-top"; Value=InvariantFormat.number p.y+"px"}] + s
            this.html.image (this.Assets, st, filename)
        /// Imports and writes an image with the supplied filename and optional styling.
        member this.image (s:Style, id:string) = fun (filename:string) ->
            this.html.image (this.Assets, [Atr("id",id); s.atr], filename)
        /// Imports and writes an image with the supplied filename and optional styling.
        member this.image (s:Style) = fun (filename:string) ->
            this.html.image (this.Assets, s, filename)
        /// Imports and writes an image with the supplied filename and optional styling.
        member this.image (filename:string) =
            this.html.image (this.Assets, filename)
        /// <summary>
        /// 指定位置に動画を表示する
        /// </summary>
        /// <param name="s">適用するスタイル</param>
        /// <param name="p">表示位置</param>
        /// <param name="filename">表示する動画のファイル名</param>
        member this.video (s:Style,p:position) = fun (filename:string) ->
            let sourceUrl = this.ImportAsset filename
            let st = Style [{Key="margin-left"; Value=InvariantFormat.number p.x+"px"}; {Key="margin-top"; Value=InvariantFormat.number p.y+"px"}] + s
            this.html.tagv ("video", [st.atr; Atr("src", sourceUrl); Atr("controls")])
            this.html.tage "video"
        /// Imports and writes a video with the supplied filename and optional styling.
        member this.video (s:Style) = fun (filename:string) ->
            let sourceUrl = this.ImportAsset filename
            this.html.tagv ("video", [s.atr; Atr("src", sourceUrl); Atr("controls")])
            this.html.tage "video"
        /// <summary>
        /// キャラクター付き解説ページ
        /// </summary>
        member this.page (c:list<CharacterImage>) (audio:Audio,audioFile:option<string>,scriptColor:string) code2 =
            this.slide position.Origin <| fun p ->
                let animationCounter = this.AnimationCount
                // 音声ファイル追加
                this.AddAudioFile(
                    match audioFile with |Some t -> t |None -> "")
                // 字幕枠
                let subtitleBackgroundStyle =
                    "width: 1880px; height: 160px; " +
                    (if this.SubtitleEnabled then "display: block; " else "display: none; ") +
                    "position: absolute; z-index: 1; margin-top: 880px; padding: 20px; background-color: #aaaaff; font-family: 'Noto Sans JP'; font-size: 48px; font-weight: 800; text-shadow: 0 1px 0 #fff, 1px 0 0 #fff, 0 -1px 0 #fff, -1px -1px 0 #fff, 1px -1px 0 #fff, -1px 1px 0 #fff, 1px 1px 0 #fff"
                this.html.tagb ("div", [Atr("id", "sb" + InvariantFormat.integer animationCounter); Atr("style", subtitleBackgroundStyle)]) <| fun () ->
                    ()
                // キャラクター画像
                this.html.tagb (
                    "div",
                    [Atr("id", "c" + InvariantFormat.integer animationCounter)
                     Atr("style", if this.CharacterEnabled then "display: block" else "display: none")]) <| fun () ->
                    for ci in c do
                        let sourceUrl = this.ImportAsset ci.CharacterImageFile
                        this.html.taga ("img", [Atr("src", sourceUrl); Atr("style", ci.CharacterImageStyle)])
                // 字幕
                let subtitleStyle =
                    "width: 1880px; height: 160px; " +
                    (if this.SubtitleEnabled then "display: block; " else "display: none; ") +
                    "position: absolute; z-index: 5; margin-top: 880px; padding: 20px; font-family: 'Noto Sans JP'; color: " + scriptColor + "; font-size: 48px; font-weight: 800; text-shadow: 0 1px 0 #fff, 1px 0 0 #fff, 0 -1px 0 #fff, -1px 0 0 #fff, -1px -1px 0 #fff, 1px -1px 0 #fff, -1px 1px 0 #fff, 1px 1px 0 #fff"
                this.html.tagb ("div", [Atr("id", "s" + InvariantFormat.integer animationCounter); Atr("style", subtitleStyle)])
                    <| fun () -> this.BodyContext.html.text audio.Subtitle
                this.switchAutoAnimation <| fun ctx ->
                    ctx.writein ("page"+InvariantFormat.integer animationCounter+": () => {")
                // メインコンテンツ
                this.html.tagb ("div", [Atr("style", "width: 1920px; height: 880px; position: absolute; z-index: 0")]) <| fun () ->
                    code2 p
                this.switchAutoAnimation <| fun ctx ->
                    ctx.writein "},"
                match this.TryLastAnimationButton() with
                | Some(fStartName,fResetName,btnx,btny) ->
                    this.startButton2 ("startButton"+fStartName) (Style[position.position "absolute"; margin.left (CssLength.pixelsInt btnx); margin.top (CssLength.pixelsInt btny); position.index 1000;]) ("animationStartMap['"+fStartName+"']()")
                    this.resetButton2 ("resetButton"+fStartName) (Style[position.position "absolute"; margin.left (CssLength.pixelsInt btnx); margin.top (CssLength.pixelsInt (btny+25)); position.index 1000;]) ("animationResetMap['"+fResetName+"']()")
                | None -> ()
                this.ClearAnimationButtons()
        /// <summary>
        /// 指定位置にスライドを生成
        /// </summary>
        /// <param name="p">スライドの表示位置</param>
        member this.slide (p:position)  code =
                let animationCounter = this.NextAnimationNumber()
                this.html.tagb (
                    "div",
                    [Atr("id", "p" + InvariantFormat.integer animationCounter)
                     Atr("style", "display: " + (if animationCounter=1 then "block" else "none") + "; position: absolute")]) <| fun wr ->
                    code p
        /// <summary>
        /// 前のページへ移動するボタンを生成
        /// </summary>
        member this.prevButton() =
                this.html.tagb ("button", [Atr("id", "prevButton"); Atr("style", "position: absolute; z-index: 100"); Atr("onclick", "drawPrev()")]) <| fun () ->
                    this.BodyContext.writein "前へ"
        /// <summary>
        /// 次のページへ移動するボタンを生成
        /// </summary>
        member this.nextButton() =
                this.html.tagb ("button", [Atr("id", "nextButton"); Atr("style", "position: absolute; margin-left: 75px; z-index: 100"); Atr("onclick", "drawNext()")]) <| fun () ->
                    this.BodyContext.writein "次へ"
        /// <summary>
        /// アニメーションを開始するボタンを生成
        /// </summary>
        member this.startButton2(id:string) (s:Style) (c:string) =
                this.html.tagb ("button", [Atr("id",id); Atr("onclick",c)]@[s.atr]) <| fun () ->
                    this.BodyContext.writein "Start"
        /// <summary>
        /// アニメーションをリセットするボタンを生成
        /// </summary>
        member this.resetButton2(id:string) (s:Style) (c:string) =
                this.html.tagb ("button", [Atr("id",id); Atr("onclick",c)]@[s.atr]) <| fun () ->
                    this.BodyContext.writein "Reset"
        /// <summary>
        /// キャラクター表示を制御するチェックボックスを生成
        /// </summary>
        member this.switchCharacter() =
            let checkedAttribute = if this.CharacterEnabled then [Atr("checked")] else []
            this.html.taga (
                "input",
                [Atr("type", "checkbox"); Atr("id", "switchCharacter"); Atr("style", "position: absolute; margin-top: 6px; margin-left: 150px; z-index: 100"); Atr("onclick", "setCharacter()")]
                @ checkedAttribute)
            this.html.tagb ("label", [Atr("style", "position: absolute; margin-top: 0px; margin-left: 165px; z-index: 100")]) <| fun () ->
                this.BodyContext.writein "キャラクター"
        /// <summary>
        /// 字幕表示を制御するチェックボックスを生成
        /// </summary>
        member this.switchSubtitle() =
            let checkedAttribute = if this.SubtitleEnabled then [Atr("checked")] else []
            this.html.taga (
                "input",
                [Atr("type", "checkbox"); Atr("id", "switchSubtitle"); Atr("style", "position: absolute; margin-top: 6px; margin-left: 270px; z-index: 100"); Atr("onclick", "setSubtitle()")]
                @ checkedAttribute)
            this.html.tagb ("label", [Atr("style", "position: absolute; margin-top: 0px; margin-left: 285px; z-index: 100")]) <| fun () ->
                this.BodyContext.writein "字幕"
        /// <summary>
        /// 音声再生を制御するチェックボックスを生成
        /// </summary>
        member this.switchAudio() =
            let checkedAttribute = if this.VoiceEnabled then [Atr("checked")] else []
            this.html.taga (
                "input",
                [Atr("type", "checkbox"); Atr("id", "switchAudio"); Atr("style", "position: absolute; margin-top: 6px; margin-left: 330px; z-index: 100"); Atr("onclick", "setSubtitle()")]
                @ checkedAttribute)
            this.html.tagb ("label", [Atr("style", "position: absolute; margin-top: 0px; margin-left: 345px; z-index: 100")]) <| fun () ->
                this.BodyContext.writein "音声"
        /// Writes an audio player for the presentation.
        member this.audioPlayer() =
                this.html.tagb ("audio", [Atr("id", "audioPlayer")]) ignore
        /// <summary>
        /// 指定位置に画像を表示
        /// </summary>
        member this.imageA (s:Style) = fun (p:position) (filename:string) ->
            let s1 = Style [{Key = "margin-left"; Value = InvariantFormat.number p.x+"px";}
                            {Key = "margin-top"; Value = InvariantFormat.number p.y+"px";}
                            {Key = "position"; Value = "absolute";}]
            let sourceUrl = this.ImportAsset filename
            this.html.taga ("img", [(s1+s).atr; Atr("src", sourceUrl)])

/// <summary>
/// 図形アニメーションを管理するクラス
/// </summary>
/// <param name="figcounter">図形の識別番号</param>
/// <param name="originX, originY">描画の基準座標</param>
/// <param name="canvasX, canvasY">キャンパスのサイズ</param>
[<AutoOpen>]
module CompilationEnvironmentAnimationExtensions =
    /// Owns the contexts and assets for an HTML presentation.
    type HtmlGenerationContext with
        /// Gets the slide animation helper.
        member this.slideAnimation = ContextSlideAnimation(this)

