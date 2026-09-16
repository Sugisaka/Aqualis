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
module dochtml =
    let private htmlpresentationCore
        (dir:string)
        (filename:string)
        (title:string)
        (cssfile:option<Url>)
        (pagesizeX:option<int>,pagesizeY:option<int>)
        isPageAnimation
        code =
        // ディレクトリ作成
        // コンテンツディレクトリ
        use context = new HtmlGenerationContext(dir, filename)
        context.switchJSAnimationStart <| fun ctx ->
            ctx.writein "const animationStartMap = {"
        context.switchJSAnimationReset <| fun ctx ->
            ctx.writein "const animationResetMap = {"
        context.switchAutoAnimation <| fun ctx ->
            ctx.writein "const autoAnimationMap = {"
        context.switchAnimationSeq <| fun ctx ->
            ctx.writein "function repeatSeq(fn, interval, Nt, onComplete)"
            ctx.writein "{"
            ctx.writein "    let t = 0;"
            ctx.writein "    function run()"
            ctx.writein "    {"
            ctx.writein "        if (t < Nt)"
            ctx.writein "        {"
            ctx.writein "            fn(t);"
            ctx.writein "            t++;"
            ctx.writein "            setTimeout(run, interval);"
            ctx.writein "        }"
            ctx.writein "        else"
            ctx.writein "        {"
            ctx.writein "            onComplete();"
            ctx.writein "        }"
            ctx.writein "    }"
            ctx.writein "    run();"
            ctx.writein "}"
            ctx.writein "function repeat(fn, interval, Nt)"
            ctx.writein "{"
            ctx.writein "    let t = 0;"
            ctx.writein "    function run()"
            ctx.writein "    {"
            ctx.writein "        if(t == Nt)"
            ctx.writein "        {"
            ctx.writein "            t = 0;"
            ctx.writein "        }"
            ctx.writein "        fn(t);"
            ctx.writein "        t++;"
            ctx.writein "        setTimeout(run, interval);"
            ctx.writein "    }"
            ctx.writein "    run();"
            ctx.writein "}"
        code context
        if isPageAnimation then
            context.slideAnimation.writeAudioList()
            context.slideAnimation.jsSetCharacter()
            context.slideAnimation.jsSetSubtitle()
            context.slideAnimation.jsDrawNext(context.ContentsUrlPrefix)
            context.slideAnimation.jsDrawPrev(context.ContentsUrlPrefix)
        // head、body要素書き込みストリームを閉じてhead、body要素のコード取得
        let codeDraw = context.switchJSMain <| fun ctx ->
            ctx.allCodes
        let codeBody = context.switchBody <| fun ctx ->
            ctx.allCodes
        // html書き込みストリーム作成
        context.switchMain <| fun ctx ->
            ctx.writein "<!DOCTYPE html>"
            // html要素
            ctx.html.tagb ("html", [Atr("lang", "ja")]) <| fun () ->
                // head要素
                ctx.html.tagb "head" <| fun () ->
                    // titleタグ
                    ctx.html.tagb "title" <| fun () ->
                        ctx.writein (HtmlEncoding.textContent title)
                    // metaタグ
                    ctx.writein "<meta charset=\"UTF-8\">"
                    //追加（5/29）viewportタブ
                    match pagesizeX with
                    |None ->
                        ctx.writein "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0, maximum-scale=1.0, minimum-scale=1.0\">"
                    |Some width ->
                        ctx.writein ("<meta name=\"viewport\" content=\"width=" + InvariantFormat.integer width + "\">")
                    HtmlAssetRendering.write "" ctx.writein context.BodyContext.htmlAssets
                    for asset in
                        ["animationSeq.js"; "animationSeqReset.js"; "animationStart.js"; "animationReset.js"; "autoAnimation.js"] do
                        ctx.html.tagb (
                            "script",
                            [Atr("type", "text/javascript"); Atr("src", context.AssetUrl(asset))]) ignore
                    // scriptタグ
                    ctx.html.tagb "script" <| fun () ->
                        match codeDraw with |Some s -> ctx.writein s |None -> ()
                    match cssfile with
                    | Some stylesheet -> ctx.html.taga ("link", [Atr("rel", "stylesheet"); Atr("href", Url.value stylesheet)])
                    | None -> ()
                // body要素
                match pagesizeX,pagesizeY with
                |None,None ->
                    let s0 = Style [area.backGroundColor "#ffffff"]
                    ctx.html.tagb ("body", [s0.atr]) <| fun () ->
                        match codeBody with |Some s -> ctx.writein s |None -> ()
                |Some x,None ->
                    let s0 = Style [area.backGroundColor "#aaaaaa"]
                    ctx.html.tagb ("body", [s0.atr]) <| fun () ->
                        let s1 = Style [
                            area.backGroundColor "#ffffff"
                            margin.left "auto"
                            margin.right "auto"
                            size.width (CssLength.pixelsInt x)]
                        ctx.html.tagb ("div", [s1.atr]) <| fun () ->
                            match codeBody with |Some s -> ctx.writein s |None -> ()
                |None,Some y->
                    let s0 = Style [area.backGroundColor "#aaaaaa"]
                    ctx.html.tagb ("body", [s0.atr]) <| fun () ->
                        let s1 = Style [
                            area.backGroundColor "#ffffff"
                            margin.left "auto"
                            margin.right "auto"
                            size.height (CssLength.pixelsInt y)]
                        ctx.html.tagb ("div", [s1.atr]) <| fun () ->
                            match codeBody with |Some s -> ctx.writein s |None -> ()
                |Some x,Some y ->
                    let s0 = Style [area.backGroundColor "#aaaaaa"]
                    ctx.html.tagb ("body", [s0.atr]) <| fun () ->
                        let s1 = Style [
                            area.backGroundColor "#ffffff"
                            margin.left "auto"
                            margin.right "auto"
                            size.width (CssLength.pixelsInt x)
                            size.height (CssLength.pixelsInt y)]
                        ctx.html.tagb ("div", [s1.atr]) <| fun () ->
                            match codeBody with |Some s -> ctx.writein s |None -> ()

                context.switchJSAnimationStart <| fun ctx ->
                    ctx.writein "test: () => {}"
                    ctx.writein "};"
                context.switchJSAnimationReset <| fun ctx ->
                    ctx.writein "test: () => {}"
                    ctx.writein "};"
                    ctx.writein ""
                    ctx.writein "function resetAll(){"
                    ctx.writein "    for (const key in animationResetMap) {"
                    ctx.writein "        if (typeof animationResetMap[key] === \"function\") {"
                    ctx.writein "            animationResetMap[key]();"
                    ctx.writein "        }"
                    ctx.writein "    }"
                    ctx.writein "}"
                context.switchAutoAnimation <| fun ctx ->
                    ctx.writein "test: () => {}"
                    ctx.writein "};"
                // bodyタグ一時コード削除
                context.switchBody <| fun c -> c.delete()
                // JavaScript関数一時コード削除
                context.switchJSMain <| fun c -> c.delete()

    /// 全体がキャンバスの無制限レイアウト
    let htmlpresentation
        (dir:string)
        (filename:string)
        (title:string)
        (cssfile:option<Url>)
        pagesize
        isPageAnimation
        code =
        htmlpresentationCore
            dir
            filename
            title
            cssfile
            pagesize
            isPageAnimation
            code

    let freeCanvas outputdir filename (title:string) cssfile code =
        htmlpresentation outputdir filename title cssfile (None, None) false <| fun ctx ->
            ctx.html.canvas <| Style [size.width "0px"; size.height "0px"] <| fun () -> code ctx

    /// 全体がキャンバスの無制限レイアウト
    let freePage outputdir filename (title:string) cssfile code =
        htmlpresentation outputdir filename title cssfile (None, None) false code

    /// 固定幅レイアウト
    let fixedWidthPage outputdir filename (title:string) pageWidth cssfile code =
        htmlpresentation outputdir filename title cssfile (Some pageWidth, None) false code

    let fixedPage outputdir filename (title:string) pageWidth pageHeight cssfile code =
        htmlpresentationCore outputdir filename title cssfile (Some pageWidth, Some pageHeight) true <| fun ctx ->
            code ctx
            ctx.prevButton()
            ctx.nextButton()
            ctx.switchCharacter()
            ctx.switchSubtitle()
            ctx.switchAudio()
            ctx.audioPlayer()

