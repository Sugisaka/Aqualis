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

type ContextSlideAnimation internal (context:HtmlGenerationContext) =
    /// <summary>
    /// 登録された音声ファイルの一覧を書きだす
    /// </summary>
    member this.writeAudioList() =
        context.switchJSMain <| fun ctx ->
            let audioFilesJson = JsonSerializer.Serialize(context.AudioFiles)
            ctx.writein ("const audioList = " + audioFilesJson + ";")
    /// <summary>
    /// キャラクター表示を制御するJavaScriptコードの生成
    /// </summary>
    member this.jsSetCharacter() =
        context.switchJSMain <| fun ctx ->
            ctx.writein "let pagecount = 1;"
            ctx.writein "function setCharacter()"
            ctx.writein "{"
            ctx.writein "        const swc = document.getElementById(\"switchCharacter\");"
            ctx.writein "        const c = document.getElementById(\"c\"+pagecount);"
            ctx.writein "        if(swc.checked)"
            ctx.writein "        {"
            ctx.writein "            c.style.display = \"block\";"
            ctx.writein "        }"
            ctx.writein "        else"
            ctx.writein "        {"
            ctx.writein "            c.style.display = \"none\";"
            ctx.writein "        }"
            ctx.writein "}"
    /// <summary>
    /// 字幕表示を制御するJavaScriptコードの生成
    /// </summary>
    member this.jsSetSubtitle() =
        context.switchJSMain <| fun ctx ->
            ctx.writein "function setSubtitle()"
            ctx.writein "{"
            ctx.writein "        const sws = document.getElementById(\"switchSubtitle\");"
            ctx.writein "        const b2 = document.getElementById(\"sb\"+pagecount);"
            ctx.writein "        const s2 = document.getElementById(\"s\"+pagecount);"
            ctx.writein "        if(sws.checked)"
            ctx.writein "        {"
            ctx.writein "            b2.style.display = \"block\";"
            ctx.writein "            s2.style.display = \"block\";"
            ctx.writein "        }"
            ctx.writein "        else"
            ctx.writein "        {"
            ctx.writein "            b2.style.display = \"none\";"
            ctx.writein "            s2.style.display = \"none\";"
            ctx.writein "        }"
            ctx.writein "}"
    /// <summary>
    /// 次のページへの遷移を制御するJavaScriptコードの生成
    /// </summary>
    member this.jsDrawNext(audioDir:string) =
        context.switchJSMain <| fun ctx ->
            let animationCount = context.AnimationCount
            let audioDirectoryLiteral = JsonSerializer.Serialize(audioDir + "/")
            ctx.writein "function drawNext()"
            ctx.writein "{"
            ctx.writein "    resetAll();"
            ctx.writein ("    if(pagecount<"+InvariantFormat.integer animationCount+")")
            ctx.writein "    {"
            ctx.writein "        const swc = document.getElementById(\"switchCharacter\");"
            ctx.writein "        const sws = document.getElementById(\"switchSubtitle\");"
            ctx.writein "        const swa = document.getElementById(\"switchAudio\");"
            ctx.writein "        "
            ctx.writein "        const p1 = document.getElementById(\"p\"+pagecount);"
            ctx.writein "        p1.style.display = \"none\";"
            ctx.writein "        const b1 = document.getElementById(\"sb\"+pagecount);"
            ctx.writein "        b1.style.display = \"none\";"
            ctx.writein "        const s1 = document.getElementById(\"s\"+pagecount);"
            ctx.writein "        s1.style.display = \"none\";"
            ctx.writein "        const c1 = document.getElementById(\"c\"+pagecount);"
            ctx.writein "        c1.style.display = \"none\";"
            ctx.writein "        pagecount++;"
            ctx.writein "        const p2 = document.getElementById(\"p\"+pagecount);"
            ctx.writein "        p2.style.display = \"block\";"
            ctx.writein "        if(sws.checked)"
            ctx.writein "        {"
            ctx.writein "            const b2 = document.getElementById(\"sb\"+pagecount);"
            ctx.writein "            b2.style.display = \"block\";"
            ctx.writein "            const s2 = document.getElementById(\"s\"+pagecount);"
            ctx.writein "            s2.style.display = \"block\";"
            ctx.writein "        }"
            ctx.writein "        else"
            ctx.writein "        {"
            ctx.writein "            const b2 = document.getElementById(\"sb\"+pagecount);"
            ctx.writein "            b2.style.display = \"none\";"
            ctx.writein "            const s2 = document.getElementById(\"s\"+pagecount);"
            ctx.writein "            s2.style.display = \"none\";"
            ctx.writein "        }"
            ctx.writein "        if(swc.checked)"
            ctx.writein "        {"
            ctx.writein "            const c2 = document.getElementById(\"c\"+pagecount);"
            ctx.writein "            c2.style.display = \"block\";"
            ctx.writein "        }"
            ctx.writein "        else"
            ctx.writein "        {"
            ctx.writein "            const c2 = document.getElementById(\"c\"+pagecount);"
            ctx.writein "            c2.style.display = \"none\";"
            ctx.writein "        }"
            ctx.writein "        const audioPlayer = document.getElementById(\"audioPlayer\");"
            ctx.writein "        if(audioList[pagecount-1] != \"\" && swa.checked)"
            ctx.writein "        {"
            ctx.writein ("            audioPlayer.src = " + audioDirectoryLiteral + " + audioList[pagecount-1];")
            ctx.writein "            audioPlayer.play();"
            ctx.writein "        }"
            ctx.writein "        autoAnimationMap['page'+pagecount]();"
            ctx.writein "    }"
            ctx.writein "}"
    /// <summary>
    /// 前のページへの遷移を制御するJavaScriptコードの生成
    /// </summary>
    member this.jsDrawPrev(audioDir:string) =
        context.switchJSMain <| fun ctx ->
            let audioDirectoryLiteral = JsonSerializer.Serialize(audioDir + "/")
            ctx.writein "function drawPrev()"
            ctx.writein "{"
            ctx.writein "    resetAll();"
            ctx.writein "    if(pagecount>1)"
            ctx.writein "    {"
            ctx.writein "        const swc = document.getElementById(\"switchCharacter\");"
            ctx.writein "        const sws = document.getElementById(\"switchSubtitle\");"
            ctx.writein "        const swa = document.getElementById(\"switchAudio\");"
            ctx.writein "        const p1 = document.getElementById(\"p\"+pagecount);"
            ctx.writein "        p1.style.display = \"none\";"
            ctx.writein "        const b1 = document.getElementById(\"sb\"+pagecount);"
            ctx.writein "        b1.style.display = \"none\";"
            ctx.writein "        const s1 = document.getElementById(\"s\"+pagecount);"
            ctx.writein "        s1.style.display = \"none\";"
            ctx.writein "        const c1 = document.getElementById(\"c\"+pagecount);"
            ctx.writein "        c1.style.display = \"none\";"
            ctx.writein "        pagecount--;"
            ctx.writein "        const p2 = document.getElementById(\"p\"+pagecount);"
            ctx.writein "        p2.style.display = \"block\";"
            ctx.writein "        if(sws.checked)"
            ctx.writein "        {"
            ctx.writein "            const b2 = document.getElementById(\"sb\"+pagecount);"
            ctx.writein "            b2.style.display = \"block\";"
            ctx.writein "            const s2 = document.getElementById(\"s\"+pagecount);"
            ctx.writein "            s2.style.display = \"block\";"
            ctx.writein "        }"
            ctx.writein "        else"
            ctx.writein "        {"
            ctx.writein "            const b2 = document.getElementById(\"sb\"+pagecount);"
            ctx.writein "            b2.style.display = \"none\";"
            ctx.writein "            const s2 = document.getElementById(\"s\"+pagecount);"
            ctx.writein "            s2.style.display = \"none\";"
            ctx.writein "        }"
            ctx.writein "        if(swc.checked)"
            ctx.writein "        {"
            ctx.writein "            const c2 = document.getElementById(\"c\"+pagecount);"
            ctx.writein "            c2.style.display = \"block\";"
            ctx.writein "        }"
            ctx.writein "        else"
            ctx.writein "        {"
            ctx.writein "            const c2 = document.getElementById(\"c\"+pagecount);"
            ctx.writein "            c2.style.display = \"none\";"
            ctx.writein "        }"
            ctx.writein "        const audioPlayer = document.getElementById(\"audioPlayer\");"
            ctx.writein "        if(audioList[pagecount-1] != \"\" && swa.checked)"
            ctx.writein "        {"
            ctx.writein ("            audioPlayer.src = " + audioDirectoryLiteral + " + audioList[pagecount-1];")
            ctx.writein "            audioPlayer.play();"
            ctx.writein "        }"
            ctx.writein "    }"
            ctx.writein "}"

    /// キャラクターのデフォルト表示・非表示設定
    /// 字幕のデフォルト表示・非表示設定
    /// 音声のデフォルト表示・非表示設定
    /// デフォルトの設定

