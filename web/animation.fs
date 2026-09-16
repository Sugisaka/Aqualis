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
module htmlexpr2 =
    type HtmlGenerationContext with
        
        member this.html = html this.BodyContext
        
        /// <summary>
        /// 手動操作型のアニメーション領域を生成
        /// </summary>
        /// <param name="s">アニメーションの領域設定</param>
        /// <param name="p">表示位置</param>
        /// <param name="buttonX, buttonY">操作ボタンの配置座標</param>
        member this.animationManual (s:ViewBoxStyle) (p:position) (buttonX:int,buttonY:int) code =
            let f =
                FigureAnimation(
                    this, this.NextFigureNumber(),
                    s.mX,s.mY,s.sX,s.sY)
            this.switchBody <| fun ctx ->
                ctx.writein  ("<svg viewBox=\"0 0 "+InvariantFormat.integer s.sX+" "+InvariantFormat.integer s.sY+"\" ")
                ctx.writein  ("width=\""+CssLength.pixelsInt s.sX+"\" ")
                ctx.writein  ("height=\""+CssLength.pixelsInt s.sY+"\" ")
                ctx.writein  "xmlns=\"http://www.w3.org/2000/svg\" "
                ctx.writein  ("style=\"margin-left: "+CssLength.pixelsInt s.mX+"; ")
                ctx.writein  ("margin-top: "+CssLength.pixelsInt s.mY+"; ")
                ctx.writein  "position: absolute;"
                ctx.writein  ("background-color: "+s.backgroundColor+";")
                ctx.writein  "\">"
                code(f,p)
                ctx.writein  "</svg>"
            let asc = this.nextAnimationGroup()
            let fnameStart = f.jsStartControll asc
            let fnameReset = f.jsResetControll asc
            this.addAnimationButton(fnameStart,fnameReset,buttonX,buttonY)

        /// <summary>
        /// 自動再生型のアニメーション領域を生成する
        /// </summary>
        member this.animationAuto (s:ViewBoxStyle) (p:position) code =
            let f =
                FigureAnimation(
                    this, this.NextFigureNumber(),
                    s.mX,s.mY,s.sX,s.sY)
            this.switchBody <| fun ctx ->
                ctx.writein  ("<svg viewBox=\"0 0 "+InvariantFormat.integer s.sX+" "+InvariantFormat.integer s.sY+"\" ")
                ctx.writein  ("width=\""+CssLength.pixelsInt s.sX+"\" ")
                ctx.writein  ("height=\""+CssLength.pixelsInt s.sY+"\" ")
                ctx.writein  "xmlns=\"http://www.w3.org/2000/svg\" "
                ctx.writein  ("style=\"margin-left: "+CssLength.pixelsInt s.mX+"; ")
                ctx.writein  ("margin-top: "+CssLength.pixelsInt s.mY+"; ")
                ctx.writein  "position: absolute;"
                ctx.writein  ("background-color: "+s.backgroundColor+";")
                ctx.writein  "\">"
                code(f,p)
                ctx.writein  "</svg>"
            let asc = this.nextAnimationGroup()
            let fnameStart = f.jsStartControll asc
            let fnameReset = f.jsResetControll asc
            this.addAutoAnimation(fnameStart,fnameReset)
