// 
// Copyright (c) 2026 Jun-ichiro Sugisaka
// 
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
// 
namespace Aqualis

    module color =
        
        /// <summary>
        /// 指定した数だけRGBカラーを生成
        /// </summary>
        /// <param name="n"></param>
        let cyclic_color (n:int) =
            let rd (x:double) = int(floor(x+0.5))
            [for i in 0..n-1 do
                let x = 6.0*(double i)/(double n)
                if 0.0<=x && x<1.0 then
                    let y = x-0.0
                    yield (rd(255.0), rd(255.0*y), rd(0.0))
                elif 1.0<=x && x<2.0 then
                    let y = x-1.0
                    yield (rd(255.0-255.0*y), rd(255.0), rd(0.0))
                elif 2.0<=x && x<3.0 then
                    let y = x-2.0
                    yield (rd(0.0), rd(255.0), rd(255.0*y))
                elif 3.0<=x && x<4.0 then
                    let y = x-3.0
                    yield (rd(0.0), rd(255.0-255.0*y), rd(255.0))
                elif 4.0<=x && x<5.0 then
                    let y = x-4.0
                    yield (rd(255.0*y), rd(0.0), rd(255.0))
                else
                    let y = x-5.0
                    yield (rd(255.0), rd(0.0), rd(255.0-255.0*y))
            ]
            
        /// <summary>塗り色情報</summary>
        type fill(col:(num0*num0*num0*num0) option) =
            member __.col with get() = col
            static member RGB(r:num0,g:num0,b:num0,a:num0) = fill(Some(r, g, b, a))
            static member RGB(r:num0,g:num0,b:num0) = fill(Some(r, g, b,D 1.0))
            static member RGB(r:int,g:int,b:int,a:double) = fill.RGB(I r,I g,I b,D a)
            static member RGB(r:int,g:int,b:int) = fill.RGB(I r,I g,I b)
            static member red with get() = fill.RGB(255, 0, 0)
            static member green with get() = fill.RGB(0, 255, 0)
            static member blue with get() = fill.RGB(0, 0, 255)
            static member black with get() = fill.RGB(0, 0, 0)
            static member white with get() = fill.RGB(255, 255, 255)
            static member magenta with get() = fill.RGB(255, 0, 255)
            static member yellow with get() = fill.RGB(255, 255, 0)
            static member cyan with get() = fill.RGB(0, 255, 255)
            static member none with get() = fill(None)
            
        /// <summary>線色情報</summary>
        type stroke(collw:(num0*num0*num0*num0*num0*(num0 list)) option) =
            member __.col with get() = collw
            static member RGB(r:num0,g:num0,b:num0,a:num0,lw:num0) = stroke(Some(r, g, b, a, lw,[]))
            static member RGB(r:num0,g:num0,b:num0,lw:num0) = stroke(Some(r, g, b, D 1.0, lw,[]))
            static member RGB(r:int,g:int,b:int,a:double,lw:num0) = stroke.RGB(I r,I g,I b, D a, lw)
            static member RGB(r:int,g:int,b:int,lw:num0) = stroke.RGB(I r,I g,I b, lw)
            static member RGB(r:int,g:int,b:int,a:double,lw:double) = stroke.RGB(I r, I g, I b, D a, D lw)
            static member RGB(r:int,g:int,b:int,lw:double) = stroke.RGB(I r,I g,I b,D lw)
            static member dashRGB(r:num0,g:num0,b:num0,a:num0,lw:num0,da:num0 list) = stroke(Some(r, g, b, a, lw,da))
            static member dashRGB(r:num0,g:num0,b:num0,lw:num0,da:num0 list) = stroke(Some(r, g, b, D 1.0, lw,da))
            static member dashRGB(r:int,g:int,b:int,a:double,lw:num0,da:num0 list) = stroke.dashRGB(I r,I g,I b, D a, lw,da)
            static member dashRGB(r:int,g:int,b:int,lw:num0,da:num0 list) = stroke.dashRGB(I r,I g,I b, lw,da)
            static member dashRGB(r:int,g:int,b:int,a:double,lw:double,da:double list) = stroke.dashRGB(I r, I g, I b, D a, D lw,da |> List.map (fun x -> D x))
            static member dashRGB(r:int,g:int,b:int,lw:double,da:double list) = stroke.dashRGB(I r,I g,I b,D lw,da |> List.map (fun x -> D x))
            static member red(lw:num0) = stroke.RGB(255, 0, 0, lw)
            static member red(lw:double) = stroke.RGB(255, 0, 0, lw)
            static member green(lw:num0) = stroke.RGB(0, 255, 0, lw)
            static member green(lw:double) = stroke.RGB(0, 255, 0, lw)
            static member blue(lw:num0) = stroke.RGB(0, 0, 255, lw)
            static member blue(lw:double) = stroke.RGB(0, 0, 255, lw)
            static member black(lw:num0) = stroke.RGB(0, 0, 0, lw)
            static member black(lw:double) = stroke.RGB(0, 0, 0, lw)
            static member white(lw:num0) = stroke.RGB(255, 255, 255, lw)
            static member white(lw:double) = stroke.RGB(255, 255, 255, lw)
            static member magenta(lw:num0) = stroke.RGB(255, 0, 255, lw)
            static member magenta(lw:double) = stroke.RGB(255, 0, 255, lw)
            static member yellow(lw:num0) = stroke.RGB(255, 255, 0, lw)
            static member yellow(lw:double) = stroke.RGB(255, 255, 0, lw)
            static member cyan(lw:num0) = stroke.RGB(0, 255, 255, lw)
            static member cyan(lw:double) = stroke.RGB(0, 255, 255, lw)
            static member dashred(lw:num0,da:num0 list) = stroke.dashRGB(255, 0, 0, lw, da)
            static member dashred(lw:double,da:double list) = stroke.dashRGB(255, 0, 0, lw, da)
            static member dashgreen(lw:num0,da:num0 list) = stroke.dashRGB(0, 255, 0, lw, da)
            static member dashgreen(lw:double,da:double list) = stroke.dashRGB(0, 255, 0, lw, da)
            static member dashblue(lw:num0,da:num0 list) = stroke.dashRGB(0, 0, 255, lw, da)
            static member dashblue(lw:double,da:double list) = stroke.dashRGB(0, 0, 255, lw, da)
            static member dashblack(lw:num0,da:num0 list) = stroke.dashRGB(0, 0, 0, lw, da)
            static member dashblack(lw:double,da:double list) = stroke.dashRGB(0, 0, 0, lw, da)
            static member dashwhite(lw:num0,da:num0 list) = stroke.dashRGB(255, 255, 255, lw, da)
            static member dashwhite(lw:double,da:double list) = stroke.dashRGB(255, 255, 255, lw, da)
            static member dashmagenta(lw:num0,da:num0 list) = stroke.dashRGB(255, 0, 255, lw, da)
            static member dashmagenta(lw:double,da:double list) = stroke.dashRGB(255, 0, 255, lw, da)
            static member dashyellow(lw:num0,da:num0 list) = stroke.dashRGB(255, 255, 0, lw, da)
            static member dashyellow(lw:double,da:double list) = stroke.dashRGB(255, 255, 0, lw, da)
            static member dashcyan(lw:num0,da:num0 list) = stroke.dashRGB(0, 255, 255, lw, da)
            static member dashcyan(lw:double,da:double list) = stroke.dashRGB(0, 255, 255, lw, da)
            static member none with get() = stroke(None)
            member this.tofill with get() =
                match collw with
                  |Some(r, g, b, a, _, _) -> fill.RGB(r,g,b,a)
                  |None -> fill.none
