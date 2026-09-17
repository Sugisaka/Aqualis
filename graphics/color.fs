// 
// Copyright (c) 2026 Jun-ichiro Sugisaka
// 
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
// 
namespace Aqualis

    /// Color values and palettes for generated graphics.
    module color =
        
        /// <summary>Generates evenly spaced RGB colors around a six-part color wheel.</summary>
        /// <param name="n">Number of colors to generate.</param>
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
            
        /// Optional fill color, opacity, and RGB channels.
        type fill(col:(int0*int0*int0*double0) option) =
            /// Gets the optional red, green, blue, and opacity values.
            member __.col with get() = col
            /// Creates a solid RGB color with the supplied opacity and line width when applicable.
            static member RGB(r:int0,g:int0,b:int0,a:double0) = fill(Some(r, g, b, a))
            /// Creates a solid RGB color with the supplied opacity and line width when applicable.
            static member RGB(r:int0,g:int0,b:int0) = fill(Some(r, g, b,D 1.0))
            /// Creates a solid RGB color with the supplied opacity and line width when applicable.
            static member RGB(r:int,g:int,b:int,a:double) = fill.RGB(I r,I g,I b,D a)
            /// Creates a solid RGB color with the supplied opacity and line width when applicable.
            static member RGB(r:int,g:int,b:int) = fill.RGB(I r,I g,I b)
            /// Creates or gets a red fill or stroke.
            static member red with get() = fill.RGB(255, 0, 0)
            /// Creates or gets a green fill or stroke.
            static member green with get() = fill.RGB(0, 255, 0)
            /// Creates or gets a blue fill or stroke.
            static member blue with get() = fill.RGB(0, 0, 255)
            /// Creates or gets a black fill or stroke.
            static member black with get() = fill.RGB(0, 0, 0)
            /// Creates or gets a white fill or stroke.
            static member white with get() = fill.RGB(255, 255, 255)
            /// Creates or gets a magenta fill or stroke.
            static member magenta with get() = fill.RGB(255, 0, 255)
            /// Creates or gets a yellow fill or stroke.
            static member yellow with get() = fill.RGB(255, 255, 0)
            /// Creates or gets a cyan fill or stroke.
            static member cyan with get() = fill.RGB(0, 255, 255)
            /// Gets a fill or stroke that is not drawn.
            static member none with get() = fill(None)
            
        /// Optional stroke color, opacity, line width, and dash pattern.
        type stroke(collw:(int0*int0*int0*double0*double0*(double0 list)) option) =
            /// Gets the optional RGB, opacity, width, and dash values.
            member __.col with get() = collw
            /// Creates a solid RGB color with the supplied opacity and line width when applicable.
            static member RGB(r:int0,g:int0,b:int0,a:double0,lw:double0) = stroke(Some(r, g, b, a, lw,[]))
            /// Creates a solid RGB color with the supplied opacity and line width when applicable.
            static member RGB(r:int0,g:int0,b:int0,lw:double0) = stroke(Some(r, g, b, D 1.0, lw,[]))
            /// Creates a solid RGB color with the supplied opacity and line width when applicable.
            static member RGB(r:int,g:int,b:int,a:double,lw:double0) = stroke.RGB(I r,I g,I b, D a, lw)
            /// Creates a solid RGB color with the supplied opacity and line width when applicable.
            static member RGB(r:int,g:int,b:int,lw:double0) = stroke.RGB(I r,I g,I b, lw)
            /// Creates a solid RGB color with the supplied opacity and line width when applicable.
            static member RGB(r:int,g:int,b:int,a:double,lw:double) = stroke.RGB(I r, I g, I b, D a, D lw)
            /// Creates a solid RGB color with the supplied opacity and line width when applicable.
            static member RGB(r:int,g:int,b:int,lw:double) = stroke.RGB(I r,I g,I b,D lw)
            /// Creates a dashed RGB stroke with the supplied opacity, width, and dash lengths.
            static member dashRGB(r:int0,g:int0,b:int0,a:double0,lw:double0,da:double0 list) = stroke(Some(r, g, b, a, lw,da))
            /// Creates a dashed RGB stroke with the supplied opacity, width, and dash lengths.
            static member dashRGB(r:int0,g:int0,b:int0,lw:double0,da:double0 list) = stroke(Some(r, g, b, D 1.0, lw,da))
            /// Creates a dashed RGB stroke with the supplied opacity, width, and dash lengths.
            static member dashRGB(r:int,g:int,b:int,a:double,lw:double0,da:double0 list) = stroke.dashRGB(I r,I g,I b, D a, lw,da)
            /// Creates a dashed RGB stroke with the supplied opacity, width, and dash lengths.
            static member dashRGB(r:int,g:int,b:int,lw:double0,da:double0 list) = stroke.dashRGB(I r,I g,I b, lw,da)
            /// Creates a dashed RGB stroke with the supplied opacity, width, and dash lengths.
            static member dashRGB(r:int,g:int,b:int,a:double,lw:double,da:double list) = stroke.dashRGB(I r, I g, I b, D a, D lw,da |> List.map (fun x -> D x))
            /// Creates a dashed RGB stroke with the supplied opacity, width, and dash lengths.
            static member dashRGB(r:int,g:int,b:int,lw:double,da:double list) = stroke.dashRGB(I r,I g,I b,D lw,da |> List.map (fun x -> D x))
            /// Creates or gets a red fill or stroke.
            static member red(lw:double0) = stroke.RGB(255, 0, 0, lw)
            /// Creates or gets a red fill or stroke.
            static member red(lw:double) = stroke.RGB(255, 0, 0, lw)
            /// Creates or gets a green fill or stroke.
            static member green(lw:double0) = stroke.RGB(0, 255, 0, lw)
            /// Creates or gets a green fill or stroke.
            static member green(lw:double) = stroke.RGB(0, 255, 0, lw)
            /// Creates or gets a blue fill or stroke.
            static member blue(lw:double0) = stroke.RGB(0, 0, 255, lw)
            /// Creates or gets a blue fill or stroke.
            static member blue(lw:double) = stroke.RGB(0, 0, 255, lw)
            /// Creates or gets a black fill or stroke.
            static member black(lw:double0) = stroke.RGB(0, 0, 0, lw)
            /// Creates or gets a black fill or stroke.
            static member black(lw:double) = stroke.RGB(0, 0, 0, lw)
            /// Creates or gets a white fill or stroke.
            static member white(lw:double0) = stroke.RGB(255, 255, 255, lw)
            /// Creates or gets a white fill or stroke.
            static member white(lw:double) = stroke.RGB(255, 255, 255, lw)
            /// Creates or gets a magenta fill or stroke.
            static member magenta(lw:double0) = stroke.RGB(255, 0, 255, lw)
            /// Creates or gets a magenta fill or stroke.
            static member magenta(lw:double) = stroke.RGB(255, 0, 255, lw)
            /// Creates or gets a yellow fill or stroke.
            static member yellow(lw:double0) = stroke.RGB(255, 255, 0, lw)
            /// Creates or gets a yellow fill or stroke.
            static member yellow(lw:double) = stroke.RGB(255, 255, 0, lw)
            /// Creates or gets a cyan fill or stroke.
            static member cyan(lw:double0) = stroke.RGB(0, 255, 255, lw)
            /// Creates or gets a cyan fill or stroke.
            static member cyan(lw:double) = stroke.RGB(0, 255, 255, lw)
            /// Creates a dashed red stroke with the supplied width and dash lengths.
            static member dashred(lw:double0,da:double0 list) = stroke.dashRGB(255, 0, 0, lw, da)
            /// Creates a dashed red stroke with the supplied width and dash lengths.
            static member dashred(lw:double,da:double list) = stroke.dashRGB(255, 0, 0, lw, da)
            /// Creates a dashed green stroke with the supplied width and dash lengths.
            static member dashgreen(lw:double0,da:double0 list) = stroke.dashRGB(0, 255, 0, lw, da)
            /// Creates a dashed green stroke with the supplied width and dash lengths.
            static member dashgreen(lw:double,da:double list) = stroke.dashRGB(0, 255, 0, lw, da)
            /// Creates a dashed blue stroke with the supplied width and dash lengths.
            static member dashblue(lw:double0,da:double0 list) = stroke.dashRGB(0, 0, 255, lw, da)
            /// Creates a dashed blue stroke with the supplied width and dash lengths.
            static member dashblue(lw:double,da:double list) = stroke.dashRGB(0, 0, 255, lw, da)
            /// Creates a dashed black stroke with the supplied width and dash lengths.
            static member dashblack(lw:double0,da:double0 list) = stroke.dashRGB(0, 0, 0, lw, da)
            /// Creates a dashed black stroke with the supplied width and dash lengths.
            static member dashblack(lw:double,da:double list) = stroke.dashRGB(0, 0, 0, lw, da)
            /// Creates a dashed white stroke with the supplied width and dash lengths.
            static member dashwhite(lw:double0,da:double0 list) = stroke.dashRGB(255, 255, 255, lw, da)
            /// Creates a dashed white stroke with the supplied width and dash lengths.
            static member dashwhite(lw:double,da:double list) = stroke.dashRGB(255, 255, 255, lw, da)
            /// Creates a dashed magenta stroke with the supplied width and dash lengths.
            static member dashmagenta(lw:double0,da:double0 list) = stroke.dashRGB(255, 0, 255, lw, da)
            /// Creates a dashed magenta stroke with the supplied width and dash lengths.
            static member dashmagenta(lw:double,da:double list) = stroke.dashRGB(255, 0, 255, lw, da)
            /// Creates a dashed yellow stroke with the supplied width and dash lengths.
            static member dashyellow(lw:double0,da:double0 list) = stroke.dashRGB(255, 255, 0, lw, da)
            /// Creates a dashed yellow stroke with the supplied width and dash lengths.
            static member dashyellow(lw:double,da:double list) = stroke.dashRGB(255, 255, 0, lw, da)
            /// Creates a dashed cyan stroke with the supplied width and dash lengths.
            static member dashcyan(lw:double0,da:double0 list) = stroke.dashRGB(0, 255, 255, lw, da)
            /// Creates a dashed cyan stroke with the supplied width and dash lengths.
            static member dashcyan(lw:double,da:double list) = stroke.dashRGB(0, 255, 255, lw, da)
            /// Gets a fill or stroke that is not drawn.
            static member none with get() = stroke(None)
            /// Converts this stroke's color and opacity to a fill value.
            member this.tofill with get() =
                match collw with
                  |Some(r, g, b, a, _, _) -> fill.RGB(r,g,b,a)
                  |None -> fill.none
