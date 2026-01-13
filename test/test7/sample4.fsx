//#############################################################################
// プレゼンテーションHTMLテストD
let projectname = "test7D"
let version = "1.0.0"
//#############################################################################

let outputdir = __SOURCE_DIRECTORY__

#I @"..\..\bin\Debug\net10.0"
#r "Aqualis.dll"

open System
open Aqualis

type AnimationLine0(s:Style,canvasX:int,canvasY:int) =
    let id = nextContentsID()
    let s0 = Style ([{Key="visibility";Value="hidden"}]@s.list)
    let s1 = Style ([{Key="visibility";Value="visible"}]@s.list)
    do
        html.taga ("line", [Atr("id",id);]@[s0.atr])
    member this.ID with get() = id
    member this.P (f:Line) = 
        let t = num0(Var(Dt,"t",NaN))
        switchAnimationSeq <| fun () ->
            writein("    var e = document.getElementById(\""+id+"\");")
            writein("    var x1 = " + (f.Start.X t).code + ";")
            writein("    var y1 = " + (canvasY - f.Start.Y t).code + ";")
            writein("    var x2 = " + (f.End.X t).code + ";")
            writein("    var y2 = " + (canvasY - f.End.Y t).code + ";")
            writein("    e.setAttribute(\"style\"," + "\"" + s1.code0 + "\");")
            writein "    e.setAttribute(\"x1\", x1);"
            writein "    e.setAttribute(\"y1\", y1);"
            writein "    e.setAttribute(\"x2\", x2);"
            writein "    e.setAttribute(\"y2\", y2);"
        switchJSAnimationSeqReset <| fun () ->
            writein("    var e = document.getElementById(\""+id+"\");")
            writein("    e.setAttribute(\"style\"," + "\"" + s0.code0 + "\");")

fixedPage outputdir projectname projectname 1920 1080 {Character=OFF; Subtitle=OFF; Voice=OFF} None <| fun () ->
    html.page
        [Tale, @"C:\home\contents\テール右斜AAA-.png";
         Dang, @"C:\home\contents\dango.png";]
        {Speaker = Dang;
         Source = AudioFile @"C:\home\contents\008-scripts_台詞_D.wav"
         Subtitle = Serif "だんごのセリフ"}
        <| fun p -> html.image (Style [size.width "100%";],p) @"C:\home\contents/title.PNG"
    html.page
        [Tale, @"C:\home\contents\テール右斜AAA-.png";
         Dang, @"C:\home\contents\dango.png";]
        {Speaker = Tale;
         Source = AudioFile @"C:\home\contents\0076-scripts_台詞_T.wav"
         Subtitle = Serif "テールのセリフ"}
        <| fun p ->
            html.textA Style[] (p+position(1860,10)) 48 "#000000" "10"
            html.contents Style[] (p+position(50,50)) "アニメーション制御の実装1"
            html.subtitle1 Style[] (p+position(80,150)) "サンプルページ1"
            html.subtitle2 Style[] (p+position(100,250)) "テキストと数式の表示"
            html.textA Style[] (p+position(100,320)) 30 "#000000" "AAA"
            ch.D "x" <| fun x ->
                ch.D "y" <| fun y ->
                    html.eqA Style[] (p+position(180,320)) 30 "#000000" "x+y"
                    html.eqB Style[] (p+position(280,320)) 30 "#FF0000" <| x-y
                    html.eqD Style[] (p+position(180,390)) 30 "#000000" 
                        [asm.sin(x+y);
                        x-y]
            html.subtitle2 Style[] (p+position(100,520)) "アニメーション1"
            html.textA Style[] (p+position(100,590)) 30 "#000000" "startボタンを押すとアニメーションが開始する"
            html.animationManual {sX=700; sY=780; mX=1140; mY=250; backgroundColor="#bbeeff"} p (1080,250) <| fun (f,p) ->
                let line1 = f.animationLine Style[stroke.width 3.0; stroke.dash "4,4"; stroke.color "#000000"]
                let elps1 = f.animationArc Style[stroke.width 3.0; stroke.color "#000000"; stroke.fill "none";]
                f.ellipse Style[stroke.width 3.0; stroke.color "#ff0000"; stroke.fill "none";] (position(200.0,400.0)) (200, 100)
                f.circle Style[stroke.width 3.0; stroke.color "#ff8800"; stroke.fill "none";] (position(400.0,400.0)) 50
                f.rect Style[stroke.width 3.0; stroke.color "#0088ff"; stroke.fill "none";] (position(400.0,200.0)) (50,100)
                f.ellipseArc Style[stroke.width 3.0; stroke.color "#00bb00"; stroke.fill "none";] (position(200.0,200.0)) (200, 200) (-Math.PI*0.5, Math.PI*1.2)
                f.polygon Style[stroke.width 3.0; stroke.color "#ff00ff"; stroke.fill "none";] [position(200.0,200.0);position(300.0,300.0);position(300.0,200.0)]
                f.polyline Style[stroke.width 3.0; stroke.color "#aa00ff"; stroke.fill "none";] [position(200.0,600.0);position(300.0,700.0);position(300.0,600.0)]
                //f.image Style[] (position(0.0,0.0)) @"C:\home\contents\アルミAAA.png"
                ch.D "t" <| fun t ->
                    f.eq Style[] (position(0.0,40.0)) (asm.sin t)
                f.text Style[] (position(0.0,80.0)) "ABC"
                /// 中心座標
                let cx,cy = I 350, I 390
                let constCenter = { X = (fun _ -> cx); Y = fun _ -> cy }
                /// 円弧の半径
                let R = D 198.0
                // 中心から右に破線描画
                f.seq {FrameTime=6; FrameNumber=100} <| fun s ->
                    line1.P {
                        Start = constCenter
                        End = {
                            X = fun t -> cx + R*t/(s.FrameNumber-1)
                            Y = fun _ -> cy }}
                // 円弧描画
                f.seq {FrameTime=6; FrameNumber=100} <| fun s ->
                    elps1.P {
                        center = constCenter
                        angle1 = fun _ -> I 0
                        angle2 = fun t -> 360*t/(s.FrameNumber-1)
                        radius = fun _ -> R }
    html.page
        [Tale, @"C:\home\contents\テール右斜AAA-.png";
         Dang, @"C:\home\contents\dango.png";]
        {Speaker = Tale;
         Source = AudioFile @"C:\home\contents\0076-scripts_台詞_T.wav"
         Subtitle = Serif "テールのセリフ"}
        <| fun p ->
            html.textA Style[] (p+position(1860,10)) 48 "#000000" "10"
            html.contents Style[] (p+position(50,50)) "アニメーション制御の実装2"
            html.subtitle1 Style[] (p+position(80,150)) "アニメーション2"
            html.textA Style[] (p+position(100,320)) 30 "#000000" "startボタンを押すとアニメーションが無限ループする"
            html.animationManual {sX=700; sY=780; mX=1140; mY=250; backgroundColor="#bbeeff"} p (1080,250) <| fun (f,p) ->
                /// 中心座標
                let cx,cy = 350.0, 390.0
                /// 円弧の半径
                let R = 198.0
                f.circle Style[stroke.width 3.0; stroke.color "#ff8800"; stroke.fill "none";] (position(cx,cy)) R
                let circ1 = f.animationEllipse Style[stroke.width 3.0; stroke.color "none"; stroke.fill "#00aa00";]
                // 円軌道上を移動
                f.loop {FrameTime=20; FrameNumber=100} <| fun s ->
                    circ1.P {
                        center = {
                            X = fun t -> cx + R*asm.cos(2*asm.pi*t/s.FrameNumber)
                            Y = fun t -> cy + R*asm.sin(2*asm.pi*t/s.FrameNumber)}
                        radiusX = fun _ -> I 10
                        radiusY = fun _ -> I 10}
    html.page
        [Tale, @"C:\home\contents\テール右斜AAA-.png";
         Dang, @"C:\home\contents\dango.png";]
        {Speaker = Tale;
         Source = AudioFile @"C:\home\contents\0076-scripts_台詞_T.wav"
         Subtitle = Serif "テールのセリフ"}
        <| fun p ->
            html.textA Style[] (p+position(1860,10)) 48 "#000000" "10"
            html.contents Style[] (p+position(50,50)) "アニメーション制御の実装3"
            html.subtitle1 Style[] (p+position(80,150)) "アニメーション3"
            html.textA Style[] (p+position(100,320)) 30 "#000000" "ページの表示直後にアニメーションが始まる"
            html.animationAuto {sX=700; sY=780; mX=1140; mY=250; backgroundColor="#bbeeff"} p <| fun (f,p) ->
                /// 中心座標
                let cx,cy = 350.0, 390.0
                /// 円弧の半径
                let R = 198.0
                f.circle Style[stroke.width 3.0; stroke.color "#ff8800"; stroke.fill "none";] (position(cx,cy)) R
                let line1 = f.animationLine Style[stroke.width 3.0; stroke.color "#ff0000"; stroke.fill "none";]
                let line2 = f.animationLine Style[stroke.width 3.0; stroke.color "#0000ff"; stroke.fill "none";]
                // 円軌道上を移動
                f.loop {FrameTime=60; FrameNumber=300} <| fun s ->
                    line1.P {
                        Start = {
                            X = fun t -> cx + R*asm.cos(2*asm.pi*t/s.FrameNumber)
                            Y = fun t -> cy + R*asm.sin(2*asm.pi*t/s.FrameNumber)}
                        End = {
                            X = fun t -> cx + R*asm.cos(2*asm.pi*t/s.FrameNumber+asm.pi)
                            Y = fun t -> cy + R*asm.sin(2*asm.pi*t/s.FrameNumber+asm.pi)}}
                    line2.P {
                        Start = {
                            X = fun t -> cx + R*asm.cos(2*asm.pi*t/s.FrameNumber+asm.pi/2)
                            Y = fun t -> cy + R*asm.sin(2*asm.pi*t/s.FrameNumber+asm.pi/2)}
                        End = {
                            X = fun t -> cx + R*asm.cos(2*asm.pi*t/s.FrameNumber+asm.pi/2+asm.pi)
                            Y = fun t -> cy + R*asm.sin(2*asm.pi*t/s.FrameNumber+asm.pi/2+asm.pi)}}
