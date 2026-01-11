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
            html.contents Style[] (p+position(50,50)) "アニメーション制御の実装"
            html.subtitle1 Style[] (p+position(80,150)) "図形のアニメーション"
            html.subtitle2 Style[] (p+position(100,250)) "描画スタイル・座標等の指定"
            html.textA Style[] (p+position(100,320)) 30 "#000000" "AAA"
            ch.D "x" <| fun x ->
                ch.D "y" <| fun y ->
                    html.eqA Style[] (p+position(180,320)) 30 "#000000" "x+y"
                    let x = var.i0 "x"
                    let y = var.i0 "y"
                    html.eqB Style[] (p+position(280,320)) 30 "#FF0000" <| x-y
                    html.eqD Style[] (p+position(180,390)) 30 "#000000" 
                        [asm.sin(x+y);
                        x-y]
            html.subtitle2 Style[] (p+position(100,520)) "破線描画"
            html.subtitle2 Style[] (p+position(100,750)) "円弧描画"
            //html.animationAuto {sX=700; sY=780; mX=1140; mY=250; backgroundColor="#bbeeff"} p <| fun (f,p) ->
            html.animationManual {sX=700; sY=780; mX=1140; mY=250; backgroundColor="#bbeeff"} p (1080,250) <| fun (f,p) ->
                let line1 = f.animationLine Style[stroke.width 3.0; stroke.dash "4,4"; stroke.color "#000000"]
                let elps1 = f.animationArc Style[stroke.width 3.0; stroke.color "#000000"; stroke.fill "none";]
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
