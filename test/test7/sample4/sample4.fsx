//#############################################################################
// プレゼンテーションHTMLテストD
let projectname = "test7D"
let version = "1.0.0"
//#############################################################################

let outputdir = __SOURCE_DIRECTORY__

#I @"..\..\..\bin\Debug\net10.0"
#r "Aqualis.dll"

open System
open System.IO
open Aqualis

type Tale(dir:string,name:string) =
    inherit Character(dir,name)
    let st = "position: absolute; margin-left: 0px; margin-top: -122px; width: 850px; object-position: right 204px top 426px; z-index: 2;"
    override _.scriptColor with get() = "#d11aff"
    override _.audioFile(a:Audio) = 
        match a.AudioSourceNumber, a.AudioFileNumber with
        |Some n, Some m -> Some (n.ToString "0000" + "-" + name + "_" + m.ToString() + ".wav") 
        |_ -> None
    override _.scriptFile(n:int) = dir + "\\" + name + "_" + n.ToString() + ".txt"
    member _.AAA with get() = {CharacterImageFile = @"C:\home\contents\テール右斜AAA-.png"; CharacterImageStyle = st}

type Dango(dir:string,name:string) =
    inherit Character(dir,name)
    let st = "position: absolute; margin-left: 1462px; margin-top: 727px; width: 450px; z-index: 3;"
    override _.scriptColor with get() = "#455eff"
    override _.audioFile(a:Audio) = 
        match a.AudioSourceNumber, a.AudioFileNumber with
        |Some n, Some m ->Some (n.ToString "0000" + "-" + name + "_" + m.ToString() + ".wav")
        |_ -> None
    override _.scriptFile(n:int) = dir + "\\" + name + "_" + n.ToString() + ".txt"
    member _.D00 with get() = {CharacterImageFile = @"C:\home\contents\dango.png"; CharacterImageStyle = st}
    
type Armillaris(dir:string,name:string) =
    inherit Character(dir,name)
    let st = "position: absolute; margin-left: 1503px; margin-top: 638px; width: 360px; z-index: 4;"
    override _.scriptColor with get() = "#ff8800"
    override _.audioFile(a:Audio) = 
        match a.AudioSourceNumber, a.AudioFileNumber with
        |Some n, Some m ->Some (n.ToString "0000" + "-" + name + "_" + m.ToString() + ".wav")
        |_ -> None
    override _.scriptFile(n:int) = dir + "\\" + name + "_" + n.ToString() + ".txt"
    member _.AAA with get() = {CharacterImageFile = @"C:\home\contents\armi.png"; CharacterImageStyle = st}
    
let scriptDir = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "script"))
let tale = Tale(scriptDir, "tale")
let dang = Dango(scriptDir, "dango")
let armi = Armillaris(scriptDir, "armi")

let textA = Style[font.size 48]
let textAM = Style[font.size 48; font.color "#ff00ff"]
let textAG = Style[font.size 48; font.color "#00ddaa"]
let textB = Style[font.size 30]
let textBR = Style[font.size 30; font.color "#ff0000"]
let lineBlack = Style[stroke.color "#000000";fill.color "none";stroke.width 3.0]
let arrowBlack = lineBlack,Style[stroke.color "#000000";fill.color "#000000";stroke.width 3.0],3.0,20.0
let NL = num0(Var(Dt,"",NaN))

let ff t = 0.2+0.2*t+0.01*cos(2.0*System.Math.PI*t/0.6)+0.1*cos(2.0*System.Math.PI*t/3.4)+0.05*sin(2.0*System.Math.PI*t/2.9)
let gg t = if abs(t) < 0.4 then 1.0 else 0.0

fixedPage outputdir projectname projectname 1920 1080 {Character=OFF; Subtitle=OFF; Voice=OFF} None <| fun () ->
    let f(x:num0) = num0(Var(Dt,"f("+x.code+")",NaN))
    let t = num0(Var(Dt,"t",NaN))
    html.page
        [tale.AAA;
         dang.D00;]
        <| dang.script "だんごのセリフ"
        <| fun p -> html.image (Style [size.width "100%";],p) @"C:\home\contents/title.PNG"
    html.page
        [tale.AAA;
         dang.D00;]
        <| tale.script "テールのセリフ"
        <| fun p ->
            html.text textA (p+position(1860,10)) "10"
            html.contents Style[] (p+position(50,50)) "アニメーション制御の実装1"
            html.subtitle1 Style[] (p+position(80,150)) "サンプルページ1"
            html.subtitle2 Style[] (p+position(100,250)) "テキストと数式の表示"
            html.text textB (p+position(100,320)) "AAA"
            ch.D "x" <| fun x ->
                ch.D "y" <| fun y ->
                    html.text textB  (p+position(180,320)) "x+y"
                    html.text textBR (p+position(280,320)) <| num0.html (2*x*(-y)*asm.pow(-y,x+1))
                    html.text textB  (p+position(180,390)) <| num0.html 
                        [asm.sin(x+y)
                         x-y]
            html.subtitle2 Style[] (p+position(100,520)) "アニメーション1"
            html.text textB (p+position(100,590)) "startボタンを押すとアニメーションが開始する"

            html.graphEq (100,650) (800,400) (-10,10,100) (-1.2,1.2) [
                Style[stroke.width 3.0; stroke.color "#ff0000"; fill.color "none";], fun x -> sin(x)
                Style[stroke.width 3.0; stroke.color "#0000ff"; fill.color "none";], fun x -> cos(x)
            ]
            html.animationManual {sX=700; sY=780; mX=1140; mY=250; backgroundColor="#bbeeff"} p (1080,250) <| fun (f,p) ->
                let line1 = f.animationLine Style[stroke.width 3.0; stroke.dasharray [4;4]; stroke.color "#000000"]
                let elps1 = f.animationArc Style[stroke.width 3.0; stroke.color "#000000"; fill.color "none";]
                f.ellipse Style[stroke.width 3.0; stroke.color "#ff0000"; fill.color "none";] (position(200.0,400.0)) (200, 100)
                f.circle Style[stroke.width 3.0; stroke.color "#ff8800"; fill.color "none";] (position(400.0,400.0)) 50
                f.rect Style[stroke.width 3.0; stroke.color "#0088ff"; fill.color "none";] (position(400.0,200.0)) (50,100)
                f.ellipseArc Style[stroke.width 3.0; stroke.color "#00bb00"; fill.color "none";] (position(200.0,200.0)) (200, 200) (-Math.PI*0.5, Math.PI*1.2)
                f.polygon Style[stroke.width 3.0; stroke.color "#ff00ff"; fill.color "none";] [position(200.0,200.0);position(300.0,300.0);position(300.0,200.0)]
                f.polyline Style[stroke.width 3.0; stroke.color "#aa00ff"; fill.color "none";] [position(200.0,600.0);position(300.0,700.0);position(300.0,600.0)]

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
        [tale.AAA;
         dang.D00;]
        <| tale.script "テールのセリフ"
        <| fun p ->
            html.text textA (p+position(1860,10)) "10"
            html.contents Style[] (p+position(50,50)) "アニメーション制御の実装2"
            html.subtitle1 Style[] (p+position(80,150)) "アニメーション2"
            html.text textB (p+position(100,320)) "startボタンを押すとアニメーションが無限ループする"

            html.graphEqs (100,500) (800,300) (-1,1,400) (-0.01,1.0)
                [
                    Style[stroke.width 3.0; stroke.color "#0000ff"; fill.color "none";], fun t -> ff t * gg t
                ]
                <| fun (line,arrow,circle,rectangle,text) ->
                    arrow arrowBlack [position(-0.4,-0.04);position(0.4,-0.04)]
                    text textB (position(-0.1,-0.04)) <| "幅：" + num0.html _1

            html.animationManual {sX=700; sY=780; mX=1140; mY=250; backgroundColor="#bbeeff"} p (1080,250) <| fun (f,p) ->
                /// 中心座標
                let cx,cy = 350.0, 390.0
                /// 円弧の半径
                let R = 198.0
                f.circle Style[stroke.width 3.0; stroke.color "#ff8800"; fill.color "none";] (position(cx,cy)) R
                let circ1 = f.animationEllipse Style[stroke.width 3.0; stroke.color "none"; fill.color "#00aa00";]
                // 円軌道上を移動
                f.loop {FrameTime=20; FrameNumber=100} <| fun s ->
                    circ1.P {
                        center = {
                            X = fun t -> cx + R*asm.cos(2*asm.pi*t/s.FrameNumber)
                            Y = fun t -> cy + R*asm.sin(2*asm.pi*t/s.FrameNumber)}
                        radiusX = fun _ -> I 10
                        radiusY = fun _ -> I 10}
    html.page
        [tale.AAA;
         dang.D00;]
        <| tale.script ("テールのセリフ："++asm.sin(t))
        <| fun p ->
            html.text textA (p+position(1860,10)) "10"
            html.contents Style[] (p+position(50,50)) "アニメーション制御の実装3"
            html.subtitle1 Style[] (p+position(80,150)) "アニメーション3"
            ch.D "x" <| fun x ->
                ch.D "y" <| fun y ->
                    html.text textAM (position(80,400)) <| num0.html [y === x + 1]
                    html.text textAG (position(80,500)) <| num0.html 
                        [y  =|= x + 1
                         NL =|= (x + 2)
                         NL =|= (x + 3)]
            html.text textB (p+position(100,320)) "ページの表示直後にアニメーションが始まる"
            html.animationAuto {sX=700; sY=780; mX=1140; mY=250; backgroundColor="#bbeeff"} p <| fun (f,p) ->
                /// 中心座標
                let cx,cy = 350.0, 390.0
                /// 円弧の半径
                let R = 198.0
                f.circle Style[stroke.width 3.0; stroke.color "#ff8800"; fill.color "none";] (position(cx,cy)) R
                let line1 = f.animationLine Style[stroke.width 3.0; stroke.color "#ff0000"; fill.color "none";]
                let line2 = f.animationLine Style[stroke.width 3.0; stroke.color "#0000ff"; fill.color "none";]
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

tale.saveScriptData()
dang.saveScriptData()
armi.saveScriptData()
