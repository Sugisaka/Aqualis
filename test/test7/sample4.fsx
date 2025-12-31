//#############################################################################
// project title
let projectname = "test7D"
let version = "1.0.0"
//#############################################################################

let outputdir = __SOURCE_DIRECTORY__

#I @"..\..\bin\Debug\net10.0"
#r "Aqualis.dll"

open System
open Aqualis

fixedPage outputdir "sampleD" 1920 1080 <| fun wr ->
    let t = num0(Var(Nt, "t", NaN))
    let ft = num0(Var(Nt, "f(t)", NaN))
    html.page
        [Tale, @"C:\home\contents\テール右斜AAA-.png";
         Dang, @"C:\home\contents\dango.png";]
        {Speaker = Dang;
         Source = AudioFile @"C:\home\contents\008-scripts_台詞_D.wav"
         Subtitle = Serif "す…すうがく。ぼく数学きらいなんだよね。数学使わずに授業やってよ"}
        <| fun p -> html.image (Style [size.width "100%";],p) @"C:\home\contents/title.PNG"
    html.page
        [Tale, @"C:\home\contents\テール右斜AAA-.png"; 
         Armi, @"C:\home\contents\アルミAA-.png";]
        {Speaker=Tale;
         Source = AudioFile @"C:\home\contents\0076-scripts_台詞_T.wav";
         Subtitle = Serif "ですが、" + Serif (ft .= asm.sin t) + Serif "のように数式で書くと、周期関数であることは疑いようがないですよね"}
        <| fun p ->
            html.fig p <| fun (f,p) ->
                f.line Style[stroke.width 1.0; stroke.color "#ff0000"] <| position(100,100) <| position(100+100,100+100)
                f.line Style[stroke.width 1.0; stroke.color "#ff0000"] <| position(300,100) <| position(300+100,100+100)
    html.page
        [Tale, @"C:\home\contents\テール右斜AAA-.png"; 
         Armi, @"C:\home\contents\アルミAA-.png";]
        {Speaker = Armi;
         Source = AudioFile @"C:\home\contents\04-scripts_台詞_A.wav"
         Subtitle = Serif "だんご君がビーチで団子フェスティバルがあるから買いに来たって"}
        <| fun p ->
            html.animation {sX=400; sY=200; mX=100; mY=100; backgroundColor="#aaaaff"} p <| fun (f,p) ->
                // 曲線のグラフを40点サンプリングして短い線分の集合（折れ線）で表す
                for i in 0..40 do
                    f.line Style[stroke.width 1.0; stroke.color "#ff0000"] 
                    // 線分の始点
                    <| {
                        //x座標：時間に依存しないのでtは不要
                        X = fun _ -> I 10*i; 
                        //y座標：時間に依存するのでtの関数として表す
                        Y = fun t -> 100+100*asm.sin(D 0.4*i-2.0*Math.PI*t/100)
                       }
                    // 線分の終点
                    <| {
                        //x座標：時間に依存しないのでtは不要
                        X = fun _ -> I 10*(i+1); 
                        //y座標：時間に依存するのでtの関数として表す
                        Y = fun t -> 100+100*asm.sin(D 0.4*(i+1)-2.0*Math.PI*t/100)
                       }
