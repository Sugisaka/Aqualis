//#############################################################################
// project title
let projectname = "test7C"
let version = "1.0.0"
//#############################################################################

let outputdir = __SOURCE_DIRECTORY__

#I @"..\..\bin\Debug\net10.0"
#r "Aqualis.dll"

open Aqualis

fixedWidthPage outputdir "sampleC" 1920 <| fun () ->
    codewrite "AAA"
    html.br()
    codewrite "AAA"
    html.br()
    codewrite "AAA"
    html.br()
    codewrite "AAA"
    html.br()
    codewrite "AAA"
    html.br()
    codewrite "AAA"
    html.br()
    codewrite "AAA"
    html.br()
    html.canvas Style[size.width "1920px"; size.height "500px";] <| fun cv ->
        
        //ページ左上角
        let p0 = position.Origin
        //ソースコードボックス1段目
        let grid_y1 = 45.0
        //ボックスーキャプション間
        let gap1 = 20.0
        //ソースコードキャプション
        let grid_y1c = grid_y1-gap1
        //ソースコードボックス左
        let grid_x1 = 0.0
        //ソースコードボックス右
        let grid_x2 = 500.0
        //スタイル：F#ソースコードボックス
        let style_codeFS = Style[font.size 10; font.color "black"; font.weight "normal"; border.style "1px solid"; area.backGroundColor "#BBEEFF"; font.lineHeight 14; padding.all 5]
        //スタイル：F#ソースコードラベル
        let style_labelFS = Style[font.size 12; font.color "white"; font.weight "bold"; align.items.center; border.style "1px solid"; area.backGroundColor "#0033FF"; padding.all 5; align.justifyContent "center"; display.flex]
        //スタイル：Cソースコードボックス
        let style_codeC  = Style[font.size 10; font.color "black"; font.weight "normal"; border.style "1px solid"; area.backGroundColor "#FFEEBB"; font.lineHeight 14; padding.all 5]
        //スタイル：Cソースコードラベル
        let style_labelC  = Style[font.size 12; font.color "white"; font.weight "bold"; align.items.center; border.style "1px solid"; area.backGroundColor "#FF8C00"; padding.all 5; align.justifyContent "center"; display.flex]
        //コードボックスサイズ
        let codeBoxWidth = 400.0
        let codeBoxHeight = 160.0
        //言語名ラベル
        let labelWidth = 35.0
        let labelHeight = 16.0
        //ソースコードボックス2段目
        let grid_y2 = grid_y1+codeBoxHeight+45.0
        //ソースコードキャプション
        let grid_y2c = grid_y2-gap1
        
        //見出しテキスト
        cv.text
            <| Style[font.size 14; font.color "black"; font.weight "bold"]
            <| p0
            <| "見出し1"
        //F#コードキャプション
        cv.text
            <| Style[font.size 12; font.color "black"; font.weight "normal"]
            <| p0.shift(grid_x1,grid_y1c)
            <| "ソースコード"
        //Cコード(1)キャプション
        cv.text
            <| Style[font.size 12; font.color "black"; font.weight "normal"]
            <| p0.shift(grid_x1,grid_y2c)
            <| "ソースコード"
        //F#コード
        let a1f = cv.blockTextcode style_codeFS
                <| p0.shift(grid_x1,grid_y1)
                <| (codeBoxWidth,codeBoxHeight)
                <| ["let f x = x + 1"
                    "let g x y = x + y"
                    "printfn \"%d\" <| (g 1 2)"
                    "printfn \"%d\" <| (g 5 3)+(f 7)"
                    "let h = fun x -> 2*x"
                    "let c = [1;4;3;9;]"
                    "for i in c do"
                    "&nbsp;&nbsp;&nbsp;&nbsp;printfn \"%d\" i"]
        //F#コードラベル
        let _ = cv.blockText style_labelFS
                <| p0.shift(grid_x1+codeBoxWidth-labelWidth,grid_y1)
                <| (labelWidth,labelHeight)
                <| ["F#"]
        //Cコード(1)
        let a1c = cv.blockTextcode style_codeC
                <| p0.shift(grid_x2,grid_y1)
                <| (codeBoxWidth,codeBoxHeight)
                <| ["#include<stdio.h>"
                    "int main()"
                    "{"
                    "&nbsp;&nbsp;int x = 0;"
                    "&nbsp;&nbsp;printf(\"%d\\n\",x);"
                    "&nbsp;return 0;"
                    "}"]
        //Cコード(1)ラベル
        let _ = cv.blockText style_labelC
                <| p0.shift(grid_x2+codeBoxWidth-labelWidth,grid_y1)
                <| (labelWidth,labelHeight)
                <| ["C言語"]
        //Cコード(2)
        let a1r = cv.blockTextcode style_codeC
                <| p0.shift(grid_x1,grid_y2)
                <| (codeBoxWidth,codeBoxHeight)
                <| ["#include<stdio.h>"
                    "int main()"
                    "{"
                    "&nbsp;&nbsp;double x = 3.1;"
                    "&nbsp;&nbsp;printf(\"%d\\n\",2.0*x);"
                    "&nbsp;return 0;"
                    "}"]
        //Cコード(2)ラベル
        let _ = cv.blockText style_labelC
                <| p0.shift(grid_x1+codeBoxWidth-labelWidth,grid_y2)
                <| (labelWidth,labelHeight)
                <| ["C言語"]
        //図形
        html.fig p0 <| fun (f,p) ->
            //矢印：Cコード(1)→F#コード
            f.linearrow
                <| Style[stroke.color "black";]
                <| position(a1c.Left, grid_y1+codeBoxHeight/2.0)
                <| position(a1f.Right, grid_y1+codeBoxHeight/2.0)
                <| 2
            //矢印：F#コード→Cコード(2)
            f.linearrow
                <| Style[stroke.color "black";]
                <| position(grid_x1+codeBoxWidth/2.0, a1f.Bottom)
                <| position(grid_x1+codeBoxWidth/2.0, a1r.Top)
                <| 2
    codewrite "BBB"
    html.br()
    codewrite "BBB"
    html.br()
    codewrite "BBB"
    html.br()
    codewrite "BBB"
    html.br()
    codewrite "BBB"
    html.br()
    codewrite "BBB"
    html.br()
    codewrite "BBB"
    html.br()
