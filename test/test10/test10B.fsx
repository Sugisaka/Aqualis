//#############################################################################
// 図面作成テスト
let projectname = "test10B"
let version = "1.0.0"
//#############################################################################

let outputdir = __SOURCE_DIRECTORY__

#I @"..\..\bin\Debug\net10.0"
#r "Aqualis.dll"

open Aqualis
open System

/// <summary>
/// 始点p1、終点p2の線分を複素平面に描画
/// </summary>
/// <param name="p1">始点</param>
/// <param name="p2">終点</param>
/// <param name="sv">描画先SVGファイル</param>
let zline (p1:double*double,p2:double*double) (sv:svgfilemaker) =
    let x1,y1 = p1
    let x2,y2 = p2
    sv.line((x1,y1),(x2,y2),color.stroke.black 1.0)
    
// 市松模様
svgfile.make (outputdir,"ichimatsuB.svg") (400.0,400.0) 1.0 <| fun sv ->
        let r = 20.0
        // 単位図形の中心が原点にあるときの各頂点
        let a =
            [
                for i in 0..3 do
                    yield r*cos((45.0+90.0*(double i-1.0))*Math.PI/180.0), r*sin((45.0+90.0*(double i-1.0))*Math.PI/180.0)
            ]
        /// 単位図形を中心位置cに描画
        let drawUnit (cx:double,cy:double) =
            for i in 0..2 do
                let x1,y1 = a[i]
                let x2,y2 = a[i+1]
                zline ((cx+x1,cy+y1),(cx+x2,cy+y2)) sv
            let x1,y1 = a[3]
            let x2,y2 = a[0]
            zline ((cx+x1,cy+y1),(cx+x2,cy+y2)) sv
        // 単位図形を並べて描画
        let x0,y0 = a[0]
        let x1,y1 = a[1]
        let x3,y3 = a[3]
        let t1x,t1y = x0+x3,y0+y3
        let t2x,t2y = x0+x1,y0+y1
        for ix in -10..10 do
            for iy in -10..10 do
                drawUnit (double ix*t1x+double iy*t2x,double ix*t1y+double iy*t2y)
