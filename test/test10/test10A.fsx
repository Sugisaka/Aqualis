//#############################################################################
// 図面作成テスト
let projectname = "test10A"
let version = "1.0.0"
//#############################################################################

let outputdir = __SOURCE_DIRECTORY__

#I @"..\..\bin\Debug\net10.0"
#r "Aqualis.dll"

open Aqualis

Compile [Fortran;C99;Python] outputdir projectname (version,"aaa") <| fun () ->
    
    /// <summary>
    /// 始点p1、終点p2の線分を複素平面に描画
    /// </summary>
    /// <param name="p1">始点</param>
    /// <param name="p2">終点</param>
    /// <param name="sv">描画先SVGファイル</param>
    let zline (p1:num0,p2:num0) (sv:svgfilemaker_aq) =
        sv.line((p1.re,p1.im),(p2.re,p2.im),color.stroke.black(1.0))
        
    // 市松模様
    svgfile.make "ichimatsuA.svg" (400.0,400.0) 1.0 <| fun sv ->
        // 単位図形の中心が原点にあるときの各頂点
        ch.z1 4 <| fun a ->
            let r = 20.0
            iter.num a.size1 <| fun i -> 
                a[i] <== r*asm.exp((45+90*(i-1))*asm.uj*asm.pi/180)
            /// 単位図形を中心位置cに描画
            let drawUnit (c:num0) =
                iter.num (a.size1-1) <| fun i -> 
                    zline (c+a[i],c+a[i+1]) sv
                zline (c+a[a.size1-1],c+a[0]) sv
            // 単位図形を並べて描画
            let t1 = a[0]+a[3]
            let t2 = a[0]+a[1]
            iter.range (-10, 10) <| fun ix ->
                iter.range (-10, 10) <| fun iy ->
                    drawUnit (ix*t1+iy*t2)
