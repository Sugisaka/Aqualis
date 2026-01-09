//#############################################################################
// シーケンス図テスト
let projectname = "test8"
let version = "1.0.0"
//#############################################################################

let outputdir = __SOURCE_DIRECTORY__

#I @"..\..\bin\Debug\net10.0"
#r "Aqualis.dll"

open Aqualis

Compile [HTMLSequenceDiagram] outputdir "test8" (version,"aaa") <| fun () ->
    // スタイル変更
    // setSequenceDiagramStyle
    //     {
    //         TopMargin = 40.0
    //         LeftMargin = 40.0
    //         VarInterval = 150.0
    //         SingleArrowLength = varInterval/4.0
    //         VarHeaderWidth = 50.0
    //         VarHeaderHeight = 20.0
    //         LineWidth = 2.0
    //         ActiveLineWidth = 10.0
    //         FrameMargin = 10.0
    //         TimeStep = 10.0 
    //         FrameBorder = 2.0
    //         ColorActiveLine = "rgba(0, 191, 255, 0.5)"
    //         ColorLoopFrame = "rgb(255, 0, 0)"
    //         ColorBranchFrame = "rgb(0, 180, 0)"
    //         ColorSectionFrame = "rgb(127,0,255)"
    //     }
    
    ch.I "x" <| fun x ->
    ch.I "x_1" <| fun x1 ->
        group.Section "section title" <| fun () ->
            x <== 0
            x <== x + 1
            x1 <== 0
        iter.range (1,10) <| fun i ->
            iter.range (11,20) <| fun j ->
                x <== x1 + i + asm.sin i + j + asm.cos j
        br.if2 (x1 .= 0)
        <| fun i ->
            br.if2 (x .= 0)
            <| fun i ->
                x <== 1
            <| fun i ->
                x <== 2
        <| fun i ->
            x <== 2
