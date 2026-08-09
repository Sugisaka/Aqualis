//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    open System
    
    [<RequireQualifiedAccess>]
    module SequenceDiagramContext =
        let topMargin = 40.0
        let leftMargin = 40.0
        let variableInterval = 150.0
        let singleArrowLength = 37.5
        let varHeaderWidth = 50.0
        let varHeaderHeight = 20.0
        let lineWidth = 2.0
        let activeLineWidth = 10.0
        let frameMargin = 10.0
        let timeStep = 10.0
        let frameBorder = 2.0
        let activeLineColor = "rgba(0, 191, 255, 0.5)"
        let loopFrameColor = "rgb(255, 0, 0)"
        let branchFrameColor = "rgb(0, 180, 0)"
        let sectionFrameColor = "rgb(127,0,255)"
        let lifeLine = 100.0 
        let lifeLineX (n:int) = leftMargin + varHeaderWidth / 2.0 + float n * variableInterval
        let p0 = position.Origin
        let styleVarHead =
            Style[
                font.size 12;
                font.color "black";
                font.weight "normal";
                area.backGroundColor "#ffffff";
                font.lineHeight 14;
                padding.top 5;
                padding.bottom 5;
                {Key="text-align"; Value="center"}]
                
    [<AutoOpen>]
    module exprEvalHS =

        type expr with

            /// 変数用
            static member addVarList (e:expr,c:Aqualis) =
                let rec makeList (e:expr) (lst:list<string*int*float>) =
                    match e with
                    |Int _ -> lst
                    |Dbl _ -> lst
                    |Var (_,vname,_) ->
                        match List.tryFind (fun (label,_,_) -> label=vname) lst with
                        |Some _ ->
                            // すでにlstに同じ変数が含まれていればこの変数は追加不要
                            lst
                        |None ->
                            match List.tryFind (fun (label,_,_) -> label=vname) c.SequenceVariables with
                            |Some d ->
                                // lstに追加
                                lst@[d]
                            |None ->
                                let varCount = c.SequenceVariables.Length
                                // dicにも未登録のためここで追加する
                                c.SequenceVariables <- c.SequenceVariables@[vname,varCount,c.TerminalLifeLine]
                                // シーケンス図に追加
                                let x =
                                    html(c).blockTextcode
                                        <| SequenceDiagramContext.styleVarHead
                                        <| SequenceDiagramContext.p0.shift(SequenceDiagramContext.leftMargin+SequenceDiagramContext.variableInterval*float varCount, SequenceDiagramContext.topMargin)
                                        <| (SequenceDiagramContext.varHeaderWidth, SequenceDiagramContext.varHeaderHeight)
                                        <| (SequenceDiagramContext.frameBorder, "solid", "#000000")
                                        <| ["\\(" + e.evalHS c + "\\)"]
                                //現在位置までライフライン描画
                                expr.drawLifeLine(c,SequenceDiagramContext.lifeLineX varCount,x.Bottom,c.TerminalLifeLine)
                                // lstに追加
                                lst@[vname,varCount,c.TerminalLifeLine]
                    |Add (_,a,b) -> makeList b (makeList a lst)
                    |Sub (_,a,b) -> makeList b (makeList a lst)
                    |Mul (_,a,b) -> makeList b (makeList a lst)
                    |Div (_,a,b) -> makeList b (makeList a lst)
                    |Pow (_,a,b) -> makeList b (makeList a lst)
                    |Sin (_,a) -> makeList a lst
                    |Cos (_,a) -> makeList a lst
                    |Tan (_,a) -> makeList a lst
                    |Asin (_,a) -> makeList a lst
                    |Acos (_,a) -> makeList a lst
                    |Atan (_,a) -> makeList a lst
                    |Atan2 (a,b) -> makeList b (makeList a lst)
                    |Exp (_,a) -> makeList a lst
                    |Log (_,a) -> makeList a lst
                    |Log10 (_,a) -> makeList a lst
                    |Sqrt (_,a) -> makeList a lst
                    |Abs (_,a) -> makeList a lst
                    |Eq (a,b) -> makeList b (makeList a lst)
                    |NEq (a,b) -> makeList b (makeList a lst)
                    |Greater (a,b) -> makeList b (makeList a lst)
                    |Less (a,b) -> makeList b (makeList a lst)
                    |GreaterEq (a,b) -> makeList b (makeList a lst)
                    |LessEq (a,b) -> makeList b (makeList a lst)
                    |_ -> lst
                makeList e []

            static member fig (c:Aqualis) (p:position) code =
                let writein = c.codewritein
                let f = figure(html(c).taga)
                code(f,p)
                let sx,sy,mx,my = f.setWriteMode()
                writein (
                    "<svg viewBox=\"0 0 "+sx.ToString()+" "+sy.ToString()+"\" "+
                    "width=\""+sx.ToString()+"px\" "+
                    "heigth=\""+sy.ToString()+"px\" "+
                    "xmlns=\"http://www.w3.org/2000/svg\" "+
                    "style=\"margin-left: "+mx.ToString()+"; "+
                    "margin-top: "+my.ToString()+"; "+
                    "position: absolute;"+
                    "\">")
                code(f,p)
                writein "</svg>"

            /// ライフラインを描画
            static member drawLifeLine(c:Aqualis,x:float,y1:float,y2:float) =
                expr.fig c SequenceDiagramContext.p0 <| fun (f,_) ->
                    //破線：classの縦線
                    f.line Style[stroke.color "black"; stroke.width 1.0; stroke.dasharray [5; 3]]
                        <| position(x,y1)
                        <| position(x,y2)

            /// 水平線を描画
            static member drawHorizontalLine(c:Aqualis,x1:float, x2:float, y:float) =
                html(c).fig SequenceDiagramContext.p0 <| fun (f,_) ->
                    f.line Style[stroke.color "black"; stroke.width SequenceDiagramContext.lineWidth]
                        <| position(x1, y)
                        <| position(x2, y)

            /// 水平矢印線を描画
            static member drawHorizontalArrowLine(c:Aqualis,x1:float, x2:float, y:float) =
                html(c).fig SequenceDiagramContext.p0 <| fun (f,_) ->
                    f.lineArrow (Style[stroke.color "black";],2,12)
                        <| position(x1, y)
                        <| position(x2, y)

            //基準線
            static member drawVerticalLine(c:Aqualis,x:float,y1:float,y2:float) =
                html(c).fig SequenceDiagramContext.p0 <| fun (f,p) ->
                    //基準線(縦線)：代入元の1番目から代入先まで(y軸)
                    f.line Style[stroke.color "black"; stroke.width SequenceDiagramContext.lineWidth]
                        <| position(x, y1)
                        <| position(x, y2)

            /// ライフライン(アクティブ)を描画
            static member drawActiveLine(c:Aqualis,x:float, y1:float, y2:float, color:string) =
                html(c).fig SequenceDiagramContext.p0 <| fun (f,p) ->
                    //実行線
                    f.line Style[stroke.color color; stroke.width SequenceDiagramContext.activeLineWidth]
                        <| position(x, y1)
                        <| position(x, y2)
                // フレーム枠(左、右、下)更新
                c.FrameStack |> List.map (fun (xMin,xMax,yMin,yMax) ->
                    let xMin' =
                        if xMin = 0.0 then x
                        elif x < xMin then x
                        else xMin
                    let xMax' =
                        if xMax = 0.0 then x
                        elif x > xMax then x
                        else xMax
                    let yMax' =
                        if yMax = 0.0 then y2
                        elif y1 > yMax && y1 > y2 then y1
                        elif y2 > yMax && y2 > y1 then y2
                        else yMax
                    xMin',xMax',yMin,yMax')
                |> fun u -> c.FrameStack <- u

            /// テキストを描画
            static member drawText(c:Aqualis,size:int,color:string,weight:string,x:float,y:float,text:string) =
                let p = SequenceDiagramContext.p0.shift(x,y)
                let s1 = Style [{Key = "margin-left"; Value = p.x.ToString()+"px";}
                                {Key = "margin-top"; Value = p.y.ToString()+"px";}
                                {Key = "position"; Value = "absolute";}
                                font.size size;
                                font.color color;
                                font.weight weight]
                html(c).tagb ("div", [s1.atr]) <| fun () -> c.codewritein text

            /// 代入式を描画
            static member substHS (x:expr) (eq:expr) (c:Aqualis) =
                let start = expr.addVarList(eq,c)
                // 代入元に変数があるか
                let goal = expr.addVarList(x,c)
                let getName = fun (name,_,_) -> name
                let mutable stepCount = 0
                match start.Length with
                |1 when getName start[0] = getName goal[0] ->
                    // 自身への代入（他の変数無し）の場合
                    let equText = "\\(" + eq.evalHS c + "\\)"
                    //存在する変数すべてにライフライン継ぎ足し
                    for _,number,_ in c.SequenceVariables do
                        expr.drawLifeLine(c,SequenceDiagramContext.lifeLineX number, c.TerminalLifeLine, c.TerminalLifeLine+SequenceDiagramContext.timeStep*float(start.Length+goal.Length+2))
                    let goalName, goalX, goalY = goal[0]
                    //代入先の実行線
                    expr.drawActiveLine(c,SequenceDiagramContext.lifeLineX goalX, goalY, c.TerminalLifeLine+SequenceDiagramContext.timeStep,SequenceDiagramContext.activeLineColor)
                    expr.drawActiveLine(c,SequenceDiagramContext.lifeLineX goalX, c.TerminalLifeLine+2.0*SequenceDiagramContext.timeStep, c.TerminalLifeLine+SequenceDiagramContext.timeStep*float(start.Length+goal.Length+1),SequenceDiagramContext.activeLineColor)
                    // 変数リストのライフラインを更新
                    c.SequenceVariables |> List.map
                        (fun (name, number, yData) ->
                            if name=goalName then
                                name, number, c.TerminalLifeLine+SequenceDiagramContext.timeStep*float(start.Length+goal.Length+1)
                            else
                                name, number, yData)
                    |> (fun u -> c.SequenceVariables <- u)
                    let baseline = SequenceDiagramContext.lifeLineX goalX + SequenceDiagramContext.singleArrowLength
                    let arrow_goal = SequenceDiagramContext.lifeLineX goalX + SequenceDiagramContext.activeLineWidth/2.0
                    //代入元の変数の数だけ矢印を引く
                    let _,s,_ = start[0]
                    // 代入元から基準線までの矢印
                    if goalX > s then
                        //右矢印：実行中→縦線
                        expr.drawHorizontalLine(c,SequenceDiagramContext.lifeLineX s + SequenceDiagramContext.activeLineWidth/2.0, baseline, c.TerminalLifeLine+SequenceDiagramContext.timeStep*float(stepCount+1))
                    elif goalX = s then
                        //右矢印：実行中→縦線
                        expr.drawHorizontalLine(c,SequenceDiagramContext.lifeLineX s + SequenceDiagramContext.activeLineWidth/2.0, baseline, c.TerminalLifeLine+SequenceDiagramContext.timeStep*float(stepCount+1))
                    else
                        //左矢印：実行中→縦線
                        expr.drawHorizontalLine(c,SequenceDiagramContext.lifeLineX s - SequenceDiagramContext.activeLineWidth/2.0, baseline, c.TerminalLifeLine+SequenceDiagramContext.timeStep*float(stepCount+1))
                    //次の変数の矢印のために1つ下にずらす
                    stepCount <- stepCount + 1
                    //基準線(縦線)：代入元の1番目から代入先まで(y軸)
                    expr.drawVerticalLine(c,baseline, c.TerminalLifeLine+SequenceDiagramContext.timeStep, c.TerminalLifeLine+SequenceDiagramContext.timeStep*float(start.Length+1))
                    //左矢印：基準線から代入先まで(x軸)
                    expr.drawHorizontalArrowLine(c,baseline, arrow_goal, c.TerminalLifeLine+SequenceDiagramContext.timeStep*float(start.Length+1))
                    // テキスト（実行内容）
                    expr.drawText(c,12, "black", "normal", baseline, c.TerminalLifeLine-SequenceDiagramContext.timeStep, equText)
                    //実行線の下辺からさらに10.0下を描き始めとする
                    c.TerminalLifeLine <- c.TerminalLifeLine+SequenceDiagramContext.timeStep*float(start.Length+goal.Length+2)
                |0 ->
                    // 定数の代入の場合
                    let equText = "\\(" + eq.evalHS c + "\\)"
                    //存在する変数すべてに破線を引く
                    for _,number,_ in c.SequenceVariables do
                        //破線：classの縦線
                        expr.drawLifeLine(c,SequenceDiagramContext.lifeLineX number,c.TerminalLifeLine,c.TerminalLifeLine+SequenceDiagramContext.timeStep*float(start.Length+goal.Length+2))
                    let goalName,goalX,_ = goal[0]
                    //代入先の実行線
                    expr.drawActiveLine(c,SequenceDiagramContext.lifeLineX goalX, c.TerminalLifeLine, c.TerminalLifeLine+SequenceDiagramContext.timeStep*float(start.Length+goal.Length+1),SequenceDiagramContext.activeLineColor)
                    c.SequenceVariables |> List.map
                        (fun (name, number, yData) ->
                            if name=goalName then
                                name, number, c.TerminalLifeLine+SequenceDiagramContext.timeStep*float(start.Length+goal.Length+1)
                            else
                                name, number, yData)
                    |> (fun u -> c.SequenceVariables <- u)
                    //左矢印：基準線から代入先まで(x軸)
                    expr.drawHorizontalArrowLine(c,SequenceDiagramContext.lifeLineX goalX + SequenceDiagramContext.singleArrowLength, SequenceDiagramContext.lifeLineX goalX + SequenceDiagramContext.activeLineWidth/2.0, c.TerminalLifeLine+SequenceDiagramContext.timeStep*float(start.Length+1))
                    // テキスト（実行内容）
                    expr.drawText(c,12,"black","normal", SequenceDiagramContext.lifeLineX goalX + SequenceDiagramContext.singleArrowLength, c.TerminalLifeLine-SequenceDiagramContext.timeStep,equText)
                    //実行線の下辺からさらに10.0下を描き始めとする
                    c.TerminalLifeLine <- c.TerminalLifeLine+SequenceDiagramContext.timeStep*float(start.Length+goal.Length+2)
                |_ ->
                    let equText = "\\(" + eq.evalHS c + "\\)"
                    //存在する変数すべてに破線を引く
                    for _,number,_ in c.SequenceVariables do
                        //破線：classの縦線
                        expr.drawLifeLine(c,SequenceDiagramContext.lifeLineX number, c.TerminalLifeLine, c.TerminalLifeLine+SequenceDiagramContext.timeStep*float(start.Length+goal.Length+2))
                    for goalName,goalX,_ in goal do
                        //代入先の実行線
                        expr.drawActiveLine(c,SequenceDiagramContext.lifeLineX goalX, c.TerminalLifeLine, c.TerminalLifeLine+SequenceDiagramContext.timeStep*float(start.Length+goal.Length+1), SequenceDiagramContext.activeLineColor)
                        c.SequenceVariables |> List.map
                            (fun (name, number, yData) ->
                                if name=goalName then
                                    name, number, c.TerminalLifeLine+SequenceDiagramContext.timeStep*float(start.Length+goal.Length+1)
                                else
                                    name, number, yData)
                        |> (fun u -> c.SequenceVariables <- u)
                        let left,right =
                            start
                            |> List.fold (fun (l,r) (_,x,_) ->
                                if goalX > x then l+1,r
                                elif goalX < x then l,r+1
                                else l,r) (0,0)
                        //基準線が左側の場合(右矢印)
                        if left > right then
                            let baseline = SequenceDiagramContext.lifeLineX goalX - SequenceDiagramContext.singleArrowLength
                            let arrow_goal = SequenceDiagramContext.lifeLineX goalX - SequenceDiagramContext.activeLineWidth/2.0
                            //代入元の変数の数だけ実行線を引く
                            for label,s,y in start do
                                //代入元の実行線
                                expr.drawActiveLine(c,SequenceDiagramContext.lifeLineX s,y,c.TerminalLifeLine+SequenceDiagramContext.timeStep*float(start.Length+goal.Length+1),SequenceDiagramContext.activeLineColor)
                                c.SequenceVariables |> List.map
                                    (fun (name, number, yData) ->
                                        if name=label then
                                            name, number, c.TerminalLifeLine+SequenceDiagramContext.timeStep*float(start.Length+goal.Length+1)
                                        else
                                            name, number, yData)
                                |> (fun u -> c.SequenceVariables <- u)
                            //代入元の変数の数だけ矢印を引く
                            for _,s,_ in start do
                                // 代入元から基準線までの矢印
                                if goalX > s then
                                    //右矢印：実行中→縦線
                                    expr.drawHorizontalLine(c,SequenceDiagramContext.lifeLineX s + SequenceDiagramContext.activeLineWidth/2.0, baseline, c.TerminalLifeLine+SequenceDiagramContext.timeStep*float(stepCount+1))
                                else
                                    //左矢印：実行中→縦線
                                    expr.drawHorizontalLine(c,SequenceDiagramContext.lifeLineX s - SequenceDiagramContext.activeLineWidth/2.0, baseline, c.TerminalLifeLine+SequenceDiagramContext.timeStep*float(stepCount+1))
                                //次の変数の矢印のために1つ下にずらす
                                stepCount <- stepCount + 1
                            //基準線(縦線)：代入元の1番目から代入先まで(y軸)
                            expr.drawVerticalLine(c,baseline, c.TerminalLifeLine+SequenceDiagramContext.timeStep,c.TerminalLifeLine+SequenceDiagramContext.timeStep*float(start.Length+1))
                            //右矢印：基準線から代入先まで(x軸)
                            expr.drawHorizontalArrowLine(c,baseline,arrow_goal,c.TerminalLifeLine+SequenceDiagramContext.timeStep*float(start.Length+1))
                            // テキスト（実行内容）
                            expr.drawText(c,12,"black","normal",baseline,c.TerminalLifeLine-SequenceDiagramContext.timeStep,equText)
                        //基準線が右側の場合(左矢印)
                        else
                            let baseline = SequenceDiagramContext.lifeLineX goalX + SequenceDiagramContext.singleArrowLength
                            let arrow_goal = SequenceDiagramContext.lifeLineX goalX + SequenceDiagramContext.activeLineWidth/2.0
                            //代入元の変数の数だけ実行線を引く
                            for label,s,y in start do
                                //代入元の実行線
                                expr.drawActiveLine(c,SequenceDiagramContext.lifeLineX s, y, c.TerminalLifeLine+SequenceDiagramContext.timeStep*float(start.Length+goal.Length+1),SequenceDiagramContext.activeLineColor)
                                c.SequenceVariables |> List.map
                                    (fun (name, number, yData) ->
                                        if name=label then
                                            name, number, c.TerminalLifeLine+SequenceDiagramContext.timeStep*float(start.Length+goal.Length+1)
                                        else
                                            name, number, yData)
                                |> (fun u -> c.SequenceVariables <- u)
                            //代入元の変数の数だけ矢印を引く
                            for label,s,y in start do
                                // 代入元から基準線までの矢印
                                if goalX >= s then
                                    //右矢印：実行中→縦線
                                    expr.drawHorizontalLine(c,SequenceDiagramContext.lifeLineX s + SequenceDiagramContext.activeLineWidth/2.0,baseline,c.TerminalLifeLine+SequenceDiagramContext.timeStep*float(stepCount+1))
                                else //goalX < s then
                                    //左矢印：実行中→縦線
                                    expr.drawHorizontalLine(c,SequenceDiagramContext.lifeLineX s - SequenceDiagramContext.activeLineWidth/2.0,baseline,c.TerminalLifeLine+SequenceDiagramContext.timeStep*float(stepCount+1))
                                //次の変数の矢印のために1つ下にずらす
                                stepCount <- stepCount + 1
                            //基準線(縦線)：代入元の1番目から代入先まで(y軸)
                            expr.drawVerticalLine(c,baseline,c.TerminalLifeLine+SequenceDiagramContext.timeStep,c.TerminalLifeLine+SequenceDiagramContext.timeStep*float(start.Length+1))
                            //左矢印：基準線から代入先まで(x軸)
                            expr.drawHorizontalArrowLine(c,baseline,arrow_goal,c.TerminalLifeLine+SequenceDiagramContext.timeStep*float(start.Length+1))
                            // テキスト（実行内容）
                            expr.drawText(c,12,"black","normal",baseline,c.TerminalLifeLine-SequenceDiagramContext.timeStep,equText)
                    //実行線の下辺からさらにtimeStep分延ばす
                    c.TerminalLifeLine <- c.TerminalLifeLine+SequenceDiagramContext.timeStep*float(start.Length+goal.Length+2)

            static member equivHS (x:expr) (y:expr) (c:Aqualis) =
                c.codewritein (x.evalHS c  + " = " + y.evalHS c)

            static member equivAlignHS (x:expr) (y:expr) (c:Aqualis) =
                c.codewritein (x.evalHS c  + " =& " + y.evalHS c)

            //破線(実行線や枠との(y座標の)隙間をつくるため)
            static member extendLifeLine(c:Aqualis) (gap:float) =
                //存在する変数すべてに破線を引く
                for _,number,_ in c.SequenceVariables do
                    //破線：classの縦線
                    expr.drawLifeLine(c,SequenceDiagramContext.lifeLineX number,c.TerminalLifeLine,c.TerminalLifeLine+gap)
                c.TerminalLifeLine <- c.TerminalLifeLine + gap

            //色線(枠用)
            static member colorLine(c:Aqualis,x1:float,y1:float,x2:float,y2:float,color:string) =
                html(c).fig SequenceDiagramContext.p0 <| fun (f,_) ->
                    f.line Style[stroke.color color; stroke.width (SequenceDiagramContext.frameBorder)]
                        <| position(x1,y1)
                        <| position(x2,y2)

            //ループの枠
            static member rectangle(c:Aqualis,startPoint_x:float,startPoint_y:float,endPoint_x:float,endPoint_y:float,color:string) =
                //上辺:左上から右上
                expr.colorLine(c,startPoint_x,startPoint_y,endPoint_x,startPoint_y,color)
                //右辺:右上から右下
                expr.colorLine(c,endPoint_x,startPoint_y,endPoint_x,endPoint_y,color)
                //下辺:右下から左下
                expr.colorLine(c,endPoint_x,endPoint_y,startPoint_x,endPoint_y,color)
                //左辺:左下から左上
                expr.colorLine(c,startPoint_x,endPoint_y,startPoint_x,startPoint_y,color)

            static member sectionHS (c:Aqualis,label:string) = fun code ->
                //上に20.0破線のスペースを作る
                expr.extendLifeLine c 20.0
                c.FrameStack <- (0.0, 0.0, c.TerminalLifeLine - 5.0, c.TerminalLifeLine)::c.FrameStack
                //stack内の要素の個数(デフォルト1個)-1個を枠の深さ(sectionCount)とする
                let sectionCount = c.FrameStack.Length-1
                code()
                //最後に入れた枠の座標と枠の深さの数値を各変数に代入する(この段階ではstackに変化はない)
                let xMin,xMax,yMin,yMax = c.FrameStack.Head
                // ループの枠
                expr.rectangle(c,xMin-50.0+SequenceDiagramContext.frameMargin*float sectionCount,yMin,xMax+50.0-SequenceDiagramContext.frameMargin*float sectionCount,yMax+5.0,SequenceDiagramContext.sectionFrameColor)
                // テキスト（グループ名）
                expr.drawText(c,12,SequenceDiagramContext.sectionFrameColor,"normal",xMin-50.0+SequenceDiagramContext.frameMargin*float sectionCount,yMin-15.0,label)
                //下にframeMargin分のスペースを作る
                expr.extendLifeLine c (SequenceDiagramContext.frameMargin)
                //枠の座標と枠の深さのリストから使った要素以外を残す(使った分を取り除く)
                c.FrameStack <- c.FrameStack.Tail
                // 外側のループ枠をframeMargin分広げる
                c.FrameStack
                |> List.map (fun (xmin,xmax,ymin,ymax) -> xmin,xmax,ymin,ymax+SequenceDiagramContext.frameMargin)
                |> (fun u -> c.FrameStack <- u)

            static member forLoopHS (c:Aqualis) (n1:expr,n2:expr) code =
                let iname,returnVar = c.i0.getVar()
                let i = Var(It 4, iname, NaN)
                let n1_ = n1.evalHS c
                let n2_ = n2.evalHS c
                c.codewritein("<summary><span class=\"op-loop\">for</span> \\(" + i.evalHS c + "=" + n1_ + "," + n2_ + "\\)</summary>")
                c.codewritein "<div class=\"insidecode-loop\">"
                c.indentInc()
                code i
                c.indentDec()
                c.codewritein "</div>"
                returnVar()

            ///<summary>無限ループ</summary>
            static member loopHS (c:Aqualis) code =
                let iname,returnVar = c.i0.getVar()
                let i = Var(It 4, iname, NaN)
                let label = c.GotoLabels.nextGotoLabel()
                let exit() = c.codewritein("goto " + label)
                expr.substH i (Int 1) c
                c.codewritein "<summary><span class=\"op-loop\">repeat</span></summary>"
                c.codewritein "<div class=\"insidecode-loop\">"
                c.indentInc()
                code(exit,i)
                expr.substH i (Add(It 4, i, Int 1)) c
                c.indentDec()
                c.codewritein "</div>"
                c.codewritein("<span class=\"continue\"><span id=\"" + label + "\">" + label + " continue</span></span>\n<br>")
                returnVar()

            ///<summary>条件を満たす間ループ</summary>
            static member whiledoHS (c:Aqualis) (cond:expr) = fun code ->
                c.codewritein("<summary><span class=\"op-loop\">while</span> \\(" + cond.evalHS c + "\\)</summary>")
                c.codewritein "<div class=\"insidecode-loop\">"
                c.indentInc()
                code()
                c.indentDec()
                c.codewritein "</div>"

            ///<summary>指定した範囲でループ</summary>
            static member rangeHS (c:Aqualis) (counter:option<string>) (i1:expr) = fun (i2:expr) -> fun code ->
                //カウンター変数の取得
                let iname,returnVar = match counter with |None -> c.i0.getVar() |Some s -> c.i0.getVar (s,It 4,A0)
                let i = Var(It 4, iname, NaN)
                //上に20.0破線のスペースを作る
                expr.extendLifeLine c 20.0
                c.FrameStack <- (0.0, 0.0, c.TerminalLifeLine - 5.0, c.TerminalLifeLine)::c.FrameStack
                //stack内の要素の個数(デフォルト1個)-1個を枠の深さ(sectionCount)とする
                let sectionCount = c.FrameStack.Length-1
                let counter_Var = expr.addVarList(i,c)
                for countName, count_number, y in counter_Var do
                    //実行線
                    expr.drawActiveLine(c,SequenceDiagramContext.lifeLineX count_number, c.TerminalLifeLine-SequenceDiagramContext.timeStep, c.TerminalLifeLine, SequenceDiagramContext.loopFrameColor)
                    // テキスト（ループ範囲）
                    expr.drawText(c,12,SequenceDiagramContext.loopFrameColor,"normal",SequenceDiagramContext.lifeLineX count_number + SequenceDiagramContext.timeStep, c.TerminalLifeLine - 25.0,"\\(" + i1.evalHS c + " \\rightarrow " + i2.evalHS c + "\\)")
                code i
                //最後に入れた枠の座標と枠の深さの数値を各変数に代入する(この段階ではstackに変化はない)
                let xMin,xMax,yMin,yMax = (c.FrameStack).Head
                // ループの枠
                expr.rectangle(c,xMin-50.0+SequenceDiagramContext.frameMargin*float sectionCount,yMin,xMax+50.0-SequenceDiagramContext.frameMargin*float sectionCount,yMax+5.0,SequenceDiagramContext.loopFrameColor)
                // テキスト（グループ名）
                expr.drawText(c,12,SequenceDiagramContext.loopFrameColor,"normal",xMin-50.0+SequenceDiagramContext.frameMargin*float sectionCount,yMin-15.0,"\\(\\mathrm{For}\\)")
                //下にframeMargin分のスペースを作る
                expr.extendLifeLine c (SequenceDiagramContext.frameMargin)
                //枠の座標と枠の深さのリストから使った要素以外を残す(使った分を取り除く)
                c.FrameStack <- (c.FrameStack).Tail
                // 外側のループ枠をframeMargin分広げる
                c.FrameStack
                |> List.map (fun (xmin,xmax,ymin,ymax) -> xmin,xmax,ymin,ymax+SequenceDiagramContext.frameMargin)
                |> (fun u -> c.FrameStack <- u)
                // 使用済みカウンタ変数を返却し再利用可能にする
                returnVar()

            ///<summary>指定した範囲でループ(途中脱出可)</summary>
            static member range_exitHS (c:Aqualis) (counter:option<string>) (i1:expr) = fun (i2:expr) -> fun code ->
                match i1,i2 with
                |Int a, Int b when a>b ->
                    let iname,returnVar = match counter with |None -> c.i0.getVar() |Some s -> c.i0.getVar (s,It 4,A0)
                    let i = Var(It 4, iname, NaN)
                    let label = c.GotoLabels.nextGotoLabel()
                    let exit() = c.codewritein("goto "+label)
                    c.comment("<summary><span class=\"op-loop\">for</span> \\(" + i.evalH c + "=" + i1.evalH c + "," + i2.evalH c + "\\)</summary>")
                    c.comment "<div class=\"insidecode-loop\">"
                    c.indentInc()
                    code(exit,i)
                    c.indentDec()
                    c.comment "</div>"
                    c.comment("<span class=\"continue\"><span id=\"" + label + "\">" + label + " continue</span></span>\n<br>")
                    c.comment(label+" continue")
                    returnVar()
                |_ ->
                    let iname,returnVar = match counter with |None -> c.i0.getVar() |Some s -> c.i0.getVar (s,It 4,A0)
                    let i = Var(It 4, iname, NaN)
                    let label = c.GotoLabels.nextGotoLabel()
                    let exit() = c.codewritein("goto "+label)
                    c.codewritein("<summary><span class=\"op-loop\">for</span> \\(" + i.evalH c + "=" + i1.evalH c + "," + i2.evalH c + "\\)</summary>")
                    c.codewritein "<div class=\"insidecode-loop\">"
                    c.indentInc()
                    code(exit,i)
                    c.indentDec()
                    c.codewritein "</div>"
                    c.codewritein("<span class=\"continue\"><span id=\"" + label + "\">" + label + " continue</span></span>\n<br>")
                    c.codewritein(label+" continue")
                    returnVar()

            static member branchHS (c:Aqualis) code =
                //新しい分岐処理枠を追加
                c.SequenceBranches <- []::c.SequenceBranches
                let ifcode (cond:expr) code =
                    //上に30.0破線のスペースを作る
                    expr.extendLifeLine c 30.0
                    // 現在の分岐処理枠に条件式とy座標追加
                    c.SequenceBranches <- ((c.SequenceBranches).Head@["\\(" + cond.evalHS c + "\\)",c.TerminalLifeLine])::c.SequenceBranches.Tail
                    c.FrameStack <- (0.0, 0.0, c.TerminalLifeLine - 5.0, c.TerminalLifeLine)::c.FrameStack
                    code()
                    //中に20.0破線のスペースを作る
                    expr.extendLifeLine c 20.0
                    //境界線のy座標をスタック用のリストに入れる
                let elseifcode (cond:expr) code =
                    //中に20.0破線のスペースを作る
                    expr.extendLifeLine c 20.0
                    // 現在の分岐処理枠に条件式とy座標追加
                    c.SequenceBranches <- ((c.SequenceBranches).Head@["\\(" + cond.evalHS c + "\\)",c.TerminalLifeLine])::(c.BranchStack).Tail
                    code()
                let elsecode code =
                    // 現在の分岐処理枠に条件式とy座標追加
                    c.SequenceBranches <- ((c.SequenceBranches).Head@["\\(\\mathrm{Else}\\)",c.TerminalLifeLine])::(c.BranchStack).Tail
                    code()

                code(ifcode,elseifcode,elsecode)

                //最後に入れた枠の座標と枠の深さの数値を各変数に代入する(この段階ではstackに変化はない)
                let xMin,xMax,yMin,yMax = (c.FrameStack).Head
                //stack内の要素の個数(デフォルト1個)-1個を枠の深さ(sectionCount)とする
                let sectionCount = (c.FrameStack).Length-1
                expr.rectangle(c,xMin-50.0+SequenceDiagramContext.frameMargin*float sectionCount,yMin-20.0,xMax+50.0-SequenceDiagramContext.frameMargin*float sectionCount,yMax+5.0,SequenceDiagramContext.branchFrameColor)
                for cond,y in (c.SequenceBranches).Head do
                    // テキスト（条件式）
                    expr.drawText(c,12,SequenceDiagramContext.branchFrameColor,"normal",5.0+xMin-50.0+SequenceDiagramContext.frameMargin*float sectionCount,y-25.0,cond)
                for _,y in (c.SequenceBranches).Head.Tail do
                    //破線：境界線(間の仕切り)
                    let x1 = xMin-50.0+SequenceDiagramContext.frameMargin*float sectionCount
                    let x2 = xMax+50.0-SequenceDiagramContext.frameMargin*float sectionCount
                    let y1 = y-25.0
                    html(c).fig SequenceDiagramContext.p0 <| fun (f,_) ->
                        //破線：条件分岐の横線
                        f.line Style[stroke.color (SequenceDiagramContext.branchFrameColor); stroke.width (SequenceDiagramContext.frameBorder); stroke.dasharray [2]]
                            <| position(x1,y1)
                            <| position(x2,y1)
                //下にframeMargin分のスペースを作る
                expr.extendLifeLine c (SequenceDiagramContext.frameMargin)
                //枠の座標と枠の深さのリストから使った要素以外を残す(使った分を取り除く)
                c.FrameStack <- (c.FrameStack).Tail
                // 外側のループ枠をマージン分広げる
                c.FrameStack
                |> List.map (fun (xmin,xmax,ymin,ymax) -> xmin,xmax,ymin,ymax+SequenceDiagramContext.frameMargin)
                |> (fun u -> c.SequenceFrames <- u)
                //先頭の分岐処理枠を削除
                c.SequenceBranches <- c.SequenceBranches.Tail

            member this.evalHS(c:Aqualis) =
                let par (s:string) (pl:int) =
                    match pl%3 with
                    |2 -> "\\left\\{" + s + "\\right\\}"
                    |1 -> "\\left[" + s + "\\right]"
                    |_ -> "\\left(" + s + "\\right)"
                let rec eval (u:expr) (pl:int) : string*int =
                    match u with
                    |False -> "false",pl
                    |True -> "true",pl
                    |Eq(x,y) ->
                        let x,nx = eval x pl
                        let y,ny = eval y pl
                        x + " = " + y, max nx ny
                    |NEq(x,y) ->
                        let x,nx = eval x pl
                        let y,ny = eval y pl
                        x + " \\neq " + y, max nx ny
                    |Greater(x,y) ->
                        let x,nx = eval x pl
                        let y,ny = eval y pl
                        x + " > " + y, max nx ny
                    |GreaterEq(x,y) ->
                        let x,nx = eval x pl
                        let y,ny = eval y pl
                        x + " \\geq " + y, max nx ny
                    |Less(x,y) ->
                        let x,nx = eval x pl
                        let y,ny = eval y pl
                        x + " < " + y, max nx ny
                    |LessEq(x,y) ->
                        let x,nx = eval x pl
                        let y,ny = eval y pl
                        x + " \\leq " + y, max nx ny
                    |AND x ->
                        x
                        |> List.map (fun v ->
                            match v with
                            |OR _ |AND _ ->
                                let v,nv = eval v pl
                                par v nv
                            |_ ->
                                let v,_ = eval v pl
                                v)
                        |> fun lst -> String.Join(" \\cap ", lst),1
                    |OR x ->
                        x
                        |> List.map (fun v ->
                            match v with
                            |OR _ |AND _ ->
                                let v,nv = eval v pl
                                par v nv
                            |_ ->
                                let v,_ = eval v pl
                                v)
                        |> fun lst -> String.Join(" \\cup ", lst),1
                    |Int x -> c.numFormat.ItoS x, pl
                    |Dbl x -> c.numFormat.DtoS x, pl
                    |Cpx (0.0,1.0) -> "uj", pl
                    |Cpx (re,im) -> eval (Dbl re + Cpx(0.0,1.0) * Dbl im) pl
                    |Var (_,s,_) -> s, pl
                    |Inv(_,x) ->
                        match x with
                        |Add _|Sub _ ->
                            let x,nx = eval x pl
                            "-" + par x nx, nx+1
                        |_ ->
                            let x,nx = eval x pl
                            "-" + x, nx
                    |Add(_,x,y) ->
                        let x,nx = eval x pl
                        let y,ny = eval y pl
                        x + "+" + y, max nx ny
                    |Sub(_,x,y) ->
                        match x,y with
                        |x,(Add _|Sub _) ->
                            let x,_  = eval x pl
                            let y,ny = eval y pl
                            x + "-" + par y ny, ny+1
                        |_ ->
                            let x,ny = eval x pl
                            let y,nx = eval y pl
                            x + "-" + y, max nx ny
                    |Mul(_,x,y) ->
                        match x,y with
                        |(Int _|Dbl _),(Int _|Dbl _) ->
                            let xx,nx =
                                match x with
                                |Int n when n<0 ->
                                    let x,nx = eval x pl
                                    par x nx,nx+1
                                |Dbl n when n<0.0 ->
                                    let x,nx = eval x pl
                                    par x nx,nx+1
                                |_ ->
                                    eval x pl
                            let yy,ny =
                                match y with
                                |Int n when n<0 ->
                                    let y,ny = eval y pl
                                    par y ny,ny+1
                                |Dbl n when n<0.0 ->
                                    let y,ny = eval y pl
                                    par y ny,ny+1
                                |_ ->
                                    eval y pl
                            xx + " \\times " + yy, max nx ny
                        |(Add _|Sub _),(Add _|Sub _|Inv _) ->
                            let x,nx = eval x pl
                            let y,ny = eval y pl
                            par x nx + " " + par y ny, max (nx+1) (ny+1)
                        |(Add _|Sub _|Inv _),(Int _| Dbl _) ->
                            let x,nx = eval x pl
                            let y,ny = eval y pl
                            y + " " +  par x (nx+1), max (nx+1) ny
                        |_,(Int _| Dbl _) ->
                            let x,nx = eval x pl
                            let y,ny = eval y pl
                            x + " " +  y, max nx ny
                        |(Add _|Sub _),_ ->
                            let x,nx = eval x pl
                            let y,ny = eval y pl
                            par x nx + " " +  y, max (nx+1) ny
                        |_,(Add _|Sub _|Inv _) ->
                            let x,nx = eval x pl
                            let y,ny = eval y pl
                            x + " " +  par y ny, max nx (ny+1)
                        |_ ->
                            let x,nx = eval x pl
                            let y,ny = eval y pl
                            x + " " + y, max nx ny
                    |Div(It 4,x,y) ->
                        eval (Floor(x/y)) pl
                    |Div(_,x,y) ->
                        let x,nx = eval x pl
                        let y,ny = eval y pl
                        "\\frac{\\displaystyle " + x + "}{\\displaystyle " + y + "}", max nx ny
                    |Mod(_,x,y) ->
                        let x,nx = eval x 0
                        let y,ny = eval y 0
                        "\\bmod(" + x + "," + y + ")", pl
                    |Pow(_,x,y) ->
                        let xx,nx =
                            let xx,nx = eval x pl
                            match x with
                            |Int n when n<0 ->
                                par xx nx, nx+1
                            |Dbl n when n<0.0 ->
                                par xx nx, nx+1
                            |Add _ |Sub _ |Mul _ |Div _ |Inv _|Pow _ ->
                                par xx nx, nx+1
                            |_ -> xx, nx
                        let yy,ny = eval y pl
                        "{" + xx + "}^{" + yy + "}", max nx ny
                    |Exp(_,x) ->
                        let x,nx = eval x pl
                        "\\exp" + par x nx, nx+1
                    |Sin(_,x) ->
                        let x,nx = eval x pl
                        "\\sin" + par x nx, nx+1
                    |Cos(_,x) ->
                        let x,nx = eval x pl
                        "\\cos" + par x nx, nx+1
                    |Tan(_,x) ->
                        let x,nx = eval x pl
                        "\\tan" + par x nx, nx+1
                    |Asin(_,x) ->
                        let x,nx = eval x pl
                        "\\arcsin" + par x nx, nx+1
                    |Acos(_,x) ->
                        let x,nx = eval x pl
                        "\\arccos" + par x nx, nx+1
                    |Atan(_,x) ->
                        let x,nx = eval x pl
                        "\\arctan" + par x nx, nx+1
                    |Atan2(x,y) ->
                        let x,nx = eval x pl
                        let y,ny = eval y pl
                        "\\arctan" + par (x + "," + y) (max nx ny), max nx ny + 1
                    |Abs(_,x) ->
                        let x,nx = eval x 0
                        "\\left|" + x + "\\right|", pl
                    |Log(_,x) ->
                        let x,nx = eval x pl
                        "\\log" + par x nx, nx+1
                    |Log10(_,x) ->
                        let x,nx = eval x pl
                        "\\log_{10}" + par x nx, nx+1
                    |Sqrt(_,x) ->
                        let x,nx = eval x 0
                        "\\sqrt{" + x + "}", pl
                    |ToInt x ->
                        let x,nx = eval x pl
                        "int" + par x nx, nx+1
                    |ToDbl x ->
                        let x,nx = eval x pl
                        "double" + par x nx, nx+1
                    |Floor x ->
                        let x,nx = eval x 0
                        "\\lfloor " + x + "\\rfloor", pl
                    |Ceil x ->
                        let x,nx = eval x 0
                        "\\lceil " + x+ "\\rceil", pl
                    |Re x ->
                        let x,nx = eval x pl
                        "\\mathrm{Re}" + par x nx, nx+1
                    |Im x ->
                        let x,nx = eval x pl
                        "\\mathrm{Im}" + par x nx, nx+1
                    |Conj x ->
                        let x,nx = eval x 0
                        "\\bar{" + x + "}", pl
                    |Idx1 (_,name,i) ->
                        let i,ni = eval i 0
                        name + "_{" + i + "}", pl
                    |Idx2 (_,name,i,j) ->
                        let i,ni = eval i 0
                        let j,nj = eval j 0
                        name + "_{" + i + "," + j + "}", pl
                    |Idx3 (_,name,i,j,k) ->
                        let i,ni = eval i 0
                        let j,nj = eval j 0
                        let k,nk = eval k 0
                        name + "_{" + i + "," + j + "," + k + "}", pl
                    |Let (t,y,f) ->
                        let x =
                            match t with
                            |It 4 -> Var (t, (fun (a,_) -> a) (c.i0.getVar()), y)
                            |Dt   -> Var (t, (fun (a,_) -> a) (c.d0.getVar()), y)
                            |Zt   -> Var (t, (fun (a,_) -> a) (c.z0.getVar()), y)
                            |_    -> NaN
                        match y with
                        |NaN -> ()
                        |_ -> expr.substHS x y c
                        eval (f x) pl
                    |Sum(t, n1, n2, f) ->
                        // 合計値格納用変数
                        eval (Let(t, Int 0, fun u ->
                            expr.forLoopHS c (n1,n2) <| fun i ->
                                // 加算・代入処理
                                expr.substHS u (Add(t,u, f i)) c
                            u)) pl
                    |IfEl(cond,n1,n2) ->
                        eval (Let(n1.etype, NaN, fun x ->
                            expr.branchHS c <| fun (ifcode,_,elsecode) ->
                                ifcode cond <| fun () ->
                                    expr.substHS x n1 c
                                elsecode <| fun () ->
                                    expr.substHS x n2 c
                            x)) pl
                    |NaN -> "NaN", pl
                let t,_ = eval this 0
                t
