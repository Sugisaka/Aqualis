//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    open System

    type SequenceDiagramStyle =
        {
            /// ダイアグラム上マージン
            TopMargin:float;
            /// ダイアグラム左マージン
            LeftMargin:float;
            /// 変数間の間隔
            VarInterval:float;
            /// 単一代入文の矢印、基準線から代入先までの矢印の長さ
            SingleArrowLength:float;
            /// 変数ヘッダーの横幅
            VarHeaderWidth:float;
            /// 変数ヘッダーの高さ
            VarHeaderHeight:float;
            /// 線の太さ
            LineWidth:float;
            /// 実効線の太さ
            ActiveLineWidth:float;
            // 枠のマージン
            FrameMargin:float;
            /// 図形描画の時間方向間隔
            TimeStep:float;
            // 枠線の太さ
            FrameBorder:float;
            /// ライフライン（実効状態）の色
            ColorActiveLine:string;
            /// ループフレームの色
            ColorLoopFrame:string;
            /// ブランチフレームの色
            ColorBranchFrame:string;
            /// セクションフレームの色
            ColorSectionFrame:string;
        }

    [<AutoOpen>]
    module sequenceDiagramParam =
        let private style(c:program) = {
            TopMargin = c.SequenceTopMargin
            LeftMargin = c.SequenceLeftMargin
            VariableInterval = c.SequenceVariableInterval
            SingleArrowLength = c.SequenceSingleArrowLength
            VariableHeaderWidth = c.SequenceVariableHeaderWidth
            VariableHeaderHeight = c.SequenceVariableHeaderHeight
            LineWidth = c.SequenceLineWidth
            ActiveLineWidth = c.SequenceActiveLineWidth
            FrameMargin = c.SequenceFrameMargin
            TimeStep = c.SequenceTimeStep
            FrameBorder = c.SequenceFrameBorder
            ActiveLineColor = c.SequenceActiveLineColor
            LoopFrameColor = c.SequenceLoopFrameColor
            BranchFrameColor = c.SequenceBranchFrameColor
            SectionFrameColor = c.SequenceSectionFrameColor
        }
        /// 上側マージン
        let topMargin c = (style c).TopMargin
        /// 左側マージン
        let leftMargin c = (style c).LeftMargin
        /// 変数間の間隔
        let varInterval c = (style c).VariableInterval
        /// 単一代入文の矢印、基準線から代入先までの矢印の長さ
        let singleArrowLength c = (style c).SingleArrowLength
        /// 変数ヘッダーの横幅
        let varHeaderWidth c = (style c).VariableHeaderWidth
        /// 変数ヘッダーの高さ
        let varHeaderHeight c = (style c).VariableHeaderHeight
        /// 線の太さ
        let lineWidth c = (style c).LineWidth
        /// 実効線の太さ
        let activeLineWidth c = (style c).ActiveLineWidth
        // 枠のマージン
        let frameMargin c = (style c).FrameMargin
        /// 図形描画の時間方向間隔
        let timeStep c = (style c).TimeStep
        /// 枠線の太さ
        let frameBorder c = (style c).FrameBorder
        /// 現在のライフライン終端座標
        let colorActiveLine c = (style c).ActiveLineColor
        let colorLoopFrame c = (style c).LoopFrameColor
        let colorBranchFrame c = (style c).BranchFrameColor
        let colorSectionFrame c = (style c).SectionFrameColor
        let setSequenceDiagramStyle(context:GenerationContext) (s:SequenceDiagramStyle) =
            let c = context.CurrentProgram
            lock c.SequenceGate (fun () ->
                c.SequenceTopMargin <- s.TopMargin
                c.SequenceLeftMargin <- s.LeftMargin
                c.SequenceVariableInterval <- s.VarInterval
                c.SequenceSingleArrowLength <- s.SingleArrowLength
                c.SequenceVariableHeaderWidth <- s.VarHeaderWidth
                c.SequenceVariableHeaderHeight <- s.VarHeaderHeight
                c.SequenceLineWidth <- s.LineWidth
                c.SequenceActiveLineWidth <- s.ActiveLineWidth
                c.SequenceFrameMargin <- s.FrameMargin
                c.SequenceTimeStep <- s.TimeStep
                c.SequenceFrameBorder <- s.FrameBorder
                c.SequenceActiveLineColor <- s.ColorActiveLine
                c.SequenceLoopFrameColor <- s.ColorLoopFrame
                c.SequenceBranchFrameColor <- s.ColorBranchFrame
                c.SequenceSectionFrameColor <- s.ColorSectionFrame)

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
    module sequenceDiagramData =
        let p0 = position.Origin
        let terminalLifeLine(c:program) = c.TerminalLifeLine
        let setTerminalLifeLine(c:program) value = c.TerminalLifeLine <- value
        /// シーケンス図に描画済み変数リスト
        let varList(c:program) = c.SequenceVariables
        let setVarList(c:program) value = c.SequenceVariables <- value
        /// フレーム枠座標スタックリスト
        let frameStack(c:program) = c.SequenceFrames
        let setFrameStack(c:program) value = c.SequenceFrames <- value
        /// 条件分岐枠スタックリスト
        let branchStack(c:program) = c.SequenceBranches
        let setBranchStack(c:program) value = c.SequenceBranches <- value
        /// 第n変数ライフラインのx座標
        let lifeLineX(c:program) (n:int) = leftMargin c + varHeaderWidth c / 2.0 + float n * varInterval c

    [<AutoOpen>]
    module exprEvalHS =

        type expr with

            /// 変数用
            static member addVarList (e:expr,c:program) =
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
                            match List.tryFind (fun (label,_,_) -> label=vname) (varList c) with
                            |Some d ->
                                // lstに追加
                                lst@[d]
                            |None ->
                                let varCount = (varList c).Length
                                // dicにも未登録のためここで追加する
                                setVarList c <| varList c@[vname,varCount,terminalLifeLine c]
                                // シーケンス図に追加
                                let x =
                                    html(c).blockTextcode
                                        <| styleVarHead
                                        <| p0.shift(leftMargin c+varInterval c*float varCount,topMargin c)
                                        <| (varHeaderWidth c,varHeaderHeight c)
                                        <| (frameBorder c,"solid","#000000")
                                        <| ["\\(" + e.evalHS c + "\\)"]
                                //現在位置までライフライン描画
                                expr.drawLifeLine(c,lifeLineX c varCount,x.Bottom,terminalLifeLine c)
                                // lstに追加
                                lst@[vname,varCount,terminalLifeLine c]
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

            static member fig (c:program) (p:position) code =
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
            static member drawLifeLine(c:program,x:float,y1:float,y2:float) =
                expr.fig c p0 <| fun (f,_) ->
                    //破線：classの縦線
                    f.line Style[stroke.color "black"; stroke.width 1.0; stroke.dasharray [5; 3]]
                        <| position(x,y1)
                        <| position(x,y2)

            /// 水平線を描画
            static member drawHorizontalLine(c:program,x1:float, x2:float, y:float) =
                html(c).fig p0 <| fun (f,_) ->
                    f.line Style[stroke.color "black"; stroke.width (lineWidth c)]
                        <| position(x1, y)
                        <| position(x2, y)

            /// 水平矢印線を描画
            static member drawHorizontalArrowLine(c:program,x1:float, x2:float, y:float) =
                html(c).fig p0 <| fun (f,_) ->
                    f.lineArrow (Style[stroke.color "black";],2,12)
                        <| position(x1, y)
                        <| position(x2, y)

            //基準線
            static member drawVerticalLine(c:program,x:float,y1:float,y2:float) =
                html(c).fig p0 <| fun (f,p) ->
                    //基準線(縦線)：代入元の1番目から代入先まで(y軸)
                    f.line Style[stroke.color "black"; stroke.width (lineWidth c)]
                        <| position(x, y1)
                        <| position(x, y2)

            /// ライフライン(アクティブ)を描画
            static member drawActiveLine(c:program,x:float, y1:float, y2:float, color:string) =
                html(c).fig p0 <| fun (f,p) ->
                    //実行線
                    f.line Style[stroke.color color; stroke.width (activeLineWidth c)]
                        <| position(x, y1)
                        <| position(x, y2)
                // フレーム枠(左、右、下)更新
                frameStack c |> List.map (fun (xMin,xMax,yMin,yMax) ->
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
                |> setFrameStack c

            /// テキストを描画
            static member drawText(c:program,size:int,color:string,weight:string,x:float,y:float,text:string) =
                let p = p0.shift(x,y)
                let s1 = Style [{Key = "margin-left"; Value = p.x.ToString()+"px";}
                                {Key = "margin-top"; Value = p.y.ToString()+"px";}
                                {Key = "position"; Value = "absolute";}
                                font.size size;
                                font.color color;
                                font.weight weight]
                html(c).tagb ("div", [s1.atr]) <| fun () -> c.codewritein text

            /// 代入式を描画
            static member substHS (x:expr) (eq:expr) (c:program) =
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
                    for _,number,_ in varList c do
                        expr.drawLifeLine(c,lifeLineX c number, terminalLifeLine c, terminalLifeLine c+timeStep c*float(start.Length+goal.Length+2))
                    let goalName, goalX, goalY = goal[0]
                    //代入先の実行線
                    expr.drawActiveLine(c,lifeLineX c goalX, goalY, terminalLifeLine c+timeStep c,colorActiveLine c)
                    expr.drawActiveLine(c,lifeLineX c goalX, terminalLifeLine c+2.0*timeStep c, terminalLifeLine c+timeStep c*float(start.Length+goal.Length+1),colorActiveLine c)
                    // 変数リストのライフラインを更新
                    varList c |> List.map
                        (fun (name, number, yData) ->
                            if name=goalName then
                                name, number, terminalLifeLine c+timeStep c*float(start.Length+goal.Length+1)
                            else
                                name, number, yData)
                    |> setVarList c
                    let baseline = lifeLineX c goalX + singleArrowLength c
                    let arrow_goal = lifeLineX c goalX + activeLineWidth c/2.0
                    //代入元の変数の数だけ矢印を引く
                    let _,s,_ = start[0]
                    // 代入元から基準線までの矢印
                    if goalX > s then
                        //右矢印：実行中→縦線
                        expr.drawHorizontalLine(c,lifeLineX c s + activeLineWidth c/2.0, baseline, terminalLifeLine c+timeStep c*float(stepCount+1))
                    elif goalX = s then
                        //右矢印：実行中→縦線
                        expr.drawHorizontalLine(c,lifeLineX c s + activeLineWidth c/2.0, baseline, terminalLifeLine c+timeStep c*float(stepCount+1))
                    else
                        //左矢印：実行中→縦線
                        expr.drawHorizontalLine(c,lifeLineX c s - activeLineWidth c/2.0, baseline, terminalLifeLine c+timeStep c*float(stepCount+1))
                    //次の変数の矢印のために1つ下にずらす
                    stepCount <- stepCount + 1
                    //基準線(縦線)：代入元の1番目から代入先まで(y軸)
                    expr.drawVerticalLine(c,baseline, terminalLifeLine c+timeStep c, terminalLifeLine c+timeStep c*float(start.Length+1))
                    //左矢印：基準線から代入先まで(x軸)
                    expr.drawHorizontalArrowLine(c,baseline, arrow_goal, terminalLifeLine c+timeStep c*float(start.Length+1))
                    // テキスト（実行内容）
                    expr.drawText(c,12, "black", "normal", baseline, terminalLifeLine c-timeStep c, equText)
                    //実行線の下辺からさらに10.0下を描き始めとする
                    setTerminalLifeLine c <| terminalLifeLine c+timeStep c*float(start.Length+goal.Length+2)
                |0 ->
                    // 定数の代入の場合
                    let equText = "\\(" + eq.evalHS c + "\\)"
                    //存在する変数すべてに破線を引く
                    for _,number,_ in varList c do
                        //破線：classの縦線
                        expr.drawLifeLine(c,lifeLineX c number,terminalLifeLine c,terminalLifeLine c+timeStep c*float(start.Length+goal.Length+2))
                    let goalName,goalX,_ = goal[0]
                    //代入先の実行線
                    expr.drawActiveLine(c,lifeLineX c goalX, terminalLifeLine c, terminalLifeLine c+timeStep c*float(start.Length+goal.Length+1),colorActiveLine c)
                    varList c |> List.map
                        (fun (name, number, yData) ->
                            if name=goalName then
                                name, number, terminalLifeLine c+timeStep c*float(start.Length+goal.Length+1)
                            else
                                name, number, yData)
                    |> setVarList c
                    //左矢印：基準線から代入先まで(x軸)
                    expr.drawHorizontalArrowLine(c,lifeLineX c goalX + singleArrowLength c, lifeLineX c goalX + activeLineWidth c/2.0, terminalLifeLine c+timeStep c*float(start.Length+1))
                    // テキスト（実行内容）
                    expr.drawText(c,12,"black","normal", lifeLineX c goalX + singleArrowLength c, terminalLifeLine c-timeStep c,equText)
                    //実行線の下辺からさらに10.0下を描き始めとする
                    setTerminalLifeLine c <| terminalLifeLine c+timeStep c*float(start.Length+goal.Length+2)
                |_ ->
                    let equText = "\\(" + eq.evalHS c + "\\)"
                    //存在する変数すべてに破線を引く
                    for _,number,_ in varList c do
                        //破線：classの縦線
                        expr.drawLifeLine(c,lifeLineX c number, terminalLifeLine c, terminalLifeLine c+timeStep c*float(start.Length+goal.Length+2))
                    for goalName,goalX,_ in goal do
                        //代入先の実行線
                        expr.drawActiveLine(c,lifeLineX c goalX, terminalLifeLine c, terminalLifeLine c+timeStep c*float(start.Length+goal.Length+1), colorActiveLine c)
                        varList c |> List.map
                            (fun (name, number, yData) ->
                                if name=goalName then
                                    name, number, terminalLifeLine c+timeStep c*float(start.Length+goal.Length+1)
                                else
                                    name, number, yData)
                        |> setVarList c
                        let left,right =
                            start
                            |> List.fold (fun (l,r) (_,x,_) ->
                                if goalX > x then l+1,r
                                elif goalX < x then l,r+1
                                else l,r) (0,0)
                        //基準線が左側の場合(右矢印)
                        if left > right then
                            let baseline = lifeLineX c goalX - singleArrowLength c
                            let arrow_goal = lifeLineX c goalX - activeLineWidth c/2.0
                            //代入元の変数の数だけ実行線を引く
                            for label,s,y in start do
                                //代入元の実行線
                                expr.drawActiveLine(c,lifeLineX c s,y,terminalLifeLine c+timeStep c*float(start.Length+goal.Length+1),colorActiveLine c)
                                varList c |> List.map
                                    (fun (name, number, yData) ->
                                        if name=label then
                                            name, number, terminalLifeLine c+timeStep c*float(start.Length+goal.Length+1)
                                        else
                                            name, number, yData)
                                |> setVarList c
                            //代入元の変数の数だけ矢印を引く
                            for _,s,_ in start do
                                // 代入元から基準線までの矢印
                                if goalX > s then
                                    //右矢印：実行中→縦線
                                    expr.drawHorizontalLine(c,lifeLineX c s + activeLineWidth c/2.0, baseline, terminalLifeLine c+timeStep c*float(stepCount+1))
                                else
                                    //左矢印：実行中→縦線
                                    expr.drawHorizontalLine(c,lifeLineX c s - activeLineWidth c/2.0, baseline, terminalLifeLine c+timeStep c*float(stepCount+1))
                                //次の変数の矢印のために1つ下にずらす
                                stepCount <- stepCount + 1
                            //基準線(縦線)：代入元の1番目から代入先まで(y軸)
                            expr.drawVerticalLine(c,baseline, terminalLifeLine c+timeStep c,terminalLifeLine c+timeStep c*float(start.Length+1))
                            //右矢印：基準線から代入先まで(x軸)
                            expr.drawHorizontalArrowLine(c,baseline,arrow_goal,terminalLifeLine c+timeStep c*float(start.Length+1))
                            // テキスト（実行内容）
                            expr.drawText(c,12,"black","normal",baseline,terminalLifeLine c-timeStep c,equText)
                        //基準線が右側の場合(左矢印)
                        else
                            let baseline = lifeLineX c goalX + singleArrowLength c
                            let arrow_goal = lifeLineX c goalX + activeLineWidth c/2.0
                            //代入元の変数の数だけ実行線を引く
                            for label,s,y in start do
                                //代入元の実行線
                                expr.drawActiveLine(c,lifeLineX c s, y, terminalLifeLine c+timeStep c*float(start.Length+goal.Length+1),colorActiveLine c)
                                varList c |> List.map
                                    (fun (name, number, yData) ->
                                        if name=label then
                                            name, number, terminalLifeLine c+timeStep c*float(start.Length+goal.Length+1)
                                        else
                                            name, number, yData)
                                |> setVarList c
                            //代入元の変数の数だけ矢印を引く
                            for label,s,y in start do
                                // 代入元から基準線までの矢印
                                if goalX >= s then
                                    //右矢印：実行中→縦線
                                    expr.drawHorizontalLine(c,lifeLineX c s + activeLineWidth c/2.0,baseline,terminalLifeLine c+timeStep c*float(stepCount+1))
                                else //goalX < s then
                                    //左矢印：実行中→縦線
                                    expr.drawHorizontalLine(c,lifeLineX c s - activeLineWidth c/2.0,baseline,terminalLifeLine c+timeStep c*float(stepCount+1))
                                //次の変数の矢印のために1つ下にずらす
                                stepCount <- stepCount + 1
                            //基準線(縦線)：代入元の1番目から代入先まで(y軸)
                            expr.drawVerticalLine(c,baseline,terminalLifeLine c+timeStep c,terminalLifeLine c+timeStep c*float(start.Length+1))
                            //左矢印：基準線から代入先まで(x軸)
                            expr.drawHorizontalArrowLine(c,baseline,arrow_goal,terminalLifeLine c+timeStep c*float(start.Length+1))
                            // テキスト（実行内容）
                            expr.drawText(c,12,"black","normal",baseline,terminalLifeLine c-timeStep c,equText)
                    //実行線の下辺からさらにtimeStep分延ばす
                    setTerminalLifeLine c <| terminalLifeLine c+timeStep c*float(start.Length+goal.Length+2)

            static member equivHS (x:expr) (y:expr) (c:program) =
                c.codewritein (x.evalHS c  + " = " + y.evalHS c)

            static member equivAlignHS (x:expr) (y:expr) (c:program) =
                c.codewritein (x.evalHS c  + " =& " + y.evalHS c)

            //破線(実行線や枠との(y座標の)隙間をつくるため)
            static member extendLifeLine(c:program) (gap:float) =
                //存在する変数すべてに破線を引く
                for _,number,_ in varList c do
                    //破線：classの縦線
                    expr.drawLifeLine(c,lifeLineX c number,terminalLifeLine c,terminalLifeLine c+gap)
                setTerminalLifeLine c <| terminalLifeLine c + gap

            //色線(枠用)
            static member colorLine(c:program,x1:float,y1:float,x2:float,y2:float,color:string) =
                html(c).fig p0 <| fun (f,_) ->
                    f.line Style[stroke.color color; stroke.width (frameBorder c)]
                        <| position(x1,y1)
                        <| position(x2,y2)

            //ループの枠
            static member rectangle(c:program,startPoint_x:float,startPoint_y:float,endPoint_x:float,endPoint_y:float,color:string) =
                //上辺:左上から右上
                expr.colorLine(c,startPoint_x,startPoint_y,endPoint_x,startPoint_y,color)
                //右辺:右上から右下
                expr.colorLine(c,endPoint_x,startPoint_y,endPoint_x,endPoint_y,color)
                //下辺:右下から左下
                expr.colorLine(c,endPoint_x,endPoint_y,startPoint_x,endPoint_y,color)
                //左辺:左下から左上
                expr.colorLine(c,startPoint_x,endPoint_y,startPoint_x,startPoint_y,color)

            static member sectionHS (c:program,label:string) = fun code ->
                //上に20.0破線のスペースを作る
                expr.extendLifeLine c 20.0
                setFrameStack c <| (0.0, 0.0, terminalLifeLine c - 5.0, terminalLifeLine c)::frameStack c
                //stack内の要素の個数(デフォルト1個)-1個を枠の深さ(sectionCount)とする
                let sectionCount = (frameStack c).Length-1
                code()
                //最後に入れた枠の座標と枠の深さの数値を各変数に代入する(この段階ではstackに変化はない)
                let xMin,xMax,yMin,yMax = (frameStack c).Head
                // ループの枠
                expr.rectangle(c,xMin-50.0+frameMargin c*float sectionCount,yMin,xMax+50.0-frameMargin c*float sectionCount,yMax+5.0,colorSectionFrame c)
                // テキスト（グループ名）
                expr.drawText(c,12,colorSectionFrame c,"normal",xMin-50.0+frameMargin c*float sectionCount,yMin-15.0,label)
                //下にframeMargin分のスペースを作る
                expr.extendLifeLine c (frameMargin c)
                //枠の座標と枠の深さのリストから使った要素以外を残す(使った分を取り除く)
                setFrameStack c <| (frameStack c).Tail
                // 外側のループ枠をframeMargin分広げる
                frameStack c
                |> List.map (fun (xmin,xmax,ymin,ymax) -> xmin,xmax,ymin,ymax+frameMargin c)
                |> setFrameStack c

            static member forLoopHS (context:GenerationContext) (n1:expr,n2:expr) code =
                let c = context.CurrentProgram
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
            static member loopHS (context:GenerationContext) code =
                let c = context.CurrentProgram
                let iname,returnVar = c.i0.getVar()
                let i = Var(It 4, iname, NaN)
                let label = context.GotoLabels.nextGotoLabel()
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
            static member whiledoHS (context:GenerationContext) (cond:expr) = fun code ->
                let c = context.CurrentProgram
                c.codewritein("<summary><span class=\"op-loop\">while</span> \\(" + cond.evalHS c + "\\)</summary>")
                c.codewritein "<div class=\"insidecode-loop\">"
                c.indentInc()
                code()
                c.indentDec()
                c.codewritein "</div>"

            ///<summary>指定した範囲でループ</summary>
            static member rangeHS (context:GenerationContext) (counter:option<string>) (i1:expr) = fun (i2:expr) -> fun code ->
                let c = context.CurrentProgram
                //カウンター変数の取得
                let iname,returnVar = match counter with |None -> c.i0.getVar() |Some s -> c.i0.getVar (s,It 4,A0)
                let i = Var(It 4, iname, NaN)
                //上に20.0破線のスペースを作る
                expr.extendLifeLine c 20.0
                setFrameStack c <| (0.0, 0.0, terminalLifeLine c - 5.0, terminalLifeLine c)::frameStack c
                //stack内の要素の個数(デフォルト1個)-1個を枠の深さ(sectionCount)とする
                let sectionCount = (frameStack c).Length-1
                let counter_Var = expr.addVarList(i,c)
                for countName, count_number, y in counter_Var do
                    //実行線
                    expr.drawActiveLine(c,lifeLineX c count_number, terminalLifeLine c-timeStep c, terminalLifeLine c, colorLoopFrame c)
                    // テキスト（ループ範囲）
                    expr.drawText(c,12,colorLoopFrame c,"normal",lifeLineX c count_number + timeStep c, terminalLifeLine c - 25.0,"\\(" + i1.evalHS c + " \\rightarrow " + i2.evalHS c + "\\)")
                code i
                //最後に入れた枠の座標と枠の深さの数値を各変数に代入する(この段階ではstackに変化はない)
                let xMin,xMax,yMin,yMax = (frameStack c).Head
                // ループの枠
                expr.rectangle(c,xMin-50.0+frameMargin c*float sectionCount,yMin,xMax+50.0-frameMargin c*float sectionCount,yMax+5.0,colorLoopFrame c)
                // テキスト（グループ名）
                expr.drawText(c,12,colorLoopFrame c,"normal",xMin-50.0+frameMargin c*float sectionCount,yMin-15.0,"\\(\\mathrm{For}\\)")
                //下にframeMargin分のスペースを作る
                expr.extendLifeLine c (frameMargin c)
                //枠の座標と枠の深さのリストから使った要素以外を残す(使った分を取り除く)
                setFrameStack c <| (frameStack c).Tail
                // 外側のループ枠をframeMargin分広げる
                frameStack c
                |> List.map (fun (xmin,xmax,ymin,ymax) -> xmin,xmax,ymin,ymax+frameMargin c)
                |> setFrameStack c
                // 使用済みカウンタ変数を返却し再利用可能にする
                returnVar()

            ///<summary>指定した範囲でループ(途中脱出可)</summary>
            static member range_exitHS (context:GenerationContext) (counter:option<string>) (i1:expr) = fun (i2:expr) -> fun code ->
                let c = context.CurrentProgram
                match i1,i2 with
                |Int a, Int b when a>b ->
                    let iname,returnVar = match counter with |None -> c.i0.getVar() |Some s -> c.i0.getVar (s,It 4,A0)
                    let i = Var(It 4, iname, NaN)
                    let label = context.GotoLabels.nextGotoLabel()
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
                    let label = context.GotoLabels.nextGotoLabel()
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

            static member branchHS (context:GenerationContext) code =
                let c = context.CurrentProgram
                //新しい分岐処理枠を追加
                setBranchStack c <| []::branchStack c
                let ifcode (cond:expr) code =
                    //上に30.0破線のスペースを作る
                    expr.extendLifeLine c 30.0
                    // 現在の分岐処理枠に条件式とy座標追加
                    setBranchStack c <| ((branchStack c).Head@["\\(" + cond.evalHS c + "\\)",terminalLifeLine c])::(branchStack c).Tail
                    setFrameStack c <| (0.0, 0.0, terminalLifeLine c - 5.0, terminalLifeLine c)::frameStack c
                    code()
                    //中に20.0破線のスペースを作る
                    expr.extendLifeLine c 20.0
                    //境界線のy座標をスタック用のリストに入れる
                let elseifcode (cond:expr) code =
                    //中に20.0破線のスペースを作る
                    expr.extendLifeLine c 20.0
                    // 現在の分岐処理枠に条件式とy座標追加
                    setBranchStack c <| ((branchStack c).Head@["\\(" + cond.evalHS c + "\\)",terminalLifeLine c])::(branchStack c).Tail
                    code()
                let elsecode code =
                    // 現在の分岐処理枠に条件式とy座標追加
                    setBranchStack c <| ((branchStack c).Head@["\\(\\mathrm{Else}\\)",terminalLifeLine c])::(branchStack c).Tail
                    code()

                code(ifcode,elseifcode,elsecode)

                //最後に入れた枠の座標と枠の深さの数値を各変数に代入する(この段階ではstackに変化はない)
                let xMin,xMax,yMin,yMax = (frameStack c).Head
                //stack内の要素の個数(デフォルト1個)-1個を枠の深さ(sectionCount)とする
                let sectionCount = (frameStack c).Length-1
                expr.rectangle(c,xMin-50.0+frameMargin c*float sectionCount,yMin-20.0,xMax+50.0-frameMargin c*float sectionCount,yMax+5.0,colorBranchFrame c)
                for cond,y in (branchStack c).Head do
                    // テキスト（条件式）
                    expr.drawText(c,12,colorBranchFrame c,"normal",5.0+xMin-50.0+frameMargin c*float sectionCount,y-25.0,cond)
                for _,y in (branchStack c).Head.Tail do
                    //破線：境界線(間の仕切り)
                    let x1 = xMin-50.0+frameMargin c*float sectionCount
                    let x2 = xMax+50.0-frameMargin c*float sectionCount
                    let y1 = y-25.0
                    html(c).fig p0 <| fun (f,_) ->
                        //破線：条件分岐の横線
                        f.line Style[stroke.color (colorBranchFrame c); stroke.width (frameBorder c); stroke.dasharray [2]]
                            <| position(x1,y1)
                            <| position(x2,y1)
                //下にframeMargin分のスペースを作る
                expr.extendLifeLine c (frameMargin c)
                //枠の座標と枠の深さのリストから使った要素以外を残す(使った分を取り除く)
                setFrameStack c <| (frameStack c).Tail
                // 外側のループ枠をマージン分広げる
                frameStack c
                |> List.map (fun (xmin,xmax,ymin,ymax) -> xmin,xmax,ymin,ymax+frameMargin c)
                |> setFrameStack c
                //先頭の分岐処理枠を削除
                setBranchStack c <| (branchStack c).Tail

            member this.evalHS(c:program) =
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
                            expr.forLoopHS (GenerationContext.ForInternalProgram c) (n1,n2) <| fun i ->
                                // 加算・代入処理
                                expr.substHS u (Add(t,u, f i)) c
                            u)) pl
                    |IfEl(cond,n1,n2) ->
                        eval (Let(n1.etype, NaN, fun x ->
                            expr.branchHS (GenerationContext.ForInternalProgram c) <| fun (ifcode,_,elsecode) ->
                                ifcode cond <| fun () ->
                                    expr.substHS x n1 c
                                elsecode <| fun () ->
                                    expr.substHS x n2 c
                            x)) pl
                    |NaN -> "NaN", pl
                let t,_ = eval this 0
                t
