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
        /// 上側マージン
        let mutable topMargin = 40.0
        /// 左側マージン
        let mutable leftMargin = 40.0
        /// 変数間の間隔
        let mutable varInterval = 150.0
        /// 単一代入文の矢印、基準線から代入先までの矢印の長さ
        let mutable singleArrowLength = varInterval/4.0
        /// 変数ヘッダーの横幅
        let mutable varHeaderWidth = 50.0
        /// 変数ヘッダーの高さ
        let mutable varHeaderHeight = 20.0
        /// 線の太さ
        let mutable lineWidth = 2.0
        /// 実効線の太さ
        let mutable activeLineWidth = 10.0
        // 枠のマージン
        let mutable frameMargin = 10.0
        /// 図形描画の時間方向間隔
        let mutable timeStep = 10.0 
        /// 枠線の太さ
        let mutable frameBorder = 2.0
        /// 現在のライフライン終端座標
        let mutable colorActiveLine = "rgba(0, 191, 255, 0.5)"
        let mutable colorLoopFrame = "rgb(255, 0, 0)"
        let mutable colorBranchFrame = "rgb(0, 180, 0)"
        let mutable colorSectionFrame = "rgb(127,0,255)"
        let setSequenceDiagramStyle(s:SequenceDiagramStyle) =
            topMargin <- s.TopMargin
            leftMargin <- s.LeftMargin
            varInterval <- s.VarInterval
            singleArrowLength <- s.SingleArrowLength
            varHeaderWidth <- s.VarHeaderWidth
            varHeaderHeight <- s.VarHeaderHeight
            lineWidth <- s.LineWidth
            activeLineWidth <- s.ActiveLineWidth
            frameMargin <- s.FrameMargin
            timeStep <- s.TimeStep
            frameBorder <- s.FrameBorder
            colorActiveLine <- s.ColorActiveLine
            colorLoopFrame <- s.ColorLoopFrame
            colorBranchFrame <- s.ColorBranchFrame
            colorSectionFrame <- s.ColorSectionFrame
            
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
        let mutable terminalLifeLine = 100.0
        /// シーケンス図に描画済み変数リスト
        let mutable varList:list<string*int*float> = []
        /// フレーム枠座標スタックリスト
        let mutable frameStack:list<float*float*float*float> = []
        /// 条件分岐枠スタックリスト
        let mutable branchStack:list<list<string*float>> = []
        /// 第n変数ライフラインのx座標
        let lifeLineX(n:int) = leftMargin + varHeaderWidth / 2.0 + float n * varInterval

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
                            match List.tryFind (fun (label,_,_) -> label=vname) varList with
                            |Some d -> 
                                // lstに追加
                                lst@[d]
                            |None ->
                                let varCount = varList.Length
                                // dicにも未登録のためここで追加する
                                varList <- varList@[vname,varCount,terminalLifeLine]
                                // シーケンス図に追加
                                let x = 
                                    html.blockTextcode 
                                        <| styleVarHead
                                        <| p0.shift(leftMargin+varInterval*float varCount,topMargin)
                                        <| (varHeaderWidth,varHeaderHeight)
                                        <| (frameBorder,"solid","#000000")
                                        <| ["\\(" + e.evalHS c + "\\)"]
                                //現在位置までライフライン描画
                                expr.drawLifeLine(lifeLineX varCount,x.Bottom,terminalLifeLine)
                                // lstに追加
                                lst@[vname,varCount,terminalLifeLine]
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
                
            static member fig (p:position) code =
                let f = figure()
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
            static member drawLifeLine(x:float,y1:float,y2:float) =
                expr.fig p0 <| fun (f,_) ->
                    //破線：classの縦線
                    f.line Style[stroke.color "black"; stroke.width 1.0; stroke.dasharray [5; 3]]
                        <| position(x,y1)
                        <| position(x,y2)
                        
            /// 水平線を描画
            static member drawHorizontalLine(x1:float, x2:float, y:float) =
                html.fig p0 <| fun (f,_) ->
                    f.line Style[stroke.color "black"; stroke.width lineWidth]
                        <| position(x1, y)
                        <| position(x2, y)
                        
            /// 水平矢印線を描画
            static member drawHorizontalArrowLine(x1:float, x2:float, y:float) =
                html.fig p0 <| fun (f,_) ->
                    f.linearrow Style[stroke.color "black";]
                        <| position(x1, y)
                        <| position(x2, y)
                        <| 2
                        
            //基準線
            static member drawVerticalLine(x:float,y1:float,y2:float) =
                html.fig p0 <| fun (f,p) ->
                    //基準線(縦線)：代入元の1番目から代入先まで(y軸)
                    f.line Style[stroke.color "black"; stroke.width lineWidth]
                        <| position(x, y1)
                        <| position(x, y2)
                        
            /// ライフライン(アクティブ)を描画
            static member drawActiveLine(x:float, y1:float, y2:float, color:string) =
                html.fig p0 <| fun (f,p) ->
                    //実行線
                    f.line Style[stroke.color color; stroke.width activeLineWidth]
                        <| position(x, y1)
                        <| position(x, y2)
                // フレーム枠(左、右、下)更新
                frameStack <- frameStack |> List.map (fun (xMin,xMax,yMin,yMax) ->
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
                    
            /// テキストを描画
            static member drawText(size:int,color:string,weight:string,x:float,y:float,text:string) =
                let p = p0.shift(x,y)
                let s1 = Style [{Key = "margin-left"; Value = p.x.ToString()+"px";}
                                {Key = "margin-top"; Value = p.y.ToString()+"px";}
                                {Key = "position"; Value = "absolute";}
                                font.size size; 
                                font.color color; 
                                font.weight weight]
                html.tagb ("div", s1) <| fun () -> writein text
                
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
                    for _,number,_ in varList do
                        expr.drawLifeLine(lifeLineX number, terminalLifeLine, terminalLifeLine+timeStep*float(start.Length+goal.Length+2))
                    let goalName, goalX, goalY = goal[0]
                    //代入先の実行線
                    expr.drawActiveLine(lifeLineX goalX, goalY, terminalLifeLine+timeStep,colorActiveLine)
                    expr.drawActiveLine(lifeLineX goalX, terminalLifeLine+2.0*timeStep, terminalLifeLine+timeStep*float(start.Length+goal.Length+1),colorActiveLine)
                    // 変数リストのライフラインを更新
                    varList <- varList |> List.map 
                        (fun (name, number, yData) -> 
                            if name=goalName then 
                                name, number, terminalLifeLine+timeStep*float(start.Length+goal.Length+1) 
                            else 
                                name, number, yData)
                    let baseline = lifeLineX goalX + singleArrowLength
                    let arrow_goal = lifeLineX goalX + activeLineWidth/2.0
                    //代入元の変数の数だけ矢印を引く
                    let _,s,_ = start[0]
                    // 代入元から基準線までの矢印
                    if goalX > s then
                        //右矢印：実行中→縦線
                        expr.drawHorizontalLine(lifeLineX s + activeLineWidth/2.0, baseline, terminalLifeLine+timeStep*float(stepCount+1))
                    elif goalX = s then
                        //右矢印：実行中→縦線
                        expr.drawHorizontalLine(lifeLineX s + activeLineWidth/2.0, baseline, terminalLifeLine+timeStep*float(stepCount+1))
                    else
                        //左矢印：実行中→縦線
                        expr.drawHorizontalLine(lifeLineX s - activeLineWidth/2.0, baseline, terminalLifeLine+timeStep*float(stepCount+1))
                    //次の変数の矢印のために1つ下にずらす
                    stepCount <- stepCount + 1
                    //基準線(縦線)：代入元の1番目から代入先まで(y軸)
                    expr.drawVerticalLine(baseline, terminalLifeLine+timeStep, terminalLifeLine+timeStep*float(start.Length+1))
                    //左矢印：基準線から代入先まで(x軸)
                    expr.drawHorizontalArrowLine(baseline, arrow_goal, terminalLifeLine+timeStep*float(start.Length+1))
                    // テキスト（実行内容）
                    expr.drawText(12, "black", "normal", baseline, terminalLifeLine-timeStep, equText)
                    //実行線の下辺からさらに10.0下を描き始めとする
                    terminalLifeLine <- terminalLifeLine+timeStep*float(start.Length+goal.Length+2)
                |0 ->
                    // 定数の代入の場合
                    let equText = "\\(" + eq.evalHS c + "\\)"
                    //存在する変数すべてに破線を引く
                    for _,number,_ in varList do
                        //破線：classの縦線
                        expr.drawLifeLine(lifeLineX number,terminalLifeLine,terminalLifeLine+timeStep*float(start.Length+goal.Length+2))
                    let goalName,goalX,_ = goal[0]
                    //代入先の実行線
                    expr.drawActiveLine(lifeLineX goalX, terminalLifeLine, terminalLifeLine+timeStep*float(start.Length+goal.Length+1),colorActiveLine)
                    varList <- varList |> List.map 
                        (fun (name, number, yData) -> 
                            if name=goalName then 
                                name, number, terminalLifeLine+timeStep*float(start.Length+goal.Length+1) 
                            else 
                                name, number, yData)
                    //左矢印：基準線から代入先まで(x軸)
                    expr.drawHorizontalArrowLine(lifeLineX goalX + singleArrowLength, lifeLineX goalX + activeLineWidth/2.0, terminalLifeLine+timeStep*float(start.Length+1))
                    // テキスト（実行内容）
                    expr.drawText(12,"black","normal", lifeLineX goalX + singleArrowLength, terminalLifeLine-timeStep,equText)
                    //実行線の下辺からさらに10.0下を描き始めとする
                    terminalLifeLine <- terminalLifeLine+timeStep*float(start.Length+goal.Length+2)
                |_ ->
                    let equText = "\\(" + eq.evalHS c + "\\)"
                    //存在する変数すべてに破線を引く
                    for _,number,_ in varList do
                        //破線：classの縦線
                        expr.drawLifeLine(lifeLineX number, terminalLifeLine, terminalLifeLine+timeStep*float(start.Length+goal.Length+2))
                    for goalName,goalX,_ in goal do
                        //代入先の実行線
                        expr.drawActiveLine(lifeLineX goalX, terminalLifeLine, terminalLifeLine+timeStep*float(start.Length+goal.Length+1), colorActiveLine)
                        varList <- varList |> List.map 
                            (fun (name, number, yData) -> 
                                if name=goalName then 
                                    name, number, terminalLifeLine+timeStep*float(start.Length+goal.Length+1) 
                                else 
                                    name, number, yData)
                        let left,right = 
                            start 
                            |> List.fold (fun (l,r) (_,x,_) -> 
                                if goalX > x then l+1,r 
                                elif goalX < x then l,r+1
                                else l,r) (0,0)
                        //基準線が左側の場合(右矢印)
                        if left > right then
                            let baseline = lifeLineX goalX - singleArrowLength
                            let arrow_goal = lifeLineX goalX - activeLineWidth/2.0
                            //代入元の変数の数だけ実行線を引く
                            for label,s,y in start do
                                //代入元の実行線
                                expr.drawActiveLine(lifeLineX s,y,terminalLifeLine+timeStep*float(start.Length+goal.Length+1),colorActiveLine)
                                varList <- varList |> List.map 
                                    (fun (name, number, yData) -> 
                                        if name=label then 
                                            name, number, terminalLifeLine+timeStep*float(start.Length+goal.Length+1)
                                        else 
                                            name, number, yData)
                            //代入元の変数の数だけ矢印を引く
                            for _,s,_ in start do
                                // 代入元から基準線までの矢印
                                if goalX > s then
                                    //右矢印：実行中→縦線
                                    expr.drawHorizontalLine(lifeLineX s + activeLineWidth/2.0, baseline, terminalLifeLine+timeStep*float(stepCount+1))
                                else
                                    //左矢印：実行中→縦線
                                    expr.drawHorizontalLine(lifeLineX s - activeLineWidth/2.0, baseline, terminalLifeLine+timeStep*float(stepCount+1))
                                //次の変数の矢印のために1つ下にずらす
                                stepCount <- stepCount + 1
                            //基準線(縦線)：代入元の1番目から代入先まで(y軸)
                            expr.drawVerticalLine(baseline, terminalLifeLine+timeStep,terminalLifeLine+timeStep*float(start.Length+1))
                            //右矢印：基準線から代入先まで(x軸)
                            expr.drawHorizontalArrowLine(baseline,arrow_goal,terminalLifeLine+timeStep*float(start.Length+1))
                            // テキスト（実行内容）
                            expr.drawText(12,"black","normal",baseline,terminalLifeLine-timeStep,equText)
                        //基準線が右側の場合(左矢印)
                        else
                            let baseline = lifeLineX goalX + singleArrowLength
                            let arrow_goal = lifeLineX goalX + activeLineWidth/2.0
                            //代入元の変数の数だけ実行線を引く
                            for label,s,y in start do
                                //代入元の実行線
                                expr.drawActiveLine(lifeLineX s, y, terminalLifeLine+timeStep*float(start.Length+goal.Length+1),colorActiveLine)
                                varList <- varList |> List.map 
                                    (fun (name, number, yData) -> 
                                        if name=label then 
                                            name, number, terminalLifeLine+timeStep*float(start.Length+goal.Length+1)
                                        else 
                                            name, number, yData)
                            //代入元の変数の数だけ矢印を引く
                            for label,s,y in start do
                                // 代入元から基準線までの矢印
                                if goalX >= s then
                                    //右矢印：実行中→縦線
                                    expr.drawHorizontalLine(lifeLineX s + activeLineWidth/2.0,baseline,terminalLifeLine+timeStep*float(stepCount+1))
                                else //goalX < s then
                                    //左矢印：実行中→縦線
                                    expr.drawHorizontalLine(lifeLineX s - activeLineWidth/2.0,baseline,terminalLifeLine+timeStep*float(stepCount+1))
                                //次の変数の矢印のために1つ下にずらす
                                stepCount <- stepCount + 1
                            //基準線(縦線)：代入元の1番目から代入先まで(y軸)
                            expr.drawVerticalLine(baseline,terminalLifeLine+timeStep,terminalLifeLine+timeStep*float(start.Length+1))
                            //左矢印：基準線から代入先まで(x軸)
                            expr.drawHorizontalArrowLine(baseline,arrow_goal,terminalLifeLine+timeStep*float(start.Length+1))
                            // テキスト（実行内容）
                            expr.drawText(12,"black","normal",baseline,terminalLifeLine-timeStep,equText)
                    //実行線の下辺からさらにtimeStep分延ばす
                    terminalLifeLine <- terminalLifeLine+timeStep*float(start.Length+goal.Length+2)
                    
            static member equivHS (x:expr) (y:expr) (c:program) =
                c.codewritein (x.evalHS c  + " = " + y.evalHS c)
                
            static member equivAlignHS (x:expr) (y:expr) (c:program) =
                c.codewritein (x.evalHS c  + " =& " + y.evalHS c)
                
            //破線(実行線や枠との(y座標の)隙間をつくるため)
            static member extendLifeLine(gap:float) =
                //存在する変数すべてに破線を引く
                for _,number,_ in varList do
                    //破線：classの縦線
                    expr.drawLifeLine(lifeLineX number,terminalLifeLine,terminalLifeLine+gap)
                terminalLifeLine <- terminalLifeLine + gap
                
            //色線(枠用)
            static member colorLine(x1:float,y1:float,x2:float,y2:float,color:string) =
                html.fig p0 <| fun (f,_) ->
                    f.line Style[stroke.color color; stroke.width frameBorder]
                        <| position(x1,y1)
                        <| position(x2,y2)
                        
            //ループの枠
            static member rectangle(startPoint_x:float,startPoint_y:float,endPoint_x:float,endPoint_y:float,color:string) =
                //上辺:左上から右上
                expr.colorLine(startPoint_x,startPoint_y,endPoint_x,startPoint_y,color)
                //右辺:右上から右下
                expr.colorLine(endPoint_x,startPoint_y,endPoint_x,endPoint_y,color)
                //下辺:右下から左下
                expr.colorLine(endPoint_x,endPoint_y,startPoint_x,endPoint_y,color)
                //左辺:左下から左上
                expr.colorLine(startPoint_x,endPoint_y,startPoint_x,startPoint_y,color)
                
            static member sectionHS (label:string) = fun code -> 
                //上に20.0破線のスペースを作る
                expr.extendLifeLine 20.0
                frameStack <- (0.0, 0.0, terminalLifeLine - 5.0, terminalLifeLine)::frameStack
                //stack内の要素の個数(デフォルト1個)-1個を枠の深さ(sectionCount)とする
                let sectionCount = frameStack.Length-1
                code()
                //最後に入れた枠の座標と枠の深さの数値を各変数に代入する(この段階ではstackに変化はない)
                let xMin,xMax,yMin,yMax = frameStack.Head
                // ループの枠
                expr.rectangle(xMin-50.0+frameMargin*float sectionCount,yMin,xMax+50.0-frameMargin*float sectionCount,yMax+5.0,colorSectionFrame)
                // テキスト（グループ名）
                expr.drawText(12,colorSectionFrame,"normal",xMin-50.0+frameMargin*float sectionCount,yMin-15.0,label)
                //下にframeMargin分のスペースを作る
                expr.extendLifeLine frameMargin
                //枠の座標と枠の深さのリストから使った要素以外を残す(使った分を取り除く)
                frameStack <- frameStack.Tail
                // 外側のループ枠をframeMargin分広げる
                frameStack <- frameStack |> List.map (fun (xmin,xmax,ymin,ymax) -> xmin,xmax,ymin,ymax+frameMargin)
                
            static member forLoopHS (c:program) (n1:expr,n2:expr) code =
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
            static member loopHS (c:program) code =
                let iname,returnVar = c.i0.getVar()
                let i = Var(It 4, iname, NaN)
                let label = gotoLabel.nextGotoLabel()
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
            static member whiledoHS (c:program) (cond:expr) = fun code ->
                c.codewritein("<summary><span class=\"op-loop\">while</span> \\(" + cond.evalHS c + "\\)</summary>")
                c.codewritein "<div class=\"insidecode-loop\">"
                c.indentInc()
                code()
                c.indentDec()
                c.codewritein "</div>"
                
            ///<summary>指定した範囲でループ</summary>
            static member rangeHS (c:program) (i1:expr) = fun (i2:expr) -> fun code -> 
                //カウンター変数の取得
                let iname,returnVar = c.i0.getVar()
                let i = Var(It 4, iname, NaN)
                //上に20.0破線のスペースを作る
                expr.extendLifeLine 20.0
                frameStack <- (0.0, 0.0, terminalLifeLine - 5.0, terminalLifeLine)::frameStack
                //stack内の要素の個数(デフォルト1個)-1個を枠の深さ(sectionCount)とする
                let sectionCount = frameStack.Length-1
                let counter_Var = expr.addVarList(i,c)
                for countName, count_number, y in counter_Var do
                    //実行線
                    expr.drawActiveLine(lifeLineX count_number, terminalLifeLine-timeStep, terminalLifeLine, colorLoopFrame)
                    // テキスト（ループ範囲）
                    expr.drawText(12,colorLoopFrame,"normal",lifeLineX count_number + timeStep, terminalLifeLine - 25.0,"\\(" + i1.evalHS c + " \\rightarrow " + i2.evalHS c + "\\)")
                code i
                //最後に入れた枠の座標と枠の深さの数値を各変数に代入する(この段階ではstackに変化はない)
                let xMin,xMax,yMin,yMax = frameStack.Head
                // ループの枠
                expr.rectangle(xMin-50.0+frameMargin*float sectionCount,yMin,xMax+50.0-frameMargin*float sectionCount,yMax+5.0,colorLoopFrame)
                // テキスト（グループ名）
                expr.drawText(12,colorLoopFrame,"normal",xMin-50.0+frameMargin*float sectionCount,yMin-15.0,"\\(\\mathrm{For}\\)")
                //下にframeMargin分のスペースを作る
                expr.extendLifeLine frameMargin
                //枠の座標と枠の深さのリストから使った要素以外を残す(使った分を取り除く)
                frameStack <- frameStack.Tail
                // 外側のループ枠をframeMargin分広げる
                frameStack <- frameStack |> List.map (fun (xmin,xmax,ymin,ymax) -> xmin,xmax,ymin,ymax+frameMargin)
                // 使用済みカウンタ変数を返却し再利用可能にする
                returnVar()
                
            ///<summary>指定した範囲でループ(途中脱出可)</summary>
            static member range_exitHS (c:program) (i1:expr) = fun (i2:expr) -> fun code -> 
                match i1,i2 with
                |Int a, Int b when a>b -> 
                    let iname,returnVar = c.i0.getVar()
                    let i = Var(It 4, iname, NaN)
                    let label = gotoLabel.nextGotoLabel()
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
                    let iname,returnVar = c.i0.getVar()
                    let i = Var(It 4, iname, NaN)
                    let label = gotoLabel.nextGotoLabel()
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
                    
            static member branchHS (c:program) code =
                //新しい分岐処理枠を追加
                branchStack <- []::branchStack
                let ifcode (cond:expr) code =
                    //上に30.0破線のスペースを作る
                    expr.extendLifeLine 30.0
                    // 現在の分岐処理枠に条件式とy座標追加
                    branchStack <- (branchStack.Head@["\\(" + cond.evalHS c + "\\)",terminalLifeLine])::branchStack.Tail
                    frameStack <- (0.0, 0.0, terminalLifeLine - 5.0, terminalLifeLine)::frameStack
                    code()
                    //中に20.0破線のスペースを作る
                    expr.extendLifeLine 20.0
                    //境界線のy座標をスタック用のリストに入れる
                let elseifcode (cond:expr) code =
                    //中に20.0破線のスペースを作る
                    expr.extendLifeLine 20.0
                    // 現在の分岐処理枠に条件式とy座標追加
                    branchStack <- (branchStack.Head@["\\(" + cond.evalHS c + "\\)",terminalLifeLine])::branchStack.Tail
                    code()
                let elsecode code =
                    // 現在の分岐処理枠に条件式とy座標追加
                    branchStack <- (branchStack.Head@["\\(\\mathrm{Else}\\)",terminalLifeLine])::branchStack.Tail
                    code()
                    
                code(ifcode,elseifcode,elsecode)
                
                //最後に入れた枠の座標と枠の深さの数値を各変数に代入する(この段階ではstackに変化はない)
                let xMin,xMax,yMin,yMax = frameStack.Head
                //stack内の要素の個数(デフォルト1個)-1個を枠の深さ(sectionCount)とする
                let sectionCount = frameStack.Length-1
                expr.rectangle(xMin-50.0+frameMargin*float sectionCount,yMin-20.0,xMax+50.0-frameMargin*float sectionCount,yMax+5.0,colorBranchFrame)
                for cond,y in branchStack.Head do
                    // テキスト（条件式）
                    expr.drawText(12,colorBranchFrame,"normal",5.0+xMin-50.0+frameMargin*float sectionCount,y-25.0,cond)
                for _,y in branchStack.Head.Tail do
                    //破線：境界線(間の仕切り)
                    let x1 = xMin-50.0+frameMargin*float sectionCount
                    let x2 = xMax+50.0-frameMargin*float sectionCount
                    let y1 = y-25.0
                    html.fig p0 <| fun (f,_) ->
                        //破線：条件分岐の横線
                        f.line Style[stroke.color colorBranchFrame; stroke.width frameBorder; stroke.dasharray [2]]
                            <| position(x1,y1)
                            <| position(x2,y1)
                //下にframeMargin分のスペースを作る
                expr.extendLifeLine frameMargin
                //枠の座標と枠の深さのリストから使った要素以外を残す(使った分を取り除く)
                frameStack <- frameStack.Tail
                // 外側のループ枠をマージン分広げる
                frameStack <- frameStack |> List.map (fun (xmin,xmax,ymin,ymax) -> xmin,xmax,ymin,ymax+frameMargin)
                //先頭の分岐処理枠を削除
                branchStack <- branchStack.Tail
                
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
                        |(Add _|Sub _),(Int _| Dbl _) ->
                            let x,nx = eval x pl
                            let y,ny = eval y pl
                            y + par x (nx+1), max (nx+1) ny
                        |_,(Int _| Dbl _) ->
                            let x,nx = eval x pl
                            let y,ny = eval y pl
                            x + y, max nx ny
                        |(Add _|Sub _),(Add _|Sub _) ->
                            let x,nx = eval x pl
                            let y,ny = eval y pl
                            par x nx + par y ny, max (nx+1) (ny+1)
                        |(Add _|Sub _),_ ->
                            let x,nx = eval x pl
                            let y,ny = eval y pl
                            par x nx + y, max (nx+1) ny
                        |_,(Add _|Sub _) ->
                            let x,nx = eval x pl
                            let y,ny = eval y pl
                            x + par y ny, max nx (ny+1)
                        |_ ->
                            let x,nx = eval x pl
                            let y,ny = eval y pl
                            x + y, max nx ny
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
                        let x,nx = eval x pl
                        let y,ny = eval y pl
                        x + " ** " + y, pl
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
