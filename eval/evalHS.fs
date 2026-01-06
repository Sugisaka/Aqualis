// 
// Copyright (c) 2026 Jun-ichiro Sugisaka
// 
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
// 
namespace Aqualis
    
    open System
    
    [<AutoOpen>]
    module value =
        
        /// classのy軸の位置
        let y40 = 40.0
        /// classーキャプション間
        let gap20 = 20.0
        /// キャプション
        let y20 = y40-gap20 //20.0
        /// class1
        let x0 = 0.0
        /// class2
        let x250 = 250.0

        /// classのサイズ
        let classWidth = 90.0 //横幅 (100.0)
        let classHeight = 40.0 //高さ (50.0)

        /// 時間軸
        let dashed_x50 = (classWidth+10.0)/2.0 //50.0

        /// 実行中
        /// 実行中のx座標の調整
        let gap15 = 15.0 
        /// 実行中のx座標 (35.0)
        let x35 = dashed_x50-gap15 
        let text_x160 = (x0+x35+x250+x35)/2.0
        /// 代入先の実行中の間隔調整
        let gap5 = 5.0
        let run_width = 10.0

        //線,テキスト
        /// 横線のy座標と繰り返しのテキストのx座標の調整
        let gap10 = 10.0 
        let mutable frame_top_list  :list<float> = []
        let mutable frame_bottom = 0.0
        let mutable class_number = -1.0
        let mutable count = -1.0
        let mutable sectionCount2 = 0.0
        let mutable dic:list<string*float*float> = []
        let mutable parameters:list<string*float*float> = []
        let mutable left = 0.0
        let mutable right = 0.0
        let mutable baseline = 0.0
        let mutable arrow_goal = 0.0
        let mutable text_position = 0.0
        let mutable count2 = -1.0
        let mutable run_top = 190.0
        let mutable distinction = 0.0
        let mutable stack  :list<float*float*float*float*int> = []
        let mutable start_goal = 0.0
        let mutable yMemry = 0.0
        let mutable for_check = 0.0
        let mutable branch_elifCheck = 0.0

        let mutable branch_text_yPoint_list:list<float> = []
        let mutable branch_text_name_list:list<string> = []
        let mutable branch_IFELSE_IFELSE_check = 0.0
        let mutable count_branch = 0.0
        let mutable count_branch_memory_list:list<float> = []
        let mutable boundaryLine = 0.0
        let mutable branch_frame_top_list:list<float> = []
        let mutable boundaryY:list<float> = []
        let mutable branch_frame_top = 0.0
        
    [<AutoOpen>]
    module seqStyles =
        //ページ左上角
        let p0 = position.Origin        
        //スタイル：class1
        let style_class1 = Style[font.size 10; font.color "black"; font.weight "normal"; border.style "1px solid"; area.backGroundColor "#BBEEFF"; font.lineHeight 14; padding.all 5]
        //スタイル：class2
        let style_class2  = Style[font.size 10; font.color "black"; font.weight "normal"; border.style "1px solid"; area.backGroundColor "#FFEEBB"; font.lineHeight 14; padding.all 5]
        
    [<AutoOpen>]
    module exprEvalHS =
        
        type expr with
            
            /// 変数用
            static member varList (e:expr,c:program) = 
                let rec makeList (e:expr) =
                    match e with
                    |Int n -> []
                    |Dbl x -> []
                    |Var (t,v,p) -> 
                        expr.draw_class (Var (t,v,p),c)
                        match List.tryFind (fun (label,x,y) -> label=v) parameters with
                        |Some (_,x,y) ->
                            [(v,x,y)]
                        |None ->
                            []
                    |Add (_,a,b) ->
                        makeList a @ makeList b
                    |Sub (_,a,b) ->
                        makeList a @ makeList b
                    |Mul (_,a,b) ->
                        makeList a @ makeList b
                    |Div (_,a,b) ->
                        makeList a @ makeList b
                    |Pow (_,a,b) ->
                        makeList a @ makeList b
                    |Sin (_,a) ->
                        makeList a
                    |Cos (_,a) ->
                        makeList a
                    |Tan (_,a) ->
                        makeList a
                    |Asin (_,a) ->
                        makeList a
                    |Acos (_,a) ->
                        makeList a
                    |Atan (_,a) ->
                        makeList a
                    |Atan2 (a,b) ->
                        makeList a @ makeList b
                    |Exp (_,a) ->
                        makeList a
                    |Log (_,a) ->
                        makeList a
                    |Log10 (_,a) ->
                        makeList a
                    |Sqrt (_,a) ->
                        makeList a
                    |Abs (_,a) ->
                        makeList a
                    |Eq (a,b) ->
                        makeList a @ makeList b
                    |NEq (a,b) ->
                        makeList a @ makeList b
                    |Greater (a,b) ->
                        makeList a @ makeList b
                    |Less (a,b) ->
                        makeList a @ makeList b
                    |GreaterEq (a,b) ->
                        makeList a @ makeList b
                    |LessEq (a,b) ->
                        makeList a @ makeList b
                    |_ -> []
                    
                e
                |> makeList
                |> List.distinct
                
            /// 定数用
            static member numList (e:expr) = 
                let rec makeList (e:expr) =
                    match e with
                    |Int n -> [(float n,0.0)]
                    |Dbl x -> [(x,0.0)]
                    |Var _ -> []
                    |Add (_,a,b) ->
                        makeList a @ makeList b
                    |Sub (_,a,b) ->
                        makeList a @ makeList b
                    |Mul (_,a,b) ->
                        makeList a @ makeList b
                    |Div (_,a,b) ->
                        makeList a @ makeList b
                    |Pow (_,a,b) ->
                        makeList a @ makeList b
                    |Sin (_,a) ->
                        makeList a
                    |Cos (_,a) ->
                        makeList a
                    |Tan (_,a) ->
                        makeList a
                    |Asin (_,a) ->
                        makeList a
                    |Acos (_,a) ->
                        makeList a
                    |Atan (_,a) ->
                        makeList a
                    |Atan2 (a,b) ->
                        makeList a @ makeList b
                    |Exp (_,a) ->
                        makeList a
                    |Log (_,a) ->
                        makeList a
                    |Log10 (_,a) ->
                        makeList a
                    |Sqrt (_,a) ->
                        makeList a
                    |Abs (_,a) ->
                        makeList a
                    |Eq (a,b) ->
                        makeList a @ makeList b
                    |NEq (a,b) ->
                        makeList a @ makeList b
                    |Greater (a,b) ->
                        makeList a @ makeList b
                    |Less (a,b) ->
                        makeList a @ makeList b
                    |GreaterEq (a,b) ->
                        makeList a @ makeList b
                    |LessEq (a,b) ->
                        makeList a @ makeList b
                    |_ -> []
                    
                e
                |> makeList
                |> List.distinct
                
            static member reset_parameters() =
                parameters <- []
                
            static member reset_count2() =
                count2 <- -1.0
                
            static member next() =
                expr.reset_parameters()
                expr.reset_count2()
                
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
                
            //破線(縦線)
            static member drawDasharray(x:float,y1:float,y2:float) =
                expr.fig p0 <| fun (f,p) ->
                    //破線：classの縦線
                    f.line Style[stroke.color "black"; stroke.dasharray 5.0]
                        <| position(x,y1)
                        <| position(x,y2)
                        
            //破線(条件を分ける横線)
            static member drawDasharray2(x1:float,x2:float,y1:float) =
                html.fig p0 <| fun (f,p) ->
                    //破線：条件分岐の横線
                    f.line Style[stroke.color "green"; stroke.dasharray 10.0]
                        <| position(x1,y1)
                        <| position(x2,y1)
            //矢印
            static member drawLineArrow(x1:float, x2:float, y:float) =
                html.fig p0 <| fun (f,p) ->
                    f.linearrow Style[stroke.color "black";]
                        <| position(x1, y)
                        <| position(x2, y)
                        <| 2
                        
            //基準線
            static member drawBaseline(x:float,y1:float,y2:float) =
                html.fig p0 <| fun (f,p) ->
                    //基準線(縦線)：代入元の1番目から代入先まで(y軸)
                    f.line Style[stroke.color "black"; stroke.width 2.0]
                        <| position(x, y1)
                        <| position(x, y2)
                        
            //代入元に変数があるか確かめる
            static member check_start(list, formula) =
                if list = [] then //変数がない
                    let start = expr.numList formula
                    distinction <- 1.0
                else //変数がある
                    distinction <- 0.0
                    
            /// 変数名
            static member draw_class(class_name:expr,c:program) =
                let class_nameText = "\\("+class_name.evalHS c + "\\)"
                
                match class_name with
                |Var (_,class_name,_) ->
                    match List.tryFind (fun (label,x,y) -> label=class_name) dic with
                    |Some (_,x,y) ->
                        class_number <- x
                        yMemry <- y

                    |None ->
                        count <- count + 1.0
                        // class_nameを辞書に追加
                        dic <- dic@[class_name,count,run_top]
                        class_number <- count
                        if for_check = 0.0 then
                            yMemry <- 0.0
                        else //for_check = 1.0
                            yMemry <- run_top
                        // yMemry <- 0.0

                        //class1
                        let x_1 = html.blockTextcode style_class1
                                <| p0.shift(x250*class_number,y40)
                                <| (classWidth,classHeight) // (90.0,40.0) 横100.0 縦50.0
                                <| [class_nameText] //x
                                
                        //破線：classの縦線
                        expr.drawDasharray(x250*class_number+dashed_x50,x_1.Bottom,run_top)
                        
                    match List.tryFind (fun (label,x,y) -> label=class_name) parameters with
                    |Some x ->
                        ()
                    |None ->
                        // class_nameをリストに追加
                        parameters <- parameters@[class_name, class_number, yMemry]
                |_ ->
                    ()
                    
            /// 実行線
            static member drawActiveLine(x:float, y1:float, y2:float, color:string) =
                html.fig p0 <| fun (f,p) ->
                    //実行線
                    f.line Style[stroke.color color; stroke.width run_width]
                        <| position(x, y1)
                        <| position(x, y2)
                        
                let updateRange(xMin,xMax,yMin,yMax,insideSection) =
                    let xMin' = 
                        if xMin = 0.0 then x 
                        elif x < xMin then x 
                        else xMin 
                    let xMax' = 
                        if xMax = 0.0 then x 
                        elif x > xMax then x 
                        else xMax
                    let yMin' =  
                        if yMin = 0.0 then y1 
                        elif y1 < yMin && y1 < y2 then y1 
                        elif y2 < yMin && y2 < y1 then y2 
                        else yMin
                    let yMax' = 
                        if yMax = 0.0 then y2 
                        elif y1 > yMax && y1 > y2 then y1 
                        elif y2 > yMax && y2 > y1 then y2 
                        else yMax
                    xMin',xMax',yMin',yMax',insideSection
                    
                stack <- List.map (fun xxyy -> updateRange xxyy) stack
                
            static member text (s:Style) = fun (p:position) (text:string) ->
                // html.writeTag "div" ("style = \"font-size:"+s.size.ToString()+ "px; margin-left:"+p.x.ToString()+"px; margin-top:"+p.y.ToString()+"px; color: "+s.color+"; white-space: nowrap; position: absolute;\"") <| fun () ->
                let s1 = Style [{Key = "margin-left"; Value = p.x.ToString()+"px";}
                                {Key = "margin-top"; Value = p.y.ToString()+"px";}
                                {Key = "position"; Value = "absolute";}]
                html.tagb ("div", s1+s) <| fun () ->
                    writein text
                    
            /// テキスト
            static member drawText(size:int,color:string,weight:string,x:float,y:float,text:string) =
                // テキスト（実行内容）
                expr.text 
                    <| Style[font.size size; font.color color; font.weight weight]
                    <| p0.shift(x,y)
                    <| text
                    
            /// 代入
            static member drawBranchArrow_multiple_list(start:list<string*float*float>,goal:list<string*float*float>,eq:expr,c:program) =
                let getName = fun (name,_,_) -> name
                if start.Length=1 && goal.Length=1 && getName start[0] = getName goal[0] then
                    
                    let equText = "\\(" + eq.evalHS c + "\\)"

                    //存在する変数すべてに破線を引く
                    for name, number, yData in dic do
                        //破線：classの縦線
                        expr.drawDasharray(dashed_x50+x250*number,run_top,run_top+gap10*(float start.Length+float goal.Length+2.0))

                    for goalName, goalX, goalY in goal do
                        //代入先の実行線
                        expr.drawActiveLine(x250*goalX+dashed_x50, goalY, run_top+gap10,"rgba(0, 191, 255, 0.5)")
                        expr.drawActiveLine(x250*goalX+dashed_x50, run_top+2.0*gap10, run_top+gap10*(float start.Length+float goal.Length+1.0),"rgba(0, 191, 255, 0.5)")
                        
                        dic <- dic |> List.map 
                            (fun (name, number, yData) -> 
                                if name=goalName then 
                                    name, number, run_top+gap10*(float start.Length+float goal.Length+1.0) 
                                else 
                                    name, number, yData)
                                    
                        //代入元に変数があるとき
                        if distinction = 0.0 then

                            for label,x,y in start do
                                // 基準線の左右分けの計算
                                if goalX > x then
                                    left <- left + 1.0
                                elif goalX = x then
                                    ()
                                else //goalX < x then
                                    right <- right + 1.0
                                    
                            //基準線が左側の場合(右矢印)
                            if left > right then
                                baseline <- x250*goalX
                                arrow_goal <- x250*goalX+dashed_x50-run_width/2.0
                                text_position <- x250*(goalX-1.0)+text_x160
                                
                                //代入元の変数の数だけ矢印を引く
                                for label,s,y in start do
                                    //次の変数の矢印のために1つ下にずらす
                                    count2 <- count2 + 1.0
                                    // 代入元から基準線までの矢印
                                    if goalX > s then
                                        //右矢印：実行中→縦線
                                        expr.drawLineArrow(x250*s+dashed_x50+run_width/2.0,baseline,run_top+gap10*(count2+1.0))
                                    elif goalX = s then
                                        //左矢印：実行中→縦線
                                        expr.drawLineArrow(x250*s+dashed_x50-run_width/2.0,baseline,run_top+gap10*(count2+1.0))
                                    else //goalX < s then
                                        //左矢印：実行中→縦線
                                        expr.drawLineArrow(x250*s+dashed_x50-run_width/2.0,baseline,run_top+gap10*(count2+1.0))
                                        
                                //基準線(縦線)：代入元の1番目から代入先まで(y軸)
                                expr.drawBaseline(baseline,run_top+gap10,run_top+gap10*(float start.Length+1.0))
                                //右矢印：基準線から代入先まで(x軸)
                                expr.drawLineArrow(baseline,arrow_goal,run_top+gap10*(float start.Length+1.0))
                                // テキスト（実行内容）
                                expr.drawText(12,"black","normal",text_position,run_top-gap10,equText)
                                
                            //基準線が右側の場合(左矢印)
                            else
                                baseline <- 130.0 + x250*goalX
                                arrow_goal <- x250*goalX+dashed_x50+run_width/2.0
                                text_position <- x250*goalX+text_x160
                                //代入元の変数の数だけ矢印を引く
                                for label,s,y in start do
                                    //次の変数の矢印のために1つ下にずらす
                                    count2 <- count2 + 1.0
                                    // 代入元から基準線までの矢印
                                    if goalX > s then
                                        //右矢印：実行中→縦線
                                        expr.drawLineArrow(x250*s+dashed_x50+run_width/2.0,baseline,run_top+gap10*(count2+1.0))
                                    elif goalX = s then
                                        //右矢印：実行中→縦線
                                        expr.drawLineArrow(x250*s+dashed_x50+run_width/2.0,baseline,run_top+gap10*(count2+1.0))
                                    else //goalX < s then
                                        //左矢印：実行中→縦線
                                        expr.drawLineArrow(x250*s+dashed_x50-run_width/2.0,baseline,run_top+gap10*(count2+1.0))
                                //基準線(縦線)：代入元の1番目から代入先まで(y軸)
                                expr.drawBaseline(baseline,run_top+gap10,run_top+gap10*(float start.Length+1.0))
                                //左矢印：基準線から代入先まで(x軸)
                                expr.drawLineArrow(baseline,arrow_goal,run_top+gap10*(float start.Length+1.0))
                                // テキスト（実行内容）
                                expr.drawText(12,"black","normal",text_position,run_top-gap10,equText)
                                
                        //定数を代入(変数なし)
                        else
                            //左矢印：基準線から代入先まで(x軸)
                            expr.drawLineArrow(x250*goalX+text_x160-5.0,x250*goalX+dashed_x50+run_width/2.0,run_top+gap10*(float start.Length+1.0))
                            // テキスト（実行内容）
                            expr.drawText(12,"black","normal",x250*goalX+text_x160,run_top-gap10,equText)
                            
                    left <- 0.0
                    right <- 0.0
                    //実行線の下辺からさらに10.0下を描き始めとする
                    run_top <- run_top+gap10*(float start.Length+float goal.Length+2.0)
                    expr.next()
                else
                    let equText = "\\(" + eq.evalHS c + "\\)"
                    //存在する変数すべてに破線を引く
                    for name, number, yData in dic do
                        //破線：classの縦線
                        expr.drawDasharray(dashed_x50+x250*number,run_top,run_top+gap10*(float start.Length+float goal.Length+2.0))
                    for goalName, goalX, goalY in goal do
                        //代入先の実行線
                        expr.drawActiveLine(x250*goalX+dashed_x50, run_top, run_top+gap10*(float start.Length+float goal.Length+1.0),"rgba(0, 191, 255, 0.5)")
                        dic <- dic |> List.map 
                            (fun (name, number, yData) -> 
                                if name=goalName then 
                                    name, number, run_top+gap10*(float start.Length+float goal.Length+1.0) 
                                else 
                                    name, number, yData)
                        //代入元に変数があるとき
                        if distinction = 0.0 then
                            for label,x,y in start do
                                // 基準線の左右分けの計算
                                if goalX > x then
                                    left <- left + 1.0
                                elif goalX = x then
                                    ()
                                else //goalX < x then
                                    right <- right + 1.0
                            //基準線が左側の場合(右矢印)
                            if left > right then
                                baseline <- x250*goalX
                                arrow_goal <- x250*goalX+dashed_x50-run_width/2.0
                                text_position <- x250*(goalX-1.0)+text_x160
                                //代入元の変数の数だけ実行線を引く
                                for label,s,y in start do
                                    //代入元の実行線
                                    expr.drawActiveLine(x250*s+dashed_x50,y,run_top+gap10*(float start.Length+float goal.Length+1.0),"rgba(0, 191, 255, 0.5)")
                                    dic <- dic |> List.map 
                                        (fun (name, number, yData) -> 
                                            if name=label then 
                                                name, number, run_top+gap10*(float start.Length+float goal.Length+1.0)
                                            else 
                                                name, number, yData)
                                //代入元の変数の数だけ矢印を引く
                                for label,s,y in start do
                                    //次の変数の矢印のために1つ下にずらす
                                    count2 <- count2 + 1.0
                                    // 代入元から基準線までの矢印
                                    if goalX > s then
                                        //右矢印：実行中→縦線
                                        expr.drawLineArrow(x250*s+dashed_x50+run_width/2.0,baseline,run_top+gap10*(count2+1.0))
                                    elif goalX = s then
                                        //左矢印：実行中→縦線
                                        expr.drawLineArrow(x250*s+dashed_x50-run_width/2.0,baseline,run_top+gap10*(count2+1.0))
                                    else //goalX < s then
                                        //左矢印：実行中→縦線
                                        expr.drawLineArrow(x250*s+dashed_x50-run_width/2.0,baseline,run_top+gap10*(count2+1.0))
                                //基準線(縦線)：代入元の1番目から代入先まで(y軸)
                                expr.drawBaseline(baseline,run_top+gap10,run_top+gap10*(float start.Length+1.0))
                                //右矢印：基準線から代入先まで(x軸)
                                expr.drawLineArrow(baseline,arrow_goal,run_top+gap10*(float start.Length+1.0))
                                // テキスト（実行内容）
                                expr.drawText(12,"black","normal",text_position,run_top-gap10,equText)
                            //基準線が右側の場合(左矢印)
                            else //left <= right then
                                baseline <- 130.0 + x250*goalX
                                arrow_goal <- x250*goalX+dashed_x50+run_width/2.0
                                text_position <- x250*goalX+text_x160
                                //代入元の変数の数だけ実行線を引く
                                for label,s,y in start do
                                    //代入元の実行線
                                    expr.drawActiveLine(x250*s+dashed_x50, y, run_top+gap10*(float start.Length+float goal.Length+1.0),"rgba(0, 191, 255, 0.5)")
                                    dic <- dic |> List.map 
                                        (fun (name, number, yData) -> 
                                            if name=label then 
                                                name, number, run_top+gap10*(float start.Length+float goal.Length+1.0)
                                            else 
                                                name, number, yData)
                                //代入元の変数の数だけ矢印を引く
                                for label,s,y in start do
                                    //次の変数の矢印のために1つ下にずらす
                                    count2 <- count2 + 1.0
                                    // 代入元から基準線までの矢印
                                    if goalX > s then
                                        //右矢印：実行中→縦線
                                        expr.drawLineArrow(x250*s+dashed_x50+run_width/2.0,baseline,run_top+gap10*(count2+1.0))
                                    elif goalX = s then
                                        //右矢印：実行中→縦線
                                        expr.drawLineArrow(x250*s+dashed_x50+run_width/2.0,baseline,run_top+gap10*(count2+1.0))
                                    else //goalX < s then
                                        //左矢印：実行中→縦線
                                        expr.drawLineArrow(x250*s+dashed_x50-run_width/2.0,baseline,run_top+gap10*(count2+1.0))
                                //基準線(縦線)：代入元の1番目から代入先まで(y軸)
                                expr.drawBaseline(baseline,run_top+gap10,run_top+gap10*(float start.Length+1.0))
                                //左矢印：基準線から代入先まで(x軸)
                                expr.drawLineArrow(baseline,arrow_goal,run_top+gap10*(float start.Length+1.0))
                                // テキスト（実行内容）
                                expr.drawText(12,"black","normal",text_position,run_top-gap10,equText)
                        //定数を代入(変数なし)
                        else //distinction = 1.0
                            //左矢印：基準線から代入先まで(x軸)
                            expr.drawLineArrow(x250*goalX+text_x160-5.0,x250*goalX+dashed_x50+run_width/2.0,run_top+gap10*(float start.Length+1.0))
                            // テキスト（実行内容）
                            expr.drawText(12,"black","normal",x250*goalX+text_x160,run_top-gap10,equText)
                    left <- 0.0
                    right <- 0.0
                    //実行線の下辺からさらに10.0下を描き始めとする
                    run_top <- run_top+gap10*(float start.Length+float goal.Length+2.0)
                    expr.next()
                    
            /// 代入式から作図までの処理
            static member assignmentExpression(leftHandSide,rightHandSide,c) =
                start_goal <- 0.0
                let start = expr.varList(rightHandSide,c)
                expr.check_start(start,rightHandSide)
                start_goal <- 1.0
                let goal = expr.varList(leftHandSide,c)
                expr.drawBranchArrow_multiple_list(start,goal,rightHandSide,c)
                
            static member substHS (x:expr) (y:expr) (c:program) =
                start_goal <- 0.0
                let start = expr.varList(y,c)
                expr.check_start(start,y)
                start_goal <- 1.0
                let goal = expr.varList(x,c)
                expr.drawBranchArrow_multiple_list(start,goal,y,c)
                
            static member equivHS (x:expr) (y:expr) (c:program) =
                c.codewritein (x.evalHS c  + " = " + y.evalHS c)
                
            static member equivAlignHS (x:expr) (y:expr) (c:program) =
                c.codewritein (x.evalHS c  + " =& " + y.evalHS c)
                
            //破線(実行線や枠との(y座標の)隙間をつくるため)
            static member space_dasharray(gap:float) =
                //存在する変数すべてに破線を引く
                for name, number, y in value.dic do
                    //破線：classの縦線
                    expr.drawDasharray(dashed_x50+x250*number,value.run_top,value.run_top+gap)
                value.run_top <- value.run_top+gap
                
            //色線(枠用)
            static member colorLine(x1:float,y1:float,x2:float,y2:float,color:string) =
                html.fig p0 <| fun (f,p) ->
                    f.line Style[stroke.color color; stroke.width 2.0]
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
                
            static member reset_frame_bottom() =
                frame_bottom <- 0.0
                
            //code()の上記
            static member aboveTheCode() =
                //枠の上辺のy座標をリスト(frame_top_list)に追加(実行線の描き始めの5.0上)
                frame_top_list <- value.run_top - 5.0 :: frame_top_list
                //枠の下辺のずらす単位を0.0にリセットする
                expr.reset_frame_bottom()
                //枠の座標を決める4つの変数を定義
                let xMin,xMax,yMin,yMax = 0.0, 0.0, 0.0, 0.0
                //枠の座標と枠の深さをセットでリストにする(stack2)
                let stack2 = 
                    [
                        for i in 0..(value.stack.Length-1) do
                            let x1,x2,y1,y2,insideSection = value.stack[i]
                            // インデックスi→内側にi個のセクションが存在することを表す
                            yield x1, x2, y1, y2, if insideSection<i+1 then i+1 else insideSection
                    ]
                //stack2をstackに乗っける(追加)
                value.stack <- (xMin,xMax,yMin,yMax,0)::stack2
                
            /// code()の下記
            static member belowTheCode() =
                //下に10.0破線のスペースを作る
                expr.space_dasharray 10.0
                //枠の上辺のリストから使った要素以外を残す(使った分を取り除く)
                frame_top_list <- frame_top_list.Tail
                //枠の下辺のずらす単位を1.0(1個分)増やす
                frame_bottom <- frame_bottom + 1.0
                //枠の座標と枠の深さのリストから使った要素以外を残す(使った分を取り除く)
                value.stack <- value.stack.Tail
                
            static member forLoopHS (c:program) (n1:expr,n2:expr) code =
                let iname,returnVar = c.i0.getVar()
                let i = Var(It 4, iname, NaN)
                let n1_ = n1.evalHS c
                let n2_ = n2.evalHS c
                c.codewritein "<div class=\"codeblock\">"
                c.codewritein "<details open>"
                c.codewritein("<summary><span class=\"op-loop\">for</span> \\(" + i.evalHS c + "=" + n1_ + "," + n2_ + "\\)</summary>")
                c.codewritein "<div class=\"insidecode-loop\">"
                c.indentInc()
                code i
                c.indentDec()
                c.codewritein "</div>"
                c.codewritein "</details>"
                c.codewritein "</div>"
                returnVar()
                
            ///<summary>無限ループ</summary>
            static member loopHS (c:program) code =
                let iname,returnVar = c.i0.getVar()
                let i = Var(It 4, iname, NaN)
                let label = gotoLabel.nextGotoLabel()
                let exit() = c.codewritein("goto " + label)
                expr.substH i (Int 1) c
                c.codewritein "<div class=\"codeblock\">"
                c.codewritein "<details open>"
                c.codewritein "<summary><span class=\"op-loop\">repeat</span></summary>"
                c.codewritein "<div class=\"insidecode-loop\">"
                c.indentInc()
                code(exit,i)
                expr.substH i (Add(It 4, i, Int 1)) c
                c.indentDec()
                c.codewritein "</div>"
                c.codewritein("<span class=\"continue\"><span id=\"" + label + "\">" + label + " continue</span></span>\n<br>")
                c.codewritein "</details>"
                c.codewritein "</div>"
                returnVar()
                
            ///<summary>条件を満たす間ループ</summary>
            static member whiledoHS (c:program) (cond:expr) = fun code ->
                c.codewritein "<div class=\"codeblock\">"
                c.codewritein "<details open>"
                c.codewritein("<summary><span class=\"op-loop\">while</span> \\(" + cond.evalHS c + "\\)</summary>")
                c.codewritein "<div class=\"insidecode-loop\">"
                c.indentInc()
                code()
                c.indentDec()
                c.codewritein "</div>"
                c.codewritein "</details>"
                c.codewritein "</div>"
                
            ///<summary>指定した範囲でループ</summary>
            static member rangeHS (c:program) (i1:expr) = fun (i2:expr) -> fun code -> 
                let iname,returnVar = c.i0.getVar()
                //上に20.0破線のスペースを作る
                expr.space_dasharray 20.0
                //枠の上辺のy座標をリスト(frame_top_list)に追加(実行線の描き始めの5.0上)
                //枠の下辺のずらす単位を0.0にリセットする
                //枠の座標を決める4つの変数を定義
                //枠の座標と枠の深さをセットでリストにする(stack2)
                //stack2をstackに乗っける(追加)
                expr.aboveTheCode()
                //stack内の要素の個数(デフォルト1個)-1個を枠の深さ(sectionCount)とする
                let sectionCount = float <| value.stack.Length-1
                //カウンター変数の処理
                let i = "i" + string (int sectionCount + 1 - int sectionCount2)
                let i0 = Var(It 4, i, NaN)
                value.for_check <- 1.0
                let counter_Var = expr.varList(i0,c)
                value.for_check <- 0.0
                for countName, count_number, y in counter_Var do
                    //実行線
                    expr.drawActiveLine(x250*count_number+dashed_x50, value.run_top-gap10, value.run_top,"red")
                    //白線(ループ範囲のスペースのために破線を消すため)
                    expr.colorLine(x250*count_number+dashed_x50, value.run_top-gap20,x250*count_number+dashed_x50, value.run_top-gap10,"white")
                    // テキスト（ループ範囲）
                    expr.drawText(12,"red","normal",x250*count_number+dashed_x50-gap10,value.run_top-25.0,string i1+"~"+string i2)
                code i0
                //最後に入れた枠の座標と枠の深さの数値を各変数に代入する(この段階ではstackに変化はない)
                let xMin,xMax,yMin,yMax,insideSection = value.stack.Head
                //最後に入れた枠の上辺のy座標の数値をframe_topとする
                let frame_top = frame_top_list.Head
                for a, b, c, d, e in value.stack do
                    printfn "check: (%f, %f, %f, %f, %d)" a b c d e
                printfn "xMin %f,xMax %f,yMin %f,yMax %f" xMin xMax yMin yMax
                printfn "sectionCount %f,insideSection %d" sectionCount insideSection
                // ループの枠
                expr.rectangle(xMin-50.0+10.0*sectionCount,frame_top,xMax+50.0-10.0*sectionCount,yMax+5.0+10.0*frame_bottom,"red")
                // テキスト（グループ名）
                expr.drawText(12,"red","normal",xMin-50.0+10.0*sectionCount,frame_top-20.0,"\\(\\mathrm{For}\\)"+" "+"\\(" + i0.evalHS c + "\\)")
                //下に10.0破線のスペースを作る
                //枠の上辺のリストから使った要素以外を残す(使った分を取り除く)
                //枠の下辺のずらす単位を1.0(1個分)増やす
                //枠の座標と枠の深さのリストから使った要素以外を残す(使った分を取り除く)
                expr.belowTheCode()
                returnVar()
                
            ///<summary>指定した範囲でループ(途中脱出可)</summary>
            static member range_exitHS (c:program) (i1:expr) = fun (i2:expr) -> fun code -> 
                match i1,i2 with
                |Int a, Int b when a>b -> 
                    let iname,returnVar = c.i0.getVar()
                    let i = Var(It 4, iname, NaN)
                    let label = gotoLabel.nextGotoLabel()
                    let exit() = c.codewritein("goto "+label)
                    c.comment "<div class=\"codeblock\">"
                    c.comment "<details open>"
                    c.comment("<summary><span class=\"op-loop\">for</span> \\(" + i.evalH c + "=" + i1.evalH c + "," + i2.evalH c + "\\)</summary>")
                    c.comment "<div class=\"insidecode-loop\">"
                    c.indentInc()
                    code(exit,i)
                    c.indentDec()
                    c.comment "</div>"
                    c.comment("<span class=\"continue\"><span id=\"" + label + "\">" + label + " continue</span></span>\n<br>")
                    c.comment "</details>"
                    c.comment "</div>"
                    c.comment(label+" continue")
                    returnVar()
                |_ ->
                    let iname,returnVar = c.i0.getVar()
                    let i = Var(It 4, iname, NaN)
                    let label = gotoLabel.nextGotoLabel()
                    let exit() = c.codewritein("goto "+label)
                    c.codewritein "<div class=\"codeblock\">"
                    c.codewritein "<details open>"
                    c.codewritein("<summary><span class=\"op-loop\">for</span> \\(" + i.evalH c + "=" + i1.evalH c + "," + i2.evalH c + "\\)</summary>")
                    c.codewritein "<div class=\"insidecode-loop\">"
                    c.indentInc()
                    code(exit,i)
                    c.indentDec()
                    c.codewritein "</div>"
                    c.codewritein("<span class=\"continue\"><span id=\"" + label + "\">" + label + " continue</span></span>\n<br>")
                    c.codewritein "</details>"
                    c.codewritein "</div>"
                    c.codewritein(label+" continue")
                    returnVar()
                    
            static member branchHS (c:program) code =
                c.codewritein "<div class=\"codeblock\">"
                c.codewritein "<details open>"
                let ifcode (cond:expr) code =
                    let cond = cond.evalHS c
                    if branch_elifCheck = 0.0 then
                        //カウンター変数名の数字を増やした分戻す変数
                        sectionCount2 <- sectionCount2 + 1.0
                        //上に30.0破線のスペースを作る
                        expr.space_dasharray 30.0
                        //枠の上辺のy座標(run_top - 5.0)をリスト(frame_top_list)に追加(実行線の描き始めの5.0上)
                        //枠の下辺のずらす単位(frame_bottom)を0.0にリセットする
                        //枠の座標を決める4つの変数(xMin,xMax,yMin,yMax)を定義
                        //枠の座標(xMin,xMax,yMin,yMax)と枠の深さ(insideSection)をセットでリストにする(stack2)
                        //stack2をstackに乗っける(追加)
                        expr.aboveTheCode()
                    //stack内の要素の個数(デフォルト1個)-1個を枠の深さ(sectionCount)とする
                    let sectionCount = float <| value.stack.Length-1
                    branch_text_name_list <- cond::branch_text_name_list
                    if branch_elifCheck =0.0 then
                        branch_text_yPoint_list <- value.run_top-5.0-20.0::branch_text_yPoint_list
                    if branch_IFELSE_IFELSE_check = 0.0 then
                        if branch_elifCheck = 0.0 then
                            if count_branch = 0.0 then
                                count_branch <- 1.0
                            else
                                count_branch <- count_branch_memory_list.Head
                                count_branch_memory_list <- count_branch_memory_list.Tail
                        else //branch_elifCheck = 1.0
                            if count_branch = 0.0 then
                                count_branch <- count_branch_memory_list.Head
                                count_branch_memory_list <- count_branch_memory_list.Tail
                            else
                                ()
                            count_branch <- count_branch + 1.0
                    else //branch_IFELSE_IFELSE_check = 1.0
                        count_branch_memory_list <- count_branch::count_branch_memory_list
                        count_branch <- 1.0
                    branch_IFELSE_IFELSE_check <- 1.0
                    code()
                    //最後に入れた枠の座標と枠の深さの数値を各変数に代入する(この段階ではstackに変化はない)
                    let xMin,xMax,yMin,yMax,insideSection = value.stack.Head
                    // 境界線のy座標を決定
                    boundaryLine <- yMax+5.0+10.0*frame_bottom
                    if branch_elifCheck = 0.0 then
                        branch_frame_top_list <- frame_top_list.Head :: branch_frame_top_list
                    //中に20.0破線のスペースを作る
                    expr.space_dasharray 20.0
                    branch_text_yPoint_list <- boundaryLine::branch_text_yPoint_list
                    //境界線のy座標をスタック用のリストに入れる
                    boundaryY <- boundaryLine::boundaryY
                    branch_IFELSE_IFELSE_check <- 0.0
                    branch_elifCheck <- 1.0
                let elseifcode (cond:expr) code =
                    ifcode cond code
                let elsecode code =
                    branch_text_name_list <- "\\(\\mathrm{Else}\\)"::branch_text_name_list
                    if branch_elifCheck = 0.0 then //ELの後のELのとき
                        count_branch <- count_branch_memory_list.Head
                        count_branch_memory_list <- count_branch_memory_list.Tail
                    else //branch_elifCheck = 1.0、elifの続きのとき
                        count_branch <- count_branch + 1.0
                    branch_IFELSE_IFELSE_check <- 1.0
                    branch_elifCheck <- 0.0
                    code()
                    //最後に入れた枠の座標と枠の深さの数値を各変数に代入する(この段階ではstackに変化はない)
                    let xMin,xMax,yMin,yMax,insideSection = value.stack.Head
                    //stack内の要素の個数(デフォルト1個)-1個を枠の深さ(sectionCount)とする
                    let sectionCount = float <| value.stack.Length-1
                    for a, b, c, d, e in value.stack do
                        printfn "check: (%f, %f, %f, %f, %d)" a b c d e
                    printfn "xMin %f,xMax %f,yMin %f,yMax %f" xMin xMax yMin yMax
                    printfn "sectionCount %f,insideSection %d" sectionCount insideSection
                    branch_frame_top <- branch_frame_top_list.Head
                    // 条件分岐の枠(全体を囲む大きい枠)
                    expr.rectangle(xMin-50.0+10.0*sectionCount,branch_frame_top-20.0,xMax+50.0-10.0*sectionCount,yMax+5.0+10.0*frame_bottom,"green")
                    branch_frame_top_list <- branch_frame_top_list.Tail
                    if count_branch = 0.0 then
                        count_branch <- count_branch_memory_list.Head
                        count_branch_memory_list <- count_branch_memory_list.Tail
                    let text_times = int count_branch
                    printfn "count_branch_memory_list %A" count_branch_memory_list
                    printfn "text_times %i" text_times
                    let boundary_times = int (count_branch - 1.0)
                    count_branch <- 0.0
                    for i in 1..text_times do
                        printfn "name %A" branch_text_name_list
                        printfn "yPoint %A" branch_text_yPoint_list
                        let branch_text_name = branch_text_name_list.Head
                        let branch_text_yPoint = branch_text_yPoint_list.Head
                        // テキスト（条件式）
                        expr.drawText(12,"green","normal",5.0+xMin-50.0+10.0*sectionCount,branch_text_yPoint,string branch_text_name)
                        branch_text_name_list <- branch_text_name_list.Tail
                        branch_text_yPoint_list <- branch_text_yPoint_list.Tail
                    branch_IFELSE_IFELSE_check <- 0.0
                    for i in 1..boundary_times do
                        printfn "boundaryY %A" boundaryY
                        //最後に入れた枠の上辺のy座標の数値をboundaryとする
                        let boundary = boundaryY.Head
                        //破線：境界線(間の仕切り)
                        expr.drawDasharray2(xMin-50.0+10.0*sectionCount,xMax+50.0-10.0*sectionCount,boundary)
                        //枠の上辺のリストから使った要素以外を残す(使った分を取り除く)
                        boundaryY <- boundaryY.Tail
                    //下に10.0破線のスペースを作る
                    //枠の上辺のリストから使った要素以外を残す(使った分を取り除く)
                    //枠の下辺のずらす単位を1.0(1個分)増やす
                    //枠の座標と枠の深さのリストから使った要素以外を残す(使った分を取り除く)
                    expr.belowTheCode()
                    //カウンター変数名の数字を増やした分戻す変数
                    sectionCount2 <- sectionCount2 - 1.0
                code(ifcode,elseifcode,elsecode)
                
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
