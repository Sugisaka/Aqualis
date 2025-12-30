namespace Aqualis
    
    [<AutoOpen>]
    module exprEvalH =
        
        open System
        
        type expr with
            
            static member substH (x:expr) (y:expr) (c:program) =
                c.codewrite "\\["
                c.codewrite "\\begin{align}"
                c.codewrite (x.evalH c  + " \\leftarrow " + y.evalH c + "")
                c.codewrite "\\end{align}"
                c.codewrite "\\]"
                
            static member equivH (x:expr) (y:expr) (c:program) =
                c.codewrite (x.evalL c  + " = " + y.evalL c)
                
            static member equivAlignH (x:expr) (y:expr) (c:program) =
                c.codewrite (x.evalL c  + " =& " + y.evalL c)
                
            static member forLoopH (c:program) (n1:expr,n2:expr) code =
                let iname,returnVar = c.i0.getVar()
                let i = Var(It 4, iname, NaN)
                let n1_ = n1.evalH c
                let n2_ = n2.evalH c
                c.codewrite "<div class=\"codeblock\">"
                c.codewrite "<details open>"
                c.codewrite("<summary><span class=\"op-loop\">for</span> \\(" + i.evalH c + "=" + n1_ + "," + n2_ + "\\)</summary>")
                c.codewrite "<div class=\"insidecode-loop\">"
                c.indentInc()
                code i
                c.indentDec()
                c.codewrite "</div>"
                c.codewrite "</details>"
                c.codewrite "</div>"
                returnVar()
                
            ///<summary>無限ループ</summary>
            static member loopH (c:program) code =
                let iname,returnVar = c.i0.getVar()
                let i = Var(It 4, iname, NaN)
                let label = gotoLabel.nextGotoLabel()
                let exit() = c.codewrite("goto " + label)
                expr.substH i (Int 1) c
                c.codewrite "<div class=\"codeblock\">"
                c.codewrite "<details open>"
                c.codewrite "<summary><span class=\"op-loop\">repeat</span></summary>"
                c.codewrite "<div class=\"insidecode-loop\">"
                c.indentInc()
                code(exit,i)
                expr.substH i (Add(It 4, i, Int 1)) c
                c.indentDec()
                c.codewrite "</div>"
                c.codewrite("<span class=\"continue\"><span id=\"" + label + "\">" + label + " continue</span></span>\n<br>")
                c.codewrite "</details>"
                c.codewrite "</div>"
                returnVar()
                
            ///<summary>条件を満たす間ループ</summary>
            static member whiledoH (c:program) (cond:expr) = fun code ->
                c.codewrite "<div class=\"codeblock\">"
                c.codewrite "<details open>"
                c.codewrite("<summary><span class=\"op-loop\">while</span> \\(" + cond.evalH c + "\\)</summary>")
                c.codewrite "<div class=\"insidecode-loop\">"
                c.indentInc()
                code()
                c.indentDec()
                c.codewrite "</div>"
                c.codewrite "</details>"
                c.codewrite "</div>"
                
            ///<summary>指定した範囲でループ</summary>
            static member rangeH (c:program) (i1:expr) = fun (i2:expr) -> fun code -> 
                match i1,i2 with
                |Int a, Int b when a>b -> 
                    let iname,returnVar = c.i0.getVar()
                    let i = Var(It 4, iname, NaN)
                    c.comment "<div class=\"codeblock\">"
                    c.comment "<details open>"
                    c.comment("<summary><span class=\"op-loop\">for</span> \\(" + i.evalH c + "=" + i1.evalH c + "," + i2.evalH c + "\\)</summary>")
                    c.comment "<div class=\"insidecode-loop\">"
                    c.indentInc()
                    code i
                    c.indentDec()
                    c.comment "</div>"
                    c.comment "</details>"
                    c.comment "</div>"
                    returnVar()
                |i1,i2 ->
                    let iname,returnVar = c.i0.getVar()
                    let i = Var(It 4, iname, NaN)
                    c.codewrite "<div class=\"codeblock\">"
                    c.codewrite "<details open>"
                    c.codewrite("<summary><span class=\"op-loop\">for</span> \\(" + i.evalH c + "=" + i1.evalH c + "," + i2.evalH c + "\\)</summary>")
                    c.codewrite "<div class=\"insidecode-loop\">"
                    c.indentInc()
                    code i
                    c.indentDec()
                    c.codewrite "</div>"
                    c.codewrite "</details>"
                    c.codewrite "</div>"
                    returnVar()
                    
            ///<summary>指定した範囲でループ(途中脱出可)</summary>
            static member range_exitH (c:program) (i1:expr) = fun (i2:expr) -> fun code -> 
                match i1,i2 with
                |Int a, Int b when a>b -> 
                    let iname,returnVar = c.i0.getVar()
                    let i = Var(It 4, iname, NaN)
                    let label = gotoLabel.nextGotoLabel()
                    let exit() = c.codewrite("goto "+label)
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
                    let exit() = c.codewrite("goto "+label)
                    c.codewrite "<div class=\"codeblock\">"
                    c.codewrite "<details open>"
                    c.codewrite("<summary><span class=\"op-loop\">for</span> \\(" + i.evalH c + "=" + i1.evalH c + "," + i2.evalH c + "\\)</summary>")
                    c.codewrite "<div class=\"insidecode-loop\">"
                    c.indentInc()
                    code(exit,i)
                    c.indentDec()
                    c.codewrite "</div>"
                    c.codewrite("<span class=\"continue\"><span id=\"" + label + "\">" + label + " continue</span></span>\n<br>")
                    c.codewrite "</details>"
                    c.codewrite "</div>"
                    c.codewrite(label+" continue")
                    returnVar()
                    
            static member branchH (c:program) code =
                c.codewrite "<div class=\"codeblock\">"
                c.codewrite "<details open>"
                let ifcode (cond:expr) code =
                    let cond = cond.evalH c
                    c.codewrite("<summary><span class=\"op-if\">if</span>"+" \\(" + cond + "\\)</summary>")
                    c.codewrite "<div class=\"insidecode-if\">"
                    c.indentInc()
                    code()
                    c.indentDec()
                    c.codewrite "</div>"
                let elseifcode (cond:expr) code =
                    let cond = cond.evalH c
                    c.codewrite("<summary><span class=\"op-if\">else if</span>"+" \\(" + cond + "\\)</summary>")
                    c.codewrite "<div class=\"insidecode-if\">"
                    c.indentInc()
                    code()
                    c.indentDec()
                    c.codewrite "</div>"
                let elsecode code =
                    c.codewrite "<summary><span class=\"op-if\">else</span></summary>"
                    c.codewrite "<div class=\"insidecode-if\">"
                    c.indentInc()
                    code()
                    c.indentDec()
                    c.codewrite "</div>"
                code(ifcode,elseifcode,elsecode)
                c.codewrite "</details>"
                c.codewrite "</div>"
                
            member this.evalH(c:program) =
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
                        |_ -> expr.substH x y c
                        eval (f x) pl
                    |Sum(t, n1, n2, f) ->
                        // 合計値格納用変数
                        eval (Let(t, Int 0, fun u ->
                            expr.forLoopH c (n1,n2) <| fun i ->
                                // 加算・代入処理
                                expr.substH u (Add(t,u, f i)) c
                            u)) pl
                    |IfEl(cond,n1,n2) -> 
                        eval (Let(n1.etype, NaN, fun x -> 
                            expr.branchH c <| fun (ifcode,_,elsecode) ->
                                ifcode cond <| fun () ->
                                    expr.substH x n1 c
                                elsecode <| fun () ->
                                    expr.substH x n2 c
                            x)) pl
                    |NaN -> "NaN", pl
                let t,_ = eval this 0
                t
