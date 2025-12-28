namespace Aqualis
    
    ///<summary>条件分岐</summary>
    type br (ifcode,elseifcode,elsecode) =
        
        ///<summary>条件分岐式のカウンタ</summary>
        let mutable con = 1
        
        ///<summary>条件式(if)</summary>
        member __.IF (cond:bool0) code =
            if con=0 then
                printfn "ELの後のIFは無視されます"
            elif con=1 then
                ifcode cond.Expr code
            else
                elseifcode cond.Expr code
            con <- con + 1
            
        ///<summary>条件式(else)</summary>
        member __.EL code =
            elsecode code
            con <- 0
            
        ///<summary>条件分岐式(2番目以降のIFは前のIFを満たさない場合のみ評価)</summary>
        static member branch code =
            expr.branch pr <| fun q ->
            let ib = br q
            code ib
            
        ///<summary>条件分岐式(if式)</summary>
        static member if1 (cond:bool0) code =
            match pr.language with
            |LaTeX|HTML ->
                br.branch <| fun b ->
                    b.IF cond <| fun () ->
                        code()
            |_ ->
                match cond.Expr.simp with
                |True -> code()
                |False -> ()
                |_ ->
                    br.branch <| fun b ->
                        b.IF cond <| fun () ->
                            code()
                        
        ///<summary>条件分岐式(if...else...式)</summary>
        static member if2 (cond:bool0) code1 code2 =
            match pr.language with
            |LaTeX|HTML ->
                br.branch <| fun b ->
                    b.IF cond <| fun () ->
                        code1()
                    b.EL <| fun () ->
                        code2()
            |_ ->
                match cond.Expr.simp with
                |True -> code1()
                |False -> code2()
                |_ ->
                    br.branch <| fun b ->
                        b.IF cond <| fun () ->
                            code1()
                        b.EL <| fun () ->
                            code2()
                            
    ///<summary>条件分岐（処理スキップ）</summary>
    type dummy_br () =
        ///<summary>条件式(if)</summary>
        member __.IF (cond:bool0) code = ()
        ///<summary>条件式(else)</summary>
        member __.EL code = ()
        ///<summary>条件分岐式(2番目以降のIFは前のIFを満たさない場合のみ評価)</summary>
        static member branch code = ()
        ///<summary>条件分岐式(if式)</summary>
        static member if1 cond code = ()
        ///<summary>条件分岐式(if...else...式)</summary>
        static member if2 cond code1 code2 = ()
        