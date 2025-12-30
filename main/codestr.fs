namespace Aqualis
    
    /// コードのグループ化
    type group =
        
        /// <summary>
        /// swがtrueの時のみcodeを実行
        /// </summary>
        /// <param name="sw">実行切り替えスイッチ</param>
        /// <param name="label">ヘッダー</param>
        /// <param name="code">内部処理</param>
        static member section (label:string) = fun code -> code()
        
        /// <summary>
        /// swがtrueの時のみcodeを実行
        /// </summary>
        /// <param name="sw">実行切り替えスイッチ</param>
        /// <param name="code">内部処理</param>
        static member section (sw:bool) = fun code -> match sw with |true -> code() |false -> ()

        /// <summary>
        /// swがONの時のみcodeを実行
        /// </summary>
        /// <param name="sw">実行切り替えスイッチ</param>
        /// <param name="code">内部処理</param>
        static member section (sw:Switch) = group.section (sw=ON)
        
        /// <summary>
        /// stepがidに等しい時のみcodeを実行
        /// </summary>
        /// <param name="step">IDに等しいときのみ内部を実行</param>
        /// <param name="id">グループID</param>
        /// <param name="code">内部処理</param>
        static member section (step:int) = fun (id:int) -> group.section (step.Equals id)
        
    ///<summary>コードの階層構造を作成。codestr→dummy_codestrで内部コード無効化</summary>
    type codestr () =
        
        static member fsection (s:string) = fun (code:unit->unit) -> 
            code()

        static member fsection (onoff:Switch) = fun (s:string) (code:unit->unit) ->
            code()
              
        static member section (s:string) (code:unit->unit) = 
            let (!===) (s:string) = 
                match pr.language with
                |LaTeX ->
                    pr.codewrite("\\section{"+s+"}")
                |HTML -> 
                    pr.codewrite("<details open>")
                    pr.codewrite("<summary><span class=\"op-section\">section</span>"+s+"</summary>")
                    pr.codewrite("<div class=\"insidecode-section\">")
                |_ ->
                    pr.comment ("==="+(s.PadRight(76,'=')))
            (!===)s
            match pr.language with
            |Python ->
                ()
            |_ ->
                pr.indentInc()
            code()
            match pr.language with
            |Python ->
                ()
            |_ ->
                pr.indentDec()
            match pr.language with 
            |Fortran |C99 |Python -> 
                (!===)("end "+s) 
                pr.codewrite "\n"
            |HTML ->
                pr.codewrite "</div>"
                pr.codewrite "</details>"
            |_  -> ()
              
        static member subsection (s:string) (code:unit->unit) = 
            let (!===) (s:string) = 
                match pr.language with
                |LaTeX ->
                    pr.codewrite("\\subsection{"+s+"}")
                |HTML -> 
                    pr.codewrite "<details open>"
                    pr.codewrite("<summary><span class=\"op-section\">section</span>"+s+"</summary>")
                    pr.codewrite "<div class=\"insidecode-section\">"
                |_ ->
                    pr.comment ("---"+(s.PadRight(76,'-')))
            (!===)s
            match pr.language with
            |Python ->
                ()
            |_ ->
                pr.indentInc()
            code()
            match pr.language with
            |Python ->
                ()
            |_ ->
                pr.indentDec()
            match pr.language with 
            |Fortran |C99 -> 
                (!===)("end "+s) 
                pr.codewrite "\n"
            |Python -> 
                (!===)("end "+s) 
            |HTML   ->
                pr.codewrite "</div>"
                pr.codewrite "</details>"
            |_  -> ()
              
        static member private header (c:char) (s:string) = 
            match pr.language with
            |Fortran ->
                pr.comment (c.ToString()+c.ToString()+c.ToString()+(s.PadRight(76,c)))
            |C99 ->
                pr.comment (c.ToString()+c.ToString()+c.ToString()+(s.PadRight(76,c)))
            |Python ->
                pr.comment (c.ToString()+c.ToString()+c.ToString()+(s.PadRight(76,c)))
            |JavaScript ->
                pr.comment (c.ToString()+c.ToString()+c.ToString()+(s.PadRight(76,c)))
            |PHP ->
                pr.comment (c.ToString()+c.ToString()+c.ToString()+(s.PadRight(76,c)))
            |LaTeX ->
                pr.codewrite("\\section{"+s+"}")
            |HTML -> 
                pr.codewrite "<details open>"
                pr.codewrite("<summary><span class=\"op-section\">section</span>"+s+"</summary>")
                pr.codewrite "<div class=\"insidecode-section\">"
            |Numeric -> ()

        static member private footer (c:char) (s:string) = 
            match pr.language with
            |Fortran   -> pr.comment (c.ToString()+c.ToString()+c.ToString()+(("end " + s).PadRight(76,c)))
            |C99 -> pr.comment (c.ToString()+c.ToString()+c.ToString()+(("end " + s).PadRight(76,c)))
            |Python -> pr.comment (c.ToString()+c.ToString()+c.ToString()+(("end " + s).PadRight(76,c)))
            |HTML   ->
                pr.codewrite "</div>"
                pr.codewrite "</details>"
            |_   -> ()

        static member private blank () = 
            pr.codewrite "\n"
            
        static member h1 (s:string) (code:unit->unit) = 
            codestr.header '#' s
            if displaySection then print.t ("### "+s+" #########################")
            match pr.language with
            |Python ->
                code()
            |_ ->
                pr.indentInc()
                code()
                pr.indentDec()
            if displaySection then print.t ("### END "+s+" #####################")
            codestr.footer '#' s
            codestr.blank()
              
        static member h2 (s:string) (code:unit->unit) = 
            codestr.header '%' s
            if displaySection then print.t ("=== "+s+" ===================")
            match pr.language with
            |Python ->
                code()
            |_ ->
                pr.indentInc()
                code()
                pr.indentDec()
            if displaySection then print.t ("=== END "+s+" ===============")
            codestr.footer '%' s
            codestr.blank()
              
        static member h3 (s:string) (code:unit->unit) = 
            codestr.header '=' s
            if displaySection then print.t ("--- "+s+" --------------")
            match pr.language with
            |Python ->
                code()
            |_ ->
                pr.indentInc()
                code()
                pr.indentDec()
            if displaySection then print.t ("--- END "+s+" ----------")
            codestr.footer '=' s
            codestr.blank()
              
        static member h4 (s:string) (code:unit->unit) = 
            codestr.header '+' s
            if displaySection then print.t ("... "+s+" ...........")
            match pr.language with
            |Python ->
                code()
            |_ ->
                pr.indentInc()
                code()
                pr.indentDec()
            if displaySection then print.t ("... END "+s+" .......")
            codestr.footer '+' s
            codestr.blank()
              
        static member h5 (s:string) (code:unit->unit) = 
            codestr.header '-' s
            if displaySection then print.t s
            match pr.language with
            |Python ->
                code()
            |_ ->
                pr.indentInc()
                code()
                pr.indentDec()
            if displaySection then print.t ("END "+s)
            codestr.footer '-' s
            codestr.blank()
            
    ///<summary>コードの階層構造を作成。dummy_codestr→codestrで内部コード有効化</summary>
    type dummy_codestr () =
        
        static member h1 (s:string) (code:unit->unit) =  ()
        
        static member h2 (s:string) (code:unit->unit) =  ()
        
        static member h3 (s:string) (code:unit->unit) =  ()
        
        static member h4 (s:string) (code:unit->unit) =  ()
        
        static member h5 (s:string) (code:unit->unit) =  ()

        static member section (s:string) (code:unit->unit) =  ()
        