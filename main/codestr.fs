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
                match programList[prIndex].language with
                |LaTeX ->
                    codewrite("\\section{"+s+"}")
                |HTML -> 
                    codewrite "<details open>"
                    codewrite("<summary><span class=\"op-section\">section</span>"+s+"</summary>")
                    codewrite "<div class=\"insidecode-section\">"
                |_ ->
                    comment ("===" + s.PadRight(76,'='))
            (!===)s
            match programList[prIndex].language with
            |Python ->
                ()
            |_ ->
                programList[prIndex].indentInc()
            code()
            match programList[prIndex].language with
            |Python ->
                ()
            |_ ->
                programList[prIndex].indentDec()
            match programList[prIndex].language with 
            |Fortran |C99 |Python -> 
                (!===)("end "+s) 
                codewrite "\n"
            |HTML ->
                codewrite "</div>"
                codewrite "</details>"
            |_  -> ()
              
        static member subsection (s:string) (code:unit->unit) = 
            let (!===) (s:string) = 
                match programList[prIndex].language with
                |LaTeX ->
                    codewrite("\\subsection{"+s+"}")
                |HTML -> 
                    codewrite "<details open>"
                    codewrite("<summary><span class=\"op-section\">section</span>"+s+"</summary>")
                    codewrite "<div class=\"insidecode-section\">"
                |_ ->
                    comment ("---" + s.PadRight(76,'-'))
            (!===)s
            match programList[prIndex].language with
            |Python ->
                ()
            |_ ->
                programList[prIndex].indentInc()
            code()
            match programList[prIndex].language with
            |Python ->
                ()
            |_ ->
                programList[prIndex].indentDec()
            match programList[prIndex].language with 
            |Fortran |C99 -> 
                (!===)("end "+s) 
                codewrite "\n"
            |Python -> 
                (!===)("end "+s) 
            |HTML   ->
                codewrite "</div>"
                codewrite "</details>"
            |_  -> ()
              
        static member private header (c:char) (s:string) = 
            match programList[prIndex].language with
            |Fortran ->
                comment (c.ToString()+c.ToString()+c.ToString()+(s.PadRight(76,c)))
            |C99 ->
                comment (c.ToString()+c.ToString()+c.ToString()+(s.PadRight(76,c)))
            |Python ->
                comment (c.ToString()+c.ToString()+c.ToString()+(s.PadRight(76,c)))
            |JavaScript ->
                comment (c.ToString()+c.ToString()+c.ToString()+(s.PadRight(76,c)))
            |PHP ->
                comment (c.ToString()+c.ToString()+c.ToString()+(s.PadRight(76,c)))
            |LaTeX ->
                codewrite("\\section{"+s+"}")
            |HTML -> 
                codewrite "<details open>"
                codewrite("<summary><span class=\"op-section\">section</span>"+s+"</summary>")
                codewrite "<div class=\"insidecode-section\">"
            |Numeric -> ()

        static member private footer (c:char) (s:string) = 
            match programList[prIndex].language with
            |Fortran   -> comment (c.ToString()+c.ToString()+c.ToString()+(("end " + s).PadRight(76,c)))
            |C99 -> comment (c.ToString()+c.ToString()+c.ToString()+(("end " + s).PadRight(76,c)))
            |Python -> comment (c.ToString()+c.ToString()+c.ToString()+(("end " + s).PadRight(76,c)))
            |HTML   ->
                codewrite "</div>"
                codewrite "</details>"
            |_   -> ()

        static member private blank () = 
            codewrite "\n"
            
        static member h1 (s:string) (code:unit->unit) = 
            codestr.header '#' s
            if displaySection then print.t ("### "+s+" #########################")
            match programList[prIndex].language with
            |Python ->
                code()
            |_ ->
                programList[prIndex].indentInc()
                code()
                programList[prIndex].indentDec()
            if displaySection then print.t ("### END "+s+" #####################")
            codestr.footer '#' s
            codestr.blank()
              
        static member h2 (s:string) (code:unit->unit) = 
            codestr.header '%' s
            if displaySection then print.t ("=== "+s+" ===================")
            match programList[prIndex].language with
            |Python ->
                code()
            |_ ->
                programList[prIndex].indentInc()
                code()
                programList[prIndex].indentDec()
            if displaySection then print.t ("=== END "+s+" ===============")
            codestr.footer '%' s
            codestr.blank()
              
        static member h3 (s:string) (code:unit->unit) = 
            codestr.header '=' s
            if displaySection then print.t ("--- "+s+" --------------")
            match programList[prIndex].language with
            |Python ->
                code()
            |_ ->
                programList[prIndex].indentInc()
                code()
                programList[prIndex].indentDec()
            if displaySection then print.t ("--- END "+s+" ----------")
            codestr.footer '=' s
            codestr.blank()
              
        static member h4 (s:string) (code:unit->unit) = 
            codestr.header '+' s
            if displaySection then print.t ("... "+s+" ...........")
            match programList[prIndex].language with
            |Python ->
                code()
            |_ ->
                programList[prIndex].indentInc()
                code()
                programList[prIndex].indentDec()
            if displaySection then print.t ("... END "+s+" .......")
            codestr.footer '+' s
            codestr.blank()
              
        static member h5 (s:string) (code:unit->unit) = 
            codestr.header '-' s
            if displaySection then print.t s
            match programList[prIndex].language with
            |Python ->
                code()
            |_ ->
                programList[prIndex].indentInc()
                code()
                programList[prIndex].indentDec()
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
        