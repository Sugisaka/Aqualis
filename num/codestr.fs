// 
// Copyright (c) 2026 Jun-ichiro Sugisaka
// 
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
// 
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
        static member section (step:int) = fun (id:int) -> group.section ((step = id))
              
        static member Section (s:string) (code:unit->unit) = 
            let (!===) (s:string) = 
                match programList[prIndex].language with
                |LaTeX ->
                    codewritein("\\section{"+s+"}")
                |HTML -> 
                    codewritein "<details open>"
                    codewritein("<summary><span class=\"op-section\">section</span>"+s+"</summary>")
                    codewritein "<div class=\"insidecode-section\">"
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
                codewritein "\n"
            |HTML ->
                codewritein "</div>"
                codewritein "</details>"
            |_  -> ()
            
        static member subSection (s:string) (code:unit->unit) = 
            let (!===) (s:string) = 
                match programList[prIndex].language with
                |LaTeX ->
                    codewritein("\\subsection{"+s+"}")
                |HTML -> 
                    codewritein "<details open>"
                    codewritein("<summary><span class=\"op-section\">section</span>"+s+"</summary>")
                    codewritein "<div class=\"insidecode-section\">"
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
                codewritein "\n"
            |Python -> 
                (!===)("end "+s) 
            |HTML   ->
                codewritein "</div>"
                codewritein "</details>"
            |_  -> ()
            
        static member private Header (c:char) (s:string) = 
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
                codewritein("\\section{"+s+"}")
            |HTML -> 
                codewritein "<details open>"
                codewritein("<summary><span class=\"op-section\">section</span>"+s+"</summary>")
                codewritein "<div class=\"insidecode-section\">"
            |HTMLSequenceDiagram -> 
                codewritein "<details open>"
                codewritein("<summary><span class=\"op-section\">section</span>"+s+"</summary>")
                codewritein "<div class=\"insidecode-section\">"
            |Numeric -> ()
            
        static member private Footer (c:char) (s:string) = 
            match programList[prIndex].language with
            |Fortran   -> comment (c.ToString()+c.ToString()+c.ToString()+(("end " + s).PadRight(76,c)))
            |C99 -> comment (c.ToString()+c.ToString()+c.ToString()+(("end " + s).PadRight(76,c)))
            |Python -> comment (c.ToString()+c.ToString()+c.ToString()+(("end " + s).PadRight(76,c)))
            |HTML   ->
                codewritein "</div>"
                codewritein "</details>"
            |HTMLSequenceDiagram   ->
                codewritein "</div>"
                codewritein "</details>"
            |_   -> ()
            
        static member private blank () = 
            codewritein "\n"
            
        static member h1 (s:string) (code:unit->unit) = 
            group.Header '#' s
            if displaySection then print.t ("### "+s+" #########################")
            match programList[prIndex].language with
            |Python ->
                code()
            |_ ->
                programList[prIndex].indentInc()
                code()
                programList[prIndex].indentDec()
            if displaySection then print.t ("### END "+s+" #####################")
            group.Footer '#' s
            group.blank()
            
        static member h2 (s:string) (code:unit->unit) = 
            group.Header '%' s
            if displaySection then print.t ("=== "+s+" ===================")
            match programList[prIndex].language with
            |Python ->
                code()
            |_ ->
                programList[prIndex].indentInc()
                code()
                programList[prIndex].indentDec()
            if displaySection then print.t ("=== END "+s+" ===============")
            group.Footer '%' s
            group.blank()
            
        static member h3 (s:string) (code:unit->unit) = 
            group.Header '=' s
            if displaySection then print.t ("--- "+s+" --------------")
            match programList[prIndex].language with
            |Python ->
                code()
            |_ ->
                programList[prIndex].indentInc()
                code()
                programList[prIndex].indentDec()
            if displaySection then print.t ("--- END "+s+" ----------")
            group.Footer '=' s
            group.blank()
            
        static member h4 (s:string) (code:unit->unit) = 
            group.Header '+' s
            if displaySection then print.t ("... "+s+" ...........")
            match programList[prIndex].language with
            |Python ->
                code()
            |_ ->
                programList[prIndex].indentInc()
                code()
                programList[prIndex].indentDec()
            if displaySection then print.t ("... END "+s+" .......")
            group.Footer '+' s
            group.blank()
            
        static member h5 (s:string) (code:unit->unit) = 
            group.Header '-' s
            if displaySection then print.t s
            match programList[prIndex].language with
            |Python ->
                code()
            |_ ->
                programList[prIndex].indentInc()
                code()
                programList[prIndex].indentDec()
            if displaySection then print.t ("END "+s)
            group.Footer '-' s
            group.blank()
            
    ///<summary>コードの階層構造を作成。dummy_codestr→codestrで内部コード有効化</summary>
    type dummy_group () =
        
        static member section (label:string) = fun code -> ()
        
        static member section (sw:bool) = fun code -> ()
        
        static member section (sw:Switch) = ()
        
        static member section (step:int) = fun (id:int) -> ()
        
        static member Section (s:string) (code:unit->unit) = ()
        
        static member subSection (s:string) (code:unit->unit) = ()
        
        static member private Header (c:char) (s:string) = ()
        
        static member private Footer (c:char) (s:string) = ()
        
        static member private blank () = ()
        
        static member h1 (s:string) (code:unit->unit) = ()
        
        static member h2 (s:string) (code:unit->unit) = ()
        
        static member h3 (s:string) (code:unit->unit) = ()
        
        static member h4 (s:string) (code:unit->unit) = ()
        
        static member h5 (s:string) (code:unit->unit) = ()
