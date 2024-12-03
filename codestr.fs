(*
Copyright (c) 2022 Jun-ichiro Sugisaka

This software is released under the MIT License.
http://opensource.org/licenses/mit-license.php
*)
namespace Aqualis
    
    open Aqualis_base

    ///<summary>コードの階層構造を作成。codestr→dummy_codestrで内部コード無効化</summary>
    type codestr () =
        
        static member fsection (s:string) = fun (code:unit->unit) -> 
            code()

        static member fsection (onoff:Switch) = fun (s:string) (code:unit->unit) ->
            code()
              
        static member section (s:string) (code:unit->unit) = 
            let (!===) (s:string) = 
                match p.lang with
                |T ->
                    p.codewrite("\\section{"+s+"}")
                |H -> 
                    p.codewrite("<details open>")
                    p.codewrite("<summary><span class=\"op-section\">section</span>"+s+"</summary>")
                    p.codewrite("<div class=\"insidecode-section\">")
                |_ ->
                    p.comment ("==="+(s.PadRight(76,'=')))
            (!===)s
            match p.lang with
            |P ->
                ()
            |_ ->
                p.indentInc()
            code()
            match p.lang with
            |P ->
                ()
            |_ ->
                p.indentDec()
            match p.lang with 
            |F |C |P-> 
                (!===)("end "+s) 
                p.codewrite(p.indentSpace+"\n")
            |H ->
                p.codewrite("</div>")
                p.codewrite("</details>")
            |T  -> ()
              
        static member subsection (s:string) (code:unit->unit) = 
            let (!===) (s:string) = 
                match p.lang with
                |T ->
                    p.codewrite("\\subsection{"+s+"}")
                |H -> 
                    p.codewrite("<details open>")
                    p.codewrite("<summary><span class=\"op-section\">section</span>"+s+"</summary>")
                    p.codewrite("<div class=\"insidecode-section\">")
                |_ ->
                    p.comment ("---"+(s.PadRight(76,'-')))
            (!===)s
            match p.lang with
            |P ->
                ()
            |_ ->
                p.indentInc()
            code()
            match p.lang with
            |P ->
                ()
            |_ ->
                p.indentDec()
            match p.lang with 
            |F |C -> 
                (!===)("end "+s) 
                p.codewrite(p.indentSpace+"\n")
            |P-> 
                (!===)("end "+s) 
            |H   ->
                p.codewrite("</div>")
                p.codewrite("</details>")
            |T  -> ()
              
        static member private header (c:char) (s:string) = 
            match p.lang with
            |F   ->
                p.comment (c.ToString()+c.ToString()+c.ToString()+(s.PadRight(76,c)))
            |C ->
                p.comment (c.ToString()+c.ToString()+c.ToString()+(s.PadRight(76,c)))
            |P ->
                p.comment (c.ToString()+c.ToString()+c.ToString()+(s.PadRight(76,c)))
            |T   ->
                p.codewrite("\\section{"+s+"}")
            |H   -> 
                p.codewrite("<details open>")
                p.codewrite("<summary><span class=\"op-section\">section</span>"+s+"</summary>")
                p.codewrite("<div class=\"insidecode-section\">")
              
        static member private footer (c:char) (s:string) = 
            match p.lang with
            |F   -> p.comment (c.ToString()+c.ToString()+c.ToString()+(("end " + s).PadRight(76,c)))
            |C -> p.comment (c.ToString()+c.ToString()+c.ToString()+(("end " + s).PadRight(76,c)))
            |P -> p.comment (c.ToString()+c.ToString()+c.ToString()+(("end " + s).PadRight(76,c)))
            |T   -> ()
            |H   ->
                p.codewrite("</div>")
                p.codewrite("</details>")

        static member private blank () = 
            p.codewrite(p.indentSpace+"\n")
            
        static member h1 (s:string) (code:unit->unit) = 
            codestr.header '#' s
            if p.displaySection then print.t ("### "+s+" #########################")
            match p.lang with
            |P ->
                code()
            |_ ->
                p.indentInc()
                code()
                p.indentDec()
            if p.displaySection then print.t ("### END "+s+" #####################")
            codestr.footer '#' s
            codestr.blank()
              
        static member h2 (s:string) (code:unit->unit) = 
            codestr.header '%' s
            if p.displaySection then print.t ("=== "+s+" ===================")
            match p.lang with
            |P ->
                code()
            |_ ->
                p.indentInc()
                code()
                p.indentDec()
            if p.displaySection then print.t ("=== END "+s+" ===============")
            codestr.footer '%' s
            codestr.blank()
              
        static member h3 (s:string) (code:unit->unit) = 
            codestr.header '=' s
            if p.displaySection then print.t ("--- "+s+" --------------")
            match p.lang with
            |P ->
                code()
            |_ ->
                p.indentInc()
                code()
                p.indentDec()
            if p.displaySection then print.t ("--- END "+s+" ----------")
            codestr.footer '=' s
            codestr.blank()
              
        static member h4 (s:string) (code:unit->unit) = 
            codestr.header '+' s
            if p.displaySection then print.t ("... "+s+" ...........")
            match p.lang with
            |P ->
                code()
            |_ ->
                p.indentInc()
                code()
                p.indentDec()
            if p.displaySection then print.t ("... END "+s+" .......")
            codestr.footer '+' s
            codestr.blank()
              
        static member h5 (s:string) (code:unit->unit) = 
            codestr.header '-' s
            if p.displaySection then print.t s
            match p.lang with
            |P ->
                code()
            |_ ->
                p.indentInc()
                code()
                p.indentDec()
            if p.displaySection then print.t ("END "+s)
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
        