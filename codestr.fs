﻿(*
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
                |LaTeX ->
                    p.codewrite("\\section{"+s+"}")
                |HTML -> 
                    p.codewrite("<details open>")
                    p.codewrite("<summary><span class=\"op-section\">section</span>"+s+"</summary>")
                    p.codewrite("<div class=\"insidecode-section\">")
                |_ ->
                    p.comment ("==="+(s.PadRight(76,'=')))
            (!===)s
            match p.lang with
            |Python ->
                ()
            |_ ->
                p.indentInc()
            code()
            match p.lang with
            |Python ->
                ()
            |_ ->
                p.indentDec()
            match p.lang with 
            |Fortran |C99 |Python -> 
                (!===)("end "+s) 
                p.codewrite(p.indentSpace+"\n")
            |HTML ->
                p.codewrite("</div>")
                p.codewrite("</details>")
            |LaTeX  -> ()
              
        static member subsection (s:string) (code:unit->unit) = 
            let (!===) (s:string) = 
                match p.lang with
                |LaTeX ->
                    p.codewrite("\\subsection{"+s+"}")
                |HTML -> 
                    p.codewrite("<details open>")
                    p.codewrite("<summary><span class=\"op-section\">section</span>"+s+"</summary>")
                    p.codewrite("<div class=\"insidecode-section\">")
                |_ ->
                    p.comment ("---"+(s.PadRight(76,'-')))
            (!===)s
            match p.lang with
            |Python ->
                ()
            |_ ->
                p.indentInc()
            code()
            match p.lang with
            |Python ->
                ()
            |_ ->
                p.indentDec()
            match p.lang with 
            |Fortran |C99 -> 
                (!===)("end "+s) 
                p.codewrite(p.indentSpace+"\n")
            |Python -> 
                (!===)("end "+s) 
            |HTML   ->
                p.codewrite("</div>")
                p.codewrite("</details>")
            |LaTeX  -> ()
              
        static member private header (c:char) (s:string) = 
            match p.lang with
            |Fortran   ->
                p.comment (c.ToString()+c.ToString()+c.ToString()+(s.PadRight(76,c)))
            |C99 ->
                p.comment (c.ToString()+c.ToString()+c.ToString()+(s.PadRight(76,c)))
            |Python ->
                p.comment (c.ToString()+c.ToString()+c.ToString()+(s.PadRight(76,c)))
            |LaTeX   ->
                p.codewrite("\\section{"+s+"}")
            |HTML   -> 
                p.codewrite("<details open>")
                p.codewrite("<summary><span class=\"op-section\">section</span>"+s+"</summary>")
                p.codewrite("<div class=\"insidecode-section\">")
              
        static member private footer (c:char) (s:string) = 
            match p.lang with
            |Fortran   -> p.comment (c.ToString()+c.ToString()+c.ToString()+(("end " + s).PadRight(76,c)))
            |C99 -> p.comment (c.ToString()+c.ToString()+c.ToString()+(("end " + s).PadRight(76,c)))
            |Python -> p.comment (c.ToString()+c.ToString()+c.ToString()+(("end " + s).PadRight(76,c)))
            |LaTeX   -> ()
            |HTML   ->
                p.codewrite("</div>")
                p.codewrite("</details>")

        static member private blank () = 
            p.codewrite(p.indentSpace+"\n")
            
        static member h1 (s:string) (code:unit->unit) = 
            codestr.header '#' s
            if p.displaySection then print.t ("### "+s+" #########################")
            match p.lang with
            |Python ->
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
            |Python ->
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
            |Python ->
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
            |Python ->
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
            |Python ->
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
        