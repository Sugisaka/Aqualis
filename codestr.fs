(*
Copyright (c) 2022 Jun-ichiro Sugisaka

This software is released under the MIT License.
http://opensource.org/licenses/mit-license.php
*)
namespace Aqualis
    
    open System
    open System.IO
    open System.Text
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
                let p = p.param
                p.codewrite("\\section{"+s+"}")
              |H -> 
                let p = p.param
                p.codewrite("<details open>")
                p.codewrite("<summary><span class=\"op-section\">section</span>"+(if s.Contains("<mi>") then "<math>"+s+"</math>" else s)+"</summary>")
                p.codewrite("<div class=\"insidecode-section\">")
              |NL ->
                ()
              |_ ->
                let p = p.param
                p.comment ("==="+(s.PadRight(76,'=')))
            (!===)s
            match p.lang with
              |NL ->
                ()
              |_ ->
                let p = p.param
                p.indentposition_inc()
            code()
            match p.lang with
              |NL ->
                ()
              |_ ->
                let p = p.param
                p.indentposition_dec()
            match p.lang with 
              |F |C89 |C99 -> 
                let p = p.param
                (!===)("end "+s) 
                p.codewrite(p.indent+"\n")
              |H   ->
                let p = p.param
                p.codewrite("</div>")
                p.codewrite("</details>")
              |T  -> ()
              |NL -> ()
              
        static member private header (c:char) (s:string) = 
            match p.lang with
              |F   ->
                let p = p.param
                p.comment (c.ToString()+c.ToString()+c.ToString()+(s.PadRight(76,c)))
              |C89 ->
                let p = p.param
                p.comment (c.ToString()+c.ToString()+c.ToString()+(s.PadRight(76,c)))
              |C99 ->
                let p = p.param
                p.comment (c.ToString()+c.ToString()+c.ToString()+(s.PadRight(76,c)))
              |T   ->
                let p = p.param
                p.codewrite("\\section{"+s+"}")
              |H   -> 
                let p = p.param
                p.codewrite("<details open>")
                p.codewrite("<summary><span class=\"op-section\">section</span>"+(if s.Contains("<mi>") then "<math>"+s+"</math>" else s)+"</summary>")
                p.codewrite("<div class=\"insidecode-section\">")
              |NL ->
                ()
              
        static member private footer (c:char) (s:string) = 
            let p = p.param
            match p.lang with
              |F   -> p.comment (c.ToString()+c.ToString()+c.ToString()+(("end " + s).PadRight(76,c)))
              |C89 -> p.comment (c.ToString()+c.ToString()+c.ToString()+(("end " + s).PadRight(76,c)))
              |C99 -> p.comment (c.ToString()+c.ToString()+c.ToString()+(("end " + s).PadRight(76,c)))
              |T   -> ()
              |H   ->
                p.codewrite("</div>")
                p.codewrite("</details>")
              |NL  -> ()

        static member private blank () = 
            let p = p.param
            p.codewrite(p.indent+"\n")
            
        static member h1 (s:string) (code:unit->unit) = 
            let p = p.param
            codestr.header '#' s
            if p.displaysection then print.t <| "### "+s+" #########################"
            p.indentposition_inc()
            code()
            p.indentposition_dec()
            if p.displaysection then print.t <| "### END "+s+" #####################"
            codestr.footer '#' s
            codestr.blank()
              
        static member h2 (s:string) (code:unit->unit) = 
            let p = p.param
            codestr.header '%' s
            if p.displaysection then print.t <| "=== "+s+" ==================="
            p.indentposition_inc()
            code()
            p.indentposition_dec()
            if p.displaysection then print.t <| "=== END "+s+" ==============="
            codestr.footer '%' s
            codestr.blank()
              
        static member h3 (s:string) (code:unit->unit) = 
            let p = p.param
            codestr.header '=' s
            if p.displaysection then print.t <| "--- "+s+" --------------"
            p.indentposition_inc()
            code()
            p.indentposition_dec()
            if p.displaysection then print.t <| "--- END "+s+" ----------"
            codestr.footer '=' s
            codestr.blank()
              
        static member h4 (s:string) (code:unit->unit) = 
            let p = p.param
            codestr.header '+' s
            if p.displaysection then print.t <| "... "+s+" ..........."
            p.indentposition_inc()
            code()
            p.indentposition_dec()
            if p.displaysection then print.t <| "... END "+s+" ......."
            codestr.footer '+' s
            codestr.blank()
              
        static member h5 (s:string) (code:unit->unit) = 
            let p = p.param
            codestr.header '-' s
            if p.displaysection then print.t <| s
            p.indentposition_inc()
            code()
            p.indentposition_dec()
            if p.displaysection then print.t <| "END "+s
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
        
