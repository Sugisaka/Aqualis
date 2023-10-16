(*
Copyright (c) 2022 Jun-ichiro Sugisaka

This software is released under the MIT License.
http://opensource.org/licenses/mit-license.php
*)
namespace Aqualis
    
    open Aqualis_base

    ///<summary>条件分岐</summary>
    type br () =
        ///<summary>条件分岐式のカウンタ</summary>
        let mutable con = 1
        ///<summary>条件分岐式を生成</summary>
        let branch_ (el:Branch) (cond:bool0) (code:(unit->unit)) =
            match p.lang with
            |F ->
                match el with
                |IF   ->
                    p.codewrite("if"+"("+cond.code+") then"+"\n")
                |ELIF ->
                    p.codewrite("else if"+"("+cond.code+") then"+"\n")
                |ELSE ->
                    p.codewrite("else"+"\n")
                p.indentInc()
                code()
                p.indentDec()
            |C ->
                match el with
                |IF   ->
                    p.codewrite("if"+"("+cond.code+")"+"\n")
                |ELIF ->
                    p.codewrite("else if"+"("+cond.code+")"+"\n")
                |ELSE ->
                    p.codewrite("else"+"\n")
                p.codewrite("{\n")
                p.indentInc()
                code()
                p.indentDec()
                p.codewrite("}\n")
            |T ->
                match el with
                |IF   ->
                    p.codewrite("if"+"("+cond.code+")"+"\n")
                |ELIF ->
                    p.codewrite("else if"+"("+cond.code+")"+"\n")
                |ELSE ->
                    p.codewrite("else"+"\n")
                p.indentInc()
                code()
                p.indentDec()
                p.codewrite("end\n")
            |H ->
                p.codewrite("<div class=\"codeblock\">\n")
                p.codewrite("<details open>\n")
                match el with
                |IF   ->
                    p.codewrite("<summary><span class=\"op-if\">if</span>"+" (<math>"+cond.code+"</math>)</summary>"+"\n")
                |ELIF ->
                    p.codewrite("<summary><span class=\"op-if\">else if</span>"+" (<math>"+cond.code+"</math>)</summary>"+"\n")
                |ELSE ->
                    p.codewrite("<summary><span class=\"op-if\">else</span></summary>"+"\n")
                p.indentInc()
                p.codewrite("<div class=\"insidecode-if\">\n")
                code()
                p.codewrite("</div>\n")
                p.indentDec()
                p.codewrite("</details>\n")
                p.codewrite("</div>\n")
        ///<summary>条件式(if)</summary>
        member __.IF (cond:bool0) code =
            if con=0 then
                printfn "ELの後のIFは無視されます"
            elif con=1 then
                branch_ IF cond code
            else
                branch_ ELIF cond code
            con <- con + 1
        ///<summary>条件式(else)</summary>
        member __.EL code =
            branch_ ELSE (bool0 Null) code
            con <- 0
        ///<summary>条件分岐式(2番目以降のIFは前のIFを満たさない場合のみ評価)</summary>
        static member branch code =
            let ib = br()
            code(ib)
            if p.lang=F then p.codewrite("end if\n")
            
        ///<summary>条件分岐式(if式)</summary>
        static member if1 (cond:bool0) code =
            match p.lang with
            |_ ->
                br.branch <| fun b ->
                    b.IF cond code
                
        ///<summary>条件分岐式(if...else...式)</summary>
        static member if2 (cond:bool0) code1 code2 =
            match p.lang with
            |_ ->
                br.branch <| fun b ->
                    b.IF cond code1
                    b.EL code2
                
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
        