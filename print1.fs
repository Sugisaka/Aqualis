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

    ///<summary>画面表示</summary>
    type print () =
        static member br with get() =
            let p = p.param
            match p.lang with
            |F -> 
                p.codewrite("write(*,*)"+"\n")
            |C -> 
                p.codewrite("printf(\"\\n\");\n")
            |_ -> ()
            p.clear_printcounter()
        static member c (v:string) =
            let p = p.param
            match p.lang with
            |F ->
                let tab =
                    p.var.setUniqVar(It 4,A0,"tab",(p.ItoS 2313))
                    int0(Var "tab")
                let int0string_format_F = 
                    let a=p.int_string_format
                    "I"+a.ToString()
                if p.printcounter<>0 then 
                    p.codewrite("write(*,fmt='(A1)',advance='no') "+tab.code+"\n")
                p.codewrite("write(*,fmt='(A"+v.Length.ToString()+")',advance='no') \""+v+"\"\n")
            |C ->
                let int0string_format_C =
                    "%"+p.int_string_format.ToString()+"d"
                if p.printcounter<>0 then 
                    p.codewrite("printf(\"\\t\");\n")
                p.codewrite("printf(\""+v+"\");\n")
            |T ->
                p.codewrite("print "+v+"\n")
            |H ->
                p.codewrite("<span class=\"fio\">print</span><math><mo>&larr;</mo>"+v+"</math>\n<br/>\n")
            p.add_printcounter(1)
        static member c (v:int0) =
            let p = p.param
            match p.lang with
            |F ->
                let tab =
                    p.var.setUniqVar(It 4,A0,"tab",(p.ItoS 2313))
                    int0(Var "tab")
                let int0string_format_F = 
                    let a=p.int_string_format
                    "I"+a.ToString()
                if p.printcounter<>0 then 
                    p.codewrite("write(*,fmt='(A1)',advance='no') "+tab.code+"\n")
                p.codewrite("write(*,fmt='("+int0string_format_F+")',advance='no') "+v.code+"\n")
            |C ->
                let int0string_format_C =
                    "%"+p.int_string_format.ToString()+"d"
                if p.printcounter<>0 then 
                    p.codewrite("printf(\"\\t\");\n")
                p.codewrite("printf(\""+int0string_format_C+"\", "+v.code+");\n")
            |T ->
                p.codewrite("print "+v.code+"\n")
            |H ->
                p.codewrite("<span class=\"fio\">print</span><math><mo>&larr;</mo>"+v.code+"</math>\n<br/>\n")
            p.add_printcounter(1)
        static member c (v:int) = print.c (v.I)
                    
        static member c (v:float0) =
            let p = p.param
            match p.lang with
            |F ->
                let tab =
                    p.var.setUniqVar(It 4,A0,"tab",(p.ItoS 2313))
                    int0(Var "tab")
                let double0string_format_F = 
                    let (a,b)=p.double_string_format
                    "E"+a.ToString()+"."+b.ToString()+"e3"
                if p.printcounter<>0 then 
                    p.codewrite("write(*,fmt='(A1)',advance='no') "+tab.code+"\n")
                p.codewrite("write(*,fmt='("+double0string_format_F+")',advance='no') "+v.code+"\n")
            |C ->
                let double0string_format_C = 
                    let (a,b)=p.double_string_format
                    "%"+a.ToString()+"."+b.ToString()+"e"
                if p.printcounter<>0 then 
                    p.codewrite("printf(\"\\t\");\n")
                p.codewrite("printf(\""+double0string_format_C+"\", "+v.code+");\n")
            |T ->
                p.codewrite("write "+v.code+"\n")
            |H ->
                p.codewrite("<span class=\"fio\">print</span><math><mo>&larr;</mo>"+v.code+"</math>\n<br/>\n")
            p.add_printcounter(1)
        static member c (v:double) = print.c (v.D)
        
        static member c (v:complex0) =
            let p = p.param
            match p.lang with
            |F ->
                let tab =
                    p.var.setUniqVar(It 4,A0,"tab",(p.ItoS 2313))
                    int0(Var "tab")
                let double0string_format_F = 
                    let (a,b)=p.double_string_format
                    "E"+a.ToString()+"."+b.ToString()+"e3"
                if p.printcounter<>0 then 
                    p.codewrite("write(*,fmt='(A1)',advance='no') "+tab.code+"\n")
                p.codewrite("write(*,fmt='("+double0string_format_F+")',advance='no') "+v.re.code+"\n")
                p.codewrite("write(*,fmt='(A1)',advance='no') "+tab.code+"\n")
                p.codewrite("write(*,fmt='("+double0string_format_F+")',advance='no') "+v.im.code+"\n")
            |C ->
                let double0string_format_C = 
                    let (a,b)=p.double_string_format
                    "%"+a.ToString()+"."+b.ToString()+"e"
                if p.printcounter<>0 then 
                    p.codewrite("printf(\"\\t\");\n")
                p.codewrite("printf(\""+double0string_format_C+"\", "+v.re.code+");\n")
                p.codewrite("printf(\"\\t\");\n")
                p.codewrite("printf(\""+double0string_format_C+"\", "+v.im.code+");\n")
            |T ->
                p.codewrite("write "+v.code+"\n")
            |H ->
                p.codewrite("<span class=\"fio\">print</span><math><mo>&larr;</mo>"+v.code+"</math>\n<br/>\n")
            p.add_printcounter(2)
            
        static member c (v:num0) =
            match v.etype with
            |It _ -> print.c (int0(v.expr))
            |Dt _ -> print.c (float0(v.expr))
            |Zt _ -> print.c (complex0(v.expr))
            |_ -> printfn "%s: printできない型です" <| v.etype.ToString()

        static member cn (v:string) =
            print.c v
            print.br
            
        static member cn (v:int) =
            print.c v
            print.br

        static member cn (v:double) =
            print.c v
            print.br
            
        static member cn (v:int0) =
            print.c v
            print.br
            
        static member cn (v:float0) =
            print.c v
            print.br
            
        static member cn (v:complex0) =
            print.c v
            print.br
            
        static member cn (v:num0) =
            print.c v
            print.br