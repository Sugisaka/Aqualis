namespace Aqualis
    
    open System
    open System.IO
    
    [<AutoOpen>]
    module Aqualis_main =
        
        ///<summary>コメント文を生成</summary>
        let (!) s = programList[prIndex].comment s
        
        ///<summary>構造体定義</summary>
        let str = structure()
        
        ///<summary>関数定義</summary>
        let func (projectname:string) (code:unit->unit) =
            let fdeclare (typ:Etype,vtp:VarType,name:string) =
                match programList[prIndex].language with
                |HTML ->
                    match vtp with 
                    |A0 -> typ.tostring programList[prIndex].language + " :: " + name
                    |A1 0 -> typ.tostring programList[prIndex].language + ",allocatable" + " :: " + name + "(:)"
                    |A2(0,0) -> typ.tostring programList[prIndex].language + ",allocatable" + " :: " + name + "(:,:)"
                    |A3(0,0,0) -> typ.tostring programList[prIndex].language + ",allocatable" + " :: " + name + "(:,:,:)"
                    |A1 _ -> typ.tostring programList[prIndex].language + " :: " + name + "(:)"
                    |A2(_,_) -> typ.tostring programList[prIndex].language + " :: " + name + "(:,:)"
                    |A3(_,_,_) -> typ.tostring programList[prIndex].language + " :: " + name + "(:,:,:)"
                |_ ->
                    match vtp with 
                    |A0 -> typ.tostring programList[prIndex].language + " :: " + name
                    |A1 0 -> typ.tostring programList[prIndex].language + ",allocatable" + " :: " + name + "(:)"
                    |A2(0,0) -> typ.tostring programList[prIndex].language + ",allocatable" + " :: " + name + "(:,:)"
                    |A3(0,0,0) -> typ.tostring programList[prIndex].language + ",allocatable" + " :: " + name + "(:,:,:)"
                    |A1 _ -> typ.tostring programList[prIndex].language + " :: " + name + "(:)"
                    |A2(_,_) -> typ.tostring programList[prIndex].language + " :: " + name + "(:,:)"
                    |A3(_,_,_) -> typ.tostring programList[prIndex].language + " :: " + name + "(:,:,:)"
            let dir = programList[prIndex].dir
            match programList[prIndex].language with
            |Fortran ->
                programList[prIndex].flist.add projectname
                let args = makeProgram [dir,projectname,Fortran] <| fun () ->
                    code()
                    programList[prIndex].close()
                    //ソースファイル(関数部分)出力
                    let writer = codeWriter(dir + "\\" + projectname, 2, programList[prIndex].language)
                    writer.codewrite "!=============================================================================================\n"
                    writer.codewrite("! Subroutine name: " + projectname + "\n")
                    for _,(_,_,nm) in programList[prIndex].arg.list do
                        writer.codewrite("!  " + nm + "\n")
                    writer.codewrite "!============================================================================================="
                    let argvar = String.Join(", ", programList[prIndex].arg.list |> List.map(fun (_,(_,_,n)) -> n))
                    writer.codewrite("subroutine " + projectname + "(" + argvar + ")\n")
                    writer.indent.inc()
                    //モジュールファイルのインクルード
                    List.iter (fun (s:string) -> writer.codewrite("use " + s + "\n")) <| programList[prIndex].mlist.list
                    writer.codewrite "implicit none"
                    //ヘッダファイルのインクルード
                    List.iter (fun (s:string) -> writer.codewrite("include " + s + "\n")) <| programList[prIndex].hlist.list
                    //サブルーチン引数の定義
                    for _,s in programList[prIndex].arg.list do
                        writer.codewrite(fdeclare s)
                    //グローバル変数の定義
                    declareall writer
                    //メインコード
                    writer.codewrite(programList[prIndex].allCodes)
                    writer.indent.dec()
                    writer.codewrite("end subroutine " + projectname + "\n")
                    writer.close()
                    programList[prIndex].delete()
                    //呼び出しコードを記述
                    String.Join(", ", programList[prIndex].arg.list |> List.map(fun (n,(_,_,_)) -> n))
                programList[prIndex].codewrite("call" + " " + projectname + "(" + args + ")\n")
            |C99 ->
                programList[prIndex].flist.add projectname
                let args = makeProgram [dir,projectname,C99] <| fun () ->
                    code()
                    programList[prIndex].close()
                    //ソースファイル(関数部分)出力
                    let writer = codeWriter(dir + "\\" + projectname, 2, programList[prIndex].language)
                    writer.codewrite "/*==========================================================================================*/\n"
                    writer.codewrite("/* Subroutine name: " + projectname + " */\n")
                    for _,(_,_,nm) in programList[prIndex].arg.list do
                        writer.codewrite("/* " + nm + " */\n")
                    writer.codewrite "/*==========================================================================================*/\n"
                    //速度を上げるために参照渡しにしている
                    let argvar =
                        programList[prIndex].arg.list
                        |> List.map (fun (_,(typ,vtp,n)) ->
                            match vtp with
                            |A1 _|A2 _|A3 _ -> typ.tostring programList[prIndex].language + " *" + n
                            |_ -> typ.tostring programList[prIndex].language + " *" + n)
                        |> fun s -> String.Join(", ", s)
                    writer.codewrite("void " + projectname + "(" + argvar + ")\n")
                    writer.codewrite "{\n"
                    writer.indent.inc()
                    //グローバル変数の定義
                    declareall writer
                    //メインコード
                    writer.codewrite(programList[prIndex].allCodes)
                    writer.indent.dec()
                    writer.codewrite "}\n"
                    writer.close()
                    programList[prIndex].delete()
                    //呼び出しコードを記述
                    programList[prIndex].arg.list
                    |> List.map (fun (n,(typ,vtp,_)) ->
                        match typ,vtp,n.StartsWith "(*" with
                        |(It _|Dt|Zt|Structure _),A0,false -> "&" + n
                        |(It _|Dt|Zt|Structure _),A0,true  -> n.Substring(2,n.Length-3)
                        |_ -> n)
                    |> fun s -> String.Join(", ", s)
                programList[prIndex].codewrite(projectname + "(" + args + ");\n")
            |LaTeX ->
                programList[prIndex].flist.add projectname
                let args = makeProgram [dir,projectname,LaTeX] <| fun () ->
                    code()
                    programList[prIndex].close()
                    //ソースファイル(関数部分)出力
                    let writer = codeWriter(dir + "\\" + projectname, 2, programList[prIndex].language)
                    writer.codewrite "%=============================================================================================\n"
                    writer.codewrite("% Subroutine name: " + projectname + "\n")
                    for _,(_,_,nm) in programList[prIndex].arg.list do
                        writer.codewrite("% " +  nm + "\n")
                    writer.codewrite("%=============================================================================================\n")
                    let argvar = String.Join(", ", programList[prIndex].arg.list |> List.map (fun (_,(_,_,n)) -> n))
                    writer.codewrite("subroutine " + projectname + "(" + argvar + ")\n")
                    writer.indent.inc()
                    //モジュールファイルのインクルード
                    List.iter (fun (s:string) -> writer.codewrite("use " + s + "\n")) <| programList[prIndex].mlist.list
                    writer.codewrite "implicit none\n"
                    //ヘッダファイルのインクルード
                    List.iter (fun (s:string) -> writer.codewrite("include " + s + "\n")) <| programList[prIndex].hlist.list
                    //サブルーチン引数の定義
                    for _,s in programList[prIndex].arg.list do
                        writer.codewrite(fdeclare s)
                    //グローバル変数の定義
                    declareall writer
                    //メインコード
                    writer.codewrite(programList[prIndex].allCodes)
                    writer.indent.dec()
                    writer.codewrite("end subroutine " + projectname + "\n")
                    writer.close()
                    programList[prIndex].delete()
                    //呼び出しコードを記述
                    String.Join(", ", programList[prIndex].arg.list |> List.map (fun (n,(_,_,_)) -> n))
                programList[prIndex].codewrite("call" + " " + projectname + "(" + args + ")\n")
            |HTML ->
                programList[prIndex].flist.add projectname
                let args = makeProgram [dir,projectname,HTML] <| fun () ->
                    code()
                    programList[prIndex].close()
                    //ソースファイル(関数部分)出力
                    let writer = codeWriter(dir + "\\" + projectname, 2, programList[prIndex].language)
                    writer.codewrite("<h3>" + projectname + "</h3>\n")
                    writer.codewrite "<ul>\n"
                    for _,(_,_,nm) in programList[prIndex].arg.list do
                        writer.codewrite("<li>\\(" + nm + "\\)</li>\n")
                    writer.codewrite "</ul>\n"
                    let argvar = String.Join(", ", programList[prIndex].arg.list |> List.map (fun (_,(_,_,n)) -> n))
                    writer.codewrite "<div class=\"codeblock\">\n"
                    writer.codewrite "<details>\n"
                    writer.codewrite("<summary><span class=\"op-func\">function</span> \\(" + projectname + "(" + argvar + ")\\)</summary>\n")
                    writer.codewrite "<div class=\"insidecode-func\">\n"
                    writer.indent.inc()
                    writer.codewrite "<ul>\n"
                    //サブルーチン引数の定義
                    for _,s in programList[prIndex].arg.list do
                        writer.codewrite("<li>" + fdeclare s + "</li>\n")
                    //グローバル変数の定義
                    declareall writer
                    writer.codewrite "</ul>"
                    //メインコード
                    writer.codewrite(programList[prIndex].allCodes)
                    writer.indent.dec()
                    writer.codewrite "</div>\n"
                    writer.codewrite "</details>\n"
                    writer.codewrite "</div>\n"
                    writer.close()
                    programList[prIndex].delete()
                    //呼び出しコードを記述
                    String.Join(", ", programList[prIndex].arg.list |> List.map (fun (n,(_,_,_)) -> n))
                programList[prIndex].codewrite("\\(" + projectname + "(" + args + ")\\)<br/>\n")
            |Python ->
                programList[prIndex].flist.add projectname
                let re_args,args = makeProgram [dir,projectname,Python] <| fun () ->
                    code()
                    programList[prIndex].close()
                    //ソースファイル(関数部分)出力
                    let writer = codeWriter(dir + "\\" + projectname, 2, programList[prIndex].language)
                    writer.codewrite "#==========================================================================================\n"
                    writer.codewrite("# Subroutine name: " + projectname + "\n")
                    for _,(_,_,nm) in programList[prIndex].arg.list do
                        writer.codewrite("# " + nm + "\n")
                    writer.codewrite "#==========================================================================================\n"
                    let argvar = 
                        programList[prIndex].arg.list
                        |> List.map (fun (_,(_,_,n)) -> n)
                        |> fun s -> String.Join(", ", s)
                    let re_argvar =
                        programList[prIndex].arg.list 
                        |> List.map (fun (_,(_,vtp,n)) -> 
                            match vtp with
                            |A1 _|A2 _|A3 _ -> ""
                            |_ -> n)
                        |> List.filter (fun s -> s <> "")
                        |> fun s -> String.Join(", ", s)
                    //呼び出しコードを記述
                    let args =
                        programList[prIndex].arg.list 
                        |> List.map (fun (n,(typ,vtp,_)) -> 
                            match typ,vtp,n.StartsWith "(*" with
                            |(It _|Dt|Zt|Structure _),A0,false -> n
                            |(It _|Dt|Zt|Structure _),A0,true  -> n.Substring(2,n.Length-3)
                            |_ -> n)
                        |> fun s -> String.Join(", ", s)
                    let re_args =
                        programList[prIndex].arg.list 
                        |> List.map (fun (n,(_,vtp,_)) ->
                            match vtp with
                            |A1 _|A2 _|A3 _ -> ""
                            |_ -> n)
                        |> List.filter (fun s -> s <> "")
                        |> fun s -> String.Join(", ", s)
                    writer.codewrite("def " + projectname + "(" + argvar + "):\n")
                    writer.indent.inc()
                    //グローバル変数の定義
                    declareall writer
                    //メインコード
                    writer.codewrite(programList[prIndex].allCodes)
                    writer.codewrite("return " + re_argvar + "\n")
                    writer.indent.dec()
                    writer.close()
                    programList[prIndex].delete()
                    re_args,args
                programList[prIndex].codewrite(re_args + " = " + projectname + "(" + args + ")\n")
            |_ -> ()
            
        ///<summary>コンパイル</summary>
        let Compile langgList dir projectname (aqver:string,codever:string) code =
            for lang in langgList do
                match lang with 
                |Fortran -> 
                    str.clear()
                    makeProgram [dir,projectname,Fortran] <| fun () ->
                        //メインコード生成
                        code()
                        programList[prIndex].close()
                        //ソースファイル出力
                        let writer = codeWriter(dir + "\\" + projectname + ".f90", 2, Fortran)
                        writer.codewrite "!=============================================================================================\n"
                        writer.codewrite("! Project name: " + projectname + "\n")
                        writer.codewrite("! Project version: " + codever + "\n")
                        writer.codewrite "!---------------------------------------------------------------------------------------------\n"
                        writer.codewrite "! Generated by Aqualis (algorithm and equation analyzer for lightwave simulation)\n"
                        writer.codewrite("! Aqualis version: " + aqver + "\n")
                        writer.codewrite("! Generated date: " + System.DateTime.Now.ToString() + "\n")
                        writer.codewrite "!=============================================================================================\n"
                        writer.codewrite("program " + projectname + "\n")
                        //モジュールファイルのインクルード
                        List.iter (fun (s:string) -> writer.codewrite("use " + s + "\n")) <| programList[prIndex].mlist.list
                        writer.codewrite "implicit none\n"
                        //ヘッダファイルのインクルード
                        List.iter (fun (s:string) -> writer.codewrite("include " + s + "\n")) <| programList[prIndex].hlist.list
                        //構造体の定義
                        str.Def_Structure writer
                        //グローバル変数の定義
                        declareall writer
                        //メインコード
                        writer.codewrite(programList[prIndex].allCodes)
                        //サブルーチン
                        writer.codewrite "\n"
                        writer.codewrite "contains\n"
                        writer.codewrite "\n"
                        for funname in programList[prIndex].flist.list do
                            writer.codewrite(File.ReadAllText(dir + "\\" + funname))
                            File.Delete(dir + "\\" + funname)
                            writer.codewrite "\n"
                        writer.codewrite("end program " + projectname + "\n")
                        writer.close()
                        //beeファイル削除
                        programList[prIndex].delete()
                    //コンパイル・実行用スクリプト生成
                    let wr = new StreamWriter(dir + "\\" + "proc_" + projectname + "_F.sh")
                    if isOaccUsed then
                        wr.Write "#!/bin/bash\n"
                        wr.Write("\n")
                        let source = String.Join(" ", programList[prIndex].slist.list)
                        let option = String.Join(" ", programList[prIndex].olist.list)
                        wr.Write("pgfortran -acc -Minfo=accel " + source + " " + projectname + ".f90 " + option + " -o " + projectname + ".exe\n")
                        wr.Write("./" + projectname + ".exe\n")
                        wr.Close()
                    else if isOmpUsed then
                        wr.Write "#!/bin/bash\n"
                        wr.Write "\n"
                        wr.Write "FC='/usr/bin/gfortran'\n"
                        wr.Write "\n"
                        let source = String.Join(" ", programList[prIndex].slist.list)
                        let option = String.Join(" ", programList[prIndex].olist.list)
                        wr.Write("$FC" + " -fopenmp " + source + " " + projectname + ".f90 " + option + " -o " + projectname + ".exe\n")
                        wr.Write("./" + projectname + ".exe\n")
                    else
                        wr.Write "#!/bin/bash\n"
                        wr.Write "\n"
                        wr.Write "FC='/usr/bin/gfortran'\n"
                        wr.Write "\n"
                        let source = String.Join(" ", programList[prIndex].slist.list)
                        let option = "-ffree-line-length-none " + String.Join(" ", programList[prIndex].olist.list)
                        wr.Write("$FC " + source + " " + projectname + ".f90 " + option + " -o " + projectname + ".exe\n")
                        wr.Write("./" + projectname + ".exe\n")
                    wr.Close()
                |C99 ->
                    str.clear()
                    makeProgram [dir,projectname,C99] <| fun () ->
                        //メインコード生成
                        programList[prIndex].olist.add "-lm"
                        programList[prIndex].indentInc()
                        code()
                        programList[prIndex].indentDec()
                        programList[prIndex].close()
                        //ソースファイル出力
                        let writer = codeWriter(dir + "\\" + projectname + ".c", 2, C99)
                        writer.codewrite "/*=============================================================================================*/\n"
                        writer.codewrite("/* Project name: " + projectname + " */\n")
                        writer.codewrite("/* Project version: " + codever + " */\n")
                        writer.codewrite "/*---------------------------------------------------------------------------------------------*/\n"
                        writer.codewrite "/* Generated by Aqualis (algorithm and equation analyzer for lightwave simulation) */\n"
                        writer.codewrite("/* Aqualis version: " + aqver + " */\n")
                        writer.codewrite("/* Generated date: " + System.DateTime.Now.ToString() + " */\n")
                        writer.codewrite "/*=============================================================================================*/\n"
                        writer.codewrite "#include <stdio.h>\n"
                        writer.codewrite "#include <stdlib.h>\n"
                        writer.codewrite "#include <complex.h>\n"
                        writer.codewrite "#include <math.h>\n"
                        //ヘッダファイルのインクルード
                        List.iter (fun (s:string) -> writer.codewrite ("#include " + s + "\n")) <| programList[prIndex].hlist.list
                        writer.codewrite "#undef I\n"
                        writer.codewrite "#define uj _Complex_I\n"
                        //構造体の定義
                        str.Def_Structure writer
                        //グローバル変数の宣言
                        declareall writer
                        //extern指定子
                        for s in programList[prIndex].elist.list do
                            writer.codewrite ("extern " + s + ";\n")
                        //関数定義
                        for funname in programList[prIndex].flist.list do
                            writer.codewrite (File.ReadAllText(dir + "\\" + funname))
                            File.Delete(dir + "\\" + funname)
                            writer.codewrite ("\n")
                        //Main関数
                        writer.codewrite "int main()\n"
                        writer.codewrite "{\n"
                        writer.codewrite (programList[prIndex].allCodes)
                        writer.codewrite "  return 0;\n"
                        writer.codewrite "}\n"
                        writer.close()
                        //beeファイル削除
                        programList[prIndex].delete()
                    //コンパイル・実行用スクリプト生成
                    let wr = new StreamWriter(dir + "\\" + "proc_" + projectname + "_C.sh")
                    if isOmpUsed then
                        wr.Write "#!/bin/bash\n"
                        wr.Write "\n"
                        let source = String.Join(" ", programList[prIndex].slist.list)
                        let option = String.Join(" ", programList[prIndex].olist.list)
                        wr.Write("gcc" + " -fopenmp " + source + " " + projectname + ".c " + option + " -o " + projectname + ".exe\n")
                        wr.Write("./" + projectname + ".exe\n")
                    else if isOaccUsed then
                        wr.Write "#!/bin/bash"
                        wr.Write "\n"
                        let source = String.Join(" ", programList[prIndex].slist.list)
                        let option = String.Join(" ", programList[prIndex].olist.list)
                        wr.Write("pgcc -acc -Minfo=accel" + source + " " + projectname + ".c " + option + " -o " + projectname + ".exe\n")
                        wr.Write("./" + projectname + ".exe\n")
                    else
                        wr.Write "#!/bin/bash\n"
                        wr.Write "\n"
                        let source = String.Join(" ", programList[prIndex].slist.list)
                        let option = String.Join(" ", programList[prIndex].olist.list)
                        wr.Write("gcc" + source + " " + projectname + ".c " + option + " -o " + projectname + ".exe\n")
                        wr.Write("./" + projectname + ".exe\n")
                    wr.Close()
                |LaTeX ->
                    str.clear()
                    makeProgram [dir,projectname,LaTeX] <| fun () ->
                        //メインコード生成
                        code()
                        programList[prIndex].close()
                        //ソースファイル出力
                        let writer = codeWriter(dir + "\\" + projectname + ".tex", 2, LaTeX)
                        writer.codewrite "\\documentclass[a4paper,fleqn]{ltjsarticle}\n"
                        writer.codewrite "\\usepackage{amsmath}\n"
                        List.iter (fun (s:string) -> writer.codewrite(s + "\n")) <| programList[prIndex].hlist.list
                        writer.codewrite "\\oddsidemargin=-0.4mm\n"
                        writer.codewrite "\\topmargin=4.6mm\n"
                        writer.codewrite "\\headheight=0mm\n"
                        writer.codewrite "\\headsep=0mm\n"
                        writer.codewrite "\\footskip=15mm\n"
                        writer.codewrite "\\textwidth=160mm\n"
                        writer.codewrite "\\textheight=237mm\n"
                        writer.codewrite "\\topsep=6pt\n"
                        writer.codewrite "\\parindent=0mm\n"
                        writer.codewrite "\\unitlength=1.00mm\n"
                        writer.codewrite "\\begin{document}\n"
                        writer.codewrite("{\\Large " + projectname.Replace("_","\\_") + "}\n")
                        //構造体の定義
                        writer.codewrite "\\section{structures}\n"
                        str.Def_Structure writer
                        //関数定義
                        writer.codewrite "\\section{subroutines}\n"
                        for funname in programList[prIndex].flist.list do
                            writer.codewrite(File.ReadAllText(dir + "\\" + funname))
                            File.Delete(dir + "\\" + funname)
                            writer.codewrite("\n")
                        //グローバル変数の定義
                        writer.codewrite "\\section{global variables}\n"
                        writer.codewrite "\\begin{itemize}\n"
                        declareall writer
                        writer.codewrite "\\end{itemize}\n"
                        //メインコード
                        writer.codewrite "\\section{main code}\n"
                        writer.codewrite(programList[prIndex].allCodes)
                        writer.codewrite "\\end{document}\n"
                        writer.close()
                        //beeファイル削除
                        programList[prIndex].delete()
                |HTML ->
                    str.clear()
                    makeProgram [dir,projectname,HTML] <| fun () ->
                        //メインコード生成
                        code()
                        programList[prIndex].close()
                        //ソースファイル出力
                        let writer = codeWriter(dir + "\\" + projectname + ".html", 2, HTML)
                        writer.codewrite "<!DOCTYPE html>\n"
                        writer.codewrite "<html lang='ja'>\n"
                        writer.codewrite "\t<head>\n"
                        writer.codewrite "\t\t<meta charset='utf-8'>\n"
                        writer.codewrite "\t\t<script>\n"
                        writer.codewrite "\t\tMathJax = {\n"
                        writer.codewrite "\t\t\tchtml: {\n"
                        writer.codewrite "\t\t\t\tdisplayAlign: \"left\",\n"
                        writer.codewrite "\t\t\t}\n"
                        writer.codewrite "\t\t};\n"
                        writer.codewrite "\t\t</script>\n"
                        writer.codewrite "\t\t<script type=\"text/javascript\" id=\"MathJax-script\" async src=\"https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml.js\"></script>\n"
                        writer.codewrite("\t\t<title>" + projectname + "</title>\n")
                        writer.codewrite "\t\t<meta name='viewport' content='width=device-width, initial-scale=1.0, maximum-scale=1.0, minimum-scale=1.0'>\n"
                        writer.codewrite "\t\t<link rel=\"preconnect\" href=\"https://fonts.googleapis.com\">\n"
                        writer.codewrite "\t\t<link rel=\"preconnect\" href=\"https://fonts.gstatic.com\" crossorigin>\n"
                        writer.codewrite "\t\t<link href=\"https://fonts.googleapis.com/css2?family=Noto + Sans + JP:wght@500;600;700&display=swap\" rel=\"stylesheet\">\n"
                        writer.codewrite "\t\t<style type=\"text/css\">\n"
                        writer.codewrite "\t\t<!--\n"
                        writer.codewrite "\t\tbody {\n"
                        writer.codewrite "\t\t\tfont-family: 'Noto Sans JP', sans-serif;\n"
                        writer.codewrite "\t\t\tfont-weight: 500;\n"
                        writer.codewrite "\t\t\tfont-size: 16px;\n"
                        writer.codewrite "\t\t}\n"
                        writer.codewrite "\t\th2 {\n"
                        writer.codewrite "\t\t\tborder-bottom: 2px solid;\n"
                        writer.codewrite "\t\t}\n"
                        writer.codewrite "\t\ta {\n"
                        writer.codewrite "\t\t\tfont-size: 11pt;\n"
                        writer.codewrite "\t\t\ttext-decoration: none;\n"
                        writer.codewrite "\t\t\tcolor: #8000ff;\n"
                        writer.codewrite "\t\t}\n"
                        writer.codewrite "\t\t.fio {\n"
                        writer.codewrite "\t\t\tmargin-right: 10px;\n"
                        writer.codewrite "\t\t\tfont-size: 11pt;\n"
                        writer.codewrite "\t\t\tcolor: #ff00ff;\n"
                        writer.codewrite "\t\t}\n"
                        writer.codewrite "\t\t.continue {\n"
                        writer.codewrite "\t\t\tfont-size: 11pt;\n"
                        writer.codewrite "\t\t\tcolor: #8000ff;\n"
                        writer.codewrite "\t\t}\n"
                        writer.codewrite "\t\t.codeblock {\n"
                        writer.codewrite "\t\t\tpadding-left: 0px;\n"
                        writer.codewrite "\t\t}\n"
                        writer.codewrite "\t\t\n"
                        writer.codewrite "\t\t.op-loop {\n"
                        writer.codewrite "\t\t\tfont-size: 11pt;\n"
                        writer.codewrite "\t\t\tcolor: #ff7f00;\n"
                        writer.codewrite "\t\t}\n"
                        writer.codewrite "\t\t\n"
                        writer.codewrite "\t\t.insidecode-loop {\n"
                        writer.codewrite "\t\t\tmargin-left: 2px;\n"
                        writer.codewrite "\t\t\tpadding-left: 30px;\n"
                        writer.codewrite "\t\t\tborder-left: solid;\n"
                        writer.codewrite "\t\t\tborder-width: 5px;\n"
                        writer.codewrite "\t\t\tborder-color: #ffa347;\n"
                        writer.codewrite "\t\t}\n"
                        writer.codewrite "\t\t\n"
                        writer.codewrite "\t\t.op-if {\n"
                        writer.codewrite "\t\t\tfont-size: 11pt;\n"
                        writer.codewrite "\t\t\tcolor: #007fff;\n"
                        writer.codewrite "\t\t}\n"
                        writer.codewrite "\t\t\n"
                        writer.codewrite "\t\t.insidecode-if {\n"
                        writer.codewrite "\t\t\tmargin-left: 2px;\n"
                        writer.codewrite "\t\t\tpadding-left: 30px;\n"
                        writer.codewrite "\t\t\tborder-left: solid;\n"
                        writer.codewrite "\t\t\tborder-width: 5px;\n"
                        writer.codewrite "\t\t\tborder-color: #47a3ff;\n"
                        writer.codewrite "\t\t}\n"
                        writer.codewrite "\t\t.op-func {\n"
                        writer.codewrite "\t\t\tfont-size: 11pt;\n"
                        writer.codewrite "\t\t\tcolor: #0000ff;\n"
                        writer.codewrite "\t\t}\n"
                        writer.codewrite "\t\t\n"
                        writer.codewrite "\t\t.insidecode-func {\n"
                        writer.codewrite "\t\t\tmargin-left: 2px;\n"
                        writer.codewrite "\t\t\tpadding-left: 30px;\n"
                        writer.codewrite "\t\t\tborder-left: solid;\n"
                        writer.codewrite "\t\t\tborder-width: 5px;\n"
                        writer.codewrite "\t\t\tborder-color: #0000ff;\n"
                        writer.codewrite "\t\t}\n"
                        writer.codewrite "\t\t.op-section {\n"
                        writer.codewrite "\t\t\tfont-size: 11pt;\n"
                        writer.codewrite "\t\t\tcolor: #008000;\n"
                        writer.codewrite "\t\t\tmargin-right: 5px;\n"
                        writer.codewrite "\t\t}\n"
                        writer.codewrite "\t\t\n"
                        writer.codewrite "\t\t.insidecode-section {\n"
                        writer.codewrite "\t\t\tmargin-left: 2px;\n"
                        writer.codewrite "\t\t\tpadding-left: 30px;\n"
                        writer.codewrite "\t\t\tborder-left: solid;\n"
                        writer.codewrite "\t\t\tborder-width: 5px;\n"
                        writer.codewrite "\t\t\tborder-color: #32cd32;\n"
                        writer.codewrite "\t\t}\n"
                        writer.codewrite "\t\t\n"
                        writer.codewrite "\t\t.comment {\n"
                        writer.codewrite "\t\t\tfont-size: 11pt;\n"
                        writer.codewrite "\t\t\tcolor: #008000;\n"
                        writer.codewrite "\t\t}\n"
                        writer.codewrite "\t\t-->\n"
                        writer.codewrite "\t\t</style>\n"
                        writer.codewrite "\t\t<script type=\"text/javascript\">\n"
                        writer.codewrite "\t\t\tfunction fsearch()\n"
                        writer.codewrite "\t\t\t{\n"
                        writer.codewrite "\t\t\t\t//var s = document.styleSheets.item(0);\n"
                        writer.codewrite "\t\t\t\t//s.cssRules[0].style.backgroundColor=\"blue\";\n"
                        writer.codewrite "\t\t\t\tvar vname = document.getElementById(\"textvar\").value;\n"
                        writer.codewrite "\t\t\t\tvar targets = document.getElementsByClassName(vname);\n"
                        writer.codewrite "\t\t\t\tfor (i = 0; i < targets.length; i++)\n"
                        writer.codewrite "\t\t\t\t{\n"
                        writer.codewrite "\t\t\t\t\ttargets[i].style.color =  '#ff0000';\n"
                        writer.codewrite "\t\t\t\t}\n"
                        writer.codewrite "\t\t\t}\n"
                        writer.codewrite "\t\t</script>\n"
                        writer.codewrite "\t</head>\n"
                        writer.codewrite "\t<body>\n"
                        writer.codewrite("\t\t<h1>" + projectname + "</h1>\n")
                        writer.codewrite "\t\t<div id=\"codeinfo\">\n"
                        writer.codewrite "\t\t\t<ul>\n"
                        writer.codewrite("\t\t\t\t<li>Project version: " + codever + "</li>\n")
                        writer.codewrite "\t\t\t\t<li>Generated by Aqualis (algorithm and equation analyzer for lightwave simulation)</li>\n"
                        writer.codewrite("\t\t\t\t<li>Aqualis version: " + aqver + "</li>\n")
                        writer.codewrite("\t\t\t\t<li>Generated date: " + System.DateTime.Now.ToString() + "</li>\n")
                        writer.codewrite "\t\t\t</ul>\n"
                        writer.codewrite "\t\t</div>\n"
                        //構造体の定義
                        writer.codewrite "\t\t<div id=\"defstr\">\n"
                        writer.codewrite "\t\t<h2>構造体定義</h2>\n"
                        str.Def_Structure writer
                        writer.codewrite "\t\t</div>\n"
                        //関数定義
                        writer.codewrite "\t\t<div id=\"deffunc\">\n"
                        writer.codewrite "\t\t<h2>関数定義</h2>\n"
                        for funname in programList[prIndex].flist.list do
                            writer.codewrite(File.ReadAllText(dir + "\\" + funname))
                            File.Delete(dir + "\\" + funname)
                            writer.codewrite "\n"
                        writer.codewrite "\t\t</div>\n"
                        //グローバル変数の定義
                        writer.codewrite "\t\t<div id=\"defvar\">\n"
                        writer.codewrite "\t\t<h2>グローバル変数</h2>\n"
                        writer.codewrite "\t\t<ul>\n"
                        declareall writer
                        writer.codewrite "\t\t</ul>\n"
                        writer.codewrite "\t\t</div>\n"
                        //メインコード
                        writer.codewrite "\t\t<div id=\"maincode\">\n"
                        writer.codewrite "\t\t<h2>メインコード</h2>\n"
                        writer.codewrite "<input type=\"text\" id=\"textvar\" value=\"\">\n"
                        writer.codewrite "<input type=\"button\" onclick=\"fsearch()\" value=\"Search\">\n"
                        writer.codewrite "<br/>\n"
                        writer.codewrite(programList[prIndex].allCodes)
                        writer.codewrite "\t\t</div>\n"
                        writer.codewrite "\t</body>\n"
                        writer.codewrite "</html>\n"
                        writer.close()
                        //beeファイル削除
                        programList[prIndex].delete()
                |Python ->
                    str.clear()
                    makeProgram [dir,projectname,Python] <| fun () ->
                        //メインコード生成
                        code()
                        programList[prIndex].close()
                        //ソースファイル出力
                        let writer = codeWriter(dir + "\\" + projectname + ".py", 2, Python)
                        writer.codewrite "#=============================================================================================\n"
                        writer.codewrite("# Project name: " + projectname + "\n")
                        writer.codewrite("# Project version: " + codever + "\n")
                        writer.codewrite "#---------------------------------------------------------------------------------------------\n"
                        writer.codewrite "# Generated by Aqualis (algorithm and equation analyzer for lightwave simulation)\n"
                        writer.codewrite("# Aqualis version: " + aqver + "\n")
                        writer.codewrite("# Generated date: " + System.DateTime.Now.ToString() + "\n")
                        writer.codewrite "#=============================================================================================\n"
                        //次のコマンドを打って、NumPyとSciPyをインストール
                        //pip install numpy scipy
                        writer.codewrite "import numpy\n"
                        writer.codewrite "import math\n"
                        writer.codewrite "import cmath\n"
                        writer.codewrite "import copy\n"
                        writer.codewrite "import struct\n"
                        writer.codewrite "import re\n"
                        writer.codewrite "from scipy.linalg import solve, svd, eig, lu\n"
                        writer.codewrite "from scipy.special import jv, yn\n"
                        writer.codewrite "import sys\n"
                        //ヘッダファイルのインクルード
                        List.iter (fun (s:string) -> writer.codewrite("import " + s + "\n")) <| programList[prIndex].hlist.list
                        //構造体の定義
                        str.Def_Structure writer
                        //グローバル変数の定義
                        declareall writer
                        //関数定義
                        for funname in programList[prIndex].flist.list do
                            writer.codewrite(File.ReadAllText(dir + "\\" + funname))
                            File.Delete(dir + "\\" + funname)
                            writer.codewrite("\n")
                        //メインコード
                        writer.codewrite(programList[prIndex].allCodes)
                        writer.close()
                        //beeファイル削除
                        programList[prIndex].delete()
                    //コンパイル・実行用スクリプト生成
                    //.pyファイルをexeファイルに変換する。pyinstaller --onefile your_script.py
                    //PyInstallerをインストールする必要がある
                    //コマンドラインで「pip install pyinstaller」を実行
                    let wr = new StreamWriter(dir + "\\" + "proc_" + projectname + "_P.sh")
                    wr.Write "#!/bin/bash\n"
                    wr.Write "\n"
                    wr.Write("python3 " + projectname + ".py\n")
                    wr.Close()
                |JavaScript ->
                    str.clear()
                    makeProgram [dir,projectname,JavaScript] <| fun () ->
                        //メインコード生成
                        programList[prIndex].indentInc()
                        code()
                        programList[prIndex].indentDec()
                        programList[prIndex].close()
                        //ソースファイル出力
                        let writer = codeWriter(dir + "\\" + projectname + ".c", 2, C99)
                        writer.codewrite "/*=============================================================================================*/\n"
                        writer.codewrite("/* Project name: " + projectname + " */\n")
                        writer.codewrite("/* Project version: " + codever + " */\n")
                        writer.codewrite "/*---------------------------------------------------------------------------------------------*/\n"
                        writer.codewrite "/* Generated by Aqualis (algorithm and equation analyzer for lightwave simulation) */\n"
                        writer.codewrite("/* Aqualis version: " + aqver + " */\n")
                        writer.codewrite("/* Generated date: " + System.DateTime.Now.ToString() + " */\n")
                        writer.codewrite "/*=============================================================================================*/\n"
                        writer.codewrite "#include <stdio.h>\n"
                        writer.codewrite "#include <stdlib.h>\n"
                        writer.codewrite "#include <complex.h>\n"
                        writer.codewrite "#include <math.h>\n"
                        //ヘッダファイルのインクルード
                        List.iter (fun (s:string) -> writer.codewrite ("#include " + s + "\n")) <| programList[prIndex].hlist.list
                        writer.codewrite "#undef I\n"
                        writer.codewrite "#define uj _Complex_I\n"
                        //構造体の定義
                        str.Def_Structure writer
                        //グローバル変数の宣言
                        declareall writer
                        //extern指定子
                        for s in programList[prIndex].elist.list do
                            writer.codewrite ("extern " + s + ";\n")
                        //関数定義
                        for funname in programList[prIndex].flist.list do
                            writer.codewrite (File.ReadAllText(dir + "\\" + funname))
                            File.Delete(dir + "\\" + funname)
                            writer.codewrite ("\n")
                        //Main
                        writer.codewrite (programList[prIndex].allCodes)
                        writer.close()
                        //beeファイル削除
                        programList[prIndex].delete()
                |PHP ->
                    str.clear()
                    makeProgram [dir,projectname,PHP] <| fun () ->
                        //メインコード生成
                        programList[prIndex].indentInc()
                        code()
                        programList[prIndex].indentDec()
                        programList[prIndex].close()
                        //ソースファイル出力
                        let writer = codeWriter(dir + "\\" + projectname + ".c", 2, C99)
                        writer.codewrite "/*=============================================================================================*/\n"
                        writer.codewrite("/* Project name: " + projectname + " */\n")
                        writer.codewrite("/* Project version: " + codever + " */\n")
                        writer.codewrite "/*---------------------------------------------------------------------------------------------*/\n"
                        writer.codewrite "/* Generated by Aqualis (algorithm and equation analyzer for lightwave simulation) */\n"
                        writer.codewrite("/* Aqualis version: " + aqver + " */\n")
                        writer.codewrite("/* Generated date: " + System.DateTime.Now.ToString() + " */\n")
                        writer.codewrite "/*=============================================================================================*/\n"
                        writer.codewrite "#include <stdio.h>\n"
                        writer.codewrite "#include <stdlib.h>\n"
                        writer.codewrite "#include <complex.h>\n"
                        writer.codewrite "#include <math.h>\n"
                        //ヘッダファイルのインクルード
                        List.iter (fun (s:string) -> writer.codewrite ("#include " + s + "\n")) <| programList[prIndex].hlist.list
                        writer.codewrite "#undef I\n"
                        writer.codewrite "#define uj _Complex_I\n"
                        //構造体の定義
                        str.Def_Structure writer
                        //グローバル変数の宣言
                        declareall writer
                        //extern指定子
                        for s in programList[prIndex].elist.list do
                            writer.codewrite ("extern " + s + ";\n")
                        //関数定義
                        for funname in programList[prIndex].flist.list do
                            writer.codewrite (File.ReadAllText(dir + "\\" + funname))
                            File.Delete(dir + "\\" + funname)
                            writer.codewrite ("\n")
                        //Main
                        writer.codewrite (programList[prIndex].allCodes)
                        writer.close()
                        //beeファイル削除
                        programList[prIndex].delete()
                |Numeric -> code()
