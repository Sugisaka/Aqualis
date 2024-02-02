(*
Copyright (c) 2022 Jun-ichiro Sugisaka

This software is released under the MIT License.
http://opensource.org/licenses/mit-license.php
*)
namespace Aqualis
    
    open System
    open System.IO
    open Aqualis_base
    
    [<AutoOpen>]
    module Aqualis_main =
        
        ///<summary>コメント文を生成</summary>
        let (!) s = p.comment s
        
        ///<summary>構造体定義</summary>
        let str = structure()
        
        ///<summary>警告を表示し、処理を一時停止</summary>
        let warning(s:string) =
            Console.WriteLine(s)
            ignore <| Console.Read()
            
        /// <summary>
        /// コードのセクション
        /// </summary>
        /// <param name="onoff">ONまたはOFF。OFFならばcodeを処理しない</param>
        /// <param name="code">コード</param>
        let ActiveSection (_:string) (onoff:Switch) code =
            match onoff with
            |OFF -> ()
            |ON  -> code()
              
        ///<summary>関数定義</summary>
        let func (projectname:string) (code:unit->unit) =
            let dir_ = p.dir
            let fdeclare (typ:Etype,vtp:VarType,name:string) =
                match p.lang with
                |H ->
                    match vtp with 
                    |A0 -> typ.tostring(p.lang)+" :: "+name
                    |A1(0) -> typ.tostring(p.lang)+",allocatable"+" :: "+name+"(:)"
                    |A2(0,0) -> typ.tostring(p.lang)+",allocatable"+" :: "+name+"(:,:)"
                    |A3(0,0,0) -> typ.tostring(p.lang)+",allocatable"+" :: "+name+"(:,:,:)"
                    |A1(_) -> typ.tostring(p.lang)+" :: "+name+"(:)"
                    |A2(_,_) -> typ.tostring(p.lang)+" :: "+name+"(:,:)"
                    |A3(_,_,_) -> typ.tostring(p.lang)+" :: "+name+"(:,:,:)"
                |_ ->
                    match vtp with 
                    |A0 -> typ.tostring(p.lang)+" :: "+name
                    |A1(0) -> typ.tostring(p.lang)+",allocatable"+" :: "+name+"(:)"
                    |A2(0,0) -> typ.tostring(p.lang)+",allocatable"+" :: "+name+"(:,:)"
                    |A3(0,0,0) -> typ.tostring(p.lang)+",allocatable"+" :: "+name+"(:,:,:)"
                    |A1(_) -> typ.tostring(p.lang)+" :: "+name+"(:)"
                    |A2(_,_) -> typ.tostring(p.lang)+" :: "+name+"(:,:)"
                    |A3(_,_,_) -> typ.tostring(p.lang)+" :: "+name+"(:,:,:)"
            match p.lang with
            |F ->
                p.param_main.funlist <- projectname::p.param_main.funlist
                p.param_add(F, dir_, projectname)
                //ここから関数定義。p.paramは関数用のものに変わる
                p.paramClear()
                //メインコード生成
                p.indentInc()
                code()
                p.indentDec()
                p.cclose()
                p.indentInc()
                p.declareall()
                p.indentDec()
                p.vclose()
                //ソースファイル(関数部分)出力
                p.hwrite("!============================================================================================="+"\n")
                p.hwrite("! Subroutine name: "+projectname+"\n")
                for _,(_,_,nm) in p.arglist do
                    p.hwrite("!  "+nm+" \n")
                p.hwrite("!============================================================================================="+"\n")
                let argvar = 
                    let cat acc i =
                        let _,(_,_,n) = p.arglist.[i]
                        match i with
                        |_ when (i<>p.arglist.Length-1) -> acc + n + ", "
                        |_ -> acc + n
                    List.fold cat "" [0..p.arglist.Length-1]
                p.codefold("subroutine "+projectname+"("+argvar+")"+"\n","",p.hwrite,100)
                p.indentInc()
                //モジュールファイルのインクルード
                List.iter (fun (s:string) -> p.hwrite("use "+s+"\n")) <| p.mlist.list
                p.hwrite(p.indentSpace + "implicit none"+"\n")
                //ヘッダファイルのインクルード
                List.iter (fun (s:string) -> p.hwrite(p.indentSpace + "include "+s+"\n")) <| p.hlist.list
                //サブルーチン引数の定義
                for _,s in p.arglist do
                    p.hwrite(p.indentSpace + fdeclare(s) + "\n")
                //グローバル変数の定義
                p.hwrite(File.ReadAllText(dir_+"\\"+projectname+"_var"+".bee"))
                File.Delete(dir_+"\\"+projectname+"_var"+".bee")
                //メインコード
                p.hwrite(File.ReadAllText(dir_+"\\"+projectname+"_code.bee"))
                File.Delete(dir_+"\\"+projectname+"_code.bee")
                p.pclose()
                File.Delete(dir_+"\\"+projectname+"_par.bee")
                p.indentDec()
                p.hwrite("end subroutine "+projectname+"\n")
                p.hclose()
                //呼び出しコードを記述
                let args = 
                    let cat acc i =
                        let n,(_,_,_) = p.arglist.[i]
                        match i with
                        |_ when (i<>p.arglist.Length-1) -> acc + n + ", "
                        |_ -> acc + n
                    List.fold cat "" [0..p.arglist.Length-1]
                //もとの関数に戻る
                p.param_back()
                p.codewrite("call" + " " + projectname + "(" + args + ")\n")
            |C ->
                p.param_main.funlist <- projectname::p.param_main.funlist
                p.param_add(C, dir_, projectname)
                //ここから関数定義。p.paramは関数用のものに変わる
                p.paramClear()
                //メインコード生成
                p.indentInc()
                code()
                p.indentDec()
                p.cclose()
                p.indentInc()
                p.declareall()
                p.indentDec()
                p.vclose()
                //ソースファイル(関数部分)出力
                p.hwrite("/*==========================================================================================*/"+"\n")
                p.hwrite("/* Subroutine name: "+projectname+" */\n")
                for _,(_,_,nm) in p.arglist do
                    p.hwrite("/* "+nm+" */\n")
                p.hwrite("/*==========================================================================================*/"+"\n")
                let argvar = 
                    let cat acc i =
                        let _,(typ,vtp,n) = p.arglist.[i]
                        let cm = if (i=p.arglist.Length-1) then "" else ", "
                        match vtp with
                        |A1(_)|A2(_)|A3(_) -> 
                            acc + typ.tostring(p.lang) + " *" + n + cm
                        |_ -> 
                            acc + typ.tostring(p.lang) + " *" + n + cm
                    List.fold cat "" [0..p.arglist.Length-1]
                p.hwrite("void "+projectname+"("+argvar+")"+"\n")
                p.hwrite("{\n")
                p.indentInc()
                //グローバル変数の定義
                p.hwrite(File.ReadAllText(dir_+"\\"+projectname+"_var"+".bee"))
                File.Delete(dir_+"\\"+projectname+"_var"+".bee")
                //メインコード
                p.hwrite(File.ReadAllText(dir_+"\\"+projectname+"_code.bee"))
                File.Delete(dir_+"\\"+projectname+"_code.bee")
                p.pclose()
                File.Delete(dir_+"\\"+projectname+"_par.bee")
                p.indentDec()
                p.hwrite("}\n")
                p.hclose()
                //呼び出しコードを記述
                let args = 
                    let cat acc i =
                        let n,(typ,vtp,_) = p.arglist.[i]
                        let cm = if (i=p.arglist.Length-1) then "" else ", "
                        match typ,vtp,(n.StartsWith "(*") with
                        |(It _|Dt|Zt|Structure _),A0,false -> 
                            acc + "&" + n + cm
                        |(It _|Dt|Zt|Structure _),A0,true  -> 
                            let n_ = n.Substring(2,n.Length-3) //(n.Split([|'*'|],StringSplitOptions.RemoveEmptyEntries)).[0]
                            acc + n_ + cm
                        |_ -> acc + n + cm
                    List.fold cat "" [0..p.arglist.Length-1]
                //もとの関数に戻る
                p.param_back()
                p.codewrite(projectname + "(" + args + ");\n")
            |T ->
                p.param_main.funlist <- projectname::p.param_main.funlist
                p.param_add(T, dir_, projectname)
                //ここから関数定義。p.paramは関数用のものに変わる
                p.paramClear()
                //メインコード生成
                p.indentInc()
                code()
                p.indentDec()
                p.cclose()
                p.indentInc()
                p.declareall()
                p.indentDec()
                p.vclose()
                //ソースファイル(関数部分)出力
                p.hwrite("%============================================================================================="+"\n")
                p.hwrite("% Subroutine name: "+projectname+"\n")
                for _,(_,_,nm) in p.arglist do
                    p.hwrite("% "+nm+"\n")
                p.hwrite("%============================================================================================="+"\n")
                let argvar = 
                    let cat acc i =
                        let _,(_,_,n) = p.arglist.[i]
                        match i with
                        |_ when (i<>p.arglist.Length-1) -> acc + n + ", "
                        |_ -> acc + n
                    List.fold cat "" [0..p.arglist.Length-1]
                p.hwrite("subroutine "+projectname+"("+argvar+")"+"\n")
                p.indentInc()
                //モジュールファイルのインクルード
                List.iter (fun (s:string) -> p.hwrite("use "+s+"\n")) <| p.mlist.list
                p.hwrite(p.indentSpace + "implicit none"+"\n")
                //ヘッダファイルのインクルード
                List.iter (fun (s:string) -> p.hwrite(p.indentSpace + "include "+s+"\n")) <| p.hlist.list
                //サブルーチン引数の定義
                for _,s in p.arglist do
                    p.hwrite(p.indentSpace + fdeclare(s) + "\n")
                //グローバル変数の定義
                p.hwrite(File.ReadAllText(dir_+"\\"+projectname+"_var"+".bee"))
                File.Delete(dir_+"\\"+projectname+"_var.bee")
                //メインコード
                p.hwrite(File.ReadAllText(dir_+"\\"+projectname+"_code.bee"))
                File.Delete(dir_+"\\"+projectname+"_code.bee")
                p.pclose()
                File.Delete(dir_+"\\"+projectname+"_par.bee")
                p.indentDec()
                p.hwrite("end subroutine "+projectname+"\n\n")
                p.hclose()
                //呼び出しコードを記述
                let args = 
                    let cat acc i =
                        let n,(_,_,_) = p.arglist.[i]
                        match i with
                        |_ when (i<>p.arglist.Length-1) -> acc + n + ", "
                        |_ -> acc + n
                    List.fold cat "" [0..p.arglist.Length-1]
                //もとの関数に戻る
                p.param_back()
                p.codewrite("call" + " " + projectname + "(" + args + ")\n")
            |H ->
                p.param_main.funlist <- projectname::p.param_main.funlist
                p.param_add(H, dir_, projectname)
                //ここから関数定義。p.paramは関数用のものに変わる
                p.paramClear()
                //メインコード生成
                p.indentInc()
                code()
                p.indentDec()
                p.cclose()
                p.indentInc()
                p.declareall()
                p.indentDec()
                p.vclose()
                //ソースファイル(関数部分)出力
                p.hwrite("<h3>"+projectname+"</h3>\n")
                p.hwrite("<ul>"+"\n")
                for _,(_,_,nm) in p.arglist do
                    p.hwrite("<li>\\("+nm+"\\)</li>\n")
                p.hwrite("</ul>"+"\n")
                let argvar = 
                    let cat acc i =
                        let _,(_,_,n) = p.arglist.[i]
                        match i with
                        |_ when (i<>p.arglist.Length-1) -> acc + n + ", "
                        |_ -> acc + n
                    List.fold cat "" [0..p.arglist.Length-1]
                p.hwrite("<div class=\"codeblock\">\n")
                p.hwrite("<details>\n")
                p.hwrite("<summary><span class=\"op-func\">function</span> \\("+projectname+"("+argvar+")\\)</summary>"+"\n")
                p.hwrite("<div class=\"insidecode-func\">\n")
                p.indentInc()
                p.hwrite("<ul>\n")
                //サブルーチン引数の定義
                for _,s in p.arglist do
                    p.hwrite(p.indentSpace + "<li>" + fdeclare(s) + "</li>" + "\n")
                //グローバル変数の定義
                p.hwrite(File.ReadAllText(dir_+"\\"+projectname+"_var"+".bee"))
                File.Delete(dir_+"\\"+projectname+"_var"+".bee")
                p.hwrite("</ul>\n")
                //メインコード
                p.hwrite(File.ReadAllText(dir_+"\\"+projectname+"_code.bee"))
                File.Delete(dir_+"\\"+projectname+"_code.bee")
                p.pclose()
                File.Delete(dir_+"\\"+projectname+"_par.bee")
                p.indentDec()
                p.hwrite("</div>\n")
                p.hwrite("</details>\n")
                p.hwrite("</div>\n")
                p.hclose()
                //呼び出しコードを記述
                let args = 
                    let cat acc i =
                        let n,(_,_,_) = p.arglist.[i]
                        match i with
                        |_ when (i<>p.arglist.Length-1) -> acc + n + ", "
                        |_ -> acc + n
                    List.fold cat "" [0..p.arglist.Length-1]
                //もとの関数に戻る
                p.param_back()
                p.codewrite("\\(" + projectname + "(" + args + ")\\)<br/>")
        ///<summary>コンパイル</summary>
        let Compile lglist dir projectname (aqver:string,codever:string) code =
            for lg in lglist do
                match lg with 
                |F -> 
                    p.clear()
                    for f in Directory.GetFiles(dir, "*.bee") do File.Delete(f) //残っている中間コードファイルを削除
                    str.clear()
                    p.param_add(F, dir, projectname)
                    p.paramClear()
                    //メインコード生成
                    code()
                    p.pclose()
                    p.cclose()
                    p.declareall()
                    p.vclose()
                    //ソースファイル出力
                    p.hwrite("!============================================================================================="+"\n")
                    p.hwrite("! Project name: "+projectname+"\n")
                    p.hwrite("! Project version: "+codever+"\n")
                    p.hwrite("!---------------------------------------------------------------------------------------------"+"\n")
                    p.hwrite("! Generated by Aqualis (algorithm and equation analyzer for lightwave simulation)\n")
                    p.hwrite("! Aqualis version: "+aqver+"\n")
                    p.hwrite("! Generated date: "+System.DateTime.Now.ToString()+"\n")
                    p.hwrite("!============================================================================================="+"\n")
                    p.hwrite("program "+projectname+"\n")
                    //モジュールファイルのインクルード
                    List.iter (fun (s:string) -> p.hwrite("use "+s+"\n")) <| p.mlist.list
                    p.hwrite("implicit none"+"\n")
                    //ヘッダファイルのインクルード
                    List.iter (fun (s:string) -> p.hwrite("include "+s+"\n")) <| p.hlist.list
                    //構造体の定義
                    str.Def_Structure()
                    //グローバル変数の定義
                    p.hwrite(File.ReadAllText(dir+"\\"+projectname+"_var"+".bee"))
                    //メインコード
                    p.hwrite(File.ReadAllText(dir+"\\"+projectname+"_code.bee"))
                    //サブルーチン
                    p.hwrite("\ncontains\n\n")
                    for funname in p.param_main.funlist_nonoverlap do
                        p.hwrite(File.ReadAllText(dir+"\\"+funname+".bee"))
                        File.Delete(dir+"\\"+funname+".bee")
                        p.hwrite("\n")
                    p.hwrite("end program "+projectname+"\n")
                    p.hclose()
                    //beeファイル削除
                    File.Delete(dir+"\\"+projectname+"_code.bee")
                    File.Delete(dir+"\\"+projectname+"_par.bee")
                    File.Delete(dir+"\\"+projectname+"_var"+".bee")
                    if File.Exists(dir+"\\"+"structure"+".bee") then File.Delete(dir+"\\"+"structure"+".bee")
                    
                    //コンパイル・実行用スクリプト生成
                    let wr = new StreamWriter(dir + "\\" + "proc_"+projectname+"_F.sh")
                    if p.isOaccUsed then
                        wr.Write("#!/bin/bash"+"\n")
                        wr.Write("\n")
                        let source = List.fold (fun acc ss -> acc+" "+ss) "" <| p.slist.list
                        let option = List.fold (fun acc op -> acc+" "+op) "" <| p.olist.list
                        wr.Write("pgfortran -acc -Minfo=accel"+source+" "+projectname+".f90"+option+" -o "+projectname+".exe"+"\n")
                        wr.Write("./"+projectname+".exe"+"\n")
                        wr.Close()
                    else if p.isOmpUsed then
                        wr.Write("#!/bin/bash"+"\n")
                        wr.Write("\n")
                        wr.Write("FC='/usr/bin/gfortran'"+"\n")
                        wr.Write("\n")
                        let source = List.fold (fun acc ss -> acc+" "+ss) "" <| p.slist.list
                        let option = List.fold (fun acc op -> acc+" "+op) "" <| p.olist.list
                        wr.Write("$FC"+" -fopenmp"+source+" "+projectname+".f90"+option+" -o "+projectname+".exe"+"\n")
                        wr.Write("./"+projectname+".exe"+"\n")
                    else
                        wr.Write("#!/bin/bash"+"\n")
                        wr.Write("\n")
                        wr.Write("FC='/usr/bin/gfortran'"+"\n")
                        wr.Write("\n")
                        let source = List.fold (fun acc ss -> acc+" "+ss) "" <| p.slist.list
                        let option = List.fold (fun acc op -> acc+" "+op) "" <| p.olist.list
                        wr.Write("$FC"+source+" "+projectname+".f90"+option+" -o "+projectname+".exe"+"\n")
                        wr.Write("./"+projectname+".exe"+"\n")
                    wr.Close()
                    //古いソースファイルを削除
                    if File.Exists(dir + "\\" + projectname+".f90") then File.Delete(dir + "\\" + projectname+".f90")
                    //新しいソースファイルを削除（beeファイルの拡張子を変更）
                    File.Move(dir + "\\" + projectname+".bee",dir + "\\" + projectname+".f90")
                |C ->
                    p.clear()
                    for f in Directory.GetFiles(dir, "*.bee") do File.Delete(f) //残っている中間コードファイルを削除
                    str.clear()
                    p.param_add(C, dir, projectname)
                    p.paramClear()
                    //メインコード生成
                    p.option("-lm")
                    p.indentInc()
                    code()
                    p.indentDec()
                    p.pclose()
                    p.cclose()
                    p.declareall()
                    p.vclose()
                    //ソースファイル出力
                    p.hwrite("/*=============================================================================================*/"+"\n")
                    p.hwrite("/* Project name: "+projectname+" */\n")
                    p.hwrite("/* Project version: "+codever+" */\n")
                    p.hwrite("/*---------------------------------------------------------------------------------------------*/"+"\n")
                    p.hwrite("/* Generated by Aqualis (algorithm and equation analyzer for lightwave simulation) */\n")
                    p.hwrite("/* Aqualis version: "+aqver+" */\n")
                    p.hwrite("/* Generated date: "+System.DateTime.Now.ToString()+" */\n")
                    p.hwrite("/*=============================================================================================*/"+"\n")
                    p.hwrite("#include <stdio.h>"+"\n")
                    p.hwrite("#include <stdlib.h>"+"\n")
                    p.hwrite("#include <complex.h>"+"\n")
                    p.hwrite("#include <math.h>"+"\n")
                    //ヘッダファイルのインクルード
                    List.iter (fun (s:string) -> p.hwrite("#include "+s+"\n")) <| p.hlist.list
                    p.hwrite("#undef I"+"\n")
                    p.hwrite("#define uj _Complex_I"+"\n")
                    //構造体の定義
                    str.Def_Structure()
                    //グローバル変数の宣言
                    p.hwrite(File.ReadAllText(dir+"\\"+projectname+"_var"+".bee"))
                    //extern指定子
                    for s in p.elist.list do
                        p.hwrite("extern "+s+";\n")
                    //関数定義
                    for funname in p.param_main.funlist_nonoverlap do
                        p.hwrite(File.ReadAllText(dir+"\\"+funname+".bee"))
                        File.Delete(dir+"\\"+funname+".bee")
                        p.hwrite("\n")
                    //Main関数
                    p.hwrite("int main()"+"\n")
                    p.hwrite("{"+"\n")
                    p.hwrite(File.ReadAllText(dir+"\\"+projectname+"_code.bee"))
                    p.hwrite("  return 0;"+"\n")
                    p.hwrite("}"+"\n")
                    p.hclose()
                    //beeファイル削除
                    File.Delete(dir+"\\"+projectname+"_code.bee")
                    File.Delete(dir+"\\"+projectname+"_par.bee")
                    File.Delete(dir+"\\"+projectname+"_var"+".bee")
                    if File.Exists(dir+"\\"+"structure"+".bee") then File.Delete(dir+"\\"+"structure"+".bee")
                    
                    //コンパイル・実行用スクリプト生成
                    let wr = new StreamWriter(dir + "\\" + "proc_"+projectname+"_C.sh")
                    if p.isOmpUsed then
                        wr.Write("#!/bin/sh"+"\n")
                        wr.Write("\n")
                        let source = List.fold (fun acc ss -> acc+" "+ss) "" <| p.slist.list
                        let option = List.fold (fun acc op -> acc+" "+op) "" <| p.olist.list
                        wr.Write("gcc"+" -fopenmp "+source+" "+projectname+".c"+option+" -o "+projectname+".exe"+"\n")
                        wr.Write("./"+projectname+".exe"+"\n")
                    else if p.isOaccUsed then
                        wr.Write("#!/bin/sh"+"\n")
                        wr.Write("\n")
                        let source = List.fold (fun acc ss -> acc+" "+ss) "" <| p.slist.list
                        let option = List.fold (fun acc op -> acc+" "+op) "" <| p.olist.list
                        wr.Write("pgcc -acc -Minfo=accel"+source+" "+projectname+".c"+option+" -o "+projectname+".exe"+"\n")
                        wr.Write("./"+projectname+".exe"+"\n")
                        wr.Close()
                    else
                        wr.Write("#!/bin/sh"+"\n")
                        wr.Write("\n")
                        let source = List.fold (fun acc ss -> acc+" "+ss) "" <| p.slist.list
                        let option = List.fold (fun acc op -> acc+" "+op) "" <| p.olist.list
                        wr.Write("gcc"+source+" "+projectname+".c"+option+" -o "+projectname+".exe"+"\n")
                        wr.Write("./"+projectname+".exe"+"\n")
                    wr.Close()
                    //古いソースファイルを削除
                    if File.Exists(dir + "\\" + projectname+".c") then File.Delete(dir + "\\" + projectname+".c")
                    //新しいソースファイルを削除（beeファイルの拡張子を変更）
                    File.Move(dir + "\\" + projectname+".bee",dir + "\\" + projectname+".c")
                |T ->
                    p.clear()
                    for f in Directory.GetFiles(dir, "*.bee") do File.Delete(f) //残っている中間コードファイルを削除
                    str.clear()
                    p.param_add(T, dir, projectname)
                    p.paramClear()
                    //メインコード生成
                    code()
                    p.pclose()
                    p.cclose()
                    p.declareall()
                    p.vclose()
                    //ソースファイル出力
                    p.hwrite("\\documentclass[a4paper,fleqn]{ltjsarticle}\n")
                    p.hwrite("\\usepackage{amsmath}\n")
                    List.iter (fun (s:string) -> p.hwrite(s+"\n")) <| p.hlist.list
                    p.hwrite("\\oddsidemargin=-0.4mm\n")
                    p.hwrite("\\topmargin=4.6mm\n")
                    p.hwrite("\\headheight=0mm\n")
                    p.hwrite("\\headsep=0mm\n")
                    p.hwrite("\\footskip=15mm\n")
                    p.hwrite("\\textwidth=160mm\n")
                    p.hwrite("\\textheight=237mm\n")
                    p.hwrite("\\topsep=6pt\n")
                    p.hwrite("\\parindent=0mm\n")
                    p.hwrite("\\unitlength=1.00mm\n")
                    p.hwrite("\\begin{document}\n")
                    p.hwrite("{\\Large "+projectname.Replace("_","\\_")+"}\n\n")
                    //構造体の定義
                    p.hwrite("\\section{structures}\n")
                    str.Def_Structure()
                    //関数定義
                    p.hwrite("\\section{subroutines}\n")
                    for funname in p.param_main.funlist_nonoverlap do
                        p.hwrite(File.ReadAllText(dir+"\\"+funname+".bee"))
                        File.Delete(dir+"\\"+funname+".bee")
                        p.hwrite("\n")
                    //グローバル変数の定義
                    p.hwrite("\\section{global variables}\n")
                    p.hwrite("\\begin{itemize}\n")
                    p.hwrite(File.ReadAllText(dir+"\\"+projectname+"_var"+".bee"))
                    p.hwrite("\\end{itemize}\n")
                    //メインコード
                    p.hwrite("\\section{main code}\n")
                    p.hwrite(File.ReadAllText(dir+"\\"+projectname+"_code.bee"))
                    p.hwrite("\\end{document}\n")
                    p.hclose()
                    //beeファイル削除
                    File.Delete(dir+"\\"+projectname+"_code.bee")
                    File.Delete(dir+"\\"+projectname+"_par.bee")
                    File.Delete(dir+"\\"+projectname+"_var"+".bee")
                    if File.Exists(dir+"\\"+"structure"+".bee") then File.Delete(dir+"\\"+"structure"+".bee")
                    //古いソースファイルを削除
                    if File.Exists(dir + "\\" + projectname+".tex") then File.Delete(dir + "\\" + projectname+".tex")
                    //新しいソースファイルを削除（beeファイルの拡張子を変更）
                    File.Move(dir + "\\" + projectname+".bee",dir + "\\" + projectname+".tex")
                |H ->
                    p.clear()
                    for f in Directory.GetFiles(dir, "*.bee") do File.Delete(f) //残っている中間コードファイルを削除
                    str.clear()
                    p.param_add(H, dir, projectname)
                    p.paramClear()
                    //メインコード生成
                    code()
                    p.pclose()
                    p.cclose()
                    p.declareall()
                    p.vclose()
                    //ソースファイル出力
                    p.hwrite("<!DOCTYPE html>")
                    p.hwrite("<html lang='ja'>")
                    p.hwrite("\t<head>")
                    p.hwrite("\t\t<meta charset='utf-8'>")
                    p.hwrite("\t\t<script>")
                    p.hwrite("\t\tMathJax = {")
                    p.hwrite("\t\t\tchtml: {")
                    p.hwrite("\t\t\t\tdisplayAlign: \"left\",")
                    p.hwrite("\t\t\t}")
                    p.hwrite("\t\t};")
                    p.hwrite("\t\t</script>")
                    p.hwrite("\t\t<script type=\"text/javascript\" id=\"MathJax-script\" async src=\"https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml.js\"></script>")
                    p.hwrite("\t\t<title>"+projectname+"</title>")
                    p.hwrite("\t\t<meta name='viewport' content='width=device-width, initial-scale=1.0, maximum-scale=1.0, minimum-scale=1.0'>")
                    p.hwrite("\t\t<link rel=\"preconnect\" href=\"https://fonts.googleapis.com\">")
                    p.hwrite("\t\t<link rel=\"preconnect\" href=\"https://fonts.gstatic.com\" crossorigin>")
                    p.hwrite("\t\t<link href=\"https://fonts.googleapis.com/css2?family=Noto+Sans+JP:wght@500;600;700&display=swap\" rel=\"stylesheet\">")
                    p.hwrite("\t\t<style type=\"text/css\">\n")
                    p.hwrite("\t\t<!--\n")
                    p.hwrite("\t\tbody {\n")
                    p.hwrite("\t\t\tfont-family: 'Noto Sans JP', sans-serif;")
                    p.hwrite("\t\t\tfont-weight: 500;")
                    p.hwrite("\t\t\tfont-size: 16px;")
                    p.hwrite("\t\t}\n")
                    p.hwrite("\t\th2 {\n")
                    p.hwrite("\t\t\tborder-bottom: 2px solid;\n")
                    p.hwrite("\t\t}\n")
                    p.hwrite("\t\ta {\n")
                    p.hwrite("\t\t\tfont-size: 11pt;\n")
                    p.hwrite("\t\t\ttext-decoration: none;\n")
                    p.hwrite("\t\t\tcolor: #8000ff;\n")
                    p.hwrite("\t\t}\n")
                    p.hwrite("\t\t.fio {")
                    p.hwrite("\t\t\tmargin-right: 10px;")
                    p.hwrite("\t\t\tfont-size: 11pt;")
                    p.hwrite("\t\t\tcolor: #ff00ff;")
                    p.hwrite("\t\t}")
                    p.hwrite("\t\t.continue {\n")
                    p.hwrite("\t\t\tfont-size: 11pt;\n")
                    p.hwrite("\t\t\tcolor: #8000ff;\n")
                    p.hwrite("\t\t}\n")
                    p.hwrite("\t\t.codeblock {\n")
                    p.hwrite("\t\t\tpadding-left: 0px;\n")
                    p.hwrite("\t\t}\n")
                    p.hwrite("\t\t\n")
                    p.hwrite("\t\t.op-loop {\n")
                    p.hwrite("\t\t\tfont-size: 11pt;\n")
                    p.hwrite("\t\t\tcolor: #ff7f00;\n")
                    p.hwrite("\t\t}\n")
                    p.hwrite("\t\t\n")
                    p.hwrite("\t\t.insidecode-loop {\n")
                    p.hwrite("\t\t\tmargin-left: 2px;\n")
                    p.hwrite("\t\t\tpadding-left: 30px;\n")
                    p.hwrite("\t\t\tborder-left: solid;\n")
                    p.hwrite("\t\t\tborder-width: 5px;\n")
                    p.hwrite("\t\t\tborder-color: #ffa347;\n")
                    p.hwrite("\t\t}\n")
                    p.hwrite("\t\t\n")
                    p.hwrite("\t\t.op-if {\n")
                    p.hwrite("\t\t\tfont-size: 11pt;\n")
                    p.hwrite("\t\t\tcolor: #007fff;\n")
                    p.hwrite("\t\t}\n")
                    p.hwrite("\t\t\n")
                    p.hwrite("\t\t.insidecode-if {\n")
                    p.hwrite("\t\t\tmargin-left: 2px;\n")
                    p.hwrite("\t\t\tpadding-left: 30px;\n")
                    p.hwrite("\t\t\tborder-left: solid;\n")
                    p.hwrite("\t\t\tborder-width: 5px;\n")
                    p.hwrite("\t\t\tborder-color: #47a3ff;\n")
                    p.hwrite("\t\t}\n")
                    p.hwrite("\t\t.op-func {\n")
                    p.hwrite("\t\t\tfont-size: 11pt;\n")
                    p.hwrite("\t\t\tcolor: #0000ff;\n")
                    p.hwrite("\t\t}\n")
                    p.hwrite("\t\t\n")
                    p.hwrite("\t\t.insidecode-func {\n")
                    p.hwrite("\t\t\tmargin-left: 2px;\n")
                    p.hwrite("\t\t\tpadding-left: 30px;\n")
                    p.hwrite("\t\t\tborder-left: solid;\n")
                    p.hwrite("\t\t\tborder-width: 5px;\n")
                    p.hwrite("\t\t\tborder-color: #0000ff;\n")
                    p.hwrite("\t\t}\n")
                    p.hwrite("\t\t.op-section {\n")
                    p.hwrite("\t\t\tfont-size: 11pt;\n")
                    p.hwrite("\t\t\tcolor: #008000;\n")
                    p.hwrite("\t\t\tmargin-right: 5px;\n")
                    p.hwrite("\t\t}\n")
                    p.hwrite("\t\t\n")
                    p.hwrite("\t\t.insidecode-section {\n")
                    p.hwrite("\t\t\tmargin-left: 2px;\n")
                    p.hwrite("\t\t\tpadding-left: 30px;\n")
                    p.hwrite("\t\t\tborder-left: solid;\n")
                    p.hwrite("\t\t\tborder-width: 5px;\n")
                    p.hwrite("\t\t\tborder-color: #32cd32;\n")
                    p.hwrite("\t\t}\n")
                    p.hwrite("\t\t\n")
                    p.hwrite("\t\t.comment {\n")
                    p.hwrite("\t\t\tfont-size: 11pt;\n")
                    p.hwrite("\t\t\tcolor: #008000;\n")
                    p.hwrite("\t\t}\n")
                    p.hwrite("\t\t-->\n")
                    p.hwrite("\t\t</style>\n")
                    p.hwrite("\t\t<script type=\"text/javascript\">\n")
                    p.hwrite("\t\t\tfunction fsearch()\n")
                    p.hwrite("\t\t\t{\n")
                    p.hwrite("\t\t\t\t//var s = document.styleSheets.item(0);\n")
                    p.hwrite("\t\t\t\t//s.cssRules[0].style.backgroundColor=\"blue\";\n")
                    p.hwrite("\t\t\t\tvar vname = document.getElementById(\"textvar\").value;\n")
                    p.hwrite("\t\t\t\tvar targets = document.getElementsByClassName(vname);\n")
                    p.hwrite("\t\t\t\tfor (i = 0; i < targets.length; i++)\n")
                    p.hwrite("\t\t\t\t{\n")
                    p.hwrite("\t\t\t\t\ttargets[i].style.color =  '#ff0000';\n")
                    p.hwrite("\t\t\t\t}\n")
                    p.hwrite("\t\t\t}\n")
                    p.hwrite("\t\t</script>\n")
                    p.hwrite("\t</head>")
                    p.hwrite("\t<body>")
                    p.hwrite("\t\t<h1>"+projectname+"</h1>\n")
                    p.hwrite("\t\t<div id=\"codeinfo\">\n")
                    p.hwrite("\t\t\t<ul>\n")
                    p.hwrite("\t\t\t\t<li>Project version: "+codever+"</li>\n")
                    p.hwrite("\t\t\t\t<li>Generated by Aqualis (algorithm and equation analyzer for lightwave simulation)</li>\n")
                    p.hwrite("\t\t\t\t<li>Aqualis version: "+aqver+"</li>\n")
                    p.hwrite("\t\t\t\t<li>Generated date: "+System.DateTime.Now.ToString()+"</li>\n")
                    p.hwrite("\t\t\t</ul>\n")
                    p.hwrite("\t\t</div>\n")
                    //構造体の定義
                    p.hwrite("\t\t<div id=\"defstr\">\n")
                    p.hwrite("\t\t<h2>構造体定義</h2>\n")
                    str.Def_Structure()
                    p.hwrite("\t\t</div>\n")
                    //関数定義
                    p.hwrite("\t\t<div id=\"deffunc\">\n")
                    p.hwrite("\t\t<h2>関数定義</h2>\n")
                    for funname in p.param_main.funlist_nonoverlap do
                        p.hwrite(File.ReadAllText(dir+"\\"+funname+".bee"))
                        File.Delete(dir+"\\"+funname+".bee")
                        p.hwrite("\n")
                    p.hwrite("\t\t</div>\n")
                    //グローバル変数の定義
                    p.hwrite("\t\t<div id=\"defvar\">\n")
                    p.hwrite("\t\t<h2>グローバル変数</h2>\n")
                    p.hwrite("\t\t<ul>\n")
                    p.hwrite(File.ReadAllText(dir+"\\"+projectname+"_var"+".bee"))
                    p.hwrite("\t\t</ul>\n")
                    p.hwrite("\t\t</div>\n")
                    //メインコード
                    p.hwrite("\t\t<div id=\"maincode\">\n")
                    p.hwrite("\t\t<h2>メインコード</h2>\n")
                    p.hwrite("<input type=\"text\" id=\"textvar\" value=\"\">\n")
                    p.hwrite("<input type=\"button\" onclick=\"fsearch()\" value=\"Search\">\n")
                    p.hwrite("<br/>\n")
                    p.hwrite(File.ReadAllText(dir+"\\"+projectname+"_code.bee"))
                    p.hwrite("\t\t</div>\n")
                    p.hwrite("\t</body>\n")
                    p.hwrite("</html>\n")
                    p.hclose()
                    //beeファイル削除
                    File.Delete(dir+"\\"+projectname+"_code.bee")
                    File.Delete(dir+"\\"+projectname+"_par.bee")
                    File.Delete(dir+"\\"+projectname+"_var"+".bee")
                    if File.Exists(dir+"\\"+"structure"+".bee") then File.Delete(dir+"\\"+"structure"+".bee")
                    //古いソースファイルを削除
                    if File.Exists(dir + "\\" + projectname+".html") then File.Delete(dir + "\\" + projectname+".html")
                    //新しいソースファイルを削除（beeファイルの拡張子を変更）
                    File.Move(dir + "\\" + projectname+".bee",dir + "\\" + projectname+".html")