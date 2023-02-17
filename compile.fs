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
    
    [<AutoOpen>]
    module Aqualis_main =
        
        ///<summary>コメント文を生成</summary>
        let (!) s = p.param.comment s
        
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
            let dir_ = p.param.dir
            let fdeclare (typ:Etype,vtp:VarType,name:string,param:string,comment:string) =
                match p.lang with
                  |H ->
                    match vtp with 
                      |A0 -> "<math><mi>"+typ.tostring()+"</mi> :: "+name+"</math>"
                      |A1(0) -> "<math><mi>"+typ.tostring()+"</mi>(allocatable,1d)"+" "+name+"</math>"
                      |A2(0,0) -> "<math><mi>"+typ.tostring()+"</mi>(allocatable,2d)"+" "+name+"</math>"
                      |A3(0,0,0) -> "<math><mi>"+typ.tostring()+"</mi>(allocatable,3d)"+" "+name+"</math>"
                      |A1(_) -> "<math><mi>"+typ.tostring()+"</mi>(1d) "+name+"</math>"
                      |A2(_,_) -> "<math><mi>"+typ.tostring()+"</mi>(2d) "+name+"</math>"
                      |A3(_,_,_) -> "<math><mi>"+typ.tostring()+"</mi>(3d) "+name+"</math>"
                  |_ ->
                    match vtp with 
                      |A0 -> typ.tostring()+" :: "+name
                      |A1(0) -> typ.tostring()+",allocatable"+" :: "+name+"(:)"
                      |A2(0,0) -> typ.tostring()+",allocatable"+" :: "+name+"(:,:)"
                      |A3(0,0,0) -> typ.tostring()+",allocatable"+" :: "+name+"(:,:,:)"
                      |A1(_) -> typ.tostring()+" :: "+name+"(:)"
                      |A2(_,_) -> typ.tostring()+" :: "+name+"(:,:)"
                      |A3(_,_,_) -> typ.tostring()+" :: "+name+"(:,:,:)"
            match p.lang with
              |F ->
                p.param_main.funlist_add(projectname)
                p.param_add(F, dir_, projectname)
                //ここから関数定義。p.paramは関数用のものに変わる
                let q = p.param
                q.clear()
                  //match p.lang with
                  //  |F   -> [(EType.CDB,"uj",VarType.A0,"(0d0,1d0)","虚数単位")
                  //           (EType.Dbl,"pi",VarType.A0,"3.14159265358979d0","円周率")]
                  //  |C89 -> [(EType.CDB,"uj",VarType.A0,"{0.0E0,1.0E0}","虚数単位")
                  //           (EType.Dbl,"pi",VarType.A0,"3.14159265358979","円周率")]
                  //  |C99 -> [(EType.Dbl,"pi",VarType.A0,"3.14159265358979","円周率")]
                  //  |P   -> [(EType.CDB,"uj",VarType.A0,"1j","虚数単位")
                  //           (EType.Dbl,"pi",VarType.A0,"np.pi","円周率")]
                  //  |T   -> [(EType.CDB,"uj",VarType.A0,"(0d0,1d0)","虚数単位")
                  //           (EType.Dbl,"pi",VarType.A0,"3.14159265358979","円周率")]
                //メインコード生成
                q.indentposition_inc()
                code()
                q.indentposition_dec()
                q.cclose()
                q.indentposition_inc()
                q.declareall()
                q.indentposition_dec()
                q.vclose()
                //ソースファイル(関数部分)出力
                q.hwrite("!============================================================================================="+"\n")
                q.hwrite("! Subroutine name: "+projectname+"\n")
                for _,(_,_,nm,_,cm) in q.arglist do
                    q.hwrite("!  "+nm+" "+cm+"\n")
                q.hwrite("!============================================================================================="+"\n")
                let argvar = 
                    List.fold (fun acc i -> 
                                let _,(_,_,n,_,_) = q.arglist.[i]
                                match i with
                                  |_ when (i<>q.arglist.Length-1) -> acc + n + ", "
                                  |_ -> acc + n) "" [0..q.arglist.Length-1]
                q.codefold("subroutine "+projectname+"("+argvar+")"+"\n","",q.hwrite,100)
                q.indentposition_inc()
                //モジュールファイルのインクルード
                List.iter (fun (s:string) -> q.hwrite("use "+s+"\n")) <| q.modl
                q.hwrite(q.indent + "implicit none"+"\n")
                //ヘッダファイルのインクルード
                List.iter (fun (s:string) -> q.hwrite(q.indent + "include "+s+"\n")) <| q.header
                //サブルーチン引数の定義
                for _,s in q.arglist do
                    q.hwrite(q.indent + fdeclare(s) + "\n")
                //グローバル変数の定義
                q.hwrite(File.ReadAllText(dir_+"\\"+projectname+"_var"+".bee"))
                File.Delete(dir_+"\\"+projectname+"_var"+".bee")
                //メインコード
                q.hwrite(File.ReadAllText(dir_+"\\"+projectname+"_code.bee"))
                File.Delete(dir_+"\\"+projectname+"_code.bee")
                q.indentposition_dec()
                q.hwrite("end subroutine "+projectname+"\n")
                q.hclose()
                //呼び出しコードを記述
                let args = 
                    List.fold (fun acc i -> 
                                let n,(_,_,_,_,_) = q.arglist.[i]
                                match i with
                                  |_ when (i<>q.arglist.Length-1) -> acc + n + ", "
                                  |_ -> acc + n) "" [0..q.arglist.Length-1]
                //もとの関数に戻る
                p.param_back()
                let q = p.param
                q.codewrite("call" + " " + projectname + "(" + args + ")\n")
              |C89 ->
                p.param_main.funlist_add(projectname)
                p.param_add(C89, dir_, projectname+"_C89")
                //ここから関数定義。p.paramは関数用のものに変わる
                let q = p.param
                q.clear()
                //メインコード生成
                q.indentposition_inc()
                code()
                q.indentposition_dec()
                q.cclose()
                q.indentposition_inc()
                q.declareall()
                q.indentposition_dec()
                q.vclose()
                //ソースファイル(関数部分)出力
                q.hwrite("/*==========================================================================================*/"+"\n")
                q.hwrite("/* Subroutine name: "+projectname+" */\n")
                for _,(_,_,nm,_,cm) in q.arglist do
                    q.hwrite("/* "+nm+" "+cm+" */\n")
                q.hwrite("/*==========================================================================================*/"+"\n")
                let argvar = 
                    List.fold (fun acc i -> 
                                let _,(typ,vtp,n,_,_) = q.arglist.[i]
                                let cm = if (i=q.arglist.Length-1) then "" else ", "
                                match vtp with
                                  |A1(_)|A2(_)|A3(_) -> 
                                    acc + typ.tostring() + " *" + n + cm
                                  |_ -> 
                                    acc + typ.tostring() + " *" + n + cm) "" [0..q.arglist.Length-1]
                q.hwrite("void "+projectname+"("+argvar+")"+"\n")
                q.hwrite("{\n")
                q.indentposition_inc()
                //グローバル変数の定義
                q.hwrite(File.ReadAllText(dir_+"\\"+projectname+"_C89"+"_var"+".bee"))
                File.Delete(dir_+"\\"+projectname+"_C89"+"_var"+".bee")
                //メインコード
                q.hwrite(File.ReadAllText(dir_+"\\"+projectname+"_C89"+"_code.bee"))
                File.Delete(dir_+"\\"+projectname+"_C89"+"_code.bee")
                q.indentposition_dec()
                q.hwrite("}\n")
                q.hclose()
                //呼び出しコードを記述
                let args = 
                    List.fold (fun acc i -> 
                                let n,(typ,vtp,_,_,_) = q.arglist.[i]
                                let cm = if (i=q.arglist.Length-1) then "" else ", "
                                match typ,vtp,(n.StartsWith "(*") with
                                  |(It _|Dt|Zt|Structure _),A0,false -> 
                                    acc + "&" + n + cm
                                  |(It _|Dt|Zt|Structure _),A0,true  -> 
                                    let n_ = n.Substring(2,n.Length-3) //(n.Split([|'*'|],StringSplitOptions.RemoveEmptyEntries)).[0]
                                    acc + n_ + cm
                                  |_ -> acc + n + cm
                               ) "" [0..q.arglist.Length-1]
                //もとの関数に戻る
                p.param_back()
                let q = p.param
                q.codewrite(projectname + "(" + args + ");\n")
              |C99 ->
                p.param_main.funlist_add(projectname)
                p.param_add(C99, dir_, projectname+"_C99")
                //ここから関数定義。p.paramは関数用のものに変わる
                let q = p.param
                q.clear()
                //メインコード生成
                q.indentposition_inc()
                code()
                q.indentposition_dec()
                q.cclose()
                q.indentposition_inc()
                q.declareall()
                q.indentposition_dec()
                q.vclose()
                //ソースファイル(関数部分)出力
                q.hwrite("/*==========================================================================================*/"+"\n")
                q.hwrite("/* Subroutine name: "+projectname+" */\n")
                for _,(_,_,nm,_,cm) in q.arglist do
                    q.hwrite("/* "+nm+" "+cm+" */\n")
                q.hwrite("/*==========================================================================================*/"+"\n")
                let argvar = 
                    List.fold (fun acc i -> 
                                let _,(typ,vtp,n,_,_) = q.arglist.[i]
                                let cm = if (i=q.arglist.Length-1) then "" else ", "
                                match vtp with
                                  |A1(_)|A2(_)|A3(_) -> 
                                    acc + typ.tostring() + " *" + n + cm
                                  |_ -> 
                                    acc + typ.tostring() + " *" + n + cm) "" [0..q.arglist.Length-1]
                q.hwrite("void "+projectname+"("+argvar+")"+"\n")
                q.hwrite("{\n")
                q.indentposition_inc()
                //グローバル変数の定義
                q.hwrite(File.ReadAllText(dir_+"\\"+projectname+"_C99"+"_var"+".bee"))
                File.Delete(dir_+"\\"+projectname+"_C99"+"_var"+".bee")
                //メインコード
                q.hwrite(File.ReadAllText(dir_+"\\"+projectname+"_C99"+"_code.bee"))
                File.Delete(dir_+"\\"+projectname+"_C99"+"_code.bee")
                q.indentposition_dec()
                q.hwrite("}\n")
                q.hclose()
                //呼び出しコードを記述
                let args = 
                    List.fold (fun acc i -> 
                                let n,(typ,vtp,_,_,_) = q.arglist.[i]
                                let cm = if (i=q.arglist.Length-1) then "" else ", "
                                match typ,vtp,(n.StartsWith "(*") with
                                  |(It _|Dt|Zt|Structure _),A0,false -> 
                                    acc + "&" + n + cm
                                  |(It _|Dt|Zt|Structure _),A0,true  -> 
                                    let n_ = n.Substring(2,n.Length-3) //(n.Split([|'*'|],StringSplitOptions.RemoveEmptyEntries)).[0]
                                    acc + n_ + cm
                                  |_ -> acc + n + cm
                               ) "" [0..q.arglist.Length-1]
                //もとの関数に戻る
                p.param_back()
                let q = p.param
                q.codewrite(projectname + "(" + args + ");\n")
              |T ->
                p.param_main.funlist_add(projectname)
                p.param_add(T, dir_, projectname)
                //ここから関数定義。p.paramは関数用のものに変わる
                let q = p.param
                q.clear()
                //メインコード生成
                q.indentposition_inc()
                code()
                q.indentposition_dec()
                q.cclose()
                q.indentposition_inc()
                q.declareall()
                q.indentposition_dec()
                q.vclose()
                //ソースファイル(関数部分)出力
                q.hwrite("%============================================================================================="+"\n")
                q.hwrite("% Subroutine name: "+projectname+"\n")
                for _,(_,_,nm,_,cm) in q.arglist do
                    q.hwrite("% "+nm+" "+cm+"\n")
                q.hwrite("%============================================================================================="+"\n")
                let argvar = 
                    List.fold (fun acc i -> 
                                let _,(_,_,n,_,_) = q.arglist.[i]
                                match i with
                                  |_ when (i<>q.arglist.Length-1) -> acc + n + ", "
                                  |_ -> acc + n) "" [0..q.arglist.Length-1]
                q.hwrite("subroutine "+projectname+"("+argvar+")"+"\n")
                q.indentposition_inc()
                //モジュールファイルのインクルード
                List.iter (fun (s:string) -> q.hwrite("use "+s+"\n")) <| q.modl
                q.hwrite(q.indent + "implicit none"+"\n")
                //ヘッダファイルのインクルード
                List.iter (fun (s:string) -> q.hwrite(q.indent + "include "+s+"\n")) <| q.header
                //サブルーチン引数の定義
                for _,s in q.arglist do
                    q.hwrite(q.indent + fdeclare(s) + "\n")
                //グローバル変数の定義
                q.hwrite(File.ReadAllText(dir_+"\\"+projectname+"_var"+".bee"))
                File.Delete(dir_+"\\"+projectname+"_var.bee")
                //メインコード
                q.hwrite(File.ReadAllText(dir_+"\\"+projectname+"_code.bee"))
                File.Delete(dir_+"\\"+projectname+"_code.bee")
                q.indentposition_dec()
                q.hwrite("end subroutine "+projectname+"\n\n")
                q.hclose()
                //呼び出しコードを記述
                let args = 
                    List.fold (fun acc i -> 
                                let n,(_,_,_,_,_) = q.arglist.[i]
                                match i with
                                  |_ when (i<>q.arglist.Length-1) -> acc + n + ", "
                                  |_ -> acc + n) "" [0..q.arglist.Length-1]
                //もとの関数に戻る
                p.param_back()
                let q = p.param
                q.codewrite("call" + " " + projectname + "(" + args + ")\n")
              |H ->
                p.param_main.funlist_add(projectname)
                p.param_add(H, dir_, projectname)
                //ここから関数定義。p.paramは関数用のものに変わる
                let q = p.param
                q.clear()
                  //match p.lang with
                  //  |F   -> [(EType.CDB,"uj",VarType.A0,"(0d0,1d0)","虚数単位")
                  //           (EType.Dbl,"pi",VarType.A0,"3.14159265358979d0","円周率")]
                  //  |C89 -> [(EType.CDB,"uj",VarType.A0,"{0.0E0,1.0E0}","虚数単位")
                  //           (EType.Dbl,"pi",VarType.A0,"3.14159265358979","円周率")]
                  //  |C99 -> [(EType.Dbl,"pi",VarType.A0,"3.14159265358979","円周率")]
                  //  |P   -> [(EType.CDB,"uj",VarType.A0,"1j","虚数単位")
                  //           (EType.Dbl,"pi",VarType.A0,"np.pi","円周率")]
                  //  |T   -> [(EType.CDB,"uj",VarType.A0,"(0d0,1d0)","虚数単位")
                  //           (EType.Dbl,"pi",VarType.A0,"3.14159265358979","円周率")]
                //メインコード生成
                q.indentposition_inc()
                code()
                q.indentposition_dec()
                q.cclose()
                q.indentposition_inc()
                q.declareall()
                q.indentposition_dec()
                q.vclose()
                //ソースファイル(関数部分)出力
                q.hwrite("<h3>"+projectname+"</h3>\n")
                q.hwrite("<ul>"+"\n")
                for _,(_,_,nm,_,cm) in q.arglist do
                    q.hwrite("<li><math>"+nm+"</math> "+cm+"</li>\n")
                q.hwrite("</ul>"+"\n")
                let argvar = 
                    List.fold (fun acc i -> 
                                let _,(_,_,n,_,_) = q.arglist.[i]
                                match i with
                                  |_ when (i<>q.arglist.Length-1) -> acc + n + ", "
                                  |_ -> acc + n) "" [0..q.arglist.Length-1]
                q.hwrite("<div class=\"codeblock\">\n")
                q.hwrite("<details>\n")
                q.hwrite("<summary><span class=\"op-func\">function</span> <math><mi>"+projectname+"</mi><mo>(</mo>"+argvar+"<mo>)</mo></math></summary>"+"\n")
                q.hwrite("<div class=\"insidecode-func\">\n")
                q.indentposition_inc()
                q.hwrite("<ul>\n")
                //サブルーチン引数の定義
                for _,s in q.arglist do
                    q.hwrite(q.indent + "<li>" + fdeclare(s) + "</li>" + "\n")
                //グローバル変数の定義
                q.hwrite(File.ReadAllText(dir_+"\\"+projectname+"_var"+".bee"))
                File.Delete(dir_+"\\"+projectname+"_var"+".bee")
                q.hwrite("</ul>\n")
                //メインコード
                q.hwrite(File.ReadAllText(dir_+"\\"+projectname+"_code.bee"))
                File.Delete(dir_+"\\"+projectname+"_code.bee")
                q.indentposition_dec()
                q.hwrite("</div>\n")
                q.hwrite("</details>\n")
                q.hwrite("</div>\n")
                q.hclose()
                //呼び出しコードを記述
                let args = 
                    List.fold (fun acc i -> 
                                let n,(_,_,_,_,_) = q.arglist.[i]
                                match i with
                                  |_ when (i<>q.arglist.Length-1) -> acc + n + ", "
                                  |_ -> acc + n) "" [0..q.arglist.Length-1]
                //もとの関数に戻る
                p.param_back()
                let q = p.param
                q.codewrite("<math>" + "<mi>" + projectname + "</mi><mo>(</mo>" + args + "<mo>)</mo></math>\n<br/>\n")
              |NL ->
                ()
        ///<summary>コンパイル</summary>
        let Compile lglist dir projectname (aqver:string,codever:string) code =
            for lg in lglist do
                match lg with 
                  |F -> 
                    p.clear()
                    for f in Directory.GetFiles(dir, "*.bee") do File.Delete(f) //残っている中間コードファイルを削除
                    str.clear()
                    p.param_add(F, dir, projectname)
                    let q = p.param
                    q.clear()
                    //メインコード生成
                    code()
                    q.pclose()
                    q.cclose()
                    q.declareall()
                    q.vclose()
                    //ソースファイル出力
                    q.hwrite("!============================================================================================="+"\n")
                    q.hwrite("! Project name: "+projectname+"\n")
                    q.hwrite("! Project version: "+codever+"\n")
                    q.hwrite("!---------------------------------------------------------------------------------------------"+"\n")
                    q.hwrite("! Generated by Aqualis (algorithm and equation analyzer for lightwave simulation)\n")
                    q.hwrite("! Aqualis version: "+aqver+"\n")
                    q.hwrite("! Generated date: "+System.DateTime.Now.ToString()+"\n")
                    q.hwrite("!============================================================================================="+"\n")
                    q.hwrite("program "+projectname+"\n")
                    //モジュールファイルのインクルード
                    List.iter (fun (s:string) -> q.hwrite("use "+s+"\n")) <| q.modl
                    q.hwrite("implicit none"+"\n")
                    //ヘッダファイルのインクルード
                    List.iter (fun (s:string) -> q.hwrite("include "+s+"\n")) <| q.header
                    //構造体の定義
                    str.Def_Structure()
                    //グローバル変数の定義
                    q.hwrite(File.ReadAllText(dir+"\\"+projectname+"_var"+".bee"))
                    //メインコード
                    q.hwrite(File.ReadAllText(dir+"\\"+projectname+"_code.bee"))
                    //サブルーチン
                    q.hwrite("\ncontains\n\n")
                    for funname in p.param_main.funlist_nonoverlap do
                        q.hwrite(File.ReadAllText(dir+"\\"+funname+".bee"))
                        File.Delete(dir+"\\"+funname+".bee")
                        q.hwrite("\n")
                    q.hwrite("end program "+projectname+"\n")
                    q.hclose()
                    //beeファイル削除
                    File.Delete(dir+"\\"+projectname+"_code.bee")
                    File.Delete(dir+"\\"+projectname+"_par.bee")
                    File.Delete(dir+"\\"+projectname+"_var"+".bee")
                    if File.Exists(dir+"\\"+"structure"+".bee") then File.Delete(dir+"\\"+"structure"+".bee")
                    
                    //コンパイル・実行用スクリプト生成
                    let wr = new StreamWriter(dir + "\\" + "proc_"+projectname+".sh")
                    if p.param.isOaccUsed then
                        wr.Write("#!/bin/bash"+"\n")
                        wr.Write("\n")
                        let source = List.fold (fun acc ss -> acc+" "+ss) "" <| q.slist
                        let option = List.fold (fun acc op -> acc+" "+op) "" <| q.olist
                        wr.Write("pgfortran -acc -Minfo=accel"+source+" "+projectname+".f90"+option+" -o "+projectname+".exe"+"\n")
                        wr.Write("./"+projectname+".exe"+"\n")
                        wr.Close()
                    else if p.param.isOmpUsed then
                        wr.Write("#!/bin/bash"+"\n")
                        wr.Write("\n")
                        wr.Write("FC='/usr/bin/gfortran'"+"\n")
                        wr.Write("\n")
                        let source = List.fold (fun acc ss -> acc+" "+ss) "" <| q.slist
                        let option = List.fold (fun acc op -> acc+" "+op) "" <| q.olist
                        wr.Write("$FC"+" -fopenmp"+source+" "+projectname+".f90"+option+" -o "+projectname+".exe"+"\n")
                        wr.Write("./"+projectname+".exe"+"\n")
                    else
                        wr.Write("#!/bin/bash"+"\n")
                        wr.Write("\n")
                        wr.Write("FC='/usr/bin/gfortran'"+"\n")
                        wr.Write("\n")
                        let source = List.fold (fun acc ss -> acc+" "+ss) "" <| q.slist
                        let option = List.fold (fun acc op -> acc+" "+op) "" <| q.olist
                        wr.Write("$FC"+source+" "+projectname+".f90"+option+" -o "+projectname+".exe"+"\n")
                        wr.Write("./"+projectname+".exe"+"\n")
                    wr.Close()
                    //必要となる他のソースファイルをコピー
                    q.slist |> List.iter (fun s -> 
                                            if File.Exists(q.sourcedir+"\\"+s)=false then
                                                warning("ファイル「"+q.sourcedir+"\\"+s+"」が存在しません")
                                            else
                                                File.Copy(q.sourcedir+"\\"+s, dir+"\\"+s, true)) 
                    //古いソースファイルを削除
                    if File.Exists(dir + "\\" + projectname+".f90") then File.Delete(dir + "\\" + projectname+".f90")
                    //新しいソースファイルを削除（beeファイルの拡張子を変更）
                    File.Move(dir + "\\" + projectname+".bee",dir + "\\" + projectname+".f90")
                  |C89 ->
                    p.clear()
                    for f in Directory.GetFiles(dir, "*.bee") do File.Delete(f) //残っている中間コードファイルを削除
                    str.clear()
                    p.param_add(C89, dir, projectname+"_C89")
                    let q = p.param
                    q.clear()
                    //メインコード生成
                    p.param.option_("-lm")
                    code()
                    q.pclose()
                    q.cclose()
                    q.declareall()
                    q.vclose()
                    //ソースファイル出力
                    q.hwrite("/*=============================================================================================*/"+"\n")
                    q.hwrite("/* Project name: "+projectname+" */\n")
                    q.hwrite("/* Project version: "+codever+" */\n")
                    q.hwrite("/*---------------------------------------------------------------------------------------------*/"+"\n")
                    q.hwrite("/* Generated by Aqualis (algorithm and equation analyzer for lightwave simulation) */\n")
                    q.hwrite("/* Aqualis version: "+aqver+" */\n")
                    q.hwrite("/* Generated date: "+System.DateTime.Now.ToString()+" */\n")
                    q.hwrite("/*=============================================================================================*/"+"\n")
                    q.hwrite("#include <stdio.h>"+"\n")
                    q.hwrite("#include <stdlib.h>"+"\n")
                    q.hwrite("#include <math.h>"+"\n")
                    q.hwrite("#include <f2c.h>"+"\n")
                    //ヘッダファイルのインクルード
                    List.iter (fun (s:string) -> q.hwrite("#include "+s+"\n")) <| q.header
                    //構造体の定義
                    str.Def_Structure()
                    //extern指定子
                    for s in q.extn do
                        q.hwrite("extern "+s+";\n")
                    //関数定義
                    for funname in p.param_main.funlist_nonoverlap do
                        q.hwrite(File.ReadAllText(dir+"\\"+funname+"_C89"+".bee"))
                        File.Delete(dir+"\\"+funname+"_C89"+".bee")
                        q.hwrite("\n")
                    //Main関数
                    q.hwrite("int main()"+"\n")
                    q.hwrite("{"+"\n")
                    //グローバル変数の宣言
                    q.hwrite(File.ReadAllText(dir+"\\"+projectname+"_C89"+"_var"+".bee"))
                    q.hwrite(File.ReadAllText(dir+"\\"+projectname+"_C89"+"_code.bee"))
                    q.hwrite("return 0;"+"\n")
                    q.hwrite("}"+"\n")
                    q.hclose()
                    //beeファイル削除
                    File.Delete(dir+"\\"+projectname+"_C89"+"_code.bee")
                    File.Delete(dir+"\\"+projectname+"_C89"+"_par.bee")
                    File.Delete(dir+"\\"+projectname+"_C89"+"_var"+".bee")
                    if File.Exists(dir+"\\"+"structure"+".bee") then File.Delete(dir+"\\"+"structure"+".bee")
                    //コンパイル・実行用スクリプト生成
                    //q.source_("f2c.h")
                    let wr = new StreamWriter(dir + "\\" + "proc_"+projectname+"_C89.sh")
                    wr.Write("#!/bin/sh"+"\n")
                    wr.Write("\n")
                    let source = List.fold (fun acc ss -> acc+" "+ss) "" <| q.slist
                    let option = List.fold (fun acc op -> acc+" "+op) "" <| q.olist
                    wr.Write("gcc"+source+" "+projectname+"_C89.c"+option+" -o "+projectname+".exe"+"\n")
                    wr.Write("./"+projectname+".exe"+"\n")
                    wr.Close()
                    //必要となる他のソースファイルをコピー
                    q.slist |> List.iter (fun s -> 
                                            if File.Exists(q.sourcedir+"\\"+s)=false then
                                                warning("ファイル「"+q.sourcedir+"\\"+s+"」が存在しません")
                                            else
                                                File.Copy(q.sourcedir+"\\"+s, dir+"\\"+s, true)) 
                    //古いソースファイルを削除
                    if File.Exists(dir + "\\" + projectname+"_C89"+".c") then File.Delete(dir + "\\" + projectname+"_C89"+".c")
                    //新しいソースファイルを削除（beeファイルの拡張子を変更）
                    File.Move(dir + "\\" + projectname+"_C89"+".bee",dir + "\\" + projectname+"_C89"+".c")
                  |C99 ->
                    p.clear()
                    for f in Directory.GetFiles(dir, "*.bee") do File.Delete(f) //残っている中間コードファイルを削除
                    str.clear()
                    p.param_add(C99, dir, projectname+"_C99")
                    let q = p.param
                    q.clear()
                    //メインコード生成
                    p.param.option_("-lm")
                    code()
                    q.pclose()
                    q.cclose()
                    q.declareall()
                    q.vclose()
                    //ソースファイル出力
                    q.hwrite("/*=============================================================================================*/"+"\n")
                    q.hwrite("/* Project name: "+projectname+" */\n")
                    q.hwrite("/* Project version: "+codever+" */\n")
                    q.hwrite("/*---------------------------------------------------------------------------------------------*/"+"\n")
                    q.hwrite("/* Generated by Aqualis (algorithm and equation analyzer for lightwave simulation) */\n")
                    q.hwrite("/* Aqualis version: "+aqver+" */\n")
                    q.hwrite("/* Generated date: "+System.DateTime.Now.ToString()+" */\n")
                    q.hwrite("/*=============================================================================================*/"+"\n")
                    q.hwrite("#include <stdio.h>"+"\n")
                    q.hwrite("#include <stdlib.h>"+"\n")
                    q.hwrite("#include <complex.h>"+"\n")
                    q.hwrite("#include <math.h>"+"\n")
                    //ヘッダファイルのインクルード
                    List.iter (fun (s:string) -> q.hwrite("#include "+s+"\n")) <| q.header
                    q.hwrite("#undef I"+"\n")
                    q.hwrite("#define uj _Complex_I"+"\n")
                    //構造体の定義
                    str.Def_Structure()
                    //グローバル変数の宣言
                    q.hwrite(File.ReadAllText(dir+"\\"+projectname+"_C99"+"_var"+".bee"))
                    //extern指定子
                    for s in q.extn do
                        q.hwrite("extern "+s+";\n")
                    //関数定義
                    for funname in p.param_main.funlist_nonoverlap do
                        q.hwrite(File.ReadAllText(dir+"\\"+funname+"_C99"+".bee"))
                        File.Delete(dir+"\\"+funname+"_C99"+".bee")
                        q.hwrite("\n")
                    //Main関数
                    q.hwrite("int main()"+"\n")
                    q.hwrite("{"+"\n")
                    q.hwrite(File.ReadAllText(dir+"\\"+projectname+"_C99"+"_code.bee"))
                    q.hwrite("return 0;"+"\n")
                    q.hwrite("}"+"\n")
                    q.hclose()
                    //beeファイル削除
                    File.Delete(dir+"\\"+projectname+"_C99"+"_code.bee")
                    File.Delete(dir+"\\"+projectname+"_C99"+"_par.bee")
                    File.Delete(dir+"\\"+projectname+"_C99"+"_var"+".bee")
                    if File.Exists(dir+"\\"+"structure"+".bee") then File.Delete(dir+"\\"+"structure"+".bee")
                    
                    //コンパイル・実行用スクリプト生成
                    let wr = new StreamWriter(dir + "\\" + "proc_"+projectname+"_C99.sh")
                    if p.param.isOmpUsed then
                        wr.Write("#!/bin/sh"+"\n")
                        wr.Write("\n")
                        let source = List.fold (fun acc ss -> acc+" "+ss) "" <| q.slist
                        let option = List.fold (fun acc op -> acc+" "+op) "" <| q.olist
                        wr.Write("gcc"+" -fopenmp "+source+" "+projectname+"_C99.c"+option+" -o "+projectname+".exe"+"\n")
                        wr.Write("./"+projectname+".exe"+"\n")
                    else if p.param.isOaccUsed then
                        wr.Write("#!/bin/sh"+"\n")
                        wr.Write("\n")
                        let source = List.fold (fun acc ss -> acc+" "+ss) "" <| q.slist
                        let option = List.fold (fun acc op -> acc+" "+op) "" <| q.olist
                        wr.Write("pgcc -acc -Minfo=accel"+source+" "+projectname+"_C99.c"+option+" -o "+projectname+".exe"+"\n")
                        wr.Write("./"+projectname+".exe"+"\n")
                        wr.Close()
                    else
                        wr.Write("#!/bin/sh"+"\n")
                        wr.Write("\n")
                        let source = List.fold (fun acc ss -> acc+" "+ss) "" <| q.slist
                        let option = List.fold (fun acc op -> acc+" "+op) "" <| q.olist
                        wr.Write("gcc"+source+" "+projectname+"_C99.c"+option+" -o "+projectname+".exe"+"\n")
                        wr.Write("./"+projectname+".exe"+"\n")
                    wr.Close()
                    //必要となる他のソースファイルをコピー
                    q.slist |> List.iter (fun s -> 
                                            if File.Exists(q.sourcedir+"\\"+s)=false then
                                                warning("ファイル「"+q.sourcedir+"\\"+s+"」が存在しません")
                                            else
                                                File.Copy(q.sourcedir+"\\"+s, dir+"\\"+s, true)) 
                    //古いソースファイルを削除
                    if File.Exists(dir + "\\" + projectname+"_C99"+".c") then File.Delete(dir + "\\" + projectname+"_C99"+".c")
                    //新しいソースファイルを削除（beeファイルの拡張子を変更）
                    File.Move(dir + "\\" + projectname+"_C99"+".bee",dir + "\\" + projectname+"_C99"+".c")
                  |T ->
                    p.clear()
                    for f in Directory.GetFiles(dir, "*.bee") do File.Delete(f) //残っている中間コードファイルを削除
                    str.clear()
                    p.param_add(T, dir, projectname)
                    let q = p.param
                    q.clear()
                    //メインコード生成
                    code()
                    q.pclose()
                    q.cclose()
                    q.declareall()
                    q.vclose()
                    //ソースファイル出力
                    q.hwrite("\\documentclass[A4paper,9pt,fleqn,landscape]{jarticle}\n")
                    q.hwrite("\n")
                    q.hwrite("%\\usepackage{sugisaka_mystyle}\n")
                    q.hwrite("\\usepackage{amsmath}\n")
                    q.hwrite("\\usepackage{calc}\n")
                    q.hwrite("\n")
                    q.hwrite("\\setlength{\\topmargin}{-1in+5mm}\n")
                    q.hwrite("\\setlength{\\textheight}{260mm}\n")
                    q.hwrite("\\setlength{\\textwidth}{180mm}\n")
                    q.hwrite("\\setlength{\\oddsidemargin}{-1in+15mm}\n")
                    q.hwrite("\\setlength{\\evensidemargin}{-1in+15mm}\n")
                    q.hwrite("\n")
                    q.hwrite("\\begin{document}\n")
                    q.hwrite("{\\Large Program: "+projectname.Replace("_","\\_")+"}\n\n")
                    //構造体の定義
                    q.hwrite("\\section{structures}\n")
                    str.Def_Structure()
                    //関数定義
                    q.hwrite("\\section{subroutines}\n")
                    for funname in p.param_main.funlist_nonoverlap do
                        q.hwrite(File.ReadAllText(dir+"\\"+funname+".bee"))
                        File.Delete(dir+"\\"+funname+".bee")
                        q.hwrite("\n")
                    //グローバル変数の定義
                    q.hwrite("\\section{global variables}\n")
                    q.hwrite("\\begin{itemize}\n")
                    q.hwrite(File.ReadAllText(dir+"\\"+projectname+"_var"+".bee"))
                    q.hwrite("\\end{itemize}\n")
                    //メインコード
                    q.hwrite("\\section{main code}\n")
                    q.hwrite(File.ReadAllText(dir+"\\"+projectname+"_code.bee"))
                    q.hwrite("\\end{document}\n")
                    q.hclose()
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
                    let q = p.param
                    q.clear()
                    //メインコード生成
                    code()
                    q.pclose()
                    q.cclose()
                    q.declareall()
                    q.vclose()
                    //ソースファイル出力
                    q.hwrite("<!DOCTYPE html>\n")
                    q.hwrite("<html>\n")
                    q.hwrite("\t<head>\n")
                    q.hwrite("\t\t<meta charset=\"UTF-8\">\n")
                    q.hwrite("\t\t<title>"+projectname+"</title>\n")
                    q.hwrite("\t\t<style type=\"text/css\">\n")
                    q.hwrite("\t\t<!--\n")
                    
                    q.hwrite("\t\tbody {\n")
                    q.hwrite("\t\t\tfont-family: \"源ノ角ゴシック Code JP\",\"原ノ味角ゴシック\",\"游ゴシック\";\n")
                    q.hwrite("\t\t}\n")
                    q.hwrite("\t\th2 {\n")
                    q.hwrite("\t\t\tborder-bottom: 2px solid;\n")
                    q.hwrite("\t\t}\n")
                    
                    q.hwrite("\t\ta {\n")
                    q.hwrite("\t\t\tfont-size: 11pt;\n")
                    q.hwrite("\t\t\ttext-decoration: none;\n")
                    q.hwrite("\t\t\tcolor: #8000ff;\n")
                    q.hwrite("\t\t}\n")
                    
                    q.hwrite("\t\t.fio {")
                    q.hwrite("\t\t\tmargin-right: 10px;")
                    q.hwrite("\t\t\tfont-size: 11pt;")
                    q.hwrite("\t\t\tcolor: #ff00ff;")
                    q.hwrite("\t\t}")
                    
                    q.hwrite("\t\t.continue {\n")
                    q.hwrite("\t\t\tfont-size: 11pt;\n")
                    q.hwrite("\t\t\tcolor: #8000ff;\n")
                    q.hwrite("\t\t}\n")
                    
                    q.hwrite("\t\t.codeblock {\n")
                    q.hwrite("\t\t\tpadding-left: 0px;\n")
                    q.hwrite("\t\t}\n")
                    q.hwrite("\t\t\n")
                    
                    q.hwrite("\t\t.op-loop {\n")
                    q.hwrite("\t\t\tfont-size: 11pt;\n")
                    q.hwrite("\t\t\tcolor: #ff7f00;\n")
                    q.hwrite("\t\t}\n")
                    q.hwrite("\t\t\n")
                    
                    q.hwrite("\t\t.insidecode-loop {\n")
                    q.hwrite("\t\t\tmargin-left: 2px;\n")
                    q.hwrite("\t\t\tpadding-left: 30px;\n")
                    q.hwrite("\t\t\tborder-left: solid;\n")
                    q.hwrite("\t\t\tborder-width: 5px;\n")
                    q.hwrite("\t\t\tborder-color: #ffa347;\n")
                    q.hwrite("\t\t}\n")
                    q.hwrite("\t\t\n")
                    
                    q.hwrite("\t\t.op-if {\n")
                    q.hwrite("\t\t\tfont-size: 11pt;\n")
                    q.hwrite("\t\t\tcolor: #007fff;\n")
                    q.hwrite("\t\t}\n")
                    q.hwrite("\t\t\n")
                    
                    q.hwrite("\t\t.insidecode-if {\n")
                    q.hwrite("\t\t\tmargin-left: 2px;\n")
                    q.hwrite("\t\t\tpadding-left: 30px;\n")
                    q.hwrite("\t\t\tborder-left: solid;\n")
                    q.hwrite("\t\t\tborder-width: 5px;\n")
                    q.hwrite("\t\t\tborder-color: #47a3ff;\n")
                    q.hwrite("\t\t}\n")
                    
                    q.hwrite("\t\t.op-func {\n")
                    q.hwrite("\t\t\tfont-size: 11pt;\n")
                    q.hwrite("\t\t\tcolor: #0000ff;\n")
                    q.hwrite("\t\t}\n")
                    q.hwrite("\t\t\n")
                    
                    q.hwrite("\t\t.insidecode-func {\n")
                    q.hwrite("\t\t\tmargin-left: 2px;\n")
                    q.hwrite("\t\t\tpadding-left: 30px;\n")
                    q.hwrite("\t\t\tborder-left: solid;\n")
                    q.hwrite("\t\t\tborder-width: 5px;\n")
                    q.hwrite("\t\t\tborder-color: #0000ff;\n")
                    q.hwrite("\t\t}\n")
                    
                    q.hwrite("\t\t.op-section {\n")
                    q.hwrite("\t\t\tfont-size: 11pt;\n")
                    q.hwrite("\t\t\tcolor: #008000;\n")
                    q.hwrite("\t\t\tmargin-right: 5px;\n")
                    q.hwrite("\t\t}\n")
                    q.hwrite("\t\t\n")

                    q.hwrite("\t\t.insidecode-section {\n")
                    q.hwrite("\t\t\tmargin-left: 2px;\n")
                    q.hwrite("\t\t\tpadding-left: 30px;\n")
                    q.hwrite("\t\t\tborder-left: solid;\n")
                    q.hwrite("\t\t\tborder-width: 5px;\n")
                    q.hwrite("\t\t\tborder-color: #32cd32;\n")
                    q.hwrite("\t\t}\n")
                    q.hwrite("\t\t\n")

                    q.hwrite("\t\t.comment {\n")
                    q.hwrite("\t\t\tfont-size: 11pt;\n")
                    q.hwrite("\t\t\tcolor: #008000;\n")
                    q.hwrite("\t\t}\n")
                    
                    q.hwrite("\t\t-->\n")
                    q.hwrite("\t\t</style>\n")
                    q.hwrite("\t\t<script type=\"text/javascript\">\n")
                    q.hwrite("\t\t\tfunction fsearch()\n")
                    q.hwrite("\t\t\t{\n")
                    q.hwrite("\t\t\t\t//var s = document.styleSheets.item(0);\n")
                    q.hwrite("\t\t\t\t//s.cssRules[0].style.backgroundColor=\"blue\";\n")
                    q.hwrite("\t\t\t\tvar vname = document.getElementById(\"textvar\").value;\n")
                    q.hwrite("\t\t\t\tvar targets = document.getElementsByClassName(vname);\n")
                    q.hwrite("\t\t\t\tfor (i = 0; i < targets.length; i++)\n")
                    q.hwrite("\t\t\t\t{\n")
                    q.hwrite("\t\t\t\t\ttargets[i].style.color =  '#ff0000';\n")
                    q.hwrite("\t\t\t\t}\n")
                    q.hwrite("\t\t\t}\n")
                    q.hwrite("\t\t</script>\n")
                    q.hwrite("\t</head>\n")
                    q.hwrite("\t<body>\n")
                    q.hwrite("\t\t<h1>"+projectname+"</h1>\n")
                    q.hwrite("\t\t<div id=\"codeinfo\">\n")
                    q.hwrite("\t\t\t<ul>\n")
                    q.hwrite("\t\t\t\t<li>Project version: "+codever+"</li>\n")
                    q.hwrite("\t\t\t\t<li>Generated by Aqualis (algorithm and equation analyzer for lightwave simulation)</li>\n")
                    q.hwrite("\t\t\t\t<li>Aqualis version: "+aqver+"</li>\n")
                    q.hwrite("\t\t\t\t<li>Generated date: "+System.DateTime.Now.ToString()+"</li>\n")
                    q.hwrite("\t\t\t</ul>\n")
                    q.hwrite("\t\t</div>\n")
                    //構造体の定義
                    q.hwrite("\t\t<div id=\"defstr\">\n")
                    q.hwrite("\t\t<h2>構造体定義</h2>\n")
                    str.Def_Structure()
                    q.hwrite("\t\t</div>\n")
                    //関数定義
                    q.hwrite("\t\t<div id=\"deffunc\">\n")
                    q.hwrite("\t\t<h2>関数定義</h2>\n")
                    for funname in p.param_main.funlist_nonoverlap do
                        q.hwrite(File.ReadAllText(dir+"\\"+funname+".bee"))
                        File.Delete(dir+"\\"+funname+".bee")
                        q.hwrite("\n")
                    q.hwrite("\t\t</div>\n")
                    //グローバル変数の定義
                    q.hwrite("\t\t<div id=\"defvar\">\n")
                    q.hwrite("\t\t<h2>グローバル変数</h2>\n")
                    q.hwrite("\t\t<ul>\n")
                    q.hwrite(File.ReadAllText(dir+"\\"+projectname+"_var"+".bee"))
                    q.hwrite("\t\t</ul>\n")
                    q.hwrite("\t\t</div>\n")
                    //メインコード
                    q.hwrite("\t\t<div id=\"maincode\">\n")
                    q.hwrite("\t\t<h2>メインコード</h2>\n")
                    q.hwrite("<input type=\"text\" id=\"textvar\" value=\"\">\n")
                    q.hwrite("<input type=\"button\" onclick=\"fsearch()\" value=\"Search\">\n")
                    q.hwrite("<br/>\n")
                    q.hwrite(File.ReadAllText(dir+"\\"+projectname+"_code.bee"))
                    q.hwrite("\t\t</div>\n")
                    q.hwrite("\t</body>\n")
                    q.hwrite("</html>\n")
                    q.hclose()
                    //beeファイル削除
                    File.Delete(dir+"\\"+projectname+"_code.bee")
                    File.Delete(dir+"\\"+projectname+"_par.bee")
                    File.Delete(dir+"\\"+projectname+"_var"+".bee")
                    if File.Exists(dir+"\\"+"structure"+".bee") then File.Delete(dir+"\\"+"structure"+".bee")
                    //古いソースファイルを削除
                    if File.Exists(dir + "\\" + projectname+".html") then File.Delete(dir + "\\" + projectname+".html")
                    //新しいソースファイルを削除（beeファイルの拡張子を変更）
                    File.Move(dir + "\\" + projectname+".bee",dir + "\\" + projectname+".html")
                  |NL ->
                    code()