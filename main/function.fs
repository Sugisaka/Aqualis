//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    open System
    open System.IO

    [<AutoOpen>]
    module Aqualis_function =

        ///<summary>関数定義</summary>
        let func (projectname:string) (code:unit->unit) =
            let fdeclare (typ:Etype,vtp:VarType,name:string) =
                match (GenerationScope.currentProgram()).language with
                |HTML ->
                    match vtp with
                    |A0 -> typ.tostring (GenerationScope.currentProgram()).language + " :: " + name
                    |A1 0 -> typ.tostring (GenerationScope.currentProgram()).language + ",allocatable" + " :: " + name + "(:)"
                    |A2(0,0) -> typ.tostring (GenerationScope.currentProgram()).language + ",allocatable" + " :: " + name + "(:,:)"
                    |A3(0,0,0) -> typ.tostring (GenerationScope.currentProgram()).language + ",allocatable" + " :: " + name + "(:,:,:)"
                    |A1 _ -> typ.tostring (GenerationScope.currentProgram()).language + " :: " + name + "(:)"
                    |A2(_,_) -> typ.tostring (GenerationScope.currentProgram()).language + " :: " + name + "(:,:)"
                    |A3(_,_,_) -> typ.tostring (GenerationScope.currentProgram()).language + " :: " + name + "(:,:,:)"
                |_ ->
                    match vtp with
                    |A0 -> typ.tostring (GenerationScope.currentProgram()).language + " :: " + name
                    |A1 0 -> typ.tostring (GenerationScope.currentProgram()).language + ",allocatable" + " :: " + name + "(:)"
                    |A2(0,0) -> typ.tostring (GenerationScope.currentProgram()).language + ",allocatable" + " :: " + name + "(:,:)"
                    |A3(0,0,0) -> typ.tostring (GenerationScope.currentProgram()).language + ",allocatable" + " :: " + name + "(:,:,:)"
                    |A1 _ -> typ.tostring (GenerationScope.currentProgram()).language + " :: " + name + "(:)"
                    |A2(_,_) -> typ.tostring (GenerationScope.currentProgram()).language + " :: " + name + "(:,:)"
                    |A3(_,_,_) -> typ.tostring (GenerationScope.currentProgram()).language + " :: " + name + "(:,:,:)"
            let dir = (GenerationScope.currentProgram()).dir
            match (GenerationScope.currentProgram()).language with
            |Fortran ->
                (GenerationScope.currentProgram()).flist.add projectname
                let args = makeProgram [dir,projectname,Fortran] <| fun () ->
                    code()
                    (GenerationScope.currentProgram()).close()
                    //ソースファイル(関数部分)出力
                    let writer = codeWriter(dir + "\\" + projectname + "_main", 2, (GenerationScope.currentProgram()).language)
                    writer.codewritein "!=============================================================================================\n"
                    writer.codewritein("! Subroutine name: " + projectname + "\n")
                    for _,(_,_,nm) in (GenerationScope.currentProgram()).arg.list do
                        writer.codewritein("!  " + nm + "\n")
                    writer.codewritein "!============================================================================================="
                    let argvar = String.Join(", ", (GenerationScope.currentProgram()).arg.list |> List.map(fun (_,(_,_,n)) -> n))
                    writer.codewritein("subroutine " + projectname + "(" + argvar + ")\n")
                    writer.indent.inc()
                    //モジュールファイルのインクルード
                    List.iter (fun (s:string) -> writer.codewritein("use " + s + "\n")) <| (GenerationScope.currentProgram()).mlist.list
                    writer.codewritein "implicit none"
                    //ヘッダファイルのインクルード
                    List.iter (fun (s:string) -> writer.codewritein("include " + s + "\n")) <| (GenerationScope.currentProgram()).hlist.list
                    //サブルーチン引数の定義
                    for _,s in (GenerationScope.currentProgram()).arg.list do
                        writer.codewritein(fdeclare s)
                    //グローバル変数の定義
                    declareall writer
                    //メインコード
                    writer.codewritein((GenerationScope.currentProgram()).allCodes)
                    writer.indent.dec()
                    writer.codewritein("end subroutine " + projectname + "\n")
                    writer.close()
                    File.Delete(dir + "\\" + projectname)
                    //呼び出しコードを記述
                    String.Join(", ", (GenerationScope.currentProgram()).arg.list |> List.map(fun (n,(_,_,_)) -> n))
                writein("call" + " " + projectname + "(" + args + ")\n")
            |C99 ->
                (GenerationScope.currentProgram()).flist.add projectname
                let args = makeProgram [dir,projectname,C99] <| fun () ->
                    code()
                    (GenerationScope.currentProgram()).close()
                    //ソースファイル(関数部分)出力
                    let writer = codeWriter(dir + "\\" + projectname + "_main", 2, (GenerationScope.currentProgram()).language)
                    writer.codewritein "/*==========================================================================================*/\n"
                    writer.codewritein("/* Subroutine name: " + projectname + " */\n")
                    for _,(_,_,nm) in (GenerationScope.currentProgram()).arg.list do
                        writer.codewritein("/* " + nm + " */\n")
                    writer.codewritein "/*==========================================================================================*/\n"
                    //速度を上げるために参照渡しにしている
                    let argvar =
                        (GenerationScope.currentProgram()).arg.list
                        |> List.map (fun (_,(typ,vtp,n)) ->
                            match vtp with
                            |A1 _|A2 _|A3 _ -> typ.tostring (GenerationScope.currentProgram()).language + " *" + n
                            |_ -> typ.tostring (GenerationScope.currentProgram()).language + " *" + n)
                        |> fun s -> String.Join(", ", s)
                    writer.codewritein("void " + projectname + "(" + argvar + ")\n")
                    writer.codewritein "{\n"
                    writer.indent.inc()
                    //グローバル変数の定義
                    declareall writer
                    //メインコード
                    writer.codewritein((GenerationScope.currentProgram()).allCodes)
                    writer.indent.dec()
                    writer.codewritein "}\n"
                    writer.close()
                    File.Delete(dir + "\\" + projectname)
                    //呼び出しコードを記述
                    (GenerationScope.currentProgram()).arg.list
                    |> List.map (fun (n,(typ,vtp,_)) ->
                        match typ,vtp,n.StartsWith "(*" with
                        |(It _|Dt|Zt|Structure _),A0,false -> "&" + n
                        |(It _|Dt|Zt|Structure _),A0,true  -> n.Substring(2,n.Length-3)
                        |_ -> n)
                    |> fun s -> String.Join(", ", s)
                writein(projectname + "(" + args + ");\n")
            |LaTeX ->
                (GenerationScope.currentProgram()).flist.add projectname
                let args = makeProgram [dir,projectname,LaTeX] <| fun () ->
                    code()
                    (GenerationScope.currentProgram()).close()
                    //ソースファイル(関数部分)出力
                    let writer = codeWriter(dir + "\\" + projectname + "_main", 2, (GenerationScope.currentProgram()).language)
                    writer.codewritein "%=============================================================================================\n"
                    writer.codewritein("% Subroutine name: " + projectname + "\n")
                    for _,(_,_,nm) in (GenerationScope.currentProgram()).arg.list do
                        writer.codewritein("% " +  nm + "\n")
                    writer.codewritein "%=============================================================================================\n"
                    let argvar = String.Join(", ", (GenerationScope.currentProgram()).arg.list |> List.map (fun (_,(_,_,n)) -> n))
                    writer.codewritein("subroutine " + projectname + "(" + argvar + ")\n")
                    writer.indent.inc()
                    //モジュールファイルのインクルード
                    List.iter (fun (s:string) -> writer.codewritein("use " + s + "\n")) <| (GenerationScope.currentProgram()).mlist.list
                    writer.codewritein "implicit none\n"
                    //ヘッダファイルのインクルード
                    List.iter (fun (s:string) -> writer.codewritein("include " + s + "\n")) <| (GenerationScope.currentProgram()).hlist.list
                    //サブルーチン引数の定義
                    for _,s in (GenerationScope.currentProgram()).arg.list do
                        writer.codewritein(fdeclare s)
                    //グローバル変数の定義
                    declareall writer
                    //メインコード
                    writer.codewritein((GenerationScope.currentProgram()).allCodes)
                    writer.indent.dec()
                    writer.codewritein("end subroutine " + projectname + "\n")
                    writer.close()
                    File.Delete(dir + "\\" + projectname)
                    //呼び出しコードを記述
                    String.Join(", ", (GenerationScope.currentProgram()).arg.list |> List.map (fun (n,(_,_,_)) -> n))
                writein("call" + " " + projectname + "(" + args + ")\n")
            |HTML ->
                (GenerationScope.currentProgram()).flist.add projectname
                let args = makeProgram [dir,projectname,HTML] <| fun () ->
                    code()
                    (GenerationScope.currentProgram()).close()
                    //ソースファイル(関数部分)出力
                    let writer = codeWriter(dir + "\\" + projectname + "_main", 2, (GenerationScope.currentProgram()).language)
                    writer.codewritein("<h3>" + projectname + "</h3>\n")
                    writer.codewritein "<ul>\n"
                    for _,(_,_,nm) in (GenerationScope.currentProgram()).arg.list do
                        writer.codewritein("<li>\\(" + nm + "\\)</li>\n")
                    writer.codewritein "</ul>\n"
                    let argvar = String.Join(", ", (GenerationScope.currentProgram()).arg.list |> List.map (fun (_,(_,_,n)) -> n))
                    writer.codewritein "<div class=\"codeblock\">\n"
                    writer.codewritein "<details>\n"
                    writer.codewritein("<summary><span class=\"op-func\">function</span> \\(" + projectname + "(" + argvar + ")\\)</summary>\n")
                    writer.codewritein "<div class=\"insidecode-func\">\n"
                    writer.indent.inc()
                    writer.codewritein "<ul>\n"
                    //サブルーチン引数の定義
                    for _,s in (GenerationScope.currentProgram()).arg.list do
                        writer.codewritein("<li>" + fdeclare s + "</li>\n")
                    //グローバル変数の定義
                    declareall writer
                    writer.codewritein "</ul>"
                    //メインコード
                    writer.codewritein((GenerationScope.currentProgram()).allCodes)
                    writer.indent.dec()
                    writer.codewritein "</div>\n"
                    writer.codewritein "</details>\n"
                    writer.codewritein "</div>\n"
                    writer.close()
                    File.Delete(dir + "\\" + projectname)
                    //呼び出しコードを記述
                    String.Join(", ", (GenerationScope.currentProgram()).arg.list |> List.map (fun (n,(_,_,_)) -> n))
                writein("\\(" + projectname + "(" + args + ")\\)<br/>\n")
            |Python ->
                (GenerationScope.currentProgram()).flist.add projectname
                let re_args,args = makeProgram [dir,projectname,Python] <| fun () ->
                    code()
                    (GenerationScope.currentProgram()).close()
                    //ソースファイル(関数部分)出力
                    let writer = codeWriter(dir + "\\" + projectname + "_main", 2, (GenerationScope.currentProgram()).language)
                    writer.codewritein "#==========================================================================================\n"
                    writer.codewritein("# Subroutine name: " + projectname + "\n")
                    for _,(_,_,nm) in (GenerationScope.currentProgram()).arg.list do
                        writer.codewritein("# " + nm + "\n")
                    writer.codewritein "#==========================================================================================\n"
                    let argvar =
                        (GenerationScope.currentProgram()).arg.list
                        |> List.map (fun (_,(_,_,n)) -> n)
                        |> fun s -> String.Join(", ", s)
                    let re_argvar =
                        (GenerationScope.currentProgram()).arg.list
                        |> List.map (fun (_,(_,vtp,n)) ->
                            match vtp with
                            |A1 _|A2 _|A3 _ -> ""
                            |_ -> n)
                        |> List.filter (fun s -> s <> "")
                        |> fun s -> String.Join(", ", s)
                    //呼び出しコードを記述
                    let args =
                        (GenerationScope.currentProgram()).arg.list
                        |> List.map (fun (n,(typ,vtp,_)) ->
                            match typ,vtp,n.StartsWith "(*" with
                            |(It _|Dt|Zt|Structure _),A0,false -> n
                            |(It _|Dt|Zt|Structure _),A0,true  -> n.Substring(2,n.Length-3)
                            |_ -> n)
                        |> fun s -> String.Join(", ", s)
                    let re_args =
                        (GenerationScope.currentProgram()).arg.list
                        |> List.map (fun (n,(_,vtp,_)) ->
                            match vtp with
                            |A1 _|A2 _|A3 _ -> ""
                            |_ -> n)
                        |> List.filter (fun s -> s <> "")
                        |> fun s -> String.Join(", ", s)
                    writer.codewritein("def " + projectname + "(" + argvar + "):\n")
                    writer.indent.inc()
                    //グローバル変数の定義
                    declareall writer
                    //メインコード
                    writer.codewritein((GenerationScope.currentProgram()).allCodes)
                    writer.codewritein("return " + re_argvar + "\n")
                    writer.indent.dec()
                    writer.close()
                    File.Delete(dir + "\\" + projectname)
                    re_args,args
                writein(re_args + " = " + projectname + "(" + args + ")\n")
            |_ -> ()
