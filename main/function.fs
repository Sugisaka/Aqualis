//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    open System

    [<AutoOpen>]
    module Aqualis_function =

        ///<summary>関数定義</summary>
        let private inheritDependencies (parent:Aqualis) (child:Aqualis) =
            child.hlist.list |> List.iter parent.hlist.add
            child.mlist.list |> List.iter parent.mlist.add
            child.elist.list |> List.iter parent.elist.add
            child.slist.list |> List.iter parent.slist.add
            child.olist.list |> List.iter parent.olist.add
            parent.IsOpenMpUsed <- parent.IsOpenMpUsed || child.IsOpenMpUsed
            parent.IsOpenAccUsed <- parent.IsOpenAccUsed || child.IsOpenAccUsed

        let private generateFunction (context:Aqualis) (projectname:string) (code:Aqualis->unit) =
            let fdeclare language (typ:Etype,vtp:VarType,name:string) =
                match language with
                |HTML ->
                    match vtp with
                    |A0 -> typ.tostring language + " :: " + name
                    |A1 0 -> typ.tostring language + ",allocatable" + " :: " + name + "(:)"
                    |A2(0,0) -> typ.tostring language + ",allocatable" + " :: " + name + "(:,:)"
                    |A3(0,0,0) -> typ.tostring language + ",allocatable" + " :: " + name + "(:,:,:)"
                    |A1 _ -> typ.tostring language + " :: " + name + "(:)"
                    |A2(_,_) -> typ.tostring language + " :: " + name + "(:,:)"
                    |A3(_,_,_) -> typ.tostring language + " :: " + name + "(:,:,:)"
                |_ ->
                    match vtp with
                    |A0 -> typ.tostring language + " :: " + name
                    |A1 0 -> typ.tostring language + ",allocatable" + " :: " + name + "(:)"
                    |A2(0,0) -> typ.tostring language + ",allocatable" + " :: " + name + "(:,:)"
                    |A3(0,0,0) -> typ.tostring language + ",allocatable" + " :: " + name + "(:,:,:)"
                    |A1 _ -> typ.tostring language + " :: " + name + "(:)"
                    |A2(_,_) -> typ.tostring language + " :: " + name + "(:,:)"
                    |A3(_,_,_) -> typ.tostring language + " :: " + name + "(:,:,:)"
            let dir = match context.dir with |Some d -> d |None -> ""
            match context.language with
            |Fortran ->
                context.flist.add projectname
                let args = Aqualis.makeProgramWithContext (dir,projectname,Fortran) <| fun childContext ->
                    code childContext
                    inheritDependencies context childContext
                    //ソースファイル(関数部分)出力
                    use writer = new codeWriter(dir + "\\" + projectname + "_main", 2, childContext.language)
                    writer.codewritein "!=============================================================================================\n"
                    writer.codewritein("! Subroutine name: " + projectname + "\n")
                    for _,(_,_,nm) in childContext.arg.list do
                        writer.codewritein("!  " + nm + "\n")
                    writer.codewritein "!============================================================================================="
                    let argvar = String.Join(", ", childContext.arg.list |> List.map(fun (_,(_,_,n)) -> n))
                    writer.codewritein("subroutine " + projectname + "(" + argvar + ")\n")
                    writer.indent.inc()
                    //モジュールファイルのインクルード
                    List.iter (fun (s:string) -> writer.codewritein("use " + s + "\n")) <| childContext.mlist.list
                    writer.codewritein "implicit none"
                    //ヘッダファイルのインクルード
                    List.iter (fun (s:string) -> writer.codewritein("include " + s + "\n")) <| childContext.hlist.list
                    //サブルーチン引数の定義
                    for _,s in childContext.arg.list do
                        writer.codewritein(fdeclare childContext.language s)
                    //グローバル変数の定義
                    declareall childContext writer
                    //メインコード
                    match childContext.allCodes with |Some s -> writer.codewritein s |None -> ()
                    writer.indent.dec()
                    writer.codewritein("end subroutine " + projectname + "\n")
                    writer.close()
                    childContext.delete()
                    //呼び出しコードを記述
                    String.Join(", ", childContext.arg.list |> List.map(fun (n,(_,_,_)) -> n))
                context.writein ("call" + " " + projectname + "(" + args + ")\n")
            |C99 ->
                context.flist.add projectname
                let args = Aqualis.makeProgramWithContext (dir,projectname,C99) <| fun childContext ->
                    code childContext
                    inheritDependencies context childContext
                    //ソースファイル(関数部分)出力
                    use writer = new codeWriter(dir + "\\" + projectname + "_main", 2, childContext.language)
                    writer.codewritein "/*==========================================================================================*/\n"
                    writer.codewritein("/* Subroutine name: " + projectname + " */\n")
                    for _,(_,_,nm) in childContext.arg.list do
                        writer.codewritein("/* " + nm + " */\n")
                    writer.codewritein "/*==========================================================================================*/\n"
                    //速度を上げるために参照渡しにしている
                    let argvar =
                        childContext.arg.list
                        |> List.map (fun (_,(typ,vtp,n)) ->
                            match vtp with
                            |A1 _|A2 _|A3 _ -> typ.tostring childContext.language + " *" + n
                            |_ -> typ.tostring childContext.language + " *" + n)
                        |> fun s -> String.Join(", ", s)
                    writer.codewritein("void " + projectname + "(" + argvar + ")\n")
                    writer.codewritein "{\n"
                    writer.indent.inc()
                    //グローバル変数の定義
                    declareall childContext writer
                    //メインコード
                    match childContext.allCodes with |Some s -> writer.codewritein s |None -> ()
                    writer.indent.dec()
                    writer.codewritein "}\n"
                    writer.close()
                    childContext.delete()
                    //呼び出しコードを記述
                    childContext.arg.list
                    |> List.map (fun (n,(typ,vtp,_)) ->
                        match typ,vtp,n.StartsWith "(*" with
                        |(It _|Dt|Zt|Structure _),A0,false -> "&" + n
                        |(It _|Dt|Zt|Structure _),A0,true  -> n.Substring(2,n.Length-3)
                        |_ -> n)
                    |> fun s -> String.Join(", ", s)
                context.writein (projectname + "(" + args + ");\n")
            |LaTeX ->
                context.flist.add projectname
                let args = Aqualis.makeProgramWithContext (dir,projectname,LaTeX) <| fun childContext ->
                    code childContext
                    inheritDependencies context childContext
                    //ソースファイル(関数部分)出力
                    use writer = new codeWriter(dir + "\\" + projectname + "_main", 2, childContext.language)
                    writer.codewritein "%=============================================================================================\n"
                    writer.codewritein("% Subroutine name: " + projectname + "\n")
                    for _,(_,_,nm) in childContext.arg.list do
                        writer.codewritein("% " +  nm + "\n")
                    writer.codewritein "%=============================================================================================\n"
                    let argvar = String.Join(", ", childContext.arg.list |> List.map (fun (_,(_,_,n)) -> n))
                    writer.codewritein("subroutine " + projectname + "(" + argvar + ")\n")
                    writer.indent.inc()
                    //モジュールファイルのインクルード
                    List.iter (fun (s:string) -> writer.codewritein("use " + s + "\n")) <| childContext.mlist.list
                    writer.codewritein "implicit none\n"
                    //ヘッダファイルのインクルード
                    List.iter (fun (s:string) -> writer.codewritein("include " + s + "\n")) <| childContext.hlist.list
                    //サブルーチン引数の定義
                    for _,s in childContext.arg.list do
                        writer.codewritein(fdeclare childContext.language s)
                    //グローバル変数の定義
                    declareall childContext writer
                    //メインコード
                    match childContext.allCodes with |Some s -> writer.codewritein s |None -> ()
                    writer.indent.dec()
                    writer.codewritein("end subroutine " + projectname + "\n")
                    writer.close()
                    childContext.delete()
                    //呼び出しコードを記述
                    String.Join(", ", childContext.arg.list |> List.map (fun (n,(_,_,_)) -> n))
                context.writein ("call" + " " + projectname + "(" + args + ")\n")
            |HTML ->
                context.flist.add projectname
                let args = Aqualis.makeProgramWithContext (dir,projectname,HTML) <| fun childContext ->
                    code childContext
                    inheritDependencies context childContext
                    //ソースファイル(関数部分)出力
                    use writer = new codeWriter(dir + "\\" + projectname + "_main", 2, childContext.language)
                    writer.codewritein("<h3>" + projectname + "</h3>\n")
                    writer.codewritein "<ul>\n"
                    for _,(_,_,nm) in childContext.arg.list do
                        writer.codewritein("<li>\\(" + nm + "\\)</li>\n")
                    writer.codewritein "</ul>\n"
                    let argvar = String.Join(", ", childContext.arg.list |> List.map (fun (_,(_,_,n)) -> n))
                    writer.codewritein "<div class=\"codeblock\">\n"
                    writer.codewritein "<details>\n"
                    writer.codewritein("<summary><span class=\"op-func\">function</span> \\(" + projectname + "(" + argvar + ")\\)</summary>\n")
                    writer.codewritein "<div class=\"insidecode-func\">\n"
                    writer.indent.inc()
                    writer.codewritein "<ul>\n"
                    //サブルーチン引数の定義
                    for _,s in childContext.arg.list do
                        writer.codewritein("<li>" + fdeclare childContext.language s + "</li>\n")
                    //グローバル変数の定義
                    declareall childContext writer
                    writer.codewritein "</ul>"
                    //メインコード
                    match childContext.allCodes with |Some s -> writer.codewritein s |None -> ()
                    writer.indent.dec()
                    writer.codewritein "</div>\n"
                    writer.codewritein "</details>\n"
                    writer.codewritein "</div>\n"
                    writer.close()
                    childContext.delete()
                    //呼び出しコードを記述
                    String.Join(", ", childContext.arg.list |> List.map (fun (n,(_,_,_)) -> n))
                context.writein  ("\\(" + projectname + "(" + args + ")\\)<br/>\n")
            |Python ->
                context.flist.add projectname
                let re_args,args = Aqualis.makeProgramWithContext (dir,projectname,Python) <| fun childContext ->
                    code childContext
                    inheritDependencies context childContext
                    //ソースファイル(関数部分)出力
                    use writer = new codeWriter(dir + "\\" + projectname + "_main", 2, childContext.language)
                    writer.codewritein "#==========================================================================================\n"
                    writer.codewritein("# Subroutine name: " + projectname + "\n")
                    for _,(_,_,nm) in childContext.arg.list do
                        writer.codewritein("# " + nm + "\n")
                    writer.codewritein "#==========================================================================================\n"
                    let argvar =
                        childContext.arg.list
                        |> List.map (fun (_,(_,_,n)) -> n)
                        |> fun s -> String.Join(", ", s)
                    let re_argvar =
                        childContext.arg.list
                        |> List.map (fun (_,(_,vtp,n)) ->
                            match vtp with
                            |A1 _|A2 _|A3 _ -> ""
                            |_ -> n)
                        |> List.filter (fun s -> s <> "")
                        |> fun s -> String.Join(", ", s)
                    //呼び出しコードを記述
                    let args =
                        childContext.arg.list
                        |> List.map (fun (n,(typ,vtp,_)) ->
                            match typ,vtp,n.StartsWith "(*" with
                            |(It _|Dt|Zt|Structure _),A0,false -> n
                            |(It _|Dt|Zt|Structure _),A0,true  -> n.Substring(2,n.Length-3)
                            |_ -> n)
                        |> fun s -> String.Join(", ", s)
                    let re_args =
                        childContext.arg.list
                        |> List.map (fun (n,(_,vtp,_)) ->
                            match vtp with
                            |A1 _|A2 _|A3 _ -> ""
                            |_ -> n)
                        |> List.filter (fun s -> s <> "")
                        |> fun s -> String.Join(", ", s)
                    writer.codewritein("def " + projectname + "(" + argvar + "):\n")
                    writer.indent.inc()
                    //グローバル変数の定義
                    declareall childContext writer
                    //メインコード
                    match childContext.allCodes with |Some s -> writer.codewritein s |None -> ()
                    writer.codewritein("return " + re_argvar + "\n")
                    writer.indent.dec()
                    writer.close()
                    childContext.delete()
                    re_args,args
                context.writein (re_args + " = " + projectname + "(" + args + ")\n")
            |_ -> ()

        type Aqualis with
            member this.func projectname code = generateFunction this projectname code
