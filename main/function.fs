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

        type private PythonFunctionArgument = {
            ActualName:string
            FormalName:string
            Shape:VarType }

        let private requiresPythonWriteBack argument =
            match argument.Shape with
            |A0 -> true
            |A1 _|A2 _|A3 _ -> false

        let private normalizePythonActualName typ shape (name:string) =
            match typ,shape,name.StartsWith "(*" with
            |(It _|Dt|Zt|Structure _),A0,true ->
                name.Substring(2,name.Length-3)
            |_ ->
                name

        let private pythonFunctionArguments arguments =
            arguments
            |> List.map (fun (actualName,(typ,shape,formalName)) -> {
                ActualName = normalizePythonActualName typ shape actualName
                FormalName = formalName
                Shape = shape })

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
                let writeBackActualNames,actualNames = Aqualis.makeProgramWithContext (dir,projectname,Python) <| fun childContext ->
                    code childContext
                    inheritDependencies context childContext
                    //ソースファイル(関数部分)出力
                    use writer = new codeWriter(dir + "\\" + projectname + "_main", 2, childContext.language)
                    writer.codewritein "#==========================================================================================\n"
                    writer.codewritein("# Subroutine name: " + projectname + "\n")
                    for _,(_,_,nm) in childContext.arg.list do
                        writer.codewritein("# " + nm + "\n")
                    writer.codewritein "#==========================================================================================\n"
                    let functionArguments =
                        pythonFunctionArguments childContext.arg.list
                    let formalNames =
                        functionArguments |> List.map _.FormalName
                    let actualNames =
                        functionArguments |> List.map _.ActualName
                    let writeBackArguments =
                        functionArguments |> List.filter requiresPythonWriteBack
                    writer.codewritein(
                        "def " + projectname + "(" + String.concat ", " formalNames + "):\n")
                    writer.indent.inc()
                    //グローバル変数の定義
                    declareall childContext writer
                    //メインコード
                    match childContext.allCodes with |Some s -> writer.codewritein s |None -> ()
                    match writeBackArguments with
                    |[] ->
                        writer.codewritein "return\n"
                    |arguments ->
                        arguments
                        |> List.map _.FormalName
                        |> String.concat ", "
                        |> fun names -> writer.codewritein("return " + names + "\n")
                    writer.indent.dec()
                    writer.close()
                    childContext.delete()
                    writeBackArguments |> List.map _.ActualName, actualNames
                let callExpression =
                    projectname + "(" + String.concat ", " actualNames + ")"
                match writeBackActualNames with
                |[] ->
                    context.writein (callExpression + "\n")
                |names ->
                    context.writein (
                        String.concat ", " names + " = " + callExpression + "\n")
            |_ -> ()

        type Aqualis with
            ///<summary>非インライン関数定義</summary>
            member this.func projectname code = generateFunction this projectname code
