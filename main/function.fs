namespace Aqualis
    
    open System
    open System.IO
    
    [<AutoOpen>]
    module Aqualis_function =
        
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
                codewrite("call" + " " + projectname + "(" + args + ")\n")
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
                codewrite(projectname + "(" + args + ");\n")
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
                    writer.codewrite "%=============================================================================================\n"
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
                codewrite("call" + " " + projectname + "(" + args + ")\n")
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
                codewrite("\\(" + projectname + "(" + args + ")\\)<br/>\n")
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
                codewrite(re_args + " = " + projectname + "(" + args + ")\n")
            |_ -> ()
