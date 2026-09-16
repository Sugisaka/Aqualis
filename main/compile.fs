//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    open System
    open System.IO
    open System.Security.Cryptography
    open System.Text

    [<AutoOpen>]
    module Aqualis_main =

        [<Literal>]
        let private MaximumProjectNameUtf8Bytes = 200

        [<Literal>]
        let private MaximumFortranIdentifierLength = 63

        let private isWindowsDeviceName (projectName:string) =
            let components = projectName.Split('.')
            let stem = components[0].ToUpperInvariant()
            match stem with
            | "CON" | "PRN" | "AUX" | "NUL" -> true
            |_ when stem.Length = 4 &&
                    (stem.StartsWith("COM", StringComparison.Ordinal) ||
                     stem.StartsWith("LPT", StringComparison.Ordinal)) &&
                    stem[3] >= '1' && stem[3] <= '9' -> true
            |_ -> false

        let private validateProjectName (projectName:string) =
            if String.IsNullOrWhiteSpace projectName then
                invalidArg "projectname" "A project name is required."
            if not (String.Equals(projectName, projectName.Trim(), StringComparison.Ordinal)) then
                invalidArg "projectname" "A project name cannot start or end with whitespace."
            if projectName = "." || projectName = ".." || Path.IsPathRooted projectName then
                invalidArg "projectname" "A project name must be one relative file-name segment."
            if projectName.IndexOfAny([| '/'; '\\' |]) >= 0 then
                invalidArg "projectname" "A project name cannot contain directory separators."
            if projectName |> Seq.exists Char.IsControl then
                invalidArg "projectname" "A project name cannot contain control characters."
            if projectName.IndexOfAny([| '<'; '>'; ':'; '"'; '|'; '?'; '*' |]) >= 0 ||
               projectName.IndexOfAny(Path.GetInvalidFileNameChars()) >= 0 then
                invalidArg "projectname" "A project name contains characters that are not portable in file names."
            if projectName.EndsWith(".", StringComparison.Ordinal) then
                invalidArg "projectname" "A project name cannot end with a period."
            if isWindowsDeviceName projectName then
                invalidArg "projectname" "A project name cannot use a reserved device name."
            if Encoding.UTF8.GetByteCount(projectName) > MaximumProjectNameUtf8Bytes then
                invalidArg "projectname" "A project name cannot exceed 200 bytes in UTF-8."
            projectName

        let private fortranProgramIdentifier (projectName:string) =
            let asciiLetter character =
                ('A' <= character && character <= 'Z') ||
                ('a' <= character && character <= 'z')
            let valid character =
                asciiLetter character || Char.IsDigit character || character = '_'
            let mapped =
                projectName
                |> Seq.map (fun character -> if valid character then character else '_')
                |> Seq.toArray
                |> String
            let candidate =
                if asciiLetter mapped[0] then mapped
                else "aqualis_" + mapped

            if candidate.Length <= MaximumFortranIdentifierLength then
                candidate
            else
                let digest =
                    projectName
                    |> Encoding.UTF8.GetBytes
                    |> SHA256.HashData
                    |> Convert.ToHexString
                    |> fun value -> value.Substring(0, 8).ToLowerInvariant()
                candidate.Substring(0, MaximumFortranIdentifierLength - digest.Length - 1) + "_" + digest

        let private singleLineMetadata argumentName (value:string) =
            if isNull value then nullArg argumentName

            let result = StringBuilder(value.Length)
            for character in value do
                match character with
                |'\r' -> result.Append("\\r") |> ignore
                |'\n' -> result.Append("\\n") |> ignore
                |'\t' -> result.Append("\\t") |> ignore
                |character when
                    Char.IsControl character ||
                    character = '\u2028' ||
                    character = '\u2029' ->
                    result.Append("\\u").Append((int character).ToString("X4")) |> ignore
                |character -> result.Append(character) |> ignore
            result.ToString()

        let private latexText (value:string) =
            let result = StringBuilder(value.Length)
            for character in value do
                match character with
                |'\\' -> result.Append("\\textbackslash{}") |> ignore
                |'{' -> result.Append("\\{") |> ignore
                |'}' -> result.Append("\\}") |> ignore
                |'$' -> result.Append("\\$") |> ignore
                |'&' -> result.Append("\\&") |> ignore
                |'#' -> result.Append("\\#") |> ignore
                |'%' -> result.Append("\\%") |> ignore
                |'_' -> result.Append("\\_") |> ignore
                |'^' -> result.Append("\\textasciicircum{}") |> ignore
                |'~' -> result.Append("\\textasciitilde{}") |> ignore
                |character -> result.Append(character) |> ignore
            result.ToString()

        type private CompilationOutputTransaction(outputDirectory:string) =
            let outputDirectory = Path.GetFullPath outputDirectory
            let transactionId = Guid.NewGuid().ToString("N")
            let stagingDirectory =
                Path.Combine(outputDirectory, ".aqualis-transaction-" + transactionId)
            let rollbackDirectory =
                Path.Combine(outputDirectory, ".aqualis-rollback-" + transactionId)
            let mutable committed = false

            let removeDirectory path =
                if Directory.Exists path then
                    Directory.Delete(path, true)

            let relativeFiles root =
                if Directory.Exists root then
                    Directory.GetFiles(root, "*", SearchOption.AllDirectories)
                    |> Array.map (fun path -> Path.GetRelativePath(root, path))
                    |> Array.sort
                else
                    [||]

            do
                if not (Directory.Exists outputDirectory) then
                    raise (DirectoryNotFoundException($"The output directory '{outputDirectory}' does not exist."))
                Directory.CreateDirectory(stagingDirectory) |> ignore

            member _.StagingDirectory = stagingDirectory

            member _.Commit() =
                let files = relativeFiles stagingDirectory
                let published = ResizeArray<string * bool>()
                let mutable currentBackup : (string * string) option = None

                AtomicOutputFile.synchronize (fun () ->
                    try
                        for relativePath in files do
                            let stagedPath = Path.Combine(stagingDirectory, relativePath)
                            let targetPath = Path.Combine(outputDirectory, relativePath)
                            let targetParent = Path.GetDirectoryName targetPath
                            Directory.CreateDirectory(targetParent) |> ignore

                            let hadExistingFile = File.Exists targetPath
                            if hadExistingFile then
                                let backupPath = Path.Combine(rollbackDirectory, relativePath)
                                Directory.CreateDirectory(Path.GetDirectoryName backupPath) |> ignore
                                File.Move(targetPath, backupPath)
                                currentBackup <- Some(targetPath, backupPath)

                            File.Move(stagedPath, targetPath)
                            published.Add(relativePath, hadExistingFile)
                            currentBackup <- None

                        committed <- true
                    with _ ->
                        match currentBackup with
                        |Some(targetPath, backupPath) when File.Exists backupPath ->
                            File.Move(backupPath, targetPath)
                        |_ -> ()

                        for relativePath,hadExistingFile in published |> Seq.rev do
                            let targetPath = Path.Combine(outputDirectory, relativePath)
                            if File.Exists targetPath then
                                File.Delete targetPath
                            if hadExistingFile then
                                let backupPath = Path.Combine(rollbackDirectory, relativePath)
                                if File.Exists backupPath then
                                    Directory.CreateDirectory(Path.GetDirectoryName targetPath) |> ignore
                                    File.Move(backupPath, targetPath)
                        reraise())

                removeDirectory rollbackDirectory
                removeDirectory stagingDirectory

            interface IDisposable with
                member _.Dispose() =
                    if not committed then
                        removeDirectory stagingDirectory
                        removeDirectory rollbackDirectory

        ///<summary>コンパイル</summary>
        let Compile langgList dir projectname (codever:string) code =
            let projectname = validateProjectName projectname
            let languages = langgList |> Seq.toList
            let codever = singleLineMetadata (nameof codever) codever
            let fortranProgramName = fortranProgramIdentifier projectname
            use transaction = new CompilationOutputTransaction(dir)
            let outputDirectory = transaction.StagingDirectory
            for lang in languages do
                match lang with
                |Fortran ->
                    Aqualis.makeIntermediateProgramInDirectoryWithContext
                        (dir, projectname, Fortran)
                        outputDirectory
                    <| fun context ->
                        //メインコード生成
                        code context
                        context.close()
                        //ソースファイル出力
                        use writer = codeWriter.CreateAtomic(Path.Combine(outputDirectory, projectname + ".f90"), 2, Fortran)
                        writer.codewritein "!=============================================================================================\n"
                        writer.codewritein("! Project name: " + projectname + "\n")
                        writer.codewritein("! Project version: " + codever + "\n")
                        writer.codewritein "!---------------------------------------------------------------------------------------------\n"
                        writer.codewritein "! Generated by Aqualis (algorithm and equation analyzer for lightwave simulation)\n"
                        writer.codewritein("! Aqualis version: " + Aqualis.Version + "\n")
                        writer.codewritein "!=============================================================================================\n"
                        writer.codewritein("program " + fortranProgramName + "\n")
                        writer.codewritein "use, intrinsic :: ieee_arithmetic\n"
                        //モジュールファイルのインクルード
                        List.iter (fun (s:string) -> writer.codewritein("use " + s + "\n")) <| context.mlist.list
                        writer.codewritein "implicit none\n"
                        //ヘッダファイルのインクルード
                        List.iter (fun (s:string) -> writer.codewritein("include " + s + "\n")) <| context.hlist.list
                        //構造体の定義
                        context.str.Def_Structure writer
                        //グローバル変数の定義
                        declareall context writer
                        //メインコード
                        match context.allCodes with |Some s -> writer.codewritein s |None -> ()
                        //サブルーチン
                        writer.codewritein "\n"
                        writer.codewritein "contains\n"
                        writer.codewritein "\n"
                        for funname in context.flist.list do
                            let functionPath = Path.Combine(context.IntermediateDirectory, funname + "_main")
                            writer.codewritein(File.ReadAllText functionPath)
                            File.Delete functionPath
                            writer.codewritein "\n"
                        writer.codewritein("end program " + fortranProgramName + "\n")
                        writer.close()
                        //beeファイル削除
                        context.delete()
                        writer.publish()
                        //コンパイル・実行用スクリプト生成
                        use wr = ShellScriptWriter.create(Path.Combine(outputDirectory, "proc_" + projectname + "_F.sh"))
                        wr.WriteLine "#!/bin/bash"
                        wr.WriteLine()
                        let sources = context.slist.list
                        let options = context.olist.list
                        let buildCompileCommand compiler fixedArguments =
                            ShellCommand.buildCompileCommand
                                compiler
                                fixedArguments
                                sources
                                (projectname + ".f90")
                                options
                                (projectname + ".exe")
                        let compileCommand =
                            if context.IsOpenAccUsed then
                                buildCompileCommand
                                    "/usr/bin/pgfortran"
                                    ["-acc"; "-Minfo=accel"]
                            else if context.IsOpenMpUsed then
                                buildCompileCommand
                                    "/usr/bin/gfortran"
                                    ["-fopenmp"]
                            else
                                buildCompileCommand
                                    "/usr/bin/gfortran"
                                    ["-ffree-line-length-none"]
                        ShellScriptWriter.writeCompileAndRun
                            wr
                            compileCommand
                            ("./" + projectname + ".exe")
                            []
                |C99 ->
                    Aqualis.makeIntermediateProgramInDirectoryWithContext
                        (dir, projectname, C99)
                        outputDirectory
                    <| fun context ->
                        //メインコード生成
                        context.indentInc()
                        code context
                        context.olist.add "-lm"
                        context.indentDec()
                        context.close()
                        //ソースファイル出力
                        use writer = codeWriter.CreateAtomic(Path.Combine(outputDirectory, projectname + ".c"), 2, C99)
                        writer.codewritein "/*=============================================================================================*/\n"
                        writer.codewritein("/* Project name: " + projectname + " */\n")
                        writer.codewritein("// Project version: " + codever + "\n")
                        writer.codewritein "/*---------------------------------------------------------------------------------------------*/\n"
                        writer.codewritein "/* Generated by Aqualis (algorithm and equation analyzer for lightwave simulation) */\n"
                        writer.codewritein("/* Aqualis version: " + Aqualis.Version + " */\n")
                        writer.codewritein "/*=============================================================================================*/\n"
                        writer.codewritein "#include <stdio.h>\n"
                        writer.codewritein "#include <stdlib.h>\n"
                        writer.codewritein "#include <complex.h>\n"
                        writer.codewritein "#include <math.h>\n"
                        //ヘッダファイルのインクルード
                        List.iter (fun (s:string) -> writer.codewritein ("#include " + s + "\n")) <| context.hlist.list
                        writer.codewritein "#undef I\n"
                        writer.codewritein "#define uj _Complex_I\n"
                        //構造体の定義
                        context.str.Def_Structure writer
                        //グローバル変数の宣言
                        declareall context writer
                        //extern指定子
                        for s in context.elist.list do
                            writer.codewritein ("extern " + s + ";\n")
                        //関数定義
                        for funname in context.flist.list do
                            let functionPath = Path.Combine(context.IntermediateDirectory, funname + "_main")
                            writer.codewritein (File.ReadAllText functionPath)
                            File.Delete functionPath
                            writer.codewritein ("\n")
                        //Main関数
                        writer.codewritein "int main()\n"
                        writer.codewritein "{\n"
                        match context.allCodes with |Some s -> writer.codewritein s |None -> ()
                        writer.codewritein "  return 0;\n"
                        writer.codewritein "}\n"
                        writer.close()
                        //beeファイル削除
                        context.delete()
                        writer.publish()
                        //コンパイル・実行用スクリプト生成
                        use wr = ShellScriptWriter.create(Path.Combine(outputDirectory, "proc_" + projectname + "_C.sh"))
                        wr.WriteLine "#!/bin/bash"
                        wr.WriteLine()
                        let sources = context.slist.list
                        let options = context.olist.list
                        let buildCompileCommand compiler fixedArguments =
                            ShellCommand.buildCompileCommand
                                compiler
                                fixedArguments
                                sources
                                (projectname + ".c")
                                options
                                (projectname + ".exe")
                        let compileCommand =
                            if context.IsOpenMpUsed then
                                buildCompileCommand "gcc" ["-fopenmp"]
                            else if context.IsOpenAccUsed then
                                buildCompileCommand "pgcc" ["-acc"; "-Minfo=accel"]
                            else
                                buildCompileCommand "gcc" []
                        ShellScriptWriter.writeCompileAndRun
                            wr
                            compileCommand
                            ("./" + projectname + ".exe")
                            []
                |LaTeX ->
                    Aqualis.makeIntermediateProgramInDirectoryWithContext
                        (dir, projectname, LaTeX)
                        outputDirectory
                    <| fun context ->
                        //メインコード生成
                        code context
                        context.close()
                        //ソースファイル出力
                        use writer = codeWriter.CreateAtomic(Path.Combine(outputDirectory, projectname + ".tex"), 2, LaTeX)
                        writer.codewritein "\\documentclass[a4paper,fleqn]{ltjsarticle}\n"
                        writer.codewritein "\\usepackage{amsmath}\n"
                        List.iter (fun (s:string) -> writer.codewritein(s + "\n")) <| context.hlist.list
                        writer.codewritein "\\oddsidemargin=-0.4mm\n"
                        writer.codewritein "\\topmargin=4.6mm\n"
                        writer.codewritein "\\headheight=0mm\n"
                        writer.codewritein "\\headsep=0mm\n"
                        writer.codewritein "\\footskip=15mm\n"
                        writer.codewritein "\\textwidth=160mm\n"
                        writer.codewritein "\\textheight=237mm\n"
                        writer.codewritein "\\topsep=6pt\n"
                        writer.codewritein "\\parindent=0mm\n"
                        writer.codewritein "\\unitlength=1.00mm\n"
                        writer.codewritein "\\begin{document}\n"
                        writer.codewritein("{\\Large " + latexText projectname + "}\n")
                        //構造体の定義
                        writer.codewritein "\\section{structures}\n"
                        context.str.Def_Structure writer
                        //関数定義
                        writer.codewritein "\\section{subroutines}\n"
                        for funname in context.flist.list do
                            let functionPath = Path.Combine(context.IntermediateDirectory, funname + "_main")
                            writer.codewritein(File.ReadAllText functionPath)
                            File.Delete functionPath
                            writer.codewritein("\n")
                        //グローバル変数の定義
                        writer.codewritein "\\section{global variables}\n"
                        writer.codewritein "\\begin{itemize}\n"
                        declareall context writer
                        writer.codewritein "\\end{itemize}\n"
                        //メインコード
                        writer.codewritein "\\section{main code}\n"
                        match context.allCodes with |Some s -> writer.codewritein s |None -> ()
                        writer.codewritein "\\end{document}\n"
                        writer.close()
                        //beeファイル削除
                        context.delete()
                        writer.publish()
                |HTML ->
                    Aqualis.makeIntermediateProgramInDirectoryWithContext
                        (dir, projectname, HTML)
                        outputDirectory
                    <| fun context ->
                        let encodedProjectName = HtmlEncoding.textContent projectname
                        //メインコード生成
                        code context
                        context.close()
                        //ソースファイル出力
                        use writer = codeWriter.CreateAtomic(Path.Combine(outputDirectory, projectname + ".html"), 2, HTML)
                        writer.codewritein "<!DOCTYPE html>\n"
                        writer.codewritein "<html lang='ja'>\n"
                        writer.codewritein "\t<head>\n"
                        writer.codewritein "\t\t<meta charset='utf-8'>\n"
                        writer.codewritein("\t\t<title>" + encodedProjectName + "</title>\n")
                        writer.codewritein "\t\t<meta name='viewport' content='width=device-width, initial-scale=1.0, maximum-scale=1.0, minimum-scale=1.0'>\n"
                        HtmlAssetRendering.write "\t\t" writer.codewritein context.htmlAssets
                        writer.codewritein "\t\t<style type=\"text/css\">\n"
                        writer.codewritein "\t\t<!--\n"
                        writer.codewritein "\t\tbody {\n"
                        writer.codewritein "\t\t\tfont-family: 'Noto Sans JP', 'Yu Gothic', 'Hiragino Kaku Gothic ProN', sans-serif;\n"
                        writer.codewritein "\t\t\tfont-weight: 500;\n"
                        writer.codewritein "\t\t\tfont-size: 16px;\n"
                        writer.codewritein "\t\t}\n"
                        writer.codewritein "\t\th2 {\n"
                        writer.codewritein "\t\t\tborder-bottom: 2px solid;\n"
                        writer.codewritein "\t\t}\n"
                        writer.codewritein "\t\ta {\n"
                        writer.codewritein "\t\t\tfont-size: 11pt;\n"
                        writer.codewritein "\t\t\ttext-decoration: none;\n"
                        writer.codewritein "\t\t\tcolor: #8000ff;\n"
                        writer.codewritein "\t\t}\n"
                        writer.codewritein "\t\t.fio {\n"
                        writer.codewritein "\t\t\tmargin-right: 10px;\n"
                        writer.codewritein "\t\t\tfont-size: 11pt;\n"
                        writer.codewritein "\t\t\tcolor: #ff00ff;\n"
                        writer.codewritein "\t\t}\n"
                        writer.codewritein "\t\t.continue {\n"
                        writer.codewritein "\t\t\tfont-size: 11pt;\n"
                        writer.codewritein "\t\t\tcolor: #8000ff;\n"
                        writer.codewritein "\t\t}\n"
                        writer.codewritein "\t\t.codeblock {\n"
                        writer.codewritein "\t\t\tpadding-left: 0px;\n"
                        writer.codewritein "\t\t}\n"
                        writer.codewritein "\t\t\n"
                        writer.codewritein "\t\t.op-loop {\n"
                        writer.codewritein "\t\t\tfont-size: 11pt;\n"
                        writer.codewritein "\t\t\tcolor: #ff7f00;\n"
                        writer.codewritein "\t\t}\n"
                        writer.codewritein "\t\t\n"
                        writer.codewritein "\t\t.insidecode-loop {\n"
                        writer.codewritein "\t\t\tmargin-left: 2px;\n"
                        writer.codewritein "\t\t\tpadding-left: 30px;\n"
                        writer.codewritein "\t\t\tborder-left: solid;\n"
                        writer.codewritein "\t\t\tborder-width: 5px;\n"
                        writer.codewritein "\t\t\tborder-color: #ffa347;\n"
                        writer.codewritein "\t\t}\n"
                        writer.codewritein "\t\t\n"
                        writer.codewritein "\t\t.op-if {\n"
                        writer.codewritein "\t\t\tfont-size: 11pt;\n"
                        writer.codewritein "\t\t\tcolor: #007fff;\n"
                        writer.codewritein "\t\t}\n"
                        writer.codewritein "\t\t\n"
                        writer.codewritein "\t\t.insidecode-if {\n"
                        writer.codewritein "\t\t\tmargin-left: 2px;\n"
                        writer.codewritein "\t\t\tpadding-left: 30px;\n"
                        writer.codewritein "\t\t\tborder-left: solid;\n"
                        writer.codewritein "\t\t\tborder-width: 5px;\n"
                        writer.codewritein "\t\t\tborder-color: #47a3ff;\n"
                        writer.codewritein "\t\t}\n"
                        writer.codewritein "\t\t.op-func {\n"
                        writer.codewritein "\t\t\tfont-size: 11pt;\n"
                        writer.codewritein "\t\t\tcolor: #0000ff;\n"
                        writer.codewritein "\t\t}\n"
                        writer.codewritein "\t\t\n"
                        writer.codewritein "\t\t.insidecode-func {\n"
                        writer.codewritein "\t\t\tmargin-left: 2px;\n"
                        writer.codewritein "\t\t\tpadding-left: 30px;\n"
                        writer.codewritein "\t\t\tborder-left: solid;\n"
                        writer.codewritein "\t\t\tborder-width: 5px;\n"
                        writer.codewritein "\t\t\tborder-color: #0000ff;\n"
                        writer.codewritein "\t\t}\n"
                        writer.codewritein "\t\t.op-section {\n"
                        writer.codewritein "\t\t\tfont-size: 11pt;\n"
                        writer.codewritein "\t\t\tcolor: #008000;\n"
                        writer.codewritein "\t\t\tmargin-right: 5px;\n"
                        writer.codewritein "\t\t}\n"
                        writer.codewritein "\t\t\n"
                        writer.codewritein "\t\t.insidecode-section {\n"
                        writer.codewritein "\t\t\tmargin-left: 2px;\n"
                        writer.codewritein "\t\t\tpadding-left: 30px;\n"
                        writer.codewritein "\t\t\tborder-left: solid;\n"
                        writer.codewritein "\t\t\tborder-width: 5px;\n"
                        writer.codewritein "\t\t\tborder-color: #32cd32;\n"
                        writer.codewritein "\t\t}\n"
                        writer.codewritein "\t\t\n"
                        writer.codewritein "\t\t.comment {\n"
                        writer.codewritein "\t\t\tfont-size: 11pt;\n"
                        writer.codewritein "\t\t\tcolor: #008000;\n"
                        writer.codewritein "\t\t}\n"
                        writer.codewritein "\t\t-->\n"
                        writer.codewritein "\t\t</style>\n"
                        writer.codewritein "\t\t<script type=\"text/javascript\">\n"
                        writer.codewritein "\t\t\tfunction fsearch()\n"
                        writer.codewritein "\t\t\t{\n"
                        writer.codewritein "\t\t\t\t//var s = document.styleSheets.item(0);\n"
                        writer.codewritein "\t\t\t\t//s.cssRules[0].style.backgroundColor=\"blue\";\n"
                        writer.codewritein "\t\t\t\tvar vname = document.getElementById(\"textvar\").value;\n"
                        writer.codewritein "\t\t\t\tvar targets = document.getElementsByClassName(vname);\n"
                        writer.codewritein "\t\t\t\tfor (i = 0; i < targets.length; i++)\n"
                        writer.codewritein "\t\t\t\t{\n"
                        writer.codewritein "\t\t\t\t\ttargets[i].style.color =  '#ff0000';\n"
                        writer.codewritein "\t\t\t\t}\n"
                        writer.codewritein "\t\t\t}\n"
                        writer.codewritein "\t\t</script>\n"
                        writer.codewritein "\t</head>\n"
                        writer.codewritein "\t<body>\n"
                        writer.codewritein("\t\t<h1>" + encodedProjectName + "</h1>\n")
                        writer.codewritein "\t\t<div id=\"codeinfo\">\n"
                        writer.codewritein "\t\t\t<ul>\n"
                        writer.codewritein("\t\t\t\t<li>Project version: " + HtmlEncoding.textContent codever + "</li>\n")
                        writer.codewritein "\t\t\t\t<li>Generated by Aqualis (algorithm and equation analyzer for lightwave simulation)</li>\n"
                        writer.codewritein("\t\t\t\t<li>Aqualis version: " + Aqualis.Version + "</li>\n")
                        writer.codewritein "\t\t\t</ul>\n"
                        writer.codewritein "\t\t</div>\n"
                        //構造体の定義
                        writer.codewritein "\t\t<div id=\"defstr\">\n"
                        writer.codewritein "\t\t<h2>構造体定義</h2>\n"
                        context.str.Def_Structure writer
                        writer.codewritein "\t\t</div>\n"
                        //関数定義
                        writer.codewritein "\t\t<div id=\"deffunc\">\n"
                        writer.codewritein "\t\t<h2>関数定義</h2>\n"
                        for funname in context.flist.list do
                            let functionPath = Path.Combine(context.IntermediateDirectory, funname + "_main")
                            writer.codewritein(File.ReadAllText functionPath)
                            File.Delete functionPath
                            writer.codewritein "\n"
                        writer.codewritein "\t\t</div>\n"
                        //グローバル変数の定義
                        writer.codewritein "\t\t<div id=\"defvar\">\n"
                        writer.codewritein "\t\t<h2>グローバル変数</h2>\n"
                        writer.codewritein "\t\t<ul>\n"
                        declareall context writer
                        writer.codewritein "\t\t</ul>\n"
                        writer.codewritein "\t\t</div>\n"
                        //メインコード
                        writer.codewritein "\t\t<div id=\"maincode\">\n"
                        writer.codewritein "\t\t<h2>メインコード</h2>\n"
                        writer.codewritein "<input type=\"text\" id=\"textvar\" value=\"\">\n"
                        writer.codewritein "<input type=\"button\" onclick=\"fsearch()\" value=\"Search\">\n"
                        writer.codewritein "<br/>\n"
                        match context.allCodes with |Some s -> writer.codewritein s |None -> ()
                        writer.codewritein "\t\t</div>\n"
                        writer.codewritein "\t</body>\n"
                        writer.codewritein "</html>\n"
                        writer.close()
                        //beeファイル削除
                        context.delete()
                        writer.publish()
                |HTMLSequenceDiagram ->
                    // 出力レイアウト作成
                    let layout = WebOutputLayout.create outputDirectory projectname
                    // HTML本体の一時出力
                    Aqualis.makeIntermediateProgramInDirectoryWithContext
                        (dir, layout.BodyTemporaryFileName, HTMLSequenceDiagram)
                        outputDirectory
                    <| fun body ->
                        code body
                        let bodyCode = body.allCodes |> Option.defaultValue ""
                        Aqualis.makeAtomicProgramInDirectoryWithContext
                            (dir, layout.MainFileName, HTMLSequenceDiagram)
                            outputDirectory
                        <| fun main ->
                            // html書き込みストリーム作成
                            main.writein "<!DOCTYPE html>"
                            // html要素
                            main.html.tagb ("html", [Atr("lang", "ja")]) <| fun () ->
                                // head要素
                                main.html.tagb "head" <| fun () ->
                                    // metaタグ
                                    main.writein "<meta charset=\"UTF-8\">"
                                    //追加（5/29）viewportタブ
                                    main.writein "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0, maximum-scale=1.0, minimum-scale=1.0\">"
                                    // titleタグ
                                    main.html.tagb "title" <| fun () ->
                                        main.html.text projectname
                                    HtmlAssetRendering.write "" main.writein body.htmlAssets
                                // body要素
                                let s0 = Style [area.backGroundColor "#ffffff"]
                                main.html.tagb ("body", [s0.atr]) <| fun () ->
                                    main.writein bodyCode
                |Python ->
                    Aqualis.makeIntermediateProgramInDirectoryWithContext
                        (dir, projectname, Python)
                        outputDirectory
                    <| fun context ->
                        //メインコード生成
                        code context
                        context.close()
                        //ソースファイル出力
                        use writer = codeWriter.CreateAtomic(Path.Combine(outputDirectory, projectname + ".py"), 2, Python)
                        writer.codewritein "#=============================================================================================\n"
                        writer.codewritein("# Project name: " + projectname + "\n")
                        writer.codewritein("# Project version: " + codever + "\n")
                        writer.codewritein "#---------------------------------------------------------------------------------------------\n"
                        writer.codewritein "# Generated by Aqualis (algorithm and equation analyzer for lightwave simulation)\n"
                        writer.codewritein("# Aqualis version: " + Aqualis.Version + "\n")
                        writer.codewritein "#=============================================================================================\n"
                        //基本モジュールと、生成中に登録されたオプション依存を出力
                        ["numpy"; "math"; "cmath"; "copy"; "struct"; "re"; "sys"]
                        |> List.iter context.pythonImports.RequireModule
                        context.pythonImports.Lines
                        |> List.iter (fun importLine -> writer.codewritein(importLine + "\n"))
                        //ヘッダファイルのインクルード
                        List.iter (fun (s:string) -> writer.codewritein("import " + s + "\n")) <| context.hlist.list
                        //構造体の定義
                        context.str.Def_Structure writer
                        //グローバル変数の定義
                        declareall context writer
                        //関数定義
                        for funname in context.flist.list do
                            let functionPath = Path.Combine(context.IntermediateDirectory, funname + "_main")
                            writer.codewritein(File.ReadAllText functionPath)
                            File.Delete functionPath
                            writer.codewritein("\n")
                        //メインコード
                        match context.allCodes with |Some s -> writer.codewritein s |None -> ()
                        writer.close()
                        //beeファイル削除
                        context.delete()
                        writer.publish()
                        use wr = ShellScriptWriter.create(Path.Combine(outputDirectory, "proc_" + projectname + "_P.sh"))
                        wr.WriteLine "#!/bin/bash"
                        wr.WriteLine()
                        ShellScriptWriter.writeExec
                            wr
                            "python3"
                            ["--"; projectname + ".py"]
                |JavaScript ->
                    Aqualis.makeIntermediateProgramInDirectoryWithContext
                        (dir, projectname, JavaScript)
                        outputDirectory
                    <| fun context ->
                        //メインコード生成
                        context.indentInc()
                        code context
                        context.indentDec()
                        context.close()
                        //ソースファイル出力
                        use writer = codeWriter.CreateAtomic(Path.Combine(outputDirectory, projectname + ".js"), 2, JavaScript)
                        writer.codewritein "/*=============================================================================================*/\n"
                        writer.codewritein("/* Project name: " + projectname + " */\n")
                        writer.codewritein("// Project version: " + codever + "\n")
                        writer.codewritein "/*---------------------------------------------------------------------------------------------*/\n"
                        writer.codewritein "/* Generated by Aqualis (algorithm and equation analyzer for lightwave simulation) */\n"
                        writer.codewritein("/* Aqualis version: " + Aqualis.Version + " */\n")
                        writer.codewritein "/*=============================================================================================*/\n"
                        //グローバル変数の宣言
                        declareall context writer
                        //関数定義
                        for funname in context.flist.list do
                            let functionPath = Path.Combine(context.IntermediateDirectory, funname + "_main")
                            writer.codewritein (File.ReadAllText functionPath)
                            File.Delete functionPath
                            writer.codewritein ("\n")
                        //Main
                        match context.allCodes with |Some s -> writer.codewritein s |None -> ()
                        writer.close()
                        //beeファイル削除
                        context.delete()
                        writer.publish()
                |PHP ->
                    Aqualis.makeAtomicProgramInDirectoryWithContext
                        (dir, projectname + ".php", PHP)
                        outputDirectory
                    <| fun context ->
                        code context
                        context.close()
                |Numeric -> Aqualis.runWithWriterlessContext Numeric code
            transaction.Commit()
