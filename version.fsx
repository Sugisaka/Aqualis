module preprocess

#if INTERACTIVE
#I "C:\\Aqualis\\lib"
#r "System.dll"
#load "setting.fsx"
#endif

open System
open System.IO

let version = "184.0.3.0"

let backup outputDir sourceDir sourceFile projectname (codever:string) = 
    //バックアップリストファイル
    let csvfile = setting.listdir+"\\"+"AqualisBackup.csv"
    //バックアップ先ディレクトリ
    let dlist =
        if setting.backupdir.Length=0 then
            None
        elif Directory.Exists setting.backupdir[0] then
            Some(setting.backupdir[0])
        else
            None
    //リストファイルが存在しない場合は新規作成
    match File.Exists(csvfile),dlist with
    |false,Some(dir) ->
        let wr = new StreamWriter(csvfile)
        let files = System.IO.Directory.GetFiles(dir,"*.fsx") |> Array.sortBy (fun file -> File.GetLastWriteTime(file))
        for file in files do
            wr.WriteLine(Path.GetFileName(file))
        wr.Close()
    |_ -> ()
    //同じプロジェクト名の既存のファイルを検索
    let flist =
        let ver = codever.Replace(".","_")
        let f1 = 
            let rd = new StreamReader(csvfile)
            let rec makeFileList lst =
                match rd.ReadLine() with
                |null ->
                    rd.Close()
                    lst
                |s when s.StartsWith("H_"+projectname+"_ver"+ver) ->
                    makeFileList (s::lst)
                |s ->
                    makeFileList lst
            makeFileList []
        let f2 = 
            Array.toList(Directory.GetFiles(outputDir,"H_"+projectname+"_ver"+ver+"*.fsx"))
        f1@f2
    //ビルド番号（最新版からインクリメント）
    let buildNum =
        [for i in flist do
            let k = i.Split([|'_'|],StringSplitOptions.RemoveEmptyEntries)
            let h = k[k.Length-1].Split([|'.'|],StringSplitOptions.RemoveEmptyEntries)
            yield
                try
                    Int32.Parse(h[0])
                with
                  | :? System.FormatException -> -1]
        |> (fun lst -> match lst with [] -> -1 |_ -> List.max lst)
        |> fun i -> i+1
    //バックアップファイル名
    let backupfile = "H_"+projectname+"_ver"+(codever.Replace(".","_"))+"_"+buildNum.ToString()+".fsx"
    //リストファイルに追記
    let wr = new StreamWriter(csvfile,true)
    wr.WriteLine(backupfile)
    wr.Close()
    //バックアップフォルダにスクリプトファイルとしてコピー
    match dlist with
    |Some(dir) ->
        System.IO.File.Copy(sourceDir+"\\"+sourceFile, dir+"\\"+backupfile, true)
        if setting.isCopySourceDir then
            System.IO.File.Copy(sourceDir+"\\"+sourceFile, outputDir+"\\"+backupfile, true)
    |None ->
        ()
    (version,codever+"."+buildNum.ToString())
