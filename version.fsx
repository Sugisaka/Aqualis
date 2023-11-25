module preprocess

#if INTERACTIVE
#I "C:\\Aqualis\\lib"
#r "System.dll"
#load "setting.fsx"
#endif

open System
open System.IO

let version = "183.0.0.0"

let backup outputdir sourceDir sourceFile projectname (codever:string) = 
    let rec d lst =
        match lst with
          |c::ls when Directory.Exists c -> Some(c) 
          |c::ls -> d ls
          |[] -> None
    //list of files with the same project name
    let dlist =
        match (d setting.backupdir) with
          |None ->
            [outputdir]
          |Some(dir) ->
            [outputdir;dir]
    let flist =
        dlist
        |> List.map (fun x -> System.IO.Directory.GetFiles(x,"H_"+projectname+"_ver"+codever.Replace(".","_")+"*.fsx"))
        |> List.map (fun x -> Array.toList(x))
        |> List.fold (fun acc x -> acc@x) []
    //ビルド番号（最新版からインクリメント）
    let buildNum =
        [for i in (flist) do
            let k = i.Split([|'_'|],StringSplitOptions.RemoveEmptyEntries)
            let h = k.[k.Length-1].Split([|'.'|],StringSplitOptions.RemoveEmptyEntries)
            yield
                try
                    Int32.Parse(h.[0])
                with
                  | :? System.FormatException -> -1]
        |> (fun lst -> match lst with [] -> -1 |_ -> List.max lst)
        |> fun i -> i+1
    //バックアップファイル名
    let backupfile = "H_"+projectname+"_ver"+(codever.Replace(".","_"))+"_"+buildNum.ToString()+".fsx"
    //バックアップフォルダにスクリプトファイルとしてコピー
    for d in dlist do
        System.IO.File.Copy(sourceDir+"\\"+sourceFile, d+"\\"+backupfile, true)
    (version,codever+"."+buildNum.ToString())
        