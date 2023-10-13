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
    
    type txtWriter(fp:string) =
        let mutable colcounter = 0
        member _.br with get() =
            let p = p.param
            match p.lang with
            |F -> 
                p.codewrite("write("+fp+",*)"+"\n")
            |C -> 
                p.codewrite("fprintf("+fp+",\"\\n\");\n")
            |_ -> ()
            colcounter <- 0
        member _.Write (v:int0) =
            let p = p.param
            match p.lang with
            |F ->
                let tab = var.ip0_noWarning("tab",2313)
                let int0string_format_F = 
                    let a=p.int_string_format
                    "I"+a.ToString()
                if colcounter<>0 then 
                    p.codewrite("write("+fp+",fmt='(A1)',advance='no') "+tab.code+"\n")
                p.codewrite("write("+fp+",fmt='("+int0string_format_F+")',advance='no') "+v.code+"\n")
            |C ->
                let int0string_format_C =
                    "%"+p.int_string_format.ToString()+"d"
                if colcounter<>0 then 
                    p.codewrite("fprintf("+fp+",\"\\t\");\n")
                p.codewrite("fprintf("+fp+",\""+int0string_format_C+"\""+v.code+");\n")
            |T ->
                p.codewrite("write("+fp+") "+v.code+"\n")
            |H ->
                p.codewrite("<span class=\"fio\">"+fp+"</span><math><mo>&larr;</mo>"+v.code+"</math>\n<br/>\n")
            colcounter <- colcounter+1
        member this.Write (v:int) = this.Write (v.I)
                    
        member _.Write (v:float0) =
            let p = p.param
            match p.lang with
            |F ->
                let tab = var.ip0_noWarning("tab",2313)
                let double0string_format_F = 
                    let (a,b)=p.double_string_format
                    "E"+a.ToString()+"."+b.ToString()+"e3"
                if colcounter<>0 then 
                    p.codewrite("write("+fp+",fmt='(A1)',advance='no') "+tab.code+"\n")
                p.codewrite("write("+fp+",fmt='("+double0string_format_F+")',advance='no') "+v.code+"\n")
            |C ->
                let double0string_format_C = 
                    let (a,b)=p.double_string_format
                    "%"+a.ToString()+"."+b.ToString()+"e"
                if colcounter<>0 then 
                    p.codewrite("fprintf("+fp+",\"\\t\");\n")
                p.codewrite("fprintf("+fp+",\""+double0string_format_C+"\""+v.code+");\n")
            |T ->
                p.codewrite("write("+fp+") "+v.code+"\n")
            |H ->
                p.codewrite("<span class=\"fio\">"+fp+"</span><math><mo>&larr;</mo>"+v.code+"</math>\n<br/>\n")
            colcounter <- colcounter+1
        member this.Write (v:double) = this.Write (v.D)
                
        member _.Write (v:complex0) =
            let p = p.param
            match p.lang with
            |F ->
                let tab = var.ip0_noWarning("tab",2313)
                let double0string_format_F = 
                    let (a,b)=p.double_string_format
                    "E"+a.ToString()+"."+b.ToString()+"e3"
                if colcounter<>0 then 
                    p.codewrite("write("+fp+",fmt='(A1)',advance='no') "+tab.code+"\n")
                p.codewrite("write("+fp+",fmt='("+double0string_format_F+")',advance='no') "+v.re.code+"\n")
                p.codewrite("write("+fp+",fmt='(A1)',advance='no') "+tab.code+"\n")
                p.codewrite("write("+fp+",fmt='("+double0string_format_F+")',advance='no') "+v.im.code+"\n")
            |C ->
                let double0string_format_C = 
                    let (a,b)=p.double_string_format
                    "%"+a.ToString()+"."+b.ToString()+"e"
                if colcounter<>0 then 
                    p.codewrite("fprintf("+fp+",\"\\t\");\n")
                p.codewrite("fprintf("+fp+",\""+double0string_format_C+"\""+v.code+");\n")
                p.codewrite("fprintf("+fp+",\"\\t\");\n")
                p.codewrite("fprintf("+fp+",\""+double0string_format_C+"\""+v.code+");\n")
            |T ->
                p.codewrite("write("+fp+") "+v.code+"\n")
            |H ->
                p.codewrite("<span class=\"fio\">"+fp+"</span><math><mo>&larr;</mo>"+v.code+"</math>\n<br/>\n")
            colcounter <- colcounter+2
            
        member this.Write (v:num0) =
            match v.etype with
            |It _ -> this.Write (int0(v.expr))
            |Dt _ -> this.Write (float0(v.expr))
            |Zt _ -> this.Write (complex0(v.expr))
            |_ -> printfn "%s: ファイル書き込みできない型です" <| v.etype.ToString()
            
    type binWriter(fp:string) =
        member _.Write (v:int0) =
            let p = p.param
            match p.lang,v.expr with
            |F,_ ->
                p.codewrite("write("+fp+") "+v.code+"\n")
            |C,Var _ ->
                p.codewrite("fwrite(&"+v.code+",sizeof("+v.code+"),1,"+fp+");\n")
            |C,_ ->
                ch.i <| fun tmp ->
                    tmp <== int0(v.expr)
                    p.codewrite("fwrite(&"+tmp.code+",sizeof("+tmp.code+"),1,"+fp+");\n")
            |T,_ ->
                p.codewrite("write("+fp+") "+v.code+"\n")
            |H,_ ->
                p.codewrite("<span class=\"fio\">"+fp+"</span><math><mo>&larr;</mo>"+v.code+"</math>\n<br/>\n")
        member this.Write (v:int) = this.Write (v.I)
        member _.Write (v:float0) =
            let p = p.param
            match p.lang,v.expr with
            |F,_ ->
                p.codewrite("write("+fp+") "+v.code+"\n")
            |C,Var _ ->
                p.codewrite("fwrite(&"+v.code+",sizeof("+v.code+"),1,"+fp+");\n")
            |C,_ ->
                ch.d <| fun tmp ->
                    tmp <== float0(v.expr)
                    p.codewrite("fwrite(&"+tmp.code+",sizeof("+tmp.code+"),1,"+fp+");\n")
            |T,_ ->
                p.codewrite("write("+fp+") "+v.code+"\n")
            |H,_ ->
                p.codewrite("<span class=\"fio\">"+fp+"</span><math><mo>&larr;</mo>"+v.code+"</math>\n<br/>\n")
        member this.Write (v:double) = this.Write (v.D)
        member _.Write (v:complex0) =
            let p = p.param
            match p.lang with
            |F ->
                p.codewrite("write("+fp+") "+v.re.code+"\n")
                p.codewrite("write("+fp+") "+v.im.code+"\n")
            |C ->
                ch.dd <| fun (re,im) ->
                    re <== v.re
                    im <== v.im
                    p.codewrite("fwrite(&"+re.code+",sizeof("+re.code+"),1,"+fp+");\n")
                    p.codewrite("fwrite(&"+im.code+",sizeof("+im.code+"),1,"+fp+");\n")
            |_ ->
                ()
    type txtReader(fp:string,iostat:int0) =
        let mutable colcounter = 0
        member _.br with get() =
            let p = p.param
            match p.lang with
            |F -> 
                p.codewrite("read("+fp+",*)"+"\n")
            |_ -> ()
            colcounter <- 0
        member _.Read (v:int0) = 
            let p = p.param
            let int0string_format_F = 
                let a=p.int_string_format
                "I"+a.ToString()
            let int0string_format_C = "%d"
            match p.lang with
            |F ->
                ch.i <| fun tmp ->
                    if colcounter<>0 then
                        p.codewrite("read("+fp+",fmt='(A1)',advance='no',iostat="+iostat.code+") "+tmp.code+"\n")
                    p.codewrite("read("+fp+",fmt='("+int0string_format_F+")',advance='no',iostat="+iostat.code+") "+v.code+"\n")
            |C ->
                p.codewrite("fscanf("+fp+",\""+int0string_format_C+"\","+v.code+");\n")
            |T ->
                p.codewrite("read("+fp+") "+v.code+"\n")
            |H ->
                p.codewrite("<math>"+v.code+"<mo>&larr;</mo></math><span class=\"fio\">"+fp+"</span>\n<br/>\n")
            colcounter <- colcounter+1
        member _.Read (v:float0) = 
            let p = p.param
            let double0string_format_F = 
                let (a,b)=p.double_string_format
                "E"+a.ToString()+"."+b.ToString()+"e3"
            let double0string_format_C = "%lf"
            match p.lang with
            |F ->
                ch.i <| fun tmp ->
                    if colcounter<>0 then
                        p.codewrite("read("+fp+",fmt='(A1)',advance='no',iostat="+iostat.code+") "+tmp.code+"\n")
                    p.codewrite("read("+fp+",fmy='("+double0string_format_F+")',advance='no',iostat="+iostat.code+") "+v.code+"\n")
            |C ->
                p.codewrite("fscanf("+fp+",\""+double0string_format_C+"\","+v.code+");\n")
            |T ->
                p.codewrite("read("+fp+") "+v.code+"\n")
            |H ->
                p.codewrite("<math>"+v.code+"<mo>&larr;</mo></math><span class=\"fio\">"+fp+"</span>\n<br/>\n")
            colcounter <- colcounter+1
        member _.Read (v:complex0) = 
            let p = p.param
            let tab = var.ip0_noWarning("tab",2313)
            let double0string_format_F = 
                let (a,b)=p.double_string_format
                "E"+a.ToString()+"."+b.ToString()+"e3"
            let int0string_format_F = 
                let a=p.int_string_format
                "I"+a.ToString()
            let int0string_format_C = "%d"
            let double0string_format_C = "%lf"
            match p.lang with
            |F ->
                ch.i <| fun tmp ->
                ch.dd <| fun (re,im) ->
                    if colcounter<>0 then
                        p.codewrite("read("+fp+",fmt='(A1)',advance='no',iostat="+iostat.code+") "+tmp.code+"\n")
                    p.codewrite("read("+fp+",fmt='("+double0string_format_F+")',advance='no',iostat="+iostat.code+") "+re.code+"\n")
                    p.codewrite("read("+fp+",fmt='("+double0string_format_F+")',advance='no',iostat="+iostat.code+") "+im.code+"\n")
                    v <== re+asm.uj*im
            |C ->
                ch.dd <| fun (re,im) ->
                    p.codewrite("fscanf("+fp+",\""+double0string_format_C+"\","+re.code+");\n")
                    p.codewrite("fscanf("+fp+",\""+double0string_format_C+"\","+im.code+");\n")
                    v <== re+asm.uj*im
            |T ->
                p.codewrite("read("+fp+") "+v.code+"\n")
            |H ->
                p.codewrite("<math>"+v.code+"<mo>&larr;</mo></math><span class=\"fio\">"+fp+"</span>\n<br/>\n")
            colcounter <- colcounter+2
            
    type binReader(fp:string,iostat:int0) =
        member _.Read (v:int0) = 
            let p = p.param
            match p.lang with
            |F ->
                match v.expr with 
                |Var n ->
                    p.codewrite("read("+fp+",iostat="+iostat.code+") "+n+"\n")
                |_ -> 
                    Console.WriteLine("ファイル読み込みデータの保存先が変数ではありません")
            |C ->
                match v.expr with 
                |Var n ->
                    p.codewrite("fread(&"+n+",sizeof("+n+"),1,"+fp+");"+"\n")
                |_ -> 
                    Console.WriteLine("ファイル読み込みデータの保存先が変数ではありません")
            |T ->
                match v.expr with 
                |Var n ->
                    p.codewrite("read("+fp+",iostat="+iostat.code+") "+n+"\n")
                |_ -> 
                    Console.WriteLine("ファイル読み込みデータの保存先が変数ではありません")
            |H ->
                match v.expr with 
                |Var n ->
                    p.codewrite("<math>"+n+"<mo>&larr;</mo></math><span class=\"fio\">"+fp+"</span>\n<br/>\n")
                |_ -> 
                    Console.WriteLine("ファイル読み込みデータの保存先が変数ではありません")
        member _.Read (v:float0) = 
            let p = p.param
            match p.lang with
            |F ->
                match v.expr with 
                |Var n ->
                    p.codewrite("read("+fp+",iostat="+iostat.code+") "+n+"\n")
                |_ -> 
                    Console.WriteLine("ファイル読み込みデータの保存先が変数ではありません")
            |C ->
                match v.expr with 
                |Var n ->
                    p.codewrite("fread(&"+n+",sizeof("+n+"),1,"+fp+");"+"\n")
                |_ -> 
                    Console.WriteLine("ファイル読み込みデータの保存先が変数ではありません")
            |T ->
                match v.expr with 
                |Var n ->
                    p.codewrite("read("+fp+",iostat="+iostat.code+") "+n+"\n")
                |_ -> 
                    Console.WriteLine("ファイル読み込みデータの保存先が変数ではありません")
            |H ->
                match v.expr with 
                |Var n ->
                    p.codewrite("<math>"+n+"<mo>&larr;</mo></math><span class=\"fio\">"+fp+"</span>\n<br/>\n")
                |_ -> 
                    Console.WriteLine("ファイル読み込みデータの保存先が変数ではありません")
        member _.Read (v:complex0) = 
            let p = p.param
            match p.lang with
            |F ->
                match v.expr with 
                |Var _ ->
                    ch.dd <| fun (re,im) ->
                        p.codewrite("read("+fp+",iostat="+iostat.code+") "+re.code+"\n")
                        p.codewrite("read("+fp+",iostat="+iostat.code+") "+im.code+"\n")
                        v <== re+asm.uj*im
                |_ -> 
                    Console.WriteLine("ファイル読み込みデータの保存先が変数ではありません")
            |C ->
                match v.expr with 
                |Var _ ->
                    ch.dd <| fun (re,im) ->
                        p.codewrite("fread(&"+re.code+",sizeof("+re.code+"),1,"+fp+");"+"\n")
                        p.codewrite("fread(&"+im.code+",sizeof("+im.code+"),1,"+fp+");"+"\n")
                        v <== re+asm.uj*im
                |_ -> 
                    Console.WriteLine("ファイル読み込みデータの保存先が変数ではありません")
            |T ->
                match v.expr with 
                |Var _ ->
                    ch.dd <| fun (re,im) ->
                        p.codewrite("read("+fp+",iostat="+iostat.code+") "+re.code+"\n")
                        p.codewrite("read("+fp+",iostat="+iostat.code+") "+im.code+"\n")
                        v <== re+asm.uj*im
                |_ -> 
                    Console.WriteLine("ファイル読み込みデータの保存先が変数ではありません")
            |H ->
                match v.expr with 
                |Var n ->
                    p.codewrite("<math>"+n+"<mo>&larr;</mo></math><span class=\"fio\">"+fp+"</span>\n<br/>\n")
                |_ -> 
                    Console.WriteLine("ファイル読み込みデータの保存先が変数ではありません")
        member _.Read_byte (e:int0) = 
            let p = p.param
            p.codewrite("read("+fp+", iostat="+iostat.code+") byte_tmp\n")
            let ee =
                match e.expr with 
                |Var n -> n 
                |_ -> "byte値を整数型以外の変数に格納できません"
            p.codewrite(ee + "=" + "byte_tmp\n")
            
    ///<summary>ファイル入出力</summary>
    type io () =
        
        static member private cat (con:string) (lst:string list) = [0..lst.Length-1] |> List.fold (fun acc i -> acc + (if i=0 then "" else con) + lst.[i]) ""
            
        static member private fileAccess (fileformat:string,filename:list<int0>) readmode isbinary code =
            let count = fileformat.Length - (fileformat.Replace("#","")).Length
            if filename.Length <> count then
                printfn "%s" <| "ファイル名が不正です。「"+fileformat+"」内の#の数は"+count.ToString()+"ですが、置き換える変数の数は"+filename.Length.ToString()+"です。"
            else
                let p = p.param
                match p.lang with
                |F ->
                    p.fcache <| fun fp ->
                        let rec divlist (slist:list<string>) (cat:string) (c:list<char>) =
                            match c,cat with
                            |'#'::lst,"" ->
                                divlist (slist@["#"]) "" lst
                            |'#'::lst,_ ->
                                divlist (slist@[cat;"#"]) "" lst
                            |u::lst,_ ->
                                divlist slist (cat+u.ToString()) lst
                            |[],"" ->
                                slist
                            |[],_ ->
                                slist@[cat]
                        let v = divlist [] "" (Array.toList(fileformat.ToCharArray()))
                        let f = 
                            v
                            |> List.map (fun s -> if s="#" then "I"+p.int_string_format.ToString()+"."+p.int_string_format.ToString() else "A")
                            |> fun p -> String.Join(",",p)
                        let s = 
                            v
                            |> List.fold (fun (lst,i) s -> if s="#" then (lst@[filename[i].code],i+1) else (lst@["\""+s+"\""],i)) ([],0)
                            |> fun (a,b) -> a
                            |> fun p -> String.Join(",",p)
                        p.tcache <| A0 <| fun id ->
                            let btname = "byte_tmp"
                            //変数byte_tmpをリストに追加（存在していない場合のみ）
                            p.var.setVar(Structure("integer(1)"),A0,btname,"")
                            p.codewrite("write("+id+", fmt='("+f+")') "+s+"\n")
                            if isbinary then
                                p.codewrite("open("+fp+", file=trim("+id+"), access='stream', form='unformatted')"+"\n")
                            else
                                p.codewrite("open("+fp+", file=trim("+id+"))"+"\n")
                            code(fp)
                            p.codewrite("close("+fp+")"+"\n")
                |C ->
                    p.fcache <| fun fp ->
                        let f = fileformat.Replace("#","\",(I"+p.int_string_format.ToString()+"),\"")
                        let s = 
                            [for s in filename -> s.code ]
                            |> io.cat ","
                        p.tcache <| A0 <| fun id ->
                            let btname = "byte_tmp"
                            //変数byte_tmpをリストに追加（存在していない場合のみ）
                            p.var.setVar(Structure("char"),A0,btname,"")
                            p.codewrite("sprintf("+id+",\""+f+"\""+(if s="" then "" else ",")+s+");\n")
                            if isbinary then
                                p.codewrite(fp+" = "+"fopen("+id+",\""+(if readmode then "rb" else "wb")+"\");"+"\n")
                            else
                                p.codewrite(fp+" = "+"fopen("+id+",\""+(if readmode then "r" else "w")+"\");"+"\n")
                            code(fp)
                            p.codewrite("fclose("+fp+")"+";\n")
                |T ->
                    p.fcache <| fun fp ->
                        let f = fileformat.Replace("#","\",(I"+p.int_string_format.ToString()+"),\"")
                        let s = 
                            filename
                            |> List.map (fun s -> s.code)
                            |> io.cat ","
                        p.tcache <| A0 <| fun id ->
                            let btname = "byte_tmp"
                            //変数byte_tmpをリストに追加（存在していない場合のみ）
                            p.var.setVar(Structure("char"),A0,btname,"")
                            p.codewrite("sprintf("+id+",\""+f+"\","+s+");\n")
                            if isbinary then
                                p.codewrite(fp+" = "+"fopen("+id+",\""+(if readmode then "rb" else "wb")+"\");"+"\n")
                            else
                                p.codewrite(fp+" = "+"fopen("+id+",\""+(if readmode then "r" else "w")+"\");"+"\n")
                            code(fp)
                            p.codewrite("fclose $"+fp+" "+"$\n")
                |H ->
                    p.fcache <| fun fp ->
                        let s = 
                            filename
                            |> List.map (fun s -> s.code)
                            |> io.cat ","
                        p.codewrite("<span class=\"fio\">file open</span><span class=\"fio\">"+fp+"</span><math><mo>=</mo>"+s+"</math>"+"\n<br/>\n")
                        code(fp)
                        p.codewrite("<span class=\"fio\">file close</span><span class=\"fio\">"+fp+"</span><math></math>\n<br/>\n")
                
        ///<summary>ファイル出力</summary>
        static member fileOutput (filename:string*list<int0>) code =
            io.fileAccess filename false false <| fun fp ->
                let wr = txtWriter(fp)
                code(wr)
                
        ///<summary>ファイル出力</summary>
        static member binfileOutput (filename:string*list<int0>) code =
            io.fileAccess filename false true <| fun fp ->
                let wr = binWriter(fp)
                code(wr)

        ///<summary>ファイル読み込み</summary>
        static member fileInput (filename:string*list<int0>) code =
            ch.i <| fun iostat ->
                io.fileAccess filename true false <| fun fp ->
                    let rd = txtReader(fp,iostat)
                    code(rd)
                    
        ///<summary>バイナリファイルの読み込み</summary>
        static member binfileInput (filename:string*list<int0>) code =
            ch.i <| fun iostat ->
                io.fileAccess filename true true <| fun fp ->
                    let rd = binReader(fp,iostat)
                    code(rd)
                    
        ///<summary>配列をファイルに保存</summary>
        static member save_text (f:int3) =
            fun filename ->
                io.fileOutput filename <| fun wr -> 
                    iter.array f <| fun (i,j,k) ->
                        wr.Write i
                        wr.Write j
                        wr.Write k
                        wr.Write f[i,j,k]
                        wr.br
        ///<summary>配列をファイルに保存</summary>
        static member save_text (f:int2) =
            fun filename ->
                io.fileOutput filename <| fun wr -> 
                    iter.array f <| fun (i,j) ->
                        wr.Write i
                        wr.Write j
                        wr.Write f[i,j]
                        wr.br
                        
        ///<summary>配列をファイルに保存</summary>
        static member save_text (f:int1) =
            fun filename ->
                io.fileOutput filename <| fun wr -> 
                    iter.array f <| fun i -> 
                        wr.Write i
                        wr.Write f[i]
                        wr.br
                        
        ///<summary>数値をファイルに保存</summary>
        static member save_text (f:int0) =
            fun filename ->
                io.fileOutput filename <| fun wr -> 
                    wr.Write f
                    
        ///<summary>配列をファイルに保存</summary>
        static member save_text (f:float3) =
            fun filename ->
                io.fileOutput filename <| fun wr -> 
                    iter.array f <| fun (i,j,k) ->
                        wr.Write i
                        wr.Write j
                        wr.Write k
                        wr.Write f[i,j,k]
                        wr.br
        ///<summary>配列をファイルに保存</summary>
        static member save_text (f:float2) =
            fun filename ->
                io.fileOutput filename <| fun wr -> 
                    iter.array f <| fun (i,j) ->
                        wr.Write i
                        wr.Write j
                        wr.Write f[i,j]
                        wr.br
                        
        ///<summary>配列をファイルに保存</summary>
        static member save_text (f:float1) =
            fun filename ->
                io.fileOutput filename <| fun wr -> 
                    iter.array f <| fun i -> 
                        wr.Write i
                        wr.Write f[i]
                        wr.br
                        
        ///<summary>数値をファイルに保存</summary>
        static member save_text (f:float0) =
            fun filename ->
                io.fileOutput filename <| fun wr -> 
                    wr.Write f
                    
        ///<summary>配列をファイルに保存</summary>
        static member save_text (f:complex3) =
            fun filename ->
                io.fileOutput filename <| fun wr -> 
                    iter.array f <| fun (i,j,k) ->
                        wr.Write i
                        wr.Write j
                        wr.Write k
                        wr.Write f[i,j,k]
                        wr.br
        ///<summary>配列をファイルに保存</summary>
        static member save_text (f:complex2) =
            fun filename ->
                io.fileOutput filename <| fun wr -> 
                    iter.array f <| fun (i,j) ->
                        wr.Write i
                        wr.Write j
                        wr.Write f[i,j]
                        wr.br
                        
        ///<summary>配列をファイルに保存</summary>
        static member save_text (f:complex1) =
            fun filename ->
                io.fileOutput filename <| fun wr -> 
                    iter.array f <| fun i -> 
                        wr.Write i
                        wr.Write f[i]
                        wr.br
                        
        ///<summary>数値をファイルに保存</summary>
        static member save_text (f:complex0) =
            fun filename ->
                io.fileOutput filename <| fun wr -> 
                    wr.Write f
                    
        ///<summary>数値をファイルに保存</summary>
        static member save (f:int0) =
            fun filename ->
                io.binfileOutput filename <| fun wr ->
                    //データフォーマット
                    wr.Write 1
                    //データ型
                    wr.Write 1004
                    //データ次元
                    wr.Write 0
                    //データサイズ
                    wr.Write 1
                    //データ本体
                    wr.Write f
                    
        ///<summary>1次元データをファイルに保存</summary>
        static member save (f:int1) =
            fun filename ->
                io.binfileOutput filename <| fun wr ->
                    //データフォーマット
                    wr.Write 1
                    //データ型
                    wr.Write 1004
                    //データ次元
                    wr.Write 1
                    //データサイズ
                    wr.Write f.size1
                    //データ本体
                    iter.num f.size1 <| fun i ->
                        wr.Write f[i]
                            
        ///<summary>2次元データをファイルに保存</summary>
        static member save (f:int2) =
            fun filename ->
                io.binfileOutput filename <| fun wr ->
                    //データフォーマット
                    wr.Write 1
                    //データ型
                    wr.Write 1004
                    //データ次元
                    wr.Write 2
                    //データサイズ
                    wr.Write f.size1
                    wr.Write f.size2
                    //データ本体
                    iter.num f.size2 <| fun j ->
                        iter.num f.size1 <| fun i ->
                            wr.Write f[i,j]
                            
        ///<summary>3次元データをファイルに保存</summary>
        static member save (f:int3) =
            fun filename ->
                io.binfileOutput filename <| fun wr ->
                    //データフォーマット
                    wr.Write 1
                    //データ型
                    wr.Write 1004
                    //データ次元
                    wr.Write 3
                    //データサイズ
                    wr.Write f.size1
                    wr.Write f.size2
                    wr.Write f.size3
                    //データ本体
                    iter.num f.size3 <| fun k ->
                        iter.num f.size2 <| fun j ->
                            iter.num f.size1 <| fun i ->
                                wr.Write f[i,j,k]
                                
        ///<summary>数値をファイルに保存</summary>
        static member save (f:float0) =
            fun filename ->
                io.binfileOutput filename <| fun wr ->
                    //データフォーマット
                    wr.Write 1
                    //データ型
                    wr.Write 2000
                    //データ次元
                    wr.Write 0
                    //データサイズ
                    wr.Write 1
                    //データ本体
                    wr.Write f
                    
        ///<summary>1次元データをファイルに保存</summary>
        static member save (f:float1) =
            fun filename ->
                io.binfileOutput filename <| fun wr ->
                    //データフォーマット
                    wr.Write 1
                    //データ型
                    wr.Write 2000
                    //データ次元
                    wr.Write 1
                    //データサイズ
                    wr.Write f.size1
                    //データ本体
                    iter.num f.size1 <| fun i ->
                        wr.Write f[i]
                            
        ///<summary>2次元データをファイルに保存</summary>
        static member save (f:float2) =
            fun filename ->
                io.binfileOutput filename <| fun wr ->
                    //データフォーマット
                    wr.Write 1
                    //データ型
                    wr.Write 2000
                    //データ次元
                    wr.Write 2
                    //データサイズ
                    wr.Write f.size1
                    wr.Write f.size2
                    //データ本体
                    iter.num f.size2 <| fun j ->
                        iter.num f.size1 <| fun i ->
                            wr.Write f[i,j]
                            
        ///<summary>3次元データをファイルに保存</summary>
        static member save (f:float3) =
            fun filename ->
                io.binfileOutput filename <| fun wr ->
                    //データフォーマット
                    wr.Write 1
                    //データ型
                    wr.Write 2000
                    //データ次元
                    wr.Write 3
                    //データサイズ
                    wr.Write f.size1
                    wr.Write f.size2
                    wr.Write f.size3
                    //データ本体
                    iter.num f.size3 <| fun k ->
                        iter.num f.size2 <| fun j ->
                            iter.num f.size1 <| fun i ->
                                wr.Write f[i,j,k]
                                
        ///<summary>数値をファイルに保存</summary>
        static member save (f:complex0) =
            fun filename ->
                io.binfileOutput filename <| fun wr ->
                    //データフォーマット
                    wr.Write 1
                    //データ型
                    wr.Write 3000
                    //データ次元
                    wr.Write 0
                    //データサイズ
                    wr.Write 1
                    //データ本体
                    wr.Write f
                    
        ///<summary>1次元データをファイルに保存</summary>
        static member save (f:complex1) =
            fun filename ->
                io.binfileOutput filename <| fun wr ->
                    //データフォーマット
                    wr.Write 1
                    //データ型
                    wr.Write 3000
                    //データ次元
                    wr.Write 1
                    //データサイズ
                    wr.Write f.size1
                    //データ本体
                    iter.num f.size1 <| fun i ->
                        wr.Write f[i]
                            
        ///<summary>2次元データをファイルに保存</summary>
        static member save (f:complex2) =
            fun filename ->
                io.binfileOutput filename <| fun wr ->
                    //データフォーマット
                    wr.Write 1
                    //データ型
                    wr.Write 3000
                    //データ次元
                    wr.Write 2
                    //データサイズ
                    wr.Write f.size1
                    wr.Write f.size2
                    //データ本体
                    iter.num f.size2 <| fun j ->
                        iter.num f.size1 <| fun i ->
                            wr.Write f[i,j]
                            
        ///<summary>3次元データをファイルに保存</summary>
        static member save (f:complex3) =
            fun filename ->
                io.binfileOutput filename <| fun wr ->
                    //データフォーマット
                    wr.Write 1
                    //データ型
                    wr.Write 3000
                    //データ次元
                    wr.Write 3
                    //データサイズ
                    wr.Write f.size1
                    wr.Write f.size2
                    wr.Write f.size3
                    //データ本体
                    iter.num f.size3 <| fun k ->
                        iter.num f.size2 <| fun j ->
                            iter.num f.size1 <| fun i ->
                                wr.Write f[i,j,k]
                                
        ///<summary>数値をファイルから読み込み</summary>
        static member load (f:int0) =
            fun (filename,nlist) ->
                let reader (r:binReader) (nt:int) =
                    ch.i <| fun n ->
                        //データ型
                        r.Read n
                        br.if2 (n=.nt)
                            <| fun () ->
                                //データ次元
                                r.Read n
                                br.if2 (n=.0)
                                    <| fun () ->
                                        ch.i <| fun n1 ->
                                            //データサイズ
                                            r.Read n1
                                            //データ本体
                                            r.Read f
                                    <| fun () ->
                                        print.cn "Invalid data dimension"
                            <| fun () ->
                                print.cn (filename+": invalid data type")
                io.binfileInput (filename,nlist) <| fun r ->
                ch.i <| fun n ->
                    //データフォーマット
                    r.Read n
                    br.branch <| fun b ->
                        b.IF (n=.1) <| fun () ->
                            reader r 1004
                                
        ///<summary>数値をファイルから読み込み</summary>
        static member load (f:float0) =
            fun (filename,nlist) ->
                let reader (r:binReader) (nt:int) =
                    ch.i <| fun n ->
                        //データ型
                        r.Read n
                        br.if2 (n=.nt)
                            <| fun () ->
                                //データ次元
                                r.Read n
                                br.if2 (n=.0)
                                    <| fun () ->
                                        ch.i <| fun n1 ->
                                            //データサイズ
                                            r.Read n1
                                            //データ本体
                                            r.Read f
                                    <| fun () ->
                                        print.cn "Invalid data dimension"
                            <| fun () ->
                                print.cn (filename+": invalid data type")
                io.binfileInput (filename,nlist) <| fun r ->
                ch.i <| fun n ->
                    //データフォーマット
                    r.Read n
                    br.branch <| fun b ->
                        b.IF (n=.1) <| fun () ->
                            reader r 2000
                                
        ///<summary>数値をファイルから読み込み</summary>
        static member load (f:complex0) =
            fun (filename,nlist) ->
                let reader (r:binReader) (nt:int) =
                    ch.i <| fun n ->
                        //データ型
                        r.Read n
                        br.if2 (n=.nt)
                            <| fun () ->
                                //データ次元
                                r.Read n
                                br.if2 (n=.0)
                                    <| fun () ->
                                        ch.i <| fun n1 ->
                                            //データサイズ
                                            r.Read n1
                                            //データ本体
                                            ch.dd <| fun (re,im) ->
                                                r.Read re
                                                r.Read im
                                                f <== re + asm.uj*im
                                    <| fun () ->
                                        print.cn "Invalid data dimension"
                            <| fun () ->
                                print.cn (filename+": invalid data type")
                io.binfileInput (filename,nlist) <| fun r ->
                ch.i <| fun n ->
                    //データフォーマット
                    r.Read n
                    br.branch <| fun b ->
                        b.IF (n=.1) <| fun () ->
                            reader r 3000
                                
        ///<summary>1次元データをファイルから読み込み</summary>
        static member load (f:int1) =
            fun (filename,nlist) ->
                let reader (r:binReader) (nt:int) =
                    ch.i <| fun n ->
                        //データ型
                        r.Read n
                        br.if2 (n=.nt)
                            <| fun () ->
                                //データ次元
                                r.Read n
                                br.if2 (n=.1)
                                    <| fun () ->
                                        ch.i <| fun n1 ->
                                            //データサイズ
                                            r.Read n1
                                            f.allocate n1
                                            //データ本体
                                            iter.num f.size1 <| fun i ->
                                                r.Read f[i]
                                    <| fun () ->
                                        print.cn "Invalid data dimension"
                            <| fun () ->
                                print.cn (filename+": invalid data type")
                io.binfileInput (filename,nlist) <| fun r ->
                ch.i <| fun n ->
                    //データフォーマット
                    r.Read n
                    br.branch <| fun b ->
                        b.IF (n=.1) <| fun () ->
                            reader r 1004
                            
        ///<summary>1次元データをファイルから読み込み</summary>
        static member load (f:float1) =
            fun (filename,nlist) ->
                let reader (r:binReader) (nt:int) =
                    ch.i <| fun n ->
                        //データ型
                        r.Read n
                        br.if2 (n=.nt)
                            <| fun () ->
                                //データ次元
                                r.Read n
                                br.if2 (n=.1)
                                    <| fun () ->
                                        ch.i <| fun n1 ->
                                            //データサイズ
                                            r.Read n1
                                            f.allocate n1
                                            //データ本体
                                            iter.num f.size1 <| fun i ->
                                                r.Read f[i]
                                    <| fun () ->
                                        print.cn "Invalid data dimension"
                            <| fun () ->
                                print.cn (filename+": invalid data type")
                io.binfileInput (filename,nlist) <| fun r ->
                ch.i <| fun n ->
                    //データフォーマット
                    r.Read n
                    br.branch <| fun b ->
                        b.IF (n=.1) <| fun () ->
                                reader r 2000
                                
        ///<summary>1次元データをファイルから読み込み</summary>
        static member load (f:complex1) =
            fun (filename,nlist) ->
                let reader (r:binReader) (nt:int) =
                    ch.i <| fun n ->
                        //データ型
                        r.Read n
                        br.if2 (n=.nt)
                            <| fun () ->
                                //データ次元
                                r.Read n
                                br.if2 (n=.1)
                                    <| fun () ->
                                        ch.i <| fun n1 ->
                                            //データサイズ
                                            r.Read n1
                                            f.allocate n1
                                            //データ本体
                                            iter.num f.size1 <| fun i ->
                                                ch.dd <| fun (re,im) ->
                                                    r.Read re
                                                    r.Read im
                                                    f[i] <== re + asm.uj*im
                                    <| fun () ->
                                        print.cn "Invalid data dimension"
                            <| fun () ->
                                print.cn (filename+": invalid data type")
                io.binfileInput (filename,nlist) <| fun r ->
                ch.i <| fun n ->
                    //データフォーマット
                    r.Read n
                    br.branch <| fun b ->
                        b.IF (n=.1) <| fun () ->
                            reader r 3000
                            
        ///<summary>2次元データをファイルから読み込み</summary>
        static member load (f:int2) =
            fun (filename,nlist) ->
                let reader (r:binReader) (nt:int) =
                    ch.i <| fun n ->
                        //データ型
                        r.Read n
                        br.if2 (n=.nt)
                            <| fun () ->
                                //データ次元
                                r.Read n
                                br.if2 (n=.2)
                                    <| fun () ->
                                        ch.ii <| fun (n1,n2) ->
                                            //データサイズ
                                            r.Read n1
                                            r.Read n2
                                            f.allocate(n1,n2)
                                            //データ本体
                                            iter.num f.size2 <| fun j ->
                                                iter.num f.size1 <| fun i ->
                                                    r.Read f[i,j]
                                    <| fun () ->
                                        print.cn "Invalid data dimension"
                            <| fun () ->
                                print.cn (filename+": invalid data type")
                io.binfileInput (filename,nlist) <| fun r ->
                ch.i <| fun n ->
                    //データフォーマット
                    r.Read n
                    br.branch <| fun b ->
                        b.IF (n=.1) <| fun () ->
                            reader r 1004
                                
        ///<summary>2次元データをファイルから読み込み</summary>
        static member load (f:float2) =
            fun (filename,nlist) ->
                let reader (r:binReader) (nt:int) =
                    ch.i <| fun n ->
                        //データ型
                        r.Read n
                        br.if2 (n=.nt)
                            <| fun () ->
                                //データ次元
                                r.Read n
                                br.if2 (n=.2)
                                    <| fun () ->
                                        ch.ii <| fun (n1,n2) ->
                                            //データサイズ
                                            r.Read n1
                                            r.Read n2
                                            f.allocate(n1,n2)
                                            //データ本体
                                            iter.num f.size2 <| fun j ->
                                                iter.num f.size1 <| fun i ->
                                                    r.Read f[i,j]
                                    <| fun () ->
                                        print.cn "Invalid data dimension"
                            <| fun () ->
                                print.cn (filename+": invalid data type")
                io.binfileInput (filename,nlist) <| fun r ->
                ch.i <| fun n ->
                    //データフォーマット
                    r.Read n
                    br.branch <| fun b ->
                        b.IF (n=.1) <| fun () ->
                            reader r 2000
                                
        ///<summary>2次元データをファイルから読み込み</summary>
        static member load (f:complex2) =
            fun (filename,nlist) ->
                let reader (r:binReader) (nt:int) =
                    ch.i <| fun n ->
                        //データ型
                        r.Read n
                        br.if2 (n=.nt)
                            <| fun () ->
                                //データ次元
                                r.Read n
                                br.if2 (n=.2)
                                    <| fun () ->
                                        ch.ii <| fun (n1,n2) ->
                                            //データサイズ
                                            r.Read n1
                                            r.Read n2
                                            f.allocate(n1,n2)
                                            //データ本体
                                            iter.num f.size2 <| fun j ->
                                                iter.num f.size1 <| fun i ->
                                                    ch.dd <| fun (re,im) ->
                                                        r.Read re
                                                        r.Read im
                                                        f[i,j] <== re + asm.uj*im
                                    <| fun () ->
                                        print.cn "Invalid data dimension"
                            <| fun () ->
                                print.cn (filename+": invalid data type")
                io.binfileInput (filename,nlist) <| fun r ->
                ch.i <| fun n ->
                    //データフォーマット
                    r.Read n
                    br.branch <| fun b ->
                        b.IF (n=.1) <| fun () ->
                            reader r 3000
                                
        ///<summary>3次元データをファイルから読み込み</summary>
        static member load (f:int3) =
            fun (filename,nlist) ->
                let reader (r:binReader) (nt:int) =
                    ch.i <| fun n ->
                        //データ型
                        r.Read n
                        br.if2 (n=.nt)
                            <| fun () ->
                                //データ次元
                                r.Read n
                                br.if2 (n=.3)
                                    <| fun () ->
                                        ch.iii <| fun (n1,n2,n3) ->
                                            //データサイズ
                                            r.Read n1
                                            r.Read n2
                                            r.Read n3
                                            f.allocate(n1,n2,n3)
                                            //データ本体
                                            iter.num f.size3 <| fun k ->
                                                iter.num f.size2 <| fun j ->
                                                    iter.num f.size1 <| fun i ->
                                                        r.Read f[i,j,k]
                                    <| fun () ->
                                        print.cn "Invalid data dimension"
                            <| fun () ->
                                print.cn (filename+": invalid data type")
                io.binfileInput (filename,nlist) <| fun r ->
                ch.i <| fun n ->
                    //データフォーマット
                    r.Read n
                    br.branch <| fun b ->
                        b.IF (n=.1) <| fun () ->
                            reader r 1004
                                
                                
        ///<summary>3次元データをファイルから読み込み</summary>
        static member load (f:float3) =
            fun (filename,nlist) ->
                let reader (r:binReader) (nt:int) =
                    ch.i <| fun n ->
                        //データ型
                        r.Read n
                        br.if2 (n=.nt)
                            <| fun () ->
                                //データ次元
                                r.Read n
                                br.if2 (n=.3)
                                    <| fun () ->
                                        ch.iii <| fun (n1,n2,n3) ->
                                            //データサイズ
                                            r.Read n1
                                            r.Read n2
                                            r.Read n3
                                            f.allocate(n1,n2,n3)
                                            //データ本体
                                            iter.num f.size3 <| fun k ->
                                                iter.num f.size2 <| fun j ->
                                                    iter.num f.size1 <| fun i ->
                                                        r.Read f[i,j,k]
                                    <| fun () ->
                                        print.cn "Invalid data dimension"
                            <| fun () ->
                                print.cn (filename+": invalid data type")
                io.binfileInput (filename,nlist) <| fun r ->
                ch.i <| fun n ->
                    //データフォーマット
                    r.Read n
                    br.branch <| fun b ->
                        b.IF (n=.1) <| fun () ->
                            reader r 2000
                                
        ///<summary>3次元データをファイルから読み込み</summary>
        static member load (f:complex3) =
            fun (filename,nlist) ->
                let reader (r:binReader) (nt:int) =
                    ch.i <| fun n ->
                        //データ型
                        r.Read n
                        br.if2 (n=.nt)
                            <| fun () ->
                                //データ次元
                                r.Read n
                                br.if2 (n=.3)
                                    <| fun () ->
                                        ch.iii <| fun (n1,n2,n3) ->
                                            //データサイズ
                                            r.Read n1
                                            r.Read n2
                                            r.Read n3
                                            f.allocate(n1,n2,n3)
                                            //データ本体
                                            iter.num f.size3 <| fun k ->
                                                iter.num f.size2 <| fun j ->
                                                    iter.num f.size1 <| fun i ->
                                                        ch.dd <| fun (re,im) ->
                                                            r.Read re
                                                            r.Read im
                                                            f[i,j,k] <== re + asm.uj*im
                                    <| fun () ->
                                        print.cn "Invalid data dimension"
                            <| fun () ->
                                print.cn (filename+": invalid data type")
                io.binfileInput (filename,nlist) <| fun r ->
                ch.i <| fun n ->
                    //データフォーマット
                    r.Read n
                    br.branch <| fun b ->
                        b.IF (n=.1) <| fun () ->
                            reader r 3000
                                
    ///<summary>ファイル入出力（処理スキップ）</summary>
    type dummy_io () =
        
        static member fileOutput (filename:string*list<int0>) code = ()
        
        static member fileInput (filename:string*list<int0>) code = ()
        
        static member binfileInput (filename:string*list<int0>) code = ()
        
        static member binfileOutput (filename:string*list<int0>) code = ()
        