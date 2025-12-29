namespace Aqualis

open System
open System.IO

type php =
    /// phpファイルを生成
    static member phpfile (dir:string,projectname:string) code =
        nextProgram(dir, projectname, Fortran)
        code ()
        pr.cwriter.close()
        backProgram()
        
    static member fnvar (s:string) = num0(Var(Nt,s,NaN))
    static member fnvar (s:exprString) = 
        reduceExprString.reduce s
        |> List.map (fun s -> match s with |RStr t -> t |RNvr t -> t.eval pr)
        |> fun s -> String.Join (" + ", s)
        |> fun s -> php.fnvar s
        
    /// htmlコード内にphpコードを埋め込み
    static member phpcode (code:unit->unit) =
        pr.cwriter.codewrite "<?php "
        code()
        pr.cwriter.codewrite " ?>"
    /// POST送信されたデータを表示
    static member postCheck() = pr.cwriter.codewrite "<?php print_r($_POST) ?>"
    /// POST送信されたファイルを表示
    static member postFileCheck() = pr.cwriter.codewrite "<?php print_r($_FILES) ?>"
    /// 論理積
    static member And (x:list<bool0>) = bool0(Var(Nt, "(" + String.Join(" && ", x |> List.map (fun s -> s.Expr.eval pr)) + ")", NaN))
    /// 論理和
    static member Or (x:list<bool0>) = bool0(Var(Nt, "(" + String.Join(" || ", x |> List.map (fun s -> s.Expr.eval pr)) + ")", NaN))
    /// 指定された変数がPOST送信されたか判定
    static member isset (x:num0) = bool0(Var(Nt, "isset(" + x.Expr.eval pr + ")", NaN))
    /// 文字列を表示
    static member echo (x:string) = php.phpcode <| fun () -> pr.cwriter.codewrite("echo \"" + x + "\";")
    /// 変数を表示
    static member echo (x:num0) = php.phpcode <| fun () -> pr.cwriter.codewrite("echo " + x.Expr.eval pr + ";")
    /// ファイル内のテキストを取得
    static member file_get_contents (filename:num0) = num0(Var(Nt, "file_get_contents(" + filename.Expr.eval pr + ")", NaN))
    /// ファイル内のテキストを取得
    static member file_get_contents filename = num0(Var(Nt, "file_get_contents("+"\""+filename+"\""+")",NaN))
    /// ファイルにテキストを書き込み
    static member file_put_contents (filename:num0,x:num0) = php.phpcode <| fun () -> pr.cwriter.codewrite("file_put_contents("+filename.Expr.eval pr+","+x.Expr.eval pr+");")
    /// ファイルにテキストを書き込み
    static member file_put_contents (filename:string,x:num0) = php.phpcode <| fun () -> pr.cwriter.codewrite("file_put_contents("+"\""+filename+"\""+","+x.Expr.eval pr+");")
    /// JSONファイルをデコード
    static member json_decode (x:num0,p:bool) = num0(Var(Nt, "json_decode("+x.Expr.eval pr+","+p.ToString()+")", NaN))
    /// JSONファイルをエンコード
    static member json_encode (x:num0) = num0(Var(Nt, "json_encode("+x.Expr.eval pr+", JSON_PRETTY_PRINT|JSON_UNESCAPED_UNICODE|JSON_UNESCAPED_SLASHES )", NaN))
    /// 指定したキーの値の配列を生成
    static member array_column(data:num0,id:string) = num0(Var(Nt, "array_column("+data.Expr.eval pr+",\""+id+"\")", NaN))
    /// 指定した要素が含まれているか判定
    static member in_array_strict(s:num0, idArray:num0) = bool0(Var(Nt, "in_array("+s.Expr.eval pr+", "+idArray.Expr.eval pr+", true)", NaN))
    /// 指定した要素の配列内でのインデックス（キー）を検索
    static member array_search(s:num0, idArray:num0) = num0(Var(Nt, "array_search("+s.Expr.eval pr+", "+idArray.Expr.eval pr+")", NaN))
    /// 配列を生成
    static member array() = Var(Nt, "array()", NaN)
    /// 配列に要素を追加
    static member array_push(a:num0,el:num0) = php.phpcode <| fun () -> pr.cwriter.codewrite("array_push("+a.Expr.eval pr+","+el.Expr.eval pr+")")
    /// ファイル内のテキストを配列に格納
    static member file(filename:num0, flag:list<FileFlag>) = 
        num0(Var(Nt, "file("+filename.Expr.eval pr+", "+(flag |> List.map (fun s -> s.str) |> (fun p -> String.Join(" | ",p)) )+")", NaN))
    /// ファイル内のテキストを配列に格納
    static member file(filename:string, flag:list<FileFlag>) = 
        num0(Var(Nt, "file('"+filename+"', "+(flag |> List.map (fun s -> s.str) |> (fun p -> String.Join(" | ",p)) )+")", NaN))
    /// ファイルを開く
    static member fopen(filename:num0,rw:FileOpenMode) = num0(Var(Nt, "fopen("+filename.Expr.eval pr+", "+rw.str+")", NaN))
    /// ファイルを開く
    static member fopen(filename:string,rw:FileOpenMode) = num0(Var(Nt, "fopen(\""+filename+"\", "+rw.str+")", NaN))
    /// ファイルに書き込み
    static member fwrite(fp:num0,t:num0) = php.phpcode <| fun () -> pr.cwriter.codewrite("fpr.cwriter.codewrite("+fp.Expr.eval pr+", "+t.Expr.eval pr+");")
    /// ファイルに書き込み
    static member fwrite(fp:num0,t:string) = php.fwrite(fp,num0.str(t))
    /// ファイルに書き込み
    static member fwrite(fp:num0,t:int) = php.phpcode <| fun () -> pr.cwriter.codewrite("fpr.cwriter.codewrite("+fp.Expr.eval pr+", "+t.ToString()+");")
    /// ファイルにShift-JISで書き込み
    static member fwrite_SJIS(fp:num0,t:num0) = php.phpcode <| fun () -> pr.cwriter.codewrite("fpr.cwriter.codewrite("+fp.Expr.eval pr+", mb_convert_encoding("+t.Expr.eval pr+", 'SJIS-win', 'UTF-8'));")
    /// ファイルにShift-JISで書き込み
    static member fwrite_SJIS(fp:num0,t:string) = php.phpcode <| fun () -> pr.cwriter.codewrite("fpr.cwriter.codewrite("+fp.Expr.eval pr+", mb_convert_encoding(\""+t+"\", 'SJIS-win', 'UTF-8'));")
    /// ファイルを閉じる
    static member fclose(filename:num0) = php.phpcode <| fun () -> pr.cwriter.codewrite("fclose("+filename.Expr.eval pr+");")
    /// 正規表現
    static member preg_match(p:string,text:num0,mat:num0) = php.phpcode <| fun () -> pr.cwriter.codewrite("preg_match('"+p+"',"+text.Expr.eval pr+","+mat.Expr.eval pr+");")
    /// ファイルのダウンロード
    static member download(filename:string) =
        php.phpcode <| fun () -> pr.cwriter.codewrite "header('Content-Type: application/octet-stream');"
        php.phpcode <| fun () -> pr.cwriter.codewrite ("header('Content-Length: '.filesize(\""+filename+"\"));")
        php.phpcode <| fun () -> pr.cwriter.codewrite ("header('Content-Disposition: attachment; filename=\""+filename+"\"');")
        php.phpcode <| fun () -> pr.cwriter.codewrite ("readfile(\""+filename+"\");")
        php.phpcode <| fun () -> pr.cwriter.codewrite "exit;"
    /// 整数に変換
    static member intval(s:num0) = num0(Var(Nt, "intval("+s.Expr.eval pr+")", NaN))
    /// 配列要素の和
    static member array_sum(data:num0) = num0(Var(Nt, "array_sum("+data.Expr.eval pr+")", NaN))
    /// 文字数
    static member strlen(data:num0) = num0(Var(Nt, "strlen("+data.Expr.eval pr+")", NaN))
    /// 数値かどうか判定
    static member is_numeric(data:num0) = bool0(Var(Nt, "is_numeric("+data.Expr.eval pr+")", NaN))
    /// 否定演算
    static member nt (data:bool0) = bool0(Var(Nt, "!"+data.Expr.eval pr,NaN))
    ///<summary>送信データをキャッシュしない（Firefoxでフォームの選択肢がリロード前から保持される現象を回避）</summary>
    static member set_nocache() = php.phpcode <| fun () -> pr.cwriter.codewrite "header( 'Cache-Control: no-store, no-cache, must-revalidate' );"
    /// HTTPヘッダを取得
    static member header(data:num0) = php.phpcode <| fun () -> pr.cwriter.codewrite("header("+data.Expr.eval pr+")")
    /// HTTPヘッダを取得
    static member header(data:string) = php.phpcode <| fun () -> pr.cwriter.codewrite("header('"+data+"')")
    // 小数に変換
    // static member float(data:num0) = Var("(float)"+data.Expr.eval pr)
    // 絶対値
    // static member abs(data:num0) = Var("abs("+data.Expr.eval pr+")")
    /// 日付を取得
    static member date(fmt:string) = num0(Var(Nt, "date(\""+fmt+"\")", NaN))
    /// 整数に丸め
    static member round(x:num0) = num0(Var(Nt, "round("+x.Expr.eval pr+")", NaN))
    /// 文字列切り出し
    static member substr(x:num0,n:num0) = num0(Var(Nt, "substr("+x.Expr.eval pr+","+n.Expr.eval pr+")", NaN))
    /// 文字列切り出し
    static member substr(x:num0,n:int) = num0(Var(Nt, "substr("+x.Expr.eval pr+","+n.ToString()+")", NaN))
    /// ファイルが存在するか確認
    static member file_exists(x:num0) = bool0(Var(Nt, "file_exists("+x.Expr.eval pr+")", NaN))
    /// ファイルが存在するか確認
    static member file_exists(x:string) = bool0(Var(Nt, "file_exists(\""+x+"\")", NaN))
    /// 文字数（全角も1字扱い）
    static member mb_strlen(x:num0) = num0(Var(Nt, "mb_strlen("+x.Expr.eval pr+")", NaN))
    /// 文字数（全角も1字扱い）
    static member mb_strwidth(x:num0) = num0(Var(Nt, "mb_strwidth("+x.Expr.eval pr+")", NaN))
    /// 文字列比較
    static member strncmp(x:num0,y:string,n:int) = num0(Var(Nt, "strncmp("+x.Expr.eval pr+",\""+y+"\","+n.ToString()+")", NaN))
    /// 指定したパターンにマッチするファイルパス取得
    static member glob(x:num0) = num0(Var(Nt, "glob("+x.Expr.eval pr+")", NaN))
    /// 指定したパターンにマッチするファイルパス取得
    static member glob(x:string) = num0(Var(Nt, "glob(\""+x+"\")", NaN))
    /// 文字列分割
    static member explode(x:num0,y:num0) = num0(Var(Nt, "explode("+x.Expr.eval pr+","+y.Expr.eval pr+")", NaN))
    /// 文字列分割
    static member explode(x:string,y:num0) = num0(Var(Nt, "explode('"+x+"',"+y.Expr.eval pr+")", NaN))
    /// 配列のソート
    static member sort(data:num0) = php.phpcode <| fun () -> pr.cwriter.codewrite("sort("+data.Expr.eval pr+")")
    /// 整数に変換
    static member toint(x:num0) = num0(Var(Nt, "(int)"+x.Expr.eval pr, NaN))
    /// 配列要素数
    static member count(x:num0) = num0(Var(Nt, "count("+x.Expr.eval pr+")", NaN))
    /// 拡張子を除いたファイル名
    static member filename_withoutExtension(x:num0) = num0(Var(Nt, "pathinfo("+x.Expr.eval pr+", PATHINFO_FILENAME)", NaN))
    /// ファイル削除
    static member unlink(data:num0) = php.phpcode <| fun () -> pr.cwriter.codewrite("unlink("+data.Expr.eval pr+")")
    /// 配列要素をランダムに入れ替え
    static member shuffle(data:num0) = php.phpcode <| fun () -> pr.cwriter.codewrite("shuffle("+data.Expr.eval pr+")")
    /// タイムゾーン設定
    static member setTimeZone(location:string) = php.phpcode <| fun () -> pr.cwriter.codewrite("date_default_timezone_set('"+location+"');")
    /// メール送信
    static member sendMail(body:string,subject:string,smtp:string,fromAddress:string,toAddress:string) =
        let cmd = num0.var "cmd"
        //cmd <== "echo \\\""+body+"\\\" | s-nail -s \\\""+subject+"\\\" -Smta=smtp://"+smtp+":25 -r "+fromAddress+" "+toAddress
        cmd <== php.fnvar("echo \\\"" + body + "\\\" | mail -s \\\"" + subject + "\\\" -S smtp=smtp://" + smtp + ":25 -r " + fromAddress + " " + toAddress)
        php.phpcode <| fun () -> pr.cwriter.codewrite("exec("+cmd.Expr.eval pr+");")
    /// メール送信
    static member sendMail(body:num0,subject:num0,fromAddress:string,toAddress:num0) =
        php.phpcode <| fun () -> pr.cwriter.codewrite("mb_language(\"ja\");")
        php.phpcode <| fun () -> pr.cwriter.codewrite("mb_internal_encoding(\"UTF-8\");")
        php.phpcode <| fun () -> pr.cwriter.codewrite("mb_send_mail("+toAddress.Expr.eval pr+","+subject.Expr.eval pr+","+body.Expr.eval pr+","+"\"From: "+fromAddress+"\");")
    /// メール送信
    static member sendMail(body:string,subject:string,fromAddress:string,toAddress:string) =
        php.phpcode <| fun () -> pr.cwriter.codewrite("mb_language(\"ja\");")
        php.phpcode <| fun () -> pr.cwriter.codewrite("mb_internal_encoding(\"UTF-8\");")
        php.phpcode <| fun () -> pr.cwriter.codewrite("mb_send_mail(\""+toAddress+"\",\""+subject+"\",\""+body+"\","+"\"From: "+fromAddress+"\");")
    /// Discordへメッセージ送信
    static member sendDiscord(body:num0,webhookURL:num0) =
        let cmd = num0.var "cmd"
        cmd <== php.fnvar("curl -H \\\"Content-Type: application/json\\\" -X POST -d \\\"{\\\\\\\"username\\\\\\\": \\\\\\\"Ediass Notification\\\\\\\", \\\\\\\"content\\\\\\\": \\\\\\\""++body++"\\\\\\\"}\\\" "++webhookURL)
        php.phpcode <| fun () -> pr.cwriter.codewrite("exec("+cmd.Expr.eval pr+");")
    /// Discordへメッセージ送信
    static member sendDiscord(body:string,webhookURL:string) =
        let cmd = num0.var "cmd"
        cmd <== php.fnvar("curl -H \\\"Content-Type: application/json\\\" -X POST -d \\\"{\\\\\\\"username\\\\\\\": \\\\\\\"Ediass Notification\\\\\\\", \\\\\\\"content\\\\\\\": \\\\\\\"" + body + "\\\\\\\"}\\\" " + webhookURL)
        php.phpcode <| fun () -> pr.cwriter.codewrite("exec("+cmd.Expr.eval pr+");")
    /// 文字列置換
    static member str_replace(strfrom:string,strto:string,str:num0) = num0(Var(Nt, "str_replace("+"\""+strfrom+"\""+","+"\""+strto+"\""+","+str.Expr.eval pr+")", NaN))
    /// 指定した文字数になるまで文字を埋める
    static member str_pad(num:num0,ndigit:int,paddingnum:int) = num0(Var(Nt, "str_pad("+num.Expr.eval pr+","+ndigit.ToString()+","+paddingnum.ToString()+", STR_PAD_LEFT)", NaN))
    /// ファイルダウンロード
    static member file_download(file:num0) =
        php.phpcode <| fun () ->
            pr.cwriter.codewrite("header('Content-Type: application/octet-stream');")
        php.phpcode <| fun () ->
            pr.cwriter.codewrite("header('Content-Transfer-Encoding: Binary');")
        php.phpcode <| fun () ->
            pr.cwriter.codewrite("header('Content-disposition: attachment; filename='.basename("+file.Expr.eval pr+"));")
        php.phpcode <| fun () ->
            pr.cwriter.codewrite("header('Content-Length: '.filesize("+file.Expr.eval pr+"));")
        php.phpcode <| fun () ->
            pr.cwriter.codewrite("while (ob_get_level()) { ob_end_clean(); }")
        php.phpcode <| fun () ->
            pr.cwriter.codewrite("readfile("+file.Expr.eval pr+");")
        php.phpcode <| fun () ->
            pr.cwriter.codewrite("exit;")
    /// ファイルパスからファイル名取得
    static member basename(file:num0) = num0(Var(Nt, "basename("+file.Expr.eval pr+")", NaN))
    /// 改行文字
    static member br = "\\n"
    /// タブ文字
    static member tb = "\\t"
type bh() =
    member this.If(s:bool0) = fun code ->
        php.phpcode <| fun () -> pr.cwriter.codewrite("if("+s.Expr.eval pr+"):")
        code()
    member this.ElseIf(s:bool0) = fun code ->
        php.phpcode <| fun () -> pr.cwriter.codewrite("elseif("+s.Expr.eval pr+"):")
        code()
    member this.Else code =
        php.phpcode <| fun () -> pr.cwriter.codewrite("else:")
        code()
type br =
    static member branch code =
        let b = bh()
        code b
        php.phpcode <| fun () -> pr.cwriter.codewrite("endif;")

    static member if1 (p:bool0) = fun code ->
        br.branch <| fun b ->
            b.If p <| fun () ->
                code()
    static member if2 (p:bool0) = fun code1 code2 ->
        br.branch <| fun b ->
            b.If p <| fun () ->
                code1()
            b.Else <| fun () ->
                code2()
type iter =
    static member range (i1:num0,i2:num0) = fun code ->
        icounter <- icounter + 1
        let i = num0.var("ic"+icounter.ToString())
        php.phpcode <| fun () -> pr.cwriter.codewrite("for("+i.Expr.eval pr+"="+i1.Expr.eval pr+"; "+i.Expr.eval pr+"<="+i2.Expr.eval pr+"; "+i.Expr.eval pr+"++):")
        code(i)
        php.phpcode <| fun () -> pr.cwriter.codewrite("endfor;")
    static member range (i1:num0,i2:int) = fun code ->
        icounter <- icounter + 1
        let i = num0.var("ic"+icounter.ToString())
        php.phpcode <| fun () -> pr.cwriter.codewrite("for("+i.Expr.eval pr+"="+i1.Expr.eval pr+"; "+i.Expr.eval pr+"<="+i2.ToString()+"; "+i.Expr.eval pr+"++):")
        code(i)
        php.phpcode <| fun () -> pr.cwriter.codewrite("endfor;")
    static member range (i1:int,i2:num0) = fun code ->
        icounter <- icounter + 1
        let i = num0.var("ic"+icounter.ToString())
        php.phpcode <| fun () -> pr.cwriter.codewrite("for("+i.Expr.eval pr+"="+i1.ToString()+"; "+i.Expr.eval pr+"<="+i2.Expr.eval pr+"; "+i.Expr.eval pr+"++):")
        code(i)
        php.phpcode <| fun () -> pr.cwriter.codewrite("endfor;")
    static member range (i1:int,i2:int) = fun code ->
        icounter <- icounter + 1
        let i = num0.var("ic"+icounter.ToString())
        php.phpcode <| fun () -> pr.cwriter.codewrite("for("+i.Expr.eval pr+"="+i1.ToString()+"; "+i.Expr.eval pr+"<="+i2.ToString()+"; "+i.Expr.eval pr+"++):")
        code(i)
        php.phpcode <| fun () -> pr.cwriter.codewrite("endfor;")
    static member foreach (array:num0) = fun code ->
        icounter <- icounter + 1
        let i = num0.var("ic"+icounter.ToString())
        php.phpcode <| fun () -> pr.cwriter.codewrite("for("+i.Expr.eval pr+"=0; "+i.Expr.eval pr+"<count("+array.Expr.eval pr+"); "+i.Expr.eval pr+"++):")
        code(i)
        php.phpcode <| fun () -> pr.cwriter.codewrite("endfor;")
    static member foreach (array:num0,key:num0,value:num0) = fun code ->
        icounter <- icounter + 1
        let i = num0.var("ic"+icounter.ToString())
        php.phpcode <| fun () -> pr.cwriter.codewrite("foreach("+array.Expr.eval pr+" as "+key.Expr.eval pr+" => "+value.Expr.eval pr+"):")
        code()
        php.phpcode <| fun () -> pr.cwriter.codewrite("endforeach;")