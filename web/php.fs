namespace docWriter

open System
open System.IO

type php =
    /// phpファイルを生成
    static member phpfile (filename:string) code =
        wrBody.FileSet filename
        code ()
        wrBody.Close()

    /// htmlコード内にphpコードを埋め込み
    static member phpcode (code:unit->unit) =
        write "<?php "
        code()
        write " ?>"
    /// POST送信されたデータを表示
    static member postCheck() = write "<?php print_r($_POST) ?>"
    /// POST送信されたファイルを表示
    static member postFileCheck() = write "<?php print_r($_FILES) ?>"
    /// 論理積
    static member And (x:list<PHPbool>) = PHPbool("("+String.Join(" && ", x |> List.map (fun s -> s.name))+")")
    /// 論理和
    static member Or (x:list<PHPbool>) = PHPbool("("+String.Join(" || ", x |> List.map (fun s -> s.name))+")")
    /// 指定された変数がPOST送信されたか判定
    static member isset (x:PHPvar) = PHPbool("isset("+x.name+")")
    /// 文字列を表示
    static member echo (x:string) = php.phpcode <| fun () -> write("echo \"" + x + "\";")
    /// 変数を表示
    static member echo (x:PHPvar) = php.phpcode <| fun () -> write("echo " + x.name + ";")
    /// ファイル内のテキストを取得
    static member file_get_contents (filename:PHPvar) = Var("file_get_contents("+filename.name+")")
    /// ファイル内のテキストを取得
    static member file_get_contents filename = Var("file_get_contents("+"\""+filename+"\""+")")
    /// ファイルにテキストを書き込み
    static member file_put_contents (filename:PHPvar,x:PHPvar) = php.phpcode <| fun () -> write("file_put_contents("+filename.name+","+x.name+");")
    /// ファイルにテキストを書き込み
    static member file_put_contents (filename:string,x:PHPvar) = php.phpcode <| fun () -> write("file_put_contents("+"\""+filename+"\""+","+x.name+");")
    /// JSONファイルをデコード
    static member json_decode (x:PHPvar,p:bool) = Var("json_decode("+x.name+","+p.ToString()+")")
    /// JSONファイルをエンコード
    static member json_encode (x:PHPvar) = Var("json_encode("+x.name+", JSON_PRETTY_PRINT|JSON_UNESCAPED_UNICODE|JSON_UNESCAPED_SLASHES )")
    /// 指定したキーの値の配列を生成
    static member array_column(data:PHPvar,id:string) = Var("array_column("+data.name+",\""+id+"\")")
    /// 指定した要素が含まれているか判定
    static member in_array_strict(s:PHPvar, idArray:PHPvar) = PHPbool("in_array("+s.name+", "+idArray.name+", true)")
    /// 指定した要素の配列内でのインデックス（キー）を検索
    static member array_search(s:PHPvar, idArray:PHPvar) = Var("array_search("+s.name+", "+idArray.name+")")
    /// 配列を生成
    static member array() = Var "array()"
    /// 配列に要素を追加
    static member array_push(a:PHPvar,el:PHPvar) = php.phpcode <| fun () -> write("array_push("+a.name+","+el.name+")")
    /// ファイル内のテキストを配列に格納
    static member file(filename:PHPvar, flag:list<FileFlag>) = 
        Var("file("+filename.name+", "+(flag |> List.map (fun s -> s.str) |> (fun p -> String.Join(" | ",p)) )+")")
    /// ファイル内のテキストを配列に格納
    static member file(filename:string, flag:list<FileFlag>) = 
        Var("file('"+filename+"', "+(flag |> List.map (fun s -> s.str) |> (fun p -> String.Join(" | ",p)) )+")")
    /// ファイルを開く
    static member fopen(filename:PHPvar,rw:FileOpenMode) = Var("fopen("+filename.name+", "+rw.str+")")
    /// ファイルを開く
    static member fopen(filename:string,rw:FileOpenMode) = Var("fopen(\""+filename+"\", "+rw.str+")")
    /// ファイルに書き込み
    static member fwrite(fp:PHPvar,t:PHPvar) = php.phpcode <| fun () -> write("fwrite("+fp.name+", "+t.name+");")
    /// ファイルに書き込み
    static member fwrite(fp:PHPvar,t:string) = php.fwrite(fp,PHPvar.str(t))
    /// ファイルに書き込み
    static member fwrite(fp:PHPvar,t:int) = php.phpcode <| fun () -> write("fwrite("+fp.name+", "+t.ToString()+");")
    /// ファイルにShift-JISで書き込み
    static member fwrite_SJIS(fp:PHPvar,t:PHPvar) = php.phpcode <| fun () -> write("fwrite("+fp.name+", mb_convert_encoding("+t.name+", 'SJIS-win', 'UTF-8'));")
    /// ファイルにShift-JISで書き込み
    static member fwrite_SJIS(fp:PHPvar,t:string) = php.phpcode <| fun () -> write("fwrite("+fp.name+", mb_convert_encoding(\""+t+"\", 'SJIS-win', 'UTF-8'));")
    /// ファイルを閉じる
    static member fclose(filename:PHPvar) = php.phpcode <| fun () -> write("fclose("+filename.name+");")
    /// 正規表現
    static member preg_match(p:string,text:PHPvar,mat:PHPvar) = php.phpcode <| fun () -> write("preg_match('"+p+"',"+text.name+","+mat.name+");")
    /// ファイルのダウンロード
    static member download(filename:string) =
        php.phpcode <| fun () -> write "header('Content-Type: application/octet-stream');"
        php.phpcode <| fun () -> write ("header('Content-Length: '.filesize(\""+filename+"\"));")
        php.phpcode <| fun () -> write ("header('Content-Disposition: attachment; filename=\""+filename+"\"');")
        php.phpcode <| fun () -> write ("readfile(\""+filename+"\");")
        php.phpcode <| fun () -> write "exit;"
    /// 整数に変換
    static member intval(s:PHPvar) = Var("intval("+s.name+")")
    /// 配列要素の和
    static member array_sum(data:PHPvar) = Var("array_sum("+data.name+")")
    /// 文字数
    static member strlen(data:PHPvar) = Var("strlen("+data.name+")")
    /// 数値かどうか判定
    static member is_numeric(data:PHPvar) = PHPbool("is_numeric("+data.name+")")
    /// 否定演算
    static member nt (data:PHPbool) = PHPbool("!"+data.name)
    ///<summary>送信データをキャッシュしない（Firefoxでフォームの選択肢がリロード前から保持される現象を回避）</summary>
    static member set_nocache() = php.phpcode <| fun () -> write "header( 'Cache-Control: no-store, no-cache, must-revalidate' );"
    /// HTTPヘッダを取得
    static member header(data:PHPvar) = php.phpcode <| fun () -> write("header("+data.name+")")
    /// HTTPヘッダを取得
    static member header(data:string) = php.phpcode <| fun () -> write("header('"+data+"')")
    /// 小数に変換
    static member float(data:PHPvar) = Var("(float)"+data.name)
    /// 絶対値
    static member abs(data:PHPvar) = Var("abs("+data.name+")")
    /// 日付を取得
    static member date(fmt:string) = Var("date(\""+fmt+"\")")
    /// 整数に丸め
    static member round(x:PHPvar) = Var("round("+x.name+")")
    /// 文字列切り出し
    static member substr(x:PHPvar,n:PHPvar) = Var("substr("+x.name+","+n.name+")")
    /// 文字列切り出し
    static member substr(x:PHPvar,n:int) = Var("substr("+x.name+","+n.ToString()+")")
    /// ファイルが存在するか確認
    static member file_exists(x:PHPvar) = PHPbool("file_exists("+x.name+")")
    /// ファイルが存在するか確認
    static member file_exists(x:string) = PHPbool("file_exists(\""+x+"\")")
    /// 文字数（全角も1字扱い）
    static member mb_strlen(x:PHPvar) = Var("mb_strlen("+x.name+")")
    /// 文字数（全角も1字扱い）
    static member mb_strwidth(x:PHPvar) = Var("mb_strwidth("+x.name+")")
    /// 文字列比較
    static member strncmp(x:PHPvar,y:string,n:int) = Var("strncmp("+x.name+",\""+y+"\","+n.ToString()+")")
    /// 指定したパターンにマッチするファイルパス取得
    static member glob(x:PHPvar) = Var("glob("+x.name+")")
    /// 指定したパターンにマッチするファイルパス取得
    static member glob(x:string) = Var("glob(\""+x+"\")")
    /// 文字列分割
    static member explode(x:PHPvar,y:PHPvar) = Var("explode("+x.name+","+y.name+")")
    /// 文字列分割
    static member explode(x:string,y:PHPvar) = Var("explode('"+x+"',"+y.name+")")
    /// 配列のソート
    static member sort(data:PHPvar) = php.phpcode <| fun () -> write("sort("+data.name+")")
    /// 整数に変換
    static member toint(x:PHPvar) = Var("(int)"+x.name)
    /// 配列要素数
    static member count(x:PHPvar) = Var("count("+x.name+")")
    /// 拡張子を除いたファイル名
    static member filename_withoutExtension(x:PHPvar) = Var("pathinfo("+x.name+", PATHINFO_FILENAME)")
    /// ファイル削除
    static member unlink(data:PHPvar) = php.phpcode <| fun () -> write("unlink("+data.name+")")
    /// 配列要素をランダムに入れ替え
    static member shuffle(data:PHPvar) = php.phpcode <| fun () -> write("shuffle("+data.name+")")
    /// タイムゾーン設定
    static member setTimeZone(location:string) = php.phpcode <| fun () -> write("date_default_timezone_set('"+location+"');")
    /// メール送信
    static member sendMail(body:string,subject:string,smtp:string,fromAddress:string,toAddress:string) =
        let cmd = PHPvar.var "cmd"
        //cmd <== "echo \\\""+body+"\\\" | s-nail -s \\\""+subject+"\\\" -Smta=smtp://"+smtp+":25 -r "+fromAddress+" "+toAddress
        cmd <== "echo \\\""+body+"\\\" | mail -s \\\""+subject+"\\\" -S smtp=smtp://"+smtp+":25 -r "+fromAddress+" "+toAddress
        php.phpcode <| fun () -> write("exec("+cmd.name+");")
    /// メール送信
    static member sendMail(body:PHPvar,subject:PHPvar,fromAddress:string,toAddress:PHPvar) =
        php.phpcode <| fun () -> write("mb_language(\"ja\");")
        php.phpcode <| fun () -> write("mb_internal_encoding(\"UTF-8\");")
        php.phpcode <| fun () -> write("mb_send_mail("+toAddress.name+","+subject.name+","+body.name+","+"\"From: "+fromAddress+"\");")
    /// メール送信
    static member sendMail(body:string,subject:string,fromAddress:string,toAddress:string) =
        php.phpcode <| fun () -> write("mb_language(\"ja\");")
        php.phpcode <| fun () -> write("mb_internal_encoding(\"UTF-8\");")
        php.phpcode <| fun () -> write("mb_send_mail(\""+toAddress+"\",\""+subject+"\",\""+body+"\","+"\"From: "+fromAddress+"\");")
    /// Discordへメッセージ送信
    static member sendDiscord(body:PHPvar,webhookURL:PHPvar) =
        let cmd = PHPvar.var "cmd"
        cmd <== "curl -H \\\"Content-Type: application/json\\\" -X POST -d \\\"{\\\\\\\"username\\\\\\\": \\\\\\\"Ediass Notification\\\\\\\", \\\\\\\"content\\\\\\\": \\\\\\\""++body++"\\\\\\\"}\\\" "++webhookURL 
        php.phpcode <| fun () -> write("exec("+cmd.name+");")
    /// Discordへメッセージ送信
    static member sendDiscord(body:string,webhookURL:string) =
        let cmd = PHPvar.var "cmd"
        cmd <== "curl -H \\\"Content-Type: application/json\\\" -X POST -d \\\"{\\\\\\\"username\\\\\\\": \\\\\\\"Ediass Notification\\\\\\\", \\\\\\\"content\\\\\\\": \\\\\\\""+body+"\\\\\\\"}\\\" "+webhookURL 
        php.phpcode <| fun () -> write("exec("+cmd.name+");")
    /// 文字列置換
    static member str_replace(strfrom:string,strto:string,str:PHPvar) = Var("str_replace("+"\""+strfrom+"\""+","+"\""+strto+"\""+","+str.name+")")
    /// 指定した文字数になるまで文字を埋める
    static member str_pad(num:PHPvar,ndigit:int,paddingnum:int) = Var("str_pad("+num.name+","+ndigit.ToString()+","+paddingnum.ToString()+", STR_PAD_LEFT)")
    /// ファイルダウンロード
    static member file_download(file:PHPvar) =
        php.phpcode <| fun () ->
            write("header('Content-Type: application/octet-stream');")
        php.phpcode <| fun () ->
            write("header('Content-Transfer-Encoding: Binary');")
        php.phpcode <| fun () ->
            write("header('Content-disposition: attachment; filename='.basename("+file.name+"));")
        php.phpcode <| fun () ->
            write("header('Content-Length: '.filesize("+file.name+"));")
        php.phpcode <| fun () ->
            write("while (ob_get_level()) { ob_end_clean(); }")
        php.phpcode <| fun () ->
            write("readfile("+file.name+");")
        php.phpcode <| fun () ->
            write("exit;")
    /// ファイルパスからファイル名取得
    static member basename(file:PHPvar) = Var("basename("+file.name+")")
    /// 改行文字
    static member br = "\\n"
    /// タブ文字
    static member tb = "\\t"
type bh() =
    member this.If(s:PHPbool) = fun code ->
        php.phpcode <| fun () -> write("if("+s.name+"):")
        code()
    member this.ElseIf(s:PHPbool) = fun code ->
        php.phpcode <| fun () -> write("elseif("+s.name+"):")
        code()
    member this.Else code =
        php.phpcode <| fun () -> write("else:")
        code()
type br =
    static member branch code =
        let b = bh()
        code b
        php.phpcode <| fun () -> write("endif;")

    static member if1 (p:PHPbool) = fun code ->
        br.branch <| fun b ->
            b.If p <| fun () ->
                code()
    static member if2 (p:PHPbool) = fun code1 code2 ->
        br.branch <| fun b ->
            b.If p <| fun () ->
                code1()
            b.Else <| fun () ->
                code2()
type iter =
    static member range (i1:PHPvar,i2:PHPvar) = fun code ->
        icounter <- icounter + 1
        let i = PHPvar.var("ic"+icounter.ToString())
        php.phpcode <| fun () -> write("for("+i.name+"="+i1.name+"; "+i.name+"<="+i2.name+"; "+i.name+"++):")
        code(i)
        php.phpcode <| fun () -> write("endfor;")
    static member range (i1:PHPvar,i2:int) = fun code ->
        icounter <- icounter + 1
        let i = PHPvar.var("ic"+icounter.ToString())
        php.phpcode <| fun () -> write("for("+i.name+"="+i1.name+"; "+i.name+"<="+i2.ToString()+"; "+i.name+"++):")
        code(i)
        php.phpcode <| fun () -> write("endfor;")
    static member range (i1:int,i2:PHPvar) = fun code ->
        icounter <- icounter + 1
        let i = PHPvar.var("ic"+icounter.ToString())
        php.phpcode <| fun () -> write("for("+i.name+"="+i1.ToString()+"; "+i.name+"<="+i2.name+"; "+i.name+"++):")
        code(i)
        php.phpcode <| fun () -> write("endfor;")
    static member range (i1:int,i2:int) = fun code ->
        icounter <- icounter + 1
        let i = PHPvar.var("ic"+icounter.ToString())
        php.phpcode <| fun () -> write("for("+i.name+"="+i1.ToString()+"; "+i.name+"<="+i2.ToString()+"; "+i.name+"++):")
        code(i)
        php.phpcode <| fun () -> write("endfor;")
    static member foreach (array:PHPvar) = fun code ->
        icounter <- icounter + 1
        let i = PHPvar.var("ic"+icounter.ToString())
        php.phpcode <| fun () -> write("for("+i.name+"=0; "+i.name+"<count("+array.name+"); "+i.name+"++):")
        code(i)
        php.phpcode <| fun () -> write("endfor;")
    static member foreach (array:PHPvar,key:PHPvar,value:PHPvar) = fun code ->
        icounter <- icounter + 1
        let i = PHPvar.var("ic"+icounter.ToString())
        php.phpcode <| fun () -> write("foreach("+array.name+" as "+key.name+" => "+value.name+"):")
        code()
        php.phpcode <| fun () -> write("endforeach;")