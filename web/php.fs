namespace Aqualis

open System
open System.IO

[<AutoOpen>]
module num0ForPHP =
    type num0 with
        member this.extcode(pr:program) = "<?php echo " + this.code + "; ?>"
        static member var x = num0(Var(Nt,"$"+x,NaN))
        static member var(x,init:num0) = 
            let v = num0(Var(Nt,"$"+x,NaN))
            v <== init
            v
        static member var(x,init:int) = 
            let v = num0(Var(Nt,"$"+x,NaN))
            v <== init
            v
        static member var(x,init:double) = 
            let v = num0(Var(Nt,"$"+x,NaN))
            v <== init
            v
            
        static member str (x:num0) = "\"" ++ x ++ "\""

        ///配列に要素を複数追加
        member this.push (x:list<exprString>) = 
            codewrite (
                "<?php array_push(" + 
                this.code + ", " + 
                String.Join(",",x |> List.map(fun q -> q.toString("",Direct))) + 
                "); ?>")
        member this.push (x:list<num0>) = codewrite ("<?php array_push(" + this.code + ", " + String.Join(",",List.map(fun (q:num0) -> q.code) x) + "); ?>")
        ///配列に文字列要素を複数追加
        member this.push (x:list<string>) = this.push (List.map(fun (q:string) -> Str q) x)
        ///配列に要素を追加
        member this.push (x:num0) = this.push [x]
        ///配列に文字列要素を追加
        member this.push (x:string) = this.push [x]
        
type PHPbool(x:string) =
    
    member this.name with get() = x
    static member var(x) = PHPbool("$"+x)
    static member (<==) (a:PHPbool,b:PHPbool) = codewrite ("<?php " + a.name + " = " + b.name + " ?>")
    
type PHPdata(x:string) =
    member this.Item(i:exprString) = PHPdata(x + "[" + i.toString(" . ",StrQuotation) + "]")
    member this.Item(i:num0) = this[Nvr i.Expr]
    member this.Item(i:int) = this[Nvr (Int i)]
    member this.Item(i:string) = this[Str i]
    member this.code with get() = "$" + x
    static member (++) (a:string,b:PHPdata) = PHPdata(a+b.code)
    static member (++) (a:PHPdata,b:string) = PHPdata(a.code+b)
    
    member this.push (x:list<num0>) = codewrite ("<?php array_push(" + this.code + ", " + String.Join(",",List.map(fun (q:num0) -> q.code) x) + "); ?>")
    member this.push (x:num0) = this.push [x]
    member this.num1 with get() = num1(Nt,Var1(A1 0, this.code))
    member this.num0 with get() = num0(Var(Nt,this.code,NaN))
    member this.expr with get() = Nvr this.num0.Expr
    member this.foreach code =
        ch.i <| fun i ->
            php.phpcode <| fun () -> codewrite("for("+i.code+"=0; "+i.code+"<count("+this.code+"); "+i.code+"++):")
            programList[prIndex].indentInc()
            code i
            programList[prIndex].indentDec()
            php.phpcode <| fun () -> codewrite "endfor;"
    member this.foreach (key:num0,value:num0) = fun code ->
        ch.i <| fun i ->
            php.phpcode <| fun () -> codewrite("foreach("+this.code+" as "+key.code+" => "+value.code+"):")
            code()
            php.phpcode <| fun () -> codewrite "endforeach;"
    static member (<==) (a:PHPdata,b:PHPdata) = codewrite ("<?php " + a.code + " = " + b.code + " ?>")
    static member (<==) (a:num0,b:PHPdata) = codewrite ("<?php " + a.code + " = " + b.code + " ?>")
    static member (<==) (a:PHPdata,b:num0) = codewrite ("<?php " + a.code + " = " + b.code + " ?>")
    static member (<==) (a:PHPdata,b:exprString) = codewrite ("<?php " + a.code + " = " + b.toString(" . ",StrQuotation) + " ?>")
    static member (.=) (a:PHPdata,b:PHPdata) = num0(Var(Nt,a.code,NaN)) .= num0(Var(Nt,b.code,NaN))
    static member (.=) (a:PHPdata,b:num0) = num0(Var(Nt,a.code,NaN)) .= b
    static member (.=) (a:num0,b:PHPdata) = a .= num0(Var(Nt,b.code,NaN))
    static member (.=/) (a:PHPdata,b:PHPdata) = num0(Var(Nt,a.code,NaN)) .= num0(Var(Nt,b.code,NaN))
    static member (.=/) (a:PHPdata,b:num0) = num0(Var(Nt,a.code,NaN)) .= b
    static member (.=/) (a:num0,b:PHPdata) = a .= num0(Var(Nt,b.code,NaN))
    
and php =
    /// phpファイルを生成
    static member phpfile (dir:string,projectname:string) code =
        makeProgram [dir, projectname, PHP] <| fun () ->
            code ()
            programList[prIndex].close()
            
    static member fnvar (s:string) = num0(Var(Nt,s,NaN))
    static member fnvar (s:exprString) = 
        s.toString(" + ",Direct)
        |> fun s -> php.fnvar s
        
    /// htmlコード内にphpコードを埋め込み
    static member phpcode (code:unit->unit) =
        codewrite "<?php "
        code()
        codewrite " ?>"
    /// POST送信されたデータを表示
    static member postCheck() = codewrite "<?php print_r($_POST) ?>"
    /// POST送信されたファイルを表示
    static member postFileCheck() = codewrite "<?php print_r($_FILES) ?>"
    /// 論理積
    static member And (x:list<bool0>) = bool0(Var(Nt, "(" + String.Join(" && ", x |> List.map (fun s -> s.code)) + ")", NaN))
    /// 論理和
    static member Or (x:list<bool0>) = bool0(Var(Nt, "(" + String.Join(" || ", x |> List.map (fun s -> s.code)) + ")", NaN))
    /// 指定された変数がPOST送信されたか判定
    static member isset (x:PHPdata) = bool0(Var(Nt, "isset(" + x.code + ")", NaN))
    static member echo (x:PHPdata) = php.phpcode <| fun () -> codewrite("echo " + x.code + ";")
    static member echo (x:exprString) = php.phpcode <| fun () -> codewrite("echo " + x.toString(" . ",StrQuotation) + ";")
    /// 文字列を表示
    static member echo (x:string) = php.echo (Str x)
    /// 変数を表示
    static member echo (x:num0) = php.echo (Nvr x.Expr)
    /// ファイル内のテキストを取得
    static member file_get_contents (filename:num0) = num0(Var(Nt, "file_get_contents(" + filename.code + ")", NaN))
    /// ファイル内のテキストを取得
    static member file_get_contents filename = num0(Var(Nt, "file_get_contents("+"\""+filename+"\""+")",NaN))
    /// ファイルにテキストを書き込み
    static member file_put_contents (filename:num0,x:num0) = php.phpcode <| fun () -> codewrite("file_put_contents("+filename.code+","+x.code+");")
    /// ファイルにテキストを書き込み
    static member file_put_contents (filename:exprString,x:num0) = php.phpcode <| fun () -> codewrite("file_put_contents("+filename.toString(" . ",StrQuotation)+","+x.code+");")
    /// ファイルにテキストを書き込み
    static member file_put_contents (filename:string,x:num0) = php.phpcode <| fun () -> codewrite("file_put_contents("+"\""+filename+"\""+","+x.code+");")
    /// JSONファイルをデコード
    static member json_decode (x:num0,p:bool) = num0(Var(Nt, "json_decode("+x.code+","+p.ToString()+");",NaN))
    /// JSONファイルをエンコード
    static member json_encode (x:num0) = num0(Var(Nt, "json_encode("+x.code+", JSON_PRETTY_PRINT|JSON_UNESCAPED_UNICODE|JSON_UNESCAPED_SLASHES )", NaN))
    /// JSONファイルをエンコード
    static member json_encode (x:PHPdata) = num0(Var(Nt, "json_encode("+x.code+", JSON_PRETTY_PRINT|JSON_UNESCAPED_UNICODE|JSON_UNESCAPED_SLASHES )", NaN))
    /// 指定したキーの値の配列を生成
    static member array_column(data:num0,id:string) = num0(Var(Nt, "array_column("+data.code+",\""+id+"\")", NaN))
    /// 指定した要素が含まれているか判定
    static member in_array_strict(s:num0, idArray:num1) = bool0(Var(Nt, "in_array("+s.code+", "+idArray.code+", true)", NaN))
    /// 指定した要素が含まれているか判定
    static member in_array_strict(s:exprString, idArray:num1) = bool0(Var(Nt, "in_array("+s.toString(" . ",StrQuotation)+", "+idArray.code+", true)", NaN))
    /// 指定した要素が含まれているか判定
    static member in_array_strict(s:PHPdata, idArray:num1) = bool0(Var(Nt, "in_array("+s.code+", "+idArray.code+", true)", NaN))
    /// 指定した要素の配列内でのインデックス（キー）を検索
    static member array_search(s:num1, idArray:num1) = num0(Var(Nt, "array_search("+s.code+", "+idArray.code+")", NaN))
    /// 指定した要素の配列内でのインデックス（キー）を検索
    static member array_search(s:PHPdata, idArray:num1) = num0(Var(Nt, "array_search("+s.code+", "+idArray.code+")", NaN))
    /// 配列を生成
    static member array() = num0(Var(Nt, "array()", NaN))
    /// 配列に要素を追加
    static member array_push(a:PHPdata,el:num0) = php.phpcode <| fun () -> codewrite("array_push("+a.code+","+el.code+")")
    /// ファイル内のテキストを配列に格納
    static member file(filename:num0, flag:list<FileFlag>) = 
        num0(Var(Nt, "file("+filename.code+", "+(flag |> List.map (fun s -> s.str) |> (fun p -> String.Join(" | ",p)) )+")", NaN))
    /// ファイル内のテキストを配列に格納
    static member file(filename:string, flag:list<FileFlag>) = 
        num0(Var(Nt, "file('"+filename+"', "+(flag |> List.map (fun s -> s.str) |> (fun p -> String.Join(" | ",p)) )+")", NaN))
    /// ファイルを開く
    static member fopen(filename:num0,rw:FileOpenMode) = num0(Var(Nt, "fopen("+filename.code+", "+rw.str+")", NaN))
    /// ファイルを開く
    static member fopen(filename:string,rw:FileOpenMode) = num0(Var(Nt, "fopen(\""+filename+"\", "+rw.str+")", NaN))
    /// ファイルに書き込み
    static member fwrite(fp:num0,t:exprString) = php.phpcode <| fun () -> codewrite("fwrite("+fp.code+", "+t.toString("",Direct)+");")
    static member fwrite(fp:num0,t:PHPdata) = php.fwrite(fp,t.expr)
    /// ファイルに書き込み
    static member fwrite(fp:num0,t:string) = php.fwrite(fp,Str t)
    /// ファイルに書き込み
    static member fwrite(fp:num0,t:int) = php.phpcode <| fun () -> codewrite("fwrite("+fp.code+", "+t.ToString()+");")
    /// ファイルにShift-JISで書き込み
    static member fwrite_SJIS(fp:num0,t:num0) = php.phpcode <| fun () -> codewrite("fwrite("+fp.code+", mb_convert_encoding("+t.code+", 'SJIS-win', 'UTF-8'));")
    /// ファイルにShift-JISで書き込み
    static member fwrite_SJIS(fp:num0,t:string) = php.phpcode <| fun () -> codewrite("fwrite("+fp.code+", mb_convert_encoding(\""+t+"\", 'SJIS-win', 'UTF-8'));")
    /// ファイルを閉じる
    static member fclose(filename:num0) = php.phpcode <| fun () -> codewrite("fclose("+filename.code+");")
    /// 正規表現
    static member preg_match(p:string,text:num0,mat:num0) = php.phpcode <| fun () -> codewrite("preg_match('"+p+"',"+text.code+","+mat.code+");")
    /// ファイルのダウンロード
    static member download(filename:string) =
        php.phpcode <| fun () -> codewrite "header('Content-Type: application/octet-stream');"
        php.phpcode <| fun () -> codewrite ("header('Content-Length: '.filesize(\""+filename+"\"));")
        php.phpcode <| fun () -> codewrite ("header('Content-Disposition: attachment; filename=\""+filename+"\"');")
        php.phpcode <| fun () -> codewrite ("readfile(\""+filename+"\");")
        php.phpcode <| fun () -> codewrite "exit;"
    /// 整数に変換
    static member intval(s:num0) = num0(Var(Nt, "intval("+s.code+")", NaN))
    /// 配列要素の和
    static member array_sum(data:num0) = num0(Var(Nt, "array_sum("+data.code+")", NaN))
    /// 文字数
    static member strlen(data:num0) = num0(Var(Nt, "strlen("+data.code+")", NaN))
    /// 数値かどうか判定
    static member is_numeric(data:num0) = bool0(Var(Nt, "is_numeric("+data.code+")", NaN))
    /// 否定演算
    static member nt (data:bool0) = bool0(Var(Nt, "!"+data.code,NaN))
    ///<summary>送信データをキャッシュしない（Firefoxでフォームの選択肢がリロード前から保持される現象を回避）</summary>
    static member set_nocache() = php.phpcode <| fun () -> codewrite "header( 'Cache-Control: no-store, no-cache, must-revalidate' );"
    /// HTTPヘッダを取得
    static member header(data:num0) = php.phpcode <| fun () -> codewrite("header("+data.code+")")
    /// HTTPヘッダを取得
    static member header(data:string) = php.phpcode <| fun () -> codewrite("header('"+data+"')")
    // 小数に変換
    // static member float(data:num0) = Var("(float)"+data.code)
    // 絶対値
    // static member abs(data:num0) = Var("abs("+data.code+")")
    /// 日付を取得
    static member date(fmt:string) = num0(Var(Nt, "date(\""+fmt+"\")", NaN))
    /// 整数に丸め
    static member round(x:num0) = num0(Var(Nt, "round("+x.code+")", NaN))
    /// 文字列切り出し
    static member substr(x:num0,n:num0) = num0(Var(Nt, "substr("+x.code+","+n.code+")", NaN))
    /// 文字列切り出し
    static member substr(x:num0,n:int) = num0(Var(Nt, "substr("+x.code+","+n.ToString()+")", NaN))
    /// ファイルが存在するか確認
    static member file_exists(x:num0) = bool0(Var(Nt, "file_exists("+x.code+")", NaN))
    /// ファイルが存在するか確認
    static member file_exists(x:string) = bool0(Var(Nt, "file_exists(\""+x+"\")", NaN))
    /// 文字数（全角も1字扱い）
    static member mb_strlen(x:num0) = num0(Var(Nt, "mb_strlen("+x.code+")", NaN))
    /// 文字数（全角も1字扱い）
    static member mb_strwidth(x:num0) = num0(Var(Nt, "mb_strwidth("+x.code+")", NaN))
    /// 文字列比較
    static member strncmp(x:num0,y:string,n:int) = num0(Var(Nt, "strncmp("+x.code+",\""+y+"\","+n.ToString()+")", NaN))
    /// 指定したパターンにマッチするファイルパス取得
    static member glob(x:num0) = PHPdata("glob("+x.code+")")
    /// 指定したパターンにマッチするファイルパス取得
    static member glob(x:string) = PHPdata("glob(\""+x+"\")")
    /// 文字列分割
    static member explode(x:num0,y:num0) = num0(Var(Nt, "explode("+x.code+","+y.code+")", NaN))
    /// 文字列分割
    static member explode(x:string,y:num0) = num0(Var(Nt, "explode('"+x+"',"+y.code+")", NaN))
    /// 配列のソート
    static member sort(data:num0) = php.phpcode <| fun () -> codewrite("sort("+data.code+")")
    /// 整数に変換
    static member toint(x:num0) = num0(Var(Nt, "(int)"+x.code, NaN))
    /// 配列要素数
    static member count(x:num0) = num0(Var(Nt, "count("+x.code+")", NaN))
    static member count(x:PHPdata) = num0(Var(Nt, "count("+x.code+")", NaN))
    /// 拡張子を除いたファイル名
    static member filename_withoutExtension(x:num0) = num0(Var(Nt, "pathinfo("+x.code+", PATHINFO_FILENAME)", NaN))
    /// 拡張子を除いたファイル名
    static member filename_withoutExtension(x:PHPdata) = num0(Var(Nt, "pathinfo("+x.code+", PATHINFO_FILENAME)", NaN))
    /// ファイル削除
    static member unlink(data:num0) = php.phpcode <| fun () -> codewrite("unlink("+data.code+")")
    /// 配列要素をランダムに入れ替え
    static member shuffle(data:num1) = php.phpcode <| fun () -> codewrite("shuffle("+data.code+")")
    /// タイムゾーン設定
    static member setTimeZone(location:string) = php.phpcode <| fun () -> codewrite("date_default_timezone_set('"+location+"');")
    /// メール送信
    static member sendMail(body:exprString,subject:num0,fromAddress:string,toAddress:PHPdata) =
        php.phpcode <| fun () -> codewrite("mb_language(\"ja\");")
        php.phpcode <| fun () -> codewrite("mb_internal_encoding(\"UTF-8\");")
        php.phpcode <| fun () -> codewrite("mb_send_mail("+toAddress.code+","+subject.code+","+body.toString(" . ",StrQuotation)+","+"\"From: "+fromAddress+"\");")
    /// メール送信
    static member sendMail(body:exprString,subject:exprString,fromAddress:string,toAddress:PHPdata) =
        php.phpcode <| fun () -> codewrite("mb_language(\"ja\");")
        php.phpcode <| fun () -> codewrite("mb_internal_encoding(\"UTF-8\");")
        php.phpcode <| fun () -> codewrite("mb_send_mail("+toAddress.code+","+subject.toString(" . ",StrQuotation)+","+body.toString(" . ",StrQuotation)+","+"\"From: "+fromAddress+"\");")
    /// メール送信
    static member sendMail(body:PHPdata,subject:num0,fromAddress:string,toAddress:num0) =
        php.phpcode <| fun () -> codewrite("mb_language(\"ja\");")
        php.phpcode <| fun () -> codewrite("mb_internal_encoding(\"UTF-8\");")
        php.phpcode <| fun () -> codewrite("mb_send_mail("+toAddress.code+","+subject.code+","+body.code+","+"\"From: "+fromAddress+"\");")
    /// メール送信
    static member sendMail(body:string,subject:string,smtp:string,fromAddress:string,toAddress:string) =
        let cmd = num0.var "cmd"
        //cmd <== "echo \\\""+body+"\\\" | s-nail -s \\\""+subject+"\\\" -Smta=smtp://"+smtp+":25 -r "+fromAddress+" "+toAddress
        cmd <== php.fnvar("echo \\\"" + body + "\\\" | mail -s \\\"" + subject + "\\\" -S smtp=smtp://" + smtp + ":25 -r " + fromAddress + " " + toAddress)
        php.phpcode <| fun () -> codewrite("exec("+cmd.code+");")
    /// メール送信
    static member sendMail(body:num0,subject:num0,fromAddress:string,toAddress:num0) =
        php.phpcode <| fun () -> codewrite("mb_language(\"ja\");")
        php.phpcode <| fun () -> codewrite("mb_internal_encoding(\"UTF-8\");")
        php.phpcode <| fun () -> codewrite("mb_send_mail("+toAddress.code+","+subject.code+","+body.code+","+"\"From: "+fromAddress+"\");")
    /// メール送信
    static member sendMail(body:string,subject:string,fromAddress:string,toAddress:string) =
        php.phpcode <| fun () -> codewrite("mb_language(\"ja\");")
        php.phpcode <| fun () -> codewrite("mb_internal_encoding(\"UTF-8\");")
        php.phpcode <| fun () -> codewrite("mb_send_mail(\""+toAddress+"\",\""+subject+"\",\""+body+"\","+"\"From: "+fromAddress+"\");")
    /// Discordへメッセージ送信
    static member sendDiscord(body:exprString,webhookURL:PHPdata) =
        let cmd = num0.var "cmd"
        cmd <== php.fnvar("curl -H \\\"Content-Type: application/json\\\" -X POST -d \\\"{\\\\\\\"username\\\\\\\": \\\\\\\"Ediass Notification\\\\\\\", \\\\\\\"content\\\\\\\": \\\\\\\""++body++"\\\\\\\"}\\\" " ++ webhookURL.code)
        php.phpcode <| fun () -> codewrite("exec("+cmd.code+");")
    /// Discordへメッセージ送信
    static member sendDiscord(body:string,webhookURL:string) =
        let cmd = num0.var "cmd"
        cmd <== php.fnvar("curl -H \\\"Content-Type: application/json\\\" -X POST -d \\\"{\\\\\\\"username\\\\\\\": \\\\\\\"Ediass Notification\\\\\\\", \\\\\\\"content\\\\\\\": \\\\\\\"" + body + "\\\\\\\"}\\\" " + webhookURL)
        php.phpcode <| fun () -> codewrite("exec("+cmd.code+");")
    /// 文字列置換
    static member str_replace(strfrom:string,strto:string,str:num0) = num0(Var(Nt, "str_replace("+"\""+strfrom+"\""+","+"\""+strto+"\""+","+str.code+")", NaN))
    /// 指定した文字数になるまで文字を埋める
    static member str_pad(num:num0,ndigit:int,paddingnum:int) = num0(Var(Nt, "str_pad("+num.code+","+ndigit.ToString()+","+paddingnum.ToString()+", STR_PAD_LEFT)", NaN))
    /// ファイルダウンロード
    static member file_download(file:PHPdata) =
        php.phpcode <| fun () ->
            codewrite("header('Content-Type: application/octet-stream');")
        php.phpcode <| fun () ->
            codewrite("header('Content-Transfer-Encoding: Binary');")
        php.phpcode <| fun () ->
            codewrite("header('Content-disposition: attachment; filename='.basename("+file.code+"));")
        php.phpcode <| fun () ->
            codewrite("header('Content-Length: '.filesize("+file.code+"));")
        php.phpcode <| fun () ->
            codewrite("while (ob_get_level()) { ob_end_clean(); }")
        php.phpcode <| fun () ->
            codewrite("readfile("+file.code+");")
        php.phpcode <| fun () ->
            codewrite("exit;")
    /// ファイルパスからファイル名取得
    static member basename(file:num0) = num0(Var(Nt, "basename("+file.code+")", NaN))
    /// 改行文字
    static member br = "\\n"
    /// タブ文字
    static member tb = "\\t"

[<AutoOpen>]
module num1ForPHP =
        
    type num1 with
        member this.extcode(pr:program) = "<?php echo " + this.code + "; ?>"
        static member var x = num1(Nt,Var1(A1 0,"$"+x))
        
        static member array(arrayname:string) = 
            let c = num1.var arrayname
            codewrite ("<?php "+arrayname+" = array(); ?>")
            c
            
        static member array(arrayname:string,data:list<string*string>) = 
            let c = num1.var arrayname
            codewrite ("<?php "+arrayname+" = array(); ?>")
            codewrite ("<?php "+arrayname+"[] = array("+String.Join(",",data |> List.map (fun (a,b) -> "'"+a+"'=>'"+b+"'"))+"); ?>")
            c

        static member array(arrayname:string,data:list<string*num0>) = 
            let c = num0.var arrayname
            codewrite ("<?php "+arrayname+" = array(); ?>")
            codewrite ("<?php "+arrayname+"[] = array("+String.Join(",",data |> List.map (fun (a,b) -> "'"+a+"'=>"+b.code))+"); ?>")
            c
        static member array(arrayname:string,data:list<string*PHPdata>) = 
            let c = num0.var arrayname
            codewrite ("<?php "+arrayname+" = array(); ?>")
            codewrite ("<?php "+arrayname+"[] = array("+String.Join(",",data |> List.map (fun (a,b) -> "'"+a+"'=>"+b.code))+"); ?>")
            c
        member this.push (x:list<num0>) = codewrite ("<?php array_push(" + this.code + ", " + String.Join(",",List.map(fun (q:num0) -> q.code) x) + "); ?>")
        member this.push (x:num0) = this.push [x]
