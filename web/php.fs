namespace Aqualis

open System
open System.IO

[<AutoOpen>]
module num0ForPHP =
    type num0 with
        member this.code(pr:program) = "<?php echo " + this.Expr.eval (programList[prIndex]) + "; ?>"
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
        
        static member array(arrayname:string) = 
            let c = num0.var arrayname
            programList[prIndex].codewrite ("<?php "+arrayname+" = array(); ?>")
            c
            
        static member array(arrayname:string,data:list<string*string>) = 
            let c = num0.var arrayname
            programList[prIndex].codewrite ("<?php "+arrayname+" = array(); ?>")
            programList[prIndex].codewrite ("<?php "+arrayname+"[] = array("+String.Join(",",data |> List.map (fun (a,b) -> "'"+a+"'=>'"+b+"'"))+"); ?>")
            c

        static member array(arrayname:string,data:list<string*num0>) = 
            let c = num0.var arrayname
            programList[prIndex].codewrite ("<?php "+arrayname+" = array(); ?>")
            programList[prIndex].codewrite ("<?php "+arrayname+"[] = array("+String.Join(",",data |> List.map (fun (a,b) -> "'"+a+"'=>"+b.Expr.eval (programList[prIndex])))+"); ?>")
            c

        ///配列に要素を複数追加
        member this.push (x:list<exprString>) = 
            programList[prIndex].codewrite (
                "<?php array_push(" + 
                this.Expr.eval (programList[prIndex]) + ", " + 
                String.Join(",",x |> List.map(fun q -> q.toString("",Direct))) + 
                "); ?>")
        member this.push (x:list<num0>) = programList[prIndex].codewrite ("<?php array_push(" + this.Expr.eval (programList[prIndex]) + ", " + String.Join(",",List.map(fun (q:num0) -> q.Expr.eval (programList[prIndex])) x) + "); ?>")
        ///配列に文字列要素を複数追加
        member this.push (x:list<string>) = this.push (List.map(fun (q:string) -> Str q) x)
        ///配列に要素を追加
        member this.push (x:num0) = this.push [x]
        ///配列に文字列要素を追加
        member this.push (x:string) = this.push [x]
        
type PHPbool(x:string) =
    
    member this.name with get() = x
    static member var(x) = PHPbool("$"+x)
    static member (<==) (a:PHPbool,b:PHPbool) = programList[prIndex].codewrite ("<?php " + a.name + " = " + b.name + " ?>")
    
type PHPdata(x:string) =
    member this.Item(i:exprString) = PHPdata(x + "[" + i.toString(" . ",StrQuotation) + "]")
    member this.Item(i:num0) = this[Nvr i.Expr]
    member this.Item(i:int) = this[Nvr (Int i)]
    member this.Item(i:string) = this[Str i]
    member this.code with get() = x
    member this.tonum0 with get() = num0(Var(Nt, x, NaN))
    static member (++) (a:string,b:PHPdata) = PHPdata(a+b.code)
    static member (++) (a:PHPdata,b:string) = PHPdata(a.code+b)
    member this.foreach code =
        ch.i <| fun i ->
            php.phpcode <| fun () -> programList[prIndex].codewrite("for("+i.Expr.eval (programList[prIndex])+"=0; "+i.Expr.eval (programList[prIndex])+"<count("+this.code+"); "+i.Expr.eval (programList[prIndex])+"++):")
            code i
            php.phpcode <| fun () -> programList[prIndex].codewrite "endfor;"
    member this.foreach (key:num0,value:num0) = fun code ->
        ch.i <| fun i ->
            php.phpcode <| fun () -> programList[prIndex].codewrite("foreach("+this.code+" as "+key.Expr.eval (programList[prIndex])+" => "+value.Expr.eval (programList[prIndex])+"):")
            code()
            php.phpcode <| fun () -> programList[prIndex].codewrite "endforeach;"
    static member (<==) (a:PHPdata,b:PHPdata) = programList[prIndex].codewrite (a.code + " = " + b.code)
    static member (.=) (a:PHPdata,b:PHPdata) = num0(Var(Nt,a.code,NaN)) .= num0(Var(Nt,b.code,NaN))
    static member (.=) (a:PHPdata,b:num0) = num0(Var(Nt,a.code,NaN)) .= b
    static member (.=) (a:num0,b:PHPdata) = a .= num0(Var(Nt,b.code,NaN))
    
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
        programList[prIndex].codewrite "<?php "
        code()
        programList[prIndex].codewrite " ?>"
    /// POST送信されたデータを表示
    static member postCheck() = programList[prIndex].codewrite "<?php print_r($_POST) ?>"
    /// POST送信されたファイルを表示
    static member postFileCheck() = programList[prIndex].codewrite "<?php print_r($_FILES) ?>"
    /// 論理積
    static member And (x:list<bool0>) = bool0(Var(Nt, "(" + String.Join(" && ", x |> List.map (fun s -> s.Expr.eval (programList[prIndex]))) + ")", NaN))
    /// 論理和
    static member Or (x:list<bool0>) = bool0(Var(Nt, "(" + String.Join(" || ", x |> List.map (fun s -> s.Expr.eval (programList[prIndex]))) + ")", NaN))
    /// 指定された変数がPOST送信されたか判定
    static member isset (x:num0) = bool0(Var(Nt, "isset(" + x.Expr.eval (programList[prIndex]) + ")", NaN))
    static member echo (x:exprString) = php.phpcode <| fun () -> programList[prIndex].codewrite("echo " + x.toString(" . ",StrQuotation) + ";")
    /// 文字列を表示
    static member echo (x:string) = php.phpcode <| fun () -> programList[prIndex].codewrite("echo \"" + x + "\";")
    /// 変数を表示
    static member echo (x:num0) = php.phpcode <| fun () -> programList[prIndex].codewrite("echo " + x.Expr.eval (programList[prIndex]) + ";")
    /// ファイル内のテキストを取得
    static member file_get_contents (filename:num0) = num0(Var(Nt, "file_get_contents(" + filename.Expr.eval (programList[prIndex]) + ")", NaN))
    /// ファイル内のテキストを取得
    static member file_get_contents filename = num0(Var(Nt, "file_get_contents("+"\""+filename+"\""+")",NaN))
    /// ファイルにテキストを書き込み
    static member file_put_contents (filename:num0,x:num0) = php.phpcode <| fun () -> programList[prIndex].codewrite("file_put_contents("+filename.Expr.eval (programList[prIndex])+","+x.Expr.eval (programList[prIndex])+");")
    /// ファイルにテキストを書き込み
    static member file_put_contents (filename:string,x:num0) = php.phpcode <| fun () -> programList[prIndex].codewrite("file_put_contents("+"\""+filename+"\""+","+x.Expr.eval (programList[prIndex])+");")
    /// JSONファイルをデコード
    static member json_decode (x:num0,p:bool) = PHPdata ("json_decode("+x.Expr.eval (programList[prIndex])+","+p.ToString()+")")
    /// JSONファイルをエンコード
    static member json_encode (x:num0) = num0(Var(Nt, "json_encode("+x.Expr.eval (programList[prIndex])+", JSON_PRETTY_PRINT|JSON_UNESCAPED_UNICODE|JSON_UNESCAPED_SLASHES )", NaN))
    /// 指定したキーの値の配列を生成
    static member array_column(data:num0,id:string) = num0(Var(Nt, "array_column("+data.Expr.eval (programList[prIndex])+",\""+id+"\")", NaN))
    /// 指定した要素が含まれているか判定
    static member in_array_strict(s:num0, idArray:num0) = bool0(Var(Nt, "in_array("+s.Expr.eval (programList[prIndex])+", "+idArray.Expr.eval (programList[prIndex])+", true)", NaN))
    /// 指定した要素の配列内でのインデックス（キー）を検索
    static member array_search(s:num0, idArray:num0) = num0(Var(Nt, "array_search("+s.Expr.eval (programList[prIndex])+", "+idArray.Expr.eval (programList[prIndex])+")", NaN))
    /// 配列を生成
    static member array() = Var(Nt, "array()", NaN)
    /// 配列に要素を追加
    static member array_push(a:num0,el:num0) = php.phpcode <| fun () -> programList[prIndex].codewrite("array_push("+a.Expr.eval (programList[prIndex])+","+el.Expr.eval (programList[prIndex])+")")
    /// ファイル内のテキストを配列に格納
    static member file(filename:num0, flag:list<FileFlag>) = 
        num0(Var(Nt, "file("+filename.Expr.eval (programList[prIndex])+", "+(flag |> List.map (fun s -> s.str) |> (fun p -> String.Join(" | ",p)) )+")", NaN))
    /// ファイル内のテキストを配列に格納
    static member file(filename:string, flag:list<FileFlag>) = 
        num0(Var(Nt, "file('"+filename+"', "+(flag |> List.map (fun s -> s.str) |> (fun p -> String.Join(" | ",p)) )+")", NaN))
    /// ファイルを開く
    static member fopen(filename:num0,rw:FileOpenMode) = num0(Var(Nt, "fopen("+filename.Expr.eval (programList[prIndex])+", "+rw.str+")", NaN))
    /// ファイルを開く
    static member fopen(filename:string,rw:FileOpenMode) = num0(Var(Nt, "fopen(\""+filename+"\", "+rw.str+")", NaN))
    /// ファイルに書き込み
    static member fwrite(fp:num0,t:exprString) = php.phpcode <| fun () -> programList[prIndex].codewrite("fwrite("+fp.Expr.eval (programList[prIndex])+", "+t.toString("",Direct)+");")
    /// ファイルに書き込み
    static member fwrite(fp:num0,t:string) = php.fwrite(fp,Str t)
    /// ファイルに書き込み
    static member fwrite(fp:num0,t:int) = php.phpcode <| fun () -> programList[prIndex].codewrite("fwrite("+fp.Expr.eval (programList[prIndex])+", "+t.ToString()+");")
    /// ファイルにShift-JISで書き込み
    static member fwrite_SJIS(fp:num0,t:num0) = php.phpcode <| fun () -> programList[prIndex].codewrite("fwrite("+fp.Expr.eval (programList[prIndex])+", mb_convert_encoding("+t.Expr.eval (programList[prIndex])+", 'SJIS-win', 'UTF-8'));")
    /// ファイルにShift-JISで書き込み
    static member fwrite_SJIS(fp:num0,t:string) = php.phpcode <| fun () -> programList[prIndex].codewrite("fwrite("+fp.Expr.eval (programList[prIndex])+", mb_convert_encoding(\""+t+"\", 'SJIS-win', 'UTF-8'));")
    /// ファイルを閉じる
    static member fclose(filename:num0) = php.phpcode <| fun () -> programList[prIndex].codewrite("fclose("+filename.Expr.eval (programList[prIndex])+");")
    /// 正規表現
    static member preg_match(p:string,text:num0,mat:num0) = php.phpcode <| fun () -> programList[prIndex].codewrite("preg_match('"+p+"',"+text.Expr.eval (programList[prIndex])+","+mat.Expr.eval (programList[prIndex])+");")
    /// ファイルのダウンロード
    static member download(filename:string) =
        php.phpcode <| fun () -> programList[prIndex].codewrite "header('Content-Type: application/octet-stream');"
        php.phpcode <| fun () -> programList[prIndex].codewrite ("header('Content-Length: '.filesize(\""+filename+"\"));")
        php.phpcode <| fun () -> programList[prIndex].codewrite ("header('Content-Disposition: attachment; filename=\""+filename+"\"');")
        php.phpcode <| fun () -> programList[prIndex].codewrite ("readfile(\""+filename+"\");")
        php.phpcode <| fun () -> programList[prIndex].codewrite "exit;"
    /// 整数に変換
    static member intval(s:num0) = num0(Var(Nt, "intval("+s.Expr.eval (programList[prIndex])+")", NaN))
    /// 配列要素の和
    static member array_sum(data:num0) = num0(Var(Nt, "array_sum("+data.Expr.eval (programList[prIndex])+")", NaN))
    /// 文字数
    static member strlen(data:num0) = num0(Var(Nt, "strlen("+data.Expr.eval (programList[prIndex])+")", NaN))
    /// 数値かどうか判定
    static member is_numeric(data:num0) = bool0(Var(Nt, "is_numeric("+data.Expr.eval (programList[prIndex])+")", NaN))
    /// 否定演算
    static member nt (data:bool0) = bool0(Var(Nt, "!"+data.Expr.eval (programList[prIndex]),NaN))
    ///<summary>送信データをキャッシュしない（Firefoxでフォームの選択肢がリロード前から保持される現象を回避）</summary>
    static member set_nocache() = php.phpcode <| fun () -> programList[prIndex].codewrite "header( 'Cache-Control: no-store, no-cache, must-revalidate' );"
    /// HTTPヘッダを取得
    static member header(data:num0) = php.phpcode <| fun () -> programList[prIndex].codewrite("header("+data.Expr.eval (programList[prIndex])+")")
    /// HTTPヘッダを取得
    static member header(data:string) = php.phpcode <| fun () -> programList[prIndex].codewrite("header('"+data+"')")
    // 小数に変換
    // static member float(data:num0) = Var("(float)"+data.Expr.eval (programList[prIndex]))
    // 絶対値
    // static member abs(data:num0) = Var("abs("+data.Expr.eval (programList[prIndex])+")")
    /// 日付を取得
    static member date(fmt:string) = num0(Var(Nt, "date(\""+fmt+"\")", NaN))
    /// 整数に丸め
    static member round(x:num0) = num0(Var(Nt, "round("+x.Expr.eval (programList[prIndex])+")", NaN))
    /// 文字列切り出し
    static member substr(x:num0,n:num0) = num0(Var(Nt, "substr("+x.Expr.eval (programList[prIndex])+","+n.Expr.eval (programList[prIndex])+")", NaN))
    /// 文字列切り出し
    static member substr(x:num0,n:int) = num0(Var(Nt, "substr("+x.Expr.eval (programList[prIndex])+","+n.ToString()+")", NaN))
    /// ファイルが存在するか確認
    static member file_exists(x:num0) = bool0(Var(Nt, "file_exists("+x.Expr.eval (programList[prIndex])+")", NaN))
    /// ファイルが存在するか確認
    static member file_exists(x:string) = bool0(Var(Nt, "file_exists(\""+x+"\")", NaN))
    /// 文字数（全角も1字扱い）
    static member mb_strlen(x:num0) = num0(Var(Nt, "mb_strlen("+x.Expr.eval (programList[prIndex])+")", NaN))
    /// 文字数（全角も1字扱い）
    static member mb_strwidth(x:num0) = num0(Var(Nt, "mb_strwidth("+x.Expr.eval (programList[prIndex])+")", NaN))
    /// 文字列比較
    static member strncmp(x:num0,y:string,n:int) = num0(Var(Nt, "strncmp("+x.Expr.eval (programList[prIndex])+",\""+y+"\","+n.ToString()+")", NaN))
    /// 指定したパターンにマッチするファイルパス取得
    static member glob(x:num0) = num0(Var(Nt, "glob("+x.Expr.eval (programList[prIndex])+")", NaN))
    /// 指定したパターンにマッチするファイルパス取得
    static member glob(x:string) = num0(Var(Nt, "glob(\""+x+"\")", NaN))
    /// 文字列分割
    static member explode(x:num0,y:num0) = num0(Var(Nt, "explode("+x.Expr.eval (programList[prIndex])+","+y.Expr.eval (programList[prIndex])+")", NaN))
    /// 文字列分割
    static member explode(x:string,y:num0) = num0(Var(Nt, "explode('"+x+"',"+y.Expr.eval (programList[prIndex])+")", NaN))
    /// 配列のソート
    static member sort(data:num0) = php.phpcode <| fun () -> programList[prIndex].codewrite("sort("+data.Expr.eval (programList[prIndex])+")")
    /// 整数に変換
    static member toint(x:num0) = num0(Var(Nt, "(int)"+x.Expr.eval (programList[prIndex]), NaN))
    /// 配列要素数
    static member count(x:num0) = num0(Var(Nt, "count("+x.Expr.eval (programList[prIndex])+")", NaN))
    static member count(x:PHPdata) = num0(Var(Nt, "count("+x.code+")", NaN))
    /// 拡張子を除いたファイル名
    static member filename_withoutExtension(x:num0) = num0(Var(Nt, "pathinfo("+x.Expr.eval (programList[prIndex])+", PATHINFO_FILENAME)", NaN))
    /// ファイル削除
    static member unlink(data:num0) = php.phpcode <| fun () -> programList[prIndex].codewrite("unlink("+data.Expr.eval (programList[prIndex])+")")
    /// 配列要素をランダムに入れ替え
    static member shuffle(data:num0) = php.phpcode <| fun () -> programList[prIndex].codewrite("shuffle("+data.Expr.eval (programList[prIndex])+")")
    /// タイムゾーン設定
    static member setTimeZone(location:string) = php.phpcode <| fun () -> programList[prIndex].codewrite("date_default_timezone_set('"+location+"');")
    /// メール送信
    static member sendMail(body:string,subject:string,smtp:string,fromAddress:string,toAddress:string) =
        let cmd = num0.var "cmd"
        //cmd <== "echo \\\""+body+"\\\" | s-nail -s \\\""+subject+"\\\" -Smta=smtp://"+smtp+":25 -r "+fromAddress+" "+toAddress
        cmd <== php.fnvar("echo \\\"" + body + "\\\" | mail -s \\\"" + subject + "\\\" -S smtp=smtp://" + smtp + ":25 -r " + fromAddress + " " + toAddress)
        php.phpcode <| fun () -> programList[prIndex].codewrite("exec("+cmd.Expr.eval (programList[prIndex])+");")
    /// メール送信
    static member sendMail(body:num0,subject:num0,fromAddress:string,toAddress:num0) =
        php.phpcode <| fun () -> programList[prIndex].codewrite("mb_language(\"ja\");")
        php.phpcode <| fun () -> programList[prIndex].codewrite("mb_internal_encoding(\"UTF-8\");")
        php.phpcode <| fun () -> programList[prIndex].codewrite("mb_send_mail("+toAddress.Expr.eval (programList[prIndex])+","+subject.Expr.eval (programList[prIndex])+","+body.Expr.eval (programList[prIndex])+","+"\"From: "+fromAddress+"\");")
    /// メール送信
    static member sendMail(body:string,subject:string,fromAddress:string,toAddress:string) =
        php.phpcode <| fun () -> programList[prIndex].codewrite("mb_language(\"ja\");")
        php.phpcode <| fun () -> programList[prIndex].codewrite("mb_internal_encoding(\"UTF-8\");")
        php.phpcode <| fun () -> programList[prIndex].codewrite("mb_send_mail(\""+toAddress+"\",\""+subject+"\",\""+body+"\","+"\"From: "+fromAddress+"\");")
    /// Discordへメッセージ送信
    static member sendDiscord(body:num0,webhookURL:num0) =
        let cmd = num0.var "cmd"
        cmd <== php.fnvar("curl -H \\\"Content-Type: application/json\\\" -X POST -d \\\"{\\\\\\\"username\\\\\\\": \\\\\\\"Ediass Notification\\\\\\\", \\\\\\\"content\\\\\\\": \\\\\\\""++body++"\\\\\\\"}\\\" "++webhookURL)
        php.phpcode <| fun () -> programList[prIndex].codewrite("exec("+cmd.Expr.eval (programList[prIndex])+");")
    /// Discordへメッセージ送信
    static member sendDiscord(body:string,webhookURL:string) =
        let cmd = num0.var "cmd"
        cmd <== php.fnvar("curl -H \\\"Content-Type: application/json\\\" -X POST -d \\\"{\\\\\\\"username\\\\\\\": \\\\\\\"Ediass Notification\\\\\\\", \\\\\\\"content\\\\\\\": \\\\\\\"" + body + "\\\\\\\"}\\\" " + webhookURL)
        php.phpcode <| fun () -> programList[prIndex].codewrite("exec("+cmd.Expr.eval (programList[prIndex])+");")
    /// 文字列置換
    static member str_replace(strfrom:string,strto:string,str:num0) = num0(Var(Nt, "str_replace("+"\""+strfrom+"\""+","+"\""+strto+"\""+","+str.Expr.eval (programList[prIndex])+")", NaN))
    /// 指定した文字数になるまで文字を埋める
    static member str_pad(num:num0,ndigit:int,paddingnum:int) = num0(Var(Nt, "str_pad("+num.Expr.eval (programList[prIndex])+","+ndigit.ToString()+","+paddingnum.ToString()+", STR_PAD_LEFT)", NaN))
    /// ファイルダウンロード
    static member file_download(file:num0) =
        php.phpcode <| fun () ->
            programList[prIndex].codewrite("header('Content-Type: application/octet-stream');")
        php.phpcode <| fun () ->
            programList[prIndex].codewrite("header('Content-Transfer-Encoding: Binary');")
        php.phpcode <| fun () ->
            programList[prIndex].codewrite("header('Content-disposition: attachment; filename='.basename("+file.Expr.eval (programList[prIndex])+"));")
        php.phpcode <| fun () ->
            programList[prIndex].codewrite("header('Content-Length: '.filesize("+file.Expr.eval (programList[prIndex])+"));")
        php.phpcode <| fun () ->
            programList[prIndex].codewrite("while (ob_get_level()) { ob_end_clean(); }")
        php.phpcode <| fun () ->
            programList[prIndex].codewrite("readfile("+file.Expr.eval (programList[prIndex])+");")
        php.phpcode <| fun () ->
            programList[prIndex].codewrite("exit;")
    /// ファイルパスからファイル名取得
    static member basename(file:num0) = num0(Var(Nt, "basename("+file.Expr.eval (programList[prIndex])+")", NaN))
    /// 改行文字
    static member br = "\\n"
    /// タブ文字
    static member tb = "\\t"
// type bh() =
//     member this.If(s:bool0) = fun code ->
//         php.phpcode <| fun () -> programList[prIndex].codewrite("if("+s.Expr.eval (programList[prIndex])+"):")
//         code()
//     member this.ElseIf(s:bool0) = fun code ->
//         php.phpcode <| fun () -> programList[prIndex].codewrite("elseif("+s.Expr.eval (programList[prIndex])+"):")
//         code()
//     member this.Else code =
//         php.phpcode <| fun () -> programList[prIndex].codewrite "else:"
//         code()
// type br =
//     static member branch code =
//         let b = bh()
//         code b
//         php.phpcode <| fun () -> programList[prIndex].codewrite "endif;"

//     static member if1 (p:bool0) = fun code ->
//         br.branch <| fun b ->
//             b.If p <| fun () ->
//                 code()
//     static member if2 (p:bool0) = fun code1 code2 ->
//         br.branch <| fun b ->
//             b.If p <| fun () ->
//                 code1()
//             b.Else <| fun () ->
//                 code2()
//type iter =
//    static member range (i1:num0,i2:num0) = fun code ->
//        icounter <- icounter + 1
//        let i = num0.var("ic"+icounter.ToString())
//        php.phpcode <| fun () -> programList[prIndex].codewrite("for("+i.Expr.eval (programList[prIndex])+"="+i1.Expr.eval (programList[prIndex])+"; "+i.Expr.eval (programList[prIndex])+"<="+i2.Expr.eval (programList[prIndex])+"; "+i.Expr.eval (programList[prIndex])+"++):")
//        code(i)
//        php.phpcode <| fun () -> programList[prIndex].codewrite("endfor;")
//    static member range (i1:num0,i2:int) = fun code ->
//        icounter <- icounter + 1
//        let i = num0.var("ic"+icounter.ToString())
//        php.phpcode <| fun () -> programList[prIndex].codewrite("for("+i.Expr.eval (programList[prIndex])+"="+i1.Expr.eval (programList[prIndex])+"; "+i.Expr.eval (programList[prIndex])+"<="+i2.ToString()+"; "+i.Expr.eval (programList[prIndex])+"++):")
//        code(i)
//        php.phpcode <| fun () -> programList[prIndex].codewrite("endfor;")
//    static member range (i1:int,i2:num0) = fun code ->
//        icounter <- icounter + 1
//        let i = num0.var("ic"+icounter.ToString())
//        php.phpcode <| fun () -> programList[prIndex].codewrite("for("+i.Expr.eval (programList[prIndex])+"="+i1.ToString()+"; "+i.Expr.eval (programList[prIndex])+"<="+i2.Expr.eval (programList[prIndex])+"; "+i.Expr.eval (programList[prIndex])+"++):")
//        code(i)
//        php.phpcode <| fun () -> programList[prIndex].codewrite("endfor;")
//    static member range (i1:int,i2:int) = fun code ->
//        icounter <- icounter + 1
//        let i = num0.var("ic"+icounter.ToString())
//        php.phpcode <| fun () -> programList[prIndex].codewrite("for("+i.Expr.eval (programList[prIndex])+"="+i1.ToString()+"; "+i.Expr.eval (programList[prIndex])+"<="+i2.ToString()+"; "+i.Expr.eval (programList[prIndex])+"++):")
//        code(i)
//        php.phpcode <| fun () -> programList[prIndex].codewrite("endfor;")
//    static member foreach (array:num0) = fun code ->
//        icounter <- icounter + 1
//        let i = num0.var("ic"+icounter.ToString())
//        php.phpcode <| fun () -> programList[prIndex].codewrite("for("+i.Expr.eval (programList[prIndex])+"=0; "+i.Expr.eval (programList[prIndex])+"<count("+array.Expr.eval (programList[prIndex])+"); "+i.Expr.eval (programList[prIndex])+"++):")
//        code(i)
//        php.phpcode <| fun () -> programList[prIndex].codewrite("endfor;")
//    static member foreach (array:num0,key:num0,value:num0) = fun code ->
//        icounter <- icounter + 1
//        let i = num0.var("ic"+icounter.ToString())
//        php.phpcode <| fun () -> programList[prIndex].codewrite("foreach("+array.Expr.eval (programList[prIndex])+" as "+key.Expr.eval (programList[prIndex])+" => "+value.Expr.eval (programList[prIndex])+"):")
//        code()
//        php.phpcode <| fun () -> programList[prIndex].codewrite("endforeach;")