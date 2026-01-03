namespace Aqualis

open System
open System.IO
        
type PHPbool(x:string) =
    
    member this.name with get() = x
    static member var(x) = PHPbool("$"+x)
    static member (<==) (a:PHPbool,b:PHPbool) = codewritein ("<?php " + a.name + " = " + b.name + " ?>")
    
type PHPdata(x:list<reduceExprString>) =
    new(x:string) = PHPdata [RStr x]
    new(x:num0) = PHPdata [RNvr x.Expr]
    member _.data with get() = x
    member this.extcode(pr:program) = "<?php echo " + this.code + "; ?>"
    static member v x = PHPdata [RNvr(Var(Nt,"$"+x,NaN))]
    static member v (x,init:PHPdata) = 
        let v = PHPdata.v x
        v <== init
        v
    static member v (x,init:num0) = 
        let v = PHPdata.v x
        v <== init
        v
    static member v (x,init:int) = 
        let v = PHPdata.v x
        v <== I init
        v
    static member v (x,init:double) = 
        let v = PHPdata.v x
        v <== D init
        v
    static member f(s:string) = PHPdata [RNvr(Var(Nt,s,NaN))]
    static member str (x:num0) = "\"" ++ x ++ "\""
    
    /// 配列を生成
    static member array() = PHPdata.f "array()"

    static member array(arrayname:string) = 
        let c = PHPdata.v arrayname
        codewritein ("<?php $"+arrayname+" = array(); ?>")
        c
        
    static member array(arrayname:string,data:list<string*string>) = 
        let c = PHPdata.v arrayname
        codewritein ("<?php "+arrayname+" = array(); ?>")
        codewritein ("<?php "+arrayname+"[] = array("+String.Join(",",data |> List.map (fun (a,b) -> "'"+a+"'=>'"+b+"'"))+"); ?>")
        c

    static member array(arrayname:string,data:list<string*PHPdata>) = 
        let c = PHPdata.v arrayname
        codewritein ("<?php "+arrayname+" = array(); ?>")
        codewritein ("<?php "+arrayname+"[] = array("+String.Join(",",data |> List.map (fun (a,b) -> "'"+a+"'=>"+b.code))+"); ?>")
        c
    // static member array(arrayname:string,data:list<string*PHPdata>) = 
    //     let c = PHPdata.var arrayname
    //     codewritein ("<?php "+arrayname+" = array(); ?>")
    //     codewritein ("<?php "+arrayname+"[] = array("+String.Join(",",data |> List.map (fun (a,b) -> "'"+a+"'=>"+b.code))+"); ?>")
    //     c

    // /// 配列に要素を追加
    // static member array_push(a:PHPdata,el:PHPdata) = php.phpcode <| fun () -> codewrite("array_push("+a.code+","+el.code+")")
    // static member array_push(a:PHPdata,el:num0) = php.array_push(a,PHPdata el)
    
    member this.push (x:list<PHPdata>) = codewritein ("<?php array_push(" + this.code + ", " + String.Join(",",List.map(fun (q:PHPdata) -> q.code) x) + "); ?>")
    member this.push (x:PHPdata) = this.push [x]
    // member this.push (x:num0) = this.push [PHPdata x]
    // member this.push (x:list<exprString>) = codewritein ("<?php array_push(" + this.code + ", " + String.Join(",",x |> List.map(fun q -> q.toString("",Direct))) + "); ?>")
    ///配列に要素を複数追加
    member this.push (x:list<num0>) = codewritein ("<?php array_push(" + this.code + ", " + String.Join(",",List.map(fun (q:num0) -> q.code) x) + "); ?>")
    ///配列に文字列要素を複数追加
    member this.push (x:list<string>) = this.push (List.map(fun (q:string) -> PHPdata q) x)
    ///配列に要素を追加
    member this.push (x:num0) = this.push [x]
    ///配列に文字列要素を追加
    member this.push (x:string) = this.push [x]
    member this.toString(c:string,op:ExprConcatOption) =
        x
        |> List.map (function
            |RStr x ->
                match op with
                |Direct -> x
                |StrQuotation -> "\""+x+"\""
                |CodeStrQuotation -> "\\\""+x+"\\\""
            |RNvr x -> x.eval (programList[prIndex]))
        |> fun s -> String.Join(c,s)
    member this.Item(i:PHPdata) = PHPdata [RNvr(Var(Nt,this.toString(" . ",StrQuotation) + "[" + i.toString(" . ",StrQuotation) + "]",NaN))]
    member this.Item(i:int) = this[PHPdata [RNvr(Int i)]]
    member this.Item(i:string) = this[PHPdata [RStr i]]
    member this.Item(i:num0) = this[PHPdata[RNvr i.Expr]]
    member this.code with get() = this.toString(" . ",StrQuotation)
    member this.phpcode with get() = "<?php echo " + this.code + " ?>"
    static member (++) (a:PHPdata,b:PHPdata) = PHPdata(a.data@b.data)
    static member (++) (a:string,b:PHPdata) = PHPdata a ++ b
    static member (++) (a:PHPdata,b:string) = a ++ PHPdata b
    static member (++) (a:PHPdata,b:num0) = a ++ PHPdata b
    

    member this.foreach code =
        ch.i <| fun i ->
            php.phpcode <| fun () -> codewrite("for("+i.code+"=0; "+i.code+"<count("+this.code+"); "+i.code+"++):")
            programList[prIndex].indentInc()
            code i
            programList[prIndex].indentDec()
            php.phpcode <| fun () -> codewrite "endfor;"
    member this.foreach (key:PHPdata,value:PHPdata) = fun code ->
        ch.i <| fun i ->
            php.phpcode <| fun () -> codewrite("foreach("+this.code+" as "+key.code+" => "+value.code+"):")
            code()
            php.phpcode <| fun () -> codewrite "endforeach;"
    static member (<==) (a:PHPdata,b:PHPdata) = codewritein ("<?php " + a.code + " = " + b.code + "; ?>")
    static member (<==) (a:PHPdata,b:string) = a <== PHPdata b
    static member (<==) (a:PHPdata,b:num0) = a <== PHPdata b
    static member (<==) (a:PHPdata,b:int) = a <== PHPdata (I b)
    static member (.=) (a:PHPdata,b:PHPdata) = num0(Var(Nt,a.code,NaN)) .= num0(Var(Nt,b.code,NaN))
    static member (.=) (a:PHPdata,b:num0) = a .= PHPdata b
    static member (.=) (a:PHPdata,b:int) = a .= PHPdata (I b)
    static member (.=) (a:PHPdata,b:string) = a .= PHPdata b
    static member (.=/) (a:PHPdata,b:PHPdata) = num0(Var(Nt,a.code,NaN)) .=/ num0(Var(Nt,b.code,NaN))
    static member (.=/) (a:PHPdata,b:num0) = a .=/ PHPdata b
    static member (.=/) (a:PHPdata,b:int) = a .=/ PHPdata (I b)
    static member (.=/) (a:PHPdata,b:string) = a .=/ PHPdata b
    static member (.<) (a:PHPdata,b:PHPdata) = num0(Var(Nt,a.code,NaN)) .< num0(Var(Nt,b.code,NaN))
    static member (.<) (a:PHPdata,b:int) = a .< PHPdata (I b)
    static member (.<=) (a:PHPdata,b:PHPdata) = num0(Var(Nt,a.code,NaN)) .<= num0(Var(Nt,b.code,NaN))
    static member (.<=) (a:PHPdata,b:int) = a .<= PHPdata (I b)
    static member (.>) (a:PHPdata,b:PHPdata) = num0(Var(Nt,a.code,NaN)) .> num0(Var(Nt,b.code,NaN))
    static member (.>) (a:PHPdata,b:int) = a .> PHPdata (I b)
    static member (.>=) (a:PHPdata,b:PHPdata) = num0(Var(Nt,a.code,NaN)) .>= num0(Var(Nt,b.code,NaN))
    static member (.>=) (a:PHPdata,b:int) = a .>= PHPdata (I b)
    
and php =
    /// phpファイルを生成
    static member phpfile (dir:string,projectname:string) code =
        makeProgram [dir, projectname, PHP] <| fun () ->
            code ()
            programList[prIndex].close()
            
    /// htmlコード内にphpコードを埋め込み
    static member phpcode (code:unit->unit) =
        codewritei "<?php "
        code()
        codewriten " ?>"
    /// POST送信されたデータを表示
    static member postCheck() = codewritein "<?php print_r($_POST); ?>"
    /// POST送信されたファイルを表示
    static member postFileCheck() = codewritein "<?php print_r($_FILES); ?>"
    /// 論理積
    static member And (x:list<bool0>) = bool0(Var(Nt, "(" + String.Join(" && ", x |> List.map (fun s -> s.code)) + ")",NaN))
    /// 論理和
    static member Or (x:list<bool0>) = bool0(Var(Nt, "(" + String.Join(" || ", x |> List.map (fun s -> s.code)) + ")",NaN))
    /// 指定された変数がPOST送信されたか判定
    static member isset (x:PHPdata) = bool0(Var(Nt, "isset(" + x.code + ")",NaN))
    static member echo (x:PHPdata) = php.phpcode <| fun () -> codewrite("echo " + x.code + ";")
    // static member echo (x:exprString) = php.phpcode <| fun () -> codewrite("echo " + x.toString(" . ",StrQuotation) + ";")
    /// 文字列を表示
    static member echo (x:string) = php.echo (PHPdata x)
    /// 変数を表示
    static member echo (x:num0) = php.echo (PHPdata x)
    /// ファイル内のテキストを取得
    static member file_get_contents (filename:PHPdata) = PHPdata.f("file_get_contents(" + filename.code + ")")
    /// ファイルにテキストを書き込み
    static member file_put_contents (filename:PHPdata,x:PHPdata) = php.phpcode <| fun () -> codewrite("file_put_contents("+filename.code+","+x.code+");")
    /// JSONファイルをデコード
    static member json_decode (x:PHPdata,p:bool) = PHPdata.f("json_decode("+x.code+","+p.ToString()+")")
    /// JSONファイルをエンコード
    static member json_encode (x:PHPdata) = PHPdata.f("json_encode("+x.code+", JSON_PRETTY_PRINT|JSON_UNESCAPED_UNICODE|JSON_UNESCAPED_SLASHES )")
    /// 指定したキーの値の配列を生成
    static member array_column(data:PHPdata,id:PHPdata) = PHPdata.f("array_column("+data.code+","+id.code+")")
    /// 指定した要素が含まれているか判定
    static member in_array_strict(s:PHPdata, idArray:PHPdata) = bool0(Var(Nt, "in_array("+s.code+", "+idArray.code+", true)",NaN))
    static member in_array_strict(s:num0, idArray:PHPdata) = php.in_array_strict(PHPdata s, idArray)
    /// 指定した要素の配列内でのインデックス（キー）を検索
    static member array_search(s:PHPdata, idArray:PHPdata) = PHPdata.f("array_search("+s.code+", "+idArray.code+")")
    /// ファイル内のテキストを配列に格納
    static member file(filename:PHPdata, flag:list<FileFlag>) = 
        PHPdata.f("file("+filename.code+", "+(flag |> List.map (fun s -> s.str) |> (fun p -> String.Join(" | ",p)) )+")")
    /// ファイルを開く
    static member fopen(filename:PHPdata,rw:FileOpenMode) = PHPdata.f("fopen("+filename.code+", "+rw.str+")")
    /// ファイルに書き込み
    static member fwrite(fp:PHPdata,t:PHPdata) = php.phpcode <| fun () -> codewrite("fwrite("+fp.code+", "+t.code+");")
    /// ファイルに書き込み
    static member fwrite(fp:PHPdata,t:int) = php.phpcode <| fun () -> codewrite("fwrite("+fp.code+", "+t.ToString()+");")
    /// ファイルにShift-JISで書き込み
    static member fwrite_SJIS(fp:PHPdata,t:string) = php.phpcode <| fun () -> codewrite("fwrite("+fp.code+", mb_convert_encoding(\""+t+"\", 'SJIS-win', 'UTF-8'));")
    /// ファイルを閉じる
    static member fclose(filename:PHPdata) = php.phpcode <| fun () -> codewrite("fclose("+filename.code+");")
    /// 正規表現
    static member preg_match(p:PHPdata,text:PHPdata,mat:PHPdata) = php.phpcode <| fun () -> codewrite("preg_match("+p.code+","+text.code+","+mat.code+");")
    /// ファイルのダウンロード
    static member download(filename:string) =
        php.phpcode <| fun () -> codewrite "header('Content-Type: application/octet-stream');"
        php.phpcode <| fun () -> codewrite ("header('Content-Length: '.filesize(\""+filename+"\"));")
        php.phpcode <| fun () -> codewrite ("header('Content-Disposition: attachment; filename=\""+filename+"\"');")
        php.phpcode <| fun () -> codewrite ("readfile(\""+filename+"\");")
        php.phpcode <| fun () -> codewrite "exit;"
    /// 整数に変換
    static member intval(s:PHPdata) = PHPdata.f("intval("+s.code+")")
    /// 配列要素の和
    static member array_sum(data:PHPdata) = PHPdata.f("array_sum("+data.code+")")
    /// 文字数
    static member strlen(data:PHPdata) = PHPdata.f("strlen("+data.code+")")
    /// 数値かどうか判定
    static member is_numeric(data:PHPdata) = bool0(Var(Nt, "is_numeric("+data.code+")",NaN))
    /// 否定演算
    static member nt (data:bool0) = bool0(Var(Nt, "!"+data.code,NaN))
    ///<summary>送信データをキャッシュしない（Firefoxでフォームの選択肢がリロード前から保持される現象を回避）</summary>
    static member set_nocache() = php.phpcode <| fun () -> codewrite "header( 'Cache-Control: no-store, no-cache, must-revalidate' );"
    /// HTTPヘッダを取得
    static member header(data:PHPdata) = php.phpcode <| fun () -> codewrite("header("+data.code+");")
    // 小数に変換
    // static member float(data:num0) = Var("(float)"+data.code)
    // 絶対値
    // static member abs(data:num0) = Var("abs("+data.code+")")
    /// 日付を取得
    static member date(fmt:string) = PHPdata.f("date(\""+fmt+"\")")
    /// 整数に丸め
    static member round(x:PHPdata) = PHPdata.f("round("+x.code+")")
    /// 文字列切り出し
    static member substr(x:PHPdata,n:PHPdata) = PHPdata.f("substr("+x.code+","+n.code+")")
    /// 文字列切り出し
    static member substr(x:PHPdata,n:int) = PHPdata.f("substr("+x.code+","+n.ToString()+")")
    /// ファイルが存在するか確認
    static member file_exists(x:PHPdata) = bool0(Var(Nt, "file_exists("+x.code+")",NaN))
    /// ファイルが存在するか確認
    static member file_exists(x:string) = bool0(Var(Nt, "file_exists(\""+x+"\")",NaN))
    /// 文字数（全角も1字扱い）
    static member mb_strlen(x:PHPdata) = PHPdata.f("mb_strlen("+x.code+")")
    /// 文字数（全角も1字扱い）
    static member mb_strwidth(x:PHPdata) = PHPdata.f("mb_strwidth("+x.code+")")
    /// 文字列比較
    static member strncmp(x:PHPdata,y:string,n:int) = PHPdata.f("strncmp("+x.code+",\""+y+"\","+n.ToString()+")")
    /// 指定したパターンにマッチするファイルパス取得
    static member glob(x:PHPdata) = PHPdata.f("glob("+x.code+")")
    /// 指定したパターンにマッチするファイルパス取得
    static member glob(x:string) = PHPdata.f("glob(\""+x+"\")")
    /// 文字列分割
    static member explode(x:PHPdata,y:PHPdata) = PHPdata.f("explode("+x.code+","+y.code+")")
    /// 文字列分割
    static member explode(x:string,y:PHPdata) = PHPdata.f("explode('"+x+"',"+y.code+")")
    /// 配列のソート
    static member sort(data:PHPdata) = php.phpcode <| fun () -> codewrite("sort("+data.code+");")
    /// 整数に変換
    static member toint(x:PHPdata) = PHPdata.f("(int)"+x.code)
    /// 配列要素数
    static member count(x:PHPdata) = num0(Var(It 4, "count("+x.code+")", NaN))
    /// 拡張子を除いたファイル名
    static member filename_withoutExtension(x:PHPdata) = PHPdata.f("pathinfo("+x.code+", PATHINFO_FILENAME)")
    /// ファイル削除
    static member unlink(data:PHPdata) = php.phpcode <| fun () -> codewrite("unlink("+data.code+");")
    /// 配列要素をランダムに入れ替え
    static member shuffle(data:PHPdata) = php.phpcode <| fun () -> codewrite("shuffle("+data.code+");")
    /// タイムゾーン設定
    static member setTimeZone(location:string) = php.phpcode <| fun () -> codewrite("date_default_timezone_set('"+location+"');")
    /// メール送信
    static member sendMail(body:PHPdata,subject:PHPdata,fromAddress:PHPdata,toAddress:PHPdata) =
        php.phpcode <| fun () -> codewrite "mb_language(\"ja\");"
        php.phpcode <| fun () -> codewrite "mb_internal_encoding(\"UTF-8\");"
        php.phpcode <| fun () -> codewrite("mb_send_mail("+toAddress.code+","+subject.code+","+body.code+","+("From: "++fromAddress).code+");")
    // /// メール送信
    // static member sendMail(body:exprString,subject:exprString,fromAddress:string,toAddress:PHPdata) =
    //     php.phpcode <| fun () -> codewrite("mb_language(\"ja\");")
    //     php.phpcode <| fun () -> codewrite("mb_internal_encoding(\"UTF-8\");")
    //     php.phpcode <| fun () -> codewrite("mb_send_mail("+toAddress.code+","+subject.toString(" . ",StrQuotation)+","+body.toString(" . ",StrQuotation)+","+"\"From: "+fromAddress+"\");")
    // /// メール送信
    // static member sendMail(body:PHPdata,subject:PHPdata,fromAddress:string,toAddress:PHPdata) =
    //     php.phpcode <| fun () -> codewrite("mb_language(\"ja\");")
    //     php.phpcode <| fun () -> codewrite("mb_internal_encoding(\"UTF-8\");")
    //     php.phpcode <| fun () -> codewrite("mb_send_mail("+toAddress.code+","+subject.code+","+body.code+","+"\"From: "+fromAddress+"\");")
    /// メール送信
    static member sendMail(body:PHPdata,subject:PHPdata,smtp:PHPdata,fromAddress:PHPdata,toAddress:PHPdata) =
        let cmd = PHPdata.v "cmd"
        cmd <== "echo \\\"" ++ body ++ "\\\" | mail -s \\\"" ++ subject ++ "\\\" -S smtp=smtp://" ++ smtp ++ ":25 -r " ++ fromAddress ++ " " ++ toAddress
        php.phpcode <| fun () -> codewrite("exec("+cmd.code+");")
    // /// メール送信
    // static member sendMail(body:PHPdata,subject:PHPdata,fromAddress:string,toAddress:PHPdata) =
    //     php.phpcode <| fun () -> codewrite("mb_language(\"ja\");")
    //     php.phpcode <| fun () -> codewrite("mb_internal_encoding(\"UTF-8\");")
    //     php.phpcode <| fun () -> codewrite("mb_send_mail("+toAddress.code+","+subject.code+","+body.code+","+"\"From: "+fromAddress+"\");")
    // /// メール送信
    // static member sendMail(body:string,subject:string,fromAddress:string,toAddress:string) =
    //     php.phpcode <| fun () -> codewrite("mb_language(\"ja\");")
    //     php.phpcode <| fun () -> codewrite("mb_internal_encoding(\"UTF-8\");")
    //     php.phpcode <| fun () -> codewrite("mb_send_mail(\""+toAddress+"\",\""+subject+"\",\""+body+"\","+"\"From: "+fromAddress+"\");")
    /// Discordへメッセージ送信
    static member sendDiscord(body:PHPdata,webhookURL:PHPdata) =
        let cmd = PHPdata.v "cmd"
        cmd <== "curl -H \\\"Content-Type: application/json\\\" -X POST -d \\\"{\\\\\\\"username\\\\\\\": \\\\\\\"Ediass Notification\\\\\\\", \\\\\\\"content\\\\\\\": \\\\\\\""++body++"\\\\\\\"}\\\" " ++ webhookURL
        php.phpcode <| fun () -> codewrite("exec("+cmd.code+");")
    // /// Discordへメッセージ送信
    // static member sendDiscord(body:string,webhookURL:string) =
    //     let cmd = PHPdata.v "cmd"
    //     cmd <== php.fnvar("curl -H \\\"Content-Type: application/json\\\" -X POST -d \\\"{\\\\\\\"username\\\\\\\": \\\\\\\"Ediass Notification\\\\\\\", \\\\\\\"content\\\\\\\": \\\\\\\"" + body + "\\\\\\\"}\\\" " + webhookURL)
    //     php.phpcode <| fun () -> codewrite("exec("+cmd.code+");")
    /// 文字列置換
    static member str_replace(strfrom:string,strto:string,str:PHPdata) = PHPdata.f("str_replace("+"\""+strfrom+"\""+","+"\""+strto+"\""+","+str.code+")")
    /// 指定した文字数になるまで文字を埋める
    static member str_pad(num:PHPdata,ndigit:int,paddingnum:int) = PHPdata.f("str_pad("+num.code+","+ndigit.ToString()+","+paddingnum.ToString()+", STR_PAD_LEFT)")
    /// ファイルダウンロード
    static member file_download(file:PHPdata) =
        php.phpcode <| fun () ->
            codewrite "header('Content-Type: application/octet-stream');"
        php.phpcode <| fun () ->
            codewrite "header('Content-Transfer-Encoding: Binary');"
        php.phpcode <| fun () ->
            codewrite("header('Content-disposition: attachment; filename='.basename("+file.code+"));")
        php.phpcode <| fun () ->
            codewrite("header('Content-Length: '.filesize("+file.code+"));")
        php.phpcode <| fun () ->
            codewrite "while (ob_get_level()) { ob_end_clean(); }"
        php.phpcode <| fun () ->
            codewrite("readfile("+file.code+");")
        php.phpcode <| fun () ->
            codewrite "exit;"
    /// ファイルパスからファイル名取得
    static member basename(file:PHPdata) = PHPdata.f("basename("+file.code+")")
    /// 改行文字
    static member br = "\\n"
    /// タブ文字
    static member tb = "\\t"

[<AutoOpen>]
module num0ForPHP =
        
    type num0 with
        member this.phpdata with get() = PHPdata [RNvr this.Expr]
        
    // type num1 with
    //     member this.phpdata with get() = PHPdata [RNvr this.Expr]
        // // member this.phpcode(pr:program) = "<?php echo " + this.code + "; ?>"
        // static member var x = num1(Nt,Var1(A1 0,"$"+x))
        
        // static member array(arrayname:string) = 
        //     let c = num1.var arrayname
        //     codewritein ("<?php "+arrayname+" = array(); ?>")
        //     c
            
        // static member array(arrayname:string,data:list<string*string>) = 
        //     let c = num1.var arrayname
        //     codewritein ("<?php "+arrayname+" = array(); ?>")
        //     codewritein ("<?php "+arrayname+"[] = array("+String.Join(",",data |> List.map (fun (a,b) -> "'"+a+"'=>'"+b+"'"))+"); ?>")
        //     c

        // static member array(arrayname:string,data:list<string*PHPdata>) = 
        //     let c = PHPdata arrayname
        //     codewritein ("<?php "+arrayname+" = array(); ?>")
        //     codewritein ("<?php "+arrayname+"[] = array("+String.Join(",",data |> List.map (fun (a,b) -> "'"+a+"'=>"+b.code))+"); ?>")
        //     c
        // // static member array(arrayname:string,data:list<string*PHPdata>) = 
        // //     let c = PHPdata.var arrayname
        // //     codewritein ("<?php "+arrayname+" = array(); ?>")
        // //     codewritein ("<?php "+arrayname+"[] = array("+String.Join(",",data |> List.map (fun (a,b) -> "'"+a+"'=>"+b.code))+"); ?>")
        // //     c
        // member this.push (x:list<PHPdata>) = codewritein ("<?php array_push(" + this.code + ", " + String.Join(",",List.map(fun (q:PHPdata) -> q.code) x) + "); ?>")
        // member this.push (x:PHPdata) = this.push [x]
        // member this.push (x:num0) = this.push [PHPdata x]
        
    type html with
        static member h1 (t:PHPdata) = html.h1 t.phpcode
        static member h2 (t:PHPdata) = html.h2 t.phpcode
        static member h3 (t:PHPdata) = html.h3 t.phpcode
        static member h4 (t:PHPdata) = html.h4 t.phpcode
