//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

open System
open System.IO

type PHPbool(x:string, ?context:GenerationContext) =

    member this.name with get() = x
    member _.Context = context
    static member var(context:GenerationContext,x) = PHPbool("$"+x, context)
    static member (<==) (a:PHPbool,b:PHPbool) =
        let target = GenerationContextMerge.requireTarget a.Context
        GenerationContextMerge.merge a.Context b.Context |> ignore
        hwritein target ("<?php ", a.name + " = " + b.name + " ?>")

type PHPdata(x:list<reduceExprString>, ?context:GenerationContext) =
    new(x:string) = PHPdata [RStr x]
    new(x:int0) = PHPdata([RNvr x.Expr], ?context=x.Context)
    new(x:double0) = PHPdata([RNvr x.Expr], ?context=x.Context)
    new(x:complex0) = PHPdata([RNvr x.Expr], ?context=x.Context)
    member _.data with get() = x
    member _.Context = context
    member this.extcode(pr:program) = "<?php echo " + this.code + "; ?>"
    static member var (context:GenerationContext,x) = PHPdata([RNvr(Var(Nt,"$"+x,NaN))], context)
    static member var (context:GenerationContext,x,init:PHPdata) =
        let v = PHPdata.var(context,x)
        v <== init
        v
    static member var (context:GenerationContext,x,init:int0) =
        let v = PHPdata.var(context,x)
        v <== init
        v
    static member var (context:GenerationContext,x,init:double0) =
        let v = PHPdata.var(context,x)
        v <== init
        v
    static member var (context:GenerationContext,x,init:complex0) =
        let v = PHPdata.var(context,x)
        v <== init
        v
    static member var (context:GenerationContext,x,init:int) =
        let v = PHPdata.var(context,x)
        v <== I init
        v
    static member var (context:GenerationContext,x,init:double) =
        let v = PHPdata.var(context,x)
        v <== D init
        v
    static member f(s:string, ?context:GenerationContext) = PHPdata([RNvr(Var(Nt,s,NaN))], ?context=context)
    static member f(context:GenerationContext,s:string) = PHPdata([RNvr(Var(Nt,s,NaN))], context)
    member this.int0 with get() =
        match x with
        |[RNvr c] -> int0(c, ?context=context)
        |_ ->
            printfn "%s" (this.toString(".",StrQuotation))
            int0 NaN
    member this.double0 with get() =
        match x with
        |[RNvr c] -> double0(c, ?context=context)
        |_ ->
            printfn "%s" (this.toString(".",StrQuotation))
            double0 NaN
    member this.complex0 with get() =
        match x with
        |[RNvr c] -> complex0(c, ?context=context)
        |_ ->
            printfn "%s" (this.toString(".",StrQuotation))
            complex0 NaN

    /// 配�Eを生戁E
    static member array() = PHPdata.f "array()"

    static member array(context:GenerationContext,arrayname:string) =
        let c = PHPdata.var(context,arrayname)
        hwritein context ("<?php ", "$"+arrayname+" = array(); ?>")
        c

    static member array(context:GenerationContext,arrayname:string,data:list<string*string>) =
        let c = PHPdata.var(context,arrayname)
        hwritein context ("<?php ", "$"+arrayname+" = array(); ?>")
        hwritein context ("<?php ", "$"+arrayname+"[] = array("+String.Join(",",data |> List.map (fun (a,b) -> "'"+a+"'=>'"+b+"'"))+"); ?>")
        c

    static member array(context:GenerationContext,arrayname:string,data:list<string*PHPdata>) =
        let c = PHPdata.var(context,arrayname)
        data |> Seq.map (fun (_, value) -> value.Context) |> GenerationContextMerge.mergeMany |> GenerationContextMerge.merge (Some context) |> ignore
        hwritein context ("<?php ", "$"+arrayname+" = array(); ?>")
        hwritein context ("<?php ", "$"+arrayname+"[] = array("+String.Join(",",data |> List.map (fun (a,b) -> "'"+a+"'=>"+b.code))+"); ?>")
        c
    // static member array(arrayname:string,data:list<string*PHPdata>) =
    //     let c = PHPdata.var arrayname
    //     writein ("<?php "+arrayname+" = array(); ?>")
    //     writein ("<?php "+arrayname+"[] = array("+String.Join(",",data |> List.map (fun (a,b) -> "'"+a+"'=>"+b.code))+"); ?>")
    //     c

    // /// 配�Eに要素を追加
    // static member array_push(a:PHPdata,el:PHPdata) = php.phpcode <| fun () -> write("array_push("+a.code+","+el.code+")")
    // static member array_push(a:PHPdata,el:num0) = php.array_push(a,PHPdata el)

    member this.push (x:list<PHPdata>) =
        let target = GenerationContextMerge.requireTarget context
        x |> Seq.map _.Context |> GenerationContextMerge.mergeMany |> GenerationContextMerge.merge context |> ignore
        hwritein target ("<?php ", "array_push(" + this.code + ", " + String.Join(",",List.map(fun (q:PHPdata) -> q.code) x) + "); ?>")
    member this.push (x:PHPdata) = this.push [x]
    // member this.push (x:num0) = this.push [PHPdata x]
    // member this.push (x:list<exprString>) = writein ("<?php array_push(" + this.code + ", " + String.Join(",",x |> List.map(fun q -> q.toString("",Direct))) + "); ?>")
    ///配�Eに要素を褁E��追加
    member this.push (x:list<int0>) = this.push (List.map (fun (value:int0) -> PHPdata value) x : PHPdata list)
    member this.push (x:list<double0>) = this.push (List.map (fun (value:double0) -> PHPdata value) x : PHPdata list)
    member this.push (x:list<complex0>) = this.push (List.map (fun (value:complex0) -> PHPdata value) x : PHPdata list)
    ///配�Eに斁E���E要素を褁E��追加
    member this.push (x:list<string>) = this.push (List.map(fun (q:string) -> PHPdata q) x)
    ///配�Eに要素を追加
    member this.push (x:int0) = this.push [x]
    member this.push (x:double0) = this.push [x]
    member this.push (x:complex0) = this.push [x]
    ///配�Eに斁E���E要素を追加
    member this.push (x:string) = this.push [x]
    member this.toString(c:string,op:ExprConcatOption) =
        x
        |> List.map (function
            |RStr x ->
                match op with
                |Direct -> x
                |StrQuotation -> "\""+x+"\""
                |CodeStrQuotation -> "\\\""+x+"\\\""
            |RNvr value ->
                match context, value with
                |Some ctx, _ -> value.eval ctx.CurrentProgram
                |None, Int value -> string value
                |None, Dbl value -> string value
                |None, Cpx(real, imaginary) -> sprintf "(%g+%g*I)" real imaginary
                |None, _ -> invalidOp "A symbolic PHP value without a GenerationContext cannot be rendered as code.")
        |> fun s -> String.Join(c,s)
    member this.Item(i:PHPdata) =
        let resultContext = GenerationContextMerge.merge context i.Context
        PHPdata([RNvr(Var(Nt,this.toString(".",StrQuotation) + "[" + i.toString(".",StrQuotation) + "]",NaN))], ?context=resultContext)
    member this.Item(i:int) = this[PHPdata [RNvr(Int i)]]
    member this.Item(i:string) = this[PHPdata [RStr i]]
    member this.Item(i:int0) = this[PHPdata i]
    member this.Item(i:double0) = this[PHPdata i]
    member this.Item(i:complex0) = this[PHPdata i]
    member this.code with get() = this.toString(".",StrQuotation)
    member this.phpcode with get() = "<?php echo " + this.code + " ?>"
    static member (++) (a:PHPdata,b:PHPdata) = PHPdata(a.data@b.data, ?context=GenerationContextMerge.merge a.Context b.Context)
    static member (++) (a:string,b:PHPdata) = PHPdata a ++ b
    static member (++) (a:PHPdata,b:string) = a ++ PHPdata b
    static member (++) (a:PHPdata,b:int0) = a ++ PHPdata b
    static member (++) (a:PHPdata,b:double0) = a ++ PHPdata b
    static member (++) (a:PHPdata,b:complex0) = a ++ PHPdata b

    member this.foreach code =
        let environment = CompilationEnvironment(Some (GenerationContextMerge.requireTarget context))
        environment.ch.i <| fun i ->
            ContextPhp(environment).phpcode <| fun () -> writei (environment.RequireGenerationContext()) ("for("+i.code+"=0; "+i.code+"<count("+this.code+"); "+i.code+"++):")
            environment.RequireGenerationContext().CurrentProgram.indentInc()
            code i
            environment.RequireGenerationContext().CurrentProgram.indentDec()
            ContextPhp(environment).phpcode <| fun () -> writei (environment.RequireGenerationContext()) "endfor;"
    member this.foreach (key:PHPdata,value:PHPdata) = fun code ->
        let environment = CompilationEnvironment(Some (GenerationContextMerge.mergeMany [context; key.Context; value.Context] |> GenerationContextMerge.requireTarget))
        environment.ch.i <| fun _ ->
            ContextPhp(environment).phpcode <| fun () -> writei (environment.RequireGenerationContext()) ("foreach("+this.code+" as "+key.code+" => "+value.code+"):")
            code()
            ContextPhp(environment).phpcode <| fun () -> writei (environment.RequireGenerationContext()) "endforeach;"
    static member (<==) (a:PHPdata,b:PHPdata) =
        let target = GenerationContextMerge.requireTarget a.Context
        GenerationContextMerge.merge a.Context b.Context |> ignore
        hwritein target ("<?php ", a.code + " = " + b.code + "; ?>")
    static member (<==) (a:PHPdata,b:string) = a <== PHPdata b
    static member (<==) (a:PHPdata,b:int0) = a <== PHPdata b
    static member (<==) (a:PHPdata,b:double0) = a <== PHPdata b
    static member (<==) (a:PHPdata,b:complex0) = a <== PHPdata b
    static member (<==) (a:PHPdata,b:int) = a <== PHPdata (I b)
    static member (<==) (a:int0,b:PHPdata) = PHPdata a <== b
    static member (<==) (a:double0,b:PHPdata) = PHPdata a <== b
    static member (<==) (a:complex0,b:PHPdata) = PHPdata a <== b
    static member private Compare(a:PHPdata,b:GenerationContext option, expression) =
        bool0(expression, ?context=GenerationContextMerge.merge a.Context b)
    static member (.=) (a:PHPdata,b:PHPdata) = PHPdata.Compare(a,b.Context,Eq(Var(Nt,a.code,NaN),Var(Nt,b.code,NaN)))
    static member (.=) (a:PHPdata,b:int0) = PHPdata.Compare(a,b.Context,Eq(Var(Nt,a.code,NaN),b.Expr))
    static member (.=) (a:PHPdata,b:double0) = PHPdata.Compare(a,b.Context,Eq(Var(Nt,a.code,NaN),b.Expr))
    static member (.=) (a:PHPdata,b:int) = PHPdata.Compare(a,None,Eq(Var(Nt,a.code,NaN),Int b))
    static member (.=) (a:PHPdata,b:string) = a .= PHPdata b
    static member (.=/) (a:PHPdata,b:PHPdata) = PHPdata.Compare(a,b.Context,NEq(Var(Nt,a.code,NaN),Var(Nt,b.code,NaN)))
    static member (.=/) (a:PHPdata,b:int0) = PHPdata.Compare(a,b.Context,NEq(Var(Nt,a.code,NaN),b.Expr))
    static member (.=/) (a:PHPdata,b:double0) = PHPdata.Compare(a,b.Context,NEq(Var(Nt,a.code,NaN),b.Expr))
    static member (.=/) (a:PHPdata,b:int) = PHPdata.Compare(a,None,NEq(Var(Nt,a.code,NaN),Int b))
    static member (.=/) (a:PHPdata,b:string) = a .=/ PHPdata b
    static member (.<) (a:PHPdata,b:PHPdata) = PHPdata.Compare(a,b.Context,Less(Var(Nt,a.code,NaN),Var(Nt,b.code,NaN)))
    static member (.<) (a:PHPdata,b:int0) = PHPdata.Compare(a,b.Context,Less(Var(Nt,a.code,NaN),b.Expr))
    static member (.<) (a:PHPdata,b:double0) = PHPdata.Compare(a,b.Context,Less(Var(Nt,a.code,NaN),b.Expr))
    static member (.<) (a:PHPdata,b:int) = PHPdata.Compare(a,None,Less(Var(Nt,a.code,NaN),Int b))
    static member (.<=) (a:PHPdata,b:PHPdata) = PHPdata.Compare(a,b.Context,LessEq(Var(Nt,a.code,NaN),Var(Nt,b.code,NaN)))
    static member (.<=) (a:PHPdata,b:int0) = PHPdata.Compare(a,b.Context,LessEq(Var(Nt,a.code,NaN),b.Expr))
    static member (.<=) (a:PHPdata,b:double0) = PHPdata.Compare(a,b.Context,LessEq(Var(Nt,a.code,NaN),b.Expr))
    static member (.<=) (a:PHPdata,b:int) = PHPdata.Compare(a,None,LessEq(Var(Nt,a.code,NaN),Int b))
    static member (.>) (a:PHPdata,b:PHPdata) = PHPdata.Compare(a,b.Context,Greater(Var(Nt,a.code,NaN),Var(Nt,b.code,NaN)))
    static member (.>) (a:PHPdata,b:int0) = PHPdata.Compare(a,b.Context,Greater(Var(Nt,a.code,NaN),b.Expr))
    static member (.>) (a:PHPdata,b:double0) = PHPdata.Compare(a,b.Context,Greater(Var(Nt,a.code,NaN),b.Expr))
    static member (.>) (a:PHPdata,b:int) = PHPdata.Compare(a,None,Greater(Var(Nt,a.code,NaN),Int b))
    static member (.>=) (a:PHPdata,b:PHPdata) = PHPdata.Compare(a,b.Context,GreaterEq(Var(Nt,a.code,NaN),Var(Nt,b.code,NaN)))
    static member (.>=) (a:PHPdata,b:int0) = PHPdata.Compare(a,b.Context,GreaterEq(Var(Nt,a.code,NaN),b.Expr))
    static member (.>=) (a:PHPdata,b:double0) = PHPdata.Compare(a,b.Context,GreaterEq(Var(Nt,a.code,NaN),b.Expr))
    static member (.>=) (a:PHPdata,b:int) = PHPdata.Compare(a,None,GreaterEq(Var(Nt,a.code,NaN),Int b))

and ContextPhp internal (environment:CompilationEnvironment) =
    let context = environment.RequireGenerationContext()
    let merge contexts = GenerationContextMerge.mergeMany (Some context :: contexts)
    let data code contexts = PHPdata.f(code, ?context=merge contexts)
    let boolean code contexts = bool0(Var(Nt,code,NaN), ?context=merge contexts)
    /// Creates a PHP variable associated with this generation context.
    member _.var(name:string) = PHPdata.var(context,name)
    /// htmlコード�Eにphpコードを埋め込み
    member this.phpcode (code:unit->unit) =
        write context "<?php "
        code()
        writen context " ?>"
    /// POST送信されたデータを表示
    member this.postCheck() = hwritein context ("<?php ", "print_r($_POST); ?>")
    /// POST送信されたファイルを表示
    member this.postFileCheck() = hwritein context ("<?php ", "print_r($_FILES); ?>")
    /// 論理穁E
    member this.And (x:list<bool0>) = boolean ("(" + String.Join(" && ", x |> List.map (fun s -> s.code)) + ")") (x |> List.map _.Context)
    /// 論理咁E
    member this.Or (x:list<bool0>) = boolean ("(" + String.Join(" || ", x |> List.map (fun s -> s.code)) + ")") (x |> List.map _.Context)
    /// 持E��された変数がPOST送信されたか判宁E
    member this.isset (x:PHPdata) = boolean ("isset(" + x.code + ")") [x.Context]
    member this.isNotset (x:PHPdata) = boolean ("!isset(" + x.code + ")") [x.Context]
    member this.echo (x:PHPdata) = this.phpcode <| fun () -> writei context ("echo " + x.code + ";")
    // member this echo (x:exprString) = this.phpcode <| fun () -> write("echo " + x.toString(".",StrQuotation) + ";")
    /// 斁E���Eを表示
    member this.echo (x:string) = this.echo (PHPdata x)
    /// 変数を表示
    member this.echo (x:int0) = this.echo (PHPdata x)
    member this.echo (x:double0) = this.echo (PHPdata x)
    member this.echo (x:complex0) = this.echo (PHPdata x)
    /// ファイル冁E�EチE��ストを取征E
    member this.file_get_contents (filename:PHPdata) = data ("file_get_contents(" + filename.code + ")") [filename.Context]
    member this.file_get_contents (filename:string) = this.file_get_contents (PHPdata filename)
    /// ファイルにチE��ストを書き込み
    member this.file_put_contents (filename:PHPdata,x:PHPdata) = this.phpcode <| fun () -> writei context ("file_put_contents("+filename.code+","+x.code+");")
    /// ファイルにチE��ストを書き込み
    member this.file_put_contents (filename:string,x:PHPdata) = this.file_put_contents(PHPdata filename, x)
    /// JSONファイルをデコーチE
    member this.json_decode (x:PHPdata,p:bool) = data ("json_decode("+x.code+","+p.ToString()+")") [x.Context]
    /// JSONファイルをエンコーチE
    member this.json_encode (x:PHPdata) = data ("json_encode("+x.code+", JSON_PRETTY_PRINT|JSON_UNESCAPED_UNICODE|JSON_UNESCAPED_SLASHES )") [x.Context]
    /// 持E��したキーの値の配�Eを生戁E
    member this.array_column(value:PHPdata,id:PHPdata) = data ("array_column("+value.code+","+id.code+")") [value.Context;id.Context]
    /// 持E��した要素が含まれてぁE��か判宁E
    member this.in_array_strict(s:PHPdata, idArray:PHPdata) = boolean ("in_array("+s.code+", "+idArray.code+", true)") [s.Context;idArray.Context]
    member this.in_array_strict(s:int0, idArray:PHPdata) = this.in_array_strict(PHPdata s, idArray)
    member this.in_array_strict(s:double0, idArray:PHPdata) = this.in_array_strict(PHPdata s, idArray)
    member this.in_array_strict(s:complex0, idArray:PHPdata) = this.in_array_strict(PHPdata s, idArray)
    /// 持E��した要素の配�E冁E��のインチE��クス�E�キー�E�を検索
    member this.array_search(s:PHPdata, idArray:PHPdata) = data ("array_search("+s.code+", "+idArray.code+")") [s.Context;idArray.Context]
    /// ファイル冁E�EチE��ストを配�Eに格紁E
    member this.file(filename:PHPdata, flag:list<FileFlag>) =
        data ("file("+filename.code+", "+(flag |> List.map (fun s -> s.str) |> (fun p -> String.Join(" | ",p)) )+")") [filename.Context]
    /// ファイルを開ぁE
    member this.fopen(filename:PHPdata,rw:FileOpenMode) = data ("fopen("+filename.code+", "+rw.str+")") [filename.Context]
    /// ファイルを開ぁE
    member this.fopen(filename:string,rw:FileOpenMode) = this.fopen(PHPdata filename, rw)
    /// ファイルに書き込み
    member this.fwrite(fp:PHPdata,t:PHPdata) = this.phpcode <| fun () -> writei context ("fwrite("+fp.code+", "+t.code+");")
    /// ファイルに書き込み
    member this.fwrite(fp:PHPdata,t:int0) = this.fwrite(fp, PHPdata t)
    member this.fwrite(fp:PHPdata,t:double0) = this.fwrite(fp, PHPdata t)
    member this.fwrite(fp:PHPdata,t:complex0) = this.fwrite(fp, PHPdata t)
    /// ファイルに書き込み
    member this.fwrite(fp:PHPdata,t:string) = this.fwrite(fp, PHPdata t)
    /// ファイルに書き込み
    member this.fwrite(fp:PHPdata,t:int) = this.phpcode <| fun () -> writei context ("fwrite("+fp.code+", "+t.ToString()+");")
    /// ファイルにShift-JISで書き込み
    member this.fwrite_SJIS(fp:PHPdata,t:PHPdata) = this.phpcode <| fun () -> writei context ("fwrite("+fp.code+", mb_convert_encoding("+t.code+", 'SJIS-win', 'UTF-8'));")
    /// ファイルにShift-JISで書き込み
    member this.fwrite_SJIS(fp:PHPdata,t:int0) = this.phpcode <| fun () -> writei context ("fwrite("+fp.code+", mb_convert_encoding("+t.code+", 'SJIS-win', 'UTF-8'));")
    member this.fwrite_SJIS(fp:PHPdata,t:double0) = this.phpcode <| fun () -> writei context ("fwrite("+fp.code+", mb_convert_encoding("+t.code+", 'SJIS-win', 'UTF-8'));")
    member this.fwrite_SJIS(fp:PHPdata,t:complex0) = this.phpcode <| fun () -> writei context ("fwrite("+fp.code+", mb_convert_encoding("+t.code+", 'SJIS-win', 'UTF-8'));")
    /// ファイルにShift-JISで書き込み
    member this.fwrite_SJIS(fp:PHPdata,t:string) = this.phpcode <| fun () -> writei context ("fwrite("+fp.code+", mb_convert_encoding(\""+t+"\", 'SJIS-win', 'UTF-8'));")
    /// ファイルを閉じる
    member this.fclose(filename:PHPdata) = this.phpcode <| fun () -> writei context ("fclose("+filename.code+");")
    /// 正規表現
    member this.preg_match(p:PHPdata,text:PHPdata,mat:PHPdata) = this.phpcode <| fun () -> writei context ("preg_match("+p.code+","+text.code+","+mat.code+");")
    /// ファイルのダウンローチE
    member this.download(filename:string) =
        this.phpcode <| fun () -> writei context "header('Content-Type: application/octet-stream');"
        this.phpcode <| fun () -> writei context ("header('Content-Length: '.filesize(\""+filename+"\"));")
        this.phpcode <| fun () -> writei context ("header('Content-Disposition: attachment; filename=\""+filename+"\"');")
        this.phpcode <| fun () -> writei context ("readfile(\""+filename+"\");")
        this.phpcode <| fun () -> writei context "exit;"
    /// 整数に変換
    member this.intval(s:PHPdata) = data ("intval("+s.code+")") [s.Context]
    /// 配�E要素の咁E
    member this.array_sum(value:PHPdata) = data ("array_sum("+value.code+")") [value.Context]
    /// 斁E��数
    member this.strlen(value:PHPdata) = data ("strlen("+value.code+")") [value.Context]
    /// 数値かどぁE��判宁E
    member this.is_numeric(value:PHPdata) = boolean ("is_numeric("+value.code+")") [value.Context]
    /// 否定演箁E
    member this.nt (value:bool0) = boolean ("!"+value.code) [value.Context]
    ///<summary>送信チE�EタをキャチE��ュしなぁE��Eirefoxでフォームの選択肢がリロード前から保持される現象を回避�E�E/summary>
    member this.set_nocache() = this.phpcode <| fun () -> writei context "header( 'Cache-Control: no-store, no-cache, must-revalidate' );"
    /// HTTPヘッダを取征E
    member this.header(data:PHPdata) = this.phpcode <| fun () -> writei context ("header("+data.code+");")
    /// HTTPヘッダを取征E
    member this.header(data:string) = this.header(PHPdata data)
    // 小数に変換
    // member this float(data:num0) = Var("(float)"+data.code)
    // 絶対値
    // member this abs(data:num0) = Var("abs("+data.code+")")
    /// 日付を取征E
    member this.date(fmt:string) = data ("date(\""+fmt+"\")") []
    /// 整数に丸めE
    member this.round(x:PHPdata) = data ("round("+x.code+")") [x.Context]
    /// 整数に丸めE
    member this.round(x:double0) = this.round(PHPdata x)
    /// 斁E���E刁E��出ぁE
    member this.substr(x:PHPdata,n:PHPdata) = data ("substr("+x.code+","+n.code+")") [x.Context;n.Context]
    /// 斁E���E刁E��出ぁE
    member this.substr(x:PHPdata,n:int) = data ("substr("+x.code+","+n.ToString()+")") [x.Context]
    /// ファイルが存在するか確誁E
    member this.file_exists(x:PHPdata) = boolean ("file_exists("+x.code+")") [x.Context]
    /// ファイルが存在するか確誁E
    member this.file_exists(x:string) = boolean ("file_exists(\""+x+"\")") []
    /// 斁E��数�E��E角も1字扱ぁE��E
    member this.mb_strlen(x:PHPdata) = data ("mb_strlen("+x.code+")") [x.Context]
    /// 斁E��数�E��E角も1字扱ぁE��E
    member this.mb_strwidth(x:PHPdata) = data ("mb_strwidth("+x.code+")") [x.Context]
    /// 斁E���E比輁E
    member this.strncmp(x:PHPdata,y:string,n:int) = data ("strncmp("+x.code+",\""+y+"\","+n.ToString()+")") [x.Context]
    /// 持E��したパターンにマッチするファイルパス取征E
    member this.glob(x:PHPdata) = data ("glob("+x.code+")") [x.Context]
    /// 持E��したパターンにマッチするファイルパス取征E
    member this.glob(x:string) = data ("glob(\""+x+"\")") []
    /// 斁E���E刁E��
    member this.explode(x:PHPdata,y:PHPdata) = data ("explode("+x.code+","+y.code+")") [x.Context;y.Context]
    /// 斁E���E刁E��
    member this.explode(x:string,y:PHPdata) = data ("explode('"+x+"',"+y.code+")") [y.Context]
    /// 配�EのソーチE
    member this.sort(data:PHPdata) = this.phpcode <| fun () -> writei context ("sort("+data.code+");")
    /// 整数に変換
    member this.toint(x:PHPdata) = int0(Var(It 4, "(int)"+x.code, NaN), ?context=merge [x.Context])
    /// 配�E要素数
    member this.count(x:PHPdata) = int0(Var(It 4, "count("+x.code+")", NaN), ?context=merge [x.Context])
    /// 拡張子を除ぁE��ファイル吁E
    member this.filename_withoutExtension(x:PHPdata) = data ("pathinfo("+x.code+", PATHINFO_FILENAME)") [x.Context]
    /// ファイル削除
    member this.unlink(data:PHPdata) = this.phpcode <| fun () -> writei context ("unlink("+data.code+");")
    /// 配�E要素をランダムに入れ替ぁE
    member this.shuffle(data:PHPdata) = this.phpcode <| fun () -> writei context ("shuffle("+data.code+");")
    /// タイムゾーン設宁E
    member this.setTimeZone(location:string) = this.phpcode <| fun () -> writei context ("date_default_timezone_set('"+location+"');")
    /// メール送信
    member this.sendMail(body:PHPdata,subject:PHPdata,fromAddress:PHPdata,toAddress:PHPdata) =
        this.phpcode <| fun () -> writei context "mb_language(\"ja\");"
        this.phpcode <| fun () -> writei context "mb_internal_encoding(\"UTF-8\");"
        this.phpcode <| fun () -> writei context ("mb_send_mail("+toAddress.code+","+subject.code+","+body.code+","+("From: "++fromAddress).code+");")
    // /// メール送信
    // member this sendMail(body:exprString,subject:exprString,fromAddress:string,toAddress:PHPdata) =
    //     this.phpcode <| fun () -> write("mb_language(\"ja\");")
    //     this.phpcode <| fun () -> write("mb_internal_encoding(\"UTF-8\");")
    //     this.phpcode <| fun () -> write("mb_send_mail("+toAddress.code+","+subject.toString(".",StrQuotation)+","+body.toString(".",StrQuotation)+","+"\"From: "+fromAddress+"\");")
    // /// メール送信
    // member this sendMail(body:PHPdata,subject:PHPdata,fromAddress:string,toAddress:PHPdata) =
    //     this.phpcode <| fun () -> write("mb_language(\"ja\");")
    //     this.phpcode <| fun () -> write("mb_internal_encoding(\"UTF-8\");")
    //     this.phpcode <| fun () -> write("mb_send_mail("+toAddress.code+","+subject.code+","+body.code+","+"\"From: "+fromAddress+"\");")
    /// メール送信
    member this.sendMail(body:PHPdata,subject:PHPdata,smtp:PHPdata,fromAddress:PHPdata,toAddress:PHPdata) =
        let cmd = PHPdata.var(context,"cmd")
        cmd <== "echo \\\"" ++ body ++ "\\\" | mail -s \\\"" ++ subject ++ "\\\" -S smtp=smtp://" ++ smtp ++ ":25 -r " ++ fromAddress ++ " " ++ toAddress
        this.phpcode <| fun () -> writei context ("exec("+cmd.code+");")
    // /// メール送信
    // member this sendMail(body:PHPdata,subject:PHPdata,fromAddress:string,toAddress:PHPdata) =
    //     this.phpcode <| fun () -> write("mb_language(\"ja\");")
    //     this.phpcode <| fun () -> write("mb_internal_encoding(\"UTF-8\");")
    //     this.phpcode <| fun () -> write("mb_send_mail("+toAddress.code+","+subject.code+","+body.code+","+"\"From: "+fromAddress+"\");")
    // /// メール送信
    // member this sendMail(body:string,subject:string,fromAddress:string,toAddress:string) =
    //     this.phpcode <| fun () -> write("mb_language(\"ja\");")
    //     this.phpcode <| fun () -> write("mb_internal_encoding(\"UTF-8\");")
    //     this.phpcode <| fun () -> write("mb_send_mail(\""+toAddress+"\",\""+subject+"\",\""+body+"\","+"\"From: "+fromAddress+"\");")
    /// DiscordへメチE��ージ送信
    member this.sendDiscord(body:PHPdata,webhookURL:PHPdata) =
        let cmd = PHPdata.var(context,"cmd")
        cmd <== "curl -H \\\"Content-Type: application/json\\\" -X POST -d \\\"{\\\\\\\"username\\\\\\\": \\\\\\\"Ediass Notification\\\\\\\", \\\\\\\"content\\\\\\\": \\\\\\\""++body++"\\\\\\\"}\\\" " ++ webhookURL
        this.phpcode <| fun () -> writei context ("exec("+cmd.code+");")
    // /// DiscordへメチE��ージ送信
    // member this sendDiscord(body:string,webhookURL:string) =
    //     let cmd = PHPdata.v "cmd"
    //     cmd <== this.fnvar("curl -H \\\"Content-Type: application/json\\\" -X POST -d \\\"{\\\\\\\"username\\\\\\\": \\\\\\\"Ediass Notification\\\\\\\", \\\\\\\"content\\\\\\\": \\\\\\\"" + body + "\\\\\\\"}\\\" " + webhookURL)
    //     this.phpcode <| fun () -> write("exec("+cmd.code+");")
    /// 斁E���E置揁E
    member this.str_replace(strfrom:string,strto:string,str:PHPdata) = data ("str_replace("+"\""+strfrom+"\""+","+"\""+strto+"\""+","+str.code+")") [str.Context]
    /// 持E��した文字数になるまで斁E��を埋めめE
    member this.str_pad(num:PHPdata,ndigit:int,paddingnum:int) = data ("str_pad("+num.code+","+ndigit.ToString()+","+paddingnum.ToString()+", STR_PAD_LEFT)") [num.Context]
    /// ファイルダウンローチE
    member this.file_download(file:PHPdata) =
        this.phpcode <| fun () ->
            writei context "header('Content-Type: application/octet-stream');"
        this.phpcode <| fun () ->
            writei context "header('Content-Transfer-Encoding: Binary');"
        this.phpcode <| fun () ->
            writei context ("header('Content-disposition: attachment; filename='.basename("+file.code+"));")
        this.phpcode <| fun () ->
            writei context ("header('Content-Length: '.filesize("+file.code+"));")
        this.phpcode <| fun () ->
            writei context "while (ob_get_level()) { ob_end_clean(); }"
        this.phpcode <| fun () ->
            writei context ("readfile("+file.code+");")
        this.phpcode <| fun () ->
            writei context "exit;"
    /// ファイルパスからファイル名取征E
    member this.basename(file:PHPdata) = data ("basename("+file.code+")") [file.Context]
    /// 改行文孁E
    member this.br = "\\n"
    /// タブ文孁E
    member this.tb = "\\t"

[<AutoOpen>]
module num0ForPHP =

    type int0 with
        member this.phpdata with get() = PHPdata([RNvr this.Expr], ?context=this.Context)

    type double0 with
        member this.phpdata with get() = PHPdata([RNvr this.Expr], ?context=this.Context)

    type complex0 with
        member this.phpdata with get() = PHPdata([RNvr this.Expr], ?context=this.Context)

    // type num1 with
    //     member this.phpdata with get() = PHPdata [RNvr this.Expr]
        // // member this.phpcode(pr:program) = "<?php echo " + this.code + "; ?>"
        // member this var x = num1(Nt,Var1(A1 0,"$"+x))

        // member this array(arrayname:string) =
        //     let c = num1.var arrayname
        //     writein ("<?php "+arrayname+" = array(); ?>")
        //     c

        // member this array(arrayname:string,data:list<string*string>) =
        //     let c = num1.var arrayname
        //     writein ("<?php "+arrayname+" = array(); ?>")
        //     writein ("<?php "+arrayname+"[] = array("+String.Join(",",data |> List.map (fun (a,b) -> "'"+a+"'=>'"+b+"'"))+"); ?>")
        //     c

        // member this array(arrayname:string,data:list<string*PHPdata>) =
        //     let c = PHPdata arrayname
        //     writein ("<?php "+arrayname+" = array(); ?>")
        //     writein ("<?php "+arrayname+"[] = array("+String.Join(",",data |> List.map (fun (a,b) -> "'"+a+"'=>"+b.code))+"); ?>")
        //     c
        // // member this array(arrayname:string,data:list<string*PHPdata>) =
        // //     let c = PHPdata.var arrayname
        // //     writein ("<?php "+arrayname+" = array(); ?>")
        // //     writein ("<?php "+arrayname+"[] = array("+String.Join(",",data |> List.map (fun (a,b) -> "'"+a+"'=>"+b.code))+"); ?>")
        // //     c
        // member this.push (x:list<PHPdata>) = writein ("<?php array_push(" + this.code + ", " + String.Join(",",List.map(fun (q:PHPdata) -> q.code) x) + "); ?>")
        // member this.push (x:PHPdata) = this.push [x]
        // member this.push (x:num0) = this.push [PHPdata x]

    type html with
        member this.h1 (t:PHPdata) = this.h1 t.phpcode
        member this.h2 (t:PHPdata) = this.h2 t.phpcode
        member this.h3 (t:PHPdata) = this.h3 t.phpcode
        member this.h4 (t:PHPdata) = this.h4 t.phpcode

    type CompilationEnvironment with
        member this.php = ContextPhp(this)
