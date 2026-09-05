//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

open System
open System.IO
open System.Text

module private PhpEncoding =
    /// Renders a .NET string as a PHP double-quoted string literal.
    let stringLiteral (value:string) =
        if isNull value then nullArg (nameof value)

        let result = StringBuilder(value.Length + 2)
        result.Append('"') |> ignore

        for character in value do
            match character with
            | '\\' -> result.Append("\\\\") |> ignore
            | '"' -> result.Append("\\\"") |> ignore
            | '$' -> result.Append("\\$") |> ignore
            | control when int control <= 0x1F || control = '\u007F' ->
                result.Append(sprintf "\\x%02X" (int control)) |> ignore
            | character -> result.Append(character) |> ignore

        result.Append('"').ToString()

    /// Renders a PHP string literal for inclusion in another quoted code string.
    let codeStringLiteral value =
        let literal = stringLiteral value
        literal.Replace("\\", "\\\\").Replace("\"", "\\\"")

type PHPbool(x:string, context:Aqualis) =

    member this.name with get() = x
    member _.Context = context
    static member var(context:Aqualis,x) = PHPbool("$"+x, context)
    static member (<==) (a:PHPbool,b:PHPbool) =
        Aqualis.merge a.Context b.Context |> ignore
        a.Context.codewritein ("<?php ", a.name + " = " + b.name + " ?>")

type PHPdata(x:list<reduceExprString>, context:Aqualis) =
    new(x:string) = PHPdata([RStr x],Aqualis.BlankWriter PHP)
    new(x:int0) = PHPdata([RNvr(x.Expr,x.Context)], x.Context)
    new(x:double0) = PHPdata([RNvr(x.Expr,x.Context)], x.Context)
    new(x:complex0) = PHPdata([RNvr(x.Expr,x.Context)], x.Context)
    member _.data with get() = x
    member _.Context = context
    member this.extcode(pr:Aqualis) = "<?php echo " + this.code + "; ?>"
    static member var (context:Aqualis,x) = PHPdata([RNvr(Var(Nt,"$"+x,NaN), context)], context)
    static member var (context:Aqualis,x,init:PHPdata) =
        let v = PHPdata.var(context,x)
        v <== init
        v
    static member var (context:Aqualis,x,init:int0) =
        let v = PHPdata.var(context,x)
        v <== init
        v
    static member var (context:Aqualis,x,init:double0) =
        let v = PHPdata.var(context,x)
        v <== init
        v
    static member var (context:Aqualis,x,init:complex0) =
        let v = PHPdata.var(context,x)
        v <== init
        v
    static member var (context:Aqualis,x,init:int) =
        let v = PHPdata.var(context,x)
        v <== I init
        v
    static member var (context:Aqualis,x,init:double) =
        let v = PHPdata.var(context,x)
        v <== D init
        v
    static member f(s:string, context:Aqualis) = PHPdata([RNvr(Var(Nt,s,NaN),context)], context)
    static member f(context:Aqualis,s:string) = PHPdata([RNvr(Var(Nt,s,NaN), context)], context)
    member this.int0 with get() =
        match x with
        |[RNvr (c,valueContext)] -> int0(c, valueContext)
        |_ ->
            printfn "%s" (this.toString(".",StrQuotation))
            int0 NaN
    member this.double0 with get() =
        match x with
        |[RNvr (c,valueContext)] -> double0(c, valueContext)
        |_ ->
            printfn "%s" (this.toString(".",StrQuotation))
            double0 NaN
    member this.complex0 with get() =
        match x with
        |[RNvr (c,valueContext)] -> complex0(c, valueContext)
        |_ ->
            printfn "%s" (this.toString(".",StrQuotation))
            complex0 NaN

    [<Obsolete("Use PHPdata.array(context) or context.php.array() so the empty array expression has a GenerationContext.")>]
    static member array() = PHPdata.f ("array()",Aqualis.BlankWriter PHP)

    /// Creates an empty PHP array expression associated with the generation context.
    static member array(context:Aqualis) = PHPdata.f(context,"array()")

    static member array(context:Aqualis,arrayname:string) =
        let c = PHPdata.var(context,arrayname)
        context.codewritein("<?php ", "$"+arrayname+" = array(); ?>")
        c

    static member array(context:Aqualis,arrayname:string,data:list<string*string>) =
        let c = PHPdata.var(context,arrayname)
        context.codewritein("<?php ", "$"+arrayname+" = array(); ?>")
        context.codewritein(
            "<?php ",
            "$" + arrayname + "[] = array(" +
            String.Join(",", data |> List.map (fun (key,value) ->
                PhpEncoding.stringLiteral key + "=>" + PhpEncoding.stringLiteral value)) +
            "); ?>")
        c

    static member array(context:Aqualis,arrayname:string,data:list<string*PHPdata>) =
        let c = PHPdata.var(context,arrayname)
        data |> Seq.map (fun (_, value) -> value.Context) |> Aqualis.mergeMany |> Aqualis.merge context |> ignore
        context.codewritein("<?php ", "$"+arrayname+" = array(); ?>")
        context.codewritein(
            "<?php ",
            "$" + arrayname + "[] = array(" +
            String.Join(",", data |> List.map (fun (key,value) ->
                PhpEncoding.stringLiteral key + "=>" + value.code)) +
            "); ?>")
        c

    member this.push (x:list<PHPdata>) =
        x |> Seq.map _.Context |> Aqualis.mergeMany |> Aqualis.merge context |> ignore
        context.codewritein("<?php ", "array_push(" + this.code + ", " + String.Join(",",List.map(fun (q:PHPdata) -> q.code) x) + "); ?>")
    member this.push (x:PHPdata) = this.push [x]
    member this.push (x:list<int0>) = this.push (List.map (fun (value:int0) -> PHPdata value) x : PHPdata list)
    member this.push (x:list<double0>) = this.push (List.map (fun (value:double0) -> PHPdata value) x : PHPdata list)
    member this.push (x:list<complex0>) = this.push (List.map (fun (value:complex0) -> PHPdata value) x : PHPdata list)
    member this.push (x:list<string>) = this.push (List.map(fun (q:string) -> PHPdata q) x)
    member this.push (x:int0) = this.push [x]
    member this.push (x:double0) = this.push [x]
    member this.push (x:complex0) = this.push [x]
    member this.push (x:string) = this.push [x]
    member this.toString(c:string,op:ExprConcatOption) =
        x
            |> List.map (function
                |RStr x ->
                    match op with
                    |Direct -> x
                    |StrQuotation -> PhpEncoding.stringLiteral x
                    |CodeStrQuotation -> PhpEncoding.codeStringLiteral x
                |RNvr (value,_) ->
                    match context.CodeFile, value with
                    |Some _, _ -> value.eval context
                    |None, Int value -> string value
                    |None, Dbl value -> string value
                    |None, Cpx(real, imaginary) -> sprintf "(%g+%g*I)" real imaginary
                    |None, _ -> invalidOp "A symbolic PHP value without a GenerationContext cannot be rendered as code.")
        |> fun s -> String.Join(c,s)
    member this.Item(i:PHPdata) =
        let resultContext = Aqualis.merge context i.Context
        PHPdata([RNvr(Var(Nt,this.toString(".",StrQuotation) + "[" + i.toString(".",StrQuotation) + "]",NaN),resultContext)], resultContext)
    member this.Item(i:int) = this[PHPdata ([RNvr(Int i,Aqualis.BlankWriter PHP)],Aqualis.BlankWriter PHP)]
    member this.Item(i:string) = this[PHPdata ([RStr i],Aqualis.BlankWriter PHP)]
    member this.Item(i:int0) = this[PHPdata i]
    member this.Item(i:double0) = this[PHPdata i]
    member this.Item(i:complex0) = this[PHPdata i]
    member this.code with get() = this.toString(".",StrQuotation)
    member this.phpcode with get() = "<?php echo " + this.code + " ?>"
    static member (++) (a:PHPdata,b:PHPdata) = PHPdata(a.data@b.data, Aqualis.merge a.Context b.Context)
    static member (++) (a:string,b:PHPdata) = PHPdata a ++ b
    static member (++) (a:PHPdata,b:string) = a ++ PHPdata b
    static member (++) (a:PHPdata,b:int0) = a ++ PHPdata b
    static member (++) (a:PHPdata,b:double0) = a ++ PHPdata b
    static member (++) (a:PHPdata,b:complex0) = a ++ PHPdata b

    member this.foreach code =
        context.ch.i <| fun i ->
            ContextPhp(context).phpcode <| fun () -> context.writei ("for("+i.code+"=0; "+i.code+"<count("+this.code+"); "+i.code+"++):")
            context.indentInc()
            code i
            context.indentDec()
            ContextPhp(context).phpcode <| fun () -> context.writei "endfor;"
    member this.foreach (key:PHPdata,value:PHPdata) = fun code ->
        let ctx = Aqualis.mergeMany [context; key.Context; value.Context]
        ctx.ch.i <| fun _ ->
            ContextPhp(ctx).phpcode <| fun () -> ctx.writei ("foreach("+this.code+" as "+key.code+" => "+value.code+"):")
            code()
            ContextPhp(ctx).phpcode <| fun () -> ctx.writei "endforeach;"
    static member (<==) (a:PHPdata,b:PHPdata) =
        Aqualis.merge a.Context b.Context |> ignore
        a.Context.codewritein("<?php ", a.code + " = " + b.code + "; ?>")
    static member (<==) (a:PHPdata,b:string) = a <== PHPdata b
    static member (<==) (a:PHPdata,b:int0) = a <== PHPdata b
    static member (<==) (a:PHPdata,b:double0) = a <== PHPdata b
    static member (<==) (a:PHPdata,b:complex0) = a <== PHPdata b
    static member (<==) (a:PHPdata,b:int) = a <== PHPdata (I b)
    static member (<==) (a:int0,b:PHPdata) = PHPdata a <== b
    static member (<==) (a:double0,b:PHPdata) = PHPdata a <== b
    static member (<==) (a:complex0,b:PHPdata) = PHPdata a <== b
    static member private Compare(a:PHPdata,b:Aqualis, expression) = bool0(expression, Aqualis.merge a.Context b)
    static member (.=) (a:PHPdata,b:PHPdata) = PHPdata.Compare(a,b.Context,Eq(Var(Nt,a.code,NaN),Var(Nt,b.code,NaN)))
    static member (.=) (a:PHPdata,b:int0) = PHPdata.Compare(a,b.Context,Eq(Var(Nt,a.code,NaN),b.Expr))
    static member (.=) (a:PHPdata,b:double0) = PHPdata.Compare(a,b.Context,Eq(Var(Nt,a.code,NaN),b.Expr))
    static member (.=) (a:PHPdata,b:int) = PHPdata.Compare(a,Aqualis.BlankWriter PHP,Eq(Var(Nt,a.code,NaN),Int b))
    static member (.=) (a:PHPdata,b:string) = a .= PHPdata b
    static member (.=/) (a:PHPdata,b:PHPdata) = PHPdata.Compare(a,b.Context,NEq(Var(Nt,a.code,NaN),Var(Nt,b.code,NaN)))
    static member (.=/) (a:PHPdata,b:int0) = PHPdata.Compare(a,b.Context,NEq(Var(Nt,a.code,NaN),b.Expr))
    static member (.=/) (a:PHPdata,b:double0) = PHPdata.Compare(a,b.Context,NEq(Var(Nt,a.code,NaN),b.Expr))
    static member (.=/) (a:PHPdata,b:int) = PHPdata.Compare(a,Aqualis.BlankWriter PHP,NEq(Var(Nt,a.code,NaN),Int b))
    static member (.=/) (a:PHPdata,b:string) = a .=/ PHPdata b
    static member (.<) (a:PHPdata,b:PHPdata) = PHPdata.Compare(a,b.Context,Less(Var(Nt,a.code,NaN),Var(Nt,b.code,NaN)))
    static member (.<) (a:PHPdata,b:int0) = PHPdata.Compare(a,b.Context,Less(Var(Nt,a.code,NaN),b.Expr))
    static member (.<) (a:PHPdata,b:double0) = PHPdata.Compare(a,b.Context,Less(Var(Nt,a.code,NaN),b.Expr))
    static member (.<) (a:PHPdata,b:int) = PHPdata.Compare(a,Aqualis.BlankWriter PHP,Less(Var(Nt,a.code,NaN),Int b))
    static member (.<=) (a:PHPdata,b:PHPdata) = PHPdata.Compare(a,b.Context,LessEq(Var(Nt,a.code,NaN),Var(Nt,b.code,NaN)))
    static member (.<=) (a:PHPdata,b:int0) = PHPdata.Compare(a,b.Context,LessEq(Var(Nt,a.code,NaN),b.Expr))
    static member (.<=) (a:PHPdata,b:double0) = PHPdata.Compare(a,b.Context,LessEq(Var(Nt,a.code,NaN),b.Expr))
    static member (.<=) (a:PHPdata,b:int) = PHPdata.Compare(a,Aqualis.BlankWriter PHP,LessEq(Var(Nt,a.code,NaN),Int b))
    static member (.>) (a:PHPdata,b:PHPdata) = PHPdata.Compare(a,b.Context,Greater(Var(Nt,a.code,NaN),Var(Nt,b.code,NaN)))
    static member (.>) (a:PHPdata,b:int0) = PHPdata.Compare(a,b.Context,Greater(Var(Nt,a.code,NaN),b.Expr))
    static member (.>) (a:PHPdata,b:double0) = PHPdata.Compare(a,b.Context,Greater(Var(Nt,a.code,NaN),b.Expr))
    static member (.>) (a:PHPdata,b:int) = PHPdata.Compare(a,Aqualis.BlankWriter PHP,Greater(Var(Nt,a.code,NaN),Int b))
    static member (.>=) (a:PHPdata,b:PHPdata) = PHPdata.Compare(a,b.Context,GreaterEq(Var(Nt,a.code,NaN),Var(Nt,b.code,NaN)))
    static member (.>=) (a:PHPdata,b:int0) = PHPdata.Compare(a,b.Context,GreaterEq(Var(Nt,a.code,NaN),b.Expr))
    static member (.>=) (a:PHPdata,b:double0) = PHPdata.Compare(a,b.Context,GreaterEq(Var(Nt,a.code,NaN),b.Expr))
    static member (.>=) (a:PHPdata,b:int) = PHPdata.Compare(a,Aqualis.BlankWriter PHP,GreaterEq(Var(Nt,a.code,NaN),Int b))

and ContextPhp internal (context:Aqualis) =
    let merge contexts = Aqualis.mergeMany (context :: contexts)
    let data code contexts = PHPdata.f(code, merge contexts)
    let boolean code contexts = bool0(Var(Nt,code,NaN), merge contexts)
    /// Creates a PHP variable associated with this generation context.
    member _.var(name:string) = PHPdata.var(context,name)
    member _.var(name:string,init:PHPdata) = PHPdata.var(context,name,init)
    member _.var(name:string,init:int0) = PHPdata.var(context,name,init)
    member _.var(name:string,init:int) = PHPdata.var(context,name,init)
    member _.var(name:string,init:double0) = PHPdata.var(context,name,init)
    member _.var(name:string,init:double) = PHPdata.var(context,name,init)
    /// Creates an empty PHP array expression associated with this generation context.
    member _.array() = PHPdata.array(context)
    member _.array(name:string) = PHPdata.array(context,name)
    member _.array(arrayname:string,data:list<string*string>) = PHPdata.array(context,arrayname,data)
    member _.array(arrayname:string,data:list<string*PHPdata>) = PHPdata.array(context,arrayname,data)
    member this.phpcode (code:unit->unit) =
        context.write "<?php "
        code()
        context.writen " ?>"
    /// POST送信されたデータを表示
    member this.postCheck() = context.codewritein("<?php ", "print_r($_POST); ?>")
    /// POST送信されたファイルを表示
    member this.postFileCheck() = context.codewritein("<?php ", "print_r($_FILES); ?>")
    member this.And (x:list<bool0>) = boolean ("(" + String.Join(" && ", x |> List.map (fun s -> s.code)) + ")") (x |> List.map _.Context)
    member this.Or (x:list<bool0>) = boolean ("(" + String.Join(" || ", x |> List.map (fun s -> s.code)) + ")") (x |> List.map _.Context)
    member this.isset (x:PHPdata) = boolean ("isset(" + x.code + ")") [x.Context]
    member this.isNotset (x:PHPdata) = boolean ("!isset(" + x.code + ")") [x.Context]
    member this.echo (x:PHPdata) = this.phpcode <| fun () -> context.writei("echo " + x.code + ";")
    member this.echo (x:string) = this.echo (PHPdata x)
    /// 変数を表示
    member this.echo (x:int0) = this.echo (PHPdata x)
    member this.echo (x:double0) = this.echo (PHPdata x)
    member this.echo (x:complex0) = this.echo (PHPdata x)
    member this.file_get_contents (filename:PHPdata) = data ("file_get_contents(" + filename.code + ")") [filename.Context]
    member this.file_get_contents (filename:string) = this.file_get_contents (PHPdata filename)
    member this.file_put_contents (filename:PHPdata,x:PHPdata) =
        merge [filename.Context; x.Context] |> ignore
        this.phpcode <| fun () ->
            context.writei("if (file_put_contents(" + filename.code + ", " + x.code + ", LOCK_EX) === false) {")
            context.indentInc()
            context.writei "throw new \\RuntimeException('Failed to write the file.');"
            context.indentDec()
            context.writei "}"
    member this.file_put_contents (filename:string,x:PHPdata) = this.file_put_contents(PHPdata filename, x)
    member this.json_decode (x:PHPdata,p:bool) = data ("json_decode("+x.code+","+p.ToString()+")") [x.Context]
    member this.json_encode (x:PHPdata) =
        data
            ("json_encode(" + x.code +
             ", JSON_THROW_ON_ERROR|JSON_PRETTY_PRINT|JSON_UNESCAPED_UNICODE|JSON_UNESCAPED_SLASHES)")
            [x.Context]
    /// Encodes a value as JSON and writes it with an exclusive lock.
    member this.writeJson(filename:PHPdata,value:PHPdata) =
        this.file_put_contents(filename, this.json_encode(value))
    /// Encodes a value as JSON and writes it with an exclusive lock.
    member this.writeJson(filename:string,value:PHPdata) =
        this.writeJson(PHPdata filename, value)
    member this.array_column(value:PHPdata,id:PHPdata) = data ("array_column("+value.code+","+id.code+")") [value.Context;id.Context]
    member this.in_array_strict(s:PHPdata, idArray:PHPdata) = boolean ("in_array("+s.code+", "+idArray.code+", true)") [s.Context;idArray.Context]
    member this.in_array_strict(s:int0, idArray:PHPdata) = this.in_array_strict(PHPdata s, idArray)
    member this.in_array_strict(s:double0, idArray:PHPdata) = this.in_array_strict(PHPdata s, idArray)
    member this.in_array_strict(s:complex0, idArray:PHPdata) = this.in_array_strict(PHPdata s, idArray)
    member this.array_search(s:PHPdata, idArray:PHPdata) = data ("array_search("+s.code+", "+idArray.code+")") [s.Context;idArray.Context]
    member this.file(filename:PHPdata, flag:list<FileFlag>) =
        data ("file("+filename.code+", "+(flag |> List.map (fun s -> s.str) |> (fun p -> String.Join(" | ",p)) )+")") [filename.Context]
    member this.fopen(filename:PHPdata,rw:FileOpenMode) = data ("fopen("+filename.code+", "+rw.str+")") [filename.Context]
    member this.fopen(filename:string,rw:FileOpenMode) = this.fopen(PHPdata filename, rw)
    /// ファイルに書き込み
    member this.fwrite(fp:PHPdata,t:PHPdata) = this.phpcode <| fun () -> context.writei("fwrite("+fp.code+", "+t.code+");")
    /// ファイルに書き込み
    member this.fwrite(fp:PHPdata,t:int0) = this.fwrite(fp, PHPdata t)
    member this.fwrite(fp:PHPdata,t:double0) = this.fwrite(fp, PHPdata t)
    member this.fwrite(fp:PHPdata,t:complex0) = this.fwrite(fp, PHPdata t)
    /// ファイルに書き込み
    member this.fwrite(fp:PHPdata,t:string) = this.fwrite(fp, PHPdata t)
    /// ファイルに書き込み
    member this.fwrite(fp:PHPdata,t:int) = this.phpcode <| fun () -> context.writei("fwrite("+fp.code+", "+t.ToString()+");")
    /// ファイルにShift-JISで書き込み
    member this.fwrite_SJIS(fp:PHPdata,t:PHPdata) = this.phpcode <| fun () -> context.writei("fwrite("+fp.code+", mb_convert_encoding("+t.code+", 'SJIS-win', 'UTF-8'));")
    /// ファイルにShift-JISで書き込み
    member this.fwrite_SJIS(fp:PHPdata,t:int0) = this.phpcode <| fun () -> context.writei("fwrite("+fp.code+", mb_convert_encoding("+t.code+", 'SJIS-win', 'UTF-8'));")
    member this.fwrite_SJIS(fp:PHPdata,t:double0) = this.phpcode <| fun () -> context.writei("fwrite("+fp.code+", mb_convert_encoding("+t.code+", 'SJIS-win', 'UTF-8'));")
    member this.fwrite_SJIS(fp:PHPdata,t:complex0) = this.phpcode <| fun () -> context.writei("fwrite("+fp.code+", mb_convert_encoding("+t.code+", 'SJIS-win', 'UTF-8'));")
    /// ファイルにShift-JISで書き込み
    member this.fwrite_SJIS(fp:PHPdata,t:string) = this.phpcode <| fun () -> context.writei("fwrite("+fp.code+", mb_convert_encoding(\""+t+"\", 'SJIS-win', 'UTF-8'));")
    /// ファイルを閉じる
    member this.fclose(filename:PHPdata) = this.phpcode <| fun () -> context.writei("fclose("+filename.code+");")
    /// 正規表現
    member this.preg_match(p:PHPdata,text:PHPdata,mat:PHPdata) = this.phpcode <| fun () -> context.writei("preg_match("+p.code+","+text.code+","+mat.code+");")
    member this.download(filename:string) =
        this.phpcode <| fun () -> context.writei "header('Content-Type: application/octet-stream');"
        this.phpcode <| fun () -> context.writei("header('Content-Length: '.filesize(\""+filename+"\"));")
        this.phpcode <| fun () -> context.writei("header('Content-Disposition: attachment; filename=\""+filename+"\"');")
        this.phpcode <| fun () -> context.writei("readfile(\""+filename+"\");")
        this.phpcode <| fun () -> context.writei "exit;"
    /// 整数に変換
    member this.intval(s:PHPdata) = data ("intval("+s.code+")") [s.Context]
    member this.array_sum(value:PHPdata) = data ("array_sum("+value.code+")") [value.Context]
    member this.strlen(value:PHPdata) = data ("strlen("+value.code+")") [value.Context]
    member this.is_numeric(value:PHPdata) = boolean ("is_numeric("+value.code+")") [value.Context]
    member this.nt (value:bool0) = boolean ("!"+value.code) [value.Context]
    member this.set_nocache() = this.phpcode <| fun () -> context.writei "header( 'Cache-Control: no-store, no-cache, must-revalidate' );"
    member this.header(data:PHPdata) = this.phpcode <| fun () -> context.writei("header("+data.code+");")
    member this.header(data:string) = this.header(PHPdata data)
    member this.date(fmt:string) = data ("date(" + PhpEncoding.stringLiteral fmt + ")") []
    member this.round(x:PHPdata) = data ("round("+x.code+")") [x.Context]
    member this.round(x:double0) = this.round(PHPdata x)
    member this.substr(x:PHPdata,n:PHPdata) = data ("substr("+x.code+","+n.code+")") [x.Context;n.Context]
    member this.substr(x:PHPdata,n:int) = data ("substr("+x.code+","+n.ToString()+")") [x.Context]
    member this.file_exists(x:PHPdata) = boolean ("file_exists("+x.code+")") [x.Context]
    member this.file_exists(x:string) = boolean ("file_exists(\""+x+"\")") []
    member this.mb_strlen(x:PHPdata) = data ("mb_strlen("+x.code+")") [x.Context]
    member this.mb_strwidth(x:PHPdata) = data ("mb_strwidth("+x.code+")") [x.Context]
    member this.strncmp(x:PHPdata,y:string,n:int) = data ("strncmp("+x.code+",\""+y+"\","+n.ToString()+")") [x.Context]
    member this.glob(x:PHPdata) = data ("glob("+x.code+")") [x.Context]
    member this.glob(x:string) = data ("glob(\""+x+"\")") []
    member this.explode(x:PHPdata,y:PHPdata) = data ("explode("+x.code+","+y.code+")") [x.Context;y.Context]
    member this.explode(x:string,y:PHPdata) = data ("explode('"+x+"',"+y.code+")") [y.Context]
    member this.sort(data:PHPdata) = this.phpcode <| fun () -> context.writei("sort("+data.code+");")
    member this.toint(x:PHPdata) = int0(Var(It 4, "(int)"+x.code, NaN), merge [x.Context])
    member this.count(x:PHPdata) = int0(Var(It 4, "count("+x.code+")", NaN), merge [x.Context])
    member this.filename_withoutExtension(x:PHPdata) = data ("pathinfo("+x.code+", PATHINFO_FILENAME)") [x.Context]
    member this.unlink(data:PHPdata) = this.phpcode <| fun () -> context.writei("unlink("+data.code+");")
    member this.shuffle(data:PHPdata) = this.phpcode <| fun () -> context.writei("shuffle("+data.code+");")
    member this.setTimeZone(location:string) = this.phpcode <| fun () -> context.writei("date_default_timezone_set('"+location+"');")
    member this.sendMail(body:PHPdata,subject:PHPdata,fromAddress:PHPdata,toAddress:PHPdata) =
        this.phpcode <| fun () -> context.writei "mb_language(\"ja\");"
        this.phpcode <| fun () -> context.writei "mb_internal_encoding(\"UTF-8\");"
        this.phpcode <| fun () -> context.writei("mb_send_mail("+toAddress.code+","+subject.code+","+body.code+","+("From: "++fromAddress).code+");")
    /// メール送信
    member this.sendMail(body:PHPdata,subject:PHPdata,smtp:PHPdata,fromAddress:PHPdata,toAddress:PHPdata) =
        merge [body.Context; subject.Context; smtp.Context; fromAddress.Context; toAddress.Context] |> ignore
        this.phpcode <| fun () ->
            context.writei "(function ($body, $subject, $smtp, $fromAddress, $toAddress): void {"
            context.writei "if (preg_match('/[\\r\\n]/', $subject) === 1) {"
            context.writei "throw new \\InvalidArgumentException('The mail subject must not contain CR or LF characters.');"
            context.writei "}"
            context.writei "if (filter_var($fromAddress, FILTER_VALIDATE_EMAIL) === false || strncmp($fromAddress, '-', 1) === 0) {"
            context.writei "throw new \\InvalidArgumentException('Invalid sender email address.');"
            context.writei "}"
            context.writei "if (filter_var($toAddress, FILTER_VALIDATE_EMAIL) === false || strncmp($toAddress, '-', 1) === 0) {"
            context.writei "throw new \\InvalidArgumentException('Invalid recipient email address.');"
            context.writei "}"
            context.writei "if (filter_var($smtp, FILTER_VALIDATE_IP, FILTER_FLAG_IPV4) === false && filter_var($smtp, FILTER_VALIDATE_DOMAIN, FILTER_FLAG_HOSTNAME) === false) {"
            context.writei "throw new \\InvalidArgumentException('Invalid SMTP host.');"
            context.writei "}"
            context.writei "$command = ['mail', '-s', $subject, '-S', 'smtp=smtp://' . $smtp . ':25', '-r', $fromAddress, $toAddress];"
            context.writei "$descriptors = [0 => ['pipe', 'r'], 1 => ['pipe', 'w'], 2 => ['pipe', 'w']];"
            context.writei "$process = proc_open($command, $descriptors, $pipes);"
            context.writei "if (!is_resource($process)) {"
            context.writei "throw new \\RuntimeException('Failed to start the mail command.');"
            context.writei "}"
            context.writei "try {"
            context.writei "if (fwrite($pipes[0], $body) === false) {"
            context.writei "throw new \\RuntimeException('Failed to write the mail body.');"
            context.writei "}"
            context.writei "fclose($pipes[0]);"
            context.writei "$stdout = stream_get_contents($pipes[1]);"
            context.writei "fclose($pipes[1]);"
            context.writei "$stderr = stream_get_contents($pipes[2]);"
            context.writei "fclose($pipes[2]);"
            context.writei "} catch (\\Throwable $error) {"
            context.writei "foreach ($pipes as $pipe) {"
            context.writei "if (is_resource($pipe)) { fclose($pipe); }"
            context.writei "}"
            context.writei "proc_terminate($process);"
            context.writei "proc_close($process);"
            context.writei "throw $error;"
            context.writei "}"
            context.writei "$exitCode = proc_close($process);"
            context.writei "if ($exitCode !== 0) {"
            context.writei "throw new \\RuntimeException('The mail command failed: ' . trim((string)$stderr));"
            context.writei "}"
            context.writei ("})("+body.code+", "+subject.code+", "+smtp.code+", "+fromAddress.code+", "+toAddress.code+");")
    member this.sendDiscord(body:PHPdata,webhookURL:PHPdata) =
        merge [body.Context; webhookURL.Context] |> ignore
        this.phpcode <| fun () ->
            context.writei "(function ($body, $webhookURL): void {"
            context.writei "$urlParts = parse_url($webhookURL);"
            context.writei "$allowedHosts = ['discord.com', 'ptb.discord.com', 'canary.discord.com', 'discordapp.com'];"
            context.writei "if ($urlParts === false || ($urlParts['scheme'] ?? null) !== 'https' || !isset($urlParts['host']) || !in_array(strtolower($urlParts['host']), $allowedHosts, true)) {"
            context.writei "throw new \\InvalidArgumentException('Invalid Discord webhook URL.');"
            context.writei "}"
            context.writei "$payload = json_encode(['username' => 'Ediass Notification', 'content' => $body], JSON_THROW_ON_ERROR | JSON_UNESCAPED_UNICODE);"
            context.writei "$curl = curl_init($webhookURL);"
            context.writei "if ($curl === false) {"
            context.writei "throw new \\RuntimeException('Failed to initialize cURL.');"
            context.writei "}"
            context.writei "try {"
            context.writei "curl_setopt_array($curl, ["
            context.writei "CURLOPT_POST => true,"
            context.writei "CURLOPT_POSTFIELDS => $payload,"
            context.writei "CURLOPT_HTTPHEADER => ['Content-Type: application/json'],"
            context.writei "CURLOPT_RETURNTRANSFER => true,"
            context.writei "CURLOPT_CONNECTTIMEOUT => 5,"
            context.writei "CURLOPT_TIMEOUT => 15,"
            context.writei "CURLOPT_FOLLOWLOCATION => false,"
            context.writei "CURLOPT_PROTOCOLS => CURLPROTO_HTTPS"
            context.writei "]);"
            context.writei "$response = curl_exec($curl);"
            context.writei "if ($response === false) {"
            context.writei "throw new \\RuntimeException('Discord webhook request failed: ' . curl_error($curl));"
            context.writei "}"
            context.writei "$statusCode = curl_getinfo($curl, CURLINFO_RESPONSE_CODE);"
            context.writei "if ($statusCode < 200 || $statusCode >= 300) {"
            context.writei "throw new \\RuntimeException('Discord webhook returned HTTP ' . $statusCode . '.');"
            context.writei "}"
            context.writei "} finally {"
            context.writei "curl_close($curl);"
            context.writei "}"
            context.writei ("})("+body.code+", "+webhookURL.code+");")
    member this.str_replace(strfrom:string,strto:string,str:PHPdata) = data ("str_replace("+"\""+strfrom+"\""+","+"\""+strto+"\""+","+str.code+")") [str.Context]
    member this.str_pad(num:PHPdata,ndigit:int,paddingnum:int) = data ("str_pad("+num.code+","+ndigit.ToString()+","+paddingnum.ToString()+", STR_PAD_LEFT)") [num.Context]
    member this.file_download(file:PHPdata) =
        this.phpcode <| fun () ->
            context.writei "header('Content-Type: application/octet-stream');"
        this.phpcode <| fun () ->
            context.writei "header('Content-Transfer-Encoding: Binary');"
        this.phpcode <| fun () ->
            context.writei("header('Content-disposition: attachment; filename='.basename("+file.code+"));")
        this.phpcode <| fun () ->
            context.writei("header('Content-Length: '.filesize("+file.code+"));")
        this.phpcode <| fun () ->
            context.writei "while (ob_get_level()) { ob_end_clean(); }"
        this.phpcode <| fun () ->
            context.writei("readfile("+file.code+");")
        this.phpcode <| fun () ->
            context.writei "exit;"
    member this.basename(file:PHPdata) = data ("basename("+file.code+")") [file.Context]
    member this.br = "\\n"
    member this.tb = "\\t"

[<AutoOpen>]
module num0ForPHP =

    type int0 with
        member this.phpdata with get() = PHPdata([RNvr(this.Expr,this.Context)], this.Context)

    type double0 with
        member this.phpdata with get() = PHPdata([RNvr(this.Expr,this.Context)], this.Context)

    type complex0 with
        member this.phpdata with get() = PHPdata([RNvr(this.Expr,this.Context)], this.Context)

    type html with
        member this.h1 (t:PHPdata) = this.h1 t.phpcode
        member this.h2 (t:PHPdata) = this.h2 t.phpcode
        member this.h3 (t:PHPdata) = this.h3 t.phpcode
        member this.h4 (t:PHPdata) = this.h4 t.phpcode

    type Aqualis with
        ///<summary>PHPコード生成</summary>
        member this.php = ContextPhp(this)
