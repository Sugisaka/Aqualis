//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

open System
open System.IO

/// Limits applied while reading and decoding a JSON file in generated PHP.
type JsonReadOptions = {
    MaxBytes: int
    MaxDepth: int
}

[<RequireQualifiedAccess>]
module JsonReadOptions =
    /// Conservative defaults for small application data files.
    let defaults = {
        MaxBytes = 1024 * 1024
        MaxDepth = 64
    }

/// Limits applied while updating a JSON file under an exclusive lock.
type JsonUpdateOptions = {
    MaxInputBytes: int
    MaxOutputBytes: int
    MaxDepth: int
    FilePermissions: int
}

[<RequireQualifiedAccess>]
module JsonUpdateOptions =
    /// Conservative defaults for small private application data files.
    let defaults = {
        MaxInputBytes = 1024 * 1024
        MaxOutputBytes = 1024 * 1024
        MaxDepth = 64
        FilePermissions = 0o640
    }

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

/// Result of a checked JSON file read emitted into generated PHP.
and JsonReadResult internal (result:PHPdata) =
    member _.IsSuccess =
        bool0(
            Var(Nt, "(" + result["success"].code + " === true)", NaN),
            result.Context)
    member _.Value = result["value"]
    member _.ErrorCode = result["error"]

/// Result of an atomic JSON update emitted into generated PHP.
and JsonUpdateResult internal (result:PHPdata) =
    member _.IsSuccess =
        bool0(
            Var(Nt, "(" + result["success"].code + " === true)", NaN),
            result.Context)
    member _.Value = result["value"]
    member _.ErrorCode = result["error"]

/// A PHP stream handle that can only be created by checked Aqualis file APIs.
and PhpFileHandle internal (value:PHPdata) =
    member internal _.Value = value
    member _.Context = value.Context

and ContextPhp internal (context:Aqualis) =
    let merge contexts = Aqualis.mergeMany (context :: contexts)
    let data code contexts = PHPdata.f(code, merge contexts)
    let boolean code contexts = bool0(Var(Nt,code,NaN), merge contexts)
    let validateRedirectStatus statusCode =
        if not (List.contains statusCode [301; 302; 303; 307; 308]) then
            invalidArg (nameof statusCode) "Redirect status must be 301, 302, 303, 307, or 308."
    member internal _.Context = context
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
    /// Emits a PHP value as escaped HTML text.
    member this.echoHtmlText (x:PHPdata) =
        this.phpcode <| fun () ->
            context.writei(
                "echo htmlspecialchars((string)(" + x.code + "), ENT_QUOTES | ENT_SUBSTITUTE, 'UTF-8');")
    member this.echoHtmlText (x:string) = this.echoHtmlText (PHPdata x)
    /// 変数を表示
    member this.echo (x:int0) = this.echo (PHPdata x)
    member this.echo (x:double0) = this.echo (PHPdata x)
    member this.echo (x:complex0) = this.echo (PHPdata x)
    /// Reads an entire file and throws when PHP reports an I/O failure.
    member this.readFile(filename:PHPdata) =
        data
            ("(function ($filename) { $contents = @file_get_contents($filename); " +
             "if ($contents === false) { throw new \\RuntimeException('Failed to read the file.'); } " +
             "return $contents; })(" + filename.code + ")")
            [filename.Context]
    member this.readFile(filename:string) = this.readFile(PHPdata filename)
    /// Reads a bounded JSON file and reports path, I/O, and JSON syntax failures without exposing warnings.
    member this.tryReadJsonFile(resultName:PhpVariableName, filename:PHPdata, options:JsonReadOptions) =
        if options.MaxBytes <= 0 then
            invalidArg (nameof options) "The maximum JSON file size must be positive."
        if options.MaxBytes = Int32.MaxValue then
            invalidArg (nameof options) "The maximum JSON file size is too large."
        if options.MaxDepth <= 0 then
            invalidArg (nameof options) "The maximum JSON depth must be positive."

        merge [filename.Context] |> ignore
        let name = PhpVariableName.value resultName
        let result = PHPdata.var(context,name)
        let resultCode = result.code
        let fileSize = "$" + name + "_fileSize"
        let jsonText = "$" + name + "_jsonText"
        let decoded = "$" + name + "_decoded"
        let maxBytes = InvariantFormat.integer options.MaxBytes
        let readLimit = InvariantFormat.integer (options.MaxBytes + 1)
        let maxDepth = InvariantFormat.integer options.MaxDepth
        let error code = PhpEncoding.stringLiteral code
        let source =
            resultCode + " = ['success' => false, 'value' => null, 'error' => null]; " +
            "if (!is_string(" + filename.code + ") || " + filename.code + " === '') { " +
            resultCode + "['error'] = " + error "invalid_path" + "; " +
            "} elseif (!is_file(" + filename.code + ")) { " +
            resultCode + "['error'] = " + error "file_missing" + "; " +
            "} elseif (!is_readable(" + filename.code + ")) { " +
            resultCode + "['error'] = " + error "file_unreadable" + "; " +
            "} else { " +
            fileSize + " = @filesize(" + filename.code + "); " +
            "if (" + fileSize + " === false) { " +
            resultCode + "['error'] = " + error "read_failed" + "; " +
            "} elseif (" + fileSize + " > " + maxBytes + ") { " +
            resultCode + "['error'] = " + error "file_too_large" + "; " +
            "} else { " +
            jsonText + " = @file_get_contents(" + filename.code + ", false, null, 0, " + readLimit + "); " +
            "if (" + jsonText + " === false) { " +
            resultCode + "['error'] = " + error "read_failed" + "; " +
            "} elseif (strlen(" + jsonText + ") > " + maxBytes + ") { " +
            resultCode + "['error'] = " + error "file_too_large" + "; " +
            "} else { try { " +
            decoded + " = json_decode(" + jsonText + ", true, " + maxDepth + ", JSON_THROW_ON_ERROR); " +
            resultCode + " = ['success' => true, 'value' => " + decoded + ", 'error' => null]; " +
            "} catch (\\JsonException $error) { " +
            resultCode + "['error'] = " + error "invalid_json" + "; " +
            "} } } }"
        context.codewritein("<?php ", source + " ?>")
        JsonReadResult(result)
    /// Reads a bounded JSON file using a static path.
    member this.tryReadJsonFile(resultName:PhpVariableName, filename:string, options:JsonReadOptions) =
        this.tryReadJsonFile(resultName, PHPdata filename, options)
    /// Reads, changes, and atomically replaces a JSON file while holding one stable sidecar lock.
    member this.updateJsonFileAtomic(resultName:PhpVariableName, filename:PHPdata, options:JsonUpdateOptions, update:PHPdata -> unit) =
        if isNull (box update) then nullArg (nameof update)
        if options.MaxInputBytes <= 0 || options.MaxInputBytes = Int32.MaxValue then
            invalidArg (nameof options) "The maximum input JSON size must be positive and leave room for a bounded read."
        if options.MaxOutputBytes <= 0 then
            invalidArg (nameof options) "The maximum output JSON size must be positive."
        if options.MaxDepth <= 0 then
            invalidArg (nameof options) "The maximum JSON depth must be positive."
        if options.FilePermissions < 0 || options.FilePermissions > 0o777 then
            invalidArg (nameof options) "File permissions must be between 0000 and 0777."

        merge [filename.Context] |> ignore
        let name = PhpVariableName.value resultName
        let result = PHPdata.var(context,name)
        let prefix = "$" + name + "_"
        let target = prefix + "target"
        let directory = prefix + "directory"
        let lockPath = prefix + "lockPath"
        let lockHandle = prefix + "lockHandle"
        let locked = prefix + "locked"
        let fileSize = prefix + "fileSize"
        let jsonText = prefix + "jsonText"
        let dataName = prefix + "data"
        let encoded = prefix + "encoded"
        let temporaryPath = prefix + "temporaryPath"
        let temporaryHandle = prefix + "temporaryHandle"
        let length = prefix + "length"
        let offset = prefix + "offset"
        let written = prefix + "written"
        let errorCode = prefix + "errorCode"
        let error = prefix + "exception"
        let inputLimit = InvariantFormat.integer options.MaxInputBytes
        let inputReadLimit = InvariantFormat.integer (options.MaxInputBytes + 1)
        let outputLimit = InvariantFormat.integer options.MaxOutputBytes
        let maxDepth = InvariantFormat.integer options.MaxDepth
        let permissions = "0" + Convert.ToString(options.FilePermissions,8)
        let fail code message =
            errorCode + " = " + PhpEncoding.stringLiteral code + "; throw new \\RuntimeException(" + PhpEncoding.stringLiteral message + ");"

        this.phpcode <| fun () ->
            context.writei (result.code + " = (function (" + target + "): array {")
            context.writei (lockHandle + " = null; " + locked + " = false; " + temporaryPath + " = null; " + temporaryHandle + " = null;")
            context.writei (errorCode + " = " + PhpEncoding.stringLiteral "update_failed" + ";")
            context.writei "try {"
            context.writei ("if (!is_string(" + target + ") || " + target + " === '') { " + fail "invalid_path" "The JSON update path is invalid." + " }")
            context.writei (directory + " = realpath(dirname(" + target + "));")
            context.writei ("if (" + directory + " === false || !is_dir(" + directory + ") || !is_writable(" + directory + ")) { " + fail "directory_unavailable" "The JSON update directory is unavailable." + " }")
            context.writei (target + " = " + directory + ".DIRECTORY_SEPARATOR.basename(" + target + ");")
            context.writei ("if (is_link(" + target + ") || !is_file(" + target + ") || !is_readable(" + target + ")) { " + fail "file_unavailable" "The JSON update file is unavailable." + " }")
            context.writei (lockPath + " = " + target + ".'.lock';")
            context.writei ("if (is_link(" + lockPath + ")) { " + fail "lock_unavailable" "The JSON update lock is unavailable." + " }")
            context.writei (lockHandle + " = @fopen(" + lockPath + ", 'c');")
            context.writei ("if (" + lockHandle + " === false) { " + fail "lock_open_failed" "Failed to open the JSON update lock." + " }")
            context.writei ("if (!@chmod(" + lockPath + ", " + permissions + ")) { " + fail "lock_protection_failed" "Failed to protect the JSON update lock." + " }")
            context.writei ("if (!flock(" + lockHandle + ", LOCK_EX)) { " + fail "lock_failed" "Failed to lock the JSON update file." + " }")
            context.writei (locked + " = true;")
            context.writei ("if (is_link(" + target + ") || !is_file(" + target + ") || !is_readable(" + target + ")) { " + fail "file_unavailable" "The JSON update file became unavailable." + " }")
            context.writei (fileSize + " = @filesize(" + target + ");")
            context.writei ("if (" + fileSize + " === false) { " + fail "read_failed" "Failed to inspect the JSON update file." + " }")
            context.writei ("if (" + fileSize + " > " + inputLimit + ") { " + fail "file_too_large" "The JSON update file is too large." + " }")
            context.writei (jsonText + " = @file_get_contents(" + target + ", false, null, 0, " + inputReadLimit + ");")
            context.writei ("if (!is_string(" + jsonText + ")) { " + fail "read_failed" "Failed to read the JSON update file." + " }")
            context.writei ("if (strlen(" + jsonText + ") > " + inputLimit + ") { " + fail "file_too_large" "The JSON update file is too large." + " }")
            context.writei ("try { " + dataName + " = json_decode(" + jsonText + ", true, " + maxDepth + ", JSON_THROW_ON_ERROR); } catch (\\JsonException " + error + ") { " + fail "invalid_json" "The JSON update file is invalid." + " }")

        update (PHPdata.f(dataName,context))

        this.phpcode <| fun () ->
            context.writei (encoded + " = json_encode(" + dataName + ", JSON_THROW_ON_ERROR | JSON_PRETTY_PRINT | JSON_UNESCAPED_UNICODE | JSON_UNESCAPED_SLASHES);")
            context.writei ("if (strlen(" + encoded + ") > " + outputLimit + ") { " + fail "output_too_large" "The updated JSON data is too large." + " }")
            context.writei (temporaryPath + " = tempnam(" + directory + ", '.aqualis-json-');")
            context.writei ("if (" + temporaryPath + " === false) { " + fail "temporary_file_failed" "Failed to create a temporary JSON file." + " }")
            context.writei (temporaryHandle + " = @fopen(" + temporaryPath + ", 'wb');")
            context.writei ("if (" + temporaryHandle + " === false) { " + fail "temporary_open_failed" "Failed to open the temporary JSON file." + " }")
            context.writei (length + " = strlen(" + encoded + "); " + offset + " = 0;")
            context.writei ("while (" + offset + " < " + length + ") { " + written + " = @fwrite(" + temporaryHandle + ", substr(" + encoded + ", " + offset + ")); if (" + written + " === false || " + written + " === 0) { " + fail "write_failed" "Failed to write the complete JSON data." + " } " + offset + " += " + written + "; }")
            context.writei ("if (!@fflush(" + temporaryHandle + ")) { " + fail "flush_failed" "Failed to flush the JSON data." + " }")
            context.writei ("if (function_exists('fsync') && !@fsync(" + temporaryHandle + ")) { " + fail "sync_failed" "Failed to synchronize the JSON data." + " }")
            context.writei ("if (!@fclose(" + temporaryHandle + ")) { " + fail "close_failed" "Failed to close the JSON data." + " }")
            context.writei (temporaryHandle + " = null;")
            context.writei ("if (!@chmod(" + temporaryPath + ", " + permissions + ")) { " + fail "file_protection_failed" "Failed to protect the JSON data." + " }")
            context.writei ("if (!@rename(" + temporaryPath + ", " + target + ")) { " + fail "publish_failed" "Failed to publish the JSON data." + " }")
            context.writei (temporaryPath + " = null;")
            context.writei ("return ['success' => true, 'value' => " + dataName + ", 'error' => null];")
            context.writei ("} catch (\\Throwable " + error + ") { error_log('Aqualis atomic JSON update failed: '." + error + "->getMessage()); return ['success' => false, 'value' => null, 'error' => " + errorCode + "]; }")
            context.writei ("finally { if (is_resource(" + temporaryHandle + ")) { @fclose(" + temporaryHandle + "); } if (is_string(" + temporaryPath + ") && is_file(" + temporaryPath + ")) { @unlink(" + temporaryPath + "); } if (" + locked + ") { flock(" + lockHandle + ", LOCK_UN); } if (is_resource(" + lockHandle + ")) { fclose(" + lockHandle + "); } }")
            context.writei ("})(" + filename.code + ");")
        JsonUpdateResult(result)
    /// Reads, changes, and atomically replaces a JSON file at a static path.
    member this.updateJsonFileAtomic(resultName:PhpVariableName, filename:string, options:JsonUpdateOptions, update:PHPdata -> unit) =
        this.updateJsonFileAtomic(resultName, PHPdata filename, options, update)
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
    /// Creates a password hash using PHP's current default password algorithm.
    member this.password_hash(password:PHPdata) =
        data ("password_hash(" + password.code + ", PASSWORD_DEFAULT)") [password.Context]
    /// Verifies a plaintext password against a hash produced by password_hash.
    member this.password_verify(password:PHPdata,passwordHash:PHPdata) =
        boolean
            ("password_verify(" + password.code + ", " + passwordHash.code + ")")
            [password.Context; passwordHash.Context]
    /// Checks whether a stored password hash should be refreshed for PASSWORD_DEFAULT.
    member this.password_needs_rehash(passwordHash:PHPdata) =
        boolean
            ("password_needs_rehash(" + passwordHash.code + ", PASSWORD_DEFAULT)")
            [passwordHash.Context]
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
    /// Reads a file into lines and throws when PHP reports an I/O failure.
    member this.readLines(filename:PHPdata, flags:list<FileFlag>) =
        let flagExpression =
            match flags with
            | [] -> "0"
            | values -> values |> List.map (fun value -> value.str) |> String.concat " | "
        data
            ("(function ($filename) { $lines = @file($filename, " + flagExpression + "); " +
             "if ($lines === false) { throw new \\RuntimeException('Failed to read the file.'); } " +
             "return $lines; })(" + filename.code + ")")
            [filename.Context]
    member this.readLines(filename:string, flags:list<FileFlag>) = this.readLines(PHPdata filename, flags)

    /// Runs generated code with a checked PHP stream in the caller's PHP variable scope.
    /// Internal variables are unique so file scopes can be nested safely.
    member this.withFile(filename:PHPdata, mode:FileOpenMode, code:PhpFileHandle -> unit) =
        if isNull (box code) then nullArg (nameof code)
        merge [filename.Context] |> ignore
        let scopeNumber = context.NextPhpFileScopeNumber() |> InvariantFormat.integer
        let prefix = "$aqualisFileScope" + scopeNumber + "_"
        let filenameName = prefix + "filename"
        let handleName = prefix + "handle"
        let flushSucceededName = prefix + "flushSucceeded"
        let closeSucceededName = prefix + "closeSucceeded"
        let handleValue = PHPdata.f(context,handleName)
        this.phpcode <| fun () ->
            context.writei (filenameName + " = " + filename.code + ";")
            context.writei (handleName + " = @fopen(" + filenameName + ", " + mode.str + ");")
            context.writei ("if (" + handleName + " === false) { throw new \\RuntimeException('Failed to open the file.'); }")
            if mode.CanWrite then
                context.writei (flushSucceededName + " = false;")
            context.writei (closeSucceededName + " = false;")
            context.writei "try {"
        code (PhpFileHandle handleValue)
        this.phpcode <| fun () ->
            context.writei "} finally {"
            context.writei ("if (is_resource(" + handleName + ")) {")
            if mode.CanWrite then
                context.writei (flushSucceededName + " = @fflush(" + handleName + ");")
            context.writei (closeSucceededName + " = @fclose(" + handleName + ");")
            context.writei "}"
            context.writei "}"
            if mode.CanWrite then
                context.writei ("if (!" + flushSucceededName + ") { throw new \\RuntimeException('Failed to flush the file.'); }")
            context.writei ("if (!" + closeSucceededName + ") { throw new \\RuntimeException('Failed to close the file.'); }")
    member this.withFile(filename:string, mode:FileOpenMode, code:PhpFileHandle -> unit) =
        this.withFile(PHPdata filename, mode, code)

    /// Writes every byte, retrying partial writes and failing on false or zero-byte writes.
    member this.writeAll(handle:PhpFileHandle, value:PHPdata) =
        merge [handle.Context; value.Context] |> ignore
        this.phpcode <| fun () ->
            context.writei ("$aqualisRemaining = (string)(" + value.code + ");")
            context.writei "while ($aqualisRemaining !== '') {"
            context.writei ("$aqualisWritten = @fwrite(" + handle.Value.code + ", $aqualisRemaining);")
            context.writei "if ($aqualisWritten === false || $aqualisWritten === 0) { throw new \\RuntimeException('Failed to write the complete file.'); }"
            context.writei "$aqualisRemaining = (string)substr($aqualisRemaining, $aqualisWritten);"
            context.writei "}"
    member this.writeAll(handle:PhpFileHandle, value:string) = this.writeAll(handle, PHPdata value)
    member this.writeAll(handle:PhpFileHandle, value:int0) = this.writeAll(handle, PHPdata value)
    member this.writeAll(handle:PhpFileHandle, value:double0) = this.writeAll(handle, PHPdata value)
    member this.writeAll(handle:PhpFileHandle, value:complex0) = this.writeAll(handle, PHPdata value)
    member this.writeAll(handle:PhpFileHandle, value:int) = this.writeAll(handle, PHPdata (I value))

    /// Converts text to Shift-JIS and writes every resulting byte.
    member this.writeAllSjis(handle:PhpFileHandle, value:PHPdata) =
        let convertedContext = merge [handle.Context; value.Context]
        let converted = PHPdata.f("mb_convert_encoding(" + value.code + ", 'SJIS-win', 'UTF-8')", convertedContext)
        this.writeAll(handle, converted)
    member this.writeAllSjis(handle:PhpFileHandle, value:string) = this.writeAllSjis(handle, PHPdata value)
    member this.writeAllSjis(handle:PhpFileHandle, value:int0) = this.writeAllSjis(handle, PHPdata value)
    member this.writeAllSjis(handle:PhpFileHandle, value:double0) = this.writeAllSjis(handle, PHPdata value)
    member this.writeAllSjis(handle:PhpFileHandle, value:complex0) = this.writeAllSjis(handle, PHPdata value)
    /// 正規表現
    member this.preg_match(p:PHPdata,text:PHPdata,mat:PHPdata) = this.phpcode <| fun () -> context.writei("preg_match("+p.code+","+text.code+","+mat.code+");")
    member this.download(filename:string) = this.file_download(PHPdata filename)
    /// 整数に変換
    member this.intval(s:PHPdata) = data ("intval("+s.code+")") [s.Context]
    member this.array_sum(value:PHPdata) = data ("array_sum("+value.code+")") [value.Context]
    member this.strlen(value:PHPdata) = data ("strlen("+value.code+")") [value.Context]
    member this.is_numeric(value:PHPdata) = boolean ("is_numeric("+value.code+")") [value.Context]
    member this.nt (value:bool0) = boolean ("!"+value.code) [value.Context]
    member this.set_nocache() = this.phpcode <| fun () -> context.writei "header( 'Cache-Control: no-store, no-cache, must-revalidate' );"
    member this.header(data:PHPdata) = this.phpcode <| fun () -> context.writei("header("+data.code+");")
    member this.header(data:string) = this.header(PHPdata data)
    /// Internal sink used only after a public API has supplied a validated URL.
    member internal this.redirectValidated(location:Url,statusCode:int) =
        validateRedirectStatus statusCode
        let locationLiteral = PHPdata (Url.value location)
        this.phpcode <| fun () ->
            context.writei "(function ($location): void {"
            context.writei ("header('Location: ' . $location, true, " + string statusCode + ");")
            context.writei "exit;"
            context.writei ("})(" + locationLiteral.code + ");")
    member this.date(fmt:string) = data ("date(" + PhpEncoding.stringLiteral fmt + ")") []
    member this.round(x:PHPdata) = data ("round("+x.code+")") [x.Context]
    member this.round(x:double0) = this.round(PHPdata x)
    member this.substr(x:PHPdata,n:PHPdata) = data ("substr("+x.code+","+n.code+")") [x.Context;n.Context]
    member this.substr(x:PHPdata,n:int) = data ("substr("+x.code+","+n.ToString()+")") [x.Context]
    member this.file_exists(x:PHPdata) = boolean ("file_exists("+x.code+")") [x.Context]
    member this.file_exists(x:string) = this.file_exists(PHPdata x)
    member this.mb_strlen(x:PHPdata) = data ("mb_strlen("+x.code+")") [x.Context]
    member this.mb_strwidth(x:PHPdata) = data ("mb_strwidth("+x.code+")") [x.Context]
    member this.strncmp(x:PHPdata,y:PHPdata,n:int) = data ("strncmp("+x.code+","+y.code+","+n.ToString()+")") [x.Context;y.Context]
    member this.strncmp(x:PHPdata,y:string,n:int) = this.strncmp(x,PHPdata y,n)
    member this.glob(x:PHPdata) = data ("glob("+x.code+")") [x.Context]
    member this.glob(x:string) = this.glob(PHPdata x)
    member this.explode(x:PHPdata,y:PHPdata) = data ("explode("+x.code+","+y.code+")") [x.Context;y.Context]
    member this.explode(x:string,y:PHPdata) = this.explode(PHPdata x,y)
    member this.sort(data:PHPdata) = this.phpcode <| fun () -> context.writei("sort("+data.code+");")
    member this.toint(x:PHPdata) = int0(Var(It 4, "(int)"+x.code, NaN), merge [x.Context])
    member this.count(x:PHPdata) = int0(Var(It 4, "count("+x.code+")", NaN), merge [x.Context])
    member this.filename_withoutExtension(x:PHPdata) = data ("pathinfo("+x.code+", PATHINFO_FILENAME)") [x.Context]
    /// Deletes an existing file and throws when it is absent or cannot be removed.
    member this.deleteFile(path:PHPdata) =
        merge [path.Context] |> ignore
        this.phpcode <| fun () ->
            context.writei ("if ((!is_file(" + path.code + ") && !is_link(" + path.code + ")) || !@unlink(" + path.code + ")) {")
            context.writei "throw new \\RuntimeException('Failed to delete the file.');"
            context.writei "}"
    member this.deleteFile(path:string) = this.deleteFile(PHPdata path)

    /// Treats an absent path as success and reports whether an existing path was deleted.
    member this.tryDeleteFile(path:PHPdata) =
        boolean
            ("((!file_exists(" + path.code + ") && !is_link(" + path.code + ")) || @unlink(" + path.code + "))")
            [path.Context]
    member this.tryDeleteFile(path:string) = this.tryDeleteFile(PHPdata path)
    member this.shuffle(data:PHPdata) = this.phpcode <| fun () -> context.writei("shuffle("+data.code+");")
    member this.setTimeZone(location:PHPdata) =
        merge [location.Context] |> ignore
        this.phpcode <| fun () -> context.writei("date_default_timezone_set("+location.code+");")
    member this.setTimeZone(location:string) = this.setTimeZone(PHPdata location)
    member this.sendMail(body:PHPdata,subject:PHPdata,fromAddress:PHPdata,toAddress:PHPdata) =
        merge [body.Context; subject.Context; fromAddress.Context; toAddress.Context] |> ignore
        this.phpcode <| fun () ->
            context.writei "(function ($body, $subject, $fromAddress, $toAddress): void {"
            context.writei "if (!is_string($body) || !is_string($subject) || !is_string($fromAddress) || !is_string($toAddress)) {"
            context.writei "throw new \\InvalidArgumentException('Mail arguments must be strings.');"
            context.writei "}"
            context.writei "if (preg_match('/[\\r\\n]/', $subject) === 1) {"
            context.writei "throw new \\InvalidArgumentException('The mail subject must not contain CR or LF characters.');"
            context.writei "}"
            context.writei "if (filter_var($fromAddress, FILTER_VALIDATE_EMAIL) === false) {"
            context.writei "throw new \\InvalidArgumentException('Invalid sender email address.');"
            context.writei "}"
            context.writei "if (filter_var($toAddress, FILTER_VALIDATE_EMAIL) === false) {"
            context.writei "throw new \\InvalidArgumentException('Invalid recipient email address.');"
            context.writei "}"
            context.writei "mb_language(\"ja\");"
            context.writei "mb_internal_encoding(\"UTF-8\");"
            context.writei "if (!mb_send_mail($toAddress, $subject, $body, ['From' => $fromAddress])) {"
            context.writei "throw new \\RuntimeException('Failed to send the mail.');"
            context.writei "}"
            context.writei ("})("+body.code+", "+subject.code+", "+fromAddress.code+", "+toAddress.code+");")
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
            context.writei "$remainingBody = $body;"
            context.writei "while ($remainingBody !== '') {"
            context.writei "$written = fwrite($pipes[0], $remainingBody);"
            context.writei "if ($written === false || $written === 0) {"
            context.writei "throw new \\RuntimeException('Failed to write the complete mail body.');"
            context.writei "}"
            context.writei "$remainingBody = (string)substr($remainingBody, $written);"
            context.writei "}"
            context.writei "if (!fclose($pipes[0])) { throw new \\RuntimeException('Failed to close the mail input pipe.'); }"
            context.writei "$pipes[0] = null;"
            context.writei "$stdout = stream_get_contents($pipes[1]);"
            context.writei "if ($stdout === false) { throw new \\RuntimeException('Failed to read the mail output pipe.'); }"
            context.writei "if (!fclose($pipes[1])) { throw new \\RuntimeException('Failed to close the mail output pipe.'); }"
            context.writei "$pipes[1] = null;"
            context.writei "$stderr = stream_get_contents($pipes[2]);"
            context.writei "if ($stderr === false) { throw new \\RuntimeException('Failed to read the mail error pipe.'); }"
            context.writei "if (!fclose($pipes[2])) { throw new \\RuntimeException('Failed to close the mail error pipe.'); }"
            context.writei "$pipes[2] = null;"
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
    member this.str_replace(strfrom:PHPdata,strto:PHPdata,str:PHPdata) =
        data ("str_replace("+strfrom.code+","+strto.code+","+str.code+")") [strfrom.Context;strto.Context;str.Context]
    member this.str_replace(strfrom:string,strto:string,str:PHPdata) =
        this.str_replace(PHPdata strfrom,PHPdata strto,str)
    member this.str_pad(num:PHPdata,ndigit:int,paddingnum:int) = data ("str_pad("+num.code+","+ndigit.ToString()+","+paddingnum.ToString()+", STR_PAD_LEFT)") [num.Context]
    /// Downloads a server-side file while presenting a separate, safe client file name.
    member this.file_download(file:PHPdata,downloadName:PHPdata) =
        Aqualis.mergeMany [context; file.Context; downloadName.Context] |> ignore
        this.phpcode <| fun () ->
            context.writei "(function ($file, $requestedName): void {"
            context.writei "if (!is_string($file) || !is_file($file) || !is_readable($file)) {"
            context.writei "throw new \\RuntimeException('The download file is unavailable.');"
            context.writei "}"
            context.writei "$downloadName = basename(str_replace('\\\\', '/', (string)$requestedName));"
            context.writei "if ($downloadName === '' || str_contains($downloadName, \"\\r\") || str_contains($downloadName, \"\\n\")) {"
            context.writei "throw new \\InvalidArgumentException('Invalid download file name.');"
            context.writei "}"
            context.writei "$fallbackName = preg_replace('/[^A-Za-z0-9._-]/', '_', $downloadName);"
            context.writei "if (!is_string($fallbackName) || $fallbackName === '') { $fallbackName = 'download'; }"
            context.writei "$fileSize = filesize($file);"
            context.writei "if ($fileSize === false) {"
            context.writei "throw new \\RuntimeException('Failed to determine the download file size.');"
            context.writei "}"
            context.writei "header('Content-Type: application/octet-stream');"
            context.writei "header('Content-Transfer-Encoding: Binary');"
            context.writei "header('X-Content-Type-Options: nosniff');"
            context.writei "header('Content-Disposition: attachment; filename=\"' . $fallbackName . '\"; filename*=UTF-8\\'\\'' . rawurlencode($downloadName));"
            context.writei "header('Content-Length: ' . $fileSize);"
            context.writei "while (ob_get_level()) { ob_end_clean(); }"
            context.writei "if (readfile($file) === false) {"
            context.writei "throw new \\RuntimeException('Failed to read the download file.');"
            context.writei "}"
            context.writei "exit;"
            context.writei ("})(" + file.code + ", " + downloadName.code + ");")

    member this.file_download(file:PHPdata,downloadName:string) =
        this.file_download(file,PHPdata downloadName)

    member this.file_download(file:PHPdata) =
        let inferredDownloadName:PHPdata = this.basename(file)
        this.file_download(file,inferredDownloadName)
    member this.basename(file:PHPdata) = data ("basename("+file.code+")") [file.Context]
    member this.br = "\n"
    member this.tb = "\t"

[<AutoOpen>]
module num0ForPHP =

    type int0 with
        member this.phpdata with get() = PHPdata([RNvr(this.Expr,this.Context)], this.Context)

    type double0 with
        member this.phpdata with get() = PHPdata([RNvr(this.Expr,this.Context)], this.Context)

    type complex0 with
        member this.phpdata with get() = PHPdata([RNvr(this.Expr,this.Context)], this.Context)

    type html with
        member this.h1 (t:PHPdata) = fun code ->
            this.tagb "h1" <| fun () -> ContextPhp(this.Context).echoHtmlText t
            code()
        member this.h2 (t:PHPdata) = fun code ->
            this.tagb "h2" <| fun () -> ContextPhp(this.Context).echoHtmlText t
            code()
        member this.h3 (t:PHPdata) = fun code ->
            this.tagb "h3" <| fun () -> ContextPhp(this.Context).echoHtmlText t
            code()
        member this.h4 (t:PHPdata) = fun code ->
            this.tagb "h4" <| fun () -> ContextPhp(this.Context).echoHtmlText t
            code()

    type Aqualis with
        ///<summary>PHPコード生成</summary>
        member this.php = ContextPhp(this)
