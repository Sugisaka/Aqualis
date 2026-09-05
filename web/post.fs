// 
// Copyright (c) 2026 Jun-ichiro Sugisaka
// 
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
// 
namespace Aqualis

open System
open System.Security.Cryptography
open System.Text

type post(context:Aqualis,id:PHPdata) =
    new(ctx:Aqualis,x:string) = post(ctx,PHPdata ([RStr x],Aqualis.BlankWriter PHP))
    new(ctx:Aqualis,x:int0) = post(ctx,PHPdata([RNvr(x.Expr,x.Context)], x.Context))
    member _.get with get() = PHPdata.f(context,"$_POST["+id.toString(".",StrQuotation)+"]")
    member this.get_html with get() = PHPdata.f(context,"htmlspecialchars(" + this.get.code + ",ENT_QUOTES)")
    ///テキストボックス
    member _.input() =
        context.html.taga(
            "input",
            [
                "type", PHPdata "text"
                "name", id
                "value", PHPdata ""
            ]
        )
    member _.input(a:list<Atr>) =
        context.html.taga(
            "input",
            [
                "type", PHPdata "text"
                "name", id
                "value", PHPdata ""
            ]@(a |> List.map (fun (p:Atr) -> p.name,PHPdata p.value))
        )

    ///パスワード入力テキストボックス
    member _.password() = 
        context.html.taga(
            "input",
            [
                "type", PHPdata "password"
                "name", id
            ]
        )
    ///テキストボックス
    member _.input(value:PHPdata) =
        context.html.taga(
            "input",
            [
                "type", PHPdata "text"
                "name", id
                "value", value
            ]
        )
    ///テキストボックス
    member _.input_hidden(value:PHPdata) =
        context.html.taga(
            "input",
            [
                "type", PHPdata "hidden"
                "name", id
                "value", value
            ]
        )
    ///テキストボックス
    member _.input(value:string) =
        context.html.taga(
            "input",
            [
                "type", PHPdata "text"
                "name", id
                "value", PHPdata value
            ]
        )
    ///テキストボックス
    member _.input_hidden(value:string) =
        context.html.taga(
            "input",
            [
                "type", PHPdata "hidden"
                "name", id
                "value", PHPdata value
            ]
        )

    member _.input(value:PHPdata,a:list<Atr>) =
        context.html.taga(
            "input",
            [
                "type", PHPdata "text"
                "name", id
                "value", value
            ]@(a |> List.map (fun p -> p.name,PHPdata p.value))
        )
    member _.input_hidden(value:PHPdata,a:list<Atr>) =
        context.html.taga(
            "input",
            [
                "type", PHPdata "hidden"
                "name", id
                "value", value
            ]@(a |> List.map (fun p -> p.name,PHPdata p.value))
        )

    member _.input(value:string,a:list<Atr>) =
        context.html.taga(
            "input",
            [
                "type", PHPdata "text"
                "name", id
                "value", PHPdata value
            ]@(a |> List.map (fun p -> p.name,PHPdata p.value))
        )

    member _.input_hidden(value:string,a:list<Atr>) =
        context.html.taga(
            "input",
            [
                "type", PHPdata "hidden"
                "name", id
                "value", PHPdata value
            ]@(a |> List.map (fun p -> p.name,PHPdata p.value))
        )

    member _.textArea() =
        context.html.tagb(
            "textarea",
            [
                "type", PHPdata "text"
                "name", id
            ]
        ) <| fun () -> ()
    member _.textArea code =
        context.html.tagb(
            "textarea",
            [
                "type", PHPdata "text"
                "name", id
            ]
        ) <| fun () -> code()
    member _.textArea(a:list<Atr>) = 
        context.html.tagb0(
            "textarea",
            [
                "type", PHPdata "text"
                "name", id
            ]@(a |> List.map (fun p -> p.name,PHPdata p.value))
        ) <| fun () -> ()
    member _.textArea_contents(a:list<Atr>) = fun code ->
        context.html.tagb0(
            "textarea",
            [
                "type", PHPdata "text"
                "name", id
            ]@(a |> List.map (fun p -> p.name,PHPdata p.value))
        ) code
    member this.textArea_copy() =
        context.html.tagb(
            "textarea",
            [
                "type", PHPdata "text"
                "name", id
            ]
        ) <| fun () -> context.writein this.get_html.phpcode
    member this.textArea_copy(a:list<Atr>) =
        context.html.tagb(
            "textarea",
            [
                "type", PHPdata "text"
                "name", id
            ]@(a |> List.map (fun p -> p.name,PHPdata p.value))
        ) <| fun () -> context.writein this.get_html.phpcode
    member _.textArea(value:string) =
        context.html.tagb(
            "textarea",
            [
                "type", PHPdata "text"
                "name", id
            ]
        ) <| fun () -> context.writein value
    member _.input_lock(value:PHPdata) =
        context.html.taga(
            "input",
            [
                "type", PHPdata "text"
                "name", id
                "readonly", PHPdata "readonly"
                "value", value
            ]
        )
    member _.input_lock(value:string) =
        context.html.taga(
            "input",
            [
                "type", PHPdata "text"
                "name", id
                "readonly", PHPdata "readonly"
                "value", PHPdata value
            ]
        )
    member _.input_lock(value:PHPdata,a:list<Atr>) =
        context.html.taga(
            "input",
            [
                "type", PHPdata "text"
                "name", id
                "readonly",PHPdata "readonly"
                "value", value
            ]@(a |> List.map (fun p -> p.name,PHPdata p.value))
        )
    member _.input_lock(value:string,a:list<Atr>) =
        context.html.taga(
            "input",
            [
                "type", PHPdata "text"
                "name", id
                "readonly",PHPdata "readonly"
                "value", PHPdata value
            ]@(a |> List.map (fun p -> p.name,PHPdata p.value))
        )

    ///テキストボックス
    member _.input(value:int0) =
        context.html.taga(
            "input",
            [
                "type", PHPdata "text"
                "name", id
                "value", PHPdata value
            ]
        )
    member _.input(value:int0,a:list<Atr>) =
        context.html.taga(
            "input",
            [
                "type", PHPdata "text"
                "name", id
                "value", PHPdata value
            ]@(a |> List.map (fun p -> p.name,PHPdata p.value))
        )

    ///テキストボックス
    member _.input_hidden(value:int0) =
        context.html.taga(
            "input",
            [
                "type", PHPdata "hidden"
                "name", id
                "value", PHPdata value
            ]
        )
    member _.input_hidden(value:int0,a:list<Atr>) =
        context.html.taga(
            "input",
            [
                "type", PHPdata "hidden"
                "name", id
                "value", PHPdata value
            ]@(a |> List.map (fun p -> p.name,PHPdata p.value))
        )

    member _.input_lock(value:int0) =
        context.html.taga(
            "input",
            [
                "type", PHPdata "text"
                "name", id
                "readonly",PHPdata "readonly"
                "value", PHPdata value
            ]
        )
    member _.input_lock(value:int0,a:list<Atr>) =
        context.html.taga(
            "input",
            [
                "type", PHPdata "text"
                "name", id
                "readonly",PHPdata "readonly"
                "value", PHPdata value
            ]@(a |> List.map (fun p -> p.name,PHPdata p.value))
        )
        
    ///パスワード入力テキストボックス
    member _.password(value:int0) = 
        context.html.taga(
            "input",
            [
                "type", PHPdata "password"
                "name", id
                "value", PHPdata value
            ]
        )

    ///テキストボックス
    member _.input(value:double0) =
        context.html.taga(
            "input",
            [
                "type", PHPdata "text"
                "name", id
                "value", PHPdata value
            ]
        )
    member _.input(value:double0,a:list<Atr>) =
        context.html.taga(
            "input",
            [
                "type", PHPdata "text"
                "name", id
                "value", PHPdata value
            ]@(a |> List.map (fun p -> p.name,PHPdata p.value))
        )

    ///テキストボックス
    member _.input_hidden(value:double0) =
        context.html.taga(
            "input",
            [
                "type", PHPdata "hidden"
                "name", id
                "value", PHPdata value
            ]
        )
    member _.input_hidden(value:double0,a:list<Atr>) =
        context.html.taga(
            "input",
            [
                "type", PHPdata "hidden"
                "name", id
                "value", PHPdata value
            ]@(a |> List.map (fun p -> p.name,PHPdata p.value))
        )

    member _.input_lock(value:double0) =
        context.html.taga(
            "input",
            [
                "type", PHPdata "text"
                "name", id
                "readonly",PHPdata "readonly"
                "value", PHPdata value
            ]
        )
    member _.input_lock(value:double0,a:list<Atr>) =
        context.html.taga(
            "input",
            [
                "type", PHPdata "text"
                "name", id
                "readonly",PHPdata "readonly"
                "value", PHPdata value
            ]@(a |> List.map (fun p -> p.name,PHPdata p.value))
        )

    ///パスワード入力テキストボックス
    member _.password(value:string) =
        context.html.taga(
            "input",
            [
                "type", PHPdata "password"
                "name", id
                "value", PHPdata value
            ]
        )
    ///パスワード入力テキストボックス
    member _.password(value:double0) = 
        context.html.taga(
            "input",
            [
                "type", PHPdata "password"
                "name", id
                "value", PHPdata value
            ]
        )

    ///テキストボックス（送信済みのメッセージを表示）
    member this.input_copy() = 
        context.html.taga(
            "input",
            [
                "type", PHPdata "text"
                "name", id
                "value", this.get
            ]
        )
    member this.input_copy(a:list<Atr>) = 
        context.html.taga(
            "input",
            [
                "type", PHPdata "text"
                "name", id
                "value", this.get
            ]@(a |> List.map (fun p -> p.name,PHPdata p.value))
        )
    ///テキストボックス（送信済みのメッセージを表示）
    member this.input_copy_hidden() = 
        context.html.taga(
            "input",
            [
                "type", PHPdata "hidden"
                "name", id
                "value", this.get
            ]
        )
    member this.input_copy_hidden(a:list<Atr>) = 
        context.html.taga(
            "input",
            [
                "type", PHPdata "hidden"
                "name", id
                "value", this.get
            ]@(a |> List.map (fun p -> p.name,PHPdata p.value))
        )
    member this.input_copy_lock() = 
        context.html.taga(
            "input",
            [
                "type", PHPdata "text"
                "name", id
                "readonly", PHPdata "readonly"
                "value", this.get
            ]
        )
    member this.input_copy_lock(a:list<Atr>) = 
        context.html.taga(
            "input",
            [
                "type", PHPdata "text"
                "name", id
                "readonly", PHPdata "readonly"
                "value", this.get
            ]@(a |> List.map (fun p -> p.name,PHPdata p.value))
        )
    ///パスワード入力テキストボックス（送信済みのメッセージを表示）
    member this.password_copy() = 
        context.html.taga(
            "input",
            [
                "type", PHPdata "password"
                "name",id
                "value", this.get
            ]
        )
    member this.password_copy_lock() =
        context.html.taga(
            "input",
            [
                "type", PHPdata "password"
                "name", id
                "readonly", PHPdata "readonly"
                "value", this.get
            ]
        )
    member _.submit(value:string) =
        context.html.taga(
            "input",
            [
                "type", PHPdata "submit"
                "name", id
                "value",PHPdata value
            ]
        )
    member _.submit(url:string,value:string) =
        context.html.taga(
            "input",
            [
                "type", PHPdata "submit"
                "name", id
                "value", PHPdata value
                "formaction", PHPdata url
            ]
        )
    member _.submit(url:string,value:string,style:string) =
        context.html.taga("input",
            [
                "type", PHPdata "submit"
                "name", id
                "class", PHPdata style
                "value", PHPdata value
                "formaction", PHPdata url
            ]
        )
    member _.select code = 
        context.html.tagb (
            "select",
            [
                "name",id
            ]
        ) code
    
/// Controls validation and storage of an uploaded file.
type UploadPolicy = {
    /// Directory relative to the generated PHP script's directory.
    DestinationDirectory:string
    /// Maximum accepted file size in bytes.
    MaxBytes:int64
    /// Maximum number of files accepted by a multiple-file upload.
    MaxFiles:int
    /// Allowed MIME type and server-controlled extension pairs.
    AllowedMimeTypes:(string*string) list
    /// Number of cryptographically random bytes used in stored file names.
    RandomNameBytes:int }

[<RequireQualifiedAccess>]
module UploadPolicy =
    /// Creates a policy for private, non-executable uploads.
    let create destinationDirectory maxBytes allowedMimeTypes = {
        DestinationDirectory = destinationDirectory
        MaxBytes = maxBytes
        MaxFiles = 10
        AllowedMimeTypes = allowedMimeTypes
        RandomNameBytes = 16 }

/// Values produced by a validated multiple-file upload.
type UploadedFiles = {
    /// One result record for every submitted file, including failures.
    Results:PHPdata
    /// Result records for files that were successfully stored.
    Successful:PHPdata
    /// Server-generated names for successfully stored files.
    StoredNames:PHPdata
    /// Client-provided base names for successfully stored files.
    OriginalNames:PHPdata
    /// Error messages for files that could not be stored.
    Errors:PHPdata
    /// True only when every submitted file was stored successfully.
    AllSucceeded:bool0 }

module private UploadGeneration =
    let phpString (value:string) =
        if isNull value then nullArg (nameof value)
        if value.IndexOf '\u0000' >= 0 then
            invalidArg (nameof value) "A PHP string literal cannot contain NUL."
        "'" +
        value
            .Replace("\\", "\\\\")
            .Replace("'", "\\'")
            .Replace("\r", "\\r")
            .Replace("\n", "\\n") +
        "'"

    let validatePolicy policy =
        if String.IsNullOrWhiteSpace policy.DestinationDirectory then
            invalidArg "policy" "The upload destination directory cannot be empty."
        if policy.MaxBytes <= 0L then
            invalidArg "policy" "The maximum upload size must be positive."
        if policy.MaxFiles <= 0 then
            invalidArg "policy" "The maximum upload file count must be positive."
        if policy.RandomNameBytes < 16 then
            invalidArg "policy" "At least 16 random bytes are required for stored file names."
        if List.isEmpty policy.AllowedMimeTypes then
            invalidArg "policy" "At least one allowed MIME type is required."
        for mimeType,extension in policy.AllowedMimeTypes do
            if String.IsNullOrWhiteSpace mimeType then
                invalidArg "policy" "An allowed MIME type cannot be empty."
            if
                String.IsNullOrWhiteSpace extension ||
                extension.Length > 16 ||
                extension |> Seq.exists (Char.IsLetterOrDigit >> not)
            then
                invalidArg "policy" "Stored file extensions must contain only letters and digits."
        let mimeTypes = policy.AllowedMimeTypes |> List.map fst
        if mimeTypes.Length <> (mimeTypes |> List.distinct).Length then
            invalidArg "policy" "Allowed MIME types must be unique."

    let prefix suffix (id:PHPdata) =
        let digest =
            id.code
            |> Encoding.UTF8.GetBytes
            |> SHA256.HashData
            |> Convert.ToHexString
        "$aqualis_upload_" + digest.Substring(0,12).ToLowerInvariant() + "_" + suffix

    let allowedTypes policy =
        policy.AllowedMimeTypes
        |> List.map (fun (mimeType,extension) ->
            phpString mimeType + " => " + phpString (extension.ToLowerInvariant()))
        |> String.concat ", "
        |> fun values -> "[" + values + "]"

    let emitSaveFunction (context:Aqualis) functionName policy =
        validatePolicy policy
        let lines = [
            functionName + " = static function (array $upload): array {"
            "if (!array_key_exists('error', $upload) || is_array($upload['error'])) {"
            "throw new \\RuntimeException('Invalid upload data.');"
            "}"
            "if ((int)$upload['error'] !== UPLOAD_ERR_OK) {"
            "throw new \\RuntimeException('Upload failed with error code '.(int)$upload['error'].'.');"
            "}"
            "if (!isset($upload['size']) || (int)$upload['size'] < 0 || (int)$upload['size'] > " + string policy.MaxBytes + ") {"
            "throw new \\RuntimeException('The uploaded file size is not allowed.');"
            "}"
            "if (!isset($upload['tmp_name']) || !is_string($upload['tmp_name']) || !is_uploaded_file($upload['tmp_name'])) {"
            "throw new \\RuntimeException('The file is not a valid HTTP upload.');"
            "}"
            "$finfo = new \\finfo(FILEINFO_MIME_TYPE);"
            "$mimeType = $finfo->file($upload['tmp_name']);"
            "$allowedTypes = " + allowedTypes policy + ";"
            "if (!is_string($mimeType) || !isset($allowedTypes[$mimeType])) {"
            "throw new \\RuntimeException('The uploaded file type is not allowed.');"
            "}"
            "$uploadRoot = realpath(__DIR__.DIRECTORY_SEPARATOR." + phpString policy.DestinationDirectory + ");"
            "if ($uploadRoot === false || !is_dir($uploadRoot) || !is_writable($uploadRoot)) {"
            "throw new \\RuntimeException('The upload directory is unavailable.');"
            "}"
            "do {"
            "$storedName = bin2hex(random_bytes(" + string policy.RandomNameBytes + ")).'.'.$allowedTypes[$mimeType];"
            "$destination = $uploadRoot.DIRECTORY_SEPARATOR.$storedName;"
            "} while (file_exists($destination));"
            "if (!move_uploaded_file($upload['tmp_name'], $destination)) {"
            "throw new \\RuntimeException('Failed to store the uploaded file.');"
            "}"
            "return ["
            "'stored_name' => $storedName,"
            "'original_name' => basename((string)($upload['name'] ?? '')) ,"
            "'mime_type' => $mimeType,"
            "'size' => (int)$upload['size'],"
            "];"
            "};" ]
        context.php.phpcode <| fun () ->
            lines |> List.iter context.writein

[<RequireQualifiedAccess>]
type UploadCardinality =
    | Single
    | Multiple

/// A single- or multiple-file upload field with one consistent API.
type postFile private (context:Aqualis,id:PHPdata,cardinality:UploadCardinality) =
    let mutable saveEmitted = false
    let fileExpression = "$_FILES[" + id.toString(".",StrQuotation) + "]"

    let appendFailure prefix message =
        context.writein (
            "$result = ['success' => false, 'stored_name' => null, 'original_name' => '', " +
            "'mime_type' => null, 'size' => null, 'error' => '" + message + "'];")
        context.writein(prefix + "_results[] = $result;")
        context.writein(prefix + "_errors[] = $result['error'];")

    static member single(ctx:Aqualis,id:PHPdata) =
        postFile(ctx,id,UploadCardinality.Single)

    static member single(ctx:Aqualis,id:string) =
        postFile.single(ctx,PHPdata id)

    static member multiple(ctx:Aqualis,id:PHPdata) =
        postFile(ctx,id,UploadCardinality.Multiple)

    static member multiple(ctx:Aqualis,id:string) =
        postFile.multiple(ctx,PHPdata id)

    member _.cardinality = cardinality

    /// True when the request contains at least one selected file for this field.
    member _.isFileSpecified with get() =
        let expression =
            match cardinality with
            | UploadCardinality.Single ->
                "isset(" + fileExpression + "['error']) && " +
                "!is_array(" + fileExpression + "['error']) && " +
                "(int)" + fileExpression + "['error'] !== UPLOAD_ERR_NO_FILE"
            | UploadCardinality.Multiple ->
                "isset(" + fileExpression + "['error'][0]) && " +
                "(int)" + fileExpression + "['error'][0] !== UPLOAD_ERR_NO_FILE"
        bool0(Var(Nt,"(" + expression + ")",NaN),context)

    /// Emits the file input. The surrounding form must use multipart/form-data.
    member _.select() =
        let attributes =
            match cardinality with
            | UploadCardinality.Single ->
                ["name",id; "type",PHPdata "file"]
            | UploadCardinality.Multiple ->
                ["multiple",PHPdata "multiple"; "name",id++"[]"; "type",PHPdata "file"]
        context.html.taga("input",attributes)

    /// Emits a standalone multipart form containing this field and a submit button.
    member this.select(actionPhpFile:string) =
        context.html.tagb (
            "form",
            [Atr("action",actionPhpFile); Atr("enctype","multipart/form-data"); Atr("method","post")])
        <| fun () ->
            this.select()
            context.html.taga("input",[Atr("type","submit"); Atr("value","アップロード")])

    /// Validates and stores the upload. Single uploads are returned as a one-item result collection.
    member _.save(policy:UploadPolicy) =
        if saveEmitted then
            invalidOp (
                "save was called more than once for upload field '" + id.code + "'. " +
                "Store and reuse the first result instead.")

        let mode =
            match cardinality with
            | UploadCardinality.Single -> "single"
            | UploadCardinality.Multiple -> "many"
        let prefix = UploadGeneration.prefix mode id
        let saveFunction = prefix + "_save_one"
        let processFunction = prefix + "_process_one"
        UploadGeneration.emitSaveFunction context saveFunction policy
        context.php.phpcode <| fun () ->
            context.writein(prefix + "_results = [];")
            context.writein(prefix + "_successful = [];")
            context.writein(prefix + "_stored_names = [];")
            context.writein(prefix + "_original_names = [];")
            context.writein(prefix + "_errors = [];")
            context.writein(
                processFunction + " = static function (array $upload) use (" +
                saveFunction + ", &" + prefix + "_results, &" + prefix + "_successful, &" +
                prefix + "_stored_names, &" + prefix + "_original_names, &" + prefix + "_errors): void {")
            context.writein "try {"
            context.writein("$result = " + saveFunction + "($upload);")
            context.writein "$result['success'] = true;"
            context.writein "$result['error'] = null;"
            context.writein(prefix + "_successful[] = $result;")
            context.writein(prefix + "_stored_names[] = $result['stored_name'];")
            context.writein(prefix + "_original_names[] = $result['original_name'];")
            context.writein "} catch (\\Throwable $exception) {"
            context.writein "$result = ["
            context.writein "'success' => false,"
            context.writein "'stored_name' => null,"
            context.writein "'original_name' => basename((string)($upload['name'] ?? '')),"
            context.writein "'mime_type' => null,"
            context.writein "'size' => isset($upload['size']) ? (int)$upload['size'] : null,"
            context.writein "'error' => $exception->getMessage(),"
            context.writein "];"
            context.writein(prefix + "_errors[] = $result['error'];")
            context.writein "}"
            context.writein(prefix + "_results[] = $result;")
            context.writein "};"

            match cardinality with
            | UploadCardinality.Single ->
                context.writein(
                    "if (!isset(" + fileExpression + ") || !is_array(" + fileExpression + ") || " +
                    "!array_key_exists('error', " + fileExpression + ") || is_array(" + fileExpression + "['error'])) {")
                appendFailure prefix "Invalid single-file upload data."
                context.writein "} else {"
                context.writein(processFunction + "(" + fileExpression + ");")
                context.writein "}"
            | UploadCardinality.Multiple ->
                context.writein("if (!isset(" + fileExpression + "['error']) || !is_array(" + fileExpression + "['error'])) {")
                appendFailure prefix "Invalid multiple-file upload data."
                context.writein("} elseif (count(" + fileExpression + "['error']) > " + string policy.MaxFiles + ") {")
                appendFailure prefix "Too many uploaded files."
                context.writein "} else {"
                context.writein("foreach (" + fileExpression + "['error'] as $index => $uploadError) {")
                context.writein "$upload = ["
                context.writein("'name' => " + fileExpression + "['name'][$index] ?? '',")
                context.writein("'tmp_name' => " + fileExpression + "['tmp_name'][$index] ?? '',")
                context.writein "'error' => $uploadError,"
                context.writein("'size' => " + fileExpression + "['size'][$index] ?? -1,")
                context.writein "];"
                context.writein(processFunction + "($upload);")
                context.writein "}"
                context.writein "}"

        let data suffix = PHPdata.f(context,prefix + suffix)
        let result = {
            Results = data "_results"
            Successful = data "_successful"
            StoredNames = data "_stored_names"
            OriginalNames = data "_original_names"
            Errors = data "_errors"
            AllSucceeded = bool0(Var(Nt,"count(" + prefix + "_errors) === 0",NaN),context) }
        saveEmitted <- true
        result

    /// Generates storage and consumes its result at the same code-generation location.
    member this.saveWith(policy:UploadPolicy) = fun (code:UploadedFiles -> unit) ->
        if isNull (box code) then nullArg (nameof code)
        let result = this.save(policy)
        code result
