namespace Aqualis.Tests

open System
open System.IO
open Xunit
open Aqualis

module PhpUploadGenerationTests =
    let private policy =
        UploadPolicy.create
            "../private-uploads"
            5_000_000L
            ["image/jpeg", "jpg"; "image/png", "png"]

    let private generate code =
        use output = new TemporaryDirectory()
        Compile [PHP] output.Path "upload" "1.0" code
        File.ReadAllText(Path.Combine(output.Path, "upload.php"))

    let private storageLines (source:string) =
        source.Split('\n')
        |> Array.filter (fun line ->
            line.Contains("$destination =") ||
            line.Contains("move_uploaded_file("))

    [<Fact>]
    let ``single upload uses a server generated file name`` () =
        let source =
            generate (fun context ->
                let upload = postFile.single(context, "avatar")
                upload.save(policy) |> ignore)

        Assert.Contains("new \\finfo(FILEINFO_MIME_TYPE)", source)
        Assert.Contains("is_uploaded_file($upload['tmp_name'])", source)
        Assert.Contains("bin2hex(random_bytes(16))", source)
        Assert.Contains("'image/jpeg' => 'jpg'", source)
        Assert.Contains("(int)$upload['size'] > 5000000", source)
        Assert.Contains(
            "realpath(__DIR__.DIRECTORY_SEPARATOR.'../private-uploads')",
            source)
        Assert.Contains("basename((string)($upload['name'] ?? ''))", source)

        let storage = storageLines source
        Assert.NotEmpty(storage)
        storage |> Array.iter (fun line ->
            Assert.DoesNotContain("['name']", line)
            Assert.DoesNotContain("$_FILES", line))

    [<Fact>]
    let ``multiple upload validates every item with the shared save function`` () =
        let source =
            generate (fun context ->
                let upload = postFile.multiple(context, "attachments")
                upload.save(policy) |> ignore)

        Assert.Contains("foreach ($_FILES[\"attachments\"]['error'] as $index => $uploadError)", source)
        Assert.Contains("$result['success'] = true", source)
        Assert.Contains("'stored_name' => $storedName", source)
        Assert.Contains("bin2hex(random_bytes(16))", source)
        Assert.Contains("count($_FILES[\"attachments\"]['error']) > 10", source)

        storageLines source |> Array.iter (fun line ->
            Assert.DoesNotContain("['name']", line)
            Assert.DoesNotContain("$_FILES", line))

    [<Fact>]
    let ``multiple upload exposes stored and original names for JSON persistence`` () =
        let source =
            generate (fun context ->
                let upload = postFile.multiple(context, "attachments")
                let result = upload.save(policy)
                let issue = context.php.array("issue")
                issue["Files"] <== result.Successful
                issue["StoredNames"] <== result.StoredNames
                issue["OriginalNames"] <== result.OriginalNames)

        Assert.Contains("_successful[] = $result", source)
        Assert.Contains("_stored_names[] = $result['stored_name']", source)
        Assert.Contains("_original_names[] = $result['original_name']", source)
        Assert.Contains("$issue[\"Files\"] = $aqualis_upload_", source)

    [<Fact>]
    let ``repeated save calls fail instead of reusing branch local variables`` () =
        use output = new TemporaryDirectory()
        use context = new Aqualis(Some output.Path, Some "duplicate.php", PHP)
        let upload = postFile.multiple(context, "attachments")
        upload.save(policy) |> ignore

        let error =
            Assert.Throws<InvalidOperationException>(fun () ->
                upload.save(policy) |> ignore)

        Assert.Contains("Store and reuse the first result instead", error.Message)

    [<Fact>]
    let ``callback API initializes results before consuming them`` () =
        let source =
            generate (fun context ->
                let upload = postFile.multiple(context, "attachments")
                upload.saveWith policy <| fun result ->
                        context.br.if1 result.AllSucceeded <| fun () ->
                            context.php.echo "stored")

        let initialization = source.IndexOf("_errors = [];", StringComparison.Ordinal)
        let consumption = source.IndexOf("if(count($aqualis_upload_", StringComparison.Ordinal)
        Assert.True(initialization >= 0)
        Assert.True(consumption > initialization)

    [<Fact>]
    let ``single upload also returns the unified result collection`` () =
        let source =
            generate (fun context ->
                let upload = postFile.single(context, "avatar")
                let result = upload.save(policy)
                let storedNames = context.php.array("storedNames")
                storedNames <== result.StoredNames)

        Assert.Contains("$_FILES[\"avatar\"]", source)
        Assert.Contains("_stored_names[] = $result['stored_name']", source)
        Assert.DoesNotContain("$_FILES[\"avatar\"]['error'] as $index", source)

    [<Fact>]
    let ``independent multiple fields retain their own input names`` () =
        let source =
            generate (fun context ->
                let newFiles = postFile.multiple(context, "newfiles")
                let commentFiles = postFile.multiple(context, "comfiles")
                newFiles.save(policy) |> ignore
                commentFiles.save(policy) |> ignore)

        Assert.Contains("foreach ($_FILES[\"newfiles\"]['error']", source)
        Assert.Contains("foreach ($_FILES[\"comfiles\"]['error']", source)

    [<Fact>]
    let ``one postFile type supports an explicit multiple workflow`` () =
        let source =
            generate (fun context ->
                let uploads = postFile.multiple(context, "attachments")
                uploads.select()
                uploads.saveWith policy <| fun result ->
                        let storedNames = context.php.array("storedNames")
                        storedNames <== result.StoredNames)

        Assert.Contains("name=\"<?php echo htmlspecialchars((string)(\"attachments\".\"[]\")", source)
        Assert.Contains("foreach ($_FILES[\"attachments\"]['error']", source)
        Assert.DoesNotContain("_single_", source)

    [<Fact>]
    let ``upload form emits valid name and multiple attributes`` () =
        let source =
            generate (fun context ->
                let singleUpload = postFile.single(context, "avatar")
                let multipleUpload = postFile.multiple(context, "documents")
                singleUpload.select()
                multipleUpload.select("receive.php"))

        Assert.DoesNotContain("<input input name", source)
        Assert.Contains("name=\"<?php echo htmlspecialchars", source)
        Assert.Contains("multiple=\"<?php echo htmlspecialchars", source)
        Assert.Contains("ENT_QUOTES | ENT_SUBSTITUTE, 'UTF-8'", source)
        Assert.Contains("enctype=\"multipart/form-data\"", source)

    [<Fact>]
    let ``upload policy rejects unsafe configuration`` () =
        use output = new TemporaryDirectory()
        use context = new Aqualis(Some output.Path, Some "invalid.php", PHP)

        let assertInvalid policy =
            Assert.Throws<ArgumentException>(fun () ->
                let upload = postFile.single(context, "file")
                upload.save(policy) |> ignore)
            |> ignore

        assertInvalid { policy with MaxBytes = 0L }
        assertInvalid { policy with MaxFiles = 0 }
        assertInvalid { policy with RandomNameBytes = 8 }
        assertInvalid { policy with AllowedMimeTypes = [] }
        assertInvalid {
            policy with
                AllowedMimeTypes = ["application/x-httpd-php", "php/../"] }
