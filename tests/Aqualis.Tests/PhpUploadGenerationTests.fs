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
                let upload = postFile(context, "avatar")
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
                let upload = postFile(context, "attachments")
                upload.saveMany(policy) |> ignore)

        Assert.Contains("foreach ($_FILES[\"attachments\"]['error'] as $index => $uploadError)", source)
        Assert.Contains("$result['success'] = true", source)
        Assert.Contains("'stored_name' => $storedName", source)
        Assert.Contains("bin2hex(random_bytes(16))", source)
        Assert.Contains("count($_FILES[\"attachments\"]['error']) > 10", source)

        storageLines source |> Array.iter (fun line ->
            Assert.DoesNotContain("['name']", line)
            Assert.DoesNotContain("$_FILES", line))

    [<Fact>]
    let ``upload form emits valid name and multiple attributes`` () =
        let source =
            generate (fun context ->
                let upload = postFile(context, "documents")
                upload.file_select()
                upload.files_select("receive.php"))

        Assert.DoesNotContain("<input input name", source)
        Assert.Contains("name = <?php", source)
        Assert.Contains("multiple = <?php", source)
        Assert.Contains("enctype = \"multipart/form-data\"", source)

    [<Fact>]
    let ``upload policy rejects unsafe configuration`` () =
        use output = new TemporaryDirectory()
        use context = new Aqualis(Some output.Path, Some "invalid.php", PHP)
        let upload = postFile(context, "file")

        let assertInvalid policy =
            Assert.Throws<ArgumentException>(fun () ->
                upload.save(policy) |> ignore)
            |> ignore

        assertInvalid { policy with MaxBytes = 0L }
        assertInvalid { policy with MaxFiles = 0 }
        assertInvalid { policy with RandomNameBytes = 8 }
        assertInvalid { policy with AllowedMimeTypes = [] }
        assertInvalid {
            policy with
                AllowedMimeTypes = ["application/x-httpd-php", "php/../"] }
