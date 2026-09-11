namespace Aqualis.Tests

open System
open System.IO
open Xunit
open Aqualis

module SecurityGenerationTests =
    let private generate name code =
        use output = new TemporaryDirectory()
        let fileName = name + ".php"
        use context = new Aqualis(Some output.Path, Some fileName, PHP)
        code context
        context.close()
        File.ReadAllText(Path.Combine(output.Path, fileName))

    let private occurrences (needle:string) (value:string) =
        let rec count offset total =
            let found = value.IndexOf(needle, offset, StringComparison.Ordinal)
            if found < 0 then total
            else count (found + needle.Length) (total + 1)
        count 0 0

    [<Fact>]
    let ``session start emits secure cookie options before one guarded start`` () =
        let generated =
            generate "session-start" <| fun context ->
                context.php.session.Start SessionOptions.production
                context.php.session.Start SessionOptions.production

        Assert.Equal(1, occurrences "session_set_cookie_params(" generated)
        Assert.Equal(1, occurrences "session_start();" generated)
        Assert.Contains("'lifetime' => 0", generated)
        Assert.Contains("'path' => \"/\"", generated)
        Assert.Contains("'secure' => true", generated)
        Assert.Contains("'httponly' => true", generated)
        Assert.Contains("'samesite' => \"Lax\"", generated)
        Assert.Contains("session_status() !== PHP_SESSION_ACTIVE", generated)
        let cookieOptionsIndex = generated.IndexOf("session_set_cookie_params(", StringComparison.Ordinal)
        let sessionStartIndex = generated.IndexOf("session_start();", StringComparison.Ordinal)
        Assert.True(cookieOptionsIndex < sessionStartIndex)

    [<Fact>]
    let ``development session permits an HTTP cookie`` () =
        let generated =
            generate "development-session" <| fun context ->
                context.php.session.Start SessionOptions.development

        Assert.Contains("'secure' => false", generated)
        Assert.Contains("'httponly' => true", generated)

    [<Fact>]
    let ``session start rejects invalid and conflicting options`` () =
        use output = new TemporaryDirectory()
        use context = new Aqualis(Some output.Path, Some "invalid-session.php", PHP)

        Assert.Throws<ArgumentException>(fun () ->
            context.php.session.Start { SessionOptions.production with Lifetime = -1 })
        |> ignore

        context.php.session.Start SessionOptions.production
        Assert.Throws<InvalidOperationException>(fun () ->
            context.php.session.Start SessionOptions.development)
        |> ignore

    [<Fact>]
    let ``CSRF token and field are emitted once and HTML escaped at runtime`` () =
        let generated =
            generate "csrf-field" <| fun context ->
                let session = context.php.session
                let csrf = context.php.csrf
                session.Start SessionOptions.production
                csrf.EnsureToken()
                csrf.EnsureToken()
                context.html.formWithCsrf("save.php", csrf) ignore

        Assert.Equal(1, occurrences "bin2hex(random_bytes(32))" generated)
        Assert.Contains("!isset($_SESSION[\"_aqualis_csrf\"])", generated)
        Assert.Contains("name=\"<?php echo htmlspecialchars", generated)
        Assert.Contains("value=\"<?php echo htmlspecialchars((string)($_SESSION[\"_aqualis_csrf\"])", generated)
        Assert.Contains("ENT_QUOTES | ENT_SUBSTITUTE, 'UTF-8'", generated)
        let formIndex = generated.IndexOf("<form", StringComparison.Ordinal)
        let fieldIndex = generated.IndexOf("_aqualis_csrf", formIndex, StringComparison.Ordinal)
        Assert.True(formIndex < fieldIndex)

    [<Fact>]
    let ``CSRF validation rejects only invalid POST requests`` () =
        let generated =
            generate "csrf-validation" <| fun context ->
                context.php.session.Start SessionOptions.production
                let csrf = context.php.csrf
                csrf.EnsureToken()
                csrf.RequireValidPost()

        Assert.Contains("$_SERVER[\"REQUEST_METHOD\"] === 'POST'", generated)
        Assert.Contains("isset($_POST[\"_aqualis_csrf\"])", generated)
        Assert.Contains("is_string($_POST[\"_aqualis_csrf\"])", generated)
        Assert.Contains(
            "hash_equals($_SESSION[\"_aqualis_csrf\"], $_POST[\"_aqualis_csrf\"])",
            generated)
        Assert.Contains("http_response_code(403)", generated)
        Assert.Contains("exit('Invalid CSRF token.')", generated)

    [<Fact>]
    let ``CSRF operations require initialization in the safe order`` () =
        use output = new TemporaryDirectory()
        use context = new Aqualis(Some output.Path, Some "csrf-order.php", PHP)
        let csrf = context.php.csrf

        Assert.Throws<InvalidOperationException>(fun () -> csrf.EnsureToken()) |> ignore

        context.php.session.Start SessionOptions.production
        Assert.Throws<InvalidOperationException>(fun () -> csrf.Field()) |> ignore
        Assert.Throws<InvalidOperationException>(fun () -> csrf.RequireValidPost()) |> ignore

    [<Fact>]
    let ``CSRF token can be rotated and multipart forms can be protected`` () =
        let generated =
            generate "csrf-upload" <| fun context ->
                context.php.session.Start SessionOptions.production
                let csrf = context.php.csrf
                csrf.RotateToken()
                context.html.formFileUploadWithCsrf("upload.php", csrf) ignore

        Assert.Equal(1, occurrences "bin2hex(random_bytes(32))" generated)
        Assert.Contains("enctype=\"multipart/form-data\"", generated)
        Assert.Contains("_aqualis_csrf", generated)

    [<Fact>]
    let ``CSRF form rejects protection from another generation context`` () =
        use output = new TemporaryDirectory()
        use first = new Aqualis(Some output.Path, Some "first.php", PHP)
        use second = new Aqualis(Some output.Path, Some "second.php", PHP)
        first.php.session.Start SessionOptions.production
        let csrf = first.php.csrf
        csrf.EnsureToken()

        Assert.Throws<ArgumentException>(fun () ->
            second.html.formWithCsrf("save.php", csrf) ignore)
        |> ignore
