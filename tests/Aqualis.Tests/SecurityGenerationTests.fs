namespace Aqualis.Tests

open System
open System.IO
open Xunit
open Aqualis

module SecurityGenerationTests =
    let private productionOptions =
        SessionOptions.production "AqualisTestSession" "/aqualis-test/"

    let private developmentOptions =
        SessionOptions.development "AqualisTestSession" "/aqualis-test/"

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
    let ``session start securely handles both new and matching active sessions`` () =
        let generated =
            generate "session-start" <| fun context ->
                context.php.session.Start productionOptions
                context.php.session.Start productionOptions

        Assert.Equal(1, occurrences "session_set_cookie_params(" generated)
        Assert.Equal(1, occurrences "session_start()" generated)
        Assert.Contains("'lifetime' => 0", generated)
        Assert.Contains("'path' => \"/aqualis-test/\"", generated)
        Assert.Contains("'domain' => ''", generated)
        Assert.Contains("'secure' => true", generated)
        Assert.Contains("'httponly' => true", generated)
        Assert.Contains("'samesite' => \"Lax\"", generated)
        Assert.Contains("headers_sent($aqualisSessionHeaderFile, $aqualisSessionHeaderLine)", generated)
        Assert.Contains("$aqualisSessionStatus === PHP_SESSION_ACTIVE", generated)
        Assert.Contains("session_name() === \"AqualisTestSession\"", generated)
        Assert.Contains("session_name(\"AqualisTestSession\")", generated)
        Assert.Contains("Failed to configure the application-specific PHP session name", generated)
        Assert.Contains("session_get_cookie_params()", generated)
        Assert.Contains("$aqualisSessionConfigurationMatches", generated)
        Assert.Contains("Start the session through Aqualis before other middleware", generated)
        Assert.Contains("ini_set('session.use_cookies', '1')", generated)
        Assert.Contains("ini_set('session.use_only_cookies', '1')", generated)
        Assert.Contains("ini_set('session.use_strict_mode', '1')", generated)
        Assert.Equal(2, occurrences "setcookie(session_name(), session_id()," generated)
        Assert.Contains("PHP sessions are disabled", generated)
        let cookieOptionsIndex = generated.IndexOf("session_set_cookie_params(", StringComparison.Ordinal)
        let sessionStartIndex = generated.IndexOf("session_start()", StringComparison.Ordinal)
        Assert.True(cookieOptionsIndex < sessionStartIndex)

    [<Fact>]
    let ``session and CSRF initialization are hoisted out of generated branches`` () =
        let generated =
            generate "security-prologue" <| fun context ->
                context.writein "BODY_BEFORE_SECURITY"
                let guard = context.php.isset(context.php.var "initializeSecurity")
                context.br.if1 guard <| fun () ->
                    context.php.session.Start productionOptions
                    context.php.csrf.EnsureToken()
                context.php.csrf.Field()

        let sessionIndex = generated.IndexOf("session_start()", StringComparison.Ordinal)
        let tokenIndex = generated.IndexOf("bin2hex(random_bytes(32))", StringComparison.Ordinal)
        let bodyIndex = generated.IndexOf("BODY_BEFORE_SECURITY", StringComparison.Ordinal)
        let branchIndex = generated.IndexOf("isset($initializeSecurity)", StringComparison.Ordinal)

        Assert.True(sessionIndex >= 0)
        Assert.True(sessionIndex < tokenIndex)
        Assert.True(tokenIndex < bodyIndex)
        Assert.True(tokenIndex < branchIndex)
        Assert.Equal(1, occurrences "session_start()" generated)
        Assert.Equal(1, occurrences "bin2hex(random_bytes(32))" generated)

    [<Fact>]
    let ``session regeneration fails closed and reissues the cookie securely`` () =
        let generated =
            generate "session-regenerate" <| fun context ->
                context.php.session.Start productionOptions
                context.php.session.RegenerateId()

        Assert.Contains("session_status() !== PHP_SESSION_ACTIVE", generated)
        Assert.Contains("if (!session_regenerate_id(true))", generated)
        Assert.Contains("Cannot regenerate an inactive PHP session", generated)
        Assert.Contains("Failed to regenerate the PHP session identifier", generated)
        Assert.Equal(3, occurrences "setcookie(session_name(), session_id()," generated)
        let regenerateIndex = generated.IndexOf("session_regenerate_id(true)", StringComparison.Ordinal)
        let reissueIndex = generated.IndexOf("setcookie(session_name(), session_id(),", regenerateIndex, StringComparison.Ordinal)
        Assert.True(regenerateIndex < reissueIndex)

    [<Fact>]
    let ``development session permits an HTTP cookie`` () =
        let generated =
            generate "development-session" <| fun context ->
                context.php.session.Start developmentOptions

        Assert.Contains("'secure' => false", generated)
        Assert.Contains("'httponly' => true", generated)

    [<Fact>]
    let ``session start rejects invalid and conflicting options`` () =
        use output = new TemporaryDirectory()
        use context = new Aqualis(Some output.Path, Some "invalid-session.php", PHP)

        Assert.Throws<ArgumentException>(fun () ->
            context.php.session.Start { productionOptions with Lifetime = -1 })
        |> ignore

        for invalidOptions in
            [ { productionOptions with Name = "" }
              { productionOptions with Name = "12345" }
              { productionOptions with Name = "shared_session" }
              { productionOptions with Path = "relative" }
              { productionOptions with Path = "/bad;path" } ] do
            Assert.Throws<ArgumentException>(fun () ->
                context.php.session.Start invalidOptions)
            |> ignore

        context.php.session.Start productionOptions
        Assert.Throws<InvalidOperationException>(fun () ->
            context.php.session.Start developmentOptions)
        |> ignore

    [<Fact>]
    let ``session destroy expires the active session cookie with matching attributes`` () =
        let generated =
            generate "session-destroy" <| fun context ->
                context.php.session.Start productionOptions
                context.php.session.Destroy()

        Assert.Contains("$_SESSION = [];", generated)
        Assert.Contains("ini_get('session.use_cookies')", generated)
        Assert.Contains("$sessionCookieParams = session_get_cookie_params();", generated)
        Assert.Contains("setcookie(session_name(), '', [", generated)
        Assert.Contains("'expires' => time() - 42000", generated)
        Assert.Contains("'path' => $sessionCookieParams['path']", generated)
        Assert.Contains("'domain' => $sessionCookieParams['domain']", generated)
        Assert.Contains("'secure' => $sessionCookieParams['secure']", generated)
        Assert.Contains("'httponly' => $sessionCookieParams['httponly']", generated)
        Assert.Contains("'samesite' => $sessionCookieParams['samesite'] ?? 'Lax'", generated)
        Assert.Contains("session_destroy();", generated)

        let deleteCookieIndex = generated.IndexOf("setcookie(session_name()", StringComparison.Ordinal)
        let destroyIndex = generated.IndexOf("session_destroy();", StringComparison.Ordinal)
        Assert.True(deleteCookieIndex < destroyIndex)

    [<Fact>]
    let ``session destroy requires explicit restart and reinitializes CSRF`` () =
        let generated =
            generate "session-restart" <| fun context ->
                let session = context.php.session
                let csrf = context.php.csrf
                session.Start productionOptions
                csrf.EnsureToken()
                session.Destroy()

                Assert.Throws<InvalidOperationException>(fun () -> csrf.Field())
                |> ignore

                session.Start productionOptions
                csrf.EnsureToken()
                csrf.Field()

        Assert.Equal(2, occurrences "session_start()" generated)
        Assert.Equal(2, occurrences "bin2hex(random_bytes(32))" generated)
        let destroyIndex = generated.IndexOf("session_destroy();", StringComparison.Ordinal)
        let restartIndex = generated.IndexOf("session_start()", destroyIndex, StringComparison.Ordinal)
        let reinitializeIndex = generated.IndexOf("bin2hex(random_bytes(32))", restartIndex, StringComparison.Ordinal)
        Assert.True(destroyIndex < restartIndex)
        Assert.True(restartIndex < reinitializeIndex)

    [<Fact>]
    let ``CSRF token and field are emitted once and HTML escaped at runtime`` () =
        let generated =
            generate "csrf-field" <| fun context ->
                let session = context.php.session
                let csrf = context.php.csrf
                session.Start productionOptions
                csrf.EnsureToken()
                csrf.EnsureToken()
                context.html.formWithCsrf(Url.relative "save.php", csrf) ignore

        Assert.Equal(1, occurrences "bin2hex(random_bytes(32))" generated)
        Assert.Contains("!isset($_SESSION[\"_aqualis_csrf\"])", generated)
        Assert.Contains("preg_match('/\\A[0-9a-f]{64}\\z/D', $_SESSION[\"_aqualis_csrf\"]) !== 1", generated)
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
                context.php.session.Start productionOptions
                let csrf = context.php.csrf
                csrf.EnsureToken()
                csrf.RequireValidPost()

        Assert.Contains("$_SERVER[\"REQUEST_METHOD\"] === 'POST'", generated)
        Assert.Contains("isset($_POST[\"_aqualis_csrf\"])", generated)
        Assert.Contains("is_string($_POST[\"_aqualis_csrf\"])", generated)
        Assert.Contains("preg_match('/\\A[0-9a-f]{64}\\z/D', $_SESSION[\"_aqualis_csrf\"]) === 1", generated)
        Assert.Contains("preg_match('/\\A[0-9a-f]{64}\\z/D', $_POST[\"_aqualis_csrf\"]) === 1", generated)
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

        context.php.session.Start productionOptions
        Assert.Throws<InvalidOperationException>(fun () -> csrf.Field()) |> ignore
        Assert.Throws<InvalidOperationException>(fun () -> csrf.RequireValidPost()) |> ignore

    [<Fact>]
    let ``CSRF token can be rotated and multipart forms can be protected`` () =
        let generated =
            generate "csrf-upload" <| fun context ->
                context.php.session.Start productionOptions
                let csrf = context.php.csrf
                csrf.RotateToken()
                context.html.formFileUploadWithCsrf(Url.relative "upload.php", csrf) ignore

        Assert.Equal(2, occurrences "bin2hex(random_bytes(32))" generated)
        Assert.Contains("enctype=\"multipart/form-data\"", generated)
        Assert.Contains("_aqualis_csrf", generated)

    [<Fact>]
    let ``CSRF form rejects protection from another generation context`` () =
        use output = new TemporaryDirectory()
        use first = new Aqualis(Some output.Path, Some "first.php", PHP)
        use second = new Aqualis(Some output.Path, Some "second.php", PHP)
        first.php.session.Start productionOptions
        let csrf = first.php.csrf
        csrf.EnsureToken()

        Assert.Throws<ArgumentException>(fun () ->
            second.html.formWithCsrf(Url.relative "save.php", csrf) ignore)
        |> ignore
