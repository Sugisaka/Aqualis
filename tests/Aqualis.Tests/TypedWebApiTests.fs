namespace Aqualis.Tests

open System
open System.IO
open Xunit
open Aqualis

module TypedWebApiTests =
    let private generate name code =
        use output = new TemporaryDirectory()
        let fileName = name + ".php"
        use context = new Aqualis(Some output.Path, Some fileName, PHP)
        code context
        context.close()
        File.ReadAllText(Path.Combine(output.Path, fileName))

    [<Fact>]
    let ``field names reject dots markup whitespace and dynamic array syntax`` () =
        FieldName.create "student_user-id:1" |> ignore

        for invalid in [""; "1field"; "student.id"; "user id"; "user[]"; "x\" autofocus"] do
            Assert.Throws<ArgumentException>(fun () -> FieldName.create invalid |> ignore)
            |> ignore

    [<Fact>]
    let ``typed PHP variable names reject expression syntax`` () =
        PhpVariableName.create "student_user_id" |> ignore

        for invalid in [""; "1value"; "$value"; "value-name"; "value[0]"] do
            Assert.Throws<ArgumentException>(fun () -> PhpVariableName.create invalid |> ignore)
            |> ignore

    [<Fact>]
    let ``CSS class names reject multiple tokens and markup`` () =
        CssClass.create "top-list_1" |> ignore

        for invalid in [""; "one two"; "x\"onclick"; "<style>"] do
            Assert.Throws<ArgumentException>(fun () -> CssClass.create invalid |> ignore)
            |> ignore

    [<Fact>]
    let ``URLs distinguish application relative and absolute HTTPS targets`` () =
        Assert.Equal("main.php?tab=1", Url.value (Url.relative "main.php?tab=1"))
        Assert.Equal("https://example.com/path", Url.value (Url.https "https://example.com/path"))

        for invalid in ["javascript:alert(1)"; "//example.com/path"; "\\\\example.com\\path"; "page.php\r\nX-Test: x"] do
            Assert.Throws<ArgumentException>(fun () -> Url.relative invalid |> ignore)
            |> ignore

        Assert.Throws<ArgumentException>(fun () -> Url.https "http://example.com" |> ignore)
        |> ignore

    [<Fact>]
    let ``typed redirect maps its status union and validates its URL`` () =
        let generated =
            generate "typed-redirect" <| fun context ->
                context.php.redirect(Url.relative "main.php", RedirectStatus.SeeOther)

        Assert.Contains("header('Location: ' . $location, true, 303);", generated)
        Assert.Contains("})(\"main.php\");", generated)

    [<Fact>]
    let ``typed POST fields generate presence type and range validation`` () =
        let generated =
            generate "typed-post" <| fun context ->
                let userId =
                    context.request.post.RequiredText(
                        FieldName.create "userid",
                        minLength = 3,
                        maxLength = 32)
                let score =
                    context.request.post.RequiredInt(
                        FieldName.create "score",
                        minimum = 0,
                        maximum = 100)
                let ratio =
                    context.request.post.RequiredFloat(
                        FieldName.create "ratio",
                        minimum = 0.0,
                        maximum = 1.0)

                context.br.if1 userId.IsPresent ignore
                context.br.if1 userId.IsValid ignore
                context.br.if1 score.IsValid ignore
                context.br.if1 ratio.IsValid ignore
                context.html.text userId.Value
                context.br.if1 (context.php.stringEquals(context.php.var "storedUser", userId.Value)) ignore
                context.br.if1 (context.php.password_verify(userId.Value, context.php.var "passwordHash")) ignore
                let scoreValue = PhpIntExpr.numeric score.Value
                let ratioValue = PhpFloatExpr.numeric ratio.Value
                context.br.if1 (scoreValue .>= 0) ignore
                context.br.if1 (ratioValue .<= 1.0) ignore

        Assert.Contains("mb_strlen($_POST[\"userid\"], 'UTF-8') >= 3", generated)
        Assert.Contains("mb_strlen($_POST[\"userid\"], 'UTF-8') <= 32", generated)
        Assert.Contains("filter_var($_POST[\"score\"], FILTER_VALIDATE_INT) !== false", generated)
        Assert.Contains("(int)($_POST[\"score\"]) >= 0", generated)
        Assert.Contains("(int)($_POST[\"score\"]) <= 100", generated)
        Assert.Contains("filter_var($_POST[\"ratio\"], FILTER_VALIDATE_FLOAT) !== false", generated)
        Assert.Contains("htmlspecialchars((string)($_POST[\"userid\"])", generated)
        Assert.Contains("$storedUser === $_POST[\"userid\"]", generated)
        Assert.DoesNotContain("$storedUser == $_POST[\"userid\"]", generated)
        Assert.Contains("password_verify($_POST[\"userid\"], $passwordHash)", generated)

    [<Fact>]
    let ``active session creates initialized typed CSRF and typed session values`` () =
        let generated =
            generate "typed-session" <| fun context ->
                let session = context.php.startSession SessionOptions.production
                let csrf = session.CsrfToken()
                let userIdKey = SessionKey.string "student_user_id"
                session.Set(userIdKey, "student01")
                context.br.if1 (session.Isset userIdKey) ignore
                context.html.text(session.Get userIdKey)
                csrf.RequireValidPost()
                context.html.postForm(Url.relative "main.php", csrf) ignore

        Assert.Contains("session_start();", generated)
        Assert.Contains("$_SESSION[\"student_user_id\"] = \"student01\"", generated)
        Assert.Contains("hash_equals($_SESSION[\"_aqualis_csrf\"], $_POST[\"_aqualis_csrf\"])", generated)
        Assert.Contains("<form method=\"post\" action=\"main.php\"", generated)
        Assert.Contains("_aqualis_csrf", generated)

    [<Fact>]
    let ``raw HTML requires an explicit trusted value`` () =
        let generated =
            generate "trusted-html" <| fun context ->
                context.html.raw(Unsafe.trustedHtml "<strong>trusted</strong>")

        Assert.Contains("<strong>trusted</strong>", generated)

    [<Fact>]
    let ``typed literal factories reject non-finite floating point values`` () =
        PhpExpr.stringLiteral "text" |> ignore
        PhpExpr.intLiteral 42 |> ignore
        PhpExpr.floatLiteral 1.25 |> ignore

        Assert.Throws<ArgumentException>(fun () -> PhpExpr.floatLiteral Double.NaN |> ignore)
        |> ignore

    [<Fact>]
    let ``typed PHP output rejects values from another generation context`` () =
        use output = new TemporaryDirectory()
        use first = new Aqualis(Some output.Path, Some "typed-first.php", PHP)
        use second = new Aqualis(Some output.Path, Some "typed-second.php", PHP)
        let foreignValue = first.php.stringVar(PhpVariableName.create "foreign")

        Assert.Throws<InvalidOperationException>(fun () -> second.html.text foreignValue)
        |> ignore
