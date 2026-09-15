namespace Aqualis.Tests

open System
open System.IO
open Xunit
open Aqualis

module PhpResponseGenerationTests =
    let private generate name code =
        use output = new TemporaryDirectory()
        let fileName = name + ".php"
        Aqualis.makeProgramWithContext (output.Path,fileName,PHP) code
        File.ReadAllText(Path.Combine(output.Path,fileName))

    [<Fact>]
    let ``Require emits encoded public message and private diagnostic`` () =
        let generated =
            generate "require" <| fun context ->
                let ready = context.php.var "ready"
                let diagnostic = PHPdata "Read failed: " ++ context.php.var "errorCode"
                context.response.Require(
                    ready .= 1,
                    ServiceUnavailable,
                    "Unavailable 'temporarily'",
                    diagnostic)

        Assert.Contains("if (!($ready == 1))", generated)
        Assert.Contains("error_log((string)(\"Read failed: \".$errorCode));", generated)
        Assert.Contains("http_response_code(503);", generated)
        Assert.Contains("exit(\"Unavailable 'temporarily'\");", generated)

    [<Fact>]
    let ``RejectIf and PHP predicates remain expression based`` () =
        let generated =
            generate "reject" <| fun context ->
                let userId = context.php.var "userId"
                let invalid = context.php.nt(context.php.And [
                    context.php.isString userId
                    context.php.matches("/\\A[A-Za-z0-9_-]{1,64}\\z/D",userId)
                ])
                context.response.RejectIf(invalid,Forbidden,"Invalid session.")

        Assert.Contains("is_string($userId)", generated)
        Assert.Contains("preg_match(\"/\\\\A[A-Za-z0-9_-]{1,64}\\\\z/D\", $userId) === 1", generated)
        Assert.Contains("http_response_code(403);", generated)

    [<Fact>]
    let ``Require rejects values from another generation context`` () =
        use output = new TemporaryDirectory()
        use first = new Aqualis(Some output.Path,Some "first.php",PHP)
        use second = new Aqualis(Some output.Path,Some "second.php",PHP)
        let foreign = first.php.var "foreign"

        Assert.Throws<InvalidOperationException>(fun () ->
            second.response.Require(foreign .= 1,BadRequest,"bad"))

    [<Fact>]
    let ``file login throttle generates namespaced reusable functions`` () =
        let generated =
            generate "login-throttle" <| fun context ->
                let throttle =
                    FileLoginThrottle(
                        context,
                        FileLoginThrottleOptions.defaults "course" "data-course" "Course")
                throttle.EmitDefinitions()
                throttle.RequireAllowed(context.php.var "userId")
                throttle.ReleaseSuccessfulAttempt(context.php.var "userId")

        Assert.Contains("const COURSE_LOGIN_WINDOW_SECONDS = 600;",generated)
        Assert.Contains("function course_reserve_login_attempt($userId): array",generated)
        Assert.Contains("function course_release_successful_login_attempt($userId): bool",generated)
        Assert.Contains("DIRECTORY_SEPARATOR.\"data-course\"",generated)
        Assert.Contains("DIRECTORY_SEPARATOR.\".course-login-rate-limit.lock\"",generated)
        Assert.Contains("DIRECTORY_SEPARATOR.\".course-login-rate-limit-secret\"",generated)
        Assert.Contains("DIRECTORY_SEPARATOR.\".course-login-rate-limit.json\"",generated)
        Assert.Contains("DIRECTORY_SEPARATOR.\".course-login-rate-limit.invalid-*.json\"",generated)
        Assert.DoesNotContain("DIRECTORY_SEPARATOR.'.login-rate-limit",generated)
        Assert.Contains("course_reserve_login_attempt($userId)",generated)
        Assert.Contains("course_release_successful_login_attempt($userId)",generated)

    [<Fact>]
    let ``file login throttle rejects unsafe identifiers`` () =
        use output = new TemporaryDirectory()
        use context = new Aqualis(Some output.Path,Some "invalid-throttle.php",PHP)
        let options = FileLoginThrottleOptions.defaults "bad-name" "data-course" "Course"

        Assert.Throws<ArgumentException>(fun () -> FileLoginThrottle(context,options) |> ignore)
