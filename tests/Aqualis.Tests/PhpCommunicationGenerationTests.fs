namespace Aqualis.Tests

open System.IO
open System.Text.RegularExpressions
open Xunit
open Aqualis

module PhpCommunicationGenerationTests =
    let private generate code =
        use output = new TemporaryDirectory()
        Compile [PHP] output.Path "communication" "1.0" code
        File.ReadAllText(Path.Combine(output.Path, "communication.php"))

    let private assertNoShellExec (source:string) =
        Assert.False(
            Regex.IsMatch(source, @"(?<![A-Za-z0-9_])exec\s*\("),
            "Generated PHP must not invoke the shell exec function.")

    [<Fact>]
    let ``empty array expressions and variables use the PHP generation context`` () =
        let source =
            generate (fun context ->
                let values = context.php.var "values"
                let empty = context.php.array()
                let initialized = context.php.var("initialized", context.php.array())
                let staticInitialized =
                    PHPdata.var(context, "staticInitialized", PHPdata.array(context))

                Assert.Same(context, empty.Context)
                Assert.Same(context, values.Context)
                Assert.Same(context, initialized.Context)
                Assert.Same(context, staticInitialized.Context)
                values <== empty)

        Assert.Contains("$values = array();", source)
        Assert.Contains("$initialized = array();", source)
        Assert.Contains("$staticInitialized = array();", source)

    [<Fact>]
    let ``PHP string literals escape quotes interpolation and control characters`` () =
        let literal = PHPdata "quote: \" slash: \\ variable: $name\r\n"

        Assert.Equal(
            "\"quote: \\\" slash: \\\\ variable: \\$name\\x0D\\x0A\"",
            literal.code)

        let source =
            generate (fun context ->
                let textBox = context.form.textBox "newselect"
                textBox.show("\"確認\"", [Atr("class", "textinput")]))

        Assert.Contains("\\\"確認\\\"", source)
        Assert.DoesNotContain("(string)(\"\"確認\"\")", source)

    [<Fact>]
    let ``structured JSON output uses the encoder and checks file writes`` () =
        let source =
            generate (fun context ->
                let issue = context.php.array("issue")
                issue["ID"] <== 42
                issue["Title"] <== "A \"quoted\" title with $name"
                issue["Main"] <== "first line\r\nsecond line"
                issue["Time"] <== context.php.date("Y-m-d \"H:i\" $zone")
                issue["Comment"] <== context.php.array()

                let users = context.php.array("users")
                let user = context.php.array("user")
                let newPassword = (post(context, "newPassword")).get
                user["ID"] <== "user001"
                user["Auth"] <== 1
                user["State"] <== context.php.array()
                user["PasswordHash"] <== context.php.password_hash(newPassword)
                users.push(user)
                issue["Users"] <== users

                context.php.writeJson("issue.json", issue))

        Assert.Contains("$issue[\"ID\"] = 42;", source)
        Assert.Contains("$issue[\"Title\"] = \"A \\\"quoted\\\" title with \\$name\";", source)
        Assert.Contains("$issue[\"Main\"] = \"first line\\x0D\\x0Asecond line\";", source)
        Assert.Contains("$issue[\"Time\"] = date(\"Y-m-d \\\"H:i\\\" \\$zone\");", source)
        Assert.Contains("$issue[\"Comment\"] = array();", source)
        Assert.Contains("$user[\"ID\"] = \"user001\";", source)
        Assert.Contains("$user[\"Auth\"] = 1;", source)
        Assert.Contains("$user[\"State\"] = array();", source)
        Assert.Contains(
            "$user[\"PasswordHash\"] = password_hash($_POST[\"newPassword\"], PASSWORD_DEFAULT);",
            source)
        Assert.DoesNotContain("$user[\"PassWord\"]", source)
        Assert.Contains("array_push($users, $user);", source)
        Assert.Contains("$issue[\"Users\"] = $users;", source)
        Assert.Contains(
            "json_encode($issue, JSON_THROW_ON_ERROR|JSON_PRETTY_PRINT|JSON_UNESCAPED_UNICODE|JSON_UNESCAPED_SLASHES)",
            source)
        Assert.Contains("file_put_contents(\"issue.json\", json_encode(", source)
        Assert.Contains(", LOCK_EX) === false", source)
        Assert.Contains("throw new \\RuntimeException('Failed to write the file.');", source)

    [<Fact>]
    let ``password APIs hash verify and detect stale hashes`` () =
        let source =
            generate (fun context ->
                let password = (post(context, "password")).get
                let passwordHash =
                    context.php.var("passwordHash", context.php.password_hash(password))

                context.br.if1(context.php.password_verify(password, passwordHash)) <| fun () ->
                    context.php.echo "verified"
                context.br.if1(context.php.password_needs_rehash(passwordHash)) <| fun () ->
                    passwordHash <== context.php.password_hash(password))

        Assert.Contains(
            "$passwordHash = password_hash($_POST[\"password\"], PASSWORD_DEFAULT);",
            source)
        Assert.Contains(
            "if(password_verify($_POST[\"password\"], $passwordHash)):",
            source)
        Assert.Contains(
            "if(password_needs_rehash($passwordHash, PASSWORD_DEFAULT)):",
            source)
        Assert.Contains(
            "$passwordHash = password_hash($_POST[\"password\"], PASSWORD_DEFAULT);",
            source)

    [<Fact>]
    let ``SMTP mail uses proc open without a shell`` () =
        let source =
            generate (fun context ->
                context.php.sendMail(
                    PHPdata.var(context, "body"),
                    PHPdata.var(context, "subject"),
                    PHPdata.var(context, "smtp"),
                    PHPdata.var(context, "fromAddress"),
                    PHPdata.var(context, "toAddress")))

        Assert.Contains("proc_open($command, $descriptors, $pipes)", source)
        Assert.Contains("$command = ['mail', '-s', $subject", source)
        Assert.Contains("fwrite($pipes[0], $body)", source)
        Assert.Contains("FILTER_VALIDATE_EMAIL", source)
        Assert.Contains("FILTER_VALIDATE_DOMAIN", source)
        assertNoShellExec source
        Assert.DoesNotContain("echo \\\"", source)
        Assert.DoesNotContain(" | mail", source)

    [<Fact>]
    let ``Discord webhook uses curl API and JSON encoding`` () =
        let source =
            generate (fun context ->
                context.php.sendDiscord(
                    PHPdata.var(context, "body"),
                    PHPdata.var(context, "webhookURL")))

        Assert.Contains("json_encode(['username' => 'Ediass Notification', 'content' => $body]", source)
        Assert.Contains("JSON_THROW_ON_ERROR", source)
        Assert.Contains("curl_init($webhookURL)", source)
        Assert.Contains("curl_setopt_array($curl", source)
        Assert.Contains("CURLOPT_POSTFIELDS => $payload", source)
        Assert.Contains("CURLPROTO_HTTPS", source)
        Assert.Contains("CURLINFO_RESPONSE_CODE", source)
        assertNoShellExec source
        Assert.DoesNotContain("$cmd =", source)
        Assert.DoesNotContain("curl -H", source)
