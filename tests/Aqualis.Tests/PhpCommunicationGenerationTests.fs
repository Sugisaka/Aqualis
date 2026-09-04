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
