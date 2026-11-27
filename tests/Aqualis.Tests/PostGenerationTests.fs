namespace Aqualis.Tests

open System.IO
open System.Reflection
open Xunit
open Aqualis

module PostGenerationTests =
    [<Fact>]
    let ``POST controls preserve generated output and public API`` () =
        use output = new TemporaryDirectory()
        let generated =
            [PHP; HTML]
            |> List.map (fun language ->
                let filename = string language + ".txt"
                use context = new Aqualis(Some output.Path, Some filename, language)
                let p = post(context, "field\"<&")
                let a = [Atr("class", "a\"<&"); Atr("value", "duplicate"); Atr("disabled")]
                p.input()
                p.input(a)
                p.password()
                p.input(PHPdata.f(context, "$dynamic"))
                p.input_hidden(PHPdata.f(context, "$dynamic"))
                p.input("text\"<&")
                p.input_hidden("text\"<&")
                p.input(PHPdata.f(context, "$dynamic"), a)
                p.input_hidden(PHPdata.f(context, "$dynamic"), a)
                p.input("text\"<&", a)
                p.input_hidden("text\"<&", a)
                p.textArea()
                p.textArea(fun () -> context.writein "callback")
                p.textArea(a)
                p.textArea_contents(a) (fun () -> context.writein "content")
                p.textArea_copy()
                p.textArea_copy(a)
                p.textArea("text\"<&")
                p.input_lock(PHPdata.f(context, "$dynamic"))
                p.input_lock("text\"<&")
                p.input_lock(PHPdata.f(context, "$dynamic"), a)
                p.input_lock("text\"<&", a)
                p.input(I 42)
                p.input(I 42, a)
                p.input_hidden(I 42)
                p.input_hidden(I 42, a)
                p.input_lock(I 42)
                p.input_lock(I 42, a)
                p.password(I 42)
                p.input(D 1.25)
                p.input(D 1.25, a)
                p.input_hidden(D 1.25)
                p.input_hidden(D 1.25, a)
                p.input_lock(D 1.25)
                p.input_lock(D 1.25, a)
                p.password("text\"<&")
                p.password(D 1.25)
                p.input_copy()
                p.input_copy(a)
                p.input_copy_hidden()
                p.input_copy_hidden(a)
                p.input_copy_lock()
                p.input_copy_lock(a)
                p.password_copy()
                p.password_copy_lock()
                p.submit("text\"<&")
                p.submit(Url.relative "next.php", "text\"<&")
                p.submit(Url.relative "next.php", "text\"<&", "text\"<&")
                p.select(fun () -> context.writein "callback")
                post(context, FieldName.create "typed").input()
                post(context, I 7).input()
                post(context, PHPdata.f(context, "$fieldName")).input()
                context.close()
                File.ReadAllText(Path.Combine(output.Path, filename)).Replace("\r\n", "\n"))
            |> String.concat "\n--- language ---\n"
        let api =
            typeof<post>.GetMembers(BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.DeclaredOnly)
            |> Array.map string
            |> Array.sort
            |> String.concat "\n"
        let actual = generated + "\n--- API ---\n" + api
        let golden = TestHelpers.goldenPath "web" "post-controls.txt"
        Assert.Equal(File.ReadAllText(golden).Replace("\r\n", "\n"), actual)
