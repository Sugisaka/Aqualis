namespace Aqualis.Tests

open System
open System.IO

type TemporaryDirectory() =
    let path =
        Path.Combine(
            Path.GetTempPath(),
            "Aqualis.Tests",
            Guid.NewGuid().ToString("N"))

    do Directory.CreateDirectory(path) |> ignore

    member _.Path = path

    interface IDisposable with
        member _.Dispose() =
            if Directory.Exists(path) then
                Directory.Delete(path, true)

module TestHelpers =
    let normalizeGeneratedCode (text: string) =
        text.Replace("\r\n", "\n")
            .Split('\n', StringSplitOptions.RemoveEmptyEntries)
        |> Array.map _.Trim()
        |> String.concat "\n"

    let goldenPath language filename =
        Path.Combine(
            AppContext.BaseDirectory,
            "golden",
            language,
            filename)
