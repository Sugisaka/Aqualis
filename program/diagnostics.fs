//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

open System
open System.Collections.Generic
open System.Threading

type DiagnosticSeverity =
    | Information
    | Warning
    | Error

type DiagnosticLocation =
    | Generation of language:Language * projectName:string option * operation:string option
    | InputFile of path:string * line:int option * column:int option
    | Expression of operation:string

type AqualisDiagnostic =
    {
        Code: string
        Severity: DiagnosticSeverity
        Message: string
        Location: DiagnosticLocation option
        Properties: Map<string,string>
    }

type DiagnosticBag(?maxDiagnostics:int) =
    let gate = obj()
    let diagnostics = ResizeArray<AqualisDiagnostic>()
    let maximum = defaultArg maxDiagnostics 1000
    let mutable overflowReported = false

    do
        if maximum <= 0 then
            invalidArg (nameof maxDiagnostics) "The maximum diagnostic count must be positive."

    member internal _.Report(diagnostic:AqualisDiagnostic) =
        lock gate (fun () ->
            if diagnostics.Count < maximum then
                diagnostics.Add diagnostic
            elif not overflowReported then
                overflowReported <- true
                diagnostics.Add {
                    Code = "AQL0000"
                    Severity = Warning
                    Message = "Additional diagnostics were suppressed."
                    Location = None
                    Properties = Map ["maximum", string maximum]
                })

    member _.Snapshot() =
        lock gate (fun () -> diagnostics |> Seq.toList)

    member _.Count =
        lock gate (fun () -> diagnostics.Count)

    member _.HasErrors =
        lock gate (fun () -> diagnostics |> Seq.exists (fun item -> item.Severity = Error))

[<RequireQualifiedAccess>]
module internal DiagnosticConsoleRenderer =
    let private severityName = function
        | Information -> "info"
        | Warning -> "warning"
        | Error -> "error"

    let write (diagnostics:AqualisDiagnostic seq) =
        for diagnostic in diagnostics do
            Console.Error.WriteLine(
                "{0} {1}: {2}",
                severityName diagnostic.Severity,
                diagnostic.Code,
                diagnostic.Message)

[<RequireQualifiedAccess>]
module internal DiagnosticScope =
    let private current = AsyncLocal<DiagnosticBag>()

    let tryCurrent() =
        current.Value |> Option.ofObj

    let push (diagnostics:DiagnosticBag) =
        let previous = current.Value
        current.Value <- diagnostics
        { new IDisposable with
            member _.Dispose() = current.Value <- previous }

    let report diagnostic =
        match tryCurrent() with
        | Some diagnostics -> diagnostics.Report diagnostic
        | None -> DiagnosticConsoleRenderer.write [diagnostic]

[<RequireQualifiedAccess>]
module Diagnostic =
    let create code severity message location properties =
        if String.IsNullOrWhiteSpace code then invalidArg (nameof code) "A diagnostic code is required."
        if String.IsNullOrWhiteSpace message then invalidArg (nameof message) "A diagnostic message is required."
        {
            Code = code
            Severity = severity
            Message = message
            Location = location
            Properties = properties
        }

    let report code severity message location properties =
        create code severity message location properties
        |> DiagnosticScope.report

[<RequireQualifiedAccess>]
module Diagnostics =
    /// Captures diagnostics produced by an operation that has no Aqualis generation context.
    let Capture(action:unit -> 'T) =
        if isNull (box action) then nullArg (nameof action)
        let bag = DiagnosticBag()
        use _scope = DiagnosticScope.push bag
        let value = action()
        value, bag.Snapshot()

type CompilationResult =
    {
        OutputFiles: string list
        Diagnostics: AqualisDiagnostic list
    }

type DiagnosticResult<'T> =
    {
        Value: 'T
        Diagnostics: AqualisDiagnostic list
    }

type DiagnosticPolicy =
    {
        TreatWarningsAsErrors: bool
        MaxDiagnostics: int
    }

[<RequireQualifiedAccess>]
module DiagnosticPolicy =
    let defaults = {
        TreatWarningsAsErrors = false
        MaxDiagnostics = 1000
    }

type AqualisCompilationException(
    message:string,
    diagnostics:AqualisDiagnostic list,
    ?innerException:exn) =
    inherit Exception(message, defaultArg innerException null)
    member _.Diagnostics = diagnostics
