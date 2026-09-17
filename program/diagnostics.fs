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

/// Classifies the impact of a diagnostic emitted during generation or compilation.
type DiagnosticSeverity =
    /// Informational message that does not indicate a problem.
    | Information
    /// Potential problem that does not by itself stop compilation.
    | Warning
    /// Problem that prevents a successful result.
    | Error

/// Identifies the source or operation associated with a diagnostic.
type DiagnosticLocation =
    /// A code-generation operation for a target language and optional project.
    | Generation of language:Language * projectName:string option * operation:string option
    /// A position in an input file; line and column may be unavailable.
    | InputFile of path:string * line:int option * column:int option
    /// An operation on a symbolic expression.
    | Expression of operation:string

/// Structured diagnostic produced by Aqualis.
type AqualisDiagnostic =
    {
        /// Stable identifier for the diagnostic, such as AQL0000.
        Code: string
        /// Impact of the diagnostic.
        Severity: DiagnosticSeverity
        /// Human-readable explanation.
        Message: string
        /// Source or operation associated with the diagnostic, when known.
        Location: DiagnosticLocation option
        /// Additional machine-readable details.
        Properties: Map<string,string>
    }

/// Thread-safe, bounded collection of diagnostics.
/// When a report exceeds the limit, it appends one AQL0000 warning and suppresses later reports.
type DiagnosticBag(?maxDiagnostics:int) =
    let gate = obj()
    let diagnostics = ResizeArray<AqualisDiagnostic>()
    let maximum = defaultArg maxDiagnostics 1000
    let mutable overflowReported = false

    do
        if maximum <= 0 then
            invalidArg (nameof maxDiagnostics) "The maximum diagnostic count must be positive."

    /// Adds a diagnostic unless the configured limit has been reached.
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

    /// Returns a stable list of the diagnostics currently in the bag.
    member _.Snapshot() =
        lock gate (fun () -> diagnostics |> Seq.toList)

    /// Gets the number of stored diagnostics, including the overflow warning if present.
    member _.Count =
        lock gate (fun () -> diagnostics.Count)

    /// Gets whether any stored diagnostic has error severity.
    member _.HasErrors =
        lock gate (fun () -> diagnostics |> Seq.exists (fun item -> item.Severity = Error))

[<RequireQualifiedAccess>]
/// Writes structured diagnostics to the console.
module internal DiagnosticConsoleRenderer =
    /// Formats a diagnostic severity for console output.
    let private severityName = function
        | Information -> "info"
        | Warning -> "warning"
        | Error -> "error"

    /// Writes a sequence of diagnostics to the console.
    let write (diagnostics:AqualisDiagnostic seq) =
        for diagnostic in diagnostics do
            Console.Error.WriteLine(
                "{0} {1}: {2}",
                severityName diagnostic.Severity,
                diagnostic.Code,
                diagnostic.Message)

[<RequireQualifiedAccess>]
/// Tracks the current diagnostic bag for nested operations.
module internal DiagnosticScope =
    /// Current diagnostic bag for an asynchronous flow.
    let private current = AsyncLocal<DiagnosticBag>()

    /// Gets the current diagnostic bag, if one is in scope.
    let tryCurrent() =
        current.Value |> Option.ofObj

    /// Makes a diagnostic bag current until the returned scope is disposed.
    let push (diagnostics:DiagnosticBag) =
        let previous = current.Value
        current.Value <- diagnostics
        { new IDisposable with
            /// Restores the previous diagnostic scope.
            member _.Dispose() = current.Value <- previous }

    /// Reports a diagnostic to the current bag when available.
    let report diagnostic =
        match tryCurrent() with
        | Some diagnostics -> diagnostics.Report diagnostic
        | None -> DiagnosticConsoleRenderer.write [diagnostic]

[<RequireQualifiedAccess>]
/// Creates and emits structured diagnostics.
module Diagnostic =
    /// Creates a diagnostic after validating its code and message.
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

    /// Reports a diagnostic to the current scope, or to standard error when no scope exists.
    let report code severity message location properties =
        create code severity message location properties
        |> DiagnosticScope.report

[<RequireQualifiedAccess>]
/// Captures diagnostics emitted by operations without a generation context.
module Diagnostics =
    /// Captures diagnostics produced by an operation that has no Aqualis generation context.
    let Capture(action:unit -> 'T) =
        if isNull (box action) then nullArg (nameof action)
        let bag = DiagnosticBag()
        use _scope = DiagnosticScope.push bag
        let value = action()
        value, bag.Snapshot()

/// Output paths and diagnostics returned from a compilation.
type CompilationResult =
    {
        /// Paths of files produced by compilation.
        OutputFiles: string list
        /// Diagnostics collected during compilation.
        Diagnostics: AqualisDiagnostic list
    }

/// A value accompanied by diagnostics produced while obtaining it.
type DiagnosticResult<'T> =
    {
        /// Result of the operation.
        Value: 'T
        /// Diagnostics emitted while producing the value.
        Diagnostics: AqualisDiagnostic list
    }

/// Controls how compilation handles warnings and limits diagnostic collection.
type DiagnosticPolicy =
    {
        /// Whether warnings cause compilation to fail.
        TreatWarningsAsErrors: bool
        /// Maximum number of diagnostics before an overflow warning is emitted.
        MaxDiagnostics: int
    }

[<RequireQualifiedAccess>]
/// Default diagnostic policy values.
module DiagnosticPolicy =
    /// Does not promote warnings to errors and permits up to 1,000 diagnostics.
    let defaults = {
        TreatWarningsAsErrors = false
        MaxDiagnostics = 1000
    }

/// Exception raised when compilation fails with structured diagnostics.
type AqualisCompilationException(
    message:string,
    diagnostics:AqualisDiagnostic list,
    ?innerException:exn) =
    inherit Exception(message, defaultArg innerException null)
    /// Gets the diagnostics associated with the compilation failure.
    member _.Diagnostics = diagnostics
