//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

open System

/// A validated relative or HTTPS URL for generated HTML and HTTP redirects.
[<Struct>]
type Url =
    private
    | Url of string
    member internal this.Value =
        let (Url value) = this
        value

[<RequireQualifiedAccess>]
module Url =
    let private validateCommon (value:string) =
        if isNull value then nullArg (nameof value)
        if String.IsNullOrWhiteSpace value then
            invalidArg (nameof value) "A URL is required."
        if value.Contains('\r') || value.Contains('\n') then
            invalidArg (nameof value) "A URL cannot contain CR or LF characters."

    /// Creates a relative URL. Scheme-relative and absolute URLs are rejected.
    let relative (value:string) =
        validateCommon value
        if value.Contains('\\') || value.StartsWith("//", StringComparison.Ordinal) || Uri.IsWellFormedUriString(value, UriKind.Absolute) then
            invalidArg (nameof value) "An application-relative URL is required."
        Url value

    /// Creates an absolute HTTPS URL.
    let https (value:string) =
        validateCommon value
        match Uri.TryCreate(value, UriKind.Absolute) with
        | true, uri when uri.Scheme = Uri.UriSchemeHttps && not (String.IsNullOrWhiteSpace uri.Host) -> Url uri.AbsoluteUri
        | _ -> invalidArg (nameof value) "An absolute HTTPS URL is required."

    let internal value (url:Url) = url.Value
