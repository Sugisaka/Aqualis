//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

/// SameSite policy used by the generated PHP session cookie.
[<RequireQualifiedAccess>]
type SameSite =
    | Lax
    | Strict

/// Options passed to PHP's session cookie configuration.
type SessionOptions = {
    Name: string
    Lifetime: int
    Path: string
    Secure: bool
    HttpOnly: bool
    SameSite: SameSite
}

/// Default configuration for generated PHP sessions.
[<RequireQualifiedAccess>]
module SessionOptions =
    /// Creates default session options for a cookie name and path.
    let private create name path secure = {
        Name = name
        Lifetime = 0
        Path = path
        Secure = secure
        HttpOnly = true
        SameSite = SameSite.Lax
    }

    /// Secure defaults for one HTTPS application. Use a distinct alphanumeric
    /// session name and the narrowest cookie path served by the application.
    let production name path = create name path true

    /// Defaults suitable for one local HTTP development application.
    let development name path = create name path false

/// Mutable session and CSRF setup state owned by one generation context.
type internal SecurityGenerationState() =
    /// Gets or sets the selected session options.
    member val SessionOptions: SessionOptions option = None with get, set
    /// Tracks whether session startup code has been registered.
    member val SessionPrologueRegistered = false with get, set
    /// Tracks whether CSRF token initialization has been emitted.
    member val CsrfTokenEmitted = false with get, set
    /// Tracks whether CSRF initialization code has been registered.
    member val CsrfPrologueRegistered = false with get, set
    /// Gets the synchronization gate for security generation.
    member val Gate = obj() with get
