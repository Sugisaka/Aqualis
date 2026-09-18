//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

open System
open System.Runtime.CompilerServices

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

/// Mutable session and CSRF setup state for a generation context.
type private SecurityGenerationState() =
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

/// Associates security generation state with an Aqualis context.
[<RequireQualifiedAccess>]
module private SecurityGenerationStates =
    /// Per-context storage for generated security state.
    let private states = ConditionalWeakTable<Aqualis, SecurityGenerationState>()
    /// Gets or creates security generation state for a context.
    let get (context:Aqualis) = states.GetOrCreateValue context

/// Builds PHP expressions for session and CSRF handling.
[<RequireQualifiedAccess>]
module private SecurityCode =
    /// Session key used to store the CSRF token.
    [<Literal>]
    let CsrfTokenName = "_aqualis_csrf"

    /// Formats a Boolean as a PHP literal.
    let boolLiteral value = if value then "true" else "false"

    /// Formats a SameSite cookie option for PHP.
    let sameSiteLiteral = function
        | SameSite.Lax -> "Lax"
        | SameSite.Strict -> "Strict"

    /// Builds PHP session-cookie options.
    let sessionCookieOptions options =
        "[" +
        "'lifetime' => " + string options.Lifetime + ", " +
        "'path' => " + PhpEncoding.stringLiteral options.Path + ", " +
        "'domain' => '', " +
        "'secure' => " + boolLiteral options.Secure + ", " +
        "'httponly' => " + boolLiteral options.HttpOnly + ", " +
        "'samesite' => " + PhpEncoding.stringLiteral (sameSiteLiteral options.SameSite) +
        "]"

    /// Builds cookie options emitted by the generated response.
    let emittedCookieOptions options =
        let expires =
            if options.Lifetime = 0 then "0"
            else "time() + " + string options.Lifetime
        "[" +
        "'expires' => " + expires + ", " +
        "'path' => " + PhpEncoding.stringLiteral options.Path + ", " +
        "'domain' => '', " +
        "'secure' => " + boolLiteral options.Secure + ", " +
        "'httponly' => " + boolLiteral options.HttpOnly + ", " +
        "'samesite' => " + PhpEncoding.stringLiteral (sameSiteLiteral options.SameSite) +
        "]"

    /// Validates configured session options.
    let validateSessionOptions options =
        let isAsciiLetter character =
            ('A' <= character && character <= 'Z') ||
            ('a' <= character && character <= 'z')
        let isAsciiLetterOrDigit character =
            isAsciiLetter character || ('0' <= character && character <= '9')

        if isNull options.Name ||
           options.Name.Length < 1 ||
           options.Name.Length > 64 ||
           not (isAsciiLetter options.Name[0]) ||
           not (options.Name |> Seq.forall isAsciiLetterOrDigit) then
            invalidArg
                (nameof options)
                "A session name must start with an ASCII letter and contain 1 to 64 ASCII letters or digits. Use a distinct name for each application."
        if options.Lifetime < 0 then
            invalidArg (nameof options) "Session lifetime must be non-negative."
        if isNull options.Path ||
           String.IsNullOrWhiteSpace options.Path ||
           not (options.Path.StartsWith("/", StringComparison.Ordinal)) ||
           options.Path.Contains(';') ||
           (options.Path |> Seq.exists Char.IsControl) then
            invalidArg
                (nameof options)
                "A session cookie path must be an absolute HTTP path without control characters or semicolons."

    /// Requires that the PHP session has been started.
    let requireStarted context operation =
        let state = SecurityGenerationStates.get context
        lock state.Gate (fun () ->
            if state.SessionOptions.IsNone then
                invalidOp (operation + " requires session.Start to be called first."))

    /// Gets options from an already started session.
    let getStartedOptions context operation =
        let state = SecurityGenerationStates.get context
        lock state.Gate (fun () ->
            match state.SessionOptions with
            | Some options -> options
            | None -> invalidOp (operation + " requires session.Start to be called first."))

/// Emits PHP session operations for one Aqualis generation context.
type WebSession internal (context:Aqualis) =
    /// Gets the owning generation context.
    member internal _.Context = context

    /// Registers an unconditional file prologue that configures a host-only
    /// session cookie and starts a session when needed.
    /// An already-active session is accepted only when all requested security settings match.
    member _.Start(options:SessionOptions) =
        SecurityCode.validateSessionOptions options
        let state = SecurityGenerationStates.get context

        lock state.Gate (fun () ->
            match state.SessionOptions with
            | Some existing when existing = options -> ()
            | Some _ ->
                invalidOp "The session has already been started with different options in this generation context."
            | None ->
                let cookieOptions = SecurityCode.sessionCookieOptions options
                /// Builds cookie options emitted by the generated response.
                let emittedCookieOptions = SecurityCode.emittedCookieOptions options
                let expectedName = PhpEncoding.stringLiteral options.Name
                let expectedPath = PhpEncoding.stringLiteral options.Path
                let expectedSameSite =
                    PhpEncoding.stringLiteral (SecurityCode.sameSiteLiteral options.SameSite)
                let expectedSecure = SecurityCode.boolLiteral options.Secure
                let expectedHttpOnly = SecurityCode.boolLiteral options.HttpOnly

                let startCode =
                    "$aqualisSessionHeaderFile = ''; $aqualisSessionHeaderLine = 0; " +
                    "if (headers_sent($aqualisSessionHeaderFile, $aqualisSessionHeaderLine)) { " +
                    "throw new \\RuntimeException('Session cookies must be configured before output is sent.'); } " +
                    "$aqualisSessionStatus = session_status(); " +
                    "if ($aqualisSessionStatus === PHP_SESSION_ACTIVE) { " +
                    "$aqualisSessionCookieParams = session_get_cookie_params(); " +
                    "$aqualisSessionConfigurationMatches = " +
                    "session_name() === " + expectedName + " " +
                    "&& (int)$aqualisSessionCookieParams['lifetime'] === " + string options.Lifetime + " " +
                    "&& (string)$aqualisSessionCookieParams['path'] === " + expectedPath + " " +
                    "&& (string)$aqualisSessionCookieParams['domain'] === '' " +
                    "&& (bool)$aqualisSessionCookieParams['secure'] === " + expectedSecure + " " +
                    "&& (bool)$aqualisSessionCookieParams['httponly'] === " + expectedHttpOnly + " " +
                    "&& (string)($aqualisSessionCookieParams['samesite'] ?? '') === " + expectedSameSite + " " +
                    "&& (bool)ini_get('session.use_cookies') " +
                    "&& (bool)ini_get('session.use_only_cookies') " +
                    "&& (bool)ini_get('session.use_strict_mode'); " +
                    "if (!$aqualisSessionConfigurationMatches) { " +
                    "throw new \\RuntimeException('An active PHP session uses cookie settings that differ from the requested SessionOptions. Start the session through Aqualis before other middleware.'); } " +
                    "if (!setcookie(session_name(), session_id(), " + emittedCookieOptions + ")) { " +
                    "throw new \\RuntimeException('Failed to reissue the active session cookie with the requested security settings.'); } " +
                    "} elseif ($aqualisSessionStatus === PHP_SESSION_NONE) { " +
                    "if (session_name(" + expectedName + ") === false || session_name() !== " + expectedName + ") { " +
                    "throw new \\RuntimeException('Failed to configure the application-specific PHP session name.'); } " +
                    "if (ini_set('session.use_cookies', '1') === false " +
                    "|| ini_set('session.use_only_cookies', '1') === false " +
                    "|| ini_set('session.use_strict_mode', '1') === false " +
                    "|| !(bool)ini_get('session.use_cookies') " +
                    "|| !(bool)ini_get('session.use_only_cookies') " +
                    "|| !(bool)ini_get('session.use_strict_mode')) { " +
                    "throw new \\RuntimeException('Failed to enable secure PHP session settings.'); } " +
                    "if (!session_set_cookie_params(" + cookieOptions + ")) { " +
                    "throw new \\RuntimeException('Failed to configure the PHP session cookie.'); } " +
                    "if (!session_start()) { throw new \\RuntimeException('Failed to start the PHP session.'); } " +
                    "if (!setcookie(session_name(), session_id(), " + emittedCookieOptions + ")) { " +
                    "throw new \\RuntimeException('Failed to issue the session cookie with the requested security settings.'); } " +
                    "} else { throw new \\RuntimeException('PHP sessions are disabled.'); } ?>"
                if state.SessionPrologueRegistered then
                    context.codewritein("<?php ", startCode)
                else
                    context.prependRaw("<?php " + startCode + Environment.NewLine)
                    state.SessionPrologueRegistered <- true
                state.SessionOptions <- Some options)

    /// Replaces the active session identifier, deletes the old session data file,
    /// and reissues the cookie with the settings supplied to Start.
    member _.RegenerateId() =
        let options = SecurityCode.getStartedOptions context "session.RegenerateId"
        /// Builds cookie options emitted by the generated response.
        let emittedCookieOptions = SecurityCode.emittedCookieOptions options
        context.codewritein(
            "<?php ",
            "if (session_status() !== PHP_SESSION_ACTIVE) { " +
            "throw new \\RuntimeException('Cannot regenerate an inactive PHP session.'); } " +
            "if (!session_regenerate_id(true)) { " +
            "throw new \\RuntimeException('Failed to regenerate the PHP session identifier.'); } " +
            "$aqualisSessionHeaderFile = ''; $aqualisSessionHeaderLine = 0; " +
            "if (headers_sent($aqualisSessionHeaderFile, $aqualisSessionHeaderLine) " +
            "|| !setcookie(session_name(), session_id(), " + emittedCookieOptions + ")) { " +
            "throw new \\RuntimeException('Failed to reissue the regenerated session cookie with the requested security settings.'); } ?>")

    /// Clears the session data and destroys the active PHP session.
    member _.Destroy() =
        SecurityCode.requireStarted context "session.Destroy"
        context.codewritein(
            "<?php ",
            "if (session_status() === PHP_SESSION_ACTIVE) { " +
            "$_SESSION = []; " +
            "if (ini_get('session.use_cookies')) { " +
            "$sessionCookieParams = session_get_cookie_params(); " +
            "setcookie(session_name(), '', [" +
            "'expires' => time() - 42000, " +
            "'path' => $sessionCookieParams['path'], " +
            "'domain' => $sessionCookieParams['domain'], " +
            "'secure' => $sessionCookieParams['secure'], " +
            "'httponly' => $sessionCookieParams['httponly'], " +
            "'samesite' => $sessionCookieParams['samesite'] ?? 'Lax']); " +
            "} " +
            "session_destroy(); } ?>")
        let state = SecurityGenerationStates.get context
        lock state.Gate (fun () ->
            state.SessionOptions <- None
            state.CsrfTokenEmitted <- false)

    /// Destroys the session and redirects; the generated branch cannot continue.
    /// Restores generation state so sibling branches can still use the session.
    member this.DestroyAndRedirect(location:Url,status:RedirectStatus) =
        let state = SecurityGenerationStates.get context
        let options = SecurityCode.getStartedOptions context "session.DestroyAndRedirect"
        let csrfTokenEmitted = lock state.Gate (fun () -> state.CsrfTokenEmitted)
        try
            this.Destroy()
            context.php.redirect(location,status)
        finally
            lock state.Gate (fun () ->
                state.SessionOptions <- Some options
                state.CsrfTokenEmitted <- csrfTokenEmitted)

    /// Returns an expression for a value in the PHP session array.
    member _.Item(key:string) =
        if isNull key then nullArg (nameof key)
        PHPdata.f("$_SESSION[" + PhpEncoding.stringLiteral key + "]", context)

    /// Tests whether a value exists in the PHP session array.
    member this.Isset(key:string) =
        ContextPhp(context).isset(this.Item key)

/// Emits synchronizer-token CSRF protection for POST forms.
type CsrfProtection internal (context:Aqualis, session:WebSession) =
    /// Gets the owning generation context.
    member internal _.Context = context

    /// Gets the session entry containing the CSRF token.
    member private _.Token = session.Item SecurityCode.CsrfTokenName

    /// Registers unconditional token initialization in the file prologue once.
    member this.EnsureToken() =
        SecurityCode.requireStarted context "csrf.EnsureToken"
        let state = SecurityGenerationStates.get context

        lock state.Gate (fun () ->
            if not state.CsrfTokenEmitted then
                let token = this.Token.code
                let ensureCode =
                    "if (!isset(" + token + ") || !is_string(" + token + ") || " +
                    "preg_match('/\\A[0-9a-f]{64}\\z/D', " + token + ") !== 1) { " +
                    token + " = bin2hex(random_bytes(32)); } ?>"
                if state.CsrfPrologueRegistered then
                    context.codewritein("<?php ", ensureCode)
                else
                    context.prependRaw("<?php " + ensureCode + Environment.NewLine)
                    state.CsrfPrologueRegistered <- true
                state.CsrfTokenEmitted <- true)

    /// Replaces the current CSRF token, for example after a privilege change.
    member this.RotateToken() =
        SecurityCode.requireStarted context "csrf.RotateToken"
        this.EnsureToken()
        let state = SecurityGenerationStates.get context

        lock state.Gate (fun () ->
            context.codewritein(
                "<?php ",
                this.Token.code + " = bin2hex(random_bytes(32)); ?>")
            state.CsrfTokenEmitted <- true)

    /// Returns the PHP expression that validates the submitted token.
    member this.IsValidPost =
        SecurityCode.requireStarted context "csrf.IsValidPost"
        let sessionToken = this.Token.code
        let postToken = "$_POST[" + PhpEncoding.stringLiteral SecurityCode.CsrfTokenName + "]"
        let expression =
            "isset(" + sessionToken + ") && is_string(" + sessionToken + ") && " +
            "preg_match('/\\A[0-9a-f]{64}\\z/D', " + sessionToken + ") === 1 && " +
            "isset(" + postToken + ") && is_string(" + postToken + ") && " +
            "preg_match('/\\A[0-9a-f]{64}\\z/D', " + postToken + ") === 1 && " +
            "hash_equals(" + sessionToken + ", " + postToken + ")"
        bool0(Var(Nt, expression, NaN), context)

    /// Emits an HTML-escaped hidden field containing the current session token.
    member this.Field() =
        let state = SecurityGenerationStates.get context
        lock state.Gate (fun () ->
            if not state.CsrfTokenEmitted then
                invalidOp "csrf.Field requires csrf.EnsureToken or csrf.RotateToken to be called first.")

        context.html.taga(
            "input",
            [ "type", PHPdata "hidden"
              "name", PHPdata SecurityCode.CsrfTokenName
              "value", this.Token ])

    /// Rejects an invalid POST request with HTTP 403 and terminates the generated script.
    member this.RequireValidPost() =
        let state = SecurityGenerationStates.get context
        lock state.Gate (fun () ->
            if not state.CsrfTokenEmitted then
                invalidOp "csrf.RequireValidPost requires csrf.EnsureToken or csrf.RotateToken to be called first.")

        let methodExpression = "$_SERVER[" + PhpEncoding.stringLiteral "REQUEST_METHOD" + "]"
        context.codewritein(
            "<?php ",
            "if (" + methodExpression + " === 'POST' && !(" + this.IsValidPost.code + ")) { " +
            "http_response_code(403); exit('Invalid CSRF token.'); } ?>")

/// A session key carrying the expected PHP value category.
[<Struct>]
type SessionKey<'T> =
    private
    | SessionKey of FieldName
    /// Gets the validated session key name.
    member internal this.Name =
        let (SessionKey fieldName) = this
        FieldName.value fieldName

/// Constructors for typed PHP session keys.
[<RequireQualifiedAccess>]
module SessionKey =
    /// Creates a string-valued session key.
    let string name : SessionKey<PhpString> = SessionKey(FieldName.create name)

    /// Creates an integer-valued session key.
    let integer name : SessionKey<PhpInt> = SessionKey(FieldName.create name)

    /// Creates a floating-point-valued session key.
    let floatingPoint name : SessionKey<PhpFloat> = SessionKey(FieldName.create name)

/// A PHP session that has already been configured and started.
type ActiveSession internal (session:WebSession) =
    /// Gets the underlying active session.
    member internal _.Session = session

    /// Gets a typed expression for a value in this session.
    member _.Get(key:SessionKey<'T>) =
        PhpExpr.ofUntyped<'T> (session.Item key.Name)

    /// Tests whether a typed session key exists.
    member _.Isset(key:SessionKey<'T>) =
        session.Isset key.Name

    /// Sets a typed session value.
    member _.Set(key:SessionKey<'T>, value:PhpExpr<'T>) =
        session.Item(key.Name) <== value.Untyped

    /// Sets a string-valued session value.
    member _.Set(key:SessionKey<PhpString>, value:string) =
        session.Item(key.Name) <== value

    /// Regenerates the active PHP session identifier.
    member _.RegenerateId() = session.RegenerateId()
    /// Destroys the active PHP session.
    member _.Destroy() = session.Destroy()
    /// Destroys the session and redirects the response.
    member _.DestroyAndRedirect(location:Url,status:RedirectStatus) =
        session.DestroyAndRedirect(location,status)

    /// Ensures that a CSRF token exists and returns an initialized token capability.
    member _.CsrfToken() =
        let protection = CsrfProtection(session.Context, session)
        protection.EnsureToken()
        CsrfToken(protection)

/// An initialized CSRF token that is safe to validate or render.
and CsrfToken internal (protection:CsrfProtection) =
    /// Gets the underlying CSRF protection helper.
    member internal _.Protection = protection
    /// Gets the owning generation context.
    member internal _.Context = protection.Context
    /// Gets the generated CSRF validity expression for a POST request.
    member _.IsValidPost = protection.IsValidPost
    /// Rejects a POST request unless its CSRF token is valid.
    member _.RequireValidPost() = protection.RequireValidPost()
    /// Rotates the CSRF token.
    member _.Rotate() = protection.RotateToken()
    /// Renders a hidden form field carrying the CSRF token.
    member _.Field() = protection.Field()

/// Adds session and CSRF operations to PHP generation contexts.
[<AutoOpen>]
module SecurityExtensions =
    type ContextPhp with
        /// Session support associated with this PHP generation context.
        member this.session = WebSession(this.Context)

        /// CSRF protection associated with this PHP generation context.
        member this.csrf = CsrfProtection(this.Context, this.session)

        /// Starts a session and returns a capability that only represents an active session.
        member this.startSession(options:SessionOptions) =
            let session = this.session
            session.Start options
            ActiveSession(session)

    type html with
        /// Generates a POST form whose first element is a CSRF hidden field.
        member internal this.formWithCsrf(action:Url, csrf:CsrfProtection) = fun code ->
            if not (Object.ReferenceEquals(this.Context, csrf.Context)) then
                invalidArg (nameof csrf) "The form and CSRF protection must use the same generation context."
            this.form action <| fun () ->
                csrf.Field()
                code()

        /// Generates a multipart POST form whose first element is a CSRF hidden field.
        member internal this.formFileUploadWithCsrf(action:Url, csrf:CsrfProtection) = fun code ->
            if not (Object.ReferenceEquals(this.Context, csrf.Context)) then
                invalidArg (nameof csrf) "The form and CSRF protection must use the same generation context."
            this.form_fileUpload action <| fun () ->
                csrf.Field()
                code()

        /// Generates a CSRF-protected POST form with a validated action URL.
        member this.postForm(action:Url, csrf:CsrfToken) = fun code ->
            if not (Object.ReferenceEquals(this.Context, csrf.Context)) then
                invalidArg (nameof csrf) "The form and CSRF token must use the same generation context."
            this.formWithCsrf(action, csrf.Protection) code

        /// Generates a CSRF-protected multipart form with a validated action URL.
        member this.postFileUploadForm(action:Url, csrf:CsrfToken) = fun code ->
            if not (Object.ReferenceEquals(this.Context, csrf.Context)) then
                invalidArg (nameof csrf) "The form and CSRF token must use the same generation context."
            this.formFileUploadWithCsrf(action, csrf.Protection) code
