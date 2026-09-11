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
    Lifetime: int
    Path: string
    Secure: bool
    HttpOnly: bool
    SameSite: SameSite
}

[<RequireQualifiedAccess>]
module SessionOptions =
    /// Secure defaults for an HTTPS production site.
    let production = {
        Lifetime = 0
        Path = "/"
        Secure = true
        HttpOnly = true
        SameSite = SameSite.Lax
    }

    /// Defaults suitable for local HTTP development.
    let development = {
        production with Secure = false
    }

type private SecurityGenerationState() =
    member val SessionOptions: SessionOptions option = None with get, set
    member val CsrfTokenEmitted = false with get, set
    member val Gate = obj() with get

[<RequireQualifiedAccess>]
module private SecurityGenerationStates =
    let private states = ConditionalWeakTable<Aqualis, SecurityGenerationState>()
    let get (context:Aqualis) = states.GetOrCreateValue context

[<RequireQualifiedAccess>]
module private SecurityCode =
    [<Literal>]
    let CsrfTokenName = "_aqualis_csrf"

    let boolLiteral value = if value then "true" else "false"

    let sameSiteLiteral = function
        | SameSite.Lax -> "Lax"
        | SameSite.Strict -> "Strict"

    let validateSessionOptions options =
        if options.Lifetime < 0 then
            invalidArg (nameof options) "Session lifetime must be non-negative."
        if isNull options.Path || String.IsNullOrWhiteSpace options.Path then
            invalidArg (nameof options) "A session cookie path is required."

    let requireStarted context operation =
        let state = SecurityGenerationStates.get context
        lock state.Gate (fun () ->
            if state.SessionOptions.IsNone then
                invalidOp (operation + " requires session.Start to be called first."))

/// Emits PHP session operations for one Aqualis generation context.
type WebSession internal (context:Aqualis) =
    member internal _.Context = context

    /// Configures the session cookie and starts a session when one is not active.
    member _.Start(options:SessionOptions) =
        SecurityCode.validateSessionOptions options
        let state = SecurityGenerationStates.get context

        lock state.Gate (fun () ->
            match state.SessionOptions with
            | Some existing when existing = options -> ()
            | Some _ ->
                invalidOp "The session has already been started with different options in this generation context."
            | None ->
                let cookieOptions =
                    "[" +
                    "'lifetime' => " + string options.Lifetime + ", " +
                    "'path' => " + PhpEncoding.stringLiteral options.Path + ", " +
                    "'secure' => " + SecurityCode.boolLiteral options.Secure + ", " +
                    "'httponly' => " + SecurityCode.boolLiteral options.HttpOnly + ", " +
                    "'samesite' => " + PhpEncoding.stringLiteral (SecurityCode.sameSiteLiteral options.SameSite) +
                    "]"

                context.codewritein(
                    "<?php ",
                    "session_set_cookie_params(" + cookieOptions + "); " +
                    "if (session_status() !== PHP_SESSION_ACTIVE) { session_start(); } ?>")
                state.SessionOptions <- Some options)

    /// Replaces the active session identifier and deletes the old session data file.
    member _.RegenerateId() =
        SecurityCode.requireStarted context "session.RegenerateId"
        context.codewritein(
            "<?php ",
            "if (session_status() === PHP_SESSION_ACTIVE) { session_regenerate_id(true); } ?>")

    /// Clears the session data and destroys the active PHP session.
    member _.Destroy() =
        SecurityCode.requireStarted context "session.Destroy"
        context.codewritein(
            "<?php ",
            "if (session_status() === PHP_SESSION_ACTIVE) { $_SESSION = []; session_destroy(); } ?>")

    /// Returns an expression for a value in the PHP session array.
    member _.Item(key:string) =
        if isNull key then nullArg (nameof key)
        PHPdata.f("$_SESSION[" + PhpEncoding.stringLiteral key + "]", context)

    /// Tests whether a value exists in the PHP session array.
    member this.Isset(key:string) =
        ContextPhp(context).isset(this.Item key)

/// Emits synchronizer-token CSRF protection for POST forms.
type CsrfProtection internal (context:Aqualis, session:WebSession) =
    member internal _.Context = context

    member private _.Token = session.Item SecurityCode.CsrfTokenName

    /// Creates a cryptographically random session token once when it is absent or invalid.
    member this.EnsureToken() =
        SecurityCode.requireStarted context "csrf.EnsureToken"
        let state = SecurityGenerationStates.get context

        lock state.Gate (fun () ->
            if not state.CsrfTokenEmitted then
                let token = this.Token.code
                context.codewritein(
                    "<?php ",
                    "if (!isset(" + token + ") || !is_string(" + token + ")) { " +
                    token + " = bin2hex(random_bytes(32)); } ?>")
                state.CsrfTokenEmitted <- true)

    /// Replaces the current CSRF token, for example after a privilege change.
    member this.RotateToken() =
        SecurityCode.requireStarted context "csrf.RotateToken"
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
            "isset(" + postToken + ") && is_string(" + postToken + ") && " +
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
    member internal this.Name =
        let (SessionKey fieldName) = this
        FieldName.value fieldName

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

    member _.RegenerateId() = session.RegenerateId()
    member _.Destroy() = session.Destroy()

    /// Ensures that a CSRF token exists and returns an initialized token capability.
    member _.CsrfToken() =
        let protection = CsrfProtection(session.Context, session)
        protection.EnsureToken()
        CsrfToken(protection)

/// An initialized CSRF token that is safe to validate or render.
and CsrfToken internal (protection:CsrfProtection) =
    member internal _.Protection = protection
    member internal _.Context = protection.Context
    member _.IsValidPost = protection.IsValidPost
    member _.RequireValidPost() = protection.RequireValidPost()
    member _.Rotate() = protection.RotateToken()
    member _.Field() = protection.Field()

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
        member this.formWithCsrf(action:string, csrf:CsrfProtection) = fun code ->
            if not (Object.ReferenceEquals(this.Context, csrf.Context)) then
                invalidArg (nameof csrf) "The form and CSRF protection must use the same generation context."
            this.form action <| fun () ->
                csrf.Field()
                code()

        /// Generates a multipart POST form whose first element is a CSRF hidden field.
        member this.formFileUploadWithCsrf(action:string, csrf:CsrfProtection) = fun code ->
            if not (Object.ReferenceEquals(this.Context, csrf.Context)) then
                invalidArg (nameof csrf) "The form and CSRF protection must use the same generation context."
            this.form_fileUpload action <| fun () ->
                csrf.Field()
                code()

        /// Generates a CSRF-protected POST form with a validated action URL.
        member this.postForm(action:Url, csrf:CsrfToken) = fun code ->
            if not (Object.ReferenceEquals(this.Context, csrf.Context)) then
                invalidArg (nameof csrf) "The form and CSRF token must use the same generation context."
            this.formWithCsrf(Url.value action, csrf.Protection) code

        /// Generates a CSRF-protected multipart form with a validated action URL.
        member this.postFileUploadForm(action:Url, csrf:CsrfToken) = fun code ->
            if not (Object.ReferenceEquals(this.Context, csrf.Context)) then
                invalidArg (nameof csrf) "The form and CSRF token must use the same generation context."
            this.formFileUploadWithCsrf(Url.value action, csrf.Protection) code
