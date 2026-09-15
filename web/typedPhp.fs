//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

/// Marker type for a PHP string expression.
[<Sealed>]
type PhpString private () = class end

/// Marker type for a PHP integer expression.
[<Sealed>]
type PhpInt private () = class end

/// Marker type for a PHP floating-point expression.
[<Sealed>]
type PhpFloat private () = class end

/// Marker type for a PHP boolean expression.
[<Sealed>]
type PhpBool private () = class end

/// Marker type for a PHP array expression whose elements have type 'T.
[<Sealed>]
type PhpArray<'T> private () = class end

/// A PHP expression carrying its value category at compile time.
type PhpExpr<'T> internal (value:PHPdata) =
    member internal _.Untyped = value
    member internal _.Code = value.code
    member _.Context = value.Context

[<RequireQualifiedAccess>]
module PhpExpr =
    /// Creates a typed PHP string literal.
    let stringLiteral (value:string) = PhpExpr<PhpString>(PHPdata value)

    /// Creates a typed PHP integer literal.
    let intLiteral value =
        PhpExpr<PhpInt>(PHPdata.f(InvariantFormat.integer (value:int), Aqualis.BlankWriter PHP))

    /// Creates a typed PHP floating-point literal.
    let floatLiteral value =
        let number = value:float
        if not (System.Double.IsFinite number) then
            invalidArg (nameof value) "A PHP floating-point literal must be finite."
        PhpExpr<PhpFloat>(PHPdata.f(InvariantFormat.number number, Aqualis.BlankWriter PHP))

    let internal ofUntyped<'T> value = PhpExpr<'T>(value)

[<RequireQualifiedAccess>]
module PhpIntExpr =
    /// Converts a typed PHP integer into Aqualis's numeric expression DSL.
    let numeric (value:PhpExpr<PhpInt>) = value.Untyped.int0

[<RequireQualifiedAccess>]
module PhpFloatExpr =
    /// Converts a typed PHP floating-point value into Aqualis's numeric expression DSL.
    let numeric (value:PhpExpr<PhpFloat>) = value.Untyped.double0

[<AutoOpen>]
module TypedPhpExtensions =
    type ContextPhp with
        /// Creates a typed PHP string variable.
        member this.stringVar(name:PhpVariableName) =
            PhpExpr<PhpString>(PHPdata.var(this.Context,PhpVariableName.value name))

        /// Creates a typed PHP integer variable.
        member this.intVar(name:PhpVariableName) =
            PhpExpr<PhpInt>(PHPdata.var(this.Context,PhpVariableName.value name))

        /// Creates a typed PHP floating-point variable.
        member this.floatVar(name:PhpVariableName) =
            PhpExpr<PhpFloat>(PHPdata.var(this.Context,PhpVariableName.value name))

        /// Emits a typed PHP string as escaped HTML text.
        member this.echoHtmlText(value:PhpExpr<PhpString>) =
            Aqualis.merge this.Context value.Context |> ignore
            this.echoHtmlText value.Untyped

        /// Compares an existing PHP string value with a typed PHP string using
        /// PHP's strict, case-sensitive equality operator.
        member this.stringEquals(left:PHPdata, right:PhpExpr<PhpString>) =
            let resultContext =
                Aqualis.mergeMany [this.Context; left.Context; right.Context]
            bool0(
                Var(Nt, "(" + left.code + " === " + right.Code + ")", NaN),
                resultContext)

        /// Verifies a typed submitted password against an existing PHP password hash.
        member this.password_verify(password:PhpExpr<PhpString>, passwordHash:PHPdata) =
            Aqualis.mergeMany [this.Context; password.Context; passwordHash.Context] |> ignore
            this.password_verify(password.Untyped,passwordHash)

        /// Emits a validated redirect and terminates the generated PHP script.
        member this.redirect(location:Url,status:RedirectStatus) =
            this.redirectValidated(location, RedirectStatus.code status)

        /// Emits an HTTP 303 See Other redirect to a validated URL.
        member this.redirect(location:Url) =
            this.redirect(location, RedirectStatus.SeeOther)

    type html with
        /// Emits a typed PHP string as escaped HTML text.
        member this.text(value:PhpExpr<PhpString>) =
            this.Context.php.echoHtmlText value

        /// Generates a link with validated URL and CSS class values.
        member this.link(url:Url, cssClass:CssClass) = fun code ->
            this.tagb(
                "a",
                [ Atr("class", CssClass.value cssClass)
                  Atr("href", Url.value url) ]) code
