//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

/// A typed POST value together with generated presence and validation expressions.
type ValidatedPost<'T> internal (isPresent:bool0, isValid:bool0, value:PhpExpr<'T>) =
    /// Gets a generated expression that checks whether the field was submitted.
    member _.IsPresent = isPresent
    /// Gets a generated expression that checks whether the field passes validation.
    member _.IsValid = isValid
    /// Gets the typed PHP expression for the submitted value.
    member _.Value = value

[<RequireQualifiedAccess>]
/// Builds PHP expressions for typed request validation.
module private RequestCode =
    /// Builds a PHP expression for a POST field.
    let postExpression fieldName =
        "$_POST[" + PhpEncoding.stringLiteral (FieldName.value fieldName) + "]"

    /// Wraps a PHP condition as a Boolean expression.
    let boolean context expression =
        bool0(Var(Nt, "(" + expression + ")", NaN), context)

    /// Appends a lower-bound validation when configured.
    let optionalMinimum expression = function
        | Some minimum -> " && " + expression + " >= " + string minimum
        | None -> ""

    /// Appends an upper-bound validation when configured.
    let optionalMaximum expression = function
        | Some maximum -> " && " + expression + " <= " + string maximum
        | None -> ""

    /// Rejects a minimum greater than its maximum.
    let validateRange argumentName minimum maximum =
        match minimum, maximum with
        | Some lower, Some upper when lower > upper ->
            invalidArg argumentName "The minimum cannot be greater than the maximum."
        | _ -> ()

/// Typed access to POST request fields.
type PostRequest internal (context:Aqualis) =
    /// Creates a required UTF-8 text field with optional character and encoded-byte bounds.
    member _.RequiredText(fieldName:FieldName, ?minLength:int, ?maxLength:int, ?maxUtf8Bytes:int) =
        let minimum = defaultArg minLength 1
        if minimum < 0 then invalidArg (nameof minLength) "The minimum length must be non-negative."
        match maxLength with
        | Some maximum when maximum < minimum ->
            invalidArg (nameof maxLength) "The maximum length cannot be less than the minimum length."
        | _ -> ()
        match maxUtf8Bytes with
        | Some maximum when maximum < 0 ->
            invalidArg (nameof maxUtf8Bytes) "The maximum UTF-8 byte count must be non-negative."
        | _ -> ()

        let expression = RequestCode.postExpression fieldName
        let length = "preg_match_all('/./us', " + expression + ")"
        let present = RequestCode.boolean context ("isset(" + expression + ")")
        let validExpression =
            "isset(" + expression + ") && is_string(" + expression + ")" +
            " && preg_match('//u', " + expression + ") === 1" +
            " && " + length + " >= " + string minimum +
            (match maxLength with
             | Some maximum -> " && " + length + " <= " + string maximum
             | None -> "") +
            (match maxUtf8Bytes with
             | Some maximum -> " && strlen(" + expression + ") <= " + string maximum
             | None -> "")
        ValidatedPost<PhpString>(
            present,
            RequestCode.boolean context validExpression,
            PhpExpr.ofUntyped<PhpString> (PHPdata.f(expression,context)))

    /// Creates a required integer field with optional inclusive bounds.
    member _.RequiredInt(fieldName:FieldName, ?minimum:int, ?maximum:int) =
        RequestCode.validateRange (nameof minimum) minimum maximum
        let expression = RequestCode.postExpression fieldName
        let converted = "(int)(" + expression + ")"
        let present = RequestCode.boolean context ("isset(" + expression + ")")
        let validExpression =
            "isset(" + expression + ") && is_string(" + expression + ")" +
            " && filter_var(" + expression + ", FILTER_VALIDATE_INT) !== false" +
            RequestCode.optionalMinimum converted minimum +
            RequestCode.optionalMaximum converted maximum
        ValidatedPost<PhpInt>(
            present,
            RequestCode.boolean context validExpression,
            PhpExpr.ofUntyped<PhpInt> (PHPdata.f(converted,context)))

    /// Creates a required floating-point field with optional inclusive bounds.
    member _.RequiredFloat(fieldName:FieldName, ?minimum:float, ?maximum:float) =
        RequestCode.validateRange (nameof minimum) minimum maximum
        let expression = RequestCode.postExpression fieldName
        let converted = "(float)(" + expression + ")"
        let present = RequestCode.boolean context ("isset(" + expression + ")")
        let validExpression =
            "isset(" + expression + ") && is_string(" + expression + ")" +
            " && filter_var(" + expression + ", FILTER_VALIDATE_FLOAT) !== false" +
            (match minimum with
             | Some lower -> " && " + converted + " >= " + InvariantFormat.number lower
             | None -> "") +
            (match maximum with
             | Some upper -> " && " + converted + " <= " + InvariantFormat.number upper
             | None -> "")
        ValidatedPost<PhpFloat>(
            present,
            RequestCode.boolean context validExpression,
            PhpExpr.ofUntyped<PhpFloat> (PHPdata.f(converted,context)))

/// Typed access to values supplied by the current HTTP request.
type ContextRequest internal (context:Aqualis) =
    /// Gets typed access to POST fields in this generation context.
    member _.post = PostRequest(context)

[<AutoOpen>]
/// Adds typed request access to generation contexts.
module RequestExtensions =
    type Aqualis with
        /// Gets typed access to the current HTTP request.
        member this.request = ContextRequest(this)
