//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

/// A typed POST value together with generated presence and validation expressions.
type ValidatedPost<'T> internal (isPresent:bool0, isValid:bool0, value:PhpExpr<'T>) =
    member _.IsPresent = isPresent
    member _.IsValid = isValid
    member _.Value = value

[<RequireQualifiedAccess>]
module private RequestCode =
    let postExpression fieldName =
        "$_POST[" + PhpEncoding.stringLiteral (FieldName.value fieldName) + "]"

    let boolean context expression =
        bool0(Var(Nt, "(" + expression + ")", NaN), context)

    let optionalMinimum expression = function
        | Some minimum -> " && " + expression + " >= " + string minimum
        | None -> ""

    let optionalMaximum expression = function
        | Some maximum -> " && " + expression + " <= " + string maximum
        | None -> ""

    let validateRange argumentName minimum maximum =
        match minimum, maximum with
        | Some lower, Some upper when lower > upper ->
            invalidArg argumentName "The minimum cannot be greater than the maximum."
        | _ -> ()

/// Typed access to POST request fields.
type PostRequest internal (context:Aqualis) =
    /// Creates a required UTF-8 text field with optional length bounds.
    member _.RequiredText(fieldName:FieldName, ?minLength:int, ?maxLength:int) =
        let minimum = defaultArg minLength 1
        if minimum < 0 then invalidArg (nameof minLength) "The minimum length must be non-negative."
        match maxLength with
        | Some maximum when maximum < minimum ->
            invalidArg (nameof maxLength) "The maximum length cannot be less than the minimum length."
        | _ -> ()

        let expression = RequestCode.postExpression fieldName
        let length = "mb_strlen(" + expression + ", 'UTF-8')"
        let present = RequestCode.boolean context ("isset(" + expression + ")")
        let validExpression =
            "isset(" + expression + ") && is_string(" + expression + ")" +
            " && " + length + " >= " + string minimum +
            (match maxLength with
             | Some maximum -> " && " + length + " <= " + string maximum
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
    member _.post = PostRequest(context)

[<AutoOpen>]
module RequestExtensions =
    type Aqualis with
        member this.request = ContextRequest(this)
