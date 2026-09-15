//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

/// HTTP failure statuses that may be emitted by a generated PHP application.
type HttpFailureStatus =
    | BadRequest
    | Forbidden
    | TooManyRequests
    | ServiceUnavailable

[<RequireQualifiedAccess>]
module private HttpFailureStatusCode =
    let value = function
        | BadRequest -> 400
        | Forbidden -> 403
        | TooManyRequests -> 429
        | ServiceUnavailable -> 503

/// Emits terminal HTTP responses for generated PHP applications.
type PhpResponse internal (context:Aqualis) =
    let requireSameContext other =
        Aqualis.merge context other |> ignore

    /// Terminates the request unless condition is true. The public message is
    /// encoded as a PHP literal; an optional diagnostic is written only to the log.
    member _.Require(condition:bool0, status:HttpFailureStatus, publicMessage:string, ?logMessage:PHPdata) =
        requireSameContext condition.Context
        logMessage |> Option.iter (fun message -> requireSameContext message.Context)
        context.php.phpcode <| fun () ->
            context.writei("if (!(" + condition.code + ")) {")
            logMessage
            |> Option.iter (fun message -> context.writei("error_log((string)(" + message.code + "));"))
            context.writei("http_response_code(" + string (HttpFailureStatusCode.value status) + ");")
            context.writei("exit(" + PhpEncoding.stringLiteral publicMessage + ");")
            context.writei "}"

    /// Terminates the request when condition is true.
    member this.RejectIf(condition:bool0, status:HttpFailureStatus, publicMessage:string, ?logMessage:PHPdata) =
        let accepted = bool0(Var(Nt,"!(" + condition.code + ")",NaN),condition.Context)
        this.Require(accepted,status,publicMessage,?logMessage=logMessage)

[<AutoOpen>]
module ResponseExtensions =
    type Aqualis with
        /// Terminal HTTP response generation associated with this context.
        member this.response = PhpResponse(this)
