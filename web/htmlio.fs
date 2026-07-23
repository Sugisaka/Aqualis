namespace Aqualis

type ContextHtmlIo internal (environment:Aqualis) =
    let context = environment.RequireGenerationContext()

    member private _.WithProgram index code =
        context.WithProgram(index, fun child -> code (Aqualis(Some child)))

    member this.switchMain code = this.WithProgram 0 code
    member this.switchBody code = this.WithProgram 1 code
    member this.switchJSMain code = this.WithProgram 2 code
    member this.switchAnimationSeq code = this.WithProgram 3 code
    member this.switchJSAnimationStart code = this.WithProgram 4 code
    member this.switchJSAnimationSeqReset code = this.WithProgram 5 code
    member this.switchJSAnimationReset code = this.WithProgram 6 code
    member this.switchAutoAnimation code = this.WithProgram 7 code

    member _.nextContentsID() =
        "contentsID" + context.NextContentsNumber().ToString()

    member _.nextAnimationSeqID() =
        let number = context.NextAnimationSequenceNumber()
        "animationSeqID" + number.ToString(), "animationSeqResetID" + number.ToString()

    member _.nextAnimationGroup() = context.NextAnimationGroupNumber().ToString()
    member _.animationButtonReset() = context.ClearAnimationButtons()
    member _.addAnimationButton(fnameStart,fnameReset,buttonX,buttonY) =
        context.AddAnimationButton(fnameStart,fnameReset,buttonX,buttonY)

    member this.addAutoAnimation(fnameStart,_) =
        this.switchAutoAnimation(fun child ->
            child.RequireGenerationContext().CurrentProgram.codewritein("animationStartMap['"+fnameStart+"']();"))

[<AutoOpen>]
module CompilationEnvironmentHtmlIoExtensions =
    type Aqualis with
        member this.htmlio = ContextHtmlIo(this)
