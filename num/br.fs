//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    ///<summary>条件分岐</summary>
    type br (ifcode,elseifcode,elsecode) =

        ///<summary>条件分岐式のカウンタ</summary>
        let mutable con = 1

        ///<summary>条件式(if)</summary>
        member __.IF (cond:bool0) code =
            if con=0 then
                printfn "ELの後のIFは無視されます"
            elif con=1 then
                ifcode cond.Expr code
            else
                elseifcode cond.Expr code
            con <- con + 1

        ///<summary>条件式(else)</summary>
        member __.EL code =
            elsecode code
            con <- 0

        ///<summary>条件分岐式(2番目以降のIFは前のIFを満たさない場合のみ評価)</summary>
    type ContextBr internal (environment:CompilationEnvironment) =
        let context = environment.GenerationContext

        member _.branch code =
            match context with
            |Some ctx -> expr.branch ctx (fun callbacks -> code (br callbacks))
            |None -> invalidOp "A symbolic branch builder is not available during Numeric execution."

        member this.if1 (condition:bool0) code =
            match context with
            |None ->
                match condition.Expr.simp with
                |True -> code()
                |False -> ()
                |_ -> invalidOp "The Numeric branch condition could not be evaluated."
            |Some ctx ->
                GenerationContextMerge.merge (Some ctx) condition.Context |> ignore
                match ctx.CurrentProgram.language, condition.Expr.simp with
                |(LaTeX|HTML), _ -> this.branch (fun branch -> branch.IF condition code)
                |_, True -> code()
                |_, False -> ()
                |_, _ -> this.branch (fun branch -> branch.IF condition code)

        member this.if2 (condition:bool0) codeWhenTrue codeWhenFalse =
            match context with
            |None ->
                match condition.Expr.simp with
                |True -> codeWhenTrue()
                |False -> codeWhenFalse()
                |_ -> invalidOp "The Numeric branch condition could not be evaluated."
            |Some ctx ->
                GenerationContextMerge.merge (Some ctx) condition.Context |> ignore
                match ctx.CurrentProgram.language, condition.Expr.simp with
                |(LaTeX|HTML), _ ->
                    this.branch (fun branch ->
                        branch.IF condition codeWhenTrue
                        branch.EL codeWhenFalse)
                |_, True -> codeWhenTrue()
                |_, False -> codeWhenFalse()
                |_, _ ->
                    this.branch (fun branch ->
                        branch.IF condition codeWhenTrue
                        branch.EL codeWhenFalse)

    [<AutoOpen>]
    module CompilationEnvironmentBrExtensions =
        type CompilationEnvironment with
            member this.br = ContextBr(this)

