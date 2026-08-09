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
    type ContextBr internal (c:Aqualis) =
        member _.branch code = expr.branch c (fun callbacks -> code (br callbacks))

        member this.if1 (condition:bool0) code =
            match c.CodeFile with
            |None ->
                match condition.Expr.simp with
                |True -> code()
                |False -> ()
                |_ -> invalidOp "The Numeric branch condition could not be evaluated."
            |Some _ ->
                Aqualis.merge c condition.Context |> ignore
                match c.language, condition.Expr.simp with
                |(LaTeX|HTML), _ -> this.branch (fun branch -> branch.IF condition code)
                |_, True -> code()
                |_, False -> ()
                |_, _ -> this.branch (fun branch -> branch.IF condition code)

        member this.if2 (condition:bool0) codeWhenTrue codeWhenFalse =
            match c.CodeFile with
            |None ->
                match condition.Expr.simp with
                |True -> codeWhenTrue()
                |False -> codeWhenFalse()
                |_ -> invalidOp "The Numeric branch condition could not be evaluated."
            |Some _ ->
                Aqualis.merge c condition.Context |> ignore
                match c.language, condition.Expr.simp with
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
        type Aqualis with
            member this.br = ContextBr this
