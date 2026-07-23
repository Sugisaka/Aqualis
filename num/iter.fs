//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    ///<summary>反復処理</summary>
    type ContextIter internal (environment:Aqualis) =
        let context = environment.GenerationContext

        member _.loop code =
            match context with
            |Some ctx ->
                expr.loop ctx (fun (exitLoop, index) ->
                    code(exitLoop, int0(index, context=ctx)))
            |None -> invalidOp "An unbounded loop is not supported during Numeric execution."

        member _.whiledo (condition:bool0) code =
            match context with
            |Some ctx ->
                GenerationContextMerge.merge (Some ctx) condition.Context |> ignore
                expr.whiledo ctx condition.Expr code
            |None ->
                let mutable keepRunning = true
                while keepRunning do
                    match condition.Expr.simp with
                    |True -> code()
                    |False -> keepRunning <- false
                    |_ -> invalidOp "The Numeric loop condition could not be evaluated."

        member _.range (first:int0, last:int0) = fun code ->
            match context with
            |Some ctx ->
                GenerationContextMerge.mergeMany [Some ctx; first.Context; last.Context] |> ignore
                expr.range ctx None first.Expr last.Expr (fun index ->
                    code (int0(index, context=ctx)))
            |None ->
                GenerationContextMerge.merge first.Context last.Context |> ignore
                expr.rangeN first.Expr last.Expr (fun index -> code (int0 index))

        member this.range (first:int, last:int) = fun code ->
            this.range (int0(Int first), int0(Int last)) code

        member this.range (first:int0, last:int) = fun code ->
            this.range (first, int0(Int last)) code

        member this.range (first:int, last:int0) = fun code ->
            this.range (int0(Int first), last) code

        member _.range (counterName:string, first:int0, last:int0) = fun code ->
            match context with
            |Some ctx ->
                GenerationContextMerge.mergeMany [Some ctx; first.Context; last.Context] |> ignore
                expr.range ctx (Some counterName) first.Expr last.Expr (fun index ->
                    code (int0(index, context=ctx)))
            |None ->
                GenerationContextMerge.merge first.Context last.Context |> ignore
                expr.rangeN first.Expr last.Expr (fun index -> code (int0 index))

        member _.range_exit (first:int0, last:int0) = fun code ->
            match context with
            |Some ctx ->
                GenerationContextMerge.mergeMany [Some ctx; first.Context; last.Context] |> ignore
                expr.range_exit ctx None first.Expr last.Expr (fun (exitLoop,index) ->
                    code(exitLoop,int0(index,context=ctx)))
            |None -> invalidOp "An early-exit loop is not supported during Numeric execution."

        member _.range_exit (counterName:string, first:int0, last:int0) = fun code ->
            match context with
            |Some ctx ->
                GenerationContextMerge.mergeMany [Some ctx; first.Context; last.Context] |> ignore
                expr.range_exit ctx (Some counterName) first.Expr last.Expr (fun (exitLoop,index) ->
                    code(exitLoop,int0(index,context=ctx)))
            |None -> invalidOp "An early-exit loop is not supported during Numeric execution."

        member this.num (count:int0) = fun code ->
            this.range (int0(Int 0), count - 1) code

        member this.num (count:int) = fun code ->
            this.num (int0(Int count)) code

        member this.num (count:int0,counterName:string) = fun code ->
            this.range (counterName,int0(Int 0),count - 1) code

        member this.num_exit (count:int0) = fun code ->
            this.range_exit (int0(Int 0),count - 1) code

        member this.num_exit (count:int0,counterName:string) = fun code ->
            this.range_exit (counterName,int0(Int 0),count - 1) code

    [<AutoOpen>]
    module CompilationEnvironmentIterExtensions =
        type Aqualis with
            member this.iter = ContextIter(this)

    ///<summary>反復処理（処理スキップ）</summary>
