//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    ///<summary>反復処理</summary>
    type ContextIter internal (c:Aqualis) =
        /// Emits an unbounded loop and passes its exit action and index to the callback.
        member _.loop code =
            match c.CodeFile with
            |Some _ ->
                expr.loop c (fun (exitLoop, index) ->
                    code(exitLoop, int0(index, c)))
            |None -> invalidOp "An unbounded loop is not supported during Numeric execution."

        /// Runs or emits a loop while the Boolean condition is true.
        member _.whiledo (condition:bool0) code =
            match c.CodeFile with
            |Some _ ->
                Aqualis.merge c condition.Context |> ignore
                expr.whiledo c condition.Expr code
            |None ->
                let mutable keepRunning = true
                while keepRunning do
                    match condition.Expr.simp with
                    |True -> code()
                    |False -> keepRunning <- false
                    |_ -> invalidOp "The Numeric loop condition could not be evaluated."

        /// Iterates inclusively from the first index to the last index.
        member _.range (first:int0, last:int0) = fun code ->
            match c.CodeFile with
            |Some _ ->
                Aqualis.mergeMany [c; first.Context; last.Context] |> ignore
                expr.range c None first.Expr last.Expr (fun index ->
                    code (int0(index, c)))
            |None ->
                Aqualis.merge first.Context last.Context |> ignore
                expr.rangeN first.Expr last.Expr (fun index -> code (int0 index))

        /// Iterates inclusively between integer bounds.
        member this.range (first:int, last:int) = fun code ->
            this.range (int0(Int first), int0(Int last)) code

        /// Iterates inclusively between numeric expression and integer bounds.
        member this.range (first:int0, last:int) = fun code ->
            this.range (first, int0(Int last)) code

        /// Iterates inclusively between integer and numeric expression bounds.
        member this.range (first:int, last:int0) = fun code ->
            this.range (int0(Int first), last) code

        /// Iterates inclusively using the specified generated counter name.
        member _.range (counterName:string, first:int0, last:int0) = fun code ->
            match c.CodeFile with
            |Some _ ->
                Aqualis.mergeMany [c; first.Context; last.Context] |> ignore
                expr.range c (Some counterName) first.Expr last.Expr (fun index ->
                    code (int0(index, c)))
            |None ->
                Aqualis.merge first.Context last.Context |> ignore
                expr.rangeN first.Expr last.Expr (fun index -> code (int0 index))

        /// Iterates inclusively and passes an early-exit action to the callback.
        member _.range_exit (first:int0, last:int0) = fun code ->
            match c.CodeFile with
            |Some _ ->
                Aqualis.mergeMany [c; first.Context; last.Context] |> ignore
                expr.range_exit c None first.Expr last.Expr (fun (exitLoop,index) ->
                    code(exitLoop,int0(index,c)))
            |None -> invalidOp "An early-exit loop is not supported during Numeric execution."

        /// Iterates inclusively with a named counter and an early-exit action.
        member _.range_exit (counterName:string, first:int0, last:int0) = fun code ->
            match c.CodeFile with
            |Some _ ->
                Aqualis.mergeMany [c; first.Context; last.Context] |> ignore
                expr.range_exit c (Some counterName) first.Expr last.Expr (fun (exitLoop,index) ->
                    code(exitLoop,int0(index,c)))
            |None -> invalidOp "An early-exit loop is not supported during Numeric execution."

        /// Iterates over zero-based indices below the specified count.
        member this.num (count:int0) = fun code ->
            this.range (int0(Int 0), count - 1) code

        /// Iterates over zero-based indices below the specified integer count.
        member this.num (count:int) = fun code ->
            this.num (int0(Int count)) code

        /// Iterates over zero-based indices using a named counter.
        member this.num (count:int0,counterName:string) = fun code ->
            this.range (counterName,int0(Int 0),count - 1) code

        /// Iterates over zero-based indices with an early-exit action.
        member this.num_exit (count:int0) = fun code ->
            this.range_exit (int0(Int 0),count - 1) code

        /// Iterates over zero-based indices with a named counter and early-exit action.
        member this.num_exit (count:int0,counterName:string) = fun code ->
            this.range_exit (counterName,int0(Int 0),count - 1) code

    [<AutoOpen>]
    /// Adds iteration helpers to Aqualis.
    module CompilationEnvironmentIterExtensions =
        type Aqualis with
            ///<summary>反復処理</summary>
            member this.iter = ContextIter this
