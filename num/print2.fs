//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    [<AutoOpen>]
    /// Array-output operations on the print context.
    module ContextPrintArrayExtensions =
        type ContextPrint with
            /// Checks that the array belongs to this output context.
            member private this.ValidateContext context =
                Aqualis.merge this.Environment context |> ignore

            /// Writes the numeric array to generated output.
            member this.t(s:int1) =
                this.ValidateContext s.Context
                this.Environment.iter.num s.size1 <| fun i -> this.tt <| i++s[i]

            /// Writes the numeric array to generated output.
            member this.t(s:double1) =
                this.ValidateContext s.Context
                this.Environment.iter.num s.size1 <| fun i -> this.tt <| i++s[i]

            /// Writes the numeric array to generated output.
            member this.t(s:complex1) =
                this.ValidateContext s.Context
                this.Environment.iter.num s.size1 <| fun i -> this.tt <| i++s[i]

            /// Writes the numeric array to generated output.
            member this.t(s:int2) =
                this.ValidateContext s.Context
                this.Environment.iter.num s.size1 <| fun i ->
                    this.Environment.iter.num s.size2 <| fun j -> this.tt <| i++j++s[i,j]

            /// Writes the numeric array to generated output.
            member this.t(s:double2) =
                this.ValidateContext s.Context
                this.Environment.iter.num s.size1 <| fun i ->
                    this.Environment.iter.num s.size2 <| fun j -> this.tt <| i++j++s[i,j]

            /// Writes the numeric array to generated output.
            member this.t(s:complex2) =
                this.ValidateContext s.Context
                this.Environment.iter.num s.size1 <| fun i ->
                    this.Environment.iter.num s.size2 <| fun j -> this.tt <| i++j++s[i,j]

            /// Writes the numeric array to generated output.
            member this.t(s:int3) =
                this.ValidateContext s.Context
                this.Environment.iter.num s.size1 <| fun i ->
                    this.Environment.iter.num s.size2 <| fun j ->
                        this.Environment.iter.num s.size3 <| fun k -> this.tt <| i++j++k++s[i,j,k]

            /// Writes the numeric array to generated output.
            member this.t(s:double3) =
                this.ValidateContext s.Context
                this.Environment.iter.num s.size1 <| fun i ->
                    this.Environment.iter.num s.size2 <| fun j ->
                        this.Environment.iter.num s.size3 <| fun k -> this.tt <| i++j++k++s[i,j,k]

            /// Writes the numeric array to generated output.
            member this.t(s:complex3) =
                this.ValidateContext s.Context
                this.Environment.iter.num s.size1 <| fun i ->
                    this.Environment.iter.num s.size2 <| fun j ->
                        this.Environment.iter.num s.size3 <| fun k -> this.tt <| i++j++k++s[i,j,k]
