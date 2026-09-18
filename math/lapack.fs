//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    open System

    /// Adds linear-algebra operations to the generation context.
    [<AutoOpen>]
    module CompilationEnvironmentLaExtensions =
        type Aqualis with
            /// Gets LAPACK-backed linear-algebra helpers bound to this context.
            member this.la = ContextLa(this)
