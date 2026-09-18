//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    open System

    /// Adds file input and output helpers to an Aqualis context.
    [<AutoOpen>]
    module CompilationEnvironmentIoExtensions =
        type Aqualis with
            /// Gets file input and output helpers bound to this context.
            member this.io = ContextIo(this)
