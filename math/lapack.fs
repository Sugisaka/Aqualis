//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    open System

    [<AutoOpen>]
    module CompilationEnvironmentLaExtensions =
        type Aqualis with
            ///<summary>線形代数ライブラリ</summary>
            member this.la = ContextLa(this)
