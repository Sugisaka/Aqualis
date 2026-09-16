//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    open System

    [<AutoOpen>]
    module CompilationEnvironmentIoExtensions =
        type Aqualis with
            ///<summary>ファイル入出力</summary>
            member this.io = ContextIo(this)
