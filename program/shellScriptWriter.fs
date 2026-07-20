//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

open System.IO
open System.Text

[<RequireQualifiedAccess>]
module internal ShellScriptWriter =
    let create path =
        let writer = new StreamWriter(path, false, UTF8Encoding(false))
        writer.NewLine <- "\n"
        writer
