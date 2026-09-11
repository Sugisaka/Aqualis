//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

/// A cell rendered by the typed HTML table API.
[<RequireQualifiedAccess>]
type TableCell =
    /// Static text. HTML-sensitive characters are encoded during generation.
    | Text of string
    /// A PHP value. The value is HTML-encoded when the generated PHP runs.
    | Php of PHPdata
