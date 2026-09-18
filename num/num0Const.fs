//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

open System

/// Constructors and literal constants for scalar numeric expressions.
[<AutoOpen>]
module num0Const =
    /// Creates an integer expression from a literal.
    let I x = int0(Int x)
    /// Creates a double-precision expression from a literal.
    let D x = double0(Dbl x)
    /// Creates a complex expression from a literal.
    let Z x = complex0(Cpx x)
    /// Gets the imaginary-unit expression.
    let uj = complex0(Var(Zt,"uj",NaN))
    /// Gets the double-precision value of pi.
    let pi = double0(Dbl Math.PI)
    /// Gets the integer constant 0.
    let _0 = int0(Int 0)
    /// Gets the integer constant 1.
    let _1 = int0(Int 1)
    /// Gets the integer constant 2.
    let _2 = int0(Int 2)
    /// Gets the integer constant 3.
    let _3 = int0(Int 3)
    /// Gets the integer constant 4.
    let _4 = int0(Int 4)
    /// Gets the integer constant 5.
    let _5 = int0(Int 5)
    /// Gets the integer constant 6.
    let _6 = int0(Int 6)
    /// Gets the integer constant 7.
    let _7 = int0(Int 7)
    /// Gets the integer constant 8.
    let _8 = int0(Int 8)
    /// Gets the integer constant 9.
    let _9 = int0(Int 9)
    /// Gets the integer constant 10.
    let _10 = int0(Int 10)
    /// Gets the double-precision constant 0.
    let _0d = double0(Dbl 0.0)
    /// Gets the double-precision constant 1.
    let _1d = double0(Dbl 1.0)
    /// Combines Boolean expressions with logical AND.
    let And (s:list<bool0>) =
        bool0(
            AND(s |> List.map (fun value -> value.Expr)),
            (s |> Seq.map _.Context |> Aqualis.mergeMany))
    /// Combines Boolean expressions with logical OR.
    let Or (s:list<bool0>) =
        bool0(
            OR(s |> List.map (fun value -> value.Expr)),
            (s |> Seq.map _.Context |> Aqualis.mergeMany))
    /// Gets an expression representing infinity for document output.
    let inf = double0(Var(Dt,"\\infty",NaN))
