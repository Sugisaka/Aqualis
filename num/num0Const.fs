//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

open System

[<AutoOpen>]
module num0Const =
    let I x = int0(Int x)
    let D x = double0(Dbl x)
    let Z x = complex0(Cpx x)
    let uj = complex0(Var(Zt,"uj",NaN))
    let pi = double0(Dbl Math.PI)
    ///<summary>0(num0型)</summary>
    let _0 = int0(Int 0)
    ///<summary>1(num0型)</summary>
    let _1 = int0(Int 1)
    ///<summary>2(num0型)</summary>
    let _2 = int0(Int 2)
    ///<summary>3(num0型)</summary>
    let _3 = int0(Int 3)
    ///<summary>4(num0型)</summary>
    let _4 = int0(Int 4)
    ///<summary>5(num0型)</summary>
    let _5 = int0(Int 5)
    ///<summary>6(num0型)</summary>
    let _6 = int0(Int 6)
    ///<summary>7(num0型)</summary>
    let _7 = int0(Int 7)
    ///<summary>8(num0型)</summary>
    let _8 = int0(Int 8)
    ///<summary>9(num0型)</summary>
    let _9 = int0(Int 9)
    ///<summary>10(num0型)</summary>
    let _10 = int0(Int 10)
    ///<summary>0.0(num0型)</summary>
    let _0d = double0(Dbl 0.0)
    ///<summary>1.0(num0型)</summary>
    let _1d = double0(Dbl 1.0)
    let And (s:list<bool0>) =
        bool0(
            AND(s |> List.map (fun value -> value.Expr)),
            (s |> Seq.map _.Context |> Aqualis.mergeMany))
    let Or (s:list<bool0>) =
        bool0(
            OR(s |> List.map (fun value -> value.Expr)),
            (s |> Seq.map _.Context |> Aqualis.mergeMany))
    let inf = double0(Var(Dt,"\\infty",NaN))
