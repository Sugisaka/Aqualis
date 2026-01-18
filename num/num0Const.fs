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
    let I x = num0(Int x)
    let D x = num0(Dbl x)
    let Z x = num0(Cpx x)
    let uj = num0(Var(Zt,"uj",NaN))
    let pi = num0(Dbl Math.PI)
    ///<summary>0(num0型)</summary>
    let _0 = num0(Int 0)
    ///<summary>1(num0型)</summary>
    let _1 = num0(Int 1)
    ///<summary>2(num0型)</summary>
    let _2 = num0(Int 2)
    ///<summary>3(num0型)</summary>
    let _3 = num0(Int 3)
    ///<summary>4(num0型)</summary>
    let _4 = num0(Int 4)
    ///<summary>5(num0型)</summary>
    let _5 = num0(Int 5)
    ///<summary>6(num0型)</summary>
    let _6 = num0(Int 6)
    ///<summary>7(num0型)</summary>
    let _7 = num0(Int 7)
    ///<summary>8(num0型)</summary>
    let _8 = num0(Int 8)
    ///<summary>9(num0型)</summary>
    let _9 = num0(Int 9)
    ///<summary>10(num0型)</summary>
    let _10 = num0(Int 10)
    ///<summary>0.0(num0型)</summary>
    let _0d = num0(Dbl 0.0)
    ///<summary>1.0(num0型)</summary>
    let _1d = num0(Dbl 1.0)
    let And (s:list<bool0>) = bool0(AND(s |> List.map (fun p -> p.Expr)))
    let Or (s:list<bool0>) = bool0(OR(s |> List.map (fun p -> p.Expr)))
    let inf = num0(Var(Dt,"\\infty",NaN))
    let (|=) (x:num0) = expr.equivAlign (Var(Nt,"",NaN)) x.Expr (programList[prIndex])
