(*
Copyright (c) 2022 Jun-ichiro Sugisaka

This software is released under the MIT License.
http://opensource.org/licenses/mit-license.php
*)
namespace Aqualis
    
    open System
    open System.IO
    open System.Text
    open Aqualis_base

    module num0calc =
        type int0 with
            member this.num0 with get() = num0(It 4, this.expr)
        type float0 with
            member this.num0 with get() = num0(Dt, this.expr)
        type complex0 with
            member this.num0 with get() = num0(Zt, this.expr)
        type int1 with
            member this.num1 with get() = num1(It 4, this.expr)
        type float1 with
            member this.num1 with get() = num1(Dt, this.expr)
        type complex1 with
            member this.num1 with get() = num1(Zt, this.expr)
        type int2 with
            member this.num2 with get() = num2(It 4, this.expr)
        type float2 with
            member this.num2 with get() = num2(Dt, this.expr)
        type complex2 with
            member this.num2 with get() = num2(Zt, this.expr)
        type int3 with
            member this.num3 with get() = num3(It 4, this.expr)
        type float3 with
            member this.num3 with get() = num3(Dt, this.expr)
        type complex3 with
            member this.num3 with get() = num3(Zt, this.expr)