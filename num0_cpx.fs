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

    [<AutoOpen>]
    module num0_cpx =
        type num0 with
            ///<summary>絶対値</summary>
            member this.abs 
                with get() =
                    asm.abs(this)
            ///<summary>絶対値の2乗</summary>
            member this.pow
                with get() =
                    match this.etype with
                      |Zt ->
                        asm.pow(asm.abs(this),2)
                      |_ ->
                        Console.WriteLine("複素変数以外に対してpowを使用できません")
                        NaN
            ///<summary>偏角</summary>
            member this.pha
                with get() =
                    match this.etype with
                      |Zt ->
                        asm.atan2(this.im,this.re)
                      |_ ->
                        Console.WriteLine("複素変数以外に対してphaを使用できません")
                        NaN
                    
