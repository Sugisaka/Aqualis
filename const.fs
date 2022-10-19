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
    module Aqualis_const =
        ///<summary>コメント文</summary>
        let (!.) (x:string) = Str_e x
        ///<summary>整数をnum0型に置換</summary>
        let I(n:int) = Int_e n
        ///<summary>小数をnum0型に置換</summary>
        let D(x:double) = Dbl_e x
        ///<summary>0(num0型)</summary>
        let _0 = Int_e 0
        ///<summary>1(num0型)</summary>
        let _1 = Int_e 1
        ///<summary>2(num0型)</summary>
        let _2 = Int_e 2
        ///<summary>3(num0型)</summary>
        let _3 = Int_e 3
        ///<summary>4(num0型)</summary>
        let _4 = Int_e 4
        ///<summary>5(num0型)</summary>
        let _5 = Int_e 5
        ///<summary>6(num0型)</summary>
        let _6 = Int_e 6
        ///<summary>7(num0型)</summary>
        let _7 = Int_e 7
        ///<summary>8(num0型)</summary>
        let _8 = Int_e 8
        ///<summary>9(num0型)</summary>
        let _9 = Int_e 9
        ///<summary>10(num0型)</summary>
        let _10 = Int_e 10
        ///<summary>0.0(num0型)</summary>
        let _0d = Dbl_e 0.0
        ///<summary>1.0(num0型)</summary>
        let _1d = Dbl_e 1.0
        