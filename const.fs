(*
Copyright (c) 2022 Jun-ichiro Sugisaka

This software is released under the MIT License.
http://opensource.org/licenses/mit-license.php
*)
namespace Aqualis
    
    [<AutoOpen>]
    module Aqualis_const =
        ///<summary>文字列</summary>
        let (!.) (x:string) = Str_c x
        ///<summary>整数をint0型に置換</summary>
        let I(n:int) = Int_c n
        ///<summary>小数をdouble0型に置換</summary>
        let D(x:double) = Dbl_c x
        ///<summary>0(num0型)</summary>
        let _0 = Int_c 0
        ///<summary>1(num0型)</summary>
        let _1 = Int_c 1
        ///<summary>2(num0型)</summary>
        let _2 = Int_c 2
        ///<summary>3(num0型)</summary>
        let _3 = Int_c 3
        ///<summary>4(num0型)</summary>
        let _4 = Int_c 4
        ///<summary>5(num0型)</summary>
        let _5 = Int_c 5
        ///<summary>6(num0型)</summary>
        let _6 = Int_c 6
        ///<summary>7(num0型)</summary>
        let _7 = Int_c 7
        ///<summary>8(num0型)</summary>
        let _8 = Int_c 8
        ///<summary>9(num0型)</summary>
        let _9 = Int_c 9
        ///<summary>10(num0型)</summary>
        let _10 = Int_c 10
        ///<summary>0.0(num0型)</summary>
        let _0d = Dbl_c 0.0
        ///<summary>1.0(num0型)</summary>
        let _1d = Dbl_c 1.0
        