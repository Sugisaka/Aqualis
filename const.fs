(*
Copyright (c) 2022 Jun-ichiro Sugisaka

This software is released under the MIT License.
http://opensource.org/licenses/mit-license.php
*)
namespace Aqualis
    
    [<AutoOpen>]
    module Aqualis_const =
        ///<summary>文字列</summary>
        let (!.) (x:string) = num0(Str_c x)
        ///<summary>整数をint0型に置換</summary>
        let I(n:int) = num0(Int_c n)
        ///<summary>小数をdouble0型に置換</summary>
        let D(x:double) = num0(Dbl_c x)
        
        ///<summary>0(num0型)</summary>
        let _0 = num0(0)
        ///<summary>1(num0型)</summary>
        let _1 = num0(1)
        ///<summary>2(num0型)</summary>
        let _2 = num0(2)
        ///<summary>3(num0型)</summary>
        let _3 = num0(3)
        ///<summary>4(num0型)</summary>
        let _4 = num0(4)
        ///<summary>5(num0型)</summary>
        let _5 = num0(5)
        ///<summary>6(num0型)</summary>
        let _6 = num0(6)
        ///<summary>7(num0型)</summary>
        let _7 = num0(7)
        ///<summary>8(num0型)</summary>
        let _8 = num0(8)
        ///<summary>9(num0型)</summary>
        let _9 = num0(9)
        ///<summary>10(num0型)</summary>
        let _10 = num0(10)
        ///<summary>0.0(num0型)</summary>
        let _0d = num0(0.0)
        ///<summary>1.0(num0型)</summary>
        let _1d = num0(1.0)
        