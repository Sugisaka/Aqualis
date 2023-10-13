(*
Copyright (c) 2022 Jun-ichiro Sugisaka

This software is released under the MIT License.
http://opensource.org/licenses/mit-license.php
*)
namespace Aqualis
    
    [<AutoOpen>]
    module Aqualis_const =
        type System.Int32 with
            ///<summary>整数をint0型に置換</summary>
            member this.I with get() =
                int0(Int_c this)
        type System.Double with
            ///<summary>小数をdouble0型に置換</summary>
            member this.D with get() =
                float0(Dbl_c this)
        type System.String with
            ///<summary>小数をdouble0型に置換</summary>
            member this.S with get() =
                str(this)
                (*
        ///<summary>文字列</summary>
        let (!.) (x:string) = str(x)
        ///<summary>整数をint0型に置換</summary>
        let I(n:int) = int0(Int_c n)
        ///<summary>小数をdouble0型に置換</summary>
        let D(x:double) = float0(Dbl_c x)
        *)
        ///<summary>0(num0型)</summary>
        let _0 = int0(Int_c 0)
        ///<summary>1(num0型)</summary>
        let _1 = int0(Int_c 1)
        ///<summary>2(num0型)</summary>
        let _2 = int0(Int_c 2)
        ///<summary>3(num0型)</summary>
        let _3 = int0(Int_c 3)
        ///<summary>4(num0型)</summary>
        let _4 = int0(Int_c 4)
        ///<summary>5(num0型)</summary>
        let _5 = int0(Int_c 5)
        ///<summary>6(num0型)</summary>
        let _6 = int0(Int_c 6)
        ///<summary>7(num0型)</summary>
        let _7 = int0(Int_c 7)
        ///<summary>8(num0型)</summary>
        let _8 = int0(Int_c 8)
        ///<summary>9(num0型)</summary>
        let _9 = int0(Int_c 9)
        ///<summary>10(num0型)</summary>
        let _10 = int0(Int_c 10)
        ///<summary>0.0(num0型)</summary>
        let _0d = float0(Dbl_c 0.0)
        ///<summary>1.0(num0型)</summary>
        let _1d = float0(Dbl_c 1.0)
        