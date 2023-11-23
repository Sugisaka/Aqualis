(*
Copyright (c) 2022 Jun-ichiro Sugisaka

This software is released under the MIT License.
http://opensource.org/licenses/mit-license.php
*)
namespace Aqualis
    
    open Aqualis_base
    
    [<AutoOpen>]
    module doperator =
        
        ///<summary>反復処理</summary>
        type Expr with
            static member ddd() = ()