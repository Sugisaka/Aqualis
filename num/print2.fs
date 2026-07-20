// 
// Copyright (c) 2026 Jun-ichiro Sugisaka
// 
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
// 
namespace Aqualis
    
    [<AutoOpen>]
    module print_ax =
        ///<summary>画面表示</summary>
        type print with
            
            ///<summary>1個の項目を画面表示</summary>
            static member c (s:int1) = 
                iter.num s.size1 <| fun i -> 
                    print.s <| i++s[i]
            ///<summary>1個の項目を画面表示</summary>
            static member c (s:double1) = 
                iter.num s.size1 <| fun i -> 
                    print.s <| i++s[i]
            ///<summary>1個の項目を画面表示</summary>
            static member c (s:complex1) = 
                iter.num s.size1 <| fun i -> 
                    print.s <| i++s[i]
                    
            ///<summary>1個の項目を画面表示</summary>
            static member c (s:int2) = 
                iter.num s.size1 <| fun i -> 
                    iter.num s.size2 <| fun j -> 
                        print.s <| i++j++s[i,j]
            ///<summary>1個の項目を画面表示</summary>
            static member c (s:double2) = 
                iter.num s.size1 <| fun i -> 
                    iter.num s.size2 <| fun j -> 
                        print.s <| i++j++s[i,j]
            ///<summary>1個の項目を画面表示</summary>
            static member c (s:complex2) = 
                iter.num s.size1 <| fun i -> 
                    iter.num s.size2 <| fun j -> 
                        print.s <| i++j++s[i,j]
                        
            ///<summary>1個の項目を画面表示</summary>
            static member c (s:int3) = 
                iter.num s.size1 <| fun i -> 
                    iter.num s.size2 <| fun j -> 
                        iter.num s.size3 <| fun k -> 
                            print.s <| i++j++k++s[i,j,k]
            ///<summary>1個の項目を画面表示</summary>
            static member c (s:double3) = 
                iter.num s.size1 <| fun i -> 
                    iter.num s.size2 <| fun j -> 
                        iter.num s.size3 <| fun k -> 
                            print.s <| i++j++k++s[i,j,k]
            ///<summary>1個の項目を画面表示</summary>
            static member c (s:complex3) = 
                iter.num s.size1 <| fun i -> 
                    iter.num s.size2 <| fun j -> 
                        iter.num s.size3 <| fun k -> 
                            print.s <| i++j++k++s[i,j,k]
                            