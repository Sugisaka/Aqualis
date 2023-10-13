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
    module print_ax =
        ///<summary>画面表示</summary>
        type print with
            
            ///<summary>1個の項目を画面表示</summary>
            static member c (s:int1) = 
                iter.range _1 s.size1 <| fun i -> 
                    print.c i
                    print.c s[i]
                    print.br
                    
            ///<summary>1個の項目を画面表示</summary>
            static member c (s:int2) = 
                iter.range _1 s.size1 <| fun i -> 
                    iter.range _1 s.size2 <| fun j -> 
                        print.c i
                        print.c j
                        print.c s[i,j]
                        print.br
                        
            ///<summary>1個の項目を画面表示</summary>
            static member c (s:int3) = 
                iter.range _1 s.size1 <| fun i -> 
                    iter.range _1 s.size2 <| fun j -> 
                        iter.range _1 s.size3 <| fun k -> 
                            print.c i
                            print.c j
                            print.c k
                            print.c s[i,j,k]
                            print.br
                            
            ///<summary>1個の項目を画面表示</summary>
            static member c (s:float1) = 
                iter.range _1 s.size1 <| fun i -> 
                    print.c i
                    print.c s[i]
                    print.br
                    
            ///<summary>1個の項目を画面表示</summary>
            static member c (s:float2) = 
                iter.range _1 s.size1 <| fun i -> 
                    iter.range _1 s.size2 <| fun j -> 
                        print.c i 
                        print.c j
                        print.c s[i,j]
                        print.br
                        
            ///<summary>1個の項目を画面表示</summary>
            static member c (s:float3) = 
                iter.range _1 s.size1 <| fun i -> 
                    iter.range _1 s.size2 <| fun j -> 
                        iter.range _1 s.size3 <| fun k -> 
                            print.c i
                            print.c j
                            print.c k
                            print.c s[i,j,k]
                            print.br
                            
            ///<summary>1個の項目を画面表示</summary>
            static member c (s:complex1) = 
                iter.range _1 s.size1 <| fun i -> 
                    print.c i
                    print.c s[i]
                    print.br
                    
            ///<summary>1個の項目を画面表示</summary>
            static member c (s:complex2) = 
                iter.range _1 s.size1 <| fun i -> 
                    iter.range _1 s.size2 <| fun j -> 
                        print.c i
                        print.c j
                        print.c s[i,j]
                        
            ///<summary>1個の項目を画面表示</summary>
            static member c (s:complex3) = 
                iter.range _1 s.size1 <| fun i -> 
                    iter.range _1 s.size2 <| fun j -> 
                        iter.range _1 s.size3 <| fun k -> 
                            print.c i
                            print.c j
                            print.c k
                            print.c s[i,j,k]
                            