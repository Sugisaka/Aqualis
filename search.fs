(*
Copyright (c) 2023 Jun-ichiro Sugisaka

This software is released under the MIT License.
http://opensource.org/licenses/mit-license.php
*)
namespace Aqualis
    
    open Aqualis_base
    
    type search() =
        ///<summary>条件式に従い配列の最大値を検索</summary>
        static member max (zA:num1,max:num0,imax:num0 option) = fun (ff:num0->num0) ->
            match imax with |Some(imax) -> imax <== 1 |_ -> ()
            max <== (ff _1)
            iter.num zA.size1 <| fun i ->
                br.if1 (max .< (ff i)) <| fun () ->
                    match imax with |Some(imax) -> imax <== i |_ -> ()
                    max <== (ff i)
                    
        ///<summary>条件式に従い配列の最小値を検索</summary>
        static member min (zA:num1,min:num0,imin:num0 option) = fun (ff:num0->num0) ->
            match imin with |Some(imin) -> imin <== 1 |_ -> ()
            min <== (ff _1)
            iter.num zA.size1 <| fun i ->
                br.if1 (min .> (ff i)) <| fun () ->
                    match imin with |Some(imin) -> imin <== i |_ -> ()
                    min <== (ff i)
                    
        ///<summary>条件式に従い配列の最大値を検索</summary>
        static member private max2 (zA:num2,(c1:num0 option,c2:num0 option),max:num0,imax:(num0*num0)option) =
            fun (ff:num0->num0->num0) ->
                match imax with
                |Some(imax1,imax2) -> 
                    imax1 <== (match c1 with |None -> _1 |Some(c) -> c)
                    imax2 <== (match c2 with |None -> _1 |Some(c) -> c)
                |_ ->
                    ()
                max <== (ff (match c1 with |None -> _1 |Some(c) -> c) (match c2 with |None -> _1 |Some(c) -> c))
                (match c1 with |None -> iter.num zA.size1 |Some(c) -> (fun (f:num0->unit) -> f c)) <| fun i ->
                (match c2 with |None -> iter.num zA.size2 |Some(c) -> (fun (f:num0->unit) -> f c)) <| fun j ->
                    br.if1 (max .< (ff i j)) <| fun () ->
                        match imax with
                        |Some(imax1,imax2) ->
                            imax1 <== i
                            imax2 <== j
                        |_ ->
                            ()
                        max <== (ff i j)
                    
        ///<summary>条件式に従い配列の最大値を検索</summary>
        static member max (zA:num2,pmax:num0,imax:(num0*num0)option) = 
            search.max2 (zA,(None,None),pmax,imax)
            
        ///<summary>条件式に従い配列の最大値を検索</summary>
        static member max (zA:num2,(c1:unit,c2:unit),max:num0,imax:(num0*num0)option) =
            search.max2 (zA,(None,None),max,imax)
            
        ///<summary>条件式に従い配列の最大値を検索</summary>
        static member max (zA:num2,(c1:unit,c2:num0),max:num0,imax:(num0*num0)option) =
            search.max2 (zA,(None,Some(c2)),max,imax)
            
        ///<summary>条件式に従い配列の最大値を検索</summary>
        static member max (zA:num2,(c1:num0,c2:unit),max:num0,imax:(num0*num0)option) =
            search.max2 (zA,(Some(c1),None),max,imax)
            
        ///<summary>条件式に従い配列の最大値を検索</summary>
        static member max (zA:num2,(c1:unit,c2:int),max:num0,imax:(num0*num0)option) =
            search.max2 (zA,(None,Some(I c2)),max,imax)
            
        ///<summary>条件式に従い配列の最大値を検索</summary>
        static member max (zA:num2,(c1:int,c2:unit),max:num0,imax:(num0*num0)option) =
            search.max2 (zA,(Some(I c1),None),max,imax)
            
        ///<summary>条件式に従い配列の最小値を検索</summary>
        static member private min2 (zA:num2,(c1:num0 option,c2:num0 option),min:num0,imin:(num0*num0)option) =
            fun (ff:num0->num0->num0) ->
                match imin with
                |Some(imin1,imin2) -> 
                    imin1 <== (match c1 with |None -> _1 |Some(c) -> c)
                    imin2 <== (match c2 with |None -> _1 |Some(c) -> c)
                |_ ->
                    ()
                min <== (ff (match c1 with |None -> _1 |Some(c) -> c) (match c2 with |None -> _1 |Some(c) -> c))
                (match c1 with |None -> iter.num zA.size1 |Some(c) -> (fun (f:num0->unit) -> f c)) <| fun i ->
                (match c2 with |None -> iter.num zA.size2 |Some(c) -> (fun (f:num0->unit) -> f c)) <| fun j ->
                    br.if1 (min .> (ff i j)) <| fun () ->
                        match imin with
                        |Some(imin1,imin2) ->
                            imin1 <== i
                            imin2 <== j
                        |_ ->
                            ()
                        min <== (ff i j)
                        
        ///<summary>条件式に従い配列の最大値を検索</summary>
        static member min (zA:num2,min:num0,imin:(num0*num0)option) =
            search.min2 (zA,(None,None),min,imin)
            
        ///<summary>条件式に従い配列の最大値を検索</summary>
        static member min (zA:num2,(c1:unit,c2:num0),min:num0,imin:(num0*num0)option) =
            search.min2 (zA,(None,Some(c2)),min,imin)
            
        ///<summary>条件式に従い配列の最大値を検索</summary>
        static member min (zA:num2,(c1:num0,c2:unit),min:num0,imin:(num0*num0)option) =
            search.min2 (zA,(Some(c1),None),min,imin)
            
        ///<summary>条件式に従い配列の最大値を検索</summary>
        static member min (zA:num2,(c1:unit,c2:int),min:num0,imin:(num0*num0)option) =
            search.min2 (zA,(None,Some(I c2)),min,imin)
            
        ///<summary>条件式に従い配列の最大値を検索</summary>
        static member min (zA:num2,(c1:int,c2:unit),min:num0,imin:(num0*num0)option) =
            search.min2 (zA,(Some(I c1),None),min,imin)
            
        ///<summary>条件式に従い配列の最大値を検索</summary>
        static member private max3 (zA:num3,(c1:num0 option,c2:num0 option,c3:num0 option),max:num0,imax:(num0*num0*num0)option) = 
            fun (ff:num0->num0->num0->num0) ->
                match imax with
                |Some(imax1,imax2,imax3) -> 
                    imax1 <== (match c1 with |None -> _1 |Some(c) -> c)
                    imax2 <== (match c2 with |None -> _1 |Some(c) -> c)
                    imax3 <== (match c3 with |None -> _1 |Some(c) -> c)
                |_ ->
                    ()
                max <== (ff (match c1 with |None -> _1 |Some(c) -> c) (match c2 with |None -> _1 |Some(c) -> c) (match c3 with |None -> _1 |Some(c) -> c))
                (match c1 with |None -> iter.num zA.size1 |Some(c) -> (fun (f:num0->unit) -> f c)) <| fun i ->
                (match c2 with |None -> iter.num zA.size2 |Some(c) -> (fun (f:num0->unit) -> f c)) <| fun j ->
                (match c3 with |None -> iter.num zA.size3 |Some(c) -> (fun (f:num0->unit) -> f c)) <| fun k ->
                    br.if1 (max .< (ff i j k)) <| fun () ->
                        match imax with
                        |Some(imax1,imax2,imax3) ->
                            imax1 <== i
                            imax2 <== j
                            imax3 <== k
                        |_ ->
                            ()
                        max <== (ff i j k)
                    
        ///<summary>条件式に従い配列の最大値を検索</summary>
        static member max (zA:num3,max:num0,imax:(num0*num0*num0)option) =
            search.max3 (zA,(None,None,None),max,imax)
            
        ///<summary>条件式に従い配列の最大値を検索</summary>
        static member max (zA:num3,(c1:unit,c2:unit,c3:unit),max:num0,imax:(num0*num0*num0)option) =
            search.max3 (zA,(None,None,None),max,imax)
            
        ///<summary>条件式に従い配列の最大値を検索</summary>
        static member max (zA:num3,(c1:num0,c2:unit,c3:unit),max:num0,imax:(num0*num0*num0)option) =
            search.max3 (zA,(Some(c1),None,None),max,imax)
            
        ///<summary>条件式に従い配列の最大値を検索</summary>
        static member max (zA:num3,(c1:unit,c2:num0,c3:unit),max:num0,imax:(num0*num0*num0)option) =
            search.max3 (zA,(None,Some(c2),None),max,imax)
            
        ///<summary>条件式に従い配列の最大値を検索</summary>
        static member max (zA:num3,(c1:unit,c2:unit,c3:num0),max:num0,imax:(num0*num0*num0)option) =
            search.max3 (zA,(None,None,Some(c3)),max,imax)
            
        ///<summary>条件式に従い配列の最大値を検索</summary>
        static member max (zA:num3,(c1:num0,c2:num0,c3:unit),max:num0,imax:(num0*num0*num0)option) =
            search.max3 (zA,(Some(c1),Some(c2),None),max,imax)
            
        ///<summary>条件式に従い配列の最大値を検索</summary>
        static member max (zA:num3,(c1:unit,c2:num0,c3:num0),max:num0,imax:(num0*num0*num0)option) =
            search.max3 (zA,(None,Some(c2),Some(c3)),max,imax)
            
        ///<summary>条件式に従い配列の最大値を検索</summary>
        static member max (zA:num3,(c1:num0,c2:unit,c3:num0),max:num0,imax:(num0*num0*num0)option) =
            search.max3 (zA,(Some(c1),None,Some(c3)),max,imax)
            
        ///<summary>条件式に従い配列の最大値を検索</summary>
        static member max (zA:num3,(c1:int,c2:unit,c3:unit),max:num0,imax:(num0*num0*num0)option) =
            search.max3 (zA,(Some(I c1),None,None),max,imax)
            
        ///<summary>条件式に従い配列の最大値を検索</summary>
        static member max (zA:num3,(c1:unit,c2:int,c3:unit),max:num0,imax:(num0*num0*num0)option) =
            search.max3 (zA,(None,Some(I c2),None),max,imax)
            
        ///<summary>条件式に従い配列の最大値を検索</summary>
        static member max (zA:num3,(c1:unit,c2:unit,c3:int),max:num0,imax:(num0*num0*num0)option) =
            search.max3 (zA,(None,None,Some(I c3)),max,imax)
            
        ///<summary>条件式に従い配列の最大値を検索</summary>
        static member max (zA:num3,(c1:int,c2:int,c3:unit),max:num0,imax:(num0*num0*num0)option) =
            search.max3 (zA,(Some(I c1),Some(I c2),None),max,imax)
            
        ///<summary>条件式に従い配列の最大値を検索</summary>
        static member max (zA:num3,(c1:unit,c2:int,c3:int),max:num0,imax:(num0*num0*num0)option) =
            search.max3 (zA,(None,Some(I c2),Some(I c3)),max,imax)
            
        ///<summary>条件式に従い配列の最大値を検索</summary>
        static member max (zA:num3,(c1:int,c2:unit,c3:int),max:num0,imax:(num0*num0*num0)option) =
            search.max3 (zA,(Some(I c1),None,Some(I c3)),max,imax)
            
        ///<summary>条件式に従い配列の最大値を検索</summary>
        static member private min3 (zA:num3,(c1:num0 option,c2:num0 option,c3:num0 option),min:num0,imin:(num0*num0*num0)option) =
            fun (ff:num0->num0->num0->num0) ->
                match imin with
                |Some(imin1,imin2,imin3) -> 
                    imin1 <== (match c1 with |None -> _1 |Some(c) -> c)
                    imin2 <== (match c2 with |None -> _1 |Some(c) -> c)
                    imin3 <== (match c3 with |None -> _1 |Some(c) -> c)
                |_ ->
                    ()
                min <== (ff (match c1 with |None -> _1 |Some(c) -> c) (match c2 with |None -> _1 |Some(c) -> c) (match c3 with |None -> _1 |Some(c) -> c))
                (match c1 with |None -> iter.num zA.size1 |Some(c) -> (fun (f:num0->unit) -> f c)) <| fun i ->
                (match c2 with |None -> iter.num zA.size2 |Some(c) -> (fun (f:num0->unit) -> f c)) <| fun j ->
                (match c3 with |None -> iter.num zA.size3 |Some(c) -> (fun (f:num0->unit) -> f c)) <| fun k ->
                    br.if1 (min .> (ff i j k)) <| fun () ->
                        match imin with
                        |Some(imin1,imin2,imin3) ->
                            imin1 <== i
                            imin2 <== j
                            imin3 <== k
                        |_ ->
                            ()
                        min <== (ff i j k)
                    
        ///<summary>条件式に従い配列の最大値を検索</summary>
        static member min (zA:num3,min:num0,imin:(num0*num0*num0)option) =
            search.min3 (zA,(None,None,None),min,imin)
            
        ///<summary>条件式に従い配列の最大値を検索</summary>
        static member min (zA:num3,(c1:unit,c2:unit,c3:unit),min:num0,imin:(num0*num0*num0)option) =
            search.min3 (zA,(None,None,None),min,imin)
            
        ///<summary>条件式に従い配列の最大値を検索</summary>
        static member min (zA:num3,(c1:num0,c2:unit,c3:unit),min:num0,imin:(num0*num0*num0)option) =
            search.min3 (zA,(Some(c1),None,None),min,imin)
            
        ///<summary>条件式に従い配列の最大値を検索</summary>
        static member min (zA:num3,(c1:unit,c2:num0,c3:unit),min:num0,imin:(num0*num0*num0)option) =
            search.min3 (zA,(None,Some(c2),None),min,imin)
            
        ///<summary>条件式に従い配列の最大値を検索</summary>
        static member min (zA:num3,(c1:unit,c2:unit,c3:num0),min:num0,imin:(num0*num0*num0)option) =
            search.min3 (zA,(None,None,Some(c3)),min,imin)
            
        ///<summary>条件式に従い配列の最大値を検索</summary>
        static member min (zA:num3,(c1:num0,c2:num0,c3:unit),min:num0,imin:(num0*num0*num0)option) =
            search.min3 (zA,(Some(c1),Some(c2),None),min,imin)
            
        ///<summary>条件式に従い配列の最大値を検索</summary>
        static member min (zA:num3,(c1:unit,c2:num0,c3:num0),min:num0,imin:(num0*num0*num0)option) =
            search.min3 (zA,(None,Some(c2),Some(c3)),min,imin)
            
        ///<summary>条件式に従い配列の最大値を検索</summary>
        static member min (zA:num3,(c1:num0,c2:unit,c3:num0),min:num0,imin:(num0*num0*num0)option) =
            search.min3 (zA,(Some(c1),None,Some(c3)),min,imin)
            
        ///<summary>条件式に従い配列の最大値を検索</summary>
        static member min (zA:num3,(c1:int,c2:unit,c3:unit),min:num0,imin:(num0*num0*num0)option) =
            search.min3 (zA,(Some(I c1),None,None),min,imin)
            
        ///<summary>条件式に従い配列の最大値を検索</summary>
        static member min (zA:num3,(c1:unit,c2:int,c3:unit),min:num0,imin:(num0*num0*num0)option) =
            search.min3 (zA,(None,Some(I c2),None),min,imin)
            
        ///<summary>条件式に従い配列の最大値を検索</summary>
        static member min (zA:num3,(c1:unit,c2:unit,c3:int),min:num0,imin:(num0*num0*num0)option) =
            search.min3 (zA,(None,None,Some(I c3)),min,imin)
            
        ///<summary>条件式に従い配列の最大値を検索</summary>
        static member min (zA:num3,(c1:int,c2:int,c3:unit),min:num0,imin:(num0*num0*num0)option) =
            search.min3 (zA,(Some(I c1),Some(I c2),None),min,imin)
            
        ///<summary>条件式に従い配列の最大値を検索</summary>
        static member min (zA:num3,(c1:unit,c2:int,c3:int),min:num0,imin:(num0*num0*num0)option) =
            search.min3 (zA,(None,Some(I c2),Some(I c3)),min,imin)
            
        ///<summary>条件式に従い配列の最大値を検索</summary>
        static member min (zA:num3,(c1:int,c2:unit,c3:int),min:num0,imin:(num0*num0*num0)option) =
            search.min3 (zA,(Some(I c1),None,Some(I c3)),min,imin)