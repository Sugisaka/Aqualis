(*
Copyright (c) 2022 Jun-ichiro Sugisaka

This software is released under the MIT License.
http://opensource.org/licenses/mit-license.php
*)
namespace Aqualis
    
    module jagarray = 

        type jagarray() =
        
            /// <summary>
            /// 2次元ジャグ配列(1次元配列の配列)
            /// ラムダ式内で使用可 (全要素数, fxの要素数, fx(i)の要素数, fx) -> code
            /// </summary>
            /// <param name="size">fx(i)の要素数</param>
            static member i11 (size:int list) = 
                fun code ->
                    let wsize = List.sum size
                    ch.i1 (I wsize) <| fun x ->
                    ch.i1 (I size.Length) <| fun offset ->
                    ch.i1 (I size.Length) <| fun xsize ->
                        for i in 1..size.Length do
                            xsize.[i] <== size.[i-1]
                        let mutable o = 0
                        for i in 1..size.Length do
                            offset.[i] <== o
                            o <- o + size.[i-1]
                        let fx (i:num0) (j:num0) = x.[j+offset.[i]]
                        code(wsize,I size.Length,xsize,fx)
                        
            /// <summary>
            /// 2次元ジャグ配列(1次元配列の配列)
            /// ラムダ式内で使用可 (全要素数, fxの要素数, fx(i)の要素数, fx) -> code
            /// </summary>
            /// <param name="size">fx(i)の要素数</param>
            static member d11 (size:int list) = 
                fun code ->
                    let wsize = List.sum size
                    ch.d1 (I wsize) <| fun x ->
                    ch.i1 (I size.Length) <| fun offset ->
                    ch.i1 (I size.Length) <| fun xsize ->
                        for i in 1..size.Length do
                            xsize.[i] <== size.[i-1]
                        let mutable o = 0
                        for i in 1..size.Length do
                            offset.[i] <== o
                            o <- o + size.[i-1]
                        let fx (i:num0) (j:num0) = x.[j+offset.[i]]
                        code(wsize,I size.Length,xsize,fx)
                        
            /// <summary>
            /// 2次元ジャグ配列 fx(i)(j)
            /// ラムダ式内で使用可 (全要素数, fxの要素数, fx(i)の要素数, fx) -> code
            /// </summary>
            /// <param name="size">fx(i)の要素数</param>
            static member z11 (size:int list) = 
                fun code ->
                    let wsize = List.sum size
                    ch.z1 (I wsize) <| fun x ->
                    ch.i1 (I size.Length) <| fun offset ->
                    ch.i1 (I size.Length) <| fun xsize ->
                        for i in 1..size.Length do
                            xsize.[i] <== size.[i-1]
                        let mutable o = 0
                        for i in 1..size.Length do
                            offset.[i] <== o
                            o <- o + size.[i-1]
                        let fx (i:num0) (j:num0) = x.[j+offset.[i]]
                        code(wsize,I size.Length,xsize,fx)
                    
            /// <summary>
            /// 2次元ジャグ配列のための配列を設定
            /// </summary>
            /// <param name="x">未割当1次元配列</param>
            /// <param name="offset">未割当1次元配列</param>
            /// <param name="xsize">未割当1次元配列→fx(i)の要素数を参照可</param>
            static member seti11 (x:num1,offset:num1,xsize:num1) =
                //fx(i)の要素数
                fun (size:int list) ->
                    let wsize = List.sum size
                    x.allocate(wsize)
                    offset.allocate(size.Length)
                    xsize.allocate(size.Length)
                    for i in 1..size.Length do
                        xsize.[i] <== size.[i-1]
                    let mutable o = 0
                    for i in 1..size.Length do
                        offset.[i] <== o
                        o <- o + size.[i-1]
                        
            /// <summary>
            /// 2次元ジャグ配列のための配列を設定
            /// </summary>
            /// <param name="x">未割当1次元配列</param>
            /// <param name="offset">未割当1次元配列</param>
            /// <param name="xsize">未割当1次元配列→fx(i)の要素数を参照可</param>
            static member seti11R (x:num1,offset:num1,xsize:num1) =
                //fx(i)の要素数
                fun (size:int list) ->
                    let wsize = List.sum size
                    //x.allocate(wsize)
                    offset.allocate(size.Length)
                    xsize.allocate(size.Length)
                    for i in 1..size.Length do
                        xsize.[i] <== size.[i-1]
                    let mutable o = 0
                    for i in 1..size.Length do
                        offset.[i] <== o
                        o <- o + size.[i-1]

            /// <summary>
            /// 2次元ジャグ配列 fi11(i)(j)
            /// </summary>
            /// <param name="x">未割当1次元配列</param>
            /// <param name="offset">未割当1次元配列</param>
            /// <param name="xsize">未割当1次元配列→fx(i)の要素数を参照可</param>
            static member fi11 (x:num1,offset:num1,xsize:num1) =
                (fun (i:num0) (j:num0) -> x.[j+offset.[i]])
                    
            /// <summary>
            /// 2次元ジャグ配列のための配列を設定
            /// </summary>
            /// <param name="x">未割当1次元配列</param>
            /// <param name="offset">未割当1次元配列</param>
            /// <param name="xsize">未割当1次元配列→fx(i)の要素数を参照可</param>
            static member setd11 (x:num1,offset:num1,xsize:num1) =
                fun (size:int list) ->
                    let wsize = List.sum size
                    x.allocate(wsize)
                    offset.allocate(size.Length)
                    xsize.allocate(size.Length)
                    for i in 1..size.Length do
                        xsize.[i] <== size.[i-1]
                    let mutable o = 0
                    for i in 1..size.Length do
                        offset.[i] <== o
                        o <- o + size.[i-1]
                        
            /// <summary>
            /// 2次元ジャグ配列のための配列を設定
            /// </summary>
            /// <param name="x">未割当1次元配列</param>
            /// <param name="offset">未割当1次元配列</param>
            /// <param name="xsize">未割当1次元配列→fx(i)の要素数を参照可</param>
            static member setd11R (x:num1,offset:num1,xsize:num1) =
                fun (size:int list) ->
                    let wsize = List.sum size
                    //x.allocate(wsize)
                    offset.allocate(size.Length)
                    xsize.allocate(size.Length)
                    for i in 1..size.Length do
                        xsize.[i] <== size.[i-1]
                    let mutable o = 0
                    for i in 1..size.Length do
                        offset.[i] <== o
                        o <- o + size.[i-1]
                        
            /// <summary>
            /// 2次元ジャグ配列 fi11(i)(j)
            /// </summary>
            /// <param name="x">未割当1次元配列</param>
            /// <param name="offset">未割当1次元配列</param>
            /// <param name="xsize">未割当1次元配列→fx(i)の要素数を参照可</param>
            static member fd11 (x:num1,offset:num1,xsize:num1) =
                (fun (i:num0) (j:num0) -> x.[j+offset.[i]])
                
            /// <summary>
            /// 2次元ジャグ配列のための配列を設定
            /// </summary>
            /// <param name="x">未割当1次元配列</param>
            /// <param name="offset">未割当1次元配列</param>
            /// <param name="xsize">未割当1次元配列→fx(i)の要素数を参照可</param>
            static member setz11 (x:num1,offset:num1,xsize:num1) =
                fun (size:int list) ->
                    let wsize = List.sum size
                    x.allocate(wsize)
                    offset.allocate(size.Length)
                    xsize.allocate(size.Length)
                    for i in 1..size.Length do
                        xsize.[i] <== size.[i-1]
                    let mutable o = 0
                    for i in 1..size.Length do
                        offset.[i] <== o
                        o <- o + size.[i-1]
                        
            /// <summary>
            /// 2次元ジャグ配列のための配列を設定
            /// </summary>
            /// <param name="x">未割当1次元配列</param>
            /// <param name="offset">未割当1次元配列</param>
            /// <param name="xsize">未割当1次元配列→fx(i)の要素数を参照可</param>
            static member setz11R (x:num1,offset:num1,xsize:num1) =
                fun (size:int list) ->
                    let wsize = List.sum size
                    //x.allocate(wsize)
                    offset.allocate(size.Length)
                    xsize.allocate(size.Length)
                    for i in 1..size.Length do
                        xsize.[i] <== size.[i-1]
                    let mutable o = 0
                    for i in 1..size.Length do
                        offset.[i] <== o
                        o <- o + size.[i-1]

            /// <summary>
            /// 2次元ジャグ配列 fi11(i)(j)
            /// </summary>
            /// <param name="x">未割当1次元配列</param>
            /// <param name="offset">未割当1次元配列</param>
            /// <param name="xsize">未割当1次元配列→fx(i)の要素数を参照可</param>
            static member fz11 (x:num1,offset:num1,xsize:num1) =
                (fun (i:num0) (j:num0) -> x.[j+offset.[i]])
            
            /// <summary>
            /// 2次元ジャグ配列 fx(i)(j,k)
            /// ラムダ式内で使用可 (全要素数, fxの要素数, fx(i)の第1要素数, fx(i)の第2要素数, fx) -> code
            /// </summary>
            /// <param name="size">fx(i)の要素数</param>
            static member i12 (size:(int*int)list) =
                fun code ->
                    let wsize = List.sum (List.map (fun (x,y) -> x*y) size)
                    ch.i1 (I wsize) <| fun x ->
                    ch.i1 (I size.Length) <| fun offset ->
                    ch.i1 (I size.Length) <| fun size1 ->
                    ch.i1 (I size.Length) <| fun size2 ->
                        for i in 1..size.Length do
                            let (sx,sy) = size.[i-1]
                            size1.[i] <== sx
                            size2.[i] <== sy
                        let mutable o = 0
                        for i in 1..size.Length do
                            offset.[i] <== o
                            o <- o + (let (sx,sy) = size.[i-1] in sx*sy)
                        let fx (i:num0) (j:num0,k:num0) = x.[j+(k-1)*size1.[i]+offset.[i]]
                        code(wsize,I size.Length,size1,size2,fx)
            
            /// <summary>
            /// 2次元ジャグ配列 fx(i)(j,k)
            /// ラムダ式内で使用可 (全要素数, fxの要素数, fx(i)の第1要素数, fx(i)の第2要素数, fx) -> code
            /// </summary>
            /// <param name="size">fx(i)の要素数</param>
            static member d12 (size:(int*int)list) =
                fun code ->
                    let wsize = List.sum (List.map (fun (x,y) -> x*y) size)
                    ch.d1 (I wsize) <| fun x ->
                    ch.i1 (I size.Length) <| fun offset ->
                    ch.i1 (I size.Length) <| fun size1 ->
                    ch.i1 (I size.Length) <| fun size2 ->
                        for i in 1..size.Length do
                            let (sx,sy) = size.[i-1]
                            size1.[i] <== sx
                            size2.[i] <== sy
                        let mutable o = 0
                        for i in 1..size.Length do
                            offset.[i] <== o
                            o <- o + (let (sx,sy) = size.[i-1] in sx*sy)
                        let fx (i:num0) (j:num0,k:num0) = x.[j+(k-1)*size1.[i]+offset.[i]]
                        code(wsize,I size.Length,size1,size2,fx)
                    
            /// <summary>
            /// 2次元ジャグ配列 fx(i)(j,k)
            /// ラムダ式内で使用可 (全要素数, fxの要素数, fx(i)の第1要素数, fx(i)の第2要素数, fx) -> code
            /// </summary>
            /// <param name="size">fx(i)の要素数</param>
            static member z12 (size:(int*int)list) =
                fun code ->
                    let wsize = List.sum (List.map (fun (x,y) -> x*y) size)
                    ch.z1 (I wsize) <| fun x ->
                    ch.i1 (I size.Length) <| fun offset ->
                    ch.i1 (I size.Length) <| fun size1 ->
                    ch.i1 (I size.Length) <| fun size2 ->
                        for i in 1..size.Length do
                            let (sx,sy) = size.[i-1]
                            size1.[i] <== sx
                            size2.[i] <== sy
                        let mutable o = 0
                        for i in 1..size.Length do
                            offset.[i] <== o
                            o <- o + (let (sx,sy) = size.[i-1] in sx*sy)
                        let fx (i:num0) (j:num0,k:num0) = x.[j+(k-1)*size1.[i]+offset.[i]]
                        code(wsize,I size.Length,size1,size2,fx)
        
            /// <summary>
            /// 2次元ジャグ配列のための配列を設定
            /// </summary>
            /// <param name="x">未割当1次元配列</param>
            /// <param name="offset">未割当1次元配列</param>
            /// <param name="size1">未割当1次元配列→fx(i)の第1要素数</param>
            /// <param name="size2">未割当1次元配列→fx(i)の第2要素数</param>
            static member seti12 (x:num1,offset:num1,size1:num1,size2:num1) =
                fun (size:(int*int)list) ->
                    let wsize = List.sum (List.map (fun (x,y) -> x*y) size)
                    x.allocate(wsize)
                    offset.allocate(size.Length)
                    size1.allocate(size.Length)
                    size2.allocate(size.Length)
                    for i in 1..size.Length do
                        let (sx,sy) = size.[i-1]
                        size1.[i] <== sx
                        size2.[i] <== sy
                    let mutable o = 0
                    for i in 1..size.Length do
                        offset.[i] <== o
                        o <- o + (let (s1,s2) = size.[i-1] in s1*s2)
                        
            /// <summary>
            /// 2次元ジャグ配列のための配列を設定
            /// </summary>
            /// <param name="x">未割当1次元配列</param>
            /// <param name="offset">未割当1次元配列</param>
            /// <param name="size1">未割当1次元配列→fx(i)の第1要素数</param>
            /// <param name="size2">未割当1次元配列→fx(i)の第2要素数</param>
            static member seti12R (x:num1,offset:num1,size1:num1,size2:num1) =
                fun (size:(int*int)list) ->
                    let wsize = List.sum (List.map (fun (x,y) -> x*y) size)
                    //x.allocate(wsize)
                    offset.allocate(size.Length)
                    size1.allocate(size.Length)
                    size2.allocate(size.Length)
                    for i in 1..size.Length do
                        let (sx,sy) = size.[i-1]
                        size1.[i] <== sx
                        size2.[i] <== sy
                    let mutable o = 0
                    for i in 1..size.Length do
                        offset.[i] <== o
                        o <- o + (let (s1,s2) = size.[i-1] in s1*s2)

            /// <summary>
            /// 2次元ジャグ配列 fx(i)(j,k)
            /// </summary>
            /// <param name="x">未割当1次元配列</param>
            /// <param name="offset">未割当1次元配列</param>
            /// <param name="size1">未割当1次元配列→fx(i)の第1要素数</param>
            /// <param name="size2">未割当1次元配列→fx(i)の第2要素数</param>
            static member fi12 (x:num1,offset:num1,size1:num1,size2:num1) =
                (fun (i:num0) (j:num0,k:num0) -> x.[j+(k-1)*size1.[i]+offset.[i]])
        
            /// <summary>
            /// 2次元ジャグ配列のための配列を設定
            /// </summary>
            /// <param name="x">未割当1次元配列</param>
            /// <param name="offset">未割当1次元配列</param>
            /// <param name="size1">未割当1次元配列→fx(i)の第1要素数</param>
            /// <param name="size2">未割当1次元配列→fx(i)の第2要素数</param>
            static member setd12 (x:num1,offset:num1,size1:num1,size2:num1) =
                fun (size:(int*int)list) ->
                    let wsize = List.sum (List.map (fun (x,y) -> x*y) size)
                    x.allocate(wsize)
                    offset.allocate(size.Length)
                    size1.allocate(size.Length)
                    size2.allocate(size.Length)
                    for i in 1..size.Length do
                        let (sx,sy) = size.[i-1]
                        size1.[i] <== sx
                        size2.[i] <== sy
                    let mutable o = 0
                    for i in 1..size.Length do
                        offset.[i] <== o
                        o <- o + (let (s1,s2) = size.[i-1] in s1*s2)
                        
            /// <summary>
            /// 2次元ジャグ配列のための配列を設定
            /// </summary>
            /// <param name="x">未割当1次元配列</param>
            /// <param name="offset">未割当1次元配列</param>
            /// <param name="size1">未割当1次元配列→fx(i)の第1要素数</param>
            /// <param name="size2">未割当1次元配列→fx(i)の第2要素数</param>
            static member setd12R (x:num1,offset:num1,size1:num1,size2:num1) =
                fun (size:(int*int)list) ->
                    let wsize = List.sum (List.map (fun (x,y) -> x*y) size)
                    //x.allocate(wsize)
                    offset.allocate(size.Length)
                    size1.allocate(size.Length)
                    size2.allocate(size.Length)
                    for i in 1..size.Length do
                        let (sx,sy) = size.[i-1]
                        size1.[i] <== sx
                        size2.[i] <== sy
                    let mutable o = 0
                    for i in 1..size.Length do
                        offset.[i] <== o
                        o <- o + (let (s1,s2) = size.[i-1] in s1*s2)

            /// <summary>
            /// 2次元ジャグ配列 fx(i)(j,k)
            /// </summary>
            /// <param name="x">未割当1次元配列</param>
            /// <param name="offset">未割当1次元配列</param>
            /// <param name="size1">未割当1次元配列→fx(i)の第1要素数</param>
            /// <param name="size2">未割当1次元配列→fx(i)の第2要素数</param>
            static member fd12 (x:num1,offset:num1,size1:num1,size2:num1) =
                (fun (i:num0) (j:num0,k:num0) -> x.[j+(k-1)*size1.[i]+offset.[i]])
        
            /// <summary>
            /// 2次元ジャグ配列のための配列を設定
            /// </summary>
            /// <param name="x">未割当1次元配列</param>
            /// <param name="offset">未割当1次元配列</param>
            /// <param name="size1">未割当1次元配列→fx(i)の第1要素数</param>
            /// <param name="size2">未割当1次元配列→fx(i)の第2要素数</param>
            static member setz12 (x:num1,offset:num1,size1:num1,size2:num1) =
                fun (size:(int*int)list) ->
                    let wsize = List.sum (List.map (fun (x,y) -> x*y) size)
                    x.allocate(wsize)
                    offset.allocate(size.Length)
                    size1.allocate(size.Length)
                    size2.allocate(size.Length)
                    for i in 1..size.Length do
                        let (sx,sy) = size.[i-1]
                        size1.[i] <== sx
                        size2.[i] <== sy
                    let mutable o = 0
                    for i in 1..size.Length do
                        offset.[i] <== o
                        o <- o + (let (s1,s2) = size.[i-1] in s1*s2)
                        
            /// <summary>
            /// 2次元ジャグ配列のための配列を設定
            /// </summary>
            /// <param name="x">未割当1次元配列</param>
            /// <param name="offset">未割当1次元配列</param>
            /// <param name="size1">未割当1次元配列→fx(i)の第1要素数</param>
            /// <param name="size2">未割当1次元配列→fx(i)の第2要素数</param>
            static member setz12R (x:num1,offset:num1,size1:num1,size2:num1) =
                fun (size:(int*int)list) ->
                    let wsize = List.sum (List.map (fun (x,y) -> x*y) size)
                    //x.allocate(wsize)
                    offset.allocate(size.Length)
                    size1.allocate(size.Length)
                    size2.allocate(size.Length)
                    for i in 1..size.Length do
                        let (sx,sy) = size.[i-1]
                        size1.[i] <== sx
                        size2.[i] <== sy
                    let mutable o = 0
                    for i in 1..size.Length do
                        offset.[i] <== o
                        o <- o + (let (s1,s2) = size.[i-1] in s1*s2)

            /// <summary>
            /// 2次元ジャグ配列 fx(i)(j,k)
            /// </summary>
            /// <param name="x">未割当1次元配列</param>
            /// <param name="offset">未割当1次元配列</param>
            /// <param name="size1">未割当1次元配列→fx(i)の第1要素数</param>
            /// <param name="size2">未割当1次元配列→fx(i)の第2要素数</param>
            static member fz12 (x:num1,offset:num1,size1:num1,size2:num1) =
                (fun (i:num0) (j:num0,k:num0) -> x.[j+(k-1)*size1.[i]+offset.[i]])
                    