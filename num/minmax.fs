//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    [<AutoOpen>]
    module asm_minmax =
        type asm with
            static member private cp1(zA:int1,v:int0,iv:int0 option,compare:int0*int0->bool0) =
                match iv with |Some(iv) -> iv <== 0 |_ -> ()
                v <== zA[_0]
                (CompilationEnvironment(zA.Context)).iter.num zA.size1 <| fun i ->
                    (CompilationEnvironment(zA.Context)).br.if1 (compare(v, i)) <| fun () ->
                        match iv with |Some(iv) -> iv <== i |_ -> ()
                        v <== zA[i]
            static member private cp1(zA:double1,v:double0,iv:int0 option,compare:double0*int0->bool0) =
                match iv with |Some(iv) -> iv <== 0 |_ -> ()
                v <== zA[_0]
                (CompilationEnvironment(zA.Context)).iter.num zA.size1 <| fun i ->
                    (CompilationEnvironment(zA.Context)).br.if1 (compare(v, i)) <| fun () ->
                        match iv with |Some(iv) -> iv <== i |_ -> ()
                        v <== zA[i]

            static member private cp2(zA:int2,v:int0,iv1:int0 option,iv2:int0 option, compare:int0*int0*int0->bool0) =
                match iv1 with |Some(iv1) -> iv1 <== 0 |_ -> ()
                match iv2 with |Some(iv2) -> iv2 <== 0 |_ -> ()
                v <== zA[_0,_0]
                (CompilationEnvironment(zA.Context)).iter.num zA.size1 <| fun i ->
                (CompilationEnvironment(zA.Context)).iter.num zA.size2 <| fun j ->
                    (CompilationEnvironment(zA.Context)).br.if1 (compare(v, i, j)) <| fun () ->
                        match iv1 with |Some(iv1) -> iv1 <== i |_ -> ()
                        match iv2 with |Some(iv2) -> iv2 <== j |_ -> ()
                        v <== zA[i,j]
            static member private cp2(zA:double2,v:double0,iv1:int0 option,iv2:int0 option, compare:double0*int0*int0->bool0) =
                match iv1 with |Some(iv1) -> iv1 <== 0 |_ -> ()
                match iv2 with |Some(iv2) -> iv2 <== 0 |_ -> ()
                v <== zA[_0,_0]
                (CompilationEnvironment(zA.Context)).iter.num zA.size1 <| fun i ->
                (CompilationEnvironment(zA.Context)).iter.num zA.size2 <| fun j ->
                    (CompilationEnvironment(zA.Context)).br.if1 (compare(v, i, j)) <| fun () ->
                        match iv1 with |Some(iv1) -> iv1 <== i |_ -> ()
                        match iv2 with |Some(iv2) -> iv2 <== j |_ -> ()
                        v <== zA[i,j]

            static member private cp3(zA:int3,v:int0,iv1:int0 option,iv2:int0 option,iv3:int0 option, compare:int0*int0*int0*int0->bool0) =
                match iv1 with |Some(iv1) -> iv1 <== 0 |_ -> ()
                match iv2 with |Some(iv2) -> iv2 <== 0 |_ -> ()
                match iv3 with |Some(iv3) -> iv3 <== 0 |_ -> ()
                v <== zA[_0,_0,_0]
                (CompilationEnvironment(zA.Context)).iter.num zA.size1 <| fun i ->
                (CompilationEnvironment(zA.Context)).iter.num zA.size2 <| fun j ->
                (CompilationEnvironment(zA.Context)).iter.num zA.size3 <| fun k ->
                    (CompilationEnvironment(zA.Context)).br.if1 (compare(v, i, j, k)) <| fun () ->
                        match iv1 with |Some(iv1) -> iv1 <== i |_ -> ()
                        match iv2 with |Some(iv2) -> iv2 <== j |_ -> ()
                        match iv3 with |Some(iv3) -> iv3 <== k |_ -> ()
                        v <== zA[i,j,k]
            static member private cp3(zA:double3,v:double0,iv1:int0 option,iv2:int0 option,iv3:int0 option, compare:double0*int0*int0*int0->bool0) =
                match iv1 with |Some(iv1) -> iv1 <== 0 |_ -> ()
                match iv2 with |Some(iv2) -> iv2 <== 0 |_ -> ()
                match iv3 with |Some(iv3) -> iv3 <== 0 |_ -> ()
                v <== zA[_0,_0,_0]
                (CompilationEnvironment(zA.Context)).iter.num zA.size1 <| fun i ->
                (CompilationEnvironment(zA.Context)).iter.num zA.size2 <| fun j ->
                (CompilationEnvironment(zA.Context)).iter.num zA.size3 <| fun k ->
                    (CompilationEnvironment(zA.Context)).br.if1 (compare(v, i, j, k)) <| fun () ->
                        match iv1 with |Some(iv1) -> iv1 <== i |_ -> ()
                        match iv2 with |Some(iv2) -> iv2 <== j |_ -> ()
                        match iv3 with |Some(iv3) -> iv3 <== k |_ -> ()
                        v <== zA[i,j,k]

            /// <summary>
            /// 最大値を検索
            /// </summary>
            /// <param name="zA">検索対象</param>
            /// <param name="max">最大値</param>
            /// <param name="imax">最大値のインデックス</param>
            static member max (zA:int1,max:int0,imax:int0 option) = asm.cp1(zA,max,imax,fun (v, i) -> v .< zA[i])
            static member max (zA:double1,max:double0,imax:int0 option) = asm.cp1(zA,max,imax,fun (v, i) -> v .< zA[i])

            /// <summary>
            /// 最小値を検索
            /// </summary>
            /// <param name="zA">検索対象</param>
            /// <param name="min">最小値</param>
            /// <param name="imin">最小値のインデックス</param>
            static member min (zA:int1,min:int0,imin:int0 option) = asm.cp1(zA,min,imin,fun (v, i) -> v .> zA[i])
            static member min (zA:double1,min:double0,imin:int0 option) = asm.cp1(zA,min,imin,fun (v, i) -> v .> zA[i])

            /// <summary>
            /// 最大値を検索
            /// </summary>
            /// <param name="zA">検索対象</param>
            /// <param name="max">最大値</param>
            /// <param name="imax1">最大値のインデックス</param>
            /// <param name="imax2">最大値のインデックス</param>
            static member max (zA:int2,max:int0,imax1:int0 option,imax2:int0 option) = asm.cp2(zA,max,imax1,imax2,fun (v, i, j) -> v .< zA[i,j])
            static member max (zA:double2,max:double0,imax1:int0 option,imax2:int0 option) = asm.cp2(zA,max,imax1,imax2,fun (v, i, j) -> v .< zA[i,j])

            /// <summary>
            /// 最小値を検索
            /// </summary>
            /// <param name="zA">検索対象</param>
            /// <param name="min">最小値</param>
            /// <param name="imin1">最小値のインデックス</param>
            /// <param name="imin2">最小値のインデックス</param>
            static member min (zA:int2,min:int0,imin1:int0 option,imin2:int0 option) = asm.cp2(zA,min,imin1,imin2,fun (v, i, j) -> v .> zA[i,j])
            static member min (zA:double2,min:double0,imin1:int0 option,imin2:int0 option) = asm.cp2(zA,min,imin1,imin2,fun (v, i, j) -> v .> zA[i,j])

            /// <summary>
            /// 最大値を検索
            /// </summary>
            /// <param name="zA">検索対象</param>
            /// <param name="max1">最大値</param>
            /// <param name="max2">最大値</param>
            /// <param name="max3">最大値</param>
            static member max (zA:int3,max:int0,imax1:int0 option,imax2:int0 option,imax3:int0 option) = asm.cp3(zA,max,imax1,imax2,imax3,fun (v, i, j, k) -> v .< zA[i,j,k])
            static member max (zA:double3,max:double0,imax1:int0 option,imax2:int0 option,imax3:int0 option) = asm.cp3(zA,max,imax1,imax2,imax3,fun (v, i, j, k) -> v .< zA[i,j,k])

            /// <summary>
            /// 最小値を検索
            /// </summary>
            /// <param name="zA">検索対象</param>
            /// <param name="min">最小値</param>
            /// <param name="imin1">最小値のインデックス</param>
            /// <param name="imin2">最小値のインデックス</param>
            /// <param name="imin3">最小値のインデックス</param>
            static member min (zA:int3,min:int0,imin1:int0 option,imin2:int0 option,imin3:int0 option) = asm.cp3(zA,min,imin1,imin2,imin3,fun (v, i, j, k) -> v .> zA[i,j,k])
            static member min (zA:double3,min:double0,imin1:int0 option,imin2:int0 option,imin3:int0 option) = asm.cp3(zA,min,imin1,imin2,imin3,fun (v, i, j, k) -> v .> zA[i,j,k])

            /// <summary>
            /// 最大値を検索
            /// </summary>
            /// <param name="zA">検索対象</param>
            /// <param name="max">最大値</param>
            /// <param name="imax">最大値のインデックス</param>
            static member max (zA:int1,max:int0,imax:int0) = asm.max(zA, max, Some(imax))
            static member max (zA:double1,max:double0,imax:int0) = asm.max(zA, max, Some(imax))

            /// <summary>
            /// 最大値を検索
            /// </summary>
            /// <param name="zA">検索対象</param>
            /// <param name="max">最大値</param>
            /// <param name="imax1">最大値のインデックス</param>
            /// <param name="imax2">最大値のインデックス</param>
            static member max (zA:int2,max:int0,imax1:int0,imax2:int0) = asm.max(zA, max, Some(imax1), Some(imax2))
            static member max (zA:double2,max:double0,imax1:int0,imax2:int0) = asm.max(zA, max, Some(imax1), Some(imax2))

            /// <summary>
            /// 最大値を検索
            /// </summary>
            /// <param name="zA">検索対象</param>
            /// <param name="max">最大値</param>
            /// <param name="imax1">最大値のインデックス</param>
            /// <param name="imax2">最大値のインデックス</param>
            /// <param name="imax3">最大値のインデックス</param>
            static member max (zA:int3,max:int0,imax1:int0,imax2:int0,imax3:int0) = asm.max(zA, max, Some(imax1), Some(imax2), Some(imax3))
            static member max (zA:double3,max:double0,imax1:int0,imax2:int0,imax3:int0) = asm.max(zA, max, Some(imax1), Some(imax2), Some(imax3))

            /// <summary>
            /// 最小値を検索
            /// </summary>
            /// <param name="zA"></param>
            /// <param name="min">最小値</param>
            /// <param name="imin">最小値のインデックス</param>
            static member min (zA:int1,min:int0,imin:int0) = asm.min(zA, min, Some(imin))
            static member min (zA:double1,min:double0,imin:int0) = asm.min(zA, min, Some(imin))

            /// <summary>
            /// 最小値を検索
            /// </summary>
            /// <param name="zA">検索対象</param>
            /// <param name="min">最小値</param>
            /// <param name="imin1">最小値のインデックス</param>
            /// <param name="imin2">最小値のインデックス</param>
            static member min (zA:int2,min:int0,imin1:int0,imin2:int0) = asm.min(zA, min, Some(imin1), Some(imin2))
            static member min (zA:double2,min:double0,imin1:int0,imin2:int0) = asm.min(zA, min, Some(imin1), Some(imin2))

            /// <summary>
            /// 最小値を検索
            /// </summary>
            /// <param name="zA">検索対象</param>
            /// <param name="min">最小値</param>
            /// <param name="imin1">最小値のインデックス</param>
            /// <param name="imin2">最小値のインデックス</param>
            /// <param name="imin3">最小値のインデックス</param>
            static member min (zA:int3,min:int0,imin1:int0,imin2:int0,imin3:int0) = asm.min(zA, min, Some(imin1), Some(imin2), Some(imin3))
            static member min (zA:double3,min:double0,imin1:int0,imin2:int0,imin3:int0) = asm.min(zA, min, Some(imin1), Some(imin2), Some(imin3))

            /// <summary>
            /// 最大値を検索
            /// </summary>
            /// <param name="zA">検索対象</param>
            /// <param name="max">最大値</param>
            static member max (zA:int1,max:int0) = asm.max(zA, max, None)
            static member max (zA:double1,max:double0) = asm.max(zA, max, None)

            /// <summary>
            /// 最大値を検索
            /// </summary>
            /// <param name="zA"></param>
            /// <param name="max">最大値</param>
            static member max (zA:int2,max:int0) = asm.max(zA, max, None, None)
            static member max (zA:double2,max:double0) = asm.max(zA, max, None, None)

            /// <summary>
            /// 最大値を検索
            /// </summary>
            /// <param name="zA">検索対象</param>
            /// <param name="max">最大値</param>
            static member max (zA:int3,max:int0) = asm.max(zA, max, None, None, None)
            static member max (zA:double3,max:double0) = asm.max(zA, max, None, None, None)

            /// <summary>
            /// 最小値を検索
            /// </summary>
            /// <param name="zA">検索対象</param>
            /// <param name="min">最小値</param>
            static member min (zA:int1,min:int0) = asm.min(zA, min, None)
            static member min (zA:double1,min:double0) = asm.min(zA, min, None)

            /// <summary>
            /// 最小値を検索
            /// </summary>
            /// <param name="zA">検索対象</param>
            /// <param name="min">最小値</param>
            static member min (zA:int2,min:int0) = asm.min(zA, min, None, None)
            static member min (zA:double2,min:double0) = asm.min(zA, min, None, None)

            /// <summary>
            /// 最小値を検索
            /// </summary>
            /// <param name="zA">検索対象</param>
            /// <param name="min">最小値</param>
            static member min (zA:int3,min:int0) = asm.min(zA, min, None, None, None)
            static member min (zA:double3,min:double0) = asm.min(zA, min, None, None, None)
