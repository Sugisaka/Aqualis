//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    ///<summary>高速多重極法で使用する数学関数を提供</summary>
    module coordinate =

        /// <summary>
        /// 座標変換
        /// </summary>
        type ContextCoordinate internal (context:Aqualis) =

            /// <summary>
            /// 座標系を(sx,sy)だけ平行移動
            /// </summary>
            member this.shift (sx:double0,sy:double0) =
                fun (x:double0,y:double0) ->
                    fun code ->
                        context.ch.dd <| fun (x_,y_) ->
                            x_ <== x - sx
                            y_ <== y - sy
                            code(x_,y_)

            /// <summary>
            /// 座標系を(sx,sy)だけ平行移動
            /// </summary>
            member this.shift (sx:double,sy:double) =
                fun (x:double0,y:double0) ->
                    fun code ->
                        this.shift (D sx,D sy) (x,y) code

            /// <summary>
            /// 座標系をradianだけ回転
            /// </summary>
            member this.rotate_rad (radian:double0) =
                fun (x:double0,y:double0) ->
                    fun code ->
                        context.ch.dd <| fun (x_,y_) ->
                            x_ <==  x * asm.cos radian + y * asm.sin radian
                            y_ <== -x * asm.sin radian + y * asm.cos radian
                            code(x_,y_)

            /// <summary>
            /// 座標系をdegreeだけ回転
            /// </summary>
            member this.rotate_deg (degree:double0) =
                fun (x:double0,y:double0) ->
                    fun code ->
                        context.ch.ddd <| fun (x_,y_,radian) ->
                            radian <== asm.pi*degree/180.0
                            this.rotate_rad radian (x,y) code

            /// <summary>
            /// 座標系をradianだけ回転
            /// </summary>
            member this.rotate_rad (radian:double) =
                fun (x:double0,y:double0) ->
                    fun code ->
                        this.rotate_rad (D radian) (x,y) code

            /// <summary>
            /// 座標系をdegreeだけ回転
            /// </summary>
            member this.rotate_deg (degree:double) =
                fun (x:double0,y:double0) ->
                    fun code ->
                        this.rotate_deg (D degree) (x,y) code

    [<AutoOpen>]
    module CompilationEnvironmentCoordinateExtensions =
        type Aqualis with
            member this.coordinate = coordinate.ContextCoordinate(this)
