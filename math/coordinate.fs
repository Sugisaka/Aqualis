//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    /// Generates two-dimensional coordinate transformations.
    module coordinate =

        /// Coordinate transformations bound to a generation context.
        type ContextCoordinate internal (context:Aqualis) =

            /// Passes <c>(x - sx, y - sy)</c> to the callback using generated temporaries.
            member this.shift (sx:double0,sy:double0) =
                fun (x:double0,y:double0) ->
                    fun code ->
                        context.ch.dd <| fun (x_,y_) ->
                            x_ <== x - sx
                            y_ <== y - sy
                            code(x_,y_)

            /// Translates coordinates by constant offsets, subtracting each offset.
            member this.shift (sx:double,sy:double) =
                fun (x:double0,y:double0) ->
                    fun code ->
                        this.shift (D sx,D sy) (x,y) code

            /// Passes coordinates rotated by the specified angle in radians to the
            /// callback: <c>(x cos a + y sin a, -x sin a + y cos a)</c>.
            member this.rotate_rad (radian:double0) =
                fun (x:double0,y:double0) ->
                    fun code ->
                        context.ch.dd <| fun (x_,y_) ->
                            x_ <==  x * asm.cos radian + y * asm.sin radian
                            y_ <== -x * asm.sin radian + y * asm.cos radian
                            code(x_,y_)

            /// Rotates coordinates by an expression measured in degrees.
            member this.rotate_deg (degree:double0) =
                fun (x:double0,y:double0) ->
                    fun code ->
                        context.ch.ddd <| fun (x_,y_,radian) ->
                            radian <== asm.pi*degree/180.0
                            this.rotate_rad radian (x,y) code

            /// Rotates coordinates by a constant angle measured in radians.
            member this.rotate_rad (radian:double) =
                fun (x:double0,y:double0) ->
                    fun code ->
                        this.rotate_rad (D radian) (x,y) code

            /// Rotates coordinates by a constant angle measured in degrees.
            member this.rotate_deg (degree:double) =
                fun (x:double0,y:double0) ->
                    fun code ->
                        this.rotate_deg (D degree) (x,y) code

    /// Adds coordinate transformation helpers to an Aqualis context.
    [<AutoOpen>]
    module CompilationEnvironmentCoordinateExtensions =
        type Aqualis with
            /// Gets coordinate transformation helpers bound to this context.
            member this.coordinate = coordinate.ContextCoordinate(this)
