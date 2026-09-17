//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

open System
open System.IO
open System.Text.Json

/// Frame interval used by animation playback.
type AnimationType =
    /// Repeating interval between two frame indices.
    |Loop of int*int
    /// Finite interval between two frame indices.
    |Range of int*int

/// Position whose coordinates depend on the current animation frame.
type tposition = {
    /// Horizontal coordinate as a function of frame number.
    X:double0->double0;
    /// Vertical coordinate as a function of frame number.
    Y:double0->double0}

/// Animated line segment defined by two time-dependent endpoints.
type Line = {
    /// Starting point.
    Start:tposition;
    /// Ending point.
    End:tposition;}

/// Animated ellipse with time-dependent center and radii.
type Ellipse = {
    /// Center position.
    center:tposition;
    /// Horizontal radius as a function of frame number.
    radiusX:double0->double0;
    /// Vertical radius as a function of frame number.
    radiusY:double0->double0;}

/// Animated circular arc with angles measured in degrees.
type Arc = {
    /// Center position.
    center:tposition;
    /// Starting angle in degrees, measured counterclockwise.
    angle1:double0->double0;
    /// Ending angle in degrees, measured counterclockwise.
    angle2:double0->double0;
    /// Radius as a function of frame number.
    radius:double0->double0;}

/// Animated text at a time-dependent position.
type Text = {
    /// Center position.
    center:tposition;
    /// Text to display.
    str:string; }

/// Animated mathematical expression at a time-dependent position.
type MathText<'a when 'a :> INum0> = {
    /// Center position.
    center:tposition;
    /// Expression to display.
    eq:'a; }

[<AutoOpen>]
/// Adds HTML helpers to an animation generation context.
module HtmlGenerationExtensions1 =
    /// Owns the contexts and assets for an HTML presentation.
    type HtmlGenerationContext with
        
        /// Gets the HTML writer for this animation's body context.
        member this.html = html this.BodyContext

/// Renders numeric expressions used by animation primitives.
module private AnimationRendering =
    /// Selects the generation context for animation output.
    let private target (context:Aqualis) (value:INum0) =
        Aqualis.merge context value.Context |> ignore
        context

    /// Renders a numeric expression for animation output.
    let render (context:Aqualis) (value:INum0) =
        value.Expr.eval (target context value)

    /// Renders a double-precision expression for animation output.
    let renderDouble context (value:double0) =
        render context (value :> INum0)

    /// Wraps a numeric expression for inline MathJax.
    let inlineMath context (value:INum0) =
        "\\(" + render context value + "\\)"

    /// Gets the animation time expression.
    let time (context:Aqualis) =
        double0(Var(Dt,"t",NaN), context)
