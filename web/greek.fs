// 
// Copyright (c) 2026 Jun-ichiro Sugisaka
// 
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
// 
namespace Aqualis

/// Symbolic expressions for LaTeX Greek-letter commands.
type greek =
    /// Gets the symbolic LaTeX command for alpha.
    static member a = Var(Nt, "\\alpha", NaN)
    /// Gets the symbolic LaTeX command for beta.
    static member b = Var(Nt, "\\beta", NaN)
    /// Gets the symbolic LaTeX command for gamma.
    static member g = Var(Nt, "\\gamma", NaN)
    /// Gets the symbolic LaTeX command for delta.
    static member d = Var(Nt, "\\delta", NaN)
    /// Gets the symbolic LaTeX command for epsilon.
    static member e = Var(Nt, "\\epsilon", NaN)
    /// Gets the symbolic LaTeX command for zeta.
    static member z = Var(Nt, "\\zeta", NaN)
    /// Gets the symbolic LaTeX command for eta.
    static member h = Var(Nt, "\\eta", NaN)
    /// Gets the symbolic LaTeX command for theta.
    static member q = Var(Nt, "\\theta", NaN)
    /// Gets the symbolic LaTeX command for iota.
    static member i = Var(Nt, "\\iota", NaN)
    /// Gets the symbolic LaTeX command for kappa.
    static member k = Var(Nt, "\\kappa", NaN)
    /// Gets the symbolic LaTeX command for lambda.
    static member l = Var(Nt, "\\lambda", NaN)
    /// Gets the symbolic LaTeX command for mu.
    static member m = Var(Nt, "\\mu", NaN)
    /// Gets the symbolic LaTeX command for nu.
    static member n = Var(Nt, "\\nu", NaN)
    /// Gets the symbolic LaTeX command for xi.
    static member x = Var(Nt, "\\xi", NaN)
    /// Gets the symbolic LaTeX command for o.
    static member o = Var(Nt, "\\o", NaN)
    /// Gets the symbolic LaTeX command for pi.
    static member p = Var(Nt, "\\pi", NaN)
    /// Gets the symbolic LaTeX command for rho.
    static member r = Var(Nt, "\\rho", NaN)
    /// Gets the symbolic LaTeX command for sigma.
    static member s = Var(Nt, "\\sigma", NaN)
    /// Gets the symbolic LaTeX command for tau.
    static member t = Var(Nt, "\\tau", NaN)
    /// Gets the symbolic LaTeX command for upsilon.
    static member u = Var(Nt, "\\upsilon", NaN)
    /// Gets the symbolic LaTeX command for phi.
    static member f = Var(Nt, "\\phi", NaN)
    /// Gets the symbolic LaTeX command for chi.
    static member c = Var(Nt, "\\chi", NaN)
    /// Gets the symbolic LaTeX command for psi.
    static member y = Var(Nt, "\\psi", NaN)
    /// Gets the symbolic LaTeX command for omega.
    static member w = Var(Nt, "\\omega", NaN)
    /// Gets the symbolic LaTeX command for Gamma.
    static member G = Var(Nt, "\\Gamma", NaN)
    /// Gets the symbolic LaTeX command for Delta.
    static member D = Var(Nt, "\\Delta", NaN)
    /// Gets the symbolic LaTeX command for Theta.
    static member Q = Var(Nt, "\\Theta", NaN)
    /// Gets the symbolic LaTeX command for Lambda.
    static member L = Var(Nt, "\\Lambda", NaN)
    /// Gets the symbolic LaTeX command for Xi.
    static member X = Var(Nt, "\\Xi", NaN)
    /// Gets the symbolic LaTeX command for Pi.
    static member P = Var(Nt, "\\Pi", NaN)
    /// Gets the symbolic LaTeX command for Upsilon.
    static member U = Var(Nt, "\\Upsilon", NaN)
    /// Gets the symbolic LaTeX command for Phi.
    static member F = Var(Nt, "\\Phi", NaN)
    /// Gets the symbolic LaTeX command for Psi.
    static member Y = Var(Nt, "\\Psi", NaN)
    /// Gets the symbolic LaTeX command for Omega.
    static member W = Var(Nt,  "\\Omega", NaN)
