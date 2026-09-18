// 
// Copyright (c) 2026 Jun-ichiro Sugisaka
// 
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
// 
namespace Aqualis

    open System
    open System.Globalization
    
    /// Target language used for generated code, markup, or direct evaluation.
    type Language =
        /// Fortran source code.
        |Fortran
        /// C99 source code.
        |C99
        /// LaTeX markup.
        |LaTeX
        /// HTML markup.
        |HTML
        /// HTML sequence diagram markup.
        |HTMLSequenceDiagram
        /// Python source code.
        |Python
        /// JavaScript source code.
        |JavaScript
        /// PHP source code.
        |PHP
        /// Direct numeric evaluation without generating source code.
        |Numeric

    /// Raises consistent errors for operations unsupported by a target or evaluation mode.
    [<RequireQualifiedAccess>]
    module internal UnsupportedOperation =
        /// Raises a <see cref="T:System.NotSupportedException"/> with the supplied message.
        let raise message : 'T =
            raise (NotSupportedException message)

        /// Rejects an operation unsupported by the requested code-generation target.
        let codeGeneration target operation : 'T =
            raise $"{target} code generation does not support {operation}."

        /// Rejects an operation unsupported by symbolic differentiation.
        let symbolicDifferentiation operation : 'T =
            raise $"Symbolic differentiation does not support {operation}."

        /// Rejects an operation unsupported by direct numeric evaluation.
        let numericEvaluation operation : 'T =
            raise $"Numeric evaluation does not support {operation}."

        /// Rejects an unsupported function-argument operation.
        let functionArgument operation : 'T =
            raise $"Function arguments do not support {operation}."

    /// Culture-independent formatting for generated source code and other
    /// machine-readable artifacts.
    [<RequireQualifiedAccess>]
    module InvariantFormat =
        /// Formats an integer without locale-dependent separators or digits.
        let integer (value:int) =
            value.ToString(CultureInfo.InvariantCulture)

        /// Formats a finite floating-point number with the specified invariant-culture format.
        /// Throws for NaN or infinity.
        let numberWithFormat format (value:double) =
            if not (Double.IsFinite value) then
                invalidArg (nameof value) "A non-finite number cannot be written to a machine-readable artifact."
            value.ToString(format, CultureInfo.InvariantCulture)

        /// Formats a finite floating-point number for round-trip parsing.
        let number (value:double) =
            numberWithFormat "R" value

        /// Formats a finite numeric literal for generated code.
        let private finiteLiteral language (value:double) =
            match language with
            |Fortran ->
                value
                    .ToString("0.0#################E0", CultureInfo.InvariantCulture)
                    .Replace("E", "d")
            |C99|Python ->
                value.ToString("0.0#################E0", CultureInfo.InvariantCulture)
            |_ ->
                value.ToString("R", CultureInfo.InvariantCulture)

        /// Formats a floating-point literal for the selected target language.
        let codeNumber language (value:double) =
            if Double.IsNaN value then
                match language with
                |Fortran -> "ieee_value(0.0d0, ieee_quiet_nan)"
                |C99 -> "NAN"
                |Python -> "float('nan')"
                |JavaScript -> "Number.NaN"
                |PHP -> "NAN"
                |LaTeX|HTML|HTMLSequenceDiagram -> "\\mathrm{NaN}"
                |Numeric -> "NaN"
            elif Double.IsPositiveInfinity value then
                match language with
                |Fortran -> "ieee_value(0.0d0, ieee_positive_inf)"
                |C99 -> "INFINITY"
                |Python -> "float('inf')"
                |JavaScript -> "Number.POSITIVE_INFINITY"
                |PHP -> "INF"
                |LaTeX|HTML|HTMLSequenceDiagram -> "\\infty"
                |Numeric -> "Infinity"
            elif Double.IsNegativeInfinity value then
                match language with
                |Fortran -> "ieee_value(0.0d0, ieee_negative_inf)"
                |C99 -> "-INFINITY"
                |Python -> "-float('inf')"
                |JavaScript -> "Number.NEGATIVE_INFINITY"
                |PHP -> "-INF"
                |LaTeX|HTML|HTMLSequenceDiagram -> "-\\infty"
                |Numeric -> "-Infinity"
            else
                finiteLiteral language value

    /// Escapes text for HTML rendering.
    [<RequireQualifiedAccess>]
    module internal HtmlTextEncoding =
        /// Escapes text for HTML content.
        let textContent (value:string) =
            if isNull value then nullArg (nameof value)

            value
                .Replace("&", "&amp;")
                .Replace("<", "&lt;")
                .Replace(">", "&gt;")

    /// On/off setting used by generated-code options.
    type Switch =
        /// Enables the option.
        |ON
        /// Disables the option.
        |OFF

    /// Shape of a scalar or array variable, including array extents.
    type VarType =
        /// Scalar variable.
        |A0
        /// One-dimensional array with its element count.
        |A1 of int
        /// Two-dimensional array with the extent of each dimension.
        |A2 of int*int
        /// Three-dimensional array with the extent of each dimension.
        |A3 of int*int*int
