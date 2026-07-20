// 
// Copyright (c) 2026 Jun-ichiro Sugisaka
// 
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
// 
namespace Aqualis

    open System
    open System.Globalization
    
    ///<summary>言語を指定</summary>
    type Language =
        ///<summary>Fortran</summary>
        |Fortran
        ///<summary>C</summary>
        |C99
        ///<summary>LaTeX</summary>
        |LaTeX
        ///<summary>HTML</summary>
        |HTML
        ///<summary>HTMLシーケンス図</summary>
        |HTMLSequenceDiagram
        ///<summary>Python</summary>
        |Python
        ///<summary>JavaScript</summary>
        |JavaScript
        ///<summary>PHP</summary>
        |PHP
        ///<summary>直接計算</summary>
        |Numeric

    /// Culture-independent formatting for generated source code and other
    /// machine-readable artifacts.
    [<RequireQualifiedAccess>]
    module InvariantFormat =
        let integer (value:int) =
            value.ToString(CultureInfo.InvariantCulture)

        let numberWithFormat format (value:double) =
            if not (Double.IsFinite value) then
                invalidArg (nameof value) "A non-finite number cannot be written to a machine-readable artifact."
            value.ToString(format, CultureInfo.InvariantCulture)

        let number (value:double) =
            numberWithFormat "R" value

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

    ///<summary>設定のONまたはOFFを指定</summary>
    type Switch =
        |ON
        |OFF

    ///<summary>変数、配列とその次元の指定</summary>
    type VarType =
        ///<summary>変数</summary>
        |A0
        ///<summary>1次元配列(要素数)</summary>
        |A1 of int
        ///<summary>2次元配列(要素数1,要素数2)</summary>
        |A2 of int*int
        ///<summary>3次元配列(要素数1,要素数2,要素数3)</summary>
        |A3 of int*int*int
