//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    /// Builds the size expression for a named array axis, numbered from one.
    [<RequireQualifiedAccess>]
    module internal ArraySize =
        let named (context:Aqualis) (name:string) (axis:int) =
            let expression =
                match context.language with
                |Fortran ->
                    Var(It 4, name + "_size(" + InvariantFormat.integer axis + ")", NaN)
                |C99 |Python |JavaScript |PHP ->
                    Var(It 4, name + "_size[" + InvariantFormat.integer (axis - 1) + "]", NaN)
                |LaTeX |HTML |HTMLSequenceDiagram ->
                    Var(It 4, "\\mathcal{S}_" + InvariantFormat.integer axis + "[" + name + "]", NaN)
                |Numeric -> NaN
            int0(expression, context)
