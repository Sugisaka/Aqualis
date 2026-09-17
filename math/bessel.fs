//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    [<AutoOpen>]
    /// Generates Bessel function expressions for supported target languages.
    module asm_bessel =
        /// Requires a target language with Bessel support and registers dependencies.
        let private requireBesselBackend (context:Aqualis) cSymbol pythonSymbol =
            match context.Language with
            | C99 -> context.elist.add ("double " + cSymbol + "(double)")
            | Python -> context.pythonImports.RequireSymbol("scipy.special", pythonSymbol)
            | JavaScript | PHP | Numeric ->
                UnsupportedOperation.codeGeneration (string context.Language) "Bessel functions"
            | Fortran | LaTeX | HTML | HTMLSequenceDiagram -> ()

        type asm with
            /// Computes the order-0 first-kind Bessel function and passes its temporary result to the callback.
            static member besselj0 (x:double0) = fun code ->
                let context = x.Context
                requireBesselBackend context "j0" "jv"
                let besselj0_ (v:double0) =
                    match context.language with
                    |Fortran -> Var(Dt,"dbesj0("+v.code+")",NaN)
                    |C99 -> Var(Dt,"j0("+v.code+")",NaN)
                    |LaTeX -> Var(Dt,"J_0\\left("+v.code+"\\right)",NaN)
                    |HTML -> Var(Dt,"J_0\\left("+v.code+"\\right)",NaN)
                    |HTMLSequenceDiagram -> Var(Dt,"J_0\\left("+v.code+"\\right)",NaN)
                    |Python -> Var(Dt,"jv(0, "+v.code+")",NaN)
                    |JavaScript | PHP | Numeric ->
                        UnsupportedOperation.codeGeneration (string context.Language) "Bessel functions"
                context.ch.d <| fun bes ->
                    bes <== double0(besselj0_ x, x.Context)
                    code bes
            /// Computes the order-0 second-kind Bessel function and passes its temporary result to the callback.
            static member bessely0 (x:double0) = fun code ->
                let context = x.Context
                requireBesselBackend context "y0" "yn"
                let bessely0_ (v:double0) =
                    match context.language with
                    |Fortran -> Var(Dt,"dbesy0("+v.code+")",NaN)
                    |C99 -> Var(Dt,"y0("+v.code+")",NaN)
                    |LaTeX -> Var(Dt,"Y_0\\left("+v.code+"\\right)",NaN)
                    |HTML -> Var(Dt,"Y_0\\left("+v.code+"\\right)",NaN)
                    |HTMLSequenceDiagram -> Var(Dt,"Y_0\\left("+v.code+"\\right)",NaN)
                    |Python -> Var(Dt,"yn(0, "+v.code+")",NaN)
                    |JavaScript | PHP | Numeric ->
                        UnsupportedOperation.codeGeneration (string context.Language) "Bessel functions"
                context.ch.d <| fun bes ->
                    bes <== double0(bessely0_(x), x.Context)
                    code bes
            /// Computes the order-0 Hankel function J minus iY and passes its temporary result to the callback.
            static member besselh0 (x:double0) = fun code ->
                let context = x.Context
                requireBesselBackend context "j0" "jv"
                requireBesselBackend context "y0" "yn"
                let besselj0_ (v:double0) =
                    match context.language with
                    |Fortran -> Var(Dt,"dbesj0("+v.code+")",NaN)
                    |C99 -> Var(Dt,"j0("+v.code+")",NaN)
                    |LaTeX -> Var(Dt,"J_0\\left("+v.code+"\\right)",NaN)
                    |HTML -> Var(Dt,"J_0\\left("+v.code+"\\right)",NaN)
                    |HTMLSequenceDiagram -> Var(Dt,"J_0\\left("+v.code+"\\right)",NaN)
                    |Python -> Var(Dt,"jv(0, "+v.code+")",NaN)
                    |JavaScript | PHP | Numeric ->
                        UnsupportedOperation.codeGeneration (string context.Language) "Bessel functions"
                let bessely0_ (v:double0) =
                    match context.language with
                    |Fortran -> Var(Dt,"dbesy0("+v.code+")",NaN)
                    |C99 -> Var(Dt,"y0("+v.code+")",NaN)
                    |LaTeX -> Var(Dt,"Y_0\\left("+v.code+"\\right)",NaN)
                    |HTML -> Var(Dt,"Y_0\\left("+v.code+"\\right)",NaN)
                    |HTMLSequenceDiagram -> Var(Dt,"Y_0\\left("+v.code+"\\right)",NaN)
                    |Python -> Var(Dt,"yn(0, "+v.code+")",NaN)
                    |JavaScript | PHP | Numeric ->
                        UnsupportedOperation.codeGeneration (string context.Language) "Bessel functions"
                context.ch.z <| fun bes ->
                    bes <== double0(besselj0_ x, x.Context)-double0(bessely0_ x, x.Context)*asm.uj
                    code bes
            /// Computes the order-1 first-kind Bessel function and passes its temporary result to the callback.
            static member besselj1 (x:double0) = fun code ->
                let context = x.Context
                requireBesselBackend context "j1" "jv"
                let besselj1_ (v:double0) =
                    match context.language with
                    |Fortran -> Var(Dt,"dbesj1("+v.code+")",NaN)
                    |C99 -> Var(Dt,"j1("+v.code+")",NaN)
                    |LaTeX -> Var(Dt,"J_1\\left("+v.code+"\\right)",NaN)
                    |HTML -> Var(Dt,"J_1\\left("+v.code+"\\right)",NaN)
                    |HTMLSequenceDiagram -> Var(Dt,"J_1\\left("+v.code+"\\right)",NaN)
                    |Python -> Var(Dt,"jv(1, "+v.code+")",NaN)
                    |JavaScript | PHP | Numeric ->
                        UnsupportedOperation.codeGeneration (string context.Language) "Bessel functions"
                context.ch.d <| fun bes ->
                    bes <== double0(besselj1_(x), x.Context)
                    code bes
            /// Computes the order-1 second-kind Bessel function and passes its temporary result to the callback.
            static member bessely1 (x:double0) = fun code ->
                let context = x.Context
                requireBesselBackend context "y1" "yn"
                let bessely1_ (v:double0) =
                    match context.language with
                    |Fortran -> Var(Dt,"dbesy1("+v.code+")",NaN)
                    |C99 -> Var(Dt,"y1("+v.code+")",NaN)
                    |LaTeX -> Var(Dt,"Y_1\\left("+v.code+"\\right)",NaN)
                    |HTML -> Var(Dt,"Y_1\\left("+v.code+"\\right)",NaN)
                    |HTMLSequenceDiagram -> Var(Dt,"Y_1\\left("+v.code+"\\right)",NaN)
                    |Python -> Var(Dt,"yn(1, "+v.code+")",NaN)
                    |JavaScript | PHP | Numeric ->
                        UnsupportedOperation.codeGeneration (string context.Language) "Bessel functions"
                context.ch.d <| fun bes ->
                    bes <== double0(bessely1_ x, x.Context)
                    code bes
            /// Computes the order-1 Hankel function J minus iY and passes its temporary result to the callback.
            static member besselh1 (x:double0) = fun code ->
                let context = x.Context
                requireBesselBackend context "j1" "jv"
                requireBesselBackend context "y1" "yn"
                let besselj1_ (v:double0) =
                    match context.language with
                    |Fortran -> Var(Dt,"dbesj1("+v.code+")",NaN)
                    |C99 -> Var(Dt,"j1("+v.code+")",NaN)
                    |LaTeX -> Var(Dt,"J_1\\left("+v.code+"\\right)",NaN)
                    |HTML -> Var(Dt,"J_1\\left("+v.code+"\\right)",NaN)
                    |HTMLSequenceDiagram -> Var(Dt,"J_1\\left("+v.code+"\\right)",NaN)
                    |Python -> Var(Dt,"jv(1, "+v.code+")",NaN)
                    |JavaScript | PHP | Numeric ->
                        UnsupportedOperation.codeGeneration (string context.Language) "Bessel functions"
                let bessely1_ (v:double0) =
                    match context.language with
                    |Fortran -> Var(Dt,"dbesy1("+v.code+")",NaN)
                    |C99 -> Var(Dt,"y1("+v.code+")",NaN)
                    |LaTeX -> Var(Dt,"Y_1\\left("+v.code+"\\right)",NaN)
                    |HTML -> Var(Dt,"Y_1\\left("+v.code+"\\right)",NaN)
                    |HTMLSequenceDiagram -> Var(Dt,"Y_1\\left("+v.code+"\\right)",NaN)
                    |Python -> Var(Dt,"yn(1, "+v.code+")",NaN)
                    |JavaScript | PHP | Numeric ->
                        UnsupportedOperation.codeGeneration (string context.Language) "Bessel functions"
                context.ch.z <| fun bes ->
                    bes <== double0(besselj1_ x, x.Context)-double0(bessely1_ x, x.Context)*asm.uj
                    code bes
