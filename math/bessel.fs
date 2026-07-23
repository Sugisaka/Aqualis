//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    [<AutoOpen>]
    module asm_bessel =
        type asm with
            static member besselj0 (x:double0) = fun code ->
                let context = GenerationContextMerge.requireTarget x.Context
                let environment = Aqualis(Some context)
                let besselj0_ (v:double0) =
                    match context.CurrentProgram.language with
                    |Fortran -> Var(Dt,"dbesj0("+v.code+")",NaN)
                    |C99 -> Var(Dt,"j0("+v.code+")",NaN)
                    |LaTeX -> Var(Dt,"J_0\\left("+v.code+"\\right)",NaN)
                    |HTML -> Var(Dt,"J_0\\left("+v.code+"\\right)",NaN)
                    |HTMLSequenceDiagram -> Var(Dt,"J_0\\left("+v.code+"\\right)",NaN)
                    |Python -> Var(Dt,"jv(0, "+v.code+")",NaN)
                    |JavaScript -> NaN
                    |PHP -> NaN
                    |Numeric -> NaN
                environment.ch.d <| fun bes ->
                    bes <== double0(besselj0_ x, ?context=x.Context)
                    code bes
            static member bessely0 (x:double0) = fun code ->
                let context = GenerationContextMerge.requireTarget x.Context
                let environment = Aqualis(Some context)
                let bessely0_ (v:double0) =
                    match context.CurrentProgram.language with
                    |Fortran -> Var(Dt,"dbesy0("+v.code+")",NaN)
                    |C99 -> Var(Dt,"y0("+v.code+")",NaN)
                    |LaTeX -> Var(Dt,"Y_0\\left("+v.code+"\\right)",NaN)
                    |HTML -> Var(Dt,"Y_0\\left("+v.code+"\\right)",NaN)
                    |HTMLSequenceDiagram -> Var(Dt,"Y_0\\left("+v.code+"\\right)",NaN)
                    |Python -> Var(Dt,"yn(0, "+v.code+")",NaN)
                    |JavaScript -> NaN
                    |PHP -> NaN
                    |Numeric -> NaN
                environment.ch.d <| fun bes ->
                    bes <== double0(bessely0_(x), ?context=x.Context)
                    code bes
            static member besselh0 (x:double0) = fun code ->
                let context = GenerationContextMerge.requireTarget x.Context
                let environment = Aqualis(Some context)
                let besselj0_ (v:double0) =
                    match context.CurrentProgram.language with
                    |Fortran -> Var(Dt,"dbesj0("+v.code+")",NaN)
                    |C99 -> Var(Dt,"j0("+v.code+")",NaN)
                    |LaTeX -> Var(Dt,"J_0\\left("+v.code+"\\right)",NaN)
                    |HTML -> Var(Dt,"J_0\\left("+v.code+"\\right)",NaN)
                    |HTMLSequenceDiagram -> Var(Dt,"J_0\\left("+v.code+"\\right)",NaN)
                    |Python -> Var(Dt,"jv(0, "+v.code+")",NaN)
                    |JavaScript -> NaN
                    |PHP -> NaN
                    |Numeric -> NaN
                let bessely0_ (v:double0) =
                    match context.CurrentProgram.language with
                    |Fortran -> Var(Dt,"dbesy0("+v.code+")",NaN)
                    |C99 -> Var(Dt,"y0("+v.code+")",NaN)
                    |LaTeX -> Var(Dt,"Y_0\\left("+v.code+"\\right)",NaN)
                    |HTML -> Var(Dt,"Y_0\\left("+v.code+"\\right)",NaN)
                    |HTMLSequenceDiagram -> Var(Dt,"Y_0\\left("+v.code+"\\right)",NaN)
                    |Python -> Var(Dt,"yn(0, "+v.code+")",NaN)
                    |JavaScript -> NaN
                    |PHP -> NaN
                    |Numeric -> NaN
                environment.ch.z <| fun bes ->
                    bes <== double0(besselj0_ x, ?context=x.Context)-double0(bessely0_ x, ?context=x.Context)*asm.uj
                    code bes
            static member besselj1 (x:double0) = fun code ->
                let context = GenerationContextMerge.requireTarget x.Context
                let environment = Aqualis(Some context)
                let besselj1_ (v:double0) =
                    match context.CurrentProgram.language with
                    |Fortran -> Var(Dt,"dbesj1("+v.code+")",NaN)
                    |C99 -> Var(Dt,"j1("+v.code+")",NaN)
                    |LaTeX -> Var(Dt,"J_1\\left("+v.code+"\\right)",NaN)
                    |HTML -> Var(Dt,"J_1\\left("+v.code+"\\right)",NaN)
                    |HTMLSequenceDiagram -> Var(Dt,"J_1\\left("+v.code+"\\right)",NaN)
                    |Python -> Var(Dt,"jv(1, "+v.code+")",NaN)
                    |JavaScript -> NaN
                    |PHP -> NaN
                    |Numeric -> NaN
                environment.ch.d <| fun bes ->
                    bes <== double0(besselj1_(x), ?context=x.Context)
                    code bes
            static member bessely1 (x:double0) = fun code ->
                let context = GenerationContextMerge.requireTarget x.Context
                let environment = Aqualis(Some context)
                let bessely1_ (v:double0) =
                    match context.CurrentProgram.language with
                    |Fortran -> Var(Dt,"dbesy1("+v.code+")",NaN)
                    |C99 -> Var(Dt,"y1("+v.code+")",NaN)
                    |LaTeX -> Var(Dt,"Y_1\\left("+v.code+"\\right)",NaN)
                    |HTML -> Var(Dt,"Y_1\\left("+v.code+"\\right)",NaN)
                    |HTMLSequenceDiagram -> Var(Dt,"Y_1\\left("+v.code+"\\right)",NaN)
                    |Python -> Var(Dt,"yn(1, "+v.code+")",NaN)
                    |JavaScript -> NaN
                    |PHP -> NaN
                    |Numeric -> NaN
                environment.ch.d <| fun bes ->
                    bes <== double0(bessely1_ x, ?context=x.Context)
                    code bes
            static member besselh1 (x:double0) = fun code ->
                let context = GenerationContextMerge.requireTarget x.Context
                let environment = Aqualis(Some context)
                let besselj1_ (v:double0) =
                    match context.CurrentProgram.language with
                    |Fortran -> Var(Dt,"dbesj1("+v.code+")",NaN)
                    |C99 -> Var(Dt,"j1("+v.code+")",NaN)
                    |LaTeX -> Var(Dt,"J_1\\left("+v.code+"\\right)",NaN)
                    |HTML -> Var(Dt,"J_1\\left("+v.code+"\\right)",NaN)
                    |HTMLSequenceDiagram -> Var(Dt,"J_1\\left("+v.code+"\\right)",NaN)
                    |Python -> Var(Dt,"jv(1, "+v.code+")",NaN)
                    |JavaScript -> NaN
                    |PHP -> NaN
                    |Numeric -> NaN
                let bessely1_ (v:double0) =
                    match context.CurrentProgram.language with
                    |Fortran -> Var(Dt,"dbesy1("+v.code+")",NaN)
                    |C99 -> Var(Dt,"y1("+v.code+")",NaN)
                    |LaTeX -> Var(Dt,"Y_1\\left("+v.code+"\\right)",NaN)
                    |HTML -> Var(Dt,"Y_1\\left("+v.code+"\\right)",NaN)
                    |HTMLSequenceDiagram -> Var(Dt,"Y_1\\left("+v.code+"\\right)",NaN)
                    |Python -> Var(Dt,"yn(1, "+v.code+")",NaN)
                    |JavaScript -> NaN
                    |PHP -> NaN
                    |Numeric -> NaN
                environment.ch.z <| fun bes ->
                    bes <== double0(besselj1_ x, ?context=x.Context)-double0(bessely1_ x, ?context=x.Context)*asm.uj
                    code bes
