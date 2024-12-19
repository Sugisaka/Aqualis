(*
Copyright (c) 2022 Jun-ichiro Sugisaka

This software is released under the MIT License.
http://opensource.org/licenses/mit-license.php
*)
namespace Aqualis
    
    open Aqualis_base
    
    [<AutoOpen>]
    module asm_bessel =
        type asm with
            static member besselj0 (x:num0) = fun code ->
                let besselj0_ (v:num0) = 
                    match p.lang with
                    |Fortran -> Var(Dt,"dbesj0("+v.code+")")
                    |C99 -> Var(Dt,"j0("+v.code+")")
                    |LaTeX -> Var(Dt,"J_0\\left("+v.code+"\\right)")
                    |HTML -> Var(Dt,"J_0\\left("+v.code+"\\right)")
                    |Python -> Var(Dt,"jv(0, "+v.code+")")
                ch.d <| fun bes ->
                    bes <== (besselj0_(x))
                    code(bes)
            static member bessely0 (x:num0) = fun code ->
                let bessely0_ (v:num0) = 
                    match p.lang with
                    |Fortran -> Var(Dt,"dbesy0("+v.code+")")
                    |C99 -> Var(Dt,"y0("+v.code+")")
                    |LaTeX -> Var(Dt,"Y_0\\left("+v.code+"\\right)")
                    |HTML -> Var(Dt,"Y_0\\left("+v.code+"\\right)")
                    |Python -> Var(Dt,"yn(0, "+v.code+")")
                ch.d <| fun bes ->
                    bes <== (bessely0_(x))
                    code(bes)
            static member besselh0 (x:num0) = fun code ->
                let besselj0_ (v:num0) = 
                    match p.lang with
                    |Fortran -> Var(Dt,"dbesj0("+v.code+")")
                    |C99 -> Var(Dt,"j0("+v.code+")")
                    |LaTeX -> Var(Dt,"J_0\\left("+v.code+"\\right)")
                    |HTML -> Var(Dt,"J_0\\left("+v.code+"\\right)")
                    |Python -> Var(Dt,"jv(0, "+v.code+")")
                let bessely0_ (v:num0) = 
                    match p.lang with
                    |Fortran -> Var(Dt,"dbesy0("+v.code+")")
                    |C99 -> Var(Dt,"y0("+v.code+")")
                    |LaTeX -> Var(Dt,"Y_0\\left("+v.code+"\\right)")
                    |HTML -> Var(Dt,"Y_0\\left("+v.code+"\\right)")
                    |Python -> Var(Dt,"yn(0, "+v.code+")")
                ch.z <| fun bes ->
                    bes <== (besselj0_(x))-(bessely0_(x))*asm.uj
                    code(bes)
            static member besselj1 (x:num0) = fun code ->
                let besselj1_ (v:num0) = 
                    match p.lang with
                    |Fortran -> Var(Dt,"dbesj1("+v.code+")")
                    |C99 -> Var(Dt,"j1("+v.code+")")
                    |LaTeX -> Var(Dt,"J_1\\left("+v.code+"\\right)")
                    |HTML -> Var(Dt,"J_1\\left("+v.code+"\\right)")
                    |Python -> Var(Dt,"jv(1, "+v.code+")")
                ch.d <| fun bes ->
                    bes <== (besselj1_(x))
                    code(bes)
            static member bessely1 (x:num0) = fun code ->
                let bessely1_ (v:num0) = 
                    match p.lang with
                    |Fortran -> Var(Dt,"dbesy1("+v.code+")")
                    |C99 -> Var(Dt,"y1("+v.code+")")
                    |LaTeX -> Var(Dt,"Y_1\\left("+v.code+"\\right)")
                    |HTML -> Var(Dt,"Y_1\\left("+v.code+"\\right)")
                    |Python -> Var(Dt,"yn(1, "+v.code+")")
                ch.d <| fun bes ->
                    bes <== (bessely1_(x))
                    code(bes)
            static member besselh1 (x:num0) = fun code ->
                let besselj1_ (v:num0) = 
                    match p.lang with
                    |Fortran -> Var(Dt,"dbesj1("+v.code+")")
                    |C99 -> Var(Dt,"j1("+v.code+")")
                    |LaTeX -> Var(Dt,"J_1\\left("+v.code+"\\right)")
                    |HTML -> Var(Dt,"J_1\\left("+v.code+"\\right)")
                    |Python -> Var(Dt,"jv(1, "+v.code+")")
                let bessely1_ (v:num0) = 
                    match p.lang with
                    |Fortran -> Var(Dt,"dbesy1("+v.code+")")
                    |C99 -> Var(Dt,"y1("+v.code+")")
                    |LaTeX -> Var(Dt,"Y_1\\left("+v.code+"\\right)")
                    |HTML -> Var(Dt,"Y_1\\left("+v.code+"\\right)")
                    |Python -> Var(Dt,"yn(1, "+v.code+")")
                ch.z <| fun bes ->
                    bes <== (besselj1_(x))-(bessely1_(x))*asm.uj
                    code(bes)