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
                    |F -> Var(Dt,"dbesj0("+v.code+")")
                    |C -> Var(Dt,"j0("+v.code+")")
                    |T -> Var(Dt,"J_0\\left("+v.code+"\\right)")
                    |H -> Var(Dt,"J_0\\left("+v.code+"\\right)")
                ch.d <| fun bes ->
                    bes <== (besselj0_(x))
                    code(bes)
            static member bessely0 (x:num0) = fun code ->
                let bessely0_ (v:num0) = 
                    match p.lang with
                    |F -> Var(Dt,"dbesy0("+v.code+")")
                    |C -> Var(Dt,"y0("+v.code+")")
                    |T -> Var(Dt,"Y_0\\left("+v.code+"\\right)")
                    |H -> Var(Dt,"Y_0\\left("+v.code+"\\right)")
                ch.d <| fun bes ->
                    bes <== (bessely0_(x))
                    code(bes)
            static member besselh0 (x:num0) = fun code ->
                let besselj0_ (v:num0) = 
                    match p.lang with
                    |F -> Var(Dt,"dbesj0("+v.code+")")
                    |C -> Var(Dt,"j0("+v.code+")")
                    |T -> Var(Dt,"J_0\\left("+v.code+"\\right)")
                    |H -> Var(Dt,"J_0\\left("+v.code+"\\right)")
                let bessely0_ (v:num0) = 
                    match p.lang with
                    |F -> Var(Dt,"dbesy0("+v.code+")")
                    |C -> Var(Dt,"y0("+v.code+")")
                    |T -> Var(Dt,"Y_0\\left("+v.code+"\\right)")
                    |H -> Var(Dt,"Y_0\\left("+v.code+"\\right)")
                ch.z <| fun bes ->
                    bes <== (besselj0_(x))-(bessely0_(x))*asm.uj
                    code(bes)
            static member besselj1 (x:num0) = fun code ->
                let besselj1_ (v:num0) = 
                    match p.lang with
                    |F -> Var(Dt,"dbesj1("+v.code+")")
                    |C -> Var(Dt,"j1("+v.code+")")
                    |T -> Var(Dt,"J_1\\left("+v.code+"\\right)")
                    |H -> Var(Dt,"J_1\\left("+v.code+"\\right)")
                ch.d <| fun bes ->
                    bes <== (besselj1_(x))
                    code(bes)
            static member bessely1 (x:num0) = fun code ->
                let bessely1_ (v:num0) = 
                    match p.lang with
                    |F -> Var(Dt,"dbesy1("+v.code+")")
                    |C -> Var(Dt,"y1("+v.code+")")
                    |T -> Var(Dt,"Y_1\\left("+v.code+"\\right)")
                    |H -> Var(Dt,"Y_1\\left("+v.code+"\\right)")
                ch.d <| fun bes ->
                    bes <== (bessely1_(x))
                    code(bes)
            static member besselh1 (x:num0) = fun code ->
                let besselj1_ (v:num0) = 
                    match p.lang with
                    |F -> Var(Dt,"dbesj1("+v.code+")")
                    |C -> Var(Dt,"j1("+v.code+")")
                    |T -> Var(Dt,"J_1\\left("+v.code+"\\right)")
                    |H -> Var(Dt,"J_1\\left("+v.code+"\\right)")
                let bessely1_ (v:num0) = 
                    match p.lang with
                    |F -> Var(Dt,"dbesy1("+v.code+")")
                    |C -> Var(Dt,"y1("+v.code+")")
                    |T -> Var(Dt,"Y_1\\left("+v.code+"\\right)")
                    |H -> Var(Dt,"Y_1\\left("+v.code+"\\right)")
                ch.z <| fun bes ->
                    bes <== (besselj1_(x))-(bessely1_(x))*asm.uj
                    code(bes)