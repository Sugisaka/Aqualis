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
            static member besselj0 (x:float0) = fun code ->
                let besselj0_ (v:float0) = 
                    match p.lang with
                    |F -> float0(Var("dbesj0("+v.code+")"))
                    |C -> float0(Var("j0("+v.code+")"))
                    |T -> float0(Var("J_0\\left("+v.code+"\\right)"))
                    |H -> float0(Var("J_0\\left("+v.code+"\\right)"))
                ch.d <| fun bes ->
                    bes <== (besselj0_(x))
                    code(bes)
            static member bessely0 (x:float0) = fun code ->
                let bessely0_ (v:float0) = 
                    match p.lang with
                    |F -> float0(Var("dbesy0("+v.code+")"))
                    |C -> float0(Var("y0("+v.code+")"))
                    |T -> float0(Var("Y_0\\left("+v.code+"\\right)"))
                    |H -> float0(Var("Y_0\\left("+v.code+"\\right)"))
                ch.d <| fun bes ->
                    bes <== (bessely0_(x))
                    code(bes)
            static member besselh0 (x:float0) = fun code ->
                let besselj0_ (v:float0) = 
                    match p.lang with
                    |F -> float0(Var("dbesj0("+v.code+")"))
                    |C -> float0(Var("j0("+v.code+")"))
                    |T -> float0(Var("J_0\\left("+v.code+"\\right)"))
                    |H -> float0(Var("J_0\\left("+v.code+"\\right)"))
                let bessely0_ (v:float0) = 
                    match p.lang with
                    |F -> float0(Var("dbesy0("+v.code+")"))
                    |C -> float0(Var("y0("+v.code+")"))
                    |T -> float0(Var("Y_0\\left("+v.code+"\\right)"))
                    |H -> float0(Var("Y_0\\left("+v.code+"\\right)"))
                ch.z <| fun bes ->
                    bes <== (besselj0_(x))-(bessely0_(x))*asm.uj
                    code(bes)
            static member besselj1 (x:float0) = fun code ->
                let besselj1_ (v:float0) = 
                    match p.lang with
                    |F -> float0(Var("dbesj1("+v.code+")"))
                    |C -> float0(Var("j1("+v.code+")"))
                    |T -> float0(Var("J_1\\left("+v.code+"\\right)"))
                    |H -> float0(Var("J_1\\left("+v.code+"\\right)"))
                ch.d <| fun bes ->
                    bes <== (besselj1_(x))
                    code(bes)
            static member bessely1 (x:float0) = fun code ->
                let bessely1_ (v:float0) = 
                    match p.lang with
                    |F -> float0(Var("dbesy1("+v.code+")"))
                    |C -> float0(Var("y1("+v.code+")"))
                    |T -> float0(Var("Y_1\\left("+v.code+"\\right)"))
                    |H -> float0(Var("Y_1\\left("+v.code+"\\right)"))
                ch.d <| fun bes ->
                    bes <== (bessely1_(x))
                    code(bes)
            static member besselh1 (x:float0) = fun code ->
                let besselj1_ (v:float0) = 
                    match p.lang with
                    |F -> float0(Var("dbesj1("+v.code+")"))
                    |C -> float0(Var("j1("+v.code+")"))
                    |T -> float0(Var("J_1\\left("+v.code+"\\right)"))
                    |H -> float0(Var("J_1\\left("+v.code+"\\right)"))
                let bessely1_ (v:float0) = 
                    match p.lang with
                    |F -> float0(Var("dbesy1("+v.code+")"))
                    |C -> float0(Var("y1("+v.code+")"))
                    |T -> float0(Var("Y_1\\left("+v.code+"\\right)"))
                    |H -> float0(Var("Y_1\\left("+v.code+"\\right)"))
                ch.z <| fun bes ->
                    bes <== (besselj1_(x))-(bessely1_(x))*asm.uj
                    code(bes)