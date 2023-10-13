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
                    |F -> num0(Dt,Var("dbesj0("+v.code+")"))
                    |C -> num0(Dt,Var("j0("+v.code+")"))
                    |T -> num0(Dt,Var("J_0\\left("+v.code+"\\right)"))
                    |H -> num0(Dt,Var("J_0\\left("+v.code+"\\right)"))
                ch.d <| fun bes ->
                    bes <== (besselj0_(x))
                    code(bes)
            static member bessely0 (x:num0) = fun code ->
                let bessely0_ (v:num0) = 
                    match p.lang with
                    |F -> num0(Dt,Var("dbesy0("+v.code+")"))
                    |C -> num0(Dt,Var("y0("+v.code+")"))
                    |T -> num0(Dt,Var("Y_0\\left("+v.code+"\\right)"))
                    |H -> num0(Dt,Var("Y_0\\left("+v.code+"\\right)"))
                ch.d <| fun bes ->
                    bes <== (bessely0_(x))
                    code(bes)
            static member besselh0 (x:num0) = fun code ->
                let besselj0_ (v:num0) = 
                    match p.lang with
                    |F -> num0(Dt,Var("dbesj0("+v.code+")"))
                    |C -> num0(Dt,Var("j0("+v.code+")"))
                    |T -> num0(Dt,Var("J_0\\left("+v.code+"\\right)"))
                    |H -> num0(Dt,Var("J_0\\left("+v.code+"\\right)"))
                let bessely0_ (v:num0) = 
                    match p.lang with
                    |F -> num0(Dt,Var("dbesy0("+v.code+")"))
                    |C -> num0(Dt,Var("y0("+v.code+")"))
                    |T -> num0(Dt,Var("Y_0\\left("+v.code+"\\right)"))
                    |H -> num0(Dt,Var("Y_0\\left("+v.code+"\\right)"))
                ch.z <| fun bes ->
                    bes <== (besselj0_(x))-(bessely0_(x))*asm.uj
                    code(bes)
            static member besselj1 (x:num0) = fun code ->
                let besselj1_ (v:num0) = 
                    match p.lang with
                    |F -> num0(Dt,Var("dbesj1("+v.code+")"))
                    |C -> num0(Dt,Var("j1("+v.code+")"))
                    |T -> num0(Dt,Var("J_1\\left("+v.code+"\\right)"))
                    |H -> num0(Dt,Var("J_1\\left("+v.code+"\\right)"))
                ch.d <| fun bes ->
                    bes <== (besselj1_(x))
                    code(bes)
            static member bessely1 (x:num0) = fun code ->
                let bessely1_ (v:num0) = 
                    match p.lang with
                    |F -> num0(Dt,Var("dbesy1("+v.code+")"))
                    |C -> num0(Dt,Var("y1("+v.code+")"))
                    |T -> num0(Dt,Var("Y_1\\left("+v.code+"\\right)"))
                    |H -> num0(Dt,Var("Y_1\\left("+v.code+"\\right)"))
                ch.d <| fun bes ->
                    bes <== (bessely1_(x))
                    code(bes)
            static member besselh1 (x:num0) = fun code ->
                let besselj1_ (v:num0) = 
                    match p.lang with
                    |F -> num0(Dt,Var("dbesj1("+v.code+")"))
                    |C -> num0(Dt,Var("j1("+v.code+")"))
                    |T -> num0(Dt,Var("J_1\\left("+v.code+"\\right)"))
                    |H -> num0(Dt,Var("J_1\\left("+v.code+"\\right)"))
                let bessely1_ (v:num0) = 
                    match p.lang with
                    |F -> num0(Dt,Var("dbesy1("+v.code+")"))
                    |C -> num0(Dt,Var("y1("+v.code+")"))
                    |T -> num0(Dt,Var("Y_1\\left("+v.code+"\\right)"))
                    |H -> num0(Dt,Var("Y_1\\left("+v.code+"\\right)"))
                ch.z <| fun bes ->
                    bes <== (besselj1_(x))-(bessely1_(x))*asm.uj
                    code(bes)