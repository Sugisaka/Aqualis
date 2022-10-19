(*
Copyright (c) 2022 Jun-ichiro Sugisaka

This software is released under the MIT License.
http://opensource.org/licenses/mit-license.php
*)
namespace Aqualis
    
    module bessel = 
        
        open Aqualis_base
        
        let set_zbessel() = p.param.module_("zbessel")
        
        let besselh0 (x:num0) code =
            match x.etype with
              |Zt ->
                  p.param.module_("zbessel")
                  p.param.source_("zbessel.f90")
                  ch.dd <| fun (zr,zi) ->
                    match p.lang with
                      |F -> p.param.codewrite("call zbesh("+(x.re.name)+", "+(x.im.name)+", 0d0, 1, 2, 1, "+zr.name+", "+zi.name+")")
                      |C89 -> p.param.codewrite("__zbessel_MOD_zbesh(&"+x.name+"->r"+", &"+x.name+"->i, 0.0, &kode, &m, &n, &"+zr.name+"->r, &"+zi.name+"->i)")
                      |C99 -> p.param.codewrite("__zbessel_MOD_zbesh(&"+x.name+"->r"+", &"+x.name+"->i, 0.0, &kode, &m, &n, &"+zr.name+"->r, &"+zi.name+"->i)")
                      |T -> p.param.codewrite("J_0\\left("+x.name+"\\right)")
                      |H -> p.param.codewrite("J_0\\left("+x.name+"\\right)")
                      |NL -> ()
                    ch.z <| fun bes ->
                      bes <== zr+zi*asm.uj
                      code(bes)
              |_ ->
                  let besselj0_ (v:num0) = 
                    match p.lang with
                    |F -> Var(Dt,"dbesj0("+x.name+")",[])
                    |C89 -> Var(Dt,"j0("+x.name+")",[])
                    |C99 -> Var(Dt,"j0("+x.name+")",[])
                    |T -> Var(Dt,"J_0\\left("+x.name+"\\right)",[])
                    |H -> Var(Dt,"J_0\\left("+x.name+"\\right)",[])
                    |NL -> NaN
                  let bessely0_ (v:num0) = 
                    match p.lang with
                    |F -> Var(Dt,"dbesy0("+x.name+")",[])
                    |C89 -> Var(Dt,"y0("+x.name+")",[])
                    |C99 -> Var(Dt,"y0("+x.name+")",[])
                    |T -> Var(Dt,"Y_0\\left("+x.name+"\\right)",[])
                    |H -> Var(Dt,"Y_0\\left("+x.name+"\\right)",[])
                    |NL -> NaN
                  ch.z <| fun bes ->
                    bes <== (besselj0_(x))-(bessely0_(x))*asm.uj
                    code(bes)
        
        let besselh1 (x:num0) code =
            match x.etype with
              |Zt ->
                  p.param.module_("zbessel")
                  ch.dd <| fun (zr,zi) ->
                    match p.lang with
                      |F -> p.param.codewrite("call zbesh("+x.re.name+", "+x.im.name+", 1d0, 1, 2, 1, "+zr.name+", "+zi.name+")")
                      |C89 -> p.param.codewrite("__zbessel_MOD_zbesh(&"+x.name+"->r"+", &"+x.name+"->i, 1.0, &kode, &m, &n, &"+zr.name+"->r, &"+zi.name+"->i)")
                      |C99 -> p.param.codewrite("__zbessel_MOD_zbesh(&"+x.name+"->r"+", &"+x.name+"->i, 1.0, &kode, &m, &n, &"+zr.name+"->r, &"+zi.name+"->i)")
                      |T -> p.param.codewrite("J_0\\left("+x.name+"\\right)")
                      |H -> p.param.codewrite("J_0\\left("+x.name+"\\right)")
                      |NL -> ()
                    ch.z <| fun bes ->
                      bes <== zr+zi*asm.uj
                      code(bes)
              |_ ->
                  let besselj1_ (v:num0) = 
                    match p.lang with
                    |F -> Var(Dt,"dbesj1("+x.name+")",[])
                    |C89 -> Var(Dt,"j1("+x.name+")",[])
                    |C99 -> Var(Dt,"j1("+x.name+")",[])
                    |T -> Var(Dt,"J_1\\left("+x.name+"\\right)",[])
                    |H -> Var(Dt,"J_1\\left("+x.name+"\\right)",[])
                    |NL -> NaN
                  let bessely1_ (v:num0) = 
                    match p.lang with
                    |F -> Var(Dt,"dbesy1("+x.name+")",[])
                    |C89 -> Var(Dt,"y1("+x.name+")",[])
                    |C99 -> Var(Dt,"y1("+x.name+")",[])
                    |T -> Var(Dt,"Y_1\\left("+x.name+"\\right)",[])
                    |H -> Var(Dt,"Y_1\\left("+x.name+"\\right)",[])
                    |NL -> NaN
                  ch.z <| fun bes ->
                    bes <== (besselj1_(x))-(bessely1_(x))*asm.uj
                    code(bes)