﻿(*
Copyright (c) 2022 Jun-ichiro Sugisaka

This software is released under the MIT License.
http://opensource.org/licenses/mit-license.php
*)
namespace Aqualis
    
    open Aqualis_base

    [<AutoOpen>]
    module asm_random =
        type asm with
            ///<summary>0～1の一様乱数を取得</summary>
            static member random (code:(((num1->unit)->unit)*(num0->unit)->unit)) =
                match p.lang with
                |Fortran ->
                    ch.i <| fun seedsize ->
                    ch.i01 <| fun seed ->
                        p.codewrite("call random_seed(size="+seedsize.code+")")
                        seed.allocate(seedsize)
                        iter.num seedsize <| fun i ->
                            p.codewrite("call system_clock(count="+seed.code+"("+(i+1).code+"))")
                            p.codewrite("call random_seed(put="+seed.code+"(:))")
                        let setseed code =
                            code seed
                            p.codewrite("call random_seed(put="+seed.code+"(:))")
                        let getrand (x:num0) =
                            match x with
                            |Var(_,n) -> 
                                p.codewrite("call random_number("+n+")")
                            |_ -> 
                                p.codewrite("Error:double型以外の変数に乱数値を代入できません")
                        code(setseed,getrand)
                        seed.deallocate()
                |C99 ->
                    p.incld("<time.h>")
                    p.codewrite("srand((unsigned) time(NULL));")
                    ch.i1 _1 <| fun seed ->
                        let setseed code =
                            code seed
                            p.codewrite("srand("+seed.code+"[0]);")
                        let getrand (x:num0) =
                            match x with
                            |Var(_,n) -> 
                                p.codewrite(n + " = (double)rand()/RAND_MAX;")
                            |_ -> 
                                p.codewrite("Error:double型以外の変数に乱数値を代入できません")
                        code(setseed,getrand)
                |LaTeX ->
                    ch.i1 _1 <| fun seed ->
                        let setseed code =
                            code seed
                            p.codewrite("random_seed="+seed.code+"[0])")
                        let getrand (x:num0) =
                            match x with
                            |Var(_,n) -> 
                                p.codewrite(n + " = (random number: 0->1);")
                            |_ -> 
                                p.codewrite("Error:double型以外の変数に乱数値を代入できません")
                        code(setseed,getrand)
                |HTML ->
                    ch.i1 _1 <| fun seed ->
                        let setseed code =
                            code seed
                            p.codewrite("random_seed="+seed.code+"[0])")
                        let getrand (x:num0) =
                            match x with
                            |Var(_,n) -> 
                                p.codewrite(n + " = (random number: 0->1);")
                            |_ -> 
                                p.codewrite("Error:double型以外の変数に乱数値を代入できません")
                        code(setseed,getrand)
                |Python ->
                    p.codewrite("random_seed = numpy.random.default_rng()")
                    ch.i1 _1 <| fun seed ->
                        let setseed code =
                            code seed
                            p.codewrite("random_seed = numpy.random.default_rng("+seed.code+"[0])")
                        let getrand (x:num0) =
                            match x with
                            |Var(_,n) -> 
                                p.codewrite(n + " = random_seed.uniform(0.0, 1.0)")
                            |_ -> 
                                p.codewrite("Error:double型以外の変数に乱数値を代入できません")
                        code(setseed,getrand)
            ///<summary>0～1の一様乱数を取得</summary>
            static member random_s (seed_:int) = fun code ->
                match p.lang with
                |Fortran ->
                    ch.i <| fun seedsize ->
                    ch.i01 <| fun seed ->
                        p.codewrite("call random_seed(size="+seedsize.code+")")
                        seed.allocate(seedsize)
                        iter.num seedsize <| fun i ->
                            p.codewrite(seed.code+"(:) = "+seed_.ToString())
                            p.codewrite("call random_seed(put="+seed.code+"(:))")
                        let setseed code =
                            code seed
                            p.codewrite("call random_seed(put="+seed.code+"(:))")
                        let getrand (x:num0) =
                            match x with
                            |Var(_,n) -> 
                                p.codewrite("call random_number("+n+")")
                            |_ -> 
                                p.codewrite("Error:double型以外の変数に乱数値を代入できません")
                        code(setseed,getrand)
                        seed.deallocate()
                |C99 ->
                    p.codewrite("srand((unsigned) time(NULL));")
                    ch.i1 _1 <| fun seed ->
                        let setseed code =
                            code seed
                            p.codewrite("srand("+seed_.ToString()+");")
                        let getrand (x:num0) =
                            match x with
                            |Var(_,n) -> 
                                p.codewrite(n + " = (double)rand()/RAND_MAX;")
                            |_ -> 
                                p.codewrite("Error:double型以外の変数に乱数値を代入できません")
                        code(setseed,getrand)
                |LaTeX ->
                    ch.i1 _1 <| fun seed ->
                        let setseed code =
                            code seed
                            p.codewrite("random_seed="+seed_.ToString())
                        let getrand (x:num0) =
                            match x with
                            |Var(_,n) -> 
                                p.codewrite(n + " = (random number: 0->1);")
                            |_ -> 
                                p.codewrite("Error:double型以外の変数に乱数値を代入できません")
                        code(setseed,getrand)
                |HTML ->
                    ch.i1 _1 <| fun seed ->
                        let setseed code =
                            code seed
                            p.codewrite("random_seed="+seed_.ToString())
                        let getrand (x:num0) =
                            match x with
                            |Var(_,n) -> 
                                p.codewrite(n + " = (random number: 0->1);")
                            |_ -> 
                                p.codewrite("Error:double型以外の変数に乱数値を代入できません")
                        code(setseed,getrand)
                |Python ->
                    p.codewrite("rng_"+seed_.ToString()+" = numpy.random.default_rng()")
                    ch.i1 _1 <| fun seed ->
                        let setseed code =
                            code seed
                            p.codewrite("rng_"+seed_.ToString()+" = numpy.random.default_rng("+seed_.ToString()+")")
                        let getrand (x:num0) =
                            match x with
                            |Var(_,n) -> 
                                p.codewrite(n + " = rng_"+seed_.ToString()+".uniform(0.0, 1.0)")
                            |_ -> 
                                p.codewrite("Error:double型以外の変数に乱数値を代入できません")
                        code(setseed,getrand)

            ///<summary>中心m、標準偏差sの正規分布に従う乱数を取得</summary>
            static member random_normaldistribution code =
                asm.random <| fun (setseed,getrand) ->
                    let getrand_normal (s:num0,m:num0,x:num0) =
                        ch.dd <| fun (a,b) ->
                        getrand a
                        getrand b
                        x <== s * asm.sqrt(-asm.log(a)) * asm.sin(2*asm.pi*b) + m
                    code(setseed,getrand_normal)
                    
            ///<summary>中心m、標準偏差sの正規分布に従う乱数を取得</summary>
            static member random_s_normaldistribution (seed:int) code =
                asm.random_s seed <| fun (setseed,getrand) ->
                    let getrand_normal (s:num0,m:num0,x:num0) =
                        ch.dd <| fun (a,b) ->
                        getrand a
                        getrand b
                        x <== s * asm.sqrt(-asm.log(a)) * asm.sin(2*asm.pi*b) + m
                    code(setseed,getrand_normal)
                    