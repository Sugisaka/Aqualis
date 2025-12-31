namespace Aqualis
    
    [<AutoOpen>]
    module asm_random =
        type asm with
            ///<summary>0～1の一様乱数を取得</summary>
            static member random (code:(((num1->unit)->unit)*(num0->unit)->unit)) =
                match programList[prIndex].language with
                |Fortran ->
                    ch.i <| fun seedsize ->
                    ch.i01 <| fun seed ->
                        codewrite("call random_seed(size="+seedsize.Expr.eval (programList[prIndex])+")")
                        seed.allocate seedsize
                        iter.num seedsize <| fun i ->
                            codewrite("call system_clock(count="+seed.code+"("+(i+1).Expr.eval (programList[prIndex])+"))")
                            codewrite("call random_seed(put="+seed.code+"(:))")
                        let setseed code =
                            code seed
                            codewrite("call random_seed(put="+seed.code+"(:))")
                        let getrand (x:num0) =
                            match x.Expr with
                            |Var(_,n,_) -> 
                                codewrite("call random_number("+n+")")
                            |_ -> 
                                codewrite "Error:double型以外の変数に乱数値を代入できません"
                        code(setseed,getrand)
                        seed.deallocate()
                |C99 ->
                    programList[prIndex].hlist.add "<time.h>"
                    codewrite "srand((unsigned) time(NULL));"
                    ch.i1 _1 <| fun seed ->
                        let setseed code =
                            code seed
                            codewrite("srand("+seed.code+"[0]);")
                        let getrand (x:num0) =
                            match x.Expr with
                            |Var(_,n,_) -> 
                                codewrite(n + " = (double)rand()/RAND_MAX;")
                            |_ -> 
                                codewrite "Error:double型以外の変数に乱数値を代入できません"
                        code(setseed,getrand)
                |LaTeX ->
                    ch.i1 _1 <| fun seed ->
                        let setseed code =
                            code seed
                            codewrite("random_seed="+seed.code+"[0])")
                        let getrand (x:num0) =
                            match x.Expr with
                            |Var(_,n,_) -> 
                                codewrite(n + " = (random number: 0->1);")
                            |_ -> 
                                codewrite "Error:double型以外の変数に乱数値を代入できません"
                        code(setseed,getrand)
                |HTML ->
                    ch.i1 _1 <| fun seed ->
                        let setseed code =
                            code seed
                            codewrite("random_seed="+seed.code+"[0])")
                        let getrand (x:num0) =
                            match x.Expr with
                            |Var(_,n,_) -> 
                                codewrite(n + " = (random number: 0->1);")
                            |_ -> 
                                codewrite "Error:double型以外の変数に乱数値を代入できません"
                        code(setseed,getrand)
                |Python ->
                    codewrite "random_seed = numpy.random.default_rng()"
                    ch.i1 _1 <| fun seed ->
                        let setseed code =
                            code seed
                            codewrite("random_seed = numpy.random.default_rng("+seed.code+"[0])")
                        let getrand (x:num0) =
                            match x.Expr with
                            |Var(_,n,_) -> 
                                codewrite(n + " = random_seed.uniform(0.0, 1.0)")
                            |_ -> 
                                codewrite "Error:double型以外の変数に乱数値を代入できません"
                        code(setseed,getrand)
                |JavaScript ->
                    ch.i1 1 <| fun seed ->
                        let setseed code =
                            code seed
                        let getrand (x:num0) =
                            match x.Expr with
                            |Var(_,n,_) -> 
                                codewrite(n + " = Math.random();")
                            |_ -> 
                                codewrite "Error:double型以外の変数に乱数値を代入できません"
                        code(setseed,getrand)
                        seed.deallocate()
                |PHP ->
                    ch.i1 1 <| fun seed ->
                        let setseed code =
                            code seed
                        let getrand (x:num0) =
                            match x.Expr with
                            |Var(_,n,_) -> 
                                codewrite(n + " = random_int(0, PHP_INT_MAX) / PHP_INT_MAX;")
                            |_ -> 
                                codewrite "Error:double型以外の変数に乱数値を代入できません"
                        code(setseed,getrand)
                        seed.deallocate()
                |Numeric ->
                    ()
                    
            ///<summary>中心m、標準偏差sの正規分布に従う乱数を取得</summary>
            static member random_normaldistribution code =
                asm.random <| fun (setseed,getrand) ->
                    let getrand_normal (s:num0,m:num0,x:num0) =
                        ch.dd <| fun (a,b) ->
                        getrand a
                        getrand b
                        x <== s * asm.sqrt(-asm.log(a)) * asm.sin(2*asm.pi*b) + m
                    code(setseed,getrand_normal)
