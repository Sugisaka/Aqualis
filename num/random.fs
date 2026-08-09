namespace Aqualis

[<AutoOpen>]
module asm_random =
    type ContextAsm with
        member this.random (code:(((int1 -> unit) -> unit) * (double0 -> unit) -> unit)) =
            let context = this.Environment
            let write line = context.codewritein(line + "\n")
            let runWithSeed (seed:int1) initialize setSeedText randomText =
                initialize()
                let setseed seedCode =
                    seedCode seed
                    write (setSeedText seed)
                let getrand (value:double0) =
                    match value.Expr with
                    |Var(_,name,_) -> write (randomText name)
                    |_ -> invalidArg (nameof value) "The random-number target must be a scalar variable."
                code(setseed,getrand)

            match context.language with
            |Fortran ->
                context.ch.i (fun seedSize ->
                    context.ch.i01 (fun seed ->
                        write ("call random_seed(size=" + seedSize.Expr.eval context + ")")
                        seed.allocate seedSize
                        try
                            context.iter.num seedSize (fun i ->
                                write ("call system_clock(count=" + seed.code + "(" + (i + 1).Expr.eval context + "))"))
                            runWithSeed seed
                                (fun () -> write ("call random_seed(put=" + seed.code + "(:))"))
                                (fun value -> "call random_seed(put=" + value.code + "(:))")
                                (fun name -> "call random_number(" + name + ")")
                        finally seed.deallocate()))
            |C99 ->
                context.hlist.add "<time.h>"
                context.ch.i1 (int0(Int 1)) (fun seed ->
                    runWithSeed seed
                        (fun () -> write "srand((unsigned) time(NULL));")
                        (fun value -> "srand(" + value.code + "[0]);")
                        (fun name -> name + " = (double)rand()/RAND_MAX;"))
            |Python ->
                context.ch.i1 (int0(Int 1)) (fun seed ->
                    runWithSeed seed
                        (fun () -> write "random_seed = numpy.random.default_rng()")
                        (fun value -> "random_seed = numpy.random.default_rng(" + value.code + "[0])")
                        (fun name -> name + " = random_seed.uniform(0.0, 1.0)"))
            |JavaScript ->
                context.ch.i1 (int0(Int 1)) (fun seed ->
                    runWithSeed seed ignore (fun _ -> "") (fun name -> name + " = Math.random();"))
            |PHP ->
                context.ch.i1 (int0(Int 1)) (fun seed ->
                    runWithSeed seed ignore (fun _ -> "")
                        (fun name -> name + " = random_int(0, PHP_INT_MAX) / PHP_INT_MAX;"))
            |LaTeX|HTML|HTMLSequenceDiagram ->
                context.ch.i1 (int0(Int 1)) (fun seed ->
                    runWithSeed seed ignore
                        (fun value -> "random_seed=" + value.code + "[0]")
                        (fun name -> name + " = (random number: 0->1)"))
            |Numeric -> ()

        member this.random_normaldistribution code =
            let context = this.Environment
            this.random (fun (setseed,getrand) ->
                let getrandNormal (standardDeviation:double0, mean:double0, target:double0) =
                    context.ch.d (fun a ->
                        context.ch.d (fun b ->
                            getrand a
                            getrand b
                            target <== standardDeviation * asm.sqrt(-asm.log(a)) * asm.sin(2 * asm.pi * b) + mean))
                code(setseed,getrandNormal))
