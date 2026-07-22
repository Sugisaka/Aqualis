namespace Aqualis

[<AutoOpen>]
module asm_random =
    type ContextAsm with
        member this.random (code:(((int1 -> unit) -> unit) * (double0 -> unit) -> unit)) =
            let environment = this.Environment
            match environment.GenerationContext with
            |None -> ()
            |Some context ->
                let program = context.CurrentProgram
                let write line = program.codewritein(line + "\n")
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

                match program.language with
                |Fortran ->
                    environment.ch.i (fun seedSize ->
                        environment.ch.i01 (fun seed ->
                            write ("call random_seed(size=" + seedSize.Expr.eval program + ")")
                            seed.allocate seedSize
                            try
                                environment.iter.num seedSize (fun i ->
                                    write ("call system_clock(count=" + seed.code + "(" + (i + 1).Expr.eval program + "))"))
                                runWithSeed seed
                                    (fun () -> write ("call random_seed(put=" + seed.code + "(:))"))
                                    (fun value -> "call random_seed(put=" + value.code + "(:))")
                                    (fun name -> "call random_number(" + name + ")")
                            finally seed.deallocate()))
                |C99 ->
                    program.hlist.add "<time.h>"
                    environment.ch.i1 (int0(Int 1)) (fun seed ->
                        runWithSeed seed
                            (fun () -> write "srand((unsigned) time(NULL));")
                            (fun value -> "srand(" + value.code + "[0]);")
                            (fun name -> name + " = (double)rand()/RAND_MAX;"))
                |Python ->
                    environment.ch.i1 (int0(Int 1)) (fun seed ->
                        runWithSeed seed
                            (fun () -> write "random_seed = numpy.random.default_rng()")
                            (fun value -> "random_seed = numpy.random.default_rng(" + value.code + "[0])")
                            (fun name -> name + " = random_seed.uniform(0.0, 1.0)"))
                |JavaScript ->
                    environment.ch.i1 (int0(Int 1)) (fun seed ->
                        runWithSeed seed ignore (fun _ -> "") (fun name -> name + " = Math.random();"))
                |PHP ->
                    environment.ch.i1 (int0(Int 1)) (fun seed ->
                        runWithSeed seed ignore (fun _ -> "")
                            (fun name -> name + " = random_int(0, PHP_INT_MAX) / PHP_INT_MAX;"))
                |LaTeX|HTML|HTMLSequenceDiagram ->
                    environment.ch.i1 (int0(Int 1)) (fun seed ->
                        runWithSeed seed ignore
                            (fun value -> "random_seed=" + value.code + "[0]")
                            (fun name -> name + " = (random number: 0->1)"))
                |Numeric -> ()

        member this.random_normaldistribution code =
            let environment = this.Environment
            this.random (fun (setseed,getrand) ->
                let getrandNormal (standardDeviation:double0, mean:double0, target:double0) =
                    environment.ch.d (fun a ->
                        environment.ch.d (fun b ->
                            getrand a
                            getrand b
                            target <== standardDeviation * asm.sqrt(-asm.log(a)) * asm.sin(2 * asm.pi * b) + mean))
                code(setseed,getrandNormal))
