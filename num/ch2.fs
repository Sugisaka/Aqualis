namespace Aqualis

// Additional temporary-variable helpers belong on ContextCh.  The legacy
// ambient/static `ch` surface was intentionally removed; new helpers should
// always be implemented against CompilationEnvironment.ch.

[<AutoOpen>]
module ContextChArrayExtensions =
  type ContextCh with
    member this.c code =
        let ctx = this.Environment.RequireGenerationContext()
        TemporaryVariableScope.useOne ctx.CurrentProgram.c0.getVar
            (fun name -> Var(Structure "char",name,NaN)) code

    member this.i01 code =
        let ctx = this.Environment.RequireGenerationContext()
        TemporaryVariableScope.useOne ctx.CurrentProgram.i1.getVar
            (fun name -> int1(It 4, Var1(A1 0,name), context=ctx)) code

    member this.d01 code =
        let ctx = this.Environment.RequireGenerationContext()
        TemporaryVariableScope.useOne ctx.CurrentProgram.d1.getVar
            (fun name -> double1(Dt, Var1(A1 0,name), context=ctx)) code

    member this.z01 code =
        let ctx = this.Environment.RequireGenerationContext()
        TemporaryVariableScope.useOne ctx.CurrentProgram.z1.getVar
            (fun name -> complex1(Zt, Var1(A1 0,name), context=ctx)) code

    member this.i1 (size:int0) code =
        this.i01 (fun value ->
            value.allocate size
            try code value finally value.deallocate())

    member this.d1 (size:int0) code =
        this.d01 (fun value ->
            value.allocate size
            try code value finally value.deallocate())

    member this.z1 (size:int0) code =
        this.z01 (fun value ->
            value.allocate size
            try code value finally value.deallocate())

    member this.d02 code =
        let ctx = this.Environment.RequireGenerationContext()
        TemporaryVariableScope.useOne ctx.CurrentProgram.d2.getVar
            (fun name -> double2(Dt, Var2(A2(0,0),name), context=ctx)) code

    member this.z02 code =
        let ctx = this.Environment.RequireGenerationContext()
        TemporaryVariableScope.useOne ctx.CurrentProgram.z2.getVar
            (fun name -> complex2(Zt, Var2(A2(0,0),name), context=ctx)) code

    member this.d2 (size1:int0) (size2:int0) code =
        this.d02 (fun value ->
            value.allocate(size1,size2)
            try code value finally value.deallocate())

    member this.z2 (size1:int0) (size2:int0) code =
        this.z02 (fun value ->
            value.allocate(size1,size2)
            try code value finally value.deallocate())

    member this.iid code = this.i (fun a -> this.i (fun b -> this.d (fun c -> code(a,b,c))))
    member this.iiii code = this.i (fun a -> this.i (fun b -> this.i (fun c -> this.i (fun d -> code(a,b,c,d)))))
