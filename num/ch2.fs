namespace Aqualis

// Additional temporary-variable helpers belong on ContextCh.  The legacy
// ambient/static `ch` surface was intentionally removed; new helpers should
// always be implemented against CompilationEnvironment.ch.

/// Adds temporary array allocation helpers to <c>ContextCh</c>.
[<AutoOpen>]
module ContextChArrayExtensions =
  type ContextCh with
    /// Provides a temporary character variable to the callback and releases its name afterward.
    member this.c code =
        let ctx = this.Environment
        TemporaryVariableScope.useOne ctx.c0.getVar
            (fun name -> Var(Structure "char",name,NaN)) code

    /// Provides an unallocated temporary integer array of rank 1 to the callback.
    member this.i01 code =
        let ctx = this.Environment
        TemporaryVariableScope.useOne ctx.i1.getVar
            (fun name -> int1(It 4, Var1(A1 0,name), context=ctx)) code

    /// Provides an unallocated temporary real array of rank 1 to the callback.
    member this.d01 code =
        let ctx = this.Environment
        TemporaryVariableScope.useOne ctx.d1.getVar
            (fun name -> double1(Dt, Var1(A1 0,name), context=ctx)) code

    /// Provides an unallocated temporary complex array of rank 1 to the callback.
    member this.z01 code =
        let ctx = this.Environment
        TemporaryVariableScope.useOne ctx.z1.getVar
            (fun name -> complex1(Zt, Var1(A1 0,name), context=ctx)) code

    /// Allocates a temporary integer array of rank 1 for the callback and deallocates it afterward.
    member this.i1 (size:int0) = fun code ->
        this.i01 (fun value ->
            value.allocate size
            try code value finally value.deallocate())

    /// Allocates a temporary real array of rank 1 for the callback and deallocates it afterward.
    member this.d1 (size:int0) = fun code ->
        this.d01 (fun value ->
            value.allocate size
            try code value finally value.deallocate())

    /// Allocates a temporary complex array of rank 1 for the callback and deallocates it afterward.
    member this.z1 (size:int0) = fun code ->
        this.z01 (fun value ->
            value.allocate size
            try code value finally value.deallocate())

    /// Allocates a temporary integer array of rank 1 for the callback and deallocates it afterward.
    member this.i1 (size:int) = fun code -> this.i1 (I size) code
    /// Allocates a temporary real array of rank 1 for the callback and deallocates it afterward.
    member this.d1 (size:int) = fun code -> this.d1 (I size) code
    /// Allocates a temporary complex array of rank 1 for the callback and deallocates it afterward.
    member this.z1 (size:int) = fun code -> this.z1 (I size) code

    /// Provides an unallocated temporary integer array of rank 2 to the callback.
    member this.i02 code =
        let ctx = this.Environment
        TemporaryVariableScope.useOne ctx.i2.getVar
            (fun name -> int2(It 4, Var2(A2(0,0),name), context=ctx)) code

    /// Provides an unallocated temporary real array of rank 2 to the callback.
    member this.d02 code =
        let ctx = this.Environment
        TemporaryVariableScope.useOne ctx.d2.getVar
            (fun name -> double2(Dt, Var2(A2(0,0),name), context=ctx)) code

    /// Provides an unallocated temporary complex array of rank 2 to the callback.
    member this.z02 code =
        let ctx = this.Environment
        TemporaryVariableScope.useOne ctx.z2.getVar
            (fun name -> complex2(Zt, Var2(A2(0,0),name), context=ctx)) code

    /// Allocates a temporary integer array of rank 2 for the callback and deallocates it afterward.
    member this.i2 (size1:int0,size2:int0) = fun code ->
        this.i02 (fun value ->
            value.allocate(size1,size2)
            try code value finally value.deallocate())

    /// Allocates a temporary real array of rank 2 for the callback and deallocates it afterward.
    member this.d2 (size1:int0,size2:int0) = fun code ->
        this.d02 (fun value ->
            value.allocate(size1,size2)
            try code value finally value.deallocate())

    /// Allocates a temporary complex array of rank 2 for the callback and deallocates it afterward.
    member this.z2 (size1:int0,size2:int0) = fun code ->
        this.z02 (fun value ->
            value.allocate(size1,size2)
            try code value finally value.deallocate())

    /// Allocates a temporary integer array of rank 2 for the callback and deallocates it afterward.
    member this.i2 (size1:int0,size2:int) = fun code -> this.i2 (size1,I size2) code
    /// Allocates a temporary integer array of rank 2 for the callback and deallocates it afterward.
    member this.i2 (size1:int,size2:int0) = fun code -> this.i2 (I size1,size2) code
    /// Allocates a temporary integer array of rank 2 for the callback and deallocates it afterward.
    member this.i2 (size1:int,size2:int) = fun code -> this.i2 (I size1,I size2) code

    /// Allocates a temporary real array of rank 2 for the callback and deallocates it afterward.
    member this.d2 (size1:int0,size2:int) = fun code -> this.d2 (size1,I size2) code
    /// Allocates a temporary real array of rank 2 for the callback and deallocates it afterward.
    member this.d2 (size1:int,size2:int0) = fun code -> this.d2 (I size1,size2) code
    /// Allocates a temporary real array of rank 2 for the callback and deallocates it afterward.
    member this.d2 (size1:int,size2:int) = fun code -> this.d2 (I size1,I size2) code
    
    /// Allocates a temporary complex array of rank 2 for the callback and deallocates it afterward.
    member this.z2 (size1:int0,size2:int) = fun code -> this.z2 (size1,I size2) code
    /// Allocates a temporary complex array of rank 2 for the callback and deallocates it afterward.
    member this.z2 (size1:int,size2:int0) = fun code -> this.z2 (I size1,size2) code
    /// Allocates a temporary complex array of rank 2 for the callback and deallocates it afterward.
    member this.z2 (size1:int,size2:int) = fun code -> this.z2 (I size1,I size2) code

    /// Provides an unallocated temporary integer array of rank 3 to the callback.
    member this.i03 code =
        let ctx = this.Environment
        TemporaryVariableScope.useOne ctx.i3.getVar
            (fun name -> int3(It 4, Var3(A3(0,0,0),name), context=ctx)) code

    /// Provides an unallocated temporary real array of rank 3 to the callback.
    member this.d03 code =
        let ctx = this.Environment
        TemporaryVariableScope.useOne ctx.d3.getVar
            (fun name -> double3(Dt, Var3(A3(0,0,0),name), context=ctx)) code

    /// Provides an unallocated temporary complex array of rank 3 to the callback.
    member this.z03 code =
        let ctx = this.Environment
        TemporaryVariableScope.useOne ctx.z3.getVar
            (fun name -> complex3(Zt, Var3(A3(0,0,0),name), context=ctx)) code
            
    /// Allocates a temporary integer array of rank 3 for the callback and deallocates it afterward.
    member this.i3 (size1:int0,size2:int0,size3:int0) = fun code ->
        this.i03 (fun value ->
            value.allocate(size1,size2,size3)
            try code value finally value.deallocate())

    /// Allocates a temporary real array of rank 3 for the callback and deallocates it afterward.
    member this.d3 (size1:int0,size2:int0,size3:int0) = fun code ->
        this.d03 (fun value ->
            value.allocate(size1,size2,size3)
            try code value finally value.deallocate())

    /// Allocates a temporary complex array of rank 3 for the callback and deallocates it afterward.
    member this.z3 (size1:int0,size2:int0,size3:int0) = fun code ->
        this.z03 (fun value ->
            value.allocate(size1,size2,size3)
            try code value finally value.deallocate())

    /// Allocates a temporary integer array of rank 3 for the callback and deallocates it afterward.
    member this.i3 (size1:int,size2:int0,size3:int0) = fun code -> this.i3 (I size1,size2,size3) code
    /// Allocates a temporary integer array of rank 3 for the callback and deallocates it afterward.
    member this.i3 (size1:int0,size2:int,size3:int0) = fun code -> this.i3 (size1,I size2,size3) code
    /// Allocates a temporary integer array of rank 3 for the callback and deallocates it afterward.
    member this.i3 (size1:int0,size2:int0,size3:int) = fun code -> this.i3 (size1,size2,I size3) code
    /// Allocates a temporary integer array of rank 3 for the callback and deallocates it afterward.
    member this.i3 (size1:int0,size2:int,size3:int) = fun code -> this.i3 (size1,I size2,I size3) code
    /// Allocates a temporary integer array of rank 3 for the callback and deallocates it afterward.
    member this.i3 (size1:int,size2:int0,size3:int) = fun code -> this.i3 (I size1,size2,I size3) code
    /// Allocates a temporary integer array of rank 3 for the callback and deallocates it afterward.
    member this.i3 (size1:int,size2:int,size3:int0) = fun code -> this.i3 (I size1,I size2,size3) code
    /// Allocates a temporary integer array of rank 3 for the callback and deallocates it afterward.
    member this.i3 (size1:int,size2:int,size3:int) = fun code -> this.i3 (I size1,I size2,I size3) code

    /// Allocates a temporary real array of rank 3 for the callback and deallocates it afterward.
    member this.d3 (size1:int,size2:int0,size3:int0) = fun code -> this.d3 (I size1,size2,size3) code
    /// Allocates a temporary real array of rank 3 for the callback and deallocates it afterward.
    member this.d3 (size1:int0,size2:int,size3:int0) = fun code -> this.d3 (size1,I size2,size3) code
    /// Allocates a temporary real array of rank 3 for the callback and deallocates it afterward.
    member this.d3 (size1:int0,size2:int0,size3:int) = fun code -> this.d3 (size1,size2,I size3) code
    /// Allocates a temporary real array of rank 3 for the callback and deallocates it afterward.
    member this.d3 (size1:int0,size2:int,size3:int) = fun code -> this.d3 (size1,I size2,I size3) code
    /// Allocates a temporary real array of rank 3 for the callback and deallocates it afterward.
    member this.d3 (size1:int,size2:int0,size3:int) = fun code -> this.d3 (I size1,size2,I size3) code
    /// Allocates a temporary real array of rank 3 for the callback and deallocates it afterward.
    member this.d3 (size1:int,size2:int,size3:int0) = fun code -> this.d3 (I size1,I size2,size3) code
    /// Allocates a temporary real array of rank 3 for the callback and deallocates it afterward.
    member this.d3 (size1:int,size2:int,size3:int) = fun code -> this.d3 (I size1,I size2,I size3) code

    /// Allocates a temporary complex array of rank 3 for the callback and deallocates it afterward.
    member this.z3 (size1:int,size2:int0,size3:int0) = fun code -> this.z3 (I size1,size2,size3) code
    /// Allocates a temporary complex array of rank 3 for the callback and deallocates it afterward.
    member this.z3 (size1:int0,size2:int,size3:int0) = fun code -> this.z3 (size1,I size2,size3) code
    /// Allocates a temporary complex array of rank 3 for the callback and deallocates it afterward.
    member this.z3 (size1:int0,size2:int0,size3:int) = fun code -> this.z3 (size1,size2,I size3) code
    /// Allocates a temporary complex array of rank 3 for the callback and deallocates it afterward.
    member this.z3 (size1:int0,size2:int,size3:int) = fun code -> this.z3 (size1,I size2,I size3) code
    /// Allocates a temporary complex array of rank 3 for the callback and deallocates it afterward.
    member this.z3 (size1:int,size2:int0,size3:int) = fun code -> this.z3 (I size1,size2,I size3) code
    /// Allocates a temporary complex array of rank 3 for the callback and deallocates it afterward.
    member this.z3 (size1:int,size2:int,size3:int0) = fun code -> this.z3 (I size1,I size2,size3) code
    /// Allocates a temporary complex array of rank 3 for the callback and deallocates it afterward.
    member this.z3 (size1:int,size2:int,size3:int) = fun code -> this.z3 (I size1,I size2,I size3) code

    /// Provides temporary integer, integer, real scalar variables to the callback and releases them afterward.
    member this.iid code = this.i (fun a -> this.i (fun b -> this.d (fun c -> code(a,b,c))))
    /// Provides temporary integer, integer, integer, integer scalar variables to the callback and releases them afterward.
    member this.iiii code = this.i (fun a -> this.i (fun b -> this.i (fun c -> this.i (fun d -> code(a,b,c,d)))))
