namespace Aqualis.Tests

open System
open System.IO
open Xunit
open Aqualis

module AssignmentTests =
    let private createContext path name =
        new Aqualis(Some path, Some name, C99)

    let private assignRealPlusOne (target:double0, value:#IReal0) =
        target <== value.ToDouble0 + 1

    let private assignNumericPlusOne (target:complex0, value:#INum0) =
        target <== value.ToComplex0 + 1

    [<Fact>]
    let ``real scalar marker includes integers and doubles but excludes complex values`` () =
        Assert.True(typeof<IReal0>.IsAssignableFrom(typeof<int0>))
        Assert.True(typeof<IReal0>.IsAssignableFrom(typeof<double0>))
        Assert.False(typeof<IReal0>.IsAssignableFrom(typeof<complex0>))

    [<Fact>]
    let ``real scalar constraint accepts integer and double expressions`` () =
        use output = new TemporaryDirectory()
        use context = createContext output.Path "real-scalar-assignment.c"
        let target = double0(Var(Dt, "target", NaN), context)
        let integerValue = int0(Var(It 4, "integerValue", NaN), context)
        let doubleValue = double0(Var(Dt, "doubleValue", NaN), context)

        Assert.Same(integerValue.Expr, (integerValue :> IReal0).ToDouble0.Expr)
        Assert.Same(context, (integerValue :> IReal0).ToDouble0.Context)
        Assert.Same(doubleValue.Expr, (doubleValue :> IReal0).ToDouble0.Expr)
        Assert.Same(context, (doubleValue :> IReal0).ToDouble0.Context)
        assignRealPlusOne(target, integerValue)
        assignRealPlusOne(target, doubleValue)

    [<Fact>]
    let ``numeric scalar conversion to complex preserves expressions and contexts`` () =
        use output = new TemporaryDirectory()
        use context = createContext output.Path "numeric-scalar-assignment.c"
        let target = complex0(Var(Zt, "target", NaN), context)
        let integerValue = int0(Var(It 4, "integerValue", NaN), context)
        let doubleValue = double0(Var(Dt, "doubleValue", NaN), context)
        let complexValue = complex0(Var(Zt, "complexValue", NaN), context)

        let values : INum0 list = [integerValue; doubleValue; complexValue]
        for value in values do
            Assert.Same(value.Expr, value.ToComplex0.Expr)
            Assert.Same(context, value.ToComplex0.Context)

        assignNumericPlusOne(target, integerValue)
        assignNumericPlusOne(target, doubleValue)
        assignNumericPlusOne(target, complexValue)

    [<Fact>]
    let ``operators merge constant and variable contexts`` () =
        use output = new TemporaryDirectory()
        use context = createContext output.Path "merge.c"
        let variable = int0(Var(It 4, "value", NaN), context)
        let result = variable + int0(Int 1)
        Assert.Same(context, result.Context)

    [<Fact>]
    let ``operators reject operands from different output contexts`` () =
        use output = new TemporaryDirectory()
        use first = createContext output.Path "first-op.c"
        use second = createContext output.Path "second-op.c"
        let left = int0(Var(It 4, "left", NaN), first)
        let right = int0(Var(It 4, "right", NaN), second)
        Assert.Throws<InvalidOperationException>(Action(fun () -> left + right |> ignore))
        |> ignore

    [<Fact>]
    let ``numeric constants inherit the assignment target context`` () =
        use output = new TemporaryDirectory()
        let path = Path.Combine(output.Path, "constant.c")
        use context = createContext output.Path "constant.c"
        let constant = _0d
        Assert.True(constant.Context.CodeFile.IsNone)
        let target = double0(Var(Dt, "target", NaN), context)
        target <== constant
        context.close()
        let generated = File.ReadAllText(path) |> TestHelpers.normalizeGeneratedCode
        Assert.Equal("target = 0.0E0;", generated)

    [<Fact>]
    let ``scalar assignment writes through the left hand context`` () =
        use output = new TemporaryDirectory()
        Aqualis.makeProgramWithContext (output.Path, "assignment.c", C99) <| fun context ->
            let value = context.var.i0 "value"
            value <== 42
        let generated =
            File.ReadAllText(Path.Combine(output.Path, "assignment.c"))
            |> TestHelpers.normalizeGeneratedCode
        Assert.Equal("value = 42;", generated)

    [<Fact>]
    let ``assignment rejects values from different contexts`` () =
        use output = new TemporaryDirectory()
        use leftContext = createContext output.Path "left.c"
        use rightContext = createContext output.Path "right.c"
        let left = int0(Var(It 4, "left", NaN), leftContext)
        let right = int0(Var(It 4, "right", NaN), rightContext)
        Assert.Throws<InvalidOperationException>(Action(fun () -> left <== right))
        |> ignore

    [<Fact>]
    let ``array assignments retain their generation context`` () =
        use output = new TemporaryDirectory()
        Aqualis.makeProgramWithContext (output.Path, "arrays.c", C99) <| fun context ->
            let values1 = context.var.i1("values1", 2)
            let values2 = context.var.i2("values2", 2, 2)
            let values3 = context.var.i3("values3", 2, 2, 2)
            Assert.Same(context, values1.Context)
            Assert.Same(context, values2.Context)
            Assert.Same(context, values3.Context)
            Assert.Same(context, values1.size1.Context)
            Assert.Same(context, values2.size2.Context)
            Assert.Same(context, values3.size3.Context)
            values1 <== 1
            values2 <== 2
            values3 <== 3
        let generated = File.ReadAllText(Path.Combine(output.Path, "arrays.c"))
        Assert.Contains("values1", generated)
        Assert.Contains("values2", generated)
        Assert.Contains("values3", generated)

    [<Fact>]
    let ``dynamic one and two dimensional arrays are registered for declaration`` () =
        use output = new TemporaryDirectory()

        Compile [Fortran; C99] output.Path "dynamic-arrays" "1.0" <| fun context ->
            let integers1 = context.var.i1 "integers1"
            let doubles1 = context.var.d1 "doubles1"
            let complexes1 = context.var.z1 "complexes1"
            let integers2 = context.var.i2 "integers2"
            let doubles2 = context.var.d2 "doubles2"
            let complexes2 = context.var.z2 "complexes2"

            Assert.Equal(It 4, integers1.Etype)
            Assert.Contains((It 4,A1 0,"integers1",""), context.cvar.list)
            Assert.Contains((Dt,A1 0,"doubles1",""), context.cvar.list)
            Assert.Contains((Zt,A1 0,"complexes1",""), context.cvar.list)
            Assert.Contains((It 4,A2(0,0),"integers2",""), context.cvar.list)
            Assert.Contains((Dt,A2(0,0),"doubles2",""), context.cvar.list)
            Assert.Contains((Zt,A2(0,0),"complexes2",""), context.cvar.list)

            integers1.allocate 2
            doubles1.allocate 2
            complexes1.allocate 2
            integers2.allocate(2,3)
            doubles2.allocate(2,3)
            complexes2.allocate(2,3)

        let generated = File.ReadAllText(Path.Combine(output.Path, "dynamic-arrays.c"))
        Assert.Contains("int *integers1 = NULL;", generated)
        Assert.Contains("int integers1_size[1] = { -1 };", generated)
        Assert.Contains("double *doubles1 = NULL;", generated)
        Assert.Contains("double complex *complexes1 = NULL;", generated)
        Assert.Contains("int *integers2 = NULL;", generated)
        Assert.Contains("int integers2_size[2] = { -1, -1 };", generated)
        Assert.Contains("double *doubles2 = NULL;", generated)
        Assert.Contains("double complex *complexes2 = NULL;", generated)
        Assert.Contains("integers1 = (int *)malloc(sizeof(int) * (size_t)integers1_size[0]);", generated)

        Assert.Contains("SIZE_MAX /", generated)

        let fortran = File.ReadAllText(Path.Combine(output.Path, "dynamic-arrays.f90"))
        Assert.Contains("integer,allocatable :: integers1(:)", fortran)
        Assert.Contains("integer :: integers1_size(1:1) = (/ -1 /)", fortran)
        Assert.Contains("double precision,allocatable :: doubles1(:)", fortran)
        Assert.Contains("complex(kind(0d0)),allocatable :: complexes1(:)", fortran)
        Assert.Contains("integer,allocatable :: integers2(:,:)", fortran)
        Assert.Contains("integer :: integers2_size(1:2) = (/ -1,-1 /)", fortran)
        Assert.Contains("double precision,allocatable :: doubles2(:,:)", fortran)
        Assert.Contains("complex(kind(0d0)),allocatable :: complexes2(:,:)", fortran)

    [<Fact>]
    let ``C dynamic arrays validate allocation access and deallocation in debug mode`` () =
        use output = new TemporaryDirectory()

        Compile [C99] output.Path "checked-arrays" "1.0" <| fun context ->
            context.Debug.debugMode <- true
            let vector = context.var.i1 "vector"
            let matrix = context.var.d2 "matrix"
            let tensor = context.var.z3 "tensor"
            vector.allocate 2
            matrix.allocate(2,3)
            tensor.allocate(2,3,4)
            vector[0] <== 1
            matrix[1,2] <== 2.0
            tensor[1,2,3] <== 3.0
            vector.deallocate()
            matrix.deallocate()
            tensor.deallocate()

        let generated = File.ReadAllText(Path.Combine(output.Path, "checked-arrays.c"))
        Assert.Contains("#include <stdint.h>", generated)
        Assert.Contains("int *vector = NULL;", generated)
        Assert.Contains("double *matrix = NULL;", generated)
        Assert.Contains("double complex *tensor = NULL;", generated)
        Assert.Contains("if (vector != NULL)", generated)
        Assert.Contains("if (matrix != NULL)", generated)
        Assert.Contains("if (tensor != NULL)", generated)
        Assert.Contains("vector_size[0] <= 0", generated)
        Assert.Contains("matrix_size[0] <= 0 || matrix_size[1] <= 0", generated)
        Assert.Contains("tensor_size[0] <= 0 || tensor_size[1] <= 0 || tensor_size[2] <= 0", generated)
        Assert.Contains("(size_t)vector_size[0] > SIZE_MAX / sizeof(int)", generated)
        Assert.Contains("(size_t)matrix_size[0] > SIZE_MAX / (size_t)matrix_size[1]", generated)
        Assert.Contains("(size_t)matrix_size[0] * (size_t)matrix_size[1] > SIZE_MAX / sizeof(double)", generated)
        Assert.Contains("(size_t)tensor_size[0] * (size_t)tensor_size[1] > SIZE_MAX / (size_t)tensor_size[2]", generated)
        Assert.Contains("(size_t)tensor_size[0] * (size_t)tensor_size[1] * (size_t)tensor_size[2] > SIZE_MAX / sizeof(double complex)", generated)
        Assert.Contains("memory allocation failed for array vector", generated)
        Assert.Contains("memory allocation failed for array matrix", generated)
        Assert.Contains("memory allocation failed for array tensor", generated)
        Assert.Contains("array vector is not allocated", generated)
        Assert.Contains("array matrix is not allocated", generated)
        Assert.Contains("array tensor is not allocated", generated)
        Assert.Contains("array vector is not allocated or was already freed", generated)
        Assert.Contains("array matrix is not allocated or was already freed", generated)
        Assert.Contains("array tensor is not allocated or was already freed", generated)
        Assert.Contains("index is out of range", generated)
        Assert.Contains("exit(EXIT_FAILURE);", generated)
        Assert.Matches(@"free\(vector\);\s+vector = NULL;", generated)
        Assert.Matches(@"free\(matrix\);\s+matrix = NULL;", generated)
        Assert.Matches(@"free\(tensor\);\s+tensor = NULL;", generated)

    [<Fact>]
    let ``C dynamic arrays guard allocation and clear pointers without debug mode`` () =
        use output = new TemporaryDirectory()

        Compile [C99] output.Path "unchecked-arrays" "1.0" <| fun context ->
            let vector = context.var.i1 "vector"
            let matrix = context.var.d2 "matrix"
            let tensor = context.var.z3 "tensor"
            vector.allocate 2
            matrix.allocate(2,3)
            tensor.allocate(2,3,4)
            vector.deallocate()
            matrix.deallocate()
            tensor.deallocate()

        let generated = File.ReadAllText(Path.Combine(output.Path, "unchecked-arrays.c"))
        Assert.Matches(@"free\(vector\);\s+vector = NULL;", generated)
        Assert.Matches(@"free\(matrix\);\s+matrix = NULL;", generated)
        Assert.Matches(@"free\(tensor\);\s+tensor = NULL;", generated)
        Assert.Contains("SIZE_MAX /", generated)
        Assert.Contains("memory allocation failed for array vector", generated)
        Assert.Contains("memory allocation failed for array matrix", generated)
        Assert.Contains("memory allocation failed for array tensor", generated)

    [<Fact>]
    let ``widening array assignments reject scalars from different contexts`` () =
        use output = new TemporaryDirectory()
        use leftContext = createContext output.Path "left-arrays.c"
        use rightContext = createContext output.Path "right-scalars.c"
        let integerValue = rightContext.var.i0 "integerValue"
        let doubleValue = rightContext.var.d0 "doubleValue"
        let assignments : (unit -> unit) list =
            [
                fun () -> leftContext.var.d1("double1", 1) <== integerValue
                fun () -> leftContext.var.z1("complex1FromInt", 1) <== integerValue
                fun () -> leftContext.var.z1("complex1FromDouble", 1) <== doubleValue
                fun () -> leftContext.var.d2("double2", 1, 1) <== integerValue
                fun () -> leftContext.var.z2("complex2FromInt", 1, 1) <== integerValue
                fun () -> leftContext.var.z2("complex2FromDouble", 1, 1) <== doubleValue
                fun () -> leftContext.var.d3("double3", 1, 1, 1) <== integerValue
                fun () -> leftContext.var.z3("complex3FromInt", 1, 1, 1) <== integerValue
                fun () -> leftContext.var.z3("complex3FromDouble", 1, 1, 1) <== doubleValue
            ]

        for assignment in assignments do
            Assert.Throws<InvalidOperationException>(Action(fun () -> assignment()))
            |> ignore

        leftContext.close()
        Assert.Equal("", File.ReadAllText(Path.Combine(output.Path, "left-arrays.c")))

    [<Fact>]
    let ``widening array assignments generate code in the shared context`` () =
        use output = new TemporaryDirectory()

        Aqualis.makeProgramWithContext (output.Path, "widening-arrays.c", C99) <| fun context ->
            let integerValue = context.var.i0 "integerValue"
            let doubleValue = context.var.d0 "doubleValue"
            context.var.d1("double1", 1) <== integerValue
            context.var.z1("complex1FromInt", 1) <== integerValue
            context.var.z1("complex1FromDouble", 1) <== doubleValue
            context.var.d2("double2", 1, 1) <== integerValue
            context.var.z2("complex2FromInt", 1, 1) <== integerValue
            context.var.z2("complex2FromDouble", 1, 1) <== doubleValue
            context.var.d3("double3", 1, 1, 1) <== integerValue
            context.var.z3("complex3FromInt", 1, 1, 1) <== integerValue
            context.var.z3("complex3FromDouble", 1, 1, 1) <== doubleValue

        let generated = File.ReadAllText(Path.Combine(output.Path, "widening-arrays.c"))
        for arrayName in
            [
                "double1"
                "complex1FromInt"
                "complex1FromDouble"
                "double2"
                "complex2FromInt"
                "complex2FromDouble"
                "double3"
                "complex3FromInt"
                "complex3FromDouble"
            ] do
            Assert.Contains(arrayName, generated)
        Assert.Contains("= integerValue;", generated)
        Assert.Contains("= doubleValue;", generated)
