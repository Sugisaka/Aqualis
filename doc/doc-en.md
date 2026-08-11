# Aqualis

- [Installation](#installation)
- [Running a Source File](#running-a-source-file)
- [Preamble](#preamble)
- [Comments](#comments)
- [Defining and Assigning Variables](#defining-and-assigning-variables)
- [Console Output](#console-output)
- [Arithmetic Operations](#arithmetic-operations)
- [Aqualis Mathematical Functions](#aqualis-mathematical-functions)
- [Arrays](#arrays)
- [Conditional Branches](#conditional-branches)
- [Loops](#loops)
- [Expressions and Functions](#expressions-and-functions)
- [File I/O](#file-io)
- [Linear Algebra](#linear-algebra)
- [OpenMP](#openmp)
- [OpenACC](#openacc)
- [Class Definition Example](#class-definition-example)
- [Symbolic Differentiation](#symbolic-differentiation)

## Installation

[Back to top](#aqualis)

1. Install the .NET 10 SDK. If you use Visual Studio or Build Tools for Visual Studio, also select **F# desktop language support**.
2. Run the following command in the repository root to build the Release version of `Aqualis.dll`.

    ```powershell
    dotnet build Aqualis.fsproj -c Release
    ```

3. Run the following command to copy the DLL to `C:\Aqualis\lib\(version number)`, where the directory name corresponds to the library version.

    ```powershell
    dotnet fsi install.fsx
    ```

   If you copy the DLL elsewhere manually, update `#I` in your F# script accordingly.

## Running a Source File

[Back to top](#aqualis)

Edit and run an F# script file with the `.fsx` extension. Depending on the selected target, Aqualis generates a source file such as an `.f90` or `.c` file. For compiled targets it also generates a shell script that automates compilation and execution.

## Preamble

[Back to top](#aqualis)

Begin each `.fsx` file as follows:

```fsharp
//#############################################################################
// project title
let projectname = "template"
let version = "1.0.0"
//#############################################################################
 
let outputdir = @"C:\home\work"

#I "C:\\Aqualis\\lib\\188_0_0_0"
#r "Aqualis.dll"
 
open Aqualis
 
Compile [Fortran] outputdir projectname version <| fun ctx ->
    (program body)
```

- Line 2: Describe the program. A description may span multiple lines, but each line must start with `//`.
- Line 3: Specify the project name inside `""`. ASCII letters, digits, and underscores may be used.
- Line 4: Specify an arbitrary version string.
- Line 7: Specify the source-file output directory.
- Line 9: Specify the directory containing `Aqualis.dll`.
- Line 10: Load `Aqualis.dll`.
- Line 12: Open the `Aqualis` namespace.
- Line 14: Specify target languages inside `[]`. Separate multiple targets with semicolons (`;`). Supported targets are:
  - Fortran
  - C99
  - Python
  - LaTeX
  - HTML
  - HTMLSequenceDiagram
  - JavaScript
  - PHP
  - Numeric (evaluates directly without generating a source file)
- Line 14: `ctx` is the context used for code generation. You may choose another name.

In the following example, `ctx.print.s "aaa"` and `ctx.print.s "bbb"` are converted to Fortran. `ctx.print.s "ccc"` is outside the callback because its indentation has returned, so it is not part of the generated code.

```fsharp
Compile [Fortran] outputdir projectname version <| fun ctx ->
    ctx.print.s "aaa"
    ctx.print.s "bbb"
ctx.print.s "ccc"
```

## Comments

[Back to top](#aqualis)

- Block comment: enclose text in `(*` and `*)`. A block comment may span multiple lines.
- Line comment: text from `//` to the end of the line is a comment.

### Comments in generated source files

Comments written with `ctx.group.comment` are also emitted into generated source files.

```fsharp
ctx.group.comment "This comment is also written to the generated code"
```

### Documentation comments

F# documentation comments may be used. They are not copied to the generated source code.

### Disabling a group of commands

Use `ctx.group.whenEnabled` to enable or disable part of code generation conditionally. The body in the following example is not generated.

```fsharp
ctx.group.whenEnabled false <| fun () ->
    ctx.print.s "This line is not generated"
```

The rest of this manual assumes that the Aqualis context passed to `Compile` is named `ctx`.

## Defining and Assigning Variables

[Back to top](#aqualis)

Declare a variable as follows:

```fsharp
ctx.ch.i <| fun x ->
```

The `i` identifies the variable type, and `x` is the F# name used in the callback. Indent subsequent lines; `x` remains available until the indentation returns.

Use `ctx.ch.I`, `ctx.ch.D`, or `ctx.ch.Z` to request a name in the generated source. In this example, Aqualis attempts to generate a variable named `aaa`. If that name is unavailable, for example because it is already in use, Aqualis assigns another name automatically.

```fsharp
ctx.ch.I "aaa" <| fun x ->
```

|Specifier|Aqualis type|Variable type|
|---|---|---|
|`i`|`int0`|Integer|
|`d`|`double0`|Double-precision floating point|
|`z`|`complex0`|Double-precision complex number|
|`I`|`int0`|Integer with a requested generated name|
|`D`|`double0`|Double with a requested generated name|
|`Z`|`complex0`|Complex number with a requested generated name|

Use `<==` to assign a value to an Aqualis variable.

```fsharp
ctx.ch.i <| fun x ->
    x <== 1
```

An F# `let` binding defines a constant. Here `a` is an `int` and `b` is a `double`; they cannot be assigned to with `<==`.

```fsharp
let a = 1
let b = 1.234
```

Assigning a decimal or complex value to an integer variable is an error. An integer can be assigned to a double variable, but a complex value cannot.

The scope of a variable starts inside the callback passed to `ctx.ch` and ends when indentation returns. The following code is invalid because the final assignment is outside the callback.

```fsharp
ctx.ch.i <| fun x ->
    x <== 1
    x <== 2
x <== 3 // x is not available here
```

Corrected version:

```fsharp
ctx.ch.i <| fun x ->
    x <== 1
    x <== 2
    x <== 3
```

Nest callbacks to define several variables.

```fsharp
ctx.ch.i <| fun x ->
    ctx.ch.i <| fun y ->
        x <== 1
        y <== 2
```

Both `x` and `y` are available in the two assignments. By contrast, `x` is out of scope in the second callback below.

```fsharp
ctx.ch.i <| fun x ->
    x <== 1
ctx.ch.i <| fun y ->
    x <== 1 // x is not available here
    y <== 2
```

When a nested callback reuses the same F# name, it shadows the outer name. The final output below refers to the outer `x`.

```fsharp
ctx.ch.i <| fun x ->
    x <== 1
    ctx.ch.i <| fun x ->
        x <== 2
    ctx.print.t x
```

Helpers can declare up to four variables at once, avoiding excessive nesting.

```fsharp
ctx.ch.ii <| fun (x,y) ->
    x <== 1
    y <== 2
```

`ii` means two integer variables.

```fsharp
ctx.ch.iiii <| fun (x,y,z,w) ->
    x <== 1
    y <== 2
    z <== 3
    w <== 4
```

Different types may be combined. Here `x` is an integer, `y` and `z` are doubles, and `w` is complex.

```fsharp
ctx.ch.iddz <| fun (x,y,z,w) ->
    x <== 1
    y <== 2.0
    z <== 3.0
    w <== 4.0+asm.uj*5.0
```

Specifiers must appear in the order `i` (integer), `d` (real), then `z` (complex). For example, `ctx.ch.iiz` is available, but `ctx.ch.izi` and `ctx.ch.zid` are not.

### Complex numbers

The following assigns $5-3j$ to `w`.

```fsharp
w <== 5.0-asm.uj*3.0
```

`asm.uj` is the imaginary unit. Complex variables expose these properties:

|Property|Meaning|
|---|---|
|`abs`|Absolute value|
|`re`|Real part|
|`im`|Imaginary part|
|`pha`|Phase|
|`pow`|Squared absolute value|

For example, assign the squared absolute value of `x` to `a` as follows:

```fsharp
a <== x.pow
```

## Console Output

[Back to top](#aqualis)

Use `ctx.print` to print strings and variable values.

```fsharp
ctx.print.s "Hello World!"
ctx.print.t x
```

Use the `++` operator to combine two or more variables, or strings and variables.

```fsharp
ctx.print.tt <| "aaa"++a++"bbb"++b
```

## Arithmetic Operations

[Back to top](#aqualis)

```fsharp
z <== x + y  // addition
z <== x - y  // subtraction
z <== x * y  // multiplication
z <== x / y  // floating-point division
z <== x ./ y // integer division; the fractional part is discarded
z <== x % y  // remainder
z <== -x     // negation
```

With `/`, `z` is floating point whether `x` and `y` are integers or doubles. With `./`, `x` and `y` are integers and the result is an integer.

## Aqualis Mathematical Functions

The following mathematical functions are available.

|Example|Meaning|
|---|---|
|`asm.uj`|Imaginary unit|
|`asm.pi`|Pi|
|`asm.abs(x)`|Absolute value of `x`|
|`asm.pow(x,y)`|`x` raised to the power `y`|
|`asm.exp(x)`|Exponential function|
|`asm.conj(x)`|Complex conjugate of `x`|
|`asm.sin(x)`|Sine|
|`asm.cos(x)`|Cosine|
|`asm.tan(x)`|Tangent|
|`asm.asin(x)`|Inverse sine|
|`asm.acos(x)`|Inverse cosine|
|`asm.atan(x)`|Inverse tangent, in the range $-\pi/2$ to $\pi/2$|
|`asm.atan2(y,x)`|Inverse tangent of $y/x$, in the range $-\pi$ to $\pi$|
|`asm.log(x)`|Natural logarithm|
|`asm.log10(x)`|Base-10 logarithm|
|`asm.sqrt(x)`|Square root|
|`asm.floor(x)`|Round down|
|`asm.ceil(x)`|Round up|
|`asm.toint(x)`|Convert floating point to integer|
|`asm.todouble(x)`|Convert integer to floating point|

### Hankel functions

Calculate $H^{(2)}_0(x)$ and receive the result as `h`:

```fsharp
asm.besselh0 x <| fun h ->
    ctx.print.t h
```

Calculate $H^{(2)}_1(x)$:

```fsharp
asm.besselh1 x <| fun h ->
    ctx.print.t h
```

## Arrays

[Back to top](#aqualis)

Aqualis supports one-, two-, and three-dimensional arrays. For example, the Aqualis type for a one-dimensional integer array is `int1`, and the type for a three-dimensional complex array is `complex3`.

### One-dimensional arrays

In `ctx.ch.i1`, `i` selects the element type (`d` and `z` are also available), `1` is the number of dimensions, `5` is the number of elements, and `a` is the F# variable name.

```fsharp
ctx.ch.i1 5 <| fun a ->
```

You may declare a dynamic array first and allocate it later.

```fsharp
ctx.ch.i01 <| fun a ->
    a.allocate(5)
    a.deallocate()
```

`allocate` reserves memory for the specified number of elements. `deallocate` releases it. After deallocation, the array cannot be used until it is allocated again.

- The element count may be an integer literal, an `int`, or an `int0`.
- `a.clear()` initializes all elements to zero. It can also be used with scalar variables.
- Aqualis array indices start at zero. For `a.size1` elements, valid indices are `0` through `a.size1-1`. Aqualis converts these to one-based indices in generated Fortran code.
- `a.size1` returns the number of elements.

```fsharp
ctx.ch.i1 5 <| fun a ->
    a[0] <== 5             // first element
    a[1] <== 10            // second element
    a[a.size1-1] <== 10    // last element
    a[5] <== 10            // error: out of range
    a[-1] <== 10           // error: out of range
```

### Two-dimensional arrays

A two-dimensional array represents values arranged in rows and columns, for example an image or an electric-field distribution on a plane.

```fsharp
ctx.ch.i2 (3,5) <| fun a ->
```

- Use `a.clear()` to initialize every element to zero.
- Access an element with syntax such as `a[1,2]`.
- `a.size1` and `a.size2` return the sizes of the first and second dimensions.

### Three-dimensional arrays

```fsharp
ctx.ch.i3 (3,4,5) <| fun a ->
```

### Array slices

The following element-wise operation on arrays `x`, `y`, and `z`:

```fsharp
ctx.iter.num z.size1 <| fun i ->
    z[i] <== x[i] - y[i]
```

can be written as follows. The same applies to two- and three-dimensional arrays.

```fsharp
z <== x - y
```

You can also specify a range. To operate only on indices 1 through 3:

```fsharp
z[(1,3)] <== x[(1,3)] - y[(1,3)]
```

`()` selects all elements in a dimension. For example, the following operates on every element in column 4 of a two-dimensional array.

```fsharp
z[(),4] <== x[(),4] - y[(),4]
```

## Conditional Branches

[Back to top](#aqualis)

There are three basic forms.

### Pattern 1

Run `code 1` when `condition 1` is true.

```fsharp
ctx.br.if1 (condition1) <| fun () ->
    (code1)
```

### Pattern 2

Run `code 1` when `condition 1` is true; otherwise run `code 2`.

```fsharp
ctx.br.if2 (condition1)
<| fun () ->
    (code1)
<| fun () ->
    (code2)
```

### Pattern 3

Use `ctx.br.branch` for an `if`/`else if` chain.

```fsharp
ctx.br.branch <| fun b ->
    b.IF (condition1) <| fun () ->
        (code1)
    b.IF (condition2) <| fun () ->
        (code2)
    b.IF (condition3) <| fun () ->
        (code3)
```

Add `b.EL` for a final `else` branch.

```fsharp
ctx.br.branch <| fun b ->
    b.IF (condition1) <| fun () ->
        (code1)
    b.IF (condition2) <| fun () ->
        (code2)
    b.IF (condition3) <| fun () ->
        (code3)
    b.EL <| fun () ->
        (code4)
```

Any number of `b.IF` branches may be added. Branches can also be nested; use different branch-variable names at each level.

```fsharp
ctx.br.branch <| fun b1 ->
    b1.IF (condition1A) <| fun () ->
        (code1A)
    b1.IF (condition2) <| fun () ->
        ctx.br.branch <| fun b2 ->
            b2.IF (condition2A) <| fun () ->
                (code2A)
            b2.IF (condition2B) <| fun () ->
                (code2B)
    b1.IF (condition1B) <| fun () ->
        (code1B)
    b1.EL <| fun () ->
        (code1C)
```

### Conditions

|Expression|Meaning|
|---|---|
|`x.>y`|`x` is greater than `y`|
|`x.<y`|`x` is less than `y`|
|`x.>=y`|`x` is greater than or equal to `y`|
|`x.<=y`|`x` is less than or equal to `y`|
|`x.=y`|`x` equals `y`|
|`x.=/y`|`x` does not equal `y`|

Combine conditions with `And` and `Or`.

```fsharp
And [x.<y; y.<z]
Or [x.<y; y.<z]
```

The first expression can also be written as a chained comparison.

```fsharp
x.<y.<z
```

## Loops

[Back to top](#aqualis)

The following repeats the body while integer variable `i` increases from 1 through 10. Indentation determines the loop body.

```fsharp
ctx.iter.range (1, 10) <| fun i ->
```

Here `aaa` is printed ten times, then `bbb` is printed once after the loop.

```fsharp
ctx.iter.range (0, 9) <| fun i ->
    ctx.print.s "aaa"
ctx.print.s "bbb"
```

The following two loops are equivalent.

```fsharp
ctx.iter.range (0, n-1) <| fun i ->
    ctx.print.t i
```

```fsharp
ctx.iter.num n <| fun i ->
    ctx.print.t i
```

Both of the following assign 1 to every element of `a`.

```fsharp
ctx.iter.range (0, a.size1-1) <| fun n ->
    a[n] <== 1
```

```fsharp
ctx.iter.num a.size1 <| fun n ->
    a[n] <== 1
```

For all elements of an array, `foreach` is more concise.

```fsharp
a.foreach <| fun n ->
    a[n] <== n
```

Loops may be nested.

```fsharp
ctx.iter.num 5 <| fun i ->
    ctx.iter.num 10 <| fun j ->
        ctx.print.tt <| i++j
```

Use a tuple with `foreach` to visit every element of a two-dimensional array.

```fsharp
a.foreach <| fun (i,j) ->
    a[i,j] <== i*j
```

`ctx.iter.loop` creates an infinite loop. `ex` exits the loop, and `i` is its counter.

```fsharp
ctx.iter.loop <| fun (ex,i) ->
    ctx.print.s "aaa"
    x <== x - i*i
    ctx.br.if1 (i.>100) <| fun () ->
        ex() // exit here
```

Use `whiledo` to repeat while a condition remains true.

```fsharp
ctx.iter.whiledo (condition) <| fun ex ->
    (code)
```

Use `ctx.iter.list` to process each item in a list.

```fsharp
ctx.iter.list [x;y;z] <| fun v ->
    ctx.print.t v
```

This is equivalent to:

```fsharp
ctx.print.t x
ctx.print.t y
ctx.print.t z
```

## Expressions and Functions

[Back to top](#aqualis)

### Let bindings

An F# `let` binding can define constants, expressions, and functions.

### Constants

The following binds the name `x` to the constant 1. This is not an Aqualis variable, and `<==` cannot assign another value to it.

```fsharp
let x = 1
```

### Expressions

The following binds `x` to the expression `y+z` rather than to a single evaluated value. If `y` or `z` changes, evaluating `x` reflects the new value.

```fsharp
y <== 1
z <== 1
let x = y + z
ctx.print.t x // 2
y <== 2
ctx.print.t x // 3
```

### Single-argument functions

Define a function that adds 1 to `x` as follows. Parentheses around the argument are optional.

```fsharp
let f(x) = x + 1
let f x = x + 1
```

Without a type annotation, F# may infer a type that is incompatible with an Aqualis variable.

```fsharp
let f x = x + 1
ctx.ch.ii <| fun (a,b) ->
    a <== 1
    b <== f a
    ctx.print.t b
```

Specify the Aqualis type when inference is insufficient.

```fsharp
let f (x:int0) = x + 1
ctx.ch.ii <| fun (a,b) ->
    a <== 1
    b <== f a
    ctx.print.t b
```

Similarly, annotate an array argument when defining a function that accesses it.

```fsharp
let f (x:int1) = x[0]
```

### Multiple-argument functions

This function takes one tuple containing two values:

```fsharp
let f(x:double0,y:double0) = x - y
a <== f(b,c)
```

This curried form takes two arguments separately:

```fsharp
let f (x:double0) (y:double0) = x - y
a <== f b c
```

A long function body may continue on indented lines.

```fsharp
let f (x:int0) (y:double0) =
    3 * x - 4 * y
w <== f p q
```

The body may contain local `let` bindings.

```fsharp
let f (x:int0) (y:double0) =
    let a = 3
    let b = 4
    a * x - b * y
w <== f p q
```

```
The following two functions perform the same operation; the only difference is the type of their arguments.
```fsharp
let f(a:double0, b:int0) =
    a <== b + 1
let f(a:double0, b:double0) =
    a <== b + 1
```
If you define a single function as shown below, you can pass both `int0` and `double0` values to the `b` argument.
```fsharp
let inline f(a:double0, b:#IReal0) =
    a <== b.ToDouble0 + 1
```
`IReal0` is an interface that represents real numbers. Therefore, specifying a variable of type `complex0` for the argument `b` will result in an error.
```
The following three functions perform the same operation; they differ only in the types of their arguments.
```fsharp
let f(a:complex0, b:int0) =
    a <== b + 1
let f(a:complex0, b:double0) =
    a <== b + 1
let f(a:complex0, b:complex0) =
    a <== b + 1
```
If you define a single function as shown below, you can pass `int0`, `double0`, or `double0` as the argument `b`.
```fsharp
let inline f(a:double0, b:#INum0) =
    a <== b.ToComplex0 + 1
```
`INum0` is an interface that represents numeric values.

### Higher-order functions 1

Functions can be passed as arguments.

```fsharp
let f(x:int0,g:int0->double0) = g x
let h (x:int0) = x - 1.2
a <== f(b,h)
```

`int0->double0` means a function that accepts an `int0` and returns a `double0`.

### Higher-order functions 2

```fsharp
let f(x:int0,y:int0,g:(int0*int0)->int0) = g (x,y)
let h (x:int0,y:int0) = x - y
a <== f(b,c,h)
```

`(int0*int0)->int0` means a function that accepts an `(int0*int0)` tuple and returns an `int0`.

### Higher-order functions 3

```fsharp
let f (x:int0) (y:int0) (g:int0->int0->int0) = g x y
let h (x:int0) (y:int0) = x - y
a <== f b c h
```

`int0->int0->int0` means a function that accepts two `int0` arguments and returns an `int0`. The named function can also be written explicitly as a lambda.

```fsharp
let h = fun (x:int0) (y:int0) -> x - y
```

If it is used only once, pass the anonymous function directly.

```fsharp
a <== f b c (fun (x:int0) (y:int0) -> x - y)
```

The same expression can be written with the reverse-pipe operator.

```fsharp
a <== f b c <| fun (x:int0) (y:int0) -> x - y
```

Loops such as `ctx.iter.range` and branches such as `ctx.br.if1` are also higher-order functions.

### Functions that return functions

This function returns another function that adds `n` to its argument.

```fsharp
let f (n:int) = (fun (x:int0) -> x + n)
let g = f 4
ctx.print.t (g 1) // 5
```

The equivalent curried definition is:

```fsharp
let f (n:int) (x:int0) = x + n
let g = f 4
ctx.print.t (g 1) // 5
```

Although `f` is declared with two arguments, `f 4` supplies only one. The result acts as a function waiting for `x`. This is called currying.

For example:

```fsharp
ctx.iter.num 10 <| fun i ->
    ctx.print.t i
```

can be partially applied as follows:

```fsharp
let loop10 = ctx.iter.num 10
loop10 <| fun i ->
    ctx.print.t i
```

This defines a reusable loop with a fixed iteration count.

## File I/O

[Back to top](#aqualis)

### Writing files

Write values to `test.dat` as follows:

```fsharp
ctx.ch.id <| fun (x,y) ->
    x <== 1
    y <== 2.0
    ctx.io.fileOutput "test.dat" <| fun wr ->
        wr.t "aaa"       // write a string
        wr.t x           // write one variable
        wr.tt <| x++y    // write multiple values separated by tabs
```

The following is invalid because `x` and `y` are F# constants rather than Aqualis variables.

```fsharp
let x = 1
let y = 2.0
ctx.io.fileOutput "test.dat" <| fun wr ->
    wr.tt <| x++y
```

Several files may be open at once. Give each writer a different name.

```fsharp
ctx.ch.id <| fun (x,y) ->
    x <== 1
    y <== 2.0
    ctx.io.fileOutput "test1.dat" <| fun wr1 ->
        ctx.io.fileOutput "test2.dat" <| fun wr2 ->
            wr1.t x
            wr2.t x
```

An integer Aqualis variable may be included in a file name.

```fsharp
ctx.ch.i <| fun n ->
    n <== 4
    ctx.io.fileOutput ("test"++n++".dat") <| fun wr -> // test00004.dat
```

### Reading files

Given a `test.dat` file containing:

```text
           3    -1.230000000000000E+001
```

read its two values as follows:

```fsharp
ctx.ch.id <| fun (x,y) ->
    ctx.io.fileInput "test.dat" <| fun rd ->
        rd <| x++y
```

## Linear Algebra

[Back to top](#aqualis)

### One system of simultaneous equations

Solve

$$
A\boldsymbol{x}=\boldsymbol{b}.
$$

`A` is a two-dimensional array and $\boldsymbol{b}$ is a one-dimensional array. For example:

$$
\begin{align}
A &= \begin{bmatrix}1&2\\3&4\end{bmatrix},
&\boldsymbol{b} &= \begin{bmatrix}5\\6\end{bmatrix}.
\end{align}
$$

Initialize the arrays using zero-based Aqualis indices.

```fsharp
A[0,0] <== 1.0
A[0,1] <== 2.0
A[1,0] <== 3.0
A[1,1] <== 4.0
b[0] <== 5.0
b[1] <== 6.0
```

Then solve the system in place:

```fsharp
ctx.la.solve_simuleq(A,b)
```

The solution $A^{-1}\boldsymbol{b}$ is written back to `b`. For this example, $x_1=-4$ and $x_2=4.5$.

Complete example:

```fsharp
//#############################################################################
// Test: simultaneous equation
let projectname = "test_simuleq"
let version = "1.0.0"
//#############################################################################
 
let outputdir = @"C:\home\work"

#I "C:\\Aqualis\\lib\\188_0_0_0"
#r "Aqualis.dll"
 
open Aqualis
 
Compile [Fortran] outputdir projectname version <| fun ctx ->
    ctx.ch.d2 (2,2) <| fun A ->
        ctx.ch.d1 2 <| fun b ->
            A[0,0] <== 1.0
            A[0,1] <== 2.0
            A[1,0] <== 3.0
            A[1,1] <== 4.0
            b[0] <== 5.0
            b[1] <== 6.0
            ctx.la.solve_simuleq(A,b)
            b.foreach <| fun i -> ctx.print.tt <| i++b[i]
```

### Multiple systems with the same coefficient matrix

For systems

$$
\begin{align}
A\boldsymbol{x}_1 &= \boldsymbol{b}_1\\
A\boldsymbol{x}_2 &= \boldsymbol{b}_2\\
&\vdots\\
A\boldsymbol{x}_N &= \boldsymbol{b}_N,
\end{align}
$$

place $\boldsymbol{b}_1,\ldots,\boldsymbol{b}_N$ in the two-dimensional array `b`, then call:

```fsharp
ctx.la.solve_simuleqs(A,b)
```

The solutions $A^{-1}\boldsymbol{b}_1,\ldots,A^{-1}\boldsymbol{b}_N$ are written back to `b`.

## OpenMP

[Back to top](#aqualis)

### Basics

The following parallelizes a `ctx.iter.num` loop.

```fsharp
ctx.omp.parallelize <| fun pctx ->
    pctx.iter.num 12 <| fun i ->
        // this body is parallelized
```

Values of `i` are assigned across CPU threads and processed concurrently. To request a specific thread count, use `parallelize_th`.

```fsharp
ctx.omp.parallelize_th 6 <| fun pctx -> // six threads
    pctx.iter.num 12 <| fun i ->
        // this body is parallelized
```

Without an explicit count, the compiler/runtime selects the available CPU thread count.

### Private variables

The counter of a parallel loop is generated as a thread-private variable automatically. Updating a shared variable as temporary working storage creates a data race, so use the loop counter directly whenever possible.

```fsharp
ctx.ch.i <| fun sum ->
    sum <== 0
    ctx.ch.i1 10000 <| fun a ->
        ctx.omp.parallelize <| fun pctx ->
            pctx.iter.num 10000 <| fun i ->
                a[i] <== i+1
        ctx.iter.num 10000 <| fun i ->
            sum <== sum + a[i]
        ctx.print.t sum
```

Only the array writes are parallelized here; accumulation into the shared variable `sum` occurs outside the parallel region. To parallelize an accumulation, use `ctx.omp.reduction` or `ctx.omp.reduction_th`.

## OpenACC

### Basics

The basic form resembles OpenMP, but the thread count cannot be specified. OpenACC generation is supported only for Fortran and C99.

```fsharp
ctx.oacc.parallelize <| fun pctx ->
    pctx.iter.num 12 <| fun i ->
        // this body is parallelized
```

OpenACC runs the parallelized section on an accelerator such as a GPU. The current public API does not provide the old `ctx.ch.copyin_*` and `ctx.ch.copyout_*` helpers; do not use those legacy calls.

## Class Definition Example

[Back to top](#aqualis)

The following defines `testClass1`. For generated Fortran and C code, fields are implemented with a structure and methods are expanded inline.

```fsharp
/// <summary>
/// testClass1
/// </summary>
type testClass1(sname_,name,ctx:Aqualis) =
    inherit structureValue<testClass1>(sname_,name,ctx)
    /// Class name
    static member sname = "testClass1"
    /// Constructor
    new(name,ctx:Aqualis) =
        ctx.str.reg(testClass1.sname,name)
        testClass1(testClass1.sname,name,ctx)
    override _.Rewrap(n,targetEnvironment) = testClass1(sname_,n,targetEnvironment)
    /// Field 1
    member public __.n1 = ctx.str.i0(sname_,name,"x1")
    /// Field 2
    member public __.x1 = ctx.str.d0(sname_,name,"y1")
    /// Field 3
    member public __.z1 = ctx.str.z0(sname_,name,"x2")
        
/// <summary>
/// Array of testClass1
/// </summary>
type testClass1_1(sname_,name,size1,ctx:Aqualis) =
    inherit structureArray1<testClass1,testClass1_1>(sname_,name,size1,ctx)
    new(name,size1,ctx:Aqualis) =
        ctx.str.reg(testClass1.sname,name,size1)
        testClass1_1(testClass1.sname,name,A1 size1,ctx)
    new(name,ctx:Aqualis) = testClass1_1(name,0,ctx)
    override _.WrapElement n = testClass1(sname_,n,ctx)
    override _.Rewrap(n,v,targetEnvironment) = testClass1_1(sname_,n,v,targetEnvironment)
    /// Define this method when this class is used as a field of another class
    static member str_mem(psname, vname, name, size1,ctx:Aqualis) =
        ctx.str.addmember(psname,(Structure(testClass1.sname),size1,name))
        testClass1_1(testClass1.sname,ctx.str.mem(vname,name), size1,ctx)
```

### Usage

```fsharp
Compile [Fortran] outputdir projectname version <| fun ctx ->
    // Create a testClass1 value named u
    let u = testClass1("u",ctx)
    // Access a field as variableName.fieldName
    u.n1 <== 1
    u.x1 <== 2.0
    u.z1 <== 3.0+asm.uj*4.0
    ctx.print.tt <| u.n1 ++ u.x1 ++ u.z1
    
    // Create a one-dimensional testClass1 array named v
    let v = testClass1_1("v",ctx)
    // Allocate memory for ten elements
    v.allocate(10)
    // Access array elements
    v.foreach <| fun i ->
        v[i].n1 <== 1
        v[i].x1 <== 2.0
        v[i].z1 <== 3.0+asm.uj*4.0
        ctx.print.tt <| v[i].n1 ++ v[i].x1 ++ v[i].z1
```

## Symbolic Differentiation

[Back to top](#aqualis)

Aqualis can calculate the derivative $g(x)=\mathrm{d}f(x)/\mathrm{d}x$ algebraically. For

$$
\begin{align}
f(x)&=2x^2+3x,\\
g(x)&=\frac{\mathrm{d}f(x)}{\mathrm{d}x},
\end{align}
$$

write:

```fsharp
ctx.ch.d <| fun x ->
    // Function f
    let f(x:double0) = 2*x*x+3*x
    // Derivative of f with respect to x
    let g(x:double0) = asm.diff(f x,x)
    
    // Verify the derivative
    ctx.iter.num 100 <| fun i ->
        x <== 0.1*i
        ctx.print.tt <| x ++ (g x) ++ (4*x+3)
```

Expressions containing functions can also be differentiated. For

$$
\begin{align}
f(x)&=2x\sin(3x),\\
g(x)&=\frac{\mathrm{d}f(x)}{\mathrm{d}x},
\end{align}
$$

use:

```fsharp
ctx.ch.d <| fun x ->
    let f(x:double0) = 2*x*asm.sin(3*x)
    let g(x:double0) = asm.diff(f x,x)
    
    ctx.iter.num 100 <| fun i ->
        x <== 0.1*i
        ctx.print.tt <| x ++ (g x) ++ (2*asm.sin(3*x)+6*x*asm.cos(3*x))
```

Another example is

$$
\begin{align}
f(x)&=\frac{2\sin(3x)}{\sqrt{x^2+1}},\\
g(x)&=\frac{\mathrm{d}f(x)}{\mathrm{d}x}.
\end{align}
$$

```fsharp
ctx.ch.d <| fun x ->
    let f(x:double0) = 2*asm.sin(3*x)/asm.sqrt(x*x+1)
    let g(x:double0) = asm.diff(f x,x)
    
    ctx.iter.num 100 <| fun i ->
        x <== 0.1*i
        ctx.print.tt <| x ++ (g x) ++ (6*asm.cos(3*x)/asm.sqrt(x*x+1) - 2*x*asm.sin(3*x)/asm.pow(x*x+1,1.5))
```

### Differentiating a series

For

$$
\begin{align}
f(x)&=2\sum_{i=1}^{5}(ix^2+1),\\
g(x)&=\frac{\mathrm{d}f(x)}{\mathrm{d}x},
\end{align}
$$

use `asm.dSum`:

```fsharp
ctx.ch.d <| fun x ->
    let f(x:double0) = 2*(asm.dSum (1,5) <| fun i -> i*x*x+1)
    let g(x:double0) = asm.diff(f x,x)
    
    ctx.iter.num 100 <| fun i ->
        x <== 0.1*i
        ctx.print.tt <| x ++ (g x) ++ (2*(asm.dSum (1,5) <| fun i -> 2*i*x))
```

Differentiating an expression containing a series may cause the same series to be evaluated repeatedly. The next expression contains the series twice in its expanded derivative.

$$
\begin{align}
f(x)
&=\frac{\displaystyle\left[2x+\sum_{i=1}^{5}(ix^2+1)\right]^2}{x+1},\\
g(x)
&=\frac{\mathrm{d}f(x)}{\mathrm{d}x}\\
&=\frac{\displaystyle 2\left[2x+\sum_{i=1}^{5}(ix^2+1)\right]
\left[2+\sum_{i=1}^{5}(2ix)\right]}{x+1}
-\frac{\displaystyle\left[2x+\sum_{i=1}^{5}(ix^2+1)\right]^2}{(x+1)^2}.
\end{align}
$$

```fsharp
ctx.ch.d <| fun x ->
    let f(x:double0) = asm.pow(2*x+(asm.dSum (1,5) <| fun i -> i*x*x+1),2)/(x+1)
    let g(x:double0) = asm.diff(f x,x)
    
    ctx.iter.num 100 <| fun i ->
        x <== 0.1*i
        ctx.print.tt <| x ++ (g x) ++ (2*(2*x+(asm.dSum (1,5) <| fun i -> i*x*x+1))*(2+(asm.dSum (1,5) <| fun i -> 2*i*x))/(x+1)-asm.pow(2*x+(asm.dSum (1,5) <| fun i -> i*x*x+1),2)/asm.pow(x+1,2))
```

Use `asm.iLet`, `asm.dLet`, or `asm.zLet` to save a calculated expression in a temporary variable and reuse it. Use `iLet` for integer expressions, `dLet` for real expressions, and `zLet` for complex expressions.

```fsharp
ctx.ch.d <| fun x ->
    let f(x:double0) =
        asm.dLet (asm.dSum (1,5) <| fun i -> i*x*x+1) <| fun tmp ->
            asm.pow(2*x+tmp,2)/(x+1)
    let g(x:double0) = asm.diff(f x,x)
    
    ctx.iter.num 100 <| fun i ->
        x <== 0.1*i
        // Evaluate the series and save it in a temporary variable
        (f x).eval()
        ctx.print.tt <| x ++ (g x)
```

### Differentiating with respect to array elements

```fsharp
let N = 100
ctx.ch.d1 N <| fun x ->
ctx.ch.d1 N <| fun y ->
    // Function f
    let f(x:double1) = 2*(asm.dSum (1,N) <| fun i -> i*asm.pow(x[i],2)+1)
    // Derivative of f with respect to x[j]
    let g(x:double1,j:int0) = asm.diff(f x,x[j])
    // Initialize x
    ctx.iter.num N <| fun i ->
        x[i] <== 0.1*i
    // Calculate y[i] = df/d(x[i])
    ctx.iter.num N <| fun j ->
        y[j] <== g (x,j)
    // Verify the derivative
    ctx.iter.num N <| fun j ->
        ctx.print.tt <| j ++ y[j] ++ (2*j*2*x[j])
```
