//#############################################################################
// 離散データテスト
let projectname = "test11"
let version = "1.0.0"
//#############################################################################
 
let outputdir = __SOURCE_DIRECTORY__

#I @"..\..\bin\Debug\net10.0"
#r "Aqualis.dll"

open Aqualis

type Distribution1Operations<'T> = {
    Add: 'T -> 'T -> 'T
    Subtract: 'T -> 'T -> 'T
    Multiply: 'T -> 'T -> 'T
    Divide: 'T -> 'T -> 'T
    Assign: 'T -> 'T -> unit
    Sum: Aqualis -> int0 -> int0 -> (int0 -> 'T) -> 'T
}

module Distribution1Operations =
    let real = {
        Add = fun a b -> a + b
        Subtract = fun a b -> a - b
        Multiply = fun a b -> a * b
        Divide = fun a b -> a / b
        Assign = fun a b -> a <== b
        Sum = fun _ n1 n2 term -> asm.dSum (n1,n2) term
    }

    let complex = {
        Add = fun a b -> a + b
        Subtract = fun a b -> a - b
        Multiply = fun a b -> a * b
        Divide = fun a b -> a / b
        Assign = fun a b -> a <== b
        Sum = fun _ n1 n2 term -> asm.zSum (n1,n2) term
    }

type Distribution1<'T>
    (u:(int0*double0)->'T,n1:int0,n2:int0,x0:double0,dx:double0,c:Aqualis,operations:Distribution1Operations<'T>) =

    member _.f = u
    member _.N1 = n1
    member _.N2 = n2
    member _.Dx = dx
    member _.X0 = x0
    member _.ctx = c
    member _.x(n:int0) = x0+dx*n
    member private _.Operations = operations
    member private _.New(value) = Distribution1(value,n1,n2,x0,dx,c,operations)
    member private this.Assign(value:(int0*double0)->'T) =
        c.iter.range (n1,n2) <| fun i ->
            c.ch.d <| fun x ->
                x <== this.x i
                operations.Assign (u(i,x)) (value(i,x))
    member this.Sum() =
        operations.Sum c n1 n2 <| fun n ->
            c.ch.d <| fun x ->
                x <== this.x n
                u(n,x)

    static member (+) (a:Distribution1<'T>,b:Distribution1<'T>) =
        a.New(fun (n,x) -> a.Operations.Add (a.f(n,x)) (b.f(n,x)))
    static member (-) (a:Distribution1<'T>,b:Distribution1<'T>) =
        a.New(fun (n,x) -> a.Operations.Subtract (a.f(n,x)) (b.f(n,x)))
    static member (*) (a:Distribution1<'T>,b:Distribution1<'T>) =
        a.New(fun (n,x) -> a.Operations.Multiply (a.f(n,x)) (b.f(n,x)))
    static member (/) (a:Distribution1<'T>,b:Distribution1<'T>) =
        a.New(fun (n,x) -> a.Operations.Divide (a.f(n,x)) (b.f(n,x)))
    static member (+) (a:Distribution1<'T>,b:double0->'T) =
        a.New(fun (n,x) -> a.Operations.Add (a.f(n,x)) (b x))
    static member (-) (a:Distribution1<'T>,b:double0->'T) =
        a.New(fun (n,x) -> a.Operations.Subtract (a.f(n,x)) (b x))
    static member (*) (a:Distribution1<'T>,b:double0->'T) =
        a.New(fun (n,x) -> a.Operations.Multiply (a.f(n,x)) (b x))
    static member (/) (a:Distribution1<'T>,b:double0->'T) =
        a.New(fun (n,x) -> a.Operations.Divide (a.f(n,x)) (b x))
    static member (+) (a:double0->'T,b:Distribution1<'T>) =
        b.New(fun (n,x) -> b.Operations.Add (a x) (b.f(n,x)))
    static member (-) (a:double0->'T,b:Distribution1<'T>) =
        b.New(fun (n,x) -> b.Operations.Subtract (a x) (b.f(n,x)))
    static member (*) (a:double0->'T,b:Distribution1<'T>) =
        b.New(fun (n,x) -> b.Operations.Multiply (a x) (b.f(n,x)))
    static member (/) (a:double0->'T,b:Distribution1<'T>) =
        b.New(fun (n,x) -> b.Operations.Divide (a x) (b.f(n,x)))
    static member (<==) (a:Distribution1<'T>,b:Distribution1<'T>) = a.Assign b.f
    static member (<==) (a:Distribution1<'T>,b:double0->'T) = a.Assign(fun (_,x) -> b x)
    
type DistributionContext(c:Aqualis) =
    member _.d(m1:int0,m2:int0,x0:double0,dx:double0) = fun code ->
        c.ch.d1 (m2-m1+1) <| fun v ->
            let p = Distribution1<double0>((fun (n:int0,_) -> v[n-m1]),m1,m2,x0,dx,c,Distribution1Operations.real)
            code p
    member _.z(m1:int0,m2:int0,x0:double0,dx:double0) = fun code ->
        c.ch.z1 (m2-m1+1) <| fun v ->
            let p = Distribution1<complex0>((fun (n:int0,_) -> v[n-m1]),m1,m2,x0,dx,c,Distribution1Operations.complex)
            code p
    member _.sum (g:Distribution1<'T>) = g.Sum()
                
[<AutoOpen>]
module DistributionContextExtension =
    type Aqualis with
        member this.distribution = DistributionContext this

Compile [Fortran] outputdir projectname version <| fun ctx ->
    ctx.distribution.d(I -10, I 10, D -10.0, D 0.2) <| fun f ->
        ctx.distribution.d(I -10, I 10, D -10.0, D 0.2) <| fun g ->
            let y = D 1.0
            let f1 (x:double0) = ctx.ch.xLet (asm.sqrt(x*x+y*y)) <| fun R -> asm.sin R/R
            let f2 (x:double0) = x*asm.sin x
            let f3 (x:double0) = x+1
            let f4 (y:double0) (x:double0) = x*asm.sin (x+y)
            let f5 (x:double0) = 2*x
            ctx.comment "test01"
            g <== f1
            ctx.comment "test02"
            f <== g * f2 * f3
            ctx.comment "test03"
            f <== fun y -> ctx.distribution.sum (g * f4 y * (f3 << f5))
