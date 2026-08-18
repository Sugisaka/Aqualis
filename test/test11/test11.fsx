//#############################################################################
// 離散データテスト
let projectname = "test11"
let version = "1.0.0"
//#############################################################################
 
let outputdir = @"C:\home\work" //__SOURCE_DIRECTORY__

#I @"..\..\bin\Debug\net10.0"
#r "Aqualis.dll"

open Aqualis

type DDistribution1(u:(int0*double0)->double0,n1:int0,n2:int0,x0:double0,dx:double0,c:Aqualis) =

    member _.f with get() = u
    member _.N1 with get() = n1
    member _.N2 with get() = n2
    member _.Dx with get() = dx
    member _.X0 with get() = x0
    member _.ctx with get() = c
    member _.x(n:int0) = x0+dx*n
    static member ( + ) (a:DDistribution1,b:DDistribution1) = DDistribution1((fun (n,x) -> a.f (n,x) + b.f (n,x)),a.N1,a.N2,a.X0,a.Dx,a.ctx)
    static member ( - ) (a:DDistribution1,b:DDistribution1) = DDistribution1((fun (n,x) -> a.f (n,x) - b.f (n,x)),a.N1,a.N2,a.X0,a.Dx,a.ctx)
    static member ( * ) (a:DDistribution1,b:DDistribution1) = DDistribution1((fun (n,x) -> a.f (n,x) * b.f (n,x)),a.N1,a.N2,a.X0,a.Dx,a.ctx)
    static member ( / ) (a:DDistribution1,b:DDistribution1) = DDistribution1((fun (n,x) -> a.f (n,x) / b.f (n,x)),a.N1,a.N2,a.X0,a.Dx,a.ctx)
    static member ( + ) (a:DDistribution1,b:double0->double0) = DDistribution1((fun (n,x) -> a.f (n,x) + b x),a.N1,a.N2,a.X0,a.Dx,a.ctx)
    static member ( - ) (a:DDistribution1,b:double0->double0) = DDistribution1((fun (n,x) -> a.f (n,x) - b x),a.N1,a.N2,a.X0,a.Dx,a.ctx)
    static member ( * ) (a:DDistribution1,b:double0->double0) = DDistribution1((fun (n,x) -> a.f (n,x) * b x),a.N1,a.N2,a.X0,a.Dx,a.ctx)
    static member ( / ) (a:DDistribution1,b:double0->double0) = DDistribution1((fun (n,x) -> a.f (n,x) / b x),a.N1,a.N2,a.X0,a.Dx,a.ctx)
    static member ( + ) (a:double0->double0,b:DDistribution1) = DDistribution1((fun (n,x) -> a x + b.f (n,x) ),b.N1,b.N2,b.X0,b.Dx,b.ctx)
    static member ( - ) (a:double0->double0,b:DDistribution1) = DDistribution1((fun (n,x) -> a x - b.f (n,x) ),b.N1,b.N2,b.X0,b.Dx,b.ctx)
    static member ( * ) (a:double0->double0,b:DDistribution1) = DDistribution1((fun (n,x) -> a x * b.f (n,x) ),b.N1,b.N2,b.X0,b.Dx,b.ctx)
    static member ( / ) (a:double0->double0,b:DDistribution1) = DDistribution1((fun (n,x) -> a x / b.f (n,x) ),b.N1,b.N2,b.X0,b.Dx,b.ctx)
    static member ( <== ) (a:DDistribution1,b:DDistribution1) = 
        a.ctx.iter.range (a.N1,a.N2) <| fun i ->
            a.ctx.ch.d <| fun x ->
                x <== a.x i
                a.f (i,x) <== b.f (i,x)
    static member ( <== ) (a:DDistribution1,b:double0->double0) = 
        a.ctx.iter.range (a.N1,a.N2) <| fun i ->
            a.ctx.ch.d <| fun x ->
                x <== a.x i
                a.f (i,x) <== b x
                
type ZDistribution1(u:(int0*double0)->complex0,n1:int0,n2:int0,x0:double0,dx:double0,c:Aqualis) =

    member _.f with get() = u
    member _.N1 with get() = n1
    member _.N2 with get() = n2
    member _.Dx with get() = dx
    member _.X0 with get() = x0
    member _.ctx with get() = c
    member _.x(n:int0) = x0+dx*n
    static member ( + ) (a:ZDistribution1,b:ZDistribution1) = ZDistribution1((fun (n,x) -> a.f (n,x) + b.f (n,x)),a.N1,a.N2,a.X0,a.Dx,a.ctx)
    static member ( - ) (a:ZDistribution1,b:ZDistribution1) = ZDistribution1((fun (n,x) -> a.f (n,x) - b.f (n,x)),a.N1,a.N2,a.X0,a.Dx,a.ctx)
    static member ( * ) (a:ZDistribution1,b:ZDistribution1) = ZDistribution1((fun (n,x) -> a.f (n,x) * b.f (n,x)),a.N1,a.N2,a.X0,a.Dx,a.ctx)
    static member ( / ) (a:ZDistribution1,b:ZDistribution1) = ZDistribution1((fun (n,x) -> a.f (n,x) / b.f (n,x)),a.N1,a.N2,a.X0,a.Dx,a.ctx)
    static member ( + ) (a:ZDistribution1,b:double0->complex0) = ZDistribution1((fun (n,x) -> a.f (n,x) + b x),a.N1,a.N2,a.X0,a.Dx,a.ctx)
    static member ( - ) (a:ZDistribution1,b:double0->complex0) = ZDistribution1((fun (n,x) -> a.f (n,x) - b x),a.N1,a.N2,a.X0,a.Dx,a.ctx)
    static member ( * ) (a:ZDistribution1,b:double0->complex0) = ZDistribution1((fun (n,x) -> a.f (n,x) * b x),a.N1,a.N2,a.X0,a.Dx,a.ctx)
    static member ( / ) (a:ZDistribution1,b:double0->complex0) = ZDistribution1((fun (n,x) -> a.f (n,x) / b x),a.N1,a.N2,a.X0,a.Dx,a.ctx)
    static member ( + ) (a:double0->complex0,b:ZDistribution1) = ZDistribution1((fun (n,x) -> a x + b.f (n,x) ),b.N1,b.N2,b.X0,b.Dx,b.ctx)
    static member ( - ) (a:double0->complex0,b:ZDistribution1) = ZDistribution1((fun (n,x) -> a x - b.f (n,x) ),b.N1,b.N2,b.X0,b.Dx,b.ctx)
    static member ( * ) (a:double0->complex0,b:ZDistribution1) = ZDistribution1((fun (n,x) -> a x * b.f (n,x) ),b.N1,b.N2,b.X0,b.Dx,b.ctx)
    static member ( / ) (a:double0->complex0,b:ZDistribution1) = ZDistribution1((fun (n,x) -> a x / b.f (n,x) ),b.N1,b.N2,b.X0,b.Dx,b.ctx)
    static member ( <== ) (a:ZDistribution1,b:ZDistribution1) = 
        a.ctx.iter.range (a.N1,a.N2) <| fun i ->
            a.ctx.ch.d <| fun x ->
                x <== a.x i
                a.f (i,x) <== b.f (i,x)
    static member ( <== ) (a:ZDistribution1,b:double0->complex0) = 
        a.ctx.iter.range (a.N1,a.N2) <| fun i ->
            a.ctx.ch.d <| fun x ->
                x <== a.x i
                a.f (i,x) <== b x

type DistributionContext(c:Aqualis) =
    member _.d(m1:int0,m2:int0,x0:double0,dx:double0) = fun code ->
        c.ch.d1 (m2-m1+1) <| fun v ->
            let p = DDistribution1((fun (n:int0,_) -> v[n-m1]),m1,m2,x0,dx,c)
            code p
    member _.z(m1:int0,m2:int0,x0:double0,dx:double0) = fun code ->
        c.ch.z1 (m2-m1+1) <| fun v ->
            let p = ZDistribution1((fun (n:int0,_) -> v[n-m1]),m1,m2,x0,dx,c)
            code p
    member _.sum (g:DDistribution1) =
        asm.dSum (g.N1,g.N2) <| fun n -> 
            g.ctx.ch.d <| fun x ->
                x <== g.x n
                g.f (n,x)
    member _.sum (g:ZDistribution1) =
        asm.zSum (g.N1,g.N2) <| fun n -> 
            g.ctx.ch.d <| fun x ->
                x <== g.x n
                g.f (n,x)
                
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
