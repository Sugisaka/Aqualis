//#############################################################################
// Wirtinger自動微分テスト
let projectname = "test6_4"
let version = "1.0.0"
//#############################################################################
 
let outputdir = @"C:\home\work" //__SOURCE_DIRECTORY__

#I @"..\..\bin\Debug\net10.0"
#r "Aqualis.dll"

open Aqualis

[<AutoOpen>]
module num0ConstZ =
    let _0z = complex0(Dbl 0.0)
    let _1z = complex0(Dbl 1.0)
    
type AutoDiff(v:complex0,dv:complex0,dv':complex0) =
    member _.x with get() = v
    member _.dx with get() = dv
    member _.dx' with get() = dv'
    member this.Target(u:complex0,u':complex0) = 
        this.dx <== u
        this.dx' <== u'
    member this.Target() = this.Target (_1z,_0z)
            
    static member ( + ) (a:AutoDiff,b:AutoDiff) = AutoDiff(a.x+b.x, a.dx+b.dx, a.dx'+b.dx')
    static member ( + ) (a:#INum0,b:AutoDiff) = AutoDiff(a.ToComplex0,_0z,_0z) + b
    static member ( + ) (a:AutoDiff,b:#INum0) = a + AutoDiff(b.ToComplex0,_0z,_0z)
    static member ( + ) (a:AutoDiff,b:int) = a + AutoDiff(Z (b,0.0),_0z,_0z)
    static member ( + ) (a:AutoDiff,b:double) = a + AutoDiff(Z (b,0.0),_0z,_0z)
    static member ( + ) (a:int,b:AutoDiff) = AutoDiff(Z (a,0.0),_0z,_0z) + b
    static member ( + ) (a:double,b:AutoDiff) = AutoDiff(Z (a,0.0),_0z,_0z) + b

    static member ( - ) (a:AutoDiff,b:AutoDiff) = AutoDiff(a.x-b.x, a.dx-b.dx, a.dx'-b.dx')
    static member ( - ) (a:#INum0,b:AutoDiff) = AutoDiff(a.ToComplex0,_0z,_0z) - b
    static member ( - ) (a:AutoDiff,b:#INum0) = a - AutoDiff(b.ToComplex0,_0z,_0z)
    static member ( - ) (a:AutoDiff,b:int) = a - AutoDiff(Z (b,0.0),_0z,_0z)
    static member ( - ) (a:AutoDiff,b:double) = a - AutoDiff(Z (b,0.0),_0z,_0z)
    static member ( - ) (a:int,b:AutoDiff) = AutoDiff(Z (a,0.0),_0z,_0z) - b
    static member ( - ) (a:double,b:AutoDiff) = AutoDiff(Z (a,0.0),_0z,_0z) - b

    static member ( * ) (a:AutoDiff,b:AutoDiff) = 
        AutoDiff(
            a.x*b.x, 
            a.dx*b.x+a.x*b.dx, 
            a.dx'*b.x+a.x*b.dx')
    static member ( * ) (a:#INum0,b:AutoDiff) = AutoDiff(a.ToComplex0,_0z,_0z) * b
    static member ( * ) (a:AutoDiff,b:#INum0) = a * AutoDiff(b.ToComplex0,_0z,_0z)
    static member ( * ) (a:AutoDiff,b:int) = a * AutoDiff(Z (b,0.0),_0z,_0z)
    static member ( * ) (a:AutoDiff,b:double) = a * AutoDiff(Z (b,0.0),_0z,_0z)
    static member ( * ) (a:int,b:AutoDiff) = AutoDiff(Z (a,0.0),_0z,_0z) * b
    static member ( * ) (a:double,b:AutoDiff) = AutoDiff(Z (a,0.0),_0z,_0z) * b

    static member ( / ) (a:AutoDiff,b:AutoDiff) = 
        AutoDiff(
            a.x/b.x, 
            (a.dx*b.x-a.x*b.dx)/(b.x*b.x),
            (a.dx'*b.x-a.x*b.dx')/(b.x*b.x)
            )
    static member ( / ) (a:#INum0,b:AutoDiff) = AutoDiff(a.ToComplex0,_0z,_0z) / b
    static member ( / ) (a:AutoDiff,b:#INum0) = a / AutoDiff(b.ToComplex0,_0z,_0z)
    static member ( / ) (a:AutoDiff,b:int) = a / AutoDiff(Z (b,0.0),_0z,_0z)
    static member ( / ) (a:AutoDiff,b:double) = a / AutoDiff(Z (b,0.0),_0z,_0z)
    static member ( / ) (a:int,b:AutoDiff) = AutoDiff(Z (a,0.0),_0z,_0z) / b
    static member ( / ) (a:double,b:AutoDiff) = AutoDiff(Z (a,0.0),_0z,_0z) / b
    
    static member ( <== ) (a:AutoDiff,b:AutoDiff) =
        a.x <== b.x
        a.dx <== b.dx
        a.dx' <== b.dx'
    static member ( <== ) (a:AutoDiff,b:complex0) =
        a.x <== b
    static member ( <== ) (a:AutoDiff,b:int) =
        a.x <== Z (b,0.0)
    static member ( <== ) (a:complex0,b:AutoDiff) =
        a <== b.x

        
type asm with
    static member sqrt(a:AutoDiff) = AutoDiff(0.5/asm.sqrt a.x, 0.5/asm.sqrt a.dx, 0.5/asm.sqrt a.dx')
    static member sin(a:AutoDiff) = AutoDiff(asm.sin a.x, a.dx*asm.cos a.x, a.dx'*asm.cos a.x)
    static member cos(a:AutoDiff) = AutoDiff(asm.cos a.x, -a.dx*asm.sin a.x, -a.dx'*asm.sin a.x)
    static member conj(a:AutoDiff) = AutoDiff(asm.conj(a.x),asm.conj(a.dx'),asm.conj(a.dx))

type AutoDiff with
    member this.pow with get() = this*asm.conj(this)
    member this.abs with get() = asm.sqrt(this.pow)

type ContextChd(c:Aqualis) =
    member _.z code =
        c.ch.zzz <| fun (v,dvre,dvim) -> 
            dvre <== 0
            dvim <== 0
            code (AutoDiff(v,dvre,dvim))
            
type ContextDiff(c:Aqualis) =
    member _.diff(f:complex0,a:complex0) code =
        match a.Expr with
        |Var(_,xname,_) ->
            c.ch.z <| fun dx ->
                /// 式の値、微分値保存用変数カウント
                let rec counter(f:complex0,n:int) = 
                    match f.Expr with
                    |Var(_,name,_) when name = xname ->
                        n
                    |Add(_,a,b) ->
                        // aとbの値を保存 → 2個追加
                        let m = n + 2
                        let na = counter (complex0 a,m)
                        let nb = counter (complex0 b,na)
                        nb
                    |Mul(_,a,b) ->
                        // aとbの値を保存 → 2個追加
                        let m = n + 2
                        let na = counter (complex0 a,m)
                        let nb = counter (complex0 b,na)
                        nb
                    |Conj a ->
                        // aの複素共役を保存 → 1個追加
                        let m = n + 1
                        let na = counter (complex0 a,m)
                        na
                    |_ -> 
                        n
                let Nf = counter(f,0)
                // 一時変数：式の値を保存
                c.ch.z1 Nf <| fun w ->
                // 一時変数：微分値を保存
                c.ch.z1 Nf <| fun w' ->
                    /// 途中の式の値を変数に保存
                    let rec dataset(f:complex0,n:int) = 
                        match f.Expr with
                        |Var(_,name,_) when name = xname ->
                            n,f
                        |Add(_,a,b) ->
                            let m = n + 2
                            // aの値を計算
                            let na,fa = dataset (complex0 a,m)
                            // aの値を保存
                            w[n] <== fa
                            // bの値を計算
                            let nb,fb = dataset (complex0 b,na)
                            // bの値を保存
                            w[n+1] <== fb
                            // この式の評価値：a+b
                            nb,w[n]+w[n+1]
                        |Mul(_,a,b) ->
                            let m = n + 2
                            // aの値を計算
                            let na,fa = dataset (complex0 a,m)
                            // aの値を保存
                            w[n] <== fa
                            // bの値を計算
                            let nb,fb = dataset (complex0 b,na)
                            // bの値を保存
                            w[n+1] <== fb
                            // この式の評価値：a+b
                            nb,w[n]*w[n+1]
                        |Conj a ->
                            let m = n + 1
                            // aの値を計算
                            let na,fa = dataset (complex0 a,m)
                            // aの値を保存
                            w[n] <== fa
                            na,w[n]
                        |p -> 
                            n,complex0 p
                    ignore <| dataset(f,0)
                    let rec dd(f:complex0,df:complex0,n:int) = 
                        match f.Expr with
                        |Var(_,name,_) when name = xname ->
                            dx <== dx + df
                            n
                        |Add(_,a,b) ->
                            let m = n + 2
                            /// aの微分値保存用
                            let da = w'[n]
                            /// bの微分値保存用
                            let db = w'[n+1]
                            da <== df
                            db <== df
                            let na = dd (complex0 a,da,m)
                            let nb = dd (complex0 b,db,na)
                            nb
                        |Mul(_,a,b) ->
                            let m = n + 2
                            /// aの微分値保存用
                            let da = w'[n]
                            /// bの微分値保存用
                            let db = w'[n+1]
                            da <== df*asm.conj (complex0 b)
                            db <== df*asm.conj (complex0 a)
                            let na = dd (complex0 a,da,m)
                            let nb = dd (complex0 b,db,na)
                            nb
                        |Conj a ->
                            let m = n + 1
                            /// aの微分値保存用
                            let da = w'[n]
                            da <== asm.conj df
                            let na = dd (complex0 a,da,m)
                            na
                        |_ -> 
                            printfn "expression not defined"
                            n
                    dx.clear()
                    ignore <| dd(f,_1z,0)
                    code dx
        |_ -> ()
        
[<AutoOpen>]
module CompilationEnvironmentChdExtensions =
    type Aqualis with
        ///<summary>一時変数生成</summary>
        member this.chd = ContextChd(this)
        
[<AutoOpen>]
module CompilationEnvironmentContextDiffExtensions =
    type Aqualis with
        ///<summary>一時変数生成</summary>
        member this.TopDown = ContextDiff(this)
        
Compile [Fortran;C99;Python] outputdir projectname version <| fun ctx ->
    let dd = 1E-5
    ctx.chd.z <| fun x ->
    ctx.ch.z <| fun y1 ->
    ctx.ch.z <| fun y2re ->
    ctx.ch.z <| fun y2im ->
    ctx.ch.z <| fun c1 ->
    ctx.ch.z <| fun c2 ->
    ctx.chd.z <| fun result ->
        x <== -10+asm.uj*6
        x.Target()
        group.section "001" <| fun () ->
            let f (x:AutoDiff) = 
                c1 <== 4+asm.uj*3
                c2 <== -2+asm.uj*5
                2*(c1*x+c2).pow
            ctx.comment "代数微分"
            result <== f x
            ctx.print.t (2*result.dx')
            ctx.comment "数値微分"
            y1 <== f x
            y2re <== f (x+dd)
            y2im <== f (x+asm.uj*dd)
            ctx.print.tt <| (y2re-y1)/dd ++ (y2im-y1)/dd
    ctx.ch.z <| fun x ->
    ctx.ch.z <| fun y1 ->
    ctx.ch.z <| fun y2re ->
    ctx.ch.z <| fun y2im ->
    ctx.ch.z <| fun c1 ->
    ctx.ch.z <| fun c2 ->
        x <== -10+asm.uj*6
        group.section "001" <| fun () ->
            let f (x:complex0) = 
                c1 <== 4+asm.uj*3
                c2 <== -2+asm.uj*5
                2*(c1*x+c2)*asm.conj(c1*x+c2)
            ctx.comment "代数微分"
            ctx.TopDown.diff(f x,x) <| fun df ->
                ctx.print.t df
            ctx.comment "数値微分"
            y1 <== f x
            y2re <== f (x+dd)
            y2im <== f (x+asm.uj*dd)
            ctx.print.tt <| (y2re-y1)/dd ++ (y2im-y1)/dd
