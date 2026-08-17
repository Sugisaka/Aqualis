//#############################################################################
// 自動微分テスト
let projectname = "test6_3"
let version = "1.0.0"
//#############################################################################
 
let outputdir = @"C:\home\work" //__SOURCE_DIRECTORY__

#I @"..\..\bin\Debug\net10.0"
#r "Aqualis.dll"

open Aqualis

type AutoDiff(v:double0,dv:double0) =
    member _.x with get() = v
    member _.dx with get() = dv
    member this.Target(u:double0) = 
        this.dx <== u
    member this.Target() = this.Target _1d
            
    static member ( + ) (a:AutoDiff,b:AutoDiff) = AutoDiff(a.x+b.x, a.dx+b.dx)
    static member ( + ) (a:#IReal0,b:AutoDiff) = AutoDiff(a.ToDouble0,_0d) + b
    static member ( + ) (a:AutoDiff,b:#IReal0) = a + AutoDiff(b.ToDouble0,_0d)
    static member ( + ) (a:AutoDiff,b:int) = a + AutoDiff(D b,_0d)
    static member ( + ) (a:AutoDiff,b:double) = a + AutoDiff(D b,_0d)
    static member ( + ) (a:int,b:AutoDiff) = AutoDiff(D a,_0d) + b
    static member ( + ) (a:double,b:AutoDiff) = AutoDiff(D a,_0d) + b

    static member ( - ) (a:AutoDiff,b:AutoDiff) = AutoDiff(a.x-b.x, a.dx-b.dx)
    static member ( - ) (a:#IReal0,b:AutoDiff) = AutoDiff(a.ToDouble0,_0d) - b
    static member ( - ) (a:AutoDiff,b:#IReal0) = a - AutoDiff(b.ToDouble0,_0d)
    static member ( - ) (a:AutoDiff,b:int) = a - AutoDiff(D b,_0d)
    static member ( - ) (a:AutoDiff,b:double) = a - AutoDiff(D b,_0d)
    static member ( - ) (a:int,b:AutoDiff) = AutoDiff(D a,_0d) - b
    static member ( - ) (a:double,b:AutoDiff) = AutoDiff(D a,_0d) - b

    static member ( * ) (a:AutoDiff,b:AutoDiff) = AutoDiff(a.x*b.x, a.dx*b.x+a.x*b.dx)
    static member ( * ) (a:#IReal0,b:AutoDiff) = AutoDiff(a.ToDouble0,_0d) * b
    static member ( * ) (a:AutoDiff,b:#IReal0) = a * AutoDiff(b.ToDouble0,_0d)
    static member ( * ) (a:AutoDiff,b:int) = a * AutoDiff(D b,_0d)
    static member ( * ) (a:AutoDiff,b:double) = a * AutoDiff(D b,_0d)
    static member ( * ) (a:int,b:AutoDiff) = AutoDiff(D a,_0d) * b
    static member ( * ) (a:double,b:AutoDiff) = AutoDiff(D a,_0d) * b

    static member ( / ) (a:AutoDiff,b:AutoDiff) = AutoDiff(a.x/b.x, (a.dx*b.x-a.x*b.dx)/(b.x*b.x))
    static member ( / ) (a:#IReal0,b:AutoDiff) = AutoDiff(a.ToDouble0,_0d) / b
    static member ( / ) (a:AutoDiff,b:#IReal0) = a / AutoDiff(b.ToDouble0,_0d)
    static member ( / ) (a:AutoDiff,b:int) = a / AutoDiff(D b,_0d)
    static member ( / ) (a:AutoDiff,b:double) = a / AutoDiff(D b,_0d)
    static member ( / ) (a:int,b:AutoDiff) = AutoDiff(D a,_0d) / b
    static member ( / ) (a:double,b:AutoDiff) = AutoDiff(D a,_0d) / b
    
    static member ( <== ) (a:AutoDiff,b:AutoDiff) =
        a.x <== b.x
        a.dx <== b.dx
    static member ( <== ) (a:AutoDiff,b:double0) =
        a.x <== b
    static member ( <== ) (a:AutoDiff,b:int) =
        a.x <== D b
    static member ( <== ) (a:double0,b:AutoDiff) =
        a <== b.x
        
type asm with
    static member sin(a:AutoDiff) = AutoDiff(asm.sin a.x, a.dx*asm.cos a.x)
    static member cos(a:AutoDiff) = AutoDiff(asm.cos a.x, -a.dx*asm.sin a.x)

type ContextChd(c:Aqualis) =
    member _.d code =
        c.ch.dd <| fun (v,dv) -> 
            dv <== 0
            code (AutoDiff(v,dv))
            
type ContextDiff(c:Aqualis) =
    member _.diff(f:double0,a:double0) code =
        match a.Expr with
        |Var(_,xname,_) ->
            c.ch.d <| fun dx ->
                /// 式の値、微分値保存用変数カウント
                let rec counter(f:double0,n:int) = 
                    match f.Expr with
                    |Var(_,name,_) when name = xname ->
                        n
                    |Add(_,a,b) ->
                        // aとbの値を保存 → 2個追加
                        let m = n + 2
                        let na = counter (double0 a,m)
                        let nb = counter (double0 b,na)
                        nb
                    |Mul(_,a,b) ->
                        // aとbの値を保存 → 2個追加
                        let m = n + 2
                        let na = counter (double0 a,m)
                        let nb = counter (double0 b,na)
                        nb
                    |_ -> 
                        n
                let Nf = counter(f,0)
                // 一時変数：式の値を保存
                c.ch.d1 Nf <| fun w ->
                // 一時変数：微分値を保存
                c.ch.d1 Nf <| fun w' ->
                    /// 途中の式の値を変数に保存
                    let rec dataset(f:double0,n:int) = 
                        match f.Expr with
                        |Var(_,name,_) when name = xname ->
                            n,f
                        |Add(_,a,b) ->
                            let m = n + 2
                            let na,fa = dataset (double0 a,m)
                            // aの値を保存
                            w[n] <== fa
                            let nb,fb = dataset (double0 b,na)
                            // bの値を保存
                            w[n+1] <== fb
                            // この式の評価値：a+b
                            nb,w[n]+w[n+1]
                        |Mul(_,a,b) ->
                            let m = n + 2
                            let na,fa = dataset (double0 a,m)
                            // aの値を保存
                            w[n] <== fa
                            let nb,fb = dataset (double0 b,na)
                            // bの値を保存
                            w[n+1] <== fb
                            // この式の評価値：a+b
                            nb,w[n]*w[n+1]
                        |p -> 
                            n,double0 p
                    ignore <| dataset(f,0)
                    let rec dd(f:double0,df:double0,n:int) = 
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
                            let na = dd (double0 a,da,m)
                            let nb = dd (double0 b,db,na)
                            nb
                        |Mul(_,a,b) ->
                            let m = n + 2
                            /// aの値を一時変数リストから取得
                            let fa = w[n] //double0 a
                            /// bの値を一時変数リストから取得
                            let fb = w[n+1] //double0 b
                            /// aの微分値保存用
                            let da = w'[n]
                            /// bの微分値保存用
                            let db = w'[n+1]
                            da <== df*fb
                            db <== df*fa
                            let na = dd (double0 a,da,m)
                            let nb = dd (double0 b,db,na)
                            nb
                        |_ -> 
                            n
                    dx.clear()
                    ignore <| dd(f,_1d,0)
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
    ctx.chd.d <| fun x ->
    ctx.ch.d <| fun y1 ->
    ctx.ch.d <| fun y2 ->
    ctx.ch.d <| fun c1 ->
    ctx.chd.d <| fun result ->
        x <== -10
        x.Target()
        group.section "001" <| fun () ->
            let f (x:AutoDiff) = 
                c1 <== 4
                x*x+2*x+c1
            ctx.comment "代数微分"
            result <== f x
            ctx.print.t result.dx
            ctx.comment "数値微分"
            y1 <== f x
            y2 <== f (x+dd)
            ctx.print.t ((y2-y1)/dd)
    ctx.ch.d <| fun x ->
    ctx.ch.d <| fun y1 ->
    ctx.ch.d <| fun y2 ->
    ctx.ch.d <| fun c1 ->
    ctx.ch.d <| fun result ->
        x <== -10
        group.section "001" <| fun () ->
            let f (x:double0) = 
                c1 <== 4
                x*x+2*x+c1
            ctx.comment "代数微分"
            ctx.TopDown.diff(f x,x) <| fun df ->
                ctx.print.t df
            ctx.comment "数値微分"
            y1 <== f x
            y2 <== f (x+dd)
            ctx.print.t ((y2-y1)/dd)
