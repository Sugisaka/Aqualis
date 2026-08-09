//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    ///<summary>関数定義の引数</summary>
    type fn() =

        /// <summary>
        /// 関数定義の引数を追加
        /// </summary>
        /// <param name="typ">変数の型</param>
        /// <param name="vtp">変数の次元</param>
        /// <param name="n">変数名</param>
        static member addarg (ctx:Aqualis,typ:Etype,vtp:VarType,n:string) =
            fun code ->
                match ctx.language with
                |Fortran ->
                    //関数内ではこの変数名を使用
                    let name =
                        match typ,vtp with
                        |(It _|Dt|Zt|Structure _),A0 ->
                            "arg"+(ctx.arg.list.Length+1).ToString("00")
                        |_ ->
                            "arg"+(ctx.arg.list.Length+1).ToString("00")
                    match vtp with
                    |A0 ->
                        ctx.arg.add(n,(typ,vtp,name))
                    |A1 _ ->
                        ctx.arg.add(n,(typ,vtp,name))
                        ctx.arg.add(n+"_size",(It 4,A1(1),name+"_size"))
                    |A2 _ ->
                        ctx.arg.add(n,(typ,vtp,name))
                        ctx.arg.add(n+"_size",(It 4,A1(2),name+"_size"))
                    |A3 _ ->
                        ctx.arg.add(n,(typ,vtp,name))
                        ctx.arg.add(n+"_size",(It 4,A1(3),name+"_size"))
                    let argname =
                        match typ,vtp with
                        |(It _|Dt|Zt|Structure _),A0 -> name
                        |_ -> name
                    code(vtp,argname)
                |C99 ->
                    //関数内ではこの変数名を使用
                    let name =
                        match typ,vtp with
                        |(It _|Dt|Zt|Structure _),A0 ->
                            "arg"+(ctx.arg.list.Length+1).ToString("00")
                        |_ ->
                            "arg"+(ctx.arg.list.Length+1).ToString("00")
                    match vtp with
                    |A0 ->
                        ctx.arg.add(n,(typ,vtp,name))
                    |A1 _ ->
                        ctx.arg.add(n,(typ,vtp,name))
                        ctx.arg.add(n+"_size",(It 4,A1(1),name+"_size"))
                    |A2 _ ->
                        ctx.arg.add(n,(typ,vtp,name))
                        ctx.arg.add(n+"_size",(It 4,A1(2),name+"_size"))
                    |A3 _ ->
                        ctx.arg.add(n,(typ,vtp,name))
                        ctx.arg.add(n+"_size",(It 4,A1(3),name+"_size"))
                    let argname =
                        match typ,vtp with
                        |(It _|Dt|Zt|Structure _),A0 -> "(*"+name+")"
                        |_ -> name
                    code(vtp,argname)
                |LaTeX ->
                    //関数内ではこの変数名を使用
                    let name =
                        match typ,vtp with
                        |(It _|Dt|Zt|Structure _),A0 ->
                            "arg"+(ctx.arg.list.Length+1).ToString("00")
                        |_ ->
                            "arg"+(ctx.arg.list.Length+1).ToString("00")
                    match vtp with
                    |A0 ->
                        ctx.arg.add(n,(typ,vtp,name))
                    |A1 _ ->
                        ctx.arg.add(n,(typ,vtp,name))
                        ctx.arg.add(n+"_size",(It 4,A1(1),name+"_size"))
                    |A2 _ ->
                        ctx.arg.add(n,(typ,vtp,name))
                        ctx.arg.add(n+"_size",(It 4,A1(2),name+"_size"))
                    |A3 _ ->
                        ctx.arg.add(n,(typ,vtp,name))
                        ctx.arg.add(n+"_size",(It 4,A1(3),name+"_size"))
                    let argname =
                        match typ,vtp with
                        |(It _|Dt|Zt|Structure _),A0 -> name
                        |_ -> name
                    code(vtp,argname)
                |HTML ->
                    //関数内ではこの変数名を使用
                    let name =
                        match typ,vtp with
                        |(It _|Dt|Zt|Structure _),A0 ->
                            "arg"+(ctx.arg.list.Length+1).ToString("00")
                        |_ ->
                            "arg"+(ctx.arg.list.Length+1).ToString("00")
                    match vtp with
                    |A0 ->
                        ctx.arg.add(n,(typ,vtp,name))
                    |A1 _ ->
                        ctx.arg.add(n,(typ,vtp,name))
                        ctx.arg.add(n+"_size",(It 4,A1(1),name+"_size"))
                    |A2 _ ->
                        ctx.arg.add(n,(typ,vtp,name))
                        ctx.arg.add(n+"_size",(It 4,A1(2),name+"_size"))
                    |A3 _ ->
                        ctx.arg.add(n,(typ,vtp,name))
                        ctx.arg.add(n+"_size",(It 4,A1(3),name+"_size"))
                    let argname =
                        match typ,vtp with
                        |(It _|Dt|Zt|Structure _),A0 -> name
                        |_ -> name
                    code(vtp,argname)
                |Python ->
                    //関数内ではこの変数名を使用
                    let name =
                        match typ,vtp with
                        |(It _|Dt|Zt|Structure _),A0 ->
                            "arg"+(ctx.arg.list.Length+1).ToString("00")
                        |_ ->
                            "arg"+(ctx.arg.list.Length+1).ToString("00")
                    match vtp with
                    |A0 ->
                        ctx.arg.add(n,(typ,vtp,name))
                    |A1 _ ->
                        ctx.arg.add(n,(typ,vtp,name))
                        ctx.arg.add(n+"_size",(It 4,A1(1),name+"_size"))
                    |A2 _ ->
                        ctx.arg.add(n,(typ,vtp,name))
                        ctx.arg.add(n+"_size",(It 4,A1(2),name+"_size"))
                    |A3 _ ->
                        ctx.arg.add(n,(typ,vtp,name))
                        ctx.arg.add(n+"_size",(It 4,A1(3),name+"_size"))
                    let argname =
                        match typ,vtp with
                        |(It _|Dt|Zt|Structure _),A0 -> name
                        |_ -> name
                    code(vtp,argname)
                |_ -> ()

        /// <summary>
        /// 関数定義の引数を追加
        /// </summary>
        /// <param name="sname">構造体名</param>
        /// <param name="vtp">変数の次元</param>
        /// <param name="n">変数名</param>
        static member addarg (context:Aqualis,sname:string,vtp:VarType,n:string) = fn.addarg(context,Structure sname,vtp,n)

    [<AutoOpen>]
    module num_farg =
        // let private argumentContexts
        //     (sourceContext:Aqualis)
        //     (targetEnvironment:Aqualis) =
        //     GenerationContextMerge.requireTarget sourceContext,
        //     targetEnvironment.RequireGenerationContext()

        type int0 with
            /// <summary>
            /// この変数を関数内変数に変換
            /// </summary>
            member this.farg (targetEnvironment:Aqualis) code =
                let sourceContext,targetContext = this.Context, targetEnvironment
                fn.addarg (targetContext,this.etype,A0,this.Expr.eval sourceContext) <| fun (_,n) ->
                    code(int0(Var(this.etype,n,NaN), context=targetContext))
        type double0 with
            /// <summary>
            /// この変数を関数内変数に変換
            /// </summary>
            member this.farg (targetEnvironment:Aqualis) code =
                let sourceContext,targetContext = this.Context, targetEnvironment
                fn.addarg (targetContext,this.etype,A0,this.Expr.eval sourceContext) <| fun (_,n) ->
                    code(double0(Var(this.etype,n,NaN), context=targetContext))
        type complex0 with
            /// <summary>
            /// この変数を関数内変数に変換
            /// </summary>
            member this.farg (targetEnvironment:Aqualis) code =
                let sourceContext,targetContext = this.Context, targetEnvironment
                fn.addarg (targetContext,this.etype,A0,this.Expr.eval sourceContext) <| fun (_,n) ->
                    code(complex0(Var(this.etype,n,NaN), context=targetContext))

        type int1 with
            /// <summary>
            /// この変数を関数内変数に変換
            /// </summary>
            member this.farg (targetEnvironment:Aqualis) = fun code ->
                match this.Expr with
                |Var1(size,name) ->
                    let _,targetContext = this.Context, targetEnvironment
                    fn.addarg (targetContext,this.etype,size,name) <| fun (v,n) -> code(int1(this.etype,Var1(v,n), context=targetContext))
                |_ ->
                    printfn "部分配列を関数の引数にできません"
        type double1 with
            /// <summary>
            /// この変数を関数内変数に変換
            /// </summary>
            member this.farg (targetEnvironment:Aqualis) = fun code ->
                match this.Expr with
                |Var1(size,name) ->
                    let _,targetContext = this.Context, targetEnvironment
                    fn.addarg (targetContext,this.etype,size,name) <| fun (v,n) -> code(double1(this.etype,Var1(v,n), context=targetContext))
                |_ ->
                    printfn "部分配列を関数の引数にできません"
        type complex1 with
            /// <summary>
            /// この変数を関数内変数に変換
            /// </summary>
            member this.farg (targetEnvironment:Aqualis) = fun code ->
                match this.Expr with
                |Var1(size,name) ->
                    let _,targetContext = this.Context, targetEnvironment
                    fn.addarg (targetContext,this.etype,size,name) <| fun (v,n) -> code(complex1(this.etype,Var1(v,n), context=targetContext))
                |_ ->
                    printfn "部分配列を関数の引数にできません"

        type int2 with
            /// <summary>
            /// この変数を関数内変数に変換
            /// </summary>
            member this.farg (targetEnvironment:Aqualis) = fun code ->
                match this.Expr with
                |Var2(size,name) ->
                    let _,targetContext = this.Context, targetEnvironment
                    fn.addarg (targetContext,this.etype,size,name) <| fun (v,n) -> code(int2(this.etype,Var2(v,n), context=targetContext))
                |_ ->
                    printfn "部分配列を関数の引数にできません"
        type double2 with
            /// <summary>
            /// この変数を関数内変数に変換
            /// </summary>
            member this.farg (targetEnvironment:Aqualis) = fun code ->
                match this.Expr with
                |Var2(size,name) ->
                    let _,targetContext = this.Context, targetEnvironment
                    fn.addarg (targetContext,this.etype,size,name) <| fun (v,n) -> code(double2(this.etype,Var2(v,n), context=targetContext))
                |_ ->
                    printfn "部分配列を関数の引数にできません"
        type complex2 with
            /// <summary>
            /// この変数を関数内変数に変換
            /// </summary>
            member this.farg (targetEnvironment:Aqualis) = fun code ->
                match this.Expr with
                |Var2(size,name) ->
                    let _,targetContext = this.Context, targetEnvironment
                    fn.addarg (targetContext,this.etype,size,name) <| fun (v,n) -> code(complex2(this.etype,Var2(v,n), context=targetContext))
                |_ ->
                    printfn "部分配列を関数の引数にできません"

        type int3 with
            /// <summary>
            /// この変数を関数内変数に変換
            /// </summary>
            member this.farg (targetEnvironment:Aqualis) = fun code ->
                match this.Expr with
                |Var3(size,name) ->
                    let _,targetContext = this.Context, targetEnvironment
                    fn.addarg (targetContext,this.etype,size,name) <| fun (v,n) -> code(int3(this.etype,Var3(v,n), context=targetContext))
                |_ ->
                    printfn "部分配列を関数の引数にできません"
        type double3 with
            /// <summary>
            /// この変数を関数内変数に変換
            /// </summary>
            member this.farg (targetEnvironment:Aqualis) = fun code ->
                match this.Expr with
                |Var3(size,name) ->
                    let _,targetContext = this.Context, targetEnvironment
                    fn.addarg (targetContext,this.etype,size,name) <| fun (v,n) -> code(double3(this.etype,Var3(v,n), context=targetContext))
                |_ ->
                    printfn "部分配列を関数の引数にできません"
        type complex3 with
            /// <summary>
            /// この変数を関数内変数に変換
            /// </summary>
            member this.farg (targetEnvironment:Aqualis) = fun code ->
                match this.Expr with
                |Var3(size,name) ->
                    let _,targetContext = this.Context, targetEnvironment
                    fn.addarg (targetContext,this.etype,size,name) <| fun (v,n) -> code(complex3(this.etype,Var3(v,n), context=targetContext))
                |_ ->
                    printfn "部分配列を関数の引数にできません"
