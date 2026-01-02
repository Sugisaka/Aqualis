namespace Aqualis
    
    ///<summary>関数定義の引数</summary>
    type fn() =
        
        /// <summary>
        /// 関数定義の引数を追加
        /// </summary>
        /// <param name="typ">変数の型</param>
        /// <param name="vtp">変数の次元</param>
        /// <param name="n">変数名</param>
        static member addarg (typ:Etype,vtp:VarType,n:string) =
            fun code ->
                match programList[prIndex].language with
                |Fortran ->
                    //関数内ではこの変数名を使用
                    let name = 
                        match typ,vtp with
                        |(It _|Dt|Zt|Structure _),A0 ->
                            "arg"+(programList[prIndex].arg.list.Length+1).ToString("00")
                        |_ -> 
                            "arg"+(programList[prIndex].arg.list.Length+1).ToString("00")
                    match vtp with
                    |A0 ->
                        programList[prIndex].arg.add(n,(typ,vtp,name))
                    |A1 _ ->
                        programList[prIndex].arg.add(n,(typ,vtp,name))
                        programList[prIndex].arg.add(n+"_size",(It 4,A1(1),name+"_size"))
                    |A2 _ ->
                        programList[prIndex].arg.add(n,(typ,vtp,name))
                        programList[prIndex].arg.add(n+"_size",(It 4,A1(2),name+"_size"))
                    |A3 _ ->
                        programList[prIndex].arg.add(n,(typ,vtp,name))
                        programList[prIndex].arg.add(n+"_size",(It 4,A1(3),name+"_size"))
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
                            "arg"+(programList[prIndex].arg.list.Length+1).ToString("00")
                        |_ -> 
                            "arg"+(programList[prIndex].arg.list.Length+1).ToString("00")
                    match vtp with
                    |A0 ->
                        programList[prIndex].arg.add(n,(typ,vtp,name))
                    |A1 _ ->
                        programList[prIndex].arg.add(n,(typ,vtp,name))
                        programList[prIndex].arg.add(n+"_size",(It 4,A1(1),name+"_size"))
                    |A2 _ ->
                        programList[prIndex].arg.add(n,(typ,vtp,name))
                        programList[prIndex].arg.add(n+"_size",(It 4,A1(2),name+"_size"))
                    |A3 _ ->
                        programList[prIndex].arg.add(n,(typ,vtp,name))
                        programList[prIndex].arg.add(n+"_size",(It 4,A1(3),name+"_size"))
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
                            "arg"+(programList[prIndex].arg.list.Length+1).ToString("00")
                        |_ -> 
                            "arg"+(programList[prIndex].arg.list.Length+1).ToString("00")
                    match vtp with
                    |A0 ->
                        programList[prIndex].arg.add(n,(typ,vtp,name))
                    |A1 _ ->
                        programList[prIndex].arg.add(n,(typ,vtp,name))
                        programList[prIndex].arg.add(n+"_size",(It 4,A1(1),name+"_size"))
                    |A2 _ ->
                        programList[prIndex].arg.add(n,(typ,vtp,name))
                        programList[prIndex].arg.add(n+"_size",(It 4,A1(2),name+"_size"))
                    |A3 _ ->
                        programList[prIndex].arg.add(n,(typ,vtp,name))
                        programList[prIndex].arg.add(n+"_size",(It 4,A1(3),name+"_size"))
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
                            "arg"+(programList[prIndex].arg.list.Length+1).ToString("00")
                        |_ -> 
                            "arg"+(programList[prIndex].arg.list.Length+1).ToString("00")
                    match vtp with
                    |A0 ->
                        programList[prIndex].arg.add(n,(typ,vtp,name))
                    |A1 _ ->
                        programList[prIndex].arg.add(n,(typ,vtp,name))
                        programList[prIndex].arg.add(n+"_size",(It 4,A1(1),name+"_size"))
                    |A2 _ ->
                        programList[prIndex].arg.add(n,(typ,vtp,name))
                        programList[prIndex].arg.add(n+"_size",(It 4,A1(2),name+"_size"))
                    |A3 _ ->
                        programList[prIndex].arg.add(n,(typ,vtp,name))
                        programList[prIndex].arg.add(n+"_size",(It 4,A1(3),name+"_size"))
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
                            "arg"+(programList[prIndex].arg.list.Length+1).ToString("00")
                        |_ -> 
                            "arg"+(programList[prIndex].arg.list.Length+1).ToString("00")
                    match vtp with
                    |A0 ->
                        programList[prIndex].arg.add(n,(typ,vtp,name))
                    |A1 _ ->
                        programList[prIndex].arg.add(n,(typ,vtp,name))
                        programList[prIndex].arg.add(n+"_size",(It 4,A1(1),name+"_size"))
                    |A2 _ ->
                        programList[prIndex].arg.add(n,(typ,vtp,name))
                        programList[prIndex].arg.add(n+"_size",(It 4,A1(2),name+"_size"))
                    |A3 _ ->
                        programList[prIndex].arg.add(n,(typ,vtp,name))
                        programList[prIndex].arg.add(n+"_size",(It 4,A1(3),name+"_size"))
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
        static member addarg (sname:string,vtp:VarType,n:string) = fn.addarg(Structure sname,vtp,n)
        
    [<AutoOpen>]
    module num_farg =                    
        type num0 with
            /// <summary>
            /// この変数を関数内変数に変換
            /// </summary>
            member this.farg code =
                fn.addarg (this.etype,A0,this.Expr.eval (programList[prIndex])) <| fun (v,n) -> 
                    code(num0(Var(this.etype, n, NaN)))
                    
        type num1 with
            /// <summary>
            /// この変数を関数内変数に変換
            /// </summary>
            member this.farg = fun code ->
                match this.Expr with
                |Var1(size,name) ->
                    fn.addarg (this.etype,size,name) <| fun (v,n) -> code(num1(this.etype,Var1(v,n)))
                |_ -> 
                    printfn "部分配列を関数の引数にできません"
                    
        type num2 with
            /// <summary>
            /// この変数を関数内変数に変換
            /// </summary>
            member this.farg = fun code ->
                match this.Expr with
                |Var2(size,name) ->
                    fn.addarg (this.etype,size,name) <| fun (v,n) -> code(num2(this.etype,Var2(v,n)))
                |_ -> 
                    printfn "部分配列を関数の引数にできません"
                    
        type num3 with
            /// <summary>
            /// この変数を関数内変数に変換
            /// </summary>
            member this.farg = fun code ->
                match this.Expr with
                |Var3(size,name) ->
                    fn.addarg (this.etype,size,name) <| fun (v,n) -> code(num3(this.etype,Var3(v,n)))
                |_ -> 
                    printfn "部分配列を関数の引数にできません"
                    