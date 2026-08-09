[目次へ戻る](index.md)

## クラス定義例

以下のコードでクラス「classAAA」を定義可能。Fortran、C言語ではフィールドを構造体、メソッドをインライン展開して実装される

```fsharp
/// <summary>
/// testClass1
/// </summary>
type testClass1(sname_,name,ctx:Aqualis) =
    inherit structureValue<testClass1>(sname_,name,ctx)
    /// クラス名
    static member sname = "testClass1"
    /// コンストラクタ
    new(name,ctx:Aqualis) =
        ctx.str.reg(testClass1.sname,name)
        testClass1(testClass1.sname,name,ctx)
    override _.Rewrap(n,targetEnvironment) = testClass1(sname_,n,targetEnvironment)
    /// フィールド1
    member public __.n1 = ctx.str.i0(sname_,name,"x1")
    /// フィールド2
    member public __.x1 = ctx.str.d0(sname_,name,"y1")
    /// フィールド3
    member public __.z1 = ctx.str.z0(sname_,name,"x2")
        
/// <summary>
/// testClass1の配列
/// </summary>
type testClass1_1(sname_,name,size1,ctx:Aqualis) =
    inherit structureArray1<testClass1,testClass1_1>(sname_,name,size1,ctx)
    new(name,size1,ctx:Aqualis) =
        ctx.str.reg(testClass1.sname,name,size1)
        testClass1_1(testClass1.sname,name,A1 size1,ctx)
    new(name,ctx:Aqualis) = testClass1_1(name,0,ctx)
    override _.WrapElement n = testClass1(sname_,n,ctx)
    override _.Rewrap(n,v,targetEnvironment) = testClass1_1(sname_,n,v,targetEnvironment)
    /// このクラスを別のクラスのフィールドにする場合は以下のメソッドも定義する
    static member str_mem(psname, vname, name, size1,ctx:Aqualis) =
        ctx.str.addmember(psname,(Structure(testClass1.sname),size1,name))
        testClass1_1(testClass1.sname,ctx.str.mem(vname,name), size1,ctx)
```

### 使用例

```fsharp
Compile [Fortran] outputdir projectname fullversion <| fun ctx ->
    
    //testClass1型変数（変数名：abc）を生成
    let u = testClass1("u",ctx)
    //フィールドへのアクセスは「変数名.フィールド名」
    u.n1 <== 1
    u.x1 <== 2.0
    u.z1 <== 3.0+asm.uj*4.0
    print.tt <| u.n1 ++ u.x1 ++ u.z1
    
    //testClass1型1次元配列（配列名：xyz）を生成
    let v = testClass1_1("v",ctx)
    //配列要素数を指定してメモリ確保
    v.allocate(10)
    //配列へのアクセス
    v.foreach <| fun i ->
        v[i].n1 <== 1
        v[i].x1 <== 2.0
        v[i].z1 <== 3.0+asm.uj*4.0
        print.tt <| v[i].a ++ v[i].x ++ v[i].w
```
