[目次へ戻る](index.md)

## クラス定義例

以下のコードでクラス「classAAA」を定義可能。Fortran、C言語ではフィールドを構造体、メソッドをインライン展開して実装される

```fsharp
/// <summary>
/// クラスの定義
/// </summary>
type classAAA(sname_,name) =
    static member sname = "classAAA"
    /// <summary>コンストラクタ</summary>
    new(name) =
        str.reg(classAAA.sname,name)
        classAAA(classAAA.sname,name)
    /// <summary>int型フィールド「a」</summary>
    member public __.a = 
        str.i0(sname_,name,"a")
    /// <summary>double型フィールド「x」</summary>
    member public __.x = 
        str.d0(sname_,name,"x")
    /// <summary>complex型フィールド「w」</summary>
    member public __.w = 
        str.z0(sname_,name,"w")
    /// <summary>メソッド</summary>
    member this.add(n:int) =
        this.a <== this.a + n
    /// <summary>このクラスのインスタンスを別クラスのフィールドとする場合は、このメソッドを必ず実装</summary>
    static member str_mem(psname, vname, name, size1) =
        str.addmember(psname,(Structure(classAAA.sname),size1,name))
        classAAA(classAAA.sname,structure.mem(vname,name))
    /// <summary>このクラスのインスタンスを非インライン関数の引数にする場合は、このメソッドを必ず実装</summary>
    member __.farg cm code =
        fn.addarg (Structure(classAAA.sname),A0,name,cm) <| fun (t,v,n) -> code(classAAA(classAAA.sname,n))
        
/// <summary>
/// classAAAの1次元配列を実装
/// </summary>
type classAAA_1(sname_,name,size1) =
    // base1(1次元配列型変数)の継承
    inherit base1(Structure(sname_),size1,name)
    /// <summary>コンストラクタ</summary>
    new(name,size1) =
        str.reg(classAAA.sname,name,size1)
        classAAA_1(classAAA.sname,name,A1(size1))
    new(name) = classAAA_1(name,0)
    /// <summary>配列のインデクサ</summary>
    member this.Item with get(i:num0) = 
        classAAA(sname_,this.Idx1(i))
    /// <summary>配列のインデクサ</summary>
    member this.Item with get(i:int ) = 
        classAAA(sname_,this.Idx1(i))
    /// <summary>このクラスのインスタンスを別クラスのフィールドとする場合は、このメソッドを必ず実装</summary>
    static member str_mem(psname, vname, name, size1) =
        str.addmember(psname,(Structure(classAAA.sname),size1,name))
        classAAA_1(classAAA.sname,structure.mem(vname,name), size1)
    /// <summary>このクラスのインスタンスを非インライン関数の引数にする場合は、このメソッドを必ず実装)</summary>
    member __.farg cm = fun code -> 
        fn.addarg (classAAA.sname,size1,name,cm) <| fun (t,v,n) -> code(classAAA_1(classAAA.sname,n,v))
```

### 使用例

```fsharp
Compile [F] outputdir projectname fullversion <| fun () ->
    
    //classAAA型変数（変数名：abc）を生成
    let u = classAAA("abc")
    //フィールドへのアクセスは「変数名.フィールド名」
    u.a <== 1
    u.x <== 2.0
    u.w <== 3.0+asm.uj*4.0
    print.ccc u.a u.x u.w
    
    //classAAA型1次元配列（配列名：xyz）を生成
    let v = classAAA_1("xyz")
    //配列要素数を指定してメモリ確保
    v.allocate(10)
    //配列へのアクセス
    v.foreach <| fun i ->
        v[i].a <== 1
        v[i].x <== 2.0
        v[i].w <== 3.0+asm.uj*4.0
        print.ccc v[i].a v[i].x v[i].w
```
