[目次へ戻る](index.md)
## クラス定義

以下のコードでクラス「classAAA」を定義可能。Fortran、C言語ではフィールドを構造体、メソッドをインライン展開して実装される

```fsharp
    /// <summary>
    /// クラスの定義
    /// </summary>
    type classAAA(sname_,name) =
        static member sname = "classAAA"
        new(name) =
            str.reg(classAAA.sname,name)
            classAAA(classAAA.sname,name)
        /// <summary>int型フィールド「a」</summary>
        member public __.a = 
            str.i0(sname_,name,"a")
        /// <summary>double型フィールド「x」</summary>
        member public __.x = 
            str.d0(sname_,name,"x")
        /// <summary>double型フィールド「w」</summary>
        member public __.w = 
            str.z0(sname_,name,"w")
        /// <summary>メソッド</summary>
        static member this.add(n:int) =
            this.a <== this.a + n
        /// <summary>このクラスのインスタンスを別クラスのフィールドとする場合は必ず実装</summary>
        static member str_mem(psname, vname, name, size1) =
            str.addmember(psname,(Structure(classAAA.sname),size1,name))
            classAAA_1(classAAA.sname,structure.mem(vname,name), size1)
        /// <summary>このクラスのインスタンスを非インライン関数の引数にする場合は、このメソッドを必ず実装</summary>
        member __.farg cm code =
            fn.addarg (Structure(classAAA.sname),A0,name,cm) <| fun (t,v,n) -> code(classAAA(classAAA.sname,n))
            
    /// <summary>
    /// classAAAの1次元配列を実装
    /// </summary>
    type classAAA_1(sname_,name,size1) =
        inherit base1(Structure(sname_),size1,name)
        new(name,size1) =
            str.reg(classAAA.sname,name,size1)
            classAAA_1(classAAA.sname,name,A1(size1))
        new(name) = classAAA_1(name,0)
        /// <summary>配列のインデクサ</summary>
        member this.Item with get(i:num0) = 
            classAAA(sname_,this.Idx1(i))
        /// <summary>配列のインデクサ</summary>
        member this.Item with get(i:int ) = classAAA(sname_,this.Idx1(i))
        /// <summary>このクラスのインスタンスを別クラスのフィールドとする場合は必ず実装</summary>
        static member str_mem(psname, vname, name, size1) =
            str.addmember(psname,(Structure(classAAA.sname),size1,name))
            classAAA_1(classAAA.sname,structure.mem(vname,name), size1)
        /// <summary>このクラスのインスタンスを非インライン関数の引数にする場合は、このメソッドを必ず実装)</summary>
        member __.farg cm = fun code -> 
            fn.addarg (classAAA.sname,size1,name,cm) <| fun (t,v,n) -> code(classAAA_1(classAAA.sname,n,v))
```
