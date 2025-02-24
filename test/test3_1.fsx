//#############################################################################
// project title
let projectname = "test3_1"
let version = "1.0.0"
//#############################################################################
 
let outputdir = __SOURCE_DIRECTORY__

#I @"..\bin\Debug\net8.0"
#r "Aqualis.dll"
 
open Aqualis

    /// <summary>
    /// 境界要素：幾何学情報
    /// </summary>
    type testClass1(sname_,name) =
        static member sname = "testClass1"
        new(name) =
            str.reg(testClass1.sname,name)
            testClass1(testClass1.sname,name)
        member public __.n1   = str.i0(sname_,name,"x1")
        member public __.x1   = str.d0(sname_,name,"y1")
        member public __.z1   = str.z0(sname_,name,"x2")
        /// <summary>
        /// この変数を関数内変数に変換
        /// </summary>
        /// <param name="cm">コメント</param>
        /// <param name="code">関数内コード</param>
        member __.farg code = fn.addarg (Structure(testClass1.sname),A0,name) <| fun (_,n) -> code(testClass1(testClass1.sname,n))
        
    /// <summary>
    /// 行列インデックスを持たない境界要素の配列
    /// </summary>
    type testClass1_1(sname_,name,size1) =
        inherit base1(Structure(testClass1.sname),Var1(size1,name))
        new(name,size1) =
            str.reg(testClass1.sname,name,size1)
            testClass1_1(testClass1.sname,name,A1(size1))
        new(name) = testClass1_1(name,0)
        member this.Item with get(i:num0) = testClass1(sname_,this.Idx1(i).code)
        member this.Item with get(i:int ) = testClass1(sname_,this.Idx1(i).code)
        member public this.allocate(n:num0) = this.allocate(n)
        member public this.allocate(n:int) = this.allocate(n.I)
        static member str_mem(psname, vname, name, size1) =
            str.addmember(psname,(Structure(testClass1.sname),size1,name))
            testClass1_1(testClass1.sname,structure.mem(vname,name), size1)
        /// <summary>
        /// この変数を関数内変数に変換
        /// </summary>
        /// <param name="cm">コメント</param>
        member __.farg code = fn.addarg (testClass1.sname,size1,name) <| fun (v,n) -> code(testClass1_1(testClass1.sname,n,v))
        
Compile [Fortran;C99;Python;HTML;LaTeX;] outputdir projectname ("aaa","aaa") <| fun () ->
    let cc = testClass1("c")
    cc.n1 <== 1
    cc.x1 <== 2.0
    cc.z1 <== 3.0 + asm.uj*4.0
    print.c cc.n1
    print.c cc.x1
    print.c cc.z1
    let dd = testClass1_1("d")
    let xx = var.i1("xx")
    dd.allocate(4)
    xx.allocate(8)
    dd.foreach <| fun i ->
        dd[i].n1 <== 1
        dd[i].x1 <== 2.0
        dd[i].z1 <== 3.0 + asm.uj*4.0
    ch.i1 10 <| fun nn ->
        nn[1] <== 1
        nn[2] <== 2
        nn[3] <== 3
        nn[4] <== 4
    ch.i1 20 <| fun nn ->
        nn[1] <== 1
        nn[2] <== 2
        nn[3] <== 3
        nn[4] <== 4