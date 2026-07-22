//#############################################################################
// 構造体・構造体配列テスト
let projectname = "test3_1"
let version = "1.0.0"
//#############################################################################
 
let outputdir = __SOURCE_DIRECTORY__

#I @"..\..\bin\Debug\net10.0"
#r "Aqualis.dll"

open Aqualis

    /// <summary>
    /// testClass1
    /// </summary>
    type testClass1(sname_,name) =
        inherit structureValue<testClass1>(sname_,name)
        static member sname = "testClass1"
        new(name) =
            str.reg(testClass1.sname,name)
            testClass1(testClass1.sname,name)
        override _.Rewrap n = testClass1(sname_,n)
        member public __.n1   = str.i0(sname_,name,"x1")
        member public __.x1   = str.d0(sname_,name,"y1")
        member public __.z1   = str.z0(sname_,name,"x2")
        
    /// <summary>
    /// testClass1の配列
    /// </summary>
    type testClass1_1(sname_,name,size1) =
        inherit structureArray1<testClass1,testClass1_1>(sname_,name,size1)
        new(name,size1) =
            str.reg(testClass1.sname,name,size1)
            testClass1_1(testClass1.sname,name,A1 size1)
        new(name) = testClass1_1(name,0)
        override _.WrapElement n = testClass1(sname_,n)
        override _.Rewrap(n,v) = testClass1_1(sname_,n,v)
        static member str_mem(psname, vname, name, size1) =
            str.addmember(psname,(Structure(testClass1.sname),size1,name))
            testClass1_1(testClass1.sname,str.mem(vname,name), size1)
        
Compile [Fortran;C99;Python;HTML;LaTeX;] outputdir projectname ("aaa","aaa") <| fun ctx ->
    let cc = testClass1 "c"
    cc.n1 <== 1
    cc.x1 <== 2.0
    cc.z1 <== 3.0 + asm.uj*4.0
    print.t cc.n1
    print.t cc.x1
    print.t cc.z1
    let dd = testClass1_1 "d"
    let xx = var.i1 "xx"
    dd.allocate 4
    xx.allocate 8
    dd.foreach <| fun i ->
        dd[i].n1 <== 1
        dd[i].x1 <== 2.0
        dd[i].z1 <== 3.0 + asm.uj*4.0
    ch.i1 10 <| fun nn ->
        nn[0] <== 0
        nn[1] <== 1
        nn[2] <== 2
        nn[3] <== 3
    ch.i1 20 <| fun nn ->
        nn[0] <== 0
        nn[1] <== 1
        nn[2] <== 2
        nn[3] <== 3
