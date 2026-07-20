//#############################################################################
// 構造体を含む構造体・構造体配列テスト
let projectname = "test3_2"
let version = "1.0.0"
//#############################################################################
 
let outputdir = __SOURCE_DIRECTORY__

#I @"..\..\bin\Debug\net10.0"
#r "Aqualis.dll"

open Aqualis

    type testClass1(sname_,name) =
        inherit structureValue<testClass1>(sname_,name)
        static member sname = "testClass1"
        new(name) =
            str.reg(testClass1.sname,name)
            testClass1(testClass1.sname,name)
        override _.Rewrap n = testClass1(sname_,n)
        member public __.n1   = str.i0(sname_,name,"n1")
        member public __.x1   = str.d0(sname_,name,"x1")
        member public __.z1   = str.z0(sname_,name,"z1")
        static member str_mem(psname, vname, name, size1) =
            str.addmember(psname,(Structure testClass1.sname,size1,name))
            testClass1(testClass1.sname,str.mem(vname,name))
        
    type testClass1_1(sname_,name,size1) =
        inherit structureArray1<testClass1,testClass1_1>(sname_,name,size1)
        new(name,size1) =
            str.reg(testClass1.sname,name,size1)
            testClass1_1(testClass1.sname,name,A1 size1)
        new(name) = testClass1_1(name,0)
        override _.WrapElement n = testClass1(sname_,n)
        override _.Rewrap(n,v) = testClass1_1(sname_,n,v)
        static member str_mem(psname, vname, name, size1) =
            str.addmember(psname,(Structure testClass1.sname,size1,name))
            testClass1_1(testClass1.sname,str.mem(vname,name), size1)
        
    type testClass2(sname_,name) =
        inherit structureValue<testClass2>(sname_,name)
        static member sname = "testClass2"
        new(name) =
            str.reg(testClass2.sname,name)
            testClass2(testClass2.sname,name)
        override _.Rewrap n = testClass2(sname_,n)
        member public __.n1   = str.i0(sname_,name,"n2")
        member public __.x1   = str.d0(sname_,name,"x2")
        member public __.z1   = str.z0(sname_,name,"z2")
        member public __.s1   = testClass1.str_mem(testClass2.sname,name,"s2",A0)
        member public __.t1   = testClass1_1.str_mem(testClass2.sname,name,"t2",A1 0)
        
    type testClass2_1(sname_,name,size1) =
        inherit structureArray1<testClass2,testClass2_1>(sname_,name,size1)
        new(name,size1) =
            str.reg(testClass2.sname,name,size1)
            testClass2_1(testClass2.sname,name,A1(size1))
        new(name) = testClass2_1(name,0)
        override _.WrapElement n = testClass2(sname_,n)
        override _.Rewrap(n,v) = testClass2_1(sname_,n,v)
        static member str_mem(psname, vname, name, size1) =
            str.addmember(psname,(Structure testClass2.sname,size1,name))
            testClass2_1(testClass2.sname,str.mem(vname,name), size1)
        
Compile [Fortran;C99;Python;HTML;LaTeX;] outputdir projectname ("aaa","aaa") <| fun () ->
    let dd = testClass1_1("d")
    let xx = var.i1("xx")
    let pp = testClass2("p")
    let qq = testClass2_1 "q"
    dd.allocate 4
    xx.allocate 8
    pp.s1.n1 <== 100
    pp.s1.x1 <== 200.0
    pp.s1.z1 <== 300.0 + asm.uj*400.0
    pp.t1.allocate 3
    
    qq.allocate 5
    qq[0].t1.allocate 2
    qq[0].t1[1].n1 <== 2000
    print.t qq[0].t1[1].n1
    
    pp.t1[0].n1 <== 1000
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
    print.t pp.s1.n1
    print.t pp.s1.x1
    print.t pp.s1.z1
