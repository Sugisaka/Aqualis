//#############################################################################
// 構造体を含む構造体・構造体配列テスト
let projectname = "test3_2"
let version = "1.0.0"
//#############################################################################
 
let outputdir = __SOURCE_DIRECTORY__

#I @"..\..\bin\Debug\net10.0"
#r "Aqualis.dll"

open Aqualis

    type testClass1(sname_,name,ctx:Aqualis) =
        inherit structureValue<testClass1>(sname_,name,?context=ctx.GenerationContext)
        static member sname = "testClass1"
        new(name,ctx:Aqualis) =
            ctx.str.reg(testClass1.sname,name)
            testClass1(testClass1.sname,name,ctx)
        override _.Rewrap(n,targetEnvironment) = testClass1(sname_,n,targetEnvironment)
        member public __.n1 = ctx.str.i0(sname_,name,"n1")
        member public __.x1 = ctx.str.d0(sname_,name,"x1")
        member public __.z1 = ctx.str.z0(sname_,name,"z1")
        static member str_mem(psname, vname, name, size1,ctx:Aqualis) =
            ctx.str.addmember(psname,(Structure testClass1.sname,size1,name))
            testClass1(testClass1.sname,ctx.str.mem(vname,name),ctx)
        
    type testClass1_1(sname_,name,size1,ctx:Aqualis) =
        inherit structureArray1<testClass1,testClass1_1>(sname_,name,size1,?context=ctx.GenerationContext)
        new(name,size1,ctx:Aqualis) =
            ctx.str.reg(testClass1.sname,name,size1)
            testClass1_1(testClass1.sname,name,A1 size1,ctx)
        new(name,ctx) = testClass1_1(name,0,ctx)
        override _.WrapElement n = testClass1(sname_,n,ctx)
        override _.Rewrap(n,v,targetEnvironment) = testClass1_1(sname_,n,v,targetEnvironment)
        static member str_mem(psname, vname, name, size1,ctx:Aqualis) =
            ctx.str.addmember(psname,(Structure testClass1.sname,size1,name))
            testClass1_1(testClass1.sname,ctx.str.mem(vname,name), size1,ctx)
        
    type testClass2(sname_,name,ctx:Aqualis) =
        inherit structureValue<testClass2>(sname_,name,?context=ctx.GenerationContext)
        static member sname = "testClass2"
        new(name,ctx:Aqualis) =
            ctx.str.reg(testClass2.sname,name)
            testClass2(testClass2.sname,name,ctx)
        override _.Rewrap(n,targetEnvironment) = testClass2(sname_,n,targetEnvironment)
        member public __.n1 = ctx.str.i0(sname_,name,"n2")
        member public __.x1 = ctx.str.d0(sname_,name,"x2")
        member public __.z1 = ctx.str.z0(sname_,name,"z2")
        member public __.s1 = testClass1.str_mem(testClass2.sname,name,"s2",A0,ctx)
        member public __.t1 = testClass1_1.str_mem(testClass2.sname,name,"t2",A1 0,ctx)
        
    type testClass2_1(sname_,name,size1,ctx:Aqualis) =
        inherit structureArray1<testClass2,testClass2_1>(sname_,name,size1,?context=ctx.GenerationContext)
        new(name,size1,ctx:Aqualis) =
            ctx.str.reg(testClass2.sname,name,size1)
            testClass2_1(testClass2.sname,name,A1(size1),ctx)
        new(name,ctx:Aqualis) = testClass2_1(name,0,ctx)
        override _.WrapElement n = testClass2(sname_,n,ctx)
        override _.Rewrap(n,v,targetEnvironment) = testClass2_1(sname_,n,v,targetEnvironment)
        static member str_mem(psname, vname, name, size1,ctx:Aqualis) =
            ctx.str.addmember(psname,(Structure testClass2.sname,size1,name))
            testClass2_1(testClass2.sname,ctx.str.mem(vname,name), size1, ctx)
        
Compile [Fortran;C99;Python;HTML;LaTeX;] outputdir projectname "aaa" <| fun ctx ->
    let dd = testClass1_1("d",ctx)
    let xx = ctx.var.i1 "xx"
    let pp = testClass2("p",ctx)
    let qq = testClass2_1("q",ctx)
    dd.allocate 4
    xx.allocate 8
    pp.s1.n1 <== 100
    pp.s1.x1 <== 200.0
    pp.s1.z1 <== 300.0 + asm.uj*400.0
    pp.t1.allocate 3
    
    qq.allocate 5
    qq[0].t1.allocate 2
    qq[0].t1[1].n1 <== 2000
    ctx.print.t qq[0].t1[1].n1
    
    pp.t1[0].n1 <== 1000
    dd.foreach <| fun i ->
        dd[i].n1 <== 1
        dd[i].x1 <== 2.0
        dd[i].z1 <== 3.0 + asm.uj*4.0
    ctx.ch.i1 10 <| fun nn ->
        nn[0] <== 0
        nn[1] <== 1
        nn[2] <== 2
        nn[3] <== 3
    ctx.ch.i1 20 <| fun nn ->
        nn[0] <== 0
        nn[1] <== 1
        nn[2] <== 2
        nn[3] <== 3
    ctx.print.t pp.s1.n1
    ctx.print.t pp.s1.x1
    ctx.print.t pp.s1.z1
