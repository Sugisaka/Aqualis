//#############################################################################
// project title
let projectname = "sample05B"
// sample program version
let version = "1.0.0"
// Directory for source file output
let outputdir = @"C:\home\work"
//#############################################################################

#r "nuget: Aqualis, 188.0.4"

open Aqualis

    type testClass1(sname_,name,ctx:Aqualis) =
        inherit structureValue(sname_,name,ctx)
        static member sname = "testClass1"
        static member Descriptor : StructureDescriptor<testClass1> =
            { StructureName = testClass1.sname
              Wrap = fun (n,targetContext) -> testClass1(testClass1.sname,n,targetContext) }
        new(name,ctx:Aqualis) =
            ctx.str.reg(testClass1.sname,name)
            testClass1(testClass1.sname,name,ctx)
        member public __.n1 = ctx.str.i0(sname_,name,"n1")
        member public __.x1 = ctx.str.d0(sname_,name,"x1")
        member public __.z1 = ctx.str.z0(sname_,name,"z1")
        static member str_mem(psname, vname, name, size1,ctx:Aqualis) =
            ctx.str.addmember(psname,(Structure testClass1.sname,size1,name))
            testClass1(testClass1.sname,ctx.str.mem(vname,name),ctx)
        
    type testClass2(sname_,name,ctx:Aqualis) =
        inherit structureValue(sname_,name,ctx)
        static member sname = "testClass2"
        static member Descriptor : StructureDescriptor<testClass2> =
            { StructureName = testClass2.sname
              Wrap = fun (n,targetContext) -> testClass2(testClass2.sname,n,targetContext) }
        new(name,ctx:Aqualis) =
            ctx.str.reg(testClass2.sname,name)
            testClass2(testClass2.sname,name,ctx)
        member public __.n1 = ctx.str.i0(sname_,name,"n2")
        member public __.x1 = ctx.str.d0(sname_,name,"x2")
        member public __.z1 = ctx.str.z0(sname_,name,"z2")
        member public __.s1 = testClass1.str_mem(testClass2.sname,name,"s2",A0,ctx)
        member public __.t1 = structureArray1<testClass1>.str_mem(testClass1.Descriptor,testClass2.sname,name,"t2",A1 0,ctx)
        
Compile [Fortran;C99;Python;HTML;LaTeX;] outputdir projectname version <| fun ctx ->
    let dd = structureArray1<testClass1>(testClass1.Descriptor,"d",0,ctx)
    let xx = ctx.var.i1 "xx"
    let pp = testClass2("p",ctx)
    let qq = structureArray1<testClass2>(testClass2.Descriptor,"q",0,ctx)
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
