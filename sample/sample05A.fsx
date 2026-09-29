//#############################################################################
// project title
let projectname = "sample05A"
// sample program version
let version = "1.0.0"
// Directory for source file output
let outputdir = @"C:\home\work"
//#############################################################################

#r "nuget: Aqualis, 188.1.1"

open Aqualis

    /// <summary>
    /// testClass1
    /// </summary>
    type testClass1(sname_,name,ctx:Aqualis) =
        inherit structureValue(sname_,name,ctx)
        static member sname = "testClass1"
        static member Descriptor : StructureDescriptor<testClass1> =
            { StructureName = testClass1.sname
              Wrap = fun (n,targetContext) -> testClass1(testClass1.sname,n,targetContext) }
        new(name,ctx:Aqualis) =
            ctx.str.reg(testClass1.sname,name)
            testClass1(testClass1.sname,name,ctx)
        member public __.n1 = ctx.str.i0(sname_,name,"x1")
        member public __.x1 = ctx.str.d0(sname_,name,"y1")
        member public __.z1 = ctx.str.z0(sname_,name,"x2")
            
Compile [Fortran;C99;Python;HTML;LaTeX;] outputdir projectname version <| fun ctx ->
    let cc = testClass1("c",ctx)
    cc.n1 <== 1
    cc.x1 <== 2.0
    cc.z1 <== 3.0 + asm.uj*4.0
    ctx.print.t cc.n1
    ctx.print.t cc.x1
    ctx.print.t cc.z1
    let dd = structureArray1<testClass1>(testClass1.Descriptor,"d",0,ctx)
    let xx = ctx.var.i1 "xx"
    dd.allocate 4
    xx.allocate 8
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
