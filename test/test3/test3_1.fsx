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
    type testClass1(sname_,name,ctx:Aqualis) =
        inherit structureValue<testClass1>(sname_,name,?context=ctx.GenerationContext)
        static member sname = "testClass1"
        new(name,ctx:Aqualis) =
            ctx.str.reg(testClass1.sname,name)
            testClass1(testClass1.sname,name,ctx)
        override _.Rewrap(n,targetEnvironment) = testClass1(sname_,n,targetEnvironment)
        member public __.n1 = ctx.str.i0(sname_,name,"x1")
        member public __.x1 = ctx.str.d0(sname_,name,"y1")
        member public __.z1 = ctx.str.z0(sname_,name,"x2")
        
    /// <summary>
    /// testClass1の配列
    /// </summary>
    type testClass1_1(sname_,name,size1,ctx:Aqualis) =
        inherit structureArray1<testClass1,testClass1_1>(sname_,name,size1,?context=ctx.GenerationContext)
        new(name,size1,ctx:Aqualis) =
            ctx.str.reg(testClass1.sname,name,size1)
            testClass1_1(testClass1.sname,name,A1 size1,ctx)
        new(name,ctx:Aqualis) = testClass1_1(name,0,ctx)
        override _.WrapElement n = testClass1(sname_,n,ctx)
        override _.Rewrap(n,v,targetEnvironment) = testClass1_1(sname_,n,v,targetEnvironment)
        static member str_mem(psname, vname, name, size1,ctx:Aqualis) =
            ctx.str.addmember(psname,(Structure(testClass1.sname),size1,name))
            testClass1_1(testClass1.sname,ctx.str.mem(vname,name), size1,ctx)
            
Compile [Fortran;C99;Python;HTML;LaTeX;] outputdir projectname "aaa" <| fun ctx ->
    let cc = testClass1("c",ctx)
    cc.n1 <== 1
    cc.x1 <== 2.0
    cc.z1 <== 3.0 + asm.uj*4.0
    ctx.print.t cc.n1
    ctx.print.t cc.x1
    ctx.print.t cc.z1
    let dd = testClass1_1("d",ctx)
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
