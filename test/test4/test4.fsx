//#############################################################################
// 関数テスト
let projectname = "test4"
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
        inherit structureValue<testClass1>(sname_,name,ctx)
        static member sname = "testClass1"
        new(name,ctx:Aqualis) =
            ctx.str.reg(testClass1.sname,name)
            testClass1(testClass1.sname,name,ctx)
        override _.Rewrap(n,targetEnvironment) = testClass1(sname_,n,targetEnvironment)
        member public __.n1 = ctx.str.i0(sname_,name,"x1")
        member public __.x1 = ctx.str.d0(sname_,name,"y1")
        member public __.z1 = ctx.str.z0(sname_,name,"x2")
        
Compile [Fortran;C99;Python] outputdir projectname "aaa" <| fun ctx ->
    let f(y:double0,x:double0,n:int0,n1:int1,s:testClass1) =
        ctx.func "func1" <| fun c ->
            y.farg c <| fun y ->
            x.farg c <| fun x ->
            n.farg c <| fun n ->
            n1.farg c <| fun n1 ->
            s.farg c <| fun s ->
                y <== x + n + n1[0] + s.x1
                c.print.t y
    ctx.ch.idd <| fun (n,x,z) ->
    ctx.ch.i1 4 <| fun n1 ->
        n <== 1
        x <== 2
        z <== 0
        n1[0] <== 3
        let s = testClass1("ss",ctx)
        s.x1 <== 100.0
        f(z,x,n,n1,s)
        ctx.print.t z
