//#############################################################################
// project title
let projectname = "test4"
let version = "1.0.0"
//#############################################################################
 
let outputdir = @"C:\home\testGit\testproj"

#I @"C:\home\LightwaveLaboratory\Aqualis\bin\Debug\net6.0"
#r "Aqualis.dll"
 
open Aqualis

type testClass1(sname_,name) =
    static member sname = "testClass1"
    new(name) =
        str.reg(testClass1.sname,name)
        testClass1(testClass1.sname,name)
    member public __.n1   = str.i0(sname_,name,"n1")
    member public __.x1   = str.d0(sname_,name,"x1")
    member public __.z1   = str.z0(sname_,name,"z1")
    static member str_mem(psname, vname, name, size1) =
        str.addmember(psname,(Structure(testClass1.sname),size1,name))
        testClass1(testClass1.sname,structure.mem(vname,name))
    member __.farg cm code = fn.addarg (Structure(testClass1.sname),A0,name) <| fun (_,n) -> code(testClass1(testClass1.sname,n))
    
Compile [F;C;] outputdir projectname ("aaa","aaa") <| fun () ->
    let f(y:num0,x:num0,n:num0,n1:num1,s:testClass1) =
        func "func1" <| fun () ->
            y.farg <| fun y ->
            x.farg <| fun x ->
            n.farg <| fun n ->
            n1.farg <| fun n1 ->
            s.farg "" <| fun s ->
                y <== x + n + n1[1] + s.x1
                print.c y
    ch.idz <| fun (n,x,z) ->
    ch.i1 4 <| fun n1 ->
        n <== 1
        x <== 2
        z <== 0
        n1[1] <== 3
        let s = testClass1("ss")
        s.x1 <== 100.0
        f(z,x,n,n1,s)
        print.c z
        