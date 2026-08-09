namespace Aqualis.Tests

open System
open System.IO
open Xunit
open Aqualis

module StructureDependencyTests =
    let private writeStructures (context:Aqualis) path =
        use writer = new codeWriter(path, 2, C99)
        context.str.Def_Structure writer
        writer.close()

    [<Fact>]
    let ``structure dependencies are emitted before their users`` () =
        use output = new TemporaryDirectory()
        use context = new Aqualis(Some output.Path, Some "acyclic-body.c", C99)
        let structurePath = Path.Combine(output.Path, "acyclic-structures.c")
        context.cstr.addstructure "leaf"
        context.cstr.addmember("leaf", (Dt, A0, "value"))
        context.cstr.addstructure "branch"
        context.cstr.addmember("branch", (Structure "leaf", A0, "leaf"))
        writeStructures context structurePath
        let generated = File.ReadAllText structurePath
        let leafPosition = generated.IndexOf("typedef struct _leaf")
        let branchPosition = generated.IndexOf("typedef struct _branch")
        Assert.True(leafPosition >= 0)
        Assert.True(branchPosition > leafPosition)

    [<Fact>]
    let ``self-referencing structure dependency is rejected`` () =
        use output = new TemporaryDirectory()
        use context = new Aqualis(Some output.Path, Some "self-cycle-body.c", C99)
        let structurePath = Path.Combine(output.Path, "self-cycle-structures.c")
        context.cstr.addstructure "node"
        context.cstr.addmember("node", (Structure "node", A0, "next"))
        let error =
            Assert.Throws<InvalidOperationException>(Action(fun () ->
                writeStructures context structurePath))
        Assert.Contains("Circular structure dependency detected: node -> node", error.Message)
        Assert.Equal("", File.ReadAllText structurePath)

    [<Fact>]
    let ``cycle in a disconnected structure component is rejected`` () =
        use output = new TemporaryDirectory()
        use context = new Aqualis(Some output.Path, Some "mutual-cycle-body.c", C99)
        let structurePath = Path.Combine(output.Path, "mutual-cycle-structures.c")
        context.cstr.addstructure "independent"
        context.cstr.addmember("independent", (Dt, A0, "value"))
        context.cstr.addstructure "alpha"
        context.cstr.addstructure "beta"
        context.cstr.addmember("alpha", (Structure "beta", A0, "beta"))
        context.cstr.addmember("beta", (Structure "alpha", A0, "alpha"))
        let error =
            Assert.Throws<InvalidOperationException>(Action(fun () ->
                writeStructures context structurePath))
        Assert.StartsWith("Circular structure dependency detected:", error.Message)
        Assert.Contains("alpha", error.Message)
        Assert.Contains("beta", error.Message)
        Assert.Equal("", File.ReadAllText structurePath)
