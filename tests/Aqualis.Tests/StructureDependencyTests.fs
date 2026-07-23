namespace Aqualis.Tests

open System
open System.IO
open Xunit
open Aqualis

module StructureDependencyTests =
    let private createContext path filename =
        GenerationContext [new program(path, filename, C99)]

    let private writeStructures (context:GenerationContext) path =
        use writer = new codeWriter(path, 2, C99)
        Aqualis(Some context).str.Def_Structure writer
        writer.close()

    [<Fact>]
    let ``structure dependencies are emitted before their users`` () =
        use output = new TemporaryDirectory()
        let context = createContext output.Path "acyclic-body.c"
        let structurePath = Path.Combine(output.Path, "acyclic-structures.c")

        try
            context.GenerateAtomically(fun _ ->
                context.CurrentProgram.str.addstructure "leaf"
                context.CurrentProgram.str.addmember(
                    "leaf",
                    (Dt, A0, "value"))
                context.CurrentProgram.str.addstructure "branch"
                context.CurrentProgram.str.addmember(
                    "branch",
                    (Structure "leaf", A0, "leaf"))
                writeStructures context structurePath)
        finally
            context.CurrentProgram.close()

        let generated = File.ReadAllText structurePath
        let leafPosition = generated.IndexOf("typedef struct _leaf")
        let branchPosition = generated.IndexOf("typedef struct _branch")

        Assert.True(leafPosition >= 0)
        Assert.True(branchPosition > leafPosition)

    [<Fact>]
    let ``self-referencing structure dependency is rejected`` () =
        use output = new TemporaryDirectory()
        let context = createContext output.Path "self-cycle-body.c"
        let structurePath = Path.Combine(output.Path, "self-cycle-structures.c")

        let error =
            try
                Assert.Throws<InvalidOperationException>(Action(fun () ->
                    context.GenerateAtomically(fun _ ->
                        context.CurrentProgram.str.addstructure "node"
                        context.CurrentProgram.str.addmember(
                            "node",
                            (Structure "node", A0, "next"))
                        writeStructures context structurePath)))
            finally
                context.CurrentProgram.close()

        Assert.Contains(
            "Circular structure dependency detected: node -> node",
            error.Message)
        Assert.Equal("", File.ReadAllText structurePath)

    [<Fact>]
    let ``cycle in a disconnected structure component is rejected`` () =
        use output = new TemporaryDirectory()
        let context = createContext output.Path "mutual-cycle-body.c"
        let structurePath = Path.Combine(output.Path, "mutual-cycle-structures.c")

        let error =
            try
                Assert.Throws<InvalidOperationException>(Action(fun () ->
                    context.GenerateAtomically(fun _ ->
                        context.CurrentProgram.str.addstructure "independent"
                        context.CurrentProgram.str.addmember(
                            "independent",
                            (Dt, A0, "value"))
                        context.CurrentProgram.str.addstructure "alpha"
                        context.CurrentProgram.str.addstructure "beta"
                        context.CurrentProgram.str.addmember(
                            "alpha",
                            (Structure "beta", A0, "beta"))
                        context.CurrentProgram.str.addmember(
                            "beta",
                            (Structure "alpha", A0, "alpha"))
                        writeStructures context structurePath)))
            finally
                context.CurrentProgram.close()

        Assert.StartsWith(
            "Circular structure dependency detected:",
            error.Message)
        Assert.Contains("alpha", error.Message)
        Assert.Contains("beta", error.Message)
        Assert.Equal("", File.ReadAllText structurePath)
