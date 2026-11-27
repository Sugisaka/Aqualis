namespace Aqualis.Tests

open System.IO
open System.Reflection
open Xunit
open Aqualis

module SequenceDiagramStateTests =
    let private assertDefaults (context:Aqualis) =
        Assert.Equal(100.0, context.TerminalLifeLine)
        Assert.Empty(context.SequenceVariables)
        Assert.Empty(context.SequenceFrames)
        Assert.Empty(context.FrameStack)
        Assert.Empty(context.SequenceBranches)
        Assert.Empty(context.BranchStack)

    [<Fact>]
    let ``diagram state defaults are preserved for every context language`` () =
        for language in [Fortran; C99; Python; JavaScript; PHP; LaTeX; HTML; HTMLSequenceDiagram; Numeric] do
            use context = Aqualis.BlankWriter language
            assertDefaults context

    [<Fact>]
    let ``diagram property aliases stay synchronized and contexts remain independent`` () =
        use first = Aqualis.BlankWriter HTMLSequenceDiagram
        use second = Aqualis.BlankWriter HTMLSequenceDiagram
        let variables = ["value", 0, 125.0]
        let frames = [1.0, 2.0, 3.0, 4.0]
        let otherFrames = [5.0, 6.0, 7.0, 8.0]
        let branches = [["condition", 130.0]]
        let otherBranches = [["else", 160.0]]
        first.TerminalLifeLine <- 200.0
        first.SequenceVariables <- variables
        first.SequenceFrames <- frames
        Assert.Same(frames, first.FrameStack)
        first.FrameStack <- otherFrames
        Assert.Same(otherFrames, first.SequenceFrames)
        first.SequenceBranches <- branches
        Assert.Same(branches, first.BranchStack)
        first.BranchStack <- otherBranches
        Assert.Same(otherBranches, first.SequenceBranches)
        Assert.Equal(200.0, first.TerminalLifeLine)
        Assert.Same(variables, first.SequenceVariables)
        assertDefaults second
        first.FrameStack <- []
        first.BranchStack <- []
        Assert.Empty(first.SequenceFrames)
        Assert.Empty(first.SequenceBranches)

    [<Fact>]
    let ``nested diagrams preserve generated output final state and public API`` () =
        use output = new TemporaryDirectory()
        let mutable finalState = ""
        Compile [HTMLSequenceDiagram] output.Path "nested-sequence" "1.0" (fun context ->
            let value = context.var.i0 "value"
            let result = context.var.i0 "result"
            value <== 1
            result <== value + 2
            expr.sectionHS(context, "nested section") (fun () ->
                context.iter.range(0, 2) (fun index ->
                    context.br.branch (fun branch ->
                        branch.IF (value .> 0) (fun () ->
                            context.iter.range(0, 1) (fun inner ->
                                result <== value + index + inner))
                        branch.EL (fun () -> result <== value - index))))
            value <== result
            finalState <- sprintf "%A" (context.TerminalLifeLine, context.SequenceVariables, context.SequenceFrames, context.SequenceBranches))
        let generated = File.ReadAllText(Path.Combine(output.Path, "nested-sequence.html")).Replace("\r\n", "\n")
        let api =
            typeof<Aqualis>.GetMembers(BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.Static ||| BindingFlags.DeclaredOnly)
            |> Array.map string |> Array.sort |> String.concat "\n"
        let actual = generated + "\n--- state ---\n" + finalState + "\n--- API ---\n" + api
        let golden = TestHelpers.goldenPath "sequence" "nested-diagram.txt"
        Assert.Equal(File.ReadAllText(golden).Replace("\r\n", "\n"), actual.Replace("\r\n", "\n"))
