//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    /// Mutable sequence-diagram state owned by one generation context.
    type internal SequenceDiagramState() =
        /// Gets or sets the endpoint of the terminal lifeline.
        member val TerminalLifeLine = 100.0 with get, set
        /// Gets or sets variables in their diagram registration order.
        member val Variables : (string * int * float) list = [] with get, set
        /// Gets or sets the frame stack, with the innermost frame first.
        member val Frames : (float * float * float * float) list = [] with get, set
        /// Gets or sets the branch-label stack, with the innermost group first.
        member val Branches : ((string * float) list) list = [] with get, set

        /// Registers a variable at the current terminal and returns its diagram index.
        member this.RegisterVariable(name:string) =
            let index = this.Variables.Length
            this.Variables <- this.Variables @ [name, index, this.TerminalLifeLine]
            index

        /// Updates a registered variable's lifeline without changing registration order.
        member this.SetVariableLifeLine(variableName:string, endpoint:float) =
            this.Variables <-
                this.Variables |> List.map (fun (name, number, previous) ->
                    if name = variableName then name, number, endpoint
                    else name, number, previous)

        /// Advances the terminal after the corresponding lines have been drawn.
        member this.AdvanceLifeLine(gap:float) =
            this.TerminalLifeLine <- this.TerminalLifeLine + gap

        /// Expands open frames using the existing zero-sentinel and endpoint rules.
        member this.IncludeActiveLine(x:float, y1:float, y2:float) =
            this.Frames <-
                this.Frames |> List.map (fun (xMin,xMax,yMin,yMax) ->
                    let xMin' =
                        if xMin = 0.0 then x
                        elif x < xMin then x
                        else xMin
                    let xMax' =
                        if xMax = 0.0 then x
                        elif x > xMax then x
                        else xMax
                    let yMax' =
                        if yMax = 0.0 then y2
                        elif y1 > yMax && y1 > y2 then y1
                        elif y2 > yMax && y2 > y1 then y2
                        else yMax
                    xMin',xMax',yMin,yMax')

        /// Opens a frame at the current terminal with the supplied top inset.
        member this.PushFrame(topInset:float) =
            this.Frames <- (0.0, 0.0, this.TerminalLifeLine - topInset, this.TerminalLifeLine) :: this.Frames

        /// Removes the innermost frame after it has been rendered.
        member this.PopFrame() =
            this.Frames <- this.Frames.Tail

        /// Extends the lower edge of every remaining open frame.
        member this.ExpandFramesBottom(gap:float) =
            this.Frames <-
                this.Frames |> List.map (fun (xmin,xmax,ymin,ymax) -> xmin,xmax,ymin,ymax + gap)

        /// Opens a branch-label group for the next conditional frame.
        member this.PushBranchGroup() =
            this.Branches <- [] :: this.Branches

        /// Appends a label at the current terminal to the innermost branch group.
        member this.AppendBranchLabel(label:string) =
            this.Branches <- (this.Branches.Head @ [label, this.TerminalLifeLine]) :: this.Branches.Tail

        /// Removes the innermost branch group after its labels have been rendered.
        member this.PopBranchGroup() =
            this.Branches <- this.Branches.Tail
