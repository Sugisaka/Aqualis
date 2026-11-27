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
