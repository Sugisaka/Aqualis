namespace Aqualis

/// Explicit code-grouping and section controls for one compilation environment.
type ContextGroup internal (environment:Aqualis) =
    let context() = environment.RequireGenerationContext()
    let program() = (context()).CurrentProgram
    let write line = (program()).codewritein(line + "\n")
    let emitComment line = (program()).comment line

    member _.section (label:string) code = code()
    member _.comment (text:string) = emitComment text
    member _.whenEnabled enabled code = if enabled then code()
    member this.whenSwitched (enabled:Switch) code = this.whenEnabled (enabled = ON) code
    member this.whenStep step id code = this.whenEnabled (step = id) code

    member _.Section (label:string) (code:unit -> unit) =
        let ctx = context()
        let current = ctx.CurrentProgram
        match current.language with
        |Fortran|C99|JavaScript ->
            emitComment ("===" + label.PadRight(76,'='))
            current.indentInc()
            try code() finally current.indentDec()
            emitComment ("=== end " + label.PadRight(76,'='))
            write ""
        |Python ->
            emitComment ("===" + label.PadRight(76,'='))
            code()
            emitComment ("=== end " + label.PadRight(76,'='))
            write ""
        |LaTeX ->
            write ("\\section{" + label + "}")
            code()
        |HTML ->
            write "<details open>"
            write ("<summary><span class=\"op-section\">section</span>" + label + "</summary>")
            write "<div class=\"insidecode-section\">"
            current.indentInc()
            try code() finally current.indentDec()
            write "</div>"
            write "</details>"
        |HTMLSequenceDiagram -> expr.sectionHS(current,label) code
        |Numeric|PHP -> code()

    member _.subSection (label:string) (code:unit -> unit) =
        let current = program()
        let header text =
            match current.language with
            |LaTeX -> write ("\\subsection{" + text + "}")
            |HTML ->
                write "<details open>"
                write ("<summary><span class=\"op-section\">section</span>" + text + "</summary>")
                write "<div class=\"insidecode-section\">"
            |_ -> emitComment ("---" + text.PadRight(76,'-'))
        header label
        if current.language <> Python then current.indentInc()
        try code() finally if current.language <> Python then current.indentDec()
        match current.language with
        |Fortran|C99 -> header ("end " + label); write ""
        |Python -> header ("end " + label)
        |HTML -> write "</div>"; write "</details>"
        |_ -> ()

    member private _.Header (marker:char) (label:string) =
        let current = program()
        match current.language with
        |Fortran|C99|Python|JavaScript|PHP ->
            emitComment (System.String(marker,3) + label.PadRight(76,marker))
        |LaTeX -> write ("\\section{" + label + "}")
        |HTML|HTMLSequenceDiagram ->
            write "<details open>"
            write ("<summary><span class=\"op-section\">section</span>" + label + "</summary>")
            write "<div class=\"insidecode-section\">"
        |Numeric -> ()

    member private _.Footer marker label =
        match (program()).language with
        |Fortran|C99|Python -> emitComment (System.String(marker,3) + ("end " + label).PadRight(76,marker))
        |HTML|HTMLSequenceDiagram -> write "</div>"; write "</details>"
        |_ -> ()

    member private this.Heading marker displayPrefix displaySuffix label code =
        let ctx = context()
        let current = ctx.CurrentProgram
        this.Header marker label
        if ctx.DisplaySection then environment.print.s (displayPrefix + label)
        if current.language = Python then code()
        else
            current.indentInc()
            try code() finally current.indentDec()
        if ctx.DisplaySection then environment.print.s (displaySuffix + label)
        this.Footer marker label
        write ""

    member this.h1 label code = this.Heading '#' "### " "### END " label code
    member this.h2 label code = this.Heading '%' "=== " "=== END " label code
    member this.h3 label code = this.Heading '=' "--- " "--- END " label code
    member this.h4 label code = this.Heading '+' "... " "... END " label code
    member this.h5 label code = this.Heading '-' "" "END " label code

[<AutoOpen>]
module CompilationEnvironmentGroupExtensions =
    type group =
        static member section (id1:int,id2:int) = fun code -> if id1=id2 then code() else ()
        static member section (id1:string,id2:string) = fun code -> if id1=id2 then code() else ()
        static member section (label:string) = fun code -> code()
    type dummy_group =
        static member section (id1:int,id2:int) = fun code -> ()
        static member section (id1:string,id2:string) = fun code -> ()
        static member section (label:string) = fun code -> ()
        
    type Aqualis with
        member this.group = ContextGroup(this)
