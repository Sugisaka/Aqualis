namespace Aqualis

/// Groups generated code into labeled sections.
type ContextGroup internal (ctx:Aqualis) =
    let write line = ctx.codewritein(line + "\n")
    let emitComment line = ctx.comment line

    /// Runs a group callback without emitting a heading.
    member _.section (label:string) code = code()
    /// Writes a target-language comment.
    member _.comment (text:string) = emitComment text
    /// Runs the callback when enabled.
    member _.whenEnabled enabled code = if enabled then code()
    /// Runs the callback when the switch is on.
    member this.whenSwitched (enabled:Switch) code = this.whenEnabled (enabled = ON) code
    /// Runs the callback when the selected step matches the identifier.
    member this.whenStep step id code = this.whenEnabled (step = id) code

    /// Wraps generated code in a labeled section for the target language.
    member _.Section (label:string) (code:unit -> unit) =
        match ctx.language with
        |Fortran|C99|JavaScript ->
            emitComment ("===" + label.PadRight(76,'='))
            ctx.indentInc()
            try code() finally ctx.indentDec()
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
            write ("<summary><span class=\"op-section\">section</span>" + HtmlEncoding.textContent label + "</summary>")
            write "<div class=\"insidecode-section\">"
            ctx.indentInc()
            try code() finally ctx.indentDec()
            write "</div>"
            write "</details>"
        |HTMLSequenceDiagram -> expr.sectionHS(ctx,label) code
        |Numeric|PHP -> code()

    /// Wraps generated code in a labeled subsection.
    member _.subSection (label:string) (code:unit -> unit) =
        let header text =
            match ctx.language with
            |LaTeX -> write ("\\subsection{" + text + "}")
            |HTML ->
                write "<details open>"
                write ("<summary><span class=\"op-section\">section</span>" + HtmlEncoding.textContent text + "</summary>")
                write "<div class=\"insidecode-section\">"
            |_ -> emitComment ("---" + text.PadRight(76,'-'))
        header label
        if ctx.language <> Python then ctx.indentInc()
        try code() finally if ctx.language <> Python then ctx.indentDec()
        match ctx.language with
        |Fortran|C99 -> header ("end " + label); write ""
        |Python -> header ("end " + label)
        |HTML -> write "</div>"; write "</details>"
        |_ -> ()

    /// Writes the opening heading for a code group.
    member private _.Header (marker:char) (label:string) =
        match ctx.language with
        |Fortran|C99|Python|JavaScript|PHP ->
            emitComment (System.String(marker,3) + label.PadRight(76,marker))
        |LaTeX -> write ("\\section{" + label + "}")
        |HTML|HTMLSequenceDiagram ->
            write "<details open>"
            write ("<summary><span class=\"op-section\">section</span>" + HtmlEncoding.textContent label + "</summary>")
            write "<div class=\"insidecode-section\">"
        |Numeric -> ()

    /// Writes the closing heading for a code group.
    member private _.Footer marker label =
        match ctx.language with
        |Fortran|C99|Python -> emitComment (System.String(marker,3) + ("end " + label).PadRight(76,marker))
        |HTML|HTMLSequenceDiagram -> write "</div>"; write "</details>"
        |_ -> ()

    /// Wraps generated code in a styled heading and footer.
    member private this.Heading marker displayPrefix displaySuffix label code =
        this.Header marker label
        if ctx.DisplaySection then ctx.print.s (displayPrefix + label)
        if ctx.language = Python then code()
        else
            ctx.indentInc()
            try code() finally ctx.indentDec()
        if ctx.DisplaySection then ctx.print.s (displaySuffix + label)
        this.Footer marker label
        write ""

    /// Writes a level-1 labeled heading around generated code.
    member this.h1 label code = this.Heading '#' "### " "### END " label code
    /// Writes a level-2 labeled heading around generated code.
    member this.h2 label code = this.Heading '%' "=== " "=== END " label code
    /// Writes a level-3 labeled heading around generated code.
    member this.h3 label code = this.Heading '=' "--- " "--- END " label code
    /// Writes a level-4 labeled heading around generated code.
    member this.h4 label code = this.Heading '+' "... " "... END " label code
    /// Writes a level-5 labeled heading around generated code.
    member this.h5 label code = this.Heading '-' "" "END " label code

/// Adds grouped code-generation helpers to Aqualis.
[<AutoOpen>]
module CompilationEnvironmentGroupExtensions =
    /// Conditional group helpers that run matching sections.
    type group =
        /// Runs the callback when this section is selected.
        static member section (id1:int,id2:int) = fun code -> if id1=id2 then code() else ()
        /// Runs the callback when this section is selected.
        static member section (id1:string,id2:string) = fun code -> if id1=id2 then code() else ()
        /// Runs the callback when this section is selected.
        static member section (label:string) = fun code -> code()
    /// Group helpers that suppress all section callbacks.
    type dummy_group =
        /// Suppresses the callback.
        static member section (id1:int,id2:int) = fun code -> ()
        /// Suppresses the callback.
        static member section (id1:string,id2:string) = fun code -> ()
        /// Suppresses the callback.
        static member section (label:string) = fun code -> ()
        
    type Aqualis with
        ///<summary>コードグループ</summary>
        member this.group = ContextGroup(this)
