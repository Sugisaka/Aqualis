// 
// Copyright (c) 2026 Jun-ichiro Sugisaka
// 
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
// 
namespace Aqualis

open System
open System.IO

/// Selector label for generated CSS rules.
type CSSLabel = 
    |HTMLTag of string
    |HTMLState of string
    |CSSClass of string
    |CSSID of string
    /// Gets the selector name without its CSS prefix.
    member this.name with get() =
        match this with
        |HTMLTag x -> x
        |HTMLState x -> x
        |CSSClass x -> x
        |CSSID x -> x
    /// Gets the selector with its CSS prefix.
    member this.header with get() =
        match this with
        |HTMLTag x -> x
        |HTMLState x -> ":"+x
        |CSSClass x -> "."+x
        |CSSID x -> "#"+x
        
/// CSS selector, declarations, and nested selectors.
type CSSdata(lab:CSSLabel,s:Style,substyle:list<CSSdata>) =
    new(lab:CSSLabel) = CSSdata(lab,Style[],[])
    new(lab:CSSLabel,s:Style) = CSSdata(lab,s,[])
    /// Gets the selector label.
    member _.label with get() = lab
    /// Gets the CSS declarations.
    member _.style with get() = s
    /// Gets nested selector data.
    member _.subStyle with get() = substyle
    /// Creates a copy with replacement declarations and nested selectors.
    member _.alter(s:Style,substyle:list<CSSdata>) = CSSdata(lab,s,substyle)
    /// Creates a copy with replacement declarations.
    member _.alter(s:Style) = CSSdata(lab,s)
    
/// Writes CSS rules to a file.
type CSSFile(outputdir:string,filename:string) =
    let wr = new StreamWriter(Path.Combine(outputdir, filename))
    /// Writes a CSS selector and its nested rules.
    member this.add (x:CSSdata) =
        let rec write (header:string) (x:CSSdata) =
            let header1 = 
                if header = "" then
                    x.label.header
                else
                    header + (match x.label with |HTMLState _ -> "" |_ -> " ") + x.label.header
            if x.style.list.Length>0 then
                wr.WriteLine (header1 + " {")
                for s in x.style.list do
                    wr.WriteLine("\t" + s.Key + ": " + s.Value + ";")
                wr.WriteLine "}"
                wr.WriteLine ""
            for y in x.subStyle do
                write header1 y
        write "" x
    /// Writes CSS rules inside a media query.
    member this.mediaQueries (cond:list<string>) code =
        wr.WriteLine ("@media " + String.Join(" and ", cond |> List.map (fun t -> "("+t+")")) + "{")
        code()
        wr.WriteLine "}"
    /// Closes the CSS output stream.
    member this.close() =
        wr.Dispose()

    interface IDisposable with
        /// Disposes the CSS output stream.
        member _.Dispose() =
            wr.Dispose()

    /// Writes a CSS file through a callback and publishes it atomically.
    static member make (outputdir:string) (filename:string) code =
        let output =
            Path.Combine(outputdir, filename)
            |> AtomicOutputFile.create
        try
            do
                use c =
                    new CSSFile(
                        Path.GetDirectoryName(output.StagingPath),
                        Path.GetFileName(output.StagingPath))
                code c
            AtomicOutputFile.publish output
        with _ ->
            AtomicOutputFile.discard output
            reraise()
