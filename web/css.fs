// 
// Copyright (c) 2026 Jun-ichiro Sugisaka
// 
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
// 
namespace Aqualis

open System
open System.IO

type CSSLabel = 
    |HTMLTag of string
    |HTMLState of string
    |CSSClass of string
    |CSSID of string
    member this.name with get() =
        match this with
        |HTMLTag x -> x
        |HTMLState x -> x
        |CSSClass x -> x
        |CSSID x -> x
    member this.header with get() =
        match this with
        |HTMLTag x -> x
        |HTMLState x -> ":"+x
        |CSSClass x -> "."+x
        |CSSID x -> "#"+x
        
type CSSdata(lab:CSSLabel,s:Style,substyle:list<CSSdata>) =
    new(lab:CSSLabel) = CSSdata(lab,Style[],[])
    new(lab:CSSLabel,s:Style) = CSSdata(lab,s,[])
    member _.label with get() = lab
    member _.style with get() = s
    member _.subStyle with get() = substyle
    member _.alter(s:Style,substyle:list<CSSdata>) = CSSdata(lab,s,substyle)
    member _.alter(s:Style) = CSSdata(lab,s)
    
type CSSFile(outputdir:string,filename:string) =
    let wr = new StreamWriter(outputdir+"\\"+filename)
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
    member this.mediaQueries (cond:list<string>) code =
        wr.WriteLine ("@media " + String.Join(" and ", cond |> List.map (fun t -> "("+t+")")) + "{")
        code()
        wr.WriteLine "}"
    member this.close() =
        wr.Close()
    static member make (outputdir:string) (filename:string) code =
        let c = CSSFile(outputdir,filename)
        code c
        c.close()
