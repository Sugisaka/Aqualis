//Aqualisテスト

open System
open System.IO

let r = Random(9876)
let wr = new StreamWriter(__SOURCE_DIRECTORY__ + @"\test1.fsx")

wr.WriteLine("//#############################################################################")
wr.WriteLine("// Aqualisテスト")
wr.WriteLine("// 実行後removeNaNを実行し非数値数式をコメントアウト")
wr.WriteLine("let projectname = \"test1\"")
wr.WriteLine("let version = \"1.0.0\"")
wr.WriteLine("//#############################################################################")
wr.WriteLine("")
wr.WriteLine("let outputdir = __SOURCE_DIRECTORY__")
wr.WriteLine("")
wr.WriteLine("#I @\"C:\\home\\LightwaveLaboratory\\Aqualis\\bin\\Debug\\net6.0\"")
wr.WriteLine("#r \"Aqualis.dll\"")
wr.WriteLine("")
wr.WriteLine("open Aqualis")
wr.WriteLine("")
wr.WriteLine("Compile [F;C] outputdir projectname (\"aaa\",\"aaa\") <| fun () ->")
wr.WriteLine("    ch.dddd <| fun (x,y,z1,z2) ->")
wr.WriteLine("        let p = "+r.Next(9).ToString()+"."+r.Next(9).ToString())
wr.WriteLine("        let q = "+r.Next(9).ToString()+"."+r.Next(9).ToString())
wr.WriteLine("        x <== p")
wr.WriteLine("        y <== q")
for i in 1..5000 do
    printfn "%d" i
    wr.WriteLine("        printfn \"%d\" "+i.ToString())
    wr.WriteLine("        !\"test"+i.ToString("000")+"\"")
    let rec maketerm(n:int) =
        match (if n=0 then 0 else r.Next(2)) with
        |0 ->
            match r.Next(2) with
            |0 ->
                match r.Next(2) with
                |0 -> r.Next(9).ToString()+"."+r.Next(9).ToString()
                |_ -> "(-"+r.Next(9).ToString()+"."+r.Next(9).ToString()+")"
            |_ ->
                match r.Next(4) with
                |0 -> "x"
                |1 -> "(-x)"
                |2 -> "y"
                |_ -> "(-y)"
        |_ ->
            let mutable eq = ""
            let m = r.Next(5)
            for _ in 0..r.Next(5) do
                let op =
                    match r.Next(4) with
                    |0 -> "+"
                    |1 -> "-"
                    |2 -> "*"
                    |_ -> "/"
                eq <- (if eq="" then eq else eq + op) + maketerm(n-1)
            if m=0 then eq else "("+eq+")"
    let eq = maketerm(3)
    wr.WriteLine("        z1 <== "+eq.Replace("x","p").Replace("y","q"))
    wr.WriteLine("        z2 <== "+eq)
    wr.WriteLine("        print.cccc (I "+i.ToString("000")+") z1 z2 (asm.abs(z1-z2))")
wr.Close()
