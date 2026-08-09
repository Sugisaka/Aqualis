//Aqualis用ランダム数式生成

open System
open System.IO

let r = Random 8201
let wr = new StreamWriter(__SOURCE_DIRECTORY__ + @"\test1.fsx")

wr.WriteLine "//#############################################################################"
wr.WriteLine "// Aqualis数式処理テスト"
wr.WriteLine "let projectname = \"test1\""
wr.WriteLine "let version = \"1.0.0\""
wr.WriteLine "//#############################################################################"
wr.WriteLine ""
wr.WriteLine "let outputdir = __SOURCE_DIRECTORY__"
wr.WriteLine ""
wr.WriteLine "#I @\"..\\..\\bin\\Debug\\net10.0\""
wr.WriteLine "#r \"Aqualis.dll\""
wr.WriteLine ""
wr.WriteLine "open Aqualis"
wr.WriteLine ""
wr.WriteLine "Compile [Fortran;C99;Python] outputdir projectname \"aaa\" <| fun ctx ->"
wr.WriteLine "    ctx.io.fileOutput \"result.dat\" <| fun wr ->"
wr.WriteLine "    ctx.ch.D \"x\" <| fun p ->"
wr.WriteLine "    ctx.ch.D \"y\" <| fun q ->"
wr.WriteLine "    ctx.ch.d <| fun z1 ->"
wr.WriteLine "    ctx.ch.d <| fun z2 ->"
wr.WriteLine("        let x = D " + r.Next(9).ToString() + "." + r.Next(9).ToString())
wr.WriteLine("        let y = D " + r.Next(9).ToString() + "." + r.Next(9).ToString())
wr.WriteLine "        p <== x"
wr.WriteLine "        q <== y"
for i in 1..1000 do
    printfn "%d" i
    wr.WriteLine("        //printfn \"%d\" "+i.ToString())
    wr.WriteLine("        ctx.print.s \"test" + i.ToString "000" + "\"")
    let rec maketerm(n:int) =
        match if n=0 then 0 else r.Next 2 with
        |0 ->
            match r.Next 2 with
            |0 ->
                match r.Next 2 with
                |0 -> r.Next(9).ToString() + "." + r.Next(9).ToString()
                |_ -> "(-" + r.Next(9).ToString() + "." + r.Next(9).ToString() + ")"
            |_ ->
                match r.Next 4 with
                |0 -> "x"
                |1 -> "(-x)"
                |2 -> "y"
                |_ -> "(-y)"
        |_ ->
            let mutable eq = ""
            let m = r.Next 5
            for _ in 0..r.Next 5 do
                let op =
                    match r.Next 4 with
                    |0 -> "+"
                    |1 -> "-"
                    |2 -> "*"
                    |_ -> "/"
                eq <- (if eq="" then eq else eq + op) + maketerm(n-1)
            if m=0 then eq else "("+eq+")"
    let eq = maketerm 3
    wr.WriteLine("        //equation: " + eq)
    if eq.Contains "x" || eq.Contains "y" then
        wr.WriteLine("        let s = (" + eq + ").Expr.simp.eval()")
        wr.WriteLine "        if s.ToString().Contains(\"NaN\") then"
        wr.WriteLine "            ctx.print.s \"NaN\""
        wr.WriteLine "        elif s.ToString().Contains(\"∞\") then"
        wr.WriteLine "            ctx.print.s \"Infinity\""
        wr.WriteLine "        else"
        wr.WriteLine("            z1 <== " + eq.Replace("x","p").Replace("y","q"))
        wr.WriteLine("            z2 <== " + eq)
        wr.WriteLine("            wr.tt <| (I " + i.ToString() + ")++z1++z2++asm.abs(z1-z2)")
wr.Close()
