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
wr.WriteLine "Compile [Fortran;C99;Python] outputdir projectname (\"aaa\",\"aaa\") <| fun () ->"
wr.WriteLine "    io.fileOutput \"result.dat\" <| fun wr ->"
wr.WriteLine "    ch.dddd <| fun (x,y,z1,z2) ->"
wr.WriteLine("        let p = " + r.Next(9).ToString() + "." + r.Next(9).ToString())
wr.WriteLine("        let q = " + r.Next(9).ToString() + "." + r.Next(9).ToString())
wr.WriteLine "        x <== p"
wr.WriteLine "        y <== q"
for i in 1..1000 do
    printfn "%d" i
    wr.WriteLine("        //printfn \"%d\" "+i.ToString())
    wr.WriteLine("        !\"test" + i.ToString "000" + "\"")
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
    wr.WriteLine("        //let z0 = " + eq)
    wr.WriteLine("        //printfn \"%d\" <| " + i.ToString())
    wr.WriteLine "        //printfn \"original:\""
    wr.WriteLine "        //printfn \"%s\" <| z0.Expr.ToString()"
    wr.WriteLine "        //printfn \"simp:\""
    wr.WriteLine "        //printfn \"%s\" <| z0.Expr.simp.ToString()"
    if eq.Contains("x") || eq.Contains("y") then
        wr.WriteLine("        let s = (" + eq + ").Expr.eval (programList[prIndex])")
        wr.WriteLine "        if (not <| s.ToString().Contains(\"NaN\")) && (not <| s.ToString().Contains(\"∞\")) then"
        wr.WriteLine("            z1 <== " + eq.Replace("x","p").Replace("y","q"))
        wr.WriteLine("            z2 <== " + eq)
        wr.WriteLine("            wr [I " + i.ToString() + "; z1; z2; asm.abs(z1-z2);]")
wr.Close()
