open System
open System.IO

type NI =
|N
|I

type Idx =
|All
|Range of NI*NI
|Fix of NI

let tname p = match p with |N -> "num0" |I -> "int"
let pattern = [Fix N;Fix I;Range (N,N);Range (N,I);Range (I,N);Range (I,I);All]

let wr0 = new StreamWriter("Item2.txt")
let wr1 = new StreamWriter("Idx2.txt")
for u in [true;false] do
    for p1 in pattern do
        for p2 in pattern do
            let setArg p a b i =
                match p with
                |All -> "_:unit"
                |Range(x,y) -> "("+a+":"+(tname x)+","+b+":"+(tname y)+")"
                |Fix x -> i+":"+(tname x)
            let setIdx p a b i =
                match p with
                |All -> i
                |Range _ -> i+"+"+a+"-1"
                |Fix N -> i
                |Fix I -> i+".I"
            let arg1 = setArg p1 "a1" "b1" "i"
            let arg2 = setArg p2 "a2" "b2" "j"
            let setRange p a b i (n:int) =
                match p with
                |All -> "this.size"+n.ToString()+", "
                |Range _ -> b+"-"+a+"+_1, "
                |Fix _ -> ""
            let range1 = setRange p1 "a1" "b1" "i" 1
            let range2 = setRange p2 "a2" "b2" "j" 2
            let setLArg p a b i =
                match p with
                |All -> i
                |Range _ -> i
                |Fix _ -> ""
            let larg1 = setLArg p1 "a1" "b1" "i"
            let larg2 = setLArg p2 "a2" "b2" "j"
            let setRef p a b i =
                match p with
                |All -> "()"
                |Range (x,y) -> "("+(match x with N -> "" |I -> "I ")+a+","+(match y with N -> "" |I -> "I ")+b+")"
                |Fix x -> (match x with N -> "" |I -> "I ")+i
            let ref1 = setRef p1 "a1" "b1" "i"
            let ref2 = setRef p2 "a2" "b2" "j"
            let f = 
                let idx1 = setIdx p1 "a1" "b1" "i"
                let idx2 = setIdx p2 "a2" "b2" "j"
                "this.Idx2("+idx1+","+idx2+")"
            let cnt = 
                let mutable c = 0
                for p in [p1;p2] do
                    match p with |Fix _ -> () |_ -> c <- c+1
                c
            let code =
                if cnt = 0 then
                    f
                elif cnt = 1 then
                    "Arx"+cnt.ToString()+"("+range1+range2+" fun "+larg1+larg2+" -> "+f+")"
                else
                    let larg = 
                        let y = "("+larg1+","+larg2+")"
                        y.Replace(",,",",").Replace(",)",")").Replace("(,","(")
                    "Arx"+cnt.ToString()+"("+range1+range2+" fun "+larg+" -> "+f+")"
            if u && cnt=0 then
                wr1.WriteLine("        member this.Idx2("+arg1+","+arg2+") = "+code)
                wr0.WriteLine("        member this.Item with get("+arg1+","+arg2+") = this.Idx2("+ref1+","+ref2+")")
            if not u && cnt<>0 then
                wr1.WriteLine("        member this.Idx2("+arg1+","+arg2+") = "+code)
                wr0.WriteLine("        member this.Item with get("+arg1+","+arg2+") = num"+cnt.ToString()+"(typ,this.Idx2("+ref1+","+ref2+"))")
wr0.Close()
wr1.Close()