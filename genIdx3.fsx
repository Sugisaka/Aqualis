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

let wr0 = new StreamWriter("Item3.txt")
let wr1 = new StreamWriter("Idx3.txt")
for u in [true;false] do
    for p1 in pattern do
        for p2 in pattern do
            for p3 in pattern do
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
                let arg3 = setArg p3 "a3" "b3" "k"
                let setRange p a b i (n:int) =
                    match p with
                    |All -> "this.size"+n.ToString()+", "
                    |Range _ -> b+"-"+a+"+_1, "
                    |Fix _ -> ""
                let range1 = setRange p1 "a1" "b1" "i" 1
                let range2 = setRange p2 "a2" "b2" "j" 2
                let range3 = setRange p3 "a3" "b3" "k" 3
                let setLArg p a b i =
                    match p with
                    |All -> i
                    |Range _ -> i
                    |Fix _ -> ""
                let larg1 = setLArg p1 "a1" "b1" "i"
                let larg2 = setLArg p2 "a2" "b2" "j"
                let larg3 = setLArg p3 "a3" "b3" "k"
                let setRef p a b i =
                    match p with
                    |All -> "()"
                    |Range (x,y) -> "("+(match x with N -> "" |I -> "I ")+a+","+(match y with N -> "" |I -> "I ")+b+")"
                    |Fix x -> (match x with N -> "" |I -> "I ")+i
                let ref1 = setRef p1 "a1" "b1" "i"
                let ref2 = setRef p2 "a2" "b2" "j"
                let ref3 = setRef p3 "a3" "b3" "k"
                let f = 
                    let idx1 = setIdx p1 "a1" "b1" "i"
                    let idx2 = setIdx p2 "a2" "b2" "j"
                    let idx3 = setIdx p3 "a3" "b3" "k"
                    "this.Idx3("+idx1+","+idx2+","+idx3+")"
                let cnt = 
                    let mutable c = 0
                    for p in [p1;p2;p3] do
                        match p with |Fix _ -> () |_ -> c <- c+1
                    c
                let code =
                    if cnt = 0 then
                        f
                    elif cnt = 1 then
                        "Arx"+cnt.ToString()+"("+range1+range2+range3+" fun "+larg1+larg2+larg3+" -> "+f+")"
                    else
                        let larg = 
                            let y = "("+larg1+","+larg2+","+larg3+")"
                            y.Replace(",,",",").Replace(",)",")").Replace("(,","(")
                        "Arx"+cnt.ToString()+"("+range1+range2+range3+" fun "+larg+" -> "+f+")"
                if u && cnt=0 then
                    wr1.WriteLine("        member this.Idx3("+arg1+","+arg2+","+arg3+") = "+code)
                    wr0.WriteLine("        member this.Item with get("+arg1+","+arg2+","+arg3+") = this.Idx3("+ref1+","+ref2+","+ref3+")")
                if not u && cnt<>0 then
                    wr1.WriteLine("        member this.Idx3("+arg1+","+arg2+","+arg3+") = "+code)
                    wr0.WriteLine("        member this.Item with get("+arg1+","+arg2+","+arg3+") = num"+cnt.ToString()+"(typ,this.Idx3("+ref1+","+ref2+","+ref3+"))")
wr0.Close()
wr1.Close()