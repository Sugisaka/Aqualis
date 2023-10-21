(*
Copyright (c) 2022 Jun-ichiro Sugisaka

This software is released under the MIT License.
http://opensource.org/licenses/mit-license.php
*)
namespace Aqualis
    
    open System
    open Aqualis_base
    
    ///<summary>変数オブジェクト</summary>
    type Expr =
        |Str_c of string
        |Int_c of int
        |Dbl_c of double
        |Var of string
        |Par of Expr
        |Inv of Expr
        |Add of Expr*Expr
        |Sub of Expr*Expr
        |Mul of Expr*Expr
        |Div of Expr*Expr
        |Formula of string
        |Eq of Expr*Expr
        |NEq of Expr*Expr
        |Greater of Expr*Expr
        |GreaterEq of Expr*Expr
        |Less of Expr*Expr
        |LessEq of Expr*Expr
        |AND of Expr list
        |OR of Expr list
        |Null
        |NaN
        
        member this.code with get() =
            match p.lang with
            |T ->
                match this with
                |Var x -> x
                |Int_c x -> p.ItoS(x)
                |Dbl_c x -> p.DtoS(x)
                |Str_c x -> "\""+x+"\""
                |Par x -> "\\left("+x.code+"\\right)"
                |Inv x -> "-"+x.code
                |Add (x,y) -> x.code+"+"+y.code
                |Sub (x,y) -> x.code+"-"+y.code
                |Mul (x,y) -> x.code+" \\cdot "+y.code
                |Div (Par x,Par y) -> "\\frac{\\displaystyle "+x.code+"}{\\displaystyle "+y.code+"}"
                |Div (Par x,y) -> "\\frac{\\displaystyle "+x.code+"}{\\displaystyle "+y.code+"}"
                |Div (x,Par y) -> "\\frac{\\displaystyle "+x.code+"}{\\displaystyle "+y.code+"}"
                |Div (x,y) -> "\\frac{\\displaystyle "+x.code+"}{\\displaystyle "+y.code+"}"
                |NaN -> "NaN"
                |AND [Less(v1,v2);Less(v2B,v3)] when v2.code=v2B.code ->
                    v1.code+" < "+v2.code+" < "+v3.code
                |AND [LessEq(v1,v2);Less(v2B,v3)] when v2.code=v2B.code ->
                    v1.code+" \\leq "+v2.code+" < "+v3.code
                |AND [Less(v1,v2);LessEq(v2B,v3)] when v2.code=v2B.code ->
                    v1.code+" < "+v2.code+" \\leq "+v3.code
                |AND [LessEq(v1,v2);LessEq(v2B,v3)] when v2.code=v2B.code ->
                    v1.code+" \\leq "+v2.code+" \\leq "+v3.code
                |Eq(v1,v2) ->
                    v1.code+" == "+v2.code
                |NEq(v1,v2) ->
                    v1.code+" \\neq "+v2.code
                |Greater(v1,v2) ->
                    v1.code+" > "+v2.code
                |GreaterEq(v1,v2) ->
                    v1.code+" \\geq "+v2.code
                |Less(v1,v2) ->
                    v1.code+" < "+v2.code
                |LessEq(v1,v2) ->
                    v1.code+" \\leq "+v2.code
                |AND(v) ->
                    //先に中身を評価
                    let uc = v |> List.map (fun q -> q.code)
                    let cat acc i =
                        if i=0 then
                            acc + "(" + uc[i] + ")"
                        else
                            acc + " \\cap " + "(" + uc[i] + ")" 
                    let code = List.fold cat "" [0..uc.Length-1]
                    //コード生成
                    code
                |OR(v) ->
                    //先に中身を評価
                    let uc = v |> List.map (fun q -> q.code)
                    let cat acc i =
                        if i=0 then
                            acc + "(" + uc[i] + ")"
                        else
                            acc + " \\cup " + "(" + uc[i] + ")"
                    let code = List.fold cat "" [0..uc.Length-1]
                    //コード生成
                    code
                |Formula s -> s
                |_ ->
                    ""
            |H ->
                match this with
                |Var x -> x
                |Int_c x -> p.ItoS(x)
                |Dbl_c x -> p.DtoS(x)
                |Str_c x -> "\""+x+"\""
                |Par x -> "\\left("+x.code+"\\right)"
                |Inv x -> "-"+x.code
                |Add (x,y) -> x.code+"+"+y.code
                |Sub (x,y) -> x.code+"-"+y.code
                |Mul (x,y) -> x.code+" \\cdot "+y.code
                |Div (Par x,Par y) -> "\\frac{\\displaystyle "+x.code+"}{\\displaystyle "+y.code+"}"
                |Div (Par x,y) -> "\\frac{\\displaystyle "+x.code+"}{\\displaystyle "+y.code+"}"
                |Div (x,Par y) -> "\\frac{\\displaystyle "+x.code+"}{\\displaystyle "+y.code+"}"
                |Div (x,y) -> "\\frac{\\displaystyle "+x.code+"}{\\displaystyle "+y.code+"}"
                |NaN -> "NaN"
                |AND [Less(v1,v2);Less(v2B,v3)] when v2.code=v2B.code ->
                    v1.code+" < "+v2.code+" < "+v3.code
                |AND [LessEq(v1,v2);Less(v2B,v3)] when v2.code=v2B.code ->
                    v1.code+" \\leq "+v2.code+" < "+v3.code
                |AND [Less(v1,v2);LessEq(v2B,v3)] when v2.code=v2B.code ->
                    v1.code+" < "+v2.code+" \\leq "+v3.code
                |AND [LessEq(v1,v2);LessEq(v2B,v3)] when v2.code=v2B.code ->
                    v1.code+" \\leq "+v2.code+" \\leq "+v3.code
                |Eq(v1,v2) ->
                    v1.code+" == "+v2.code
                |NEq(v1,v2) ->
                    v1.code+" \\neq "+v2.code
                |Greater(v1,v2) ->
                    v1.code+" > "+v2.code
                |GreaterEq(v1,v2) ->
                    v1.code+" \\geq "+v2.code
                |Less(v1,v2) ->
                    v1.code+" < "+v2.code
                |LessEq(v1,v2) ->
                    v1.code+" \\leq "+v2.code
                |AND(v) ->
                    //先に中身を評価
                    let uc = v |> List.map (fun q -> q.code)
                    let cat acc i =
                        if i=0 then
                            acc + "(" + uc[i] + ")"
                        else
                            acc + " \\cap " + "(" + uc[i] + ")" 
                    let code = List.fold cat "" [0..uc.Length-1]
                    //コード生成
                    code
                |OR(v) ->
                    //先に中身を評価
                    let uc = v |> List.map (fun q -> q.code)
                    let cat acc i =
                        if i=0 then
                            acc + "(" + uc[i] + ")"
                        else
                            acc + " \\cup " + "(" + uc[i] + ")"
                    let code = List.fold cat "" [0..uc.Length-1]
                    //コード生成
                    code
                |Formula s -> s
                |_ ->
                    ""
            |F ->
                match this with
                |Var x -> x
                |Int_c x -> p.ItoS(x)
                |Dbl_c x -> p.DtoS(x)
                |Str_c x -> "\""+x+"\""
                |Par x -> "("+x.code+")"
                |Inv x -> "-"+x.code
                |Add (x,y) -> x.code+"+"+y.code
                |Sub (x,y) -> x.code+"-"+y.code
                |Mul (x,y) -> x.code+"*"+y.code
                |Div (x,y) -> x.code+"/"+y.code
                |NaN -> "NaN"
                |Eq(v1,v2) ->
                    v1.code+" == "+v2.code
                |NEq(v1,v2) ->
                    v1.code+" /= "+v2.code
                |Greater(v1,v2) ->
                    v1.code+" > "+v2.code
                |GreaterEq(v1,v2) ->
                    v1.code+" >= "+v2.code
                |Less(v1,v2) ->
                    v1.code+" < "+v2.code
                |LessEq(v1,v2) ->
                    v1.code+" <= "+v2.code
                |AND(v) ->
                    //先に中身を評価
                    let uc = v |> List.map (fun q -> q.code)
                    let cat acc i =
                        if i=0 then
                            acc + "(" + uc[i] + ")"
                        else
                            acc + " .and. " + "(" + uc[i] + ")"
                    let code = List.fold cat "" [0..uc.Length-1]
                    //コード生成
                    code
                |OR(v) ->
                    //先に中身を評価
                    let uc = v |> List.map (fun q -> q.code)
                    let cat acc i =
                        if i=0 then
                            acc + "(" + uc[i] + ")"
                        else
                            acc + " .or. " + "(" + uc[i] + ")"
                    let code = List.fold cat "" [0..uc.Length-1]
                    //コード生成
                    code
                |Formula s -> s
                |_ ->
                    ""
            |C ->
                match this with
                |Var x -> x
                |Int_c x -> p.ItoS(x)
                |Dbl_c x -> p.DtoS(x)
                |Str_c x -> "\""+x+"\""
                |Par x -> "("+x.code+")"
                |Inv x -> "-"+x.code
                |Add (x,y) -> x.code+"+"+y.code
                |Sub (x,y) -> x.code+"-"+y.code
                |Mul (x,y) -> x.code+"*"+y.code
                |Div (x,y) -> x.code+"/"+y.code
                |NaN -> "NaN"
                |Eq(v1,v2) ->
                    v1.code+" == "+v2.code
                |NEq(v1,v2) ->
                    v1.code+" != "+v2.code
                |Greater(v1,v2) ->
                    v1.code+" > "+v2.code
                |GreaterEq(v1,v2) ->
                    v1.code+" >= "+v2.code
                |Less(v1,v2) ->
                    v1.code+" < "+v2.code
                |LessEq(v1,v2) ->
                    v1.code+" <= "+v2.code
                |AND(v) ->
                    //先に中身を評価
                    let uc = v |> List.map (fun q -> q.code)
                    let cat acc i =
                        if i<>0 then
                            acc + " && " + "(" + uc[i] + ")"
                        else acc + "(" + uc[i] + ")"
                    let code = List.fold cat "" [0..uc.Length-1]
                    //コード生成
                    code
                |OR(v) ->
                    //先に中身を評価
                    let uc = v |> List.map (fun q -> q.code)
                    let cat acc i =
                        if i<>0 then
                            acc + " || " + "(" + uc[i] + ")"
                        else acc + "(" + uc[i] + ")"
                    let code = List.fold cat "" [0..uc.Length-1]
                    //コード生成
                    code
                |Formula s -> s
                |_ ->
                    ""
                    
        ///<summary>負号</summary>
        static member ( ~- ) (x:Expr) =
            match x with
            |Int_c 0|Dbl_c 0.0 -> x
            |Int_c x when x<0   -> (Int_c -x)
            |Dbl_c x when x<0.0 -> (Dbl_c -x)
            |Inv(v) -> v
            |Add _ -> Inv(Par x)
            |Sub(x,y) -> y-x
            |_ -> Inv(x)
              
        ///<summary>加算</summary>
        static member ( + ) (x:Expr,y:Expr) : Expr =
            if isEqSimplify then
                match x,y with
                (* x+0 *)
                |_,Int_c 0|_,Dbl_c 0.0 -> x
                (* 0+y *)
                |Int_c 0,_|Dbl_c 0.0,_ -> y
                (* x+x *)
                |v1,v2 when v1.code=v2.code -> (Int_c 2)*v1
                (* (-x)+y *)
                |Int_c x,_ when x<0   -> Inv(Int_c (-x)) + y
                (* (-x)+y *)
                |Dbl_c x,_ when x<0.0 -> Inv(Dbl_c (-x)) + y
                (* x+(-y) *)
                |_,Int_c y when y<0   -> x + Inv(Int_c (-y))
                (* x+(-y) *)
                |_,Dbl_c y when y<0.0 -> x + Inv(Dbl_c (-y))
                (* (-x)+(-y) *)
                |Inv(v1),Inv(v2) -> -(v1+v2)
                (* x+(-y) *)
                |_,Inv(v2) -> x-v2
                (* (-x)+y *)
                |Inv(v1),_ -> y-v1
                (* x+[整数定数] *)
                |_,Int_c v2 when v2<0   -> x-Int_c(-v2)
                (* x+[小数定数] *)
                |_,Dbl_c v2 when v2<0.0 -> x-Dbl_c(-v2)
                (* [整数定数]+[整数定数] *)
                |Int_c v1,Int_c v2 -> Int_c(v1+v2)
                (* [整数定数]+[小数定数] *)
                |Int_c v1,Dbl_c v2 -> Dbl_c((double v1)+v2)
                (* [小数定数]+[整数定数] *)
                |Dbl_c v1,Int_c v2 -> Dbl_c(v1+(double v2))
                (* [小数定数]+[小数定数] *)
                |Dbl_c v1,Dbl_c v2 -> Dbl_c(v1+v2)
                (* [整数定数]+Add([整数定数],[]) *)
                |Int_c v1,Add(Int_c v2,u2) -> (Int_c (v1+v2))+u2
                (* [小数定数]+Add([整数定数],[]) *)
                |Dbl_c v1,Add(Int_c v2,u2) -> (Dbl_c (v1+double v2))+u2
                (* [整数定数]+Add([小数定数],[]) *)
                |Int_c v1,Add(Dbl_c v2,u2) -> (Dbl_c (double v1+v2))+u2
                (* [小数定数]+Add([小数定数],[]) *)
                |Dbl_c v1,Add(Dbl_c v2,u2) -> (Dbl_c (v1+v2))+u2
                (* [整数定数]+Add([],[整数定数]) *)
                |Int_c v1,Add(u2,Int_c v2) -> u2+(Int_c (v1+v2))
                (* [小数定数]+Add([],[整数定数]) *)
                |Dbl_c v1,Add(u2,Int_c v2) -> u2+(Dbl_c (v1+double v2))
                (* [整数定数]+Add([],[小数定数]) *)
                |Int_c v1,Add(u2,Dbl_c v2) -> u2+(Dbl_c (double v1+v2))
                (* [小数定数]+Add([],[小数定数]) *)
                |Dbl_c v1,Add(u2,Dbl_c v2) -> u2+(Dbl_c (v1+v2))
                (* Add([整数定数],[])+[整数定数] *)
                |Add(Int_c v2,u2),Int_c v1 -> (Int_c (v1+v2))+u2
                (* Add([整数定数],[])+[小数定数] *)
                |Add(Int_c v2,u2),Dbl_c v1 -> (Dbl_c (v1+double v2))+u2
                (* Add([小数定数],[])+[整数定数] *)
                |Add(Dbl_c v2,u2),Int_c v1 -> (Dbl_c (double v1+v2))+u2
                (* Add([小数定数],[])+[小数定数] *)
                |Add(Dbl_c v2,u2),Dbl_c v1 -> (Dbl_c (v1+v2))+u2
                (* Add([],[整数定数])+[整数定数] *)
                |Add(u2,Int_c v2),Int_c v1 -> u2+(Int_c (v1+v2))
                (* Add([],[整数定数])+[小数定数] *)
                |Add(u2,Int_c v2),Dbl_c v1 -> u2+(Dbl_c (v1+double v2))
                (* Add([],[小数定数])+[整数定数] *)
                |Add(u2,Dbl_c v2),Int_c v1 -> u2+(Dbl_c (double v1+v2))
                (* Add([],[小数定数])+[小数定数] *)
                |Add(u2,Dbl_c v2),Dbl_c v1 -> u2+(Dbl_c (v1+v2))
                (* [整数定数]+Sub([整数定数],[]) *)
                |Int_c v1,Sub(Int_c v2,u2) -> (Int_c (v1+v2))-u2
                (* [小数定数]+Sub([整数定数],[]) *)
                |Dbl_c v1,Sub(Int_c v2,u2) -> (Dbl_c (v1+double v2))-u2
                (* [整数定数]+Sub([小数定数],[]) *)
                |Int_c v1,Sub(Dbl_c v2,u2) -> (Dbl_c (double v1+v2))-u2
                (* [小数定数]+Sub([小数定数],[]) *)
                |Dbl_c v1,Sub(Dbl_c v2,u2) -> (Dbl_c (v1+v2))-u2
                (* [整数定数]+Sub([],[整数定数]) *)
                |Int_c v1,Sub(u2,Int_c v2) -> u2+(Int_c (v1-v2))
                (* [小数定数]+Sub([],[整数定数]) *)
                |Dbl_c v1,Sub(u2,Int_c v2) -> u2+(Dbl_c (v1-double v2))
                (* [整数定数]+Sub([],[小数定数]) *)
                |Int_c v1,Sub(u2,Dbl_c v2) -> u2+(Dbl_c (double v1-v2))
                (* [小数定数]+Sub([],[小数定数]) *)
                |Dbl_c v1,Sub(u2,Dbl_c v2) -> u2+(Dbl_c (v1-v2))
                (* Sub([整数定数],[])+[整数定数] *)
                |Sub(Int_c v2,u2),Int_c v1 -> (Int_c (v1+v2))-u2
                (* Sub([整数定数],[])+[小数定数] *)
                |Sub(Int_c v2,u2),Dbl_c v1 -> (Dbl_c (v1+double v2))-u2
                (* Sub([小数定数],[])+[整数定数] *)
                |Sub(Dbl_c v2,u2),Int_c v1 -> (Dbl_c (double v1+v2))-u2
                (* Sub([小数定数],[])+[小数定数] *)
                |Sub(Dbl_c v2,u2),Dbl_c v1 -> (Dbl_c (v1+v2))-u2
                (* Sub([],[整数定数])+[整数定数] *)
                |Sub(u2,Int_c v2),Int_c v1 -> u2+(Int_c (v1-v2))
                (* Sub([],[整数定数])+[小数定数] *)
                |Sub(u2,Int_c v2),Dbl_c v1 -> u2+(Dbl_c (v1-double v2))
                (* Sub([],[小数定数])+[整数定数] *)
                |Sub(u2,Dbl_c v2),Int_c v1 -> u2+(Dbl_c (double v1-v2))
                (* Sub([],[小数定数])+[小数定数] *)
                |Sub(u2,Dbl_c v2),Dbl_c v1 -> u2+(Dbl_c (v1-v2))
                (* Par([])+[] *)
                |Par(v1),v2 -> v1+v2
                (* []+Par([]) *)
                |v1,Par(v2) -> v1+v2
                (* x+y *)
                |_ -> Add(x,y)
            else
                Add(x,y)
                
        ///<summary>減算</summary>
        static member ( - ) (x:Expr,y:Expr) : Expr =
            if isEqSimplify then
                match x,y with
                (* x-0 *)
                |_,Int_c 0|_,Dbl_c 0.0 -> x
                (* 0-y *)
                |Int_c 0,_|Dbl_c 0.0,_ -> -y
                (* x-x *)
                |v1,v2 when v1.code=v2.code -> Int_c 0
                (* (-x)-y *)
                |Int_c x,_ when x<0   -> Inv(Int_c (-x)) - y
                (* (-x)-y *)
                |Dbl_c x,_ when x<0.0 -> Inv(Dbl_c (-x)) - y
                (* x-(-y) *)
                |_,Int_c y when y<0   -> x - Inv(Int_c (-y))
                (* x-(-y) *)
                |_,Dbl_c y when y<0.0 -> x - Inv(Dbl_c (-y))
                (* x-[整数定数] *)
                |_,Int_c v2 when v2<0   -> x+Int_c(-v2)
                (* x-[小数定数] *)
                |_,Dbl_c v2 when v2<0.0 -> x+Dbl_c(-v2)
                (* [整数定数]-[整数定数] *)
                |Int_c v1,Int_c v2 -> Int_c(v1-v2)
                (* [整数定数]-[小数定数] *)
                |Int_c v1,Dbl_c v2 -> Dbl_c((double v1)-v2)
                (* [小数定数]-[整数定数] *)
                |Dbl_c v1,Int_c v2 -> Dbl_c(v1-(double v2))
                (* [小数定数]-[小数定数] *)
                |Dbl_c v1,Dbl_c v2 -> Dbl_c(v1-v2)
                (* (-x)-(-y) *)
                |Inv(x),Inv(y) -> y-x
                (* (-x)-y *)
                |Inv(x),_ -> -(x+y)
                (* x-(-y) *)
                |_,Inv(y) -> x+y
                (* [整数定数]-Add([整数定数],[]) *)
                |Int_c v1,Add(Int_c v2,u2) -> (Int_c (v1-v2))-u2
                (* [小数定数]-Add([整数定数],[]) *)
                |Dbl_c v1,Add(Int_c v2,u2) -> (Dbl_c (v1-double v2))-u2
                (* [整数定数]-Add([小数定数],[]) *)
                |Int_c v1,Add(Dbl_c v2,u2) -> (Dbl_c (double v1-v2))-u2
                (* [小数定数]-Add([小数定数],[]) *)
                |Dbl_c v1,Add(Dbl_c v2,u2) -> (Dbl_c (v1-v2))-u2
                (* [整数定数]-Add([],[整数定数]) *)
                |Int_c v1,Add(u2,Int_c v2) -> (Int_c (v1-v2))-u2
                (* [小数定数]-Add([],[整数定数]) *)
                |Dbl_c v1,Add(u2,Int_c v2) -> (Dbl_c (v1-double v2))-u2
                (* [整数定数]-Add([],[小数定数]) *)
                |Int_c v1,Add(u2,Dbl_c v2) -> (Dbl_c (double v1-v2))-u2
                (* [小数定数]-Add([],[小数定数]) *)
                |Dbl_c v1,Add(u2,Dbl_c v2) -> (Dbl_c (v1-v2))-u2
                (* Add([整数定数],[])-[整数定数] *)
                |Add(Int_c v2,u2),Int_c v1 -> (Int_c (v2-v1))+u2
                (* Add([整数定数],[])-[小数定数] *)
                |Add(Int_c v2,u2),Dbl_c v1 -> (Dbl_c (double v2-v1))+u2
                (* Add([小数定数],[])-[整数定数] *)
                |Add(Dbl_c v2,u2),Int_c v1 -> (Dbl_c (v2-double v1))+u2
                (* Add([小数定数],[])-[小数定数] *)
                |Add(Dbl_c v2,u2),Dbl_c v1 -> (Dbl_c (v2-v1))+u2
                (* Add([],[整数定数])-[整数定数] *)
                |Add(u2,Int_c v2),Int_c v1 -> u2+(Int_c (v2-v1))
                (* Add([],[整数定数])-[小数定数] *)
                |Add(u2,Int_c v2),Dbl_c v1 -> u2+(Dbl_c (double v2-v1))
                (* Add([],[小数定数])-[整数定数] *)
                |Add(u2,Dbl_c v2),Int_c v1 -> u2+(Dbl_c (v2-double v1))
                (* Add([],[小数定数])-[小数定数] *)
                |Add(u2,Dbl_c v2),Dbl_c v1 -> u2+(Dbl_c (v2-v1))
                (* [整数定数]-Sub([整数定数],[]) *)
                |Int_c v1,Sub(Int_c v2,u2) -> u2+(Int_c (v1-v2))
                (* [小数定数]-Sub([整数定数],[]) *)
                |Dbl_c v1,Sub(Int_c v2,u2) -> u2+(Dbl_c (v1-double v2))
                (* [整数定数]-Sub([小数定数],[]) *)
                |Int_c v1,Sub(Dbl_c v2,u2) -> u2+(Dbl_c (double v1-v2))
                (* [小数定数]-Sub([小数定数],[]) *)
                |Dbl_c v1,Sub(Dbl_c v2,u2) -> u2+(Dbl_c (v1-v2))
                (* [整数定数]-Sub([],[整数定数]) *)
                |Int_c v1,Sub(u2,Int_c v2) -> (Int_c (v1+v2))-u2
                (* [小数定数]-Sub([],[整数定数]) *)
                |Dbl_c v1,Sub(u2,Int_c v2) -> (Dbl_c (v1+double v2))-u2
                (* [整数定数]-Sub([],[小数定数]) *)
                |Int_c v1,Sub(u2,Dbl_c v2) -> (Dbl_c (double v1+v2))-u2
                (* [小数定数]-Sub([],[小数定数]) *)
                |Dbl_c v1,Sub(u2,Dbl_c v2) -> (Dbl_c (v1+v2))-u2
                (* Sub([整数定数],[])-[整数定数] *)
                |Sub(Int_c v2,u2),Int_c v1 -> (Int_c (v2-v1))-u2
                (* Sub([整数定数],[])-[小数定数] *)
                |Sub(Int_c v2,u2),Dbl_c v1 -> (Dbl_c (double v2-v1))-u2
                (* Sub([小数定数],[])-[整数定数] *)
                |Sub(Dbl_c v2,u2),Int_c v1 -> (Dbl_c (v2-double v1))-u2
                (* Sub([小数定数],[])-[小数定数] *)
                |Sub(Dbl_c v2,u2),Dbl_c v1 -> (Dbl_c (v2-v1))-u2
                (* Sub([],[整数定数])-[整数定数] *)
                |Sub(u2,Int_c v2),Int_c v1 -> u2-(Int_c (v1+v2))
                (* Sub([],[整数定数])-[小数定数] *)
                |Sub(u2,Int_c v2),Dbl_c v1 -> u2-(Dbl_c (v1+double v2))
                (* Sub([],[小数定数])-[整数定数] *)
                |Sub(u2,Dbl_c v2),Int_c v1 -> u2-(Dbl_c (double v1+v2))
                (* Sub([],[小数定数])-[小数定数] *)
                |Sub(u2,Dbl_c v2),Dbl_c v1 -> u2-(Dbl_c (v1+v2))
                (* x-(y1+y2) *)
                |_,Add(y1,y2) -> x-y1-y2
                (* x-(y1-y2) *)
                |_,Sub(y1,y2) -> x-y1+y2
                (* x-y *)
                |_ -> Sub(x,y)
            else
                Sub(x,y)
                
        ///<summary>乗算</summary>
        static member ( * ) (x:Expr,y:Expr) : Expr =
            if isEqSimplify then
                match x,y with
                (* x*0 *)
                |Int_c 0,_|Dbl_c 0.0,_|_,Int_c 0|_,Dbl_c 0.0 -> Int_c 0
                (* 1*y *)
                |Int_c 1,_|Dbl_c 1.0,_ -> y
                (* x*1 *)
                |_,Int_c 1|_,Dbl_c 1.0 -> x
                (* (-x)*y *)
                |Int_c x,_ when x<0   -> Inv(Int_c (-x)) * y
                (* (-x)*y *)
                |Dbl_c x,_ when x<0.0 -> Inv(Dbl_c (-x)) * y
                (* x*(-y) *)
                |_,Int_c y when y<0   -> x * Inv(Int_c (-y))
                (* x*(-y) *)
                |_,Dbl_c y when y<0.0 -> x * Inv(Dbl_c (-y))
                (* [負の整数定数]*y *)
                |Int_c x,_ when x<0   -> -(Int_c (-x)*y)
                (* [負の小数定数]*y *)
                |Dbl_c x,_ when x<0.0 -> -(Dbl_c (-x)*y)
                (* x*[負の整数定数] *)
                |_,Int_c y when y<0   -> -(x*Int_c (-y))
                (* x*[負の小数定数] *)
                |_,Dbl_c y when y<0.0 -> -(x*Dbl_c (-y))
                (* (-x)*(-y) *)
                |Inv(x),Inv(y) -> x*y
                (* x*(-y) *)
                |_,Inv(y) -> -(x*y)
                (* (-x)*y *)
                |Inv(x),_ -> -(x*y)
                (* [整数定数]*[整数定数] *)
                |Int_c x,Int_c y -> Int_c(x*y)
                (* [整数定数]*[小数定数] *)
                |Int_c x,Dbl_c y -> Dbl_c((double x)*y)
                (* [小数定数]*[整数定数] *)
                |Dbl_c x,Int_c y -> Dbl_c(x*(double y))
                (* [小数定数]*[小数定数] *)
                |Dbl_c x,Dbl_c y -> Dbl_c(x*y)
                (* [整数定数]*Mul([整数定数],[]) *)
                |Int_c v1,Mul(Int_c v2,u2) -> (Int_c (v1*v2))*u2
                (* [整数定数]*Mul([小数定数],[]) *)
                |Int_c v1,Mul(Dbl_c v2,u2) -> (Dbl_c (double v1*v2))*u2
                (* [小数定数]*Mul([整数定数],[]) *)
                |Dbl_c v1,Mul(Int_c v2,u2) -> (Dbl_c (v1*double v2))*u2
                (* [小数定数]*Mul([小数定数],[]) *)
                |Dbl_c v1,Mul(Dbl_c v2,u2) -> (Dbl_c (v1*v2))*u2
                (* Mul([整数定数],[])*[整数定数] *)
                |Mul(Int_c v2,u2),Int_c v1 -> (Int_c (v1*v2))*u2
                (* Mul([整数定数],[])*[小数定数] *)
                |Mul(Int_c v2,u2),Dbl_c v1 -> (Dbl_c (v1*double v2))*u2
                (* Mul([小数定数],[])*[整数定数] *)
                |Mul(Dbl_c v2,u2),Int_c v1 -> (Dbl_c (double v1*v2))*u2
                (* Mul([小数定数],[])*[小数定数] *)
                |Mul(Dbl_c v2,u2),Dbl_c v1 -> (Dbl_c (v1*v2))*u2
                (* [整数定数]*Div([整数定数],[]) *)
                |Int_c v1,Div(Int_c v2,u2) -> (Int_c (v1*v2))/u2
                (* [整数定数]*Div([小数定数],[]) *)
                |Int_c v1,Div(Dbl_c v2,u2) -> (Dbl_c (double v1*v2))/u2
                (* [小数定数]*Div([整数定数],[]) *)
                |Dbl_c v1,Div(Int_c v2,u2) -> (Dbl_c (v1*double v2))/u2
                (* [小数定数]*Div([小数定数],[]) *)
                |Dbl_c v1,Div(Dbl_c v2,u2) -> (Dbl_c (v1*v2))/u2
                (* Div([整数定数],[])*[整数定数] *)
                |Div(Int_c v2,u2),Int_c v1 -> (Int_c (v1*v2))/u2
                (* Div([整数定数],[])*[小数定数] *)
                |Div(Int_c v2,u2),Dbl_c v1 -> (Dbl_c (v1*double v2))/u2
                (* Div([小数定数],[])*[整数定数] *)
                |Div(Dbl_c v2,u2),Int_c v1 -> (Dbl_c (double v1*v2))/u2
                (* Div([小数定数],[])*[小数定数] *)
                |Div(Dbl_c v2,u2),Dbl_c v1 -> (Dbl_c (v1*v2))/u2
                (* x*(y1+y2) or x*(y1-y2) *)
                |_,(Add _|Sub _) -> x*Par(y)
                (* (x1+x2)*y or (x1-x2)*y *)
                |(Add _|Sub _),_ -> Par(x)*y
                (* x*y *)
                |_ -> Mul(x,y)
            else
                Mul(x,y)
                
        ///<summary>除算</summary>
        static member ( / ) (x:Expr,y:Expr) : Expr =
            if isEqSimplify then
                match x,y with
                (* x/0 *)
                |_,Int_c 0 |_,Dbl_c 0.0 ->
                    Console.WriteLine("Error: ゼロ割りを検出しました")
                    NaN
                (* 0/y *)
                |Int_c 0,_ -> Int_c 0
                |Dbl_c 0.0,_ -> Dbl_c 0.0
                (* x/1 *)
                |_,Dbl_c 1.0 -> x
                (* x/1 *)
                |_,Int_c 1 -> x
                (* x/x *)
                |v1,v2 when v1.code=v2.code -> Int_c 1
                (* (-x)/y *)
                |Int_c x,_ when x<0   -> Inv(Int_c (-x)) / y
                (* (-x)/y *)
                |Dbl_c x,_ when x<0.0 -> Inv(Dbl_c (-x)) / y
                (* x/(-y) *)
                |_,Int_c y when y<0   -> x / Inv(Int_c (-y))
                (* x/(-y) *)
                |_,Dbl_c y when y<0.0 -> x / Inv(Dbl_c (-y))
                (* [整数]/[整数] 剰余0 *)
                |Int_c x,Int_c y when x%y=0 -> Int_c (x/y)
                (* [整数]/[整数] *)
                |Int_c x,Int_c y -> Dbl_c (double x/double y)
                (* [負の整数定数]/[負の整数定数] *)
                |Dbl_c x,Int_c y when x<0.0 && y<0 -> Dbl_c(-x/(double -y))
                (* [負の整数定数]/[整数定数] *)
                |Dbl_c x,Int_c y when x<0.0 -> -Dbl_c((-x)/(double y))
                (* [整数定数]/[負の整数定数] *)
                |Dbl_c x,Int_c y when y<0 -> -Dbl_c(x/(-(double y)))
                (* [整数定数]/[整数定数] *)
                |Dbl_c x,Int_c y -> Dbl_c(x/(double y))
                (* [負の整数定数]/[負の小数定数] *)
                |Int_c x,Dbl_c y when x<0 && y<0.0 -> Dbl_c((double -x)/(-y))
                (* [負の整数定数]/[小数定数] *)
                |Int_c x,Dbl_c y when x<0 -> -Dbl_c((double -x)/y)
                (* [整数定数]/[負の小数定数] *)
                |Int_c x,Dbl_c y when y<0.0 -> -Dbl_c((double x)/(-y))
                (* [整数定数]/[小数定数] *)
                |Int_c x,Dbl_c y -> Dbl_c((double x)/y)
                (* [負の小数定数]/[負の小数定数] *)
                |Dbl_c x,Dbl_c y when x<0.0 && y<0.0 -> Dbl_c((-x)/(-y))
                (* [負の小数定数]/[小数定数] *)
                |Dbl_c x,Dbl_c y when x<0.0 -> -Dbl_c((-x)/y)
                (* [小数定数]/[負の小数定数] *)
                |Dbl_c x,Dbl_c y when y<0.0 -> -Dbl_c(x/(-y))
                (* [小数定数]/[小数定数] *)
                |Dbl_c x,Dbl_c y -> Dbl_c(x/y)
                (* (-x)/[小数定数] *)
                |Dbl_c x,_ when x<0.0 -> -(Dbl_c (-x)/y)
                (* x/[負の小数定数] *)
                |_,Dbl_c y when y<0.0 -> -(x/Dbl_c (-y))
                (* (-x)/[整数定数] *)
                |Int_c x,_ when x<0 -> -(Int_c (-x)/y)
                (* x/[負の整数定数] *)
                |_,Int_c y when y<0 -> -(x/Int_c (-y))
                (* (-x)/(-y) *)
                |Inv(x),Inv(y) -> x/y
                (* x/(-y) *)
                |_,Inv(y) -> -(x/y)
                (* (-x)/y *)
                |Inv(x),_ -> -(x/y)
                (* [整数定数]/Mul([整数定数],[]) *)
                |Int_c v1,Mul(Int_c v2,u2) when v1%v2=0 -> (Int_c (v1/v2))/u2
                (* [整数定数]/Mul([],[整数定数]) *)
                |Int_c v1,Mul(v2,Int_c u2) when v1%u2=0 -> (Int_c (v1/u2))/v2
                (* [整数定数]/Mul([整数定数],[]) *)
                |Int_c v1,Mul(Int_c v2,u2) -> (Dbl_c (double v1/double v2))/u2
                (* [整数定数]/Mul([],[整数定数]) *)
                |Int_c v1,Mul(v2,Int_c u2) -> (Dbl_c (double v1/double u2))/v2
                (* [整数定数]/Mul([小数定数],[]) *)
                |Int_c v1,Mul(Dbl_c v2,u2) -> (Dbl_c (double v1/v2))/u2
                (* [整数定数]/Mul([],[小数定数]) *)
                |Int_c v1,Mul(v2,Dbl_c u2) -> (Dbl_c (double v1/u2))/v2
                (* [小数定数]/Mul([整数定数],[]) *)
                |Dbl_c v1,Mul(Int_c v2,u2) -> (Dbl_c (v1/double v2))/u2
                (* [小数定数]/Mul([],[整数定数]) *)
                |Dbl_c v1,Mul(v2,Int_c u2) -> (Dbl_c (v1/double u2))/v2
                (* [小数定数]/Mul([小数定数],[]) *)
                |Dbl_c v1,Mul(Dbl_c v2,u2) -> (Dbl_c (v1/v2))/u2
                (* [小数定数]/Mul([],[小数定数]) *)
                |Dbl_c v1,Mul(v2,Dbl_c u2) -> (Dbl_c (v1/u2))/v2
                (* Mul([整数定数],[])/[整数定数] *)
                |Mul(Int_c v2,u2),Int_c v1 when v2%v1=0 -> (Int_c (v2/v1))*u2
                (* Mul([],[整数定数])/[整数定数] *)
                |Mul(v2,Int_c u2),Int_c v1 when u2%v1=0 -> v2*(Int_c (u2/v1))
                (* Mul([整数定数],[])/[整数定数] *)
                |Mul(Int_c v2,u2),Int_c v1 -> (Dbl_c (double v2/double v1))*u2
                (* Mul([],[整数定数])/[整数定数] *)
                |Mul(v2,Int_c u2),Int_c v1 -> v2*(Dbl_c (double u2/double v1))
                (* Mul([整数定数],[])/[小数定数] *)
                |Mul(Int_c v2,u2),Dbl_c v1 -> (Dbl_c (double v2/v1))*u2
                (* Mul([],[整数定数])/[小数定数] *)
                |Mul(v2,Int_c u2),Dbl_c v1 -> v2*(Dbl_c (double u2/v1))
                (* Mul([小数定数],[])/[整数定数] *)
                |Mul(Dbl_c v2,u2),Int_c v1 -> (Dbl_c (v2/double v1))*u2
                (* Mul([],[小数定数])/[整数定数] *)
                |Mul(v2,Dbl_c u2),Int_c v1 -> v2*(Dbl_c (u2/double v1))
                (* Mul([小数定数],[])/[小数定数] *)
                |Mul(Dbl_c v2,u2),Dbl_c v1 -> (Dbl_c (v2/v1))*u2
                (* Mul([],[小数定数])/[小数定数] *)
                |Mul(v2,Dbl_c u2),Dbl_c v1 -> v2*(Dbl_c (u2/v1))
                (* [整数定数]/Div([整数定数],[]) *)
                |Int_c v1,Div(Int_c v2,u2) when v1%v2=0 -> (Int_c (v1/v2))*u2
                (* [整数定数]/Div([],[整数定数]) *)
                |Int_c v1,Div(v2,Int_c u2) -> (Int_c (v1*u2))/v2
                (* [整数定数]/Div([整数定数],[]) *)
                |Int_c v1,Div(Int_c v2,u2) -> (Dbl_c (double v1/double v2))*u2
                (* [整数定数]/Div([小数定数],[]) *)
                |Int_c v1,Div(Dbl_c v2,u2) -> (Dbl_c (double v1/v2))*u2
                (* [整数定数]/Div([],[小数定数]) *)
                |Int_c v1,Div(v2,Dbl_c u2) -> (Dbl_c (double v1*u2))/v2
                (* [小数定数]/Div([整数定数],[]) *)
                |Dbl_c v1,Div(Int_c v2,u2) -> (Dbl_c (v1/double v2))*u2
                (* [小数定数]/Div([],[整数定数]) *)
                |Dbl_c v1,Div(v2,Int_c u2) -> (Dbl_c (v1*double u2))/v2
                (* [小数定数]/Div([小数定数],[]) *)
                |Dbl_c v1,Div(Dbl_c v2,u2) -> (Dbl_c (v1/v2))*u2
                (* [小数定数]/Div([],[小数定数]) *)
                |Dbl_c v1,Div(v2,Dbl_c u2) -> (Dbl_c (v1*u2))/v2
                (* Div([整数定数],[])/[整数定数] *)
                |Div(Int_c v2,u2),Int_c v1 when v2%v1=0 -> (Int_c (v2/v1))/u2
                (* Div([],[整数定数])/[整数定数] *)
                |Div(v2,Int_c u2),Int_c v1 -> v2/(Int_c (u2*v1))
                (* Div([整数定数],[])/[整数定数] *)
                |Div(Int_c v2,u2),Int_c v1 -> (Dbl_c (double v2/double v1))/u2
                (* Div([整数定数],[])/[小数定数] *)
                |Div(Int_c v2,u2),Dbl_c v1 -> (Dbl_c (double v2/v1))/u2
                (* Div([],[整数定数])/[小数定数] *)
                |Div(v2,Int_c u2),Dbl_c v1 -> v2/(Dbl_c (double u2*v1))
                (* Div([小数定数],[])/[整数定数] *)
                |Div(Dbl_c v2,u2),Int_c v1 -> (Dbl_c (v2/double v1))/u2
                (* Div([],[小数定数])/[整数定数] *)
                |Div(v2,Dbl_c u2),Int_c v1 -> v2/(Dbl_c (u2*double v1))
                (* Div([小数定数],[])/[小数定数] *)
                |Div(Dbl_c v2,u2),Dbl_c v1 -> (Dbl_c (v2/v1))/u2
                (* Div([],[小数定数])/[小数定数] *)
                |Div(v2,Dbl_c u2),Dbl_c v1 -> v2/(Dbl_c (u2*v1))
                (* x/(y1+y2+…) or x/(y1-y2) or x/(y1*y2) or x/(y1/y2) *)
                |_,(Add _|Sub _|Mul _|Div _) -> x/Par(y)
                (* (x1+x2+…)/y or (x1-x2)/y *)
                |(Add _|Sub _),_ -> Par(x)/y
                (* x/y *)
                |_ -> Div(x,y)
            else
                Div(x,y)
                
        ///<summary>整数同士の除算(剰余無視)</summary>
        static member ( ./ ) (x:Expr,y:Expr) : Expr =
            if isEqSimplify then
                match (x,y) with
                |_,Int_c 0 -> NaN
                |Int_c 0,_ -> Int_c 0
                |_,Int_c 1 -> x
                |v1,v2 when v1.code=v2.code -> Int_c 1
                |Int_c v1,Int_c v2 when v1<0 && v2<0  -> Int_c((-v1)/(-v2))
                |Int_c v1,_ when v1<0   -> -(Int_c (-v1)/y)
                |_,Int_c v2 when v2<0   -> -(x/Int_c (-v2))
                |Int_c v1,Int_c v2 -> Int_c(v1/v2)
                |Inv(v1),Inv(v2) -> (v1/v2)
                |_,Inv(v2) -> -(x/v2)
                |Inv(v1),_ -> -(v1/y)
                |Mul(Int_c v2,u2),Int_c v1 when v2%v1=0 -> (Int_c (v2/v1))*u2
                |Mul(v2,Int_c u2),Int_c v1 when u2%v1=0 -> v2*(Int_c (u2/v1))
                |_,(Add _|Sub _|Mul _|Div _) -> x./Par(y)
                |(Add _|Sub _),_ -> Par(x)./y
                |_ -> Div(x,y)
            else
              Div(x,y)

        ///<summary>剰余</summary>
        static member ( % ) (x:Expr,y:Expr) : Expr =
                match isEqSimplify,x,y with
                |true,_,Int_c 0 -> NaN
                |true,Int_c 0,_ -> Int_c 0
                |true,_,Int_c 1 -> Int_c 0
                |true,Int_c v1,_ when v1<0   -> -((Int_c (-v1))%y)
                |true,_,Int_c v2 when v2<0   -> (x%(Int_c (-v2)))
                |true,Int_c v1,Int_c v2 -> Int_c(v1%v2)
                |true,_,(Add _|Sub _|Mul _|Div _) -> x%Par(y)
                |true,(Add _|Sub _),_ -> Par(x) % y
                |_ ->
                    match p.lang with
                    |F -> Formula <| "mod("+x.code+","+y.code+")"
                    |C -> Formula <| x.code+"%"+y.code
                    |H -> Formula <| "\\mathrm{mod}("+x.code+","+y.code+")"
                    |T -> Formula <| "\\mathrm{mod}("+x.code+","+y.code+")"
                    
        ///<summary>累乗</summary>
        static member pow(x:Expr, y:Expr) =
            match p.lang,x,y with
            (* 0^0 *)
            |_,(Int_c 0|Dbl_c 0.0),(Int_c 0|Dbl_c 0.0) -> Expr.NaN
            (* 0^y *)
            |_,(Int_c 0|Dbl_c 0.0),_ -> Int_c 0
            (* x^0 *)
            |_,_,(Int_c 0|Dbl_c 0.0) -> Int_c 1
            (* [整数定数]^[整数定数] *)
            |(F|C),Int_c v1,Int_c v2 -> Dbl_c(double(v1)**double(v2))
            (* [整数定数]^[小数定数] *)
            |(F|C),Int_c v1,Dbl_c v2 -> Dbl_c((double v1)**v2)
            (* [小数定数]^[整数定数] *)
            |(F|C),Dbl_c v1,Int_c v2 -> Dbl_c(v1**(double v2))
            (* [小数定数]^[小数定数] *)
            |(F|C),Dbl_c v1,Dbl_c v2 -> Dbl_c(v1**v2)
            (* [負の整数定数]^y *)
            |_,Int_c v1,_ when v1<0 -> Expr.pow(Par(x),y)
            (* [負の小数定数]^y *)
            |_,Dbl_c v1,_ when v1<0.0 -> Expr.pow(Par(x),y)
            (* x^[負の整数定数] *)
            |T,_,Int_c v2 when v2<0 -> Expr.pow(x,y)
            |_,_,Int_c v2 when v2<0 -> Expr.pow(x,Par(y))
            (* x^[負の小数定数] *)
            |T,_,Dbl_c v2 when v2<0.0 -> Expr.pow(x,y)
            |_,_,Dbl_c v2 when v2<0.0 -> Expr.pow(x,Par(y))
            (* x = [負号付] or (x1+x2+…) or (x1-x2) or (x1*x2) or (x1/x2) 
               y = [負号付] or (y1+y2+…) or (y1-y2) or (y1*y2) or (y1/y2) *)
            |T,(Inv _|Add _|Sub _|Mul _|Div _),(Inv _|Add _|Sub _|Mul _|Div _) -> Expr.pow(Par(x),y)
            |_,(Inv _|Add _|Sub _|Mul _|Div _),(Inv _|Add _|Sub _|Mul _|Div _) -> Expr.pow(Par(x),Par(y))
            (* x = [負号付] or (x1+x2+…) or (x1-x2) or (x1*x2) or (x1/x2) *)
            |_,(Inv _|Add _|Sub _|Mul _|Div _),_ -> Expr.pow(Par(x),y)
            (* y = [負号付] or (y1+y2+…) or (y1-y2) or (y1*y2) or (y1/y2) *)
            |_,_,(Inv _|Add _|Sub _|Mul _|Div _) -> Expr.pow(x,Par(y))
            (* x^y *)
            |_ -> 
                match p.lang with
                |F -> Formula <| x.code+"**"+y.code
                |C -> Formula <| "pow("+x.code+","+y.code+")"
                |H -> Formula <| x.code+"^{"+y.code+"}"
                |T -> Formula <| x.code+"^{"+y.code+"}"
                
        ///<summary>等号</summary>
        static member (.=) (v1:Expr,v2:Expr) = Eq(v1,v2)
        ///<summary>不等号</summary>
        static member (.=/) (v1:Expr,v2:Expr) = NEq(v1,v2)        
        ///<summary>比較（より小）</summary>
        static member (.<) (v1:Expr,v2:Expr) = 
            match v1 with
            |Less(u1,u2) ->
                let x1 = u1.<u2
                let x2 = u2.<v2
                AND([x1;x2])
            |LessEq(u1,u2) ->
                let x1 = u1.<=u2
                let x2 = u2.<v2
                AND([x1;x2])
            |Greater(u1,u2) ->
                let x1 = u1.>u2
                let x2 = u2.<v2
                AND([x1;x2])
            |GreaterEq(u1,u2) ->
                let x1 = u1.>=u2
                let x2 = u2.<v2
                AND([x1;x2])
            |AND(lst) ->
                let u1 =
                    match lst with
                    |[] -> NaN
                    |_ ->
                      match lst[lst.Length-1] with
                      |Less(_,u2) -> u2
                      |LessEq(_,u2) -> u2
                      |Greater(_,u2) -> u2
                      |GreaterEq(_,u2) -> u2
                      |_ -> NaN
                let x2 = u1.<v2
                AND(lst@[x2])
            |_ ->
                Less(v1,v2)
        ///<summary>比較（以下）</summary>
        static member (.<=) (v1:Expr,v2:Expr) = 
            match v1 with
            |Less(u1,u2) ->
                let x1 = u1.<u2
                let x2 = u2.<=v2
                AND([x1;x2])
            |LessEq(u1,u2) ->
                let x1 = u1.<=u2
                let x2 = u2.<=v2
                AND([x1;x2])
            |Greater(u1,u2) ->
                let x1 = u1.>u2
                let x2 = u2.<=v2
                AND([x1;x2])
            |GreaterEq(u1,u2) ->
                let x1 = u1.>=u2
                let x2 = u2.<=v2
                AND([x1;x2])
            |AND(lst) ->
                let u1 =
                    match lst with
                    |[] -> NaN
                    |_ ->
                        match lst[lst.Length-1] with
                        |Less(_,u2) -> u2
                        |LessEq(_,u2) -> u2
                        |Greater(_,u2) -> u2
                        |GreaterEq(_,u2) -> u2
                        |_ -> NaN
                let x2 = (u1.<=v2)
                AND(lst@[x2])
            |_ ->
                LessEq(v1,v2)
        ///<summary>比較（より大）</summary>
        static member (.>) (v1:Expr,v2:Expr) = 
            match v1 with
            |Less(u1,u2) ->
                let x1 = u1.<u2
                let x2 = u2.>v2
                AND[x1;x2]
            |LessEq(u1,u2) ->
                let x1 = u1.<=u2
                let x2 = u2.>v2
                AND[x1;x2]
            |Greater(u1,u2) ->
                let x1 = u1.>u2
                let x2 = u2.>v2
                AND[x1;x2]
            |GreaterEq(u1,u2) ->
                let x1 = u1.>=u2
                let x2 = u2.>v2
                AND[x1;x2]
            |AND(lst) ->
                let u1 =
                    match lst with
                    |[] -> NaN
                    |_ ->
                        match lst[lst.Length-1] with
                        |Less(_,u2) -> u2
                        |LessEq(_,u2) -> u2
                        |Greater(_,u2) -> u2
                        |GreaterEq(_,u2) -> u2
                        |_ -> NaN
                let x2 = u1.>v2
                AND(lst@[x2])
            |_ ->
                Greater(v1,v2)
        ///<summary>比較（以上）</summary>
        static member (.>=) (v1:Expr,v2:Expr) = 
            match v1 with
            |Less(u1,u2) ->
                let x1 = u1.<u2
                let x2 = u2.>=v2
                AND[x1;x2]
            |LessEq(u1,u2) ->
                let x1 = u1.<=u2
                let x2 = u2.>=v2
                AND[x1;x2]
            |Greater(u1,u2) ->
                let x1 = u1.>u2
                let x2 = u2.>=v2
                AND[x1;x2]
            |GreaterEq(u1,u2) ->
                let x1 = u1.>=u2
                let x2 = u2.>=v2
                AND[x1;x2]
            |AND(lst) ->
                let u1 =
                    match lst with
                    |[] -> NaN
                    |_ ->
                        match lst[lst.Length-1] with
                        |Less(_,u2) -> u2
                        |LessEq(_,u2) -> u2
                        |Greater(_,u2) -> u2
                        |GreaterEq(_,u2) -> u2
                        |_ -> NaN
                let x2 = u1.>=v2
                AND(lst@[x2])
            |_ ->
                GreaterEq(v1,v2)

        ///<summary>代入</summary>
        static member (<==) (x:Expr,y:Expr) =
            match p.lang with
            |F ->
                p.codewrite(x.code + " = " + y.code)
            |C->
                p.codewrite(x.code + " = " + y.code + ";")
            |T ->
                p.codewrite("\\begin{align}")
                p.codewrite(x.code + " \\leftarrow " + y.code)
                p.codewrite("\\end{align}")
            |H ->
                p.codewrite("\\[")
                p.codewrite("\\begin{align}")
                p.codewrite(x.code + " \\leftarrow " + y.code)
                p.codewrite("\\end{align}")
                p.codewrite("\\]")