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
        |Var of Etype*string
        |Par of Etype*Expr
        |Inv of Etype*Expr
        |Add of Etype*Expr*Expr
        |Sub of Etype*Expr*Expr
        |Mul of Etype*Expr*Expr
        |Div of Etype*Expr*Expr
        |Pow of Etype*Expr*Expr
        |Exp of Etype*Expr
        |Sin of Etype*Expr
        |Cos of Etype*Expr
        |Tan of Etype*Expr
        |Asin of Etype*Expr
        |Acos of Etype*Expr
        |Atan of Etype*Expr
        |Atan2 of Expr*Expr
        |Abs of Etype*Expr
        |Log of Etype*Expr
        |Log10 of Etype*Expr
        |Sqrt of Etype*Expr
        |Formula of Etype*string
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

        member this.etype with get() =
            match this with
            |Str_c _ -> St
            |Int_c _ -> It 4
            |Dbl_c _ -> Dt
            |Var (t,_) -> t
            |Par (t,_) -> t
            |Inv (t,_) -> t
            |Add (t,_,_) -> t
            |Sub (t,_,_) -> t
            |Mul (t,_,_) -> t
            |Div (t,_,_) -> t
            |Pow (t,_,_) -> t
            |Exp (t,_) -> t
            |Sin (t,_) -> t
            |Cos (t,_) -> t
            |Tan (t,_) -> t
            |Asin (t,_) -> t
            |Acos (t,_) -> t
            |Atan (t,_) -> t
            |Atan2 _ -> Dt
            |Abs (t,_) -> t
            |Log (t,_) -> t
            |Log10 (t,_) -> t
            |Sqrt (t,_) -> t
            |Formula (t,_) -> t
            |Eq _ -> Bt
            |NEq _ -> Bt
            |Greater _ -> Bt
            |GreaterEq _ -> Bt
            |Less _ -> Bt
            |LessEq _ -> Bt
            |AND _ -> Bt
            |OR _ -> Bt
            |Null -> Nt
            |NaN -> Nt
        
        member this.code with get() =
            match p.lang with
            |T ->
                match this with
                |Var(_,x) -> x
                |Int_c x -> p.ItoS(x)
                |Dbl_c x -> p.DtoS(x)
                |Str_c x -> "\""+x+"\""
                |Par(_,x) -> "\\left("+x.code+"\\right)"
                |Inv(_,x) -> "-"+x.code
                |Add(_,x,y) -> x.code+"+"+y.code
                |Sub(_,x,y) -> x.code+"-"+y.code
                |Mul(_,x,y) -> x.code+" \\cdot "+y.code
                |Div(_,Par(_,x),Par(_,y)) -> "\\frac{\\displaystyle "+x.code+"}{\\displaystyle "+y.code+"}"
                |Div(_,Par(_,x),y) -> "\\frac{\\displaystyle "+x.code+"}{\\displaystyle "+y.code+"}"
                |Div(_,x,Par(_,y)) -> "\\frac{\\displaystyle "+x.code+"}{\\displaystyle "+y.code+"}"
                |Div(_,x,y) -> "\\frac{\\displaystyle "+x.code+"}{\\displaystyle "+y.code+"}"
                |Pow(_,x,y) -> x.code+"^{"+y.code+"}"
                |Exp(_,x) -> "\\exp\\left("+x.code+"\\right)"
                |Sin(_,x) -> "\\sin\\left("+x.code+"\\right)"
                |Cos(_,x) -> "\\cos\\left("+x.code+"\\right)"
                |Tan(_,x) -> "\\tan\\left("+x.code+"\\right)"
                |Asin(_,x) -> "\\asin\\left("+x.code+"\\right)"
                |Acos(_,x) -> "\\acos\\left("+x.code+"\\right)"
                |Atan(_,x) -> "\\atan\\left("+x.code+"\\right)"
                |Atan2 (x,y) -> "\\atan2\\left("+x.code+","+y.code+"\\right)"
                |Abs(_,x) -> "\\left|"+x.code+"\\right|"
                |Log(_,x) -> "\\ln\\left("+x.code+"\\right)"
                |Log10(_,x) -> "\\log\\left("+x.code+"\\right)"
                |Sqrt(_,x) -> "\\sqrt{"+x.code+"}"
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
                |Formula(_,s) -> s
                |_ ->
                    ""
            |H ->
                match this with
                |Var(_,x) -> x
                |Int_c x -> p.ItoS(x)
                |Dbl_c x -> p.DtoS(x)
                |Str_c x -> "\""+x+"\""
                |Par(_,x) -> "\\left("+x.code+"\\right)"
                |Inv(_,x) -> "-"+x.code
                |Add (_,x,y) -> x.code+"+"+y.code
                |Sub (_,x,y) -> x.code+"-"+y.code
                |Mul (_,x,y) -> x.code+" \\cdot "+y.code
                |Div (_,Par(_,x),Par(_,y)) -> "\\frac{\\displaystyle "+x.code+"}{\\displaystyle "+y.code+"}"
                |Div (_,Par(_,x),y) -> "\\frac{\\displaystyle "+x.code+"}{\\displaystyle "+y.code+"}"
                |Div (_,x,Par(_,y)) -> "\\frac{\\displaystyle "+x.code+"}{\\displaystyle "+y.code+"}"
                |Div (_,x,y) -> "\\frac{\\displaystyle "+x.code+"}{\\displaystyle "+y.code+"}"
                |Pow (_,x,y) -> x.code+"^{"+y.code+"}"
                |Exp(_,x) -> "\\exp\\left("+x.code+"\\right)"
                |Sin(_,x) -> "\\sin\\left("+x.code+"\\right)"
                |Cos(_,x) -> "\\cos\\left("+x.code+"\\right)"
                |Tan(_,x) -> "\\tan\\left("+x.code+"\\right)"
                |Asin(_,x) -> "\\asin\\left("+x.code+"\\right)"
                |Acos(_,x) -> "\\acos\\left("+x.code+"\\right)"
                |Atan(_,x) -> "\\atan\\left("+x.code+"\\right)"
                |Atan2 (x,y) -> "\\atan2\\left("+x.code+","+y.code+"\\right)"
                |Abs(_,x) -> "\\left|"+x.code+"\\right|"
                |Log(_,x) -> "\\ln\\left("+x.code+"\\right)"
                |Log10(_,x) -> "\\log\\left("+x.code+"\\right)"
                |Sqrt(_,x) -> "\\sqrt{"+x.code+"}"
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
                |Formula(_,s) -> s
                |_ ->
                    ""
            |F ->
                match this with
                |Var(_,x) -> x
                |Int_c x -> p.ItoS(x)
                |Dbl_c x -> p.DtoS(x)
                |Str_c x -> "\""+x+"\""
                |Par(_,x) -> "("+x.code+")"
                |Inv(_,x) -> "-"+x.code
                |Add (_,x,y) -> x.code+"+"+y.code
                |Sub (_,x,y) -> x.code+"-"+y.code
                |Mul (_,x,y) -> x.code+"*"+y.code
                |Div (_,x,y) -> x.code+"/"+y.code
                |Pow (_,x,y) -> x.code+"**("+y.code+")"
                |Exp(_,x) -> "exp("+x.code+")"
                |Sin(_,x) -> "sin("+x.code+")"
                |Cos(_,x) -> "cos("+x.code+")"
                |Tan(_,x) -> "tan("+x.code+")"
                |Asin(_,x) -> "asin("+x.code+")"
                |Acos(_,x) -> "acos("+x.code+")"
                |Atan(_,x) -> "atan("+x.code+")"
                |Atan2 (x,y) -> "atan2("+x.code+","+y.code+")"
                |Abs(_,x) -> "abs("+x.code+")"
                |Log(_,x) -> "log("+x.code+")"
                |Log10(_,x) -> "log10("+x.code+")"
                |Sqrt(_,x) -> "sqrt("+x.code+")"
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
                |Formula(_,s) -> s
                |_ ->
                    ""
            |C ->
                match this with
                |Var(_,x) -> x
                |Int_c x -> p.ItoS(x)
                |Dbl_c x -> p.DtoS(x)
                |Str_c x -> "\""+x+"\""
                |Par(_,x) -> "("+x.code+")"
                |Inv(_,x) -> "-"+x.code
                |Add (_,x,y) -> x.code+"+"+y.code
                |Sub (_,x,y) -> x.code+"-"+y.code
                |Mul (_,x,y) -> x.code+"*"+y.code
                |Div (_,x,y) -> x.code+"/"+y.code
                |Pow (Zt,x,y) -> "cpow("+x.code+","+y.code+")"
                |Exp(Zt,x) -> "cexp("+x.code+")"
                |Sin(Zt,x) -> "csin("+x.code+")"
                |Cos(Zt,x) -> "ccos("+x.code+")"
                |Tan(Zt,x) -> "ctan("+x.code+")"
                |Asin(Zt,x) -> "casin("+x.code+")"
                |Acos(Zt,x) -> "cacos("+x.code+")"
                |Atan(Zt,x) -> "catan("+x.code+")"
                |Pow (_,x,y) -> "pow("+x.code+","+y.code+")"
                |Exp(_,x) -> "exp("+x.code+")"
                |Sin(_,x) -> "sin("+x.code+")"
                |Cos(_,x) -> "cos("+x.code+")"
                |Tan(_,x) -> "tan("+x.code+")"
                |Asin(_,x) -> "asin("+x.code+")"
                |Acos(_,x) -> "acos("+x.code+")"
                |Atan(_,x) -> "atan("+x.code+")"
                |Atan2 (x,y) -> "atan2("+x.code+","+y.code+")"
                |Abs(_,x) -> "abs("+x.code+")"
                |Log(_,x) -> "log("+x.code+")"
                |Log10(_,x) -> "log10("+x.code+")"
                |Sqrt(_,x) -> "sqrt("+x.code+")"
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
                |Formula(_,s) -> s
                |_ ->
                    ""
                    
        ///<summary>優先度の高い型を選択</summary>
        static member ( %% ) (x:Expr,y:Expr) = 
            match x.etype,y.etype with
            |Zt,(Zt|Dt|It _) -> Zt
            |(Zt|Dt|It _),Zt -> Zt
            |Dt,(Dt|It _) -> Dt
            |(Dt|It _),Dt -> Dt
            |It a,It b -> It (if a>b then a else b)
            |_ -> Nt

        ///<summary>負号</summary>
        static member ( ~- ) (x:Expr) =
            match x with
            |Int_c 0|Dbl_c 0.0 -> x
            |Int_c x when x<0   -> (Int_c -x)
            |Dbl_c x when x<0.0 -> (Dbl_c -x)
            |Inv(_,v) -> v
            |Add _ -> Inv(x.etype,Par(x.etype,x))
            |Sub(_,x,y) -> y-x
            |_ -> Inv(x.etype,x)
            
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
                |Int_c x,_ when x<0   -> Inv(It 4,Int_c (-x)) + y
                (* (-x)+y *)
                |Dbl_c x,_ when x<0.0 -> Inv(Dt, Dbl_c (-x)) + y
                (* x+(-y) *)
                |_,Int_c y when y<0   -> x + Inv(It 4, Int_c (-y))
                (* x+(-y) *)
                |_,Dbl_c y when y<0.0 -> x + Inv(Dt, Dbl_c (-y))
                (* (-x)+(-y) *)
                |Inv(_,v1),Inv(_,v2) -> -(v1+v2)
                (* x+(-y) *)
                |_,Inv(_,v2) -> x-v2
                (* (-x)+y *)
                |Inv(_,v1),_ -> y-v1
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
                |Int_c v1,Add(_,Int_c v2,u2) -> (Int_c (v1+v2))+u2
                (* [小数定数]+Add([整数定数],[]) *)
                |Dbl_c v1,Add(_,Int_c v2,u2) -> (Dbl_c (v1+double v2))+u2
                (* [整数定数]+Add([小数定数],[]) *)
                |Int_c v1,Add(_,Dbl_c v2,u2) -> (Dbl_c (double v1+v2))+u2
                (* [小数定数]+Add([小数定数],[]) *)
                |Dbl_c v1,Add(_,Dbl_c v2,u2) -> (Dbl_c (v1+v2))+u2
                (* [整数定数]+Add([],[整数定数]) *)
                |Int_c v1,Add(_,u2,Int_c v2) -> u2+(Int_c (v1+v2))
                (* [小数定数]+Add([],[整数定数]) *)
                |Dbl_c v1,Add(_,u2,Int_c v2) -> u2+(Dbl_c (v1+double v2))
                (* [整数定数]+Add([],[小数定数]) *)
                |Int_c v1,Add(_,u2,Dbl_c v2) -> u2+(Dbl_c (double v1+v2))
                (* [小数定数]+Add([],[小数定数]) *)
                |Dbl_c v1,Add(_,u2,Dbl_c v2) -> u2+(Dbl_c (v1+v2))
                (* Add([整数定数],[])+[整数定数] *)
                |Add(_,Int_c v2,u2),Int_c v1 -> (Int_c (v1+v2))+u2
                (* Add([整数定数],[])+[小数定数] *)
                |Add(_,Int_c v2,u2),Dbl_c v1 -> (Dbl_c (v1+double v2))+u2
                (* Add([小数定数],[])+[整数定数] *)
                |Add(_,Dbl_c v2,u2),Int_c v1 -> (Dbl_c (double v1+v2))+u2
                (* Add([小数定数],[])+[小数定数] *)
                |Add(_,Dbl_c v2,u2),Dbl_c v1 -> (Dbl_c (v1+v2))+u2
                (* Add([],[整数定数])+[整数定数] *)
                |Add(_,u2,Int_c v2),Int_c v1 -> u2+(Int_c (v1+v2))
                (* Add([],[整数定数])+[小数定数] *)
                |Add(_,u2,Int_c v2),Dbl_c v1 -> u2+(Dbl_c (v1+double v2))
                (* Add([],[小数定数])+[整数定数] *)
                |Add(_,u2,Dbl_c v2),Int_c v1 -> u2+(Dbl_c (double v1+v2))
                (* Add([],[小数定数])+[小数定数] *)
                |Add(_,u2,Dbl_c v2),Dbl_c v1 -> u2+(Dbl_c (v1+v2))
                (* [整数定数]+Sub([整数定数],[]) *)
                |Int_c v1,Sub(_,Int_c v2,u2) -> (Int_c (v1+v2))-u2
                (* [小数定数]+Sub([整数定数],[]) *)
                |Dbl_c v1,Sub(_,Int_c v2,u2) -> (Dbl_c (v1+double v2))-u2
                (* [整数定数]+Sub([小数定数],[]) *)
                |Int_c v1,Sub(_,Dbl_c v2,u2) -> (Dbl_c (double v1+v2))-u2
                (* [小数定数]+Sub([小数定数],[]) *)
                |Dbl_c v1,Sub(_,Dbl_c v2,u2) -> (Dbl_c (v1+v2))-u2
                (* [整数定数]+Sub([],[整数定数]) *)
                |Int_c v1,Sub(_,u2,Int_c v2) -> u2+(Int_c (v1-v2))
                (* [小数定数]+Sub([],[整数定数]) *)
                |Dbl_c v1,Sub(_,u2,Int_c v2) -> u2+(Dbl_c (v1-double v2))
                (* [整数定数]+Sub([],[小数定数]) *)
                |Int_c v1,Sub(_,u2,Dbl_c v2) -> u2+(Dbl_c (double v1-v2))
                (* [小数定数]+Sub([],[小数定数]) *)
                |Dbl_c v1,Sub(_,u2,Dbl_c v2) -> u2+(Dbl_c (v1-v2))
                (* Sub([整数定数],[])+[整数定数] *)
                |Sub(_,Int_c v2,u2),Int_c v1 -> (Int_c (v1+v2))-u2
                (* Sub([整数定数],[])+[小数定数] *)
                |Sub(_,Int_c v2,u2),Dbl_c v1 -> (Dbl_c (v1+double v2))-u2
                (* Sub([小数定数],[])+[整数定数] *)
                |Sub(_,Dbl_c v2,u2),Int_c v1 -> (Dbl_c (double v1+v2))-u2
                (* Sub([小数定数],[])+[小数定数] *)
                |Sub(_,Dbl_c v2,u2),Dbl_c v1 -> (Dbl_c (v1+v2))-u2
                (* Sub([],[整数定数])+[整数定数] *)
                |Sub(_,u2,Int_c v2),Int_c v1 -> u2+(Int_c (v1-v2))
                (* Sub([],[整数定数])+[小数定数] *)
                |Sub(_,u2,Int_c v2),Dbl_c v1 -> u2+(Dbl_c (v1-double v2))
                (* Sub([],[小数定数])+[整数定数] *)
                |Sub(_,u2,Dbl_c v2),Int_c v1 -> u2+(Dbl_c (double v1-v2))
                (* Sub([],[小数定数])+[小数定数] *)
                |Sub(_,u2,Dbl_c v2),Dbl_c v1 -> u2+(Dbl_c (v1-v2))
                (* Par([])+[] *)
                |Par(_,v1),v2 -> v1+v2
                (* []+Par([]) *)
                |v1,Par(_,v2) -> v1+v2
                (* x+y *)
                |_ -> Add(x%%y,x,y)
            else
                Add(x%%y,x,y)
                
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
                |Int_c x,_ when x<0   -> Inv(It 4,Int_c (-x)) - y
                (* (-x)-y *)
                |Dbl_c x,_ when x<0.0 -> Inv(Dt,Dbl_c (-x)) - y
                (* x-(-y) *)
                |_,Int_c y when y<0   -> x - Inv(It 4,Int_c (-y))
                (* x-(-y) *)
                |_,Dbl_c y when y<0.0 -> x - Inv(Dt,Dbl_c (-y))
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
                |Inv(_,x),Inv(_,y) -> y-x
                (* (-x)-y *)
                |Inv(_,x),_ -> -(x+y)
                (* x-(-y) *)
                |_,Inv(_,y) -> x+y
                (* [整数定数]-Add([整数定数],[]) *)
                |Int_c v1,Add(_,Int_c v2,u2) -> (Int_c (v1-v2))-u2
                (* [小数定数]-Add([整数定数],[]) *)
                |Dbl_c v1,Add(_,Int_c v2,u2) -> (Dbl_c (v1-double v2))-u2
                (* [整数定数]-Add([小数定数],[]) *)
                |Int_c v1,Add(_,Dbl_c v2,u2) -> (Dbl_c (double v1-v2))-u2
                (* [小数定数]-Add([小数定数],[]) *)
                |Dbl_c v1,Add(_,Dbl_c v2,u2) -> (Dbl_c (v1-v2))-u2
                (* [整数定数]-Add([],[整数定数]) *)
                |Int_c v1,Add(_,u2,Int_c v2) -> (Int_c (v1-v2))-u2
                (* [小数定数]-Add([],[整数定数]) *)
                |Dbl_c v1,Add(_,u2,Int_c v2) -> (Dbl_c (v1-double v2))-u2
                (* [整数定数]-Add([],[小数定数]) *)
                |Int_c v1,Add(_,u2,Dbl_c v2) -> (Dbl_c (double v1-v2))-u2
                (* [小数定数]-Add([],[小数定数]) *)
                |Dbl_c v1,Add(_,u2,Dbl_c v2) -> (Dbl_c (v1-v2))-u2
                (* Add([整数定数],[])-[整数定数] *)
                |Add(_,Int_c v2,u2),Int_c v1 -> (Int_c (v2-v1))+u2
                (* Add([整数定数],[])-[小数定数] *)
                |Add(_,Int_c v2,u2),Dbl_c v1 -> (Dbl_c (double v2-v1))+u2
                (* Add([小数定数],[])-[整数定数] *)
                |Add(_,Dbl_c v2,u2),Int_c v1 -> (Dbl_c (v2-double v1))+u2
                (* Add([小数定数],[])-[小数定数] *)
                |Add(_,Dbl_c v2,u2),Dbl_c v1 -> (Dbl_c (v2-v1))+u2
                (* Add([],[整数定数])-[整数定数] *)
                |Add(_,u2,Int_c v2),Int_c v1 -> u2+(Int_c (v2-v1))
                (* Add([],[整数定数])-[小数定数] *)
                |Add(_,u2,Int_c v2),Dbl_c v1 -> u2+(Dbl_c (double v2-v1))
                (* Add([],[小数定数])-[整数定数] *)
                |Add(_,u2,Dbl_c v2),Int_c v1 -> u2+(Dbl_c (v2-double v1))
                (* Add([],[小数定数])-[小数定数] *)
                |Add(_,u2,Dbl_c v2),Dbl_c v1 -> u2+(Dbl_c (v2-v1))
                (* [整数定数]-Sub([整数定数],[]) *)
                |Int_c v1,Sub(_,Int_c v2,u2) -> u2+(Int_c (v1-v2))
                (* [小数定数]-Sub([整数定数],[]) *)
                |Dbl_c v1,Sub(_,Int_c v2,u2) -> u2+(Dbl_c (v1-double v2))
                (* [整数定数]-Sub([小数定数],[]) *)
                |Int_c v1,Sub(_,Dbl_c v2,u2) -> u2+(Dbl_c (double v1-v2))
                (* [小数定数]-Sub([小数定数],[]) *)
                |Dbl_c v1,Sub(_,Dbl_c v2,u2) -> u2+(Dbl_c (v1-v2))
                (* [整数定数]-Sub([],[整数定数]) *)
                |Int_c v1,Sub(_,u2,Int_c v2) -> (Int_c (v1+v2))-u2
                (* [小数定数]-Sub([],[整数定数]) *)
                |Dbl_c v1,Sub(_,u2,Int_c v2) -> (Dbl_c (v1+double v2))-u2
                (* [整数定数]-Sub([],[小数定数]) *)
                |Int_c v1,Sub(_,u2,Dbl_c v2) -> (Dbl_c (double v1+v2))-u2
                (* [小数定数]-Sub([],[小数定数]) *)
                |Dbl_c v1,Sub(_,u2,Dbl_c v2) -> (Dbl_c (v1+v2))-u2
                (* Sub([整数定数],[])-[整数定数] *)
                |Sub(_,Int_c v2,u2),Int_c v1 -> (Int_c (v2-v1))-u2
                (* Sub([整数定数],[])-[小数定数] *)
                |Sub(_,Int_c v2,u2),Dbl_c v1 -> (Dbl_c (double v2-v1))-u2
                (* Sub([小数定数],[])-[整数定数] *)
                |Sub(_,Dbl_c v2,u2),Int_c v1 -> (Dbl_c (v2-double v1))-u2
                (* Sub([小数定数],[])-[小数定数] *)
                |Sub(_,Dbl_c v2,u2),Dbl_c v1 -> (Dbl_c (v2-v1))-u2
                (* Sub([],[整数定数])-[整数定数] *)
                |Sub(_,u2,Int_c v2),Int_c v1 -> u2-(Int_c (v1+v2))
                (* Sub([],[整数定数])-[小数定数] *)
                |Sub(_,u2,Int_c v2),Dbl_c v1 -> u2-(Dbl_c (v1+double v2))
                (* Sub([],[小数定数])-[整数定数] *)
                |Sub(_,u2,Dbl_c v2),Int_c v1 -> u2-(Dbl_c (double v1+v2))
                (* Sub([],[小数定数])-[小数定数] *)
                |Sub(_,u2,Dbl_c v2),Dbl_c v1 -> u2-(Dbl_c (v1+v2))
                (* x-(y1+y2) *)
                |_,Add(_,y1,y2) -> x-y1-y2
                (* x-(y1-y2) *)
                |_,Sub(_,y1,y2) -> x-y1+y2
                (* x-y *)
                |_ -> Sub(x%%y,x,y)
            else
                Sub(x%%y,x,y)
                
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
                |Int_c x,_ when x<0   -> Inv(It 4,Int_c (-x)) * y
                (* (-x)*y *)
                |Dbl_c x,_ when x<0.0 -> Inv(Dt,Dbl_c (-x)) * y
                (* x*(-y) *)
                |_,Int_c y when y<0   -> x * Inv(It 4,Int_c (-y))
                (* x*(-y) *)
                |_,Dbl_c y when y<0.0 -> x * Inv(Dt,Dbl_c (-y))
                (* [負の整数定数]*y *)
                |Int_c x,_ when x<0   -> -(Int_c (-x)*y)
                (* [負の小数定数]*y *)
                |Dbl_c x,_ when x<0.0 -> -(Dbl_c (-x)*y)
                (* x*[負の整数定数] *)
                |_,Int_c y when y<0   -> -(x*Int_c (-y))
                (* x*[負の小数定数] *)
                |_,Dbl_c y when y<0.0 -> -(x*Dbl_c (-y))
                (* (-x)*(-y) *)
                |Inv(_,x),Inv(_,y) -> x*y
                (* x*(-y) *)
                |_,Inv(_,y) -> -(x*y)
                (* (-x)*y *)
                |Inv(_,x),_ -> -(x*y)
                (* [整数定数]*[整数定数] *)
                |Int_c x,Int_c y -> Int_c(x*y)
                (* [整数定数]*[小数定数] *)
                |Int_c x,Dbl_c y -> Dbl_c((double x)*y)
                (* [小数定数]*[整数定数] *)
                |Dbl_c x,Int_c y -> Dbl_c(x*(double y))
                (* [小数定数]*[小数定数] *)
                |Dbl_c x,Dbl_c y -> Dbl_c(x*y)
                (* [整数定数]*Mul([整数定数],[]) *)
                |Int_c v1,Mul(_,Int_c v2,u2) -> (Int_c (v1*v2))*u2
                (* [整数定数]*Mul([小数定数],[]) *)
                |Int_c v1,Mul(_,Dbl_c v2,u2) -> (Dbl_c (double v1*v2))*u2
                (* [小数定数]*Mul([整数定数],[]) *)
                |Dbl_c v1,Mul(_,Int_c v2,u2) -> (Dbl_c (v1*double v2))*u2
                (* [小数定数]*Mul([小数定数],[]) *)
                |Dbl_c v1,Mul(_,Dbl_c v2,u2) -> (Dbl_c (v1*v2))*u2
                (* Mul([整数定数],[])*[整数定数] *)
                |Mul(_,Int_c v2,u2),Int_c v1 -> (Int_c (v1*v2))*u2
                (* Mul([整数定数],[])*[小数定数] *)
                |Mul(_,Int_c v2,u2),Dbl_c v1 -> (Dbl_c (v1*double v2))*u2
                (* Mul([小数定数],[])*[整数定数] *)
                |Mul(_,Dbl_c v2,u2),Int_c v1 -> (Dbl_c (double v1*v2))*u2
                (* Mul([小数定数],[])*[小数定数] *)
                |Mul(_,Dbl_c v2,u2),Dbl_c v1 -> (Dbl_c (v1*v2))*u2
                (* [整数定数]*Div([整数定数],[]) *)
                |Int_c v1,Div(_,Int_c v2,u2) -> (Int_c (v1*v2))/u2
                (* [整数定数]*Div([小数定数],[]) *)
                |Int_c v1,Div(_,Dbl_c v2,u2) -> (Dbl_c (double v1*v2))/u2
                (* [小数定数]*Div([整数定数],[]) *)
                |Dbl_c v1,Div(_,Int_c v2,u2) -> (Dbl_c (v1*double v2))/u2
                (* [小数定数]*Div([小数定数],[]) *)
                |Dbl_c v1,Div(_,Dbl_c v2,u2) -> (Dbl_c (v1*v2))/u2
                (* Div([整数定数],[])*[整数定数] *)
                |Div(_,Int_c v2,u2),Int_c v1 -> (Int_c (v1*v2))/u2
                (* Div([整数定数],[])*[小数定数] *)
                |Div(_,Int_c v2,u2),Dbl_c v1 -> (Dbl_c (v1*double v2))/u2
                (* Div([小数定数],[])*[整数定数] *)
                |Div(_,Dbl_c v2,u2),Int_c v1 -> (Dbl_c (double v1*v2))/u2
                (* Div([小数定数],[])*[小数定数] *)
                |Div(_,Dbl_c v2,u2),Dbl_c v1 -> (Dbl_c (v1*v2))/u2
                (* x*(y1+y2) or x*(y1-y2) *)
                |_,(Add _|Sub _) -> x*Par(y.etype,y)
                (* (x1+x2)*y or (x1-x2)*y *)
                |(Add _|Sub _),_ -> Par(x.etype,x)*y
                (* x*y *)
                |_ -> Mul(x%%y,x,y)
            else
                Mul(x%%y,x,y)
                
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
                |Int_c x,_ when x<0   -> Inv(It 4,Int_c (-x)) / y
                (* (-x)/y *)
                |Dbl_c x,_ when x<0.0 -> Inv(Dt,Dbl_c (-x)) / y
                (* x/(-y) *)
                |_,Int_c y when y<0   -> x / Inv(It 4,Int_c (-y))
                (* x/(-y) *)
                |_,Dbl_c y when y<0.0 -> x / Inv(Dt,Dbl_c (-y))
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
                |Inv(_,x),Inv(_,y) -> x/y
                (* x/(-y) *)
                |_,Inv(_,y) -> -(x/y)
                (* (-x)/y *)
                |Inv(_,x),_ -> -(x/y)
                (* [整数定数]/Mul([整数定数],[]) *)
                |Int_c v1,Mul(_,Int_c v2,u2) when v1%v2=0 -> (Int_c (v1/v2))/u2
                (* [整数定数]/Mul([],[整数定数]) *)
                |Int_c v1,Mul(_,v2,Int_c u2) when v1%u2=0 -> (Int_c (v1/u2))/v2
                (* [整数定数]/Mul([整数定数],[]) *)
                |Int_c v1,Mul(_,Int_c v2,u2) -> (Dbl_c (double v1/double v2))/u2
                (* [整数定数]/Mul([],[整数定数]) *)
                |Int_c v1,Mul(_,v2,Int_c u2) -> (Dbl_c (double v1/double u2))/v2
                (* [整数定数]/Mul([小数定数],[]) *)
                |Int_c v1,Mul(_,Dbl_c v2,u2) -> (Dbl_c (double v1/v2))/u2
                (* [整数定数]/Mul([],[小数定数]) *)
                |Int_c v1,Mul(_,v2,Dbl_c u2) -> (Dbl_c (double v1/u2))/v2
                (* [小数定数]/Mul([整数定数],[]) *)
                |Dbl_c v1,Mul(_,Int_c v2,u2) -> (Dbl_c (v1/double v2))/u2
                (* [小数定数]/Mul([],[整数定数]) *)
                |Dbl_c v1,Mul(_,v2,Int_c u2) -> (Dbl_c (v1/double u2))/v2
                (* [小数定数]/Mul([小数定数],[]) *)
                |Dbl_c v1,Mul(_,Dbl_c v2,u2) -> (Dbl_c (v1/v2))/u2
                (* [小数定数]/Mul([],[小数定数]) *)
                |Dbl_c v1,Mul(_,v2,Dbl_c u2) -> (Dbl_c (v1/u2))/v2
                (* Mul([整数定数],[])/[整数定数] *)
                |Mul(_,Int_c v2,u2),Int_c v1 when v2%v1=0 -> (Int_c (v2/v1))*u2
                (* Mul([],[整数定数])/[整数定数] *)
                |Mul(_,v2,Int_c u2),Int_c v1 when u2%v1=0 -> v2*(Int_c (u2/v1))
                (* Mul([整数定数],[])/[整数定数] *)
                |Mul(_,Int_c v2,u2),Int_c v1 -> (Dbl_c (double v2/double v1))*u2
                (* Mul([],[整数定数])/[整数定数] *)
                |Mul(_,v2,Int_c u2),Int_c v1 -> v2*(Dbl_c (double u2/double v1))
                (* Mul([整数定数],[])/[小数定数] *)
                |Mul(_,Int_c v2,u2),Dbl_c v1 -> (Dbl_c (double v2/v1))*u2
                (* Mul([],[整数定数])/[小数定数] *)
                |Mul(_,v2,Int_c u2),Dbl_c v1 -> v2*(Dbl_c (double u2/v1))
                (* Mul([小数定数],[])/[整数定数] *)
                |Mul(_,Dbl_c v2,u2),Int_c v1 -> (Dbl_c (v2/double v1))*u2
                (* Mul([],[小数定数])/[整数定数] *)
                |Mul(_,v2,Dbl_c u2),Int_c v1 -> v2*(Dbl_c (u2/double v1))
                (* Mul([小数定数],[])/[小数定数] *)
                |Mul(_,Dbl_c v2,u2),Dbl_c v1 -> (Dbl_c (v2/v1))*u2
                (* Mul([],[小数定数])/[小数定数] *)
                |Mul(_,v2,Dbl_c u2),Dbl_c v1 -> v2*(Dbl_c (u2/v1))
                (* [整数定数]/Div([整数定数],[]) *)
                |Int_c v1,Div(_,Int_c v2,u2) when v1%v2=0 -> (Int_c (v1/v2))*u2
                (* [整数定数]/Div([],[整数定数]) *)
                |Int_c v1,Div(_,v2,Int_c u2) -> (Int_c (v1*u2))/v2
                (* [整数定数]/Div([整数定数],[]) *)
                |Int_c v1,Div(_,Int_c v2,u2) -> (Dbl_c (double v1/double v2))*u2
                (* [整数定数]/Div([小数定数],[]) *)
                |Int_c v1,Div(_,Dbl_c v2,u2) -> (Dbl_c (double v1/v2))*u2
                (* [整数定数]/Div([],[小数定数]) *)
                |Int_c v1,Div(_,v2,Dbl_c u2) -> (Dbl_c (double v1*u2))/v2
                (* [小数定数]/Div([整数定数],[]) *)
                |Dbl_c v1,Div(_,Int_c v2,u2) -> (Dbl_c (v1/double v2))*u2
                (* [小数定数]/Div([],[整数定数]) *)
                |Dbl_c v1,Div(_,v2,Int_c u2) -> (Dbl_c (v1*double u2))/v2
                (* [小数定数]/Div([小数定数],[]) *)
                |Dbl_c v1,Div(_,Dbl_c v2,u2) -> (Dbl_c (v1/v2))*u2
                (* [小数定数]/Div([],[小数定数]) *)
                |Dbl_c v1,Div(_,v2,Dbl_c u2) -> (Dbl_c (v1*u2))/v2
                (* Div([整数定数],[])/[整数定数] *)
                |Div(_,Int_c v2,u2),Int_c v1 when v2%v1=0 -> (Int_c (v2/v1))/u2
                (* Div([],[整数定数])/[整数定数] *)
                |Div(_,v2,Int_c u2),Int_c v1 -> v2/(Int_c (u2*v1))
                (* Div([整数定数],[])/[整数定数] *)
                |Div(_,Int_c v2,u2),Int_c v1 -> (Dbl_c (double v2/double v1))/u2
                (* Div([整数定数],[])/[小数定数] *)
                |Div(_,Int_c v2,u2),Dbl_c v1 -> (Dbl_c (double v2/v1))/u2
                (* Div([],[整数定数])/[小数定数] *)
                |Div(_,v2,Int_c u2),Dbl_c v1 -> v2/(Dbl_c (double u2*v1))
                (* Div([小数定数],[])/[整数定数] *)
                |Div(_,Dbl_c v2,u2),Int_c v1 -> (Dbl_c (v2/double v1))/u2
                (* Div([],[小数定数])/[整数定数] *)
                |Div(_,v2,Dbl_c u2),Int_c v1 -> v2/(Dbl_c (u2*double v1))
                (* Div([小数定数],[])/[小数定数] *)
                |Div(_,Dbl_c v2,u2),Dbl_c v1 -> (Dbl_c (v2/v1))/u2
                (* Div([],[小数定数])/[小数定数] *)
                |Div(_,v2,Dbl_c u2),Dbl_c v1 -> v2/(Dbl_c (u2*v1))
                (* x/(y1+y2+…) or x/(y1-y2) or x/(y1*y2) or x/(y1/y2) *)
                |_,(Add _|Sub _|Mul _|Div _) -> x/Par(y.etype,y)
                (* (x1+x2+…)/y or (x1-x2)/y *)
                |(Add _|Sub _),_ -> Par(x.etype,x)/y
                (* x/y *)
                |_ -> Div(x%%y,x,y)
            else
                Div(x%%y,x,y)
                
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
                |Inv(_,v1),Inv(_,v2) -> (v1/v2)
                |_,Inv(_,v2) -> -(x/v2)
                |Inv(_,v1),_ -> -(v1/y)
                |Mul(_,Int_c v2,u2),Int_c v1 when v2%v1=0 -> (Int_c (v2/v1))*u2
                |Mul(_,v2,Int_c u2),Int_c v1 when u2%v1=0 -> v2*(Int_c (u2/v1))
                |_,(Add _|Sub _|Mul _|Div _) -> x./Par(y.etype,y)
                |(Add _|Sub _),_ -> Par(x.etype,x)./y
                |_ -> Div(x%%y,x,y)
            else
              Div(x%%y,x,y)
              
        ///<summary>剰余</summary>
        static member ( % ) (x:Expr,y:Expr) : Expr =
                match isEqSimplify,x,y with
                |true,_,Int_c 0 -> NaN
                |true,Int_c 0,_ -> Int_c 0
                |true,_,Int_c 1 -> Int_c 0
                |true,Int_c v1,_ when v1<0   -> -((Int_c (-v1))%y)
                |true,_,Int_c v2 when v2<0   -> (x%(Int_c (-v2)))
                |true,Int_c v1,Int_c v2 -> Int_c(v1%v2)
                |true,_,(Add _|Sub _|Mul _|Div _) -> x%Par(y.etype,y)
                |true,(Add _|Sub _),_ -> Par(x.etype,x) % y
                |_ ->
                    match p.lang with
                    |F -> Formula (It 4, "mod("+x.code+","+y.code+")")
                    |C -> Formula (It 4, x.code+"%"+y.code)
                    |H -> Formula (It 4, "\\mathrm{mod}("+x.code+","+y.code+")")
                    |T -> Formula (It 4, "\\mathrm{mod}("+x.code+","+y.code+")")
                    
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
            |_,Int_c v1,_ when v1<0 -> Expr.pow(Par(x.etype,x),y)
            (* [負の小数定数]^y *)
            |_,Dbl_c v1,_ when v1<0.0 -> Expr.pow(Par(x.etype,x),y)
            (* x^[負の整数定数] *)
            |T,_,Int_c v2 when v2<0 -> Expr.pow(x,y)
            |_,_,Int_c v2 when v2<0 -> Expr.pow(x,Par(y.etype,y))
            (* x^[負の小数定数] *)
            |T,_,Dbl_c v2 when v2<0.0 -> Expr.pow(x,y)
            |_,_,Dbl_c v2 when v2<0.0 -> Expr.pow(x,Par(y.etype,y))
            (* x = [負号付] or (x1+x2+…) or (x1-x2) or (x1*x2) or (x1/x2) 
               y = [負号付] or (y1+y2+…) or (y1-y2) or (y1*y2) or (y1/y2) *)
            |T,(Inv _|Add _|Sub _|Mul _|Div _),(Inv _|Add _|Sub _|Mul _|Div _) -> Expr.pow(Par(x.etype,x),y)
            |_,(Inv _|Add _|Sub _|Mul _|Div _),(Inv _|Add _|Sub _|Mul _|Div _) -> Expr.pow(Par(x.etype,x),Par(y.etype,y))
            (* x = [負号付] or (x1+x2+…) or (x1-x2) or (x1*x2) or (x1/x2) *)
            |_,(Inv _|Add _|Sub _|Mul _|Div _),_ -> Expr.pow(Par(x.etype,x),y)
            (* y = [負号付] or (y1+y2+…) or (y1-y2) or (y1*y2) or (y1/y2) *)
            |_,_,(Inv _|Add _|Sub _|Mul _|Div _) -> Expr.pow(x,Par(y.etype,y))
            (* x^y *)
            |_ -> 
                match p.lang with
                |F -> Formula(x%%y, x.code+"**"+y.code)
                |C -> Formula(x%%y, "pow("+x.code+","+y.code+")")
                |H -> Formula(x%%y, x.code+"^{"+y.code+"}")
                |T -> Formula(x%%y, x.code+"^{"+y.code+"}")
                
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