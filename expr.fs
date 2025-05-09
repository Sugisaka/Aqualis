﻿(*
Copyright (c) 2022 Jun-ichiro Sugisaka

This software is released under the MIT License.
http://opensource.org/licenses/mit-license.php
*)
namespace Aqualis
    
    open System
    open Aqualis_base
    
    type bool0 =
        |False
        |True
        |Eq of num0*num0
        |NEq of num0*num0
        |Greater of num0*num0
        |GreaterEq of num0*num0
        |Less of num0*num0
        |LessEq of num0*num0
        |AND of bool0 list
        |OR of bool0 list
        |Null
        
        static member equal(x:bool0,y:bool0) =
            match x,y with
            |False,False -> true
            |True,True -> true
            |Eq(v1,u1),Eq(v2,u2) -> num0.equal(v1,v2) && num0.equal(u1,u2)
            |NEq(v1,u1),NEq(v2,u2) -> num0.equal(v1,v2) && num0.equal(u1,u2)
            |Greater(v1,u1),Greater(v2,u2) -> num0.equal(v1,v2) && num0.equal(u1,u2)
            |GreaterEq(v1,u1),GreaterEq(v2,u2) -> num0.equal(v1,v2) && num0.equal(u1,u2)
            |Less(v1,u1),Less(v2,u2) -> num0.equal(v1,v2) && num0.equal(u1,u2)
            |LessEq(v1,u1),LessEq(v2,u2) -> num0.equal(v1,v2) && num0.equal(u1,u2)
            |AND v1,AND u2 ->
                if v1.Length=u2.Length then
                    let rec cmp (v:List<bool0>,u:List<bool0>) =
                        match v,u with
                        |v::_,u::_ when (not <| bool0.equal(v,u)) -> false
                        |_::v0,_::u0 -> cmp (v0,u0)
                        |_ -> true
                    cmp (v1,u2)
                else
                    false
            |OR v1,OR u2 ->
                if v1.Length=u2.Length then
                    let rec cmp (v:List<bool0>,u:List<bool0>) =
                        match v,u with
                        |v::_,u::_ when (bool0.equal(v,u)) -> true
                        |_::v0,_::u0 -> cmp (v0,u0)
                        |_ -> false
                    cmp (v1,u2)
                else
                    false
            |Null,Null -> true
            |_ -> false
            
        member this.code with get() =
            match p.lang with
            |LaTeX ->
                match this with
                |True -> "true"
                |False -> "false"
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
                |AND(v) when (match List.tryFind (fun x -> bool0.equal(x,False)) v with |None -> false |Some _ -> true) -> False.code
                |OR(v)  when (match List.tryFind (fun x -> bool0.equal(x,True )) v with |None -> false |Some _ -> true) -> True.code
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
                |Null -> ""
            |HTML ->
                match this with
                |True -> "true"
                |False -> "false"
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
                |AND(v) when (match List.tryFind (fun x -> bool0.equal(x,False)) v with |None -> false |Some _ -> true) -> False.code
                |OR(v)  when (match List.tryFind (fun x -> bool0.equal(x,True )) v with |None -> false |Some _ -> true) -> True.code
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
                |Null -> ""
            |Fortran ->
                match this with
                |True -> ".true."
                |False -> ".false."
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
                |AND(v) when (match List.tryFind (fun x -> bool0.equal(x,False)) v with |None -> false |Some _ -> true) -> False.code
                |OR(v)  when (match List.tryFind (fun x -> bool0.equal(x,True )) v with |None -> false |Some _ -> true) -> True.code
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
                |Null -> ""
            |C99 ->
                match this with
                |True -> "1"
                |False -> "0"
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
                |AND(v) when (match List.tryFind (fun x -> bool0.equal(x,False)) v with |None -> false |Some _ -> true) -> False.code
                |OR(v)  when (match List.tryFind (fun x -> bool0.equal(x,True )) v with |None -> false |Some _ -> true) -> True.code
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
                |Null -> ""
            //　accはaccumulationの略で累積値
            |Python ->
                match this with
                |True -> "True"
                |False -> "False"
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
                |AND(v) when (match List.tryFind (fun x -> bool0.equal(x,False)) v with |None -> false |Some _ -> true) -> False.code
                |OR(v)  when (match List.tryFind (fun x -> bool0.equal(x,True )) v with |None -> false |Some _ -> true) -> True.code
                |AND(v) ->
                    //先に中身を評価
                    let uc = v |> List.map (fun q -> q.code)
                    let cat acc i =
                        if i<>0 then
                            acc + " and " + "(" + uc[i] + ")"
                        else acc + "(" + uc[i] + ")"
                    let code = List.fold cat "" [0..uc.Length-1]
                    //コード生成
                    code
                |OR(v) ->
                    //先に中身を評価
                    let uc = v |> List.map (fun q -> q.code)
                    let cat acc i =
                        if i<>0 then
                            acc + " or " + "(" + uc[i] + ")"
                        else acc + "(" + uc[i] + ")"
                    let code = List.fold cat "" [0..uc.Length-1]
                    //コード生成
                    code
                |Null -> ""
                
        static member (.<) (v1:bool0,v2:num0) = 
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
                Null
        static member (.<) (x:bool0,y:int) = x .< (Int_c y)
        static member (.<) (x:bool0,y:double) = x .< (Dbl_c y)
        
        static member (.<=) (v1:bool0,v2:num0) = 
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
                Null
        static member (.<=) (x:bool0,y:int) = x .<= (Int_c y)
        static member (.<=) (x:bool0,y:double) = x .<= (Dbl_c y)
        
        static member (.>) (v1:bool0,v2:num0) = 
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
                Null
        static member (.>) (x:bool0,y:int) = x .> (Int_c y)
        static member (.>) (x:bool0,y:double) = x .> (Dbl_c y)
        
        static member (.>=) (v1:bool0,v2:num0) = 
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
                Null
        static member (.>=) (x:bool0,y:int) = x .>= (Int_c y)
        static member (.>=) (x:bool0,y:double) = x .>= (Dbl_c y)
        
    ///<summary>変数オブジェクト</summary>
    and num0 =
        |Str_c of string
        |Int_c of int
        |Dbl_c of double
        |Var of Etype*string
        |Par of Etype*string*string*num0
        |Inv of Etype*num0
        |Add of Etype*num0*num0
        |Sub of Etype*num0*num0
        |Mul of Etype*num0*num0
        |Div of Etype*num0*num0
        |Pow of Etype*num0*num0
        |Exp of Etype*num0
        |Sin of Etype*num0
        |Cos of Etype*num0
        |Tan of Etype*num0
        |Asin of Etype*num0
        |Acos of Etype*num0
        |Atan of Etype*num0
        |Atan2 of num0*num0
        |Abs of Etype*num0
        |Log of Etype*num0
        |Log10 of Etype*num0
        |Sqrt of Etype*num0
        |Idx1 of Etype*string*num0
        |Idx2 of Etype*string*num0*num0
        |Idx3 of Etype*string*num0*num0*num0
        |Formula of Etype*string
        |Sum of Etype*num0*num0*(num0->num0)
        |Let of Etype*num0*num0
        |NaN
        
        member this.etype with get() =
            match this with
            |Str_c _ -> St
            |Int_c _ -> It 4
            |Dbl_c _ -> Dt
            |Var (t,_) -> t
            |Par (t,_,_,_) -> t
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
            |Idx1 (t,_,_) -> t
            |Idx2 (t,_,_,_) -> t
            |Idx3 (t,_,_,_,_) -> t
            |Formula (t,_) -> t
            |Sum (t,_,_,_) -> t
            |Let (t,_,_) -> t
            |NaN -> Nt
            
        static member internal looprange (i1:num0) = fun (i2:num0) -> fun code -> 
            match p.lang with
            |Fortran ->
                p.getloopvar <| fun counter ->
                    if p.isparmode then p.pvar.setVar(It 4,A0,counter,"")
                    p.codewrite("do "+counter+"="+i1.code+","+i2.code+"\n")
                    p.indentInc()
                    code(Var(It 4,counter))
                    p.indentDec()
                    p.codewrite("end do"+"\n")
            |C99 ->
                p.getloopvar <| fun counter ->
                    if p.isparmode then p.pvar.setVar(It 4,A0,counter,"")
                    p.codewrite("for("+counter+"="+i1.code+"; "+counter+"<="+i2.code+"; "+counter+"++)"+"\n")
                    p.codewrite("{"+"\n")
                    p.indentInc()
                    code(Var(It 4,counter))
                    p.indentDec()
                    p.codewrite("}"+"\n")
            |Python ->
                p.getloopvar <| fun counter ->
                    if p.isparmode then p.pvar.setVar(It 4,A0,counter,"")
                    p.codewrite("for "+counter+" in range("+i1.code+", "+i2.code+"+1, 1):"+"\n")
                    p.indentInc()
                    code(Var(It 4,counter))
                    p.indentDec()
            |_ -> ()
                
        ///<summary>一時変数の生成(再利用しない)</summary>
        static member internal ch(t:Etype) =
            match t with
            |It 4 -> Var(It 4,p.i_cache_var.getAutoVar())
            |Dt   -> Var(Dt  ,p.d_cache_var.getAutoVar())
            |Zt   -> Var(Zt  ,p.z_cache_var.getAutoVar())
            |_    -> NaN
            
        ///<summary>整数型一時変数の生成(式の比較用。変数宣言・再利用しない)</summary>
        static member internal chdum() =
            Var(It 4,p.dum_cache_var.getAutoVar())
            
        static member internal equal(x:num0,y:num0) =
            match x,y with
            |Var(t1,u1),Var(t2,u2) when t1=t2 && u1=u2 -> true
            |Int_c u1,Int_c u2 when u1=u2 -> true
            |Dbl_c u1,Dbl_c u2 when u1=u2 -> true
            |Str_c u1,Str_c u2 when u1=u2 -> true
            |Par(t1,_,_,u1),Par(t2,_,_,u2) when t1=t2 && num0.equal(u1,u2) -> true
            |Inv(t1,u1),Inv(t2,u2) when t1=t2 && num0.equal(u1,u2) -> true
            |Add(t1,u1,v1),Add(t2,u2,v2) when t1=t2 && ((num0.equal(u1,u2) && num0.equal(v1,v2))||(num0.equal(u1,v2) && num0.equal(v2,v1)))-> true
            |Sub(t1,u1,v1),Sub(t2,u2,v2) when t1=t2 && num0.equal(u1,u2) && num0.equal(v1,v2) -> true
            |Mul(t1,u1,v1),Mul(t2,u2,v2) when t1=t2 && ((num0.equal(u1,u2) && num0.equal(v1,v2))||(num0.equal(u1,v2) && num0.equal(v2,v1)))-> true
            |Div(t1,u1,v1),Div(t2,u2,v2) when t1=t2 && num0.equal(u1,u2) && num0.equal(v1,v2) -> true
            |Pow(t1,u1,v1),Pow(t2,u2,v2) when t1=t2 && num0.equal(u1,u2) && num0.equal(v1,v2) -> true
            |Exp(t1,u1),Exp(t2,u2) when t1=t2 && num0.equal(u1,u2) -> true
            |Sin(t1,u1),Sin(t2,u2) when t1=t2 && num0.equal(u1,u2) -> true
            |Cos(t1,u1),Cos(t2,u2) when t1=t2 && num0.equal(u1,u2) -> true
            |Tan(t1,u1),Tan(t2,u2) when t1=t2 && num0.equal(u1,u2) -> true
            |Asin(t1,u1),Asin(t2,u2) when t1=t2 && num0.equal(u1,u2) -> true
            |Acos(t1,u1),Acos(t2,u2) when t1=t2 && num0.equal(u1,u2) -> true
            |Atan(t1,u1),Atan(t2,u2) when t1=t2 && num0.equal(u1,u2) -> true
            |Atan2(u1,v1),Atan2(u2,v2) when num0.equal(u1,u2) && num0.equal(v1,v2) -> true
            |Abs(t1,u1),Abs(t2,u2) when t1=t2 && num0.equal(u1,u2) -> true
            |Log(t1,u1),Log(t2,u2) when t1=t2 && num0.equal(u1,u2) -> true
            |Log10(t1,u1),Log10(t2,u2) when t1=t2 && num0.equal(u1,u2) -> true
            |Sqrt(t1,u1),Sqrt(t2,u2) when t1=t2 && num0.equal(u1,u2) -> true
            |Idx1(t1,u1,nA1),Idx1(t2,u2,nA2) when t1=t2 && u1=u2 && num0.equal(nA1,nA2) -> true
            |Idx2(t1,u1,nA1,nB1),Idx2(t2,u2,nA2,nB2) when t1=t2 && u1=u2 && num0.equal(nA1,nA2) && num0.equal(nB1,nB2) -> true
            |Idx3(t1,u1,nA1,nB1,nC1),Idx3(t2,u2,nA2,nB2,nC2) when t1=t2 && u1=u2 && num0.equal(nA1,nA2) && num0.equal(nB1,nB2) && num0.equal(nC1,nC2) -> true
            |Formula(t1,u1),Formula(t2,u2) when t1=t2 && u1=u2 -> true
            |Sum(t1,n1a,n2a,f1),Sum(t2,n1b,n2b,f2) ->
                isEqSimplify <- false
                let n = num0.chdum()
                let u1 = f1 n
                let u2 = f2 n
                isEqSimplify <- true
                t1=t2 && num0.equal(n1a,n1b) && num0.equal(n2a,n2b) && num0.equal(u1,u2)
            |Let(t1,v1,u1),Let(t2,v2,u2) when t1=t2 && num0.equal(v1,v2) && num0.equal(u1,u2) -> true
            |NaN,NaN -> true
            |_ -> false
                
        member this.code with get() =
            match p.lang with
            |LaTeX ->
                match this with
                |Var(_,x) -> x
                |Int_c x -> p.ItoS(x)
                |Dbl_c x -> p.DtoS(x)
                |Str_c x -> "\""+x+"\""
                |Par(_,"(",")",x) -> "\\left("+x.code+"\\right)"
                |Par(_,popen,pclose,x) -> popen+x.code+pclose
                |Inv(_,x) -> "-"+x.code
                |Add(_,x,y) -> x.code+"+"+y.code
                |Sub(_,x,y) -> x.code+"-"+y.code
                |Mul(_,x,y) -> x.code+" "+y.code
                |Div(_,Par(_,_,_,x),Par(_,_,_,y)) when p.param.frac_style = 1 -> "("+x.code+")/("+y.code+")"
                |Div(_,Par(_,_,_,x),Par(_,_,_,y)) -> "\\frac{\\displaystyle "+x.code+"}{\\displaystyle "+y.code+"}"
                |Div(_,Par(_,_,_,x),y) when p.param.frac_style = 1 -> "("+x.code+")/"+y.code
                |Div(_,Par(_,_,_,x),y) -> "\\frac{\\displaystyle "+x.code+"}{\\displaystyle "+y.code+"}"
                |Div(_,x,Par(_,_,_,y)) when p.param.frac_style = 1 -> x.code+"/("+y.code+")"
                |Div(_,x,Par(_,_,_,y)) -> "\\frac{\\displaystyle "+x.code+"}{\\displaystyle "+y.code+"}"
                |Div(_,x,y) when p.param.frac_style = 1 -> x.code+"/"+y.code
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
                |Idx1(_,u,n1) -> u+"["+n1.code+"]"
                |Idx2(_,u,n1,n2) -> u+"["+n1.code+","+n2.code+"]"
                |Idx3(_,u,n1,n2,n3) -> u+"["+n1.code+","+n2.code+","+n3.code+"]"
                |Formula(_,s) -> s
                |Sum(_,n1,n2,f) ->
                    let mutable n = ""
                    p.getloopvar <| fun counter ->
                        n <- counter
                    "\\sum_{"+n+"="+n1.code+"}^{"+n2.code+"}"+(f (Var(It 4,n))).code
                |Let(_,v,_) -> v.code
                |NaN -> "NaN"
            |HTML ->
                match this with
                |Var(_,x) -> x
                |Int_c x -> p.ItoS(x)
                |Dbl_c x -> p.DtoS(x)
                |Str_c x -> "\""+x+"\""
                |Par(_,"(",")",x) -> "\\left("+x.code+"\\right)"
                |Par(_,popen,pclose,x) -> popen+x.code+pclose
                |Inv(_,x) -> "-"+x.code
                |Add (_,x,y) -> x.code+"+"+y.code
                |Sub (_,x,y) -> x.code+"-"+y.code
                |Mul (_,x,y) -> x.code+" "+y.code
                |Div(_,Par(_,_,_,x),Par(_,_,_,y)) when p.param.frac_style = 1 -> "("+x.code+")/("+y.code+")"
                |Div(_,Par(_,_,_,x),Par(_,_,_,y)) -> "\\frac{\\displaystyle "+x.code+"}{\\displaystyle "+y.code+"}"
                |Div(_,Par(_,_,_,x),y) when p.param.frac_style = 1 -> "("+x.code+")/"+y.code
                |Div(_,Par(_,_,_,x),y) -> "\\frac{\\displaystyle "+x.code+"}{\\displaystyle "+y.code+"}"
                |Div(_,x,Par(_,_,_,y)) when p.param.frac_style = 1 -> x.code+"/("+y.code+")"
                |Div(_,x,Par(_,_,_,y)) -> "\\frac{\\displaystyle "+x.code+"}{\\displaystyle "+y.code+"}"
                |Div(_,x,y) when p.param.frac_style = 1 -> x.code+"/"+y.code
                |Div(_,x,y) -> "\\frac{\\displaystyle "+x.code+"}{\\displaystyle "+y.code+"}"
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
                |Idx1(_,u,n1) -> u+"["+n1.code+"]"
                |Idx2(_,u,n1,n2) -> u+"["+n1.code+","+n2.code+"]"
                |Idx3(_,u,n1,n2,n3) -> u+"["+n1.code+","+n2.code+","+n3.code+"]"
                |Formula(_,s) -> s
                |Sum(_,n1,n2,f) ->
                    let mutable n = ""
                    p.getloopvar <| fun counter ->
                        n <- counter
                    "\\sum_{"+n+"="+n1.code+"}^{"+n2.code+"}"+(f (Var(It 4,n))).code
                |Let(_,v,_) -> v.code
                |NaN -> "NaN"
            |Fortran ->
                match this with
                |Var(_,x) -> x
                |Int_c x -> p.ItoS(x)
                |Dbl_c x -> p.DtoS(x)
                |Str_c x -> "\""+x+"\""
                |Par(_,_,_,x) -> "("+x.code+")"
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
                |Idx1(_,u,n1) -> u+"("+(n1+Int_c 1).code+")"
                |Idx2(_,u,n1,n2) -> u+"("+(n1+Int_c 1).code+","+(n2+Int_c 1).code+")"
                |Idx3(_,u,n1,n2,n3) -> u+"("+(n1+Int_c 1).code+","+(n2+Int_c 1).code+","+(n3+Int_c 1).code+")"
                |Formula(_,s) -> s
                |Sum(t,n1,n2,f) ->
                    let g = num0.ch t
                    g.clear()
                    num0.looprange n1 n2 <| fun n ->
                        g <== g + (f n)
                    g.code
                |Let(_,v,_) -> v.code
                |NaN -> "NaN"
            |C99 ->
                match this with
                |Var(_,x) -> x
                |Int_c x -> p.ItoS(x)
                |Dbl_c x -> p.DtoS(x)
                |Str_c x -> "\""+x+"\""
                |Par(_,_,_,x) -> "("+x.code+")"
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
                |Abs(_,x) -> 
                    match x.etype with
                    |Zt ->  "cabs("+x.code+")"
                    |Dt ->  "fabs("+x.code+")"
                    |_  ->  "abs("+x.code+")"
                |Log(Zt,x) -> "clog("+x.code+")"
                |Log(_,x) -> "log("+x.code+")"
                |Log10(Zt,x) -> "clog10("+x.code+")"
                |Log10(_,x) -> "log10("+x.code+")"
                |Sqrt(Zt,x) -> "csqrt("+x.code+")"
                |Sqrt(_,x) -> "sqrt("+x.code+")"
                |Idx1(_,u,n1) -> u+"["+n1.code+"]"
                |Idx2(_,u,n1,n2) -> 
                    let size1 = Var(It 4,u+"_size[0]")
                    u+"["+(n2*size1+n1).code+"]"
                |Idx3(_,u,n1,n2,n3) -> 
                    let size1 = Var(It 4,u+"_size[0]")
                    let size2 = Var(It 4,u+"_size[1]")
                    u+"["+(n3*size1*size2+n2*size1+n1).code+"]"
                |Formula(_,s) -> s
                |Sum(t,n1,n2,f) ->
                    let g = num0.ch t
                    g.clear()
                    num0.looprange n1 n2 <| fun n ->
                        g <== g + (f n)
                    g.code
                |Let(_,v,_) -> v.code
                |NaN -> "NaN"
            |Python ->
                match this with
                |Var(_,x) -> x
                |Int_c x -> p.ItoS(x)
                |Dbl_c x -> p.DtoS(x)
                |Str_c x -> "\""+x+"\""
                |Par(_,_,_,x) -> "("+x.code+")"
                |Inv(_,x) -> "-"+x.code
                |Add (_,x,y) -> x.code+"+"+y.code
                |Sub (_,x,y) -> x.code+"-"+y.code
                |Mul (_,x,y) -> x.code+"*"+y.code
                |Div (It _,x,y) -> x.code+"//"+y.code     
                |Div (_,x,y) -> x.code+"/"+y.code
                |Pow (_,x,y) -> "math.pow("+x.code+","+y.code+")"
                |Exp(Zt,x) -> "cmath.exp("+x.code+")"
                |Sin(Zt,x) -> "cmath.sin("+x.code+")"
                |Cos(Zt,x) -> "cmath.cos("+x.code+")"
                |Tan(Zt,x) -> "cmath.tan("+x.code+")"
                |Asin(Zt,x) -> "cmath.asin("+x.code+")"
                |Acos(Zt,x) -> "cmath.acos("+x.code+")"
                |Atan(Zt,x) -> "cmath.atan("+x.code+")"
                |Exp(_,x) -> "math.exp("+x.code+")"
                |Sin(_,x) -> "math.sin("+x.code+")"
                |Cos(_,x) -> "math.cos("+x.code+")"
                |Tan(_,x) -> "math.tan("+x.code+")"
                |Asin(_,x) -> "math.asin("+x.code+")"
                |Acos(_,x) -> "math.acos("+x.code+")"
                |Atan(_,x) -> "math.atan("+x.code+")"
                |Atan2 (x,y) -> "math.atan2("+x.code+","+y.code+")"
                |Abs(_,x) -> "abs("+x.code+")"
                |Log(Zt,x) -> "cmath.log("+x.code+")"
                |Log(_,x) -> "math.log("+x.code+")"
                |Log10(Zt,x) -> "cmath.log10("+x.code+")"
                |Log10(_,x) -> "math.log10("+x.code+")"
                |Sqrt(Zt,x) -> "cmath.sqrt("+x.code+")"
                |Sqrt(_,x) -> "math.sqrt("+x.code+")"
                //↓一応いじったけど、不安が残る
                |Idx1(_,u,n1) -> u+"["+n1.code+"]"
                |Idx2(_,u,n1,n2) -> 
                    u+"["+n1.code+","+n2.code+"]"
                |Idx3(_,u,n1,n2,n3) -> 
                    u+"["+n1.code+","+n2.code+","+n3.code+"]"
                |Formula(_,s) -> s
                |Sum(t,n1,n2,f) ->
                    let g = num0.ch t
                    g.clear()
                    num0.looprange n1 n2 <| fun n ->
                        g <== g + (f n)
                    g.code
                |Let(_,v,_) -> v.code
                |NaN -> "NaN"
                
        ///<summary>Letで事前に登録された変数に値を保存</summary>
        member this.eval() =
            let mutable c:List<num0> = []
            let rec ev0 (g:num0) =
                match g with
                |Str_c _ -> false
                |Int_c _ -> false
                |Dbl_c _ -> false
                |Var _ -> false
                |Par(_,_,_,v) -> ev0 v
                |Inv(_,v) -> ev0 v
                |Add(_,u,v) ->
                    ev0 u || ev0 v
                |Sub(_,u,v) ->
                    ev0 u || ev0 v
                |Mul(_,u,v) ->
                    ev0 u || ev0 v
                |Div(_,u,v) ->
                    ev0 u || ev0 v
                |Pow(_,u,v) ->
                    ev0 u || ev0 v
                |Exp(_,v) ->
                    ev0 v
                |Sin(_,v) ->
                    ev0 v
                |Cos(_,v) ->
                    ev0 v
                |Tan(_,v) ->
                    ev0 v
                |Asin(_,v) ->
                    ev0 v
                |Acos(_,v) ->
                    ev0 v
                |Atan(_,v) ->
                    ev0 v
                |Atan2(u,v) ->
                    ev0 u || ev0 v
                |Abs(_,v) ->
                    ev0 v
                |Log(_,v) ->
                    ev0 v
                |Log10(_,v) ->
                    ev0 v
                |Sqrt(_,v) ->
                    ev0 v
                |Idx1(_,_,n1) ->
                    ev0 n1
                |Idx2(_,_,n1,n2) ->
                    ev0 n1 || ev0 n2
                |Idx3(_,_,n1,n2,n3) ->
                    ev0 n1 || ev0 n2 || ev0 n3
                |Formula _ -> false
                |Sum(_,n1,n2,f) ->
                    ev0 (f <| Int_c 0)
                |Let(_,v,u) -> 
                    true
                |NaN -> false

            let rec ev (g:num0) =
                match g with
                |Str_c _ -> ()
                |Int_c _ -> ()
                |Dbl_c _ -> ()
                |Var _ -> ()
                |Par(_,_,_,v) -> ev v
                |Inv(_,v) -> ev v
                |Add(_,u,v) ->
                    ev u
                    ev v
                |Sub(_,u,v) ->
                    ev u
                    ev v
                |Mul(_,u,v) ->
                    ev u
                    ev v
                |Div(_,u,v) ->
                    ev u
                    ev v
                |Pow(_,u,v) ->
                    ev u
                    ev v
                |Exp(_,v) ->
                    ev v
                |Sin(_,v) ->
                    ev v
                |Cos(_,v) ->
                    ev v
                |Tan(_,v) ->
                    ev v
                |Asin(_,v) ->
                    ev v
                |Acos(_,v) ->
                    ev v
                |Atan(_,v) ->
                    ev v
                |Atan2(u,v) ->
                    ev u
                    ev v
                |Abs(_,v) ->
                    ev v
                |Log(_,v) ->
                    ev v
                |Log10(_,v) ->
                    ev v
                |Sqrt(_,v) ->
                    ev v
                |Idx1(_,_,n1) ->
                    ev n1
                |Idx2(_,_,n1,n2) ->
                    ev n1
                    ev n2
                |Idx3(_,_,n1,n2,n3) ->
                    ev n1
                    ev n2
                    ev n3
                |Formula _ -> ()
                |Sum(_,n1,n2,f) ->
                    if (ev0 (f <| Int_c 0)) then
                        num0.looprange n1 n2 <| fun n ->
                            ev (f n)
                |Let(_,v,u) -> 
                    ev u
                    printfn "%s %s" (g.ToString()) g.code
                    match List.tryFind (fun h -> num0.equal(h,g)) c with
                    |None ->
                        //評価済みでない式のみ処理
                        v <== u
                    |Some _ -> ()
                    c <- g::c
                |NaN -> ()
            ev this
            
        ///<summary>優先度の高い型を選択</summary>
        static member ( %% ) (x:num0,y:num0) = 
            match x.etype,y.etype with
            |Zt,(Zt|Dt|It _) -> Zt
            |(Zt|Dt|It _),Zt -> Zt
            |Dt,(Dt|It _) -> Dt
            |(Dt|It _),Dt -> Dt
            |It a,It b -> It (if a>b then a else b)
            |_ -> Nt

        static member internal todouble(x:num0) = 
            let rec dbl (e:num0) =
                match p.lang,e with
                |_,Int_c x  -> Dbl_c (double x)
                |_,Dbl_c x  -> Dbl_c x
                |_,Add(_,a,b) -> (dbl a)+(dbl b)
                |_,Sub(_,a,b) -> (dbl a)-(dbl b)
                |_,Mul(_,a,b) -> (dbl a)*(dbl b)
                |_,Div(_,a,b) -> (dbl a)/(dbl b)
                |Fortran,_          -> Formula(Dt,"dble("+e.code+")")
                |LaTeX,_          -> Formula(Dt,"\\mathrm{double}("+e.code+")")
                |HTML,_          -> Formula(Dt,"\\mathrm{double}("+e.code+")")
                |Python,_          -> Formula(Dt,"float("+e.code+")")
                |_,_          -> Formula(Dt,"(double)("+e.code+")")
            dbl x
            
        ///<summary>負号</summary>
        static member ( ~- ) (x:num0) =
            match x with
            |Int_c 0|Dbl_c 0.0 -> x
            |Int_c x when x<0   -> (Int_c -x)
            |Dbl_c x when x<0.0 -> (Dbl_c -x)
            |Inv(_,v) -> v
            |Add _ -> Inv(x.etype,Par(x.etype,"(",")",x))
            |Sub(_,x,y) -> y-x
            |_ -> Inv(x.etype,x)
            
        ///<summary>加算</summary>
        static member ( + ) (x:num0,y:num0) : num0 =
            if isEqSimplify then
                match x,y with
                (* x+0 *)
                |_,Int_c 0|_,Dbl_c 0.0 -> x
                (* 0+y *)
                |Int_c 0,_|Dbl_c 0.0,_ -> y
                (* x+x *)
                |v1,v2 when num0.equal(v1,v2) -> (Int_c 2)*v1
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
                |Par(_,_,_,v1),v2 -> v1+v2
                (* []+Par([]) *)
                |v1,Par(_,_,_,v2) -> v1+v2
                (* x+y *)
                |_ -> Add(x%%y,x,y)
            else
                Add(x%%y,x,y)
                
        static member ( + ) (x:num0,y:double) = x+(Dbl_c y)
        static member ( + ) (x:num0,y:int) = x+(Int_c y)
        static member ( + ) (x:double,y:num0) = (Dbl_c x)+y
        static member ( + ) (x:int,y:num0) = (Int_c x)+y
        
        ///<summary>減算</summary>
        static member ( - ) (x:num0,y:num0) : num0 =
            if isEqSimplify then
                match x,y with
                (* x-0 *)
                |_,Int_c 0|_,Dbl_c 0.0 -> x
                (* 0-y *)
                |Int_c 0,_|Dbl_c 0.0,_ -> -y
                (* x-x *)
                |v1,v2 when num0.equal(v1,v2) -> Int_c 0
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
                
        static member ( - ) (x:num0,y:double) = x-(Dbl_c y)
        static member ( - ) (x:num0,y:int) = x-(Int_c y)
        static member ( - ) (x:double,y:num0) = (Dbl_c x)-y
        static member ( - ) (x:int,y:num0) = (Int_c x)-y
        
        ///<summary>乗算</summary>
        static member ( * ) (x:num0,y:num0) : num0 =
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
                |_,(Add _|Sub _) -> x*Par(y.etype,"(",")",y)
                (* (x1+x2)*y or (x1-x2)*y *)
                |(Add _|Sub _),_ -> Par(x.etype,"(",")",x)*y
                (* x*y *)
                |_ -> Mul(x%%y,x,y)
            else
                Mul(x%%y,x,y)
                
        static member ( * ) (x:num0,y:double) = x*(Dbl_c y)
        static member ( * ) (x:num0,y:int) = x*(Int_c y)
        static member ( * ) (x:double,y:num0) = (Dbl_c x)*y
        static member ( * ) (x:int,y:num0) = (Int_c x)*y
        
        ///<summary>除算</summary>
        static member ( / ) (x:num0,y:num0) : num0 =
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
                |v1,v2 when num0.equal(v1,v2) -> Int_c 1
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
                |_,(Add _|Sub _|Mul _|Div _) -> x/Par(y.etype,"(",")",y)
                (* (x1+x2+…)/y or (x1-x2)/y *)
                |(Add _|Sub _),_ -> Par(x.etype,"(",")",x)/y
                |_ when x.etype=(It 4) && y.etype=(It 4) -> num0.todouble(x)/num0.todouble(y)
                (* x/y *)
                |_ -> Div(x%%y,x,y)
            else
                Div(x%%y,x,y)
                
        static member ( / ) (x:num0,y:double) = x/(Dbl_c y)
        static member ( / ) (x:num0,y:int) = x/Dbl_c(double y)
        static member ( / ) (x:double,y:num0) = (Dbl_c x)/y
        static member ( / ) (x:int,y:num0) = Dbl_c(double x)/y
        
        ///<summary>整数同士の除算(剰余無視)</summary>
        static member ( ./ ) (x:num0,y:num0) : num0 =
            if isEqSimplify then
                match (x,y) with
                |_,Int_c 0 -> NaN
                |Int_c 0,_ -> Int_c 0
                |_,Int_c 1 -> x
                |v1,v2 when num0.equal(v1,v2) -> Int_c 1
                |Int_c v1,Int_c v2 when v1<0 && v2<0  -> Int_c((-v1)/(-v2))
                |Int_c v1,_ when v1<0   -> -(Int_c (-v1)/y)
                |_,Int_c v2 when v2<0   -> -(x/Int_c (-v2))
                |Int_c v1,Int_c v2 -> Int_c(v1/v2)
                |Inv(_,v1),Inv(_,v2) -> (v1/v2)
                |_,Inv(_,v2) -> -(x/v2)
                |Inv(_,v1),_ -> -(v1/y)
                |Mul(_,Int_c v2,u2),Int_c v1 when v2%v1=0 -> (Int_c (v2/v1))*u2
                |Mul(_,v2,Int_c u2),Int_c v1 when u2%v1=0 -> v2*(Int_c (u2/v1))
                |_,(Add _|Sub _|Mul _|Div _) -> x./Par(y.etype,"(",")",y)
                |(Add _|Sub _),_ -> Par(x.etype,"(",")",x)./y
                |_ -> Div(x%%y,x,y)
            else
                Div(x%%y,x,y)
                    
        static member ( ./ ) (x:num0,y:int) = x./(Int_c y)
        static member ( ./ ) (x:int,y:num0) = (Int_c x)./y
        
        ///<summary>剰余</summary>
        static member ( % ) (x:num0,y:num0) : num0 =
                match isEqSimplify,x,y with
                |true,_,Int_c 0 -> NaN
                |true,Int_c 0,_ -> Int_c 0
                |true,_,Int_c 1 -> Int_c 0
                |true,Int_c v1,_ when v1<0   -> -((Int_c (-v1))%y)
                |true,_,Int_c v2 when v2<0   -> (x%(Int_c (-v2)))
                |true,Int_c v1,Int_c v2 -> Int_c(v1%v2)
                |true,_,(Add _|Sub _|Mul _|Div _) -> x%Par(y.etype,"(",")",y)
                |true,(Add _|Sub _),_ -> Par(x.etype,"(",")",x) % y
                |_ ->
                    match p.lang with
                    |Fortran -> Formula (It 4, "mod("+x.code+","+y.code+")")
                    |C99 -> Formula (It 4, x.code+"%"+y.code)
                    |HTML -> Formula (It 4, "\\mathrm{mod}("+x.code+","+y.code+")")
                    |LaTeX -> Formula (It 4, "\\mathrm{mod}("+x.code+","+y.code+")")
                    |Python -> Formula (It 4, x.code+"%"+y.code)
                    
        static member ( % ) (x:num0,y:int) = x%(Int_c y)
        static member ( % ) (x:int,y:num0) = (Int_c x)%y
        
        ///<summary>累乗</summary>
        static member powr(x:num0, y:num0) =
            match p.lang,x,y with
            (* 0^0 *)
            |_,(Int_c 0|Dbl_c 0.0),(Int_c 0|Dbl_c 0.0) -> num0.NaN
            (* 0^y *)
            |_,(Int_c 0|Dbl_c 0.0),_ -> Int_c 0
            (* x^0 *)
            |_,_,(Int_c 0|Dbl_c 0.0) -> Int_c 1
            (* [整数定数]^[整数定数] *)
            |(Fortran|C99|Python),Int_c v1,Int_c v2 -> Dbl_c(double(v1)**double(v2))
            (* [整数定数]^[小数定数] *)
            |(Fortran|C99|Python),Int_c v1,Dbl_c v2 -> Dbl_c((double v1)**v2)
            (* [小数定数]^[整数定数] *)
            |(Fortran|C99|Python),Dbl_c v1,Int_c v2 -> Dbl_c(v1**(double v2))
            (* [小数定数]^[小数定数] *)
            |(Fortran|C99|Python),Dbl_c v1,Dbl_c v2 -> Dbl_c(v1**v2)
            (* [負の整数定数]^y *)
            |_,Int_c v1,_ when v1 < 0 -> num0.powr(Par(x.etype,"(",")",x),y)
            (* [負の小数定数]^y *)
            |_,Dbl_c v1,_ when v1 < 0.0 -> num0.powr(Par(x.etype,"(",")",x),y)
            (* x^[負の整数定数] *)
            |LaTeX,_,Int_c v2 when v2 < 0 -> num0.powr(x,y)
            |_,_,Int_c v2 when v2 < 0 -> num0.powr(x,Par(y.etype,"(",")",y))
            (* x^[負の小数定数] *)
            |LaTeX,_,Dbl_c v2 when v2 < 0.0 -> num0.powr(x,y)
            |_,_,Dbl_c v2 when v2 < 0.0 -> num0.powr(x,Par(y.etype,"(",")",y))
            (* x = [負号付] or (x1+x2+…) or (x1-x2) or (x1*x2) or (x1/x2) 
               y = [負号付] or (y1+y2+…) or (y1-y2) or (y1*y2) or (y1/y2) *)
            |LaTeX,(Inv _|Add _|Sub _|Mul _|Div _),(Inv _|Add _|Sub _|Mul _|Div _) -> num0.powr(Par(x.etype,"(",")",x),y)
            |_,(Inv _|Add _|Sub _|Mul _|Div _),(Inv _|Add _|Sub _|Mul _|Div _) -> num0.powr(Par(x.etype,"(",")",x),Par(y.etype,"(",")",y))
            (* x = [負号付] or (x1+x2+…) or (x1-x2) or (x1*x2) or (x1/x2) *)
            |_,(Inv _|Add _|Sub _|Mul _|Div _),_ -> num0.powr(Par(x.etype,"(",")",x),y)
            (* y = [負号付] or (y1+y2+…) or (y1-y2) or (y1*y2) or (y1/y2) *)
            |_,_,(Inv _|Add _|Sub _|Mul _|Div _) -> num0.powr(x,Par(y.etype,"(",")",y))
            (* x^y *)
            |_ -> Pow(x%%y, x,y)
            
        ///<summary>等号</summary>
        static member (.=) (v1:num0,v2:num0) = 
            match v1,v2 with
            |Int_c a,Int_c b when a=b -> True
            |Int_c _,Int_c _ -> False
            |Dbl_c a,Dbl_c b when a=b -> True
            |Dbl_c _,Dbl_c _ -> False
            |_ -> Eq(v1,v2)
        static member (.=) (x:int,y:num0) = (Int_c x) .= y
        static member (.=) (x:double,y:num0) = (Dbl_c x) .= y
        static member (.=) (x:num0,y:int) = x .= (Int_c y)
        static member (.=) (x:num0,y:double) = x .= (Dbl_c y)
        ///<summary>不等号</summary>
        static member (.=/) (v1:num0,v2:num0) = 
            match v1,v2 with
            |Int_c a,Int_c b when a<>b -> True
            |Int_c _,Int_c _ -> False
            |Dbl_c a,Dbl_c b when a<>b -> True
            |Dbl_c _,Dbl_c _ -> False
            |_ -> NEq(v1,v2)
        static member (.=/) (x:int,y:num0) = (Int_c x) .=/ y
        static member (.=/) (x:double,y:num0) = (Dbl_c x) .=/ y
        static member (.=/) (x:num0,y:int) = x .=/ (Int_c y)
        static member (.=/) (x:num0,y:double) = x .=/ (Dbl_c y)
        ///<summary>比較（より小）</summary>
        static member (.<) (v1:num0,v2:num0) = 
            match v1,v2 with
            |Int_c a,Int_c b when a < b -> True
            |Int_c _,Int_c _ -> False
            |Dbl_c a,Dbl_c b when a < b -> True
            |Dbl_c _,Dbl_c _ -> False
            |_ -> Less(v1,v2)
        static member (.<) (x:int,y:num0) = (Int_c x) .< y
        static member (.<) (x:double,y:num0) = (Dbl_c x) .< y
        static member (.<) (x:num0,y:int) = x .< (Int_c y)
        static member (.<) (x:num0,y:double) = x .< (Dbl_c y)
        
        ///<summary>比較（以下）</summary>
        static member (.<=) (v1:num0,v2:num0) = 
            match v1,v2 with
            |Int_c a,Int_c b when a<=b -> True
            |Int_c _,Int_c _ -> False
            |Dbl_c a,Dbl_c b when a<=b -> True
            |Dbl_c _,Dbl_c _ -> False
            |_ -> LessEq(v1,v2)
        static member (.<=) (x:int,y:num0) = (Int_c x) .<= y
        static member (.<=) (x:double,y:num0) = (Dbl_c x) .<= y
        static member (.<=) (x:num0,y:int) = x .<= (Int_c y)
        static member (.<=) (x:num0,y:double) = x .<= (Dbl_c y)
        
        ///<summary>比較（より大）</summary>
        static member (.>) (v1:num0,v2:num0) = 
            match v1,v2 with
            |Int_c a,Int_c b when a>b -> True
            |Int_c _,Int_c _ -> False
            |Dbl_c a,Dbl_c b when a>b -> True
            |Dbl_c _,Dbl_c _ -> False
            |_ -> Greater(v1,v2)        
        static member (.>) (x:int,y:num0) = (Int_c x) .> y
        static member (.>) (x:double,y:num0) = (Dbl_c x) .> y
        static member (.>) (x:num0,y:int) = x .> (Int_c y)
        static member (.>) (x:num0,y:double) = x .> (Dbl_c y)
        
        ///<summary>比較（以上）</summary>
        static member (.>=) (v1:num0,v2:num0) = 
            match v1,v2 with
            |Int_c a,Int_c b when a>=b -> True
            |Int_c _,Int_c _ -> False
            |Dbl_c a,Dbl_c b when a>=b -> True
            |Dbl_c _,Dbl_c _ -> False
            |_ -> GreaterEq(v1,v2)
        static member (.>=) (x:int,y:num0) = (Int_c x) .>= y
        static member (.>=) (x:double,y:num0) = (Dbl_c x) .>= y
        static member (.>=) (x:num0,y:int) = x .>= (Int_c y)
        static member (.>=) (x:num0,y:double) = x .>= (Dbl_c y)
        
        ///<summary>代入</summary>
        static member (<==) (x:num0,y:num0) =
            match x.etype,y.etype with
            |Dt,Zt -> printfn "Warning: complex型からdouble型への代入です：%s←%s" x.code y.code
            |It _,Dt -> printfn "Warning: double型からint型への代入です：%s←%s" x.code y.code
            |It 2,It 4 -> printfn "Warning: int型からbyte型への代入です：%s←%s" x.code y.code
            |_ ->
                match p.lang with
                |Fortran ->
                    p.codewrite(x.code + " = " + y.code)
                |C99 ->
                    p.codewrite(x.code + " = " + y.code + ";")
                |LaTeX ->
                    p.codewrite(x.code + " \\leftarrow " + y.code)
                |HTML ->
                    p.codewrite(x.code + " \\leftarrow " + y.code)
                |Python ->
                    p.codewrite(x.code+" = "+y.code)
                    
        static member (<==) (x:num0,y:int) = x <== (Int_c y)
        static member (<==) (x:num0,y:double) = x <== (Dbl_c y)
        static member (<==) (x:list<num0>,y:num0) = for v in x do v <== y
        static member (<==) (x:list<num0>,y:int) = for v in x do v <== y
        static member (<==) (x:list<num0>,y:double) = for v in x do v <== y
        member this.clear() = this <== 0
        
        ///<summary>等式(TeX、HTMLのみ)</summary>
        static member (===) (x:num0,y:num0) =
            match x.etype,y.etype with
            |Dt,Zt -> printfn "Warning: complex型からdouble型への代入です：%s←%s" x.code y.code
            |It _,Dt -> printfn "Warning: double型からint型への代入です：%s←%s" x.code y.code
            |It 2,It 4 -> printfn "Warning: int型からbyte型への代入です：%s←%s" x.code y.code
            |_ ->
                match p.lang with
                |Fortran ->
                    ()
                |C99 ->
                    ()
                |LaTeX ->
                    p.codewrite(x.code + " = " + y.code)
                |HTML ->
                    p.codewrite(x.code + " = " + y.code)
                |Python->
                    ()
        static member (===) (x:num0,y:int) = x === (Int_c y)
        static member (===) (x:num0,y:double) = x === (Dbl_c y)
        
        ///<summary>等号揃等式(TeX、HTMLのみ)</summary>
        static member (=|=) (x:num0,y:num0) =
            match x.etype,y.etype with
            |Dt,Zt -> printfn "Warning: complex型からdouble型への代入です：%s←%s" x.code y.code
            |It _,Dt -> printfn "Warning: double型からint型への代入です：%s←%s" x.code y.code
            |It 2,It 4 -> printfn "Warning: int型からbyte型への代入です：%s←%s" x.code y.code
            |_ ->
                match p.lang with
                |Fortran ->
                    ()
                |C99 ->
                    ()
                |LaTeX ->
                    p.codewrite(x.code + " =& " + y.code)
                |HTML ->
                    p.codewrite(x.code + " =& " + y.code)
                |Python->
                    ()
        static member (=|=) (x:num0,y:int) = x =|= (Int_c y)
        static member (=|=) (x:num0,y:double) = x =|= (Dbl_c y)