namespace Aqualis
    
    open Aqualis_base
    
    [<AutoOpen>]
    module asm_diff =
        type asm with
            ///<summary>共役複素数</summary>
            static member diff (f:num0) = fun (x:num0) ->
                    match f,x with
                    |Str_c _,_ ->
                        printfn "文字列を微分できません"
                        NaN
                    |Int_c _,_ ->
                        Int_c 0
                    |Dbl_c _,_ ->
                        Int_c 0
                    |Var(_,vt),Var(_,xt) when vt=xt ->
                        Int_c 1
                    |Var(_,_),_ ->
                        Int_c 0
                    |Par(_,v),(Var(_,_)|Idx1(_,_,_)|Idx2(_,_,_,_)|Idx3(_,_,_,_,_)) ->
                        asm.diff v x
                    |Inv(_,v),(Var(_,_)|Idx1(_,_,_)|Idx2(_,_,_,_)|Idx3(_,_,_,_,_)) ->
                        -(asm.diff v x)
                    |Add(_,v,u),(Var(_,_)|Idx1(_,_,_)|Idx2(_,_,_,_)|Idx3(_,_,_,_,_)) ->
                        (asm.diff v x)+(asm.diff u x)
                    |Sub(_,v,u),(Var(_,_)|Idx1(_,_,_)|Idx2(_,_,_,_)|Idx3(_,_,_,_,_)) ->
                        (asm.diff v x)-(asm.diff u x)
                    |Mul(_,v,u),(Var(_,_)|Idx1(_,_,_)|Idx2(_,_,_,_)|Idx3(_,_,_,_,_)) ->
                        v*(asm.diff u x)+(asm.diff v x)*u
                    |Div(_,v,u),(Var(_,_)|Idx1(_,_,_)|Idx2(_,_,_,_)|Idx3(_,_,_,_,_)) ->
                        (asm.diff v x)/u-v/(u*u)*(asm.diff u x)
                    |Pow(_,Abs(_,v),Int_c 2),(Var(_,_)|Idx1(_,_,_)|Idx2(_,_,_,_)|Idx3(_,_,_,_,_)) when v.etype=Zt ->
                        let u = asm.conj(asm.diff v x)
                        (Int_c 2)*v*u
                    |Pow(t,v,Int_c u),(Var(_,_)|Idx1(_,_,_)|Idx2(_,_,_,_)|Idx3(_,_,_,_,_)) ->
                        u*Pow(t,v,Int_c (u-1))*(asm.diff v x)
                    |Pow(t,v,Dbl_c u),(Var(_,_)|Idx1(_,_,_)|Idx2(_,_,_,_)|Idx3(_,_,_,_,_)) ->
                        u*Pow(t,v,Dbl_c (u-1.0))*(asm.diff v x)
                    |Pow(_,v,u),(Var(_,_)|Idx1(_,_,_)|Idx2(_,_,_,_)|Idx3(_,_,_,_,_)) ->
                        Exp(u%%v,u*Log(v.etype,v))*(asm.diff (u*Log(v.etype,v)) x)
                    |Exp(t,v),(Var(_,_)|Idx1(_,_,_)|Idx2(_,_,_,_)|Idx3(_,_,_,_,_)) ->
                        Exp(t,v)*(asm.diff v x)
                    |Sin(t,v),(Var(_,_)|Idx1(_,_,_)|Idx2(_,_,_,_)|Idx3(_,_,_,_,_)) ->
                        Cos(t,v)*(asm.diff v x)
                    |Cos(t,v),(Var(_,_)|Idx1(_,_,_)|Idx2(_,_,_,_)|Idx3(_,_,_,_,_)) ->
                        -Sin(t,v)*(asm.diff v x)
                    |Tan(t,v),(Var(_,_)|Idx1(_,_,_)|Idx2(_,_,_,_)|Idx3(_,_,_,_,_)) ->
                        (Int_c 1)/Pow(t,Cos(t,v),Int_c 2)*(asm.diff v x)
                    |Asin(t,v),(Var(_,_)|Idx1(_,_,_)|Idx2(_,_,_,_)|Idx3(_,_,_,_,_)) ->
                        (asm.diff v x)/Sqrt(v.etype,Int_c 1-v*v)
                    |Acos(t,v),(Var(_,_)|Idx1(_,_,_)|Idx2(_,_,_,_)|Idx3(_,_,_,_,_)) ->
                        -(asm.diff v x)/Sqrt(v.etype,Int_c 1-v*v)
                    |Atan(t,v),(Var(_,_)|Idx1(_,_,_)|Idx2(_,_,_,_)|Idx3(_,_,_,_,_)) ->
                        (asm.diff v x)/(Int_c 1+v*v)
                    |Atan2 _,_ ->
                        printfn "atan2を微分できません"
                        NaN
                    |Abs(t,v),(Var(_,_)|Idx1(_,_,_)|Idx2(_,_,_,_)|Idx3(_,_,_,_,_)) -> 
                        let u = asm.conj(asm.diff v x)
                        v/Abs(t,v)*u
                    |Log(t,v),(Var(_,_)|Idx1(_,_,_)|Idx2(_,_,_,_)|Idx3(_,_,_,_,_)) ->
                        (asm.diff v x)/v
                    |Log10(t,v),(Var(_,_)|Idx1(_,_,_)|Idx2(_,_,_,_)|Idx3(_,_,_,_,_)) ->
                        (Dbl_c(log10(2.718281828459045)))*(asm.diff v x)/v
                    |Sqrt(t,v),(Var(_,_)|Idx1(_,_,_)|Idx2(_,_,_,_)|Idx3(_,_,_,_,_)) ->
                        (Dbl_c 0.5)*(asm.diff v x)/Sqrt(t,v)
                    |Idx1(t1,u1,nA1),Idx1(t2,u2,nB1) ->
                        if t1=t2 && u1=u2 then
                            let c = num0.ch(It 4)
                            br.if2 (nA1.=nB1)
                                <| fun () -> c <== 1 
                                <| fun () -> c <== 0 
                            c
                        else
                           Int_c 0
                    |Idx2(t1,u1,nA1,nA2),Idx2(t2,u2,nB1,nB2) ->
                        if t1=t2 && u1=u2 then
                            let c = num0.ch(It 4)
                            br.if2 (And [nA1.=nB1;nA2.=nB2;])
                                <| fun () -> c <== 1 
                                <| fun () -> c <== 0 
                            c
                        else
                           Int_c 0
                    |Idx3(t1,u1,nA1,nA2,nA3),Idx3(t2,u2,nB1,nB2,nB3) ->
                        if t1=t2 && u1=u2 then
                            let c = num0.ch(It 4)
                            br.if2 (And [nA1.=nB1;nA2.=nB2;nA3.=nB3;])
                                <| fun () -> c <== 1 
                                <| fun () -> c <== 0 
                            c
                        else
                           Int_c 0
                    |Formula(_,r),_ ->
                        printfn "微分できない数式です: %s" <| r.ToString()
                        NaN
                    |Sum(t,n1,n2,f),_ ->
                        Sum(t,n1,n2,fun n -> asm.diff (f n) x)
                    |NaN,_ ->
                        printfn "NaNを微分できません"
                        NaN
                    |_ ->
                        printfn "Error 変数以外のもの「%s」で微分できません" (x.ToString())
                        NaN