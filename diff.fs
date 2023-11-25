namespace Aqualis
    
    open Aqualis_base
    
    [<AutoOpen>]
    module asm_diff =
        type asm with
            ///<summary>共役複素数</summary>
            static member diff (f:num0) = fun (x:num0) ->
                match x with
                |Var(_,xt) ->
                    match f with
                    |Str_c _ ->
                        printfn "文字列を微分できません"
                        NaN
                    |Int_c _ ->
                        Int_c 0
                    |Dbl_c _ ->
                        Int_c 0
                    |Var(_,vt) when vt=xt ->
                        Int_c 1
                    |Var(_,_) ->
                        Int_c 0
                    |Par(_,v) ->
                        asm.diff v x
                    |Inv(_,v) ->
                        -(asm.diff v x)
                    |Add(_,v,u) ->
                        (asm.diff v x)+(asm.diff u x)
                    |Sub(_,v,u) ->
                        (asm.diff v x)-(asm.diff u x)
                    |Mul(_,v,u) ->
                        v*(asm.diff u x)+(asm.diff v x)*u
                    |Div(_,v,u) ->
                        (asm.diff v x)/u-v/(u*u)*(asm.diff u x)
                    |Pow(_,Abs(_,v),Int_c 2) when v.etype=Zt ->
                        let u = asm.conj(asm.diff v x)
                        (Int_c 2)*v*u
                    |Pow(t,v,Int_c u) ->
                        u*Pow(t,v,Int_c (u-1))*(asm.diff v x)
                    |Pow(t,v,Dbl_c u) ->
                        u*Pow(t,v,Dbl_c (u-1.0))*(asm.diff v x)
                    |Pow(_,v,u) ->
                        Exp(u%%v,u*Log(v.etype,v))*(asm.diff (u*Log(v.etype,v)) x)
                    |Exp(t,v) ->
                        Exp(t,v)*(asm.diff v x)
                    |Sin(t,v) ->
                        Cos(t,v)*(asm.diff v x)
                    |Cos(t,v) ->
                        -Sin(t,v)*(asm.diff v x)
                    |Tan(t,v) ->
                        (Int_c 1)/Pow(t,Cos(t,v),Int_c 2)*(asm.diff v x)
                    |Asin(t,v) ->
                        (asm.diff v x)/Sqrt(v.etype,Int_c 1-v*v)
                    |Acos(t,v) ->
                        -(asm.diff v x)/Sqrt(v.etype,Int_c 1-v*v)
                    |Atan(t,v) ->
                        (asm.diff v x)/(Int_c 1+v*v)
                    |Atan2 _ ->
                        printfn "atan2を微分できません"
                        NaN
                    |Abs(t,v) -> 
                        let u = asm.conj(asm.diff v x)
                        v/Abs(t,v)*u
                    |Log(t,v) ->
                        (asm.diff v x)/v
                    |Log10(t,v) ->
                        (Dbl_c(log10(2.718281828459045)))*(asm.diff v x)/v
                    |Sqrt(t,v) ->
                        (Dbl_c 0.5)*(asm.diff v x)/Sqrt(t,v)
                    |Formula(_,r) ->
                        printfn "微分できない数式です: %s" <| r.ToString()
                        NaN
                    |Sum(t,n1,n2,f) ->
                        Sum(t,n1,n2,fun n -> asm.diff (f n) x)
                    |NaN ->
                        printfn "NaNを微分できません"
                        NaN
                |_ ->
                    printfn "Error 変数以外のもの「%s」で微分できません" (x.ToString())
                    NaN