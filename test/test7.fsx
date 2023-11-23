//#############################################################################
// project title
let projectname = "test7"
let version = "1.0.0"
//#############################################################################
 
let outputdir = __SOURCE_DIRECTORY__

#I @"C:\home\LightwaveLaboratory\Aqualis\bin\Debug\net6.0"
#r "Aqualis.dll"
 
open Aqualis
open Aqualis_base

type su (n1:num0,n2:num0,f:num0->num0) =
    member this.n1 with get() = n1
    member this.n2 with get() = n2
    member this.f with get() = f
    
    member this.fsum ff code =
        ch.d <| fun x ->
            x.clear()
            iter.range n1 n2 <| fun n ->
                x <== x + (ff(f n))
            code x
    member this.sum = this.fsum (fun x -> x)
    
type Soperator =
    |Fml of Expr
    |Sum of su
    |Add of Soperator*Soperator
    |Sub of Soperator*Soperator
    |Mul of Soperator*Soperator
    |Div of Soperator*Soperator
    |Pow of Soperator*Soperator
    |Abs of Soperator
    |Exp of Soperator
    |Sin of Soperator
    |Cos of Soperator
    |Tan of Soperator
    |Sqr of Soperator
    |Lgn of Soperator
    |Lgc of Soperator
    |Asn of Soperator
    |Acs of Soperator
    |Atn of Soperator
    
let diff(ss:Soperator,x:num0) code =
    let rec ceval (ss:Soperator,cnt1:int) : int =
        match ss with
        |Fml _ -> cnt1
        |Sum _ -> cnt1+1
        |Add(sA,sB) -> 
            let cnt2 = ceval(sA,cnt1)
            let cnt3 = ceval(sB,cnt2)
            cnt3
        |Sub(sA,sB) -> 
            let cnt2 = ceval(sA,cnt1)
            let cnt3 = ceval(sB,cnt2)
            cnt3
        |Mul(sA,sB) -> 
            let cnt2 = ceval(sA,cnt1)
            let cnt3 = ceval(sB,cnt2)
            cnt3
        |Div(sA,sB) -> 
            let cnt2 = ceval(sA,cnt1)
            let cnt3 = ceval(sB,cnt2)
            cnt3
        |Pow(sA,sB) -> 
            let cnt2 = ceval(sA,cnt1)
            let cnt3 = ceval(sB,cnt2)
            cnt3
        |Abs(s) -> 
            let cnt2 = ceval(s,cnt1)
            cnt2
        |Exp(s) -> 
            let cnt2 = ceval(s,cnt1)
            cnt2
        |Sin(s) -> 
            let cnt2 = ceval(s,cnt1)
            cnt2
        |Cos(s) -> 
            let cnt2 = ceval(s,cnt1)
            cnt2
        |Tan(s) -> 
            let cnt2 = ceval(s,cnt1)
            cnt2
        |Sqr(s) -> 
            let cnt2 = ceval(s,cnt1)
            cnt2
        |Lgn(s) -> 
            let cnt2 = ceval(s,cnt1)
            cnt2
        |Lgc(s) -> 
            let cnt2 = ceval(s,cnt1)
            cnt2
        |Asn(s) -> 
            let cnt2 = ceval(s,cnt1)
            cnt2
        |Acs(s) -> 
            let cnt2 = ceval(s,cnt1)
            cnt2
        |Atn(s) -> 
            let cnt2 = ceval(s,cnt1)
            cnt2
    let rec cdiff (ss:Soperator,x:num0,cnt1:int) : int =
        match ss with
        |Fml _ -> cnt1
        |Sum _ -> cnt1+1
        |Add(sA,sB) -> 
            let cnt2 = cdiff(sA,x,cnt1)
            let cnt3 = cdiff(sB,x,cnt2)
            cnt3
        |Sub(sA,sB) -> 
            let cnt2 = cdiff(sA,x,cnt1)
            let cnt3 = cdiff(sB,x,cnt2)
            cnt3
        |Mul(sA,sB) -> 
            let cnt2 = cdiff(sA,x,cnt1)
            let cnt3 = ceval(sB,cnt2)
            let cnt4 = ceval(sA,cnt3)
            let cnt5 = cdiff(sB,x,cnt4)
            cnt5
        |Div(sA,sB) -> 
            let cnt2 = cdiff(sA,x,cnt1)
            let cnt3 = ceval(sB,cnt2)
            let cnt4 = ceval(sA,cnt3)
            let cnt5 = cdiff(sB,x,cnt4)
            cnt5
        |Pow(sA,Fml(Int_c n)) -> 
            let cnt2 = ceval(sA,cnt1)
            let cnt3 = cdiff(sA,x,cnt2)
            cnt3
        |Pow(sA,Fml(Dbl_c n)) -> 
            let cnt2 = ceval(sA,cnt1)
            let cnt3 = cdiff(sA,x,cnt2)
            cnt3
        |Pow(sA,sB) -> 
            let cnt2 = ceval(sA,cnt1)
            let cnt3 = ceval(sB,cnt2)
            let cnt4 = cdiff(sA,x,cnt3)
            let cnt5 = cdiff(sB,x,cnt4)
            cnt5
        |Abs(s) -> 
            let cnt2 = ceval(s,cnt1)
            let cnt3 = cdiff(s,x,cnt2)
            cnt3
        |Exp(s) -> 
            let cnt2 = ceval(s,cnt1)
            let cnt3 = cdiff(s,x,cnt2)
            cnt3
        |Sin(s) -> 
            let cnt2 = ceval(s,cnt1)
            let cnt3 = cdiff(s,x,cnt2)
            cnt3
        |Cos(s) -> 
            let cnt2 = ceval(s,cnt1)
            let cnt3 = cdiff(s,x,cnt2)
            cnt3
        |Tan(s) -> 
            let cnt2 = ceval(s,cnt1)
            let cnt3 = cdiff(s,x,cnt2)
            cnt3
        |Sqr(s) -> 
            let cnt2 = ceval(s,cnt1)
            let cnt3 = cdiff(s,x,cnt2)
            cnt3
        |Lgn(s) -> 
            let cnt2 = ceval(s,cnt1)
            let cnt3 = cdiff(s,x,cnt2)
            cnt3
        |Lgc(s) -> 
            let cnt2 = ceval(s,cnt1)
            let cnt3 = cdiff(s,x,cnt2)
            cnt3
        |Asn(s) -> 
            let cnt2 = ceval(s,cnt1)
            let cnt3 = cdiff(s,x,cnt2)
            cnt3
        |Acs(s) -> 
            let cnt2 = ceval(s,cnt1)
            let cnt3 = cdiff(s,x,cnt2)
            cnt3
        |Atn(s) -> 
            let cnt2 = ceval(s,cnt1)
            let cnt3 = cdiff(s,x,cnt2)
            cnt3
    let ntmp = cdiff(ss,_0,0)
    (if ntmp=0 then fun code -> code (num1(Nt,Var1(A1(0),""))) else (if x.etype=Zt then ch.z1 ntmp else ch.d1 ntmp)) <| fun tmp ->
        let rec eval (ss:Soperator,cnt1:int) : (num0*int) =
            match ss with
            |Fml h -> num0(h),cnt1
            |Sum(s) ->
                tmp[cnt1].clear()
                iter.range s.n1 s.n2 <| fun i ->
                    tmp[cnt1] <== tmp[cnt1] + (s.f i)
                tmp[cnt1],cnt1+1
            |Add(sA,sB) -> 
                let (a,cnt2) = eval(sA,cnt1)
                let (b,cnt3) = eval(sB,cnt2)
                (a+b),cnt3
            |Sub(sA,sB) -> 
                let (a,cnt2) = eval(sA,cnt1)
                let (b,cnt3) = eval(sB,cnt2)
                (a-b),cnt3
            |Mul(sA,sB) -> 
                let (a,cnt2) = eval(sA,cnt1)
                let (b,cnt3) = eval(sB,cnt2)
                (a*b),cnt3
            |Div(sA,sB) -> 
                let (a,cnt2) = eval(sA,cnt1)
                let (b,cnt3) = eval(sB,cnt2)
                (a/b),cnt3
            |Pow(sA,sB) -> 
                let (a,cnt2) = eval(sA,cnt1)
                let (b,cnt3) = eval(sB,cnt2)
                (asm.pow(a,b)),cnt3
            |Abs(s) -> 
                let (a,cnt2) = eval(s,cnt1)
                (asm.abs(a)),cnt2
            |Exp(s) -> 
                let (a,cnt2) = eval(s,cnt1)
                (asm.exp(a)),cnt2
            |Sin(s) -> 
                let (a,cnt2) = eval(s,cnt1)
                (asm.sin(a)),cnt2
            |Cos(s) -> 
                let (a,cnt2) = eval(s,cnt1)
                (asm.cos(a)),cnt2
            |Tan(s) -> 
                let (a,cnt2) = eval(s,cnt1)
                (asm.tan(a)),cnt2
            |Sqr(s) -> 
                let (a,cnt2) = eval(s,cnt1)
                (asm.sqrt(a)),cnt2
            |Lgn(s) -> 
                let (a,cnt2) = eval(s,cnt1)
                (asm.log(a)),cnt2
            |Lgc(s) -> 
                let (a,cnt2) = eval(s,cnt1)
                (asm.log10(a)),cnt2
            |Asn(s) -> 
                let (a,cnt2) = eval(s,cnt1)
                (asm.asin(a)),cnt2
            |Acs(s) -> 
                let (a,cnt2) = eval(s,cnt1)
                (asm.acos(a)),cnt2
            |Atn(s) -> 
                let (a,cnt2) = eval(s,cnt1)
                (asm.atan(a)),cnt2
        let rec diff (ss:Soperator,x:num0,cnt1:int) : (num0*int) =
            match ss with
            |Fml h -> asm.diff(num0(h),x),cnt1
            |Sum(s) ->
                tmp[cnt1].clear()
                iter.range s.n1 s.n2 <| fun i ->
                    tmp[cnt1] <== tmp[cnt1] + asm.diff(s.f i,x)
                tmp[cnt1],cnt1+1
            |Add(sA,sB) -> 
                let (a,cnt2) = diff(sA,x,cnt1)
                let (b,cnt3) = diff(sB,x,cnt2)
                a+b,cnt3
            |Sub(sA,sB) -> 
                let (a,cnt2) = diff(sA,x,cnt1)
                let (b,cnt3) = diff(sB,x,cnt2)
                a-b,cnt3
            |Mul(sA,sB) -> 
                let (fa,cnt2) = eval(sA,cnt1)
                let (fb,cnt3) = eval(sB,cnt2)
                let (da,cnt4) = diff(sA,x,cnt3)
                let (db,cnt5) = diff(sB,x,cnt4)
                (da*fb+fa*db),cnt5
            |Div(sA,sB) -> 
                let (fa,cnt2) = eval(sA,cnt1)
                let (fb,cnt3) = eval(sB,cnt2)
                let (da,cnt4) = diff(sA,x,cnt3)
                let (db,cnt5) = diff(sB,x,cnt4)
                (da/fb-fa*db/(fb*fb)),cnt5
            |Pow(sA,Fml(Int_c n)) -> 
                let (fa,cnt2) = eval(sA,cnt1)
                let (da,cnt3) = diff(sA,x,cnt2)
                (n*asm.pow(fa,n-1)*da),cnt3
            |Pow(sA,Fml(Dbl_c n)) -> 
                let (fa,cnt2) = eval(sA,cnt1)
                let (da,cnt3) = diff(sA,x,cnt2)
                (n*asm.pow(fa,n-1.0)*da),cnt3
            |Pow(sA,sB) -> 
                let (fa,cnt2) = eval(sA,cnt1)
                let (fb,cnt3) = eval(sB,cnt2)
                let (da,cnt4) = diff(sA,x,cnt3)
                let (db,cnt5) = diff(sB,x,cnt4)
                asm.exp(fb*asm.log(fa))*(db*asm.log(fa)+fb*da/fa),cnt5
            |Abs(s) -> 
                let (fa,cnt2) = eval(s,cnt1)
                let (da,cnt3) = diff(s,x,cnt2)
                (fa/asm.abs(fa)*asm.conj(da)),cnt3
            |Exp(s) -> 
                let (fa,cnt2) = eval(s,cnt1)
                let (da,cnt3) = diff(s,x,cnt2)
                (asm.exp(fa)*da),cnt3
            |Sin(s) -> 
                let (fa,cnt2) = eval(s,cnt1)
                let (da,cnt3) = diff(s,x,cnt2)
                (asm.cos(fa)*da),cnt3
            |Cos(s) -> 
                let (fa,cnt2) = eval(s,cnt1)
                let (da,cnt3) = diff(s,x,cnt2)
                (-asm.sin(fa)*da),cnt3
            |Tan(s) -> 
                let (fa,cnt2) = eval(s,cnt1)
                let (da,cnt3) = diff(s,x,cnt2)
                (1/asm.pow(asm.cos(fa),2)*da),cnt3
            |Sqr(s) -> 
                let (fa,cnt2) = eval(s,cnt1)
                let (da,cnt3) = diff(s,x,cnt2)
                (da/(2*asm.sqrt(fa))),cnt3
            |Lgn(s) -> 
                let (fa,cnt2) = eval(s,cnt1)
                let (da,cnt3) = diff(s,x,cnt2)
                (da/fa),cnt3
            |Lgc(s) -> 
                let (fa,cnt2) = eval(s,cnt1)
                let (da,cnt3) = diff(s,x,cnt2)
                (da/(fa*log(10.0))),cnt3
            |Asn(s) -> 
                let (fa,cnt2) = eval(s,cnt1)
                let (da,cnt3) = diff(s,x,cnt2)
                (da/asm.sqrt(1-fa*fa)),cnt3
            |Acs(s) -> 
                let (fa,cnt2) = eval(s,cnt1)
                let (da,cnt3) = diff(s,x,cnt2)
                (-da/asm.sqrt(1-fa*fa)),cnt3
            |Atn(s) -> 
                let (fa,cnt2) = eval(s,cnt1)
                let (da,cnt3) = diff(s,x,cnt2)
                (da/(1+fa*fa)),cnt3
        let (v,_) = diff(ss,x,1)
        code v
        
Compile [F;C;] outputdir projectname ("aaa","aaa") <| fun () ->
    let dd = 1E-5
    ch.dddd <| fun (x,y1,y2,dy) ->
        x <== -10
        codestr.section "001" <| fun () ->
            print.c <| asm.diff(x+2+y1,x)
        codestr.section "002" <| fun () ->
            print.c <| asm.diff(5*x+2,x)
        codestr.section "003" <| fun () ->
            print.c <| asm.diff(5*x*x+2,x)
        codestr.section "004" <| fun () ->
            let f1(x:num0) = (x+1)/(2*x+7)
            y1 <== f1 x
            y2 <== f1 (x+dd)
            dy <== (y2-y1)/dd
            print.c <| dy
            print.c <| asm.diff(f1 x,x)
        codestr.section "005" <| fun () ->
            let f1(x:num0) = x*asm.cos(x*x+2*x+1)
            y1 <== f1 x
            y2 <== f1 (x+dd)
            dy <== (y2-y1)/dd
            print.c <| dy
            print.c <| asm.diff(f1 x,x)
        codestr.section "006A" <| fun () ->
            let f = 3*x
            diff (Fml(f.expr),x) <| fun y ->
                print.c y
        codestr.section "006B" <| fun () ->
            let f = 3*x*x+4*x
            diff (Fml(f.expr),x) <| fun y ->
                print.c y
        codestr.section "006C" <| fun () ->
            diff (Sum(su(_1,_4,fun n -> n*x)),x) <| fun y ->
                print.c y
        codestr.section "006D" <| fun () ->
            diff (Add(Sum(su(_1,_4,fun n -> n*x)),Sum(su(_1,_4,fun n -> n*x))),x) <| fun y ->
                print.c y
        codestr.section "006E" <| fun () ->
            diff (Mul(Sum(su(_1,_4,fun n -> n*x)),Sum(su(_1,_4,fun n -> n*x))),x) <| fun y ->
                print.c y
        codestr.section "006F" <| fun () ->
            diff (Div(Sum(su(_1,_4,fun n -> n*x*x)),Sum(su(_1,_4,fun n -> n*x))),x) <| fun y ->
                print.c y
        codestr.section "006G" <| fun () ->
            diff (Exp(Sum(su(_1,_4,fun n -> n*x/100))),x) <| fun y ->
                print.c y
        codestr.section "006H" <| fun () ->
            diff (Abs(Sum(su(_1,_4,fun n -> n*x))),x) <| fun y ->
                print.c y
        codestr.section "006I" <| fun () ->
            diff (Pow(Abs(Sum(su(_1,_4,fun n -> n*x))),Fml(Int_c 2)),x) <| fun y ->
                print.c y